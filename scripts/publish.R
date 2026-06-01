options(width = 120)
suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
  library(lubridate)
})
source("R/sheet_auth.R")
source("R/elo_table.R")
source("R/cube_tier.R")

PUBLISH_DIR <- "data/publish"
CMDSTAN_VERSION <- Sys.getenv("CMDSTAN_VERSION", "2.38.0")
SHEET_URL <- "https://docs.google.com/spreadsheets/d/1bq5DXQs1nobk0nu9cN-4UOHPkcPK3fvkTLa2t2lVNKk/edit"

# One row per GAME (game1:game3 unpivoted), named cube preserved, title-cased.
load_results_games <- function(sheet_url = SHEET_URL) {
  read_sheet(sheet_url) |>
    mutate(match_id = dplyr::row_number(), date = lubridate::as_date(date)) |>
    tidyr::pivot_longer(game1:game3, names_to = "game", values_to = "winner") |>
    tidyr::drop_na(winner) |>
    mutate(
      player1 = stringr::str_to_title(player1),
      player2 = stringr::str_to_title(player2),
      winner  = stringr::str_to_title(winner),
      cube    = stringr::str_to_title(cube)
    )
}

# One row per MATCH, with the match winner. Input = load_results_games() output.
load_match_results <- function(games) {
  games |>
    dplyr::summarise(
      p1_game_wins = sum(.data$winner == .data$player1),
      p2_game_wins = sum(.data$winner == .data$player2),
      .by = c(match_id, date, player1, player2, cube)
    ) |>
    dplyr::mutate(match_winner = dplyr::if_else(p1_game_wins > p2_game_wins, player1, player2))
}

# Calendar tab joined to the Cube list (for out-links). One row per scheduled event.
load_calendar_data <- function(sheet_url = SHEET_URL) {
  cube_list <- read_sheet(sheet_url, sheet = "Cube list") |>
    janitor::clean_names() |>
    transmute(
      cube_key  = stringr::str_to_lower(name),
      cube_link = dplyr::if_else(stringr::str_detect(link, "^https?://"), link, NA_character_)
    )
  read_sheet(sheet_url, sheet = "Cube calendar") |>
    janitor::clean_names() |>
    mutate(
      date = lubridate::as_date(date),
      cube = stringr::str_to_title(cube),
      host = stringr::str_to_title(host),
      cube_key = stringr::str_to_lower(cube)
    ) |>
    dplyr::filter(!is.na(cube), cube != "", cube != "Na") |>
    dplyr::left_join(cube_list, by = "cube_key") |>
    dplyr::select(-cube_key) |>
    dplyr::arrange(date)
}

# list_results_dirs() returns newest-first (decreasing = TRUE in elo_table.R)
newest_results_dir <- function() {
  dirs <- list_results_dirs()
  dirs[1]
}

opted_in_players <- function() {
  cube_gs4_auth()
  # fetch_table_players() returns lowercase nicknames; player_summary.csv uses
  # title-case names (from str_to_title() in prepare_cube_data). Match the case.
  stringr::str_to_title(fetch_table_players())
}

TIERS <- c("High", "Medium", "Low", "Other")

# Per-player last game date from a results dir's processed_data.rds.
last_game_dates <- function(results_dir) {
  pd <- readRDS(file.path(results_dir, "processed_data.rds"))
  dplyr::bind_rows(
    dplyr::transmute(pd, player = player1, date),
    dplyr::transmute(pd, player = player2, date)
  ) |>
    dplyr::mutate(player = stringr::str_to_title(player)) |>
    dplyr::summarise(last_game = max(date), .by = player)
}

# Run rank_estimates for the overall table and each tier, return player -> nr columns.
.ranks_for <- function(summary, last_game, ref_date, optin_lc) {
  cutoff <- ref_date - lubridate::weeks(8)
  overall <- summary |>
    dplyr::transmute(player, score = score_median, wins, total) |>
    dplyr::left_join(last_game, by = "player") |>
    rank_estimates(18, 0.15, 8, optin_lc, cutoff) |>
    dplyr::transmute(player, rank = nr, score)
  out <- list(overall)
  for (tier in TIERS) {
    tr <- summary |>
      dplyr::transmute(player,
        score = .data[[paste0(tier, "_elo_median")]],
        wins  = .data[[paste0(tier, "_wins")]],
        total = .data[[paste0(tier, "_total")]]
      ) |>
      dplyr::left_join(last_game, by = "player") |>
      rank_estimates(3, 0.0, 8, optin_lc, cutoff) |>
      dplyr::transmute(
        player, !!paste0(tier, "_rank") := nr,
        !!paste0(tier, "_elo") := score
      )
    out <- c(out, list(tr))
  }
  purrr::reduce(out, dplyr::full_join, by = "player")
}

rankings_attach_ranks <- function(base, now, prev, last_now, last_prev,
                                  ref_now, ref_prev, optin_lc) {
  cur <- .ranks_for(now, last_now, ref_now, optin_lc)
  cur_join <- cur |>
    dplyr::select(player, rank, dplyr::ends_with("_rank"))
  base <- base |>
    dplyr::left_join(cur_join, by = "player") |>
    dplyr::left_join(last_now, by = "player") |>
    dplyr::mutate(last_date = as.character(last_game)) |>
    dplyr::select(-last_game)
  if (!is.null(prev)) {
    pv <- .ranks_for(prev, last_prev, ref_prev, optin_lc)
    pv_join <- pv |>
      dplyr::transmute(player,
        prev_rank = rank, prev_score_median = score,
        !!!rlang::set_names(
          purrr::map(TIERS, ~ rlang::sym(paste0(.x, "_rank"))),
          paste0(TIERS, "_prev_rank")
        ),
        !!!rlang::set_names(
          purrr::map(TIERS, ~ rlang::sym(paste0(.x, "_elo"))),
          paste0(TIERS, "_prev_elo")
        )
      )
    base <- dplyr::left_join(base, pv_join, by = "player")
  } else {
    base <- base |>
      dplyr::mutate(prev_rank = NA_integer_, prev_score_median = NA_real_)
  }
  base
}

build_rankings <- function(results_dir, prev_dir, optin) {
  optin_lc <- stringr::str_to_lower(optin)
  now <- readr::read_csv(file.path(results_dir, "player_summary.csv"), show_col_types = FALSE)
  base <- now |>
    dplyr::filter(.data$player %in% optin) |>
    dplyr::arrange(dplyr::desc(.data$score_median))
  last_now <- last_game_dates(results_dir)
  ref_now <- max(last_now$last_game)
  if (!is.na(prev_dir) && dir.exists(prev_dir)) {
    prev <- readr::read_csv(file.path(prev_dir, "player_summary.csv"), show_col_types = FALSE)
    last_prev <- last_game_dates(prev_dir)
    ref_prev <- max(last_prev$last_game)
  } else {
    prev <- NULL
    last_prev <- NULL
    ref_prev <- NULL
  }
  rankings_attach_ranks(base, now, prev, last_now, last_prev, ref_now, ref_prev, optin_lc)
}

build_meta_enriched <- function(results_dir, rankings, processed_data, games) {
  tiers <- games |>
    dplyr::distinct(cube) |>
    dplyr::mutate(tier = cube_tier(cube)) |>
    dplyr::arrange(cube) |>
    split(~tier) |>
    purrr::map(~ as.list(.x$cube))
  list(
    generated_at    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    fit_date        = basename(results_dir),
    n_players       = nrow(rankings),
    n_games         = nrow(processed_data),
    n_dates         = dplyr::n_distinct(processed_data$date),
    reference_date  = as.character(max(processed_data$date)),
    cmdstan_version = CMDSTAN_VERSION,
    model           = "bradley_terry_temporal",
    tiers           = tiers
  )
}

write_json_file <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_json(x, path, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = 6)
}

slugify <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_replace_all("[^a-z0-9]+", "-") |>
    stringr::str_replace_all("(^-|-$)", "")
}

# Filter (min games, min win-rate, opt-in, recent activity) then dense-rank by score.
# Mirrors prepare_ranking_data() (R/elo_table.R) but is fit-free: estimates come from CSV.
# `df` needs columns: player (title-case), score, wins, total, last_game (Date).
# `table_players` is a lowercase opt-in vector (or NULL).
# `max_absence_weeks` toggles the absence filter; `absence_cutoff` is the precomputed
# Date floor the caller derives from the dataset's reference date (= max event date),
# NOT wall-clock Sys.Date() — a run weeks after the last event must not drop active players.
rank_estimates <- function(df, min_total_games, min_winrate, max_absence_weeks,
                           table_players, absence_cutoff) {
  df |>
    dplyr::filter(!is.na(.data$score)) |>
    dplyr::mutate(hlutf = .data$wins / .data$total) |>
    dplyr::arrange(dplyr::desc(.data$score)) |>
    dplyr::filter(
      .data$total >= min_total_games,
      .data$hlutf >= min_winrate,
      is.null(table_players) | stringr::str_to_lower(.data$player) %in% table_players,
      is.null(max_absence_weeks) | .data$last_game >= absence_cutoff
    ) |>
    dplyr::mutate(nr = dplyr::row_number())
}

# Build {player -> {player, slug, summary, history}} from the stacked CSV history.
player_history_records <- function(hist) {
  hist <- hist |>
    dplyr::arrange(date, dplyr::desc(score_median)) |>
    dplyr::mutate(rank = dplyr::row_number(), .by = date) |>
    dplyr::mutate(win_rate = dplyr::if_else(total > 0, wins / total * 100, 0))
  split(hist, hist$player) |>
    purrr::imap(function(rows, player) {
      rows <- dplyr::arrange(rows, date)
      history <- rows |>
        dplyr::transmute(
          date = as.character(date),
          score_median, score_q25, score_q75, score_lower, score_upper,
          rank, win_rate, games = total, wins, losses,
          strength = purrr::pmap(
            list(gamma_High, gamma_Medium, gamma_Low, gamma_Other),
            ~ list(High = ..1, Medium = ..2, Low = ..3, Other = ..4)
          )
        )
      latest <- rows[nrow(rows), ]
      gammas <- c(
        High = latest$gamma_High, Medium = latest$gamma_Medium,
        Low = latest$gamma_Low, Other = latest$gamma_Other
      )
      gammas <- gammas[!is.na(gammas)]
      strongest <- if (length(gammas)) names(gammas)[which.max(gammas)] else NA_character_
      list(
        player = player, slug = slugify(player),
        summary = list(
          score_median = latest$score_median, rank = latest$rank,
          win_rate = latest$wins / latest$total * 100,
          games = latest$total, strongest_tier = strongest
        ),
        history = purrr::transpose(history)
      )
    })
}

# Per (opted-in player, named cube): game W/L/total/win% + tier + trophy count.
per_cube_player_records <- function(games, optin) {
  optin_lc <- stringr::str_to_lower(optin)
  long <- dplyr::bind_rows(
    games |> dplyr::transmute(match_id, date, cube, player = player1, win = as.integer(winner == player1)),
    games |> dplyr::transmute(match_id, date, cube, player = player2, win = as.integer(winner == player2))
  ) |>
    dplyr::filter(stringr::str_to_lower(player) %in% optin_lc)

  records <- long |>
    dplyr::summarise(wins = sum(win), total = dplyr::n(), .by = c(player, cube)) |>
    dplyr::mutate(
      losses = total - wins,
      win_rate = round(wins / total * 100),
      tier = cube_tier(cube)
    )

  match_winners <- games |>
    dplyr::summarise(
      p1 = sum(winner == player1), p2 = sum(winner == player2),
      player1 = dplyr::first(player1), player2 = dplyr::first(player2),
      .by = c(match_id, date, cube)
    ) |>
    dplyr::mutate(match_winner = dplyr::if_else(p1 > p2, player1, player2))
  trophies <- match_winners |>
    dplyr::count(date, cube, player = match_winner, name = "match_wins") |> # MATCH wins per event
    dplyr::filter(match_wins >= 3) |>
    dplyr::summarise(trophies = dplyr::n(), .by = c(player, cube))

  records |>
    dplyr::left_join(trophies, by = c("player", "cube")) |>
    dplyr::mutate(trophies = tidyr::replace_na(trophies, 0L)) |>
    dplyr::select(player, cube, tier, wins, losses, games = total, win_rate, trophies)
}

build_players <- function(optin, games) {
  hist <- combine_player_summaries()
  hist <- dplyr::filter(hist, .data$player %in% optin)
  profiles <- player_history_records(hist)
  pc <- per_cube_player_records(games, optin)
  purrr::imap(profiles, function(prof, player) {
    prof$cubes <- pc |>
      dplyr::filter(.data$player == !!player) |>
      dplyr::select(cube, tier, wins, losses, games, win_rate, trophies) |>
      purrr::transpose()
    prof
  })
}

head_to_head_records <- function(games, optin) {
  g <- games |>
    dplyr::filter(.data$player1 %in% optin, .data$player2 %in% optin) |>
    dplyr::mutate(
      a = pmin(player1, player2), b = pmax(player1, player2),
      a_won = as.integer(winner == a)
    )
  overall <- g |>
    dplyr::summarise(a_wins = sum(a_won), b_wins = sum(1L - a_won), .by = c(a, b))
  by_cube <- g |>
    dplyr::summarise(a_wins = sum(a_won), b_wins = sum(1L - a_won), .by = c(a, b, cube))
  purrr::pmap(overall, function(a, b, a_wins, b_wins) {
    bc <- by_cube |>
      dplyr::filter(.data$a == !!a, .data$b == !!b) |>
      dplyr::transmute(cube, a_wins, b_wins) |>
      purrr::transpose()
    list(player_a = a, player_b = b, a_wins = a_wins, b_wins = b_wins, by_cube = bc)
  })
}

build_head_to_head <- function(games, optin) head_to_head_records(games, optin)

build_cubes <- function(results_dir) {
  pd <- readRDS(file.path(results_dir, "processed_data.rds"))
  pd |>
    dplyr::summarise(
      n_games = dplyr::n(),
      n_events = dplyr::n_distinct(.data$date),
      .by = cube
    ) |>
    purrr::pmap(function(cube, n_games, n_events) {
      list(cube = cube, n_games = n_games, n_events = n_events)
    })
}

build_calendar <- function(results_dir) {
  game_dates <- readRDS(file.path(results_dir, "game_dates.rds"))
  game_dates |>
    dplyr::arrange(.data$date) |>
    dplyr::transmute(date = as.character(.data$date)) |>
    purrr::pmap(function(date) list(date = date))
}

main <- function() {
  results_dir <- newest_results_dir()
  optin <- opted_in_players()
  rankings <- build_rankings(results_dir, optin)
  processed_data <- readRDS(file.path(results_dir, "processed_data.rds"))
  games <- load_results_games()

  write_json_file(rankings, file.path(PUBLISH_DIR, "rankings.json"))
  write_json_file(build_meta_enriched(results_dir, rankings, processed_data, games), file.path(PUBLISH_DIR, "meta.json"))
  write_json_file(build_head_to_head(results_dir, optin), file.path(PUBLISH_DIR, "head_to_head.json"))
  write_json_file(build_cubes(results_dir), file.path(PUBLISH_DIR, "cubes.json"))
  write_json_file(build_calendar(results_dir), file.path(PUBLISH_DIR, "calendar.json"))

  players <- build_players(optin)
  for (p in players) write_json_file(p, file.path(PUBLISH_DIR, "players", paste0(slugify(p$player), ".json")))

  cat("published", nrow(rankings), "players,", length(players), "profiles from", basename(results_dir), "\n")
}

if (sys.nframe() == 0) main()
