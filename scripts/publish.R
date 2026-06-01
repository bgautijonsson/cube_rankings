options(width = 120)
suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
})
source("R/sheet_auth.R")
source("R/elo_table.R")

PUBLISH_DIR <- "data/publish"
CMDSTAN_VERSION <- Sys.getenv("CMDSTAN_VERSION", "2.38.0")

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

build_rankings <- function(results_dir, optin) {
  summary <- readr::read_csv(file.path(results_dir, "player_summary.csv"),
    show_col_types = FALSE
  )
  summary |>
    filter(.data$player %in% optin) |>
    arrange(desc(.data$score_median))
}

build_meta <- function(results_dir, rankings) {
  list(
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    fit_date = basename(results_dir),
    n_players = nrow(rankings),
    cmdstan_version = CMDSTAN_VERSION,
    model = "bradley_terry_temporal"
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

build_players <- function(optin) {
  hist <- combine_player_summaries()
  hist <- dplyr::filter(hist, .data$player %in% optin)
  split(hist, hist$player) |>
    purrr::imap(function(rows, player) {
      list(
        player = player,
        history = rows |>
          dplyr::arrange(.data$date) |>
          dplyr::transmute(
            date = as.character(.data$date),
            score_median = .data$score_median,
            score_lower = .data$score_lower,
            score_upper = .data$score_upper
          )
      )
    })
}

build_head_to_head <- function(results_dir, optin) {
  pd <- readRDS(file.path(results_dir, "processed_data.rds"))
  pd |>
    dplyr::filter(.data$player1 %in% optin, .data$player2 %in% optin) |>
    dplyr::mutate(
      a = pmin(.data$player1, .data$player2),
      b = pmax(.data$player1, .data$player2),
      a_won = as.integer(.data$winner == .data$a)
    ) |>
    dplyr::summarise(
      a_wins = sum(.data$a_won),
      b_wins = sum(1L - .data$a_won),
      .by = c(a, b)
    ) |>
    purrr::pmap(function(a, b, a_wins, b_wins) {
      list(player_a = a, player_b = b, a_wins = a_wins, b_wins = b_wins)
    })
}

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

  write_json_file(rankings, file.path(PUBLISH_DIR, "rankings.json"))
  write_json_file(build_meta(results_dir, rankings), file.path(PUBLISH_DIR, "meta.json"))
  write_json_file(build_head_to_head(results_dir, optin), file.path(PUBLISH_DIR, "head_to_head.json"))
  write_json_file(build_cubes(results_dir), file.path(PUBLISH_DIR, "cubes.json"))
  write_json_file(build_calendar(results_dir), file.path(PUBLISH_DIR, "calendar.json"))

  players <- build_players(optin)
  for (p in players) write_json_file(p, file.path(PUBLISH_DIR, "players", paste0(slugify(p$player), ".json")))

  cat("published", nrow(rankings), "players,", length(players), "profiles from", basename(results_dir), "\n")
}

if (sys.nframe() == 0) main()
