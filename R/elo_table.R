library(tidyverse)
library(lubridate)
library(posterior)
library(cmdstanr)
library(gt)
library(gtExtras)
library(scales)
library(glue)
library(googlesheets4)
library(reactable)
library(htmltools)

.default_results_root <- "results"
.default_fallback_dir <- "data"
.default_sheet_url <- "https://docs.google.com/spreadsheets/d/1bq5DXQs1nobk0nu9cN-4UOHPkcPK3fvkTLa2t2lVNKk/edit?gid=636250879#gid=636250879"

#' Fetch table players from Google Sheets
#' @param sheet_url URL to the Google Sheets document
#' @return Character vector of player nicknames eligible for ELO table
fetch_table_players <- function(sheet_url = .default_sheet_url) {
  read_sheet(sheet_url, sheet = "Players") |>
    janitor::clean_names() |>
    filter(elo_table == TRUE) |>
    pull(nickname)
}

#' Fetch newest game date from Google Sheets
#' @param sheet_url URL to the Google Sheets document
#' @return Date of most recent game
fetch_newest_date <- function(sheet_url = .default_sheet_url) {
  read_sheet(sheet_url, sheet = 1) |>
    pull(date) |>
    max() |>
    as_date()
}

list_results_dirs <- function(results_root = .default_results_root) {
  if (!dir.exists(results_root)) {
    return(character())
  }

  candidates <- dir(results_root, full.names = FALSE, recursive = FALSE)
  parsed_dates <- suppressWarnings(as.Date(candidates))
  valid <- !is.na(parsed_dates)

  if (!any(valid)) {
    return(character())
  }

  ordered <- order(parsed_dates[valid], decreasing = TRUE)
  file.path(results_root, candidates[valid][ordered])
}

#' Load results from a temporal model fit
#'
#' For the temporal model, we only need the most recent fit (rank=1)
#' since the model contains the full time series internally.
#' The 'previous' comparison uses alpha at T-1 from the same model.
load_results_set <- function(
    rank = 1,
    results_root = .default_results_root,
    fallback_dir = .default_fallback_dir) {
  dirs <- list_results_dirs(results_root)

  if (length(dirs) >= rank) {
    results_dir <- dirs[rank]
    files_present <- all(file.exists(file.path(
      results_dir,
      c("fitted_model.rds", "players.rds", "processed_data.rds")
    )))

    if (files_present) {
      # Check if game_dates exists (temporal model)
      game_dates_path <- file.path(results_dir, "game_dates.rds")
      game_dates <- if (file.exists(game_dates_path)) {
        readRDS(game_dates_path)
      } else {
        NULL
      }

      return(list(
        fit = readRDS(file.path(results_dir, "fitted_model.rds")),
        players = readRDS(file.path(results_dir, "players.rds")),
        processed_data = readRDS(file.path(results_dir, "processed_data.rds")),
        game_dates = game_dates,
        source_dir = results_dir
      ))
    }
  }

  if (rank > 1) {
    return(NULL)
  }

  fallback_files <- file.path(
    fallback_dir,
    c(
      "fitted_model.rds",
      "players.rds",
      "processed_data.rds"
    )
  )

  if (!all(file.exists(fallback_files))) {
    stop(
      "No fitted model outputs found in results/ or data/. ",
      "Run run_analysis.R or run_historical_analysis.R first."
    )
  }

  game_dates_path <- file.path(fallback_dir, "game_dates.rds")
  game_dates <- if (file.exists(game_dates_path)) {
    readRDS(game_dates_path)
  } else {
    NULL
  }

  list(
    fit = readRDS(fallback_files[1]),
    players = readRDS(fallback_files[2]),
    processed_data = readRDS(fallback_files[3]),
    game_dates = game_dates,
    source_dir = fallback_dir
  )
}

#' Compute player estimates from temporal model on ELO scale
#'
#' For temporal models, extracts alpha_current (strength at most recent time).
#' For static models (legacy), extracts alpha directly.
#' Converts Bradley-Terry parameters to ELO scale via bt_to_elo().
compute_player_estimates <- function(fit, players, time_idx = NULL) {
  # Check if this is a temporal model by looking for alpha_current
  param_names <- fit$metadata()$stan_variables

  if ("alpha_current" %in% param_names) {
    # Temporal model: use alpha_current for latest, or alpha[time_idx,] for specific time
    if (is.null(time_idx)) {
      draws <- fit$draws("alpha_current", format = "draws_df") |>
        as_tibble() |>
        select(starts_with("alpha_current"), .draw)
    } else {
      # Extract alpha at specific time point
      draws <- fit$draws("alpha", format = "draws_df") |>
        as_tibble() |>
        select(matches(glue("^alpha\\[{time_idx},")), .draw)

      # Rename to match expected format
      names(draws) <- gsub(
        glue("alpha\\[{time_idx},([0-9]+)\\]"),
        "alpha_current[\\1]",
        names(draws)
      )
    }

    # Convert to ELO scale
    draws |>
      pivot_longer(
        cols = starts_with("alpha_current"),
        names_to = "param",
        values_to = "alpha"
      ) |>
      mutate(
        name = parse_number(param),
        elo = bt_to_elo(alpha)
      ) |>
      inner_join(players, by = join_by(name == player_nr)) |>
      summarise(
        median = round(median(elo)),
        lower = round(quantile(elo, 0.05)),
        upper = round(quantile(elo, 0.95)),
        .by = c(player, wins, losses, total, text)
      )
  } else {
    # Legacy static model
    fit$draws("alpha") |>
      as_draws_df() |>
      as_tibble() |>
      pivot_longer(
        cols = c(-starts_with(".")),
        names_to = "param",
        values_to = "alpha"
      ) |>
      mutate(
        name = parse_number(param),
        elo = bt_to_elo(alpha)
      ) |>
      inner_join(players, by = join_by(name == player_nr)) |>
      summarise(
        median = round(median(elo)),
        lower = round(quantile(elo, 0.05)),
        upper = round(quantile(elo, 0.95)),
        .by = c(player, wins, losses, total, text)
      )
  }
}

#' Compute player estimates at a specific time point
compute_player_estimates_at_time <- function(fit, players, time_idx) {
  compute_player_estimates(fit, players, time_idx = time_idx)
}

#' Compute cube-specific player estimates on ELO scale
#'
#' For a given cube type, combines alpha_current (base strength) with gamma[k,c]
#' (cube-specific effect) to produce per-player ELO estimates.
#' Returns the same shape as compute_player_estimates() so downstream code works
#' identically.
#'
#' @param fit A cmdstanr fit object from the temporal model
#' @param players Data frame with player info (player_nr, player, wins, losses, total)
#' @param cube_nr Integer cube index (alphabetical: High=1, Low=2, Medium=3, Other=4)
#' @param processed_data Processed game data (used to compute per-cube win/loss)
#' @param cube_name Character name of the cube type (e.g., "High")
#' @return Data frame with player, wins, losses, total, text, median, lower, upper
compute_cube_estimates <- function(fit, players, cube_nr, processed_data, cube_name) {
  # Extract alpha_current draws (base strength)
  alpha_draws <- fit$draws("alpha_current", format = "draws_df") |>
    as_tibble() |>
    select(starts_with("alpha_current"), .draw) |>
    pivot_longer(
      cols = starts_with("alpha_current"),
      names_to = "param",
      values_to = "alpha"
    ) |>
    mutate(player_nr = parse_number(param))

  # Extract gamma draws for this specific cube type
  # gamma[k, c] — we want column c = cube_nr for all players k
  gamma_draws <- fit$draws("gamma", format = "draws_df") |>
    as_tibble() |>
    select(matches(paste0("gamma\\[\\d+,", cube_nr, "\\]")), .draw) |>
    pivot_longer(
      cols = starts_with("gamma"),
      names_to = "param",
      values_to = "gamma"
    ) |>
    mutate(player_nr = as.integer(str_extract(param, "(?<=\\[)\\d+")))

  # Combine alpha + gamma per draw, convert to ELO
  combined <- alpha_draws |>
    inner_join(gamma_draws, by = c(".draw", "player_nr")) |>
    mutate(elo = bt_to_elo(alpha + gamma))

  # Compute per-cube win/loss/total from processed_data
  cube_stats <- processed_data |>
    filter(cube == cube_name) |>
    select(player1, player2, winner) |>
    pivot_longer(c(player1, player2), names_to = NULL, values_to = "player") |>
    mutate(win = as.integer(winner == player)) |>
    summarise(
      wins = sum(win),
      losses = sum(1 - win),
      total = wins + losses,
      .by = player
    ) |>
    mutate(text = glue::glue(
      "{scales::percent(wins / total, accuracy = 1)} ({wins}/{total})"
    ))

  # Summarise ELO draws and join with cube-specific stats
  combined |>
    inner_join(players |> select(player_nr, player), by = "player_nr") |>
    summarise(
      median = round(median(elo)),
      lower = round(quantile(elo, 0.05)),
      upper = round(quantile(elo, 0.95)),
      .by = c(player, player_nr)
    ) |>
    inner_join(cube_stats, by = "player") |>
    select(player, wins, losses, total, text, median, lower, upper)
}

bt_to_elo <- function(x) {
  (400 / log(10)) * x + 1500
}

#' Convert Bradley-Terry parameters to ELO scale
#'
#' For each MCMC draw, applies bt_to_elo() to convert raw alpha values
#' to the standard ELO scale (centered at 1500).
#'
#' @param fit A cmdstanr fit object
#' @param param Name of the alpha parameter (default: "alpha_current")
#' @return Data frame with player_nr, score_median, score_lower, score_upper
normalize_to_score <- function(fit, param = "alpha_current") {
  draws <- fit$draws(param, format = "draws_df") |>
    as_tibble() |>
    select(starts_with(param), .draw)

  # Pivot to long format
  draws_long <- draws |>
    pivot_longer(
      cols = starts_with(param),
      names_to = "param_name",
      values_to = "alpha"
    ) |>
    mutate(
      player_nr = parse_number(param_name),
      elo = bt_to_elo(alpha)
    )

  # Summarize across draws (rounded to integers)
  draws_long |>
    summarise(
      score_median = round(median(elo)),
      score_lower = round(quantile(elo, 0.05)),
      score_upper = round(quantile(elo, 0.95)),
      .by = player_nr
    )
}

#' Extract and save player summary CSV from fitted temporal model
#'
#' Creates a CSV with ELO-scale medians/intervals and cube effects for each player.
#' Bradley-Terry parameters are converted to ELO via bt_to_elo().
#' Saves to the specified results directory.
#'
#' @param fit A cmdstanr fit object from the temporal model
#' @param players Data frame with player info (player_nr, player, wins, losses, total)
#' @param cube_types Data frame mapping cube names to cube_nr
#' @param output_dir Directory to save the CSV
#' @param filename Name of the CSV file (default: "player_summary.csv")
#' @return Invisibly returns the summary data frame
save_player_summary_csv <- function(
    fit,
    players,
    cube_types,
    output_dir,
    processed_data = NULL,
    filename = "player_summary.csv") {
  # Extract alpha_current draws (keep .draw for per-cube ELO computation)
  alpha_draws_raw <- fit$draws("alpha_current", format = "draws_df") |>
    as_tibble() |>
    select(starts_with("alpha_current"), .draw) |>
    pivot_longer(
      cols = starts_with("alpha_current"),
      names_to = "param",
      values_to = "alpha"
    ) |>
    mutate(player_nr = parse_number(param))

  # Summarise overall ELO
  alpha_summary <- alpha_draws_raw |>
    mutate(elo = bt_to_elo(alpha)) |>
    summarise(
      score_median = round(median(elo)),
      score_q25 = round(quantile(elo, 0.25)),
      score_q75 = round(quantile(elo, 0.75)),
      score_lower = round(quantile(elo, 0.05)),
      score_upper = round(quantile(elo, 0.95)),
      .by = player_nr
    )

  # Check if this fit has cube effects (gamma parameter)
  has_gamma <- "gamma" %in% fit$metadata()$stan_variables

  gamma_summary <- NULL
  per_cube_elo <- NULL
  per_cube_stats <- NULL

  if (has_gamma) {
    # Extract gamma draws for all players and cube types
    gamma_draws_raw <- fit$draws("gamma", format = "draws_df") |>
      as_tibble() |>
      pivot_longer(
        cols = starts_with("gamma"),
        names_to = "param",
        values_to = "gamma"
      ) |>
      mutate(
        indices = str_extract(param, "\\[\\d+,\\d+\\]"),
        player_nr = as.integer(str_extract(indices, "(?<=\\[)\\d+")),
        cube_nr = as.integer(str_extract(indices, "\\d+(?=\\])"))
      )

    # Gamma medians (backward-compatible columns)
    gamma_summary <- gamma_draws_raw |>
      summarise(
        gamma_median = median(gamma),
        .by = c(player_nr, cube_nr)
      ) |>
      left_join(cube_types, by = "cube_nr") |>
      select(player_nr, cube, gamma_median) |>
      pivot_wider(
        names_from = cube,
        values_from = gamma_median,
        names_prefix = "gamma_"
      )

    # Per-cube ELO: alpha + gamma per draw, then summarise
    per_cube_elo <- alpha_draws_raw |>
      inner_join(gamma_draws_raw |> select(.draw, player_nr, cube_nr, gamma),
        by = c(".draw", "player_nr")
      ) |>
      mutate(elo = bt_to_elo(alpha + gamma)) |>
      left_join(cube_types, by = "cube_nr") |>
      summarise(
        elo_median = round(median(elo)),
        elo_q25 = round(quantile(elo, 0.25)),
        elo_q75 = round(quantile(elo, 0.75)),
        elo_lower = round(quantile(elo, 0.05)),
        elo_upper = round(quantile(elo, 0.95)),
        .by = c(player_nr, cube)
      ) |>
      pivot_wider(
        names_from = cube,
        values_from = c(elo_median, elo_q25, elo_q75, elo_lower, elo_upper),
        names_glue = "{cube}_{.value}"
      )

    # Per-cube win/loss/total from processed_data
    if (!is.null(processed_data)) {
      per_cube_stats <- processed_data |>
        select(cube, player1, player2, winner) |>
        pivot_longer(c(player1, player2), names_to = NULL, values_to = "player") |>
        mutate(win = as.integer(winner == player)) |>
        summarise(
          wins = sum(win),
          losses = sum(1 - win),
          total = wins + losses,
          .by = c(player, cube)
        ) |>
        # Map player names back to player_nr
        inner_join(players |> select(player_nr, player), by = "player") |>
        select(-player) |>
        pivot_wider(
          names_from = cube,
          values_from = c(wins, losses, total),
          names_glue = "{cube}_{.value}"
        )
    }
  }

  # Combine everything
  summary_df <- players |>
    select(player_nr, player, wins, losses, total) |>
    left_join(alpha_summary, by = "player_nr")

  if (!is.null(gamma_summary)) {
    summary_df <- summary_df |> left_join(gamma_summary, by = "player_nr")
  }
  if (!is.null(per_cube_elo)) {
    summary_df <- summary_df |> left_join(per_cube_elo, by = "player_nr")
  }

  if (!is.null(per_cube_stats)) {
    summary_df <- summary_df |>
      left_join(per_cube_stats, by = "player_nr")
  }

  summary_df <- summary_df |>
    arrange(desc(score_median))

  # Save CSV
  output_path <- file.path(output_dir, filename)
  write_csv(summary_df, output_path)
  cat("  Saved player summary to:", output_path, "\n")

  invisible(summary_df)
}

render_score_table <- function(
    top_n = 10,
    results_root = .default_results_root,
    fallback_dir = .default_fallback_dir,
    min_total_games = 0,
    min_winrate = 0.15,
    max_absence_weeks = 8,
    table_players = NULL,
    newest_date = NULL) {
  # Load latest and previous date's models
  # Always compare to previous date's model (not T-1 from same model)
  # because temporal models can shift historical estimates with new data
  latest <- load_results_set(
    rank = 1,
    results_root = results_root,
    fallback_dir = fallback_dir
  )

  previous <- load_results_set(
    rank = 2,
    results_root = results_root,
    fallback_dir = fallback_dir
  )

  # Compute last game date for each player from processed_data
  last_game_dates <- bind_rows(
    latest$processed_data |> select(player = player1, date),
    latest$processed_data |> select(player = player2, date)
  ) |>
    mutate(player = str_to_title(player)) |>
    summarise(last_game = max(date), .by = player)

  # Reference date for absence calculation
  reference_date <- max(latest$processed_data$date)
  absence_cutoff <- reference_date - weeks(max_absence_weeks)

  # Current estimates from latest model (ELO scale)
  estimates <- compute_player_estimates(
    fit = latest$fit,
    players = latest$players
  ) |>
    rename(score = median) |>
    # Join last game dates
    left_join(last_game_dates, by = "player") |>
    mutate(player = fct_reorder(player, score)) |>
    mutate(hlutf = wins / total) |>
    arrange(desc(score)) |>
    filter(
      total >= min_total_games,
      hlutf >= min_winrate,
      is.null(table_players) | str_to_lower(player) %in% table_players,
      # Filter out players absent for too long
      is.null(max_absence_weeks) | last_game >= absence_cutoff
    ) |>
    select(-last_game) |>
    mutate(
      nr = row_number(),
      deild = case_when(
        nr <= 10 ~ "Deild 1",
        nr <= 20 ~ "Deild 2",
        TRUE ~ "Deild 3"
      )
    ) |>
    select(-lower, -upper)

  # Previous estimates from previous date's model (ELO scale)
  if (!is.null(previous)) {
    # Compute last game dates for previous model's data
    prev_last_game_dates <- bind_rows(
      previous$processed_data |> select(player = player1, date),
      previous$processed_data |> select(player = player2, date)
    ) |>
      mutate(player = str_to_title(player)) |>
      summarise(last_game = max(date), .by = player)

    prev_reference_date <- max(previous$processed_data$date)
    prev_absence_cutoff <- prev_reference_date - weeks(max_absence_weeks)

    previous_estimates <- compute_player_estimates(
      fit = previous$fit,
      players = previous$players
    ) |>
      rename(score = median) |>
      left_join(prev_last_game_dates, by = "player") |>
      mutate(hlutf = wins / total) |>
      arrange(desc(score)) |>
      filter(
        total >= min_total_games,
        hlutf >= min_winrate,
        is.null(table_players) | str_to_lower(player) %in% table_players,
        is.null(max_absence_weeks) | last_game >= prev_absence_cutoff
      ) |>
      mutate(nr_prev = row_number()) |>
      select(
        player,
        score_prev = score,
        nr_prev
      )

    estimates <- estimates |>
      left_join(previous_estimates, by = "player")
  } else {
    estimates <- estimates |>
      mutate(
        score_prev = NA_real_,
        nr_prev = NA_integer_
      )
  }

  estimates <- estimates |>
    mutate(
      # Round scores to integers first
      score = round(score),
      score_prev = round(score_prev),
      # Then compute deltas from rounded values
      rank_delta = nr_prev - nr,
      score_delta = score - score_prev,
      rank_delta_label = case_when(
        is.na(rank_delta) ~ "",
        rank_delta > 0 ~ paste0("+", rank_delta),
        rank_delta < 0 ~ as.character(rank_delta),
        TRUE ~ "0"
      ),
      score_delta_label = case_when(
        is.na(score_delta) ~ "",
        score_delta > 0 ~ paste0("+", score_delta),
        score_delta < 0 ~ as.character(score_delta),
        TRUE ~ "0"
      )
    )

  if (!is.null(top_n)) {
    estimates <- estimates |>
      filter(nr <= top_n)
  }

  table <- estimates |>
    gt() |>
    cols_hide(c(total, text, deild, rank_delta, score_delta)) |>
    cols_move(hlutf, after = losses) |>
    cols_move(score_prev, after = score) |>
    cols_move(score_delta_label, after = score_prev) |>
    cols_move(nr, after = score_delta_label) |>
    cols_move(nr_prev, after = nr) |>
    cols_move(rank_delta_label, after = nr_prev) |>
    cols_label(
      player = "Leikmaður",
      wins = "S",
      losses = "T",
      hlutf = "%",
      score = "ELO",
      score_prev = "Fyrra",
      score_delta_label = "±",
      nr = "#",
      nr_prev = "Fyrra",
      rank_delta_label = "±"
    ) |>
    cols_align(columns = player, align = "left") |>
    cols_align(columns = -player, align = "center") |>
    cols_merge(columns = c(player, nr), pattern = "{2}. {1}") |>
    cols_width(player ~ px(180)) |>
    fmt_percent(hlutf, decimals = 0) |>
    fmt_number(c(score, score_prev), decimals = 0) |>
    sub_missing(missing_text = "-") |>
    # Clean minimal styling
    tab_header(
      title = if (!is.null(newest_date)) glue("Uppfært {newest_date}") else NULL
    ) |>
    tab_spanner(
      label = "ELO",
      columns = c(score, score_prev, score_delta_label)
    ) |>
    tab_spanner(label = "Sæti", columns = c(nr_prev, rank_delta_label)) |>
    # Style positive/negative changes - muted green/red
    tab_style(
      locations = cells_body(
        columns = score_delta_label,
        rows = str_detect(score_delta_label, "^\\+")
      ),
      style = cell_text(color = "#00733e")
    ) |>
    tab_style(
      locations = cells_body(
        columns = score_delta_label,
        rows = str_detect(score_delta_label, "^\\-")
      ),
      style = cell_text(color = "#d3202a")
    ) |>
    tab_style(
      locations = cells_body(
        columns = rank_delta_label,
        rows = str_detect(rank_delta_label, "^\\+")
      ),
      style = cell_text(color = "#00733e")
    ) |>
    tab_style(
      locations = cells_body(
        columns = rank_delta_label,
        rows = str_detect(rank_delta_label, "^\\-")
      ),
      style = cell_text(color = "#d3202a")
    ) |>
    # Greyscale table options
    tab_options(
      table.font.size = px(18),
      table.font.color = "#2a1f14",
      heading.title.font.size = px(13),
      heading.title.font.weight = "normal",
      heading.align = "left",
      column_labels.font.weight = "bold",
      column_labels.font.size = px(13),
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "#0e68ab",
      table_body.hlines.color = "#e8e3de",
      table.border.top.style = "hidden",
      table.border.bottom.style = "hidden",
      data_row.padding = px(8)
    )

  table
}

# Backward-compatible alias
render_elo_table <- render_score_table

#' Prepare ranking data for rendering
#'
#' Shared data preparation logic for both overall and per-cube rankings.
#' Loads models, computes estimates, applies filters, and computes deltas.
#'
#' @param results_root Root directory for results
#' @param fallback_dir Fallback directory
#' @param min_total_games Minimum games to appear in table
#' @param min_winrate Minimum win rate to appear in table
#' @param max_absence_weeks Maximum weeks since last game
#' @param table_players Optional character vector of eligible players
#' @param cube_type Optional cube type name (e.g., "High", "Medium", "Low", "Other")
#' @param top_n Maximum number of rows to return
#' @return Data frame with estimates, deltas, and rankings
prepare_ranking_data <- function(
    results_root = .default_results_root,
    fallback_dir = .default_fallback_dir,
    min_total_games = 0,
    min_winrate = 0.15,
    max_absence_weeks = 8,
    table_players = NULL,
    cube_type = NULL,
    top_n = Inf) {
  latest <- load_results_set(
    rank = 1, results_root = results_root, fallback_dir = fallback_dir
  )
  previous <- load_results_set(
    rank = 2, results_root = results_root, fallback_dir = fallback_dir
  )

  # Compute last game dates — always use overall data (not cube-specific)
  # because cube categories rotate infrequently and the absence filter
  # answers "is this player still active?" not "did they play this cube recently?"
  last_game_dates <- bind_rows(
    latest$processed_data |> select(player = player1, date),
    latest$processed_data |> select(player = player2, date)
  ) |>
    mutate(player = str_to_title(player)) |>
    summarise(last_game = max(date), .by = player)

  reference_date <- max(latest$processed_data$date)
  absence_cutoff <- reference_date - weeks(max_absence_weeks)

  # Choose estimate function based on cube_type
  if (!is.null(cube_type)) {
    # Resolve cube_nr from processed_data
    cube_nr <- latest$processed_data |>
      filter(cube == cube_type) |>
      pull(cube_nr) |>
      unique()
    if (length(cube_nr) == 0) {
      stop("Cube type '", cube_type, "' not found in processed_data")
    }
    cube_nr <- cube_nr[1]

    estimates <- compute_cube_estimates(
      fit = latest$fit,
      players = latest$players,
      cube_nr = cube_nr,
      processed_data = latest$processed_data,
      cube_name = cube_type
    )
  } else {
    estimates <- compute_player_estimates(
      fit = latest$fit, players = latest$players
    )
  }

  estimates <- estimates |>
    rename(score = median) |>
    left_join(last_game_dates, by = "player") |>
    mutate(player = fct_reorder(player, score)) |>
    mutate(hlutf = wins / total) |>
    arrange(desc(score)) |>
    filter(
      total >= min_total_games,
      hlutf >= min_winrate,
      is.null(table_players) | str_to_lower(player) %in% table_players,
      is.null(max_absence_weeks) | last_game >= absence_cutoff
    ) |>
    select(-last_game) |>
    mutate(nr = row_number()) |>
    select(-lower, -upper)

  # Compute previous estimates for deltas
  if (!is.null(previous)) {
    prev_last_game_dates <- bind_rows(
      previous$processed_data |> select(player = player1, date),
      previous$processed_data |> select(player = player2, date)
    ) |>
      mutate(player = str_to_title(player)) |>
      summarise(last_game = max(date), .by = player)

    prev_reference_date <- max(previous$processed_data$date)
    prev_absence_cutoff <- prev_reference_date - weeks(max_absence_weeks)

    if (!is.null(cube_type)) {
      prev_cube_nr <- previous$processed_data |>
        filter(cube == cube_type) |>
        pull(cube_nr) |>
        unique()
      if (length(prev_cube_nr) > 0) {
        previous_estimates <- compute_cube_estimates(
          fit = previous$fit,
          players = previous$players,
          cube_nr = prev_cube_nr[1],
          processed_data = previous$processed_data,
          cube_name = cube_type
        )
      } else {
        previous_estimates <- NULL
      }
    } else {
      previous_estimates <- compute_player_estimates(
        fit = previous$fit, players = previous$players
      )
    }

    if (!is.null(previous_estimates)) {
      previous_estimates <- previous_estimates |>
        rename(score = median) |>
        left_join(prev_last_game_dates, by = "player") |>
        mutate(hlutf = wins / total) |>
        arrange(desc(score)) |>
        filter(
          total >= min_total_games,
          hlutf >= min_winrate,
          is.null(table_players) | str_to_lower(player) %in% table_players,
          is.null(max_absence_weeks) | last_game >= prev_absence_cutoff
        ) |>
        mutate(nr_prev = row_number()) |>
        select(player, score_prev = score, nr_prev)

      estimates <- estimates |>
        left_join(previous_estimates, by = "player")
    } else {
      estimates <- estimates |>
        mutate(score_prev = NA_real_, nr_prev = NA_integer_)
    }
  } else {
    estimates <- estimates |>
      mutate(score_prev = NA_real_, nr_prev = NA_integer_)
  }

  estimates <- estimates |>
    mutate(
      score = round(score),
      score_prev = round(score_prev),
      rank_delta = nr_prev - nr,
      score_delta = score - score_prev
    )

  if (!is.null(top_n) && is.finite(top_n)) {
    estimates <- estimates |> filter(nr <= top_n)
  }

  estimates
}

#' Render an interactive score table using reactable
#'
#' Reuses the same data preparation as render_score_table() but outputs a
#' searchable, sortable reactable widget instead of a static gt table.
render_score_table_interactive <- function(
    top_n = 10,
    results_root = .default_results_root,
    fallback_dir = .default_fallback_dir,
    min_total_games = 0,
    min_winrate = 0.15,
    max_absence_weeks = 8,
    table_players = NULL,
    newest_date = NULL,
    labels = ranking_table_labels("is")) {
  estimates <- prepare_ranking_data(
    results_root = results_root,
    fallback_dir = fallback_dir,
    min_total_games = min_total_games,
    min_winrate = min_winrate,
    max_absence_weeks = max_absence_weeks,
    table_players = table_players,
    top_n = top_n
  )

  build_ranking_reactable(estimates, newest_date, labels)
}

#' Render a cube-specific interactive score table
#'
#' Thin wrapper around prepare_ranking_data() + the reactable builder from
#' render_score_table_interactive(). Uses per-cube ELO (alpha + gamma) and
#' per-cube win/loss counts. Lower default min_total_games since games are
#' split across cube categories.
#'
#' @param cube_type Cube category name: "High", "Medium", "Low", or "Other"
#' @inheritParams render_score_table_interactive
render_cube_score_table_interactive <- function(
    cube_type,
    top_n = Inf,
    results_root = .default_results_root,
    fallback_dir = .default_fallback_dir,
    min_total_games = 3,
    min_winrate = 0.0,
    max_absence_weeks = 8,
    table_players = NULL,
    newest_date = NULL,
    labels = ranking_table_labels("is")) {
  estimates <- prepare_ranking_data(
    results_root = results_root,
    fallback_dir = fallback_dir,
    min_total_games = min_total_games,
    min_winrate = min_winrate,
    max_absence_weeks = max_absence_weeks,
    table_players = table_players,
    cube_type = cube_type,
    top_n = top_n
  )

  # Build reactable using shared builder
  build_ranking_reactable(estimates, newest_date, labels)
}

#' Build reactable widget from prepared ranking estimates
#'
#' Shared rendering logic used by both render_score_table_interactive() and
#' render_cube_score_table_interactive().
#'
#' @param estimates Data frame from prepare_ranking_data()
#' @param newest_date Optional date string for subtitle
#' @param labels Named character vector from ranking_table_labels()
#' @return An htmltools tag or reactable widget
build_ranking_reactable <- function(
    estimates,
    newest_date = NULL,
    labels = ranking_table_labels("is")) {
  # --- Prepare display data ---
  display_df <- estimates |>
    mutate(
      hlutf_pct = round(hlutf * 100),
      player_display = paste0(nr, ". ", player)
    ) |>
    select(
      player_display, wins, losses, hlutf_pct,
      score, score_prev, score_delta,
      nr_prev, rank_delta
    )

  # Delta cell renderer: colored pill badge with arrow
  render_delta_cell <- function(value) {
    if (is.na(value)) {
      return(span(style = "color: #4a4640;", "\u2013"))
    }
    if (value == 0) {
      return(span(style = "color: #4a4640; font-size: 12px;", "\u2014"))
    }

    if (value > 0) {
      arrow <- "\u25B2"
      color <- "#2ecc71"
      bg <- "rgba(46, 204, 113, 0.10)"
      label <- paste0("+", value)
    } else {
      arrow <- "\u25BC"
      color <- "#e74c3c"
      bg <- "rgba(231, 76, 60, 0.10)"
      label <- as.character(value)
    }

    span(
      style = paste0(
        "display: inline-flex; align-items: center; gap: 2px; ",
        "font-size: 12px; font-weight: 600; ",
        "color: ", color, "; ",
        "background: ", bg, "; ",
        "padding: 2px 6px; border-radius: 10px; ",
        "line-height: 1;"
      ),
      span(style = "font-size: 9px;", arrow),
      label
    )
  }

  # --- Build reactable ---
  tbl <- reactable(
    display_df,
    searchable = FALSE,
    language = reactableLang(searchPlaceholder = labels[["search_placeholder"]]),
    defaultSorted = list(),
    defaultPageSize = nrow(display_df),
    showPagination = FALSE,
    compact = TRUE,
    borderless = TRUE,
    striped = TRUE,
    class = "score-table",
    rowStyle = function(index) {
      if (index == 1) {
        list(background = "linear-gradient(90deg, rgba(201,169,38,0.10) 0%, rgba(201,169,38,0.02) 100%)")
      } else if (index == 2) {
        list(background = "linear-gradient(90deg, rgba(138,138,138,0.08) 0%, rgba(138,138,138,0.02) 100%)")
      } else if (index == 3) {
        list(background = "linear-gradient(90deg, rgba(160,101,42,0.08) 0%, rgba(160,101,42,0.02) 100%)")
      } else {
        list()
      }
    },
    theme = reactableTheme(
      backgroundColor = "transparent",
      highlightColor = "rgba(59, 158, 221, 0.08)",
      cellPadding = "10px 8px",
      headerStyle = list(
        fontWeight = "bold",
        fontSize = "12px",
        textTransform = "uppercase",
        letterSpacing = "0.03em",
        textAlign = "center",
        padding = "8px 8px 10px"
      )
    ),
    columns = list(
      player_display = colDef(
        name = labels[["player"]],
        align = "left",
        headerStyle = list(textAlign = "left"),
        minWidth = 175,
        style = list(fontWeight = 500, textAlign = "left")
      ),
      wins = colDef(
        name = labels[["wins"]],
        width = 55,
        style = list(
          color = "#2ecc71",
          fontWeight = 600,
          fontVariantNumeric = "tabular-nums"
        )
      ),
      losses = colDef(
        name = labels[["losses"]],
        width = 55,
        style = list(
          color = "#e74c3c",
          fontWeight = 600,
          fontVariantNumeric = "tabular-nums"
        )
      ),
      hlutf_pct = colDef(
        name = labels[["win_rate_short"]],
        minWidth = 110,
        align = "center",
        cell = function(value) {
          width <- paste0(value, "%")
          t <- min(1, max(0, (value - 35) / 30))
          r_col <- round(231 - t * (231 - 46))
          g_col <- round(76 + t * (204 - 76))
          b_col <- round(60 + t * (113 - 60))
          bar_color <- sprintf("rgb(%d, %d, %d)", r_col, g_col, b_col)
          div(
            style = "display: flex; align-items: center; gap: 6px;",
            div(
              style = "flex: 1; height: 6px; background: rgba(255,255,255,0.06); border-radius: 3px; overflow: hidden;",
              div(style = paste0(
                "height: 100%; width: ", width, "; background: ", bar_color,
                "; border-radius: 3px;"
              ))
            ),
            span(
              style = "font-size: 12px; color: #7a7670; min-width: 30px; text-align: right; font-variant-numeric: tabular-nums;",
              paste0(value, "%")
            )
          )
        }
      ),
      score = colDef(
        name = labels[["elo"]],
        width = 80,
        class = "score-elo-cell",
        style = list(
          fontWeight = 700,
          fontSize = "18px",
          fontVariantNumeric = "tabular-nums"
        )
      ),
      score_prev = colDef(
        name = labels[["previous"]],
        width = 70,
        class = "score-prev-cell",
        style = list(fontSize = "13px"),
        cell = function(value) if (is.na(value)) "\u2013" else value
      ),
      score_delta = colDef(
        name = labels[["delta"]],
        width = 70,
        cell = function(value) render_delta_cell(value)
      ),
      nr_prev = colDef(
        name = labels[["previous"]],
        width = 65,
        class = "score-prev-cell",
        style = list(fontSize = "13px"),
        cell = function(value) if (is.na(value)) "\u2013" else value
      ),
      rank_delta = colDef(
        name = labels[["delta"]],
        width = 70,
        cell = function(value) render_delta_cell(value)
      )
    ),
    columnGroups = list(
      colGroup(
        name = labels[["elo_group"]],
        columns = c("score", "score_prev", "score_delta")
      ),
      colGroup(
        name = labels[["rank_group"]],
        columns = c("nr_prev", "rank_delta")
      )
    )
  )

  if (!is.null(newest_date)) {
    htmltools::div(
      htmltools::p(
        paste(labels[["updated_prefix"]], newest_date),
        style = "color: #7a7670; font-size: 13px; margin-bottom: 8px;"
      ),
      tbl
    )
  } else {
    tbl
  }
}

ranking_table_labels <- function(language = c("is", "en")) {
  language <- match.arg(language)

  switch(
    language,
    is = c(
      search_placeholder = "Leita...",
      player = "Leikmaður",
      wins = "S",
      losses = "T",
      win_rate_short = "%",
      elo = "ELO",
      previous = "Fyrra",
      delta = "\u00b1",
      elo_group = "ELO",
      rank_group = "S\u00e6ti",
      updated_prefix = "Uppf\u00e6rt"
    ),
    en = c(
      search_placeholder = "Search...",
      player = "Player",
      wins = "W",
      losses = "L",
      win_rate_short = "%",
      elo = "ELO",
      previous = "Prev",
      delta = "\u00b1",
      elo_group = "ELO",
      rank_group = "Rank",
      updated_prefix = "Updated"
    )
  )
}

#' Backfill player summary CSVs for existing results directories
#'
#' Scans the results directory for date folders that don't have
#' player_summary.csv and generates them from the existing fitted models.
#'
#' @param results_root Root directory containing date-stamped result folders
#' @param force If TRUE, regenerate CSVs even if they already exist
#' @return Invisibly returns vector of directories that were processed
backfill_player_summaries <- function(
    results_root = .default_results_root,
    force = FALSE) {
  dirs <- list_results_dirs(results_root)

  if (length(dirs) == 0) {
    cat("No results directories found.\n")
    return(invisible(character()))
  }

  processed <- character()

  for (result_dir in dirs) {
    csv_path <- file.path(result_dir, "player_summary.csv")

    # Skip if CSV exists and not forcing

    if (!force && file.exists(csv_path)) {
      next
    }

    # Check required files exist
    required_files <- c("fitted_model.rds", "players.rds", "cube_types.rds")
    if (!all(file.exists(file.path(result_dir, required_files)))) {
      cat("Skipping", basename(result_dir), "(missing required files)\n")
      next
    }

    cat("Processing", basename(result_dir), "...\n")

    fit <- readRDS(file.path(result_dir, "fitted_model.rds"))
    players <- readRDS(file.path(result_dir, "players.rds"))
    cube_types <- readRDS(file.path(result_dir, "cube_types.rds"))

    # Load processed_data for per-cube win/loss stats
    pd_path <- file.path(result_dir, "processed_data.rds")
    pd <- if (file.exists(pd_path)) readRDS(pd_path) else NULL

    save_player_summary_csv(
      fit = fit,
      players = players,
      cube_types = cube_types,
      output_dir = result_dir,
      processed_data = pd
    )

    processed <- c(processed, result_dir)
  }

  cat("\nProcessed", length(processed), "directories.\n")
  invisible(processed)
}

#' Combine all player summary CSVs into a single historical dataset
#'
#' Reads all player_summary.csv files from results directories and
#' combines them into a single data frame with a date column.
#'
#' @param results_root Root directory containing date-stamped result folders
#' @return Data frame with all historical player summaries
combine_player_summaries <- function(results_root = .default_results_root) {
  dirs <- list_results_dirs(results_root)

  summaries <- map(dirs, function(result_dir) {
    csv_path <- file.path(result_dir, "player_summary.csv")

    if (!file.exists(csv_path)) {
      return(NULL)
    }

    # Extract date from directory name
    date_str <- basename(result_dir)

    read_csv(csv_path, show_col_types = FALSE) |>
      mutate(date = as_date(date_str), .before = 1)
  }) |>
    compact() |>
    list_rbind()

  summaries
}
