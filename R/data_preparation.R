library(tidyverse)
library(googlesheets4)
source("R/theme_colors.R")
source("R/sheet_auth.R")
source("R/cube_tier.R")
theme_set(theme_mtgkubbur())

.sheet_url <- "https://docs.google.com/spreadsheets/d/1bq5DXQs1nobk0nu9cN-4UOHPkcPK3fvkTLa2t2lVNKk/edit?usp=sharing"

download_cube_results <- function(
  sheet_url = .sheet_url,
  email = Sys.getenv("GOOGLE_MAIL")
) {
  cube_gs4_auth(email = email)
  read_sheet(sheet_url)
}

prepare_cube_data <- function(d_raw, upto_date = NULL) {
  processed_data <- d_raw |>
    mutate(date = as_date(date)) |>
    pivot_longer(c(game1:game3), names_to = "game", values_to = "winner") |>
    drop_na(winner) |>
    mutate(
      game = parse_number(game),
      result = as.integer(winner == player1)
    ) |>
    mutate_at(
      vars(winner, player1, player2),
      str_to_title
    )

  if (!is.null(upto_date)) {
    upto_date <- as_date(upto_date)
    processed_data <- processed_data |>
      filter(date <= upto_date)
  }

  players <- processed_data |>
    pivot_longer(c(player1, player2), values_to = "player") |>
    mutate(win = as.integer(winner == player)) |>
    summarise(
      wins = sum(win),
      losses = sum(1 - win),
      total = wins + losses,
      .by = player
    ) |>
    mutate(
      player_nr = row_number(),
      text = glue::glue(
        "{scales::percent(wins / total, accuracy = 1)} ({wins}/{total})"
      )
    )

  processed_data <- processed_data |>
    inner_join(
      players |>
        rename(player_nr1 = player_nr) |>
        select(player_nr1, player),
      by = join_by(player1 == player)
    ) |>
    inner_join(
      players |>
        rename(player_nr2 = player_nr) |>
        select(player_nr2, player),
      by = join_by(player2 == player)
    ) |>
    mutate(
      cube = str_to_lower(cube),
      cube = cube_tier(cube),
      cube_nr = as.numeric(as.factor(cube))
    )

  list(
    processed_data = processed_data,
    players = players,
    stan_data = list(
      N = nrow(processed_data),
      K = nrow(players),
      C = length(unique(processed_data$cube)),
      cube = processed_data$cube_nr,
      player1 = processed_data$player_nr1,
      player2 = processed_data$player_nr2,
      y = processed_data$result
    )
  )
}

#' Prepare data for time-varying Bradley-Terry model
#'
#' Extends prepare_cube_data() with temporal indexing:
#' - game_time: which time point (game night) each observation belongs to
#' - delta_t: time gaps between game nights (in weeks)
#' - played: K x T matrix indicating player attendance
#'
#' @param d_raw Raw data from Google Sheets
#' @param upto_date Optional date to filter data up to
#' @return List with processed_data, players, game_dates, and stan_data_temporal
prepare_cube_data_temporal <- function(d_raw, upto_date = NULL) {
  # Get base preparation
  base <- prepare_cube_data(d_raw, upto_date = upto_date)

  # Extract unique game dates in order
  game_dates <- base$processed_data |>
    distinct(date) |>
    arrange(date) |>
    mutate(
      time_idx = row_number(),
      # Time since previous game night (in weeks)
      # First date gets delta_t = 1 (baseline)
      days_since_prev = as.numeric(date - lag(date, default = date[1])),
      delta_t = pmax(days_since_prev / 7, 1)
    )

  # Add time index to processed data
  processed_data <- base$processed_data |>
    inner_join(
      game_dates |> select(date, time_idx),
      by = "date"
    )

  # Build attendance matrix: K players x T time points
  # played[k, t] = 1 if player k participated at time t
  attendance <- processed_data |>
    select(time_idx, player_nr1, player_nr2) |>
    pivot_longer(
      c(player_nr1, player_nr2),
      values_to = "player_nr"
    ) |>
    distinct(time_idx, player_nr) |>
    mutate(played = 1)

  # Create full K x T matrix (default 0 = absent)
  K <- nrow(base$players)
  T_dates <- nrow(game_dates)

  played_matrix <- matrix(0, nrow = K, ncol = T_dates)
  for (i in seq_len(nrow(attendance))) {
    played_matrix[attendance$player_nr[i], attendance$time_idx[i]] <- 1
  }

  # Build temporal Stan data (includes cube effects)
  stan_data_temporal <- list(
    N = nrow(processed_data),
    K = K,
    T = T_dates,
    C = length(unique(processed_data$cube_nr)),
    player1 = processed_data$player_nr1,
    player2 = processed_data$player_nr2,
    y = processed_data$result,
    game_time = processed_data$time_idx,
    cube = processed_data$cube_nr,
    delta_t = game_dates$delta_t,
    played = played_matrix
  )

  # Extract cube type mapping for visualization
  cube_types <- processed_data |>
    distinct(cube, cube_nr) |>
    arrange(cube_nr)

  list(
    processed_data = processed_data,
    players = base$players,
    game_dates = game_dates,
    attendance = attendance,
    cube_types = cube_types,
    stan_data = base$stan_data,
    stan_data_temporal = stan_data_temporal
  )
}

save_prepared_datasets <- function(datasets, output_dir = "data") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(datasets$stan_data, file.path(output_dir, "stan_data.rds"))
  saveRDS(datasets$players, file.path(output_dir, "players.rds"))
  saveRDS(datasets$processed_data, file.path(output_dir, "processed_data.rds"))

  # Save temporal data if present
  if (!is.null(datasets$stan_data_temporal)) {
    saveRDS(
      datasets$stan_data_temporal,
      file.path(output_dir, "stan_data_temporal.rds")
    )
    saveRDS(datasets$game_dates, file.path(output_dir, "game_dates.rds"))
    saveRDS(datasets$cube_types, file.path(output_dir, "cube_types.rds"))
  }
}

run_data_preparation <- function(
  output_dir = "data",
  sheet_url = .sheet_url,
  email = Sys.getenv("GOOGLE_MAIL"),
  save_outputs = TRUE
) {
  d_raw <- download_cube_results(sheet_url = sheet_url, email = email)
  datasets <- prepare_cube_data(d_raw)

  if (save_outputs) {
    save_prepared_datasets(datasets, output_dir = output_dir)
    cat("Data preparation complete. Saved:\n")
    cat(file.path(output_dir, "stan_data.rds"), "\n", sep = "")
    cat(file.path(output_dir, "players.rds"), "\n", sep = "")
    cat(file.path(output_dir, "processed_data.rds"), "\n", sep = "")
  }

  datasets
}
