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
    filename = "player_summary.csv") {
  # Extract alpha_current and convert to ELO scale
  alpha_draws <- fit$draws("alpha_current", format = "draws_df") |>
    as_tibble() |>
    select(starts_with("alpha_current"), .draw) |>
    pivot_longer(
      cols = starts_with("alpha_current"),
      names_to = "param",
      values_to = "alpha"
    ) |>
    mutate(
      player_nr = parse_number(param),
      elo = bt_to_elo(alpha)
    ) |>
    summarise(
      score_median = round(median(elo)),
      score_q25 = round(quantile(elo, 0.25)),
      score_q75 = round(quantile(elo, 0.75)),
      score_lower = round(quantile(elo, 0.05)),
      score_upper = round(quantile(elo, 0.95)),
      .by = player_nr
    )

  # Extract gamma (cube effects) for each player and cube type
  # gamma[k, c] is player k's effect for cube type c
  gamma_draws <- fit$draws("gamma", format = "draws_df") |>
    as_tibble() |>
    pivot_longer(
      cols = starts_with("gamma"),
      names_to = "param",
      values_to = "gamma"
    ) |>
    mutate(
      # Parse gamma[k,c] pattern
      indices = str_extract(param, "\\[\\d+,\\d+\\]"),
      player_nr = as.integer(str_extract(indices, "(?<=\\[)\\d+")),
      cube_nr = as.integer(str_extract(indices, "\\d+(?=\\])"))
    ) |>
    summarise(
      gamma_median = median(gamma),
      .by = c(player_nr, cube_nr)
    ) |>
    # Join cube type names

    left_join(cube_types, by = "cube_nr") |>
    select(player_nr, cube, gamma_median) |>
    # Pivot wider so each cube type is a column
    pivot_wider(
      names_from = cube,
      values_from = gamma_median,
      names_prefix = "gamma_"
    )

  # Combine everything
  summary_df <- players |>
    select(player_nr, player, wins, losses, total) |>
    left_join(alpha_draws, by = "player_nr") |>
    left_join(gamma_draws, by = "player_nr") |>
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
      nr_prev = "Fyrri",
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
    newest_date = NULL) {
  # --- Reuse same data preparation as render_score_table() ---
  latest <- load_results_set(
    rank = 1, results_root = results_root, fallback_dir = fallback_dir
  )
  previous <- load_results_set(
    rank = 2, results_root = results_root, fallback_dir = fallback_dir
  )

  last_game_dates <- bind_rows(
    latest$processed_data |> select(player = player1, date),
    latest$processed_data |> select(player = player2, date)
  ) |>
    mutate(player = str_to_title(player)) |>
    summarise(last_game = max(date), .by = player)

  reference_date <- max(latest$processed_data$date)
  absence_cutoff <- reference_date - weeks(max_absence_weeks)

  estimates <- compute_player_estimates(
    fit = latest$fit, players = latest$players
  ) |>
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

  if (!is.null(previous)) {
    prev_last_game_dates <- bind_rows(
      previous$processed_data |> select(player = player1, date),
      previous$processed_data |> select(player = player2, date)
    ) |>
      mutate(player = str_to_title(player)) |>
      summarise(last_game = max(date), .by = player)

    prev_reference_date <- max(previous$processed_data$date)
    prev_absence_cutoff <- prev_reference_date - weeks(max_absence_weeks)

    previous_estimates <- compute_player_estimates(
      fit = previous$fit, players = previous$players
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
      select(player, score_prev = score, nr_prev)

    estimates <- estimates |>
      left_join(previous_estimates, by = "player")
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
      return(span(style = "color: #b0a99f;", "\u2013"))
    }
    if (value == 0) {
      return(span(style = "color: #b0a99f; font-size: 12px;", "\u2014"))
    }

    if (value > 0) {
      arrow <- "\u25B2"
      color <- "#00733e"
      bg <- "rgba(0, 115, 62, 0.08)"
      label <- paste0("+", value)
    } else {
      arrow <- "\u25BC"
      color <- "#d3202a"
      bg <- "rgba(211, 32, 42, 0.08)"
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
    language = reactableLang(searchPlaceholder = "Leita..."),
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
      color = "#2a1f14",
      stripedColor = "rgba(42, 31, 20, 0.025)",
      highlightColor = "#e8f1f8",
      cellPadding = "10px 8px",
      headerStyle = list(
        fontWeight = "bold",
        fontSize = "12px",
        color = "#5c4f42",
        textTransform = "uppercase",
        letterSpacing = "0.03em",
        borderBottom = "2px solid #0e68ab",
        textAlign = "center",
        padding = "8px 8px 10px"
      ),
      searchInputStyle = list(
        border = "1px solid #d4cfc9",
        borderRadius = "8px",
        padding = "8px 12px 8px 32px",
        fontSize = "14px",
        width = "220px"
      )
    ),
    columns = list(
      player_display = colDef(
        name = "Leikma\u00f0ur",
        align = "left",
        headerStyle = list(textAlign = "left"),
        minWidth = 175,
        style = list(fontWeight = 500, textAlign = "left")
      ),
      wins = colDef(
        name = "S",
        width = 55,
        style = list(
          color = "#00733e",
          fontWeight = 600,
          fontVariantNumeric = "tabular-nums"
        )
      ),
      losses = colDef(
        name = "T",
        width = 55,
        style = list(
          color = "#d3202a",
          fontWeight = 600,
          fontVariantNumeric = "tabular-nums"
        )
      ),
      hlutf_pct = colDef(
        name = "%",
        minWidth = 110,
        align = "center",
        cell = function(value) {
          width <- paste0(value, "%")
          # Smooth gradient: red(<=35%) -> neutral(50%) -> green(>=65%)
          t <- min(1, max(0, (value - 35) / 30)) # 0 at 35%, 1 at 65%
          r_col <- round(211 - t * 211) # 211 -> 0
          g_col <- round(32 + t * (115 - 32)) # 32 -> 115
          b_col <- round(42 + t * (62 - 42)) # 42 -> 62
          bar_color <- sprintf("rgb(%d, %d, %d)", r_col, g_col, b_col)
          div(
            style = "display: flex; align-items: center; gap: 6px;",
            div(
              style = "flex: 1; height: 6px; background: #eae6e1; border-radius: 3px; overflow: hidden;",
              div(style = paste0(
                "height: 100%; width: ", width, "; background: ", bar_color,
                "; border-radius: 3px;"
              ))
            ),
            span(
              style = "font-size: 12px; color: #5c4f42; min-width: 30px; text-align: right; font-variant-numeric: tabular-nums;",
              paste0(value, "%")
            )
          )
        }
      ),
      score = colDef(
        name = "ELO",
        width = 80,
        style = list(
          fontWeight = 700,
          fontSize = "18px",
          color = "#2a1f14",
          fontVariantNumeric = "tabular-nums"
        )
      ),
      score_prev = colDef(
        name = "Fyrra",
        width = 70,
        style = list(color = "#5c4f42", fontSize = "13px"),
        cell = function(value) if (is.na(value)) "\u2013" else value
      ),
      score_delta = colDef(
        name = "\u00b1",
        width = 70,
        cell = function(value) render_delta_cell(value)
      ),
      nr_prev = colDef(
        name = "Fyrra",
        width = 65,
        style = list(color = "#5c4f42", fontSize = "13px"),
        cell = function(value) if (is.na(value)) "\u2013" else value
      ),
      rank_delta = colDef(
        name = "\u00b1",
        width = 70,
        cell = function(value) render_delta_cell(value)
      )
    ),
    columnGroups = list(
      colGroup(
        name = "ELO",
        columns = c("score", "score_prev", "score_delta")
      ),
      colGroup(
        name = "S\u00e6ti",
        columns = c("nr_prev", "rank_delta")
      )
    )
  )

  # Wrap with subtitle if newest_date provided
  if (!is.null(newest_date)) {
    htmltools::div(
      htmltools::p(
        paste("Uppf\u00e6rt", newest_date),
        style = "color: #5c4f42; font-size: 13px; margin-bottom: 8px;"
      ),
      tbl
    )
  } else {
    tbl
  }
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

    save_player_summary_csv(
      fit = fit,
      players = players,
      cube_types = cube_types,
      output_dir = result_dir
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
