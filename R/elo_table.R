library(tidyverse)
library(posterior)
library(cmdstanr)
library(gt)
library(gtExtras)
library(scales)
library(glue)
library(googlesheets4)

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

load_results_set <- function(
    rank = 1,
    results_root = .default_results_root,
    fallback_dir = .default_fallback_dir
) {
  dirs <- list_results_dirs(results_root)
  
  if (length(dirs) >= rank) {
    results_dir <- dirs[rank]
    files_present <- all(file.exists(file.path(
      results_dir,
      c("fitted_model.rds", "players.rds", "processed_data.rds")
    )))
    
    if (files_present) {
      return(list(
        fit = readRDS(file.path(results_dir, "fitted_model.rds")),
        players = readRDS(file.path(results_dir, "players.rds")),
        processed_data = readRDS(file.path(results_dir, "processed_data.rds")),
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
  
  list(
    fit = readRDS(fallback_files[1]),
    players = readRDS(fallback_files[2]),
    processed_data = readRDS(fallback_files[3]),
    source_dir = fallback_dir
  )
}

compute_player_estimates <- function(fit, players) {
  fit$draws("alpha") |>
    as_draws_df() |>
    as_tibble() |>
    pivot_longer(c(-starts_with("."))) |>
    mutate(name = parse_number(name)) |>
    inner_join(players, by = join_by(name == player_nr)) |>
    summarise(
      median = median(value),
      lower = quantile(value, 0.05),
      upper = quantile(value, 0.95),
      .by = c(player, wins, losses, total, text)
    )
}

bt_to_elo <- function(x) {
  (400 / log(10)) * x + 1500
}

render_elo_table <- function(
    top_n = 10,
    results_root = .default_results_root,
    fallback_dir = .default_fallback_dir,
    min_total_games = 0,
    min_winrate = 0.15,
    table_players = NULL,
    newest_date = NULL
) {
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
  
  estimates <- compute_player_estimates(
    fit = latest$fit,
    players = latest$players
  ) |>
    mutate(player = fct_reorder(player, median)) |>
    mutate(
      hlutf = wins / total,
      median = bt_to_elo(median)
    ) |>
    arrange(desc(median)) |>
    filter(
      total >= min_total_games,
      hlutf >= min_winrate,
      is.null(table_players) | str_to_lower(player) %in% table_players
    ) |>
    mutate(
      nr = row_number(),
      deild = case_when(
        nr <= 10 ~ "Deild 1",
        nr <= 20 ~ "Deild 2",
        TRUE ~ "Deild 3"
      )
    ) |>
    select(-lower, -upper)
  
  if (!is.null(previous)) {
    previous_estimates <- compute_player_estimates(
      fit = previous$fit,
      players = previous$players
    ) |>
      mutate(
        median = bt_to_elo(median),
        hlutf = wins / total
      ) |>
      arrange(desc(median)) |>
      filter(
        total >= min_total_games,
        hlutf >= min_winrate,
        is.null(table_players) | str_to_lower(player) %in% table_players
      ) |>
      mutate(nr_prev = row_number()) |>
      select(
        player,
        median_prev = median,
        nr_prev
      )
    
    estimates <- estimates |>
      left_join(previous_estimates, by = "player")
  } else {
    estimates <- estimates |>
      mutate(
        median_prev = NA_real_,
        nr_prev = NA_integer_
      )
  }
  
  estimates <- estimates |>
    mutate(
      rank_delta = nr_prev - nr,
      elo_delta = median - median_prev,
      rank_delta_label = case_when(
        is.na(rank_delta) ~ "",
        rank_delta > 0 ~ paste0("+", rank_delta),
        rank_delta < 0 ~ as.character(rank_delta),
        TRUE ~ "0"
      ),
      elo_delta_label = case_when(
        is.na(elo_delta) ~ "",
        elo_delta > 0 ~ paste0("+", round(elo_delta, 0)),
        elo_delta < 0 ~ as.character(round(elo_delta, 0)),
        TRUE ~ "0"
      )
    )
  
  if (!is.null(top_n)) {
    estimates <- estimates |>
      filter(nr <= top_n)
  }
  
  
  table <- estimates |>
    # filter(
    #   !player %in% c("Tommi", "Diddi")
    # ) |> 
    # add_row(
    #   player = "Tommi", wins = 0, losses = 999, total = 999,
    #   text = "0% (0/999)", median = 0, hlutf = 0, nr = 21, deild = "Deild 3", median_prev = 0,
    #   nr_prev = 21, rank_delta = 0, elo_delta = 0, rank_delta_label = "0", elo_delta_label = "0"
    # ) |> 
    # add_row(
    #   player = "Diddi", wins = 0, losses = 999, total = 999,
    #   text = "0% (0/999)", median = 0, hlutf = 0, nr = 22, deild = "Deild 3", median_prev = 0,
    #   nr_prev = 22, rank_delta = 0, elo_delta = 0, rank_delta_label = "0", elo_delta_label = "0"
    # ) |> 
    # mutate(
    #   nr = if_else(
    #     (nr > 4),
    #     nr - 2,
    #     nr
    #   ),
    #   nr_prev = if_else(
    #     nr_prev > 4,
    #     nr_prev - 2,
    #     nr_prev - 2
    #   )
    # ) |> 
    gt() |>
    cols_hide(c(total, text, deild, rank_delta, elo_delta)) |>
    cols_move(hlutf, after = losses) |>
    cols_move(median_prev, after = median) |>
    cols_move(elo_delta_label, after = median_prev) |>
    cols_move(nr, after = elo_delta_label) |>
    cols_move(nr_prev, after = nr) |>
    cols_move(rank_delta_label, after = nr_prev) |>
    cols_label(
      player = "Leikmaður",
      wins = "S",
      losses = "T",
      hlutf = "%",
      median = "ELO",
      median_prev = "Fyrra",
      elo_delta_label = "±",
      nr = "#",
      nr_prev = "Fyrra",
      rank_delta_label = "±"
    ) |>
    cols_align(columns = player, align = "left") |>
    cols_align(columns = -player, align = "center") |>
    cols_merge(columns = c(player, nr), pattern = "{2}. {1}") |>
    cols_width(player ~ px(180)) |>
    fmt_percent(hlutf, decimals = 0) |>
    fmt_number(c(median, median_prev), decimals = 0) |>
    sub_missing(missing_text = "-") |>
    # Clean minimal styling
    tab_header(
      title = if (!is.null(newest_date)) glue("Uppfært {newest_date}") else NULL
    ) |>
    tab_spanner(label = "ELO stig", columns = c(median, median_prev, elo_delta_label)) |>
    tab_spanner(label = "Sæti", columns = c(nr_prev, rank_delta_label)) |>
    # Style positive/negative changes - muted green/red
    tab_style(
      locations = cells_body(columns = elo_delta_label, rows = str_detect(elo_delta_label, "^\\+")),
      style = cell_text(color = "#2e7d32")
    ) |>
    tab_style(
      locations = cells_body(columns = elo_delta_label, rows = str_detect(elo_delta_label, "^\\-")),
      style = cell_text(color = "#c62828")
    ) |>
    tab_style(
      locations = cells_body(columns = rank_delta_label, rows = str_detect(rank_delta_label, "^\\+")),
      style = cell_text(color = "#2e7d32")
    ) |>
    tab_style(
      locations = cells_body(columns = rank_delta_label, rows = str_detect(rank_delta_label, "^\\-")),
      style = cell_text(color = "#c62828")
    ) |>
    # Greyscale table options
    tab_options(
      table.font.size = px(14),
      table.font.color = "#333333",
      heading.title.font.size = px(13),
      heading.title.font.weight = "normal",
      heading.align = "left",
      column_labels.font.weight = "bold",
      column_labels.font.size = px(13),
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "#333333",
      table_body.hlines.color = "#e0e0e0",
      table.border.top.style = "hidden",
      table.border.bottom.style = "hidden",
      data_row.padding = px(8)
    )
  
  table
}
