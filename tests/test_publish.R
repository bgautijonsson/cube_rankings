options(width = 120)
suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
})
source("R/elo_table.R")
source("scripts/publish.R", local = TRUE)

results_dir <- list_results_dirs()[1]
stopifnot(dir.exists(results_dir))

fake_optin <- readr::read_csv(file.path(results_dir, "player_summary.csv"),
  show_col_types = FALSE
)$player[1:3]
r <- build_rankings(results_dir, NA_character_, fake_optin)
stopifnot(all(r$player %in% fake_optin))
stopifnot(!is.unsorted(rev(r$score_median)))
cat("PASS: build_rankings filters to opt-in and sorts\n")

# CRITICAL: no non-opted-in player may appear in ANY artifact.
fake_games <- tibble::tibble(
  match_id = rep(1:2, each = 3), date = as.Date("2026-05-14"),
  player1 = "A", player2 = rep(c("B", "Zzz"), each = 3), cube = "Bolti",
  winner = c("A", "A", "B", "A", "Zzz", "A")
)
hh <- build_head_to_head(fake_games, c("A", "B")) # "Zzz" is NOT opted in
leaked <- purrr::map(hh, ~ c(.x$player_a, .x$player_b)) |>
  unlist() |>
  intersect("Zzz")
stopifnot(length(leaked) == 0)
cat("PASS: opt-in filter holds across artifacts\n")

# Contract keys present on freshly-built artifacts (no stale-file dependency, no auth).
r_obj <- build_rankings(results_dir, NA_character_, fake_optin)
stopifnot(all(c("player", "score_median", "score_lower", "score_upper") %in% names(r_obj)))
processed_data <- readRDS(file.path(results_dir, "processed_data.rds"))
m_obj <- build_meta_enriched(results_dir, r_obj, processed_data, fake_games)
stopifnot(all(c("generated_at", "fit_date", "n_players", "cmdstan_version", "model") %in% names(m_obj)))
cat("PASS: built artifacts have the contract keys\n")

# CRITICAL (enriched): no non-opted-in player may appear in cubes detail or head-to-head.
source("R/cube_tier.R")
.fake_games <- tibble::tibble(
  match_id = rep(1:2, each = 3), date = as.Date("2026-05-14"),
  player1 = "A", player2 = rep(c("B", "Zzz"), each = 3), cube = "Bolti",
  winner = c("A", "A", "B", "A", "Zzz", "A")
)
.fake_optin <- c("A", "B") # "Zzz" is NOT opted in
.mr <- load_match_results(.fake_games)
.det <- cube_detail(.fake_games, .mr, slug = "bolti", optin = .fake_optin)
.names <- c(
  purrr::map_chr(.det$player_rankings, "player"),
  unlist(purrr::map(.det$events, ~ purrr::map_chr(.x$results, "player")))
)
stopifnot(!("Zzz" %in% .names))
.hh <- head_to_head_records(.fake_games, .fake_optin)
.hh_names <- unlist(purrr::map(.hh, ~ c(.x$player_a, .x$player_b)))
stopifnot(!("Zzz" %in% .hh_names))
cat("PASS: opt-in filter holds across enriched artifacts (cubes detail, head-to-head)\n")
