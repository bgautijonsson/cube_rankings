options(width = 120)
suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
})
source("R/elo_table.R")
source("scripts/publish.R", local = TRUE)

results_dir <- newest_results_dir()
stopifnot(dir.exists(results_dir))

fake_optin <- readr::read_csv(file.path(results_dir, "player_summary.csv"),
  show_col_types = FALSE
)$player[1:3]
r <- build_rankings(results_dir, fake_optin)
stopifnot(all(r$player %in% fake_optin))
stopifnot(!is.unsorted(rev(r$score_median)))
cat("PASS: build_rankings filters to opt-in and sorts\n")

# CRITICAL: no non-opted-in player may appear in ANY artifact.
all_players <- readr::read_csv(file.path(results_dir, "player_summary.csv"),
  show_col_types = FALSE
)$player
not_optin <- setdiff(all_players, fake_optin)
hh <- build_head_to_head(results_dir, fake_optin)
leaked <- purrr::map(hh, ~ c(.x$player_a, .x$player_b)) |>
  unlist() |>
  intersect(not_optin)
stopifnot(length(leaked) == 0)
cat("PASS: opt-in filter holds across artifacts\n")

# Contract keys present on freshly-built artifacts (no stale-file dependency, no auth).
r_obj <- build_rankings(results_dir, fake_optin)
stopifnot(all(c("player", "score_median", "score_lower", "score_upper") %in% names(r_obj)))
m_obj <- build_meta(results_dir, r_obj)
stopifnot(all(c("generated_at", "fit_date", "n_players", "cmdstan_version", "model") %in% names(m_obj)))
cat("PASS: built artifacts have the contract keys\n")
