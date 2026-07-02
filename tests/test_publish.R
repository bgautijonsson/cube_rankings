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

# Defence-in-depth (2026-07-02): skra's Sheet write race could append duplicate
# rows for one match, keyed by the UUID in the sheet's own match_id column (H).
# load_results_games must drop them (first row kept) BEFORE rebuilding match_id
# as row_number(); blank ids (solo-era rows) are distinct matches, never dupes.
.sheet_raw <- tibble::tibble(
  date = as.Date("2026-07-02"),
  player1 = c("A", "A", "C", "E", "G"),
  player2 = c("B", "B", "D", "F", "H"),
  game1 = c("A", "B", "C", "E", "G"),
  game2 = c("B", "B", "D", "F", "H"),
  game3 = c("A", "B", "C", "E", "G"),
  cube = "Bolti",
  match_id = c("uuid-1", "uuid-1", "uuid-2", NA, NA)
)
.warns <- character(0)
.games <- withCallingHandlers(
  load_results_games(reader = function(url) .sheet_raw),
  warning = function(w) {
    .warns <<- c(.warns, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
stopifnot(dplyr::n_distinct(.games$match_id) == 4L) # 5 sheet rows -> 4 matches
.ab <- dplyr::filter(.games, player1 == "A")
stopifnot(nrow(.ab) == 3L, sum(.ab$winner == "A") == 2L) # first copy kept (A wins 2-1)
stopifnot(all(c("E", "G") %in% .games$player1)) # both blank-id rows survive
stopifnot(length(.warns) == 1L, grepl("1 duplicate", .warns), grepl("uuid-1", .warns))
.clean <- withCallingHandlers(
  load_results_games(reader = function(url) .sheet_raw[c(1, 3, 4, 5), ]),
  warning = function(w) stop("unexpected warning on duplicate-free sheet: ", conditionMessage(w))
)
stopifnot(dplyr::n_distinct(.clean$match_id) == 4L)
cat("PASS: duplicate column-H rows dropped (first kept, blanks untouched, count warned)\n")
