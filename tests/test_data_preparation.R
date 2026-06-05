options(width = 120)
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})
source("R/data_preparation.R")

# Regression (2026-06-05): the Google Sheet's Results tab gained sparse metadata
# columns (match_id, event_id) populated for only a handful of rows. A bare
# drop_na() in prepare_cube_data() dropped every row with an NA in ANY column,
# collapsing 1462 played games -> 37 (a single tier) and breaking the fit +
# publish. drop_na() must only drop UNPLAYED games (winner is NA).
d_raw <- tibble::tibble(
  date     = as.Date(c("2026-01-01", "2026-01-01", "2026-01-08")),
  cube     = c("Bolti", "Synergy Cube", "Meta Memories"), # High, Medium, Medium
  player1  = c("A", "C", "A"),
  player2  = c("B", "D", "C"),
  game1    = c("A", "C", "A"),
  game2    = c("B", NA, "C"), # some matches only go 1-2 games
  game3    = c(NA, NA, NA),
  match_id = c("m1", NA, NA), # sparse new metadata column
  event_id = c("e1", NA, NA)
)

prepared <- prepare_cube_data(d_raw)

# All 5 PLAYED games survive (game1 x3 + game2 x2); sparse match_id/event_id NAs
# must NOT drop rows. The bug collapsed this to 2.
stopifnot(nrow(prepared$processed_data) == 5L)
cat("PASS: prepare_cube_data keeps played games despite sparse metadata NAs\n")

# Tiers are not collapsed: both High and Medium present (bug left only High).
stopifnot(identical(sort(unique(prepared$processed_data$cube)), c("High", "Medium")))
cat("PASS: prepare_cube_data preserves all played tiers (no single-tier collapse)\n")

# upto_date filtering still works (drop the 2026-01-08 game).
prepared_cut <- prepare_cube_data(d_raw, upto_date = "2026-01-01")
stopifnot(nrow(prepared_cut$processed_data) == 3L)
stopifnot(all(prepared_cut$processed_data$date == as.Date("2026-01-01")))
cat("PASS: prepare_cube_data upto_date filter unaffected by the fix\n")
