options(width = 120)
suppressPackageStartupMessages({
  library(tidyverse)
})
source("R/cube_tier.R")

stopifnot(cube_tier("Bolti") == "High")
stopifnot(cube_tier("VINTAGE ARON") == "High")
stopifnot(cube_tier("inventors' fair") == "Medium")
stopifnot(cube_tier("Horror Cube") == "Medium")
stopifnot(cube_tier("Pauper Cube Diddi") == "Low")
stopifnot(cube_tier("the ab wheel") == "Low")
stopifnot(cube_tier("boltalandi\u00f0") == "Other")
stopifnot(cube_tier("Some Cube That Does Not Exist") == "Other")
stopifnot(identical(
  cube_tier(c("Bolti", "Synergy Cube", "Khans Cube", "Genesis")),
  c("High", "Medium", "Low", "Other")
))
cat("PASS: cube_tier maps the case_when exactly + vectorises\n")

source("scripts/publish.R", local = TRUE)

.rk_df <- tibble::tibble(
  player    = c("A", "B", "C", "D", "E"),
  score     = c(1600, 1550, 1500, 1400, 1350),
  wins      = c(40, 30, 2, 20, 3),
  total     = c(60, 50, 3, 30, 30), # C has < 18 games; D absent; E has 3/30 = 10% win
  last_game = as.Date(c("2026-05-14", "2026-05-14", "2026-05-14", "2026-01-01", "2026-05-14"))
)
# absence_cutoff is the 8-week floor from the dataset reference date (2026-05-14) = 2026-03-19.
# D's last game (2026-01-01) is before the cutoff — dropped for absence only (win-rate 20/30=66.7% passes).
# E's win-rate (3/30=10%) is below min_winrate=0.15 — dropped for low win-rate only (recent, enough games).
.cut <- as.Date("2026-05-14") - lubridate::weeks(8) # ref = newest event date; fixed, no time coupling
.ranked <- rank_estimates(.rk_df,
  min_total_games = 18, min_winrate = 0.15,
  max_absence_weeks = 8, table_players = c("a", "b", "c", "d", "e"),
  absence_cutoff = .cut
)
# A and B qualify (>=18 games, >=15% win, recent, opted in); C dropped (<18 games);
# D dropped (absent); E dropped (low win-rate)
stopifnot(identical(.ranked$player, c("A", "B")))
stopifnot(identical(.ranked$nr, c(1L, 2L)))
cat("PASS: rank_estimates filters (games/winrate/absence/opt-in) then dense-ranks by score\n")

.pd <- tibble::tibble(
  date = as.Date(c("2026-05-07", "2026-05-14", "2026-05-14")),
  cube = c("High", "Low", "High")
)
.games_meta <- tibble::tibble(cube = c("Bolti", "Khans Cube", "Genesis"))
.meta <- build_meta_enriched(
  results_dir = "results/2026-05-14",
  rankings = tibble::tibble(player = c("A", "B")),
  processed_data = .pd, games = .games_meta
)
stopifnot(.meta$n_players == 2, .meta$n_games == 3, .meta$n_dates == 2)
stopifnot(.meta$reference_date == "2026-05-14")
stopifnot(.meta$tiers$High == "Bolti", .meta$tiers$Low == "Khans Cube", .meta$tiers$Other == "Genesis")
cat("PASS: build_meta_enriched adds counts, reference_date, and tier->cube map\n")
