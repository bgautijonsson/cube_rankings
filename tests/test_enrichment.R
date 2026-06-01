options(width = 120)
suppressPackageStartupMessages({ library(tidyverse) })
source("R/cube_tier.R")

stopifnot(cube_tier("Bolti") == "High")
stopifnot(cube_tier("VINTAGE ARON") == "High")
stopifnot(cube_tier("inventors' fair") == "Medium")
stopifnot(cube_tier("Horror Cube") == "Medium")
stopifnot(cube_tier("Pauper Cube Diddi") == "Low")
stopifnot(cube_tier("the ab wheel") == "Low")
stopifnot(cube_tier("boltaland\u00ed\u00f0") == "Other")
stopifnot(cube_tier("Some Cube That Does Not Exist") == "Other")
stopifnot(identical(
  cube_tier(c("Bolti", "Synergy Cube", "Khans Cube", "Genesis")),
  c("High", "Medium", "Low", "Other")
))
cat("PASS: cube_tier maps the case_when exactly + vectorises\n")

source("scripts/publish.R", local = TRUE)

.rk_df <- tibble::tibble(
  player    = c("A", "B", "C", "D"),
  score     = c(1600, 1550, 1500, 1400),
  wins      = c(40, 30, 2, 5),
  total     = c(60, 50, 3, 30),          # C has < 18 games; D has 5/30 = 16.7% win
  last_game = as.Date(c("2026-05-14", "2026-05-14", "2026-05-14", "2026-01-01"))
)
.cut <- as.Date("2026-05-14") - lubridate::weeks(8)
.ranked <- rank_estimates(.rk_df, min_total_games = 18, min_winrate = 0.15,
                          max_absence_weeks = 8, table_players = c("a","b","c","d"),
                          absence_cutoff = .cut)
# A and B qualify (>=18 games, >=15% win, recent); C dropped (<18 games); D dropped (absent + low win)
stopifnot(identical(.ranked$player, c("A", "B")))
stopifnot(identical(.ranked$nr, c(1L, 2L)))
cat("PASS: rank_estimates filters (games/winrate/absence/opt-in) then dense-ranks by score
")
