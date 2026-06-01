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
stopifnot(
  identical(.meta$tiers$High, list("Bolti")),
  identical(.meta$tiers$Low, list("Khans Cube")),
  identical(.meta$tiers$Other, list("Genesis"))
)
cat("PASS: build_meta_enriched adds counts, reference_date, and tier->cube map\n")

.mk_summary <- function(scores) {
  tibble::tibble(
    player = c("A", "B", "C"), player_nr = 1:3,
    wins = c(40, 30, 20), losses = c(20, 20, 20), total = c(60, 50, 40),
    score_median = scores, score_q25 = scores - 10, score_q75 = scores + 10,
    score_lower = scores - 50, score_upper = scores + 50,
    gamma_High = 0.1, gamma_Medium = 0, gamma_Low = -0.1, gamma_Other = 0,
    High_elo_median = scores + 5, Medium_elo_median = scores,
    Low_elo_median = scores - 5, Other_elo_median = scores,
    High_wins = 20, Medium_wins = 10, Low_wins = 6, Other_wins = 4,
    High_total = 30, Medium_total = 12, Low_total = 10, Other_total = 8
  )
}
.lastgame <- tibble::tibble(player = c("A", "B", "C"), last_game = as.Date("2026-05-14"))
.now <- .mk_summary(c(1600, 1550, 1500))
.prev <- .mk_summary(c(1500, 1560, 1490)) # B was ahead of A previously
.r <- rankings_attach_ranks(
  base = dplyr::arrange(.now, dplyr::desc(score_median)),
  now = .now, prev = .prev,
  last_now = .lastgame, last_prev = .lastgame,
  ref_now = as.Date("2026-05-14"), ref_prev = as.Date("2026-05-14"),
  optin_lc = c("a", "b", "c")
)
.a <- .r[.r$player == "A", ]
stopifnot(.a$rank == 1, .a$prev_rank == 2, .a$prev_score_median == 1500)
stopifnot(.a$last_date == "2026-05-14")
stopifnot("High_rank" %in% names(.r), "High_prev_elo" %in% names(.r))
cat("PASS: rankings_attach_ranks bakes current+previous rank/ELO overall and per tier\n")
