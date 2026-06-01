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
# Assert the FULL per-tier + prev enrichment column set, not just 2 of them — if the
# TIERS loop silently dropped Medium/Low/Other this would catch it. (Pattern matches the
# 4 *_rank, 4 *_prev_rank, 4 *_prev_elo, plus the overall prev_rank — 13 names; the bare
# overall `rank` has no underscore so is excluded, matching the with-prev contract.)
.expected_enrich <- sort(c(
  "prev_rank",
  paste0(c("High", "Medium", "Low", "Other"), "_rank"),
  paste0(c("High", "Medium", "Low", "Other"), "_prev_rank"),
  paste0(c("High", "Medium", "Low", "Other"), "_prev_elo")
))
.actual_enrich <- sort(grep("_rank$|_prev_elo$|_prev_rank$|_elo$", names(.r), value = TRUE))
stopifnot(identical(.actual_enrich, .expected_enrich))
cat("PASS: rankings_attach_ranks bakes current+previous rank/ELO overall and per tier\n")

# No-prev branch (prev = NULL): spec-prescribed asymmetry — prev_rank is NA_integer_
# and the 8 per-tier *_prev_rank/*_prev_elo columns are ABSENT (not NA). Confirms the
# structural difference from the with-prev path is intentional and known to Task 10.
.r0 <- rankings_attach_ranks(
  base = dplyr::arrange(.now, dplyr::desc(score_median)),
  now = .now, prev = NULL,
  last_now = .lastgame, last_prev = NULL,
  ref_now = as.Date("2026-05-14"), ref_prev = NULL,
  optin_lc = c("a", "b", "c")
)
.a0 <- .r0[.r0$player == "A", ]
stopifnot(identical(.a0$prev_rank, NA_integer_))
stopifnot(!("High_prev_rank" %in% names(.r0)), !("High_prev_elo" %in% names(.r0)))
cat("PASS: rankings_attach_ranks no-prev path yields NA prev_rank and omits per-tier prev columns\n")

.hist <- tibble::tibble(
  player = c("A", "B", "A", "B"),
  date = as.Date(c("2026-05-07", "2026-05-07", "2026-05-14", "2026-05-14")),
  score_median = c(1500, 1490, 1600, 1480), score_q25 = 1480, score_q75 = 1520,
  score_lower = 1450, score_upper = 1550, wins = c(5, 4, 12, 6), losses = c(3, 4, 6, 10),
  total = c(8, 8, 18, 16), gamma_High = c(0.2, 0, 0.3, 0), gamma_Medium = 0,
  gamma_Low = -0.1, gamma_Other = 0
)
.ph <- player_history_records(.hist)
.a <- .ph[["A"]]
stopifnot(length(.a$history) == 2)
stopifnot(.a$history[[2]]$rank == 1) # A leads on 2026-05-14
stopifnot(abs(.a$history[[2]]$win_rate - (12 / 18 * 100)) < 1e-9)
stopifnot(all(c("score_q25", "score_q75", "games", "strength") %in% names(.a$history[[2]])))
stopifnot(.a$summary$strongest_tier == "High", .a$summary$games == 18)

.games_pc <- tibble::tibble(
  player1 = c("A", "A", "C"), player2 = c("B", "C", "A"), winner = c("A", "C", "C"),
  cube = c("Bolti", "Khans Cube", "Bolti"), date = as.Date("2026-05-14"),
  match_id = 1:3
)
.pc <- per_cube_player_records(.games_pc, optin = c("A", "B")) # C is NOT opted in
stopifnot(all(.pc$player %in% c("A", "B"))) # C omitted
.a_vc <- .pc[.pc$player == "A" & .pc$cube == "Bolti", ]
stopifnot(.a_vc$wins == 1, .a_vc$tier == "High")
cat("PASS: player_history_records (series+rank+summary) and per_cube_player_records (opt-in, tier)\n")

.g_h2h <- tibble::tibble(
  player1 = c("A", "A", "A"), player2 = c("B", "B", "B"), winner = c("A", "B", "A"),
  cube = c("Bolti", "Bolti", "Khans Cube"), date = as.Date("2026-05-14"), match_id = 1:3
)
.hh <- head_to_head_records(.g_h2h, optin = c("A", "B"))
.pair <- .hh[[1]]
stopifnot(.pair$player_a == "A", .pair$player_b == "B")
stopifnot(.pair$a_wins == 2, .pair$b_wins == 1) # game-level
.bc <- .pair$by_cube
stopifnot(length(.bc) == 2) # Bolti + Khans Cube
cat("PASS: head_to_head_records is game-level with per-named-cube by_cube, both opted-in\n")

.g_cube <- tibble::tibble(
  match_id = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  date = as.Date("2026-05-14"),
  player1 = "A", player2 = rep(c("B", "C", "D"), each = 3), # C, D not opted in
  cube = "Bolti",
  winner = c("A", "A", "A", "A", "A", "B", "A", "D", "A")
)
.mr <- load_match_results(.g_cube)
.idx <- cubes_index(.g_cube, .mr)
stopifnot(.idx[[1]]$cube == "Bolti", .idx[[1]]$slug == "bolti", .idx[[1]]$tier == "High")
stopifnot(.idx[[1]]$n_events == 1)
.det <- cube_detail(.g_cube, .mr, slug = "bolti", optin = c("A", "B")) # C,D omitted
.players_listed <- unique(purrr::map_chr(.det$player_rankings, "player"))
stopifnot(all(.players_listed %in% c("A", "B"))) # PRIVACY: no C/D
.a_trophy <- purrr::keep(.det$trophy_leaders, ~ .x$player == "A")
stopifnot(length(.a_trophy) == 1, .a_trophy[[1]]$trophies == 1) # A won 3 matches at the event
cat("PASS: cubes index + detail (named, tiered, opt-in omitted, trophies = 3+ match wins)\n")
