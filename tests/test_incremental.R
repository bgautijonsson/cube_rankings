options(width = 120)
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})
source("R/incremental.R")

d <- tibble::tibble(
  date = as.Date(c("2026-05-14", "2026-05-07", "2026-05-14")),
  cube = "X", player1 = "A", player2 = "B",
  game1 = "A", game2 = "A", game3 = NA_character_
)
stopifnot(identical(sheet_play_dates(d), c("2026-05-07", "2026-05-14")))
cat("PASS: sheet_play_dates distinct + sorted + character\n")

stopifnot(identical(new_result_dates(c("2026-05-14", "2026-05-21"), "2026-05-14"), "2026-05-21"))
stopifnot(identical(new_result_dates("2026-05-14", "2026-05-14"), character(0)))
cat("PASS: new_result_dates returns unfit dates only\n")

tmp <- tempfile()
dir.create(tmp)
dir.create(file.path(tmp, "2026-05-14"))
dir.create(file.path(tmp, "notadate"))
stopifnot(identical(sort(existing_result_dates(tmp)), "2026-05-14"))
cat("PASS: existing_result_dates filters to date-named dirs\n")

# --- content-aware detection: a fitted date is re-fit when its rows change ---
# Each results/<date>/processed_data.rds is cumulative, and its derived index
# columns (player_nr1/2, cube_nr, time_idx) are numbered relative to the
# cumulative player/cube set, so they shift as later dates are added. A date's
# content fingerprint must depend only on that date's own game rows, never on
# those indices.

mk_pd <- function(rows, cube_nr = 1L, time_idx = 1L, nr_offset = 0L) {
  rows |>
    mutate(
      date = as_date(date),
      result = as.integer(winner == player1),
      player_nr1 = match(player1, LETTERS) + nr_offset,
      player_nr2 = match(player2, LETTERS) + nr_offset,
      cube_nr = cube_nr,
      time_idx = time_idx
    )
}

day_rows <- tibble::tibble(
  date = "2026-05-14", cube = "Medium",
  player1 = c("A", "A", "B"),
  player2 = c("B", "B", "C"),
  game = c(1, 2, 1),
  winner = c("A", "B", "C")
)

# Same date content in two different cumulative contexts -> identical fingerprint.
fp_ctx1 <- date_fingerprint(mk_pd(day_rows, cube_nr = 1, time_idx = 1, nr_offset = 0), "2026-05-14")
fp_ctx2 <- date_fingerprint(mk_pd(day_rows, cube_nr = 5, time_idx = 3, nr_offset = 100), "2026-05-14")
stopifnot(identical(fp_ctx1, fp_ctx2))
cat("PASS: date_fingerprint ignores context-dependent derived index columns\n")

# Appending a game row to the date changes its fingerprint.
day_rows_more <- bind_rows(day_rows, tibble::tibble(
  date = "2026-05-14", cube = "Medium", player1 = "A", player2 = "C", game = 1, winner = "A"
))
stopifnot(!identical(date_fingerprint(mk_pd(day_rows_more), "2026-05-14"), fp_ctx1))
cat("PASS: date_fingerprint changes when a game is added to a date\n")

# dates_needing_fit: refit changed + never-fit dates, skip unchanged (fast-path).
sheet_fps <- c("2026-05-07" = "old", "2026-05-14" = "new", "2026-05-21" = "brandnew")
stored_fps <- c("2026-05-07" = "old", "2026-05-14" = "stale")
stopifnot(identical(dates_needing_fit(sheet_fps, stored_fps), c("2026-05-14", "2026-05-21")))
stopifnot(identical(dates_needing_fit(stored_fps, stored_fps), character(0)))
cat("PASS: dates_needing_fit flags changed + new dates, skips unchanged\n")

# End-to-end (the 2026-06-26 incident): games appended to an already-fitted date
# whose folder already exists must now trigger a refit.
res_root <- tempfile()
dir.create(file.path(res_root, "2026-05-14"), recursive = TRUE)
saveRDS(mk_pd(day_rows), file.path(res_root, "2026-05-14", "processed_data.rds"))

new_day <- mk_pd(tibble::tibble(
  date = "2026-05-21", cube = "High", player1 = "A", player2 = "B", game = 1, winner = "B"
), cube_nr = 2, time_idx = 2)

stored_now <- stored_date_fingerprints(existing_result_dates(res_root), res_root)

# 4 games now on 2026-05-14 (was 3) + a brand-new date -> both need fitting.
changed_sheet <- bind_rows(mk_pd(day_rows_more), new_day)
stopifnot(identical(
  dates_needing_fit(sheet_date_fingerprints(changed_sheet), stored_now),
  c("2026-05-14", "2026-05-21")
))
cat("PASS: rows appended to a fitted date trigger a refit (end-to-end)\n")

# Untouched fitted date stays skipped; only the genuinely new date is fit.
unchanged_sheet <- bind_rows(mk_pd(day_rows), new_day)
stopifnot(identical(
  dates_needing_fit(sheet_date_fingerprints(unchanged_sheet), stored_now),
  "2026-05-21"
))
cat("PASS: untouched fitted date stays skipped (fast-path preserved)\n")
