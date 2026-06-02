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
