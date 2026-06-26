# Pure helpers for incremental fitting: which play-dates still need a model fit.

# Distinct play dates in the raw Results sheet, ascending, as character "YYYY-MM-DD".
sheet_play_dates <- function(d_raw) {
  d_raw |>
    dplyr::mutate(date = lubridate::as_date(date)) |>
    dplyr::distinct(date) |>
    dplyr::arrange(date) |>
    dplyr::pull(date) |>
    as.character()
}

# Date-named result dirs already on disk, as character "YYYY-MM-DD".
existing_result_dates <- function(results_root = "results") {
  dirs <- if (dir.exists(results_root)) {
    list.dirs(results_root, recursive = FALSE, full.names = FALSE)
  } else {
    character(0)
  }
  dirs[grepl("^\\d{4}-\\d{2}-\\d{2}$", dirs)]
}

# Sheet dates with no fitted result dir yet (sorted): the set the model must fit.
new_result_dates <- function(all_dates, existing_dates) {
  sort(setdiff(as.character(all_dates), as.character(existing_dates)))
}

# Source-faithful columns that define a date's game content. Excludes the derived
# index columns (player_nr1/2, cube_nr, time_idx) because those are numbered
# relative to the cumulative player/cube set and shift as later dates are added;
# including them would falsely invalidate earlier, untouched dates.
.fingerprint_cols <- c("date", "cube", "player1", "player2", "game", "winner")

# Content fingerprint of one date's rows within a processed_data tibble: the row
# count plus a hash of the date's sorted semantic rows. Order-independent, and
# stable across the cumulative-context reindexing described above.
date_fingerprint <- function(processed_data, the_date) {
  cols <- intersect(.fingerprint_cols, names(processed_data))
  keep <- as.character(processed_data$date) == as.character(the_date)
  sub <- processed_data[keep, cols, drop = FALSE]
  sep <- intToUtf8(31L) # unit separator: cannot occur in tiers, names, or game numbers
  rows <- sort(do.call(paste, c(lapply(sub, as.character), sep = sep)))
  paste0(length(rows), ":", rlang::hash(rows))
}

# Per-date fingerprints for every date in a processed_data tibble (the live sheet).
sheet_date_fingerprints <- function(processed_data) {
  dates <- sort(unique(as.character(processed_data$date)))
  stats::setNames(
    vapply(dates, date_fingerprint, character(1), processed_data = processed_data),
    dates
  )
}

# Per-date fingerprints reconstructed from each fitted date's stored
# processed_data.rds (cumulative up to that date; sliced to the date's own rows).
# Dates whose stored data can't be read are omitted, so they fall through to a
# refit rather than being trusted as current.
stored_date_fingerprints <- function(existing_dates, results_root = "results") {
  fps <- character(0)
  for (d in existing_dates) {
    pd_path <- file.path(results_root, d, "processed_data.rds")
    if (!file.exists(pd_path)) next
    pd <- tryCatch(readRDS(pd_path), error = function(e) NULL)
    if (!is.null(pd)) fps[[d]] <- date_fingerprint(pd, d)
  }
  fps
}

# Content-aware successor to new_result_dates(): every sheet date whose current
# fingerprint differs from its stored one — never fit (no stored fingerprint) OR
# its underlying rows changed since the last fit. Sorted.
dates_needing_fit <- function(sheet_fps, stored_fps) {
  dates <- names(sheet_fps)
  needs <- vapply(
    dates,
    function(d) !(d %in% names(stored_fps)) || !identical(stored_fps[[d]], sheet_fps[[d]]),
    logical(1)
  )
  sort(dates[needs])
}
