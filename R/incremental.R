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
