# Defence-in-depth against duplicate Results rows. The skra app writes one row
# per match, keyed by a UUID in the sheet's match_id column (column H); its
# Sheet write leg could race (fixed 2026-07-02, skra 42c09e0) and manual edits
# can still duplicate rows. Keep the first row per id; blank ids (solo-era
# rows) are distinct matches, never duplicates of each other.
drop_duplicate_sheet_rows <- function(d_raw) {
  if (!"match_id" %in% names(d_raw)) {
    warning(
      "Results sheet has no match_id column; duplicate-row guard skipped",
      call. = FALSE, immediate. = TRUE
    )
    return(d_raw)
  }
  id <- d_raw$match_id
  if (is.list(id)) {
    id <- vapply(
      id,
      function(x) if (length(x) == 0 || is.na(x[[1]])) NA_character_ else as.character(x[[1]]),
      character(1)
    )
  }
  id <- as.character(id)
  id[!is.na(id) & !nzchar(trimws(id))] <- NA_character_
  dup <- duplicated(id) & !is.na(id)
  if (any(dup)) {
    warning(
      sprintf(
        "dropped %d duplicate Results row(s) sharing a sheet match_id (kept first occurrence): %s",
        sum(dup), paste(unique(id[dup]), collapse = ", ")
      ),
      call. = FALSE, immediate. = TRUE
    )
  }
  d_raw[!dup, , drop = FALSE]
}
