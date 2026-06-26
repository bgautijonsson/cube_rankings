suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})
source("R/data_preparation.R") # download_cube_results + prepare_cube_data
source("R/incremental.R")

# Content-aware detection: a date needs (re)fitting when it has no results folder
# yet OR its underlying game rows have changed since it was last fit. The latter
# catches games appended to an already-fitted date, which presence-only detection
# silently missed -- leaving the site's tallies and rankings split-brain.
processed <- prepare_cube_data(download_cube_results())$processed_data
new_dates <- dates_needing_fit(
  sheet_date_fingerprints(processed),
  stored_date_fingerprints(existing_result_dates())
)
cat(
  "Dates needing a (re)fit:",
  if (length(new_dates)) paste(new_dates, collapse = ", ") else "(none)", "\n"
)

out <- Sys.getenv("GITHUB_OUTPUT", "")
line <- sprintf("new_dates=%s\n", if (length(new_dates) > 0) "true" else "false")
if (nzchar(out)) cat(line, file = out, append = TRUE) else cat(line)
