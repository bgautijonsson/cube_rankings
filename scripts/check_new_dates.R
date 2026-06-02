suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})
source("R/data_preparation.R") # download_cube_results (auth + read)
source("R/incremental.R")

new_dates <- new_result_dates(
  sheet_play_dates(download_cube_results()),
  existing_result_dates()
)
cat(
  "New result dates:",
  if (length(new_dates)) paste(new_dates, collapse = ", ") else "(none)", "\n"
)

out <- Sys.getenv("GITHUB_OUTPUT", "")
line <- sprintf("new_dates=%s\n", if (length(new_dates) > 0) "true" else "false")
if (nzchar(out)) cat(line, file = out, append = TRUE) else cat(line)
