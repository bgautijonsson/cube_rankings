# Backfill player summary CSVs for all existing results
#
# Run this once to generate player_summary.csv files for
# results directories that were created before this feature
# was added.
#
# Usage: Rscript backfill_summaries.R

cat("=== Backfilling Player Summary CSVs ===\n\n")

source("R/elo_table.R")

backfill_player_summaries(results_root = "results", force = FALSE)

cat("\nDone.\n")
