# Cube Rankings Historical Analysis with Temporal Model
#
# Fits the time-varying Bradley-Terry model incrementally:
# - Checks which dates exist in data vs results/
# - Only fits models for missing dates
# - Each date folder contains the model fit using data up to that date
#
# This allows incremental updates: when new game nights are added,
# only the new date(s) need to be fit.
#
# Usage:
#   Rscript run_historical_analysis.R          # incremental (default)
#   Rscript run_historical_analysis.R --force  # refit all dates

cat("=== Cube Rankings Temporal Analysis ===\n\n")

# Parse --force flag from command line
args <- commandArgs(trailingOnly = TRUE)
force_refit <- "--force" %in% args

if (force_refit) {
  cat("Force mode: will refit ALL dates.\n\n")
}

source("R/data_preparation.R")
source("R/model_fitting.R")
source("R/elo_table.R")

# Download and prepare complete dataset
cat("Downloading and preparing complete dataset...\n")
d_raw <- download_cube_results()

# Get all unique dates in the data
all_dates <- d_raw |>
  mutate(date = as_date(date)) |>
  distinct(date) |>
  arrange(date) |>
  pull(date)

cat("Found", length(all_dates), "unique play dates in data.\n")

# Check which dates already have results
results_root <- "results"
dir.create(results_root, showWarnings = FALSE)

existing_dirs <- if (dir.exists(results_root)) {
  list.dirs(results_root, recursive = FALSE, full.names = FALSE)
} else {
  character(0)
}
existing_dates <- existing_dirs[grepl("^\\d{4}-\\d{2}-\\d{2}$", existing_dirs)]

cat("Found", length(existing_dates), "dates with existing results.\n")

# Find dates to process
if (force_refit) {
  dates_to_fit <- sort(as.character(all_dates))
} else {
  dates_to_fit <- sort(setdiff(as.character(all_dates), existing_dates))
}

if (length(dates_to_fit) == 0) {
  cat("\nAll dates have results. Nothing to do.\n")
  cat("Use --force to refit all dates.\n")
  quit(save = "no")
}

cat(
  "\n", if (force_refit) "Refitting" else "Missing results for",
  length(dates_to_fit), "date(s):\n"
)
cat("  ", paste(dates_to_fit, collapse = ", "), "\n\n")

# Compile the model once (reused for all fits)
cat("Compiling temporal model...\n")
model <- cmdstan_model("stan/bradley_terry_temporal.stan")
cat("Model compiled.\n\n")

# Fit model for each missing date
for (i in seq_along(dates_to_fit)) {
  current_date <- dates_to_fit[i]
  cat(
    "=== Processing date:",
    current_date,
    "(",
    i,
    "/",
    length(dates_to_fit),
    ") ===\n"
  )

  # Prepare data up to this date
  prepared <- prepare_cube_data_temporal(d_raw, upto_date = current_date)

  # Check if we have enough data
  if (prepared$stan_data_temporal$N < 10 || prepared$stan_data_temporal$K < 3) {
    cat(
      "  Skipping (insufficient data: N=",
      prepared$stan_data_temporal$N,
      ", K=",
      prepared$stan_data_temporal$K,
      ")\n\n"
    )
    next
  }

  cat(
    "  Data: K=",
    prepared$stan_data_temporal$K,
    "players, ",
    "N=",
    prepared$stan_data_temporal$N,
    "games, ",
    "T=",
    prepared$stan_data_temporal$T,
    "dates\n"
  )

  # Set up results directory
  result_dir <- file.path(results_root, current_date)
  dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)

  fit_path <- file.path(result_dir, "fitted_model.rds")

  # Fit the model
  cat("  Fitting model...\n")

  fit <- model$sample(
    data = prepared$stan_data_temporal,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1500,
    iter_sampling = 3000,
    adapt_delta = 0.9,
    refresh = 100,
    show_messages = TRUE
  )

  # Check for issues
  # if (fit$num_divergences() > 0) {
  #   cat("  Warning:", fit$num_divergences(), "divergent transitions\n")
  # }

  # Save outputs
  fit$save_object(file = fit_path)
  save_prepared_datasets(prepared, output_dir = result_dir)

  # Save player summary CSV with ELO, cube effects, and per-cube stats
  save_player_summary_csv(
    fit = fit,
    players = prepared$players,
    cube_types = prepared$cube_types,
    output_dir = result_dir,
    processed_data = prepared$processed_data
  )

  # Print summary for this date
  phi_summary <- fit$summary(c("phi_present", "phi_absent"))
  cat(
    "  phi_present:",
    round(phi_summary$median[1], 3),
    ", phi_absent:",
    round(phi_summary$median[2], 3),
    "\n"
  )

  cat("  Saved to:", result_dir, "\n\n")
}

cat("=== Historical Analysis Complete ===\n")
cat("Results saved to:", results_root, "\n")
cat("Run 'quarto render' in mtgkubbur.github.io to update the website.\n")
