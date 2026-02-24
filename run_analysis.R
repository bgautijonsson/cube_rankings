# Cube Rankings Analysis Pipeline
# This script runs the complete analysis pipeline in the correct order

cat("=== Cube Rankings Analysis Pipeline ===\n\n")

# Load functions
source("R/data_preparation.R")
source("R/model_fitting.R")
source("R/visualization.R")

# Step 1: Data Preparation
cat("Step 1: Preparing data...\n")
prep_results <- run_data_preparation()
stan_data <- prep_results$stan_data
players <- prep_results$players
processed_data <- prep_results$processed_data
cat("✓ Data preparation complete\n\n")

# Step 2: Model Fitting
cat("Step 2: Fitting Bradley-Terry model...\n")
fit <- run_model_fitting()
cat("✓ Model fitting complete\n\n")

# Step 3: Visualization
cat("Step 3: Creating visualizations...\n")
run_analysis_plots <- render_visualizations(
  fit = fit,
  players = players,
  processed_data = processed_data
)
cat("✓ Visualization complete\n\n")

cat("=== Analysis Pipeline Complete ===\n")
cat("Check the plots/ directory for output visualizations.\n")
