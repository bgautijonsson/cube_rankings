# Cube Rankings Analysis Pipeline
# This script runs the complete analysis pipeline in the correct order

cat("=== Cube Rankings Analysis Pipeline ===\n\n")

# Step 1: Data Preparation
cat("Step 1: Preparing data...\n")
source("R/data_preparation.R")
cat("✓ Data preparation complete\n\n")

# Step 2: Model Fitting
cat("Step 2: Fitting Bradley-Terry model...\n")
source("R/model_fitting.R")
cat("✓ Model fitting complete\n\n")

# Step 3: Visualization
cat("Step 3: Creating visualizations...\n")
source("R/visualization.R")
cat("✓ Visualization complete\n\n")

cat("=== Analysis Pipeline Complete ===\n")
cat("Check the plots/ directory for output visualizations.\n")
