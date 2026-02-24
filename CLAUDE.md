# cube_rankings

R/Stan statistical analysis pipeline — fits Bradley-Terry models to estimate player strength from cube draft results.

## Commands

```bash
# Full pipeline: data prep → model fit → visualization
Rscript run_analysis.R

# Historical analysis: fits model per game date (incremental, skips existing)
Rscript run_historical_analysis.R
```

Requires `GOOGLE_MAIL` environment variable for Google Sheets authentication.

## Data Pipeline

Google Sheets → `download_cube_results()` → `prepare_cube_data()` → `fit_cube_model()` → `render_visualizations()`

- `R/data_preparation.R` — download, clean, prepare Stan input
- `R/model_fitting.R` — cmdstanr wrapper for Stan models
- `R/visualization.R` — ggplot2-based ranking plots
- `R/elo_table.R` — ELO conversion and player opt-in filtering

## Stan Models

- `stan/bradley_terry.stan` — Basic: P(i wins) = logit(alpha_i - alpha_j)
- `stan/bradley_terry_cubeeffects.stan` — Extended with cube-specific strength offsets
- `stan/bradley_terry_temporal.stan` — Time-varying player strength

## Output

- `results/` — per-date RDS model fits + `player_summary.csv`
- `plots/` — PNG visualizations

## Conventions

- ELO scale: `bt_to_elo()` converts Bradley-Terry logit to ELO centered at 1500
- Cube power categories: "High" (vintage), "Medium" (modern), "Low" (limited/pauper)

## Dependencies

**R packages:** tidyverse, cmdstanr, posterior, googlesheets4, ggplot2, ggtext, bayesplot, gt

**Stan:** Pre-compiled models in `stan/`
