# cube_rankings

R/Stan statistical analysis pipeline — fits Bradley-Terry models to estimate player strength from cube draft results.

## Commands

```bash
# Full pipeline: data prep → model fit → visualization
Rscript run_analysis.R

# Historical analysis: fits model per game date (incremental, skips existing)
Rscript run_historical_analysis.R

# Backfill player summaries for existing result folders
Rscript backfill_summaries.R
```

Set `GOOGLE_MAIL` before any command that needs Google Sheets access.

## Data Pipeline

Google Sheets → `download_cube_results()` → `prepare_cube_data()` → `fit_cube_model()` → `render_visualizations()`

- `R/data_preparation.R` — download, clean, prepare Stan input
- `R/model_fitting.R` — cmdstanr wrapper for Stan models
- `R/visualization.R` — ggplot2-based ranking plots
- `R/elo_table.R` — ELO conversion and player opt-in filtering
- `R/data_check.R` — sanity checks for processed data

## Stan Models

- `stan/bradley_terry.stan` — Basic: P(i wins) = logit(alpha_i - alpha_j)
- `stan/bradley_terry_cubeeffects.stan` — Extended with cube-specific strength offsets
- `stan/bradley_terry_temporal.stan` — Time-varying player strength

## Output

- `results/<date>/` — per-date RDS model fits + `player_summary.csv`
- `plots/` — PNG visualizations
- `data/` — cached processed inputs (`processed_data.rds`, `players.rds`, `game_dates.rds`)

Large fitted artifacts and compiled Stan binaries are gitignored. Do not commit regenerated binaries.

## Conventions

- ELO scale: `bt_to_elo()` converts Bradley-Terry logit to ELO centred at 1500
- Cube power categories: "High" (vintage), "Medium" (modern), "Low" (limited/pauper)
- Historical runs should be executed before rebuilding any downstream site that depends on `results/`

## Dependencies

**R packages:** tidyverse, cmdstanr, posterior, googlesheets4, ggplot2, ggtext, bayesplot, gt

**Stan:** Pre-compiled models in `stan/`; rebuild only if model code changes
