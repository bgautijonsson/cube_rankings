# AGENTS.md

Lean, local instructions for coding agents in `cube_rankings/`.

## Read Order

1. This file.
2. Local `CLAUDE.md`.
3. `../AGENTS.md` and `../CLAUDE.md`.
4. `../.claude/rules/*.md` (canonical, path-scoped).

## Must-Keep Constraints

- `GOOGLE_MAIL` is required for Google Sheets access.
- Public player-facing outputs must include only opted-in players (`elo_table == TRUE`).
- Keep R changes in existing tidyverse/cmdstanr style.

## Dependency

- Changes here feed `mtgkubbur.github.io/` via symlinked `R/` and `results/`.
