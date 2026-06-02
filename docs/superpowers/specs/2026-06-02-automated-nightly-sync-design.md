# Automated nightly sync — design

- **Date:** 2026-06-02
- **Status:** Approved direction; awaiting spec review
- **Scope:** Phase A only. Phase B (Ice Cube cube-preference pod assignment) is a separate design-only spec under `skra/docs/superpowers/specs/`.
- **Repos touched:** `cube_rankings` (code + workflow), `skra` (docs), `mtgkubbur.github.io` (retirement), parent `MagicTheGathering/` (docs — not version-controlled).

## Problem

New cube results and `Cube calendar` edits in the source-of-truth Google Sheet
(`1bq5DXQs1nobk0nu9cN-4UOHPkcPK3fvkTLa2t2lVNKk`) currently reach the live site
(mtgkubbur.is, a Fly app) only via the daily 07:00 UTC `fit.yml` cron or a manual
`republish.yml` dispatch. We want a clean, low-maintenance mechanism that:

1. Refits the Bradley-Terry model **only when new cube results exist**.
2. Updates published site data (calendar, head-to-head, per-cube, etc.) on **any**
   change **without** running the model.
3. Stays simple — a scheduled poll, **no webhooks**, no Apps Script, no new secrets.

## Decision

Poll-based, not event-driven. GitHub polls the Sheet nightly; the model runs only for
new result dates; `publish.R` runs every night regardless so calendar/correction edits
flow to the site. This was chosen over a webhook/`repository_dispatch` design (Apps
Script + skra dispatch) because:

- The latency target relaxes to **next-morning**, which matches the existing runbook
  expectation ("Next morning (rebuild rankings)") and the live in-night coverage already
  provided by standings.mtgkubbur.is.
- It avoids an Apps Script bound to the user's Google account, a skra code change, and two
  new PATs/secrets.
- `cube_rankings` is a **public** repo, so the nightly poll is free.

If lower latency is ever wanted, the cron frequency is a one-line change (still free); a
webhook can be layered on later. Not built speculatively (YAGNI).

### Why this is mostly a refinement, not a new system

`run_historical_analysis.R` is already incremental (fits only dates without a
`results/<date>/` dir), and `fit.yml` already runs `publish.R` unconditionally each night
with commit/push gated on "if changed". So "model only for new results" and "calendar
flows without the model" are *already* true. The only genuine gaps:

1. The Stan toolchain (CmdStan cache restore + fit step) spins up even on no-op nights.
   This spec **gates** it behind an explicit new-results check, so calendar-only/quiet
   nights skip Stan entirely — making the intent legible and the run lighter.
2. Every runbook still tells the user to rebuild **manually on a laptop** and render the
   **superseded** `mtgkubbur.github.io` Quarto site. These docs are corrected.
3. `mtgkubbur.github.io` is an orphaned public site (stale since 2026-05-15; its `CNAME`
   claims `mtgkubbur.is`, a domain Fly now serves). It is retired.

## Architecture / data flow

```
Google Sheet (Results + Cube calendar tabs)
        |
        v   (nightly 07:00 UTC cron, or manual dispatch)
   fit.yml  ── check: new result dates?  ──no──┐
        |                                       |
       yes (or force)                           |
        v                                        v
   [CmdStan + incremental fit]            (skip Stan toolchain)
        |                                        |
        └──────────────> publish.R <─────────────┘   (ALWAYS runs)
                              |
                  data/publish/*.json  (opt-in filtered)
                              |
            commit-if-changed to cube_rankings (results/, data/publish/)
                              |
            rsync + push JSON to mtgkubbur/mtgkubbur.is  (if changed)
                              |
                   mtgkubbur.is deploy.yml (pytest + ruff + validate_publish.py)
                              |
                        flyctl deploy  -> live
```

The shared `concurrency: group: fit` (already on both workflows) keeps a manual
`republish.yml` and the nightly `fit.yml` from racing on the git push.

`republish.yml` is **retained unchanged** as the manual "publish now" escape hatch (no
model) — e.g. for a calendar edit a user wants live immediately rather than next morning.

## Components

### 1. `R/incremental.R` (new) — pure, testable date logic

Single source of truth for "which play dates still need a fit", shared by the fit script
and the CI check so they can never disagree.

```r
# Pure helpers for incremental fitting: which play-dates still need a model fit.

# Distinct play dates in the raw Results sheet, ascending, as character "YYYY-MM-DD".
sheet_play_dates <- function(d_raw) {
  d_raw |>
    dplyr::mutate(date = lubridate::as_date(date)) |>
    dplyr::distinct(date) |>
    dplyr::arrange(date) |>
    dplyr::pull(date) |>
    as.character()
}

# Date-named result dirs already on disk, as character "YYYY-MM-DD".
existing_result_dates <- function(results_root = "results") {
  dirs <- if (dir.exists(results_root)) {
    list.dirs(results_root, recursive = FALSE, full.names = FALSE)
  } else {
    character(0)
  }
  dirs[grepl("^\\d{4}-\\d{2}-\\d{2}$", dirs)]
}

# Sheet dates with no fitted result dir yet (sorted): the set the model must fit.
new_result_dates <- function(all_dates, existing_dates) {
  sort(setdiff(as.character(all_dates), as.character(existing_dates)))
}
```

### 2. `run_historical_analysis.R` (refactor) — reuse the shared logic

Replace the inline date-detection block (current L33-60) with calls to the new helpers, so
the fit's "what to fit" decision is identical to the CI check's. Behaviour unchanged:

```r
source("R/incremental.R")
# ...
all_dates     <- sheet_play_dates(d_raw)
existing_dates <- existing_result_dates(results_root)
dates_to_fit  <- if (force_refit) sort(as.character(all_dates)) else new_result_dates(all_dates, existing_dates)
```

### 3. `scripts/check_new_dates.R` (new) — emits the CI gate decision

Auth path is identical to the fit (`download_cube_results()` -> `cube_gs4_auth()`), so it
works under CI's `GCP_SA_JSON` and locally under `GOOGLE_MAIL`. Sources only
`R/data_preparation.R` + `R/incremental.R` — no cmdstanr.

```r
suppressPackageStartupMessages({ library(tidyverse); library(lubridate) })
source("R/data_preparation.R")   # download_cube_results (auth + read)
source("R/incremental.R")

new_dates <- new_result_dates(
  sheet_play_dates(download_cube_results()),
  existing_result_dates()
)
cat("New result dates:",
    if (length(new_dates)) paste(new_dates, collapse = ", ") else "(none)", "\n")

out  <- Sys.getenv("GITHUB_OUTPUT", "")
line <- sprintf("new_dates=%s\n", if (length(new_dates) > 0) "true" else "false")
if (nzchar(out)) cat(line, file = out, append = TRUE) else cat(line)
```

### 4. `.github/workflows/fit.yml` (edit) — gate the Stan steps

Insert a check step after `setup-r-dependencies`, before the CmdStan cache step:

```yaml
      - name: Detect new result dates
        id: check
        run: Rscript scripts/check_new_dates.R
```

Add an `if:` to the four model-only steps (`Cache CmdStan installation`,
`Install CmdStan if not cached`, `Cache compiled Stan model`, `Fit ...`). Gate expression
(mirrors the existing `inputs.force == true` usage so manual force still refits):

```yaml
        if: steps.check.outputs.new_dates == 'true' || inputs.force == true
```

(The `Install CmdStan if not cached` step keeps its existing cache-miss condition AND-ed
with the gate.) `Run tests`, `Publish JSON`, `Commit outputs if changed`, and
`Push published JSON to mtgkubbur.is` stay **ungated** — they run every night.

`GCP_SA_JSON` is already a job-level env var, so the check step needs no extra `env:`.

### 5. `tests/test_incremental.R` (new) — TDD, auth-free, Stan-free

Same standalone `stopifnot()` + `cat("PASS: ...")` style as `test_publish.R`:

```r
options(width = 120)
suppressPackageStartupMessages({ library(tidyverse); library(lubridate) })
source("R/incremental.R")

d <- tibble::tibble(
  date = as.Date(c("2026-05-14", "2026-05-07", "2026-05-14")),
  cube = "X", player1 = "A", player2 = "B",
  game1 = "A", game2 = "A", game3 = NA_character_
)
stopifnot(identical(sheet_play_dates(d), c("2026-05-07", "2026-05-14")))
cat("PASS: sheet_play_dates distinct + sorted + character\n")

stopifnot(identical(new_result_dates(c("2026-05-14", "2026-05-21"), "2026-05-14"), "2026-05-21"))
stopifnot(identical(new_result_dates("2026-05-14", "2026-05-14"), character(0)))
cat("PASS: new_result_dates returns unfit dates only\n")

tmp <- tempfile(); dir.create(tmp)
dir.create(file.path(tmp, "2026-05-14")); dir.create(file.path(tmp, "notadate"))
stopifnot(identical(sort(existing_result_dates(tmp)), "2026-05-14"))
cat("PASS: existing_result_dates filters to date-named dirs\n")
```

Add `Rscript tests/test_incremental.R` to the `Run tests` step in `fit.yml` (and
optionally `republish.yml`).

## Invariants preserved (must not break)

- **Opt-in privacy filter** — `publish.R` calls `opted_in_players()` on every run; both the
  gated and ungated paths run `publish.R`, so the `elo_table == TRUE` filter always applies.
- **JSON contract** — `data/publish/` -> `mtgkubbur.is/data/kubbur/`; mtgkubbur.is
  `deploy.yml` runs `validate_publish.py` before deploy.
- **Game-level results contract** — `match_id`/`event_id` remain ignored by the model.
- **Fly auto-deploy** — unchanged (push to mtgkubbur.is master triggers `deploy.yml`).
- **Concurrency** — shared `group: fit` continues to serialise fit vs republish.
- **Secrets** — no new secrets; `GCP_SA_JSON` + `MTGKUBBUR_PUSH_TOKEN` unchanged.

## Documentation updates

1. **`skra/README.md`** — rewrite the "Next morning (rebuild rankings)" section: there is
   nothing to do by hand; the nightly `fit.yml` refits new dates and publishes to
   mtgkubbur.is automatically; to update sooner, run the workflow manually
   (`gh workflow run "Fit Cube Rankings" -R bgautijonsson/cube_rankings`). Remove the
   `cd mtgkubbur.github.io && ./update_rankings.sh` instruction and the "cmdstanr stays on
   your machine — never in CI" line.
2. **`MagicTheGathering/.claude/skills/rebuild/SKILL.md`** — repurpose `/rebuild` to kick
   and watch the automated flow (`gh workflow run` + `gh run watch`, then verify
   mtgkubbur.is) instead of running the local pipeline + `quarto render` of the retired
   github.io site.
3. **`MagicTheGathering/CLAUDE.md`** — update the `/rebuild` row, the "Cross-Project Rebuild
   Sequence", the "Preview Servers" (drop the retired mtgkubbur Quarto preview), and the
   repo table to reflect mtgkubbur.is as the live frontend and github.io as retired.

## github.io retirement (destructive — execute only on explicit OK)

`mtgkubbur.github.io` (PUBLIC, not archived). Steps, in order (Pages-disable + commits
**before** archive, since archiving makes the repo read-only):

```bash
# 1. Add a short "retired — see mtgkubbur.is" note to README and remove the stale CNAME.
git -C mtgkubbur.github.io rm CNAME docs/CNAME
#    (+ edit README.md with the retirement note)
git -C mtgkubbur.github.io commit -m "docs: retire site — superseded by mtgkubbur.is (Fly)"
git -C mtgkubbur.github.io push
# 2. Disable GitHub Pages so it stops building/serving.
gh api -X DELETE repos/mtgkubbur/mtgkubbur.github.io/pages
# 3. Archive (read-only).
gh repo archive mtgkubbur/mtgkubbur.github.io --yes
```

DNS already points `mtgkubbur.is` at Fly, so there is no serving cutover — this only
removes the orphan.

## Testing & verification

- **Unit (TDD):** write `tests/test_incremental.R` first (fails — no `R/incremental.R`),
  then add `R/incremental.R`, then it passes. Refactor `run_historical_analysis.R` and
  re-run the full `tests/` to confirm no regression.
- **Live end-to-end (only after spec review + on OK; never fire live workflows before
  that):**
  - *Calendar path:* edit a `Cube calendar` cell, run `republish.yml` (or the nightly
    sync), confirm `calendar.json` updates and mtgkubbur.is reflects it within minutes
    (`gh run watch` + a live check). Revert the edit.
  - *Results path:* append a result under a throwaway `event_id` (or the `Scratch` tab per
    skra's `RESULT_TAB` convention), confirm the check reports a new date, the gated fit
    runs, and the site updates. **Clean up the test row afterwards.**
- **No-op proof:** a manual `fit.yml` run with no new dates should show the four Stan steps
  skipped and still publish/commit-if-changed.

## Risks & mitigations

- **Check vs fit disagree** -> eliminated by sharing `R/incremental.R`.
- **Sheet read fails in the check step** -> step errors, workflow fails loudly (same blast
  radius as today's fit reading the sheet); the daily cron retries next day.
- **Forgotten "end the night" in skra** -> not relevant here; per-match sync already lands
  rows in the Sheet, and the nightly poll picks them up regardless.
- **Node 20 action deprecation warning** (actions/cache@v4, actions/checkout@v4, forced to
  Node 24 on 2026-06-16) -> noted, out of scope; track separately.

## Out of scope

- Phase B (Ice Cube cube-preference pod assignment) — separate design-only spec.
- Bumping cron frequency / adding webhooks — deferred unless latency need arises.
- Upgrading deprecated Node-20 actions — separate maintenance task.
