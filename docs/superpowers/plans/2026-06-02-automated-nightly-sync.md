# Automated Nightly Sync Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `fit.yml` run the Stan model only when the Sheet has new result dates, while always republishing site data, and fix the stale runbooks + retire the orphaned github.io site.

**Architecture:** A shared, unit-tested `R/incremental.R` defines "which play-dates need a fit". A new `scripts/check_new_dates.R` uses it to emit a CI gate output; `fit.yml` gates the CmdStan + fit steps behind that output (or `force`), leaving publish/commit/push nightly-unconditional. Docs are corrected; github.io is archived.

**Tech Stack:** R (tidyverse, googlesheets4, cmdstanr), GitHub Actions, bash/gh.

**Spec:** `docs/superpowers/specs/2026-06-02-automated-nightly-sync-design.md`

**Auth note:** `GOOGLE_MAIL` is set locally (do not check it, just run). CI uses `GCP_SA_JSON`.

**Gating note:** Tasks 1-6 are local (commits + parent-tree edits) and safe. Tasks 7-8 (push, live workflow test, github.io retirement) are **outward-facing / destructive** and run **only on the user's explicit OK**.

---

### Task 1: Shared incremental date logic (TDD)

**Files:**
- Test: `tests/test_incremental.R` (create)
- Create: `R/incremental.R`

- [ ] **Step 1: Write the failing test**

Create `tests/test_incremental.R`:

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

- [ ] **Step 2: Run test to verify it fails**

Run: `cd cube_rankings && Rscript tests/test_incremental.R`
Expected: FAIL — `cannot open file 'R/incremental.R': No such file or directory`

- [ ] **Step 3: Write minimal implementation**

Create `R/incremental.R`:

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

- [ ] **Step 4: Run test to verify it passes**

Run: `cd cube_rankings && Rscript tests/test_incremental.R`
Expected: three `PASS:` lines, exit 0.

- [ ] **Step 5: Commit**

```bash
git -C cube_rankings add R/incremental.R tests/test_incremental.R
git -C cube_rankings commit -m "feat: shared incremental date helpers + tests"
```

---

### Task 2: Refactor `run_historical_analysis.R` to reuse the helpers

**Files:**
- Modify: `run_historical_analysis.R` (source block ~L25-27; date-detection block ~L33-60)

- [ ] **Step 1: Add the source line**

Edit the source block so it reads:

```r
source("R/data_preparation.R")
source("R/model_fitting.R")
source("R/elo_table.R")
source("R/incremental.R")
```

- [ ] **Step 2: Replace the date-detection block**

Replace the block from `# Get all unique dates in the data` through the `dates_to_fit <- ...` if/else (current L33-60) with:

```r
# Get all unique play dates in the data
all_dates <- sheet_play_dates(d_raw)

cat("Found", length(all_dates), "unique play dates in data.\n")

# Check which dates already have results
results_root <- "results"
dir.create(results_root, showWarnings = FALSE)
existing_dates <- existing_result_dates(results_root)

cat("Found", length(existing_dates), "dates with existing results.\n")

# Find dates to process (incremental unless --force)
if (force_refit) {
  dates_to_fit <- sort(as.character(all_dates))
} else {
  dates_to_fit <- new_result_dates(all_dates, existing_dates)
}
```

- [ ] **Step 3: Verify the file still parses (no execution, no fit risk)**

Run: `cd cube_rankings && Rscript -e 'invisible(parse(file = "run_historical_analysis.R")); cat("parse OK\n")'`
Expected: `parse OK`

- [ ] **Step 4: Commit**

```bash
git -C cube_rankings add run_historical_analysis.R
git -C cube_rankings commit -m "refactor: run_historical_analysis uses shared incremental helpers"
```

---

### Task 3: CI gate decision script

**Files:**
- Create: `scripts/check_new_dates.R`

- [ ] **Step 1: Write the script**

Create `scripts/check_new_dates.R`:

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

- [ ] **Step 2: Run it (reads the live sheet; no fit)**

Run: `cd cube_rankings && Rscript scripts/check_new_dates.R`
Expected: `New result dates: (none)` then `new_dates=false` (all sheet dates are already fit). If it prints dates, that is a genuinely unfit date — stop and tell the user before proceeding.

- [ ] **Step 3: Commit**

```bash
git -C cube_rankings add scripts/check_new_dates.R
git -C cube_rankings commit -m "feat: check_new_dates.R emits CI gate decision"
```

---

### Task 4: Gate the Stan steps in `fit.yml`

**Files:**
- Modify: `.github/workflows/fit.yml`

- [ ] **Step 1: Insert the check step**

After the `setup-r-dependencies` step and before `Cache CmdStan installation`, insert:

```yaml
      - name: Detect new result dates
        id: check
        run: Rscript scripts/check_new_dates.R
```

- [ ] **Step 2: Gate the four model-only steps**

Add this `if:` to `Cache CmdStan installation`, `Cache compiled Stan model`, and `Fit (skips dates already present unless --force)`:

```yaml
        if: steps.check.outputs.new_dates == 'true' || inputs.force == true
```

For `Install CmdStan if not cached`, AND it with the existing cache condition:

```yaml
        if: (steps.check.outputs.new_dates == 'true' || inputs.force == true) && steps.cache-cmdstan.outputs.cache-hit != 'true'
```

- [ ] **Step 3: Add the incremental test to the test step**

Change the `Run tests` step `run:` to:

```yaml
        run: Rscript tests/test_enrichment.R && Rscript tests/test_publish.R && Rscript tests/test_incremental.R
```

- [ ] **Step 4: Verify the workflow YAML parses**

Run: `cd cube_rankings && python3 -c "import yaml; yaml.safe_load(open('.github/workflows/fit.yml')); print('yaml OK')"`
Expected: `yaml OK`

- [ ] **Step 5: Commit**

```bash
git -C cube_rankings add .github/workflows/fit.yml
git -C cube_rankings commit -m "ci: gate Stan toolchain behind new-results check; run incremental test"
```

---

### Task 5: Fix skra's "Next morning" runbook

**Files:**
- Modify: `skra/README.md` (the `### Next morning (rebuild rankings)` section)

- [ ] **Step 1: Replace the section**

Replace the `### Next morning (rebuild rankings)` heading and its body (the `cd ~/MagicTheGathering/mtgkubbur.github.io` block, the trailing paragraph, and the "cmdstanr stays on your machine — never in CI" line) with:

```markdown
### Next morning (rankings update automatically)

Nothing to do by hand. A nightly GitHub Action (`Fit Cube Rankings` in
`bgautijonsson/cube_rankings`, 07:00 UTC) refits the model for any new result
dates and republishes the data to **mtgkubbur.is** (the Fly frontend). It runs
the model only when there are new results; calendar edits republish without it.

To update sooner, trigger it manually:

​```bash
gh workflow run "Fit Cube Rankings" -R bgautijonsson/cube_rankings
gh run watch -R bgautijonsson/cube_rankings
​```

Every result is already in the sheet the moment skrá saves it, so the action
always sees the latest data.
```

(Remove the literal zero-width characters around the code fence — they are only here to keep this plan's own fence intact.)

- [ ] **Step 2: Verify no stale references remain**

Run: `cd skra && python3 -c "t=open('README.md',encoding='utf-8').read(); import sys; bad=[s for s in ['update_rankings.sh','mtgkubbur.github.io','never in CI'] if s in t]; print('STALE:',bad) if bad else print('clean')"`
Expected: `clean`

- [ ] **Step 3: Commit (local; push is gated Task 7)**

```bash
git -C skra add README.md
git -C skra commit -m "docs: rankings update automatically via CI, not a manual laptop rebuild"
```

---

### Task 6: Repurpose `/rebuild` and update parent CLAUDE.md (parent tree — no commits)

**Files:**
- Modify: `.claude/skills/rebuild/SKILL.md` (parent `MagicTheGathering/`)
- Modify: `CLAUDE.md` (parent `MagicTheGathering/`)

- [ ] **Step 1: Rewrite the `/rebuild` skill body**

Replace the body below the frontmatter of `.claude/skills/rebuild/SKILL.md` with:

```markdown
Trigger and watch the automated cube-rankings rebuild, then verify the live site.

The pipeline runs in GitHub Actions, not locally: the `Fit Cube Rankings` workflow
in `bgautijonsson/cube_rankings` refits any new result dates and publishes JSON to
`mtgkubbur.is` (the Fly frontend). The model runs only when there are new results.

Steps:

1. Trigger the workflow: `gh workflow run "Fit Cube Rankings" -R bgautijonsson/cube_rankings`
2. Watch it: `gh run watch -R bgautijonsson/cube_rankings`
3. If it pushed JSON, the `mtgkubbur.is` repo's `deploy.yml` runs (`validate_publish.py`
   + `flyctl deploy`). Confirm the change is live at https://mtgkubbur.is.
4. Report what changed (new fits, updated rankings, calendar).

Notes:

- Player data is subject to the opt-in requirement in `.claude/rules/data-privacy.md`
  (`elo_table == TRUE`). The pipeline's `publish.R` enforces this on every run.
- For a data-only republish without refitting, run the `Republish JSON` workflow instead.
```

Also update the `allowed-tools` if needed (Bash + Read are sufficient).

- [ ] **Step 2: Update parent `CLAUDE.md`**

Make these focused edits in `MagicTheGathering/CLAUDE.md`:
- Repo table: mark `mtgkubbur.github.io/` as **retired/archived (superseded by mtgkubbur.is)**; ensure `mtgkubbur.is` (Fly frontend) is listed as the live site.
- `/rebuild` command row: "Trigger + watch the cube_rankings CI workflow and verify mtgkubbur.is".
- "Cross-Project Rebuild Sequence": replace the local symlink/`quarto render` description with: data changes are picked up by the nightly `fit.yml` (or a manual dispatch), which publishes JSON to mtgkubbur.is; no local render needed.
- "Preview Servers": drop the retired mtgkubbur (port 4849) Quarto preview; keep rvkicecube.

- [ ] **Step 3: Verify no execution needed**

These are docs in the non-versioned parent tree. No commit. Confirm the files were saved.

---

### Task 7 (GATED — explicit OK): push + live end-to-end test

Do **not** start this task until the user has reviewed the diffs and approved.

- [ ] **Step 1: Rebase + push cube_rankings** (pushing `fit.yml` does NOT fire any run — fit.yml has no `push` trigger)

```bash
git -C cube_rankings pull --rebase origin master
git -C cube_rankings push
```

- [ ] **Step 2: Push skra** (docs-only; a Pages redeploy of unchanged app is harmless)

```bash
git -C skra pull --rebase origin main
git -C skra push
```

- [ ] **Step 3: Live test — calendar path (no model)**

Make a harmless edit to a `Cube calendar` cell in the Sheet, then:

```bash
gh workflow run "Republish JSON" -R bgautijonsson/cube_rankings
gh run watch -R bgautijonsson/cube_rankings
```

Expected: run succeeds, `calendar.json` updates, change appears on mtgkubbur.is within minutes. Revert the Sheet edit afterwards.

- [ ] **Step 4: Live test — results path (model gated on)**

Append one result row under a throwaway `event_id` (or the `Scratch` tab per skrá's `RESULT_TAB` convention) on a NEW date, then:

```bash
gh workflow run "Fit Cube Rankings" -R bgautijonsson/cube_rankings
gh run watch -R bgautijonsson/cube_rankings
```

Expected: the `Detect new result dates` step reports the new date and the Stan steps run; site updates. **Then delete the test row and its `results/<date>/` dir, and re-run to restore clean state.** (Coordinate cleanup with the user — do not leave test data in the Sheet or repo.)

- [ ] **Step 5: No-op proof**

```bash
gh workflow run "Fit Cube Rankings" -R bgautijonsson/cube_rankings
gh run watch -R bgautijonsson/cube_rankings
```

Expected (no new dates): the four Stan steps show as skipped; publish/commit still run.

---

### Task 8 (GATED — explicit OK): retire github.io

Do **not** start until the user approves. Execute in this order (commits before archive):

- [ ] **Step 1: Add retirement note + remove CNAME**

Edit `mtgkubbur.github.io/README.md` to add a top note: "⚠️ Retired — superseded by https://mtgkubbur.is (Fly). This repo is archived." Then:

```bash
git -C mtgkubbur.github.io rm CNAME docs/CNAME
git -C mtgkubbur.github.io add README.md
git -C mtgkubbur.github.io commit -m "docs: retire site — superseded by mtgkubbur.is (Fly)"
git -C mtgkubbur.github.io pull --rebase origin master
git -C mtgkubbur.github.io push
```

- [ ] **Step 2: Disable Pages**

```bash
gh api -X DELETE repos/mtgkubbur/mtgkubbur.github.io/pages
```

- [ ] **Step 3: Archive**

```bash
gh repo archive mtgkubbur/mtgkubbur.github.io --yes
```

- [ ] **Step 4: Verify**

```bash
gh repo view mtgkubbur/mtgkubbur.github.io --json isArchived --jq .isArchived   # true
curl -sI https://mtgkubbur.is/ | grep -i '^server'                              # still Fly
```

---

## Done when

- `tests/test_incremental.R` passes; `fit.yml` gates Stan behind the check; `check_new_dates.R` reports `(none)` against the live sheet.
- skra README, `/rebuild`, and parent `CLAUDE.md` describe the automated mtgkubbur.is flow with no github.io / manual-laptop references.
- (On OK) live calendar + results tests pass and clean up; github.io archived.
