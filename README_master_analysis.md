# COMPASS Master Analysis — Paper 1

`COMPASS_master_analysis.R` unifies the COMPASS database analysis into a single
script that produces the **final scenario sample set** under five approaches
(A–E) and compares them. The High-CDR vs High-RE (top-tercile) classification is
identical across all five — only the **scenario filter** and the **ambition
definition** change.

| Approach | Scenario filter (vetting)            | Ambition split |
|----------|--------------------------------------|----------------|
| **A**    | none (all scenarios)                 | AR6 category — C1/C2 = 1.5C, C3/C4 = 2C |
| **B**    | none (all scenarios)                 | Median peak warming — ≤1.7°C = 1.5C, 1.7–2.0°C = 2C |
| **C**    | full SCI vetting list                | AR6 category |
| **D**    | full SCI vetting list                | Median peak warming |
| **E**    | partial SCI — technological feasibility (solar + wind + CDR) | AR6 category |

## How it works

- **Stage 1 (once):** loads `compass_interp.rds` + metadata, builds the R10
  timeseries, computes the expensive annual outcome tables (rfasst mortality,
  DLE gap/headcount/implied-CO₂, energy jobs) and the CDR/RE deployment metrics.
- **Stage 2 (per approach):** filter scenarios → assign ambition → classify
  High-CDR/High-RE terciles (within each ambition group) → cumulate outcomes to
  the ambition window (1.5C→2060, 2C→2075) → build that approach's `df_master`.
- **Stage 3:** cross-approach comparison tables.

Because the outcomes are computed once, the five approaches are directly
comparable — same underlying numbers, different sample sets.

## Outputs

Per approach `X`, under `OUT_DIR/approach_X/`:
`compass_master_dataset_X.{rds,csv}`, `compass_pathway_tercile_X.{rds,csv}`,
`compass_scenario_set_X.csv`, `compass_cdr_cumulative_X.csv`.

Cross-approach, under `OUT_DIR/comparison/`:
`approach_scenario_counts.csv`, `approach_pathway_counts.csv`,
`approach_scenario_membership.csv`, `approach_summary.csv`,
`approach_set_overlap.csv` (pairwise Jaccard of selected scenario sets).

The per-approach file names/objects mirror the originals, so the figure scripts
can consume any single approach by pointing `df_master` /
`compass_pathway_tercile.rds` at that approach's subfolder.

## Metadata columns used (confirmed against `compass_r10_meta.csv`)

All vetting and ambition classification comes straight from the COMPASS
metadata — column names are hardcoded (with light fallback detection):

- **AR6 category** (A, C, E): `Climate Category|AR6 [Name]` → parsed to C1–C4.
- **Median peak warming** (B, D): `Climate Assessment|Peak Warming|Median
  [MAGICCv7.5.3]`. Thresholds `WARMING_15C_MAX = 1.7`, `WARMING_2C_MAX = 2.0`.
- **Full SCI vetting** (C, D): `Vetting|SCI 2025`, keeping values in
  `SCI_VET_PASS` (`"ok"`). `"failed"` / `"insufficient reporting"` / blank drop.
- **Technological feasibility** (E): `Feasibility Concern|...|World|2030` for
  `Solar PV Capacity`, `Onshore Wind Capacity`, and `Carbon Capture`. Values are
  coded `ok` / `medium` / `high` / blank. A scenario is dropped only if flagged
  `high` on any of solar, wind, or CDR (`TECHFEAS_FAIL_VALUES = "high"`,
  `TECHFEAS_REQUIRE_ALL = TRUE`). Add `"medium"` to `TECHFEAS_FAIL_VALUES` for a
  stricter E.

### Reference sample sizes (metadata C1–C4 universe = 947 scenarios)

These are the classification-side counts; the runnable universe is the
intersection with scenarios that have R10 timeseries in `compass_interp.rds`.

| Approach | Filter                     | n scenarios | 1.5C | 2C |
|----------|----------------------------|-------------|------|----|
| A / B    | none                       | 947         | 304 / 364 | 643 / 583 |
| E        | partial (tech-feasibility) | 401         | 96   | 305 |
| C / D    | full SCI (`ok`)            | 152         | 24 / 34 | 128 / 118 |

(A/C/E use AR6-category ambition; B/D use peak-warming ambition — hence the two
values in the 1.5C / 2C columns.) E sits between none and full, as intended.

Other knobs at the top: `TOP_FRAC` (tercile = 1/3), `WINDOW_15C`/`WINDOW_2C`
(outcome cumulation windows), and the `COMPASS_DIR` / `AR6_DIR` / `OUT_DIR`
paths.

## Originals

The five source scripts are preserved unmodified in `original_scripts/` for
reference.
