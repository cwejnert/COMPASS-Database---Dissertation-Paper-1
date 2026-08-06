# COMPASS Master Analysis — Paper 1

`COMPASS_master_analysis.R` unifies the COMPASS database analysis into a single
script that produces the **final scenario sample set** under ten approaches
(A–J) and compares them. The High-CDR vs High-RE classification is identical
across all ten — three things vary between approaches: the **scenario filter**,
the **ambition definition**, and the **classification cut**.

| Approach | Scenario filter (vetting) | Ambition split | Top fraction |
|----------|---------------------------|-----------------|--------------|
| **A / F** | none (all scenarios)                 | AR6 category — C1/C2 = 1.5C, C3/C4 = 2C | 1/3 tercile / 1/2 median |
| **B / G** | none (all scenarios)                 | Median peak warming — ≤1.7°C = 1.5C, 1.7–2.0°C = 2C | 1/3 tercile / 1/2 median |
| **C / H** | full SCI vetting list                | AR6 category | 1/3 tercile / 1/2 median |
| **D / I** | full SCI vetting list                | Median peak warming | 1/3 tercile / 1/2 median |
| **E / J** | partial SCI — technological feasibility (solar + wind + CDR) | AR6 category | 1/3 tercile / 1/2 median |

A–E use the top-tercile (top 1/3) cut for High-CDR/High-RE classification;
F–J repeat the same five filter/ambition combinations at an above-median
(top 1/2) cut, roughly doubling group sizes — most useful where tercile n is
small (the full-SCI-vetted approaches C/D → H/I).

## How it works

- **Stage 1 (once):** loads `compass_interp.rds` + metadata, builds the R10
  timeseries, computes the expensive annual outcome tables (rfasst mortality,
  DLE gap/headcount/implied-CO₂, energy jobs) and the CDR/RE deployment metrics.
- **Stage 2 (per approach):** filter scenarios → assign ambition → classify
  High-CDR/High-RE at the approach's top fraction (within each ambition group)
  → cumulate outcomes to the ambition window (1.5C→2060, 2C→2075) → build that
  approach's `df_master`, at **R10-region and aggregated ("World" = 5-region
  sum) resolution**, with **both absolute and population-normalised
  (per-capita) outcome columns**.
- **Stage 3:** cross-approach comparison tables.

Because the outcomes are computed once, the ten approaches are directly
comparable — same underlying numbers, different sample sets.

### Population normalisation

Denominator is the fixed 2020 population (median across scenarios) per R10
region; the aggregate ("World") row uses the 5-region total. Per-capita
columns sit alongside the absolute columns in the same `df_master` — nothing
is dropped:

| Absolute column | Per-capita column | Unit |
|---|---|---|
| `cumulative_deaths_mln` | `mort_per_1k` | PM2.5 deaths per 1,000 pop |
| `mean_headcount_millions` | `headcount_pct` | % of population below DLS |
| `cumulative_gap_EJ` | `gap_GJ_pc` | GJ per capita |
| `cumulative_implied_CO2_GtCO2` | `implied_CO2_tpc` | tCO2 per capita |
| `jobs_Renewables` | `re_jobs_per_1k` | RE total-employment person-years per 1,000 pop |
| `jobs_Fossil` | `fossil_jobs_per_1k` | Fossil total-employment person-years per 1,000 pop |
| (derived) `jobs_Renewables - jobs_Fossil` | `net_re_jobs_per_1k` | Net RE jobs per 1,000 pop |

Jobs are **total energy-sector employment** (Rutovitz-style), cumulated over the
ambition window as person-years: **build** jobs (construction + manufacturing)
applied to capacity *additions*, plus **O&M + fuel** jobs (oem + extraction +
refinery) applied to installed *capacity stock* each year. (A prior version
applied only the O&M factor to additions — a dimensional mismatch that captured
neither build nor true O&M/fuel employment.) Fuel/extraction employment is
proxied per installed GW (constant-capacity-factor approximation), consistent
with the per-GW `job_factors_complete` table; geothermal factors, absent from
that table, are added from GEA (2015) values distributed by a regional labour
multiplier.

## Outputs

Per approach `X` (X in A–J), under `OUT_DIR/approach_X/`:
`compass_master_dataset_X.{rds,csv}` (all regions + World, absolute + per-capita),
`compass_pathway_tercile_X.{rds,csv}`,
`compass_scenario_set_X.csv`, `compass_cdr_cumulative_X.csv`.

Cross-approach, under `OUT_DIR/comparison/`:
`approach_scenario_counts.csv`, `approach_pathway_counts.csv`,
`approach_scenario_membership.csv`, `approach_summary.csv`,
`approach_set_overlap.csv` (pairwise Jaccard of selected scenario sets).

The per-approach file names/objects mirror the originals, so the figure scripts
can consume any single approach by pointing `df_master` /
`compass_pathway_tercile.rds` at that approach's subfolder.

## Figures (`compare_approaches_figures.R`)

Reads all ten approach folders + the comparison tables and produces:

- **Sample-set size (S1–S5):** selection funnel, pathway counts, approach
  overlap (Jaccard), AR6-vs-peak-warming ambition split, and tercile-vs-median
  paired group-size comparison (e.g. C vs H).
- **Results, World level (R1–R2):** per-capita outcome distributions
  (High-CDR vs High-RE), split into a tercile batch (A–E) and a median batch
  (F–J) for direct comparison, plus a contrast forest plot across all ten
  approaches with Mann-Whitney significance and Cliff's delta.
- **Results, regional breakdown (R3):** one figure per approach × headline
  outcome (mortality, deprivation, DLE gap, net RE jobs), faceted by ambition
  × region (5 R10 regions + World), so you can see which regions drive the
  contrast.

Figures use per-capita units; the exported `approach_outcome_contrast.csv`
carries both per-capita and absolute medians/differences.

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

Other knobs at the top: the `approaches` tribble's `top_frac` column (1/3 =
tercile for A–E, 1/2 = median for F–J), `WINDOW_15C`/`WINDOW_2C` (outcome
cumulation windows), and the `COMPASS_DIR` / `AR6_DIR` / `OUT_DIR` paths.

## Originals

The five source scripts are preserved unmodified in `original_scripts/` for
reference.
