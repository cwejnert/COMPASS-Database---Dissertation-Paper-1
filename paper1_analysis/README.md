# Paper 1 analysis pipeline

High-CMT against High-RE mitigation pathways and wellbeing outcomes, across the AR6 R10
regions plus World, at two levels of ambition. Everything is cumulated **2020–2050**, the
net-zero window.

`COMPASS_master_analysis.R` in the repo root produces the inputs these scripts consume
(`compass_pathway_tercile_*.rds`, `compass_master_dataset_*.rds`, `compass_mortality_r10.csv`,
`compass_dle_annual.rds`, `jobs_type.rds`). The scripts here read those from the working
directory; they do not re-run the master.

## Shared engine

| file | role |
|---|---|
| `engine.R` | `load_approach()`, the mortality coverage gate, `cliff_d()` |
| `stratified.R.fns` | `load_frame()` — the single windowed loader every downstream script uses — plus the outcome/family constants and `cell5()` |

`load_frame()` is the only place window logic lives. Anything that cuts a window itself is a bug.

## Window re-cuts

Each outcome is re-cut from its annual/decadal source. Every re-cut was verified to reproduce
the master **exactly** at 2020–2100 before the window was shortened, so the window is a
parameter rather than a fork in the pipeline.

| file | outcome | convention |
|---|---|---|
| `S1_mort2050.R` | PM2.5 mortality | decadal, `sum(deaths_pm25 * 10)/1e6`. Uses `deaths_pm25`, **not** `deaths_total` — the master picks the first of (deaths_pm25, FUSION, deaths_total) that exists |
| `S2_dle2050.R` | energy deprivation | truly annual (81 values). Gap is a **sum**, headcount is a **mean**. `gap_GJ_pc` carries a ×1000 factor (EJ/mln → GJ/cap) |
| `S3_check_x10.R` | — | four tests confirming `deaths_pm25` is an annual rate, so the ×10 decadal multiplier is right |
| `S4_baseyear_gate.R` | — | tests the 2010 base year as an alternative quality gate (it earns nothing, but documents the model-fingerprint problem) |
| `Q2_window2050.R` | jobs | decadal, plain sum. Also rebuilds the World row as the ten-region sum |

## Audit

Run in order; each writes its own `.rds`. See `AUDIT_V2.md` for the findings and what each cost.

| file | asks |
|---|---|
| `T1b_audit_classification.R` | Is the carbon-management axis one construct? Who is excluded? Does the tercile cut matter? Are scenarios independent? |
| `T2_audit_sample_inference.R` | The split threshold sample (26% of scenarios report no renewable capacity) and the cluster bootstrap |
| `T3_audit_outcomes.R` | What `pop_mln` is, whether jobs is the ranking axis rescaled, what the mortality gate removes, and how many independent results there really are |
| `T4_audit_followups.R` | The three things T3 turned up |
| `W1_novel_cdr_coverage.R` | Whether "Novel CDR = 0" is a real zero or a missing row (it is a missing row), and whether that biases the classification axis |

`T1b` supersedes an earlier `T1` whose component and cut-sensitivity checks were **vacuous** —
two labellings that are both non-`NA` for a scenario must agree, because they share the
`hi_re` branch and quantiles are monotone in the cut. The information is entirely in the `NA`
transitions. Don't reintroduce that check.

## Results and outputs

| file | produces |
|---|---|
| `U1_final_results.R` | `FINAL_RESULTS.rds` — every cell, three outcome families, cluster-robust intervals, across four samples (full/vetted × all/matched) |
| `re_spec_sensitivity.R` | The SI definition-sensitivity table the master asks for. Needs master re-runs at `RE_SPEC = low_carbon` and `with_biomass` — it cannot be approximated downstream, since `ds_A.rds` carries only the aggregated Renewable Capacity. Instructions in the file header. |
| `Z4_final_table.R` | the final grid: nine regions plus World, 48/60 |
| `Y1_final_figs.R` | `Y1`–`Y7` figures on the nine-region basis, incl. label coherence and the NH3 gap |
| `V1_figs.R` | superseded by `Y1_final_figs.R`; kept for the 11-region versions |
| `build_final_deck.js` | the 31-slide deck (`npm i pptxgenjs`) |
| `build_brief.py` | assembles `brief.html` from `brief.head.html` + `brief.body.html`, inlining figures as data URIs |

## NH3 sensitivity

**Settled by the probe:** removing NH3 cuts global PM2.5 mortality by about
**11%** (FUSION: -11.1, -10.7, -10.9, -12.4% across four scenarios; GBD -8 to
-10%; GEMM -11 to -13%). NH3 matters, so the byte-identical files were a failed
run, not a null finding.

Cliff's delta is rank-based, so a *uniform* shift would leave it unchanged. The
arm test (37 scenarios, 18 High-CMT / 19 High-RE) shows the shift is **not**
uniform, and the result is the opposite of what the probe suggested:

| arm | median change when NH3 is removed |
|---|---|
| High-CMT | **−8.66%** (IQR −9.35 to −6.39) |
| High-RE | **−0.15%** (IQR −2.02 to −0.14) |

Wilcoxon p = 7.7e-08; Cliff's delta between the two change distributions = 0.912.
**3 of 10 regional mortality cells change sign**, and cells favouring High-RE fall
from 5/10 to 2/10.

The cause is a model-reporting artefact, not a pathway property. NH3's
contribution to PM2.5 mortality by family: IMAGE 12.4%, POLES-JRC 9.4%, AIM 8.9%,
MESSAGEix-GLOBIOM 6.4%, **REMIND-MAgPIE 0.16%, REMIND 0.14%** — a **58×** gap.
Ammonium nitrate and sulfate are typically 20–50% of PM2.5 mass in industrialised
regions, so REMIND at 0.15% is not describing a cleaner world; it is not reporting
agricultural ammonia (which lives in MAgPIE). REMIND is 18 of the 19 High-RE
scenarios sampled, so "High-RE has less ammonia" is really "REMIND reports less
ammonia".

**Consequence: the pooled mortality contrast is not reportable as it stands.**
This is the same Simpson problem already flagged for mortality (4–5% within-model
variance in five regions), now with a concrete quantified mechanism. Jobs and
deprivation are untouched — NH3 has nothing to do with either.

The first attempt returned two byte-identical mortality files (MD5 `bbaa7f67...`).
Tracing `DROP_NH3` through the pipeline, the mechanism *should* work: patch 01d
drops `Emissions|NH3` from `pollutant_map`, so NH3 never reaches `em_clean`, and
the per-scenario backfill then re-adds it at `value_kt = 0` because
`required_pols` still hardcodes it. NH3 does end up zeroed.

That leaves two possibilities, and they need different responses:

- the no-NH3 run never actually re-ran, and the old `03_nh3_run.R` copied the
  untouched main file to the `_noNH3` name — it never checked the file changed; or
- NH3 genuinely does not move PM2.5 mortality through this code path, in which
  case the byte-identical files were correct and the sensitivity is moot.

| file | role |
|---|---|
| `nh3_probe.R` | Runs four scenarios twice each, NH3 as reported and NH3 forced to zero, and compares. About a minute — an rfasst pair is ~0.1 min. Sources the rfasst script only up to `SECTION 5`, so the helpers load without the batch loop starting. Asserts the two emission lists genuinely differ in NH3 *before* running — the guard that was missing. |
| `nh3_arm_test.R` | The decisive test. Samples N scenarios from each arm, runs each with and without NH3, and **recomputes Cliff's delta both ways** — the number that would actually appear in the paper. Also Wilcoxon-tests whether the per-scenario % change differs by arm. |
| `nh3_arm_analysis.R` | Reads `nh3_arm_test_result.rds` and separates the arm effect from the model-family effect. This is where the 58× REMIND gap is quantified. |
| `nh3_harmonised_run.R` | **Use this one.** Zeroes ammonia directly in `em_clean` and drives rfasst itself — no `DROP_NH3` flag, no patch 01d, no text-editing of source files. Runs only the ~590 classified scenarios (~75 min, not five hours), writes `compass_mortality_r10_noNH3.csv` in the master's exact schema, and never touches the main outputs. |
| `nh3_mortality_rebuild.R` | Consumes `compass_mortality_r10_noNH3.csv` and produces the mortality half of the scorecard on the same basis as everything else. Holds the quality gate fixed at the ORIGINAL run (the gate counts NH3 as a precursor, so recomputing it on an ammonia-free run would fail everything, and freezing it keeps the same scenarios in both). Self-checks by re-cutting the original file first: verified to reproduce the published mortality cells with max \|difference\| **0.000** in Cliff's delta across all 22 cells. |
| `nh3_run_checked.R` | Superseded. It inherited the flag-flipping design and fails with `no unique 'DROP_NH3 <- ...' line found` wherever patch 01d never applied — which is also the likeliest reason the original sensitivity produced two identical files. |

Order: `nh3_probe.R` (does NH3 matter at all? — yes, ~11%) -> `nh3_arm_test.R` +
`nh3_arm_analysis.R` (does it move the *contrast*? — yes, and it is a model
artefact) -> `nh3_run_checked.R` for the full sample, which the arm test has now
justified.

**`m3_get_mort_pm25` returns no single mortality column.** It gives one row per
region × year × age × disease with a column per concentration-response function —
`GBD`, `GEMM` and `FUSION`. Anything reading its output must name one of those;
the master's `deaths_pm25` derives from FUSION. The first version of this probe
looked for `mort_pm25`/`value`/`deaths`, found none, and reported "no PM2.5
output" on a run that had in fact succeeded.

## Ingestion

`ingest_bergero.R` appends a new scenario set (Bergero / State of CDR) to
`compass_interp.rds` from an IAMC-format export. It validates variable coverage
against what the master needs, interpolates to annual, and writes
`compass_interp_plus.rds` — it never overwrites the original.

Run the added set as a **separate approach**, not as a replacement for A.
Classification is a tercile within the pooled sample, so adding CDR-focused
scenarios raises the carbon-management threshold and reclassifies scenarios that
have nothing to do with the addition.

## Regions shown in the by-region results

**Pacific OECD is excluded from the regional display** and kept only inside the
World aggregate (a ten-region sum). Reason: `Z3` shows Cliff's delta on that
region's *own* renewable deployment between the two arms is **−0.19** (1.5C) and
**−0.07** (2C) — High-RE builds no more renewables there than High-CMT does.
Scoring a renewables-versus-carbon-management contrast in a region where the two
arms deploy the same renewables is not a weak result; it is not the comparison
the paper claims to make.

Reforming economies is weak but real (+0.26, +0.45) and is **kept**, flagged.

`Z4_final_table.R` produces the final grid on that basis: nine regions plus World.

## Readiness (`Z1_readiness.R`)

One pass over all three families, asking the same five questions: complete grid,
levels, cluster-robust significance, survival of SCI vetting, and survival of the
model-composition guard.

| family | full DB | vetted | direction agrees | var-share median | regions <0.10 | within-model | verdict |
|---|---|---|---|---|---|---|---|
| **Jobs** | 22/22 (20 sig) | 22/22 | 100% | 0.45 | 0 | **0 flips of 22** | **ready** |
| **Deprivation** | 17/22 (18 sig) | 14/22 | 86% | 0.47 | 0 | underpowered — see below | **ready, with a stated limit** |
| **Health** | 14/22 (10 sig) | 12/22 | 73% | 0.12 | 5 | 4 flips of 11 testable | not reportable |

### The within-model check is underpowered for deprivation (`Z2_strat_fair.R`)

An earlier pass reported deprivation's pooled and within-model contrasts
disagreeing in 12 of 22 cells and called the result "not robust". **That was
over-stated.** The stratified estimate is too thin to referee:

- **1.5C**: exactly ONE family qualifies (REMIND, 3 High-CMT against 8 High-RE)
  and carries **100%** of the weight.
- **2C**: four families qualify, but `cliff_strat` weights by `n_cmt * n_re`, so
  MESSAGEix-GLOBIOM carries **52%** of the estimate on just **4** High-RE runs.

Two follow-ups show the flips are an artefact of that thinness, not of the result:

- **Treatment strength is not the driver.** At 2C the High-RE scenarios inside
  both-arms families sit at median `re_depth` 1.19 (barely past the threshold)
  against 1.67 outside — so the stratified test does compare a weaker treatment.
  But depth-matching the *pooled* estimate to the same band changes almost
  nothing: **1 sign flip against 12** for the within-model test.
- Jobs, on the same thin strata, flips **zero** times.

So the within-model check confirms jobs and is simply unable to adjudicate
deprivation. It neither confirms nor refutes it. State that as a limitation;
do not report it as a failure.

### The global label is regionally coherent (`Z3_label_coherence.R`)

The axes are global sums, but the outcomes are regional, so the label has to
describe the region it scores. It does, in 8 of 10 regions: Cliff's delta on
*regional* renewable deployment between the arms runs 0.72–0.99, and on regional
carbon management −0.75 to −1.00 everywhere.

**Two exceptions, and they are the same two regions the jobs decomposition
already flagged:**

| region | re_delta 1.5C | re_delta 2C | reading |
|---|---|---|---|
| Pacific OECD | **−0.19** | **−0.07** | High-RE builds no more renewables here — the label does not describe this region |
| Reforming econ. | +0.26 | +0.45 | weak |

Relabelling per region rather than globally moves deprivation by **2 sign
changes out of 20** (15/20 → 13/20), so the global-versus-regional choice is not
what drives the result.

## Conventions that matter

- **Cliff's delta is signed as ADVANTAGE**: positive *always* means High-RE is better. The
  three lower-is-better outcomes (`mort_per_1k`, `gap_GJ_pc`, `headcount_pct`) are sign-flipped.
- **Three outcome families, not five measures.** The two jobs measures correlate ρ = 0.97 and
  the two deprivation measures ρ = 0.99. The headline counts 66 family-cells; the second measure
  in each family is a within-family check.
- **Significance is cluster-robust.** 590 scenarios sit in 312 model × scenario-family clusters.
  Naive Wilcoxon p-values overstate significance by 13 cells out of 98.
- **Two jobs contrasts only**: `REFOSS` (renewables − fossil) and `LOWC` (renewables + bioenergy
  + nuclear − fossil). An earlier "NET" measure was the gross sum of all four, not a contrast,
  and was dropped as invalid.
- **Missing is not zero.** The carbon-management axis sums its three components
  with `na.rm = TRUE`, so an unreported component scores as a real zero. Only 29%
  of scenarios report Novel CDR at all, and among those that do it is 43% of the
  axis — the largest component, not the smallest. Reporting is a model
  fingerprint (0% in four families, 97% in WITCH). `W1` tests it: rebuilding the
  axis on land + fossil CCS alone moves 9% of scenarios, flips zero arms, and
  moves the headline 79% -> 77%. Keep the published axis; report the two-component
  version as a sensitivity.
- **Jobs are job-years.** Decadal values x10 (rectangle integration), matching the
  mortality convention. The x10 is a pure relabelling: no Cliff's delta and no
  percentage gap changes, only the units.
- **Variance guard**: `share_within = mean(within-family variance) / total variance`. Below 0.10
  the cell is comparing model inventories, not pathways.
