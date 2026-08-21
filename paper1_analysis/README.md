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

`T1b` supersedes an earlier `T1` whose component and cut-sensitivity checks were **vacuous** —
two labellings that are both non-`NA` for a scenario must agree, because they share the
`hi_re` branch and quantiles are monotone in the cut. The information is entirely in the `NA`
transitions. Don't reintroduce that check.

## Results and outputs

| file | produces |
|---|---|
| `U1_final_results.R` | `FINAL_RESULTS.rds` — every cell, three outcome families, cluster-robust intervals, across four samples (full/vetted × all/matched) |
| `V1_figs.R` | `V1`–`V6` figures, all from `FINAL_RESULTS.rds` |
| `build_final_deck.js` | the 26-slide deck (`npm i pptxgenjs`) |
| `build_brief.py` | assembles `brief.html` from `brief.head.html` + `brief.body.html`, inlining figures as data URIs |

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
- **Variance guard**: `share_within = mean(within-family variance) / total variance`. Below 0.10
  the cell is comparing model inventories, not pathways.
