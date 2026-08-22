# COMPASS Paper 1 — analysis pipeline and review index

**The question.** Using the AR6 scenario database, classify mitigation scenarios
into carbon-management-led (**High-CMT**) and renewables-led (**High-RE**)
archetypes at matched climate ambition, post-process three wellbeing outcomes,
and compare them at World and R10 level.

**The answer, in one table.** Medians across scenarios, cumulative 2020–2050,
World, full database:

| outcome | ambition | High-CMT | High-RE | difference | 95% interval |
|---|---|---|---|---|---|
| Energy jobs, job-years per 1,000 | 1.5 °C | 6.05 | 13.44 | **+7.39** | [+6.35, +9.13] |
| | 2 °C | 2.43 | 8.87 | **+6.44** | [+5.62, +7.20] |
| Deprivation gap, GJ per capita | 1.5 °C | 13.96 | 10.01 | **−3.95** | [+0.57, +5.87] |
| | 2 °C | 13.83 | 9.56 | **−4.27** | [+0.28, +5.32] |
| PM2.5 mortality, deaths per 1,000 | 1.5 °C | 26.21 | 26.29 | +0.08 | [−0.90, +1.19] |
| | 2 °C | 28.01 | 27.26 | −0.74 | [−3.11, +1.87] |

In people: **+56 million job-years** to 2050 (≈1.9 million sustained jobs) and
**229 million fewer people** below the decent-living energy threshold at 1.5 °C.
No detectable air-quality difference.

**Scorecard, nine regions plus World:** 44 of 60 favour High-RE, 32 clearing the
interval, 8 against. Jobs 20/20 · deprivation 16/20 · health 8/20.

**The binding constraint, and it shapes every conclusion.** REMIND supplies 85%
of the High-RE arm and 1% of the High-CMT arm (95% and 0% in the mortality
subsample). Every pooled comparison is a pathway contrast *plus* a
modelling-framework contrast. The within-model test (`W2`) is what separates
them, and it is why the three outcomes end with three different epistemic
statuses:

| | pooled | model families agreeing | verdict |
|---|---|---|---|
| Jobs | 20/20 | **83%** | a result |
| Deprivation | 16/20 | **42%** | an association, attribution unresolved |
| Health | 8/20 | 60% (2 families only) | no difference |

---

## Start here for review

| what | file |
|---|---|
| **The deck** | `build_final_deck.js` → `COMPASS_Paper1_final.pptx` (66 slides) |
| **The results grid** | `where_high_re_wins.html` — every cell, all four designs |
| **The conventions** | "Conventions that matter" at the bottom of this file |
| **The one thing to check first** | `W2_within_model.R` — it decides what the paper can claim |

---

## Core pipeline

| script | does | writes |
|---|---|---|
| `stratified.R.fns` | shared engine: `load_frame()`, `cliff_d()`, region and outcome constants | — |
| `engine.R` | earlier shared helpers, retained for the audit scripts | — |
| `U1_final_results.R` | the original result grid on Cliff's delta | `FINAL_RESULTS.rds` |
| **`W6_raw_effects.R`** | **the published results**: raw medians with a cluster bootstrap on the *difference in medians* | `RAW_RESULTS.rds` |
| `nh3_final_grid.R` | splices ammonia-harmonised mortality into all four designs | `FINAL_RESULTS_NH3.rds` |

**Why `W6` supersedes `U1`.** `U1` put the confidence interval on Cliff's delta,
which measures rank overlap, not the size of the gap. Since the tables report raw
levels, the interval is now computed on the quantity actually printed. It moves
one cell in 60 and it moves it conservatively.

## The interrogation — "but is it real?"

| script | asks | headline finding |
|---|---|---|
| **`W2_within_model.R`** | does the result survive inside a single modelling framework? | Jobs yes (83% of families). Deprivation no (42%). Health cannot be asked. |
| `W9_spec_landscape.R` | which specification gives the strongest result? | Range 47–87%; the maximum runs on six scenarios per arm. Pre-specified primary is 73%. Jobs is 20/20 in every cell of the landscape. |
| `W3_window_2100.R` | would 2100 strengthen it? | No — 44/60 → 39/60, and jobs' within-model agreement falls 83% → 62%. |
| `W4_label_basis.R` | global tercile or per-region? | 3.6% of labels change; REMIND stays at 84% of the High-RE arm, so it fixes nothing that matters. |
| `W5_aggregation_order.R` | cumulate-then-median, or median-then-cumulate? | Differs by 0.2–2.5%; reverses no cell. Cumulate-per-scenario is required for inference regardless. |
| `Z2_strat_fair.R` | is the stratified estimator fair? | Underpowered: at 1.5 °C one family carries 100% of the weight. |
| `Z3_label_coherence.R` | does the global label describe each region? | Yes except Pacific OECD (δ −0.19, −0.07). |

## Mechanism — the "why"

| script | asks | finding |
|---|---|---|
| `W10_sectors.R` | which technologies and end-uses carry the result? | **Solar PV = 85% of the jobs gap**; nuclear is the largest loss at −11%. Deprivation correlates **+0.63 with industrial** final energy and −0.15 with residential. |
| `W11_world_drivers.R` | which regions drive the World number? | Jobs broad-based (top two 50%, none against). Deprivation is Africa 65% + Latin America 47% − Rest of Asia 32%. Mortality null is offsetting regions. |
| `W1_novel_cdr_coverage.R` | is Novel CDR really ~0%? | No — a missing-row artefact. Among the 318 scenarios reporting it, it is 42.9% of the axis. |

## The ammonia problem

Read in this order. This is the one place the database actively misleads.

| script | step |
|---|---|
| `nh3_probe.R` | measures the ammonia share of PM2.5 mortality per model. IMAGE 12.4%, POLES-JRC 9.4%, AIM 8.9%, MESSAGEix 6.4%, **REMIND 0.15%** |
| `nh3_arm_test.R` · `nh3_arm_analysis.R` | shows the effect is *arm-differential*, p = 7.7e-08, because agricultural NH₃ lives in MAgPIE |
| `nh3_harmonised_run.R` | re-runs TM5-FASST with ammonia zeroed for **every** model (≈75 min) → `compass_mortality_r10_noNH3.csv` |
| `nh3_mortality_rebuild.R` | re-cuts mortality on the harmonised file. **Self-check: reproduces the published cells exactly, max \|Δ\| = 0.000** |
| `W8_asreported.R` | computes the as-reported version on the same raw basis, so both sit in one table |
| `W7_impute_nh3.R` | quantifies the three options — revert **+1.67**, harmonise **−0.08**, impute **−1.25** deaths per 1,000 |
| `nh3_which_script.R` · `nh3_check_mapping.R` | diagnostics that resolved the two-copies-of-the-script bug |
| `nh3_synthetic_regions.R` | flags scenarios whose regional emissions were filled in from a World total *(still open — see below)* |

**What to conclude.** "Put the ammonia back" means two opposite things. Reverting
to each model's reported data hands High-RE a +1.67 advantage; imputing the
ammonia REMIND never filed hands it a −1.25 disadvantage. The published
harmonised figure sits between them. On the raw basis the as-reported World
advantage was **never significant** either: +1.67 [−0.12, +3.08].

## Figures

Built by `Y11_raw_figs.R` (Y1, Y2, Y8), `Y1_final_figs.R` (Y3–Y7),
`Y8_nh3_figs.R` (Y9), `W2_within_model.R` (Y10), `Y12_burden.R` (Y12).

| fig | shows |
|---|---|
| `Y1_scorecard.png` | 44 of 60, % change in the median |
| `Y2_world_forest.png` | World gaps in native units with intervals |
| `Y3_robustness.png` | every alternative specification |
| `Y4_jobs_decomposition.png` | build versus demolish |
| `Y5_label_coherence.png` | why Pacific OECD is dropped |
| `Y6_nh3_gap.png` | the ammonia reporting gap by model |
| `Y7_variance.png` | which outcomes can be pooled at all |
| `Y8_nh3_correction.png` | mortality gap before and after harmonising |
| `Y9_nh3_asymmetry.png` | −8.0% vs −0.36% — the 22× asymmetry |
| **`Y10_within_model.png`** | **pooled vs within-model — the decisive figure** |
| `Y12_burden.png` | the 4.7× regional burden gap |

## Earlier audit trail

Superseded but kept for provenance: `T1b` (classification churn), `T2` (sample
and inference), `T3`/`T4` (outcome construction), `S1`–`S4` and `Q2` (window
re-cuts), `V1_figs.R`, `Z1_readiness.R`, `Z4_final_table.R`, `AUDIT_V2.md`.
`ingest_bergero.R` and `re_spec_sensitivity.R` are parked scaffolds.

---

## Data files

**Inputs** (produced by `COMPASS_master_analysis.R`, live on the analysis machine):

| file | contents |
|---|---|
| `ds_A.rds` / `ds_C.rds` | master dataset, full database / SCI-vetted |
| `pw_A.rds` / `pw_C.rds` | pathway labels (`Pathway_excl`), ambition, category |
| `jobs_type.rds` | jobs by fuel × tech group × stream × job type × year |
| `fe_pop.rds` | final energy total / industry / transport, and population |
| `dle_annual.rds` | decent-living gap and headcount by year |
| `mort_annual.csv` | PM2.5 deaths by model × scenario × R10 × year (as reported) |
| `mort_coverage.rds` | precursor coverage per scenario — drives the mortality gate |
| `MORT_WINDOWS.rds` / `DLE_WINDOWS.rds` | pre-cut 2020–2050 and 2020–2100 windows |
| `compass_mortality_r10_noNH3.csv` | the harmonised run's output |

**Results** (regenerable; excluded from git by `.gitignore`):

`RAW_RESULTS.rds` (**the published grid**) · `FINAL_RESULTS_NH3.rds` ·
`NH3_MORT_REBUILD.rds` · `W2_WITHIN.rds` · `W3_WINDOWS.rds` · `W4_LABELS.rds` ·
`W5_AGGORDER.rds` · `W7_IMPUTE.rds` · `W8_ASREPORTED.rds` · `W9_SPEC.rds` ·
`W10_SECTORS.rds` · `W11_DRIVERS.rds`

---

## Regions shown in the by-region results

Nine regions plus World. **Pacific OECD is excluded from the regional display**
and retained inside the World aggregate: High-RE scenarios build no more
renewables there than High-CMT ones (δ −0.19 and −0.07), so the contrast being
measured does not exist locally. **Reforming Economies is retained with a flag**
(δ +0.07 at 1.5 °C — the weakest coherence of any region still shown).

## Conventions that matter

- **Window 2020–2050** throughout, the net-zero window. 2100 is a sensitivity.
- **Sign convention:** positive *always* means High-RE is better. The two
  lower-is-better outcomes (deprivation gap, mortality) are sign-flipped.
- **Jobs are job-years** — decadal values × 10, rectangle integration.
- **Mortality** uses the same ×10 convention; `sum(deaths_pm25 * 10)/1e6`.
- **Deprivation** is a true annual sum; the headcount is a mean.
- **Per capita** uses a **fixed base-period population** (7.63 bn at World),
  identical in every scenario, so it rescales levels and cannot touch a contrast.
- **Ambition:** AR6 C1+C2 → 1.5 °C (136 scenarios), C3+C4 → 2 °C (454).
- **Inference:** 2,000-replicate cluster bootstrap over **312 model × scenario-family
  clusters**, on the raw difference in medians. Design effect ≈ 1.9×.
- **Three families, not five measures.** Jobs ρ = 0.974, deprivation ρ = 0.996.
  Counting five would report two results twice.

## Known limitations, stated because they are load-bearing

1. **Model composition is the binding constraint** — 85% vs 1% REMIND. A
   property of AR6, not of this analysis, and it limits what any study
   classifying AR6 by technology can claim.
2. **The jobs dividend is front-loaded** — O&M is 16% of the gap to 2050,
   rising to 25% by 2100. A construction surge, not a permanent workforce.
3. **The deprivation measure tracks industrial energy** (r = +0.63) more than
   household access (−0.15), because the gap truncates at zero and industry is
   the binding sector. A property of the Kikstra/DESIRE operationalisation.
4. **The outcome set is energy-system centric** — it does not capture land
   competition, the dominant welfare channel for High-CMT in Latin America (73%
   land-based removal) and Africa (49%).
5. **Two mortality cells are unresolvable** — North America and Reforming
   Economies have no model family holding both arms.

## Still open

- **`nh3_synthetic_regions.R` has not been run.** Scenarios reporting emissions
  only at World have their R10 detail filled in by population weight, which
  carries no pathway information. If those concentrate in one arm the regional
  mortality cells need re-cutting. Script is written and reads only.
- **RE_SPEC definition sensitivity** (`re_spec_sensitivity.R`) — needs two
  master re-runs at `low_carbon` and `with_biomass`.
- **Impute ammonia for REMIND** rather than deleting it for everyone. Needs an
  external agricultural ammonia source. Would move the result *further* against
  High-RE, not back.
- **Bergero / State of CDR scenarios** — parked deliberately; adding a
  CDR-focused ensemble moves the tercile thresholds and reclassifies unrelated
  scenarios.
