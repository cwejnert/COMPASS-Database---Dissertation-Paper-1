# COMPASS Paper 1 — analysis pipeline and review index

**The question.** Using the AR6 scenario database, classify mitigation scenarios
into carbon-management-led (**High-CMT**) and renewables-led (**High-RE**)
archetypes at matched climate ambition, post-process three wellbeing outcomes,
and compare them at World and R10 level. *Does High-RE deliver more wellbeing
than High-CMT?*

> **Read this first.** This index describes the **revamped design** — engineered
> CMT (land-based removal excluded), cumulative **2020–2100**, and
> reporting-complete mortality. Everything under
> [Superseded](#superseded--the-2020-2050-total-cdr-design) below belongs to the
> earlier 2020–2050 Total-CDR design and its results **do not carry over**. The
> older scripts are retained for provenance, not for citation.

**The answer, in one table.** Medians across scenarios, cumulative 2020–2100,
World, full database, strict ten-region aggregation:

| outcome | ambition | High-CMT | High-RE | difference | 95% interval |
|---|---|---|---|---|---|
| Energy jobs, job-years per 1,000 | 1.5 °C | 290.97 | 686.78 | **+395.81** | [+284.72, +507.84] |
| | 2 °C | 173.29 | 465.97 | **+292.68** | [+147.15, +410.87] |
| Deprivation gap, GJ per capita | 1.5 °C | 18.67 | 12.98 | −5.69 | [−1.21, +12.48] |
| | 2 °C | 15.58 | 11.27 | −4.31 | [−0.38, +8.05] |
| PM2.5 mortality, million cumulative deaths | 1.5 °C | 423.82 | 415.12 | **−8.69** | [+0.97, +46.90] |
| | 2 °C | 433.70 | 436.73 | +3.03 | [−19.41, +29.80] |

Positive intervals favour High-RE throughout. **Seven of the eight World cells
favour High-RE; three clear the interval.**

**Scorecard, nine regions plus World:** 42 of 60 favour High-RE, **30 clearing
the interval**, 4 against. Jobs 20/20 (all 20 significant) · deprivation 13/20
(6 significant, 3 against) · health 9/20 (4 significant, 1 against).

**Arms are balanced by construction** — 64/64 at 1.5 °C and 239/239 at 2 °C. The
portfolio rule takes same-size terciles on the same sample and subtracts the same
overlap from each, so the two arms are necessarily equal.

**The binding constraint, and it still shapes every conclusion.** REMIND supplies
**71%** of the High-RE arm and 1% of the High-CMT arm — materially better than
the 85% / 1% of the previous design, but not resolved. MESSAGEix now holds 44
High-CMT against 37 High-RE, and eight of ten families hold both arms. Every
pooled comparison remains a pathway contrast *plus* a modelling-framework
contrast, which is why the three outcomes end with three different epistemic
statuses:

| | pooled | within-model support | verdict |
|---|---|---|---|
| Jobs | 20/20, all significant | 6 of 6 families at 2 °C; mixed at 1.5 °C | **a result** |
| Deprivation | 13/20, 6 significant, 3 against | within-model median often disagrees | a *regional* result, not a global one |
| Health | 9/20, 4 significant, 1 against | thin — 1 family at 1.5 °C, 3 at 2 °C | Europe holds; World 1.5 °C holds; no universal claim |

---

## Start here for review

| what | file |
|---|---|
| **The deck** | `build_final_deck.js` → `decks/COMPASS_Paper1_final_8.25.pptx` (49 slides) |
| **The figures** | `Z9_century_figs.R` → `figures_century/F1`–`F5` |
| **The rebuild everything rests on** | `V2_rebuild_century.R` — reproduces the published medians exactly |
| **The one thing to check first** | `W2_within_model.R` — it decides what the paper can claim |
| **The conventions** | "Conventions that matter" at the bottom of this file |

---

## The revamped design — the current pipeline

| script | does | writes |
|---|---|---|
| **`V2_rebuild_century.R`** | **rebuilds the whole result grid** on the engineered-CMT / 2020–2100 design with cluster-robust intervals. Self-checks against the published medians (max \|Δ\| = 0) | `CENTURY_RESULTS.rds` |
| **`V3_world_strict.R`** | implements the strict World aggregation (option C) and rebuilds World on it | `STRICT_WORLD.rds` |
| `V4_verify_port.R` | extracts the ported block from the master **verbatim**, runs it on the published R10 rows, and confirms it reproduces `V3` to machine precision | — |
| `W12_land_sensitivity.R` | re-runs the classification with land-based removal **in** the CMT axis | `W12_LAND.rds` |
| `W13_zeros_and_land_mortality.R` | are the zero-renewable scenarios true zeros? does mortality move on the land axis? | `W13_ZEROS_LANDMORT.rds` |
| `Z9_century_figs.R` | the figure set `F1`–`F5` | `figures_century/` |
| `build_final_deck.js` | the deck | `decks/COMPASS_Paper1_final_8.25.pptx` |

**Where the numbers come from.** Regional cells come from `CENTURY_RESULTS.rds`;
**World cells come from `STRICT_WORLD.rds`**, which is the only thing the
aggregation fix changed. `Z9` splices them the same way the deck does, so the
figures and the tables quote identical intervals rather than two independently
seeded bootstrap draws.

### The World aggregation fix

The master built the World row by summing outcomes *within* each
deployment-variable group, so a scenario's World total inherited the regional
coverage of whichever CDR variable that row belonged to. All 288 discrepant
scenario-regions showed exactly this pattern. World outcomes are now built from
an outcome-only R10 table, **gated independently per outcome** on having all ten
R10 values, with explicit coverage fields (`n_regions_jobs`, `n_regions_gap`,
`n_regions_mortality`, `world_complete_*`) carried through.

It is **ported into `analysis_scripts/COMPASS_master_analysis_allR10.R`** —
`build_df_master()` — and verified by `V4`. Impact: 15 partial sums became `NA`,
48 values changed, 1,302 unchanged. World jobs at 1.5 °C is **unchanged**; 2 °C
moves 461.44 → 465.97. Every regional cell is untouched by construction.
Mortality needed no fix: the reporting-complete pipeline already enforces
ten-region completeness upstream.

### Sensitivities that a reviewer will ask for

| question | answer | where |
|---|---|---|
| What if land-based removal goes back into the CMT axis? | **No scenario switches arms** (0 of 530). Axes correlate 0.898. The High-RE jobs advantage gets *larger* (+396 → +456), so excluding land is the **conservative** choice. Model composition is worse on the land axis (GEM 25 → 0, COFFEE 12 → 0). | `W12`, `W13`, `F3` |
| Are the zero-renewable scenarios true zeros? | **No.** All 50 have *no* Renewable Capacity row at all — the pivot's zero fill invented the value. 16 of 64 High-CMT at 1.5 °C, 29 of 239 at 2 °C, **none** in High-RE. Concentrated in GCAM (33 of 50); SCI vetting already excludes every one. | `W13`, `F5` |
| Does dropping them change a headline? | Yes, one. Jobs weakens slightly and still clears (+395.8 → +378.4). **Deprivation becomes significant at both levels** (1.5 °C −5.69 n.s. → −8.45 [+2.51, +17.33]; 2 °C −4.31 n.s. → −6.87 [+0.60, +9.11]). Currently reported as a labelled sensitivity, not as the default. | `W13` |

## The interrogation — "but is it real?"

| script | asks | headline finding |
|---|---|---|
| **`W2_within_model.R`** | does the result survive inside a single modelling framework? | Jobs: **6 of 6 families agree at 2 °C**; mixed at 1.5 °C. Deprivation: within-model median often disagrees with the pooled direction. Health: too thin to ask properly. |
| `W9_spec_landscape.R` | which specification gives the strongest result? | The maximum runs on the smallest sample; it scores highest *because* it is smallest. |
| `W4_label_basis.R` | global tercile or per-region? | 3.6% of labels change and REMIND's share is unmoved, so it fixes nothing that matters. |
| `W5_aggregation_order.R` | cumulate-then-median, or median-then-cumulate? | Differs by 0.2–2.5%; reverses no cell. Cumulate-per-scenario is required for inference regardless. |
| `Z3_label_coherence.R` | does the global label describe each region? | Yes except Pacific OECD — excluded from the regional display, retained in the World aggregate. |

## Mechanism — the "why"

| script | asks | finding |
|---|---|---|
| `W10_sectors.R` | which technologies carry the result? | The renewable build is the entire jobs result (+371 of +396). High-CMT **wins** bioenergy (−44), fossil (−25) and nuclear (−17) and loses anyway — so the result is not an artefact of counting fossil job destruction. |
| `W11_world_drivers.R` | which regions drive the World number? | Jobs is broad-based. Deprivation is a genuine regional split, not a weak global effect. Europe carries the mortality result (4.61 m avoided, 29%). |
| `W1_novel_cdr_coverage.R` | is Novel CDR really ~0%? | No — a missing-row artefact, the same class of defect as the zero-renewables problem. |

## Figures

Built by `Z9_century_figs.R`, written to `figures_century/`.

| fig | shows |
|---|---|
| `F1_scorecard.png` | 42 of 60, % change in the median, every region × ambition × outcome |
| `F2_world.png` | the three World cells in native units with their intervals |
| `F3_land.png` | engineered vs with-land axis, every cell, identical outcome data |
| `F4_coverage.png` | classified → complete jobs / deprivation / mortality |
| `F5_zeros.png` | what the non-reporting scenarios do to the High-CMT arm |

---

## Superseded — the 2020–2050 Total-CDR design

Kept for provenance. **These results do not carry over** and should not be quoted.

- **The old result grid:** `U1_final_results.R`, `W6_raw_effects.R`
  (`RAW_RESULTS.rds`), `W8_asreported.R`, `W3_window_2100.R`,
  `Y1`–`Y12` figures in `figures/`, `where_high_re_wins.html`.
- **The ammonia problem** — `nh3_*.R`, `W7_impute_nh3.R`, `nh3_final_grid.R`.
  Ammonia was 6–12% of PM2.5 mortality in most models and **0.15% in REMIND**
  (its agricultural emissions live in MAgPIE and never reach `Emissions|NH3`),
  flattering High-RE by ≈9%. The harmonised re-run was the fix at the time.
  **It is now obsolete:** requiring all five precursors to be reported *directly*
  at R10 removes the asymmetry at source, and also retires the synthetic-regions
  risk, by dropping the scenarios that caused both rather than patching them.
- **Earlier audit trail:** `T1b`, `T2`, `T3`/`T4`, `S1`–`S4`, `Q2`, `V1_figs.R`,
  `Z1_readiness.R`, `Z4_final_table.R`, `AUDIT_V2.md`. `ingest_bergero.R` and
  `re_spec_sensitivity.R` are parked scaffolds.

---

## Data files

**Published outputs** (in the repo):

| file | contents |
|---|---|
| `final_outcomes/century_outcome_medians_no_land_engineered_cmt.csv` | the published outcome medians |
| `final_outcomes/century_outcome_within_model_no_land_engineered_cmt.csv` | the within-model grid |
| `final_outcomes/engineered_cmt_century_broad_labels.csv` | the published classification labels |
| `final_outcomes/mortality_reporting_complete_*` | the reporting-complete mortality run |
| `master_outputs/approach_{A,C}/compass_master_dataset_*.csv` | master dataset, full database / SCI-vetted |
| `master_outputs/approach_{A,C}/compass_cdr_cumulative_*.csv` | cumulative deployment by axis — the classification input |

**Results** (regenerable; excluded from git by `.gitignore`):

`CENTURY_RESULTS.rds` · `STRICT_WORLD.rds` · `W12_LAND.rds` ·
`W13_ZEROS_LANDMORT.rds` · `W2_WITHIN.rds` · `W9_SPEC.rds` · `W10_SECTORS.rds` ·
`W11_DRIVERS.rds`

---

## Regions shown in the by-region results

Nine regions plus World. **Pacific OECD is excluded from the regional display**
and retained inside the World aggregate: the global label does not describe local
behaviour there, and it is the only region where the jobs result fails at both
ambition levels. This is a display decision, not a data exclusion — nothing is
discarded.

## Conventions that matter

- **Window 2020–2100** for *both* the classification and the outcomes.
- **CMT axis = engineered removal**: Novel CDR + fossil/industrial CCS.
  **Land-based removal excluded.**
- **RE axis = cumulative renewable capacity** — wind, solar, hydro, geothermal.
  Nuclear and biomass excluded; biomass is the substrate of BECCS, so counting it
  would let one scenario score on both axes.
- **Labels:** top tercile on the focal axis **and not** top tercile on the
  opposing axis, within ambition band, quantile type 7. Scenarios high on both
  are genuinely both and are dropped. This is what makes the arms balanced.
- **Sign convention:** positive *always* means High-RE is better. The two
  lower-is-better outcomes (deprivation gap, mortality) are sign-flipped.
- **Jobs are job-years** — one person employed for one year. A *stock of work*
  over the period, not a headcount at any moment.
- **Mortality** is reported as **million cumulative deaths**, not per 1,000.
- **Per capita** uses a **fixed base-period population** (7,625 million across
  the ten R10 regions), identical in every scenario, so it rescales levels and
  cannot touch a contrast.
- **Ambition:** AR6 C1+C2 → 1.5 °C, C3+C4 → 2 °C.
- **Inference:** 2,000-replicate cluster bootstrap over **290 model ×
  scenario-family clusters**, on the raw **difference in medians** — the quantity
  the tables actually print. Cliff's delta is retained only for the within-model
  test, where the question is genuinely about rank overlap.
- **World is strict:** computed only when all ten R10 regions are present for
  *that* outcome, gated per outcome. Partial sums are `NA`, never reported.

## Known limitations, stated because they are load-bearing

1. **Model composition remains the binding constraint** — 71% vs 1% REMIND,
   improved from 85% vs 1%. A property of AR6, not of this analysis, and it
   limits what any study classifying AR6 by technology can claim.
2. **Fifty scenarios enter the High-CMT arm on a renewables value they never
   reported.** The zero fill treats a missing row as zero deployment. Retaining
   them is conservative for jobs and anti-conservative for deprivation.
3. **Mortality runs on a restricted sample by design** — 422 of 643 classified
   targets; 150 rejected rather than filled. The restriction is the right call,
   but the mortality arms are smaller and the comparison less precise.
4. **The deprivation measure truncates at zero**, so it responds only to sectors
   where a region falls short — not always the household sector. It is a regional
   aggregate and cannot say who inside a region is deprived.
5. **Excluding land-based removal sharpens the comparison but narrows it.** The
   paper compares two energy-system strategies cleanly and says nothing about
   land-based removal, which is a large part of many real mitigation portfolios
   and carries its own distributional consequences.

## Still open

- **How to report the zero-renewable scenarios.** Retained with the zero fill is
  the published behaviour; dropping them makes World deprivation significant at
  both ambition levels. This is the one open item that changes a headline, and it
  is a reporting decision rather than a technical one.
- **Report within-model results alongside pooled ones** in the paper, not only
  the SI. Six of six families agreeing on jobs at 2 °C is the most persuasive
  number in the study and it is currently buried.
- **RE_SPEC definition sensitivity** (`re_spec_sensitivity.R`) — whether the
  renewables axis should include nuclear or biomass. Needs two master re-runs.
- **A land-based CDR companion analysis.** The excluded pathways are not
  uninteresting; they are a different paper with a different outcome set — land
  competition, food prices and tenure rather than jobs and air quality.
