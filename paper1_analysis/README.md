# COMPASS Paper 1 — analysis pipeline and review index

**The question.** Using the AR6 scenario database, classify mitigation scenarios
into carbon-removal-led (**High-CDR**) and renewables-led (**High-RE**)
archetypes at matched climate ambition, post-process three wellbeing outcomes,
and compare them at World and R10 level. *Does High-RE deliver more wellbeing
than High-CDR?*

> **Read this first.** The **primary specification is ALL CDR** — engineered
> removal (novel CDR + fossil/industrial CCS) **plus land-based removal** —
> because the question is High-RE against High-CDR and an axis that excludes
> land names a subset of what it claims. The **engineered-only** axis is
> reported throughout as the sensitivity, and it is the *conservative* one:
> same direction, smaller advantage. Everything under
> [Superseded](#superseded--the-2020-2050-total-cdr-design) belongs to the
> earlier 2020–2050 design and its results **do not carry over**.

> **Two upstream data defects were found and fixed**, and both moved numbers.
> Neither reversed a finding. See [Data defects](#data-defects) below — do not
> quote `century_outcome_medians_no_land_engineered_cmt.csv` or anything
> derived from it without reading that section first.

**The answer, in one table.** Medians across scenarios, cumulative 2020–2100,
World, full database, all-CDR axis, repaired keys, strict ten-region aggregation:

| outcome | ambition | High-CDR | High-RE | difference | 95% interval |
|---|---|---|---|---|---|
| Energy jobs, job-years per 1,000 | 1.5 °C | 227.60 | 691.52 | **+463.92** | [+380.11, +512.69] |
| | 2 °C | 186.50 | 471.15 | **+284.65** | [+181.22, +337.42] |
| Deprivation gap, GJ per capita | 1.5 °C | 18.43 | 11.62 | **−6.81** | [+1.72, +9.73] |
| | 2 °C | 15.46 | 11.37 | −4.09 | [−0.56, +7.75] |
| PM2.5 mortality, million cumulative deaths | 1.5 °C | 426.23 | 415.12 | **−11.11** | [+3.82, +50.40] |
| | 2 °C | 433.60 | 437.88 | +4.28 | [−20.63, +35.86] |

Positive intervals favour High-RE throughout. **Four of the six World cells clear
the interval, and all four favour High-RE** — jobs at both ambition levels,
deprivation at 1.5 °C, mortality at 1.5 °C. Both failures are at 2 °C.

**Scorecard, nine regions plus World:** 42 of 60 favour High-RE, **36 clearing
the interval**, 4 against. Jobs 20/20 (all significant) · deprivation 14/20
(11 significant, 2 against) · health 8/20 (5 significant, 2 against).

On the **engineered-only sensitivity**: 44 of 60 favour High-RE, 32 clearing,
3 against. The narrower axis favours High-RE in slightly *more* cells and clears
in *fewer*, and it reports a **smaller** advantage at World (jobs +405 against
+464; mortality 8.7 million against 11.1). The headline does not rest on the
wider definition.

**Arms are balanced by construction** — 67/67 at 1.5 °C and 238/238 at 2 °C. The
portfolio rule takes same-size terciles on the same sample and subtracts the same
overlap from each, so the two arms are necessarily equal.

**The binding constraint.** REMIND supplies **73%** of the High-RE arm and under
1% of High-CDR. Seven of ten model families hold both arms (eight on the
engineered axis — COFFEE and GEM drop out when land enters). Every pooled
comparison remains a pathway contrast *plus* a modelling-framework contrast:

| | pooled | within-model support | verdict |
|---|---|---|---|
| Jobs | 20/20, all significant | confirms in every family holding both arms | **a result** |
| Deprivation | 14/20, 11 significant, 2 against | within-model median often disagrees | holds at 1.5 °C; regionally split at 2 °C |
| Health | 8/20, 5 significant, 2 against | thin | Europe holds; World 1.5 °C holds; **Africa reverses significantly** |

> The within-model grid has **not yet been re-run** on the repaired keys and the
> all-CDR axis. It is the one number here still on the older sample.

---

## Data defects

**Defect one — the World aggregation.** The master built the World row by summing
outcomes *within* each deployment-variable group, so a scenario's World total
inherited the regional coverage of whichever CDR variable that row belonged to.
All 288 discrepant scenario-regions showed exactly this pattern. World outcomes
are now built from an outcome-only R10 table, **gated independently per outcome**
on having all ten R10 values, with explicit coverage fields carried through.
Fixed in `V3_world_strict.R`, **ported into
`analysis_scripts/COMPASS_master_analysis_allR10.R`** (`build_df_master()`), and
verified by `V4_verify_port.R` to machine precision.

**Defect two — mangled scenario keys, and this one was larger.**
`engineered_cmt_century_broad_labels.csv` stores degree signs as the **literal
seven-character text `<U+00B0>`** while the master files carry a real UTF-8
degree sign, so `COMMIT-2<U+00B0>C-2020` never equals `COMMIT-2°C-2020`.
`COMPASS_engineered_cmt_century_outcomes_summary.R` joins the two with
`inner_join()`, which drops non-matching rows **silently**: **71 classified
scenarios** — REMIND 34, IMAGE 20, COFFEE 6, WITCH 6, GCAM 3, MESSAGEix 2 —
never reached the outcome tables. Neither normalisation in the codebase repairs
it: `iconv(sub="")` *deletes* the real degree sign on the master side, and
`normalise_id()` has never seen this escape.

Repairing the keys takes all 746 labels from 675 joining to **746 joining**. On
the engineered axis it moves World 1.5 °C jobs from +395.8 to +405.0 (arms 42 v
43 → 53 v 64), turns World 1.5 °C deprivation from non-significant to
significant, and moves the scorecard from 42/60 to 44/60. **No cell reverses
significantly.** Diagnosed and quantified in `V6_key_repair.R`.

> Do not call `enc2utf8()` when normalising these keys. In a C locale it
> re-encodes already-UTF-8 bytes as though they were latin1 and breaks the side
> that was correct.

## Start here for review

| what | file |
|---|---|
| **The deck** | `build_final_deck.js` → `decks/COMPASS_Paper1_final_8.25.pptx` (49 slides) |
| **The figures** | `Z9_century_figs.R` → `figures_century/F1`–`F5` |
| **The rebuild everything rests on** | `V5_land_primary.R` — both axes, repaired keys, strict World |
| **The defect you must know about** | `V6_key_repair.R` — 71 scenarios the published join dropped silently |
| **The one thing to check first** | `W2_within_model.R` — it decides what the paper can claim |
| **The conventions** | "Conventions that matter" at the bottom of this file |

---

## The current pipeline

| script | does | writes |
|---|---|---|
| **`V5_land_primary.R`** | **THE SCRIPT EVERYTHING RESTS ON.** Builds both label sets from the deployment file with the published rule on repaired keys, checks the engineered set reproduces the published classification exactly (606 of 606, approach A), and rebuilds the full grid on both axes with strict World | `LAND_PRIMARY.rds` |
| **`V6_key_repair.R`** | diagnoses the mangled-key defect and quantifies what repairing it moves | `KEYS_REPAIRED.rds` |
| `V3_world_strict.R` | the strict World aggregation (option C), as originally implemented | `STRICT_WORLD.rds` |
| `V4_verify_port.R` | extracts the ported block from the master **verbatim** and confirms it reproduces `V3` to machine precision | — |
| `V2_rebuild_century.R` | the earlier engineered-axis rebuild. **Superseded by `V5`** — it carries the key defect | `CENTURY_RESULTS.rds` |
| `W12_land_sensitivity.R` | the first land in/out comparison, on reconstructed labels. Superseded by `V5`, retained for the churn diagnostics | `W12_LAND.rds` |
| `W13_zeros_and_land_mortality.R` | are the zero-renewable scenarios true zeros? | `W13_ZEROS_LANDMORT.rds` |
| `Z9_century_figs.R` | the figure set `F1`–`F5`, all from `LAND_PRIMARY` | `figures_century/` |
| `build_final_deck.js` | the deck | `decks/COMPASS_Paper1_final_8.25.pptx` |

**Where the numbers come from.** Every number in the deck and every figure comes
from **`LAND_PRIMARY.rds`** — one pipeline, both axes, repaired keys, strict
World. `CENTURY_RESULTS.rds` and `STRICT_WORLD.rds` predate the key repair and
should not be quoted; they are kept because `V4` and `V6` compare against them.

### The World aggregation fix

World outcomes are built from an outcome-only R10 table, **gated independently
per outcome** on having all ten R10 values, with explicit coverage fields
(`n_regions_jobs`, `n_regions_gap`, `n_regions_mortality`, `world_complete_*`).
It is ported into the master and verified by `V4`. Mortality needed no fix: the
reporting-complete pipeline already enforces ten-region completeness upstream.

### The mortality caveat, and it is the important one

The reporting-complete mortality run was **targeted against the engineered
labels**. The 80 scenarios the all-CDR axis newly admits have **no mortality run
at all** — 0 of 80. That is target selection, not a property of those scenarios,
and it is why the mortality arms on the primary axis (37 v 37 at 1.5 °C) are
smaller than on the engineered axis (42 v 37). **Re-running the mortality targets
against the all-CDR labels is the highest-value outstanding job.**

### Sensitivities that a reviewer will ask for

| question | answer | where |
|---|---|---|
| Why is land-based removal in the CDR axis? | Because the question names CDR and land-based removal is CDR. In Latin America, whose removal is overwhelmingly land-based, an engineered-only axis makes carbon management nearly invisible. | `V5` |
| Did you pick the axis that flattered the result? | The narrower axis is the **conservative** one and it agrees: World 1.5 °C jobs +405 against +464, mortality 8.7 m against 11.1 m, and 20/20 jobs cells significant on both. Both are reported. | `V5`, `F3` |
| How much do the two axes actually differ? | Of 530 scenarios classified under both, **not one switches arms**. The axes differ by which scenarios they admit (+80 / −76), not by how they label a shared one. They correlate 0.898. | `V5` |
| What does the wider axis cost? | Model families holding both arms falls 8 → 7: COFFEE loses its High-CDR arm and GEM drops out. MESSAGEix gains 55 High-CDR scenarios — the land-heavy pathways an engineered axis cannot see. | `V5` |
| Are the zero-renewable scenarios true zeros? | **No.** None has a Renewable Capacity row at all; the pivot's zero fill invented the value. 14 of 67 High-CDR at 1.5 °C, 28 of 238 at 2 °C, **none** in High-RE. SCI vetting already excludes every one. | `W13`, `F5` |
| Does dropping them change a headline? | Not any more. On the engineered axis it was the difference between a non-significant and a significant World deprivation result; on the all-CDR axis that cell already clears. | `W13` |

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
| `W10_sectors.R` | which technologies carry the result? | The renewable build is the entire jobs result (+451 of +464). High-CDR **wins** fossil (−22), nuclear (−10) and bioenergy (−10) and loses anyway — so the result is not an artefact of counting fossil job destruction. |
| `W11_world_drivers.R` | which regions drive the World number? | Jobs is broad-based. Deprivation is a genuine regional split, not a weak global effect. Europe carries the mortality result (4.61 m avoided, 29%). |
| `W1_novel_cdr_coverage.R` | is Novel CDR really ~0%? | No — a missing-row artefact, the same class of defect as the zero-renewables problem. |

## Figures

Built by `Z9_century_figs.R`, written to `figures_century/`.

| fig | shows |
|---|---|
| `F1_scorecard.png` | 42 of 60, % change in the median, every region × ambition × outcome |
| `F2_world.png` | the three World cells in native units with their intervals |
| `F3_land.png` | all-CDR (primary) vs engineered-only (sensitivity), every cell |
| `F4_coverage.png` | classified → complete jobs / deprivation / mortality |
| `F5_zeros.png` | what the non-reporting scenarios do to the High-CDR arm |

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
- **CDR axis = ALL removal**: Novel CDR + fossil/industrial CCS **+ land-based
  removal**. The engineered-only axis is the sensitivity.
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
- **Inference:** 2,000-replicate cluster bootstrap over **335 model ×
  scenario-family clusters**, on the raw **difference in medians** — the quantity
  the tables actually print. Cliff's delta is retained only for the within-model
  test, where the question is genuinely about rank overlap.
- **World is strict:** computed only when all ten R10 regions are present for
  *that* outcome, gated per outcome. Partial sums are `NA`, never reported.

## Known limitations, stated because they are load-bearing

1. **Model composition remains the binding constraint** — 73% vs under 1%
   REMIND, and seven of ten families holding both arms rather than eight. A property of AR6, not of this analysis, and it
   limits what any study classifying AR6 by technology can claim.
2. **Forty-two scenarios enter the High-CDR arm on a renewables value they never
   reported.** The zero fill treats a missing row as zero deployment. Retaining
   them is conservative for jobs and anti-conservative for deprivation.
3. **Mortality runs on a restricted sample, and on this axis the restriction is
   worse.** Its targets were drawn against the engineered labels, so the 80
   scenarios the all-CDR axis newly admits have no mortality run at all.
4. **The deprivation measure truncates at zero**, so it responds only to sectors
   where a region falls short — not always the household sector. It is a regional
   aggregate and cannot say who inside a region is deprived.
5. **The outcome set is energy-system centric, and including land-based removal
   makes that cut harder.** The analysis now scores land-heavy pathways on jobs,
   energy access and air quality while saying nothing about land competition,
   food prices, tenure or biodiversity — the channels through which land-based
   removal most plausibly affects wellbeing. Fair on what it measures, silent on
   what it does not.

## Still open

- **Re-run the mortality targets against the all-CDR labels.** 80 newly
  classified scenarios have no mortality output because the target list was
  drawn under the engineered axis. This limits a headline result, not just a
  robustness check — it is the top priority.
- **Re-run the within-model grid** (`W2_within_model.R`) on the repaired keys and
  the all-CDR axis. It is the only number in this index still on the older
  sample, and it decides how strongly each claim can be stated.
- **Fix the labels file at source.** The mangled degree signs should be corrected
  where the file is written, and
  `COMPASS_engineered_cmt_century_outcomes_summary.R`'s `inner_join()` should
  fail loudly on unmatched keys rather than dropping them silently.
- **Decide how to report the zero-renewable scenarios.** Retained with the zero
  fill is the published behaviour; on the all-CDR axis dropping them no longer
  changes a headline, so this is now presentational.
- **RE_SPEC definition sensitivity** (`re_spec_sensitivity.R`) — whether the
  renewables axis should include nuclear or biomass. Needs two master re-runs.
