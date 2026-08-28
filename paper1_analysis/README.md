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

**Read direction first — these are associations, not causal estimates.** The
primary evidence is which way each cell points and how consistently. Intervals
are reported throughout as context on how firmly a cell points; they are not
what the conclusions rest on, and nothing here is a causal effect of choosing
one pathway over another.

**At World, seven of eight cells point to High-RE** — energy employment, the
decent-living gap and the deprived headcount at both ambition levels, and PM2.5
mortality at 1.5 °C. The single exception is mortality at 2 °C, where High-RE
sits 4.3 million deaths higher.

**Scorecard, nine regions plus World:** 42 of 60 **point** to High-RE, 18 to
High-CDR. Jobs 20/20 · deprivation 14/20 · health 8/20. (Of those, 36 clear the
interval and 4 clear it against — context, not the claim.)

> **Three outcome families, not five measures.** The deprivation gap and the
> deprived headcount move together in every cell; counting both would report one
> finding twice.

On the **engineered-only sensitivity**: 44 of 60 favour High-RE, 32 clearing,
3 against. The narrower axis favours High-RE in slightly *more* cells and clears
in *fewer*, and it reports a **smaller** advantage at World (jobs +405 against
+464; mortality 8.7 million against 11.1). The headline does not rest on the
wider definition.

**Arms are balanced by construction** — 67/67 at 1.5 °C and 238/238 at 2 °C. The
portfolio rule takes same-size terciles on the same sample and subtracts the same
overlap from each, so the two arms are necessarily equal.

**The binding constraint is an asymmetry in CONCENTRATION between the arms.**
The High-CDR arm is a genuine multi-model ensemble; the High-RE arm is REMIND
with a fringe:

| | High-CDR | High-RE |
|---|---|---|
| 1.5 °C — largest family | MESSAGEix 37% | **REMIND 88%** |
| 1.5 °C — **effective number of models** | **3.6** | **1.3** |
| 2 °C — largest family | MESSAGEix 31% | **REMIND 69%** |
| 2 °C — **effective number of models** | **4.7** | **2.0** |

Effective number of models = 1 / Σ(share²), the inverse Herfindahl index: how
many models an arm is *really* made of, so an arm that is 88% one model counts
as ~1 however long its tail. A pooled difference between a 3.6-model arm and a
1.3-model arm could be a pathway effect or a REMIND effect, and composition
alone cannot distinguish them.

**The leave-one-out test is what distinguishes them**, and it is decisive
(`W15_arm_composition.R`):

| World cell | with REMIND | without REMIND | residual RE arm | survives? |
|---|---|---|---|---|
| Jobs · 1.5 °C | **+463.9** ✓ | +84.0 | 8 | sign holds; 18% of the gap |
| Jobs · 2 °C | **+284.7** ✓ | **+37.4** ✓ | 73 | **still significant** |
| Deprivation · 1.5 °C | **−6.81** ✓ | +2.77 | 8 | **sign flips** |
| Deprivation · 2 °C | −4.09 | +4.31 | 65 | **sign flips** |
| Mortality · 1.5 °C | **−11.11** ✓ | — | 2 | cannot be scored |

Across all nine regions plus World, **in direction terms**: **jobs** keeps its
sign in **19 of 20** cells (retaining a median 18% of the pooled magnitude);
**deprivation reverses in 8 of 20**; **mortality** cannot be scored at all at
1.5 °C (2 residual scenarios) and reverses in 2 of the 10 scoreable 2 °C cells.

> **This is a sensitivity, not a headline result.** The arm imbalance is a
> property of what AR6 contains and which models report which pathways — an
> artefact of the database, not of the analysis. Removing REMIND leaves 8
> High-RE scenarios at 1.5 °C, so intervals widen and significance is lost
> almost everywhere; that part is lost power and should be discounted. Read it
> as a ceiling on attribution: the employment direction survives it, and the
> other two are bounded by it. It does not change any pooled direction reported
> above.

### Mortality is the most model-dependent of the three

The precursor-reporting gate — the fix for the ammonia asymmetry — culls
families unevenly, and it culls exactly the ones that gave the High-RE arm its
breadth:

| pass rate through the gate | | mortality High-RE arm | |
|---|---|---|---|
| REMIND | **73%** | 1.5 °C REMIND share | 88% → **94.6%** |
| GCAM | 18% | 1.5 °C effective models | 1.3 → **1.1** |
| WITCH | 38% | 2 °C REMIND share | 69% → **82.7%** |
| TIAM · COFFEE | **0%** | 2 °C effective models | 2.0 → **1.4** |

So the mortality High-RE arm is *more* concentrated than the classified one —
effectively a single model at 1.5 °C. Every 1.5 °C mortality cell, including the
Europe result the air-quality section leads with, is **untestable**: dropping
REMIND leaves two scenarios. At 2 °C none of the three significant cells
survives and two reverse. The two data decisions interact unfavourably and
should be reported together.

> **Read this in both directions.** Dropping REMIND leaves 8 High-RE scenarios
> at 1.5 °C, so losing significance there is partly lost power, not proof of a
> confound. But lost power widens the interval around a *stable* estimate — it
> does not move the estimate by 80% or flip signs. Both happen. The defensible
> reading: the jobs **direction** survives its dominant model (significantly so
> at 2 °C, on 73 independent scenarios), the jobs **magnitude** is largely a
> REMIND effect, and deprivation does not survive at all.

Every pooled comparison is therefore a pathway contrast *plus* a
modelling-framework contrast:

| | pooled | within-model | without REMIND | verdict |
|---|---|---|---|---|
| Jobs | 20/20, all significant | **72%** agree | 19/20 keep sign; **18%** of size | **direction is a result; magnitude is not** |
| Deprivation | 14/20, 11 significant, 2 against | **42%** agree | **8 flips; 0 of 13 survive** | an association; attribution unresolved |
| Health | 8/20, 5 significant, 2 against | **25%** agree · 2 families | 2 flips; 1 of 3 survives | Europe only; no general claim |

> **The real limit is ambition, not outcome.** At 1.5 °C only **one** family
> (MESSAGEix, on three High-RE scenarios) holds both arms well enough to be
> asked, so every high-ambition cell — including the headline ones — rests on
> pooling. At 2 °C four families can be asked and jobs holds in 16 of 20 cells.
> Quoting a single agreement rate across both levels hides this. Source:
> `W14_within_model_landprimary.R`.

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
| **The one thing to check first** | `W14_within_model_landprimary.R` — it decides what the paper can claim |
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
| **`W14_within_model_landprimary.R`** | the within-model test on the primary axis; replaces `W2` | `W14_WITHIN.rds` |
| **`W15_arm_composition.R`** | what each arm is made of, and leave-one-family-out — **the decisive robustness test** | `W15_ARMS.rds` |
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
| **`W14_within_model_landprimary.R`** | does the result survive inside a single modelling framework? | Jobs 72% agree (16/20 cells); deprivation 42% (9 conflicts); health 25% (2 families). At 1.5 °C only MESSAGEix can be asked at all. Self-checks that it reproduces the pooled grid exactly before comparing against it. |
| `W2_within_model.R` | the same question on the **superseded** design | Not runnable from a clean checkout — it sources `stratified.R.fns` (`WINDOW <- "2020-2050"`), filters `Variable == "Total CDR"`, takes labels from `pw_*.rds$Pathway_excl`, and needs five gitignored `.rds` inputs. Replaced by `W14`. |
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

## Why Pacific OECD is not shown as a regional row

**It removes no scenarios.** One fixed global classification is applied unchanged
in every region, so the arm sizes are identical everywhere — 58 v 67 at 1.5 °C
and ~212 v 238 at 2 °C, Pacific OECD included. Excluding it is a **display**
decision; it is retained inside the World aggregate, because dropping it there
would change what "World" means.

**The reason is label coherence.** The test is whether the global label describes
local behaviour: Cliff's δ on each region's *own* cumulative renewable capacity,
High-CDR against High-RE.

| region | δ 1.5 °C | δ 2 °C | High-RE ÷ High-CDR median renewables, 1.5 °C |
|---|---|---|---|
| **Pacific OECD** | **−0.11** | +0.03 | **0.88×** |
| Reforming econ. | +0.27 | +0.54 | 1.15× |
| Middle East | +0.73 | +0.79 | 1.67× |
| Europe | +0.83 | +0.83 | 1.66× |
| North America | +0.86 | +0.92 | 1.67× |
| Latin America | +0.88 | +0.87 | 1.75× |
| China+ | +0.89 | +0.71 | 1.57× |
| Africa | +0.90 | +0.82 | 2.94× |
| Rest of Asia | +0.93 | +0.88 | 4.34× |
| India+ | +0.99 | +0.85 | 2.46× |

Everywhere else, scenarios labelled High-RE really do build more renewables in
that region. In Pacific OECD they build slightly **less** at 1.5 °C and the same
at 2 °C — the contrast being measured does not exist locally, so the row would
report an outcome difference between two groups that are not actually different
there. **Reforming economies is the weakest region still shown** (δ +0.27) and is
flagged accordingly.

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

1. **Model composition remains the binding constraint, and it is worst at
   1.5 °C** — REMIND is 88% of the High-RE arm at high ambition (73% pooled),
   and only one family holds both arms well enough to be asked there. A property of AR6, not of this analysis, and it
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
- **Get more model families into the 1.5 °C comparison.** `W14` exposes the real
  limit: at 1.5 °C the arms are almost perfectly segregated by model (REMIND 88%
  of High-RE; IMAGE and WITCH pure High-CDR), and only MESSAGEix — on three
  High-RE scenarios — can be asked within-model. Nothing in this dataset fixes
  that; it is a property of AR6, and plausibly a substantive one, since
  frameworks diverge structurally in how they reach the most stringent target.
  The paper must state it by ambition level rather than quoting a pooled share.
- **Fix the labels file at source.** The mangled degree signs should be corrected
  where the file is written, and
  `COMPASS_engineered_cmt_century_outcomes_summary.R`'s `inner_join()` should
  fail loudly on unmatched keys rather than dropping them silently.
- **Decide how to report the zero-renewable scenarios.** Retained with the zero
  fill is the published behaviour; on the all-CDR axis dropping them no longer
  changes a headline, so this is now presentational.
- **RE_SPEC definition sensitivity** (`re_spec_sensitivity.R`) — whether the
  renewables axis should include nuclear or biomass. Needs two master re-runs.
