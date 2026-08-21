# Section-by-section audit — COMPASS Paper 1

Everything below was tested, not asserted. Scripts: `T1b`, `T2`, `T3`, `T4`, `U1`.

---

## 1. Classification (T1b)

### Two "100% agreement" results from the first pass were vacuous — retracted

`lab = High-CMT` requires `hi_cmt & !hi_re`; `lab = High-RE` requires `hi_re & !hi_cmt`.
Both alternative labellings shared the same `hi_re`, and quantiles are monotone in `q`.
So any two labellings that are both non-`NA` for a scenario **must** agree — disagreement is
impossible by construction. The old check filtered to the non-`NA` intersection and then
measured agreement on it. All the information is in the `NA` transitions.

Replaced with **set-membership churn** (counting moves in and out of the classified set)
and, more usefully, with **re-running the headline** under each alternative.

### Findings

| Test | Result | Verdict |
|---|---|---|
| Are the three CMT components one construct? | Spearman between components −0.63 to 0.85; median share Land 35%, FossilCCS 51%, **Novel CDR 0%** | Axis is really *land CDR + fossil CCS*. Novel CDR contributes nothing at the median and is degenerate as a standalone axis (684 High-CMT / 0 High-RE). |
| Single-component axis instead of the sum | fccs-only: 20% churn; land-only: 25%; **zero flips** in every case | No scenario ever crosses from High-CMT to High-RE. All churn is in/out of the classified set. Reassuring. |
| Tercile cut 0.50 / 0.60 / 2⁄3 / 0.75 | headline 84% / 84% / 82% / 81%; jobs 22/22 at every cut | Conclusion is cut-insensitive. |
| Sign changes between the 0.50 and 0.75 cut | 7 of 110, all with \|δ\| ≤ 0.36 at one end and ≈0 at the other, concentrated in mortality and Middle East | The unstable cells are the ones already flagged as weak. |
| Who is excluded | 1.5C: 133 neither-high + 9 both-high; 2C: 561 + 78. GEM-E3, MESSAGE-GLOBIOM, POLES, WITCH-GLOBIOM 100% excluded | The comparison is of **two corners**, not a spectrum. Median 2C deployment: High-CMT 647k CMT / 1.60M RE; High-RE 100k / 3.13M. Must be described as a corner contrast. |

---

## 2. Sample and inference (T2)

### (A) The two terciles are computed on different samples — real, and now quantified

**367 of 1,425 scenarios (26%) report no Renewable Capacity at all.** `re_thresh` is a
tercile of 1,058 scenarios; `cdr_thresh` is a tercile of 1,394. That — not anything
substantive — is why the arms are **335 / 255** instead of equal.

Missing-RE families: MESSAGE-GLOBIOM, POLES, WITCH-GLOBIOM (100%), GCAM (74%), AIM (44%),
IMAGE (34%). Their median CDR is **0**, so they are mostly scenarios reporting neither axis.

Recomputing both thresholds on the 1,027 common-support scenarios gives balanced arms
(1.5C 62/62, 2C 224/224) and **92% cell agreement**.

| | cells | High-RE wins | median δ |
|---|---|---|---|
| current (split sample) | 110 | 93 (85%) | 0.494 |
| common support | 110 | 90 (82%) | 0.468 |

Three cells change sign, all with \|δ\| ≤ 0.06. **Conclusion unaffected; report as sensitivity.**

Separately verified: `total_cdr` is a clean ten-region sum — ratio to the ten-region sum
is exactly **1.000**, so the World row is *not* double-counted.

### (B) Non-independence is real and it costs significance

590 classified scenarios sit in **312 model × stem clusters** — design effect ≈ **1.9×**.
Largest: REMIND-MAgPIE ENGAGE-NPi2020 (n=30), POLES-JRC ENGAGE-NPi2020 (n=26).

Cluster bootstrap (2,000 reps, resampling whole clusters) vs naive Wilcoxon + BH:

| | significant |
|---|---|
| naive Wilcoxon + BH | 98 / 110 |
| cluster bootstrap 95% CI | **85 / 110** |
| lost | 13 |
| gained | 0 |

The 13 losses are concentrated exactly where they should be: 7 of 13 are mortality cells.
**All significance in the paper should be cluster-robust.** Nothing that was insignificant
becomes significant, so this is a one-directional correction — it only ever costs claims.

---

## 3. Outcome construction (T3, T4)

| Check | Finding |
|---|---|
| `pop_mln` | **Constant across every scenario** — a fixed base-period vector totalling 7,625 mln (≈2018). Per-capita is a pure rescaling: Cliff's delta on raw jobs totals equals Cliff's delta per capita to **0.000** in all 22 cells. Good for the contrast; it does mean cross-*region* level comparisons are on ~2018 population shares (Africa 14%, not its ~25% 2050 share). |
| Jobs — is it capacity rescaled? | No. jobs-per-capacity IQR/median 0.31–0.59 by region; overall ρ(capacity, RE jobs) = 0.78. |
| Jobs — circularity | ρ(RE axis, REFOSS) 0.40–0.90. The axis is *global*, the outcome *regional*, so this is transmission, not tautology. But ρ(CMT axis, REFOSS) is **−0.48 to −0.62**, not ≈0 — the axes are anti-correlated, which should be stated rather than called a clean control. |
| Mortality gate | **Balanced**: 85% of High-CMT and 85% of High-RE pass the ≥6-precursor gate. COFFEE (0/12) and TIAM-ECN (0/23) are entirely removed. |
| Deprivation gap vs headcount | ρ = **0.988–0.999** in every region. These are one result, not two. |
| Jobs REFOSS vs LOWC | ρ = **0.84–0.997**, median 0.974. Also one result. |
| Degenerate cells | None. No cell has >10% exact zeros or <20 unique values. Mortality has by far the tightest spread (median IQR/median 0.16 vs 0.96–1.15 for jobs) — its contrast rides on a much smaller signal. |
| Deprivation vs mortality | ρ = **−0.58**. Scenarios with a larger energy-deprivation gap have *lower* PM2.5 mortality — less combustion, less access. High-RE winning both is the non-trivial part of the result. |

### A correction to my own earlier reading

Row-median fossil jobs fall 5.6 → 0.1 (thousand) over 2020–2050, which looked like fossil
employment vanishing by 2030. That was a median over disaggregated fuel × stream × type
rows, not a scenario total. **Scenario-level fossil jobs summed over the ten regions go
5,433k → 2,261k**, a 58% decline. Fossil is 7–34% of energy jobs by region. The contrast
is a real contrast.

### The jobs decomposition — the mechanism result

| Region (2C) | δ on RE jobs | δ on fossil jobs (+ = High-RE has fewer) | δ on contrast |
|---|---|---|---|
| India+ | +0.89 | **0.00** | +0.89 |
| Rest Asia | +0.89 | **−0.18** | +0.88 |
| Middle East | +0.85 | **−0.20** | +0.79 |
| Europe | +0.68 | +0.67 | +0.80 |
| REF_ECON (1.5C) | **+0.07** | **+0.70** | +0.28 |
| Pac OECD | **+0.12** | +0.53 | +0.23 |

Two distinct stories, and they need separating in the WHY:
- In most regions High-RE wins by **building more labour-intensive capacity**, and in
  India+, Rest Asia and the Middle East it wins *while retaining more fossil jobs* — the
  strongest form of the result.
- In **REF_ECON and Pac OECD** the jobs "win" is mostly **fossil job destruction**, not RE
  job creation. Those two cells should not be presented as employment gains.

---

## 4. What the headline should now be

Counting all five measures double-counts jobs and deprivation. Counting **three families**:

**53 of 66 region × ambition × family cells (80%) favour High-RE.**
41 significantly so (cluster-robust); 7 significantly favour High-CMT.

| Family | cells | High-RE | sig. for RE | sig. for CMT | median δ | median gap |
|---|---|---|---|---|---|---|
| Jobs (RE − fossil) | 22 | **22** | 20 | 0 | +0.87 | +170% |
| Energy deprivation (gap) | 22 | 17 | 14 | 4 | +0.35 | +25% |
| Health (PM2.5 mortality) | 22 | 14 | 7 | 3 | +0.15 | +6% |

World: 9 of 10 cells significant. The single exception is **2C mortality**
(δ = +0.33, CI −0.05 to 0.76).

Robustness of the 66-cell headline:

| sample | scenarios | High-RE | sig. RE | sig. CMT |
|---|---|---|---|---|
| A full database, all | 590 | 53 (80%) | 41 | 7 |
| A full database, matched | 537 | 54 (82%) | 41 | 7 |
| C SCI-vetted, all | 137 | 48 (73%) | 29 | 8 |
| C SCI-vetted, matched | 132 | 48 (73%) | 27 | 7 |

Direction survives vetting; significance thins as expected at n=137.

---

## 5. Remaining weaknesses to state in the paper, not fix

1. **Corner contrast.** 78% of scenarios are excluded at 2C. The claim is about the top
   tercile of each axis, not about a monotone dose–response.
2. **Novel CDR contributes ~0** to the CMT axis at the median. "Carbon management" here is
   land-based CDR plus fossil CCS.
3. **Model composition.** POLES-JRC 100% High-CMT, REMIND family ~95% High-RE. For mortality,
   only 4–5% of variance is within-family in four regions — the arms barely meet inside a
   model. This is why mortality is the weakest family and why 3 of its 22 cells go the other
   way significantly.
4. **Base-period population denominator** distorts cross-region *levels* (not contrasts).
5. **Jobs are a sum of decadal snapshots**, not an integral — fine for a contrast, but the
   units are "thousand jobs summed over four decade marks", not job-years.
6. **REF_ECON and Pac OECD jobs wins are fossil-destruction-driven**, not RE-creation-driven.
