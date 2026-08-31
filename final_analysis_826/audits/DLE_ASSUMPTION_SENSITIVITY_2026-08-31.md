# DLE assumption-sensitivity audit

Date: 31 August 2026
Primary horizon: 2020–2100

## What was corrected

The previous frozen master tables contained an earlier DLE/headcount release.
They were not regenerated when the authoritative outcome horizon changed to
2020–2100. In addition, their `headcount_pct` divided an average future deprived
population by fixed 2020 population. That quantity is not a valid percentage
and can exceed 100% in growing-population scenarios.

W30 supersedes those DLE columns. It calculates:

- **DLE gap:** cumulative 2020–2100 energy needed to raise the
  below-threshold tail to the regional threshold, divided by 2020 population;
- **deprived share:** total deprived person-years divided by total population-
  years over 2020–2100, bounded between 0% and 100% and exactly aggregable from
  R10 regions to World.

The regional lognormal distribution is defined by scenario total final energy
and the DESIRE final-energy Gini. Sectoral DESIRE thresholds are summed before
the distributional calculation. Industry and transportation are retained as
diagnostics; the remainder after subtracting them from total final energy is
called **residual final energy**, not residential/commercial energy.

## Assumption grid

The final sensitivity is a 3 × 3 × 3 factorial:

| Assumption | Values |
|---|---|
| DESIRE threshold scale | 0.75x, 1.00x, 1.25x |
| Regional final-energy Gini shift | -0.05, baseline, +0.05 |
| Provisioning-efficiency trajectory | slower (1.5%/yr, 60% floor), baseline (1.9%/yr, 50% floor), faster (2.3%/yr, 40% floor) |

Pathway classification is held fixed. The sweep changes only the deprivation
assumptions.

## Full-database World baseline

| Outcome | Ambition | High-CMT | High-RE | High-RE advantage |
|---|---|---:|---:|---:|
| Cumulative DLE gap, GJ per 2020 capita | 1.5°C | 26.18 | 17.93 | +8.25 |
| Cumulative DLE gap, GJ per 2020 capita | 2°C | 22.02 | 16.81 | +5.21 |
| Population-year-weighted deprived share, % | 1.5°C | 6.97 | 4.58 | +2.39 pp |
| Population-year-weighted deprived share, % | 2°C | 5.77 | 4.38 | +1.39 pp |

## Robustness result

At World, both outcomes favor High-RE in all 27 specifications for ambitions
pooled, 1.5°C, and 2°C. The ranges of the World raw High-RE advantage are:

- 1.5°C DLE gap: +1.25 to +25.67 GJ per 2020 capita;
- 2°C DLE gap: +0.59 to +17.09 GJ per 2020 capita;
- 1.5°C deprived share: +0.51 to +4.46 percentage points;
- 2°C deprived share: +0.28 to +2.83 percentage points.

The direction is less stable for a few small regional contrasts. In the Full
database, all eleven World/R10 gap cells are stable at both ambition levels;
the deprived-share direction is stable in 9/11 cells at 1.5°C and 8/11 at 2°C.
The unstable split-ambition cells are Middle East and Rest of Asia deprived
share at 1.5°C, and Europe, Reforming Economies, and Rest of Asia deprived share
at 2°C.
With the corrected baseline DLE values, the final distinct regional scorecard is
43 of 60 cells favoring High-RE.

## What this does and does not establish

The World descriptive direction is robust to reasonable threshold, inequality,
and provisioning-efficiency assumptions. It is not robust to every change in
model composition: leave-one-family-out results can reverse, and SCI-vetted
2°C pooled deprivation favors High-CMT. The correct evidentiary statement is:

> Within the Full COMPASS scenario database, High-RE-labelled pathways are
> associated with lower modeled deprivation at World across the tested DLE
> assumptions. The association remains sensitive to which model families and
> projects populate the two pathway arms and is not an independently identified
> renewable-technology effect.

## Authoritative files

- `paper1_analysis/W30_dle_assumption_sensitivity.R`
- `inputs/desire_r10_dle_inputs.csv`
- `final_outcomes/dle_sensitivity/compass_dle_scenario_values_2020_2100.csv`
- `final_outcomes/dle_sensitivity/W30_dle_direction_stability_summary.csv`
- `final_outcomes/dle_sensitivity/W30_world_dle_assumption_sensitivity.png`
- `final_outcomes/tiered_analysis/` for the composition and matched-project layers
