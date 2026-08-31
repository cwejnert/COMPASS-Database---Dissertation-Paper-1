# Targeted RFASST rerun results

Date: 2026-08-28

## Purpose

Resolve the 70 classified all-CDR-axis scenarios that passed the W23 raw-emissions screen but lacked mortality output. The screen was intentionally permissive: it checked for the five PM2.5 precursor series at World-equivalent coverage. This rerun applies the full production RFASST quality gates.

## Configuration

- Outcome: PM2.5/FUSION premature mortality; SSP2 health counterfactual.
- Emissions allocation: pollutant-specific TM5-FASST baseline-emissions shares.
- NH3: as reported; credible NH3 required.
- Reporting completeness: all five PM2.5 precursors must be nonzero and complete across the required region-pollutant-year grid.
- Short-gap interpolation: permitted for at most two bracketed decadal nodes; no values required interpolation in this run.
- O3: not run.
- Existing mortality outputs were not overwritten.

## Target partition

| Partition | Targets | Validated successes | Rejected |
|---|---:|---:|---:|
| Direct R10, primary | 63 | 42 | 21 |
| Reported-World fallback sensitivity | 7 | 0 | 7 |
| Total | 70 | 42 | 28 |

The 21 primary failures are REMIND-family pathways with absent or insufficiently credible reported NH3. The seven sensitivity failures have incomplete regional-pollutant-time grids even after the reported-World allocation step. They were not converted to zeros and were not merged into the primary release.

## Coverage after the validated merge

| Ambition | Pathway | Classified | Mortality available | Missing | Coverage |
|---|---|---:|---:|---:|---:|
| 1.5C | High-CDR | 67 | 57 | 10 | 85.1% |
| 1.5C | High-RE | 67 | 54 | 13 | 80.6% |
| 2C | High-CDR | 238 | 189 | 49 | 79.4% |
| 2C | High-RE | 238 | 172 | 66 | 72.3% |

Primary mortality coverage increased from 430 to 472 of 610 classified scenarios. Every merged scenario has ten R10 regions, nine decadal observations from 2020 through 2100, finite PM2.5 mortality, and one unique model-scenario-region-year record.

## Interpretation

The original 70-case raw screen should not be called a list of 70 runnable RFASST scenarios. It was a candidate screen. The production quality gate confirms 42 as primary reporting-complete cases and identifies explicit reasons for rejecting the remaining 28.

The mortality direction should now be recomputed using the 472-scenario primary release. The World and regional pooled, ambition-specific, equal-family, leave-one-family-out, within-family, SCI-vetted and matched-project results should all be regenerated before updating the paper or deck.
