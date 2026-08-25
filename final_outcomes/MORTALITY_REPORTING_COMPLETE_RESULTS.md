# Reporting-complete PM2.5 mortality diagnostic (2020–2100)

## Final rule

This is a quality-controlled diagnostic for the century-long broad-archetype comparison. Pathways are classified within ambition using cumulative 2020–2100 deployment: engineered CMT is Novel CDR plus fossil/industrial CCS, excluding land-based CDR; High-engineered-CMT and High-RE are top-tercile on their focal axis and not top-tercile on the opposing axis.

TM5-FASST/RFASST mortality is calculated only when all five PM2.5 precursors (SO2, NOx, BC, OM and NH3) are directly reported at R10 level. World-only emissions and NH3 sidecar values are not used. Short bracketed missing-time-point interpolation was permitted, with a maximum of two consecutive decadal nodes; no interpolations were needed in this run.

## Coverage

* 643 unique classified target scenarios.
* 572 had directly reported R10 emissions and entered the input-quality screen.
* 422 produced complete PM2.5 mortality output.
* 150 were rejected, rather than filled: 98 had no reported PM2.5 precursor and 52 had incomplete/nonzero precursor grids.
* 71 classified targets had no direct R10 emissions input and were excluded before screening.

World-equivalent coverage in the full database is 42/64 High-engineered-CMT and 37/64 High-RE at 1.5C; 138/239 and 168/239 at 2C. In the SCI-vetted sample it is 7/7 and 7/7 at 1.5C; 55/63 and 62/63 at 2C.

## Descriptive result and interpretation

The raw aggregate-R10 median High-RE minus High-engineered-CMT contrast (million cumulative PM2.5 deaths, 2020–2100) is -8.69 at full-database 1.5C, +3.03 at full-database 2C, -50.42 at SCI 1.5C, and +22.45 at SCI 2C. The sign changes by ambition. In the full-database within-model diagnostic, just one model contributes at 1.5C; at 2C, three models contribute and all have lower High-RE mortality even though the pooled median is slightly higher. This is direct evidence that model composition affects the pooled comparison.

Therefore, report this as a coverage-qualified mortality robustness diagnostic, not a universal pathway ranking. The main paper results remain employment and energy-deprivation outcomes from the complete outcome ensemble.

## Output files

* `mortality_reporting_complete_coverage_2020_2100.csv`
* `mortality_reporting_complete_medians_2020_2100.csv`
* `mortality_reporting_complete_within_model_2020_2100.csv`
* `mortality_reporting_complete_scenario_values_2020_2100.csv`
