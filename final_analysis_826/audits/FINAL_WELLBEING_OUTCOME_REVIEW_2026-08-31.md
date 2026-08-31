# Final wellbeing-outcome review

Date: 31 August 2026
Primary horizon: 2020–2100
Primary classification: mutually exclusive upper-tercile carbon-management/removals and renewable-capacity portfolios within ambition

## QA status

The revised employment pipeline, 472-scenario mortality reconstruction, and
integrated four-outcome tiered analysis completed successfully. All integrated
QA invariants passed: unique scenario-region-outcome rows, both arms in every
pooled cell, complete 2 database × 3 ambition × 11 geography × 4 outcome
coverage, finite comparable family and matched-project estimates, exactly 472
Full-database mortality scenarios, and all four outcomes in the World synthesis.

## Full-database World pooled medians

These are descriptive scenario-weighted medians, not causal treatment effects.
For deprivation and mortality, lower is better.

| Outcome | Ambition | High-CMT | High-RE | Direction-coded advantage | Direction |
|---|---|---:|---:|---:|---|
| Total energy-sector job-years per 1,000 | 1.5°C | 358.06 | 748.65 | +390.60 | High-RE |
| Total energy-sector job-years per 1,000 | 2°C | 294.41 | 542.51 | +248.10 | High-RE |
| DLE gap, GJ per capita | 1.5°C | 18.43 | 11.62 | +6.81 | High-RE |
| DLE gap, GJ per capita | 2°C | 15.46 | 11.37 | +4.09 | High-RE |
| Deprived headcount, percent | 1.5°C | 6.46 | 4.22 | +2.23 pp | High-RE |
| Deprived headcount, percent | 2°C | 5.77 | 4.17 | +1.60 pp | High-RE |
| Cumulative PM2.5 deaths, million | 1.5°C | 425.81 | 417.94 | +7.86 | High-RE |
| Cumulative PM2.5 deaths, million | 2°C | 440.50 | 434.62 | +5.87 | High-RE |

## Employment conclusion

The World direction is supported, but the scenario-weighted magnitude is
inflated by model composition. With ambitions pooled, the Full-database effect
is +91.7%, while the equal-family mean is +33.0%; no leave-one-family-out run
reverses the World direction. The no-decommissioning comparison also favors
High-RE (+386.45 job-years per 1,000 at 1.5°C and +243.32 at 2°C), so the result
does not depend on the JEDI decommissioning proxy.

The revised total-employment definition changes the interpretation of model
disagreements:

- AIM ENGAGE at 2°C is a robust reversal (20/11). High-RE has +27.9 renewable
  job-years per 1,000, but this is more than offset by lower nuclear (-12.0),
  fossil (-10.2), and decommissioning (-6.0) job-years, yielding -25.0 total.
  This is a larger-versus-different energy-system result, not evidence that AIM
  directly models superior labor-market performance.
- WITCH's pooled reversal is not a controlled WITCH structural result. Its
  ENGAGE comparison mixes project/ambition composition and favors High-CMT,
  while the sparse within-COMMIT comparison (2/1) strongly favors High-RE.

Report employment as gross/direct modeled energy-sector job-years, not net
economy-wide employment. Manufacturing is assigned to the deploying region;
O&M, extraction and refining are stock-based proxies. Retirement-induced loss
of future operating/upstream jobs is already reflected in lower stock and must
not be subtracted a second time. Gross displaced positions remain a separate
transition diagnostic.

## Deprivation conclusion

The DLE gap and deprived headcount are two readings of the same fitted
lognormal energy distribution. They are one deprivation finding, not two
independent confirmations. The World pooled direction favors High-RE, but both
measures reverse when influential model families are excluded. At 2°C the
regional pooled result is evenly split: five R10 regions favor each arm.

Therefore the defensible statement is a database association: High-RE-labelled
scenarios in this database tend to have higher final energy relative to the
modeled decent-living distribution, but the analysis does not identify an
independent renewable-technology effect. The threshold, mean energy, and fixed
regional inequality parameter should remain visible in the methods.

## Mortality conclusion

The final primary release contains 472 of 610 Full-database classified
scenarios. The targeted rerun added 42 validated direct-R10 scenarios; 21
candidate runs failed credible NH3 reporting and seven World-fallback cases
failed the complete region-pollutant-time grid. Missing cases were not converted
to zero or imputed into the primary analysis.

Full pooled medians favor High-RE modestly at both ambition levels. The
family-balanced direction and all three robust matched model-project-ambition
cells also favor High-RE, but leave-one-family-out estimates can cross zero.
SCI-vetted 2°C pooled mortality favors High-CMT while its only informative
within-family and robust matched-project evidence favors High-RE; that reversal
is therefore compositional. Mortality remains a coverage-qualified,
composition-sensitive association.

## Required final-paper/deck statements

1. Define the primary carbon-management axis precisely. It combines land-based
   CDR, novel CDR, and fossil/industrial CCS. Fossil CCS is not itself CDR, so
   `High-CMT/removals` is technically safer than an unexplained `High-CDR` label.
2. Replace every renewable-minus-fossil employment headline with total
   energy-sector job-years. Retain the former only as a labeled diagnostic.
3. Print outcome-specific scenario counts; balanced classification arms do not
   imply balanced reporting-complete outcome samples.
4. Present pooled, equal-family/influence, and matched-project evidence in that
   order. Do not validate a pooled result by majority vote across model families.
5. Describe gap and headcount as one distributional deprivation finding.
6. State the 472/610 mortality coverage and the stricter High-RE 2°C coverage
   limitation.
7. Keep Pacific OECD in World totals but omit or flag its standalone regional
   interpretation because the global High-RE label does not separate local
   renewable deployment there.
8. Avoid generic claims of statistical significance. The final evidentiary
   hierarchy is direction, raw magnitude, outcome coverage, family influence,
   and matched project support.

## Remaining work

No additional full-database rerun is required before writing. The appropriate
remaining tasks are presentation and documentation updates: replace stale
employment figures/tables, use the 472-scenario mortality release, add the
classification terminology note, and ensure every final table identifies its
estimand and scenario counts. Further scenario-level audits should be triggered
only if a reviewer challenges a specific sparse matched cell.
