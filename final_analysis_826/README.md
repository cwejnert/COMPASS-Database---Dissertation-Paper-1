# COMPASS Dissertation Paper 1 — final reproducible analysis

This directory is the authoritative 2020–2100 Paper 1 pipeline. The primary
comparison is mutually exclusive high carbon-management/removals versus High-RE
portfolios, classified within ambition band. The primary carbon-management axis
adds land-based CDR, novel CDR, and fossil/industrial CCS. Because fossil CCS is
not itself carbon removal, `High-CMT` is the precise code label; `High-CDR` may
be used in presentation text only when this broader operational definition is
stated. Engineered-only classification is a sensitivity.

## Final run order

1. `python_scripts/compass_pull_final.py` — optional database refresh. The
   committed master datasets are the frozen analysis inputs.
2. `analysis_scripts/COMPASS_master_analysis_allR10.R` — builds full and
   SCI-vetted outcome inputs for all ten R10 regions.
3. `paper1_analysis/V5_land_primary.R` — repaired mutually exclusive labels,
   strict World aggregation, all-CDR primary and engineered-only sensitivity.
4. `analysis_scripts/COMPASS_rfasst_full_allR10.R` — optional expensive RFASST
   run. The completed reporting-complete mortality output is committed under
   `final_outcomes/` and does not need to be rerun for reproduction.
5. `paper1_analysis/W19_jobs_retirement_decommissioning.R` — revised employment
   outcome, inferred capacity retirements, decommissioning job-years, displaced
   plant/upstream positions, and Full/SCI plus pooled/within-model comparisons.
6. `paper1_analysis/W26_targeted_rfasst_rerun.R` — optional expensive targeted
   RFASST rerun. The validated 472-scenario release is already frozen under
   `final_outcomes/mortality_472/`.
7. `paper1_analysis/W27_regenerate_mortality_472.R` — rebuilds mortality pooled,
   regional, within-family, equal-family, leave-one-family-out, and matched
   model-project-ambition results from the validated release.
8. `paper1_analysis/W28_integrated_tiered_analysis.R` — final four-outcome
   synthesis: Full/SCI; ambitions pooled/split; pooled, family-balanced,
   leave-one-family-out, and matched project estimates; World and every R10
   region.
9. `paper1_analysis/W29_employment_family_mechanisms.R` — targeted AIM/WITCH
   project and component audit using the revised total-employment definition.

From this directory, run `Rscript run_final_analysis.R`. It uses the frozen
employment and mortality releases by default. Set `COMPASS_REBUILD_JOBS=true`
only to rebuild the full annual jobs pipeline, and
`COMPASS_RUN_TARGETED_RFASST=true` only when intentionally repeating the costly
targeted mortality run with its runtime emissions inputs available.

## Classification rule

Within each ambition band, a scenario is High-CDR when it lies in the top
tercile of cumulative 2020–2100 CDR and not the top tercile of renewable
capacity. It is High-RE when the reverse is true. Scenarios high in both are
excluded. Because identical tercile sizes and the same overlap are removed from
both arms, the classified arms are balanced by construction.

## Frozen final inputs and outputs

- Full database: approach A.
- SCI-vetted database: approach C.
- Mortality: 472 Full-database reporting-complete scenarios in
  `final_outcomes/mortality_472/`; the regenerated Full/SCI scenario-value file
  is `final_outcomes/mortality_allcdr_reporting_complete_scenario_values_2020_2100.csv`.
- Revised employment: `final_outcomes/jobs_revision/`.
- Final four-outcome tables and figures: `final_outcomes/tiered_analysis/`.
- Model-family methodology and targeted project audits: `audits/`.

`paper1_analysis/README.md` contains the longer audit history. Scripts identified
there as superseded are retained for provenance and are not part of the final
run order.

## Employment outcome and retirement accounting

The primary employment outcome is total cumulative energy-sector job-years:
construction, manufacturing, operations and maintenance, fuel extraction,
refining, and decommissioning. `jobs_RE_minus_fossil` is retained only as a
diagnostic and must not be described as total or net employment.

Because COMPASS does not report retirements directly, annual gross retirements
are inferred by fuel, region, model, and scenario as
`max(0, previous stock + additions - current stock)`. A retirement already
removes future O&M, extraction, and refining employment through the lower
installed-stock path; those displaced positions are reported separately and
are not subtracted again from cumulative job-years. Decommissioning is a
positive, one-time employment stream. Its factors use U.S. NREL JEDI-based
GCAMUSAJobs values scaled with the existing regional construction-labour
multipliers; `COMPASS_DECOMMISSIONING_MODE=none` provides the no-decommissioning
sensitivity.

The primary employment release uses 2020–2100. Set
`COMPASS_OUTCOME_WINDOW_END=2050` only for the documented near-term sensitivity.
The 131 MB annual retirement RDS is deliberately excluded from GitHub; W19 uses
the committed compact cumulative tables, while the master script can regenerate
the annual audit file.

## Interpretation hierarchy

Scenario-weighted pooled medians describe this database; they are not causal
treatment effects. Final interpretation proceeds through pooled results,
within-family and equal-family comparisons, leave-one-family-out influence, and
matched model-version × project × ambition cells. Employment is directionally
supported but magnitude-sensitive to composition. Deprivation is a database
association rather than an independently identified technology effect. Mortality
is a coverage-qualified and composition-sensitive association. SCI-vetted 1.5°C
cells are especially sparse and must remain descriptive.
