# COMPASS Dissertation Paper 1 — final reproducible analysis

This repository contains the authoritative 2020–2100 Paper 1 pipeline. The
primary comparison is mutually exclusive High-CDR versus High-RE portfolios,
classified within ambition band. High-CDR uses all CDR, including land-based
removal; engineered-only is a sensitivity.

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
5. `paper1_analysis/W14_within_model_landprimary.R` — original primary
   within-model diagnostic.
6. `paper1_analysis/W15_arm_composition.R` — arm composition and leave-one-out.
7. `paper1_analysis/W16_factorial_model_robustness.R` — final factorial analysis:
   full versus SCI-vetted; ambitions pooled versus split; pooled versus every
   eligible model family; World plus all R10 regions; four reported measures.
8. `paper1_analysis/W16_factorial_figures.R` — final robustness matrices.
9. `paper1_analysis/W19_jobs_retirement_decommissioning.R` — revised employment
   outcome, inferred capacity retirements, decommissioning job-years, displaced
   plant/upstream positions, and Full/SCI plus pooled/within-model comparisons.

Run steps 3 and 5–8 together with `Rscript run_final_analysis.R`. Set
`COMPASS_RUN_RFASST=true` only when intentionally rebuilding mortality.

## Classification rule

Within each ambition band, a scenario is High-CDR when it lies in the top
tercile of cumulative 2020–2100 CDR and not the top tercile of renewable
capacity. It is High-RE when the reverse is true. Scenarios high in both are
excluded. Because identical tercile sizes and the same overlap are removed from
both arms, the classified arms are balanced by construction.

## Frozen final inputs and outputs

- Full database: approach A.
- SCI-vetted database: approach C.
- Mortality: `final_outcomes/mortality_allcdr_reporting_complete_scenario_values_2020_2100.csv`.
- Final factorial tables: `final_outcomes/W16_factorial_*.csv`.
- Final matrices: `final_outcomes/W16_*.png`.

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

W19 writes both a 2020–2050 reconstruction from the current master release and a
2020–2100 deck-compatibility table. The repository description and classification
window are 2020–2100, while the current master script sets the outcome window to
2020–2050. Select and document one outcome window before freezing the final
tables and slides.
