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

