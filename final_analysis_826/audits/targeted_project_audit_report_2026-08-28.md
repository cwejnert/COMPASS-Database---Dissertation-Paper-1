# COMPASS Paper 1 targeted project audit

> Historical audit trail. Final reporting is in `FINAL_WELLBEING_OUTCOME_REVIEW_2026-08-31.md` and `final_outcomes/tiered_analysis/`, which integrate the validated 472-scenario mortality release and the revised 2020–2100 employment outcome with inferred retirements and decommissioning. Do not quote the earlier employment estimates in this file as final results.

Date: 2026-08-28
Scope: World results first, with project/model/version tracing triggered by the quantitative influence screen. Regional project audits should follow only for cells that survive the World screen or remain substantively important.

## Bottom line

The three outcomes do not currently have the same validation status.

1. **Employment direction is supported, but its magnitude is composition-sensitive.** The full-database pooled result favors High-RE by 164%, the equal-family mean is +62%, six of seven informative families point toward High-RE, and no leave-one-family-out run reverses the pooled direction. REMIND strongly inflates the pooled magnitude, while WITCH is the only robust family pointing the other way in the all-ambition analysis.

2. **The deprivation result is not validated as a model-independent High-RE effect.** Removing REMIND reverses the World pooled result for both the DLE gap and deprived headcount. REMIND supplies 224 of the 305 classified High-RE scenarios (73%) but only one of the 305 High-CDR scenarios. The pooled comparison therefore largely compares REMIND High-RE pathways with other models' High-CDR pathways. It remains a valid description of this scenario database, but not a technology-attribution result.

3. **Mortality is coverage-qualified and composition-sensitive; comparable evidence leans High-RE.** The targeted run added 42 validated direct-R10 scenarios, increasing primary coverage from 430 to 472 of 610. Full-database World pooled medians favor High-RE by +2.40% with ambitions pooled, +1.85% at 1.5C and +1.33% at 2C. The family-balanced means are also positive, and all three robust matched model-project-ambition cells favor High-RE. However, excluding REMIND reverses the all-ambition pooled estimate to -0.44%, and several 2C exclusions produce very small negative estimates. SCI-vetted pooled reversals are not reproduced within IMAGE, its only informative family, demonstrating rather than resolving the composition effect.

## Tier 1 - quantitative influence screen

### Classification composition

The arms are balanced overall within ambition by construction, but they are not balanced within model families.

| Ambition | Arm | REMIND scenarios | Arm total | REMIND share |
|---|---:|---:|---:|---:|
| 1.5C | High-RE | 59 | 67 | 88.1% |
| 1.5C | High-CDR | 1 | 67 | 1.5% |
| 2C | High-RE | 165 | 238 | 69.3% |
| 2C | High-CDR | 0 | 238 | 0% |

The sole classified REMIND High-CDR scenario is **REMIND 3.0 / NAVIGATE Demand-1.5C-all_u**. It lies above the 1.5C all-CDR cutoff (670,690 versus 565,430) and below the renewable cutoff (2.20 million versus 2.70 million in the deployment metric). This is a real application of the classification rule, not a coding anomaly, but one scenario cannot identify a REMIND-specific arm contrast.

### World influence decisions

| Outcome | Pooled, all ambitions | Equal-family mean | Leave-one-family-out | Current status |
|---|---:|---:|---|---|
| Employment | +164.4% | +61.9% | No direction flips | Direction supported; magnitude composition-sensitive |
| DLE gap | +27.1% | +12.8% | Flips to -26.4% without REMIND | Database association only |
| Deprived headcount | +28.3% | +13.8% | Flips to -25.7% without REMIND | Database association only |
| PM2.5 mortality | +1.84% | +3.39% | Two direction flips | Provisional; rerun coverage gap |

Positive values are direction-coded to favor High-RE.

## Tier 2 - targeted project audit

### REMIND and the deprivation result

The quantitative mechanism is straightforward. Across the World scenario values, average final energy per person is strongly negatively associated with both deprivation measures inside most model families. Spearman correlations between final energy and the DLE gap are about -0.73 to -0.96 for the larger MESSAGEix, IMAGE, POLES, and REMIND samples; the headcount relationship is similarly negative.

REMIND's High-RE projects generally have 2020-2100 time-average World final energy of roughly 46-77 GJ per person, while its lone High-CDR scenario is about 38.8 GJ per person. The DLE method necessarily turns higher final energy, holding the regional threshold and energy-distribution Gini fixed, into a smaller gap and lower deprived share. Thus the pooled deprivation result is best understood as a **selected final-energy-profile association**: the High-RE arm contains many high-final-energy REMIND scenarios, while the High-CDR arm contains almost none from REMIND.

The project documents support treating these scenarios as policy bundles rather than exchangeable replications:

- **ENGAGE** deliberately varies cost-effective pathways, technological/geophysical constraints, institutional constraints, and social enablers. Its own feasibility work emphasizes that these assumptions redistribute mitigation across regions and technologies. The 78 REMIND-MAgPIE 2.1-4.2 ENGAGE High-RE scenarios therefore share a project design and should not count as 78 independent confirmations. Sources: [ENGAGE feasible futures brief](https://engage-climate.org/wp-content/uploads/2023/10/PB41_Feasible_futures_web-2.pdf) and [Brutschin et al. feasibility framework](https://engage-climate.org/wp-content/uploads/2021/09/A-multidimensional-feasibility-evaluation-of-low-carbon-scenarios_Brutschin-et-al..pdf).

- **NAVIGATE Demand** is explicitly a demand-side intervention experiment. The project synthesis distinguishes activity, technology, electrification/fuel-shift, and combined strategies; it also notes that electrification strategies can substantially increase electricity demand. The `all_u` scenario is therefore not a neutral counter-arm observation. Source: [NAVIGATE synthesis report](https://www.navigate-h2020.eu/wp-content/uploads/2023/11/NAVIGATE-synthesis-report-compressed.pdf).

- **PEP** varies near-term policy packages (NDC, good practice, net zero, cost-effective) and full versus reduced CDR availability. These are bundled policy designs that affect energy demand, technology deployment, and timing together. Source: [Kriegler et al. 2018](https://newclimate.org/sites/default/files/2018/07/Kriegler_2018_Environ._Res._Lett._13_074022.pdf).

- **CEMICS** was designed to study CDR portfolios and limits, including the differing regional potentials and side effects of CDR technologies. A CEMICS pathway falling into this analysis's High-RE tercile does not mean the original experiment isolated a renewable-energy treatment. Source: [Strefler et al. 2021](https://doi.org/10.1088/1748-9326/AC0A11).

Conclusion: deeper reading of every REMIND project cannot manufacture a within-REMIND comparison that the database does not contain. The appropriate next estimator is a matched model-project-ambition contrast wherever both arms exist; otherwise the cell is an identification gap.

### WITCH and employment

WITCH's apparent all-ambition disagreement is mostly an ambition/project-composition result, not a stable WITCH structural result.

- All ambitions: High-CDR median 310.8 versus High-RE 240.1 job-years per 1,000; -22.7% direction-coded effect.
- 2C only: High-CDR 248.9 (n=2) versus High-RE 240.1 (n=16); -3.6%, below the minimum robust cell size.
- 1.5C: six High-CDR scenarios and no High-RE comparator.
- Within ENGAGE, all six High-CDR scenarios are 1.5C and all eleven High-RE scenarios are 2C. The large fuel decomposition difference is dominated by lower wind job-years in the High-RE set, but it is explicitly a mixed-ambition comparison.
- Within 2C COMMIT, the single High-RE scenario has much higher employment than the two High-CDR scenarios (about 695 versus 270 job-years per 1,000). This goes opposite to the all-WITCH median.

COMMIT's Bridge scenario combines good-practice policies through 2030 with a later cost-effective 2C transition, including renewable scale-up, electrification, efficiency, and afforestation. It is another policy bundle, not a single-axis renewable/CDR intervention. Sources: [COMMIT Scenario Explorer](https://zenodo.org/records/5727072) and [van Soest et al. multi-model analysis](https://doi.org/10.1038/s41467-021-26595-z).

Conclusion: WITCH is evidence of structural heterogeneity, but not evidence that the employment conclusion reverses under a controlled WITCH comparison. The full-database employment direction remains stable after excluding WITCH or REMIND; only the magnitude changes substantially.

### Mortality mechanism and coverage

Where a genuine model-project contrast exists, the emissions mechanism is coherent:

- MESSAGEix-GLOBIOM 1.1 ENGAGE at 2C has 60-62 High-CDR and 17-20 High-RE scenarios with usable pollutant series. High-RE median cumulative emissions are lower for every PM precursor: approximately -3% OC, -5% BC/CO/NH3, -7% SO2, and -11% NOx.
- MESSAGEix GEI shows the same direction at 1.5C across all PM precursors, approximately -7% to -12%.
- WITCH COMMIT at 2C is only 2 versus 1 and has mixed, near-zero pollutant changes; it cannot support a robust mortality direction.

These raw-emissions integrations are explanatory diagnostics, not substitutes for RFASST: RFASST is nonlinear, spatially allocates emissions, and applies the PM2.5 concentration-response calculation.

Before the targeted rerun, the mortality output covered 430 of 610 classified scenarios. Of the 180 missing scenarios:

- 110 lack complete raw PM-precursor reporting and represent a genuine data ceiling;
- 70 passed the raw-data screening test but were absent from the output (24 High-CDR and 46 High-RE). This screen confirmed five PM-precursor series at World-equivalent coverage but was deliberately weaker than RFASST's full quality gate.

The targeted run resolved this screen: 42 direct-R10 cases succeeded; 21 direct-R10 cases failed the NH3 credibility gate; and seven World-fallback candidates failed the complete-grid gate. The primary release now contains 472 classified scenarios. Regenerated results preserve the Full World High-RE direction at both ambition levels, but confirm that the magnitude and some pooled directions remain sensitive to model composition. Matched model-project-ambition results provide the strongest available support: all three robust Full-database cells favor High-RE.

## Tier 3 - scenario-specific audit triggers

Do not audit every scenario. Audit only cases that can change interpretation:

1. **REMIND 3.0 / NAVIGATE Demand-1.5C-all_u** - the sole REMIND High-CDR case; inspect demand assumptions, CDR composition, renewable deployment, final energy, and why it falls just above/below the two tercile boundaries.
2. **REMIND-MAgPIE 2.1-4.2 / ENGAGE** - 78 High-RE scenarios and the largest single source of High-RE composition; audit shared feasibility assumptions and scenario-family clustering.
3. **MESSAGEix-GLOBIOM 1.1 / ENGAGE and GEI** - the strongest matched-project mortality evidence; verify pollutant reporting, spatial coverage, and whether scenario variants are near-duplicates.
4. **WITCH 5.0 / ENGAGE and COMMIT** - separate the mixed-ambition ENGAGE comparison from the sparse within-2C COMMIT contrast.
5. **The 28 RFASST-rejected mortality omissions** - 21 require a defensible NH3 source or an explicitly labelled NH3 sensitivity; seven require additional regional/time reporting or an explicitly labelled spatial/temporal imputation sensitivity. They must not enter the primary reporting-complete estimator as currently reported.

Only if these audits leave a direction unexplained should the analysis move to a full parameter-by-parameter scenario audit.

## Synthesis rule for the paper

Do not validate a pooled result by a simple majority vote of model families. Families are not equally informative, project ensembles are not independent, and many family/arm cells are one-sided.

Use four explicit evidence classes instead:

1. **Cross-model supported:** pooled and family-balanced directions agree; no influential family exclusion flips direction; at least two robust, comparable families or projects agree.
2. **Directionally supported but composition-sensitive:** direction survives influence tests, but pooled and family-balanced magnitudes differ materially or one robust family disagrees. Employment currently fits here.
3. **Database association only / not identified across models:** pooled direction flips when a dominant one-sided family is removed, or matched within-family/project comparisons are unavailable. Deprivation currently fits here.
4. **Provisional due outcome coverage:** enough runnable missing observations exist to plausibly affect the conclusion. Mortality currently fits here.

Report the scenario-weighted pooled estimand and the family-balanced estimand side by side. Add a third, matched model-project-ambition estimand only for cells with both arms; do not fill non-comparable cells by extrapolation.

## Reproducible outputs

- `influence_register.csv` and `World_outcome_validation_decisions.csv`: audit triggers and current evidence classes.
- `classified_arm_family_shares.csv` and `REMIND_WITCH_scenario_classification_audit.csv`: classification asymmetry and boundary values.
- `targeted_world_project_effects.csv`: outcome medians by model version and project.
- `WITCH_jobs_by_project_and_arm.png` and `WITCH_ENGAGE_jobs_mechanism_by_fuel.png`: WITCH employment composition diagnostics.
- `dle_world_energy_scenario_mechanism.csv`, `dle_energy_project_effects.csv`, and `dle_energy_outcome_correlations.csv`: deprivation mechanism audit.
- `mortality_remaining_missing_scenarios.csv`, `mortality_coverage_by_model_project_arm.csv`, and `mortality_project_pollutant_effects.csv`: mortality coverage and emissions mechanism audit.

Scripts are in `work/project_audit/W19_*.R` through `W25_*.R`.
