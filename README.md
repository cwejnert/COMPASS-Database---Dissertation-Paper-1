# COMPASS Dissertation Paper 1

This repository contains the analysis history for Camille Wejnert's COMPASS
Scenario Database dissertation Paper 1.

## Authoritative release

Use [`final_analysis_826/`](final_analysis_826/) for the final reproducible
2020–2100 analysis. Its README defines the run order, frozen inputs, outcome
construction, model-composition validation, and interpretation limits.

The final release includes:

- mutually exclusive High-CMT/removals and High-RE classification within 1.5°C
  and 2°C ambition bands;
- Full-database and SCI-vetted results;
- strict ten-R10 World aggregation;
- total energy-sector job-years with inferred retirements and decommissioning;
- distributional decent-living energy gap and deprived headcount;
- the validated 472-scenario reporting-complete RFASST mortality release;
- pooled, within-family, equal-family, leave-one-family-out, and matched
  model-project-ambition analyses;
- model-family methodology and targeted project audits.

## Historical material

Top-level `analysis_scripts/`, `master_outputs/`, `final_outcomes/`,
`paper1_analysis/`, `classification_base/`, and earlier decks preserve previous
analysis stages. They are retained for provenance and must not be mixed with the
authoritative `final_analysis_826/` release.

## Reproduce the final summaries

From `final_analysis_826/`, run:

```text
Rscript run_final_analysis.R
```

The standard run uses committed compact employment and mortality releases. See
the nested README before requesting the expensive jobs or RFASST rebuilds.
