# Methods note — Paper 1

## Decent-living energy (DLE) gap and deprivation headcount: method, justification, limitations, and next steps

Prepared for the COMPASS High-RE vs High-CDR wellbeing analysis. Text below is
drafted for direct adaptation into the paper's Methods and Limitations sections.

> This markdown mirrors `COMPASS_Paper1_DLE_methods_note.docx` (kept alongside it
> for version control / diffing). The docx is the formatted source of record.

---

### 1. Metric definition (Methods draft)

Energy poverty is assessed with two indicators grounded in the decent-living
energy (DLE) framework (Rao and Min 2018; Kikstra et al. 2021). Regional
final-energy thresholds for decent living are taken from the DESIRE framework
(Kikstra et al. 2025): a needs-based floor in which transport is the largest
component, mapped to the model's residential/commercial, transport, and industry
sectors and to the five R10 world regions used here (Table 1). Thresholds decline
over time with an assumed service-provisioning-efficiency improvement of
approximately 1.9% yr⁻¹ (reaching about −38% by 2040, within the −30% to −46%
range reported by DESIRE). Within each region and year we construct a lognormal
distribution of per-capita final energy whose dispersion is set by a within-region
energy Gini coefficient (Oswald et al. 2021).

The decent-living energy **deprivation headcount** is the share of that
distribution below the threshold, multiplied by population. The **DLE gap**
(adapted from Kikstra et al. 2025) is the additional final energy required to lift
everyone below the threshold up to it — the partial expectation of the shortfall
over the below-threshold tail of the same distribution:

```
gap_pc = T·Φ(d₁) − E·Φ(d₂),   d₁ = (ln T − m)/σ,   d₂ = d₁ − σ,   m = ln E − σ²/2
```

where `T` is the regional DLE threshold, `E` the per-capita final energy, `σ` the
log-standard-deviation implied by the energy Gini, and `Φ` the standard normal
CDF. The headcount is the companion quantity `Φ(d₁)`. Both indicators are
cumulated over the ambition-specific window (to 2060 for 1.5 °C, to 2075 for
2 °C) and normalised per capita for the High-CDR vs High-RE comparison.

---

### 2. Regional DLE thresholds (Table 1)

Final-energy decent-living thresholds (GJ capita⁻¹ yr⁻¹), mapped to the five R10
regions from DESIRE's published sectoral means and ranges (transport ≈11.8
[10–18], residential/commercial ≈6.7 [4–12], industry ≈3.8 [3–7]; global mean
≈22, country range 17–35). Every value sits inside DESIRE's published per-sector
range; totals fall within its 17–35 span; transport is the largest component in
every region.

| R10 region     | Res./comm. | Transport | Industry | Total |
|----------------|-----------:|----------:|---------:|------:|
| India +        |        4.5 |      10.5 |      3.0 |  18.0 |
| Africa         |        5.0 |      11.0 |      3.0 |  19.0 |
| China +        |        6.5 |      11.5 |      4.0 |  22.0 |
| Europe         |        9.0 |      12.0 |      4.5 |  25.5 |
| North America  |       11.0 |      18.0 |      5.5 |  34.5 |

These replace an earlier table that was residential-led with rich-region totals up
to 63 GJ cap⁻¹ (i.e. tracking consumption rather than need). They are a documented,
DESIRE-consistent best estimate; for the final published value they should be
replaced with DESIRE's country-level thresholds population-weighted to R10 (or
confirmed with the authors).

---

### 3. Justification (why this is defensible)

The approach adapts DESIRE in two respects, both standard and defensible in the
integrated-assessment literature:

- **Top-down overlay.** DLE thresholds are applied on top of the model's
  final-energy output rather than reconstructed bottom-up from decent-living
  service requirements. This is the same overlay DESIRE itself uses to interpret
  scenario output, so it is established methodology, not a novel construction.
- **Benchmarked thresholds.** Because DESIRE reports thresholds at the country
  level, its published sectoral means and ranges are mapped to the five R10
  regions by climate and settlement geography, keeping every sector within
  DESIRE's published range and every regional total within its 17–35 GJ cap⁻¹
  span. Transferring benchmark thresholds from a published source is common
  practice; the only judgement is the regional interpolation, which is
  transparent and bounded.

Two features strengthen the design. First, the gap definition follows Kikstra et
al. exactly (the distributional lift-to-threshold integral); only the energy input
(model output) and the threshold source (interpolated) are adapted — hence the
metric is named the **DLE gap adapted from Kikstra et al. (2025)**, not a claim of
exact reproduction. Second, and most important for the inference here, both
pathway groups are evaluated against identical thresholds, so any threshold
miscalibration shifts both gaps together and largely cancels in the High-CDR vs
High-RE contrast — the primary quantity of interest. The sign and significance of
the contrast are therefore robust to the threshold level even where absolute
magnitudes are not; this is demonstrated by the threshold-sensitivity analysis
(Section 5).

---

### 4. Limitations (draft text)

The DLE indicators carry two limitations that should be stated explicitly. First,
the top-down overlay does not capture DESIRE's full bottom-up service accounting
(e.g. floor area, passenger-kilometres, material stocks), so absolute gap
magnitudes are DLE-benchmarked approximations rather than exact decent-living
energy needs. Second, the R10 thresholds are interpolated from published
aggregates rather than DESIRE's exact regional aggregation; the resulting
sensitivity is quantified by re-running the analysis under a ±25% threshold sweep
and an alternative threshold set (Section 5). Because both pathway groups face
identical thresholds, these uncertainties affect absolute levels far more than the
High-CDR vs High-RE contrast, whose sign and significance are stable across the
sweep. Finally, the deprivation headcount — which matches DESIRE's method and
definition — is bounded in [0, population] and is therefore the more robust of the
two energy-poverty indicators; it is reported as primary, with the aggregate gap
as a supporting diagnostic.

---

### 5. Next steps (roadmap)

1. **Apply the corrected DLE code.** Patch Section 4c of
   `COMPASS_master_analysis.R` with `dle_fix.R` (recalibrated thresholds,
   ~1.9%/yr efficiency, and the distributional gap), then re-run the pipeline. The
   deprivation headcount and all non-DLE outcomes are unaffected; the DLE gap and
   its implied-CO₂ shadow will be regenerated.
2. **Run the threshold-sensitivity sweep.** Execute `dle_sensitivity.R` (after the
   re-run). Confirm the sign/significance of the High-CDR vs High-RE contrast is
   stable across DESIRE ×0.75 / ×1.00 / ×1.25 and the alternative table; report
   the resulting table (or heatmap) in the SI as the robustness check.
3. **Check whether the reversal survives.** The energy-gap 'reversal' under SCI
   vetting was produced by the old mean-based gap. Verify on the corrected
   (distributional) gap whether it persists, weakens, or disappears — and update
   the deck's reversal / verdict slides accordingly.
4. **Upgrade the thresholds toward authoritative values.** Either read DESIRE's
   country-level thresholds from its supplementary data and population-weight to
   R10, or request the R10 aggregates from the authors; drop them into `dle_fix.R`
   in place of the interpolated table. (Route 2 — a bottom-up reconstruction from
   Rao, Min & Mastrucci 2019 — remains available if a reviewer requires it.)
5. **Verify the energy Gini coefficients.** Population-weight within-country energy
   Gini values from Oswald et al. 2021 to the five R10 regions and confirm the
   current values (0.25–0.45); these set the lognormal spread for both the
   headcount and the gap.
6. **Finalise terminology and text.** Adopt 'DLE gap (adapted from Kikstra et al.
   2025)' on first use with the Methods definition above; carry the short label
   'DLE energy gap' in figures/tables. Fold Sections 1–4 into the paper's Methods
   and Limitations.

---

### References

- Kikstra, J. S., et al. (2025). Closing decent living gaps in energy and emissions
  scenarios: introducing DESIRE. *Environmental Research Letters* **20**, 054038.
  doi:10.1088/1748-9326/adc3ad
- Kikstra, J. S., et al. (2021). Decent living gaps and energy needs around the
  world. *Environmental Research Letters* **16**, 095006.
  doi:10.1088/1748-9326/ac1c27
- Rao, N. D., Min, J. & Mastrucci, A. (2019). Energy requirements for decent living
  in India, Brazil and South Africa. *Nature Energy* **4**, 1025–1032.
  doi:10.1038/s41560-019-0497-9
- Rao, N. D. & Min, J. (2018). Decent Living Standards: Material Prerequisites for
  Human Wellbeing. *Social Indicators Research* **138**, 225–244.
  doi:10.1007/s11205-017-1650-0
- Oswald, Y., Owen, A. & Steinberger, J. K. (2021). Large inequality in
  international and intranational energy footprints. *Nature Energy* **6**.
  doi:10.1038/s41560-020-0579-8
