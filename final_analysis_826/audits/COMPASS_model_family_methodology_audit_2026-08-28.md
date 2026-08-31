# COMPASS Paper 1: verified model-family methodology audit

## Audit status

This version supersedes the earlier high-level audit. It is matched to the exact model/version strings in the COMPASS all-CDR classified arms and separates documented structure, implications for COMPASS, and hypotheses that still require testing. No model is assigned a general accuracy rank. The models generate pathways; COMPASS subsequently calculates employment, deprivation and RFASST mortality.

## Exact scope in the classified arms

| Family | Exact model/version strings present |
|---|---|
| AIM | AIM/CGE V2.2 |
| COFFEE | COFFEE 1.1 |
| GCAM | GCAM 5.3; GCAM 6.0 NGFS; GCAM 7.0 |
| IMAGE | IMAGE 3.0; IMAGE 3.2; IMAGE 3.3 |
| MESSAGEix | MESSAGEix-GLOBIOM 1.1; 1.2; 2.0-M-R12-NGFS; GEI 1.0 |
| POLES | POLES-JRC ENGAGE |
| REMIND | REMIND 2.1; REMIND 3.0; REMIND-Buildings 2.0; REMIND-Transport 2.1; multiple REMIND-MAgPIE 1.7–3.3 / MAgPIE 3.0–4.8 combinations |
| TIAM | TIAM-ECN 1.1 |
| WITCH | WITCH 5.0 |

Exact counts by database, ambition, arm and version are recorded in `work/model_audit/exact_model_version_counts.csv`.

## Verified family audit

### AIM/CGE V2.2

**Documented structure — high confidence**

- Computable general equilibrium model with relatively disaggregated energy, agriculture and land sectors.
- Solves a mixed-complementarity problem in GAMS/PATH.
- Recursive-dynamic and myopic, normally with annual steps to 2100.
- Production uses mostly nested CES functions; consumption, production, trade, capital and labor allocation respond to equilibrium prices.
- GDP is endogenous in policy runs; baseline GDP pathways are used to calibrate exogenous TFP.

**COMPASS relevance:** Economy-wide price and substitution responses affect final energy and the size and composition of the energy system. AIM does not directly produce the COMPASS employment result; endogenous labor allocation does not replace the external capacity-based employment factors.

**Hypotheses to test:** Sectoral substitution and income effects may change final-energy demand; recursive adjustment may produce different build timing than perfect-foresight optimizers.

**Source:** https://www.iamcdocumentation.eu/images/9/9e/AIM-Hub_12Mar2020.pdf

### COFFEE 1.1

**Documented structure — medium-high confidence**

- Multi-regional, multi-sectoral partial-equilibrium model integrating energy and land use.
- Based on the MESSAGE optimization framework.
- Physical mass, energy, exergy and land balances are solved using linear programming.
- Perfect foresight, five-year steps over 2010–2100, 18 regions plus a global region.
- Can be soft-linked to TEA, a separate recursive-dynamic CGE model. The label `COFFEE 1.1` does not establish that TEA was active.

**COMPASS relevance:** Informative about long-horizon, technology-rich, cost-optimized energy/land pathways. The earlier description as merely a generic bottom-up framework was incomplete.

**Hypotheses to test:** Perfect foresight, resource potentials and bioenergy/land assumptions may alter deployment timing and technology concentration.

**Documentation gap:** Public documentation does not establish whether the specific AR6 runs were soft-linked to TEA.

**Source:** https://www.iamcdocumentation.eu/images/4/49/COFFEE-TEA-V1_12Mar2020.pdf

### GCAM 5.3, 6.0 NGFS and 7.0

**Documented structure — high confidence for the family core; medium for project overlays**

- Integrated energy, economy, agriculture/land, water and climate model.
- Recursive-dynamic market-equilibrium simulation, not an intertemporal welfare optimizer.
- Technology and commodity shares generally use calibrated logit choice rather than winner-take-all least-cost selection.
- Period markets are solved so supplies and demands balance.
- Developer documentation is versioned for GCAM 5.3, 6.0 and 7.0.

**COMPASS relevance:** Logit competition, period-by-period clearing, land/bioenergy interactions and demand responses affect deployment, final energy and emissions.

**Hypotheses to test:** Logit sharing may preserve technology diversity; recursive decisions may change build timing; NGFS adds project-specific assumptions beyond base GCAM.

**Sources:** https://jgcri.github.io/gcam-doc/v6.0/ and https://github.com/JGCRI/gcam-doc

### IMAGE 3.0, 3.2 and 3.3

**Documented structure — high confidence for TIMER core; medium-high across minor versions**

- Modular integrated assessment framework.
- TIMER energy component is a deterministic, recursive-dynamic simulation model.
- Explicitly myopic: decisions use current-period information rather than an intertemporal optimum.
- Represents demand, conversion and supply, including capital-stock inertia, learning-by-doing, depletion, trade and endogenous energy prices.
- TIMER does not calculate economy-wide mitigation feedbacks such as GDP losses.

**COMPASS relevance:** Particularly informative about energy-system inertia, technology diffusion, final energy and source emissions.

**Hypotheses to test:** Myopic investment, stock turnover, learning and reliability constraints may alter renewable timing and mix. Small mortality effects may reflect pollutant controls or low baseline emissions.

**Documentation gap:** The core is well documented for IMAGE 3.0/3.2; incremental IMAGE 3.3 and project settings require scenario-specific checking if they drive a disputed result.

**Sources:** https://www.pbl.nl/sites/default/files/downloads/pbl-2014-integrated_assessment_of_global_environmental_change_with_image30_735.pdf and https://models.pbl.nl/image/Energy_supply_and_demand

### MESSAGEix-GLOBIOM family

**Documented structure — high confidence for the family core; medium for project overlays**

- MESSAGEix is a detailed energy-systems optimization framework.
- MESSAGEix-GLOBIOM combines MESSAGEix energy with GLOBIOM land use and emissions/climate components.
- Can include macroeconomic feedback through the stylized MACRO module.
- Standard global applications are long-horizon, intertemporal cost-optimization exercises with technology, resource, conversion and service-demand constraints.

**COMPASS relevance:** Strong for technology choice, build schedules, costs and bioenergy/land interactions; not a direct labor-market model.

**Hypotheses to test:** Perfect foresight, resource constraints and GLOBIOM bioenergy supply may concentrate portfolios differently. GEI and NGFS overlays may matter as much as version number.

**Documentation gap:** Do not assume MACRO is active in every scenario merely because the framework supports it.

**Sources:** https://pure.iiasa.ac.at/id/eprint/17115/ and https://docs.messageix.org/projects/models/en/stable/global/index.html

### POLES-JRC ENGAGE

**Documented structure — high confidence**

- World energy-economy partial-equilibrium simulation model.
- Recursive year-by-year dynamics with endogenous international energy prices and lagged adjustment.
- Detailed final-demand sectors, fuel markets, energy transformation, capacity planning and dispatch.
- Technology diffusion depends on relative costs, vintages, resource potentials and learning.
- Includes air-pollutant emissions; IAMC identifies POLES ENGAGE as the application version.

**COMPASS relevance:** Strong for annual market adjustment, load representation, fuel markets and pollutant precursors; not an economy-wide employment model.

**Hypotheses to test:** Annual capacity planning, detailed fuel prices, dispatch and diffusion may alter construction timing, fossil retention and VRE penetration.

**Sources:** https://publications.jrc.ec.europa.eu/repository/bitstream/JRC113757/kjna29454enn.pdf and https://www.iamcdocumentation.eu/index.php/POLES

### REMIND family and specialized variants

**Documented structure — high confidence for REMIND 2.1 core; medium-high across variants**

- Energy-economy general-equilibrium model hard-linking a Ramsey-type macroeconomic growth model and bottom-up energy system.
- Perfect foresight; jointly selects economic and energy investments.
- Macro production uses capital, labor and final energy.
- Represents technology vintages, learning, expansion adjustment costs, trade and source-linked emissions.
- MAgPIE combinations add a coupled land-use system.

**COMPASS relevance:** Provides economically consistent investment pathways, but COMPASS jobs remain externally calculated from capacity additions and stocks.

**Hypotheses to test:** Economic-energy optimization and learning may drive rapid RE scale-up; adjustment costs and vintages shape timing; Buildings and Transport variants may alter sectoral final energy.

**Documentation gap:** REMIND 2.1, 3.0, Buildings, Transport and multiple MAgPIE pairings share a core but must not be assumed identical in sector modules or land coupling.

**Source:** https://www.pik-potsdam.de/en/institute/departments/transformation-pathways/models/remind

### TIAM-ECN 1.1

**Documented structure — medium-high confidence**

- This is TIAM-ECN, developed/used by ECN/TNO—not TIAM-UCL.
- Linear-programming IAM based on TIMES/TIAM.
- Minimizes discounted global energy-system costs in partial equilibrium while meeting energy-service demands and constraints.
- Documentation supports intertemporal optimization and lists perfect or myopic configurations.
- Base year 2005, ten-year steps to 2100, 36 regions.
- Technology choice is linear/lowest-cost with detailed resources-to-end-use chains.

**Correction:** The earlier audit incorrectly substituted TIAM-UCL. Related TIMES models are not interchangeable.

**COMPASS relevance:** Relevant to engineering-feasible, cost-optimized portfolios, not direct macroeconomic employment.

**Documentation gap:** The exact foresight configuration of the AR6 TIAM-ECN scenarios requires their originating scenario publication.

**Source:** https://www.iamcdocumentation.eu/index.php/TIAM-ECN

### WITCH 5.0

**Documented structure — high confidence**

- Dynamic IAM combining a top-down optimal-growth economy with a bottom-up energy system.
- Energy investments and macroeconomic allocation are jointly determined.
- Represents endogenous R&D/innovation, learning, capital depreciation, resource markets and VRE integration constraints.
- Can represent cooperative or non-cooperative regional strategic interaction depending on the experiment.

**COMPASS relevance:** Important structural counterexample for employment because it integrates energy investment with macroeconomic opportunity costs. Its COMPASS employment result remains a post-processed capacity result; WITCH does not validate the external employment factors.

**Hypotheses to test:** Capital opportunity costs, innovation, technology mix or VRE constraints may reduce or defer renewable capacity additions.

**Documentation gap:** The database label is `WITCH 5.0`, not `WITCH-GLOBIOM`; do not assert GLOBIOM linkage without scenario documentation. Cooperative versus non-cooperative mode also requires project evidence.

**Sources:** https://www.iamcdocumentation.eu/Archive_page_of_-_WITCH , https://doc.witchmodel.org/general-framework.html , https://doc.witchmodel.org/energy-supply.html , and https://doc.witchmodel.org/system-integration.html

## Verification boundary

### Verified sufficiently for the structural audit

- Core solution paradigm and documented foresight.
- Macro-energy linkage type.
- Major technology-choice, learning, stock and market features.
- Exact model/version strings in the classified COMPASS arms.
- COMPASS outcomes are post-processed and are not endogenous predictions of jobs, deprivation or RFASST mortality.

### Still requiring scenario-level evidence

- Whether optional MESSAGE-MACRO or COFFEE-TEA linkages were active.
- NGFS, GEI and ENGAGE project overlays.
- Cooperative/non-cooperative WITCH settings.
- Differences among REMIND Buildings, Transport and MAgPIE combinations.
- Exact technology costs, learning rates, substitution elasticities, pollution controls and resource potentials used by each scenario.

Therefore this audit is accurate as a **model-structure audit**, not yet a scenario-by-scenario parameter audit. Structural explanations are hypotheses until confirmed through upstream output decomposition or the originating scenario publications.

## Publication-safe conclusion

> Model families differ in solution concept, foresight, macroeconomic coupling, technology choice and sectoral representation. These documented differences provide hypotheses for model disagreement but do not establish that one family is generally more accurate. A pooled COMPASS result is validated only when it is stable under equal-family weighting and family exclusion, supported by adequate within-family comparisons, substantively non-negligible, and consistent with the upstream variables that generate the outcome.
