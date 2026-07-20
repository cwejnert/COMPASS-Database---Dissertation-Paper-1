# =============================================================================
# SCRIPT 1: COMPASS-ONLY CDR WELLBEING ANALYSIS
# Primary database: COMPASS Scenario Initiative (scenariocompass.org)
# All scenarios | 5 R10 regions | C1/C2/C3/C4 categories
#
# OUTCOMES:
#   - DLE energy gap (cumulative 2020-2050)            [full coverage]
#   - Deprivation headcount (mean 2020-2050)           [full coverage]
#   - Implied CO2 from closing DLE gap                 [full coverage]
#   - Renewable energy jobs (SE|Electricity proxy)     [approx, EJ-based]
#   - Fossil energy jobs (SE|Electricity proxy)        [approx, EJ-based]
#   - Air pollution mortality (rfasst/GEMM, full run)  [full coverage]
#
# CDR METRICS:
#   Total CDR     : sum of Novel + Fossil + Land-based (MtCO2/yr, real units)
#   Novel CDR     : DAC + BECCS (all sectors) + Enhanced Weathering (MtCO2/yr)
#   Fossil CCS    : Fossil energy + Geological + Industrial CCS (MtCO2/yr)
#   Land-based CDR: Carbon Removal|Land Use (MtCO2/yr)
#   Renewable Cap : Capacity|Electricity|Solar+Wind+Hydro+Nuclear+Biomass (GW)
#   All CDR/RE variables now in real units (proxy = FALSE)
#
# INPUTS:
#   compass_interp.rds    — from COMPASS_data_collection.R
#   compass_cum_ccs.rds   — from COMPASS_data_collection.R
#   compass_r10_meta.csv  — from compass_pull.py
#   compass_mortality_summary.rds — from GEMM_mortality.R (full coverage)
#   job_factors_complete.csv — job intensity factors
# =============================================================================

library(tidyverse)
library(zoo)
library(scales)
library(broom)
library(ggplot2)
library(patchwork)
library(writexl)

COMPASS_DIR <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/COMPASS"
OUT_DIR     <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_only"
dir.create(OUT_DIR,                    showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT_DIR, "Figs"), showWarnings = FALSE)

save_fig <- function(p, name, w = 14, h = 8) {
  ggsave(file.path(OUT_DIR, "Figs", name), p,
         width = w, height = h, dpi = 300, bg = "white")
  message("Saved: Figs/", name)
}


# =============================================================================
# SECTION 1: CONFIGURATION AND HELPERS
# =============================================================================

regions_r10     <- c("R10AFRICA", "R10CHINA+", "R10INDIA+",
                     "R10EUROPE", "R10NORTH_AM", "World")
categories_keep <- c("C1", "C2", "C3", "C4")

CATEGORY_COLORS <- c("C1" = "#1a9641", "C2" = "#fdae61",
                     "C3" = "#d7191c", "C4" = "#7b2d8b")
PATH5_COLORS    <- c(
  "High-CDR only" = "#2166ac",
  "High-RE only"  = "#d6604d",
  "Both High"     = "#542788",
  "Low (both)"    = "#4dac26"
)

theme_paper <- function(base_size = 10) {
  theme_bw(base_size = base_size) +
    theme(
      strip.background   = element_rect(fill = "white", color = "black"),
      strip.text         = element_text(face = "bold"),
      legend.position    = "bottom",
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title    = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
      plot.caption  = element_text(color = "gray50", hjust = 0, size = 7)
    )
}

# Scenario name harmonisation: COMPASS → mortality naming convention
harmonise_scenario <- function(s) {
  s %>%
    str_replace("^ENGAGE-NPi",   "EN_NPi") %>%
    str_replace("^ENGAGE-INDCi", "EN_INDCi") %>%
    str_replace("^COVID-Shift-", "COV_") %>%
    str_replace_all("CEMICS-",   "CEMICS_") %>%
    str_replace("1\\.5°C",       "1p5") %>%
    str_replace("2\\.0°C",       "2C") %>%
    str_replace("1\\.5C",        "1p5") %>%
    str_replace_all("(?<=LeastTotalCost)-", "_") %>%
    str_replace_all("-brkLR",    "_brkLR") %>%
    str_replace_all("-brkSR",    "_brkSR") %>%
    str_replace_all("-SSP",      "_SSP") %>%
    str_replace_all("-P50",      "_P50") %>%
    str_replace("CD-LINKS-",     "CD-LINKS_") %>%
    str_replace_all("(?<=[A-Za-z0-9])-(?=[A-Za-z])", "_")
}


# =============================================================================
# SECTION 2: LOAD COMPASS DATA
# =============================================================================

cat("Loading COMPASS data...\n")

compass_interp  <- readRDS(file.path(COMPASS_DIR, "compass_interp.rds"))
compass_cum_ccs <- readRDS(file.path(COMPASS_DIR, "compass_cum_ccs.rds"))

# Proxy flag index (proxy=TRUE rows are EJ-unit, rank use only)
compass_proxy_index <- compass_interp %>%
  distinct(Model, Scenario, Variable, proxy)

cat("Scenarios:  ", n_distinct(paste(compass_interp$Model,
                                     compass_interp$Scenario)), "\n")
cat("Regions:    ", paste(sort(unique(compass_interp$Region)), collapse=", "), "\n")
cat("Categories: ", paste(sort(unique(compass_interp$Category)), collapse=", "), "\n")
cat("Variables:  ", paste(sort(unique(compass_interp$Variable)), collapse=", "), "\n")


# =============================================================================
# SECTION 3: FILTERED WORKING DATASET
# =============================================================================

compass_filtered <- compass_interp %>%
  filter(
    Region   %in% regions_r10,
    Category %in% categories_keep,
    Year >= 2020, Year <= 2100,
    !is.na(Value)
  ) %>%
  mutate(
    Model_Group         = "COMPASS",
    ModelGroup_Scenario = paste("COMPASS", Scenario, sep = "_")
  )


compass_filtered <- compass_filtered %>% 
  filter(Scenario %in% c(
    "SDI-2.5\u00b0C",
    "SDI-Baseline",
    "SSP1-19",
    "SSP1-26",
    "SSP1-34",
    "SSP1-45",
    "SSP1-Baseline",
    "SSP3-Baseline",
    "NGFS Phase 2-Below 2\u00b0C",
    "NGFS Phase 2-Current Policies",
    "NGFS Phase 2-Delayed Transition",
    "NGFS Phase 2-Nationally Determined Contributions (NDCs)",
    "NGFS Phase 5-Below 2\u00b0C",
    "NGFS Phase 5-Current Policies",
    "NGFS Phase 5-Delayed Transition",
    "NGFS Phase 5-Fragmented World",
    "NGFS Phase 5-Low Demand",
    "NGFS Phase 5-Nationally Determined Contributions (NDCs)",
    "NGFS Phase 5-Net-Zero 2050",
    "COMMIT-2\u00b0C-2030",
    "COMMIT-Current-Policies",
    "COMMIT-NDCplus",
    "ENGAGE-INDCi2030-1000",
    "ENGAGE-INDCi2030-1000f",
    "ENGAGE-INDCi2030-1200",
    "ENGAGE-INDCi2030-1200f",
    "ENGAGE-INDCi2030-1400",
    "ENGAGE-INDCi2030-1400f",
    "ENGAGE-INDCi2030-3000",
    "ENGAGE-INDCi2030-3000f",
    "ENGAGE-INDCi2030-800f",
    "ENGAGE-INDCi2100",
    "ENGAGE-NPi2020-1000",
    "ENGAGE-NPi2020-1200",
    "ENGAGE-NPi2020-1400",
    "ENGAGE-NPi2020-1400f",
    "ENGAGE-NPi2020-3000",
    "ENGAGE-NPi2020-3000f",
    "ENGAGE-NPi2100",
    "SSP2021-SSP1-Baseline",
    "SSP2021-SSP1-SPA1-19-Default",
    "SSP2021-SSP1-SPA1-19-Default-LowBiomass",
    "SSP2021-SSP1-SPA1-19-Lifestyle",
    "SSP2021-SSP1-SPA1-19-Lifestyle-Renewables",
    "SSP2021-SSP1-SPA1-19-Renewables",
    "SSP2021-SSP1-SPA1-19-Renewables-LowBiomass",
    "SSP2021-SSP1-SPA1-26-Default",
    "SSP2021-SSP1-SPA1-26-Lifestyle",
    "SSP2021-SSP1-SPA1-26-Lifestyle-Renewables",
    "SSP2021-SSP1-SPA1-26-Renewables",
    "SSP2021-SSP1-SPA1-34-Default",
    "SSP2021-SSP1-SPA1-34-Lifestyle",
    "SSP2021-SSP1-SPA1-34-Lifestyle-Renewables",
    "SSP2021-SSP1-SPA1-34-Renewables",
    "SSP2021-SSP2-Baseline",
    "SSP2021-SSP2-SPA0-26-Default",
    "SSP2021-SSP2-SPA1-19-Default-LowBiomass",
    "SSP2021-SSP2-SPA2-19-Default",
    "SSP2021-SSP2-SPA2-19-Lifestyle",
    "SSP2021-SSP2-SPA2-19-Lifestyle-Renewables",
    "SSP2021-SSP2-SPA2-19-Renewables",
    "SSP2021-SSP2-SPA2-26-Default",
    "SSP2021-SSP2-SPA2-26-Lifestyle",
    "SSP2021-SSP2-SPA2-26-Lifestyle-Renewables",
    "SSP2021-SSP2-SPA2-26-Renewables",
    "SSP2021-SSP2-SPA2-34-Default",
    "SSP2021-SSP2-SPA2-34-Lifestyle",
    "SSP2021-SSP2-SPA2-34-Lifestyle-Renewables",
    "SSP2021-SSP2-SPA2-34-Renewables",
    "SSP2021-SSP2-SPA2-45-Default",
    "SSP2021-SSP2-SPA2-45-Lifestyle",
    "SSP2021-SSP2-SPA2-45-Lifestyle-Renewables",
    "SSP2021-SSP2-SPA2-45-Renewables",
    "SSP2021-SSP3-Baseline",
    "SSP2021-SSP4-Baseline",
    "SSP2021-SSP5-Baseline",
    "ECEMF-DIAG-NPi",
    "NAVIGATE Demand-1.5\u00b0C-ele_u",
    "NAVIGATE Demand-1.5\u00b0C-tec_u",
    "NAVIGATE Demand-2.0\u00b0C-ele_u",
    "NAVIGATE Demand-2.0\u00b0C-ref",
    "NAVIGATE Demand-2.0\u00b0C-tec_u",
    "NAVIGATE Demand-NPi-ele",
    "NAVIGATE Demand-NPi-ref",
    "NAVIGATE Demand-NPi-tec",
    "ENGAGE-NoPolicy",
    "NAVIGATE Demand-2.0\u00b0C-act_u",
    "NAVIGATE Demand-2.0\u00b0C-all_u",
    "NAVIGATE Demand-NPi-act",
    "NAVIGATE Demand-NPi-all",
    "COVID-Shift-GreenPush_max_GDP",
    "COVID-Shift-NoPolicyNoCOVID",
    "COVID-Shift-Restore",
    "COVID-Shift-SelfReliance",
    "COVID-Shift-SelfReliance_max_GDP",
    "COVID-Shift-SmartUse",
    "COMMIT-2\u00b0C-2020",
    "COMMIT-Baseline",
    "COMMIT-Bridge",
    "COMMIT-Bridge-No-Tax",
    "COMMIT-GPP",
    "COMMIT-GPP-No-Tax",
    "COMMIT-NDC-2050-Convergence",
    "ENGAGE-INDCi2030-1000-COV",
    "ENGAGE-INDCi2030-1000-COV-NDCp",
    "ENGAGE-INDCi2030-1000-NDCp",
    "ENGAGE-INDCi2030-1000f-COV",
    "ENGAGE-INDCi2030-1000f-COV-NDCp",
    "ENGAGE-INDCi2030-1000f-NDCp",
    "ENGAGE-INDCi2030-1600",
    "ENGAGE-INDCi2030-1600f",
    "ENGAGE-INDCi2030-1800",
    "ENGAGE-INDCi2030-1800f",
    "ENGAGE-INDCi2030-2000",
    "ENGAGE-INDCi2030-2000f",
    "ENGAGE-INDCi2030-2500",
    "ENGAGE-INDCi2030-2500f",
    "ENGAGE-INDCi2030-300f",
    "ENGAGE-INDCi2030-400f",
    "ENGAGE-INDCi2030-500f",
    "ENGAGE-INDCi2030-600f",
    "ENGAGE-INDCi2030-600f-COV",
    "ENGAGE-INDCi2030-600f-COV-NDCp",
    "ENGAGE-INDCi2030-600f-NDCp",
    "ENGAGE-INDCi2030-700f",
    "ENGAGE-INDCi2030-900",
    "ENGAGE-INDCi2030-900f",
    "ENGAGE-INDCi2100-COV",
    "ENGAGE-INDCi2100-COV-NDCp",
    "ENGAGE-INDCi2100-NDCp",
    "ENGAGE-NPi2020-1000f",
    "ENGAGE-NPi2020-1000f-COV",
    "ENGAGE-NPi2020-1200f",
    "ENGAGE-NPi2020-1600",
    "ENGAGE-NPi2020-1600f",
    "ENGAGE-NPi2020-1800",
    "ENGAGE-NPi2020-1800f",
    "ENGAGE-NPi2020-2000",
    "ENGAGE-NPi2020-2000f",
    "ENGAGE-NPi2020-2500",
    "ENGAGE-NPi2020-2500f",
    "ENGAGE-NPi2020-900f",
    "ENGAGE-NPi2100-COV",
    "EMF30-BCOC-EndU",
    "EMF30-Baseline",
    "EMF30-CH4-Only",
    "EMF30-D-BCOC-Red",
    "EMF30-D-CH4-ClimatePolicy",
    "EMF30-D-Frozen-CH4",
    "EMF30-D-Frozen-EF",
    "EMF30-D-Frozen-EF-EndU",
    "EMF30-D-Frozen-EF-SLCF",
    "EMF30-SLCF",
    "EMF30-Slower-Action",
    "EMF30-Slower-to-Faster",
    "ADVANCE-NoPolicy",
    "ADVANCE-Reference",
    "CEMICS-Ref",
    "LeastTotalCost-Base-brkLR15-SSP1-P50",
    "LeastTotalCost-Base-brkLR15-SSP2-P50",
    "LeastTotalCost-Base-brkLR15-SSP5-P50",
    "LeastTotalCost-Base-brkSR15-SSP1-P50",
    "LeastTotalCost-Base-brkSR15-SSP2-P50",
    "LeastTotalCost-Base-brkSR15-SSP5-P50",
    "R2p1-SSP1-Baseline",
    "R2p1-SSP2-Baseline",
    "R2p1-SSP5-Baseline",
    "Rescuing-1.5\u00b0C-Highest-Possible-Ambition",
    "BEG-Baseline",
    "BEG-Efficiency",
    "CD-LINKS-No-Policy",
    "EMF33-Baseline",
    "EMF33-medium-2\u00b0C-cost100",
    "EMF33-medium-2\u00b0C-full",
    "EMF33-medium-2\u00b0C-limbio",
    "EMF33-medium-2\u00b0C-nofuel",
    "EMF33-tax-hi-full",
    "EMF33-tax-hi-none",
    "EMF33-tax-lo-full",
    "EMF33-tax-lo-none",
    "PEP-NPi",
    "PEP-NoPolicy",
    "SMP-Reference-Default",
    "SMP-Reference-Sustainable",
    "Diff-NoPolicy-Baseline",
    "NGFS Phase 2-Current Policies [IPD 95th]",
    "NGFS Phase 2-Current Policies [IPD Median]",
    "DeepElectrification-SSP2-Baseline",
    "DeepElectrification-SSP2-NPi",
    "SHAPE-SSP2-NPi",
    "SHAPE-SSP2-NPi [with Climate Change Impacts]",
    "RESCUE-End-of-Century-Budget-1150",
    "RESCUE-End-of-Century-Budget-1150-with-OAE",
    "RESCUE-End-of-Century-Budget-500",
    "RESCUE-End-of-Century-Budget-500-with-OAE",
    "RESCUE-Peak-Budget-1150",
    "RESCUE-Peak-Budget-1150-with-OAE",
    "ENGAGE-NPi2020-1000-COV",
    "ENGAGE-NPi2020-800",
    "ENGAGE-NPi2020-800f",
    "ENGAGE-NPi2020-900",
    "ENGAGE-INDCi2030-1200-NDCp",
    "ENGAGE-INDCi2030-1200f-NDCp",
    "ENGAGE-INDCi2030-1400-NDCp",
    "ENGAGE-INDCi2030-1400f-NDCp",
    "ENGAGE-INDCi2030-1600-NDCp",
    "ENGAGE-INDCi2030-1600f-NDCp",
    "ENGAGE-INDCi2030-1800-NDCp",
    "ENGAGE-INDCi2030-1800f-NDCp",
    "ENGAGE-INDCi2030-2000-NDCp",
    "ENGAGE-INDCi2030-2000f-NDCp",
    "ENGAGE-INDCi2030-2500-NDCp",
    "ENGAGE-INDCi2030-2500f-NDCp",
    "ENGAGE-INDCi2030-3000-NDCp",
    "ENGAGE-INDCi2030-3000f-NDCp",
    "ENGAGE-INDCi2030-700f-NDCp",
    "ENGAGE-INDCi2030-800",
    "ENGAGE-INDCi2030-800-NDCp",
    "ENGAGE-INDCi2030-800f-NDCp",
    "ENGAGE-INDCi2030-900-NDCp",
    "ENGAGE-INDCi2030-900f-NDCp"
  ))

cat("\nFiltered rows:", nrow(compass_filtered), "\n")


# =============================================================================
# SECTION 4: CDR AND RE COMPOSITE VARIABLES
#
# All CDR and RE variables are now pre-computed in compass_interp (from the
# updated COMPASS_data_collection script) using real COMPASS native variables.
# All proxy = FALSE — units are MtCO2/yr (CDR) and GW (RE capacity).
#
# CDR types:
#   Novel CDR     = DAC + BECCS (all sectors) + Enhanced Weathering
#   Fossil CCS    = Fossil energy + Geological + Industrial CCS
#   Land-based CDR= Carbon Removal|Land Use
#   Total CDR     = sum of all three
#   Renewable Cap = Solar + Wind + Hydro + Nuclear + Biomass (GW stock)
# =============================================================================

cat("\n=== SECTION 4: CDR and RE variables (real units from compass_interp) ===\n")

# ---- 4a. Extract CDR components from compass_interp -------------------------
novel_cdr <- compass_filtered %>%
  filter(Variable == "Novel CDR") %>%
  mutate(proxy = FALSE)

fossil_ccs <- compass_filtered %>%
  filter(Variable == "Fossil CCS") %>%
  mutate(proxy = FALSE)

land_cdr <- compass_filtered %>%
  filter(Variable == "Land-based CDR") %>%
  mutate(proxy = FALSE)

total_cdr_ts <- compass_filtered %>%
  filter(Variable == "Total CDR") %>%
  mutate(proxy = FALSE)

re_capacity <- compass_filtered %>%
  filter(Variable == "Renewable Capacity") %>%
  mutate(proxy = FALSE)

# ---- 4b. CO2 Emissions ------------------------------------------------------
co2_emissions <- compass_filtered %>%
  filter(Variable == "Emissions|CO2") %>%
  mutate(Variable = "CO2 Emissions", proxy = FALSE)

# ---- 4c. Coal electricity energy (EJ/yr) ------------------------------------
coal_elec <- compass_filtered %>%
  filter(Variable == "Secondary Energy|Electricity|Coal") %>%
  mutate(Variable = "Coal Electricity Energy", proxy = FALSE)

# ---- 4d. Combine all derived variables --------------------------------------
compass_derived <- bind_rows(
  novel_cdr,
  fossil_ccs,
  land_cdr,
  total_cdr_ts,
  re_capacity,
  co2_emissions,
  coal_elec
) %>%
  distinct(Model, Scenario, Region, Variable, Year, .keep_all = TRUE)

cat("Derived variable rows:", nrow(compass_derived), "\n")
cat("CDR coverage (non-zero annual rows):\n")
compass_derived %>%
  filter(Variable %in% c("Novel CDR","Fossil CCS","Land-based CDR",
                         "Total CDR","Renewable Capacity"),
         Value > 0) %>%
  count(Variable) %>%
  print()


# =============================================================================
# SECTION 5: CUMULATE CDR AND RE FOR CLASSIFICATION AND REGRESSIONS
#
# Two cumulative datasets:
#
# (A) cdr_cumulative_ts — cumulative 2020-2100 from real MtCO2/yr timeseries
#     Used for: tercile classification of CDR and RE pathway types
#     Variables: Novel CDR, Fossil CCS, Land-based CDR, Total CDR (MtCO2),
#                Renewable Capacity (GW·yr — summed for ranking only)
#
# (B) cdr_cumulative_scalar — scalar Gt CO2 from metadata
#     Used for: regressions (comparable across databases, single global value)
#     Variable: Total CDR (proxy = FALSE, Gt CO2)
# =============================================================================

compass_with_total <- compass_derived

# ---- 5a. Cumulative timeseries (for tercile classification) -----------------
cdr_cumulative_ts <- compass_with_total %>%
  filter(Variable %in% c("Novel CDR", "Fossil CCS", "Land-based CDR",
                         "Total CDR", "Renewable Capacity"),
         Year >= 2020, Year <= 2100) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Category, Variable, proxy) %>%
  summarise(Total_Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  filter(Total_Value > 0)

# ---- 5b. Scalar Gt CO2 from metadata (for regressions) ---------------------
cdr_cumulative_scalar <- compass_cum_ccs %>%
  mutate(
    Model_Group         = "COMPASS",
    ModelGroup_Scenario = paste("COMPASS", Scenario, sep = "_"),
    Variable            = "Total CDR",
    Total_Value         = cum_ccs_GtCO2,
    proxy               = FALSE
  ) %>%
  # Global scalar replicated to all 5 R10 regions for region-level regressions
  crossing(Region = regions_r10) %>%
  select(Model_Group, Model, Scenario, ModelGroup_Scenario,
         Region, Category, Variable, Total_Value, proxy)

# ---- 5c. Combine — ts rows for classification, scalar for regressions ------
# Deduplication: if a scenario has both ts Total CDR and scalar Total CDR,
# keep the scalar (proxy=FALSE from metadata) for regressions, ts for the
# component variables (Novel/Fossil/Land) which have no scalar equivalent.
cdr_cumulative <- bind_rows(cdr_cumulative_ts, cdr_cumulative_scalar) %>%
  distinct(Model, Scenario, Region, Variable, proxy, .keep_all = TRUE)

cat("\nCDR cumulative rows:", nrow(cdr_cumulative), "\n")
cat("Variables in cdr_cumulative:\n")
cdr_cumulative %>%
  count(Variable, proxy) %>%
  arrange(Variable, proxy) %>%
  print()


# =============================================================================
# SECTION 6: NET-ZERO DATE
# =============================================================================

scenario_netzero <- co2_emissions %>%
  filter(Year >= 2020, Year <= 2100) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Category) %>%
  mutate(base_2020 = Value[Year == 2020][1]) %>%
  filter(!is.na(base_2020), base_2020 > 0) %>%
  summarise(
    netzero_year  = {
      yrs <- Year[Value <= 0.05 * base_2020[1] & Value > 0]
      if (length(yrs) > 0) as.integer(min(yrs)) else 2100L
    },
    never_netzero = (netzero_year == 2100L),
    .groups = "drop"
  )

cat("\n--- Net-zero date summary by Category ---\n")
scenario_netzero %>%
  filter(Region == regions_r10[1]) %>%   # one region to avoid duplication
  group_by(Category) %>%
  summarise(
    n_scenarios  = n(),
    n_reach_nz   = sum(!never_netzero),
    pct_reach_nz = round(100 * mean(!never_netzero), 1),
    median_nz_yr = median(netzero_year[!never_netzero], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()


# =============================================================================
# SECTION 7: AIR POLLUTION MORTALITY (full coverage via GEMM_mortality.R)
# Computed for all COMPASS C1/C2/C3/C4 scenarios using rfasst + GEMM.
# Emissions downscaled from R10 to TM5-FASST regions via population weights.
# Primary outcome: PM2.5 premature mortality (FUSION estimator).
# Run GEMM_mortality.R first to generate compass_mortality_summary.rds.
# =============================================================================

cat("\n=== SECTION 7: Air Pollution Mortality (rfasst/GEMM, full coverage) ===\n")

mortality_summary <- readRDS(
  file.path(COMPASS_DIR, "compass_mortality_summary.rds")
)

cat("Scenarios with mortality:",
    n_distinct(paste(mortality_summary$model, mortality_summary$scenario)), "\n")
cat("Regions:", paste(sort(unique(mortality_summary$r10_region)), collapse=", "), "\n")
cat("Categories:", paste(sort(unique(mortality_summary$Category)), collapse=", "), "\n")
# mortality_summary is joined onto df_master in Section 10

# =============================================================================
# SECTION 8: ENERGY JOBS
#
# Two-track approach:
#   Primary:  Capacity Additions (GW/yr) × job intensity — direct calculation
#             covering construction + manufacturing + O&M jobs
#   Fallback: Secondary Energy (EJ/yr) → implied GW × OEM intensity — used
#             for fuels/scenarios where capacity additions are not reported
#
# Job factors file units determine which track is used:
#   If job_factors_complete has a "gw_additions" category → primary track
#   Otherwise falls back to EJ-based OEM proxy
# =============================================================================

cat("\n=== SECTION 8: Energy Jobs ===\n")

job_factors_complete <- read.csv(
  file.path("C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/AR6",
            "job_factors_complete.csv")
)

cat("Job factor categories available:", paste(sort(unique(job_factors_complete$category)),
                                              collapse=", "), "\n")

# Capacity Additions variable → fuel mapping
cap_additions_fuel_map <- tribble(
  ~Variable,                                              ~fuel,        ~tech_group,
  "Capacity Additions|Electricity|Solar",                 "solar_pv",   "Renewables",
  "Capacity Additions|Electricity|Wind",                  "wind_on",    "Renewables",
  "Capacity Additions|Electricity|Hydro",                 "hydro",      "Renewables",
  "Capacity Additions|Electricity|Nuclear",               "nuclear",    "Renewables",
  "Capacity Additions|Electricity|Biomass",               "biomass",    "Renewables",
  "Capacity Additions|Electricity|Coal",                  "coal",       "Fossil",
  "Capacity Additions|Electricity|Gas",                   "gas",        "Fossil",
  "Capacity Additions|Electricity|Oil",                   "oil",        "Fossil"
)

# Secondary Energy variable → fuel mapping (fallback)
se_fuel_map <- tribble(
  ~Variable,                                           ~fuel,        ~tech_group,
  "Secondary Energy|Electricity|Solar",                "solar_pv",   "Renewables",
  "Secondary Energy|Electricity|Wind",                 "wind_on",    "Renewables",
  "Secondary Energy|Electricity|Hydro",                "hydro",      "Renewables",
  "Secondary Energy|Electricity|Nuclear",              "nuclear",    "Renewables",
  "Secondary Energy|Electricity|Biomass",              "biomass",    "Renewables",
  "Secondary Energy|Electricity|Coal",                 "coal",       "Fossil",
  "Secondary Energy|Electricity|Gas",                  "gas",        "Fossil",
  "Secondary Energy|Electricity|Oil",                  "oil",        "Fossil"
)

cap_factors <- tribble(
  ~fuel,        ~CF,
  "solar_pv",   0.18,
  "wind_on",    0.25,
  "hydro",      0.40,
  "nuclear",    0.85,
  "coal",       0.55,
  "gas",        0.45,
  "oil",        0.40,
  "biomass",    0.75
)

# Check whether capacity additions data is available
cap_add_vars_available <- compass_filtered %>%
  filter(str_detect(Variable, "^Capacity Additions\\|Electricity\\|"),
         !str_detect(Variable, "w/ CCS|w/o CCS"),
         Value > 0) %>%
  distinct(Variable) %>%
  nrow()

cat("Capacity Additions variables with data:", cap_add_vars_available, "\n")

if (cap_add_vars_available >= 4) {
  # PRIMARY TRACK: Capacity Additions (GW/yr) × job intensity
  cat("Using Capacity Additions (GW/yr) for jobs calculation\n")
  
  jobs_compass_raw <- compass_filtered %>%
    filter(Variable %in% cap_additions_fuel_map$Variable,
           Year >= 2020, Year <= 2100) %>%
    inner_join(cap_additions_fuel_map, by = "Variable") %>%
    mutate(GW = Value,   # already in GW/yr
           job_category = "oem") %>%
    left_join(
      job_factors_complete %>% select(region, fuel, category, job_intensity),
      by = c("Region" = "region", "fuel", "job_category" = "category")
    )
  
  # If oem category not available in job factors, try any available category
  if (all(is.na(jobs_compass_raw$job_intensity))) {
    cat("  OEM category not found — using first available category\n")
    first_cat <- job_factors_complete %>%
      distinct(category) %>% slice(1) %>% pull(category)
    jobs_compass_raw <- compass_filtered %>%
      filter(Variable %in% cap_additions_fuel_map$Variable,
             Year >= 2020, Year <= 2100) %>%
      inner_join(cap_additions_fuel_map, by = "Variable") %>%
      mutate(GW = Value, job_category = first_cat) %>%
      left_join(
        job_factors_complete %>% select(region, fuel, category, job_intensity),
        by = c("Region" = "region", "fuel", "job_category" = "category")
      )
  }
  
  jobs_compass <- jobs_compass_raw %>%
    filter(!is.na(job_intensity)) %>%
    mutate(jobs_thousands = GW * job_intensity / 1000)
  
} else {
  # FALLBACK TRACK: Secondary Energy (EJ/yr) → implied GW × OEM intensity
  cat("Capacity Additions sparse — falling back to SE|Electricity proxy\n")
  
  jobs_compass <- compass_filtered %>%
    filter(Variable %in% se_fuel_map$Variable,
           Year >= 2020, Year <= 2100) %>%
    inner_join(se_fuel_map, by = "Variable") %>%
    left_join(cap_factors, by = "fuel") %>%
    mutate(
      GW           = Value / (CF * 8760 * 3.6e-3),
      job_category = "oem"
    ) %>%
    left_join(
      job_factors_complete %>% select(region, fuel, category, job_intensity),
      by = c("Region" = "region", "fuel", "job_category" = "category")
    ) %>%
    filter(!is.na(job_intensity)) %>%
    mutate(jobs_thousands = GW * job_intensity / 1000)
}

jobs_by_tech <- jobs_compass %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Category, Year, tech_group) %>%
  summarise(jobs_thousands = sum(jobs_thousands, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Category, tech_group) %>%
  summarise(total_jobs_thousands = sum(jobs_thousands, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider(
    id_cols      = c(Model_Group, Model, Scenario, ModelGroup_Scenario,
                     Region, Category),
    names_from   = tech_group,
    values_from  = total_jobs_thousands,
    names_prefix = "jobs_",
    values_fill  = 0
  )

cat("Jobs rows:", nrow(jobs_by_tech), "\n")


# =============================================================================
# SECTION 9: DLE / DESIRE ANALYSIS
# =============================================================================

cat("\n=== SECTION 9: DLE / DESIRE Analysis ===\n")

# ---- 9a. DLE thresholds (GJ/capita/yr, Kikstra et al. 2021) ----------------
dle_thresholds <- tribble(
  ~Region,           ~res_comm_GJ, ~industry_GJ, ~transport_GJ,
  "R10AFRICA",             12.0,         8.0,          4.5,
  "R10CHINA+",             18.0,        14.0,          5.0,
  "R10EUROPE",             28.0,        16.0,          8.0,
  "R10INDIA+",             10.0,         8.5,          4.0,
  "R10NORTH_AM",           35.0,        18.0,         10.0
)

# ---- 9b. Service Efficiency Factor ------------------------------------------
sef_lookup <- expand_grid(Year = 2020:2100,
                          sector = c("res_comm", "industry", "transport")) %>%
  mutate(
    annual_rate = case_when(
      sector == "res_comm"  ~ 0.012,
      sector == "industry"  ~ 0.010,
      sector == "transport" ~ 0.015
    ),
    SEF = pmax(0.5, 1 - annual_rate * (Year - 2020))
  ) %>%
  select(Year, sector, SEF)

dle_thresh_long <- dle_thresholds %>%
  pivot_longer(c(res_comm_GJ, industry_GJ, transport_GJ),
               names_to  = "sector",
               values_to = "threshold_GJ_base") %>%
  mutate(sector = str_remove(sector, "_GJ"))

# ---- 9c. Sector energy from COMPASS -----------------------------------------
# Final Energy total
regional_fe_total <- compass_filtered %>%
  filter(Variable == "Final Energy") %>%
  select(Model_Group, Model, Scenario, ModelGroup_Scenario,
         Region, Year, Category, fe_total = Value)

# Industry and Transport directly available in COMPASS
regional_fe_sectors <- compass_filtered %>%
  filter(Variable %in% c("Final Energy|Industry",
                         "Final Energy|Transportation")) %>%
  mutate(sector = case_when(
    Variable == "Final Energy|Industry"       ~ "industry",
    Variable == "Final Energy|Transportation" ~ "transport"
  )) %>%
  select(Model_Group, Model, Scenario, ModelGroup_Scenario,
         Region, Year, Category, sector, energy_EJ = Value)

# res_comm as residual: total - industry - transport
regional_fe_wide <- regional_fe_sectors %>%
  pivot_wider(names_from = sector, values_from = energy_EJ) %>%
  left_join(regional_fe_total,
            by = c("Model_Group", "Model", "Scenario", "ModelGroup_Scenario",
                   "Region", "Year", "Category")) %>%
  mutate(
    res_comm = pmax(fe_total - coalesce(industry, 0) - coalesce(transport, 0), 0)
  )

cat("\nRes+comm residual summary (should be positive):\n")
summary(regional_fe_wide$res_comm)

energy_by_sector <- regional_fe_wide %>%
  select(Model_Group, Model, Scenario, ModelGroup_Scenario,
         Region, Year, Category, industry, transport, res_comm) %>%
  pivot_longer(c(res_comm, industry, transport),
               names_to  = "sector",
               values_to = "energy_EJ") %>%
  filter(!is.na(energy_EJ))

# ---- 9d. Population ---------------------------------------------------------
pop_data <- compass_filtered %>%
  filter(Variable == "Population") %>%
  select(Model_Group, Model, Scenario, ModelGroup_Scenario,
         Region, Year, Category, pop_millions = Value)

# ---- 9e. Per-capita energy vs SEF-adjusted threshold -----------------------
energy_vs_threshold <- energy_by_sector %>%
  left_join(pop_data,
            by = c("Model_Group", "Model", "Scenario", "ModelGroup_Scenario",
                   "Region", "Year", "Category")) %>%
  left_join(dle_thresh_long, by = c("Region", "sector")) %>%
  left_join(sef_lookup,      by = c("Year", "sector")) %>%
  filter(!is.na(pop_millions), pop_millions > 0,
         !is.na(threshold_GJ_base),
         !is.na(energy_EJ)) %>%
  mutate(
    energy_GJ_pc    = (energy_EJ * 1e9) / (pop_millions * 1e6),
    threshold_GJ_pc = threshold_GJ_base * SEF,
    gap_GJ_pc       = pmax(0, threshold_GJ_pc - energy_GJ_pc),
    gap_EJ_total    = gap_GJ_pc * (pop_millions * 1e6) / 1e9
  )

# 3-sector completeness filter
complete_sectors <- energy_vs_threshold %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Year, Category) %>%
  summarise(n_sectors = n_distinct(sector), .groups = "drop") %>%
  filter(n_sectors == 3)

energy_vs_threshold <- energy_vs_threshold %>%
  semi_join(complete_sectors,
            by = c("Model_Group", "Model", "Scenario", "ModelGroup_Scenario",
                   "Region", "Year", "Category"))

cat("\nRows retained after 3-sector completeness filter:",
    nrow(energy_vs_threshold), "\n")

# ---- 9f. Lognormal deprivation headcount ------------------------------------
energy_gini <- tribble(
  ~Region,        ~gini,
  "R10AFRICA",     0.45,
  "R10CHINA+",     0.38,
  "R10EUROPE",     0.25,
  "R10INDIA+",     0.42,
  "R10NORTH_AM",   0.28
) %>%
  mutate(sigma_ln = sqrt(2) * qnorm((gini + 1) / 2))

headcount_data <- energy_vs_threshold %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Year, Category, pop_millions) %>%
  summarise(
    energy_GJ_pc_total    = sum(energy_GJ_pc,    na.rm = TRUE),
    threshold_GJ_pc_total = sum(threshold_GJ_pc, na.rm = TRUE),
    gap_EJ_total          = sum(gap_EJ_total,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(energy_gini, by = "Region") %>%
  mutate(
    mu_ln            = log(pmax(energy_GJ_pc_total, 0.01)) - sigma_ln^2 / 2,
    deprivation_rate = pnorm(
      (log(pmax(threshold_GJ_pc_total, 0.01)) - mu_ln) / sigma_ln
    ),
    headcount_millions = deprivation_rate * pop_millions
  )

# ---- 9g. Emissions intensity (for implied CO2) ------------------------------
emissions_intensity <- compass_filtered %>%
  filter(Variable %in% c("Emissions|CO2|Energy", "Final Energy")) %>%
  pivot_wider(
    id_cols     = c(Model_Group, Model, Scenario, ModelGroup_Scenario,
                    Region, Year, Category),
    names_from  = Variable,
    values_from = Value
  ) %>%
  rename(co2_energy = `Emissions|CO2|Energy`,
         fe_EJ      = `Final Energy`) %>%
  filter(!is.na(co2_energy), !is.na(fe_EJ), fe_EJ > 0) %>%
  mutate(ei_MtCO2_per_EJ = (co2_energy * 1000) / fe_EJ)

# ---- 9h. Implied CO2 from closing the DLE gap ------------------------------
implied_emissions <- headcount_data %>%
  left_join(emissions_intensity,
            by = c("Model_Group", "Model", "Scenario", "ModelGroup_Scenario",
                   "Region", "Year", "Category")) %>%
  mutate(implied_CO2_GtCO2 = gap_EJ_total * ei_MtCO2_per_EJ / 1000)

# ---- 9i. Cumulate DLE metrics 2020-2050 ------------------------------------
dle_cumulative <- headcount_data %>%
  filter(Year >= 2020, Year <= 2050) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Category) %>%
  summarise(
    cumulative_gap_EJ       = sum(gap_EJ_total,       na.rm = TRUE),
    mean_headcount_millions = mean(headcount_millions, na.rm = TRUE),
    .groups = "drop"
  )

implied_cumulative <- implied_emissions %>%
  filter(Year >= 2020, Year <= 2050) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Category) %>%
  summarise(
    cumulative_implied_CO2_GtCO2 = sum(implied_CO2_GtCO2, na.rm = TRUE),
    .groups = "drop"
  )

cat("DLE cumulative rows:      ", nrow(dle_cumulative), "\n")
cat("Implied CO2 rows:         ", nrow(implied_cumulative), "\n")


# =============================================================================
# SECTION 10: MASTER DATASET
# =============================================================================

# Use Gt CO2 scalar for Total CDR in master (proxy=FALSE rows from cum_ccs)
cdr_for_master <- cdr_cumulative %>%
  filter(!proxy | Variable != "Total CDR") %>%
  select(-proxy)

df_master <- cdr_for_master %>%
  left_join(
    mortality_summary %>%
      select(Model, Scenario, Region, Category,
             cumulative_deaths_mln_pm25,
             cumulative_deaths_mln_o3,
             cumulative_deaths_mln_total),
    by = c("Model", "Scenario", "Region", "Category")
  ) %>%
  mutate(
    cumulative_deaths_mln = cumulative_deaths_mln_pm25
  ) %>%
  left_join(jobs_by_tech,
            by = c("Model_Group", "Model", "Scenario", "ModelGroup_Scenario",
                   "Region", "Category")) %>%
  left_join(dle_cumulative,
            by = c("Model_Group", "Model", "Scenario", "ModelGroup_Scenario",
                   "Region", "Category")) %>%
  left_join(implied_cumulative,
            by = c("Model_Group", "Model", "Scenario", "ModelGroup_Scenario",
                   "Region", "Category"))

cat("\n=== MASTER DATASET ===\n")
cat("Rows:       ", nrow(df_master), "\n")
cat("Scenarios:  ",
    n_distinct(paste(df_master$Model, df_master$Scenario)), "\n")
cat("Categories: ",
    paste(sort(unique(df_master$Category)), collapse=", "), "\n")

# DLE coverage check
cat("\nMortality coverage (non-NA rows in master):\n")
df_master %>%
  filter(Variable == "Total CDR") %>%
  summarise(
    n_total      = n(),
    n_mortality  = sum(!is.na(cumulative_deaths_mln)),
    pct_coverage = round(100 * n_mortality / n_total, 1)
  ) %>%
  print()

# Mortality coverage check
cat("\nMortality coverage (non-NA rows in master):\n")
df_master %>%
  filter(Variable == "Total CDR") %>%
  summarise(
    n_total     = n(),
    n_mortality = sum(!is.na(cumulative_deaths_mln)),
    pct_coverage = round(100 * n_mortality / n_total, 1)
  ) %>%
  print()


# =============================================================================
# SECTION 11: REGRESSIONS
# NOTE: Total CDR (Gt CO2) used for cross-scenario regressions.
#       Novel CDR / Fossil CCS / Renewable Capacity are proxy (EJ) —
#       tercile classification only, not regressions against DLE outcomes
#       in absolute terms.
# =============================================================================

cat("\n--- Regression coverage: n observations per Region x Category ---\n")
df_master %>%
  filter(Variable == "Total CDR",
         cumulative_gap_EJ > 0,
         Total_Value > 0,
         Region %in% regions_r10) %>%
  group_by(Region, Category) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(Region, Category) %>%
  print(n = Inf)

run_loglog <- function(data, x_col, y_col, min_obs = 5) {
  data %>%
    filter(.data[[x_col]] > 0, .data[[y_col]] > 0,
           Region %in% regions_r10) %>%
    mutate(log_x = log(.data[[x_col]]),
           log_y = log(.data[[y_col]])) %>%
    group_by(Region) %>%
    filter(n() >= min_obs) %>%
    group_modify(function(df, keys) {
      tryCatch({
        m <- lm(log_y ~ log_x, data = df)
        tidy(m, conf.int = TRUE) %>%
          filter(term == "log_x") %>%
          mutate(r_squared   = glance(m)$r.squared,
                 n_obs       = nrow(df),
                 x_var       = x_col,
                 y_var       = y_col,
                 significant = p.value < 0.05)
      }, error = function(e) {
        message("Skipped: ", keys$Region, " — ", e$message); tibble()
      })
    }) %>%
    ungroup()
}

# Regression outcomes
outcome_specs <- list(
  list(y = "cumulative_gap_EJ",
       label = "DLE Energy Gap (EJ)"),
  list(y = "mean_headcount_millions",
       label = "Deprivation Headcount (millions)"),
  list(y = "cumulative_implied_CO2_GtCO2",
       label = "Implied CO2 from Closing DLE Gap (GtCO2)"),
  list(y = "jobs_Renewables",
       label = "Renewable Energy Jobs (thousands, proxy)"),
  list(y = "jobs_Fossil",
       label = "Fossil Energy Jobs (thousands, proxy)"),
  list(y = "cumulative_deaths_mln",
       label = "Air Pollution Mortality (millions, PM2.5)")
)

reg_results <- map_dfr(outcome_specs, function(oc) {
  df_sub <- df_master %>% filter(Variable == "Total CDR")
  if (!oc$y %in% names(df_sub)) return(NULL)
  run_loglog(df_sub, "Total_Value", oc$y) %>%
    mutate(X_Type = "Total CDR (Gt CO2)", y_label = oc$label)
})

write.csv(reg_results,
          file.path(OUT_DIR, "regression_results_compass.csv"),
          row.names = FALSE)


# =============================================================================
# SECTION 11b: TERCILE CLASSIFICATION (High/Medium/Low CDR and RE)
# Uses proxy EJ-based variables for classification within COMPASS
# =============================================================================

classify_deployment <- function(df, x_var_name, label_prefix) {
  df %>%
    filter(Variable == x_var_name, Total_Value > 0,
           Region %in% regions_r10) %>%
    group_by(Region) %>%
    mutate(
      deployment_level = case_when(
        Total_Value <= quantile(Total_Value, 1/3, na.rm = TRUE) ~ "Low",
        Total_Value <= quantile(Total_Value, 2/3, na.rm = TRUE) ~ "Medium",
        TRUE                                                     ~ "High"
      ),
      deployment_level = factor(deployment_level,
                                levels = c("Low", "Medium", "High"))
    ) %>%
    ungroup() %>%
    mutate(deployment_var = label_prefix)
}

# Use proxy CDR/RE from cdr_cumulative for classification
cdr_for_class <- cdr_cumulative %>%
  left_join(
    df_master %>%
      select(Model, Scenario, Region, Category,
             cumulative_gap_EJ, mean_headcount_millions,
             cumulative_implied_CO2_GtCO2, jobs_Renewables, jobs_Fossil,
             cumulative_deaths_mln) %>%
      distinct(),
    by = c("Model", "Scenario", "Region", "Category")
  )

ccs_levels <- classify_deployment(cdr_for_class, "Total CDR",
                                  "CCS/CDR Level")
re_levels  <- classify_deployment(cdr_for_class, "Renewable Capacity",
                                  "RE Level")
deployment_data <- bind_rows(ccs_levels, re_levels)


# =============================================================================
# SECTION 12: FIGURES
# =============================================================================

x_axis_labels <- c(
  "Total CDR (Gt CO2)" = "Total CDR cumul. 2020-2100 (Gt CO2)",
  "Renewable Capacity" = "Renewable Capacity cumul. 2020-2100 (EJ)"
)

# ---- Figure 1: DLE gap time series ------------------------------------------
fig1 <- headcount_data %>%
  filter(Region %in% regions_r10, !is.na(gap_EJ_total)) %>%
  ggplot(aes(x = Year, y = gap_EJ_total,
             group = ModelGroup_Scenario)) +
  geom_line(alpha = 0.10, linewidth = 0.35, colour = "#4393c3") +
  stat_summary(aes(group = 1), fun = median, na.rm = TRUE,
               geom = "line", linewidth = 1.1, colour = "#084594") +
  facet_wrap(~ Region, scales = "free_y", ncol = 3) +
  scale_x_continuous(breaks = c(2020, 2035, 2050)) +
  coord_cartesian(xlim = c(2020, 2050)) +
  labs(
    title    = "Decent Living Energy Gap Over Time",
    subtitle = paste0("Thin = individual scenarios  |  Thick = overall median  |  ",
                      "COMPASS database (", n_distinct(paste(headcount_data$Model,
                                                             headcount_data$Scenario)), " scenarios)"),
    x = "Year", y = "Energy gap (EJ/yr)",
    caption = "DLE thresholds: Kikstra et al. (2021). SEF-adjusted for efficiency improvement."
  ) +
  theme_paper()

save_fig(fig1, "fig1_dle_gap_timeseries.png", 14, 8)

# ---- Figure 2: Headcount 2020 vs 2050 ---------------------------------------
fig2 <- headcount_data %>%
  filter(Region %in% regions_r10, Year %in% c(2020, 2050)) %>%
  group_by(Region, Year) %>%
  summarise(med_hc = median(headcount_millions, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(Year = factor(Year)) %>%
  ggplot(aes(x = Region, y = med_hc, fill = Year)) +
  geom_col(position = position_dodge(0.7), width = 0.65) +
  geom_text(aes(label = round(med_hc, 0)),
            position = position_dodge(0.7),
            hjust = -0.15, size = 3.2) +
  coord_flip() +
  scale_fill_manual(values = c("2020" = "#d73027", "2050" = "#4575b4")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Median Energy Deprivation Headcount: 2020 vs 2050",
    subtitle = "People below Decent Living Energy threshold  |  All COMPASS scenarios (C1–C4) pooled",
    x = NULL, y = "Headcount (millions)", fill = "Year"
  ) +
  theme_paper()

save_fig(fig2, "fig2_headcount_change.png", 14, 6)

# ---- Figure 3: DLE outcomes vs Total CDR ------------------------------------
scatter_dle <- function(y_col, y_label) {
  df_master %>%
    filter(Variable == "Total CDR",
           !is.na(.data[[y_col]]),
           Total_Value > 0,
           .data[[y_col]] > 0) %>%
    ggplot(aes(x = Total_Value, y = .data[[y_col]], colour = Category)) +
    geom_point(alpha = 0.25, size = 1.2) +
    geom_smooth(method = "lm", se = TRUE,
                alpha = 0.12, linewidth = 0.9) +
    facet_wrap(~ Region, scales = "free", ncol = 3) +
    scale_x_log10(labels = label_comma()) +
    scale_y_log10(labels = label_comma()) +
    scale_colour_manual(values = CATEGORY_COLORS, name = "Category") +
    labs(
      title    = paste0(y_label, "\nvs Total CDR Deployment"),
      subtitle = "Log-log  |  All COMPASS scenarios  |  coloured by C1/C2/C3/C4",
      x        = "Total CDR cumul. 2020-2100 (Gt CO2)",
      y        = y_label
    ) +
    theme_paper()
}

save_fig(scatter_dle("cumulative_gap_EJ", "DLE Energy Gap (EJ, cumul. 2020-2050)"),
         "fig3a_dle_gap_vs_totalcdr.png", 14, 8)
save_fig(scatter_dle("mean_headcount_millions", "Deprivation Headcount (millions, mean)"),
         "fig3b_headcount_vs_totalcdr.png", 14, 8)
save_fig(scatter_dle("cumulative_implied_CO2_GtCO2",
                     "Implied CO2 from Closing DLE Gap (GtCO2)"),
         "fig3c_impliedCO2_vs_totalcdr.png", 14, 8)

# ---- Figure 3d/e: Jobs vs Total CDR (proxy, interpret cautiously) -----------
if ("jobs_Renewables" %in% names(df_master)) {
  save_fig(scatter_dle("jobs_Renewables",
                       "Renewable Energy Jobs (thousands, OEM proxy)"),
           "fig3d_jobs_renew_vs_totalcdr.png", 14, 8)
  save_fig(scatter_dle("jobs_Fossil",
                       "Fossil Energy Jobs (thousands, OEM proxy)"),
           "fig3e_jobs_fossil_vs_totalcdr.png", 14, 8)
}

# ---- Figure 3f: Mortality vs Total CDR (full coverage) ----------------------
if (any(!is.na(df_master$cumulative_deaths_mln))) {
  save_fig(
    df_master %>%
      filter(Variable == "Total CDR",
             !is.na(cumulative_deaths_mln),
             Total_Value > 0,
             cumulative_deaths_mln > 0) %>%
      ggplot(aes(x = Total_Value, y = cumulative_deaths_mln,
                 colour = Category)) +
      geom_point(alpha = 0.4, size = 1.5) +
      geom_smooth(method = "lm", se = TRUE, alpha = 0.15) +
      facet_wrap(~ Region, scales = "free", ncol = 3) +
      scale_x_log10(labels = label_comma()) +
      scale_y_log10(labels = label_comma()) +
      scale_colour_manual(values = CATEGORY_COLORS, name = "Category") +
      labs(
        title    = "Air Pollution Mortality vs Total CDR",
        subtitle = "Log-log  |  All COMPASS scenarios (C1–C4)  |  rfasst/GEMM PM2.5 mortality",
        x        = "Total CDR cumul. 2020-2100 (Gt CO2)",
        y        = "Air pollution mortality (millions, 2020-2100)",
        caption  = "FUSION estimator  |  PM2.5 only  |  rfasst package via GEMM_mortality.R"
      ) +
      theme_paper(),
    "fig3f_mortality_vs_totalcdr.png", 14, 8
  )
}

# ---- Figure 4: Coefficient plots --------------------------------------------
if (nrow(reg_results) > 0) {
  fig4 <- reg_results %>%
    filter(!is.na(estimate)) %>%
    mutate(y_label = str_wrap(y_label, 28)) %>%
    ggplot(aes(x = Region, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "gray40", linewidth = 0.6) +
    geom_point(size = 2.2, colour = "#4393c3") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0.25, linewidth = 0.7, colour = "#4393c3") +
    geom_point(data = ~ filter(.x, significant),
               shape = 8, size = 3.5, colour = "#084594") +
    facet_wrap(~ y_label, scales = "free_x", ncol = 3) +
    coord_flip() +
    labs(
      title    = "Log-Log Elasticities: Wellbeing Outcomes vs Total CDR",
      subtitle = "All COMPASS scenarios pooled (C1–C4)  |  * = p < 0.05  |  rfasst PM2.5 mortality",
      x = "Region", y = "Elasticity (% outcome per 1% CDR)",
      caption  = "Positive = outcome increases with CDR  |  Negative = outcome decreases"
    ) +
    theme_paper(base_size = 9)
  
  save_fig(fig4, "fig4_coefficients.png", 16, 10)
}

# ---- Figure 5: DLE gap by Category ------------------------------------------
fig5 <- headcount_data %>%
  filter(Region %in% regions_r10, Year == 2050,
         !is.na(gap_EJ_total)) %>%
  left_join(
    compass_interp %>% distinct(Model, Scenario, Category),
    by = c("Model", "Scenario")
  ) %>%
  mutate(Category = coalesce(Category.x, Category.y)) %>%
  ggplot(aes(x = Region, y = gap_EJ_total, fill = Category)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.75,
               position = position_dodge(0.8), width = 0.65) +
  coord_flip() +
  scale_fill_manual(values = CATEGORY_COLORS) +
  scale_y_log10(labels = label_comma()) +
  labs(
    title    = "DLE Energy Gap in 2050 by Scenario Category",
    subtitle = "All COMPASS scenarios (C1–C4)  |  5 R10 regions",
    x = NULL, y = "Energy gap (EJ/yr, log scale)",
    caption  = "C1: ≤1.5°C  |  C2: 1.5°C overshoot  |  C3: ≤2°C  |  C4: >2°C"
  ) +
  theme_paper()

save_fig(fig5, "fig5_dle_by_category.png", 12, 7)

# ---- Figure 6: High/Low deployment comparison --------------------------------
plot_deployment_comparison <- function(dep_data, y_col, y_label, dep_var_label) {
  dep_data %>%
    filter(deployment_var == dep_var_label,
           !is.na(.data[[y_col]]),
           .data[[y_col]] > 0) %>%
    ggplot(aes(x = deployment_level, y = .data[[y_col]])) +
    geom_boxplot(outlier.size = 0.8, alpha = 0.75, fill = "#4393c3") +
    facet_wrap(~ Region, scales = "free_y", ncol = 3) +
    scale_y_log10(labels = label_comma()) +
    labs(
      title    = paste0(y_label, "\nby ", dep_var_label,
                        " — Low / Medium / High terciles"),
      subtitle = "All COMPASS scenarios (C1–C4) pooled  |  terciles within Region",
      x = dep_var_label, y = y_label
    ) +
    theme_paper()
}

dle_outcomes <- list(
  list(y = "cumulative_gap_EJ",
       label = "DLE Energy Gap (EJ)"),
  list(y = "mean_headcount_millions",
       label = "Deprivation Headcount (millions)"),
  list(y = "cumulative_implied_CO2_GtCO2",
       label = "Implied CO2 from Closing Gap (GtCO2)")
)

for (oc in dle_outcomes) {
  slug <- gsub("[^a-zA-Z0-9]", "_", tolower(oc$label))
  save_fig(
    plot_deployment_comparison(deployment_data, oc$y, oc$label, "CCS/CDR Level"),
    paste0("fig6a_ccs_", slug, ".png"), 14, 8
  )
  save_fig(
    plot_deployment_comparison(deployment_data, oc$y, oc$label, "RE Level"),
    paste0("fig6b_re_", slug, ".png"), 14, 8
  )
}


# =============================================================================
# SECTION 13: SAVE OUTPUTS
# =============================================================================

write.csv(df_master,
          file.path(OUT_DIR, "compass_master_dataset.csv"),
          row.names = FALSE)
write.csv(headcount_data,
          file.path(OUT_DIR, "compass_dle_headcount_timeseries.csv"),
          row.names = FALSE)
write.csv(cdr_cumulative,
          file.path(OUT_DIR, "compass_cdr_cumulative.csv"),
          row.names = FALSE)
write.csv(reg_results,
          file.path(OUT_DIR, "compass_regression_results.csv"),
          row.names = FALSE)

cat("\n=== COMPASS-ONLY ANALYSIS COMPLETE ===\n")
cat("Scenarios: ", n_distinct(paste(df_master$Model, df_master$Scenario)), "\n")
cat("Regions:   ", paste(sort(unique(df_master$Region)), collapse=", "), "\n")
cat("Outputs saved to:", OUT_DIR, "\n")
cat("\nKey notes:\n")
cat("  - Mortality: full coverage via rfasst/GEMM (FUSION estimator, PM2.5)\n")
cat("  - Jobs: EJ-based OEM proxy — indicative, not precise\n")
cat("  - Novel/Fossil CDR split: EJ proxy — used for classification only\n")
cat("  - Total CDR: Gt CO2 from COMPASS metadata — used in regressions\n")
cat("  - Categories: C1 (≤1.5°C), C2 (1.5°C overshoot), C3 (≤2°C), C4 (>2°C)\n")

