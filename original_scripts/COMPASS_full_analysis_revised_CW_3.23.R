# =============================================================================
# COMPASS CDR WELLBEING ANALYSIS — FULLY REVISED
#
# PIPELINE OVERVIEW:
#   1. Load compass_interp.rds (5-year timesteps, 2015–2100, real units)
#   2. Build 5-year timeseries dataset with CDR types, RE, fuel mix,
#      emissions, energy, population — for all scenarios × regions
#   3. Compute wellbeing outcomes (DLE, jobs, mortality) on the timeseries
#   4. Cumulate outcomes to ambition-specific windows:
#        1.5C (C1+C2): 2020–2060  |  2C (C3+C4): 2020–2075
#        [IPCC AR6 net-zero references retained as figure annotations only]
#   5. Cumulate CDR/RE deployment 2020–2100 for pathway classification
#   6. Build df_master (one row per scenario × region, outcomes as columns)
#   7. Four regression analyses:
#        (A) All scenarios, CDR deployment → wellbeing (log-log)
#        (B) All scenarios, RE deployment → wellbeing (log-log)
#        (C) High-CDR + High-RE OVERLAPPING (top tercile and top quartile)
#        (D) High-CDR + High-RE MUTUALLY EXCLUSIVE (top tercile and quartile)
#
# CDR VARIABLES (real units, MtCO2/yr from compass_interp):
#   Novel CDR      = Carbon Removal|Geological Storage|Direct Air Capture
#                  + Carbon Capture|Geological Storage|Biomass
#                  + Carbon Removal|Enhanced Weathering
#   Fossil CCS     = Carbon Capture|Energy|Fossil
#                  + Carbon Capture|Industrial Processes
#   Land-based CDR = Carbon Removal|Land Use
#   Total CDR      = Novel CDR + Fossil CCS + Land-based CDR
#   Renewable Cap  = Capacity|Electricity|Solar + Wind + Hydro + Nuclear
#                  + Biomass + Geothermal (GW stock)
#
# AMBITION GROUPS:
#   1.5C (High-Ambition):   C1 + C2  ->  cumulate outcomes 2020-2055
#   2C   (Medium-Ambition): C3 + C4  ->  cumulate outcomes 2020-2075
#
# IPCC AR6 NET-ZERO REFERENCES (used for figure reference lines only, NOT outcome windows):
#   C1: "early 2050s" (AR6 WG3 SPM C.2; Carbon Brief: 2050-2055)
#   C2: "2055-2060" (Carbon Brief 2022; AR6 WG3 Chapter 3)
#   C3: "2070-2075" (Carbon Brief 2022; AR6 WG3 Chapter 3)
#   C4: ~2075 (AR6 WG3 Table SPM.2)
#
# AGGREGATED R10 REGIONS: outcomes aggregated as population-weighted mean across R10 regions
# (no "World" region from COMPASS database used in wellbeing outcome figures)
# =============================================================================

library(tidyverse)
library(zoo)
library(scales)
library(broom)
library(ggplot2)
library(patchwork)
library(writexl)

COMPASS_DIR <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/COMPASS"
AR6_DIR     <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/AR6"
OUT_DIR     <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_only"
dir.create(OUT_DIR,                    showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT_DIR, "Figs"), showWarnings = FALSE)

save_fig <- function(p, name, w = 14, h = 8) {
  ggsave(file.path(OUT_DIR, "Figs", name), p,
         width = w, height = h, dpi = 300, bg = "white")
  message("Saved: Figs/", name)
}


# =============================================================================
# SECTION 1: CONFIGURATION
# =============================================================================

regions_r10  <- c("R10AFRICA", "R10CHINA+", "R10INDIA+",
                  "R10EUROPE", "R10NORTH_AM")
# World dropped — analyses run at R10 only, then aggregated to "All Regions"
# (population-weighted mean across R10 regions, added in df_master build)
cats_keep    <- c("C1", "C2", "C3", "C4")

# Wellbeing outcome cumulation windows — ambition-specific:
#   1.5C (C1+C2): 2020-2060  (C2 high-overshoot median net-zero ~2060; covers full drawdown)
#   2C   (C3+C4): 2020-2075  (C3+C4 median net-zero ~2070-2075 per IPCC AR6)
# C1 and C2 share the 2060 window since they are pooled as one ambition group.
WINDOW_C1  <- 2060L
WINDOW_C2  <- 2060L
WINDOW_C3  <- 2075L
WINDOW_C4  <- 2075L
START_YEAR <- 2020L

# Net-zero reference lines for figures (same values, used for vertical lines only)
WINDOW_15C <- 2060L   # 1.5C pooled window (C1+C2)
WINDOW_2C  <- 2075L   # 2C pooled window (C3+C4)

assign_ambition <- function(df, col = "Category") {
  df %>% mutate(Ambition = case_when(
    .data[[col]] %in% c("C1", "C2") ~ "1.5C (High-Ambition)",
    .data[[col]] %in% c("C3", "C4") ~ "2C (Medium-Ambition)",
    TRUE ~ NA_character_
  ))
}

# Per-category window lookup
get_window <- function(category_vec) {
  case_when(
    category_vec == "C1" ~ WINDOW_C1,
    category_vec == "C2" ~ WINDOW_C2,
    category_vec == "C3" ~ WINDOW_C3,
    category_vec == "C4" ~ WINDOW_C4,
    TRUE ~ WINDOW_C3   # fallback
  )
}

CATEGORY_COLORS <- c("C1" = "#1a9641", "C2" = "#74c476",
                     "C3" = "#fdae61", "C4" = "#d7191c")
AMB_COLORS      <- c("1.5C (High-Ambition)" = "#1a9641",
                     "2C (Medium-Ambition)"  = "#d7191c")
PATH5_COLORS    <- c("High-CDR only" = "#2166ac", "High-RE only" = "#d6604d",
                     "Both High" = "#542788", "Low (both)" = "#4dac26")
PATH2_COLORS    <- c("High-CDR" = "#2166ac", "High-RE" = "#d6604d")

theme_paper <- function(base_size = 10) {
  theme_bw(base_size = base_size) +
    theme(
      strip.background   = element_rect(fill = "#1c3a5e", colour = NA),
      strip.text         = element_text(colour = "white", face = "bold"),
      legend.position    = "bottom",
      panel.grid.minor   = element_blank(),
      plot.title    = element_text(face = "bold", hjust = 0),
      plot.subtitle = element_text(hjust = 0, color = "gray40"),
      plot.caption  = element_text(color = "gray50", hjust = 0, size = 7)
    )
}


# =============================================================================
# SECTION 2: LOAD DATA
# =============================================================================

cat("=== SECTION 2: Loading data ===\n")

compass_interp <- readRDS(file.path(COMPASS_DIR, "compass_interp.rds"))
# Note: compass_cum_ccs no longer used — Total CDR comes from computed timeseries

cat("Scenarios:  ", n_distinct(paste(compass_interp$Model,
                                     compass_interp$Scenario)), "\n")
cat("Regions:    ", paste(sort(unique(compass_interp$Region)), collapse=", "), "\n")
cat("Categories: ", paste(sort(unique(compass_interp$Category)), collapse=", "), "\n")
cat("Years:      ", paste(range(compass_interp$Year), collapse="-"), "\n")
cat("World rows: ", sum(compass_interp$Region == "World"), "\n")


# =============================================================================
# SECTION 2b: FILTER TO VETTED-FOR-2025 SCENARIOS
# Source: COMPASS dataset "vetted for 2025" column — filtered manually.
# Only scenarios in this list are retained for all downstream analysis.
# =============================================================================

vetted_scenarios <- c(
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
)

n_before <- n_distinct(compass_interp$Scenario)
compass_interp <- compass_interp %>%
  filter(Scenario %in% vetted_scenarios)
n_after <- n_distinct(compass_interp$Scenario)

dropped <- setdiff(
  unique(compass_interp$Scenario),   # already filtered, so use vetted list diff
  vetted_scenarios
)
cat(sprintf(
  "Vetted filter: %d → %d unique scenarios (%d dropped)\n",
  n_before, n_after, n_before - n_after
))

# Warn about any vetted scenarios not found in the loaded data
not_found <- setdiff(vetted_scenarios, unique(compass_interp$Scenario))
if (length(not_found) > 0) {
  cat(sprintf(
    "NOTE: %d vetted scenario(s) not present in compass_interp (may be absent from pull):\n",
    length(not_found)
  ))
  for (s in sort(not_found)) cat("  -", s, "\n")
}


# =============================================================================
# SECTION 3: BUILD TIMESERIES DATASET
# =============================================================================

cat("\n=== SECTION 3: Building timeseries ===\n")

compass_ts <- compass_interp %>%
  filter(
    Region   %in% regions_r10,   # R10 only — World not used in wellbeing outcomes
    Category %in% cats_keep,
    Year >= 2015, Year <= 2100,
    !is.na(Value)
  ) %>%
  mutate(
    Model_Group         = "COMPASS",
    ModelGroup_Scenario = paste("COMPASS", Scenario, sep = "_")
  )

# ---- 3a. CDR variables (MtCO2/yr) -------------------------------------------
cdr_vars_needed <- c("Novel CDR", "Fossil CCS", "Land-based CDR", "Total CDR")
cdr_present <- compass_ts %>%
  filter(Variable %in% cdr_vars_needed, Value > 0) %>%
  distinct(Variable) %>% pull(Variable)

cat("CDR variables present:", paste(cdr_present, collapse=", "), "\n")

novel_cdr_ts <- if ("Novel CDR" %in% cdr_present) {
  compass_ts %>% filter(Variable == "Novel CDR")
} else {
  cat("  Computing Novel CDR from raw components\n")
  compass_ts %>%
    filter(Variable %in% c(
      "Carbon Removal|Geological Storage|Direct Air Capture",
      "Carbon Capture|Geological Storage|Biomass",
      "Carbon Removal|Enhanced Weathering"
    )) %>%
    group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
             Region, Category, Year) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    mutate(Variable = "Novel CDR")
}

fossil_ccs_ts <- if ("Fossil CCS" %in% cdr_present) {
  compass_ts %>% filter(Variable == "Fossil CCS")
} else {
  cat("  Computing Fossil CCS from raw components\n")
  compass_ts %>%
    filter(Variable %in% c(
      "Carbon Capture|Energy|Fossil",
      "Carbon Capture|Industrial Processes"
    )) %>%
    group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
             Region, Category, Year) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    mutate(Variable = "Fossil CCS")
}

land_cdr_ts <- if ("Land-based CDR" %in% cdr_present) {
  compass_ts %>% filter(Variable == "Land-based CDR")
} else {
  cat("  Computing Land-based CDR from Carbon Removal|Land Use\n")
  compass_ts %>%
    filter(Variable == "Carbon Removal|Land Use") %>%
    mutate(Variable = "Land-based CDR")
}

total_cdr_ts <- if ("Total CDR" %in% cdr_present) {
  compass_ts %>% filter(Variable == "Total CDR")
} else {
  cat("  Computing Total CDR as sum of components\n")
  bind_rows(novel_cdr_ts, fossil_ccs_ts, land_cdr_ts) %>%
    group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
             Region, Category, Year) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    mutate(Variable = "Total CDR")
}

# ---- 3b. Renewable capacity (GW) --------------------------------------------
re_vars <- c(
  "Capacity|Electricity|Solar",
  "Capacity|Electricity|Wind",
  "Capacity|Electricity|Hydro",
  "Capacity|Electricity|Nuclear",
  "Capacity|Electricity|Biomass",
  "Capacity|Electricity|Geothermal"
)

re_components_ts <- compass_ts %>% filter(Variable %in% re_vars)
re_total_ts <- re_components_ts %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Category, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Variable = "Renewable Capacity")

cat("RE capacity covered by:",
    paste(unique(re_components_ts$Variable), collapse=", "), "\n")

# ---- 3c. Capacity additions by fuel (GW/yr) ---------------------------------
cap_add_vars <- c(
  "Capacity Additions|Electricity|Solar",
  "Capacity Additions|Electricity|Wind",
  "Capacity Additions|Electricity|Hydro",
  "Capacity Additions|Electricity|Nuclear",
  "Capacity Additions|Electricity|Biomass",
  "Capacity Additions|Electricity|Geothermal",
  "Capacity Additions|Electricity|Coal",
  "Capacity Additions|Electricity|Gas",
  "Capacity Additions|Electricity|Oil"
)
cap_additions_ts <- compass_ts %>% filter(Variable %in% cap_add_vars)

se_elec_vars <- c(
  "Secondary Energy|Electricity|Solar",
  "Secondary Energy|Electricity|Wind",
  "Secondary Energy|Electricity|Hydro",
  "Secondary Energy|Electricity|Nuclear",
  "Secondary Energy|Electricity|Biomass",
  "Secondary Energy|Electricity|Coal",
  "Secondary Energy|Electricity|Gas",
  "Secondary Energy|Electricity|Oil"
)
se_elec_ts <- compass_ts %>% filter(Variable %in% se_elec_vars)

use_cap_additions <- nrow(filter(cap_additions_ts, Value > 0)) > 100
cat("Using Capacity Additions for jobs:", use_cap_additions, "\n")

# ---- 3d-3f. Energy, population, emissions -----------------------------------
energy_ts <- compass_ts %>%
  filter(Variable %in% c("Final Energy","Final Energy|Industry",
                         "Final Energy|Transportation"))

pop_ts <- compass_ts %>% filter(Variable == "Population")

emissions_ts <- compass_ts %>%
  filter(Variable %in% c("Emissions|CO2","Emissions|CO2|Energy")) %>%
  mutate(Variable = recode(Variable,
                           "Emissions|CO2"        = "CO2 Emissions",
                           "Emissions|CO2|Energy" = "CO2 Energy Emissions"))


# =============================================================================
# SECTION 4: NET-ZERO DATES
# =============================================================================

cat("\n=== SECTION 4: Net-zero dates ===\n")

scenario_netzero <- emissions_ts %>%
  filter(Variable == "CO2 Emissions",
         Region %in% regions_r10,
         Year >= 2020, Year <= 2100) %>%
  group_by(Model, Scenario, Category, Region) %>%
  mutate(base_2020 = Value[Year == 2020][1]) %>%
  filter(!is.na(base_2020), base_2020 > 0) %>%
  summarise(
    netzero_year = {
      yrs <- Year[Value <= 0.05 * base_2020[1]]
      if (length(yrs) > 0) as.integer(min(yrs)) else 2100L
    },
    .groups = "drop"
  )

cat("Net-zero summary by Category (median, % reaching net-zero):\n")
scenario_netzero %>%
  group_by(Category) %>%
  summarise(
    pct_reach = round(100 * mean(netzero_year < 2100), 1),
    p25 = quantile(netzero_year[netzero_year < 2100], 0.25, na.rm=TRUE),
    med = median(netzero_year[netzero_year < 2100], na.rm=TRUE),
    p75 = quantile(netzero_year[netzero_year < 2100], 0.75, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  print()

cat("\nFixed IPCC AR6 windows used:\n")
cat("  1.5C (C1+C2): 2020-", WINDOW_15C, "\n")
cat("  2C   (C3+C4): 2020-", WINDOW_2C,  "\n")


# =============================================================================
# SECTION 5: CUMULATE CDR/RE FOR CLASSIFICATION (2020-2100)
# =============================================================================

cat("\n=== SECTION 5: Cumulative CDR/RE for classification ===\n")

# Cumulate all CDR/RE from computed timeseries 2020-2100 (for pathway classification).
# NOTE: CDR/RE deployment cumulates to 2100 for pathway classification (High-CDR / High-RE).
#       Wellbeing outcomes: 1.5C (C1+C2) → 2060; 2C (C3+C4) → 2075.
# Total CDR comes from the timeseries (Novel + Fossil + Land-based), NOT the
# metadata scalar. This ensures regression x-axis matches CDR breakdown figures.
# Units: MtCO2 (CDR components), GW-yr (RE capacity)
cdr_cumulative <- bind_rows(
  novel_cdr_ts, fossil_ccs_ts, land_cdr_ts, total_cdr_ts, re_total_ts
) %>%
  filter(Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Category, Variable) %>%
  summarise(Total_Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  filter(Total_Value > 0) %>%
  mutate(proxy = FALSE)

cat("CDR cumulative rows:", nrow(cdr_cumulative), "\n")
cat("Coverage by variable (scenarios with non-zero values):\n")
cdr_cumulative %>%
  group_by(Variable) %>%
  summarise(n_scenarios = n_distinct(paste(Model, Scenario)),
            n_regions   = n_distinct(Region),
            med_val     = round(median(Total_Value), 1),
            .groups = "drop") %>%
  print()

# ---- 5b. World-region deployment for pathway classification ------------------
# Attempt to use compass_interp Region == "World" rows directly for Total CDR
# and Renewable Capacity. These are the IAM-reported global aggregates (not a
# sum of R10 regions, which may differ due to rounding or coverage gaps).
# Falls back to summed R10 if World coverage < 50% of scenario universe.

world_re_cap_vars <- c(
  "Capacity|Electricity|Solar",   "Capacity|Electricity|Wind",
  "Capacity|Electricity|Hydro",   "Capacity|Electricity|Nuclear",
  "Capacity|Electricity|Biomass", "Capacity|Electricity|Geothermal"
)
world_cdr_component_vars <- c(
  "Carbon Removal|Geological Storage|Direct Air Capture",
  "Carbon Capture|Geological Storage|Biomass",
  "Carbon Removal|Enhanced Weathering",
  "Carbon Capture|Energy|Fossil",
  "Carbon Capture|Industrial Processes",
  "Carbon Removal|Land Use"
)

# Pull World-region rows from compass_interp (no R10 filter applied here)
world_ts_raw <- compass_interp %>%
  filter(Region == "World",
         Category %in% cats_keep,
         Year >= 2020, Year <= 2100,
         !is.na(Value)) %>%
  mutate(Model_Group         = "COMPASS",
         ModelGroup_Scenario = paste("COMPASS", Scenario, sep = "_"))

n_world_scens <- n_distinct(paste(world_ts_raw$Model, world_ts_raw$Scenario))
n_r10_scens   <- n_distinct(paste(compass_ts$Model,   compass_ts$Scenario))
cat("World-region rows in compass_interp:", nrow(world_ts_raw), "\n")
cat("World-region unique scenarios:", n_world_scens,
    "vs R10 scenarios:", n_r10_scens, "\n")

# Build World Total CDR: prefer pre-computed "Total CDR" variable; else sum components
world_cdr_direct <- world_ts_raw %>%
  filter(Variable == "Total CDR", Value > 0)

world_cdr_computed <- world_ts_raw %>%
  filter(Variable %in% world_cdr_component_vars) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario, Category, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  filter(Value > 0) %>%
  mutate(Region = "World", Variable = "Total CDR")

world_total_cdr_ts <- if (nrow(world_cdr_direct) > 0) {
  cat("  Using pre-computed World Total CDR rows:", nrow(world_cdr_direct), "\n")
  world_cdr_direct
} else if (nrow(world_cdr_computed) > 0) {
  cat("  Computing World Total CDR from components:", nrow(world_cdr_computed), "\n")
  world_cdr_computed
} else {
  cat("  No World CDR rows found\n")
  tibble()
}

# Build World Renewable Capacity
world_re_ts <- world_ts_raw %>%
  filter(Variable %in% world_re_cap_vars, Value > 0) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario, Category, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = "World", Variable = "Renewable Capacity")

cat("  World CDR timeseries rows:", nrow(world_total_cdr_ts), "\n")
cat("  World RE timeseries rows: ", nrow(world_re_ts), "\n")

# Cumulate World-region deployment 2020-2100
world_cumulative_direct <- bind_rows(world_total_cdr_ts, world_re_ts) %>%
  filter(Year >= 2020, Year <= 2100) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario, Category, Variable) %>%
  summarise(Total_Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  filter(Total_Value > 0)

# Summed-R10 values (already computed in cdr_cumulative above)
world_cumulative_sumR10 <- cdr_cumulative %>%
  filter(Variable %in% c("Total CDR", "Renewable Capacity")) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario, Category, Variable) %>%
  summarise(Total_Value = sum(Total_Value, na.rm = TRUE), .groups = "drop") %>%
  filter(Total_Value > 0)

# Coverage: what fraction of R10 scenarios have World-region CDR values?
n_world_cdr_direct <- world_cumulative_direct %>%
  filter(Variable == "Total CDR") %>%
  { n_distinct(paste(.$Model, .$Scenario)) }
coverage_pct <- if (n_r10_scens > 0) round(100 * n_world_cdr_direct / n_r10_scens, 1) else 0

cat(sprintf("World-region CDR coverage: %d / %d scenarios (%.1f%%)\n",
            n_world_cdr_direct, n_r10_scens, coverage_pct))

# Decision: use real World rows if >= 50% coverage, else fall back to summed R10
USE_WORLD_REGION <- coverage_pct >= 50

if (USE_WORLD_REGION) {
  cat("PATHWAY CLASSIFICATION: using real World-region deployment values\n")
  # For any scenario missing World rows, fill gap with summed R10
  world_deploy_for_classification <- world_cumulative_sumR10 %>%
    select(Model, Scenario, Category, Variable, total_sumR10 = Total_Value) %>%
    left_join(
      world_cumulative_direct %>%
        select(Model, Scenario, Category, Variable, total_world = Total_Value),
      by = c("Model","Scenario","Category","Variable")
    ) %>%
    mutate(
      filled_from_r10 = is.na(total_world),        # flag BEFORE coalesce
      Total_Value     = coalesce(total_world, total_sumR10)
    ) %>%
    select(Model, Scenario, Category, Variable, Total_Value, filled_from_r10)
  
  cat(sprintf("  (Gaps: %d scenario×variable rows filled with summed R10)\n",
              sum(world_deploy_for_classification$filled_from_r10)))
  
  world_deploy_for_classification <- world_deploy_for_classification %>%
    select(-filled_from_r10)
} else {
  cat("PATHWAY CLASSIFICATION: fallback to summed R10 (World coverage too low)\n")
  world_deploy_for_classification <- world_cumulative_sumR10 %>%
    select(Model, Scenario, Category, Variable, Total_Value)
}


# =============================================================================
# SECTION 6: PATHWAY CLASSIFICATION
# =============================================================================

cat("\n=== SECTION 6: Pathway classification ===\n")

classify_pathways <- function(top_frac = 1/3) {
  # Uses world_deploy_for_classification (set in Section 5b):
  #   Real "World" region values if coverage >= 50%, summed R10 otherwise.
  # See USE_WORLD_REGION flag and coverage diagnostics printed above.
  world_cdr <- world_deploy_for_classification %>%
    filter(Variable == "Total CDR") %>%
    group_by(Model, Scenario, Category) %>%
    summarise(total_cdr = sum(Total_Value, na.rm = TRUE), .groups = "drop")
  
  world_re <- world_deploy_for_classification %>%
    filter(Variable == "Renewable Capacity") %>%
    group_by(Model, Scenario, Category) %>%
    summarise(total_re = sum(Total_Value, na.rm = TRUE), .groups = "drop")
  
  world_deploy <- world_cdr %>%
    full_join(world_re, by = c("Model","Scenario","Category")) %>%
    assign_ambition("Category") %>%
    filter(!is.na(Ambition))
  
  world_deploy %>%
    group_by(Ambition) %>%
    mutate(
      cdr_thresh    = quantile(total_cdr, 1 - top_frac, na.rm = TRUE),
      re_thresh     = quantile(total_re,  1 - top_frac, na.rm = TRUE),
      high_cdr      = total_cdr >= cdr_thresh,
      high_re       = total_re  >= re_thresh,
      # Overlapping (can be in both)
      Pathway_overlap = case_when(
        high_cdr & high_re  ~ "Both High",
        high_cdr & !high_re ~ "High-CDR only",
        !high_cdr & high_re ~ "High-RE only",
        TRUE                ~ "Low (both)"
      ),
      # Mutually exclusive
      high_cdr_only = high_cdr & !high_re,
      high_re_only  = high_re  & !high_cdr,
      Pathway_excl  = case_when(
        high_cdr_only ~ "High-CDR",
        high_re_only  ~ "High-RE",
        TRUE          ~ NA_character_
      ),
      threshold_label = paste0("top_", round(top_frac * 100), "pct")
    ) %>%
    ungroup()
}

pathway_tercile  <- classify_pathways(1/3)
pathway_quartile <- classify_pathways(1/4)

cat("Pathway counts (tercile, overlapping):\n")
pathway_tercile %>% count(Ambition, Pathway_overlap) %>%
  arrange(Ambition, Pathway_overlap) %>% print()
cat("\nPathway counts (tercile, mutually exclusive):\n")
pathway_tercile %>% filter(!is.na(Pathway_excl)) %>%
  count(Ambition, Pathway_excl) %>% print()
cat("\nPathway counts (quartile, overlapping):\n")
pathway_quartile %>% count(Ambition, Pathway_overlap) %>%
  arrange(Ambition, Pathway_overlap) %>% print()


# =============================================================================
# SECTION 6b: SHARE-BASED PATHWAY CLASSIFICATION
#
# Alternative to relative tercile: classify scenarios based on technology
# share metrics that are more interpretable and stable across scenario lists.
#
# High-CDR: CDR share of normalised (CDR + RE) deployment >= 30%
#   Both CDR (MtCO2) and RE (GW·yr) are normalised to [0,1] across all
#   scenarios first so units are comparable, then:
#   cdr_share = cdr_norm / (cdr_norm + re_norm)
#
# High-RE: RE capacity share of total electricity capacity >= 60%
#   Numerator  : Renewable Capacity (Solar+Wind+Hydro+Nuclear+Biomass+Geo)
#   Denominator: All electricity capacity technologies summed
#   Both cumulated 2020-2100 (GW·yr)
#
# Thresholds:
#   CDR_SHARE_THRESH : 0.30
#   RE_SHARE_THRESH  : 0.60
#
# Mutual exclusivity: if a scenario meets both thresholds, it is assigned
# to whichever share is relatively higher (cdr_share vs re_share_norm).
# =============================================================================

cat("\n=== SECTION 6b: Share-based pathway classification ===\n")

CDR_SHARE_THRESH <- 0.30
RE_SHARE_THRESH  <- 0.60

# ---- Step 1: CDR share = normalised CDR / (normalised CDR + normalised RE) --
share_base <- world_deploy_for_classification %>%
  filter(Variable %in% c("Total CDR", "Renewable Capacity")) %>%
  group_by(Model, Scenario, Category, Variable) %>%
  summarise(total = sum(Total_Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Variable, values_from = total) %>%
  rename(total_cdr = `Total CDR`, total_re = `Renewable Capacity`) %>%
  assign_ambition("Category") %>%
  filter(!is.na(Ambition)) %>%
  group_by(Ambition) %>%              # <-- normalise WITHIN ambition group
  mutate(
    cdr_norm  = (total_cdr - min(total_cdr, na.rm = TRUE)) /
      (max(total_cdr, na.rm = TRUE) - min(total_cdr, na.rm = TRUE)),
    re_norm   = (total_re  - min(total_re,  na.rm = TRUE)) /
      (max(total_re,  na.rm = TRUE) - min(total_re,  na.rm = TRUE)),
    cdr_share = if_else((cdr_norm + re_norm) > 0,
                        cdr_norm / (cdr_norm + re_norm),
                        NA_real_)
  ) %>%
  ungroup()

cat("CDR share distribution by ambition (threshold =", CDR_SHARE_THRESH, "):\n")
share_base %>%
  group_by(Ambition) %>%
  summarise(
    n                = n(),
    p10              = round(quantile(cdr_share, 0.10, na.rm = TRUE), 3),
    p25              = round(quantile(cdr_share, 0.25, na.rm = TRUE), 3),
    median           = round(quantile(cdr_share, 0.50, na.rm = TRUE), 3),
    p75              = round(quantile(cdr_share, 0.75, na.rm = TRUE), 3),
    p90              = round(quantile(cdr_share, 0.90, na.rm = TRUE), 3),
    pct_above_thresh = round(100 * mean(cdr_share >= CDR_SHARE_THRESH, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>% print()

# ---- Step 2: RE share = RE capacity / total electricity capacity -------------
# All individual capacity variables to sum for total electricity
all_cap_vars <- c(
  "Capacity|Electricity|Solar",
  "Capacity|Electricity|Wind",
  "Capacity|Electricity|Hydro",
  "Capacity|Electricity|Nuclear",
  "Capacity|Electricity|Biomass",
  "Capacity|Electricity|Geothermal",
  "Capacity|Electricity|Coal",
  "Capacity|Electricity|Gas",
  "Capacity|Electricity|Oil",
  "Capacity|Electricity|Other",
  "Capacity|Electricity|Hydrogen"
)

re_cap_vars <- c(
  "Capacity|Electricity|Solar",
  "Capacity|Electricity|Wind",
  "Capacity|Electricity|Hydro",
  "Capacity|Electricity|Nuclear",
  "Capacity|Electricity|Biomass",
  "Capacity|Electricity|Geothermal"
)

# Cumulative GW·yr 2020-2100 from compass_ts (R10 regions)
cap_cumulative <- compass_ts %>%
  filter(Variable %in% all_cap_vars,
         Region %in% regions_r10,
         Year >= 2020, Year <= 2100,
         !is.na(Value)) %>%
  mutate(is_re = Variable %in% re_cap_vars) %>%
  group_by(Model, Scenario, Category) %>%
  summarise(
    total_elec_cap = sum(Value[Variable %in% all_cap_vars], na.rm = TRUE),
    re_cap         = sum(Value[is_re],                       na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    re_share = if_else(total_elec_cap > 0,
                       re_cap / total_elec_cap,
                       NA_real_)
  )

cat("\nRE capacity share distribution by ambition (threshold =", RE_SHARE_THRESH, "):\n")
cap_cumulative %>%
  left_join(share_base %>% select(Model, Scenario, Ambition),
            by = c("Model", "Scenario")) %>%
  filter(!is.na(Ambition)) %>%
  group_by(Ambition) %>%
  summarise(
    n                = n(),
    p10              = round(quantile(re_share, 0.10, na.rm = TRUE), 3),
    p25              = round(quantile(re_share, 0.25, na.rm = TRUE), 3),
    median           = round(quantile(re_share, 0.50, na.rm = TRUE), 3),
    p75              = round(quantile(re_share, 0.75, na.rm = TRUE), 3),
    p90              = round(quantile(re_share, 0.90, na.rm = TRUE), 3),
    pct_above_thresh = round(100 * mean(re_share >= RE_SHARE_THRESH, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>% print()

# ---- Step 3: Combine and classify -------------------------------------------
classify_pathways_share <- function(cdr_thresh = CDR_SHARE_THRESH,
                                    re_thresh  = RE_SHARE_THRESH) {
  share_base %>%
    left_join(
      cap_cumulative %>% select(Model, Scenario, re_share),
      by = c("Model", "Scenario")
    ) %>%
    mutate(
      meets_cdr = !is.na(cdr_share) & cdr_share >= cdr_thresh,
      meets_re  = !is.na(re_share)  & re_share  >= re_thresh,
      # For scenarios meeting both thresholds: assign to whichever share
      # is relatively stronger (cdr_share vs re_share, both already in [0,1])
      high_cdr = case_when(
        meets_cdr & meets_re  ~ cdr_share >= re_share,   # both: pick dominant
        meets_cdr & !meets_re ~ TRUE,
        TRUE                  ~ FALSE
      ),
      high_re = case_when(
        meets_re & meets_cdr  ~ re_share > cdr_share,    # both: pick dominant
        meets_re & !meets_cdr ~ TRUE,
        TRUE                  ~ FALSE
      ),
      high_cdr_only = high_cdr & !high_re,
      high_re_only  = high_re  & !high_cdr,
      Pathway_overlap = case_when(
        high_cdr & high_re  ~ "Both High",
        high_cdr & !high_re ~ "High-CDR only",
        !high_cdr & high_re ~ "High-RE only",
        TRUE                ~ "Low (both)"
      ),
      Pathway_excl = case_when(
        high_cdr_only ~ "High-CDR",
        high_re_only  ~ "High-RE",
        TRUE          ~ NA_character_
      ),
      threshold_label = paste0("cdr_share>=", cdr_thresh,
                               "_re_share>=", re_thresh)
    )
}

pathway_threshold <- classify_pathways_share()

cat("\nShare-based classification counts (overlapping):\n")
pathway_threshold %>%
  count(Ambition, Pathway_overlap) %>%
  arrange(Ambition, Pathway_overlap) %>% print()

cat("\nShare-based classification counts (mutually exclusive):\n")
pathway_threshold %>%
  filter(!is.na(Pathway_excl)) %>%
  count(Ambition, Pathway_excl) %>% print()

cat("\nComparison — tercile vs share-based (mutually exclusive):\n")
bind_rows(
  pathway_tercile %>%
    filter(!is.na(Pathway_excl)) %>%
    count(Ambition, Pathway_excl) %>%
    mutate(Method = "Tercile"),
  pathway_threshold %>%
    filter(!is.na(Pathway_excl)) %>%
    count(Ambition, Pathway_excl) %>%
    mutate(Method = "Share-based")
) %>%
  arrange(Ambition, Pathway_excl, Method) %>%
  print()

cat("\nShare distributions for classified scenarios:\n")
pathway_threshold %>%
  filter(!is.na(Pathway_excl)) %>%
  group_by(Ambition, Pathway_excl) %>%
  summarise(
    n            = n(),
    med_cdr_share = round(median(cdr_share, na.rm = TRUE), 3),
    med_re_share  = round(median(re_share,  na.rm = TRUE), 3),
    .groups = "drop"
  ) %>% print()

cat("\nNOTE: Adjust CDR_SHARE_THRESH and RE_SHARE_THRESH at the top of this\n")
cat("section if group sizes are too small (<10) or too large (>60% of scenarios).\n")

# Save alongside tercile
saveRDS(pathway_threshold, file.path(COMPASS_DIR, "compass_pathway_threshold.rds"))
write.csv(pathway_threshold,
          file.path(OUT_DIR, "compass_pathway_threshold.csv"), row.names = FALSE)
cat("Saved: compass_pathway_threshold.rds / .csv\n")


# =============================================================================
# SECTION 7: AIR POLLUTION MORTALITY (ambition-specific window)
# =============================================================================

cat("\n=== SECTION 7: Air pollution mortality ===\n")

mortality_summary <- readRDS(
  file.path(COMPASS_DIR, "compass_mortality_summary.rds")
)

# Standardise column names to title case (only rename if title case not already present)
if ("model" %in% names(mortality_summary) && !"Model" %in% names(mortality_summary))
  mortality_summary <- mortality_summary %>%
  rename(Model = model, Scenario = scenario)
if ("r10_region" %in% names(mortality_summary) && !"Region" %in% names(mortality_summary))
  mortality_summary <- mortality_summary %>% rename(Region = r10_region)
# Drop any remaining lowercase duplicates
mortality_summary <- mortality_summary %>%
  select(-any_of(c("model", "scenario", "r10_region")))

cat("Mortality scenarios:", n_distinct(paste(mortality_summary$Model,
                                             mortality_summary$Scenario)), "\n")

# Annual timeseries for windowed cumulation
mort_r10_path <- file.path(COMPASS_DIR, "compass_mortality_r10.csv")

if (file.exists(mort_r10_path)) {
  mortality_annual <- read_csv(mort_r10_path, show_col_types = FALSE)
  if ("model" %in% names(mortality_annual) && !"Model" %in% names(mortality_annual))
    mortality_annual <- mortality_annual %>%
      rename(Model = model, Scenario = scenario)
  if ("year" %in% names(mortality_annual) && !"Year" %in% names(mortality_annual))
    mortality_annual <- mortality_annual %>% rename(Year = year)
  if ("r10_region" %in% names(mortality_annual) && !"Region" %in% names(mortality_annual))
    mortality_annual <- mortality_annual %>% rename(Region = r10_region)
  # Drop any remaining lowercase duplicates
  mortality_annual <- mortality_annual %>%
    select(-any_of(c("model", "scenario", "year", "r10_region")))
  
  # Find deaths column
  deaths_col <- intersect(names(mortality_annual),
                          c("deaths_pm25","FUSION","deaths_total"))[1]
  cat("Using mortality column:", deaths_col, "\n")
  
  # Join Category from compass_interp — check name matching first
  cat("  mortality_annual columns:", paste(names(mortality_annual), collapse=", "), "\n")
  cat("  mortality_annual Model sample:", head(unique(mortality_annual$Model), 3), "\n")
  
  # Category lookup from compass_interp
  cat_lookup <- compass_interp %>% distinct(Model, Scenario, Category)
  
  # Diagnostic: check model name overlap
  n_overlap <- length(intersect(unique(mortality_annual$Model),
                                unique(cat_lookup$Model)))
  cat("  Model overlap (mortality vs interp):", n_overlap, "\n")
  cat("  Sample mortality models:", paste(head(unique(mortality_annual$Model), 3), collapse="; "), "\n")
  cat("  Sample interp models:   ", paste(head(unique(cat_lookup$Model),      3), collapse="; "), "\n")
  
  mortality_joined <- mortality_annual %>%
    left_join(cat_lookup, by = c("Model","Scenario"))
  
  n_matched <- sum(!is.na(mortality_joined$Category))
  cat("  Rows with Category after join:", n_matched, "/", nrow(mortality_joined), "\n")
  
  if (n_matched == 0) {
    warning("Category join returned 0 matches — falling back to mortality_summary")
    mort_deaths_col <- intersect(names(mortality_summary),
                                 c("cumulative_deaths_mln_pm25",
                                   "cumulative_deaths_mln",
                                   "deaths_pm25"))[1]
    cat("  Using mortality_summary column:", mort_deaths_col, "\n")
    mortality_cumulative <- mortality_summary %>%
      assign_ambition("Category") %>%
      filter(!is.na(Ambition)) %>%
      mutate(
        window_frac = (get_window(Category) - START_YEAR) / 80,
        cumulative_deaths_mln = .data[[mort_deaths_col]] * window_frac
      ) %>%
      select(Model, Scenario, Category, Ambition, Region, cumulative_deaths_mln)
  } else {
    mortality_cumulative <- mortality_joined %>%
      filter(!is.na(Category)) %>%
      assign_ambition("Category") %>%
      filter(!is.na(Ambition), Year >= START_YEAR) %>%
      mutate(window_end = get_window(Category)) %>%
      filter(Year <= window_end) %>%
      group_by(Model, Scenario, Category, Ambition, Region) %>%
      summarise(
        cumulative_deaths_mln = sum(.data[[deaths_col]] * 10, na.rm=TRUE) / 1e6,
        .groups = "drop"
      )
  }
  cat("  mortality_cumulative rows:", nrow(mortality_cumulative), "\n")
} else {
  warning("compass_mortality_r10.csv not found — scaling from cumulative summary")
  mort_deaths_col <- intersect(names(mortality_summary),
                               c("cumulative_deaths_mln_pm25",
                                 "cumulative_deaths_mln",
                                 "deaths_pm25"))[1]
  cat("  Using mortality_summary column:", mort_deaths_col, "\n")
  mortality_cumulative <- mortality_summary %>%
    assign_ambition("Category") %>%
    filter(!is.na(Ambition)) %>%
    mutate(
      window_frac = (get_window(Category) - START_YEAR) / 80,
      cumulative_deaths_mln = .data[[mort_deaths_col]] * window_frac
    ) %>%
    select(Model, Scenario, Category, Ambition, Region, cumulative_deaths_mln)
}

cat("Mortality cumulative rows:", nrow(mortality_cumulative), "\n")


# =============================================================================
# SECTION 8: ENERGY JOBS (ambition-specific window)
# =============================================================================

cat("\n=== SECTION 8: Energy jobs ===\n")

job_factors_complete <- read.csv(file.path(AR6_DIR, "job_factors_complete.csv"))

cap_additions_fuel_map <- tribble(
  ~Variable,                                    ~fuel,        ~tech_group,
  "Capacity Additions|Electricity|Solar",       "solar_pv",   "Renewables",
  "Capacity Additions|Electricity|Wind",        "wind_on",    "Renewables",
  "Capacity Additions|Electricity|Hydro",       "hydro",      "Renewables",
  "Capacity Additions|Electricity|Nuclear",     "nuclear",    "Renewables",
  "Capacity Additions|Electricity|Biomass",     "biomass",    "Renewables",
  "Capacity Additions|Electricity|Geothermal",  "geothermal", "Renewables",
  "Capacity Additions|Electricity|Coal",        "coal",       "Fossil",
  "Capacity Additions|Electricity|Gas",         "gas",        "Fossil",
  "Capacity Additions|Electricity|Oil",         "oil",        "Fossil"
)

se_fuel_map <- tribble(
  ~Variable,                               ~fuel,        ~tech_group,
  "Secondary Energy|Electricity|Solar",    "solar_pv",   "Renewables",
  "Secondary Energy|Electricity|Wind",     "wind_on",    "Renewables",
  "Secondary Energy|Electricity|Hydro",    "hydro",      "Renewables",
  "Secondary Energy|Electricity|Nuclear",  "nuclear",    "Renewables",
  "Secondary Energy|Electricity|Biomass",  "biomass",    "Renewables",
  "Secondary Energy|Electricity|Coal",     "coal",       "Fossil",
  "Secondary Energy|Electricity|Gas",      "gas",        "Fossil",
  "Secondary Energy|Electricity|Oil",      "oil",        "Fossil"
)

cap_factors <- tribble(
  ~fuel,         ~CF,
  "solar_pv",    0.18,
  "wind_on",     0.25,
  "hydro",       0.40,
  "nuclear",     0.85,
  "coal",        0.55,
  "gas",         0.45,
  "oil",         0.40,
  "biomass",     0.75,
  "geothermal",  0.85
)

# ---- Map capacity stock variables to fuel names (for stock-diff fallback) ----
# Use only aggregate variables (no w/ CCS / w/o CCS) to avoid double-counting.
# e.g. "Capacity|Electricity|Coal" already includes both CCS and non-CCS coal.
cap_stock_fuel_map <- tribble(
  ~Variable,                          ~fuel,        ~tech_group,
  "Capacity|Electricity|Solar",       "solar_pv",   "Renewables",
  "Capacity|Electricity|Wind",        "wind_on",    "Renewables",
  "Capacity|Electricity|Hydro",       "hydro",      "Renewables",
  "Capacity|Electricity|Nuclear",     "nuclear",    "Renewables",
  "Capacity|Electricity|Biomass",     "biomass",    "Renewables",
  "Capacity|Electricity|Geothermal",  "geothermal", "Renewables",
  "Capacity|Electricity|Coal",        "coal",       "Fossil",
  "Capacity|Electricity|Gas",         "gas",        "Fossil",
  "Capacity|Electricity|Oil",         "oil",        "Fossil"
)
# Confirm these are the aggregate-only variables (no w/ CCS suffix)
# compass_ts may also have Coal|w/ CCS, Coal|w/o CCS etc — those are excluded
# intentionally since the aggregate Coal already sums them

# ---- Identify which scenario×region combos have capacity additions data ------
scens_with_additions <- cap_additions_ts %>%
  filter(Value > 0, Year >= START_YEAR) %>%
  distinct(Model, Scenario, Region)

scens_needing_stockdiff <- compass_ts %>%
  filter(Variable %in% cap_stock_fuel_map$Variable,
         Year >= START_YEAR) %>%
  distinct(Model, Scenario, Region) %>%
  anti_join(scens_with_additions, by = c("Model","Scenario","Region"))

cat("Jobs track coverage:\n")
cat("  Scenarios with Capacity Additions:  ",
    n_distinct(paste(scens_with_additions$Model, scens_with_additions$Scenario,
                     scens_with_additions$Region)), "\n")
cat("  Scenarios using stock-diff fallback:",
    n_distinct(paste(scens_needing_stockdiff$Model, scens_needing_stockdiff$Scenario,
                     scens_needing_stockdiff$Region)), "\n")

# ---- TRACK 1: Capacity Additions (reported directly) -------------------------
jobs_raw_additions <- cap_additions_ts %>%
  semi_join(scens_with_additions, by = c("Model","Scenario","Region")) %>%
  filter(Year >= START_YEAR) %>%
  inner_join(cap_additions_fuel_map, by = "Variable") %>%
  mutate(GW = Value, job_category = "oem") %>%
  left_join(job_factors_complete %>%
              select(region, fuel, category, job_intensity),
            by = c("Region"="region","fuel","job_category"="category"))

# ---- TRACK 2: Stock difference (implied additions = diff in GW stock) --------
# For models that report Capacity|Electricity|* but not Capacity Additions|*
jobs_raw_stockdiff <- compass_ts %>%
  filter(Variable %in% cap_stock_fuel_map$Variable,
         Year >= START_YEAR) %>%
  semi_join(scens_needing_stockdiff, by = c("Model","Scenario","Region")) %>%
  inner_join(cap_stock_fuel_map, by = "Variable") %>%
  arrange(Model, Scenario, Region, fuel, Year) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Category, fuel, tech_group) %>%
  mutate(
    # Year-on-year diff in stock = implied new additions each period
    # Divide by 5 to get GW/yr (5-year timesteps), floor at 0 (no negative additions)
    GW = pmax(0, (Value - lag(Value, default = first(Value))) / 5)
  ) %>%
  ungroup() %>%
  filter(Year > min(Year)) %>%   # drop first year (no lag available)
  mutate(job_category = "oem") %>%
  left_join(job_factors_complete %>%
              select(region, fuel, category, job_intensity),
            by = c("Region"="region","fuel","job_category"="category"))

cat("  Track 1 (additions) rows:", nrow(jobs_raw_additions), "\n")
cat("  Track 2 (stock-diff) rows:", nrow(jobs_raw_stockdiff), "\n")

# ---- Combine both tracks -----------------------------------------------------
jobs_raw <- bind_rows(jobs_raw_additions, jobs_raw_stockdiff)

jobs_annual <- jobs_raw %>%
  filter(!is.na(job_intensity)) %>%
  mutate(jobs_thousands = GW * job_intensity / 1000)

# Cumulate to ambition-specific window
jobs_cumulative <- jobs_annual %>%
  assign_ambition("Category") %>%
  filter(!is.na(Ambition)) %>%
  mutate(window_end = get_window(Category)) %>%
  filter(Year <= window_end) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Category, Ambition, tech_group) %>%
  summarise(total_jobs = sum(jobs_thousands, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    id_cols    = c(Model_Group, Model, Scenario, ModelGroup_Scenario,
                   Region, Category, Ambition),
    names_from = tech_group, values_from = total_jobs,
    names_prefix = "jobs_", values_fill = 0
  )

# Ensure Renewables and Fossil columns always exist even if no data
if (!"jobs_Renewables" %in% names(jobs_cumulative))
  jobs_cumulative <- jobs_cumulative %>% mutate(jobs_Renewables = NA_real_)
if (!"jobs_Fossil" %in% names(jobs_cumulative))
  jobs_cumulative <- jobs_cumulative %>% mutate(jobs_Fossil = NA_real_)

cat("Jobs cumulative rows:", nrow(jobs_cumulative), "\n")
cat("Jobs columns:", paste(grep("jobs_", names(jobs_cumulative), value=TRUE), collapse=", "), "\n")


# =============================================================================
# SECTION 9: DLE ANALYSIS (ambition-specific window)
# =============================================================================

cat("\n=== SECTION 9: DLE analysis ===\n")

dle_thresholds <- tribble(
  ~Region,        ~res_comm_GJ, ~industry_GJ, ~transport_GJ,
  "R10AFRICA",          12.0,         8.0,          4.5,
  "R10CHINA+",          18.0,        14.0,          5.0,
  "R10EUROPE",          28.0,        16.0,          8.0,
  "R10INDIA+",          10.0,         8.5,          4.0,
  "R10NORTH_AM",        35.0,        18.0,         10.0
)

sef_lookup <- expand_grid(
  Year   = unique(compass_ts$Year),
  sector = c("res_comm","industry","transport")
) %>%
  mutate(
    annual_rate = case_when(
      sector == "res_comm"  ~ 0.012,
      sector == "industry"  ~ 0.010,
      sector == "transport" ~ 0.015
    ),
    SEF = pmax(0.5, 1 - annual_rate * (Year - 2020))
  )

# Total threshold per region = sum of three sectoral thresholds (for fallback)
dle_thresholds_total <- dle_thresholds %>%
  mutate(total_GJ = res_comm_GJ + industry_GJ + transport_GJ) %>%
  select(Region, total_GJ)

# SEF for total: weighted average of three sector rates
sef_total <- expand_grid(Year = unique(compass_ts$Year)) %>%
  mutate(
    SEF_total = pmax(0.5, 1 - 0.012 * (Year - 2020))  # use res_comm rate as proxy
  )

dle_thresh_long <- dle_thresholds %>%
  pivot_longer(c(res_comm_GJ, industry_GJ, transport_GJ),
               names_to = "sector", values_to = "threshold_GJ_base") %>%
  mutate(sector = str_remove(sector, "_GJ"))

fe_total_r10 <- energy_ts %>%
  filter(Variable == "Final Energy", Region %in% regions_r10) %>%
  select(Model_Group, Model, Scenario, ModelGroup_Scenario,
         Region, Year, Category, fe_total = Value)

fe_sectors_r10 <- energy_ts %>%
  filter(Variable %in% c("Final Energy|Industry","Final Energy|Transportation"),
         Region %in% regions_r10) %>%
  mutate(sector = case_when(
    Variable == "Final Energy|Industry"       ~ "industry",
    Variable == "Final Energy|Transportation" ~ "transport"
  )) %>%
  select(Model_Group, Model, Scenario, ModelGroup_Scenario,
         Region, Year, Category, sector, energy_EJ = Value)

fe_wide <- fe_sectors_r10 %>%
  pivot_wider(names_from = sector, values_from = energy_EJ) %>%
  left_join(fe_total_r10,
            by = c("Model_Group","Model","Scenario","ModelGroup_Scenario",
                   "Region","Year","Category")) %>%
  mutate(res_comm = pmax(fe_total - coalesce(industry,0) -
                           coalesce(transport,0), 0))

energy_by_sector <- fe_wide %>%
  select(Model_Group, Model, Scenario, ModelGroup_Scenario,
         Region, Year, Category, industry, transport, res_comm) %>%
  pivot_longer(c(res_comm,industry,transport),
               names_to = "sector", values_to = "energy_EJ") %>%
  filter(!is.na(energy_EJ))

pop_r10 <- pop_ts %>%
  filter(Region %in% regions_r10) %>%
  select(Model_Group, Model, Scenario, ModelGroup_Scenario,
         Region, Year, Category, pop_millions = Value)

# ---- TRACK 1: 3-sector (preferred, where all sectors available) -------------
energy_vs_threshold_3s <- energy_by_sector %>%
  left_join(pop_r10,
            by = c("Model_Group","Model","Scenario","ModelGroup_Scenario",
                   "Region","Year","Category")) %>%
  left_join(dle_thresh_long, by = c("Region","sector")) %>%
  left_join(sef_lookup, by = c("Year","sector")) %>%
  filter(!is.na(pop_millions), pop_millions > 0,
         !is.na(threshold_GJ_base), !is.na(energy_EJ)) %>%
  mutate(
    energy_GJ_pc    = (energy_EJ * 1e9) / (pop_millions * 1e6),
    threshold_GJ_pc = threshold_GJ_base * SEF,
    gap_GJ_pc       = pmax(0, threshold_GJ_pc - energy_GJ_pc),
    gap_EJ_total    = gap_GJ_pc * (pop_millions * 1e6) / 1e9
  )

complete_3s <- energy_vs_threshold_3s %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Year, Category) %>%
  summarise(n = n_distinct(sector), .groups = "drop") %>%
  filter(n == 3) %>%
  select(-n) %>%
  mutate(dle_track = "3-sector")

energy_vs_threshold_3s <- semi_join(energy_vs_threshold_3s, complete_3s,
                                    by = c("Model_Group","Model","Scenario","ModelGroup_Scenario",
                                           "Region","Year","Category"))

# ---- TRACK 2: 1-sector fallback (total FE vs summed threshold) ---------------
# Used for scenario-region-years where sectoral breakdown is unavailable
complete_3s_keys <- complete_3s %>%
  select(Model_Group, Model, Scenario, ModelGroup_Scenario,
         Region, Year, Category)

energy_vs_threshold_1s <- fe_total_r10 %>%
  anti_join(complete_3s_keys,
            by = c("Model_Group","Model","Scenario","ModelGroup_Scenario",
                   "Region","Year","Category")) %>%
  left_join(pop_r10,
            by = c("Model_Group","Model","Scenario","ModelGroup_Scenario",
                   "Region","Year","Category")) %>%
  left_join(dle_thresholds_total, by = "Region") %>%
  left_join(sef_total, by = "Year") %>%
  filter(!is.na(pop_millions), pop_millions > 0,
         !is.na(fe_total), !is.na(total_GJ)) %>%
  mutate(
    sector          = "total",
    energy_GJ_pc    = (fe_total * 1e9) / (pop_millions * 1e6),
    threshold_GJ_pc = total_GJ * SEF_total,
    gap_GJ_pc       = pmax(0, threshold_GJ_pc - energy_GJ_pc),
    gap_EJ_total    = gap_GJ_pc * (pop_millions * 1e6) / 1e9,
    energy_EJ       = fe_total,
    dle_track       = "1-sector"
  )

cat("DLE track coverage:\n")
cat("  3-sector rows:", nrow(energy_vs_threshold_3s),
    "| scenario-region-years:", nrow(complete_3s), "\n")
cat("  1-sector fallback rows:", nrow(energy_vs_threshold_1s), "\n")

# ---- Combine both tracks -------------------------------------------------------
# Standardise columns for headcount calculation
energy_vs_threshold <- bind_rows(
  energy_vs_threshold_3s %>%
    mutate(dle_track = "3-sector"),
  energy_vs_threshold_1s %>%
    select(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Year, Category, pop_millions, sector,
           energy_GJ_pc, threshold_GJ_pc, gap_GJ_pc, gap_EJ_total,
           dle_track)
)

# Lognormal deprivation headcount
energy_gini <- tribble(
  ~Region,        ~gini,
  "R10AFRICA",    0.45,
  "R10CHINA+",    0.38,
  "R10EUROPE",    0.25,
  "R10INDIA+",    0.42,
  "R10NORTH_AM",  0.28
) %>% mutate(sigma_ln = sqrt(2) * qnorm((gini + 1) / 2))

headcount_data <- energy_vs_threshold %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Year, Category, pop_millions, dle_track) %>%
  summarise(
    energy_GJ_pc_total    = sum(energy_GJ_pc,    na.rm=TRUE),
    threshold_GJ_pc_total = sum(threshold_GJ_pc, na.rm=TRUE),
    gap_EJ_total          = sum(gap_EJ_total,     na.rm=TRUE),
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

cat("headcount_data rows:", nrow(headcount_data), "\n")
headcount_data %>% count(dle_track) %>% print()

# Emissions intensity for implied CO2
emissions_intensity <- compass_ts %>%
  filter(Variable %in% c("Emissions|CO2|Energy","Final Energy"),
         Region %in% regions_r10) %>%
  # values_fn = mean guards against duplicate rows per id+variable
  pivot_wider(
    id_cols    = c(Model_Group,Model,Scenario,ModelGroup_Scenario,
                   Region,Year,Category),
    names_from  = Variable,
    values_from = Value,
    values_fn   = mean
  ) %>%
  # Rename only columns that actually exist
  rename(any_of(c(co2_energy = "Emissions|CO2|Energy",
                  fe_EJ      = "Final Energy"))) %>%
  filter(!is.na(co2_energy), !is.na(fe_EJ), fe_EJ > 0) %>%
  mutate(ei_MtCO2_per_EJ = (co2_energy * 1000) / fe_EJ)

implied_emissions <- headcount_data %>%
  left_join(emissions_intensity,
            by = c("Model_Group","Model","Scenario","ModelGroup_Scenario",
                   "Region","Year","Category")) %>%
  mutate(implied_CO2_GtCO2 = gap_EJ_total * ei_MtCO2_per_EJ / 1000)

# Cumulate to ambition-specific window
dle_cumulative <- headcount_data %>%
  assign_ambition("Category") %>%
  filter(!is.na(Ambition), Year >= START_YEAR) %>%
  mutate(window_end = get_window(Category)) %>%
  filter(Year <= window_end) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Category, Ambition) %>%
  summarise(
    cumulative_gap_EJ       = sum(gap_EJ_total,       na.rm=TRUE),
    mean_headcount_millions = mean(headcount_millions, na.rm=TRUE),
    .groups = "drop"
  )

implied_cumulative <- implied_emissions %>%
  assign_ambition("Category") %>%
  filter(!is.na(Ambition), Year >= START_YEAR) %>%
  mutate(window_end = get_window(Category)) %>%
  filter(Year <= window_end) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Category, Ambition) %>%
  summarise(
    cumulative_implied_CO2_GtCO2 = sum(implied_CO2_GtCO2, na.rm=TRUE),
    .groups = "drop"
  )

cat("DLE cumulative rows:     ", nrow(dle_cumulative), "\n")
cat("Implied CO2 rows:        ", nrow(implied_cumulative), "\n")


# =============================================================================
# SECTION 10: MASTER DATASET
# =============================================================================

cat("\n=== SECTION 10: Building df_master ===\n")

# Build df_master — join on Category only (not Ambition) to avoid missing column
# Ambition is added once at the end via assign_ambition
# All outcome joins use minimal key (Model, Scenario, Region, Category)
# to avoid silent mismatches from Model_Group / ModelGroup_Scenario formatting
df_master <- cdr_cumulative %>%
  select(-proxy) %>%
  left_join(
    mortality_cumulative %>%
      select(Model, Scenario, Category, Region, cumulative_deaths_mln),
    by = c("Model","Scenario","Category","Region")
  ) %>%
  left_join(
    jobs_cumulative %>%
      select(Model, Scenario, Region, Category, jobs_Renewables, jobs_Fossil),
    by = c("Model","Scenario","Region","Category")
  ) %>%
  left_join(
    dle_cumulative %>%
      select(Model, Scenario, Region, Category,
             cumulative_gap_EJ, mean_headcount_millions),
    by = c("Model","Scenario","Region","Category")
  ) %>%
  left_join(
    implied_cumulative %>%
      select(Model, Scenario, Region, Category, cumulative_implied_CO2_GtCO2),
    by = c("Model","Scenario","Region","Category")
  ) %>%
  assign_ambition("Category")   # add Ambition once here for all downstream use

# Add "Aggregated R10 regions" row — population-weighted mean across R10
# Uses 2030 population as weight (mid-window reference year)
pop_weights <- pop_ts %>%
  filter(Region %in% regions_r10, Year == 2030) %>%
  group_by(Model, Scenario, Region) %>%
  summarise(pop_weight = median(Value, na.rm = TRUE), .groups = "drop")

df_master_agg <- df_master %>%
  left_join(pop_weights, by = c("Model", "Scenario", "Region")) %>%
  filter(!is.na(pop_weight)) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Category, Ambition, Variable) %>%
  summarise(
    Total_Value = weighted.mean(Total_Value, pop_weight, na.rm = TRUE),
    across(any_of(c("cumulative_deaths_mln", "jobs_Renewables", "jobs_Fossil",
                    "cumulative_gap_EJ", "mean_headcount_millions",
                    "cumulative_implied_CO2_GtCO2")),
           ~ {
             vals <- .x[!is.na(.x)]
             wts  <- pop_weight[!is.na(.x)]
             if (length(vals) == 0) NA_real_
             else weighted.mean(vals, wts, na.rm = TRUE)
           }),
    .groups = "drop"
  ) %>%
  mutate(Region = "Aggregated R10 regions")

df_master <- bind_rows(df_master, df_master_agg)

cat("df_master rows:", nrow(df_master), "\n")
cat("Scenarios:    ", n_distinct(paste(df_master$Model, df_master$Scenario)), "\n")

cat("\nOutcome coverage (% non-NA, Total CDR rows):\n")
df_master %>%
  filter(Variable == "Total CDR") %>%
  summarise(
    n = n(),
    across(any_of(c("cumulative_deaths_mln", "jobs_Renewables", "jobs_Fossil",
                    "cumulative_gap_EJ", "mean_headcount_millions",
                    "cumulative_implied_CO2_GtCO2")),
           ~ round(100 * mean(!is.na(.x)), 1),
           .names = "pct_{.col}")
  ) %>% print()


# =============================================================================
# SECTION 11: REGRESSIONS
#
# (A) All scenarios, Total CDR -> outcomes (log-log, by Region x Ambition)
# (B) All scenarios, RE Capacity -> outcomes (log-log, by Region x Ambition)
# (C) High-CDR + High-RE overlapping, tercile and quartile thresholds
# (D) High-CDR + High-RE mutually exclusive, tercile and quartile thresholds
# =============================================================================

cat("\n=== SECTION 11: Regressions ===\n")

outcome_specs <- list(
  list(y="cumulative_deaths_mln",        label="Air Pollution Mortality
(million deaths, 2020–net-zero)"),
  list(y="cumulative_implied_CO2_GtCO2", label="CO2 Cost of DLE Gap
(GtCO2, 2020–net-zero)"),
  list(y="cumulative_gap_EJ",            label="DLE Energy Gap
(EJ, cumul. 2020–net-zero)"),
  list(y="mean_headcount_millions",      label="Energy Deprivation Headcount
(millions, 2020–net-zero)"),
  list(y="jobs_Renewables",              label="Renewable Energy Jobs
(thousands, cumul. 2020–net-zero)"),
  list(y="jobs_Fossil",                  label="Fossil Energy Jobs
(thousands, cumul. 2020–net-zero)")
)

run_loglog <- function(data, x_col, y_col, min_obs = 5) {
  data %>%
    assign_ambition("Category") %>%
    filter(!is.na(Ambition),
           .data[[x_col]] > 0, .data[[y_col]] > 0,
           !is.na(.data[[x_col]]), !is.na(.data[[y_col]])) %>%
    group_by(Region, Ambition) %>%
    filter(n() >= min_obs) %>%
    group_modify(function(df, keys) {
      tryCatch({
        fml <- as.formula(paste0("log(`", y_col, "`) ~ log(`", x_col, "`)"))
        m   <- lm(fml, data = df)
        tidy(m, conf.int = TRUE) %>%
          filter(str_detect(term, "log")) %>%
          mutate(r_squared = glance(m)$r.squared,
                 n_obs = nrow(df), x_var = x_col, y_var = y_col,
                 significant = p.value < 0.05,
                 model_type = "log-log")
      }, error = function(e) {
        message("Skipped: ", keys$Region, "/", keys$Ambition,
                " — ", e$message); tibble()
      })
    }) %>%
    ungroup()
}

# Raw OLS (untransformed y ~ x) — same structure as run_loglog
run_ols <- function(data, x_col, y_col, min_obs = 5) {
  data %>%
    assign_ambition("Category") %>%
    filter(!is.na(Ambition),
           .data[[x_col]] > 0, !is.na(.data[[x_col]]),
           !is.na(.data[[y_col]])) %>%
    group_by(Region, Ambition) %>%
    filter(n() >= min_obs) %>%
    group_modify(function(df, keys) {
      tryCatch({
        fml <- as.formula(paste0("`", y_col, "` ~ `", x_col, "`"))
        m   <- lm(fml, data = df)
        tidy(m, conf.int = TRUE) %>%
          filter(term == x_col) %>%
          mutate(r_squared = glance(m)$r.squared,
                 n_obs = nrow(df), x_var = x_col, y_var = y_col,
                 significant = p.value < 0.05,
                 model_type = "OLS")
      }, error = function(e) {
        message("OLS skipped: ", keys$Region, "/", keys$Ambition,
                " — ", e$message); tibble()
      })
    }) %>%
    ungroup()
}

# (A) All scenarios, CDR -> outcomes — log-log and raw OLS
cat("A: All scenarios, CDR\n")
reg_A <- map_dfr(outcome_specs, function(oc) {
  df_sub <- df_master %>% filter(Variable == "Total CDR")
  if (!oc$y %in% names(df_sub)) return(NULL)
  bind_rows(
    run_loglog(df_sub, "Total_Value", oc$y) %>%
      mutate(X_Type = "Total CDR (GtCO2)", y_label = oc$label,
             regression = "A_All_CDR", threshold_label = "all"),
    run_ols(df_sub, "Total_Value", oc$y) %>%
      mutate(X_Type = "Total CDR (GtCO2)", y_label = oc$label,
             regression = "A_All_CDR_OLS", threshold_label = "all")
  )
})

# (B) All scenarios, RE -> outcomes — log-log and raw OLS
cat("B: All scenarios, RE\n")
reg_B <- map_dfr(outcome_specs, function(oc) {
  df_sub <- df_master %>% filter(Variable == "Renewable Capacity")
  if (!oc$y %in% names(df_sub)) return(NULL)
  bind_rows(
    run_loglog(df_sub, "Total_Value", oc$y) %>%
      mutate(X_Type = "Renewable Capacity (GW yr)", y_label = oc$label,
             regression = "B_All_RE", threshold_label = "all"),
    run_ols(df_sub, "Total_Value", oc$y) %>%
      mutate(X_Type = "Renewable Capacity (GW yr)", y_label = oc$label,
             regression = "B_All_RE_OLS", threshold_label = "all")
  )
})

# Helper: run C and D regressions for a given pathway classification
run_CD_regressions <- function(pathway_df, thresh_label) {
  # Join pathway info onto df_master — join on Model+Scenario only
  # (Ambition already present in df_master from assign_ambition in Section 10)
  df_cls <- df_master %>%
    filter(Variable == "Total CDR") %>%
    left_join(
      pathway_df %>% select(Model, Scenario,
                            Pathway_overlap, Pathway_excl,
                            total_cdr, total_re),
      by = c("Model","Scenario")
    )
  
  # (C) Overlapping — include High-CDR only, High-RE only, Both High
  df_C <- df_cls %>%
    filter(Pathway_overlap %in% c("High-CDR only","High-RE only","Both High"))
  
  reg_C <- map_dfr(outcome_specs, function(oc) {
    if (!oc$y %in% names(df_C)) return(NULL)
    bind_rows(
      run_loglog(df_C, "total_cdr", oc$y) %>%
        mutate(X_Type="Total CDR (GtCO2)", y_label=oc$label,
               regression="C_Overlap_CDR", threshold_label=thresh_label),
      run_loglog(df_C, "total_re",  oc$y) %>%
        mutate(X_Type="Renewable Capacity (GW yr)", y_label=oc$label,
               regression="C_Overlap_RE", threshold_label=thresh_label),
      run_ols(df_C, "total_cdr", oc$y) %>%
        mutate(X_Type="Total CDR (GtCO2)", y_label=oc$label,
               regression="C_Overlap_CDR_OLS", threshold_label=thresh_label),
      run_ols(df_C, "total_re",  oc$y) %>%
        mutate(X_Type="Renewable Capacity (GW yr)", y_label=oc$label,
               regression="C_Overlap_RE_OLS", threshold_label=thresh_label)
    )
  })
  
  # (D) Mutually exclusive
  df_D_cdr <- df_cls %>% filter(Pathway_excl == "High-CDR")
  df_D_re  <- df_cls %>% filter(Pathway_excl == "High-RE")
  
  reg_D <- map_dfr(outcome_specs, function(oc) {
    if (!oc$y %in% names(df_D_cdr)) return(NULL)
    bind_rows(
      run_loglog(df_D_cdr, "total_cdr", oc$y) %>%
        mutate(X_Type="Total CDR (GtCO2)", y_label=oc$label,
               pathway_group="High-CDR",
               regression="D_Excl_CDR", threshold_label=thresh_label),
      run_loglog(df_D_re,  "total_re",  oc$y) %>%
        mutate(X_Type="Renewable Capacity (GW yr)", y_label=oc$label,
               pathway_group="High-RE",
               regression="D_Excl_RE", threshold_label=thresh_label),
      run_ols(df_D_cdr, "total_cdr", oc$y) %>%
        mutate(X_Type="Total CDR (GtCO2)", y_label=oc$label,
               pathway_group="High-CDR",
               regression="D_Excl_CDR_OLS", threshold_label=thresh_label),
      run_ols(df_D_re,  "total_re",  oc$y) %>%
        mutate(X_Type="Renewable Capacity (GW yr)", y_label=oc$label,
               pathway_group="High-RE",
               regression="D_Excl_RE_OLS", threshold_label=thresh_label)
    )
  })
  
  bind_rows(reg_C, reg_D)
}

cat("C+D: Tercile (global classification)\n")
reg_CD_tercile  <- run_CD_regressions(pathway_tercile,  "tercile")
cat("C+D: Quartile (global classification)\n")
reg_CD_quartile <- run_CD_regressions(pathway_quartile, "quartile")

# =============================================================================
# REGIONAL TERCILE REGRESSIONS (E + F)
# Classification within each Region x Ambition separately.
# E_Reg_Overlap = top tercile CDR or RE (overlapping allowed)
# F_Reg_Excl    = top tercile CDR only OR RE only (mutually exclusive)
# X-axis = region-level deployment (that region's own Total_Value)
# =============================================================================
cat("E+F: Regional tercile classification\n")

regional_pathway <- df_master %>%
  filter(Variable %in% c("Total CDR","Renewable Capacity"),
         Region %in% regions_r10) %>%
  select(Model, Scenario, Category, Region, Ambition, Variable, Total_Value) %>%
  pivot_wider(names_from = Variable, values_from = Total_Value) %>%
  rename(region_cdr = `Total CDR`, region_re = `Renewable Capacity`) %>%
  group_by(Region, Ambition) %>%
  mutate(
    cdr_tercile   = ntile(region_cdr, 3),
    re_tercile    = ntile(region_re,  3),
    high_cdr      = cdr_tercile == 3,
    high_re       = re_tercile  == 3,
    high_cdr_only = high_cdr & !high_re,
    high_re_only  = high_re  & !high_cdr,
    high_any      = high_cdr | high_re
  ) %>%
  ungroup()

run_regional_regressions <- function(excl_flag) {
  map_dfr(outcome_specs, function(oc) {
    if (!oc$y %in% names(df_master)) return(NULL)
    df_reg <- df_master %>%
      filter(Variable == "Total CDR", Region %in% regions_r10) %>%
      left_join(
        regional_pathway %>%
          select(Model, Scenario, Region, Ambition,
                 region_cdr, region_re,
                 high_cdr_only, high_re_only, high_any),
        by = c("Model","Scenario","Region","Ambition")
      )
    if (excl_flag) {
      df_cdr <- df_reg %>% filter(high_cdr_only)
      df_re  <- df_reg %>% filter(high_re_only)
      lab_cdr <- "F_Reg_Excl_CDR"; lab_re <- "F_Reg_Excl_RE"
    } else {
      df_cdr <- df_reg %>% filter(high_any)
      df_re  <- df_reg %>% filter(high_any)
      lab_cdr <- "E_Reg_Overlap_CDR"; lab_re <- "E_Reg_Overlap_RE"
    }
    cdr <- run_loglog(df_cdr, "region_cdr", oc$y) %>%
      mutate(X_Type="Total CDR (regional)", y_label=oc$label,
             regression=lab_cdr, threshold_label="world_tercile")
    re  <- run_loglog(df_re,  "region_re",  oc$y) %>%
      mutate(X_Type="Renewable Capacity (regional)", y_label=oc$label,
             regression=lab_re,  threshold_label="world_tercile")
    cdr_ols <- run_ols(df_cdr, "region_cdr", oc$y) %>%
      mutate(X_Type="Total CDR (regional)", y_label=oc$label,
             regression=paste0(lab_cdr, "_OLS"), threshold_label="world_tercile")
    re_ols  <- run_ols(df_re,  "region_re",  oc$y) %>%
      mutate(X_Type="Renewable Capacity (regional)", y_label=oc$label,
             regression=paste0(lab_re, "_OLS"),  threshold_label="world_tercile")
    bind_rows(cdr, re, cdr_ols, re_ols)
  })
}

reg_E <- run_regional_regressions(excl_flag = FALSE)
reg_F <- run_regional_regressions(excl_flag = TRUE)
cat("Regional regression rows — overlapping:", nrow(reg_E),
    "| exclusive:", nrow(reg_F), "\n")

reg_all <- bind_rows(reg_A, reg_B, reg_CD_tercile, reg_CD_quartile,
                     reg_E, reg_F)

cat("Total regression rows:", nrow(reg_all), "\n")
reg_all %>% count(regression, threshold_label) %>% print(n=20)


# =============================================================================
# SECTION 12: SAVE OUTPUTS
# =============================================================================

cat("\n=== SECTION 12: Saving ===\n")

write.csv(df_master,
          file.path(OUT_DIR, "compass_master_dataset.csv"), row.names=FALSE)
write.csv(headcount_data,
          file.path(OUT_DIR, "compass_dle_headcount_timeseries.csv"), row.names=FALSE)
write.csv(cdr_cumulative,
          file.path(OUT_DIR, "compass_cdr_cumulative.csv"), row.names=FALSE)
write.csv(reg_all,
          file.path(OUT_DIR, "compass_regression_results_all.csv"), row.names=FALSE)
# Separate OLS-only export for figures
reg_all_ols <- reg_all %>% filter(str_detect(regression, "_OLS$"))
reg_all_loglog <- reg_all %>% filter(!str_detect(regression, "_OLS$"))
write.csv(reg_all_ols,
          file.path(OUT_DIR, "compass_regression_results_OLS.csv"), row.names=FALSE)
write.csv(reg_all_loglog,
          file.path(OUT_DIR, "compass_regression_results_loglog.csv"), row.names=FALSE)
saveRDS(reg_all_ols,    file.path(COMPASS_DIR, "compass_reg_all_ols.rds"))
saveRDS(reg_all_loglog, file.path(COMPASS_DIR, "compass_reg_all_loglog.rds"))
write.csv(pathway_tercile,
          file.path(OUT_DIR, "compass_pathway_tercile.csv"), row.names=FALSE)
write.csv(pathway_quartile,
          file.path(OUT_DIR, "compass_pathway_quartile.csv"), row.names=FALSE)
write.csv(pathway_threshold,
          file.path(OUT_DIR, "compass_pathway_threshold.csv"), row.names=FALSE)

saveRDS(df_master,        file.path(COMPASS_DIR, "compass_master_dataset.rds"))
saveRDS(cdr_cumulative,   file.path(COMPASS_DIR, "compass_cdr_cumulative.rds"))
saveRDS(pathway_tercile,  file.path(COMPASS_DIR, "compass_pathway_tercile.rds"))
saveRDS(pathway_quartile, file.path(COMPASS_DIR, "compass_pathway_quartile.rds"))
saveRDS(pathway_threshold,file.path(COMPASS_DIR, "compass_pathway_threshold.rds"))

cat("\n=== COMPASS ANALYSIS COMPLETE ===\n")
cat("Scenarios:  ", n_distinct(paste(df_master$Model, df_master$Scenario)), "\n")
cat("Regions:    ", paste(sort(unique(df_master$Region)), collapse=", "), "\n")
cat("Outcome windows:\n")
cat("  1.5C (C1+C2): 2020-2060\n")
cat("  2C   (C3+C4): 2020-2075\n")
cat("Regressions:\n")
cat("  A: All scenarios, CDR deployment\n")
cat("  B: All scenarios, RE deployment\n")
cat("  C: High-CDR+RE overlapping (tercile + quartile)\n")
cat("  D: High-CDR+RE mutually exclusive (tercile + quartile)\n")
cat("Outputs: ", OUT_DIR, "\n")