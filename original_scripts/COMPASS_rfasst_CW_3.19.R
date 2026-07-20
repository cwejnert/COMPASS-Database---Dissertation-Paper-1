# =============================================================================
# COMPASS × rfasst AIR POLLUTION MORTALITY
# Computes PM2.5 and O3 premature mortality for all COMPASS scenarios
# using the TM5-FASST reduced-form air quality model via the rfasst package.
#
# APPROACH:
#   Bypasses rfasst Module 1 (GCAM-specific database reader) by directly
#   constructing the em.list object that Modules 2 and 3 expect, from
#   COMPASS emissions data already pulled via compass_pull.py.
#
# INPUTS:
#   compass_emissions_raw.csv  — 729,643 rows, 8 pollutants, 5 R10 + World
#                                columns: model, scenario, version, region,
#                                         variable, unit, year, value
#
# OUTPUTS:
#   compass_mortality_pm25_fasst.csv  — PM2.5 mortality by TM5-FASST region
#   compass_mortality_o3_fasst.csv    — O3 mortality by TM5-FASST region
#   compass_mortality_r10.csv         — Combined, aggregated to R10 regions
#   compass_mortality_summary.csv     — Cumulative 2020-2050, R10, all scenarios
#
# UNITS:
#   COMPASS emissions: Mt/yr  →  multiply by 1000 to get kt/yr (rfasst units)
#   OC → POM: multiply by CONV_OC_POM (rfasst internal = 1.3)
#   Mortality output: deaths/yr per TM5-FASST region
#
# RUNTIME ESTIMATE:
#   ~1543 scenarios × two pollutant pathways (PM2.5 + O3)
#   Expected: 30-90 minutes depending on machine speed.
#   Progress is printed every 50 scenarios.
# =============================================================================

library(tidyverse)
library(rfasst)

COMPASS_DIR <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/COMPASS"
OUT_DIR     <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_mortality"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# SECTION 1: CONFIGURATION
# =============================================================================

# rfasst internal constants
FASST_YEARS  <- rfasst:::all_years          # years rfasst processes
CONV_OC_POM  <- rfasst:::CONV_OC_POM       # OC → POM factor (~1.3)
cat("rfasst years:", paste(FASST_YEARS, collapse=", "), "\n")
cat("OC→POM factor:", CONV_OC_POM, "\n")

# Categories to keep
categories_keep <- c("C1", "C2", "C3", "C4")

# COMPASS variable → rfasst pollutant name mapping
# rfasst internal pollutant names (from m1 source): SO2, NOx, BC, POM, NH3, VOC, CH4, CO
pollutant_map <- c(
  "Emissions|Sulfur" = "SO2",
  "Emissions|NOx"    = "NOx",
  "Emissions|BC"     = "BC",
  "Emissions|OC"     = "OC",    # converted to POM below
  "Emissions|CO"     = "CO",
  "Emissions|NH3"    = "NH3",
  "Emissions|VOC"    = "VOC",
  "Emissions|CH4"    = "CH4"
)

# COMPASS region names (as delivered by Python pull, already renamed to R10)
COMPASS_R10_REGIONS <- c("R10AFRICA", "R10CHINA+", "R10INDIA+",
                         "R10NORTH_AM", "R10EUROPE")

# TM5-FASST region → R10 region mapping
# Each of TM5-FASST's 56 regions is assigned to one R10 region.
# Non-target R10 regions (LATIN_AM, MIDDLE_EAST, REF_ECON, REST_ASIA, PAC_OECD)
# are included for completeness but mortality will be aggregated only for the
# 5 target R10 regions in the final output.
fasst_to_r10 <- tribble(
  ~fasst_region, ~r10_region,
  # R10AFRICA
  "EAF",    "R10AFRICA",    # East Africa
  "WAF",    "R10AFRICA",    # West Africa
  "SAF",    "R10AFRICA",    # Southern Africa (excl. RSA)
  "RSA",    "R10AFRICA",    # Republic of South Africa
  "NOA",    "R10AFRICA",    # North Africa
  # R10CHINA+
  "CHN",    "R10CHINA+",    # China
  "COR",    "R10CHINA+",    # Korea
  "TWN",    "R10CHINA+",    # Taiwan
  "MON",    "R10CHINA+",    # Mongolia
  # R10INDIA+
  "NDE",    "R10INDIA+",    # India
  "RSAS",   "R10INDIA+",    # Rest of South Asia (Pakistan, Bangladesh, etc.)
  # R10EUROPE
  "AUT",    "R10EUROPE",
  "BLX",    "R10EUROPE",    # Belgium, Luxembourg, Netherlands
  "BGR",    "R10EUROPE",
  "CHE",    "R10EUROPE",
  "ESP",    "R10EUROPE",
  "FIN",    "R10EUROPE",
  "FRA",    "R10EUROPE",
  "GBR",    "R10EUROPE",
  "GRC",    "R10EUROPE",
  "HUN",    "R10EUROPE",
  "ITA",    "R10EUROPE",
  "NOR",    "R10EUROPE",
  "POL",    "R10EUROPE",
  "RCEU",   "R10EUROPE",    # Rest of Central Europe
  "RCZ",    "R10EUROPE",    # Czech Republic / Slovakia
  "RFA",    "R10EUROPE",    # Germany
  "ROM",    "R10EUROPE",    # Romania / Moldova
  "SWE",    "R10EUROPE",
  "TUR",    "R10EUROPE",
  "UKR",    "R10EUROPE",
  # R10NORTH_AM
  "USA",    "R10NORTH_AM",
  "CAN",    "R10NORTH_AM",
  "MEX",    "R10NORTH_AM",
  # Non-target R10 regions (included for full TM5-FASST coverage)
  "IDN",    "R10REST_ASIA",
  "JPN",    "R10REST_ASIA",
  "MYS",    "R10REST_ASIA",
  "PAC",    "R10REST_ASIA",
  "PHL",    "R10REST_ASIA",
  "RSEA",   "R10REST_ASIA",
  "THA",    "R10REST_ASIA",
  "VNM",    "R10REST_ASIA",
  "NZL",    "R10PAC_OECD",
  "AUS",    "R10PAC_OECD",
  "ARG",    "R10LATIN_AM",
  "BRA",    "R10LATIN_AM",
  "CHL",    "R10LATIN_AM",
  "RCAM",   "R10LATIN_AM",
  "RSAM",   "R10LATIN_AM",
  "GOLF",   "R10MIDDLE_EAST",
  "MEME",   "R10MIDDLE_EAST",
  "EGY",    "R10MIDDLE_EAST",
  "KAZ",    "R10REF_ECON",
  "RIS",    "R10REF_ECON",
  "RUS",    "R10REF_ECON"
)

# Population weights (millions, ~2015) for distributing R10 emissions
# across constituent TM5-FASST regions.
# Source: UN World Population Prospects 2019, aggregated to TM5-FASST regions.
fasst_pop <- tribble(
  ~fasst_region, ~pop,
  "EAF",   348.0, "WAF",   340.0, "SAF",    60.0, "RSA",    55.0, "NOA",   215.0,
  "CHN",  1376.0, "COR",    51.0, "TWN",    23.5, "MON",     3.0,
  "NDE",  1310.0, "RSAS",  420.0,
  "AUT",     8.6, "BLX",    28.1, "BGR",     7.2, "CHE",     8.3,
  "ESP",    46.4, "FIN",     5.5, "FRA",    66.4, "GBR",    65.1,
  "GRC",    10.8, "HUN",     9.8, "ITA",    59.8, "NOR",     5.2,
  "POL",    38.0, "RCEU",   30.0, "RCZ",    16.0, "RFA",    81.7,
  "ROM",    20.0, "SWE",    10.0, "TUR",    78.0, "UKR",    44.0,
  "USA",   321.0, "CAN",    35.9, "MEX",   127.0,
  "IDN",   259.0, "JPN",   127.0, "MYS",    30.3, "PAC",    12.0,
  "PHL",   101.0, "RSEA",  120.0, "THA",    68.0, "VNM",    91.0,
  "NZL",     4.5, "AUS",    23.8,
  "ARG",    43.4, "BRA",   207.8, "CHL",    17.9, "RCAM",   47.0, "RSAM",  30.0,
  "GOLF",   51.0, "MEME",  120.0, "EGY",    91.5,
  "KAZ",    17.5, "RIS",    90.0, "RUS",   144.0
)

# Compute population-based distribution weights: for each R10 region,
# what share of emissions goes to each constituent TM5-FASST region?
fasst_weights <- fasst_to_r10 %>%
  left_join(fasst_pop, by = "fasst_region") %>%
  group_by(r10_region) %>%
  mutate(weight = pop / sum(pop, na.rm = TRUE)) %>%
  ungroup() %>%
  select(fasst_region, r10_region, weight)

cat("\nRegional weight check (should sum to 1.0 per R10 region):\n")
fasst_weights %>%
  group_by(r10_region) %>%
  summarise(total_weight = round(sum(weight), 4), .groups="drop") %>%
  print()


# =============================================================================
# SECTION 2: LOAD AND PREPARE COMPASS EMISSIONS
# =============================================================================

cat("\nLoading COMPASS emissions...\n")

em_raw <- read.csv(
  file.path(COMPASS_DIR, "compass_emissions_raw.csv"),
  stringsAsFactors = FALSE
) %>%
  mutate(year = as.integer(year), value = as.numeric(value)) %>%
  filter(!is.na(value))

cat("Loaded", nrow(em_raw), "rows,",
    n_distinct(paste(em_raw$model, em_raw$scenario)), "scenarios\n")

# Map variable names and convert units
# Corrected pollutant name mapping — must match rfasst base_em exactly
pollutant_map <- c(
  "Emissions|Sulfur" = "SO2",
  "Emissions|NOx"    = "NOX",   # was "NOx" — rfasst uses uppercase X
  "Emissions|BC"     = "BC",
  "Emissions|OC"     = "OC",    # converted to OM below (not POM)
  "Emissions|CO"     = "CO",
  "Emissions|NH3"    = "NH3",
  "Emissions|VOC"    = "VOC",
  "Emissions|CH4"    = "CH4"
)

# Then in the OC→OM conversion (replaces OC→POM):
# In em_clean construction — change Mt → kg instead of Mt → kt
# COMPASS reports Mt/yr
# rfasst base_em is in kg
# Mt × 1e9 = kg

em_clean <- em_raw %>%
  filter(
    variable %in% names(pollutant_map),
    region   %in% c(COMPASS_R10_REGIONS, "World"),
    year     %in% FASST_YEARS
  ) %>%
  mutate(
    pollutant = recode(variable, !!!pollutant_map),
    # Convert Mt/yr → kg (rfasst base_em units): Mt × 1e9 = kg
    value_kt  = value * 1e9,
    value_kt  = if_else(pollutant == "OC", value_kt * CONV_OC_POM, value_kt),
    pollutant = if_else(pollutant == "OC", "OM", pollutant)
  ) %>%
  filter(region != "World") %>%
  mutate(year = as.integer(year)) %>%
  select(model, scenario, region, pollutant, year, value_kt)

cat("em_clean rows (R10 scenarios):", nrow(em_clean), "\n")
cat("Scenarios in em_clean:", n_distinct(paste(em_clean$model, em_clean$scenario)), "\n")

# =============================================================================
# WORLD-ONLY DISAGGREGATION
# Some scenarios only report emissions at the World level (no R10 breakdown).
# These are detected automatically and distributed to R10 regions using
# population weights, then appended to em_clean so they run through rfasst.
# =============================================================================

# Identify scenarios that have World emissions but no R10 breakdown
scen_region_coverage <- em_raw %>%
  filter(variable %in% names(pollutant_map),
         year %in% FASST_YEARS) %>%
  group_by(model, scenario) %>%
  summarise(
    has_r10   = any(region %in% COMPASS_R10_REGIONS),
    has_world = any(region == "World"),
    .groups   = "drop"
  )

world_only_scens <- scen_region_coverage %>%
  filter(!has_r10, has_world) %>%
  select(model, scenario)

cat("\nWorld-only scenarios detected (will be disaggregated to R10):",
    nrow(world_only_scens), "\n")
if (nrow(world_only_scens) > 0) {
  for (s in sort(world_only_scens$scenario)) cat(" -", s, "\n")
}

# R10 population shares — renormalised to the 5 target R10 regions only
r10_pop_shares <- fasst_weights %>%
  group_by(r10_region) %>%
  summarise(r10_share = sum(weight), .groups = "drop") %>%
  filter(r10_region %in% COMPASS_R10_REGIONS) %>%
  mutate(r10_share = r10_share / sum(r10_share))

# Disaggregate World emissions proportionally to R10 regions
if (nrow(world_only_scens) > 0) {
  em_world_disagg <- em_raw %>%
    inner_join(world_only_scens, by = c("model", "scenario")) %>%
    filter(
      variable %in% names(pollutant_map),
      region   == "World",
      year     %in% FASST_YEARS
    ) %>%
    mutate(
      pollutant = recode(variable, !!!pollutant_map),
      value_kt  = value * 1e9,
      value_kt  = if_else(pollutant == "OC", value_kt * CONV_OC_POM, value_kt),
      pollutant = if_else(pollutant == "OC", "OM", pollutant),
      year      = as.integer(year)
    ) %>%
    select(model, scenario, pollutant, year, value_kt) %>%
    crossing(r10_pop_shares) %>%
    mutate(
      value_kt = value_kt * r10_share,
      region   = r10_region
    ) %>%
    select(model, scenario, region, pollutant, year, value_kt)
  
  em_clean <- bind_rows(em_clean, em_world_disagg)
  cat("em_clean rows after World disaggregation:", nrow(em_clean), "\n")
}

cat("\nFinal em_clean: ", n_distinct(paste(em_clean$model, em_clean$scenario)),
    "scenarios |", nrow(em_clean), "rows\n")
cat("Years present:", paste(sort(unique(em_clean$year)), collapse=", "), "\n")
cat("Pollutants:", paste(sort(unique(em_clean$pollutant)), collapse=", "), "\n")

# Load COMPASS metadata for category lookup
compass_meta <- read.csv(
  file.path(COMPASS_DIR, "compass_r10_meta.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Extract AR6 category from metadata
category_col <- names(compass_meta)[
  str_detect(names(compass_meta), "Climate Category\\|AR6 \\[Name\\]")
][1]

cat("Category column:", category_col, "\n")

compass_categories <- compass_meta %>%
  rename(Model = model, Scenario = scenario) %>%
  select(Model, Scenario, cat_raw = !!sym(category_col)) %>%
  mutate(
    Category = case_when(
      str_detect(cat_raw, "^C1") ~ "C1",
      str_detect(cat_raw, "^C2") ~ "C2",
      str_detect(cat_raw, "^C3") ~ "C3",
      str_detect(cat_raw, "^C4") ~ "C4",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Category)) %>%
  distinct(Model, Scenario, Category)

cat("Scenarios with valid categories:",
    n_distinct(paste(compass_categories$Model, compass_categories$Scenario)), "\n")

# Vetted scenario list — must match the list in COMPASS_full_analysis_revised_CW_3_23.R
vetted_scenarios <- c(
  "SSP2-45",
  "SSP2-60",
  "SSP2-Baseline",
  "SSP3-34",
  "SSP3-45",
  "SSP3-60",
  "SSP3-Baseline",
  "SSP4-45",
  "SSP4-Baseline",
  "SSP5-45",
  "SSP5-60",
  "SSP5-Baseline",
  "COMMIT-Current-Policies",
  "ENGAGE-INDCi2030-1000f",
  "ENGAGE-INDCi2030-1200",
  "ENGAGE-INDCi2030-1200f",
  "ENGAGE-INDCi2030-1400",
  "ENGAGE-INDCi2030-1400f",
  "ENGAGE-INDCi2030-1600",
  "ENGAGE-INDCi2030-1600f",
  "ENGAGE-INDCi2030-1800",
  "ENGAGE-INDCi2030-1800f",
  "ENGAGE-INDCi2030-800f",
  "ENGAGE-INDCi2030-900f",
  "ENGAGE-INDCi2100",
  "ENGAGE-NPi2100",
  "SDI-2.5\u00b0C",
  "SDI-2\u00b0C",
  "SDI-Baseline",
  "COMMIT-Current-Policies",
  "ENGAGE-NPi2100",
  "NAVIGATE Demand-NPi-act",
  "NAVIGATE Demand-NPi-all",
  "NAVIGATE Demand-NPi-ele",
  "NAVIGATE Demand-NPi-ref",
  "NAVIGATE Demand-NPi-tec",
  "NAVIGATE Industry-NPi",
  "SSP1-19",
  "SSP1-26",
  "SSP1-34",
  "SSP1-45",
  "SSP1-Baseline",
  "SSP2-19",
  "SSP2-26",
  "SSP2-34",
  "SSP2-45",
  "SSP2-60",
  "SSP2-Baseline",
  "SSP3-Baseline",
  "SSP4-26",
  "SSP4-34",
  "SSP4-45",
  "SSP4-Baseline",
  "SSP5-19",
  "SSP5-26",
  "SSP5-34",
  "SSP5-45",
  "SSP5-60",
  "NGFS Phase 1-Current Policies",
  "Deep-Mitigation-Baseline",
  "Deep-Mitigation-MAC_55_n0",
  "Deep-Mitigation-MAC_60_n0",
  "Deep-Mitigation-MAC_60_n8",
  "Deep-Mitigation-MAC_65_n0",
  "Deep-Mitigation-MAC_65_n8",
  "Deep-Mitigation-MAC_70_n0",
  "Deep-Mitigation-MAC_70_n8",
  "Deep-Mitigation-MAC_75_n0",
  "Deep-Mitigation-MAC_75_n8",
  "Deep-Mitigation-MAC_80_n0",
  "Deep-Mitigation-MAC_80_n8",
  "Deep-Mitigation-MAC_85_n0",
  "Deep-Mitigation-MAC_85_n8",
  "Deep-Mitigation-MAC_90_n0",
  "Deep-Mitigation-MAC_90_n8",
  "Deep-Mitigation-MAC_95_n0",
  "Deep-Mitigation-MAC_95_n8",
  "Deep-Mitigation-MAC_99_n0",
  "Deep-Mitigation-MAC_99_n8",
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
  "IAM-COMPACT-Current-Policies-Emissions-Intensity",
  "IAM-COMPACT-NDC-Emissions-Intensity",
  "IAM-COMPACT-NDC-Long-Term-Pledges",
  "ParisReinforce-Baseline",
  "ParisReinforce-Current-Policies-PriceOnly",
  "ParisReinforce-NDC-PriceOnly",
  "ENGAGE-INDCi2030-1000",
  "ENGAGE-INDCi2030-1000-COV",
  "ENGAGE-INDCi2030-1000-COV-NDCp",
  "ENGAGE-INDCi2030-1000-NDCp",
  "ENGAGE-INDCi2030-1000f",
  "ENGAGE-INDCi2030-1000f-COV",
  "ENGAGE-INDCi2030-1000f-COV-NDCp",
  "ENGAGE-INDCi2030-1000f-NDCp",
  "ENGAGE-INDCi2030-1400",
  "ENGAGE-INDCi2030-1400f",
  "ENGAGE-INDCi2030-1800",
  "ENGAGE-INDCi2030-1800f",
  "ENGAGE-INDCi2030-600f",
  "ENGAGE-INDCi2030-600f-COV",
  "ENGAGE-INDCi2030-600f-COV-NDCp",
  "ENGAGE-INDCi2030-600f-NDCp",
  "ENGAGE-INDCi2030-800",
  "ENGAGE-INDCi2030-800f",
  "ENGAGE-INDCi2100",
  "ENGAGE-INDCi2100-COV",
  "ENGAGE-INDCi2100-COV-NDCp",
  "ENGAGE-INDCi2100-NDCp",
  "ENGAGE-NPi2020-1800",
  "ENGAGE-NPi2020-1800f",
  "ENGAGE-NPi2100",
  "ENGAGE-NPi2100-COV",
  "COMMIT-2\u00b0C-2030",
  "COMMIT-Baseline",
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
  "ENGAGE-NoPolicy",
  "ADVANCE-2030-medium-2\u00b0C",
  "ADVANCE-2030-well-below-2\u00b0C",
  "ADVANCE-INDC",
  "ADVANCE-NoPolicy",
  "ADVANCE-Reference",
  "CD-LINKS-INDC2030i_1600",
  "CD-LINKS-NDC2030i_1000",
  "CD-LINKS-NPi",
  "CD-LINKS-No-Policy",
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
  "EMF30-Slower-Action+SLCF",
  "EMF30-Slower-to-Faster",
  "EMF30-Slower-to-Faster+SLCF",
  "EMF30-Slower-to-Faster+SLCF+HFC",
  "SSP1-34",
  "SSP1-45",
  "SSP1-Baseline",
  "SSP2-34",
  "SSP2-45",
  "SSP2-60",
  "SSP2-Baseline",
  "SSP3-34",
  "SSP3-45",
  "SSP3-60",
  "SSP3-Baseline",
  "SSP4-45",
  "SSP4-60",
  "SSP4-Baseline",
  "SSP5-34",
  "SSP5-45",
  "SSP5-60",
  "EMF33-Baseline",
  "EMF33-tax-lo-full",
  "EMF33-tax-lo-none",
  "SSP2021-SSP1-Baseline",
  "SSP2021-SSP1-SPA1-19-Default",
  "SSP2021-SSP1-SPA1-19-Default-LowBiomass",
  "SSP2021-SSP1-SPA1-19-Lifestyle",
  "SSP2021-SSP1-SPA1-19-Lifestyle-Renewables",
  "SSP2021-SSP1-SPA1-19-Lifestyle-Renewables-LowBiomass",
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
  "SSP2021-SSP2-SPA1-19-Lifestyle-Renewables-LowBiomass",
  "SSP2021-SSP2-SPA1-19-Renewables-LowBiomass",
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
  "SHAPE-SSP1-NPi",
  "SHAPE-SSP2-NPi",
  "SHAPE-SSP2-NPi [with Climate Change Impacts]",
  "EMF30-BCOC-EndU",
  "EMF30-Baseline",
  "EMF30-D-BCOC-Red",
  "EMF30-D-Frozen-CH4",
  "EMF30-D-Frozen-EF",
  "EMF30-D-Frozen-EF-EndU",
  "EMF30-D-Frozen-EF-SLCF",
  "SSP2-Baseline",
  "ENGAGE-NPi2100",
  "ENGAGE-NoPolicy",
  "NAVIGATE Demand-2.0\u00b0C-act_u",
  "NAVIGATE Demand-2.0\u00b0C-all_u",
  "NAVIGATE Demand-2.0\u00b0C-ele_u",
  "NAVIGATE Demand-2.0\u00b0C-ref",
  "NAVIGATE Demand-2.0\u00b0C-tec_u",
  "NAVIGATE Demand-NPi-act",
  "NAVIGATE Demand-NPi-all",
  "NAVIGATE Demand-NPi-ele",
  "NAVIGATE Demand-NPi-ref",
  "NAVIGATE Demand-NPi-tec",
  "COVID-Shift-GreenPush_max_GDP",
  "COVID-Shift-NoPolicyNoCOVID",
  "COVID-Shift-Restore",
  "COVID-Shift-SelfReliance",
  "COVID-Shift-SelfReliance_max_GDP",
  "COVID-Shift-SmartUse",
  "NGFS Phase 5-Below 2\u00b0C",
  "NGFS Phase 5-Current Policies",
  "NGFS Phase 5-Delayed Transition",
  "NGFS Phase 5-Fragmented World",
  "NGFS Phase 5-Low Demand",
  "NGFS Phase 5-Nationally Determined Contributions (NDCs)",
  "NGFS Phase 5-Net-Zero 2050",
  "ADVANCE-2020-medium-2\u00b0C",
  "ADVANCE-2020-well-below-2\u00b0C",
  "ADVANCE-2030-1.5\u00b0C-2100",
  "ADVANCE-2030-Price 1.5\u00b0C",
  "ADVANCE-2030-medium-2\u00b0C",
  "ADVANCE-2030-well-below-2\u00b0C",
  "ADVANCE-INDC",
  "ADVANCE-NoPolicy",
  "ADVANCE-Reference",
  "CD-LINKS-No-Policy",
  "EMF30-BCOC-EndU",
  "EMF30-Baseline",
  "EMF30-D-BCOC-Red",
  "EMF30-D-Frozen-CH4",
  "EMF30-D-Frozen-EF",
  "EMF30-D-Frozen-EF-EndU",
  "EMF30-D-Frozen-EF-SLCF",
  "EMF30-Slower-to-Faster",
  "EMF33-Baseline",
  "EMF33-tax-lo-full",
  "EMF33-tax-lo-none",
  "COMMIT-2\u00b0C-2020",
  "COMMIT-2\u00b0C-2030",
  "COMMIT-Baseline",
  "COMMIT-Bridge",
  "COMMIT-Bridge-No-Tax",
  "COMMIT-Current-Policies",
  "COMMIT-GPP",
  "COMMIT-GPP-No-Tax",
  "COMMIT-NDC-2050-Convergence",
  "COMMIT-NDCplus",
  "ENGAGE-Feasibility-1000/Technology",
  "ENGAGE-INDCi2030-1000",
  "ENGAGE-INDCi2030-1000-COV",
  "ENGAGE-INDCi2030-1000-COV-NDCp",
  "ENGAGE-INDCi2030-1000-NDCp",
  "ENGAGE-INDCi2030-1000f",
  "ENGAGE-INDCi2030-1000f-COV",
  "ENGAGE-INDCi2030-1000f-COV-NDCp",
  "ENGAGE-INDCi2030-1000f-NDCp",
  "ENGAGE-INDCi2030-1200",
  "ENGAGE-INDCi2030-1200f",
  "ENGAGE-INDCi2030-1400",
  "ENGAGE-INDCi2030-1400f",
  "ENGAGE-INDCi2030-1600",
  "ENGAGE-INDCi2030-1600f",
  "ENGAGE-INDCi2030-1800",
  "ENGAGE-INDCi2030-1800f",
  "ENGAGE-INDCi2030-2000",
  "ENGAGE-INDCi2030-2000f",
  "ENGAGE-INDCi2030-2500",
  "ENGAGE-INDCi2030-2500f",
  "ENGAGE-INDCi2030-3000",
  "ENGAGE-INDCi2030-3000f",
  "ENGAGE-INDCi2030-300f",
  "ENGAGE-INDCi2030-400f",
  "ENGAGE-INDCi2030-500f",
  "ENGAGE-INDCi2030-600f",
  "ENGAGE-INDCi2030-600f-COV",
  "ENGAGE-INDCi2030-600f-COV-NDCp",
  "ENGAGE-INDCi2030-600f-NDCp",
  "ENGAGE-INDCi2030-700f",
  "ENGAGE-INDCi2030-800f",
  "ENGAGE-INDCi2030-900",
  "ENGAGE-INDCi2030-900f",
  "ENGAGE-INDCi2100",
  "ENGAGE-INDCi2100-COV",
  "ENGAGE-INDCi2100-COV-NDCp",
  "ENGAGE-INDCi2100-NDCp",
  "ENGAGE-NPi2020-1000f",
  "ENGAGE-NPi2020-1000f-COV",
  "ENGAGE-NPi2020-1200f",
  "ENGAGE-NPi2020-1400",
  "ENGAGE-NPi2020-1400f",
  "ENGAGE-NPi2020-1600",
  "ENGAGE-NPi2020-1600f",
  "ENGAGE-NPi2020-1800",
  "ENGAGE-NPi2020-1800f",
  "ENGAGE-NPi2020-2000",
  "ENGAGE-NPi2020-2000f",
  "ENGAGE-NPi2020-2500",
  "ENGAGE-NPi2020-2500f",
  "ENGAGE-NPi2020-3000",
  "ENGAGE-NPi2020-3000f",
  "ENGAGE-NPi2020-900f",
  "ENGAGE-NPi2100",
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
  "ECEMF-DIAG-NPi",
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
  "SSP1-Baseline",
  "SSP2-34",
  "SSP2-45",
  "SSP2-60",
  "SSP2-Baseline",
  "CD-LINKS-No-Policy",
  "COMMIT-Baseline",
  "COMMIT-Current-Policies",
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
  "ENGAGE-NoPolicy",
  "NGFS Phase 2-Current Policies",
  "NGFS Phase 2-Current Policies [IPD 95th]",
  "NGFS Phase 2-Current Policies [IPD Median]",
  "NGFS Phase 2-Delayed Transition",
  "NGFS Phase 2-Nationally Determined Contributions (NDCs)",
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
  "NGFS Phase 5-Below 2\u00b0C",
  "NGFS Phase 5-Current Policies",
  "NGFS Phase 5-Delayed Transition",
  "NGFS Phase 5-Fragmented World",
  "NGFS Phase 5-Low Demand",
  "NGFS Phase 5-Nationally Determined Contributions (NDCs)",
  "NGFS Phase 5-Net-Zero 2050",
  "DISCRATE-Reference-dr2p",
  "DISCRATE-Reference-dr3p",
  "DISCRATE-Reference-dr4p",
  "DISCRATE-Reference-dr5p",
  "COMMIT-Baseline",
  "ENGAGE-INDCi2030-1000",
  "ENGAGE-INDCi2030-1000-NDCp",
  "ENGAGE-INDCi2030-1000f",
  "ENGAGE-INDCi2030-1000f-NDCp",
  "ENGAGE-INDCi2030-1200",
  "ENGAGE-INDCi2030-1200-NDCp",
  "ENGAGE-INDCi2030-1200f",
  "ENGAGE-INDCi2030-1200f-NDCp",
  "ENGAGE-INDCi2030-1400",
  "ENGAGE-INDCi2030-1400-NDCp",
  "ENGAGE-INDCi2030-1400f",
  "ENGAGE-INDCi2030-1400f-NDCp",
  "ENGAGE-INDCi2030-1600",
  "ENGAGE-INDCi2030-1600-NDCp",
  "ENGAGE-INDCi2030-1600f",
  "ENGAGE-INDCi2030-1600f-NDCp",
  "ENGAGE-INDCi2030-1800",
  "ENGAGE-INDCi2030-1800-NDCp",
  "ENGAGE-INDCi2030-1800f",
  "ENGAGE-INDCi2030-1800f-NDCp",
  "ENGAGE-INDCi2030-2000",
  "ENGAGE-INDCi2030-2000-NDCp",
  "ENGAGE-INDCi2030-2000f",
  "ENGAGE-INDCi2030-2000f-NDCp",
  "ENGAGE-INDCi2030-2500",
  "ENGAGE-INDCi2030-2500-NDCp",
  "ENGAGE-INDCi2030-2500f",
  "ENGAGE-INDCi2030-2500f-NDCp",
  "ENGAGE-INDCi2030-3000",
  "ENGAGE-INDCi2030-3000-NDCp",
  "ENGAGE-INDCi2030-3000f",
  "ENGAGE-INDCi2030-3000f-NDCp",
  "ENGAGE-INDCi2030-500f",
  "ENGAGE-INDCi2030-600f",
  "ENGAGE-INDCi2030-600f-NDCp",
  "ENGAGE-INDCi2030-700f",
  "ENGAGE-INDCi2030-700f-NDCp",
  "ENGAGE-INDCi2030-800",
  "ENGAGE-INDCi2030-800-NDCp",
  "ENGAGE-INDCi2030-800f",
  "ENGAGE-INDCi2030-800f-NDCp",
  "ENGAGE-INDCi2030-900",
  "ENGAGE-INDCi2030-900-NDCp",
  "ENGAGE-INDCi2030-900f",
  "ENGAGE-INDCi2030-900f-NDCp",
  "ENGAGE-INDCi2100",
  "ENGAGE-INDCi2100-NDCp",
  "ENGAGE-NPi2020-3000",
  "ENGAGE-NPi2020-3000f",
  "ENGAGE-NPi2100",
  "ENGAGE-NoPolicy",
  "NAVIGATE Demand-2.0\u00b0C-ref",
  "NAVIGATE Demand-NPi-act",
  "NAVIGATE Demand-NPi-all",
  "NAVIGATE Demand-NPi-ele",
  "NAVIGATE Demand-NPi-ref",
  "NAVIGATE Demand-NPi-tec",
  "SSP1-Baseline",
  "SSP4-45",
  "SSP4-Baseline",
  "CD-LINKS-NPi",
  "CD-LINKS-No-Policy"
)

# Get unique scenario list — filtered to C1/C2/C3/C4 and vetted scenarios only
scenarios_to_run <- em_clean %>%
  distinct(model, scenario) %>%
  left_join(
    compass_categories,
    by = c("model" = "Model", "scenario" = "Scenario")
  ) %>%
  select(model, scenario, Category) %>%
  filter(!is.na(Category), Category %in% c("C1","C2","C3","C4")) %>%
  filter(scenario %in% vetted_scenarios) %>%
  arrange(model, scenario)

cat("Scenarios to run:", nrow(scenarios_to_run), "\n")
cat("By Category:\n")
scenarios_to_run %>% count(Category) %>% print()

# Check COFFEE 1.1 coverage
scenarios_to_run %>%
  filter(model == "COFFEE 1.1") %>%
  nrow() %>%
  cat("COFFEE 1.1 scenarios:", ., "\n")

cat("Scenarios to process:", nrow(scenarios_to_run), "\n")


# =============================================================================
# SECTION 3: BUILD em.list FORMAT
# =============================================================================
# rfasst's m2/m3 functions call m1_emissions_rescale internally and expect
# em.list: a named list of dataframes (one per year), each with columns:
#   region    : TM5-FASST region code (e.g. "CHN", "NDE", "RFA")
#   year      : integer or factor year
#   pollutant : pollutant name (SO2, NOx, BC, POM, NH3, VOC, CH4, CO)
#   value     : emissions in kt/yr
#   units     : "kt" (constant)
#
# We build this by distributing R10 emissions to TM5-FASST regions using
# population weights, then formatting into the list structure.

build_em_list <- function(em_scenario) {
  
  all_pollutants    <- c("BC", "CH4", "CO", "CO2", "N2O", "NH3",
                         "NOX", "OM", "PM25", "SO2", "VOC")
  all_fasst_regions <- c(unique(rfasst::fasst_reg$fasst_region),
                         "AIR", "SHIP", "RUE")
  all_years_chr     <- as.character(sort(FASST_YEARS))
  
  # Distribute R10 emissions to TM5-FASST regions
  em_fasst <- em_scenario %>%
    inner_join(fasst_weights,
               by = c("region" = "r10_region"),
               relationship = "many-to-many") %>%
    mutate(
      value_fasst = value_kt * weight,
      units       = "kg"
    ) %>%
    select(region = fasst_region, year, pollutant,
           value = value_fasst, units) %>%
    group_by(region, year, pollutant, units) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = as.character(year))
  
  # Complete grid — fill missing region/pollutant/year combos with 0
  complete_grid <- crossing(
    region    = all_fasst_regions,
    pollutant = all_pollutants,
    year      = all_years_chr
  ) %>% mutate(units = "kg")
  
  em_fasst_complete <- complete_grid %>%
    left_join(em_fasst, by = c("region", "pollutant", "year", "units")) %>%
    mutate(value = replace_na(value, 0))
  
  # Split into named list by year
  year_vals <- sort(unique(em_fasst_complete$year))
  em_list <- lapply(year_vals, function(y) {
    em_fasst_complete %>% filter(year == y)
  })
  names(em_list) <- year_vals
  
  em_list
}


# =============================================================================
# SECTION 4: MONKEY-PATCH m2 TO ACCEPT EXTERNAL em.list
# =============================================================================
# rfasst's m2/m3 functions call m1_emissions_rescale internally — we can't
# pass em.list directly. The cleanest approach is to temporarily override
# m1_emissions_rescale in the rfasst namespace to return our pre-built em.list,
# then restore it after each scenario.

# Updated run_rfasst_for_scenario with correct cache clearing
# Updated run_rfasst_for_scenario — m1 patch only, clear all caches
run_rfasst_for_scenario <- function(em_list_input, scen_label) {
  
  # Clear ALL rfasst caches
  for (v in c("m2_get_conc_pm25.output", "m2_get_conc_m6m.output",
              "m2_get_conc_o3.output",   "m3_get_mort_pm25.output",
              "m3_get_mort_o3.output")) {
    if (exists(v, envir=.GlobalEnv)) rm(list=v, envir=.GlobalEnv)
  }
  
  original_m1 <- rfasst::m1_emissions_rescale
  assignInNamespace("m1_emissions_rescale",
                    function(...) em_list_input, ns="rfasst")
  
  mort_pm25 <- tryCatch(
    rfasst::m3_get_mort_pm25(
      prj=list(dummy=TRUE), scen_name=scen_label,
      saveOutput=FALSE, map=FALSE, recompute=TRUE
    ),
    error = function(e) { message("  PM2.5 error: ", e$message); NULL }
  )
  
  # Clear m2 cache between PM2.5 and O3
  for (v in c("m2_get_conc_pm25.output", "m2_get_conc_m6m.output",
              "m3_get_mort_pm25.output")) {
    if (exists(v, envir=.GlobalEnv)) rm(list=v, envir=.GlobalEnv)
  }
  
  mort_o3 <- tryCatch(
    rfasst::m3_get_mort_o3(
      prj=list(dummy=TRUE), scen_name=scen_label,
      saveOutput=FALSE, map=FALSE, recompute=TRUE
    ),
    error = function(e) { message("  O3 error: ", e$message); NULL }
  )
  
  assignInNamespace("m1_emissions_rescale", original_m1, ns="rfasst")
  
  list(pm25=mort_pm25, o3=mort_o3)
}

# Reinitialise
results_pm25 <- list()
results_o3   <- list()
errors        <- tibble(model=character(), scenario=character(), error=character())

# Clear all caches before starting
for (v in c("m2_get_conc_pm25.output", "m2_get_conc_m6m.output",
            "m3_get_mort_pm25.output", "m3_get_mort_o3.output")) {
  if (exists(v, envir=.GlobalEnv)) rm(list=v, envir=.GlobalEnv)
}


# =============================================================================
# SECTION 5: RUN rfasst FOR ALL SCENARIOS
# =============================================================================

cat("\n=== Running rfasst for", nrow(scenarios_to_run), "scenarios ===\n")
cat("This will take approximately",
    round(nrow(scenarios_to_run) * 4 / 60, 0), "minutes\n\n")

# Create output directory structure rfasst expects
if (!dir.exists("output"))        dir.create("output")
if (!dir.exists("output/m2"))     dir.create("output/m2")
if (!dir.exists("output/m3"))     dir.create("output/m3")
if (!dir.exists("output/maps"))   dir.create("output/maps")
if (!dir.exists("output/maps/m2")) dir.create("output/maps/m2")
if (!dir.exists("output/maps/m3")) dir.create("output/maps/m3")
if (!dir.exists("output/maps/m2/maps_pm2.5")) dir.create("output/maps/m2/maps_pm2.5")
if (!dir.exists("output/maps/m2/maps_m6m"))   dir.create("output/maps/m2/maps_m6m")
if (!dir.exists("output/maps/m3/maps_mort_pm25")) dir.create("output/maps/m3/maps_mort_pm25")
if (!dir.exists("output/maps/m3/maps_mort_o3"))   dir.create("output/maps/m3/maps_mort_o3")

results_pm25 <- list()
results_o3   <- list()
errors       <- tibble(model=character(), scenario=character(), error=character())

for (i in seq_len(nrow(scenarios_to_run))) {
  
  mod  <- scenarios_to_run$model[i]
  scen <- scenarios_to_run$scenario[i]
  cat_label <- scenarios_to_run$Category[i]
  scen_key  <- paste(mod, scen, sep = "||")
  
  if (i %% 50 == 0 || i <= 5) {
    cat(sprintf("[%d/%d] %s — %s (%s)\n",
                i, nrow(scenarios_to_run), mod, scen, cat_label))
  }
  
  # Extract emissions for this scenario
  em_scen <- em_clean %>%
    filter(model == mod, scenario == scen)
  
  if (nrow(em_scen) == 0) {
    errors <- bind_rows(errors,
                        tibble(model=mod, scenario=scen,
                               error="No emissions data"))
    next
  }
  
  # Check we have all required pollutants
  required_pols <- c("SO2", "NOX", "BC", "OM", "NH3", "VOC", "CH4", "CO")
  present_pols  <- unique(em_scen$pollutant)
  missing_pols  <- setdiff(required_pols, present_pols)
  if (length(missing_pols) > 0) {
    # Fill missing pollutants with zeros (some models may not report all)
    fill_rows <- crossing(
      region    = COMPASS_R10_REGIONS,
      pollutant = missing_pols,
      year      = as.integer(FASST_YEARS)
    ) %>% mutate(value_kt = 0,
                 model    = mod,
                 scenario = scen)
    em_scen <- bind_rows(em_scen, fill_rows)
  }
  
  # Build em.list
  em_list_scen <- tryCatch(
    build_em_list(em_scen),
    error = function(e) {
      errors <<- bind_rows(errors,
                           tibble(model=mod, scenario=scen,
                                  error=paste("em.list build:", e$message)))
      NULL
    }
  )
  
  if (is.null(em_list_scen)) next
  
  # Run rfasst
  res <- run_rfasst_for_scenario(em_list_scen, scen)
  
  # Store results with scenario metadata
  if (!is.null(res$pm25)) {
    results_pm25[[scen_key]] <- bind_rows(res$pm25) %>%
      mutate(model = mod, scenario = scen, Category = cat_label)
  }
  if (!is.null(res$o3)) {
    results_o3[[scen_key]] <- bind_rows(res$o3) %>%
      mutate(model = mod, scenario = scen, Category = cat_label)
  }
}

cat("\n=== rfasst runs complete ===\n")
cat("Successful PM2.5:", length(results_pm25), "\n")
cat("Successful O3:   ", length(results_o3), "\n")
cat("Errors:          ", nrow(errors), "\n")
if (nrow(errors) > 0) {
  cat("Error summary:\n")
  print(errors %>% count(error))
}


# === SECTION 6: CORRECTED AGGREGATION ===

# PM2.5: columns are region, year, age, disease, GBD, GEMM, FUSION
pm25_all <- bind_rows(results_pm25)

pm25_r10 <- pm25_all %>%
  left_join(fasst_to_r10, by = c("region" = "fasst_region")) %>%
  filter(!is.na(r10_region), r10_region %in% COMPASS_R10_REGIONS) %>%
  group_by(model, scenario, Category, r10_region, year) %>%
  summarise(deaths_pm25 = sum(FUSION, na.rm = TRUE), .groups = "drop")

cat("PM2.5 R10 rows:", nrow(pm25_r10), "\n")
print(head(pm25_r10, 3))

# O3: columns are region, year, disease, Jerret2009, GBD2016
o3_all <- bind_rows(results_o3)

o3_r10 <- o3_all %>%
  left_join(fasst_to_r10, by = c("region" = "fasst_region")) %>%
  filter(!is.na(r10_region), r10_region %in% COMPASS_R10_REGIONS) %>%
  group_by(model, scenario, Category, r10_region, year) %>%
  summarise(deaths_o3 = sum(GBD2016, na.rm = TRUE), .groups = "drop")

cat("O3 R10 rows:", nrow(o3_r10), "\n")
print(head(o3_r10, 3))

# Join PM2.5 and O3
mortality_r10 <- pm25_r10 %>%
  full_join(o3_r10,
            by = c("model","scenario","Category","r10_region","year")) %>%
  mutate(
    deaths_pm25  = replace_na(deaths_pm25, 0),
    deaths_o3    = replace_na(deaths_o3, 0),
    deaths_total = deaths_pm25 + deaths_o3
  )

write.csv(mortality_r10,
          file.path(OUT_DIR, "compass_mortality_r10.csv"),
          row.names = FALSE)
cat("Saved: compass_mortality_r10.csv\n")

# === SECTION 7: CUMULATIVE SUMMARY 2020-2100 ===
mortality_summary <- mortality_r10 %>%
  filter(year >= 2020, year <= 2100) %>%
  group_by(model, scenario, Category, r10_region) %>%
  summarise(
    cumulative_deaths_pm25_2020_2100  = sum(deaths_pm25,  na.rm = TRUE),
    cumulative_deaths_o3_2020_2100    = sum(deaths_o3,    na.rm = TRUE),
    cumulative_deaths_total_2020_2100 = sum(deaths_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    cumulative_deaths_mln_pm25  = cumulative_deaths_pm25_2020_2100  / 1e6,
    cumulative_deaths_mln_o3    = cumulative_deaths_o3_2020_2100    / 1e6,
    cumulative_deaths_mln_total = cumulative_deaths_total_2020_2100 / 1e6,
    Model               = model,
    Scenario            = scenario,
    Region              = r10_region,
    Model_Group         = "COMPASS",
    ModelGroup_Scenario = paste("COMPASS", scenario, sep = "_")
  )

cat("\nScenarios in summary:",
    n_distinct(paste(mortality_summary$model, mortality_summary$scenario)), "\n")
cat("Regions:", paste(sort(unique(mortality_summary$r10_region)), collapse=", "), "\n")

cat("\nMedian cumulative PM2.5 mortality (millions, 2020-2100) by Region × Category:\n")
mortality_summary %>%
  group_by(Region, Category) %>%
  summarise(
    n            = n(),
    median_pm25  = round(median(cumulative_deaths_mln_pm25,  na.rm = TRUE), 2),
    median_total = round(median(cumulative_deaths_mln_total, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(Region, Category) %>%
  print(n = Inf)

write.csv(mortality_summary,
          file.path(OUT_DIR, "compass_mortality_summary.csv"),
          row.names = FALSE)

saveRDS(mortality_summary,
        file.path(COMPASS_DIR, "compass_mortality_summary.rds"))

cat("\nSaved: compass_mortality_summary.csv\n")
cat("Saved: compass_mortality_summary.rds\n")
cat("\n=== COMPLETE ===\n")


# =============================================================================
# SECTION 8: INTEGRATION INTO MAIN ANALYSIS SCRIPTS
# =============================================================================
# Add this block to COMPASS_full_analysis.R or Combined_AR6_COMPASS_analysis.R
# in Section 7 (mortality), replacing the existing partial-match approach:
#
#   mortality_summary <- readRDS(
#     file.path(COMPASS_DIR, "compass_mortality_summary.rds")
#   )
#
#   # Already has Model, Scenario, Region, Category, Model_Group,
#   # ModelGroup_Scenario, and cumulative_deaths_mln_pm25/o3/total columns.
#   # Use cumulative_deaths_mln_total as the main mortality outcome,
#   # or cumulative_deaths_mln_pm25 alone for comparability with AR6
#   # (which was also primarily PM2.5-driven in the original mortality.rds).
#
#   mortality_for_master <- mortality_summary %>%
#     select(Model, Scenario, Region, Category,
#            Model_Group, ModelGroup_Scenario,
#            cumulative_deaths_2020_2100 = cumulative_deaths_pm25_2020_2100,
#            cumulative_deaths_mln       = cumulative_deaths_mln_pm25)

