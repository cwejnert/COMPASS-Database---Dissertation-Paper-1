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
#   COMPASS emissions: Mt/yr  →  multiply by 1e9 to get kg/yr.  rfasst
#                              compares these values with its kg base inventory.
#   OC → OM: multiply by CONV_OC_POM (rfasst internal = 1.3)
#   Mortality output: deaths/yr per TM5-FASST region
#
# RUNTIME ESTIMATE:
#   ~1543 scenarios × two pollutant pathways (PM2.5 + O3)
#   Expected: 30-90 minutes depending on machine speed.
#   Progress is printed every 50 scenarios.
# =============================================================================

library(tidyverse)
library(rfasst)

COMPASS_DIR <- Sys.getenv(
  "COMPASS_DATA_DIR",
  "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/COMPASS"
)
OUT_DIR <- Sys.getenv(
  "COMPASS_MORTALITY_OUT_DIR",
  "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_mortality"
)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# SECTION 1: CONFIGURATION
# =============================================================================

# rfasst internal constants
FASST_YEARS  <- rfasst:::all_years          # years rfasst processes
CONV_OC_POM  <- rfasst:::CONV_OC_POM       # OC → OM factor (~1.3)
cat("rfasst years:", paste(FASST_YEARS, collapse=", "), "\n")
cat("OC→OM factor:", CONV_OC_POM, "\n")

# Categories to keep
categories_keep <- c("C1", "C2", "C3", "C4")

# COMPASS region names (as delivered by Python pull, already renamed to R10)
# ALL TEN R10 regions. fasst_to_r10 and fasst_pop below already cover all 56
# TM5-FASST regions and all ten R10 groupings, so no other change is needed here.
# NH3 and spatial-allocation configuration.  The production comparison should
# use `linked_or_world_fallback` plus `base_emission`: use linked MAgPIE NH3
# where supplied, otherwise disaggregate a reported World NH3 total, and never
# silently replace missing NH3 with zero. `fixed_zero` is retained only as a
# harmonised diagnostic, not as the full-PM2.5 primary outcome.
NH3_MODE <- Sys.getenv("COMPASS_NH3_MODE", "linked_or_world_fallback")
if (!NH3_MODE %in% c("fixed_zero", "as_reported", "world_fallback",
                     "linked_or_world_fallback")) {
  stop("Unknown COMPASS_NH3_MODE: ", NH3_MODE)
}
SPATIAL_ALLOCATION <- Sys.getenv("COMPASS_SPATIAL_ALLOCATION", "base_emission")
if (!SPATIAL_ALLOCATION %in% c("base_emission", "population")) {
  stop("COMPASS_SPATIAL_ALLOCATION must be base_emission or population")
}
COMPLETE_PM_ONLY <- tolower(Sys.getenv("COMPASS_COMPLETE_PM_ONLY", "true")) %in% c("true", "1", "yes")
REQUIRE_CREDIBLE_NH3 <- tolower(Sys.getenv("COMPASS_REQUIRE_CREDIBLE_NH3", "true")) %in% c("true", "1", "yes")
NH3_CREDIBILITY_MIN <- as.numeric(Sys.getenv("COMPASS_NH3_CREDIBILITY_MIN", "0.10"))
INTERPOLATE_SHORT_PM_GAPS <- tolower(Sys.getenv("COMPASS_INTERPOLATE_SHORT_PM_GAPS", "false")) %in% c("true", "1", "yes")
MAX_INTERP_MISSING_NODES <- as.integer(Sys.getenv("COMPASS_MAX_INTERP_MISSING_NODES", "2"))
ALLOW_WORLD_ONLY_DISAGG <- tolower(Sys.getenv("COMPASS_ALLOW_WORLD_ONLY_DISAGG", "true")) %in% c("true", "1", "yes")
if (!is.finite(MAX_INTERP_MISSING_NODES) || MAX_INTERP_MISSING_NODES < 1L) {
  stop("COMPASS_MAX_INTERP_MISSING_NODES must be a positive integer")
}
NH3_REFERENCE_YEAR <- 2020L
NH3_SIDECAR <- Sys.getenv(
  "COMPASS_NH3_SIDECAR",
  file.path(COMPASS_DIR, "compass_nh3_linked_magpie.csv")
)
DROP_NH3 <- identical(NH3_MODE, "fixed_zero")
MORTALITY_RUN_LABEL <- Sys.getenv("COMPASS_MORTALITY_RUN_LABEL", "")
MORTALITY_FILE_TAG <- paste0(
  "_", NH3_MODE, "_", SPATIAL_ALLOCATION,
  if (nzchar(MORTALITY_RUN_LABEL)) paste0("_", MORTALITY_RUN_LABEL) else ""
)
cat("Mortality config: NH3=", NH3_MODE, " | allocation=", SPATIAL_ALLOCATION,
    " | complete PM only=", COMPLETE_PM_ONLY,
    " | require credible NH3=", REQUIRE_CREDIBLE_NH3,
    " | interpolate short PM gaps=", INTERPOLATE_SHORT_PM_GAPS,
    " | allow World-only disaggregation=", ALLOW_WORLD_ONLY_DISAGG, "\n", sep = "")

# All scenarios are evaluated with the same demographic and baseline-mortality
# counterfactual.  Explicitly pass this to rfasst rather than relying on its
# default; this standardises health accounting across IAM pathways and is not a
# pathway-specific socioeconomic projection.
HEALTH_SSP <- "SSP2"
# The paper's mortality outcome is the PM2.5/FUSION result. O3 can be enabled
# for a separate all-pollutant exercise, but it is unaffected by NH3 infill and
# is not the column consumed by the master analysis.
RUN_O3 <- tolower(Sys.getenv("COMPASS_RUN_O3", "false")) %in% c("true", "1", "yes")

COMPASS_R10_REGIONS <- c("R10AFRICA", "R10CHINA+", "R10EUROPE", "R10INDIA+",
                         "R10LATIN_AM", "R10MIDDLE_EAST", "R10NORTH_AM",
                         "R10PAC_OECD", "R10REF_ECON", "R10REST_ASIA")

# TM5-FASST region → R10 region mapping
# Each of TM5-FASST's 56 regions is assigned to one R10 region.
# All ten R10 regions are retained and later aggregated to the World-equivalent
# ten-region total.
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
  "RSAS",   "R10REST_ASIA", # AFG BGD BTN NPL PAK. COMPASS R10INDIA+ is
                          # India alone (18.2% of world), so these 417 mln
                          # belong in Rest of Asia to match it.
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
  "UKR",    "R10REF_ECON",  # BLR + MDA + UKR = former Soviet Union
  # R10NORTH_AM
  "USA",    "R10NORTH_AM",
  "CAN",    "R10NORTH_AM",
  "MEX",    "R10LATIN_AM",  # AR6 North America = USA + CAN
  # Non-target R10 regions (included for full TM5-FASST coverage)
  "IDN",    "R10REST_ASIA",
  "JPN",    "R10PAC_OECD",  # AR6 Pacific OECD = AUS + JPN + NZL
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
  "RUS",    "R10REF_ECON",
  "RUE",    "R10REF_ECON"   # has population and base emissions but was
                          # unmapped, so its deaths were discarded
)

# Population weights (millions, ~2015) for distributing R10 emissions
# across constituent TM5-FASST regions.
# Source: UN World Population Prospects 2019, aggregated to TM5-FASST regions.
# Population by TM5-FASST region, millions, rebuilt from rfasst::pop.all.SSP2
# at 2015. Replaces a hand-entered table in which GOLF and MEME were roughly
# transposed, MON and RSAM were an order of magnitude out, and RUE was absent.
# Using the package's own population keeps the emission weights consistent with
# the population m3 uses to convert concentration into deaths.
fasst_pop <- tribble(
  ~fasst_region, ~pop,
  "ARG",   45.45, "AUS",   24.14, "AUT",   10.62, "BGR",   7.267,
  "BLX",   28.49, "BRA",   203.2, "CAN",   35.86, "CHE",     7.9,
  "CHL",   17.87, "CHN",    1371, "COR",   48.91, "EAF",   436.5,
  "EGY",   88.08, "ESP",   58.62, "FIN",   5.493, "FRA",    64.7,
  "GBR",   68.95, "GOLF",  193.5, "GRC",   12.63, "HUN",   9.853,
  "IDN",   252.6, "ITA",   61.91, "JPN",   126.1, "KAZ",   16.87,
  "MEME",  46.52, "MEX",     120, "MON",   27.87, "MYS",   36.65,
  "NDE",    1330, "NOA",   89.34, "NOR",   5.497, "NZL",   4.611,
  "PAC",   10.49, "PHL",   101.3, "POL",   45.17, "RCAM",  88.07,
  "RCEU",  23.87, "RCZ",    16.3, "RFA",   82.12, "RIS",   47.04,
  "ROM",   21.14, "RSA",   56.03, "RSAM",  145.5, "RSAS",  416.7,
  "RSEA",  70.72, "RUE",   37.21, "RUS",   122.5, "SAF",   97.88,
  "SWE",   15.45, "THA",   71.11, "TUR",   77.04, "TWN",   23.47,
  "UKR",   56.93, "USA",   322.8, "VNM",   92.38, "WAF",   372.7
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

# Primary spatial allocation: distribute each R10 pollutant to constituent
# TM5-FASST receptors by that pollutant's rfasst baseline-emissions share, not
# population. Population weights are retained as an explicit sensitivity. This
# preserves each IAM R10 total while avoiding the assumption that every source
# has the same spatial pattern as population.
base_em_long <- get("raw.base_em", envir = asNamespace("rfasst")) %>%
  as_tibble() %>%
  rename(fasst_region = COUNTRY) %>%
  filter(fasst_region %in% fasst_to_r10$fasst_region) %>%
  pivot_longer(any_of(c("SO2", "NOX", "BC", "OM", "NH3", "VOC", "CH4", "CO")),
               names_to = "pollutant", values_to = "base_kg") %>%
  left_join(fasst_to_r10, by = "fasst_region") %>%
  group_by(r10_region, pollutant) %>%
  mutate(weight = base_kg / sum(base_kg, na.rm = TRUE)) %>%
  ungroup() %>%
  select(fasst_region, r10_region, pollutant, base_kg, weight)
population_weights_long <- crossing(fasst_weights,
                                    pollutant = c("SO2", "NOX", "BC", "OM", "NH3", "VOC", "CH4", "CO"))
allocation_weights <- if (SPATIAL_ALLOCATION == "base_emission") base_em_long else population_weights_long
stopifnot(all(allocation_weights %>% group_by(r10_region, pollutant) %>%
                 summarise(s = sum(weight), .groups = "drop") %>% pull(s) > .999999))

# Shares for a World-only total entering each R10, likewise pollutant-specific.
r10_base_shares <- base_em_long %>%
  group_by(r10_region, pollutant) %>%
  summarise(base_kg = sum(base_kg), .groups = "drop") %>%
  group_by(pollutant) %>%
  mutate(r10_share = base_kg / sum(base_kg)) %>%
  ungroup() %>% select(r10_region, pollutant, r10_share)


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

# FASST has a 2005 baseline year, but COMPASS begins in 2010.  The model
# builder creates the 2005 baseline placeholder it requires; completeness of
# *reported* pathway precursors must only be judged against years supplied by
# COMPASS.  Requiring a 2005 observation would reject every otherwise-complete
# scenario by construction.
GRID_CHECK_YEARS <- intersect(FASST_YEARS, sort(unique(em_raw$year)))
if (length(GRID_CHECK_YEARS) == 0) {
  stop("No rfasst years are represented in compass_emissions_raw.csv")
}

cat("Loaded", nrow(em_raw), "rows,",
    n_distinct(paste(em_raw$model, em_raw$scenario)), "scenarios\n")

# Map variables to the exact column names used in rfasst::raw.base_em.
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

# COMPASS reports Mt/yr
# rfasst base_em is in kg (despite older comments in prior script versions)
# Mt × 1e9 = kg

if (DROP_NH3) {
  pollutant_map <- pollutant_map[names(pollutant_map) != "Emissions|NH3"]
  cat("\n*** DROP_NH3 = TRUE: ammonia excluded for ALL models ***\n")
  cat("    harmonised NH3-fixed comparison; not a full PM2.5 burden estimate\n\n")
}

em_clean <- em_raw %>%
  filter(
    variable %in% names(pollutant_map),
    region   %in% c(COMPASS_R10_REGIONS, "World"),
    year     %in% FASST_YEARS
  ) %>%
  mutate(
    pollutant = recode(variable, !!!pollutant_map),
    # Convert Mt/yr → kg (rfasst base_em units): Mt × 1e9 = kg
    value_kg  = value * 1e9,
    value_kg  = if_else(pollutant == "OC", value_kg * CONV_OC_POM, value_kg),
    pollutant = if_else(pollutant == "OC", "OM", pollutant)
  ) %>%
  filter(region != "World") %>%
  mutate(year = as.integer(year)) %>%
  select(model, scenario, region, pollutant, year, value_kg)

cat("em_clean rows (R10 scenarios):", nrow(em_clean), "\n")
cat("Scenarios in em_clean:", n_distinct(paste(em_clean$model, em_clean$scenario)), "\n")

# A missing interior reporting year is distinct from an unreported series. For
# the reporting-complete mortality diagnostic, permit only short, bracketed
# gaps in a directly reported R10 PM2.5 precursor series. Do not extrapolate,
# and do not create an entire region/pollutant series that was never reported.
# Every interpolation is retained in a separate provenance log.
PM_PRECURSORS <- c("SO2", "NOX", "BC", "OM", "NH3")
interpolation_log <- tibble(model = character(), scenario = character(),
  region = character(), pollutant = character(), year = integer(),
  value_kg = numeric(), method = character())
if (INTERPOLATE_SHORT_PM_GAPS) {
  interpolated_pm <- em_clean %>%
    filter(pollutant %in% PM_PRECURSORS) %>%
    group_by(model, scenario, region, pollutant) %>%
    group_modify(function(series, key) {
      series <- series %>% arrange(year)
      observed_years <- sort(unique(series$year))
      missing_years <- setdiff(as.integer(GRID_CHECK_YEARS), observed_years)
      if (!length(missing_years)) return(tibble())
      miss_index <- match(missing_years, as.integer(GRID_CHECK_YEARS))
      blocks <- split(miss_index, cumsum(c(1, diff(miss_index) != 1)))
      map_dfr(blocks, function(idx) {
        left_i <- min(idx) - 1L
        right_i <- max(idx) + 1L
        if (length(idx) > MAX_INTERP_MISSING_NODES || left_i < 1L ||
            right_i > length(GRID_CHECK_YEARS)) return(tibble())
        left_year <- as.integer(GRID_CHECK_YEARS[left_i])
        right_year <- as.integer(GRID_CHECK_YEARS[right_i])
        endpoints <- series %>% filter(year %in% c(left_year, right_year))
        if (nrow(endpoints) != 2L || any(!is.finite(endpoints$value_kg)) ||
            any(endpoints$value_kg < 0)) return(tibble())
        tibble(
          year = as.integer(GRID_CHECK_YEARS[idx]),
          value_kg = approx(endpoints$year, endpoints$value_kg,
                            xout = as.integer(GRID_CHECK_YEARS[idx]), method = "linear",
                            rule = 1)$y,
          method = "linear_bracketed_short_gap"
        )
      })
    }) %>% ungroup()
  if (nrow(interpolated_pm)) {
    em_clean <- bind_rows(em_clean, interpolated_pm %>% select(-method))
    interpolation_log <- interpolated_pm
  }
  cat("Short, bracketed PM precursor values interpolated:", nrow(interpolation_log), "\n")
}

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

cat("\nWorld-only scenarios detected:",
    nrow(world_only_scens), "\n")
if (ALLOW_WORLD_ONLY_DISAGG && nrow(world_only_scens) > 0) {
  for (s in sort(world_only_scens$scenario)) cat(" -", s, "\n")
}

# R10 population shares — renormalised to the 5 target R10 regions only
# FIX: fasst_weights$weight is normalised WITHIN each r10_region, so
# sum(weight) is exactly 1.0 for every region and renormalising a vector of
# ones produced an EQUAL 1/N split rather than population shares (20% each to
# five regions, regardless of size). Aggregate population first, then share.
r10_pop_shares <- fasst_to_r10 %>%
  left_join(fasst_pop, by = "fasst_region") %>%
  group_by(r10_region) %>%
  summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop") %>%
  filter(r10_region %in% COMPASS_R10_REGIONS) %>%
  mutate(r10_share = pop / sum(pop)) %>%
  select(r10_region, r10_share)

r10_population_shares_long <- crossing(r10_pop_shares,
  pollutant = c("SO2", "NOX", "BC", "OM", "NH3", "VOC", "CH4", "CO"))
r10_allocation_shares <- if (SPATIAL_ALLOCATION == "base_emission")
  r10_base_shares else r10_population_shares_long

stopifnot(abs(sum(r10_pop_shares$r10_share) - 1) < 1e-9,
          all(r10_pop_shares$r10_share > 0),
          nrow(r10_pop_shares) == length(COMPASS_R10_REGIONS))
cat("\nWorld-only disaggregation shares (population-weighted):\n")
print(as.data.frame(r10_pop_shares %>% mutate(pct = round(100*r10_share, 2)) %>%
                      arrange(desc(pct))), row.names = FALSE)

# Disaggregate World emissions proportionally to R10 regions only where the
# configured analysis explicitly permits this spatial-imputation sensitivity.
if (ALLOW_WORLD_ONLY_DISAGG && nrow(world_only_scens) > 0) {
  em_world_disagg <- em_raw %>%
    inner_join(world_only_scens, by = c("model", "scenario")) %>%
    filter(
      variable %in% names(pollutant_map),
      region   == "World",
      year     %in% FASST_YEARS
    ) %>%
    mutate(
      pollutant = recode(variable, !!!pollutant_map),
      value_kg  = value * 1e9,
      value_kg  = if_else(pollutant == "OC", value_kg * CONV_OC_POM, value_kg),
      pollutant = if_else(pollutant == "OC", "OM", pollutant),
      year      = as.integer(year)
    ) %>%
    select(model, scenario, pollutant, year, value_kg) %>%
    inner_join(r10_allocation_shares, by = "pollutant", relationship = "many-to-many") %>%
    mutate(
      value_kg = value_kg * r10_share,
      region   = r10_region
    ) %>%
    select(model, scenario, region, pollutant, year, value_kg)
  
  em_clean <- bind_rows(em_clean, em_world_disagg)
  cat("em_clean rows after World disaggregation:", nrow(em_clean), "\n")
} else if (!ALLOW_WORLD_ONLY_DISAGG && nrow(world_only_scens) > 0) {
  cat("World-only emission scenarios excluded from no-imputation diagnostic:",
      nrow(world_only_scens), "\n")
}

# NH3 provenance hierarchy. Retain credible native reported R10 NH3. If the
# reported total is absent or implausibly small relative to the TM5-FASST base
# inventory, it is incomplete for a full-PM2.5 calculation (as happens for the
# affected REMIND-family records) and the linked full-NH3 sidecar replaces it.
# This is therefore a credibility-gated hierarchy, not a blind override. If a
# scenario-year has no R10 NH3, a reported World total is transparently
# disaggregated. Missing NH3 is never silently zero-filled in these modes.
nh3_input_provenance <- tibble(model = character(), scenario = character(),
  year = integer(), nh3_source = character())
native_nh3_provenance <- em_clean %>%
  filter(pollutant == "NH3") %>%
  distinct(model, scenario, year) %>%
  mutate(nh3_source = "reported_r10")
if (!DROP_NH3 && NH3_MODE == "linked_or_world_fallback" && file.exists(NH3_SIDECAR)) {
  nh3_sidecar_raw <- read.csv(NH3_SIDECAR, stringsAsFactors = FALSE, check.names = FALSE)
  val_col <- intersect(c("value", "value_Mt", "Value"), names(nh3_sidecar_raw))[1]
  if (is.na(val_col) || !all(c("model", "scenario", "region", "year") %in% names(nh3_sidecar_raw))) {
    stop("NH3 sidecar must contain model, scenario, region, year and value/value_Mt")
  }
  nh3_linked <- nh3_sidecar_raw %>%
    transmute(model, scenario, region, year = as.integer(year),
      pollutant = "NH3", value_kg = as.numeric(.data[[val_col]]) * 1e9) %>%
    filter(region %in% COMPASS_R10_REGIONS, year %in% FASST_YEARS,
           is.finite(value_kg), value_kg >= 0) %>% distinct()
  base_nh3_total <- get("raw.base_em", envir = asNamespace("rfasst")) %>%
    filter(COUNTRY == "*TOTAL*") %>% pull(NH3)
  native_nh3_status <- em_clean %>% filter(pollutant == "NH3", year == NH3_REFERENCE_YEAR) %>%
    group_by(model, scenario) %>% summarise(native_nh3_kg = sum(value_kg), .groups = "drop") %>%
    mutate(native_nh3_credible = is.finite(native_nh3_kg) &
             native_nh3_kg / base_nh3_total >= NH3_CREDIBILITY_MIN)
  sidecar_scenarios <- nh3_linked %>% distinct(model, scenario) %>%
    left_join(native_nh3_status, by = c("model", "scenario")) %>%
    mutate(native_nh3_credible = coalesce(native_nh3_credible, FALSE))
  native_nh3_keys <- em_clean %>% filter(pollutant == "NH3") %>%
    distinct(model, scenario, region, pollutant, year)
  nh3_linked_fill <- nh3_linked %>%
    anti_join(native_nh3_keys,
              by = c("model", "scenario", "region", "pollutant", "year"))
  nh3_linked_replace <- nh3_linked %>%
    semi_join(filter(sidecar_scenarios, !native_nh3_credible),
              by = c("model", "scenario"))
  nh3_sidecar_apply <- bind_rows(nh3_linked_fill, nh3_linked_replace) %>% distinct()
  em_clean <- em_clean %>%
    anti_join(nh3_sidecar_apply %>% select(model, scenario, region, pollutant, year),
              by = c("model", "scenario", "region", "pollutant", "year")) %>%
    bind_rows(nh3_sidecar_apply)
  nh3_input_provenance <- bind_rows(nh3_input_provenance,
    native_nh3_provenance %>%
      anti_join(nh3_sidecar_apply %>% distinct(model, scenario, year),
                by = c("model", "scenario", "year")),
    nh3_sidecar_apply %>% distinct(model, scenario, year) %>%
      mutate(nh3_source = "linked_sidecar_after_native_credibility_check"))
  cat("Applied linked-NH3 sidecar rows after native credibility check: ",
      nrow(nh3_sidecar_apply), " / ", nrow(nh3_linked), " supplied rows\n", sep = "")
} else {
  nh3_input_provenance <- bind_rows(nh3_input_provenance, native_nh3_provenance)
}

if (!DROP_NH3 && NH3_MODE %in% c("world_fallback", "linked_or_world_fallback")) {
  nh3_world <- em_raw %>%
    filter(variable == "Emissions|NH3", region == "World", year %in% FASST_YEARS) %>%
    transmute(model, scenario, year = as.integer(year), pollutant = "NH3",
              value_kg = as.numeric(value) * 1e9) %>%
    filter(is.finite(value_kg), value_kg >= 0)
  nh3_r10_keys <- em_clean %>% filter(pollutant == "NH3") %>%
    distinct(model, scenario, year)
  nh3_world_fallback <- nh3_world %>%
    anti_join(nh3_r10_keys, by = c("model", "scenario", "year")) %>%
    inner_join(filter(r10_allocation_shares, pollutant == "NH3"), by = "pollutant") %>%
    transmute(model, scenario, region = r10_region, pollutant, year,
              value_kg = value_kg * r10_share)
  if (nrow(nh3_world_fallback) > 0) {
    em_clean <- bind_rows(em_clean, nh3_world_fallback)
    nh3_input_provenance <- bind_rows(nh3_input_provenance,
      nh3_world_fallback %>% distinct(model, scenario, year) %>%
        mutate(nh3_source = "world_total_disaggregated"))
  }
  cat("NH3 World-fallback R10 rows: ", nrow(nh3_world_fallback), "\n", sep = "")
}

if (!DROP_NH3) {
  base_nh3_total <- get("raw.base_em", envir = asNamespace("rfasst")) %>%
    filter(COUNTRY == "*TOTAL*") %>% pull(NH3)
  nh3_status <- em_clean %>% filter(pollutant == "NH3", year == NH3_REFERENCE_YEAR) %>%
    group_by(model, scenario) %>% summarise(nh3_kg = sum(value_kg), .groups = "drop") %>%
    mutate(nh3_ratio_to_fasst_base = nh3_kg / base_nh3_total,
           nh3_credible = is.finite(nh3_ratio_to_fasst_base) &
                          nh3_ratio_to_fasst_base >= NH3_CREDIBILITY_MIN)
  nh3_source_by_scenario <- nh3_input_provenance %>%
    group_by(model, scenario) %>% summarise(nh3_source = paste(sort(unique(nh3_source)), collapse = ";"),
      .groups = "drop")
  nh3_input_provenance <- nh3_status %>%
    left_join(nh3_source_by_scenario, by = c("model", "scenario")) %>%
    mutate(nh3_source = coalesce(nh3_source, "reported_r10"))
} else {
  nh3_status <- tibble(model = character(), scenario = character(),
    nh3_kg = numeric(), nh3_ratio_to_fasst_base = numeric(), nh3_credible = logical())
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
# ---- Scenario selection --------------------------------------------------
# RUN_ALL_SCENARIOS = TRUE runs rfasst on EVERY scenario that has pollutant
# emissions, WITHOUT pre-filtering on the metadata AR6 category. This matters:
# the master analysis classifies C1-C4 from compass_interp, but this metadata
# category (compass_categories) is missing / C5+ / fails to name-match for some
# of those same scenarios — so a C1-C4 pre-filter here silently drops scenarios
# the master DOES analyse, leaving them without mortality. Running on all
# emissions scenarios guarantees mortality exists for every classified
# scenario; the master joins by Model+Scenario and uses only what it needs.
# Category is kept as a (possibly NA) label only. Set FALSE to reproduce the
# original vetted, C1-C4-only run.
RUN_ALL_SCENARIOS <- TRUE

# per-scenario record of which pollutants had to be zero-filled
pollutant_coverage <- tibble(model = character(), scenario = character(),
                             n_pm_precursors = integer(), n_pm_complete = integer(),
                             n_pm_nonzero = integer(), missing_pm_slots = integer(),
                             nh3_source = character(), nh3_ratio_to_fasst_base = numeric(),
                             nh3_credible = logical(), filled = character(),
                             reported_all_zero = character())

scenarios_to_run <- em_clean %>%
  distinct(model, scenario) %>%
  left_join(
    compass_categories,
    by = c("model" = "Model", "scenario" = "Scenario")
  ) %>%
  select(model, scenario, Category)

# Optional explicit analysis universe. This is used for targeted sensitivity
# runs where the paper only needs the mutually-exclusive High-CMT/High-RE
# scenarios from the declared full-database and SCI-vetted contrasts. It avoids
# computing mortality for pathways that cannot enter any reported contrast.
SCENARIO_SET_FILE <- Sys.getenv("COMPASS_MORTALITY_SCENARIO_SET", "")
if (nzchar(SCENARIO_SET_FILE)) {
  if (!file.exists(SCENARIO_SET_FILE))
    stop("COMPASS_MORTALITY_SCENARIO_SET does not exist: ", SCENARIO_SET_FILE)
  selected_raw <- read.csv(SCENARIO_SET_FILE, stringsAsFactors = FALSE, check.names = FALSE)
  if ("Model" %in% names(selected_raw)) names(selected_raw)[names(selected_raw) == "Model"] <- "model"
  if ("Scenario" %in% names(selected_raw)) names(selected_raw)[names(selected_raw) == "Scenario"] <- "scenario"
  if (!all(c("model", "scenario") %in% names(selected_raw)))
    stop("COMPASS_MORTALITY_SCENARIO_SET must contain model/scenario or Model/Scenario")
  selected <- selected_raw %>% distinct(model, scenario)
  scenarios_to_run <- scenarios_to_run %>% semi_join(selected, by = c("model", "scenario"))
  cat("Applied explicit mortality scenario set:", nrow(scenarios_to_run),
      "scenarios from", SCENARIO_SET_FILE, "\n")
}

if (!RUN_ALL_SCENARIOS) {
  scenarios_to_run <- scenarios_to_run %>%
    filter(!is.na(Category), Category %in% c("C1","C2","C3","C4")) %>%
    filter(scenario %in% vetted_scenarios)
}
scenarios_to_run <- scenarios_to_run %>%
  arrange(model, scenario)

cat("RUN_ALL_SCENARIOS:", RUN_ALL_SCENARIOS,
    "(TRUE = all emissions scenarios, no category pre-filter)\n")
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
#   pollutant : pollutant name (SO2, NOX, BC, OM, NH3, VOC, CH4, CO)
#   value     : emissions in kg/yr
#   units     : "kg" (constant)
#
# We build this by distributing R10 emissions to TM5-FASST regions using the
# configured pollutant-specific allocation, then formatting into the list.

build_em_list <- function(em_scenario) {
  
  all_pollutants    <- c("BC", "CH4", "CO", "CO2", "N2O", "NH3",
                         "NOX", "OM", "PM25", "SO2", "VOC")
  all_fasst_regions <- c(unique(rfasst::fasst_reg$fasst_region),
                         "AIR", "SHIP", "RUE")
  all_years_chr     <- as.character(sort(FASST_YEARS))
  
  # Distribute R10 emissions to TM5-FASST regions
  em_fasst <- em_scenario %>%
    inner_join(allocation_weights,
               by = c("region" = "r10_region", "pollutant"),
               relationship = "many-to-many") %>%
    mutate(
      value_fasst = value_kg * weight,
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
      ssp=HEALTH_SSP,
      saveOutput=FALSE, map=FALSE, recompute=TRUE
    ),
    error = function(e) { message("  PM2.5 error: ", e$message); NULL }
  )
  
  mort_o3 <- NULL
  if (RUN_O3) {
    # Clear m2 cache between PM2.5 and O3
    for (v in c("m2_get_conc_pm25.output", "m2_get_conc_m6m.output",
                "m3_get_mort_pm25.output")) {
      if (exists(v, envir=.GlobalEnv)) rm(list=v, envir=.GlobalEnv)
    }
    mort_o3 <- tryCatch(
      rfasst::m3_get_mort_o3(
        prj=list(dummy=TRUE), scen_name=scen_label,
        ssp=HEALTH_SSP,
        saveOutput=FALSE, map=FALSE, recompute=TRUE
      ),
      error = function(e) { message("  O3 error: ", e$message); NULL }
    )
  }
  
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
  
  # Check we have all required pollutants.
  # "Not reported" is NOT the same fact as "zero". Zero-filling an unreported
  # pollutant silently understates mortality; zero-filling ALL of them makes
  # every affected scenario return the same population-driven vector (this is
  # why 65 scenarios across six model families shared one identical result).
  # We still fill, because rfasst needs a complete em.list, but we RECORD what
  # was filled and refuse outright when no PM2.5 precursor is present at all.
  required_pols <- c("SO2", "NOX", "BC", "OM", "NH3", "VOC", "CH4", "CO")
  # PM2.5 in rfasst Module 2 is driven by SO2, NOX, NH3, BC and OM. NMVOC,
  # CO and CH4 are ozone/methane inputs, not PM2.5 precursors; do not use them
  # to label a PM2.5 result as complete.
  pm_pols       <- if (DROP_NH3) c("SO2", "NOX", "BC", "OM")
                   else      c("SO2", "NOX", "BC", "OM", "NH3")
  present_pols  <- unique(em_scen$pollutant)
  missing_pols  <- setdiff(required_pols, present_pols)

  # PRESENCE IS NOT SIGNAL. A model can report every precursor as a series of
  # literal zeros: em_raw keeps those rows (it only drops NA), so a name-based
  # check passes, rfasst runs on an all-zero emission field, and the result is
  # exactly zero deaths -- indistinguishable from a computed answer. This is
  # what produced 105 WITCH scenarios at 0.000 across all ten regions while
  # WITCH-GLOBIOM, with identical coverage, returned a median of 3.33 million.
  # Count only precursors that are non-zero somewhere in the window.
  nz <- em_scen %>%
    group_by(pollutant) %>%
    summarise(any_nz = any(value_kg > 0, na.rm = TRUE), .groups = "drop") %>%
    filter(any_nz) %>% pull(pollutant)
  empty_pols    <- setdiff(intersect(pm_pols, present_pols), nz)
  n_pm_present  <- length(intersect(pm_pols, present_pols))
  n_pm_nonzero  <- length(intersect(pm_pols, nz))
  required_pm_grid <- crossing(region = COMPASS_R10_REGIONS, pollutant = pm_pols,
                               year = as.integer(GRID_CHECK_YEARS))
  observed_pm_grid <- em_scen %>% filter(pollutant %in% pm_pols) %>%
    distinct(region, pollutant, year)
  missing_pm_slots <- nrow(anti_join(required_pm_grid, observed_pm_grid,
                                     by = c("region", "pollutant", "year")))
  n_pm_complete <- pm_pols %>%
    vapply(function(p) sum(observed_pm_grid$pollutant == p) ==
      length(COMPASS_R10_REGIONS) * length(GRID_CHECK_YEARS), logical(1)) %>% sum()
  nh3_info <- nh3_status %>% filter(model == mod, scenario == scen) %>%
    slice_head(n = 1)
  nh3_source_label <- nh3_input_provenance %>% filter(model == mod, scenario == scen) %>%
    pull(nh3_source) %>% unique() %>% paste(collapse = ";")

  pollutant_coverage <- bind_rows(pollutant_coverage, tibble(
    model = mod, scenario = scen,
    n_pm_precursors = n_pm_present, n_pm_complete = n_pm_complete,
    n_pm_nonzero    = n_pm_nonzero,
    missing_pm_slots = missing_pm_slots,
    nh3_source = nh3_source_label,
    nh3_ratio_to_fasst_base = if (nrow(nh3_info)) nh3_info$nh3_ratio_to_fasst_base else NA_real_,
    nh3_credible = if (nrow(nh3_info)) nh3_info$nh3_credible else NA,
    filled = paste(sort(missing_pols), collapse = ";"),
    reported_all_zero = paste(sort(empty_pols), collapse = ";")))

  if (n_pm_nonzero == 0) {
    errors <- bind_rows(errors, tibble(model = mod, scenario = scen,
      error = if (n_pm_present == 0)
        "No PM2.5 precursors reported - not run (would return a constant)"
      else
        "All PM2.5 precursors reported as ZERO - not run (would return zero deaths)"))
    next
  }

  if (COMPLETE_PM_ONLY && (n_pm_complete < length(pm_pols) ||
                           n_pm_nonzero < length(pm_pols))) {
    errors <- bind_rows(errors, tibble(model = mod, scenario = scen,
      error = "Incomplete/nonzero PM2.5 precursor grid - not run"))
    next
  }
  if (!DROP_NH3 && REQUIRE_CREDIBLE_NH3 &&
      (!nrow(nh3_info) || !isTRUE(nh3_info$nh3_credible))) {
    errors <- bind_rows(errors, tibble(model = mod, scenario = scen,
      error = "NH3 below credibility threshold or absent - not run"))
    next
  }

  if (length(missing_pols) > 0) {
    fill_rows <- crossing(
      region    = COMPASS_R10_REGIONS,
      pollutant = missing_pols,
      year      = as.integer(FASST_YEARS)
    ) %>% mutate(value_kg = 0,
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

pm25_r10 <- if (nrow(pm25_all) == 0) {
  tibble(model = character(), scenario = character(), Category = character(),
         r10_region = character(), year = integer(), deaths_pm25 = numeric())
} else {
  pm25_all %>%
    left_join(fasst_to_r10, by = c("region" = "fasst_region")) %>%
    filter(!is.na(r10_region), r10_region %in% COMPASS_R10_REGIONS) %>%
    group_by(model, scenario, Category, r10_region, year) %>%
    summarise(deaths_pm25 = if (all(is.na(FUSION))) NA_real_
                                else sum(FUSION, na.rm = TRUE), .groups = "drop")
}

cat("PM2.5 R10 rows:", nrow(pm25_r10), "\n")
print(head(pm25_r10, 3))

# O3: columns are region, year, disease, Jerret2009, GBD2016
o3_all <- bind_rows(results_o3)

o3_r10 <- if (nrow(o3_all) == 0) {
  tibble(model = character(), scenario = character(), Category = character(),
         r10_region = character(), year = integer(), deaths_o3 = numeric())
} else {
  o3_all %>%
    left_join(fasst_to_r10, by = c("region" = "fasst_region")) %>%
    filter(!is.na(r10_region), r10_region %in% COMPASS_R10_REGIONS) %>%
    group_by(model, scenario, Category, r10_region, year) %>%
    summarise(deaths_o3 = if (all(is.na(GBD2016))) NA_real_
                              else sum(GBD2016, na.rm = TRUE), .groups = "drop")
}

cat("O3 R10 rows:", nrow(o3_r10), "\n")
print(head(o3_r10, 3))

# Join PM2.5 and O3
mortality_r10 <- pm25_r10 %>%
  full_join(o3_r10,
            by = c("model","scenario","Category","r10_region","year")) %>%
  # A MISSING PM2.5 RUN IS NOT ZERO DEATHS. When m3_get_mort_pm25 errors the
  # scenario never reaches results_pm25, but its O3 rows still carry the
  # full_join -- so replace_na() was converting "the model crashed" into a
  # confident 0.000. That is what produced 1,050 WITCH rows at exactly zero.
  # Keep the absence as NA and let the analysis exclude it honestly.
  mutate(
    deaths_o3    = replace_na(deaths_o3, 0),
    deaths_total = ifelse(is.na(deaths_pm25), NA_real_, deaths_pm25 + deaths_o3)
  )

# Persist the error log. A scenario that errored in m3 is now an NA rather
# than a zero, so this file is the only place the reason survives.
if (nrow(errors) > 0) {
  write.csv(errors, file.path(OUT_DIR, "compass_mortality_errors.csv"),
            row.names = FALSE)
  write.csv(errors, file.path(COMPASS_DIR, "compass_mortality_errors.csv"),
            row.names = FALSE)
  cat("\n*** ", nrow(errors), " scenarios FAILED and are NA, not zero ***\n", sep = "")
  print(as.data.frame(errors %>%
    mutate(fam = sub("[ /].*$", "", model)) %>%
    count(fam, error, name = "n_scenarios")))
  cat("saved: compass_mortality_errors.csv\n\n")
}

if (INTERPOLATE_SHORT_PM_GAPS) {
  interp_name <- paste0("compass_mortality_interpolation", MORTALITY_FILE_TAG, ".csv")
  write.csv(interpolation_log, file.path(OUT_DIR, interp_name), row.names = FALSE)
  write.csv(interpolation_log, file.path(COMPASS_DIR, interp_name), row.names = FALSE)
  cat("Saved interpolation provenance:", interp_name, "(", nrow(interpolation_log), " values)\n")
}

# which families lost PM2.5 entirely
pm_lost <- mortality_r10 %>%
  mutate(fam = sub("[ /].*$", "", model)) %>%
  group_by(fam) %>%
  summarise(scenarios = n_distinct(paste(model, scenario)),
            pm25_all_NA = sum(is.na(deaths_pm25)) == n(), .groups = "drop") %>%
  filter(pm25_all_NA)
if (nrow(pm_lost) > 0) {
  cat("MODEL FAMILIES WITH NO USABLE PM2.5 MORTALITY:\n")
  print(as.data.frame(pm_lost %>% select(fam, scenarios)))
  cat("\n")
}

mortality_r10_name <- paste0("compass_mortality_r10", MORTALITY_FILE_TAG, ".csv")
write.csv(mortality_r10, file.path(OUT_DIR, mortality_r10_name), row.names = FALSE)
# ALSO write to COMPASS_DIR, with an explicit release tag. This prevents a
# harmonised run from overwriting the as-reported transparency input (or vice
# versa) and lets the master script request the intended release explicitly.
write.csv(mortality_r10, file.path(COMPASS_DIR, mortality_r10_name), row.names = FALSE)
cat("Saved:", mortality_r10_name, "(to OUT_DIR and COMPASS_DIR)\n")

# === SECTION 7: CUMULATIVE SUMMARY 2020-2100 ===
mortality_summary <- mortality_r10 %>%
  filter(year >= 2020, year <= 2100) %>%
  group_by(model, scenario, Category, r10_region) %>%
  summarise(
    # all-NA must not become 0 here either
    cumulative_deaths_pm25_2020_2100  = if (all(is.na(deaths_pm25)))  NA_real_
                                        else sum(deaths_pm25,  na.rm = TRUE),
    cumulative_deaths_o3_2020_2100    = if (all(is.na(deaths_o3)))    NA_real_
                                        else sum(deaths_o3,    na.rm = TRUE),
    cumulative_deaths_total_2020_2100 = if (all(is.na(deaths_total))) NA_real_
                                        else sum(deaths_total, na.rm = TRUE),
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

mortality_summary_name <- paste0("compass_mortality_summary", MORTALITY_FILE_TAG)
write.csv(mortality_summary,
          file.path(OUT_DIR, paste0(mortality_summary_name, ".csv")),
          row.names = FALSE)

saveRDS(mortality_summary,
        file.path(COMPASS_DIR, paste0(mortality_summary_name, ".rds")))

# Which pollutants had to be zero-filled, per scenario. Needed downstream to
# separate genuinely-computed mortality from partially-filled mortality.
coverage_name <- paste0("compass_mortality_coverage", MORTALITY_FILE_TAG, ".rds")
saveRDS(pollutant_coverage,
        file.path(COMPASS_DIR, coverage_name))
cat("
POLLUTANT COVERAGE
")
cat("  scenarios run                  :", nrow(pollutant_coverage), "
")
cat("  with all required PM2.5 precursors:",
    sum(pollutant_coverage$n_pm_precursors == length(pm_pols)), "
")
cat("  PARTIAL (potentially understated):",
    sum(pollutant_coverage$n_pm_precursors < length(pm_pols)), "
")
cat("  reported but ALL ZERO          :",
    sum(pollutant_coverage$n_pm_nonzero < pollutant_coverage$n_pm_precursors), "
")
cat("\nScenarios whose precursors are present but entirely zero:\n")
print(as.data.frame(pollutant_coverage %>%
  filter(nchar(reported_all_zero) > 0) %>%
  count(model, reported_all_zero, name = "n_scenarios")))
print(as.data.frame(dplyr::count(pollutant_coverage, n_pm_precursors)))

cat("\nSaved:", mortality_summary_name, ".csv\n", sep = "")
cat("Saved:", mortality_summary_name, ".rds\n", sep = "")
cat("Saved:", coverage_name, "\n")
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
