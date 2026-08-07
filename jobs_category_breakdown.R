# =============================================================================
# jobs_category_breakdown.R  —  RUN LOCALLY
#
# Breaks total energy-sector employment down by JOB CATEGORY (construction,
# manufacturing, O&M, extraction, refinery — the raw Rutovitz et al. 2015
# categories) rather than by technology group. COMPASS_master_analysis.R
# computes each category's jobs internally (build_ef = construction +
# manufacturing applied to capacity ADDITIONS; ongoing_ef = oem + extraction +
# refinery applied to installed STOCK) but only exports the two combined
# streams, folded into tech-group totals. This script keeps all five
# categories separate instead of collapsing them, so we can show which part
# of the employment effect is construction/manufacturing (one-time, tied to
# the pace of build-out) vs. O&M/extraction/refinery (ongoing, tied to the
# size of the installed fleet).
#
# Mirrors COMPASS_master_analysis.R Section 4b (job_factors_complete load +
# geothermal injection, cap_additions_fuel_map / cap_stock_fuel_map, additions_ts
# / stock_ts) exactly, so results are consistent with the master pipeline.
# Self-contained: reloads compass_interp.rds and job_factors_complete.csv
# directly rather than assuming an existing R session, so it will work
# whether or not you still have COMPASS_master_analysis.R's environment open.
#
# REQUIRES: approach_A and approach_C already run once by COMPASS_master_analysis.R
# (reads their compass_pathway_tercile_*.csv for the High-CDR/High-RE labels).
#
# OUTPUT: jobs_category_breakdown.csv (small — attach or paste back)
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr)
})

COMPASS_DIR <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/COMPASS"
AR6_DIR     <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/AR6"
MASTER_OUT_DIR <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_master"
START_YEAR  <- 2020L
WINDOW_END  <- 2100L   # matches OUTCOME_WINDOW_END in COMPASS_master_analysis.R

regions_r10 <- c("R10AFRICA", "R10CHINA+", "R10INDIA+",
                 "R10EUROPE", "R10NORTH_AM")
cats_keep   <- c("C1", "C2", "C3", "C4")

# ---- 1. load + rebuild the R10 timeseries (same filter as the master script) --
compass_interp <- readRDS(file.path(COMPASS_DIR, "compass_interp.rds"))
compass_ts <- compass_interp %>%
  filter(Region %in% regions_r10, Category %in% cats_keep,
         Year >= 2015, Year <= 2100, !is.na(Value))

pop_ts <- compass_ts %>% filter(Variable == "Population")
pop2020_r10 <- pop_ts %>% filter(Region %in% regions_r10, Year == 2020) %>%
  group_by(Region) %>% summarise(pop_mln = median(Value, na.rm = TRUE), .groups = "drop")
pop2020_total <- sum(pop2020_r10$pop_mln, na.rm = TRUE)
cat("2020 5-region population (mln):", round(pop2020_total), "\n")

# ---- 2. job factors + geothermal injection (identical to master script) -----
job_factors_complete <- read.csv(file.path(AR6_DIR, "job_factors_complete.csv"))
if (!any(job_factors_complete$fuel == "geothermal")) {
  .geo_global <- tribble(~category,        ~ef_global,
                         "oem",            1170,
                         "construction",   3100,
                         "manufacturing",  3300)
  .donors <- c("biomass", "hydro", "wind_on", "solar_pv", "nuclear")
  .gm <- function(x) exp(mean(log(x)))
  .region_mult <- job_factors_complete %>%
    filter(category == "oem", fuel %in% .donors,
           region %in% regions_r10, job_intensity > 0) %>%
    group_by(fuel) %>%
    mutate(ratio = job_intensity / .gm(job_intensity)) %>%
    group_by(region) %>%
    summarise(mult = .gm(ratio), .groups = "drop")
  geothermal_factors <- expand_grid(region = regions_r10,
                                    category = .geo_global$category) %>%
    left_join(.geo_global, by = "category") %>%
    left_join(.region_mult, by = "region") %>%
    transmute(region, fuel = "geothermal", category,
              job_intensity = ef_global * mult)
  job_factors_complete <- bind_rows(job_factors_complete, geothermal_factors)
}
job_ef_long <- job_factors_complete %>%
  filter(category %in% c("construction", "manufacturing", "oem", "extraction", "refinery")) %>%
  select(region, fuel, category, job_intensity)

# ---- 3. capacity additions / stock by fuel (identical maps to master script) -
cap_additions_fuel_map <- tribble(
  ~Variable,                                    ~fuel,        ~tech_group,
  "Capacity Additions|Electricity|Solar",       "solar_pv",   "Renewables",
  "Capacity Additions|Electricity|Wind",        "wind_on",    "Renewables",
  "Capacity Additions|Electricity|Hydro",       "hydro",      "Renewables",
  "Capacity Additions|Electricity|Geothermal",  "geothermal", "Renewables",
  "Capacity Additions|Electricity|Nuclear",     "nuclear",    "Nuclear",
  "Capacity Additions|Electricity|Biomass",     "biomass",    "Bioenergy",
  "Capacity Additions|Electricity|Coal",        "coal",       "Fossil",
  "Capacity Additions|Electricity|Gas",         "gas",        "Fossil",
  "Capacity Additions|Electricity|Oil",         "oil",        "Fossil"
)
cap_stock_fuel_map <- tribble(
  ~Variable,                          ~fuel,        ~tech_group,
  "Capacity|Electricity|Solar",       "solar_pv",   "Renewables",
  "Capacity|Electricity|Wind",        "wind_on",    "Renewables",
  "Capacity|Electricity|Hydro",       "hydro",      "Renewables",
  "Capacity|Electricity|Geothermal",  "geothermal", "Renewables",
  "Capacity|Electricity|Nuclear",     "nuclear",    "Nuclear",
  "Capacity|Electricity|Biomass",     "biomass",    "Bioenergy",
  "Capacity|Electricity|Coal",        "coal",       "Fossil",
  "Capacity|Electricity|Gas",         "gas",        "Fossil",
  "Capacity|Electricity|Oil",         "oil",        "Fossil"
)

cap_additions_ts <- compass_ts %>% filter(Variable %in% cap_additions_fuel_map$Variable)
scens_with_additions <- cap_additions_ts %>% filter(Value > 0, Year >= START_YEAR) %>%
  distinct(Model, Scenario, Region)
scens_needing_stockdiff <- compass_ts %>%
  filter(Variable %in% cap_stock_fuel_map$Variable, Year >= START_YEAR) %>%
  distinct(Model, Scenario, Region) %>%
  anti_join(scens_with_additions, by = c("Model", "Scenario", "Region"))

stock_ts <- compass_ts %>%
  filter(Variable %in% cap_stock_fuel_map$Variable, Year >= START_YEAR) %>%
  inner_join(cap_stock_fuel_map, by = "Variable") %>%
  transmute(Model, Scenario, Region, Category, Year, fuel, tech_group, stock_GW = pmax(0, Value))

add_direct <- cap_additions_ts %>%
  semi_join(scens_with_additions, by = c("Model", "Scenario", "Region")) %>%
  filter(Year >= START_YEAR) %>%
  inner_join(cap_additions_fuel_map, by = "Variable") %>%
  transmute(Model, Scenario, Region, Category, Year, fuel, tech_group, add_GW = pmax(0, Value))
add_fallback <- compass_ts %>%
  filter(Variable %in% cap_stock_fuel_map$Variable, Year >= START_YEAR) %>%
  semi_join(scens_needing_stockdiff, by = c("Model", "Scenario", "Region")) %>%
  inner_join(cap_stock_fuel_map, by = "Variable") %>%
  arrange(Model, Scenario, Region, fuel, Year) %>%
  group_by(Model, Scenario, Region, Category, fuel, tech_group) %>%
  mutate(add_GW = pmax(0, (Value - lag(Value)) / (Year - lag(Year)))) %>%
  ungroup() %>% filter(!is.na(add_GW)) %>%
  transmute(Model, Scenario, Region, Category, Year, fuel, tech_group, add_GW)
additions_ts <- bind_rows(add_direct, add_fallback)

# ---- 4. jobs by the FIVE raw categories, not collapsed -----------------------
# construction + manufacturing -> applied to ADDITIONS (one-time, per GW built)
# oem + extraction + refinery  -> applied to STOCK (ongoing, per GW installed)
flow_cats  <- c("construction", "manufacturing")
stock_cats <- c("oem", "extraction", "refinery")

jobs_flow <- additions_ts %>%
  inner_join(job_ef_long %>% filter(category %in% flow_cats),
             by = c("Region" = "region", "fuel")) %>%
  transmute(Model, Scenario, Region, Category, Year, category,
            jobs_thousands = add_GW * job_intensity / 1000)
jobs_stock <- stock_ts %>%
  inner_join(job_ef_long %>% filter(category %in% stock_cats),
             by = c("Region" = "region", "fuel")) %>%
  transmute(Model, Scenario, Region, Category, Year, category,
            jobs_thousands = stock_GW * job_intensity / 1000)

jobs_annual_cat <- bind_rows(jobs_flow, jobs_stock) %>%
  filter(!is.na(jobs_thousands)) %>%
  group_by(Model, Scenario, Region, Category, Year, category) %>%
  summarise(jobs_thousands = sum(jobs_thousands, na.rm = TRUE), .groups = "drop")

# ---- 5. cumulate to World, 2020-2100 window ----------------------------------
jobs_cum_world <- jobs_annual_cat %>%
  filter(Year >= START_YEAR, Year <= WINDOW_END) %>%
  group_by(Model, Scenario, category) %>%
  summarise(jobs_thousands = sum(jobs_thousands, na.rm = TRUE), .groups = "drop") %>%
  mutate(jobs_per_1k = jobs_thousands / pop2020_total)

# ---- 6. join to pathway classification, approaches A and C ------------------
read_pw <- function(id) {
  f <- file.path(MASTER_OUT_DIR, paste0("approach_", id), paste0("compass_pathway_tercile_", id, ".csv"))
  stopifnot(file.exists(f))
  read.csv(f, stringsAsFactors = FALSE) %>%
    select(Model, Scenario, Ambition, Pathway_excl) %>%
    filter(Pathway_excl %in% c("High-CDR", "High-RE")) %>%
    mutate(approach = id)
}
pw <- bind_rows(read_pw("A"), read_pw("C"))

j <- inner_join(jobs_cum_world, pw, by = c("Model", "Scenario"))

# ---- 7. summarise: mean +/- 95% CI per category x Pathway x Ambition x approach
ci95 <- function(x) { x <- x[!is.na(x)]; if (length(x) < 2) return(c(NA_real_, NA_real_))
  se <- sd(x) / sqrt(length(x)); m <- mean(x); c(m - 1.96*se, m + 1.96*se) }

summ <- j %>%
  group_by(approach, Ambition, category, Pathway_excl) %>%
  summarise(n = n(), mean_jobs_per_1k = mean(jobs_per_1k, na.rm = TRUE),
            lo = ci95(jobs_per_1k)[1], hi = ci95(jobs_per_1k)[2], .groups = "drop") %>%
  arrange(approach, Ambition, category, Pathway_excl)

cat("\n=== jobs per 1,000 pop, by RAW category x pathway x ambition (A & C) ===\n")
print(as.data.frame(summ), row.names = FALSE)

OUT <- file.path(MASTER_OUT_DIR, "jobs_category_breakdown.csv")
ok <- tryCatch({ write_csv(summ, OUT); TRUE },
               error = function(e) { write_csv(summ, "jobs_category_breakdown.csv"); FALSE })
if (!ok) OUT <- file.path(getwd(), "jobs_category_breakdown.csv")
cat("\nWROTE:", normalizePath(OUT, winslash = "/", mustWork = FALSE), "\n")
