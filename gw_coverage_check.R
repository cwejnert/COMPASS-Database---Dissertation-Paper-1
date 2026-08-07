# =============================================================================
# gw_coverage_check.R  —  RUN LOCALLY
#
# WHY: approaches K and L classify ambition from SCI's own GW0-GW3 tiers
#   (764 scenarios) instead of AR6 C1-C4 (947 scenarios) -- a different,
#   mostly-but-not-fully overlapping pool (GW3 draws mostly on C2/C3, only 17
#   of AR6's 200 C4 scenarios land in GW3; see warming_threshold_check.csv
#   crosstab). Before trusting K/L results, confirm those 764 scenarios
#   actually have data for every wellbeing outcome, not just a GW label.
#
# WHAT THIS CHECKS, per scenario in the GW0-GW3 pool:
#   1. deployment metrics  (Total CDR / Renewable Capacity -> classification)
#   2. Final Energy + Population, all 5 R10 regions            -> DLE gap/headcount
#   3. mortality (compass_mortality_r10.csv)                   -> PM2.5 deaths
#   4. capacity data for jobs (Capacity|Electricity|* )         -> energy jobs
#
# OUTPUT: gw_coverage_summary.csv (small -- attach or paste back)
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
})

COMPASS_DIR    <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/COMPASS"
MASTER_OUT_DIR <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_master"
MORT_OUT_DIR   <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_mortality"

META    <- file.path(COMPASS_DIR, "compass_r10_meta.csv")
INTERP  <- file.path(COMPASS_DIR, "compass_interp.rds")
.mort_candidates <- c(file.path(COMPASS_DIR, "compass_mortality_r10.csv"),
                      file.path(MORT_OUT_DIR, "compass_mortality_r10.csv"))
.mort_found <- .mort_candidates[file.exists(.mort_candidates)]
MORT_CSV <- if (length(.mort_found)) .mort_found[which.max(file.mtime(.mort_found))] else NA

stopifnot(file.exists(META), file.exists(INTERP))
regions_r10 <- c("R10AFRICA","R10CHINA+","R10EUROPE","R10INDIA+","R10NORTH_AM")

# ---- 1. build the GW0-GW3 candidate pool from metadata ----------------------
meta <- read.csv(META, stringsAsFactors = FALSE, check.names = FALSE)
names(meta)[tolower(names(meta)) == "model"]    <- "Model"
names(meta)[tolower(names(meta)) == "scenario"] <- "Scenario"
gw_col <- names(meta)[str_detect(names(meta), "SCI 2025 \\[Tier I\\]")][1]
stopifnot(!is.na(gw_col))

pool <- meta %>%
  transmute(Model, Scenario, gw = .data[[gw_col]],
            Ambition = case_when(gw %in% c("GW0","GW1","GW2") ~ "1.5C",
                                 gw == "GW3"                  ~ "2C",
                                 TRUE ~ NA_character_)) %>%
  filter(!is.na(Ambition)) %>%
  distinct(Model, Scenario, Ambition)
cat("GW0-GW3 candidate pool:", nrow(pool), "scenarios",
    sprintf("(%d at 1.5C, %d at 2C)\n", sum(pool$Ambition == "1.5C"), sum(pool$Ambition == "2C")))

# ---- 2. load interp once, check what each scenario actually has -------------
ci <- readRDS(INTERP)
has_var <- function(vars, region_filter = regions_r10) {
  ci %>% filter(Variable %in% vars, Region %in% region_filter, !is.na(Value)) %>%
    distinct(Model, Scenario)
}

dep_vars <- c("Total CDR", "Renewable Capacity",
              "Capacity|Electricity|Solar", "Capacity|Electricity|Wind",
              "Capacity|Electricity|Hydro", "Capacity|Electricity|Geothermal")
have_dep <- ci %>%
  filter(Variable %in% dep_vars, Region == "World", !is.na(Value), Value > 0) %>%
  distinct(Model, Scenario)

fe_ok  <- has_var("Final Energy")
pop_ok <- has_var("Population")
cap_ok <- has_var(c("Capacity|Electricity|Solar","Capacity|Electricity|Wind",
                    "Capacity|Electricity|Hydro","Capacity|Electricity|Nuclear",
                    "Capacity|Electricity|Biomass","Capacity|Electricity|Geothermal",
                    "Capacity|Electricity|Coal","Capacity|Electricity|Gas",
                    "Capacity|Electricity|Oil"))

# a scenario has FULL R10 coverage only if it has the variable in ALL 5 regions
full_region_coverage <- function(vars) {
  ci %>% filter(Variable %in% vars, Region %in% regions_r10, !is.na(Value)) %>%
    distinct(Model, Scenario, Region) %>%
    count(Model, Scenario) %>% filter(n == length(regions_r10)) %>%
    distinct(Model, Scenario)
}
fe_full  <- full_region_coverage("Final Energy")
pop_full <- full_region_coverage("Population")

mort_ok <- if (!is.na(MORT_CSV)) {
  m <- read_csv(MORT_CSV, show_col_types = FALSE)
  names(m)[tolower(names(m)) %in% c("model")]    <- "Model"
  names(m)[tolower(names(m)) %in% c("scenario")] <- "Scenario"
  m %>% distinct(Model, Scenario)
} else tibble(Model = character(), Scenario = character())

# ---- 3. tabulate coverage against the pool -----------------------------------
flag <- function(df) pool %>% semi_join(df, by = c("Model","Scenario")) %>% nrow()
n <- nrow(pool)
summary_tab <- tibble(
  check = c("deployment metrics (World, for classification)",
           "Final Energy, ALL 5 R10 regions (DLE)",
           "Population, ALL 5 R10 regions (DLE)",
           "any electricity capacity data (jobs)",
           "mortality (compass_mortality_r10.csv)"),
  n_covered = c(flag(have_dep), flag(fe_full), flag(pop_full), flag(cap_ok), flag(mort_ok)),
  n_pool = n
) %>% mutate(pct = round(100 * n_covered / n_pool))

cat("\n=== GW0-GW3 pool coverage, by outcome ===\n")
print(as.data.frame(summary_tab), row.names = FALSE)

# scenarios missing ANY of the four analysis-critical inputs
critical <- have_dep %>% inner_join(fe_full, by=c("Model","Scenario")) %>%
  inner_join(pop_full, by=c("Model","Scenario"))
missing_any <- pool %>% anti_join(critical, by = c("Model","Scenario"))
cat(sprintf("\nscenarios in the pool missing deployment+FE+population (can't be used at all): %d / %d (%.0f%%)\n",
            nrow(missing_any), n, 100*nrow(missing_any)/n))
if (nrow(missing_any) > 0) {
  cat("by ambition:\n")
  print(as.data.frame(missing_any %>% inner_join(pool, by=c("Model","Scenario")) %>%
    count(Ambition)), row.names = FALSE)
}

cat(sprintf("\nmortality coverage ceiling for the pool: %d / %d (%.0f%%)  ",
            flag(mort_ok), n, 100*flag(mort_ok)/n))
cat("(compare to the master's own MORTALITY COVERAGE CEILING diagnostic for A/C)\n")

OUT <- file.path(MASTER_OUT_DIR, "gw_coverage_summary.csv")
ok <- tryCatch({ write_csv(summary_tab, OUT); TRUE },
               error = function(e) { write_csv(summary_tab, "gw_coverage_summary.csv"); FALSE })
if (!ok) OUT <- file.path(getwd(), "gw_coverage_summary.csv")
cat("\nWROTE:", normalizePath(OUT, winslash = "/", mustWork = FALSE), "\n")
