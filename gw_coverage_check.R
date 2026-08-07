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
# A first run found real gaps in the GW pool: deployment 95%, Final Energy
# (all 5 R10 regions) 80%, Population (all 5 regions) 74%, capacity 77%,
# mortality 100%. The missing scenarios span nearly every model family
# (AIM, IMAGE, MESSAGE(ix), POLES, REMIND, WITCH, TIAM-ECN...), which looks
# like a general "many scenario-comparison vintages (ADVANCE/EMF33/CD-LINKS/
# ENGAGE) didn't report Final Energy/Population at full R10 resolution"
# pattern rather than something specific to SCI's GW3 tier. THIS VERSION runs
# the identical check against the AR6 C1-C4 pool (used by approaches A-J) so
# we can tell whether K/L's coverage is worse than what A/C already have, or
# just the same baseline COMPASS reporting gap everyone lives with.
#
# WHAT THIS CHECKS, per scenario in EACH pool (GW0-GW3 and AR6 C1-C4):
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

# ---- 1. build BOTH candidate pools from metadata -----------------------------
meta <- read.csv(META, stringsAsFactors = FALSE, check.names = FALSE)
names(meta)[tolower(names(meta)) == "model"]    <- "Model"
names(meta)[tolower(names(meta)) == "scenario"] <- "Scenario"
gw_col  <- names(meta)[str_detect(names(meta), "SCI 2025 \\[Tier I\\]")][1]
ar6_col <- names(meta)[str_detect(names(meta), "Climate Category\\|AR6 \\[ID\\]")][1]
stopifnot(!is.na(gw_col), !is.na(ar6_col))

pool_gw <- meta %>%
  transmute(Model, Scenario, gw = .data[[gw_col]],
            Ambition = case_when(gw %in% c("GW0","GW1","GW2") ~ "1.5C",
                                 gw == "GW3"                  ~ "2C",
                                 TRUE ~ NA_character_)) %>%
  filter(!is.na(Ambition)) %>% distinct(Model, Scenario, Ambition)

pool_ar6 <- meta %>%
  transmute(Model, Scenario, cat = .data[[ar6_col]],
            Ambition = case_when(cat %in% c("C1","C2") ~ "1.5C",
                                 cat %in% c("C3","C4") ~ "2C",
                                 TRUE ~ NA_character_)) %>%
  filter(!is.na(Ambition)) %>% distinct(Model, Scenario, Ambition)

cat("GW0-GW3 pool (K/L):  ", nrow(pool_gw), "scenarios",
    sprintf("(%d at 1.5C, %d at 2C)\n", sum(pool_gw$Ambition=="1.5C"), sum(pool_gw$Ambition=="2C")))
cat("AR6 C1-C4 pool (A-J):", nrow(pool_ar6), "scenarios",
    sprintf("(%d at 1.5C, %d at 2C)\n", sum(pool_ar6$Ambition=="1.5C"), sum(pool_ar6$Ambition=="2C")))

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

# ---- 3. tabulate coverage against EACH pool, side by side -------------------
flag <- function(pool, df) pool %>% semi_join(df, by = c("Model","Scenario")) %>% nrow()
coverage_tab <- function(pool, label) {
  n <- nrow(pool)
  tibble(
    pool = label,
    check = c("deployment metrics (World, for classification)",
             "Final Energy, ALL 5 R10 regions (DLE)",
             "Population, ALL 5 R10 regions (DLE)",
             "any electricity capacity data (jobs)",
             "mortality (compass_mortality_r10.csv)"),
    n_covered = c(flag(pool, have_dep), flag(pool, fe_full), flag(pool, pop_full),
                 flag(pool, cap_ok), flag(pool, mort_ok)),
    n_pool = n
  ) %>% mutate(pct = round(100 * n_covered / n_pool))
}
summary_tab <- bind_rows(coverage_tab(pool_gw, "GW0-GW3 (K/L)"),
                         coverage_tab(pool_ar6, "AR6 C1-C4 (A-J)"))

cat("\n=== coverage by outcome, GW pool vs AR6 pool (side by side) ===\n")
print(as.data.frame(summary_tab %>%
  select(check, pool, pct) %>%
  pivot_wider(names_from = pool, values_from = pct)), row.names = FALSE)
cat("\n(full detail:)\n")
print(as.data.frame(summary_tab), row.names = FALSE)

# scenarios missing ANY of the three analysis-critical inputs, per pool
critical <- have_dep %>% inner_join(fe_full, by=c("Model","Scenario")) %>%
  inner_join(pop_full, by=c("Model","Scenario"))
for (pl in list(list(pool_gw, "GW0-GW3 (K/L)"), list(pool_ar6, "AR6 C1-C4 (A-J)"))) {
  pool <- pl[[1]]; label <- pl[[2]]; n <- nrow(pool)
  missing_any <- pool %>% anti_join(critical, by = c("Model","Scenario"))
  cat(sprintf("\n%s: missing deployment+FE+population: %d / %d (%.0f%%)\n",
              label, nrow(missing_any), n, 100*nrow(missing_any)/n))
}

cat("\nREAD: if the AR6 pool's percentages are similar to the GW pool's, this is a\n",
    "  general COMPASS reporting gap that already affects approaches A-J, not\n",
    "  something specific to SCI's GW3 tier. If the AR6 pool is much better\n",
    "  covered, the GW pool is disproportionately drawing on incompletely-\n",
    "  reported scenario vintages and K/L's results should be read with that\n",
    "  in mind (smaller effective n, possible vintage-composition bias).\n", sep="")

OUT <- file.path(MASTER_OUT_DIR, "gw_coverage_summary.csv")
ok <- tryCatch({ write_csv(summary_tab, OUT); TRUE },
               error = function(e) { write_csv(summary_tab, "gw_coverage_summary.csv"); FALSE })
if (!ok) OUT <- file.path(getwd(), "gw_coverage_summary.csv")
cat("\nWROTE:", normalizePath(OUT, winslash = "/", mustWork = FALSE), "\n")
