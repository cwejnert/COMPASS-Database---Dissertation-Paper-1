# =============================================================================
# COMPASS Paper 1 -- engineered-CMT opposing-portfolio recut
#
# Reclassifies the current 2020--2050 scenario-level outcome release without
# recomputing outcome values. Jobs, DLE, and mortality are functions of a
# scenario's annual inputs, not of its pathway label; reusing the already-built
# annual outcome release is therefore numerically identical to an outcome rerun
# while avoiding a memory-intensive duplicate construction.
#
# Primary axis: Novel CDR + Fossil/industrial CCS, excluding Land-based CDR.
# Primary rule: top-tercile focal axis AND bottom-tercile opposing axis.
# Supplement: identical rule using Total CDR (all removals).
#
# This script intentionally does NOT recreate the published W6 cluster
# bootstrap: the model x scenario-family map is not locally available. It
# writes all inputs required for that bootstrap plus raw and within-IAM checks.
# =============================================================================

suppressPackageStartupMessages(library(tidyverse))

BASE_OUT <- Sys.getenv(
  "COMPASS_BASE_OUT",
  "C:/Users/camwe/Documents/Codex/2026-08-22/i-a/outputs/master_nh3infill_central"
)
OUT_DIR <- Sys.getenv(
  "COMPASS_ENGINEERED_CMT_OUT",
  "C:/Users/camwe/Documents/Codex/2026-08-22/i-a/outputs/engineered_cmt_primary_2020_2050"
)
RFASST_FILE <- Sys.getenv(
  "COMPASS_ENGINEERED_CMT_MORTALITY_FILE",
  "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_mortality/compass_mortality_r10_linked_or_world_fallback_base_emission_engineered_cmt_primary_nh3infill_central.csv"
)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

APPROACHES <- c("A", "C")
WORLD_REGION <- "Aggregated R10 regions"
OUTCOMES <- c("net_re_jobs_per_1k", "gap_GJ_pc", "headcount_pct",
              "mort_per_1k", "cumulative_deaths_mln")

first_finite <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) x[[1L]] else NA_real_
}

normalise_id <- function(x) {
  x <- as.character(x)
  x <- gsub("<U+00B0>", "°", x, fixed = TRUE)
  gsub("�", "°", x, fixed = TRUE)
}

build_labels <- function(approach, axis = c("engineered_cmt", "total_cdr")) {
  axis <- match.arg(axis)
  cdr <- read_csv(file.path(BASE_OUT, paste0("approach_", approach),
                            paste0("compass_cdr_cumulative_", approach, ".csv")),
                  show_col_types = FALSE)
  scenario_set <- read_csv(file.path(BASE_OUT, paste0("approach_", approach),
                                     paste0("compass_scenario_set_", approach, ".csv")),
                           show_col_types = FALSE)

  metrics <- cdr %>%
    filter(Variable %in% c("Novel CDR", "Fossil CCS", "Total CDR",
                           "Renewable Capacity")) %>%
    group_by(Model, Scenario, Category, Variable) %>%
    summarise(value = sum(Total_Value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Variable, values_from = value, values_fill = 0) %>%
    rename(total_cdr = `Total CDR`, renewables = `Renewable Capacity`,
           novel_cdr = `Novel CDR`, fossil_ccs = `Fossil CCS`) %>%
    mutate(engineered_cmt = novel_cdr + fossil_ccs)

  axis_col <- if (axis == "engineered_cmt") "engineered_cmt" else "total_cdr"
  scenario_set %>%
    select(Model, Scenario, Category, Ambition) %>%
    distinct() %>%
    inner_join(metrics, by = c("Model", "Scenario", "Category")) %>%
    group_by(Ambition) %>%
    mutate(
      cmt_low = quantile(.data[[axis_col]], 1 / 3, na.rm = TRUE),
      cmt_high = quantile(.data[[axis_col]], 2 / 3, na.rm = TRUE),
      re_low = quantile(renewables, 1 / 3, na.rm = TRUE),
      re_high = quantile(renewables, 2 / 3, na.rm = TRUE),
      Pathway = case_when(
        .data[[axis_col]] >= cmt_high & renewables <= re_low ~ "High-CMT / Low-RE",
        renewables >= re_high & .data[[axis_col]] <= cmt_low ~ "High-RE / Low-CMT",
        TRUE ~ NA_character_
      ),
      cmt_axis = if_else(axis == "engineered_cmt", "Engineered CMT", "Total CDR"),
      portfolio_rule = "top-tercile focal axis + bottom-tercile opposing axis"
    ) %>%
    ungroup()
}

collapse_outcomes <- function(approach) {
  raw <- read_csv(file.path(BASE_OUT, paste0("approach_", approach),
                            paste0("compass_master_dataset_", approach, ".csv")),
                  show_col_types = FALSE)
  raw %>%
    group_by(Model, Scenario, Category, Ambition, Region) %>%
    summarise(across(all_of(OUTCOMES), first_finite), .groups = "drop")
}

summarise_contrast <- function(data, group_cols) {
  parts <- if (length(group_cols))
    split(data, interaction(data[, group_cols], drop = TRUE, lex.order = TRUE)) else
    list(all_rows = data)
  map_dfr(parts, function(part) {
    map_dfr(OUTCOMES, function(outcome) {
      cmt <- part %>% filter(Pathway == "High-CMT / Low-RE") %>% pull(.data[[outcome]])
      re <- part %>% filter(Pathway == "High-RE / Low-CMT") %>% pull(.data[[outcome]])
      cmt <- cmt[is.finite(cmt)]; re <- re[is.finite(re)]
      tibble(
        !!!part[1, group_cols, drop = FALSE], outcome = outcome,
        n_cmt = length(cmt), n_re = length(re),
        median_cmt = if (length(cmt)) median(cmt) else NA_real_,
        median_re = if (length(re)) median(re) else NA_real_,
        re_minus_cmt = if (length(cmt) && length(re)) median(re) - median(cmt) else NA_real_
      )
    })
  })
}

all_labels <- list(); all_outcomes <- list(); all_results <- list()
for (approach in APPROACHES) {
  labels <- build_labels(approach, "engineered_cmt") %>% mutate(approach = approach)
  outcomes <- collapse_outcomes(approach) %>% mutate(approach = approach)
  joined <- outcomes %>% inner_join(labels, by = c("Model", "Scenario", "Category", "Ambition", "approach")) %>%
    filter(!is.na(Pathway))
  all_labels[[approach]] <- labels
  all_outcomes[[approach]] <- joined
  all_results[[approach]] <- summarise_contrast(joined,
    c("approach", "cmt_axis", "portfolio_rule", "Region", "Ambition"))
}

labels_engineered <- bind_rows(all_labels)
outcomes_engineered <- bind_rows(all_outcomes)
results_engineered <- bind_rows(all_results)

# Replace the older mortality column in the scenario-level master release with
# the newly rerun engineered-CMT RFASST output. The interval is the trapezoidal
# 2020--2050 area under annual PM2.5 deaths, matching the master convention.
if (!file.exists(RFASST_FILE)) stop("Fresh engineered-CMT RFASST file missing: ", RFASST_FILE)
population <- read_csv(file.path(BASE_OUT, "approach_A", "compass_master_dataset_A.csv"),
                       show_col_types = FALSE) %>%
  filter(Region != WORLD_REGION, is.finite(pop_mln)) %>%
  group_by(Region) %>% summarise(pop_mln = median(pop_mln), .groups = "drop")
world_pop <- sum(population$pop_mln)
mortality_r10 <- read_csv(RFASST_FILE, show_col_types = FALSE) %>%
  transmute(Model = normalise_id(model), Scenario = normalise_id(scenario),
            Region = r10_region, Year = as.integer(year), deaths = deaths_pm25) %>%
  filter(Year >= 2020L, Year <= 2050L) %>%
  group_by(Model, Scenario, Region) %>%
  summarise(cumulative_deaths_mln = {
    ord <- order(Year); yr <- Year[ord]; dth <- deaths[ord]
    if (length(dth) < 2L || anyNA(dth)) NA_real_
    else sum((dth[-1L] + dth[-length(dth)]) / 2 * diff(yr)) / 1e6
  }, .groups = "drop") %>%
  left_join(population, by = "Region") %>%
  mutate(mort_per_1k = cumulative_deaths_mln / pop_mln * 1000)
mortality_world <- mortality_r10 %>%
  group_by(Model, Scenario) %>%
  summarise(n_regions = n_distinct(Region),
            cumulative_deaths_mln = if (all(is.na(cumulative_deaths_mln))) NA_real_
                                    else sum(cumulative_deaths_mln, na.rm = TRUE),
            .groups = "drop") %>%
  filter(n_regions == nrow(population)) %>%
  transmute(Model, Scenario, Region = WORLD_REGION, cumulative_deaths_mln,
            mort_per_1k = cumulative_deaths_mln / world_pop * 1000)
mortality_fresh <- bind_rows(
  mortality_r10 %>% select(Model, Scenario, Region, cumulative_deaths_mln, mort_per_1k),
  mortality_world
)
refresh_mortality <- function(data) {
  data %>%
  select(-any_of(c("mort_per_1k", "cumulative_deaths_mln"))) %>%
  mutate(.model_key = normalise_id(Model), .scenario_key = normalise_id(Scenario)) %>%
  left_join(mortality_fresh %>%
              rename(.model_key = Model, .scenario_key = Scenario),
            by = c(".model_key", ".scenario_key", "Region")) %>%
  select(-.model_key, -.scenario_key)
}
outcomes_engineered <- refresh_mortality(outcomes_engineered)
results_engineered <- summarise_contrast(outcomes_engineered,
  c("approach", "cmt_axis", "portfolio_rule", "Region", "Ambition"))

write_csv(labels_engineered, file.path(OUT_DIR, "engineered_cmt_primary_labels.csv"))
write_csv(
  labels_engineered %>% filter(!is.na(Pathway)) %>%
    distinct(Model, Scenario) %>% arrange(Model, Scenario),
  file.path(OUT_DIR, "engineered_cmt_primary_rfasst_targets.csv")
)
write_csv(results_engineered, file.path(OUT_DIR, "engineered_cmt_primary_outcome_medians.csv"))

separation <- labels_engineered %>% filter(!is.na(Pathway)) %>%
  group_by(approach, cmt_axis, Ambition, Pathway) %>%
  summarise(n = n(), median_engineered_cmt = median(engineered_cmt),
            median_total_cdr = median(total_cdr), median_renewables = median(renewables),
            .groups = "drop")
write_csv(separation, file.path(OUT_DIR, "engineered_cmt_primary_separation.csv"))

coverage <- outcomes_engineered %>% filter(Region == WORLD_REGION) %>%
  group_by(approach, Ambition, Pathway) %>%
  summarise(n_pathways = n(), mortality_available = sum(is.finite(mort_per_1k)),
            mortality_coverage_pct = 100 * mortality_available / n_pathways,
            .groups = "drop")
write_csv(coverage, file.path(OUT_DIR, "engineered_cmt_primary_mortality_coverage.csv"))

outcome_coverage <- outcomes_engineered %>%
  pivot_longer(all_of(OUTCOMES), names_to = "outcome", values_to = "value") %>%
  group_by(approach, Region, Ambition, Pathway, outcome) %>%
  summarise(n_pathways = n(), n_available = sum(is.finite(value)),
            coverage_pct = 100 * n_available / n_pathways, .groups = "drop")
write_csv(outcome_coverage, file.path(OUT_DIR, "engineered_cmt_primary_outcome_coverage.csv"))

# Equal-weight within-IAM diagnostic. A model enters only when it has both
# opposing portfolio arms in the relevant region/ambition comparison.
within <- outcomes_engineered %>%
  group_by(approach, Model, cmt_axis, portfolio_rule, Region, Ambition) %>%
  group_modify(~ summarise_contrast(.x, character())) %>%
  ungroup() %>%
  filter(n_cmt > 0, n_re > 0) %>%
  group_by(approach, cmt_axis, portfolio_rule, Region, Ambition, outcome) %>%
  summarise(n_models = n(), median_model_effect = median(re_minus_cmt, na.rm = TRUE),
            n_re_higher = sum(re_minus_cmt > 0, na.rm = TRUE),
            n_re_lower = sum(re_minus_cmt < 0, na.rm = TRUE), .groups = "drop")
write_csv(within, file.path(OUT_DIR, "engineered_cmt_primary_within_model.csv"))

# Identical opposing-portfolio rule for the all-removals supplement.
all_removals <- map_dfr(APPROACHES, function(approach) {
  labels <- build_labels(approach, "total_cdr") %>% mutate(approach = approach)
  outcomes <- collapse_outcomes(approach) %>% mutate(approach = approach)
  joined <- outcomes %>% inner_join(labels, by = c("Model", "Scenario", "Category", "Ambition", "approach")) %>%
    refresh_mortality() %>%
    filter(!is.na(Pathway))
  summarise_contrast(joined, c("approach", "cmt_axis", "portfolio_rule", "Region", "Ambition"))
})
write_csv(all_removals, file.path(OUT_DIR, "all_removals_opposing_portfolio_supplement.csv"))

writeLines(c(
  "release=engineered-cmt-opposing-terciles_2020-2050",
  "axis=Novel CDR + Fossil/industrial CCS; land-based CDR excluded",
  "rule=top-tercile focal axis + bottom-tercile opposing axis, within ambition",
  "outcomes=reused from classification-independent scenario-level 2020-2050 release",
  paste0("mortality=fresh RFASST release: ", RFASST_FILE),
  "bootstrap=not rerun: original model x scenario-family cluster map is unavailable locally"
), file.path(OUT_DIR, "RUN_MANIFEST.txt"))

message("Wrote engineered-CMT recut to: ", OUT_DIR)
