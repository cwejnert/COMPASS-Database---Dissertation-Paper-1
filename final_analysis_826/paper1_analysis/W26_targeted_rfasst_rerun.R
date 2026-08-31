# =============================================================================
# W26 - Targeted RFASST rerun for classified all-CDR mortality gaps
#
# Purpose
#   1. Reconstruct the exact raw-emissions keys for the 70 classified scenarios
#      identified by W23 as having all five PM2.5 precursors at World-equivalent
#      coverage but no mortality output.
#   2. Run the 63 direct-R10 candidates with the paper's reporting-complete
#      settings.
#   3. Run the 7 World-fallback candidates separately as a sensitivity.
#   4. Merge only validated direct-R10 results into a new, non-destructive
#      all-CDR mortality release and write complete provenance/coverage tables.
#
# This script never overwrites the existing final mortality release.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
})

options(warn = 1)

BASE_DIR <- normalizePath(
  Sys.getenv("COMPASS_FINAL_ANALYSIS_DIR", "."),
  winslash = "/", mustWork = TRUE
)
RUN_ROOT <- Sys.getenv(
  "COMPASS_TARGETED_RFASST_OUT_DIR",
  file.path(BASE_DIR, "final_outcomes/mortality_472/rfasst_targeted_70")
)
DIRECT_OUT <- file.path(RUN_ROOT, "direct_R10_primary")
WORLD_OUT <- file.path(RUN_ROOT, "world_fallback_sensitivity")
RUNTIME_DATA <- file.path(RUN_ROOT, "runtime_data")
RUN_WORK <- file.path(RUN_ROOT, "rfasst_work")

CORE_SCRIPT <- file.path(
  BASE_DIR, "analysis_scripts/COMPASS_rfasst_full_allR10.R"
)
SOURCE_DATA <- Sys.getenv(
  "COMPASS_TARGETED_RFASST_SOURCE_DATA",
  file.path(RUN_ROOT, "runtime_data")
)
MISSING_FILE <- file.path(
  BASE_DIR, "final_outcomes/mortality_472/audit_inputs",
  "mortality_remaining_missing_scenarios.csv"
)
LAND_PRIMARY_FILE <- file.path(
  BASE_DIR, "LAND_PRIMARY.rds"
)

BASE_ENGINEERED_FILE <- file.path(
  BASE_DIR, "../mortality",
  "compass_mortality_r10_as_reported_base_emission_engineeredcmt_century_reported_pminterp.csv"
)
BASE_GAP80_FILE <- file.path(
  BASE_DIR, "final_outcomes/mortality_472/audit_inputs",
  "compass_mortality_r10_as_reported_base_emission_allcdr_gap80_directR10.csv"
)

DIRECT_LABEL <- "targeted70_directR10_pminterp"
WORLD_LABEL <- "targeted70_worldFallback_sensitivity_pminterp"
DIRECT_RESULT_FILE <- file.path(
  DIRECT_OUT,
  paste0("compass_mortality_r10_as_reported_base_emission_", DIRECT_LABEL, ".csv")
)
WORLD_RESULT_FILE <- file.path(
  WORLD_OUT,
  paste0("compass_mortality_r10_as_reported_base_emission_", WORLD_LABEL, ".csv")
)

R10 <- c(
  "R10AFRICA", "R10CHINA+", "R10EUROPE", "R10INDIA+", "R10LATIN_AM",
  "R10MIDDLE_EAST", "R10NORTH_AM", "R10PAC_OECD", "R10REF_ECON", "R10REST_ASIA"
)
WORLD <- "Aggregated R10 regions"

dir.create(RUN_ROOT, recursive = TRUE, showWarnings = FALSE)
dir.create(DIRECT_OUT, recursive = TRUE, showWarnings = FALSE)
dir.create(WORLD_OUT, recursive = TRUE, showWarnings = FALSE)
dir.create(RUNTIME_DATA, recursive = TRUE, showWarnings = FALSE)
dir.create(RUN_WORK, recursive = TRUE, showWarnings = FALSE)

required_files <- c(
  CORE_SCRIPT, MISSING_FILE, LAND_PRIMARY_FILE,
  BASE_ENGINEERED_FILE, BASE_GAP80_FILE,
  file.path(SOURCE_DATA, "compass_emissions_raw.csv"),
  file.path(SOURCE_DATA, "compass_r10_meta.csv")
)
missing_required <- required_files[!file.exists(required_files)]
if (length(missing_required)) {
  stop("Required input file(s) missing:\n", paste(missing_required, collapse = "\n"))
}

# The historical data contain a mixture of literal degree signs, replacement
# characters and the printed string <U+00B0>. Use a stable ASCII join key for
# audit joins, then pass the exact raw-emissions spellings to RFASST.
norm_key <- function(x) {
  x <- gsub("<U\\+00B0>", "<DEG>", x)
  x <- gsub("<DEG>", "<DEG>", x, fixed = TRUE)
  x <- gsub("[^ -~]", "<DEG>", x)
  x
}

link_or_copy <- function(source, destination) {
  if (file.exists(destination)) {
    same_size <- isTRUE(file.info(source)$size == file.info(destination)$size)
    if (!same_size) stop("Staged input exists with the wrong size: ", destination)
    return(invisible(destination))
  }
  linked <- suppressWarnings(file.link(source, destination))
  if (!isTRUE(linked)) {
    copied <- file.copy(source, destination, overwrite = FALSE, copy.date = TRUE)
    if (!isTRUE(copied)) stop("Could not stage input: ", source)
  }
  invisible(destination)
}

link_or_copy(
  file.path(SOURCE_DATA, "compass_emissions_raw.csv"),
  file.path(RUNTIME_DATA, "compass_emissions_raw.csv")
)
link_or_copy(
  file.path(SOURCE_DATA, "compass_r10_meta.csv"),
  file.path(RUNTIME_DATA, "compass_r10_meta.csv")
)

cat("\n=== W26 targeted RFASST rerun ===\n")
cat("Core script:", CORE_SCRIPT, "\n")
cat("Run root:   ", RUN_ROOT, "\n\n")

# -----------------------------------------------------------------------------
# 1. Rebuild exact target keys
# -----------------------------------------------------------------------------

screened <- read_csv(MISSING_FILE, show_col_types = FALSE) %>%
  filter(has_all_raw_pm_precursors %in% TRUE) %>%
  mutate(
    model_key = norm_key(Model),
    scenario_key = norm_key(Scenario),
    target_partition = if_else(
      any_world_fallback %in% TRUE,
      "World-fallback sensitivity",
      "Direct-R10 primary"
    )
  )

if (nrow(screened) != 70L) {
  stop("Expected 70 screened mortality gaps, found ", nrow(screened), ". Rerun W23 first.")
}

raw_keys <- read_csv(
  file.path(RUNTIME_DATA, "compass_emissions_raw.csv"),
  col_select = c(model, scenario),
  col_types = cols(model = col_character(), scenario = col_character()),
  progress = FALSE
) %>%
  distinct(model, scenario) %>%
  mutate(model_key = norm_key(model), scenario_key = norm_key(scenario))

ambiguous_raw_keys <- raw_keys %>%
  count(model_key, scenario_key, name = "n_exact_spellings") %>%
  filter(n_exact_spellings != 1L)
if (nrow(ambiguous_raw_keys)) {
  write_csv(ambiguous_raw_keys, file.path(RUN_ROOT, "ambiguous_normalized_raw_keys.csv"))
  stop("Normalized keys map to multiple raw spellings; see ambiguous_normalized_raw_keys.csv")
}

targets <- screened %>%
  left_join(
    raw_keys %>% select(model_key, scenario_key, raw_model = model, raw_scenario = scenario),
    by = c("model_key", "scenario_key")
  )

if (anyNA(targets$raw_model) || anyNA(targets$raw_scenario)) {
  write_csv(
    targets %>% filter(is.na(raw_model) | is.na(raw_scenario)),
    file.path(RUN_ROOT, "unmatched_target_keys.csv")
  )
  stop("At least one screened target did not match the raw-emissions keys.")
}
if (n_distinct(paste(targets$raw_model, targets$raw_scenario, sep = "||")) != 70L) {
  stop("The screened list did not resolve to 70 unique raw-emissions scenarios.")
}

direct_targets <- targets %>% filter(target_partition == "Direct-R10 primary")
world_targets <- targets %>% filter(target_partition == "World-fallback sensitivity")
if (nrow(direct_targets) != 63L || nrow(world_targets) != 7L) {
  stop(
    "Expected 63 direct-R10 and 7 World-fallback targets; found ",
    nrow(direct_targets), " and ", nrow(world_targets), "."
  )
}

write_csv(
  targets %>%
    select(
      Model_display, Scenario_display, amb, Pathway, family, project,
      coverage_status, target_partition, raw_model, raw_scenario
    ),
  file.path(RUN_ROOT, "targeted_70_manifest.csv")
)
write_csv(
  direct_targets %>% transmute(model = raw_model, scenario = raw_scenario),
  file.path(RUN_ROOT, "targeted_63_direct_R10.csv")
)
write_csv(
  world_targets %>% transmute(model = raw_model, scenario = raw_scenario),
  file.path(RUN_ROOT, "targeted_7_world_fallback.csv")
)

cat("Target manifest rebuilt with exact raw keys:\n")
cat("  Direct-R10 primary:        ", nrow(direct_targets), "\n")
cat("  World-fallback sensitivity:", nrow(world_targets), "\n\n")

# -----------------------------------------------------------------------------
# 2. Run the existing production RFASST core in two isolated passes
# -----------------------------------------------------------------------------

run_core_pass <- function(target_file, output_dir, run_label, allow_world_fallback) {
  pass_work <- file.path(RUN_WORK, run_label)
  dir.create(pass_work, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  env_values <- c(
    COMPASS_DATA_DIR = RUNTIME_DATA,
    COMPASS_MORTALITY_OUT_DIR = output_dir,
    COMPASS_MORTALITY_SCENARIO_SET = target_file,
    COMPASS_NH3_MODE = "as_reported",
    COMPASS_SPATIAL_ALLOCATION = "base_emission",
    COMPASS_COMPLETE_PM_ONLY = "true",
    COMPASS_REQUIRE_CREDIBLE_NH3 = "true",
    COMPASS_INTERPOLATE_SHORT_PM_GAPS = "true",
    COMPASS_MAX_INTERP_MISSING_NODES = "2",
    COMPASS_ALLOW_WORLD_ONLY_DISAGG = ifelse(allow_world_fallback, "true", "false"),
    COMPASS_RUN_O3 = "false",
    COMPASS_MORTALITY_RUN_LABEL = run_label
  )

  env_names <- names(env_values)
  old_values <- Sys.getenv(env_names, unset = NA_character_)
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    Sys.unsetenv(env_names)
    restore <- !is.na(old_values)
    if (any(restore)) do.call(Sys.setenv, as.list(old_values[restore]))
  }, add = TRUE)

  do.call(Sys.setenv, as.list(env_values))
  setwd(pass_work)
  cat("\n--- Starting RFASST pass:", run_label, "---\n")
  source(CORE_SCRIPT, local = new.env(parent = globalenv()), echo = FALSE)
  cat("--- Completed RFASST pass:", run_label, "---\n")
}

run_core_pass(
  file.path(RUN_ROOT, "targeted_63_direct_R10.csv"),
  DIRECT_OUT,
  DIRECT_LABEL,
  allow_world_fallback = FALSE
)

run_core_pass(
  file.path(RUN_ROOT, "targeted_7_world_fallback.csv"),
  WORLD_OUT,
  WORLD_LABEL,
  allow_world_fallback = TRUE
)

# -----------------------------------------------------------------------------
# 3. Validate scenario completeness and merge only direct-R10 successes
# -----------------------------------------------------------------------------

read_run_result <- function(path, source, priority) {
  if (!file.exists(path)) {
    return(tibble(
      model = character(), scenario = character(), Category = character(),
      r10_region = character(), year = integer(), deaths_pm25 = numeric(),
      deaths_o3 = numeric(), deaths_total = numeric(), source = character(),
      priority = integer()
    ))
  }
  read_csv(path, show_col_types = FALSE) %>%
    mutate(source = source, priority = priority)
}

validate_complete_scenarios <- function(x) {
  x %>%
    filter(r10_region %in% R10, year >= 2020L, year <= 2100L) %>%
    group_by(model, scenario) %>%
    summarise(
      n_regions = n_distinct(r10_region),
      n_years = n_distinct(year),
      n_cells = n_distinct(paste(r10_region, year)),
      all_pm25_finite = all(is.finite(deaths_pm25)),
      complete = n_regions == 10L & n_years == 9L & n_cells == 90L & all_pm25_finite,
      .groups = "drop"
    )
}

direct_raw <- read_run_result(DIRECT_RESULT_FILE, "targeted direct-R10 rerun", 3L)
world_raw <- read_run_result(WORLD_RESULT_FILE, "World-fallback sensitivity", 3L)
direct_validation <- validate_complete_scenarios(direct_raw)
world_validation <- validate_complete_scenarios(world_raw)

write_csv(direct_validation, file.path(RUN_ROOT, "direct_R10_validation.csv"))
write_csv(world_validation, file.path(RUN_ROOT, "world_fallback_validation.csv"))

direct_success <- direct_validation %>% filter(complete) %>% select(model, scenario)
world_success <- world_validation %>% filter(complete) %>% select(model, scenario)

direct_errors <- if (file.exists(file.path(DIRECT_OUT, "compass_mortality_errors.csv"))) {
  read_csv(file.path(DIRECT_OUT, "compass_mortality_errors.csv"), show_col_types = FALSE)
} else tibble(model = character(), scenario = character(), error = character())
world_errors <- if (file.exists(file.path(WORLD_OUT, "compass_mortality_errors.csv"))) {
  read_csv(file.path(WORLD_OUT, "compass_mortality_errors.csv"), show_col_types = FALSE)
} else tibble(model = character(), scenario = character(), error = character())

target_status <- targets %>%
  mutate(raw_pair = paste(raw_model, raw_scenario, sep = "||")) %>%
  left_join(
    direct_success %>% mutate(raw_pair = paste(model, scenario, sep = "||"), direct_success = TRUE) %>%
      select(raw_pair, direct_success),
    by = "raw_pair"
  ) %>%
  left_join(
    world_success %>% mutate(raw_pair = paste(model, scenario, sep = "||"), world_success = TRUE) %>%
      select(raw_pair, world_success),
    by = "raw_pair"
  ) %>%
  left_join(
    direct_errors %>% mutate(raw_pair = paste(model, scenario, sep = "||")) %>%
      select(raw_pair, direct_error = error),
    by = "raw_pair"
  ) %>%
  left_join(
    world_errors %>% mutate(raw_pair = paste(model, scenario, sep = "||")) %>%
      select(raw_pair, world_error = error),
    by = "raw_pair"
  ) %>%
  mutate(
    direct_success = coalesce(direct_success, FALSE),
    world_success = coalesce(world_success, FALSE),
    rerun_status = case_when(
      target_partition == "Direct-R10 primary" & direct_success ~ "Primary direct-R10 success",
      target_partition == "Direct-R10 primary" ~ "Primary direct-R10 failed quality gate",
      target_partition == "World-fallback sensitivity" & world_success ~ "World-fallback sensitivity success",
      TRUE ~ "World-fallback sensitivity failed quality gate"
    ),
    failure_reason = coalesce(direct_error, world_error)
  )

write_csv(
  target_status %>%
    select(
      Model_display, Scenario_display, amb, Pathway, family, project,
      target_partition, rerun_status, failure_reason, raw_model, raw_scenario
    ),
  file.path(RUN_ROOT, "targeted_70_run_status.csv")
)

base_engineered <- read_run_result(BASE_ENGINEERED_FILE, "original engineered-target run", 1L)
base_gap80 <- read_run_result(BASE_GAP80_FILE, "original all-CDR gap-80 run", 2L)
direct_complete_rows <- direct_raw %>%
  semi_join(direct_success, by = c("model", "scenario"))

merged_r10 <- bind_rows(base_engineered, base_gap80, direct_complete_rows) %>%
  mutate(model_key = norm_key(model), scenario_key = norm_key(scenario)) %>%
  arrange(model_key, scenario_key, r10_region, year, desc(priority)) %>%
  distinct(model_key, scenario_key, r10_region, year, .keep_all = TRUE) %>%
  select(-model_key, -scenario_key, -priority)

MERGED_R10_FILE <- file.path(
  RUN_ROOT,
  "compass_mortality_r10_allcdr_reporting_complete_targeted70_merged.csv"
)
write_csv(merged_r10, MERGED_R10_FILE)

integrate_decadal <- function(year, value) {
  keep <- year >= 2020L & year <= 2100L
  year <- year[keep]
  value <- value[keep]
  ord <- order(year)
  year <- year[ord]
  value <- value[ord]
  if (length(unique(year)) != 9L || any(!is.finite(value))) return(NA_real_)
  sum(diff(year) * (head(value, -1L) + tail(value, -1L)) / 2)
}

r10_values <- merged_r10 %>%
  filter(r10_region %in% R10, year >= 2020L, year <= 2100L) %>%
  group_by(model, scenario, r10_region) %>%
  summarise(
    cumulative_pm25_deaths = integrate_decadal(year, deaths_pm25),
    .groups = "drop"
  ) %>%
  filter(is.finite(cumulative_pm25_deaths)) %>%
  rename(Region = r10_region)

world_values <- merged_r10 %>%
  filter(r10_region %in% R10, year >= 2020L, year <= 2100L) %>%
  group_by(model, scenario, year) %>%
  summarise(
    n_regions = n_distinct(r10_region),
    deaths_pm25 = if_else(n_regions == 10L, sum(deaths_pm25), NA_real_),
    .groups = "drop"
  ) %>%
  group_by(model, scenario) %>%
  summarise(
    cumulative_pm25_deaths = integrate_decadal(year, deaths_pm25),
    .groups = "drop"
  ) %>%
  filter(is.finite(cumulative_pm25_deaths)) %>%
  mutate(Region = WORLD)

mortality_values <- bind_rows(r10_values, world_values) %>%
  transmute(
    model_key = norm_key(model), scenario_key = norm_key(scenario),
    Region, cumulative_pm25_deaths,
    cumulative_pm25_deaths_mln = cumulative_pm25_deaths / 1e6
  )

land_primary <- readRDS(LAND_PRIMARY_FILE)
labels <- land_primary$labels_land %>%
  filter(axis == "with land", approach == "A") %>%
  transmute(
    approach, Model, Scenario, Ambition = amb, Pathway,
    cmt_axis = "all CDR",
    portfolio_rule = "top tercile focal / not top tercile opposing",
    model_key = norm_key(Model), scenario_key = norm_key(Scenario)
  )

scenario_values <- labels %>%
  inner_join(mortality_values, by = c("model_key", "scenario_key")) %>%
  select(-model_key, -scenario_key)

SCENARIO_VALUES_FILE <- file.path(
  RUN_ROOT,
  "mortality_allcdr_reporting_complete_scenario_values_targeted70_2020_2100.csv"
)
write_csv(scenario_values, SCENARIO_VALUES_FILE)

coverage <- labels %>%
  distinct(approach, Model, Scenario, Ambition, Pathway, model_key, scenario_key) %>%
  mutate(classified = 1L) %>%
  left_join(
    mortality_values %>% filter(Region == WORLD) %>%
      distinct(model_key, scenario_key) %>% mutate(mortality_available = 1L),
    by = c("model_key", "scenario_key")
  ) %>%
  group_by(Ambition, Pathway) %>%
  summarise(
    classified = n(),
    mortality_available = sum(coalesce(mortality_available, 0L)),
    mortality_missing = classified - mortality_available,
    coverage_pct = 100 * mortality_available / classified,
    .groups = "drop"
  )
write_csv(coverage, file.path(RUN_ROOT, "mortality_coverage_after_targeted_rerun.csv"))

run_summary <- tibble(
  metric = c(
    "screened targets", "direct-R10 targets", "World-fallback targets",
    "validated direct-R10 successes", "validated World-fallback sensitivity successes",
    "direct-R10 failures", "World-fallback sensitivity failures",
    "classified scenarios with primary mortality after merge"
  ),
  value = c(
    nrow(targets), nrow(direct_targets), nrow(world_targets),
    nrow(direct_success), nrow(world_success),
    nrow(direct_targets) - nrow(direct_success),
    nrow(world_targets) - nrow(world_success),
    n_distinct(paste(
      scenario_values$Model[scenario_values$Region == WORLD],
      scenario_values$Scenario[scenario_values$Region == WORLD], sep = "||"
    ))
  )
)
write_csv(run_summary, file.path(RUN_ROOT, "targeted_rfasst_run_summary.csv"))

cat("\n=== Targeted rerun summary ===\n")
print(run_summary, n = Inf)
cat("\nPrimary coverage after merge:\n")
print(coverage, n = Inf)
cat("\nPrimary merged R10 output:\n  ", MERGED_R10_FILE, "\n", sep = "")
cat("Primary scenario values:\n  ", SCENARIO_VALUES_FILE, "\n", sep = "")
cat("Full target status:\n  ", file.path(RUN_ROOT, "targeted_70_run_status.csv"), "\n", sep = "")
