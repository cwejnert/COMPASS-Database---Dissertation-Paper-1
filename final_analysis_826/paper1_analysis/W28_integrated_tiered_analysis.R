# =============================================================================
# W28 - Integrated tiered analysis for COMPASS Paper 1
#
# Tier 1: quantitative screen
#   pooled, ambition split/no split, Full/SCI-vetted, within-family,
#   equal-family, leave-one-family-out, World and all R10 regions.
#
# Tier 2: targeted project audit
#   model-version x project x ambition matched cells for every outcome,
#   selected when Tier 1 identifies influence, disagreement or sparse support.
#
# Tier 3: scenario-specific audit triggers
#   sole/near-sole family comparators and sparse matched-project contrasts,
#   plus dominant project clusters. This does not audit every scenario.
#
# Mortality uses the validated 472-scenario RFASST release from W26/W27.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(ggplot2)
})

options(width = 240, warn = 1)

BASE_DIR <- normalizePath(
  Sys.getenv("COMPASS_FINAL_ANALYSIS_DIR", "."),
  winslash = "/", mustWork = TRUE
)
OUT <- Sys.getenv(
  "COMPASS_TIERED_OUT_DIR",
  file.path(BASE_DIR, "final_outcomes/tiered_analysis")
)
MORT_FRAME_FILE <- Sys.getenv(
  "COMPASS_MORTALITY_472_FRAME",
  file.path(BASE_DIR, "final_outcomes/mortality_472",
            "W27_mortality_analysis_frame.csv")
)
LP_FILE <- Sys.getenv(
  "COMPASS_LAND_PRIMARY",
  file.path(BASE_DIR, "LAND_PRIMARY.rds")
)
CLASS_AUDIT_FILE <- file.path(
  BASE_DIR, "audits/REMIND_WITCH_scenario_classification_audit.csv"
)
JOBS_FILE <- Sys.getenv(
  "COMPASS_REVISED_JOBS_FILE",
  file.path(BASE_DIR, "final_outcomes/jobs_revision",
            "compass_jobs_cumulative_2020_2100.rds")
)
DLE_FILE <- Sys.getenv(
  "COMPASS_REVISED_DLE_FILE",
  file.path(BASE_DIR, "final_outcomes/dle_sensitivity",
            "compass_dle_scenario_values_2020_2100.csv")
)

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

R10 <- c(
  "R10AFRICA", "R10CHINA+", "R10EUROPE", "R10INDIA+", "R10LATIN_AM",
  "R10MIDDLE_EAST", "R10NORTH_AM", "R10PAC_OECD", "R10REF_ECON", "R10REST_ASIA"
)
WORLD <- "Aggregated R10 regions"
ALL_REGIONS <- c(WORLD, R10)
DATABASES <- c("Full", "SCI-vetted")
AMBITIONS <- c("All ambitions", "1.5C", "2C")
MIN_ROBUST_N <- 3L
LARGE_INFLUENCE_SHIFT_PP <- 20

OUTCOMES <- c(
  total_energy_jobyears_per_1k = "Employment",
  gap_GJ_pc = "DLE gap",
  headcount_pct = "Deprived share",
  mortality_million_deaths = "PM2.5 mortality"
)
OUTCOME_ORDER <- unname(OUTCOMES)
LOWER_IS_BETTER <- c("gap_GJ_pc", "headcount_pct", "mortality_million_deaths")
UNITS <- c(
  total_energy_jobyears_per_1k = "total energy-sector job-years per 1,000 people",
  gap_GJ_pc = "GJ per capita of cumulative DLE gap",
  headcount_pct = "population-year-weighted mean % below the DLE threshold",
  mortality_million_deaths = "million cumulative PM2.5-attributable deaths"
)

required <- c(LP_FILE, MORT_FRAME_FILE, JOBS_FILE, DLE_FILE)
if (any(!file.exists(required))) {
  stop("Missing required input(s):\n", paste(required[!file.exists(required)], collapse = "\n"))
}

norm_key <- function(x) {
  x <- gsub("<U\\+00B0>", "<DEG>", x)
  gsub("[^ -~]", "<DEG>", x)
}

family_name <- function(model) sub("[ /-].*$", "", model)

project_tag <- function(scenario) {
  case_when(
    grepl("^ENGAGE", scenario) ~ "ENGAGE",
    grepl("^NAVIGATE", scenario) ~ "NAVIGATE",
    grepl("^COMMIT", scenario) ~ "COMMIT",
    grepl("^NGFS Phase 2", scenario) ~ "NGFS Phase 2",
    grepl("^NGFS Phase 5", scenario) ~ "NGFS Phase 5",
    grepl("^SHAPE", scenario) ~ "SHAPE",
    grepl("^SSP2021", scenario) ~ "SSP2021",
    grepl("^GEI", scenario) ~ "GEI",
    grepl("^COVID", scenario) ~ "COVID",
    grepl("^GEO7", scenario) ~ "GEO7",
    grepl("^PEP", scenario) ~ "PEP",
    grepl("^CEMICS", scenario) ~ "CEMICS",
    grepl("^EMF", scenario) ~ sub("-.*$", "", scenario),
    TRUE ~ sub("[-/].*$", "", scenario)
  )
}

safe_median <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) median(x) else NA_real_
}

cliffs_delta <- function(cdr, re) {
  cdr <- cdr[is.finite(cdr)]
  re <- re[is.finite(re)]
  if (!length(cdr) || !length(re)) return(NA_real_)
  comparisons <- outer(re, cdr, "-")
  (sum(comparisons > 0) - sum(comparisons < 0)) / (length(cdr) * length(re))
}

direction_label <- function(effect) {
  case_when(
    effect > 0 ~ "High-RE",
    effect < 0 ~ "High-CDR",
    effect == 0 ~ "Tie",
    TRUE ~ NA_character_
  )
}

direction_sign <- function(outcome) if_else(outcome %in% LOWER_IS_BETTER, -1, 1)

LP <- readRDS(LP_FILE)

# Revised employment values: construction, manufacturing, O&M, extraction,
# refining and decommissioning across all technologies. World is defined only
# when all ten R10 regional values exist for a scenario.
pop_r10 <- readRDS(file.path(
  BASE_DIR, "master_outputs/approach_A/compass_master_dataset_A.rds"
)) %>%
  filter(Region %in% R10) %>%
  distinct(Region, pop_mln)
stopifnot(nrow(pop_r10) == length(R10), !anyNA(pop_r10$pop_mln))
pop_world <- sum(pop_r10$pop_mln)

jobs_regional <- readRDS(JOBS_FILE) %>%
  filter(Region %in% R10) %>%
  distinct(Model, Scenario, Region, .keep_all = TRUE) %>%
  left_join(pop_r10, by = "Region") %>%
  transmute(
    model_key = norm_key(Model), scenario_key = norm_key(Scenario), Region,
    total_energy_jobyears_per_1k = jobs_EnergyTotal / pop_mln
  )
jobs_world <- readRDS(JOBS_FILE) %>%
  filter(Region %in% R10) %>%
  group_by(Model, Scenario) %>%
  summarise(
    n_regions = n_distinct(Region),
    jobs_EnergyTotal = if (n_regions == length(R10))
      sum(jobs_EnergyTotal, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  transmute(
    model_key = norm_key(Model), scenario_key = norm_key(Scenario),
    Region = WORLD,
    total_energy_jobyears_per_1k = jobs_EnergyTotal / pop_world
  )
jobs_values <- bind_rows(jobs_regional, jobs_world)

# Final DESIRE-based deprivation values on the same 2020-2100 horizon as jobs
# and mortality. The archived approach master files contain the historical
# 2020-2050 DLE release and are retained only for population denominators and
# provenance.
dle_values <- read_csv(DLE_FILE, show_col_types = FALSE) %>%
  filter(is_baseline, Region %in% ALL_REGIONS) %>%
  transmute(
    database,
    model_key = norm_key(Model), scenario_key = norm_key(Scenario), Region,
    gap_GJ_pc, headcount_pct
  ) %>%
  distinct(database, model_key, scenario_key, Region, .keep_all = TRUE)

build_nonmortality <- function(ap) {
  database_name <- ifelse(ap == "A", "Full", "SCI-vetted")
  labels <- LP$labels_land %>%
    filter(axis == "with land", .data$approach == .env$ap) %>%
    transmute(
      Model, Scenario,
      model_key = norm_key(Model), scenario_key = norm_key(Scenario),
      Pathway = recode(Pathway, `High-CMT` = "High-CDR"),
      amb
    ) %>%
    distinct(model_key, scenario_key, .keep_all = TRUE)

  dle_database <- dle_values %>%
    filter(.data$database == .env$database_name) %>%
    select(-database)

  dle_frame <- dle_database %>%
    inner_join(labels, by = c("model_key", "scenario_key"), relationship = "many-to-one") %>%
    mutate(
      database = database_name,
      family = family_name(Model),
      project = project_tag(Scenario)
    ) %>%
    select(database, Model, Scenario, Pathway, amb, family, project, Region,
           gap_GJ_pc, headcount_pct) %>%
    pivot_longer(
      cols = c(gap_GJ_pc, headcount_pct),
      names_to = "outcome", values_to = "value"
    )

  jobs_frame <- jobs_values %>%
    inner_join(labels, by = c("model_key", "scenario_key"), relationship = "many-to-one") %>%
    mutate(
      database = database_name,
      family = family_name(Model),
      project = project_tag(Scenario)
    ) %>%
    transmute(
      database, Model, Scenario, Pathway, amb, family, project, Region,
      outcome = "total_energy_jobyears_per_1k",
      value = total_energy_jobyears_per_1k
    )

  bind_rows(jobs_frame, dle_frame)
}

cat("Building unified all-outcome analysis frame...\n")
nonmortality <- bind_rows(build_nonmortality("A"), build_nonmortality("C"))

mortality <- read_csv(MORT_FRAME_FILE, show_col_types = FALSE) %>%
  transmute(
    database, Model, Scenario,
    Pathway = recode(Pathway, `High-CMT` = "High-CDR"),
    amb, family, project, Region,
    outcome = "mortality_million_deaths",
    value = mortality_million_deaths
  )

analysis_frame <- bind_rows(nonmortality, mortality) %>%
  mutate(
    outcome_label = unname(OUTCOMES[outcome]),
    outcome_unit = unname(UNITS[outcome]),
    lower_is_better = outcome %in% LOWER_IS_BETTER
  ) %>%
  distinct(database, Model, Scenario, Region, outcome, .keep_all = TRUE)

write_csv(analysis_frame, file.path(OUT, "T0_unified_analysis_frame.csv"))

views <- bind_rows(
  analysis_frame %>% mutate(ambition_view = "All ambitions"),
  analysis_frame %>% mutate(ambition_view = amb)
) %>%
  mutate(
    database = factor(database, levels = DATABASES),
    ambition_view = factor(ambition_view, levels = AMBITIONS),
    Region = factor(Region, levels = ALL_REGIONS),
    outcome_label = factor(outcome_label, levels = OUTCOME_ORDER)
  )

# =============================================================================
# TIER 1 - QUANTITATIVE SCREEN
# =============================================================================

pooled <- views %>%
  group_by(database, ambition_view, Region, outcome, outcome_label, outcome_unit, lower_is_better) %>%
  summarise(
    n_cdr = sum(Pathway == "High-CDR" & is.finite(value)),
    n_re = sum(Pathway == "High-RE" & is.finite(value)),
    median_cdr = safe_median(value[Pathway == "High-CDR"]),
    median_re = safe_median(value[Pathway == "High-RE"]),
    cliffs_delta_raw = cliffs_delta(value[Pathway == "High-CDR"], value[Pathway == "High-RE"]),
    .groups = "drop"
  ) %>%
  mutate(
    sign_code = if_else(lower_is_better, -1, 1),
    direction_coded_difference = sign_code * (median_re - median_cdr),
    relative_effect_pct = 100 * direction_coded_difference / abs(median_cdr),
    direction_coded_cliffs_delta = sign_code * cliffs_delta_raw,
    favours = direction_label(direction_coded_difference)
  ) %>%
  select(-sign_code)

family_effects <- views %>%
  group_by(database, ambition_view, Region, outcome, outcome_label, outcome_unit,
           lower_is_better, family) %>%
  summarise(
    n_cdr = sum(Pathway == "High-CDR" & is.finite(value)),
    n_re = sum(Pathway == "High-RE" & is.finite(value)),
    median_cdr = safe_median(value[Pathway == "High-CDR"]),
    median_re = safe_median(value[Pathway == "High-RE"]),
    cliffs_delta_raw = cliffs_delta(value[Pathway == "High-CDR"], value[Pathway == "High-RE"]),
    .groups = "drop"
  ) %>%
  mutate(
    comparable = n_cdr >= 1L & n_re >= 1L,
    robust = n_cdr >= MIN_ROBUST_N & n_re >= MIN_ROBUST_N,
    sign_code = if_else(lower_is_better, -1, 1),
    direction_coded_difference = if_else(comparable, sign_code * (median_re - median_cdr), NA_real_),
    relative_effect_pct = 100 * direction_coded_difference / abs(median_cdr),
    direction_coded_cliffs_delta = if_else(comparable, sign_code * cliffs_delta_raw, NA_real_),
    favours = direction_label(direction_coded_difference)
  ) %>%
  select(-sign_code) %>%
  left_join(
    pooled %>% select(database, ambition_view, Region, outcome,
                      pooled_effect_pct = relative_effect_pct, pooled_favours = favours),
    by = c("database", "ambition_view", "Region", "outcome")
  ) %>%
  mutate(agrees_with_pooled = comparable & sign(relative_effect_pct) == sign(pooled_effect_pct))

equal_family <- family_effects %>%
  filter(comparable) %>%
  group_by(database, ambition_view, Region, outcome, outcome_label) %>%
  summarise(
    n_informative_families = n(),
    n_robust_families = sum(robust),
    equal_family_mean_pct = mean(relative_effect_pct),
    equal_family_median_pct = median(relative_effect_pct),
    robust_equal_family_mean_pct = if (any(robust)) mean(relative_effect_pct[robust]) else NA_real_,
    robust_equal_family_median_pct = if (any(robust)) median(relative_effect_pct[robust]) else NA_real_,
    families_favour_re = sum(relative_effect_pct > 0),
    families_favour_cdr = sum(relative_effect_pct < 0),
    robust_families_favour_re = sum(relative_effect_pct > 0 & robust),
    robust_families_favour_cdr = sum(relative_effect_pct < 0 & robust),
    .groups = "drop"
  )

calc_loo <- function(data) {
  map_dfr(sort(unique(data$family)), function(excluded_family) {
    z <- data %>% filter(family != excluded_family)
    cdr <- z$value[z$Pathway == "High-CDR"]
    re <- z$value[z$Pathway == "High-RE"]
    med_cdr <- safe_median(cdr)
    med_re <- safe_median(re)
    lower <- unique(z$lower_is_better)
    sign_code <- ifelse(length(lower) && lower[1], -1, 1)
    effect <- sign_code * (med_re - med_cdr)
    tibble(
      excluded_family,
      n_cdr = sum(is.finite(cdr)), n_re = sum(is.finite(re)),
      median_cdr = med_cdr, median_re = med_re,
      direction_coded_difference = effect,
      loo_effect_pct = 100 * effect / abs(med_cdr),
      favours = direction_label(effect)
    )
  })
}

loo <- views %>%
  group_by(database, ambition_view, Region, outcome, outcome_label) %>%
  group_modify(~calc_loo(.x)) %>%
  ungroup() %>%
  left_join(
    pooled %>% select(database, ambition_view, Region, outcome,
                      pooled_effect_pct = relative_effect_pct, pooled_favours = favours),
    by = c("database", "ambition_view", "Region", "outcome")
  ) %>%
  mutate(
    direction_flip = sign(loo_effect_pct) != sign(pooled_effect_pct),
    influence_shift_pct_points = abs(loo_effect_pct - pooled_effect_pct)
  )

influence <- loo %>%
  group_by(database, ambition_view, Region, outcome, outcome_label,
           pooled_effect_pct, pooled_favours) %>%
  summarise(
    loo_min_pct = if (all(is.na(loo_effect_pct))) NA_real_ else min(loo_effect_pct, na.rm = TRUE),
    loo_max_pct = if (all(is.na(loo_effect_pct))) NA_real_ else max(loo_effect_pct, na.rm = TRUE),
    n_families_tested = sum(is.finite(loo_effect_pct)),
    n_direction_flips = sum(direction_flip, na.rm = TRUE),
    max_influence_shift_pct_points = max(influence_shift_pct_points, na.rm = TRUE),
    most_negative_exclusion = if (all(is.na(loo_effect_pct))) NA_character_ else excluded_family[which.min(loo_effect_pct)],
    most_positive_exclusion = if (all(is.na(loo_effect_pct))) NA_character_ else excluded_family[which.max(loo_effect_pct)],
    .groups = "drop"
  )

validation <- pooled %>%
  select(database, ambition_view, Region, outcome, outcome_label, outcome_unit,
         n_cdr, n_re, median_cdr, median_re,
         pooled_effect = direction_coded_difference,
         pooled_effect_pct = relative_effect_pct, pooled_favours = favours) %>%
  left_join(equal_family, by = c("database", "ambition_view", "Region", "outcome", "outcome_label")) %>%
  left_join(influence, by = c("database", "ambition_view", "Region", "outcome", "outcome_label",
                              "pooled_effect_pct", "pooled_favours")) %>%
  mutate(
    pooled_equal_family_agree = sign(pooled_effect_pct) == sign(equal_family_mean_pct),
    leave_one_out_stable = n_direction_flips == 0,
    magnitude_composition_sensitive = abs(pooled_effect_pct - equal_family_mean_pct) >=
      LARGE_INFLUENCE_SHIFT_PP,
    structural_class = case_when(
      is.na(n_informative_families) | n_informative_families < 2 ~
        "Not identified across model families",
      !pooled_equal_family_agree ~ "Composition-sensitive",
      !leave_one_out_stable ~ "Dominant-family sensitive",
      robust_families_favour_re > 0 & robust_families_favour_cdr > 0 ~
        "Directionally stable but structurally heterogeneous",
      TRUE ~ "Cross-model aligned"
    ),
    evidence_class = case_when(
      outcome == "mortality_million_deaths" &
        structural_class %in% c("Composition-sensitive", "Dominant-family sensitive") ~
        "Coverage-qualified; composition-sensitive association",
      outcome == "mortality_million_deaths" &
        structural_class == "Not identified across model families" ~
        "Coverage-qualified; not identified across model families",
      outcome == "mortality_million_deaths" ~
        paste0("Coverage-qualified; ", structural_class),
      structural_class == "Not identified across model families" ~
        "Not identified across model families",
      structural_class %in% c("Composition-sensitive", "Dominant-family sensitive") ~
        "Database association only; model-composition sensitive",
      magnitude_composition_sensitive ~
        "Direction supported; magnitude composition-sensitive",
      structural_class == "Directionally stable but structurally heterogeneous" ~
        "Direction supported; structural heterogeneity remains",
      structural_class == "Cross-model aligned" ~ "Cross-model supported",
      TRUE ~ structural_class
    )
  )

# World-first screening table used to trigger Tier 2.
world_screen <- validation %>%
  filter(Region == WORLD) %>%
  arrange(database, ambition_view, outcome_label)

# Direction counts are descriptive pooled summaries, not validation scores.
# The 60-cell regional headline keeps the deck's original convention:
# 10 R10 regions x 3 distinct findings (employment, DLE, mortality) x 2 ambitions.
direction_scorecard <- pooled %>%
  filter(database == "Full", ambition_view %in% c("1.5C", "2C")) %>%
  mutate(
    geography = if_else(Region == WORLD, "World", "R10 regions"),
    distinct_finding = outcome != "headcount_pct"
  ) %>%
  group_by(geography, ambition_view, outcome, outcome_label, distinct_finding) %>%
  summarise(
    cells_favour_re = sum(favours == "High-RE"),
    cells_favour_cdr = sum(favours == "High-CDR"),
    cells_total = n(),
    .groups = "drop"
  )

direction_headlines <- bind_rows(
  direction_scorecard %>%
    filter(geography == "World") %>%
    summarise(
      headline = "World: all four displayed outcomes x two ambitions",
      cells_favour_re = sum(cells_favour_re),
      cells_favour_cdr = sum(cells_favour_cdr),
      cells_total = sum(cells_total)
    ),
  direction_scorecard %>%
    filter(geography == "R10 regions", distinct_finding) %>%
    summarise(
      headline = "R10: three distinct findings x two ambitions",
      cells_favour_re = sum(cells_favour_re),
      cells_favour_cdr = sum(cells_favour_cdr),
      cells_total = sum(cells_total)
    )
)

# =============================================================================
# TIER 2 - MATCHED MODEL-VERSION x PROJECT x AMBITION AUDIT
# =============================================================================

matched_cells <- analysis_frame %>%
  group_by(database, Region, amb, outcome, outcome_label, outcome_unit, lower_is_better,
           family, Model, project) %>%
  summarise(
    n_cdr = sum(Pathway == "High-CDR" & is.finite(value)),
    n_re = sum(Pathway == "High-RE" & is.finite(value)),
    median_cdr = safe_median(value[Pathway == "High-CDR"]),
    median_re = safe_median(value[Pathway == "High-RE"]),
    .groups = "drop"
  ) %>%
  mutate(
    comparable = n_cdr >= 1L & n_re >= 1L,
    robust = n_cdr >= MIN_ROBUST_N & n_re >= MIN_ROBUST_N,
    sign_code = if_else(lower_is_better, -1, 1),
    direction_coded_difference = if_else(comparable, sign_code * (median_re - median_cdr), NA_real_),
    relative_effect_pct = 100 * direction_coded_difference / abs(median_cdr),
    favours = direction_label(direction_coded_difference),
    matched_unit = paste(Model, project, amb, sep = " | ")
  ) %>%
  select(-sign_code)

matched_views <- bind_rows(
  matched_cells %>% mutate(ambition_view = "All ambitions"),
  matched_cells %>% mutate(ambition_view = amb)
) %>%
  mutate(ambition_view = factor(ambition_view, levels = AMBITIONS))

matched_summary <- matched_views %>%
  filter(comparable) %>%
  group_by(database, ambition_view, Region, outcome, outcome_label) %>%
  summarise(
    n_comparable_cells = n(),
    n_robust_cells = sum(robust),
    matched_cell_mean_pct = mean(relative_effect_pct),
    matched_cell_median_pct = median(relative_effect_pct),
    robust_matched_cell_mean_pct = if (any(robust)) mean(relative_effect_pct[robust]) else NA_real_,
    robust_matched_cell_median_pct = if (any(robust)) median(relative_effect_pct[robust]) else NA_real_,
    cells_favour_re = sum(relative_effect_pct > 0),
    cells_favour_cdr = sum(relative_effect_pct < 0),
    robust_cells_favour_re = sum(relative_effect_pct > 0 & robust),
    robust_cells_favour_cdr = sum(relative_effect_pct < 0 & robust),
    total_cdr_support = sum(n_cdr), total_re_support = sum(n_re),
    .groups = "drop"
  )

targeted_families <- loo %>%
  filter(Region == WORLD) %>%
  left_join(
    family_effects %>%
      filter(Region == WORLD) %>%
      select(database, ambition_view, Region, outcome, family,
             family_n_cdr = n_cdr, family_n_re = n_re,
             family_effect_pct = relative_effect_pct,
             family_favours = favours, family_robust = robust,
             family_agrees_with_pooled = agrees_with_pooled),
    by = c("database", "ambition_view", "Region", "outcome", "excluded_family" = "family")
  ) %>%
  rename(family = excluded_family) %>%
  mutate(
    targeted = direction_flip |
      influence_shift_pct_points >= LARGE_INFLUENCE_SHIFT_PP |
      (family_robust & !coalesce(family_agrees_with_pooled, TRUE)),
    target_reason = case_when(
      direction_flip & family_robust & !coalesce(family_agrees_with_pooled, TRUE) ~
        "Pooled direction flips when excluded; robust family also disagrees",
      direction_flip ~ "Pooled direction flips when family is excluded",
      family_robust & !coalesce(family_agrees_with_pooled, TRUE) ~
        "Robust family direction disagrees with pooled",
      influence_shift_pct_points >= LARGE_INFLUENCE_SHIFT_PP ~
        "Large leave-one-family-out magnitude shift",
      TRUE ~ "Not targeted"
    )
  ) %>%
  arrange(desc(targeted), database, ambition_view, outcome_label,
          desc(direction_flip), desc(influence_shift_pct_points))

targeted_project_cells <- matched_views %>%
  filter(Region == WORLD) %>%
  inner_join(
    targeted_families %>%
      filter(targeted) %>%
      distinct(database, ambition_view, outcome, family, target_reason),
    by = c("database", "ambition_view", "outcome", "family")
  ) %>%
  arrange(database, ambition_view, outcome_label, family, desc(comparable), desc(robust), project)

# Project composition is outcome-specific because reporting coverage differs.
project_composition <- analysis_frame %>%
  filter(is.finite(value)) %>%
  group_by(database, amb, Region, outcome, outcome_label, family, Model, project, Pathway) %>%
  summarise(n_scenarios = n_distinct(paste(Model, Scenario, sep = "||")), .groups = "drop")

# =============================================================================
# TIER 3 - TARGETED PROJECT/SCENARIO TRIGGERS
# =============================================================================

# Minority-arm family scenarios are the highest-priority scenario-specific
# audit when a family has only one or two observations in one arm.
family_support <- analysis_frame %>%
  filter(Region == WORLD, is.finite(value)) %>%
  group_by(database, amb, outcome, outcome_label, family, Pathway) %>%
  summarise(n_arm = n_distinct(paste(Model, Scenario, sep = "||")), .groups = "drop") %>%
  pivot_wider(names_from = Pathway, values_from = n_arm, values_fill = 0,
              names_prefix = "n_") %>%
  rename(n_cdr = `n_High-CDR`, n_re = `n_High-RE`) %>%
  mutate(minority_arm = if_else(n_cdr <= n_re, "High-CDR", "High-RE"),
         minority_n = pmin(n_cdr, n_re))

scenario_triggers <- analysis_frame %>%
  filter(Region == WORLD, is.finite(value)) %>%
  inner_join(family_support, by = c("database", "amb", "outcome", "outcome_label", "family")) %>%
  filter(minority_n <= 2L, Pathway == minority_arm) %>%
  transmute(
    audit_priority = if_else(minority_n == 1L, 1L, 2L),
    trigger_type = if_else(minority_n == 1L,
                           "Sole family-arm comparator",
                           "Two-scenario family-arm comparator"),
    database, amb, outcome, outcome_label, family, Model, project, Scenario, Pathway,
    family_n_cdr = n_cdr, family_n_re = n_re,
    recommended_audit =
      "Inspect classification position, project design, bundled assumptions and outcome mechanism"
  ) %>%
  distinct()

# Sparse matched cells are project-level triggers; robust cells are retained as
# positive validation evidence rather than scenario-audit targets.
sparse_matched_triggers <- matched_cells %>%
  filter(Region == WORLD, comparable, !robust) %>%
  transmute(
    audit_priority = 2L,
    trigger_type = "Sparse matched model-project-ambition contrast",
    database, amb, outcome, outcome_label, family, Model, project,
    n_cdr, n_re, relative_effect_pct, favours,
    recommended_audit =
      "Read project protocol and inspect the small number of arm-defining scenarios before interpreting direction"
  )

# Dominant project clusters are handled at project level rather than listing
# every member scenario for line-by-line audit.
dominant_projects <- project_composition %>%
  filter(Region == WORLD) %>%
  group_by(database, amb, outcome, outcome_label, family, Pathway) %>%
  mutate(
    family_arm_total = sum(n_scenarios),
    project_share_pct = 100 * n_scenarios / family_arm_total
  ) %>%
  ungroup() %>%
  filter(n_scenarios >= 10L, project_share_pct >= 25) %>%
  transmute(
    audit_priority = 2L,
    trigger_type = "Dominant project cluster",
    database, amb, outcome, outcome_label, family, Model, project, Pathway,
    n_scenarios, family_arm_total, project_share_pct,
    recommended_audit =
      "Treat the project ensemble as a clustered design; audit shared assumptions rather than counting scenarios as independent confirmations"
  )

if (file.exists(CLASS_AUDIT_FILE)) {
  boundary_context <- read_csv(CLASS_AUDIT_FILE, show_col_types = FALSE)
  scenario_triggers <- scenario_triggers %>%
    left_join(
      boundary_context %>%
        transmute(
          Model, Scenario,
          cdr_metric = cdr_all, re_metric = renewables,
          cdr_cutoff, re_cutoff, cdr_ratio_to_cutoff, re_ratio_to_cutoff,
          classification_state
        ),
      by = c("Model", "Scenario")
    )
}

# =============================================================================
# FINAL WORLD SYNTHESIS AND OUTPUTS
# =============================================================================

world_synthesis <- world_screen %>%
  left_join(
    matched_summary %>% filter(Region == WORLD),
    by = c("database", "ambition_view", "Region", "outcome", "outcome_label")
  ) %>%
  mutate(
    matched_direction = direction_label(matched_cell_mean_pct),
    pooled_matched_agree = sign(pooled_effect_pct) == sign(matched_cell_mean_pct),
    final_tiered_conclusion = case_when(
      outcome == "total_energy_jobyears_per_1k" & database == "Full" & ambition_view == "All ambitions" ~
        "Employment direction supported; pooled magnitude is composition-sensitive",
      outcome %in% c("gap_GJ_pc", "headcount_pct") &
        database == "Full" & ambition_view == "All ambitions" ~
        "Deprivation is a database association driven by model/final-energy composition",
      outcome == "mortality_million_deaths" &
        database == "Full" & ambition_view == "All ambitions" ~
        "Mortality is coverage-qualified and composition-sensitive; matched evidence leans High-RE",
      TRUE ~ evidence_class
    )
  )

write_csv(pooled, file.path(OUT, "T1_pooled_world_r10.csv"))
write_csv(family_effects, file.path(OUT, "T1_within_family_world_r10.csv"))
write_csv(equal_family, file.path(OUT, "T1_equal_family_world_r10.csv"))
write_csv(loo, file.path(OUT, "T1_leave_one_family_out_world_r10.csv"))
write_csv(validation, file.path(OUT, "T1_validation_world_r10.csv"))
write_csv(world_screen, file.path(OUT, "T1_world_screen.csv"))
write_csv(direction_scorecard, file.path(OUT, "T1_direction_scorecard.csv"))
write_csv(direction_headlines, file.path(OUT, "T1_direction_headlines.csv"))
write_csv(matched_cells, file.path(OUT, "T2_matched_model_project_ambition_cells.csv"))
write_csv(matched_summary, file.path(OUT, "T2_matched_model_project_ambition_summary.csv"))
write_csv(targeted_families, file.path(OUT, "T2_targeted_family_register.csv"))
write_csv(targeted_project_cells, file.path(OUT, "T2_targeted_project_cells.csv"))
write_csv(project_composition, file.path(OUT, "T2_project_arm_composition.csv"))
write_csv(scenario_triggers, file.path(OUT, "T3_scenario_specific_triggers.csv"))
write_csv(sparse_matched_triggers, file.path(OUT, "T3_sparse_matched_project_triggers.csv"))
write_csv(dominant_projects, file.path(OUT, "T3_dominant_project_clusters.csv"))
write_csv(world_synthesis, file.path(OUT, "TIERED_WORLD_SYNTHESIS.csv"))

# QA invariants
full_counts <- analysis_frame %>%
  filter(database == "Full", Region == WORLD) %>%
  count(outcome, name = "observed_scenarios")
qa_checks <- tibble(
  check = c(
    "Unified frame has no duplicate database-scenario-region-outcome rows",
    "Every pooled specification has both pathway arms",
    "Tier 1 covers 2 databases x 3 ambition views x 11 regions x 4 outcomes",
    "Every comparable family effect is finite",
    "Every comparable matched cell is finite",
    "Validated Full mortality sample contains 472 scenarios",
    "Employment, DLE gap, deprived share and mortality all appear in World synthesis"
  ),
  passed = c(
    !anyDuplicated(analysis_frame[c("database", "Model", "Scenario", "Region", "outcome")]),
    all(pooled$n_cdr > 0 & pooled$n_re > 0),
    nrow(pooled) == length(DATABASES) * length(AMBITIONS) * length(ALL_REGIONS) * length(OUTCOMES),
    family_effects %>% filter(comparable) %>%
      summarise(ok = all(is.finite(median_cdr), is.finite(median_re), is.finite(relative_effect_pct))) %>% pull(ok),
    matched_cells %>% filter(comparable) %>%
      summarise(ok = all(n_cdr >= 1L, n_re >= 1L, is.finite(relative_effect_pct))) %>% pull(ok),
    full_counts %>% filter(outcome == "mortality_million_deaths") %>% pull(observed_scenarios) == 472L,
    n_distinct(world_synthesis$outcome) == length(OUTCOMES)
  )
)
write_csv(qa_checks, file.path(OUT, "TIERED_QA_checks.csv"))
if (!all(qa_checks$passed)) {
  print(qa_checks, n = Inf)
  stop("At least one integrated tiered-analysis QA invariant failed.")
}

# =============================================================================
# FIGURES
# =============================================================================

theme_compass <- theme_minimal(base_family = "Arial", base_size = 12) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", color = "#142A3D", size = 10),
    axis.title = element_blank(),
    axis.text = element_text(color = "#293746"),
    plot.title = element_text(face = "bold", size = 19, color = "#142A3D"),
    plot.subtitle = element_text(size = 11, color = "#5C6F82"),
    plot.caption = element_text(size = 9, color = "#5C6F82"),
    legend.position = "bottom",
    plot.margin = margin(12, 20, 10, 18)
  )

all_families <- sort(unique(family_effects$family))
world_cells <- bind_rows(
  pooled %>%
    filter(Region == WORLD) %>%
    transmute(database, ambition_view, outcome_label, family = "POOLED",
              relative_effect_pct, n_cdr, n_re, robust = TRUE),
  family_effects %>%
    filter(Region == WORLD) %>%
    transmute(database, ambition_view, outcome_label, family,
              relative_effect_pct, n_cdr, n_re, robust)
) %>%
  complete(
    database = factor(DATABASES, levels = DATABASES),
    ambition_view = factor(AMBITIONS, levels = AMBITIONS),
    outcome_label = factor(OUTCOME_ORDER, levels = OUTCOME_ORDER),
    family = c("POOLED", all_families)
  ) %>%
  mutate(
    family = factor(family, levels = c("POOLED", all_families)),
    label = if_else(
      is.finite(relative_effect_pct),
      paste0(sprintf("%+.0f", relative_effect_pct), "\n", n_cdr, "/", n_re,
             if_else(!coalesce(robust, FALSE) & family != "POOLED", "*", "")),
      if_else(!is.na(n_cdr) | !is.na(n_re),
              paste0("-\n", coalesce(n_cdr, 0L), "/", coalesce(n_re, 0L)), "")
    )
  )

p_tier1 <- ggplot(
  world_cells,
  aes(family, outcome_label, fill = pmax(-100, pmin(100, relative_effect_pct)))
) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = label), size = 2.25, fontface = "bold", color = "#142A3D", lineheight = 0.85) +
  facet_grid(database ~ ambition_view) +
  scale_fill_gradient2(
    low = "#C56D42", mid = "#F2F3F3", high = "#16859C", midpoint = 0,
    limits = c(-100, 100), na.value = "#E6EAED",
    breaks = c(-100, 0, 100), labels = c("CDR 100%", "0", "RE 100%")
  ) +
  labs(
    title = "Tier 1: pooled and within-family evidence across all outcomes",
    subtitle = "Direction-coded median difference (%); second line is n High-CDR / n High-RE",
    caption = "Teal favours High-RE; orange favours High-CDR. * fewer than 3 scenarios in either arm. Color saturates at +/-100%.",
    fill = "Median difference"
  ) +
  theme_compass +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 8), axis.text.y = element_text(size = 9))
ggsave(file.path(OUT, "T1_world_all_outcomes_family_magnitude_counts.png"), p_tier1,
       width = 18, height = 11, dpi = 240, bg = "white")

regional_full <- pooled %>%
  filter(database == "Full", ambition_view == "All ambitions") %>%
  mutate(
    Region = factor(Region, levels = rev(ALL_REGIONS)),
    outcome_label = factor(outcome_label, levels = OUTCOME_ORDER),
    label = paste0(sprintf("%+.0f", relative_effect_pct), "\n", n_cdr, "/", n_re)
  )
p_regional <- ggplot(
  regional_full,
  aes(outcome_label, Region, fill = pmax(-100, pmin(100, relative_effect_pct)))
) +
  geom_tile(color = "white", linewidth = 0.9) +
  geom_text(aes(label = label), size = 2.7, fontface = "bold", color = "#142A3D", lineheight = 0.9) +
  scale_fill_gradient2(
    low = "#C56D42", mid = "#F2F3F3", high = "#16859C", midpoint = 0,
    limits = c(-100, 100), na.value = "#E6EAED",
    breaks = c(-100, 0, 100), labels = c("CDR 100%", "0", "RE 100%")
  ) +
  labs(
    title = "Tier 1 regional screen: Full database, ambitions pooled",
    subtitle = "Direction-coded pooled median difference (%); second line is n High-CDR / n High-RE",
    caption = "Mortality uses the validated 472-scenario RFASST release. Pacific OECD remains descriptive because of composition concerns.",
    fill = "Median difference"
  ) +
  theme_compass + theme(axis.text.x = element_text(angle = 18, hjust = 1))
ggsave(file.path(OUT, "T1_regional_all_outcomes_full_all_ambitions.png"), p_regional,
       width = 12.5, height = 9.5, dpi = 240, bg = "white")

class_levels <- c(
  "Cross-model supported",
  "Direction supported; structural heterogeneity remains",
  "Direction supported; magnitude composition-sensitive",
  "Database association only; model-composition sensitive",
  "Coverage-qualified; composition-sensitive association",
  "Coverage-qualified; not identified across model families",
  "Not identified across model families"
)
world_class_plot <- world_synthesis %>%
  mutate(
    database = factor(database, levels = DATABASES),
    ambition_view = factor(ambition_view, levels = AMBITIONS),
    outcome_label = factor(outcome_label, levels = OUTCOME_ORDER),
    display_class = factor(evidence_class, levels = class_levels),
    short_label = case_when(
      grepl("Cross-model", evidence_class) ~ "Supported",
      grepl("magnitude", evidence_class) ~ "Magnitude-sensitive",
      grepl("structural heterogeneity", evidence_class) ~ "Heterogeneous",
      grepl("Database association", evidence_class) ~ "Database association",
      grepl("Coverage-qualified; not identified", evidence_class) ~ "Coverage-qualified\nnot identified",
      grepl("Coverage-qualified", evidence_class) ~ "Coverage-qualified",
      TRUE ~ "Not identified"
    )
  )
p_class <- ggplot(world_class_plot, aes(ambition_view, outcome_label, fill = display_class)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = short_label), size = 3, fontface = "bold", color = "#142A3D") +
  facet_grid(. ~ database) +
  scale_fill_manual(
    values = c(
      "Cross-model supported" = "#83BBC4",
      "Direction supported; structural heterogeneity remains" = "#B8D3D8",
      "Direction supported; magnitude composition-sensitive" = "#D6E4E6",
      "Database association only; model-composition sensitive" = "#E9C3AE",
      "Coverage-qualified; composition-sensitive association" = "#E5D4B5",
      "Coverage-qualified; not identified across model families" = "#E5DED1",
      "Not identified across model families" = "#DDE2E5"
    ),
    drop = FALSE
  ) +
  labs(
    title = "Tiered evidence classification at World",
    subtitle = "Classification combines pooled, equal-family, influence and coverage diagnostics",
    fill = NULL
  ) +
  theme_compass +
  theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "none")
ggsave(file.path(OUT, "TIERED_world_evidence_classes.png"), p_class,
       width = 13, height = 6.5, dpi = 240, bg = "white")

cat("\nWorld tiered synthesis:\n")
print(
  world_synthesis %>%
    select(database, ambition_view, outcome_label, n_cdr, n_re,
           pooled_effect_pct, equal_family_mean_pct, n_direction_flips,
           matched_cell_mean_pct, n_robust_cells, final_tiered_conclusion),
  n = Inf, width = Inf
)
cat("\nQA checks:\n")
print(qa_checks, n = Inf)
cat("\nWrote integrated tiered package to:\n", OUT, "\n")
