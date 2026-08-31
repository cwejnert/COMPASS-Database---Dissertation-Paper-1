# =============================================================================
# W27 - Regenerate the full mortality robustness matrix from the validated
#       472-scenario targeted-RFASST release.
#
# Outputs:
#   * Pooled World/R10 medians: Full and SCI-vetted; pooled and ambition split
#   * Within-family World/R10 effects and scenario counts
#   * Equal-family summaries
#   * Leave-one-family-out influence results
#   * Matched model-version x project x ambition cells and summaries
#   * Pre/post targeted-rerun comparisons
#   * Presentation-ready magnitude/count diagnostics
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(ggplot2)
})

options(width = 220, warn = 1)

BASE_DIR <- normalizePath(
  Sys.getenv("COMPASS_FINAL_ANALYSIS_DIR", "."),
  winslash = "/", mustWork = TRUE
)
OUT <- Sys.getenv(
  "COMPASS_MORTALITY_472_OUT_DIR",
  file.path(BASE_DIR, "final_outcomes/mortality_472")
)
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

MORT_R10_FILE <- Sys.getenv(
  "COMPASS_MORTALITY_472_R10_FILE",
  file.path(OUT, "compass_mortality_r10_allcdr_reporting_complete_472.csv")
)
LP_FILE <- Sys.getenv(
  "COMPASS_LAND_PRIMARY",
  file.path(BASE_DIR, "LAND_PRIMARY.rds")
)
OLD_OUT <- file.path(BASE_DIR, "final_outcomes")

R10 <- c(
  "R10AFRICA", "R10CHINA+", "R10EUROPE", "R10INDIA+", "R10LATIN_AM",
  "R10MIDDLE_EAST", "R10NORTH_AM", "R10PAC_OECD", "R10REF_ECON", "R10REST_ASIA"
)
WORLD <- "Aggregated R10 regions"
ALL_REGIONS <- c(WORLD, R10)
AMBITIONS <- c("All ambitions", "1.5C", "2C")
DATABASES <- c("Full", "SCI-vetted")
MIN_ROBUST_N <- 3L

required <- c(MORT_R10_FILE, LP_FILE)
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

cat("Loading and integrating the validated mortality release...\n")
mort_r10 <- read_csv(MORT_R10_FILE, show_col_types = FALSE)
if (anyDuplicated(mort_r10[c("model", "scenario", "r10_region", "year")])) {
  stop("Merged RFASST input contains duplicate model-scenario-region-year records.")
}

mort_regional <- mort_r10 %>%
  filter(r10_region %in% R10, year >= 2020L, year <= 2100L) %>%
  group_by(model, scenario, r10_region) %>%
  summarise(
    n_decades = n_distinct(year),
    cumulative_pm25_deaths = integrate_decadal(year, deaths_pm25),
    .groups = "drop"
  ) %>%
  filter(n_decades == 9L, is.finite(cumulative_pm25_deaths)) %>%
  transmute(
    model_key = norm_key(model), scenario_key = norm_key(scenario),
    Region = r10_region,
    mortality_million_deaths = cumulative_pm25_deaths / 1e6
  )

mort_world <- mort_r10 %>%
  filter(r10_region %in% R10, year >= 2020L, year <= 2100L) %>%
  group_by(model, scenario, year) %>%
  summarise(
    n_regions = n_distinct(r10_region),
    deaths_pm25 = if_else(n_regions == 10L, sum(deaths_pm25), NA_real_),
    .groups = "drop"
  ) %>%
  group_by(model, scenario) %>%
  summarise(
    n_decades = n_distinct(year),
    cumulative_pm25_deaths = integrate_decadal(year, deaths_pm25),
    .groups = "drop"
  ) %>%
  filter(n_decades == 9L, is.finite(cumulative_pm25_deaths)) %>%
  transmute(
    model_key = norm_key(model), scenario_key = norm_key(scenario),
    Region = WORLD,
    mortality_million_deaths = cumulative_pm25_deaths / 1e6
  )

mortality <- bind_rows(mort_world, mort_regional)

LP <- readRDS(LP_FILE)
labels <- LP$labels_land %>%
  filter(axis == "with land", approach %in% c("A", "C")) %>%
  transmute(
    database = recode(approach, A = "Full", C = "SCI-vetted"),
    Model, Scenario,
    model_key = norm_key(Model), scenario_key = norm_key(Scenario),
    Pathway = recode(Pathway, `High-CMT` = "High-CDR"),
    amb,
    family = family_name(Model),
    project = project_tag(Scenario)
  ) %>%
  distinct(database, model_key, scenario_key, .keep_all = TRUE)

frames <- labels %>%
  # The same scenario can intentionally appear once in Full and once in
  # SCI-vetted, while mortality has eleven regional rows. This is an expected
  # label-set x region expansion, not duplicate scenario data.
  inner_join(mortality, by = c("model_key", "scenario_key"), relationship = "many-to-many") %>%
  select(-model_key, -scenario_key) %>%
  distinct(database, Model, Scenario, Pathway, amb, family, project, Region, .keep_all = TRUE)

full_world_n <- frames %>%
  filter(database == "Full", Region == WORLD) %>%
  summarise(n = n_distinct(paste(Model, Scenario, sep = "||"))) %>% pull(n)
if (full_world_n != 472L) stop("Expected 472 Full-database World scenarios, found ", full_world_n)

write_csv(frames, file.path(OUT, "W27_mortality_analysis_frame.csv"))

# Canonical scenario-value release consumed by W16 and the final run wrapper.
# Mortality is calculated once, then relabelled under Full (A) and SCI-vetted
# (C) classifications; the physical mortality value is unchanged.
scenario_values <- frames %>%
  transmute(
    approach = recode(database, Full = "A", `SCI-vetted` = "C"),
    Model, Scenario,
    Ambition = amb,
    Pathway = recode(Pathway, `High-CDR` = "High-CMT"),
    cmt_axis = "all CDR including land",
    portfolio_rule = "mutually exclusive upper terciles within ambition",
    Region,
    n_decades = 9L,
    cumulative_pm25_deaths = mortality_million_deaths * 1e6,
    cumulative_pm25_deaths_mln = mortality_million_deaths
  ) %>%
  distinct(approach, Model, Scenario, Region, .keep_all = TRUE)
write_csv(
  scenario_values,
  file.path(BASE_DIR, "final_outcomes",
            "mortality_allcdr_reporting_complete_scenario_values_2020_2100.csv")
)

views <- bind_rows(
  frames %>% mutate(ambition_view = "All ambitions"),
  frames %>% mutate(ambition_view = amb)
) %>%
  mutate(
    database = factor(database, levels = DATABASES),
    ambition_view = factor(ambition_view, levels = AMBITIONS),
    Region = factor(Region, levels = ALL_REGIONS)
  )

# -----------------------------------------------------------------------------
# Pooled scenario-weighted medians
# -----------------------------------------------------------------------------

pooled <- views %>%
  group_by(database, ambition_view, Region) %>%
  summarise(
    n_cdr = sum(Pathway == "High-CDR" & is.finite(mortality_million_deaths)),
    n_re = sum(Pathway == "High-RE" & is.finite(mortality_million_deaths)),
    median_cdr_million_deaths = safe_median(mortality_million_deaths[Pathway == "High-CDR"]),
    median_re_million_deaths = safe_median(mortality_million_deaths[Pathway == "High-RE"]),
    cliffs_delta_raw = cliffs_delta(
      mortality_million_deaths[Pathway == "High-CDR"],
      mortality_million_deaths[Pathway == "High-RE"]
    ),
    .groups = "drop"
  ) %>%
  mutate(
    direction_coded_difference_million_deaths =
      median_cdr_million_deaths - median_re_million_deaths,
    relative_effect_pct = 100 * direction_coded_difference_million_deaths /
      abs(median_cdr_million_deaths),
    direction_coded_cliffs_delta = -cliffs_delta_raw,
    favours = direction_label(direction_coded_difference_million_deaths),
    estimand = "Scenario-weighted pooled medians"
  )

# -----------------------------------------------------------------------------
# Within-family effects
# -----------------------------------------------------------------------------

family_effects <- views %>%
  group_by(database, ambition_view, Region, family) %>%
  summarise(
    n_cdr = sum(Pathway == "High-CDR" & is.finite(mortality_million_deaths)),
    n_re = sum(Pathway == "High-RE" & is.finite(mortality_million_deaths)),
    median_cdr_million_deaths = safe_median(mortality_million_deaths[Pathway == "High-CDR"]),
    median_re_million_deaths = safe_median(mortality_million_deaths[Pathway == "High-RE"]),
    cliffs_delta_raw = cliffs_delta(
      mortality_million_deaths[Pathway == "High-CDR"],
      mortality_million_deaths[Pathway == "High-RE"]
    ),
    .groups = "drop"
  ) %>%
  mutate(
    comparable = n_cdr >= 1L & n_re >= 1L,
    robust = n_cdr >= MIN_ROBUST_N & n_re >= MIN_ROBUST_N,
    direction_coded_difference_million_deaths = if_else(
      comparable,
      median_cdr_million_deaths - median_re_million_deaths,
      NA_real_
    ),
    relative_effect_pct = 100 * direction_coded_difference_million_deaths /
      abs(median_cdr_million_deaths),
    direction_coded_cliffs_delta = if_else(comparable, -cliffs_delta_raw, NA_real_),
    favours = direction_label(direction_coded_difference_million_deaths)
  ) %>%
  left_join(
    pooled %>%
      select(database, ambition_view, Region, pooled_relative_effect_pct = relative_effect_pct,
             pooled_favours = favours),
    by = c("database", "ambition_view", "Region")
  ) %>%
  mutate(agrees_with_pooled = comparable & sign(relative_effect_pct) == sign(pooled_relative_effect_pct))

# -----------------------------------------------------------------------------
# Equal-family summaries
# -----------------------------------------------------------------------------

equal_family <- family_effects %>%
  filter(comparable) %>%
  group_by(database, ambition_view, Region) %>%
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

# -----------------------------------------------------------------------------
# Leave-one-family-out pooled influence
# -----------------------------------------------------------------------------

calc_loo <- function(data) {
  families <- sort(unique(data$family))
  map_dfr(families, function(excluded_family) {
    z <- data %>% filter(family != excluded_family)
    cdr <- z$mortality_million_deaths[z$Pathway == "High-CDR"]
    re <- z$mortality_million_deaths[z$Pathway == "High-RE"]
    med_cdr <- safe_median(cdr)
    med_re <- safe_median(re)
    tibble(
      excluded_family,
      n_cdr = sum(is.finite(cdr)),
      n_re = sum(is.finite(re)),
      median_cdr_million_deaths = med_cdr,
      median_re_million_deaths = med_re,
      direction_coded_difference_million_deaths = med_cdr - med_re,
      loo_effect_pct = 100 * (med_cdr - med_re) / abs(med_cdr),
      favours = direction_label(med_cdr - med_re)
    )
  })
}

loo <- views %>%
  group_by(database, ambition_view, Region) %>%
  group_modify(~calc_loo(.x)) %>%
  ungroup() %>%
  left_join(
    pooled %>% select(database, ambition_view, Region, pooled_effect_pct = relative_effect_pct,
                      pooled_favours = favours),
    by = c("database", "ambition_view", "Region")
  ) %>%
  mutate(direction_flip = sign(loo_effect_pct) != sign(pooled_effect_pct))

influence <- loo %>%
  group_by(database, ambition_view, Region, pooled_effect_pct, pooled_favours) %>%
  summarise(
    loo_min_pct = if (all(is.na(loo_effect_pct))) NA_real_ else min(loo_effect_pct, na.rm = TRUE),
    loo_max_pct = if (all(is.na(loo_effect_pct))) NA_real_ else max(loo_effect_pct, na.rm = TRUE),
    n_families_tested = sum(is.finite(loo_effect_pct)),
    n_direction_flips = sum(direction_flip, na.rm = TRUE),
    most_negative_exclusion = if (all(is.na(loo_effect_pct))) NA_character_ else excluded_family[which.min(loo_effect_pct)],
    most_positive_exclusion = if (all(is.na(loo_effect_pct))) NA_character_ else excluded_family[which.max(loo_effect_pct)],
    .groups = "drop"
  )

validation <- pooled %>%
  select(database, ambition_view, Region, pooled_n_cdr = n_cdr, pooled_n_re = n_re,
         pooled_median_cdr = median_cdr_million_deaths,
         pooled_median_re = median_re_million_deaths,
         pooled_effect_million_deaths = direction_coded_difference_million_deaths,
         pooled_effect_pct = relative_effect_pct, pooled_favours = favours) %>%
  left_join(equal_family, by = c("database", "ambition_view", "Region")) %>%
  left_join(influence, by = c("database", "ambition_view", "Region", "pooled_effect_pct", "pooled_favours")) %>%
  mutate(
    pooled_equal_family_agree = sign(pooled_effect_pct) == sign(equal_family_mean_pct),
    leave_one_out_stable = n_direction_flips == 0,
    validation_class = case_when(
      n_informative_families < 2 ~ "Insufficient cross-model support",
      !pooled_equal_family_agree ~ "Composition-sensitive",
      !leave_one_out_stable ~ "Dominant-family sensitive",
      families_favour_re > 0 & families_favour_cdr > 0 ~
        "Structurally heterogeneous but directionally stable",
      TRUE ~ "Cross-model aligned"
    )
  )

# -----------------------------------------------------------------------------
# Matched model-version x project x ambition cells
# -----------------------------------------------------------------------------

matched_project_cells <- frames %>%
  group_by(database, Region, amb, family, Model, project) %>%
  summarise(
    n_cdr = sum(Pathway == "High-CDR" & is.finite(mortality_million_deaths)),
    n_re = sum(Pathway == "High-RE" & is.finite(mortality_million_deaths)),
    median_cdr_million_deaths = safe_median(mortality_million_deaths[Pathway == "High-CDR"]),
    median_re_million_deaths = safe_median(mortality_million_deaths[Pathway == "High-RE"]),
    .groups = "drop"
  ) %>%
  mutate(
    comparable = n_cdr >= 1L & n_re >= 1L,
    robust = n_cdr >= MIN_ROBUST_N & n_re >= MIN_ROBUST_N,
    direction_coded_difference_million_deaths = if_else(
      comparable,
      median_cdr_million_deaths - median_re_million_deaths,
      NA_real_
    ),
    relative_effect_pct = 100 * direction_coded_difference_million_deaths /
      abs(median_cdr_million_deaths),
    favours = direction_label(direction_coded_difference_million_deaths),
    matched_unit = paste(Model, project, amb, sep = " | ")
  )

matched_views <- bind_rows(
  matched_project_cells %>% mutate(ambition_view = "All ambitions"),
  matched_project_cells %>% mutate(ambition_view = amb)
) %>%
  mutate(ambition_view = factor(ambition_view, levels = AMBITIONS))

matched_project_summary <- matched_views %>%
  filter(comparable) %>%
  group_by(database, ambition_view, Region) %>%
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
    total_cdr_support = sum(n_cdr),
    total_re_support = sum(n_re),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# Pre/post targeted rerun comparisons
# -----------------------------------------------------------------------------

old_pooled_file <- file.path(OLD_OUT, "W16_factorial_pooled.csv")
old_family_file <- file.path(OLD_OUT, "W18_family_effects.csv")
old_validation_file <- file.path(OLD_OUT, "W18_cross_model_validation.csv")

pre_post_pooled <- tibble()
pre_post_family <- tibble()
pre_post_validation <- tibble()

if (file.exists(old_pooled_file)) {
  old_pooled <- read_csv(old_pooled_file, show_col_types = FALSE) %>%
    filter(outcome == "mort_per_1k") %>%
    transmute(
      database, ambition_view, Region,
      old_n_cdr = n_cdr, old_n_re = n_re,
      old_median_cdr = median_cdr, old_median_re = median_re,
      old_effect_million_deaths = effect,
      old_relative_effect_pct = 100 * effect / abs(median_cdr),
      old_favours = favours
    )
  pre_post_pooled <- pooled %>%
    transmute(
      database = as.character(database), ambition_view = as.character(ambition_view),
      Region = as.character(Region),
      new_n_cdr = n_cdr, new_n_re = n_re,
      new_median_cdr = median_cdr_million_deaths,
      new_median_re = median_re_million_deaths,
      new_effect_million_deaths = direction_coded_difference_million_deaths,
      new_relative_effect_pct = relative_effect_pct,
      new_favours = favours
    ) %>%
    left_join(old_pooled, by = c("database", "ambition_view", "Region")) %>%
    mutate(
      added_cdr = new_n_cdr - old_n_cdr,
      added_re = new_n_re - old_n_re,
      effect_shift_pct_points = new_relative_effect_pct - old_relative_effect_pct,
      direction_changed = new_favours != old_favours
    )
}

if (file.exists(old_family_file)) {
  old_family <- read_csv(old_family_file, show_col_types = FALSE) %>%
    filter(outcome == "mort_per_1k") %>%
    transmute(
      database, ambition_view, Region, family = fam,
      old_n_cdr = n_cdr, old_n_re = n_re,
      old_relative_effect_pct = relative_effect_pct,
      # The historical `favours` field was based on Cliff's delta, while this
      # comparison is explicitly about the median difference. Re-derive the
      # historical direction from its saved median-based percentage effect.
      old_favours = direction_label(relative_effect_pct), old_robust = robust
    )
  pre_post_family <- family_effects %>%
    transmute(
      database = as.character(database), ambition_view = as.character(ambition_view),
      Region = as.character(Region), family,
      new_n_cdr = n_cdr, new_n_re = n_re,
      new_relative_effect_pct = relative_effect_pct,
      new_favours = favours, new_robust = robust
    ) %>%
    full_join(old_family, by = c("database", "ambition_view", "Region", "family")) %>%
    mutate(
      added_cdr = coalesce(new_n_cdr, 0L) - coalesce(old_n_cdr, 0L),
      added_re = coalesce(new_n_re, 0L) - coalesce(old_n_re, 0L),
      effect_shift_pct_points = new_relative_effect_pct - old_relative_effect_pct,
      direction_changed = !is.na(new_favours) & !is.na(old_favours) & new_favours != old_favours
    )
}

if (file.exists(old_validation_file)) {
  old_validation <- read_csv(old_validation_file, show_col_types = FALSE) %>%
    filter(outcome == "mort_per_1k") %>%
    transmute(
      database, ambition_view, Region,
      old_equal_family_mean_pct = equal_family_mean_pct,
      old_n_direction_flips = n_direction_flips,
      old_validation_class = validation_class
    )
  pre_post_validation <- validation %>%
    transmute(
      database = as.character(database), ambition_view = as.character(ambition_view),
      Region = as.character(Region),
      new_equal_family_mean_pct = equal_family_mean_pct,
      new_n_direction_flips = n_direction_flips,
      new_validation_class = validation_class
    ) %>%
    left_join(old_validation, by = c("database", "ambition_view", "Region")) %>%
    mutate(validation_class_changed = new_validation_class != old_validation_class)
}

# -----------------------------------------------------------------------------
# Write tables
# -----------------------------------------------------------------------------

write_csv(pooled, file.path(OUT, "W27_pooled_world_r10.csv"))
write_csv(family_effects, file.path(OUT, "W27_within_family_world_r10.csv"))
write_csv(equal_family, file.path(OUT, "W27_equal_family_world_r10.csv"))
write_csv(loo, file.path(OUT, "W27_leave_one_family_out_world_r10.csv"))
write_csv(validation, file.path(OUT, "W27_cross_model_validation_world_r10.csv"))
write_csv(matched_project_cells, file.path(OUT, "W27_matched_model_project_ambition_cells.csv"))
write_csv(matched_project_summary, file.path(OUT, "W27_matched_model_project_ambition_summary.csv"))
write_csv(pre_post_pooled, file.path(OUT, "W27_pre_post_pooled_comparison.csv"))
write_csv(pre_post_family, file.path(OUT, "W27_pre_post_family_comparison.csv"))
write_csv(pre_post_validation, file.path(OUT, "W27_pre_post_validation_comparison.csv"))

world_synthesis <- validation %>%
  filter(Region == WORLD) %>%
  left_join(
    matched_project_summary %>% filter(Region == WORLD),
    by = c("database", "ambition_view", "Region")
  ) %>%
  arrange(database, ambition_view)
write_csv(world_synthesis, file.path(OUT, "W27_world_synthesis.csv"))

# Reproducibility and shape checks. These are written as data so the release
# records what was verified, and the script stops before making figures if any
# primary invariant fails.
pooled_count_invariance <- pooled %>%
  group_by(database, ambition_view) %>%
  summarise(
    cdr_count_versions = n_distinct(n_cdr),
    re_count_versions = n_distinct(n_re),
    .groups = "drop"
  )
qa_checks <- tibble(
  check = c(
    "Full World contains 472 unique mortality scenarios",
    "Analysis frame has no duplicate database-scenario-region rows",
    "Every observed Full scenario has all 11 World/R10 result rows",
    "Pooled table contains 2 databases x 3 ambition views x 11 regions",
    "Pooled arm counts are invariant across regions within each specification",
    "Every comparable family effect has finite medians and a finite effect",
    "Every comparable matched cell has both arms and a finite effect",
    "Pre/post pooled comparison covers every pooled specification"
  ),
  passed = c(
    full_world_n == 472L,
    !anyDuplicated(frames[c("database", "Model", "Scenario", "Region")]),
    frames %>% filter(database == "Full") %>%
      count(Model, Scenario) %>% summarise(ok = all(n == 11L)) %>% pull(ok),
    nrow(pooled) == length(DATABASES) * length(AMBITIONS) * length(ALL_REGIONS),
    all(pooled_count_invariance$cdr_count_versions == 1L &
          pooled_count_invariance$re_count_versions == 1L),
    family_effects %>% filter(comparable) %>%
      summarise(ok = all(is.finite(median_cdr_million_deaths),
                         is.finite(median_re_million_deaths),
                         is.finite(relative_effect_pct))) %>% pull(ok),
    matched_project_cells %>% filter(comparable) %>%
      summarise(ok = all(n_cdr >= 1L, n_re >= 1L, is.finite(relative_effect_pct))) %>% pull(ok),
    nrow(pre_post_pooled) == nrow(pooled)
  )
)
write_csv(qa_checks, file.path(OUT, "W27_QA_checks.csv"))
if (!all(qa_checks$passed)) {
  print(qa_checks, n = Inf)
  stop("At least one W27 QA invariant failed.")
}

# -----------------------------------------------------------------------------
# Static diagnostics
# -----------------------------------------------------------------------------

theme_compass <- theme_minimal(base_family = "Arial", base_size = 12) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", color = "#142A3D", size = 11),
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
    transmute(
      database, ambition_view, family = "POOLED", relative_effect_pct,
      n_cdr, n_re, robust = TRUE
    ),
  family_effects %>%
    filter(Region == WORLD) %>%
    transmute(database, ambition_view, family, relative_effect_pct, n_cdr, n_re, robust)
) %>%
  complete(
    database = factor(DATABASES, levels = DATABASES),
    ambition_view = factor(AMBITIONS, levels = AMBITIONS),
    family = c("POOLED", all_families)
  ) %>%
  mutate(
    family = factor(family, levels = c("POOLED", all_families)),
    label = if_else(
      is.finite(relative_effect_pct),
      paste0(sprintf("%+.1f%%", relative_effect_pct), "\n", n_cdr, "/", n_re,
             if_else(!coalesce(robust, FALSE) & family != "POOLED", "*", "")),
      if_else(
        !is.na(n_cdr) | !is.na(n_re),
        paste0("-\n", coalesce(n_cdr, 0L), "/", coalesce(n_re, 0L)),
        ""
      )
    )
  )

p_world <- ggplot(world_cells, aes(family, "Mortality", fill = pmax(-25, pmin(25, relative_effect_pct)))) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = label), fontface = "bold", size = 3, color = "#142A3D", lineheight = 0.9) +
  facet_grid(database ~ ambition_view) +
  scale_fill_gradient2(
    low = "#C56D42", mid = "#F2F3F3", high = "#16859C", midpoint = 0,
    limits = c(-25, 25), na.value = "#E6EAED",
    breaks = c(-25, 0, 25), labels = c("CDR 25%", "0", "RE 25%")
  ) +
  labs(
    title = "World PM2.5 mortality after the targeted RFASST rerun",
    subtitle = "Direction-coded difference between arm medians; second line is n High-CDR / n High-RE",
    caption = "Teal favours High-RE; orange favours High-CDR. Color saturates at +/-25%; printed values are uncapped. * fewer than 3 scenarios in either arm.",
    fill = "Median difference"
  ) +
  theme_compass +
  theme(axis.text.x = element_text(angle = 35, hjust = 1), axis.text.y = element_blank())
ggsave(file.path(OUT, "W27_world_family_magnitude_counts.png"), p_world,
       width = 16, height = 6.5, dpi = 240, bg = "white")

pooled_plot <- pooled %>%
  mutate(
    database = factor(database, levels = DATABASES),
    ambition_view = factor(ambition_view, levels = AMBITIONS),
    Region = factor(Region, levels = rev(ALL_REGIONS)),
    label = paste0(sprintf("%+.1f%%", relative_effect_pct), "\n", n_cdr, "/", n_re)
  )
p_regional <- ggplot(
  pooled_plot,
  aes(ambition_view, Region, fill = pmax(-25, pmin(25, relative_effect_pct)))
) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = label), size = 2.8, fontface = "bold", color = "#142A3D", lineheight = 0.9) +
  facet_grid(. ~ database) +
  scale_fill_gradient2(
    low = "#C56D42", mid = "#F2F3F3", high = "#16859C", midpoint = 0,
    limits = c(-25, 25), na.value = "#E6EAED",
    breaks = c(-25, 0, 25), labels = c("CDR 25%", "0", "RE 25%")
  ) +
  labs(
    title = "Pooled mortality direction and magnitude across World and R10 regions",
    subtitle = "Percent advantage of the lower-mortality arm; second line is n High-CDR / n High-RE",
    caption = "Scenario-weighted arm medians. Color saturates at +/-25%; printed values are uncapped.",
    fill = "Median difference"
  ) +
  theme_compass + theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave(file.path(OUT, "W27_regional_pooled_magnitude_counts.png"), p_regional,
       width = 13.5, height = 9.5, dpi = 240, bg = "white")

family_plot <- family_effects %>%
  mutate(
    database = factor(database, levels = DATABASES),
    ambition_view = factor(ambition_view, levels = AMBITIONS),
    Region = factor(Region, levels = rev(ALL_REGIONS)),
    family = factor(family, levels = all_families),
    label = if_else(
      comparable,
      paste0(sprintf("%+.0f", relative_effect_pct), "\n", n_cdr, "/", n_re,
             if_else(!robust, "*", "")),
      if_else(n_cdr + n_re > 0, paste0("-\n", n_cdr, "/", n_re), "")
    )
  )
p_family <- ggplot(
  family_plot,
  aes(family, Region, fill = pmax(-25, pmin(25, relative_effect_pct)))
) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = label), size = 2.0, fontface = "bold", color = "#142A3D", lineheight = 0.85) +
  facet_grid(database ~ ambition_view) +
  scale_fill_gradient2(
    low = "#C56D42", mid = "#F2F3F3", high = "#16859C", midpoint = 0,
    limits = c(-25, 25), na.value = "#E6EAED",
    breaks = c(-25, 0, 25), labels = c("CDR 25%", "0", "RE 25%")
  ) +
  labs(
    title = "Within-family mortality effects by specification and region",
    subtitle = "Cell value is direction-coded percent difference; second line is n High-CDR / n High-RE",
    caption = "Blank/dash means a family lacks both arms. * fewer than 3 scenarios in either arm. Color saturates at +/-25%.",
    fill = "Median difference"
  ) +
  theme_compass +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 8), axis.text.y = element_text(size = 8))
ggsave(file.path(OUT, "W27_regional_within_family_magnitude_counts.png"), p_family,
       width = 18, height = 13, dpi = 240, bg = "white")

loo_world <- loo %>%
  filter(Region == WORLD) %>%
  mutate(
    database = factor(database, levels = DATABASES),
    ambition_view = factor(ambition_view, levels = AMBITIONS),
    excluded_family = factor(excluded_family, levels = rev(sort(unique(excluded_family))))
  )
p_loo <- ggplot(loo_world, aes(loo_effect_pct, excluded_family)) +
  geom_vline(xintercept = 0, color = "#8393A3", linewidth = 0.5) +
  geom_vline(aes(xintercept = pooled_effect_pct), color = "#142A3D", linewidth = 0.7) +
  geom_point(aes(color = direction_flip), size = 2.8) +
  facet_grid(database ~ ambition_view, scales = "free_x") +
  scale_color_manual(values = c(`FALSE` = "#16859C", `TRUE` = "#C56D42"),
                     labels = c(`FALSE` = "Direction stable", `TRUE` = "Direction flips")) +
  labs(
    title = "World mortality influence: pooled result after excluding each family",
    subtitle = "Vertical navy line is the corresponding all-family pooled estimate; points are leave-one-family-out estimates",
    x = "Direction-coded median difference (%)",
    color = NULL
  ) +
  theme_compass + theme(axis.title.x = element_text(color = "#293746"))
ggsave(file.path(OUT, "W27_world_leave_one_family_out.png"), p_loo,
       width = 15, height = 8.5, dpi = 240, bg = "white")

matched_world_plot <- matched_project_cells %>%
  filter(Region == WORLD, comparable) %>%
  mutate(
    database = factor(database, levels = DATABASES),
    amb = factor(amb, levels = c("1.5C", "2C")),
    cell_label = paste(Model, project, sep = " / "),
    cell_label = factor(cell_label, levels = rev(unique(cell_label))),
    support_label = paste0(n_cdr, "/", n_re, if_else(!robust, "*", ""))
  )
p_matched <- ggplot(matched_world_plot, aes(relative_effect_pct, cell_label)) +
  geom_vline(xintercept = 0, color = "#8393A3", linewidth = 0.6) +
  geom_segment(aes(x = 0, xend = relative_effect_pct, yend = cell_label),
               color = "#AAB5BF", linewidth = 0.8) +
  geom_point(aes(color = favours, shape = robust), size = 3) +
  geom_text(aes(label = support_label), nudge_x = 0.45, hjust = 0,
            color = "#142A3D", size = 3, fontface = "bold") +
  facet_grid(database ~ amb, scales = "free_y", space = "free_y") +
  scale_color_manual(values = c(`High-RE` = "#16859C", `High-CDR` = "#C56D42")) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1),
                     labels = c(`TRUE` = "At least 3/3", `FALSE` = "Sparse cell")) +
  scale_x_continuous(expand = expansion(mult = c(0.06, 0.16))) +
  labs(
    title = "World mortality within matched model-version, project, and ambition cells",
    subtitle = "Positive values favour High-RE; labels are n High-CDR / n High-RE",
    caption = "Each contrast holds model version, project and ambition fixed. * fewer than 3 scenarios in either arm.",
    x = "Direction-coded median difference (%)", color = NULL, shape = NULL
  ) +
  theme_compass +
  theme(axis.title.x = element_text(color = "#293746"), axis.text.y = element_text(size = 9))
ggsave(file.path(OUT, "W27_world_matched_model_project_ambition.png"), p_matched,
       width = 15, height = 8.5, dpi = 240, bg = "white")

cat("\nWorld synthesis after targeted RFASST rerun:\n")
print(world_synthesis, n = Inf, width = Inf)
cat("\nPre/post pooled direction changes:\n")
if (nrow(pre_post_pooled)) {
  print(
    pre_post_pooled %>%
      filter(direction_changed | Region == WORLD) %>%
      select(database, ambition_view, Region, old_n_cdr, old_n_re, new_n_cdr, new_n_re,
             old_relative_effect_pct, new_relative_effect_pct, old_favours, new_favours,
             direction_changed),
    n = Inf, width = Inf
  )
}
cat("\nWrote regenerated mortality package to:\n", OUT, "\n")
