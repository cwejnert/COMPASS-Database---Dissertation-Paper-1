suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
})

# Revised employment outcome and transition accounting.
#
# Main outcome:
#   total cumulative energy-sector job-years, 2020-2050
#   = construction + manufacturing + O&M + extraction + refinery
#     + decommissioning.
#
# Transition diagnostics (reported separately, not subtracted again):
#   gross plant jobs displaced at retirement (O&M only), and
#   gross upstream jobs displaced (extraction + refinery).
#
# The old renewables-minus-fossil measure is retained only as a portfolio-
# composition diagnostic. It is not total or net employment because a fossil
# job loss mechanically raises that quantity.

R10 <- c("R10AFRICA", "R10CHINA+", "R10EUROPE", "R10INDIA+",
         "R10LATIN_AM", "R10MIDDLE_EAST", "R10NORTH_AM",
         "R10PAC_OECD", "R10REF_ECON", "R10REST_ASIA")
WORLD <- "Aggregated R10 regions"

JOBS_DIR <- Sys.getenv(
  "COMPASS_REVISED_JOBS_DIR",
  "C:/Users/camwe/Documents/Codex/2026-08-25/referenced-chatgpt-conversation-this-is-an/outputs/jobs_revision_2026-08-31"
)
LP_PATH <- Sys.getenv("COMPASS_LAND_PRIMARY", "LAND_PRIMARY.rds")
OLD_MASTER_A <- Sys.getenv(
  "COMPASS_OLD_MASTER_A",
  "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_master/approach_A/compass_master_dataset_A.rds"
)
OUT_DIR <- Sys.getenv("COMPASS_REVISED_JOBS_RESULTS", JOBS_DIR)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

jobs <- readRDS(file.path(JOBS_DIR, "compass_jobs_cumulative_2020_2050.rds"))
LP <- readRDS(LP_PATH)

# Population is fixed at 2020 in the master analysis. Read the already-frozen
# table rather than reloading the 51-million-row interpolation solely for ten
# denominators.
old_master <- readRDS(OLD_MASTER_A)
pop_r10 <- old_master %>%
  filter(Region %in% R10) %>%
  distinct(Region, pop_mln)
stopifnot(nrow(pop_r10) == length(R10), !anyNA(pop_r10$pop_mln))
pop_world <- sum(pop_r10$pop_mln)

# Compatibility frame for the existing deck's archived full-century jobs
# calculation. This lets the revised definition be compared on the same
# 2020-2100 horizon as the displayed 227.6/691.5-type employment medians while
# the submission pipeline's 2020-2050 outcome window remains available above.
legacy_world_core <- old_master %>%
  filter(Region == WORLD) %>%
  distinct(Model, Scenario, .keep_all = TRUE) %>%
  transmute(Model, Scenario, Region, pop_mln,
            jobs_RE_minus_fossil = jobs_Renewables - jobs_Fossil,
            jobs_EnergyTotal_no_decommission = jobs_Renewables + jobs_Fossil +
              jobs_Nuclear + jobs_Bioenergy)
rm(old_master)
gc()

metric_cols <- c(
  "jobs_RE_minus_fossil",
  "jobs_EnergyTotal_no_decommission",
  "jobs_EnergyTotal",
  "jobs_Decommission",
  "gross_plant_jobs_displaced",
  "gross_upstream_jobs_displaced",
  "inferred_retirements_GW",
  "absolute_balance_residual_GW",
  "unexplained_additions_GW"
)

# Strict World row: an aggregate exists only when all ten R10 regions are
# present. Sum first, then divide by the combined population.
world <- jobs %>%
  filter(Region %in% R10) %>%
  group_by(Model, Scenario, Category) %>%
  summarise(
    n_regions = n_distinct(Region),
    across(all_of(metric_cols),
           ~ if (n_regions == length(R10)) sum(.x, na.rm = TRUE) else NA_real_),
    .groups = "drop") %>%
  mutate(Region = WORLD, pop_mln = pop_world)

regional <- jobs %>%
  filter(Region %in% R10) %>%
  left_join(pop_r10, by = "Region") %>%
  mutate(n_regions = 1L)

values <- bind_rows(regional, world) %>%
  mutate(
    jobs_RE_minus_fossil_per_1k = jobs_RE_minus_fossil / pop_mln,
    total_energy_jobyears_no_decommission_per_1k =
      jobs_EnergyTotal_no_decommission / pop_mln,
    total_energy_jobyears_per_1k = jobs_EnergyTotal / pop_mln,
    decommission_jobyears_per_1k = jobs_Decommission / pop_mln,
    gross_plant_jobs_displaced_per_1k =
      gross_plant_jobs_displaced / pop_mln,
    gross_upstream_jobs_displaced_per_1k =
      gross_upstream_jobs_displaced / pop_mln)

labels <- LP$labels_land %>%
  filter(approach %in% c("A", "C")) %>%
  transmute(database = if_else(approach == "A", "Full", "SCI-vetted"),
            Model, Scenario, Pathway, ambition = amb)

frames <- labels %>%
  inner_join(values, by = c("Model", "Scenario"),
             relationship = "many-to-many") %>%
  mutate(model_family = sub("[ /-].*$", "", Model))

label_counts <- labels %>%
  group_by(database, ambition, Pathway) %>%
  summarise(classified = n_distinct(paste(Model, Scenario)), .groups = "drop")
joined_counts <- frames %>%
  filter(Region == WORLD, !is.na(jobs_EnergyTotal)) %>%
  group_by(database, ambition, Pathway) %>%
  summarise(with_complete_world_jobs = n_distinct(paste(Model, Scenario)),
            .groups = "drop")
coverage <- label_counts %>%
  left_join(joined_counts,
            by = c("database", "ambition", "Pathway")) %>%
  mutate(with_complete_world_jobs = coalesce(with_complete_world_jobs, 0L),
         pct = 100 * with_complete_world_jobs / classified)
write_csv(coverage, file.path(OUT_DIR, "W19_jobs_classification_coverage.csv"))

metrics <- tribble(
  ~metric, ~label, ~unit,
  "jobs_RE_minus_fossil_per_1k",
  "Renewable minus fossil job-years (diagnostic)", "job-years per 1,000",
  "total_energy_jobyears_no_decommission_per_1k",
  "Total energy job-years, excluding decommissioning", "job-years per 1,000",
  "total_energy_jobyears_per_1k",
  "Total energy job-years, including decommissioning", "job-years per 1,000",
  "decommission_jobyears_per_1k",
  "Decommissioning job-years", "job-years per 1,000",
  "gross_plant_jobs_displaced_per_1k",
  "Gross plant jobs displaced at retirement", "positions per 1,000",
  "gross_upstream_jobs_displaced_per_1k",
  "Gross upstream jobs displaced at retirement", "positions per 1,000",
  "inferred_retirements_GW", "Inferred retired capacity", "GW"
)

frames_views <- bind_rows(
  frames %>% mutate(ambition_view = ambition),
  frames %>% mutate(ambition_view = "All ambitions pooled")
)

arm_medians <- crossing(
  frames_views %>% distinct(database, ambition_view, Region),
  metrics
) %>%
  pmap_dfr(function(database, ambition_view, Region, metric, label, unit) {
    z <- frames_views %>%
      filter(.data$database == .env$database,
             .data$ambition_view == .env$ambition_view,
             .data$Region == .env$Region)
    bind_rows(lapply(c("High-CMT", "High-RE"), function(arm) {
      x <- z[[metric]][z$Pathway == arm]
      x <- x[!is.na(x)]
      tibble(database, ambition_view, Region, metric, label, unit,
             Pathway = arm, n = length(x),
             median = if (length(x)) median(x) else NA_real_)
    }))
  })

comparisons <- arm_medians %>%
  select(database, ambition_view, Region, metric, label, unit,
         Pathway, n, median) %>%
  pivot_wider(names_from = Pathway, values_from = c(n, median)) %>%
  mutate(raw_difference_high_re_minus_high_cmt =
           `median_High-RE` - `median_High-CMT`,
         favours_more_employment = case_when(
           raw_difference_high_re_minus_high_cmt > 0 ~ "High-RE",
           raw_difference_high_re_minus_high_cmt < 0 ~ "High-CMT",
           TRUE ~ "Tie"))

write_csv(arm_medians, file.path(OUT_DIR, "W19_jobs_arm_medians.csv"))
write_csv(comparisons, file.path(OUT_DIR, "W19_jobs_arm_comparisons.csv"))

# Within-model-family version of the same raw-median comparison. One scenario
# in each arm is retained descriptively; n is printed so fragile cells remain
# visible rather than being mistaken for robust evidence.
family_arm_medians <- crossing(
  frames_views %>%
    distinct(database, ambition_view, Region, model_family),
  metrics
) %>%
  pmap_dfr(function(database, ambition_view, Region, model_family,
                    metric, label, unit) {
    z <- frames_views %>%
      filter(.data$database == .env$database,
             .data$ambition_view == .env$ambition_view,
             .data$Region == .env$Region,
             .data$model_family == .env$model_family)
    bind_rows(lapply(c("High-CMT", "High-RE"), function(arm) {
      x <- z[[metric]][z$Pathway == arm]
      x <- x[!is.na(x)]
      tibble(database, ambition_view, Region, model_family,
             metric, label, unit, Pathway = arm, n = length(x),
             median = if (length(x)) median(x) else NA_real_)
    }))
  })
family_comparisons <- family_arm_medians %>%
  select(database, ambition_view, Region, model_family,
         metric, label, unit, Pathway, n, median) %>%
  pivot_wider(names_from = Pathway, values_from = c(n, median)) %>%
  mutate(comparable = `n_High-CMT` >= 1 & `n_High-RE` >= 1,
         robust_3_per_arm = `n_High-CMT` >= 3 & `n_High-RE` >= 3,
         raw_difference_high_re_minus_high_cmt =
           `median_High-RE` - `median_High-CMT`,
         favours_more_employment = case_when(
           !comparable ~ NA_character_,
           raw_difference_high_re_minus_high_cmt > 0 ~ "High-RE",
           raw_difference_high_re_minus_high_cmt < 0 ~ "High-CMT",
           TRUE ~ "Tie"))
write_csv(family_arm_medians,
          file.path(OUT_DIR, "W19_jobs_within_model_arm_medians.csv"))
write_csv(family_comparisons,
          file.path(OUT_DIR, "W19_jobs_within_model_comparisons.csv"))

# World-only concise review table: the three definitions needed to understand
# whether the old employment headline survives the corrected outcome.
world_review <- comparisons %>%
  filter(Region == WORLD,
         metric %in% c("jobs_RE_minus_fossil_per_1k",
                       "total_energy_jobyears_no_decommission_per_1k",
                       "total_energy_jobyears_per_1k")) %>%
  arrange(database, factor(ambition_view,
                            levels = c("All ambitions pooled", "1.5C", "2C")),
          metric)
write_csv(world_review, file.path(OUT_DIR, "W19_jobs_world_review.csv"))

# ---- existing-deck full-century compatibility result ------------------------
retirement_annual <- readRDS(file.path(
  JOBS_DIR, "compass_jobs_retirements_decommissioning_annual.rds"))
transition_fuel_world_2100 <- retirement_annual %>%
  filter(Year >= 2020, Year <= 2100, Region %in% R10) %>%
  group_by(Model, Scenario, fuel, tech_group) %>%
  summarise(
    n_regions = n_distinct(Region),
    inferred_retirements_GW = if (n_regions == length(R10))
      sum(inferred_retirements_GW, na.rm = TRUE) else NA_real_,
    decommission_jobyears_thousands = if (n_regions == length(R10))
      sum(decommission_jobyears_thousands, na.rm = TRUE) else NA_real_,
    plant_jobs_displaced_thousands = if (n_regions == length(R10))
      sum(plant_jobs_displaced_thousands, na.rm = TRUE) else NA_real_,
    upstream_jobs_displaced_thousands = if (n_regions == length(R10))
      sum(upstream_jobs_displaced_thousands, na.rm = TRUE) else NA_real_,
    .groups = "drop")
transition_world_2100 <- retirement_annual %>%
  filter(Year >= 2020, Year <= 2100, Region %in% R10) %>%
  group_by(Model, Scenario) %>%
  summarise(
    n_regions = n_distinct(Region),
    jobs_Decommission = if (n_regions == length(R10))
      sum(decommission_jobyears_thousands, na.rm = TRUE) else NA_real_,
    gross_plant_jobs_displaced = if (n_regions == length(R10))
      sum(plant_jobs_displaced_thousands, na.rm = TRUE) else NA_real_,
    gross_upstream_jobs_displaced = if (n_regions == length(R10))
      sum(upstream_jobs_displaced_thousands, na.rm = TRUE) else NA_real_,
    inferred_retirements_GW = if (n_regions == length(R10))
      sum(inferred_retirements_GW, na.rm = TRUE) else NA_real_,
    .groups = "drop")
rm(retirement_annual)
gc()

transition_fuel_frames <- labels %>%
  inner_join(transition_fuel_world_2100,
             by = c("Model", "Scenario"), relationship = "many-to-many")
transition_fuel_views <- bind_rows(
  transition_fuel_frames %>% mutate(ambition_view = ambition),
  transition_fuel_frames %>% mutate(ambition_view = "All ambitions pooled")
)
transition_by_fuel <- transition_fuel_views %>%
  pivot_longer(
    c(inferred_retirements_GW, decommission_jobyears_thousands,
      plant_jobs_displaced_thousands, upstream_jobs_displaced_thousands),
    names_to = "measure", values_to = "value") %>%
  group_by(database, ambition_view, Pathway, fuel, tech_group, measure) %>%
  summarise(n = sum(!is.na(value)),
            median = median(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Pathway, values_from = c(n, median)) %>%
  mutate(raw_difference_high_re_minus_high_cmt =
           `median_High-RE` - `median_High-CMT`)
write_csv(transition_by_fuel,
          file.path(OUT_DIR,
                    "W19_jobs_transition_by_fuel_world_2020_2100.csv"))

legacy_values <- legacy_world_core %>%
  left_join(transition_world_2100, by = c("Model", "Scenario")) %>%
  mutate(
    jobs_EnergyTotal = jobs_EnergyTotal_no_decommission + jobs_Decommission,
    jobs_RE_minus_fossil_per_1k = jobs_RE_minus_fossil / pop_mln,
    total_energy_jobyears_no_decommission_per_1k =
      jobs_EnergyTotal_no_decommission / pop_mln,
    total_energy_jobyears_per_1k = jobs_EnergyTotal / pop_mln,
    decommission_jobyears_per_1k = jobs_Decommission / pop_mln,
    gross_plant_jobs_displaced_per_1k =
      gross_plant_jobs_displaced / pop_mln,
    gross_upstream_jobs_displaced_per_1k =
      gross_upstream_jobs_displaced / pop_mln)

legacy_frames <- labels %>%
  inner_join(legacy_values, by = c("Model", "Scenario"),
             relationship = "many-to-many")
legacy_views <- bind_rows(
  legacy_frames %>% mutate(ambition_view = ambition),
  legacy_frames %>% mutate(ambition_view = "All ambitions pooled")
)
legacy_metrics <- metrics %>%
  filter(metric %in% c("jobs_RE_minus_fossil_per_1k",
                       "total_energy_jobyears_no_decommission_per_1k",
                       "total_energy_jobyears_per_1k",
                       "decommission_jobyears_per_1k",
                       "gross_plant_jobs_displaced_per_1k",
                       "gross_upstream_jobs_displaced_per_1k",
                       "inferred_retirements_GW"))
legacy_arm_medians <- crossing(
  legacy_views %>% distinct(database, ambition_view, Region),
  legacy_metrics
) %>%
  pmap_dfr(function(database, ambition_view, Region, metric, label, unit) {
    z <- legacy_views %>%
      filter(.data$database == .env$database,
             .data$ambition_view == .env$ambition_view,
             .data$Region == .env$Region)
    bind_rows(lapply(c("High-CMT", "High-RE"), function(arm) {
      x <- z[[metric]][z$Pathway == arm]
      x <- x[!is.na(x)]
      tibble(database, ambition_view, Region, metric, label, unit,
             Pathway = arm, n = length(x),
             median = if (length(x)) median(x) else NA_real_)
    }))
  })
legacy_comparisons <- legacy_arm_medians %>%
  select(database, ambition_view, Region, metric, label, unit,
         Pathway, n, median) %>%
  pivot_wider(names_from = Pathway, values_from = c(n, median)) %>%
  mutate(raw_difference_high_re_minus_high_cmt =
           `median_High-RE` - `median_High-CMT`,
         favours_more_employment = case_when(
           raw_difference_high_re_minus_high_cmt > 0 ~ "High-RE",
           raw_difference_high_re_minus_high_cmt < 0 ~ "High-CMT",
           TRUE ~ "Tie"))
write_csv(legacy_comparisons,
          file.path(OUT_DIR, "W19_jobs_world_review_2020_2100_deck_compatibility.csv"))

# Balance QA by model family. A residual means reported additions were smaller
# than the observed annual stock increase; the retirement flow itself is never
# forced to absorb that inconsistency.
balance_qa <- values %>%
  filter(Region == WORLD) %>%
  mutate(model_family = sub("[ /-].*$", "", Model)) %>%
  group_by(model_family) %>%
  summarise(
    scenarios = n(),
    median_retirements_GW = median(inferred_retirements_GW, na.rm = TRUE),
    median_unexplained_additions_GW =
      median(unexplained_additions_GW, na.rm = TRUE),
    pct_with_nonzero_residual =
      100 * mean(absolute_balance_residual_GW > 1e-8, na.rm = TRUE),
    residual_as_pct_of_retirements =
      100 * sum(absolute_balance_residual_GW, na.rm = TRUE) /
      sum(inferred_retirements_GW, na.rm = TRUE),
    .groups = "drop") %>%
  arrange(desc(residual_as_pct_of_retirements))
write_csv(balance_qa, file.path(OUT_DIR, "W19_jobs_stock_balance_QA.csv"))

cat("\nClassification coverage:\n")
print(coverage, n = Inf)
cat("\nWorld review:\n")
print(world_review %>%
        select(database, ambition_view, label,
               `n_High-CMT`, `n_High-RE`,
               `median_High-CMT`, `median_High-RE`,
               raw_difference_high_re_minus_high_cmt,
               favours_more_employment),
      n = Inf)
cat("\nStock-balance QA by model family:\n")
print(balance_qa, n = Inf)
cat("\nWrote revised employment results to: ", OUT_DIR, "\n", sep = "")
