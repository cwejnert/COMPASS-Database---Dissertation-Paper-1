suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

BASE_DIR <- normalizePath(
  Sys.getenv("COMPASS_FINAL_ANALYSIS_DIR", "."),
  winslash = "/", mustWork = TRUE
)
JOBS_FILE <- Sys.getenv(
  "COMPASS_REVISED_JOBS_FILE",
  file.path(BASE_DIR, "final_outcomes/jobs_revision",
            "compass_jobs_cumulative_2020_2100.rds")
)
OUT <- Sys.getenv(
  "COMPASS_REVISED_JOBS_RESULTS",
  file.path(BASE_DIR, "final_outcomes/jobs_revision")
)
LP_FILE <- Sys.getenv("COMPASS_LAND_PRIMARY", file.path(BASE_DIR, "LAND_PRIMARY.rds"))
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

R10 <- c(
  "R10AFRICA", "R10CHINA+", "R10EUROPE", "R10INDIA+", "R10LATIN_AM",
  "R10MIDDLE_EAST", "R10NORTH_AM", "R10PAC_OECD", "R10REF_ECON", "R10REST_ASIA"
)

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
    grepl("^PEP", scenario) ~ "PEP",
    TRUE ~ sub("[-/].*$", "", scenario)
  )
}

components <- c(
  "jobs_Renewables", "jobs_Nuclear", "jobs_Bioenergy", "jobs_Fossil",
  "jobs_Decommission", "jobs_EnergyTotal"
)

pop_r10 <- readRDS(file.path(
  BASE_DIR, "master_outputs/approach_A/compass_master_dataset_A.rds"
)) %>%
  filter(Region %in% R10) %>%
  distinct(Region, pop_mln)
pop_world <- sum(pop_r10$pop_mln)

world <- readRDS(JOBS_FILE) %>%
  filter(Region %in% R10) %>%
  group_by(Model, Scenario) %>%
  summarise(
    n_regions = n_distinct(Region),
    across(all_of(components),
           ~ if (n_regions == length(R10)) sum(.x, na.rm = TRUE) else NA_real_),
    .groups = "drop"
  ) %>%
  mutate(across(all_of(components), ~ .x / pop_world))

labels <- readRDS(LP_FILE)$labels_land %>%
  filter(axis == "with land", approach == "A") %>%
  transmute(Model, Scenario,
            Pathway = recode(Pathway, `High-CMT` = "High-CDR"), amb,
            family = sub("[ /-].*$", "", Model),
            project = project_tag(Scenario))

scenario_components <- labels %>%
  inner_join(world, by = c("Model", "Scenario")) %>%
  select(Model, Scenario, family, project, amb, Pathway, all_of(components))

views <- bind_rows(
  scenario_components %>% mutate(ambition_view = amb),
  scenario_components %>% mutate(ambition_view = "All ambitions")
)

arm_components <- views %>%
  pivot_longer(all_of(components), names_to = "component", values_to = "value") %>%
  group_by(ambition_view, family, project, Pathway, component) %>%
  summarise(n = sum(is.finite(value)), median = median(value, na.rm = TRUE),
            .groups = "drop")

component_differences <- arm_components %>%
  select(ambition_view, family, project, component, Pathway, n, median) %>%
  pivot_wider(names_from = Pathway, values_from = c(n, median), values_fill = 0) %>%
  mutate(
    comparable = `n_High-CDR` >= 1 & `n_High-RE` >= 1,
    robust_3_per_arm = `n_High-CDR` >= 3 & `n_High-RE` >= 3,
    raw_difference_high_re_minus_high_cdr = `median_High-RE` - `median_High-CDR`,
    favours = case_when(
      !comparable ~ NA_character_,
      raw_difference_high_re_minus_high_cdr > 0 ~ "High-RE",
      raw_difference_high_re_minus_high_cdr < 0 ~ "High-CDR",
      TRUE ~ "Tie"
    )
  )

write_csv(scenario_components,
          file.path(OUT, "W29_employment_scenario_components_world.csv"))
write_csv(arm_components,
          file.path(OUT, "W29_employment_arm_components_by_family_project.csv"))
write_csv(component_differences,
          file.path(OUT, "W29_employment_component_differences_by_family_project.csv"))

cat("\nComparable AIM and WITCH total-employment project cells:\n")
print(
  component_differences %>%
    filter(family %in% c("AIM", "WITCH"), component == "jobs_EnergyTotal",
           comparable) %>%
    arrange(ambition_view, family, project),
  n = Inf
)
