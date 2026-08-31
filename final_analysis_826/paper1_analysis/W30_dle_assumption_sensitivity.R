# =============================================================================
# W30 - DLE assumption sensitivity using the final ten-region DESIRE method
#
# Varies three assumptions without changing pathway classification:
#   1. DESIRE threshold level: 0.75x, 1.00x, 1.25x
#   2. Regional final-energy Gini: baseline -0.05, baseline, baseline +0.05
#   3. Provisioning-efficiency trajectory: slower, baseline, faster
#
# The outcome calculation uses total final energy directly. Industry and
# transportation remain useful source diagnostics, but the remaining energy is
# correctly described as residual final energy rather than residential/
# commercial energy. The sector thresholds are summed before the region-total
# lognormal headcount and gap are calculated.
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
  "COMPASS_DLE_SENSITIVITY_OUT_DIR",
  file.path(BASE_DIR, "final_outcomes/dle_sensitivity")
)
BASE_FILE <- Sys.getenv(
  "COMPASS_DLE_SENSITIVITY_BASE",
  file.path(OUT, "dle_sensitivity_base_2020_2100.rds")
)
FE_POP_SOURCE <- Sys.getenv("COMPASS_FE_POP_FILE", "")
DLE_INPUT_FILE <- Sys.getenv(
  "COMPASS_DLE_INPUT_FILE",
  file.path(BASE_DIR, "inputs/desire_r10_dle_inputs.csv")
)
LP_FILE <- Sys.getenv(
  "COMPASS_LAND_PRIMARY",
  file.path(BASE_DIR, "LAND_PRIMARY.rds")
)

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

R10 <- c(
  "R10AFRICA", "R10CHINA+", "R10EUROPE", "R10INDIA+", "R10LATIN_AM",
  "R10MIDDLE_EAST", "R10NORTH_AM", "R10PAC_OECD", "R10REF_ECON", "R10REST_ASIA"
)
WORLD <- "Aggregated R10 regions"
YEARS <- 2020:2100
N_R10 <- length(R10)

norm_key <- function(x) {
  x <- gsub("<U\\+00B0>", "<DEG>", x)
  gsub("[^ -~]", "<DEG>", x)
}

safe_median <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) median(x) else NA_real_
}

direction_label <- function(x, tolerance = 1e-10) {
  case_when(
    x > tolerance ~ "High-RE",
    x < -tolerance ~ "High-CMT",
    is.finite(x) ~ "Tie",
    TRUE ~ NA_character_
  )
}

if (!file.exists(LP_FILE)) stop("Missing classification file: ", LP_FILE)
if (!file.exists(DLE_INPUT_FILE)) stop("Missing final DESIRE R10 input: ", DLE_INPUT_FILE)

LP <- readRDS(LP_FILE)

labels <- imap_dfr(c(A = "Full", C = "SCI-vetted"), function(database, ap) {
  LP$labels_land %>%
    filter(axis == "with land", approach == ap) %>%
    transmute(
      database,
      Model,
      Scenario,
      model_key = norm_key(Model),
      scenario_key = norm_key(Scenario),
      Pathway = recode(Pathway, `High-CDR` = "High-CMT"),
      ambition = amb
    ) %>%
    distinct(database, model_key, scenario_key, .keep_all = TRUE)
})

label_keys <- labels %>% distinct(model_key, scenario_key)

# Build the compact classified-scenario base only once. The complete raw
# final-energy/population extract is intentionally not copied into the release.
if (!file.exists(BASE_FILE)) {
  if (!nzchar(FE_POP_SOURCE) || !file.exists(FE_POP_SOURCE)) {
    stop(
      "The compact DLE sensitivity base is missing. Set COMPASS_FE_POP_FILE ",
      "to compass_fe_pop.rds for the one-time build."
    )
  }

  message("Building compact DLE sensitivity base from: ", FE_POP_SOURCE)
  fe_pop_raw <- readRDS(FE_POP_SOURCE) %>%
    mutate(model_key = norm_key(Model), scenario_key = norm_key(Scenario))

  pop2020_r10 <- fe_pop_raw %>%
    filter(Variable == "Population", Region %in% R10, Year == 2020) %>%
    group_by(Region) %>%
    summarise(pop2020_millions = median(Value, na.rm = TRUE), .groups = "drop")

  base_annual <- fe_pop_raw %>%
    semi_join(label_keys, by = c("model_key", "scenario_key")) %>%
    filter(
      Region %in% R10,
      Year %in% YEARS,
      Variable %in% c(
        "Final Energy", "Final Energy|Industry",
        "Final Energy|Transportation", "Population"
      )
    ) %>%
    group_by(Model, Scenario, model_key, scenario_key, Region, Year, Variable) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Variable, values_from = Value) %>%
    transmute(
      Model, Scenario, model_key, scenario_key, Region, Year,
      final_energy_EJ = `Final Energy`,
      industry_EJ = `Final Energy|Industry`,
      transport_EJ = `Final Energy|Transportation`,
      pop_millions = Population,
      residual_final_energy_EJ = final_energy_EJ - industry_EJ - transport_EJ
    ) %>%
    filter(is.finite(final_energy_EJ), final_energy_EJ >= 0,
           is.finite(pop_millions), pop_millions > 0)

  compact <- list(
    annual = base_annual,
    pop2020_r10 = pop2020_r10,
    source = normalizePath(FE_POP_SOURCE, winslash = "/", mustWork = TRUE),
    built_at = as.character(Sys.time())
  )
  saveRDS(compact, BASE_FILE, compress = "xz")
  rm(fe_pop_raw, base_annual, compact)
  invisible(gc())
}

compact <- readRDS(BASE_FILE)
if (!is.list(compact) || !all(c("annual", "pop2020_r10") %in% names(compact))) {
  stop("Unexpected compact DLE sensitivity base format: ", BASE_FILE)
}
base_annual <- compact$annual
pop2020_r10 <- compact$pop2020_r10

stopifnot(
  setequal(unique(base_annual$Region), R10),
  nrow(pop2020_r10) == N_R10,
  !anyDuplicated(base_annual[c("model_key", "scenario_key", "Region", "Year")])
)

dle_inputs <- read_csv(DLE_INPUT_FILE, show_col_types = FALSE) %>%
  filter(Region %in% R10) %>%
  transmute(
    Region,
    threshold_total_GJ = res_comm_GJ + industry_GJ + transport_GJ,
    desire_energy_gini
  )
stopifnot(nrow(dle_inputs) == N_R10, !anyNA(dle_inputs))

threshold_grid <- tibble(
  threshold_scale = c(0.75, 1.00, 1.25),
  threshold_label = c("0.75x", "1.00x", "1.25x")
)
gini_grid <- tibble(
  gini_shift = c(-0.05, 0, 0.05),
  gini_label = c("Gini -0.05", "Baseline Gini", "Gini +0.05")
)
efficiency_grid <- tribble(
  ~efficiency_label, ~sef_rate, ~sef_floor,
  "Slower improvement",   0.015, 0.60,
  "Baseline improvement", 0.019, 0.50,
  "Faster improvement",   0.023, 0.40
)

specs <- crossing(threshold_grid, gini_grid, efficiency_grid) %>%
  mutate(
    spec_id = sprintf(
      "T%s_G%+.2f_E%s",
      threshold_label, gini_shift,
      recode(
        efficiency_label,
        `Slower improvement` = "slow",
        `Baseline improvement` = "base",
        `Faster improvement` = "fast"
      )
    ),
    is_baseline = threshold_scale == 1 & gini_shift == 0 &
      efficiency_label == "Baseline improvement"
  ) %>%
  select(spec_id, is_baseline, everything())

write_csv(specs, file.path(OUT, "W30_dle_assumption_grid.csv"))

base_with_inputs <- base_annual %>%
  inner_join(dle_inputs, by = "Region")

build_one_spec <- function(spec, window_end = 2100L) {
  spec <- as.list(spec)

  annual <- base_with_inputs %>%
    filter(Year <= window_end) %>%
    mutate(
      threshold_GJ_pc = threshold_total_GJ * spec$threshold_scale *
        pmax(spec$sef_floor, 1 - spec$sef_rate * (Year - 2020)),
      gini = pmin(0.75, pmax(0.15, desire_energy_gini + spec$gini_shift)),
      sigma_ln = sqrt(2) * qnorm((gini + 1) / 2),
      energy_GJ_pc = final_energy_EJ * 1000 / pop_millions,
      mu_ln = log(pmax(energy_GJ_pc, 0.01)) - sigma_ln^2 / 2,
      d1 = (log(pmax(threshold_GJ_pc, 0.01)) - mu_ln) / sigma_ln,
      d2 = d1 - sigma_ln,
      deprivation_rate = pnorm(d1),
      headcount_millions = pnorm(d1) * pop_millions,
      gap_GJ_pc_annual = pmax(
        0,
        threshold_GJ_pc * pnorm(d1) - energy_GJ_pc * pnorm(d2)
      ),
      gap_EJ_total = gap_GJ_pc_annual * pop_millions / 1000
    )

  regional <- annual %>%
    group_by(Model, Scenario, model_key, scenario_key, Region) %>%
    summarise(
      n_years = n_distinct(Year),
      cumulative_gap_EJ = sum(gap_EJ_total, na.rm = TRUE),
      deprived_person_years_millions = sum(headcount_millions, na.rm = TRUE),
      population_person_years_millions = sum(pop_millions, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(pop2020_r10, by = "Region") %>%
    mutate(
      gap_GJ_pc = cumulative_gap_EJ * 1000 / pop2020_millions,
      headcount_pct = deprived_person_years_millions /
        population_person_years_millions * 100
    )

  world <- regional %>%
    group_by(Model, Scenario, model_key, scenario_key) %>%
    summarise(
      n_regions_gap = sum(is.finite(cumulative_gap_EJ)),
      n_regions_headcount = sum(
        is.finite(deprived_person_years_millions) &
          is.finite(population_person_years_millions)
      ),
      cumulative_gap_EJ = if (n_regions_gap == N_R10)
        sum(cumulative_gap_EJ) else NA_real_,
      deprived_person_years_millions = if (n_regions_headcount == N_R10)
        sum(deprived_person_years_millions) else NA_real_,
      population_person_years_millions = if (n_regions_headcount == N_R10)
        sum(population_person_years_millions) else NA_real_,
      n_years = min(n_years),
      .groups = "drop"
    ) %>%
    mutate(
      Region = WORLD,
      pop2020_millions = sum(pop2020_r10$pop2020_millions),
      gap_GJ_pc = cumulative_gap_EJ * 1000 / pop2020_millions,
      headcount_pct = deprived_person_years_millions /
        population_person_years_millions * 100
    )

  bind_rows(
    regional %>% select(Model, Scenario, model_key, scenario_key, Region,
                        n_years, gap_GJ_pc, headcount_pct),
    world %>% select(Model, Scenario, model_key, scenario_key, Region,
                     n_years, gap_GJ_pc, headcount_pct)
  ) %>%
    mutate(
      spec_id = spec$spec_id,
      is_baseline = spec$is_baseline,
      threshold_scale = spec$threshold_scale,
      threshold_label = spec$threshold_label,
      gini_shift = spec$gini_shift,
      gini_label = spec$gini_label,
      efficiency_label = spec$efficiency_label,
      sef_rate = spec$sef_rate,
      sef_floor = spec$sef_floor
    )
}

message("Running ", nrow(specs), " DLE assumption combinations...")
scenario_physical <- map_dfr(seq_len(nrow(specs)), function(i) {
  message("  ", i, "/", nrow(specs), ": ", specs$spec_id[[i]])
  build_one_spec(specs[i, ])
})

scenario_values <- scenario_physical %>%
  inner_join(
    labels %>% select(database, model_key, scenario_key, Pathway, ambition),
    by = c("model_key", "scenario_key"), relationship = "many-to-many"
  ) %>%
  select(database, Model, Scenario, Pathway, ambition, Region,
         spec_id, is_baseline, threshold_scale, threshold_label,
         gini_shift, gini_label, efficiency_label, sef_rate, sef_floor,
         n_years, gap_GJ_pc, headcount_pct)

stopifnot(!anyDuplicated(scenario_values[c(
  "database", "Model", "Scenario", "Region", "spec_id"
)]))

saveRDS(
  scenario_values,
  file.path(OUT, "W30_dle_sensitivity_scenario_values.rds"),
  compress = "xz"
)
write_csv(
  scenario_values %>% filter(is_baseline),
  file.path(OUT, "compass_dle_scenario_values_2020_2100.csv")
)

long <- scenario_values %>%
  pivot_longer(
    c(gap_GJ_pc, headcount_pct),
    names_to = "outcome", values_to = "value"
  ) %>%
  bind_rows(
    .,
    mutate(., ambition = "All ambitions")
  )

arm_medians <- long %>%
  group_by(
    database, spec_id, is_baseline, threshold_scale, threshold_label,
    gini_shift, gini_label, efficiency_label, sef_rate, sef_floor,
    ambition, Region, outcome, Pathway
  ) %>%
  summarise(
    n_scenarios = n_distinct(paste(Model, Scenario)),
    median = safe_median(value),
    .groups = "drop"
  )

contrasts <- arm_medians %>%
  select(-n_scenarios) %>%
  pivot_wider(names_from = Pathway, values_from = median) %>%
  left_join(
    arm_medians %>%
      select(database, spec_id, ambition, Region, outcome, Pathway,
             n_scenarios) %>%
      pivot_wider(names_from = Pathway, values_from = n_scenarios,
                  names_prefix = "n_"),
    by = c("database", "spec_id", "ambition", "Region", "outcome")
  ) %>%
  mutate(
    raw_advantage_high_re = `High-CMT` - `High-RE`,
    direction = direction_label(raw_advantage_high_re)
  )

write_csv(arm_medians, file.path(OUT, "W30_dle_sensitivity_arm_medians.csv"))
write_csv(contrasts, file.path(OUT, "W30_dle_sensitivity_contrasts.csv"))

stability <- contrasts %>%
  group_by(database, ambition, Region, outcome) %>%
  summarise(
    baseline_advantage = raw_advantage_high_re[is_baseline][1],
    baseline_direction = direction[is_baseline][1],
    min_advantage = min(raw_advantage_high_re, na.rm = TRUE),
    max_advantage = max(raw_advantage_high_re, na.rm = TRUE),
    n_specs = n(),
    n_high_re = sum(direction == "High-RE", na.rm = TRUE),
    n_high_cmt = sum(direction == "High-CMT", na.rm = TRUE),
    n_tie = sum(direction == "Tie", na.rm = TRUE),
    direction_stable = n_distinct(direction[!is.na(direction)]) == 1,
    .groups = "drop"
  )

write_csv(
  stability,
  file.path(OUT, "W30_dle_direction_stability_summary.csv")
)

# Compare with the historical frozen master values. Those files predate both
# the common 2020-2100 outcome horizon and the final input vintage, so this is a
# documented supersession comparison rather than a reproduction requirement.
reference_values <- imap_dfr(c(A = "Full", C = "SCI-vetted"), function(database, ap) {
  regional <- readRDS(file.path(
    BASE_DIR, "master_outputs", paste0("approach_", ap),
    paste0("compass_master_dataset_", ap, ".rds")
  )) %>%
    filter(Region %in% R10) %>%
    distinct(Model, Scenario, Region, gap_GJ_pc, headcount_pct)

  world <- LP$world %>%
    filter(approach == ap, Region == WORLD) %>%
    distinct(Model, Scenario, Region, gap_GJ_pc, headcount_pct)

  bind_rows(regional, world) %>%
    mutate(database, model_key = norm_key(Model), scenario_key = norm_key(Scenario)) %>%
    semi_join(
      labels %>% filter(.data$database == .env$database),
      by = c("model_key", "scenario_key")
    ) %>%
    pivot_longer(c(gap_GJ_pc, headcount_pct), names_to = "outcome",
                 values_to = "reference_value")
})

baseline_values <- scenario_values %>%
  filter(is_baseline) %>%
  mutate(model_key = norm_key(Model), scenario_key = norm_key(Scenario)) %>%
  pivot_longer(c(gap_GJ_pc, headcount_pct), names_to = "outcome",
               values_to = "regenerated_value")

baseline_qa <- reference_values %>%
  inner_join(
    baseline_values %>%
      select(database, model_key, scenario_key, Region, outcome,
             regenerated_value),
    by = c("database", "model_key", "scenario_key", "Region", "outcome")
  ) %>%
  mutate(abs_difference = abs(reference_value - regenerated_value)) %>%
  group_by(database, outcome, Region = if_else(Region == WORLD, WORLD, "R10 regions")) %>%
  summarise(
    matched_values = n(),
    max_abs_difference = max(abs_difference, na.rm = TRUE),
    median_abs_difference = median(abs_difference, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(baseline_qa, file.path(OUT, "W30_historical_frozen_comparison.csv"))

residual_qa <- base_annual %>%
  summarise(
    annual_scenario_region_rows = n(),
    rows_with_sector_components = sum(is.finite(industry_EJ) & is.finite(transport_EJ)),
    rows_with_negative_residual = sum(residual_final_energy_EJ < -1e-8, na.rm = TRUE),
    minimum_residual_EJ = min(residual_final_energy_EJ, na.rm = TRUE),
    note = paste(
      "The DLE distribution uses reported total final energy directly.",
      "Industry and transportation are diagnostics; their remainder is residual final energy,",
      "not a clean residential/commercial estimate."
    )
  )
write_csv(residual_qa, file.path(OUT, "W30_residual_final_energy_QA.csv"))

physical_range_ok <-
  !any(is.infinite(scenario_physical$gap_GJ_pc)) &&
  !any(is.infinite(scenario_physical$headcount_pct)) &&
  all(scenario_physical$gap_GJ_pc[!is.na(scenario_physical$gap_GJ_pc)] >= 0) &&
  all(scenario_physical$headcount_pct[!is.na(scenario_physical$headcount_pct)] >= 0) &&
  all(scenario_physical$headcount_pct[!is.na(scenario_physical$headcount_pct)] <= 100)

threshold_monotonic <- scenario_physical %>%
  arrange(Model, Scenario, Region, gini_shift, efficiency_label,
          threshold_scale) %>%
  group_by(Model, Scenario, Region, gini_shift, efficiency_label) %>%
  summarise(
    gap_ok = all(diff(gap_GJ_pc) >= -1e-8, na.rm = TRUE),
    headcount_ok = all(diff(headcount_pct) >= -1e-8, na.rm = TRUE),
    .groups = "drop"
  )

efficiency_monotonic <- scenario_physical %>%
  mutate(
    efficiency_order = recode(
      efficiency_label,
      `Faster improvement` = 1L,
      `Baseline improvement` = 2L,
      `Slower improvement` = 3L
    )
  ) %>%
  arrange(Model, Scenario, Region, threshold_scale, gini_shift,
          efficiency_order) %>%
  group_by(Model, Scenario, Region, threshold_scale, gini_shift) %>%
  summarise(
    gap_ok = all(diff(gap_GJ_pc) >= -1e-8, na.rm = TRUE),
    headcount_ok = all(diff(headcount_pct) >= -1e-8, na.rm = TRUE),
    .groups = "drop"
  )

qa_checks <- tribble(
  ~check, ~passed,
  "Exactly 27 threshold x Gini x efficiency specifications", nrow(specs) == 27,
  "Exactly one baseline specification", sum(specs$is_baseline) == 1,
  "Compact base covers all ten R10 regions", n_distinct(base_annual$Region) == N_R10,
  "Scenario sensitivity values have unique database-scenario-region-spec keys",
    !anyDuplicated(scenario_values[c("database", "Model", "Scenario", "Region", "spec_id")]),
  "All scenario DLE values are finite and inside physical ranges",
    physical_range_ok,
  "Raising the DLE threshold never reduces deprivation",
    all(threshold_monotonic$gap_ok & threshold_monotonic$headcount_ok),
  "Faster provisioning improvement never increases deprivation",
    all(efficiency_monotonic$gap_ok & efficiency_monotonic$headcount_ok),
  "Every stability cell contains all 27 specifications",
    all(stability$n_specs == 27)
)
write_csv(qa_checks, file.path(OUT, "W30_DLE_QA_checks.csv"))

if (!all(qa_checks$passed)) {
  print(qa_checks)
  stop("One or more W30 DLE QA checks failed")
}

world_plot <- contrasts %>%
  filter(database == "Full", Region == WORLD) %>%
  mutate(
    outcome = recode(
      outcome,
      gap_GJ_pc = "DLE gap (GJ per capita)",
      headcount_pct = "Deprived share (percentage points)"
    ),
    ambition = factor(ambition, levels = c("All ambitions", "1.5C", "2C")),
    threshold_label = factor(threshold_label, levels = c("0.75x", "1.00x", "1.25x")),
    assumption_row = factor(
      paste(gini_label, efficiency_label, sep = " / "),
      levels = rev(as.vector(outer(
        c("Gini -0.05", "Baseline Gini", "Gini +0.05"),
        c("Slower improvement", "Baseline improvement", "Faster improvement"),
        paste, sep = " / "
      )))
    )
  )

p <- ggplot(world_plot, aes(threshold_label, assumption_row,
                            fill = raw_advantage_high_re)) +
  geom_tile(color = "white", linewidth = 0.35) +
  geom_text(aes(label = sprintf("%+.2f", raw_advantage_high_re)), size = 2.3) +
  facet_grid(outcome ~ ambition, scales = "free") +
  scale_fill_gradient2(
    low = "#C56A3A", mid = "#F2F2F2", high = "#16839B", midpoint = 0,
    name = "High-RE advantage\n(lower deprivation)"
  ) +
  labs(
    title = "DLE direction under 27 threshold, inequality and efficiency assumptions",
    subtitle = "Full database, World. Positive values favor High-RE; values are raw median differences.",
    x = "DESIRE threshold scale", y = NULL,
    caption = "Gap and headcount are two readings of the same fitted final-energy distribution."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    axis.text.y = element_text(size = 7)
  )

ggsave(
  file.path(OUT, "W30_world_dle_assumption_sensitivity.png"),
  p, width = 15, height = 9, dpi = 220, bg = "white"
)

cat("\nW30 DLE QA checks:\n")
print(qa_checks)
cat("\nWorld directional stability:\n")
print(stability %>% filter(database == "Full", Region == WORLD))
cat("\nWrote final DLE sensitivity package to:\n", normalizePath(OUT, winslash = "/"), "\n")
