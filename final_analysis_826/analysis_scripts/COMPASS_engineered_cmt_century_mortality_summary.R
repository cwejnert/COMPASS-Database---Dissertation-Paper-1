# =============================================================================
# Coverage-qualified, no-imputation PM2.5 mortality diagnostic: 2020-2100.
# Integrates the directly reported (or short, bracketed temporal-interpolated)
# decadal RFASST output and joins it to the no-land broad engineered-CMT labels.
# =============================================================================

suppressPackageStartupMessages(library(tidyverse))

OUT_DIR <- "C:/Users/camwe/Documents/Codex/2026-08-22/i-a/outputs/engineered_cmt_century_broad"
MORT_FILE <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_mortality/compass_mortality_r10_as_reported_base_emission_engineeredcmt_century_reported_pminterp.csv"
R10 <- c("R10AFRICA", "R10CHINA+", "R10EUROPE", "R10INDIA+", "R10LATIN_AM",
         "R10MIDDLE_EAST", "R10NORTH_AM", "R10PAC_OECD", "R10REF_ECON", "R10REST_ASIA")

normalise_id <- function(x) {
  x %>% enc2utf8() %>% gsub("\\u00b0", "°", .) %>% gsub("�", "°", ., fixed = TRUE)
}
integrate_decadal <- function(year, value) {
  ord <- order(year)
  year <- year[ord]; value <- value[ord]
  if (length(year) < 2L || any(!is.finite(value))) return(NA_real_)
  sum(diff(year) * (head(value, -1L) + tail(value, -1L)) / 2)
}

labels <- read_csv(file.path(OUT_DIR, "engineered_cmt_century_broad_labels.csv"), show_col_types = FALSE) %>%
  mutate(Model = normalise_id(Model), Scenario = normalise_id(Scenario))
raw <- read_csv(MORT_FILE, show_col_types = FALSE) %>%
  transmute(Model = normalise_id(model), Scenario = normalise_id(scenario),
            Region = r10_region, Year = as.integer(year), deaths = deaths_pm25) %>%
  filter(Region %in% R10, Year >= 2020L, Year <= 2100L)

r10_mortality <- raw %>%
  group_by(Model, Scenario, Region) %>%
  summarise(n_decades = n_distinct(Year),
            cumulative_pm25_deaths = integrate_decadal(Year, deaths), .groups = "drop") %>%
  filter(n_decades == 9L, is.finite(cumulative_pm25_deaths))
world_mortality <- raw %>%
  group_by(Model, Scenario, Year) %>%
  summarise(n_regions = n_distinct(Region), deaths = if (n_regions == length(R10)) sum(deaths) else NA_real_,
            .groups = "drop") %>%
  group_by(Model, Scenario) %>%
  summarise(n_decades = n_distinct(Year),
            cumulative_pm25_deaths = integrate_decadal(Year, deaths), .groups = "drop") %>%
  filter(n_decades == 9L, is.finite(cumulative_pm25_deaths)) %>%
  mutate(Region = "Aggregated R10 regions")
mortality <- bind_rows(r10_mortality, world_mortality) %>%
  mutate(cumulative_pm25_deaths_mln = cumulative_pm25_deaths / 1e6)

joined <- labels %>%
  filter(!is.na(Pathway)) %>%
  select(approach, Model, Scenario, Ambition, Pathway, cmt_axis, portfolio_rule) %>%
  inner_join(mortality, by = c("Model", "Scenario"))

coverage <- labels %>% filter(!is.na(Pathway)) %>%
  distinct(approach, Model, Scenario, Ambition, Pathway) %>%
  count(approach, Ambition, Pathway, name = "classified_pathways") %>%
  left_join(
    joined %>% filter(Region == "Aggregated R10 regions") %>%
      distinct(approach, Model, Scenario, Ambition, Pathway) %>%
      count(approach, Ambition, Pathway, name = "mortality_available"),
    by = c("approach", "Ambition", "Pathway")
  ) %>%
  mutate(mortality_available = replace_na(mortality_available, 0L),
         coverage_pct = 100 * mortality_available / classified_pathways)

medians <- joined %>%
  group_by(approach, cmt_axis, portfolio_rule, Region, Ambition, Pathway) %>%
  summarise(n = n(), median_cumulative_pm25_deaths_mln = median(cumulative_pm25_deaths_mln),
            mean_cumulative_pm25_deaths_mln = mean(cumulative_pm25_deaths_mln), .groups = "drop") %>%
  pivot_wider(names_from = Pathway,
              values_from = c(n, median_cumulative_pm25_deaths_mln, mean_cumulative_pm25_deaths_mln))

within_model <- joined %>%
  group_by(approach, Model, Region, Ambition, Pathway) %>%
  summarise(model_median_mln = median(cumulative_pm25_deaths_mln), .groups = "drop") %>%
  pivot_wider(names_from = Pathway, values_from = model_median_mln) %>%
  filter(!is.na(`High-engineered-CMT`), !is.na(`High-RE`)) %>%
  mutate(re_minus_cmt_mln = `High-RE` - `High-engineered-CMT`) %>%
  group_by(approach, Region, Ambition) %>%
  summarise(n_models = n(), median_model_re_minus_cmt_mln = median(re_minus_cmt_mln),
            n_re_lower = sum(re_minus_cmt_mln < 0), n_re_higher = sum(re_minus_cmt_mln > 0),
            .groups = "drop")

write_csv(coverage, file.path(OUT_DIR, "mortality_reporting_complete_coverage_2020_2100.csv"))
write_csv(medians, file.path(OUT_DIR, "mortality_reporting_complete_medians_2020_2100.csv"))
write_csv(within_model, file.path(OUT_DIR, "mortality_reporting_complete_within_model_2020_2100.csv"))
write_csv(joined, file.path(OUT_DIR, "mortality_reporting_complete_scenario_values_2020_2100.csv"))
writeLines(c(
  "PM2.5 mortality diagnostic; 2020-2100 trapezoidal integration of decadal RFASST annual deaths",
  "inputs=directly reported R10 PM precursors; no World-to-R10 or NH3 sidecar imputation",
  "interpolation=short bracketed PM temporal gaps permitted (zero values required in this run)",
  "interpretation=coverage-qualified diagnostic; not a full-ensemble causal pathway ranking"
), file.path(OUT_DIR, "MORTALITY_REPORTING_COMPLETE_MANIFEST.txt"))

cat("Reporting-complete World coverage:\n")
print(coverage)
