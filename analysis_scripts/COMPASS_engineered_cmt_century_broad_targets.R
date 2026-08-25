# =============================================================================
# Build the century-long, broad-archetype engineered-CMT mortality universe.
#
# Definition: within each ambition group, top-tercile engineered CMT (Novel CDR
# + Fossil/industrial CCS; land CDR excluded) and not top-tercile renewables,
# versus the converse. This intentionally reproduces the broad-archetype rule
# used in the August deck, while replacing Total CDR with engineered CMT.
# =============================================================================

suppressPackageStartupMessages(library(tidyverse))

BASE_OUT <- "C:/Users/camwe/Documents/Codex/2026-08-22/i-a/outputs/master_nh3infill_central"
OUT_DIR <- "C:/Users/camwe/Documents/Codex/2026-08-22/i-a/outputs/engineered_cmt_century_broad"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

normalise_id <- function(x) {
  x %>% enc2utf8() %>% gsub("\\u00b0", "°", .) %>% gsub("�", "°", ., fixed = TRUE)
}

build_labels <- function(approach) {
  cdr <- read_csv(file.path(BASE_OUT, paste0("approach_", approach),
                            paste0("compass_cdr_cumulative_", approach, ".csv")),
                  show_col_types = FALSE)
  scenario_set <- read_csv(file.path(BASE_OUT, paste0("approach_", approach),
                                     paste0("compass_scenario_set_", approach, ".csv")),
                           show_col_types = FALSE)
  metrics <- cdr %>%
    filter(Variable %in% c("Novel CDR", "Fossil CCS", "Renewable Capacity")) %>%
    group_by(Model, Scenario, Category, Variable) %>%
    summarise(value = sum(Total_Value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Variable, values_from = value, values_fill = 0) %>%
    rename(novel_cdr = `Novel CDR`, fossil_ccs = `Fossil CCS`, renewables = `Renewable Capacity`) %>%
    mutate(engineered_cmt = novel_cdr + fossil_ccs)

  scenario_set %>%
    select(Model, Scenario, Category, Ambition) %>% distinct() %>%
    inner_join(metrics, by = c("Model", "Scenario", "Category")) %>%
    group_by(Ambition) %>%
    mutate(cmt_high = quantile(engineered_cmt, 2 / 3, na.rm = TRUE, type = 7),
           re_high = quantile(renewables, 2 / 3, na.rm = TRUE, type = 7),
           high_cmt = engineered_cmt >= cmt_high,
           high_re = renewables >= re_high,
           Pathway = case_when(
             high_cmt & !high_re ~ "High-engineered-CMT",
             high_re & !high_cmt ~ "High-RE",
             TRUE ~ NA_character_
           ),
           cmt_axis = "Novel CDR + Fossil/industrial CCS (land CDR excluded)",
           portfolio_rule = "top-tercile focal axis; not top-tercile opposing axis") %>%
    ungroup() %>% mutate(approach = approach)
}

labels <- bind_rows(build_labels("A"), build_labels("C")) %>%
  mutate(Model = normalise_id(Model), Scenario = normalise_id(Scenario))
targets <- labels %>% filter(!is.na(Pathway)) %>% distinct(Model, Scenario) %>% arrange(Model, Scenario)
counts <- labels %>% filter(!is.na(Pathway)) %>% count(approach, Ambition, Pathway, name = "n")

write_csv(labels, file.path(OUT_DIR, "engineered_cmt_century_broad_labels.csv"))
write_csv(targets, file.path(OUT_DIR, "engineered_cmt_century_broad_rfasst_targets.csv"))
write_csv(counts, file.path(OUT_DIR, "engineered_cmt_century_broad_counts.csv"))
writeLines(c(
  "classification horizon=2020-2100 cumulative deployment",
  "outcome horizon=2020-2100 mortality",
  "CMT axis=Novel CDR + Fossil/industrial CCS; land-based CDR excluded",
  "portfolio rule=top-tercile focal axis; not top-tercile opposing axis",
  "mortality input policy=direct R10 reporting with only short bracketed temporal interpolation"
), file.path(OUT_DIR, "RUN_MANIFEST.txt"))

cat("Wrote", nrow(targets), "unique mortality targets to", OUT_DIR, "\n")
print(counts)
