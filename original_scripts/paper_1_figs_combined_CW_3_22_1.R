# =============================================================================
# COMPASS COMMITTEE PRESENTATION FIGURES
# PhD Dissertation Paper 1
#
# DATA SOURCES (all read directly from saved outputs — no recomputation):
#   compass_interp.rds          <- COMPASS_data_collection_CW_3_18_revised.R
#   compass_cdr_cumulative.rds  <- COMPASS_full_analysis_revised.R
#   compass_master_dataset.rds  <- COMPASS_full_analysis_revised.R
#   compass_pathway_tercile.rds <- COMPASS_full_analysis_revised.R
#   compass_mortality_r10.csv   <- rfasst mortality script
#
# RUN ORDER:
#   1. COMPASS_data_collection_CW_3_18_revised.R
#   2. rfasst mortality script
#   3. COMPASS_full_analysis_revised.R
#   4. THIS script
# =============================================================================

library(tidyverse)
library(scales)
library(patchwork)
library(broom)

# =============================================================================
# SECTION 0: PATHS
# =============================================================================

COMPASS_DIR <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/COMPASS"
OUT_DIR     <- COMPASS_DIR
FIG_OUT     <- file.path(COMPASS_DIR, "Figures", "placeholder.png")

dir.create(dirname(FIG_OUT), showWarnings = FALSE, recursive = TRUE)
cat("Paths set. Loading data from:", COMPASS_DIR, "\n")

# =============================================================================
# SECTION 1: LOAD ALL DATA FROM SAVED RDS/CSV FILES
# =============================================================================

cat("\n=== Loading saved outputs ===\n")

# --- From COMPASS_data_collection_CW_3_18_revised.R ---
compass_interp <- readRDS(file.path(COMPASS_DIR, "compass_interp.rds"))
cat("  compass_interp rows:", nrow(compass_interp), "\n")

# --- From COMPASS_full_analysis_revised.R ---
df_master <- readRDS(file.path(COMPASS_DIR, "compass_master_dataset.rds"))
cat("  df_master rows:", nrow(df_master),
    "| scenarios:", n_distinct(paste(df_master$Model, df_master$Scenario)), "\n")

cdr_cumulative <- readRDS(file.path(COMPASS_DIR, "compass_cdr_cumulative.rds"))
cat("  cdr_cumulative rows:", nrow(cdr_cumulative), "\n")

pathway_tercile  <- readRDS(file.path(COMPASS_DIR, "compass_pathway_tercile.rds"))
pathway_quartile <- readRDS(file.path(COMPASS_DIR, "compass_pathway_quartile.rds"))
pathway_threshold <- readRDS(file.path(COMPASS_DIR, "compass_pathway_threshold.rds"))
cat("  pathway_tercile rows:", nrow(pathway_tercile), "\n")
cat("  pathway_threshold rows:", nrow(pathway_threshold), "\n")

# --- From rfasst mortality script ---
mort_r10_path <- file.path(COMPASS_DIR, "compass_mortality_r10.csv")
if (file.exists(mort_r10_path)) {
  mortality_r10_raw <- read_csv(mort_r10_path, show_col_types = FALSE)
  # Standardise column names
  if ("model" %in% names(mortality_r10_raw) && !"Model" %in% names(mortality_r10_raw))
    mortality_r10_raw <- rename(mortality_r10_raw, Model = model, Scenario = scenario)
  if ("r10_region" %in% names(mortality_r10_raw) && !"Region" %in% names(mortality_r10_raw))
    mortality_r10_raw <- rename(mortality_r10_raw, Region = r10_region)
  if ("year" %in% names(mortality_r10_raw) && !"Year" %in% names(mortality_r10_raw))
    mortality_r10_raw <- rename(mortality_r10_raw, Year = year)
  mortality_r10_raw <- mortality_r10_raw %>%
    select(-any_of(c("model","scenario","year","r10_region")))
  cat("  mortality_r10_raw rows:", nrow(mortality_r10_raw), "\n")
  cat("  mortality_r10_raw cols:", paste(names(mortality_r10_raw), collapse=", "), "\n")
} else {
  warning("compass_mortality_r10.csv not found — P4b/P4c will be skipped")
  mortality_r10_raw <- tibble()
}

# =============================================================================
# SECTION 2: DERIVE WORKING OBJECTS FROM LOADED DATA
# =============================================================================

cat("\n=== Deriving working objects ===\n")

regions_r10     <- c("R10AFRICA", "R10CHINA+", "R10EUROPE",
                     "R10INDIA+", "R10NORTH_AM")
categories_keep <- c("C1", "C2", "C3", "C4")

# compass_filtered: R10 timeseries used for trend figures (P2, P3)
compass_filtered <- compass_interp %>%
  filter(Region   %in% regions_r10,
         Category %in% categories_keep,
         Year >= 2020, Year <= 2100,
         !is.na(Value)) %>%
  mutate(Model_Group         = "COMPASS",
         ModelGroup_Scenario = paste("COMPASS", Scenario, sep = "_"))

cat("  compass_filtered rows:", nrow(compass_filtered), "\n")

# compass_with_total: add proxy flag column for backwards compatibility
compass_with_total <- compass_filtered %>% mutate(proxy = FALSE)

# world_cum_f2: pathway classification — read directly from tercile file
# (top tercile, mutually exclusive High-CDR / High-RE)
world_cum_f2 <- pathway_tercile %>%
  select(Model, Scenario, Category, Ambition,
         total_cdr, total_re,
         high_cdr_only, high_re_only,
         Pathway_overlap, Pathway_excl,
         threshold_label)

high_scenarios_f2 <- world_cum_f2 %>%
  filter(high_cdr_only | high_re_only)

cat("  High-CDR-only:", sum(world_cum_f2$high_cdr_only, na.rm=TRUE), "\n")
cat("  High-RE-only: ", sum(world_cum_f2$high_re_only,  na.rm=TRUE), "\n")

# world_cum_thresh: absolute threshold classification (parallel to world_cum_f2)
world_cum_thresh <- pathway_threshold %>%
  select(Model, Scenario, Category, Ambition,
         total_cdr, total_re,
         high_cdr_only, high_re_only,
         Pathway_overlap, Pathway_excl,
         threshold_label)

high_scenarios_thresh <- world_cum_thresh %>%
  filter(high_cdr_only | high_re_only)

cat("  [Threshold] High-CDR-only:", sum(world_cum_thresh$high_cdr_only, na.rm=TRUE), "\n")
cat("  [Threshold] High-RE-only: ", sum(world_cum_thresh$high_re_only,  na.rm=TRUE), "\n")

# =============================================================================
# ALTERNATIVE PATHWAY CLASSIFICATIONS — B (overlapping) and C (ratio)
#
# Run in parallel with the original (A: mutually exclusive top tercile).
# world_cum_f2 and all original figures are UNCHANGED.
# New objects world_cum_overlap (B) and world_cum_ratio (C) are used only
# in the comparison figures appended at the end of the script.
#
# Option B — Overlapping top tercile:
#   high_cdr = top tercile of total_cdr  (regardless of RE level)
#   high_re  = top tercile of total_re   (regardless of CDR level)
#   Scenarios in both groups are assigned to whichever dimension is
#   relatively stronger (standardised z-score within ambition group).
#
# Option C — CDR ratio (top/bottom tercile of CDR share):
#   cdr_ratio = total_cdr / (total_cdr + total_re)
#   High-CDR  = top tercile of ratio  (CDR-dominant)
#   High-RE   = bottom tercile of ratio (RE-dominant)
#   No mutual exclusivity issue by construction.
# =============================================================================
cat("\n--- Building alternative pathway classifications ---\n")

# ── Option B: Overlapping top tercile ────────────────────────────────────────
world_cum_overlap <- pathway_tercile %>%
  select(Model, Scenario, Category, Ambition, total_cdr, total_re,
         high_cdr_only, high_re_only) %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition)) %>%
  group_by(Ambition) %>%
  mutate(
    high_cdr_B = ntile(total_cdr, 3) == 3,
    high_re_B  = ntile(total_re,  3) == 3,
    # For scenarios in both, assign to whichever is relatively stronger
    cdr_z      = (total_cdr - mean(total_cdr, na.rm = TRUE)) /
      sd(total_cdr, na.rm = TRUE),
    re_z       = (total_re  - mean(total_re,  na.rm = TRUE)) /
      sd(total_re,  na.rm = TRUE),
    Pathway_B  = case_when(
      high_cdr_B & !high_re_B               ~ "High-CDR",
      high_re_B  & !high_cdr_B              ~ "High-RE",
      high_cdr_B &  high_re_B & cdr_z >= re_z ~ "High-CDR",
      high_cdr_B &  high_re_B & cdr_z <  re_z ~ "High-RE",
      TRUE                                   ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(Pathway_B))

cat("Option B (overlapping, Both assigned by relative z-score):\n")
world_cum_overlap %>% count(Ambition, Pathway_B) %>% print()

# ── Option C: CDR ratio ───────────────────────────────────────────────────────
world_cum_ratio <- pathway_tercile %>%
  select(Model, Scenario, Category, Ambition, total_cdr, total_re) %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition),
         !is.na(total_cdr), !is.na(total_re),
         (total_cdr + total_re) > 0) %>%
  group_by(Ambition) %>%
  mutate(
    cdr_ratio  = total_cdr / (total_cdr + total_re),
    ratio_tile = ntile(cdr_ratio, 3),
    Pathway_C  = case_when(
      ratio_tile == 3 ~ "High-CDR",   # top tercile of CDR share
      ratio_tile == 1 ~ "High-RE",    # bottom tercile of CDR share
      TRUE            ~ NA_character_ # middle tercile dropped
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(Pathway_C))

cat("Option C (CDR ratio tercile):\n")
world_cum_ratio %>% count(Ambition, Pathway_C) %>% print()

# ── Comparison summary ────────────────────────────────────────────────────────
cat("\n--- Classification n= comparison ---\n")
bind_rows(
  world_cum_f2 %>%
    filter(high_cdr_only | high_re_only) %>%
    mutate(Pathway = if_else(high_cdr_only, "High-CDR", "High-RE"),
           Method  = "A: Excl. tercile") %>%
    count(Method, Ambition, Pathway),
  world_cum_overlap %>%
    mutate(Method = "B: Overlap tercile") %>%
    count(Method, Ambition, Pathway = Pathway_B),
  world_cum_ratio %>%
    mutate(Method = "C: CDR ratio") %>%
    count(Method, Ambition, Pathway = Pathway_C)
) %>%
  arrange(Ambition, Pathway, Method) %>%
  print(n = Inf)

# mortality_summary: cumulative summary (used where annual timeseries not needed)
mort_summary_path <- file.path(COMPASS_DIR, "compass_mortality_summary.rds")
if (file.exists(mort_summary_path)) {
  mortality_summary <- readRDS(mort_summary_path)
  if ("model" %in% names(mortality_summary) && !"Model" %in% names(mortality_summary))
    mortality_summary <- rename(mortality_summary, Model=model, Scenario=scenario)
  if ("r10_region" %in% names(mortality_summary) && !"Region" %in% names(mortality_summary))
    mortality_summary <- rename(mortality_summary, Region=r10_region)
  mortality_summary <- mortality_summary %>%
    select(-any_of(c("model","scenario","r10_region")))
  cat("  mortality_summary rows:", nrow(mortality_summary), "\n")
} else {
  mortality_summary <- tibble()
  warning("compass_mortality_summary.rds not found")
}

# =============================================================================
# SECTION 3: CONSTANTS — LABELS, COLOURS, THEMES, HELPERS
# =============================================================================

REGION_LABELS_FIG <- c(
  "R10AFRICA"              = "Africa",
  "R10CHINA+"              = "China+",
  "R10EUROPE"              = "Europe",
  "R10INDIA+"              = "India+",
  "R10NORTH_AM"            = "N. America",
  "Aggregated R10 regions" = "Aggregated R10 regions"
)

PATHWAY_COLORS       <- c("High-CDR" = "#2166ac", "High-RE" = "#d6604d")
PATHWAY_COLORS_SIMPLE <- PATHWAY_COLORS

theme_paper <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(strip.background = element_rect(fill = "#1c3a5e", colour = NA),
          strip.text       = element_text(colour = "white", face = "bold"),
          legend.position  = "bottom",
          panel.grid.minor = element_blank(),
          plot.title       = element_text(face = "bold", hjust = 0, size = 13))
}

sf2 <- function(x, digits = 2) signif(x, digits)

# add_pathway: joins pathway classification from world_cum_f2
# Ambition is taken from world_cum_f2 (not the input df) to avoid missing col
add_pathway <- function(df) {
  df %>%
    left_join(world_cum_f2 %>%
                select(Model, Scenario,
                       high_cdr_only, high_re_only),
              by = c("Model", "Scenario")) %>%
    filter(!is.na(high_cdr_only), high_cdr_only | high_re_only) %>%
    assign_amb("Category") %>%
    filter(!is.na(Ambition)) %>%
    mutate(Pathway = if_else(high_cdr_only, "High-CDR", "High-RE"))
}

# add_pathway_thresh: same but using absolute threshold classification
add_pathway_thresh <- function(df) {
  df %>%
    left_join(world_cum_thresh %>%
                select(Model, Scenario,
                       high_cdr_only, high_re_only),
              by = c("Model", "Scenario")) %>%
    filter(!is.na(high_cdr_only), high_cdr_only | high_re_only) %>%
    assign_amb("Category") %>%
    filter(!is.na(Ambition)) %>%
    mutate(Pathway = if_else(high_cdr_only, "High-CDR", "High-RE"))
}

# assign_amb: ambition grouping C1+C2=1.5C, C3+C4=2C
assign_amb <- function(df, col = "Category") {
  df %>% mutate(Ambition = case_when(
    .data[[col]] %in% c("C1","C2") ~ "1.5C (High-Ambition)",
    .data[[col]] %in% c("C3","C4") ~ "2C (Medium-Ambition)",
    TRUE ~ NA_character_
  ))
}

# Wellbeing outcome cumulation windows — ambition-specific:
#   1.5C (C1+C2): 2020-2060  (C2 high-overshoot median net-zero ~2060)
#   2C   (C3+C4): 2020-2075  (C3+C4 median net-zero ~2070-2075 per IPCC AR6)
WINDOW_15C <- 2060L
WINDOW_2C  <- 2075L
get_cutoff <- function(amb) if_else(str_detect(amb, "1.5C"), WINDOW_15C, WINDOW_2C)

# =============================================================================
# SECTION 4: VALIDATION
# =============================================================================

cat("\n=== DATA LOAD COMPLETE — Validation ===\n")

required_objects <- c(
  "compass_filtered", "compass_with_total", "cdr_cumulative",
  "df_master", "world_cum_f2", "high_scenarios_f2",
  "mortality_r10_raw", "mortality_summary",
  "regions_r10", "REGION_LABELS_FIG",
  "PATHWAY_COLORS", "PATHWAY_COLORS_SIMPLE",
  "FIG_OUT", "theme_paper", "sf2", "add_pathway", "assign_amb",
  "WINDOW_15C", "WINDOW_2C", "get_cutoff"
)

present <- sapply(required_objects, exists)
if (all(present)) {
  cat("All required objects present. Proceeding to figures.\n")
} else {
  cat("MISSING:", paste(names(present)[!present], collapse=", "), "\n")
}


# =============================================================================
# COMPASS COMMITTEE PRESENTATION — FINAL FIGURE SCRIPT
# PhD Dissertation Committee Presentation
#
# CUMULATION WINDOWS:
#   CDR + RE deployment : 2020–2100 (full decarbonisation period)
#   Wellbeing outcomes  : 2020 → median net-zero year per ambition group
#                         (computed from scenario_netzero, averaged across
#                          High-CDR and High-RE within each ambition group)
#
# NARRATIVE ORDER:
#   Section 1 — What is in COMPASS?
#   Section 2 — Variables used and their trends (all scenarios)
#   Section 3 — Trends by pathway type (High-CDR vs High-RE)
#   Section 4 — Wellbeing outcomes by pathway type
#   Section 5 — Equity + robustness + takeaways
#
# REQUIRES (in environment):
#   compass_filtered, compass_with_total, cdr_cumulative, df_master
#   world_cum_f2, high_scenarios_f2, mortality_r10_raw
#   scenario_netzero (or will be computed)
#   regions_r10, REGION_LABELS_FIG, PATHWAY_COLORS, PATHWAY_COLORS_SIMPLE
#   theme_paper(), sf2(), FIG_OUT
#   compute_spearman_fig8() from figF2_replicate.R
# =============================================================================

library(tidyverse)
library(patchwork)
library(scales)
library(broom)

cat("=== COMPASS Committee Presentation Figures ===\n")

# Output directory
FIG_COMM <- file.path(dirname(FIG_OUT), "Committee")
dir.create(FIG_COMM, showWarnings = FALSE, recursive = TRUE)
cat("Saving to:", FIG_COMM, "\n")

sc <- function(p, name, w = 14, h = 8) {
  ggsave(file.path(FIG_COMM, name), p,
         width = w, height = h, dpi = 300, bg = "white")
  message("Saved: ", name)
}

# ---- Shared aesthetics ------------------------------------------------------
theme_c <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(
      strip.background   = element_rect(fill = "#1c3a5e", colour = NA),
      strip.text         = element_text(colour = "white", face = "bold",
                                        size = 10),
      strip.text.y       = element_text(angle = -90),
      legend.position    = "bottom",
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title    = element_text(face = "bold", hjust = 0, size = 14),
      plot.subtitle = element_text(hjust = 0, colour = "grey40", size = 10),
      plot.caption  = element_text(colour = "grey50", hjust = 0, size = 8),
      panel.spacing = unit(0.4, "lines")
    )
}

PCOLS  <- c("High-CDR" = "#2166ac", "High-RE" = "#d6604d")
ACOLS  <- c("1.5C (High-Ambition)" = "#1a9641",
            "2C (Medium-Ambition)"  = "#fdae61")
CCOLS  <- c("C1"="#1a9641","C2"="#74c476","C3"="#fdae61","C4"="#d7191c")
CCDR   <- c("Novel CDR"="#2166ac","Fossil CCS"="#636363",
            "Land-based CDR"="#74c476")
PELEC  <- c("Coal"="black","Gas"="#969696","Oil"="darkred",
            "Biomass"="#74c476","Nuclear"="orange",
            "Hydro"="darkblue","Wind"="lightblue","Solar"="#f7c948", "Geothermal" = "purple")

simplify_model <- function(m) {
  case_when(
    str_detect(m,"REMIND")  ~ "REMIND",
    str_detect(m,"MESSAGE") ~ "MESSAGE",
    str_detect(m,"IMAGE")   ~ "IMAGE",
    str_detect(m,"WITCH")   ~ "WITCH",
    str_detect(m,"COFFEE")  ~ "COFFEE",
    str_detect(m,"AIM")     ~ "AIM",
    str_detect(m,"POLES")   ~ "POLES",
    str_detect(m,"GCAM")    ~ "GCAM",
    str_detect(m,"GEM")     ~ "GEM-E3",
    str_detect(m,"TIAM")    ~ "TIAM",
    str_detect(m,"EPPA")    ~ "EPPA",
    TRUE ~ "Other"
  )
}

assign_amb <- function(df, col = "Category") {
  df %>% mutate(Ambition = case_when(
    .data[[col]] %in% c("C1","C2") ~ "1.5C (High-Ambition)",
    .data[[col]] %in% c("C3","C4") ~ "2C (Medium-Ambition)",
    TRUE ~ NA_character_
  ))
}

# add_pathway already defined above in preamble

# =============================================================================
# COMPUTE NET-ZERO CUTOFFS
# =============================================================================
cat("\n--- Computing net-zero cutoffs ---\n")

if (!exists("scenario_netzero")) {
  scenario_netzero <- compass_filtered %>%
    filter(Variable %in% c("Emissions|CO2","CO2 Emissions"),
           Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
    group_by(Model, Scenario, Category, Year) %>%
    summarise(co2 = sum(Value, na.rm=TRUE), .groups="drop") %>%
    group_by(Model, Scenario, Category) %>%
    arrange(Year) %>%
    summarise(
      netzero_year = { yr <- Year[co2 <= 0]
      if (length(yr) > 0) min(yr) else NA_integer_ },
      .groups = "drop"
    )
}

# Median net-zero per ambition group (averaged across High-CDR and High-RE)
nz_by_group <- scenario_netzero %>%
  add_pathway() %>%
  filter(!is.na(netzero_year)) %>%
  group_by(Ambition, Pathway) %>%
  summarise(med_nz = median(netzero_year, na.rm=TRUE), .groups="drop")

# Single cutoff per ambition = average of High-CDR and High-RE medians
nz_cutoffs <- nz_by_group %>%
  group_by(Ambition) %>%
  summarise(cutoff = round(mean(med_nz)), .groups="drop")

cat("Net-zero cutoffs:\n")
print(nz_cutoffs)
print(nz_by_group)

NZ_15 <- nz_cutoffs %>% filter(str_detect(Ambition,"1.5C")) %>% pull(cutoff)
NZ_2C <- nz_cutoffs %>% filter(str_detect(Ambition,"2C"))   %>% pull(cutoff)

# Snap net-zero cutoffs to nearest 10-year mortality snapshot
mort_years <- seq(2010, 2100, 10)
snap_year  <- function(yr) mort_years[which.min(abs(mort_years - yr))]
NZ_15_snap <- snap_year(NZ_15)
NZ_2C_snap <- snap_year(NZ_2C)
cat(sprintf("Raw net-zero cutoffs: 1.5C = %d  |  2C = %d\n", NZ_15, NZ_2C))

# mortality_r10_raw only has 10-year snapshots (2010,2020,...,2100)
# Wellbeing outcome windows: 1.5C → 2060, 2C → 2075.
# NZ_15_snap / NZ_2C_snap used for mortality trajectory reference lines (P4b).

# Helper: get cutoff year — standardised 2020-2050 for all ambition groups
get_cutoff <- function(amb) 2050L

# =============================================================================
# BUILD WELLBEING OUTCOMES AT NET-ZERO CUTOFF
# =============================================================================
cat("\n--- Building wellbeing outcomes to net-zero cutoff ---\n")

# Mortality cumulated to net-zero cutoff per ambition
# mortality_r10_raw is already standardised to Model/Scenario/Region/Year/deaths_pm25
mort_to_nz <- mortality_r10_raw %>%
  filter(Region %in% regions_r10, Year >= 2020) %>%
  # Join pathway classification directly — avoids add_pathway() which needs Category
  left_join(
    world_cum_f2 %>%
      select(Model, Scenario, Category, Ambition, high_cdr_only, high_re_only),
    by = c("Model", "Scenario")
  ) %>%
  filter(!is.na(high_cdr_only), high_cdr_only | high_re_only,
         !is.na(Ambition)) %>%
  mutate(
    Pathway = if_else(high_cdr_only, "High-CDR", "High-RE"),
    cutoff  = if_else(str_detect(Ambition, "1.5C"), WINDOW_15C, WINDOW_2C)
  ) %>%
  filter(Year <= cutoff) %>%
  group_by(Model, Scenario, Pathway, Ambition, Region) %>%
  summarise(
    cum_deaths_nz = sum(deaths_pm25 * 5, na.rm = TRUE) / 1e6,
    .groups = "drop"
  )

cat("mort_to_nz rows:", nrow(mort_to_nz),
    "| pathways:", paste(sort(unique(mort_to_nz$Pathway)), collapse=", "),
    "| regions:", n_distinct(mort_to_nz$Region), "\n")

# df_master outcomes — re-compute cumulative outcomes to net-zero cutoff
# using annual timeseries from compass_filtered
dle_to_nz <- compass_filtered %>%
  filter(Variable %in% c("Final Energy","Population"),
         Region %in% regions_r10, Year >= 2020) %>%
  add_pathway() %>%
  mutate(cutoff = get_cutoff(Ambition)) %>%
  filter(Year <= cutoff)

# Jobs — already cumulative in df_master; scale by cutoff/2100 fraction
# Use the df_master values but join cutoff-scaled mortality as the mortality outcome
# Population timeseries — needed for per-capita normalization in P4a
# Build from compass_filtered if not already in environment
if (!exists("pop_ts")) {
  pop_ts <- compass_filtered %>%
    filter(Variable == "Population", Region %in% regions_r10) %>%
    select(Model, Scenario, Region, Year, Value)
  cat("pop_ts built:", nrow(pop_ts), "rows\n")
}

outcomes_nz <- df_master %>%
  filter(Variable == "Total CDR", Region %in% regions_r10) %>%
  add_pathway() %>%
  mutate(cutoff = get_cutoff(Ambition)) %>%
  left_join(mort_to_nz %>%
              select(Model, Scenario, Region,
                     cum_deaths_nz),
            by = c("Model","Scenario","Region")) %>%
  select(Model, Scenario, Pathway, Ambition, Region, cutoff,
         cum_deaths_nz,
         cumulative_gap_EJ,
         mean_headcount_millions,
         cumulative_implied_CO2_GtCO2,
         jobs_Fossil,
         jobs_Renewables) %>%
  left_join(
    pop_ts %>%
      filter(Region %in% regions_r10, Year == 2020) %>%
      group_by(Region) %>%
      summarise(pop_mln = median(Value, na.rm = TRUE), .groups = "drop"),
    by = "Region"
  ) %>%
  mutate(
    # Normalise outcomes to per million 2020 population
    cum_deaths_nz             = (cum_deaths_nz * 1e6)          / pop_mln,
    mean_headcount_millions   = (mean_headcount_millions * 1e6) / pop_mln,
    jobs_Renewables           = jobs_Renewables                 / pop_mln,
    jobs_Fossil               = jobs_Fossil                     / pop_mln,
    Region_label = REGION_LABELS_FIG[Region],
    Group = paste0(Pathway, "\n", Ambition),
    Group = factor(Group, levels = c(
      "High-CDR\n1.5C (High-Ambition)",
      "High-RE\n1.5C (High-Ambition)",
      "High-CDR\n2C (Medium-Ambition)",
      "High-RE\n2C (Medium-Ambition)"
    ))
  )

cat("Outcomes to net-zero rows:", nrow(outcomes_nz), "\n")

# Aggregated R10 regions — population-weighted aggregate from df_master
outcomes_nz_world <- df_master %>%
  filter(Variable == "Total CDR", Region == "Aggregated R10 regions") %>%
  add_pathway() %>%
  mutate(cutoff = get_cutoff(Ambition)) %>%
  left_join(mort_to_nz %>%
              group_by(Model, Scenario, Pathway, Ambition) %>%
              summarise(cum_deaths_nz = sum(cum_deaths_nz, na.rm=TRUE),
                        .groups="drop"),
            by = c("Model","Scenario","Pathway","Ambition")) %>%
  select(Model, Scenario, Pathway, Ambition, cutoff, cum_deaths_nz,
         cumulative_gap_EJ, mean_headcount_millions,
         cumulative_implied_CO2_GtCO2, jobs_Fossil, jobs_Renewables) %>%
  left_join(
    pop_ts %>%
      filter(Region %in% regions_r10, Year == 2020) %>%
      group_by(Region) %>%
      summarise(pop_mln = median(Value, na.rm = TRUE), .groups = "drop") %>%
      summarise(pop_agg = sum(pop_mln)),
    by = character()
  ) %>%
  mutate(
    # Normalise outcomes to per million 2020 population
    cum_deaths_nz             = (cum_deaths_nz * 1e6)      / pop_agg,
    mean_headcount_millions   = (mean_headcount_millions * 1e6) / pop_agg,
    jobs_Renewables           = jobs_Renewables             / pop_agg,
    jobs_Fossil               = jobs_Fossil                 / pop_agg,
    Region_label = "Aggregated R10 regions",
    Group = paste0(Pathway, "\n", Ambition),
    Group = factor(Group, levels = c(
      "High-CDR\n1.5C (High-Ambition)",
      "High-RE\n1.5C (High-Ambition)",
      "High-CDR\n2C (Medium-Ambition)",
      "High-RE\n2C (Medium-Ambition)"
    ))
  )

# =============================================================================
# SECTION 1: WHAT IS IN COMPASS?
# =============================================================================
cat("\n=== SECTION 1: What is in COMPASS? ===\n")

# P1a — Scenario counts by model family × category
p1a_dat <- compass_filtered %>%
  distinct(Model, Scenario, Category) %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition)) %>%
  mutate(Model_family = simplify_model(Model)) %>%
  count(Model_family, Category, Ambition)

p1a <- ggplot(p1a_dat,
              aes(x = reorder(Model_family, -n, sum),
                  y = n, fill = Category)) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.3) +
  facet_wrap(~ Ambition, ncol = 2, scales = "free_x") +
  scale_fill_manual(values = CCOLS) +
  labs(title    = "P1a: COMPASS Scenario Coverage",
       subtitle = "Unique Model × Scenario combinations by model family and climate category",
       x = NULL, y = "Number of scenarios", fill = "Category",
       caption = paste0(
         "C1 = 1.5°C no/limited overshoot  ·  ",
         "C2 = 1.5°C high overshoot  ·  ",
         "C3 = below 2°C  ·  C4 = below 3°C\n",
         "1,467 total scenarios across 11 model families.")) +
  theme_c() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

sc(p1a, "P1a_scenario_coverage.png", 14, 7)

# P1a (pathway) — identical layout to original P1a but filtered to High-CDR and
# High-RE scenarios only. Two panels side by side = Ambition (1.5C | 2C).
# Fill = Category (C1/C2/C3/C4), same colours as original.
p1a_path_dat <- compass_filtered %>%
  distinct(Model, Scenario, Category) %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition)) %>%
  left_join(
    world_cum_f2 %>%
      mutate(Pathway = case_when(
        high_cdr_only ~ "High-CDR",
        high_re_only  ~ "High-RE",
        TRUE          ~ NA_character_)) %>%
      select(Model, Scenario, Pathway),
    by = c("Model", "Scenario")) %>%
  filter(!is.na(Pathway)) %>%
  mutate(Model_family = simplify_model(Model)) %>%
  count(Model_family, Category, Ambition)

p1a_path <- ggplot(p1a_path_dat,
                   aes(x = reorder(Model_family, -n, sum),
                       y = n, fill = Category)) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.3) +
  facet_wrap(~ Ambition, ncol = 2, scales = "free_x") +
  scale_fill_manual(values = CCOLS) +
  labs(
    title    = "P1a (pathway): Scenario Coverage — High-CDR and High-RE only",
    subtitle = "Unique Model x Scenario combinations after mutually exclusive pathway filtering",
    x = NULL, y = "Number of scenarios", fill = "Category",
    caption  = paste0(
      "C1 = 1.5 degrees no/limited overshoot  .  C2 = 1.5 degrees high overshoot  .  ",
      "C3 = below 2 degrees  .  C4 = below 3 degrees\n",
      "1.5C: 59 High-CDR + 68 High-RE = 127 scenarios  .  ",
      "2C: 209 High-CDR + 228 High-RE = 437 scenarios"
    )
  ) +
  theme_c() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

sc(p1a_path, "P1a_pathway_scenario_coverage.png", 14, 7)

# P1b — Total CDR + Renewable Capacity distributions side by side
# Total CDR: from compass_cum_ccs scalar (GtCO2, proxy=FALSE)
p1b_cdr <- cdr_cumulative %>%
  filter(Variable == "Total CDR") %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition)) %>%
  mutate(Model_family = simplify_model(Model))

# Renewable Capacity: proxy=TRUE (EJ-based), sum across R10 regions
p1b_re <- cdr_cumulative %>%
  filter(Variable == "Renewable Capacity", Region %in% regions_r10) %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition)) %>%
  group_by(Model, Scenario, Category, Ambition) %>%
  summarise(Total_Value = sum(Total_Value, na.rm=TRUE), .groups="drop") %>%
  mutate(Model_family = simplify_model(Model))

cat("  p1b_cdr rows:", nrow(p1b_cdr), "| p1b_re rows:", nrow(p1b_re), "\n")

pb_cdr <- ggplot(p1b_cdr,
                 aes(x = reorder(Model_family, Total_Value, median),
                     y = Total_Value / 1e3, fill = Ambition)) +
  geom_boxplot(alpha = 0.65, outlier.size = 0.4, linewidth = 0.4) +
  coord_flip() +
  facet_wrap(~ Ambition, scales = "free_x") +
  scale_fill_manual(values = ACOLS, guide = "none") +
  labs(title = "Total CDR (World, cumul. 2020-2100)",
       x = NULL, y = "TtCO2") +
  theme_c(10)

pb_re <- ggplot(p1b_re,
                aes(x = reorder(Model_family, Total_Value, median),
                    y = Total_Value / 1e3, fill = Ambition)) +
  geom_boxplot(alpha = 0.65, outlier.size = 0.4, linewidth = 0.4) +
  coord_flip() +
  facet_wrap(~ Ambition, scales = "free_x") +
  scale_fill_manual(values = ACOLS, guide = "none") +
  labs(title = "Renewable Capacity (mean R10, cumul. 2020-2100)",
       x = NULL, y = "Thousands of GW·yr") +
  theme_c(10)

p1b <- pb_cdr + pb_re +
  plot_annotation(
    title    = "P1b: CDR and RE Deployment by Model Family",
    subtitle = "Cumulative 2020-2100  ·  Distribution across all scenarios",
    caption  = "Left: Total CDR (aggregated R10 regions, cumul. 2020-2100). Right: Renewable Capacity, sum across R10 regions (cumul. 2020-2100).",
    theme = theme_c()
  )

sc(p1b, "P1b_deployment_by_model.png", 16, 9)

# P1c — CDR component coverage: which models report each CDR component type
# Uses compass_interp which has the pre-computed composite variables
p1c_dat <- compass_interp %>%
  filter(Variable %in% c("Novel CDR","Fossil CCS","Land-based CDR"),
         Region %in% regions_r10,
         Year == 2050, !is.na(Value), Value > 0) %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition)) %>%
  mutate(
    Model_family = simplify_model(Model),
    CDR_type     = factor(Variable,
                          levels = c("Novel CDR","Fossil CCS",
                                     "Land-based CDR"))
  ) %>%
  distinct(Model_family, CDR_type, Model, Scenario) %>%
  count(Model_family, CDR_type, name = "n_scenarios")

p1c <- ggplot(p1c_dat,
              aes(x = reorder(Model_family, n_scenarios, sum),
                  y = n_scenarios, fill = CDR_type)) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.3) +
  coord_flip() +
  scale_fill_manual(values = CCDR, name = "CDR type") +
  labs(
    title    = "P1c: CDR Component Coverage by Model Family",
    subtitle = "Number of scenarios reporting non-zero values at 2050 for each CDR component type",
    x = NULL, y = "Scenarios with non-zero CDR in 2050",
    caption  = paste0(
      "Novel CDR = DAC + BECCS + Enhanced Weathering  ·  ",
      "Fossil CCS = Fossil energy + industrial CCS  ·  ",
      "Land-based = Carbon Removal|Land Use"
    )
  ) +
  theme_c()

sc(p1c, "P1c_CDR_component_coverage.png", 14, 8)

# =============================================================================
# SECTION 2: VARIABLE TRENDS (ALL SCENARIOS)
# =============================================================================
cat("\n=== SECTION 2: Variable trends (all scenarios) ===\n")

# P2a — Annual CDR and RE trends world (all scenarios, median + IQR ribbon)
p2a_cdr <- compass_filtered %>%
  filter(Variable == "Total CDR",
         Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  group_by(Model, Scenario, Category, Year) %>%
  summarise(Value = sum(Value, na.rm=TRUE), .groups="drop") %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition)) %>%
  group_by(Ambition, Year) %>%
  summarise(
    med = median(Value, na.rm=TRUE),
    q25 = quantile(Value, 0.25, na.rm=TRUE),
    q75 = quantile(Value, 0.75, na.rm=TRUE),
    .groups="drop"
  )

p2a_re <- compass_filtered %>%
  filter(Variable %in% c("Capacity|Electricity|Solar",
                         "Capacity|Electricity|Wind",
                         "Capacity|Electricity|Hydro",
                         "Capacity|Electricity|Nuclear",
                         "Capacity|Electricity|Biomass"),
         Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  group_by(Model, Scenario, Category, Year) %>%
  summarise(Value = sum(Value, na.rm=TRUE), .groups="drop") %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition)) %>%
  group_by(Ambition, Year) %>%
  summarise(
    med = median(Value, na.rm=TRUE),
    q25 = quantile(Value, 0.25, na.rm=TRUE),
    q75 = quantile(Value, 0.75, na.rm=TRUE),
    .groups="drop"
  )

plot_trend <- function(dat, title, ylab, div=1) {
  ggplot(dat, aes(x = Year, colour = Ambition, fill = Ambition)) +
    geom_ribbon(aes(ymin = q25/div, ymax = q75/div),
                alpha = 0.2, colour = NA) +
    geom_line(aes(y = med/div), linewidth = 1.2) +
    scale_colour_manual(values = ACOLS) +
    scale_fill_manual(values   = ACOLS) +
    scale_x_continuous(breaks  = seq(2020,2100,20)) +
    labs(title = title, x = "Year", y = ylab,
         colour = "Ambition", fill = "Ambition") +
    theme_c(10)
}

p2a <- plot_trend(p2a_cdr,
                  "Annual Total CDR (aggregated R10 regions, MtCO2/yr)",
                  "MtCO2/yr") +
  plot_trend(p2a_re,
             "Annual Total Renewable Capacity (Aggregated R10 regions, GW)",
             "GW") +
  plot_annotation(
    title    = "P2a: CDR and Renewable Capacity — Annual Trends (All Scenarios)",
    subtitle = "Median ± IQR across all scenarios within each ambition group  ·  2020-2100",
    caption  = "Solid line = median; shaded band = interquartile range.",
    theme    = theme_c()
  )

sc(p2a, "P2a_CDR_RE_annual_trends.png", 14, 6)

# P2a_reg — same but faceted by R10 region
p2a_cdr_reg <- compass_filtered %>%
  filter(Variable == "Total CDR",
         Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  add_pathway_diag() %>%
  mutate(Region_label = REGION_LABELS_FIG[Region]) %>%
  group_by(Pathway, Ambition, Region_label, Year) %>%
  summarise(med = median(Value, na.rm=T),
            q25 = quantile(Value, .25, na.rm=T),
            q75 = quantile(Value, .75, na.rm=T),
            .groups = "drop")

p2a_re_reg <- compass_filtered %>%
  filter(Variable %in% c("Capacity|Electricity|Solar","Capacity|Electricity|Wind",
                         "Capacity|Electricity|Hydro","Capacity|Electricity|Nuclear",
                         "Capacity|Electricity|Biomass"),
         Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  group_by(Model, Scenario, Category, Region, Year) %>%
  summarise(Value = sum(Value, na.rm=TRUE), .groups="drop") %>%
  add_pathway_diag() %>%
  mutate(Region_label = REGION_LABELS_FIG[Region]) %>%
  group_by(Pathway, Ambition, Region_label, Year) %>%
  summarise(med = median(Value, na.rm=T),
            q25 = quantile(Value, .25, na.rm=T),
            q75 = quantile(Value, .75, na.rm=T),
            .groups = "drop")

plot_trend_reg <- function(dat, ylab) {
  ggplot(dat, aes(x = Year, colour = Pathway, fill = Pathway)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.2, colour = NA) +
    geom_line(aes(y = med), linewidth = 0.9) +
    facet_grid(Ambition ~ Region_label, scales = "free_y") +
    scale_colour_manual(values = PCOLS_PATH) +
    scale_fill_manual(values   = PCOLS_PATH) +
    scale_x_continuous(breaks  = c(2020, 2060, 2100)) +
    labs(x = "Year", y = ylab, colour = "Pathway", fill = "Pathway") +
    theme_c(9)
}

p2a_reg <- plot_trend_reg(p2a_cdr_reg, "Total CDR (MtCO2/yr)") /
  plot_trend_reg(p2a_re_reg,  "Renewable Capacity (GW)") +
  plot_annotation(
    title    = "P2a (regional): CDR and RE Trends by R10 Region — High-CDR vs High-RE",
    subtitle = "Median ± IQR  ·  2020-2100  ·  High-CDR and High-RE scenarios only",
    theme = theme_c()
  )
sc(p2a_reg, "P2a_CDR_RE_trends_by_region.png", 18, 10)

# P2b — Air pollution emissions trends (NOx, SO2) — feeds mortality
p2b_dat <- compass_filtered %>%
  filter(Variable %in% c("Emissions|NOx","Emissions|Sulfur"),
         Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  group_by(Model, Scenario, Category, Variable, Year) %>%
  summarise(Value = sum(Value, na.rm=TRUE), .groups="drop") %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition)) %>%
  group_by(Ambition, Variable, Year) %>%
  summarise(
    med = median(Value, na.rm=TRUE),
    q25 = quantile(Value, 0.25, na.rm=TRUE),
    q75 = quantile(Value, 0.75, na.rm=TRUE),
    .groups="drop"
  ) %>%
  mutate(Variable = recode(Variable,
                           "Emissions|NOx"    = "NOx (Mt/yr)",
                           "Emissions|Sulfur" = "SO2 (Mt/yr)"
  ))

p2b <- ggplot(p2b_dat,
              aes(x = Year, colour = Ambition, fill = Ambition)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.2, colour = NA) +
  geom_line(aes(y = med), linewidth = 1.2) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = ACOLS) +
  scale_fill_manual(values   = ACOLS) +
  scale_x_continuous(breaks  = seq(2020,2100,20)) +
  labs(
    title    = "P2b: Air Pollutant Emissions — Annual Trends (All Scenarios)",
    subtitle = "Sum across R10 regions  ·  Median ± IQR  ·  These feed the rfasst mortality model",
    x = "Year", y = "Emissions (Mt/yr)",
    colour = "Ambition", fill = "Ambition",
    caption  = "NOx and SO2 are the primary PM2.5 precursors used by TM5-FASST to estimate mortality."
  ) +
  theme_c()

sc(p2b, "P2b_emissions_trends.png", 14, 6)

# P2b_reg — faceted by R10 region
p2b_reg_dat <- compass_filtered %>%
  filter(Variable %in% c("Emissions|NOx","Emissions|Sulfur"),
         Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition)) %>%
  mutate(Region_label = REGION_LABELS_FIG[Region],
         Variable = recode(Variable, "Emissions|NOx"="NOx (Mt/yr)",
                           "Emissions|Sulfur"="SO2 (Mt/yr)")) %>%
  group_by(Ambition, Region_label, Variable, Year) %>%
  summarise(med=median(Value,na.rm=T), q25=quantile(Value,.25,na.rm=T),
            q75=quantile(Value,.75,na.rm=T), .groups="drop")

p2b_reg <- ggplot(p2b_reg_dat, aes(x=Year, colour=Ambition, fill=Ambition)) +
  geom_ribbon(aes(ymin=q25, ymax=q75), alpha=0.2, colour=NA) +
  geom_line(aes(y=med), linewidth=0.9) +
  facet_grid(Variable ~ Region_label, scales="free_y") +
  scale_colour_manual(values=ACOLS) + scale_fill_manual(values=ACOLS) +
  scale_x_continuous(breaks=c(2020,2060,2100)) +
  labs(title="P2b (regional): Air Pollutant Emissions by R10 Region",
       subtitle="Median ± IQR  ·  All scenarios",
       x="Year", y="Emissions (Mt/yr)", colour="Ambition", fill="Ambition") +
  theme_c(9) + theme(strip.text.y=element_text(angle=-90))
sc(p2b_reg, "P2b_emissions_trends_by_region.png", 18, 8)

# P2c — Final energy trends by region — feeds DLE
# Check what Final Energy variable names exist
fe_vars <- compass_filtered %>%
  filter(str_detect(Variable, "Final Energy"), Region %in% regions_r10) %>%
  distinct(Variable) %>% pull(Variable)
cat("  Final Energy variables available:", paste(fe_vars, collapse=", "), "\n")
fe_var_use <- if ("Final Energy" %in% fe_vars) "Final Energy" else fe_vars[1]
cat("  Using:", fe_var_use, "\n")

p2c_dat <- compass_filtered %>%
  filter(Variable == fe_var_use,
         Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition)) %>%
  mutate(Region_label = REGION_LABELS_FIG[Region]) %>%
  group_by(Ambition, Region_label, Year) %>%
  summarise(
    med = median(Value, na.rm=TRUE),
    q25 = quantile(Value, 0.25, na.rm=TRUE),
    q75 = quantile(Value, 0.75, na.rm=TRUE),
    .groups="drop"
  )

p2c <- ggplot(p2c_dat,
              aes(x = Year, colour = Ambition, fill = Ambition)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.2, colour = NA) +
  geom_line(aes(y = med), linewidth = 1.0) +
  facet_wrap(~ Region_label, scales = "free_y", ncol = 5) +
  scale_colour_manual(values = ACOLS) +
  scale_fill_manual(values   = ACOLS) +
  scale_x_continuous(breaks  = c(2020,2060,2100)) +
  labs(
    title    = "P2c: Final Energy by Region — Annual Trends",
    subtitle = "Compared against DLE thresholds (Kikstra et al. 2021) to compute energy access gap",
    x = "Year", y = "Final Energy (EJ/yr)",
    colour = "Ambition", fill = "Ambition",
    caption  = "DLE thresholds differ by region: Africa 24.5, India+ 22.5, China+ 37, Europe 52, N.America 63 GJ/capita/yr."
  ) +
  theme_c() +
  theme(strip.text = element_text(size = 9))

sc(p2c, "P2c_final_energy_by_region.png", 16, 6)

# P2d — Capacity additions trends by fuel type (feeds jobs)
p2d_dat <- compass_filtered %>%
  filter(Variable %in% c(
    "Capacity Additions|Electricity|Solar",
    "Capacity Additions|Electricity|Wind",
    "Capacity Additions|Electricity|Hydro",
    "Capacity Additions|Electricity|Nuclear",
    "Capacity Additions|Electricity|Biomass",
    "Capacity Additions|Electricity|Coal",
    "Capacity Additions|Electricity|Gas"
  ),
  Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  mutate(
    fuel = str_remove(Variable, "Capacity Additions\\|Electricity\\|"),
    tech_group = if_else(fuel %in% c("Coal","Gas","Oil"), "Fossil","Renewables")
  ) %>%
  group_by(Model, Scenario, Category, fuel, tech_group, Year) %>%
  summarise(Value = sum(Value, na.rm=TRUE), .groups="drop") %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition)) %>%
  group_by(Ambition, fuel, tech_group, Year) %>%
  summarise(med = median(Value, na.rm=TRUE), .groups="drop")

p2d <- ggplot(p2d_dat,
              aes(x = Year, y = med, colour = fuel, linetype = Ambition)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ tech_group, scales = "free_y", ncol = 2) +
  scale_colour_brewer(palette = "Set1", name = "Fuel type") +
  scale_linetype_manual(values = c("1.5C (High-Ambition)" = "solid",
                                   "2C (Medium-Ambition)"  = "dashed")) +
  scale_x_continuous(breaks = seq(2020,2100,20)) +
  labs(
    title    = "P2d: Capacity Additions by Fuel Type — Annual Trends",
    subtitle = "Median across all scenarios  ·  Sum across R10 regions  ·  These feed the jobs calculation",
    x = "Year", y = "Capacity Additions (GW/yr)",
    caption  = "Jobs = construction + O&M intensity × GW added per year."
  ) +
  theme_c()

sc(p2d, "P2d_capacity_additions_trends.png", 14, 7)

# P2d_reg — faceted by R10 region
p2d_reg_dat <- compass_filtered %>%
  filter(Variable %in% c(
    "Capacity Additions|Electricity|Solar","Capacity Additions|Electricity|Wind",
    "Capacity Additions|Electricity|Hydro","Capacity Additions|Electricity|Nuclear",
    "Capacity Additions|Electricity|Biomass",
    "Capacity Additions|Electricity|Coal","Capacity Additions|Electricity|Gas"),
    Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  mutate(fuel=str_remove(Variable,"Capacity Additions|Electricity|"),
         tech_group=if_else(fuel %in% c("Coal","Gas","Oil"),"Fossil","Renewables")) %>%
  assign_amb("Category") %>% filter(!is.na(Ambition)) %>%
  mutate(Region_label = REGION_LABELS_FIG[Region]) %>%
  group_by(Ambition, Region_label, fuel, tech_group, Year) %>%
  summarise(med=median(Value,na.rm=T), .groups="drop")

p2d_reg <- ggplot(p2d_reg_dat,
                  aes(x=Year, y=med, colour=fuel, linetype=Ambition)) +
  geom_line(linewidth=0.7) +
  facet_grid(tech_group ~ Region_label, scales="free_y") +
  scale_colour_brewer(palette="Set1", name="Fuel") +
  scale_linetype_manual(values=c("1.5C (High-Ambition)"="solid",
                                 "2C (Medium-Ambition)"="dashed")) +
  scale_x_continuous(breaks=c(2020,2060,2100)) +
  labs(title="P2d (regional): Capacity Additions by Fuel and Region",
       subtitle="Median across all scenarios  ·  GW/yr",
       x="Year", y="Capacity Additions (GW/yr)") +
  theme_c(9) + theme(strip.text.y=element_text(angle=-90))
sc(p2d_reg, "P2d_capacity_additions_by_region.png", 18, 9)

# =============================================================================
# SECTION 3: TRENDS BY PATHWAY TYPE
# =============================================================================
cat("\n=== SECTION 3: Trends by pathway type ===\n")

# P3a — CDR component breakdown by pathway × ambition (world)
# Diagnostic: check coverage of each CDR component
cat("P3a CDR component coverage in cdr_cumulative:\n")
cdr_cumulative %>%
  filter(Variable %in% c("Novel CDR","Fossil CCS","Land-based CDR"),
         Region %in% regions_r10) %>%
  group_by(Variable) %>%
  summarise(n_scenarios = n_distinct(paste(Model, Scenario)),
            n_nonzero   = sum(Total_Value > 0, na.rm=TRUE),
            mean_val    = round(mean(Total_Value, na.rm=TRUE), 1),
            .groups="drop") %>%
  print()

# Also check what Carbon Removal variables exist in compass_filtered
cat("Carbon Removal variables in compass_filtered:\n")
compass_filtered %>%
  filter(str_detect(Variable, "Carbon Removal|Land Use|BECCS|DAC")) %>%
  distinct(Variable) %>% print()

p3a_dat <- cdr_cumulative %>%
  filter(Variable %in% c("Novel CDR","Fossil CCS","Land-based CDR"),
         Region %in% regions_r10) %>%
  group_by(Model, Scenario, Category, Variable) %>%
  summarise(Total_Value = sum(Total_Value, na.rm=TRUE), .groups="drop") %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition)) %>%
  left_join(world_cum_f2 %>% select(Model, Scenario,
                                    high_cdr_only, high_re_only),
            by = c("Model","Scenario")) %>%
  filter(high_cdr_only | high_re_only) %>%
  mutate(
    Pathway  = if_else(high_cdr_only, "High-CDR", "High-RE"),
    Variable = factor(Variable, levels = c("Novel CDR","Fossil CCS",
                                           "Land-based CDR"))
  ) %>%
  group_by(Pathway, Ambition, Variable) %>%
  summarise(mean_val = mean(Total_Value / 1e3, na.rm=TRUE),
            n = n_distinct(paste(Model, Scenario)),
            .groups="drop")

# P3a: fix label positions using position_stack so labels appear mid-segment
# n= labels placed at top of each bar using after_stat
p3a <- ggplot(p3a_dat,
              aes(x = Pathway, y = mean_val, fill = Variable)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
  # n= labels removed per analysis plan
  facet_wrap(~ Ambition, scales = "free_x", ncol = 2) +
  scale_fill_manual(values = CCDR, name = "CDR type") +
  scale_y_continuous(labels = comma_format(), expand = expansion(mult=c(0,.1))) +
  labs(
    title    = "P3a: CDR Component Breakdown — High-CDR vs High-RE",
    subtitle = "Mean cumulative 2020-2100, summed across R10  ·  World-level tercile classification",
    x = NULL, y = "Mean cumulative CDR (TtCO2)",
    caption  = "Fossil CCS dominates High-CDR pathways; High-RE pathways rely more on land-based and novel CDR."
  ) +
  theme_c() +
  theme(axis.text.x = element_text(angle = 10, hjust = 1, size = 8.5))

sc(p3a, "P3a_CDR_components_by_pathway.png", 12, 7)

# s6_reg_class: region-level tercile classification (top tercile CDR or RE within each region)
# Defined here early so P3a_reg, P3b_reg, and P6 figures can all use it
if (!exists("s6_reg_class")) {
  s6_reg_class <- df_master %>%
    filter(Variable %in% c("Total CDR","Renewable Capacity"),
           Region %in% regions_r10) %>%
    select(Model, Scenario, Category, Region, Ambition, Variable, Total_Value) %>%
    pivot_wider(names_from = Variable, values_from = Total_Value) %>%
    rename(region_cdr = `Total CDR`, region_re = `Renewable Capacity`) %>%
    group_by(Region, Ambition) %>%
    mutate(
      high_cdr      = ntile(region_cdr, 3) == 3,
      high_re       = ntile(region_re,  3) == 3,
      high_cdr_only = high_cdr & !high_re,
      high_re_only  = high_re  & !high_cdr,
      high_any      = high_cdr | high_re,
      Pathway_reg   = case_when(
        high_cdr_only ~ "High-CDR",
        high_re_only  ~ "High-RE",
        TRUE          ~ NA_character_
      )
    ) %>%
    ungroup()
  cat("s6_reg_class built:", nrow(s6_reg_class), "rows\n")
}

# P3a_reg — same but with R10 regions faceted
p3a_reg_dat <- cdr_cumulative %>%
  filter(Variable %in% c("Novel CDR","Fossil CCS","Land-based CDR"),
         Region %in% regions_r10) %>%
  left_join(s6_reg_class %>% select(Model, Scenario, Region, Ambition,
                                    high_cdr_only, high_re_only),
            by = c("Model","Scenario","Region")) %>%
  filter(high_cdr_only | high_re_only) %>%
  assign_amb("Category") %>% filter(!is.na(Ambition)) %>%
  mutate(Pathway  = if_else(high_cdr_only, "High-CDR", "High-RE"),
         Variable = factor(Variable, levels=c("Novel CDR","Fossil CCS","Land-based CDR")),
         Region_label = REGION_LABELS_FIG[Region]) %>%
  group_by(Pathway, Ambition, Region_label, Variable) %>%
  summarise(mean_val = mean(Total_Value/1e3, na.rm=T),
            n = n_distinct(paste(Model,Scenario)), .groups="drop")

p3a_reg <- ggplot(p3a_reg_dat,
                  aes(x = Pathway, y = mean_val, fill = Variable)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.25) +
  facet_grid(Region_label ~ Ambition) +
  scale_fill_manual(values = CCDR, name = "CDR type") +
  scale_y_continuous(labels = comma_format(), expand = expansion(mult=c(0,.08))) +
  labs(title    = "P3a (regional): CDR Component Breakdown by R10 Region",
       subtitle = "Mean cumulative 2020-2100  ·  World-level tercile classification",
       x = NULL, y = "Mean cumulative CDR (TtCO2)") +
  theme_c(9) + theme(strip.text.y=element_text(angle=-90))
sc(p3a_reg, "P3a_CDR_components_by_region.png", 13, 14)

# P3b — Electricity mix by pathway (what fuels dominate)
elec_vars_map <- c(
  "Secondary Energy|Electricity|Coal"    = "Coal",
  "Secondary Energy|Electricity|Gas"     = "Gas",
  "Secondary Energy|Electricity|Oil"     = "Oil",
  "Secondary Energy|Electricity|Biomass" = "Biomass",
  "Secondary Energy|Electricity|Nuclear" = "Nuclear",
  "Secondary Energy|Electricity|Hydro"   = "Hydro",
  "Secondary Energy|Electricity|Wind"    = "Wind",
  "Secondary Energy|Electricity|Solar"   = "Solar",
  "Secondary Energy|Electricty|Geothermal" = "Geothermal"
)

p3b_dat <- compass_filtered %>%
  filter(Variable %in% names(elec_vars_map),
         Region %in% regions_r10,
         Year %in% c(2030, 2050, 2070)) %>%
  left_join(world_cum_f2 %>% select(Model, Scenario,
                                    high_cdr_only, high_re_only),
            by = c("Model","Scenario")) %>%
  filter(high_cdr_only | high_re_only) %>%
  mutate(
    Pathway = if_else(high_cdr_only, "High-CDR", "High-RE"),
    fuel    = elec_vars_map[Variable]
  ) %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition)) %>%
  group_by(Model, Scenario, Pathway, Ambition, Year) %>%
  mutate(share = Value / sum(Value, na.rm=TRUE)) %>%
  filter(!is.na(share), is.finite(share)) %>%
  group_by(Pathway, Ambition, Year, fuel) %>%
  summarise(med_share = median(share, na.rm=TRUE), .groups="drop") %>%
  mutate(fuel = factor(fuel, levels = rev(names(PELEC))))

p3b <- ggplot(p3b_dat %>% filter(Year =="2050"),
              aes(x = factor(Year), y = med_share, fill = fuel)) +
  geom_col(width = 0.8, colour = "white", linewidth = 0.2, position = "fill") +
  facet_grid(Ambition ~ Pathway) +
  scale_fill_manual(values = PELEC,
                    guide  = guide_legend(nrow = 2, reverse = TRUE)) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "P3b: Electricity Mix — High-CDR vs High-RE",
    subtitle = "Median share of total electricity  ·  Aggregated R10 regions  ·  2030 / 2050 / 2070",
    x = "Year", y = "Share of electricity",
    fill = "Fuel type",
    caption  = paste0(
      "Key finding: High-CDR maintains higher Biomass (BECCS) share through 2070.\n",
      "High-RE shows faster Solar and Wind ramp-up, earlier coal phase-out."
    )
  ) +
  theme_c()

sc(p3b, "P3b_electricity_mix_by_pathway.png", 13, 9)

# P3b_reg — faceted by R10 region
p3b_reg_dat <- compass_filtered %>%
  filter(Variable %in% names(elec_vars_map),
         Region %in% regions_r10, Year %in% c(2030,2050,2070)) %>%
  left_join(s6_reg_class %>% select(Model, Scenario, Region, Ambition,
                                    high_cdr_only, high_re_only),
            by = c("Model","Scenario","Region")) %>%
  filter(high_cdr_only | high_re_only) %>%
  mutate(Pathway = if_else(high_cdr_only,"High-CDR","High-RE"),
         fuel    = elec_vars_map[Variable]) %>%
  assign_amb("Category") %>% filter(!is.na(Ambition)) %>%
  mutate(Region_label = REGION_LABELS_FIG[Region]) %>%
  group_by(Model, Scenario, Pathway, Ambition, Region_label, Year) %>%
  mutate(share = Value/sum(Value, na.rm=T)) %>%
  filter(!is.na(share), is.finite(share)) %>%
  group_by(Pathway, Ambition, Region_label, Year, fuel) %>%
  summarise(med_share = median(share, na.rm=T), .groups="drop") %>%
  mutate(fuel = factor(fuel, levels=rev(names(PELEC))))

# x = Pathway (High-CDR | High-RE side by side within each ambition group)
# facet cols = Ambition x Year, facet rows = Region
# position = "fill" ensures bars reach 100% regardless of median share summing
p3b_reg_dat <- p3b_reg_dat %>% filter(Year == "2050") %>% 
  mutate(
    Pathway = factor(Pathway, levels = c("High-CDR", "High-RE")),
    AmbYear = factor(
      paste0(if_else(str_detect(Ambition, "1.5"), "1.5C", "2C"), " ", Year),
      levels = c("1.5C 2030","1.5C 2050","1.5C 2070",
                 "2C 2030",  "2C 2050",  "2C 2070")
    )
  )

p3b_reg <- ggplot(p3b_reg_dat,
                  aes(x = Pathway, y = med_share, fill = fuel)) +
  geom_col(width = 0.8, colour = "white", linewidth = 0.15,
           position = "fill") +
  facet_grid(Region_label ~ AmbYear) +
  scale_fill_manual(values = PELEC, guide = guide_legend(nrow=2, reverse=T)) +
  scale_y_continuous(labels = percent_format()) +
  labs(title    = "P3b (regional): Electricity Mix by Pathway and Region",
       subtitle = "Median share  ·  2030/2050/2070  ·  High-CDR vs High-RE side by side within each ambition group",
       x = NULL, y = "Share of electricity", fill = "Fuel type") +
  theme_c(8) +
  theme(strip.text.y = element_text(angle = -90, size = 7),
        axis.text.x  = element_text(angle = 20, hjust = 1, size = 7))

sc(p3b_reg, "P3b_electricity_mix_by_region.png", 24, 16)

# P3c — NOx emissions trajectories by pathway × region
# This is the direct mechanism connecting pathway choice to mortality
p3c_dat <- compass_filtered %>%
  filter(Variable == "Emissions|NOx",
         Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  left_join(world_cum_f2 %>% select(Model, Scenario,
                                    high_cdr_only, high_re_only),
            by = c("Model","Scenario")) %>%
  filter(high_cdr_only | high_re_only) %>%
  mutate(Pathway = if_else(high_cdr_only, "High-CDR", "High-RE")) %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition)) %>%
  mutate(Region_label = REGION_LABELS_FIG[Region]) %>%
  group_by(Pathway, Ambition, Region_label, Year) %>%
  summarise(
    med = median(Value, na.rm=TRUE),
    q25 = quantile(Value, 0.25, na.rm=TRUE),
    q75 = quantile(Value, 0.75, na.rm=TRUE),
    .groups="drop"
  )

# Add vertical lines for net-zero cutoffs
nz_lines <- tibble(
  Ambition  = c("1.5C (High-Ambition)", "2C (Medium-Ambition)"),
  cutoff    = c(NZ_15_snap, NZ_2C_snap)
)

p3c <- ggplot(p3c_dat,
              aes(x = Year, colour = Pathway, fill = Pathway)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.15, colour = NA) +
  geom_line(aes(y = med), linewidth = 1.0) +
  geom_vline(data = nz_lines,
             aes(xintercept = cutoff),
             linetype = "dashed", colour = "grey40", linewidth = 0.7) +
  geom_text(data = nz_lines,
            aes(x = cutoff + 1, y = Inf,
                label = paste0("nz≈", cutoff)),
            inherit.aes = FALSE,
            vjust = 1.5, hjust = 0, size = 2.8, colour = "grey40") +
  facet_grid(Region_label ~ Ambition, scales = "free_y") +
  scale_colour_manual(values = PCOLS) +
  scale_fill_manual(values   = PCOLS) +
  scale_x_continuous(breaks  = c(2020, 2050, 2080)) +
  labs(
    title    = "P3c: NOx Emissions by Pathway Type — Regional Trajectories",
    subtitle = paste0(
      "Primary PM2.5 precursor driving air pollution mortality  ·  ",
      "Median ± IQR  ·  Dashed = ambition group median net-zero year"
    ),
    x = "Year", y = "NOx emissions (Mt/yr)",
    colour = "Pathway", fill = "Pathway",
    caption  = paste0(
      "Key mechanism: If High-CDR pathways have higher NOx than High-RE up to net-zero,\n",
      "this directly explains worse mortality outcomes in CDR-heavy pathways."
    )
  ) +
  theme_c() +
  theme(strip.text.y = element_text(angle = -90, size = 8))

sc(p3c, "P3c_NOx_by_pathway_region.png", 14, 14)

# =============================================================================
# SECTION 4: WELLBEING OUTCOMES BY PATHWAY TYPE
# =============================================================================
cat("\n=== SECTION 4: Wellbeing outcomes by pathway type ===\n")

OUTCOME_LABS_NZ <- c(
  "cum_deaths_nz"           = "Air Pollution Mortality\n(deaths per million, cumul.)",
  "mean_headcount_millions" = "Energy Deprivation\nHeadcount (people per million, mean)",
  "jobs_Renewables"         = "Renewable Energy\nJobs (per million, cumul.)",
  "jobs_Fossil"             = "Fossil Energy\nJobs (per million, cumul.)"
)

GROUP_COLS_4 <- c(
  "High-CDR\n1.5C (High-Ambition)" = "#2166ac",
  "High-RE\n1.5C (High-Ambition)"  = "#92c5de",
  "High-CDR\n2C (Medium-Ambition)" = "#d6604d",
  "High-RE\n2C (Medium-Ambition)"  = "#fddbc7"
)

# P4a — Outcome violins at net-zero cutoff (world average)
p4a_long <- outcomes_nz_world %>%
  select(Model, Scenario, Group, all_of(names(OUTCOME_LABS_NZ))) %>%
  pivot_longer(cols = all_of(names(OUTCOME_LABS_NZ)),
               names_to = "outcome_col", values_to = "val") %>%
  filter(!is.na(val), val > 0) %>%
  mutate(
    outcome_lab = OUTCOME_LABS_NZ[outcome_col],
    outcome_lab = factor(outcome_lab, levels = unname(OUTCOME_LABS_NZ))
  )

p4a <- ggplot(p4a_long,
              aes(x = Group, y = val, fill = Group, colour = Group)) +
  geom_violin(alpha = 0.45, colour = NA, scale = "width", trim = TRUE) +
  geom_boxplot(width = 0.12, outlier.size = 0.4,
               colour = "grey30", fill = "white", alpha = 0.8) +
  facet_wrap(~ outcome_lab, scales = "free_y", ncol = 4) +
  scale_fill_manual(values   = GROUP_COLS_4) +
  scale_colour_manual(values = GROUP_COLS_4) +
  scale_y_continuous(labels  = comma_format()) +
  labs(
    title    = "P4a: Wellbeing Outcomes — High-CDR vs High-RE",
    subtitle = paste0(
      "Aggregated R10 regions  ·  Outcomes cumulated to ambition net-zero window  ·  ",
      sprintf("1.5C: 2020-%d  ·  2C: 2020-%d", WINDOW_15C, WINDOW_2C)
    ),
    x = NULL, y = NULL,
    fill = "Pathway × Ambition", colour = "Pathway × Ambition",
    caption  = paste0(
      "Violin = full distribution; box = IQR; line = median.\n",
      "Mortality = cumulative PM2.5 deaths via rfasst/TM5-FASST. ",
      "Jobs = construction + O&M × capacity additions."
    )
  ) +
  theme_c() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

sc(p4a, "P4a_outcome_violins_netzero.png", 18, 8)

# ── P4a variants: 1.5C only, 2C only, combined ──────────────────────────────
# Helper to build aggregated violin for a given ambition filter
make_p4a <- function(dat_long, amb_filter, title_sfx, fname, w=14, h=8) {
  d <- if (is.null(amb_filter)) dat_long else
    dat_long %>% filter(str_detect(Group, amb_filter))
  cols_use <- GROUP_COLS_4[names(GROUP_COLS_4) %in% unique(d$Group)]
  ggplot(d, aes(x = Group, y = val, fill = Group, colour = Group)) +
    geom_violin(alpha = 0.45, colour = NA, scale = "width", trim = TRUE) +
    geom_boxplot(width = 0.12, outlier.size = 0.4,
                 colour = "grey30", fill = "white", alpha = 0.8) +
    stat_summary(fun = median, geom = "point",
                 size = 2.5, colour = "grey10", shape = 18) +
    facet_wrap(~ outcome_lab, scales = "free_y", ncol = 4) +
    scale_fill_manual(values = cols_use) +
    scale_colour_manual(values = cols_use) +
    scale_y_continuous(labels = comma_format()) +
    labs(
      title    = paste0("P4a: Wellbeing Outcomes — High-CDR vs High-RE", title_sfx),
      subtitle = "Aggregated R10 regions  ·  Per million 2020 population  ·  Outcomes cumulated to ambition net-zero window",
      x = NULL, y = NULL,
      fill = "Pathway x Ambition", colour = "Pathway x Ambition",
      caption = paste0(
        "Violin = full distribution; box = IQR; diamond = median.\n",
        "Mortality = cumulative PM2.5 deaths via rfasst/TM5-FASST. ",
        "Jobs = construction + O&M x capacity additions."
      )
    ) +
    theme_c() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
}

sc(make_p4a(p4a_long, "1.5C", " (1.5C High-Ambition)",
            "P4a_outcome_violins_15C.png"), "P4a_outcome_violins_15C.png", 14, 8)
sc(make_p4a(p4a_long, "2C",   " (2C Medium-Ambition)",
            "P4a_outcome_violins_2C.png"),  "P4a_outcome_violins_2C.png",  14, 8)
sc(make_p4a(p4a_long, NULL,   " (1.5C and 2C Combined)",
            "P4a_outcome_violins_combined.png"), "P4a_outcome_violins_combined.png", 18, 8)
cat("  P4a 1.5C, 2C, and combined saved.\n")

# P4a_reg — faceted by R10 region
p4a_reg_long <- outcomes_nz %>%
  filter(!is.na(Region_label)) %>%
  select(Model, Scenario, Group, Region_label, all_of(names(OUTCOME_LABS_NZ))) %>%
  pivot_longer(all_of(names(OUTCOME_LABS_NZ)),
               names_to="outcome_col", values_to="val") %>%
  filter(!is.na(val), val > 0) %>%
  mutate(outcome_lab = factor(OUTCOME_LABS_NZ[outcome_col],
                              levels=unname(OUTCOME_LABS_NZ)))

p4a_reg <- ggplot(p4a_reg_long,
                  aes(x=Group, y=val, fill=Group, colour=Group)) +
  geom_violin(alpha=0.40, colour=NA, scale="width", trim=TRUE) +
  geom_boxplot(width=0.10, outlier.size=0.3,
               colour="grey30", fill="white", alpha=0.8) +
  facet_grid(outcome_lab ~ Region_label, scales="free_y") +
  scale_fill_manual(values=GROUP_COLS_4) +
  scale_colour_manual(values=GROUP_COLS_4) +
  scale_y_continuous(labels=comma_format()) +
  labs(title="P4a (regional): Wellbeing Outcomes by Pathway and Region",
       subtitle="Outcomes cumulated to ambition net-zero window  ·  All four pathway × ambition groups",
       x=NULL, y=NULL) +
  theme_c(8) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        strip.text.y=element_text(angle=-90,size=7))
sc(p4a_reg, "P4a_outcome_violins_by_region.png", 20, 18)

# ── P4a_reg variants: 1.5C only, 2C only, combined ──────────────────────────
make_p4a_reg <- function(dat_long, amb_filter, title_sfx, fname, w=20, h=14) {
  d <- if (is.null(amb_filter)) dat_long else
    dat_long %>% filter(str_detect(Group, amb_filter))
  cols_use <- GROUP_COLS_4[names(GROUP_COLS_4) %in% unique(d$Group)]
  ggplot(d, aes(x = Group, y = val, fill = Group, colour = Group)) +
    geom_violin(alpha = 0.40, colour = NA, scale = "width", trim = TRUE) +
    geom_boxplot(width = 0.10, outlier.size = 0.3,
                 colour = "grey30", fill = "white", alpha = 0.8) +
    stat_summary(fun = median, geom = "point",
                 size = 2, colour = "grey10", shape = 18) +
    facet_grid(outcome_lab ~ Region_label, scales = "free_y") +
    scale_fill_manual(values = cols_use) +
    scale_colour_manual(values = cols_use) +
    scale_y_continuous(labels = comma_format()) +
    labs(
      title    = paste0("P4a (regional): Wellbeing Outcomes by R10 Region", title_sfx),
      subtitle = "Per million 2020 population  ·  Outcomes cumulated to ambition net-zero window",
      x = NULL, y = NULL,
      fill = "Pathway x Ambition", colour = "Pathway x Ambition"
    ) +
    theme_c(8) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          strip.text.y = element_text(angle = -90, size = 7))
}

sc(make_p4a_reg(p4a_reg_long, "1.5C", " (1.5C High-Ambition)",
                "P4a_reg_outcome_violins_15C.png"), "P4a_reg_outcome_violins_15C.png", 20, 10)
sc(make_p4a_reg(p4a_reg_long, "2C",   " (2C Medium-Ambition)",
                "P4a_reg_outcome_violins_2C.png"),  "P4a_reg_outcome_violins_2C.png",  20, 10)
sc(make_p4a_reg(p4a_reg_long, NULL,   " (1.5C and 2C Combined)",
                "P4a_reg_outcome_violins_combined.png"), "P4a_reg_outcome_violins_combined.png", 20, 18)
cat("  P4a regional 1.5C, 2C, and combined saved.\n")

# P4b — Mortality trajectory world (2020 to 2100)
# Sum across R10 regions (no World region used)
# mortality_r10_raw already standardised — sum R10 regions for world total
p4b_dat <- mortality_r10_raw %>%
  filter(Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  left_join(
    world_cum_f2 %>% select(Model, Scenario, Ambition, high_cdr_only, high_re_only),
    by = c("Model","Scenario")
  ) %>%
  filter(!is.na(high_cdr_only), high_cdr_only | high_re_only, !is.na(Ambition)) %>%
  mutate(Pathway = if_else(high_cdr_only, "High-CDR", "High-RE")) %>%
  group_by(Model, Scenario, Pathway, Ambition, Year) %>%
  summarise(deaths = sum(deaths_pm25, na.rm=TRUE), .groups="drop") %>%
  mutate(Group = paste0(Pathway, "\n", Ambition),
         Group = factor(Group, levels = names(GROUP_COLS_4)))
cat("  p4b_dat rows:", nrow(p4b_dat), "\n")

# Add net-zero rectangles
nz_rects <- tibble(
  Ambition = c("1.5C (High-Ambition)", "2C (Medium-Ambition)"),
  xmin = c(NZ_15_snap, NZ_2C_snap), xmax = c(2100, 2100),
  ymin = -Inf, ymax = Inf
)

p4b <- ggplot(p4b_dat,
              aes(x = Year, y = deaths / 1e6,
                  group = interaction(Model, Scenario),
                  colour = Group)) +
  geom_rect(data = nz_rects,
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            inherit.aes = FALSE,
            fill = "grey95", alpha = 0.6) +
  geom_line(alpha = 0.10, linewidth = 0.3) +
  stat_summary(aes(group = Group), fun = median,
               geom = "line", linewidth = 1.3, na.rm = TRUE) +
  geom_vline(data = nz_rects,
             aes(xintercept = xmin),
             linetype = "dashed", colour = "grey40",
             linewidth = 0.8, inherit.aes = FALSE) +
  facet_wrap(~ Ambition, ncol = 2) +
  scale_colour_manual(values = GROUP_COLS_4,
                      guide  = guide_legend(
                        title = "Pathway", nrow = 2,
                        override.aes = list(alpha=1, linewidth=1.3))) +
  scale_x_continuous(breaks = seq(2020, 2100, 10)) +
  labs(
    title    = "P4b: Air Pollution Mortality Trajectories — Aggregated R10 regions",
    subtitle = paste0(
      "Thin = individual scenarios  ·  Bold = median  ·  ",
      "Shaded = post net-zero period  ·  Dashed = ambition net-zero cutoff"
    ),
    x = "Year", y = "PM2.5 deaths (millions/yr)",
    caption  = paste0(
      "Aggregated R10 regions = sum across 5 R10 regions. ",
      "Post net-zero period (shaded) excluded from cumulative outcome calculation."
    )
  ) +
  theme_c()

sc(p4b, "P4b_mortality_trajectory_world.png", 14, 7)

# P4b_reg — mortality trajectory by R10 region
p4b_reg_dat <- mortality_r10_raw %>%
  filter(Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  left_join(
    world_cum_f2 %>% select(Model, Scenario, Ambition, high_cdr_only, high_re_only),
    by = c("Model","Scenario")
  ) %>%
  filter(!is.na(high_cdr_only), high_cdr_only | high_re_only, !is.na(Ambition)) %>%
  mutate(Pathway = if_else(high_cdr_only, "High-CDR", "High-RE")) %>%
  mutate(Group = paste0(Pathway,"\n",Ambition),
         Group = factor(Group, levels=names(GROUP_COLS_4)),
         Region_label = REGION_LABELS_FIG[Region])

p4b_reg <- ggplot(p4b_reg_dat,
                  aes(x=Year, y=deaths_pm25/1e6,
                      group=interaction(Model,Scenario), colour=Group)) +
  geom_line(alpha=0.08, linewidth=0.3) +
  stat_summary(aes(group=Group), fun=median, geom="line",
               linewidth=1.1, na.rm=TRUE) +
  geom_vline(data=nz_lines, aes(xintercept=cutoff),
             linetype="dashed", colour="grey40", linewidth=0.6,
             inherit.aes=FALSE) +
  facet_grid(Region_label ~ Ambition, scales="free_y") +
  scale_colour_manual(values=GROUP_COLS_4,
                      guide=guide_legend(title="Pathway",nrow=2,
                                         override.aes=list(alpha=1,linewidth=1.2))) +
  scale_x_continuous(breaks=c(2020,2050,2080)) +
  labs(title="P4b (regional): Mortality Trajectories by R10 Region",
       subtitle="Thin=individual scenarios  ·  Bold=median  ·  Dashed=net-zero cutoff",
       x="Year", y="PM2.5 deaths (millions/yr)") +
  theme_c(9) + theme(strip.text.y=element_text(angle=-90,size=8))
sc(p4b_reg, "P4b_mortality_trajectory_by_region.png", 14, 16)

# P4c — SKIPPED per analysis plan

# P4d — Outcome dot plot: High-CDR vs High-RE medians by region
# Cleaner than difference bars — shows both absolute levels and direction
# One panel per outcome; x = median value; y = region; colour = pathway
p4d_outcomes <- c(
  "cum_deaths_nz"              = "Air Pollution Mortality\n(million deaths)",
  "cumulative_gap_EJ"          = "DLE Energy Gap (EJ)",
  "mean_headcount_millions"    = "Energy Deprivation\nHeadcount (millions)",
  "cumulative_implied_CO2_GtCO2" = "CO2 Cost\nDLE Gap (GtCO2)",
  "jobs_Fossil"                = "Fossil Energy Jobs\n(thousands)",
  "jobs_Renewables"            = "Renewable Energy Jobs\n(thousands)"
)

# P4d redesign: one panel per outcome, 1.5C and 2C on same panel
# Colour = Pathway (CDR=blues, RE=reds), dark=1.5C light=2C; shape = ambition
p4d_outcomes <- c(
  "cum_deaths_nz"              = "Air Pollution Mortality\n(million deaths, 2020–net-zero)",
  "cumulative_gap_EJ"          = "DLE Energy Gap\n(EJ, 2020–net-zero)",
  "mean_headcount_millions"    = "Energy Deprivation\nHeadcount (millions)",
  "cumulative_implied_CO2_GtCO2" = "CO2 Cost DLE Gap\n(GtCO2)",
  "jobs_Fossil"                = "Fossil Energy Jobs\n(thousands)",
  "jobs_Renewables"            = "Renewable Energy Jobs\n(thousands)"
)

p4d_dat <- outcomes_nz %>%
  filter(!is.na(Region_label),
         Region_label != "Aggregated R10 regions") %>%
  select(Model, Scenario, Pathway, Ambition, Region_label,
         any_of(names(p4d_outcomes))) %>%
  pivot_longer(cols = any_of(names(p4d_outcomes)),
               names_to = "outcome_col", values_to = "val") %>%
  filter(!is.na(val), val > 0) %>%
  mutate(
    outcome_lab  = p4d_outcomes[outcome_col],
    outcome_lab  = factor(outcome_lab, levels = unname(p4d_outcomes)),
    Region_label = factor(Region_label,
                          levels = rev(unname(REGION_LABELS_FIG[regions_r10]))),
    PathAmb = paste0(Pathway, "\n", Ambition)
  ) %>%
  group_by(Pathway, Ambition, PathAmb, Region_label, outcome_lab) %>%
  summarise(
    med  = median(val, na.rm = TRUE),
    q25  = quantile(val, 0.25, na.rm = TRUE),
    q75  = quantile(val, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(PathAmb = factor(PathAmb, levels = c(
    "High-CDR\n1.5C (High-Ambition)", "High-CDR\n2C (Medium-Ambition)",
    "High-RE\n1.5C (High-Ambition)",  "High-RE\n2C (Medium-Ambition)"
  )))

P4D_COLS <- c(
  "High-CDR\n1.5C (High-Ambition)" = "#2166ac",
  "High-CDR\n2C (Medium-Ambition)" = "#92c5de",
  "High-RE\n1.5C (High-Ambition)"  = "#d6604d",
  "High-RE\n2C (Medium-Ambition)"  = "#f4a582"
)
P4D_SHAPES <- c(
  "High-CDR\n1.5C (High-Ambition)" = 16,
  "High-CDR\n2C (Medium-Ambition)" = 17,
  "High-RE\n1.5C (High-Ambition)"  = 16,
  "High-RE\n2C (Medium-Ambition)"  = 17
)

p4d <- ggplot(p4d_dat,
              aes(x = med, y = Region_label,
                  colour = PathAmb, shape = PathAmb)) +
  geom_linerange(aes(xmin = q25, xmax = q75),
                 linewidth = 1.0, alpha = 0.55,
                 position = position_dodge(width = 0.65)) +
  geom_point(size = 2.8, position = position_dodge(width = 0.65)) +
  facet_wrap(~ outcome_lab, scales = "free_x", ncol = 3) +
  scale_colour_manual(values = P4D_COLS,
                      labels = c("High-CDR · 1.5C", "High-CDR · 2C",
                                 "High-RE · 1.5C",  "High-RE · 2C"),
                      name   = "Pathway × Ambition") +
  scale_shape_manual(values = P4D_SHAPES,
                     labels = c("High-CDR · 1.5C", "High-CDR · 2C",
                                "High-RE · 1.5C",  "High-RE · 2C"),
                     name   = "Pathway × Ambition") +
  scale_x_continuous(labels = comma_format()) +
  labs(
    title    = "P4d: Wellbeing Outcomes — High-CDR vs High-RE by Region",
    subtitle = paste0(
      "Point = median; bar = IQR  ·  1.5C and 2C on same panel\n",
      "Blues = High-CDR; Reds = High-RE  ·  Filled circle = 1.5C; triangle = 2C"
    ),
    x = "Outcome value (median ± IQR)", y = NULL,
    caption  = paste0(
      "Outcomes cumulated to ambition net-zero window (1.5C: 2020–2060; 2C: 2020–2075).\n",
      "World-level tercile classification.\n",
      "For mortality, DLE gap, headcount, CO2 cost: lower = better. For jobs: higher = better."
    )
  ) +
  theme_c() +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3))

sc(p4d, "P4d_outcomes_dotplot_by_region.png", 16, 12)

# P4d per-capita — same figure but filtered to 4 main outcomes with per-million labels
# outcomes_nz already has per-capita normalization applied (from pop_ts join above)
p4d_percap_outcomes <- c(
  "cum_deaths_nz"           = "Air Pollution Mortality\n(deaths per million, cumul.)",
  "mean_headcount_millions" = "Energy Deprivation\nHeadcount (per million, mean)",
  "jobs_Renewables"         = "Renewable Energy Jobs\n(per million, cumul.)",
  "jobs_Fossil"             = "Fossil Energy Jobs\n(per million, cumul.)"
)

p4d_percap_dat <- outcomes_nz %>%
  filter(!is.na(Region_label),
         Region_label != "Aggregated R10 regions") %>%
  select(Model, Scenario, Pathway, Ambition, Region_label,
         any_of(names(p4d_percap_outcomes))) %>%
  pivot_longer(cols = any_of(names(p4d_percap_outcomes)),
               names_to = "outcome_col", values_to = "val") %>%
  filter(!is.na(val), val > 0) %>%
  mutate(
    outcome_lab  = p4d_percap_outcomes[outcome_col],
    outcome_lab  = factor(outcome_lab, levels = unname(p4d_percap_outcomes)),
    Region_label = factor(Region_label,
                          levels = rev(unname(REGION_LABELS_FIG[regions_r10]))),
    PathAmb = factor(paste0(Pathway, "\n", Ambition), levels = c(
      "High-CDR\n1.5C (High-Ambition)", "High-CDR\n2C (Medium-Ambition)",
      "High-RE\n1.5C (High-Ambition)",  "High-RE\n2C (Medium-Ambition)"
    ))
  ) %>%
  group_by(Pathway, Ambition, PathAmb, Region_label, outcome_lab) %>%
  summarise(
    med = median(val, na.rm = TRUE),
    q25 = quantile(val, 0.25, na.rm = TRUE),
    q75 = quantile(val, 0.75, na.rm = TRUE),
    .groups = "drop"
  )



# Helper to build p4d_percap for a given ambition filter
make_p4d_percap <- function(dat, amb_filter, title_sfx, fname, w=14, h=10) {
  d <- if (is.null(amb_filter)) dat else
    dat %>% filter(str_detect(as.character(PathAmb), fixed(amb_filter)))
  cols_use  <- P4D_COLS[names(P4D_COLS)   %in% unique(as.character(d$PathAmb))]
  shape_use <- P4D_SHAPES[names(P4D_SHAPES) %in% unique(as.character(d$PathAmb))]
  lab_use   <- names(cols_use) %>%
    str_replace("High-CDR\\n1.5C.*", "High-CDR · 1.5C") %>%
    str_replace("High-CDR\\n2C.*",   "High-CDR · 2C") %>%
    str_replace("High-RE\\n1.5C.*",  "High-RE · 1.5C") %>%
    str_replace("High-RE\\n2C.*",    "High-RE · 2C")
  
  # divider lines between regions
  region_levels <- rev(unname(REGION_LABELS_FIG[regions_r10]))
  dividers <- tibble(
    yint = seq(1.5, length(region_levels) - 0.5, by = 1)
  )
  
  p <- ggplot(d, aes(x = med, y = Region_label,
                     colour = PathAmb, shape = PathAmb)) +
    geom_hline(data = dividers, aes(yintercept = yint),
               inherit.aes = FALSE,
               colour = "grey70", linewidth = 0.5, linetype = "solid") +
    geom_linerange(aes(xmin = q25, xmax = q75),
                   linewidth = 1.0, alpha = 0.55,
                   position = position_dodge(width = 0.65)) +
    geom_point(size = 2.8, position = position_dodge(width = 0.65)) +
    facet_wrap(~ outcome_lab, scales = "free_x", ncol = 2) +
    scale_colour_manual(values = cols_use,  labels = lab_use,
                        name = "Pathway x Ambition") +
    scale_shape_manual(values  = shape_use, labels = lab_use,
                       name = "Pathway x Ambition") +
    scale_x_continuous(labels = comma_format()) +
    labs(
      title    = paste0("P4d (per-capita): Wellbeing Outcomes per Million Population", title_sfx),
      subtitle = paste0(
        "Point = median; bar = IQR  ·  Per million 2020 population\n",
        "Blues = High-CDR; Reds = High-RE  ·  Filled circle = 1.5C; triangle = 2C"
      ),
      x = "Outcome value per million population (median +/- IQR)", y = NULL,
      caption  = paste0(
        "Outcomes cumulated to ambition net-zero window (1.5C: 2020-2060; 2C: 2020-2075).\n",
        "World-level tercile classification.\n",
        "For mortality and headcount: lower = better. For jobs: higher = better."
      )
    ) +
    theme_c() +
    theme(legend.position    = "bottom",
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3))
  sc(p, fname, w, h)
}

make_p4d_percap(p4d_percap_dat, "1.5C",
                " — 1.5C (High-Ambition)",
                "P4d_percap_outcomes_dotplot_15C.png")
make_p4d_percap(p4d_percap_dat, "2C",
                " — 2C (Medium-Ambition)",
                "P4d_percap_outcomes_dotplot_2C.png")
make_p4d_percap(p4d_percap_dat, NULL,
                " — 1.5C and 2C Combined",
                "P4d_percap_outcomes_dotplot_combined.png")
cat("  P4d per-capita dotplots (1.5C, 2C, combined) saved.\n")

# P4e — Two Spearman heatmaps: regionally-defined pathways
# P4e_excl    = mutually exclusive (high-CDR only OR high-RE only per region)
# P4e_overlap = overlapping (high-CDR or high-RE, including both, per region)
# X-axis = region-level deployment; classification within each region
cat("Building Spearman heatmaps (P4e)...\n")

# Local ambition grouping for P4e: C3-only for 2C, matching original figF2
assign_amb_p4e <- function(df, col = "Category") {
  df %>% mutate(Ambition = case_when(
    .data[[col]] %in% c("C1", "C2") ~ "1.5C (High-Ambition)",
    .data[[col]] == "C3"            ~ "2C (Medium-Ambition)",
    TRUE ~ NA_character_
  ))
}

# Outcome columns and labels — same order as original figF2
outcomes_f2_p4e <- c(
  "cumulative_deaths_mln"        = "Air Pollution Mortality\n(million deaths, 2020–net-zero)",
  "cumulative_implied_CO2_GtCO2" = "CO2 cost of closing\nDLE gap (GtCO2, 2020–net-zero)",
  "cumulative_gap_EJ"            = "DLE Energy Gap\n(EJ, cumul. 2020–net-zero)",
  "mean_headcount_millions"      = "Energy Deprivation\nHeadcount (millions, 2020–net-zero)",
  "jobs_Fossil"                  = "Fossil Energy Jobs\n(thousands, cumul. 2020–net-zero)",
  "jobs_Renewables"              = "Renewable Energy Jobs\n(thousands, cumul. 2020–net-zero)"
)

# Region labels — R10 regions only (World added separately below)
region_labels_f2_p4e <- c(
  "R10NORTH_AM"            = "N. America",
  "R10EUROPE"              = "Europe",
  "R10INDIA+"              = "India+",
  "R10CHINA+"              = "China+",
  "R10AFRICA"              = "Africa",
  "Aggregated R10 regions" = "Aggregated R10 regions"
)

# P4e FINAL HEATMAP DESIGN:
#   X predictor  : World-level cumulative CDR or RE (2020-2100)
#                  Source: real "World" region from compass_interp if available,
#                  otherwise summed R10 — same source as pathway_tercile classification.
#   Y outcome    : Region-specific wellbeing outcomes (2020-2050)
#   Scenario set : Pooled High-CDR + High-RE (top world-level tercile, mutually exclusive)
#   Rows         : Each R10 region + "Aggregated R10 regions" (no World row)
#   Facets       : Ambition (1.5C / 2C) × Deployment type (CDR / RE)

# Step 1: World-level deployment scalars — read directly from pathway_tercile,
# which already carries total_cdr and total_re computed from world_deploy_for_classification
# (real World rows or summed R10 fallback, see Section 5b of analysis script).
world_deploy_p4e <- pathway_tercile %>%
  select(Model, Scenario, Category, Ambition,
         total_cdr, total_re,
         high_cdr_only, high_re_only) %>%
  rename(world_cdr = total_cdr, world_re = total_re)

cat("  World-level pathway counts for P4e (mutually exclusive):\n")
world_deploy_p4e %>%
  group_by(Ambition) %>%
  summarise(high_cdr = sum(high_cdr_only, na.rm=TRUE),
            high_re  = sum(high_re_only,  na.rm=TRUE),
            .groups = "drop") %>% print()

# Step 3: Outcomes — R10 regions + Aggregated R10 regions (no World row)
# Join world-level deployment onto region-specific outcomes
all_region_labels_p4e <- c(
  "R10NORTH_AM"            = "N. America",
  "R10EUROPE"              = "Europe",
  "R10INDIA+"              = "India+",
  "R10CHINA+"              = "China+",
  "R10AFRICA"              = "Africa",
  "Aggregated R10 regions" = "Aggregated R10 regions"
)

regions_for_p4e <- c(regions_r10, "Aggregated R10 regions")

p4e_base <- df_master %>%
  filter(Variable == "Total CDR",
         Region %in% regions_for_p4e) %>%
  select(Model, Scenario, Category, Region, Ambition,
         any_of(names(outcomes_f2_p4e))) %>%
  # Join on Model+Scenario only — pathway_tercile has one row per scenario
  # (not per region), so joining on Category/Ambition can cause mismatches
  left_join(
    world_deploy_p4e %>%
      select(Model, Scenario,
             world_cdr, world_re, high_cdr_only, high_re_only) %>%
      distinct(Model, Scenario, .keep_all = TRUE),
    by = c("Model","Scenario")
  ) %>%
  filter(!is.na(Ambition))

# Step 4: Pooled High-CDR + High-RE (mutually exclusive, world-level classification)
p4e_pooled_dat <- p4e_base %>% filter(high_cdr_only | high_re_only)

cat("  P4e pooled (High-CDR + High-RE, world-level) rows:", nrow(p4e_pooled_dat), "\n")

# Step 5: Spearman correlations — world-level deployment vs region-specific outcomes
compute_sp <- function(data, x_col, y_col) {
  d <- data %>% filter(!is.na(.data[[x_col]]), !is.na(.data[[y_col]]),
                       .data[[x_col]] > 0)
  if (nrow(d) < 5) return(list(rho=NA_real_, pval=NA_real_))
  test <- tryCatch(
    cor.test(d[[x_col]], d[[y_col]], method="spearman", exact=FALSE),
    error = function(e) NULL)
  if (is.null(test)) return(list(rho=NA_real_, pval=NA_real_))
  list(rho=test$estimate, pval=test$p.value)
}

make_p4e_final_results <- function(data) {
  map_dfr(names(outcomes_f2_p4e), function(out_var) {
    if (!out_var %in% names(data)) return(NULL)
    map_dfr(list(list(col="world_cdr", lab="Total CDR"),
                 list(col="world_re",  lab="Renewable Capacity")),
            function(dep) {
              map_dfr(c("1.5C (High-Ambition)","2C (Medium-Ambition)"), function(amb) {
                map_dfr(regions_for_p4e, function(reg) {
                  d   <- data %>% filter(Region == reg, Ambition == amb)
                  res <- compute_sp(d, dep$col, out_var)
                  tibble(
                    outcome_lab  = outcomes_f2_p4e[out_var],
                    deployment   = dep$lab,
                    Region_label = unname(all_region_labels_p4e[reg]),
                    Ambition     = amb,
                    rho          = res$rho,
                    n_scen       = nrow(d),
                    cell_label   = if_else(
                      !is.na(res$rho),
                      paste0(sprintf("%.2f", res$rho),
                             if_else(!is.na(res$pval) & res$pval < 0.05, "*", "")),
                      "")
                  )
                })
              })
            })
  })
}

corr_results_p4e_final <- make_p4e_final_results(p4e_pooled_dat)
cat("  P4e final Spearman results rows:", nrow(corr_results_p4e_final), "\n")

# Keep old names for backwards compat
corr_results_excl    <- corr_results_p4e_final
corr_results_overlap <- corr_results_p4e_final

# compute_sp already defined above — used again by regional heatmaps below

region_level_order_p4e <- rev(unname(all_region_labels_p4e[regions_for_p4e]))

make_p4e_heatmap <- function(results, title_suffix, subtitle_note) {
  results %>%
    mutate(
      outcome_lab  = factor(outcome_lab,  levels = unname(outcomes_f2_p4e)),
      Region_label = factor(Region_label, levels = region_level_order_p4e),
      Ambition     = factor(Ambition,
                            levels = c("1.5C (High-Ambition)",
                                       "2C (Medium-Ambition)")),
      # Factor levels must exactly match the lab= values in make_p4e_final_results
      deployment   = factor(deployment,
                            levels = c("Renewable Capacity", "Total CDR"))
    ) %>%
    filter(!is.na(rho)) %>%
    ggplot(aes(x = outcome_lab, y = Region_label, fill = rho)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = cell_label), size = 2.8, colour = "black") +
    facet_grid(Ambition ~ deployment, scales = "free", space = "free") +
    scale_fill_distiller(
      palette = "RdBu", direction = 1,
      limits  = c(-1, 1), na.value = "grey92",
      name    = "Spearman \u03c1",
      breaks  = c(-1, -0.5, 0, 0.5, 1),
      labels  = c("-1.0","-0.5","0.0","0.5","1.0"),
      guide   = guide_colourbar(title.position = "top",
                                barwidth  = unit(6,"lines"),
                                barheight = unit(0.6,"lines"))
    ) +
    labs(
      title    = paste0("P4e: Spearman Correlations — ", title_suffix),
      subtitle = paste0(
        "COMPASS  \u00b7  Pooled High-CDR + High-RE  \u00b7  * = p < 0.05\n",
        subtitle_note
      ),
      x = NULL, y = NULL,
      caption  = paste0(
        "X-axis: world-level cumulative deployment (CDR or RE, 2020\u20132100).\n",
        "Y-axis: region-specific wellbeing outcomes (2020\u20132050).\n",
        "Scenarios: pooled High-CDR + High-RE (top world-level tercile, mutually exclusive).\n",
        "Grey = insufficient data (n < 5)."
      )
    ) +
    theme_c() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 7.5,
                                     lineheight = 1.1),
          panel.grid  = element_blank())
}

# P4e FINAL: Spearman correlation heatmap
p4e <- make_p4e_heatmap(
  corr_results_p4e_final,
  "World Deployment → Region-Specific Outcomes (Spearman ρ)",
  "Pooled High-CDR + High-RE  ·  World-level tercile classification  ·  2020–net-zero outcomes (1.5C=2060, 2C=2075)"
)
sc(p4e, "P4e_spearman_world_deploy_region_outcomes.png", 14, 10)

# P4e OLS companion heatmap — linear slope per cell
compute_ols_cell <- function(data, x_col, y_col) {
  d <- data %>% filter(!is.na(.data[[x_col]]), !is.na(.data[[y_col]]),
                       .data[[x_col]] > 0)
  if (nrow(d) < 5) return(list(slope=NA_real_, pval=NA_real_))
  tryCatch({
    m <- lm(as.formula(paste0("`", y_col, "` ~ `", x_col, "`")), data = d)
    list(slope = coef(m)[2], pval = summary(m)$coefficients[2, 4])
  }, error = function(e) list(slope=NA_real_, pval=NA_real_))
}

make_p4e_ols_results <- function(data) {
  map_dfr(names(outcomes_f2_p4e), function(out_var) {
    if (!out_var %in% names(data)) return(NULL)
    map_dfr(list(list(col="world_cdr", lab="Total CDR"),
                 list(col="world_re",  lab="Renewable Capacity")),
            function(dep) {
              map_dfr(c("1.5C (High-Ambition)","2C (Medium-Ambition)"), function(amb) {
                map_dfr(regions_for_p4e, function(reg) {
                  d   <- data %>% filter(Region == reg, Ambition == amb)
                  res <- compute_ols_cell(d, dep$col, out_var)
                  tibble(
                    outcome_lab  = outcomes_f2_p4e[out_var],
                    deployment   = dep$lab,
                    Region_label = unname(all_region_labels_p4e[reg]),
                    Ambition     = amb,
                    rho          = res$slope,
                    n_scen       = nrow(d),
                    cell_label   = if_else(
                      !is.na(res$slope),
                      paste0(formatC(res$slope, format="e", digits=1),
                             if_else(!is.na(res$pval) & res$pval < 0.05, "*", "")),
                      "")
                  )
                })
              })
            })
  })
}

ols_results_p4e <- make_p4e_ols_results(p4e_pooled_dat)

# OLS heatmap: symmetric colour scale, scientific notation in cells
make_p4e_ols_heatmap <- function(results) {
  max_abs <- max(abs(results$rho[!is.na(results$rho)]))
  if (is.na(max_abs) || max_abs == 0) max_abs <- 1
  results %>%
    mutate(
      outcome_lab  = factor(outcome_lab,  levels = unname(outcomes_f2_p4e)),
      Region_label = factor(Region_label, levels = region_level_order_p4e),
      Ambition     = factor(Ambition,
                            levels = c("1.5C (High-Ambition)","2C (Medium-Ambition)")),
      deployment   = factor(deployment,   levels = c("Renewable Capacity","Total CDR"))
    ) %>%
    filter(!is.na(rho)) %>%
    ggplot(aes(x = outcome_lab, y = Region_label, fill = rho)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = cell_label), size = 2.2, colour = "black") +
    facet_grid(Ambition ~ deployment, scales = "free", space = "free") +
    scale_fill_distiller(
      palette = "RdBu", direction = 1,
      limits  = c(-max_abs, max_abs), na.value = "grey92",
      name    = "OLS slope",
      guide   = guide_colourbar(title.position = "top",
                                barwidth  = unit(6,"lines"),
                                barheight = unit(0.6,"lines"))
    ) +
    labs(
      title    = "P4e (OLS): Linear Slopes — World Deployment → Region-Specific Outcomes",
      subtitle = "COMPASS  ·  Pooled High-CDR + High-RE  ·  * = p < 0.05\nWorld-level tercile classification  ·  2020–net-zero outcomes (1.5C=2060, 2C=2075)",
      x = NULL, y = NULL,
      caption  = paste0(
        "X-axis: world-level cumulative deployment (2020–2100).\n",
        "Y-axis: region-specific outcomes (1.5C: 2020–2060; 2C: 2020–2075).\n",
        "Slope = change in outcome per unit deployment increase.  Grey = n < 5."
      )
    ) +
    theme_c() +
    theme(axis.text.x = element_text(angle=35, hjust=1, size=7.5, lineheight=1.1),
          panel.grid  = element_blank())
}

p4e_ols <- make_p4e_ols_heatmap(ols_results_p4e)
sc(p4e_ols, "P4e_OLS_world_deploy_region_outcomes.png", 14, 10)
cat("  P4e OLS heatmap saved.\n")

# =============================================================================
# P4e_reg — Spearman heatmap: REGION-LEVEL deployment vs region outcomes
#
# Unlike P4e (world-level CDR/RE vs region outcomes), here the x-axis is the
# region's OWN cumulative deployment. Each correlation is computed within a
# single region × ambition group — so it answers: "within Africa, do scenarios
# with more CDR deployed in Africa have better/worse African outcomes?"
#
# Two versions produced:
#   P4e_reg_highonly  — restricted to High-CDR + High-RE scenarios (top tercile)
#   P4e_reg_all       — all scenarios (C1+C2 or C3+C4 within each ambition group)
# =============================================================================
cat("Building regional Spearman heatmaps (P4e_reg)...\n")

# Region-level deployment from df_master (Total_Value when Variable == "Total CDR"
# is the cumulative CDR for that specific region)
region_deploy <- df_master %>%
  filter(Variable %in% c("Total CDR","Renewable Capacity"),
         Region %in% regions_r10) %>%
  select(Model, Scenario, Category, Region, Variable, Total_Value) %>%
  pivot_wider(names_from = Variable, values_from = Total_Value) %>%
  rename(region_cdr = `Total CDR`, region_re = `Renewable Capacity`) %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition))

# Outcomes at region level
region_outcomes <- df_master %>%
  filter(Variable == "Total CDR", Region %in% regions_r10) %>%  # R10 only, no All Regions
  select(Model, Scenario, Category, Region,
         any_of(names(outcomes_f2_p4e))) %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition))

# Combined: deployment + outcomes, same region (R10 only)
corr_data_reg <- region_deploy %>%
  filter(Region %in% regions_r10) %>%
  left_join(region_outcomes %>% select(-Category, -Ambition),
            by = c("Model","Scenario","Region"))

cat("  corr_data_reg rows:", nrow(corr_data_reg),
    "| regions:", paste(sort(unique(corr_data_reg$Region)), collapse=", "), "\n")

# Helper: build correlation results for a given data subset
make_reg_corr <- function(data, label) {
  map_dfr(names(outcomes_f2_p4e), function(out_var) {
    if (!out_var %in% names(data)) return(NULL)
    map_dfr(list(list(col="region_cdr", lab="Total CDR"),
                 list(col="region_re",  lab="Renewable Capacity")), function(dep) {
                   map_dfr(c("1.5C (High-Ambition)","2C (Medium-Ambition)"), function(amb) {
                     map_dfr(regions_r10, function(reg) {
                       d <- data %>% filter(Region == reg, Ambition == amb)
                       res <- compute_sp(d, dep$col, out_var)
                       tibble(
                         outcome_lab  = outcomes_f2_p4e[out_var],
                         deployment   = dep$lab,
                         Region_label = unname(REGION_LABELS_FIG[reg]),
                         Ambition     = amb,
                         rho          = res$rho,
                         cell_label   = if_else(
                           !is.na(res$rho),
                           paste0(sprintf("%.2f", res$rho),
                                  if_else(!is.na(res$pval) & res$pval < 0.05, "*", "")),
                           ""),
                         scenario_set = label
                       )
                     })
                   })
                 })
  })
}

# ── World-level tercile classification ──────────────────────────────────────────
# Within each region × ambition group, rank scenarios by that region's own
# CDR and RE deployment independently, then classify into terciles.
# This is purely region-specific — a scenario can be high-CDR in Africa but
# low-CDR in Europe.

regional_terciles <- corr_data_reg %>%
  group_by(Region, Ambition) %>%
  mutate(
    cdr_tercile    = ntile(region_cdr, 3),
    re_tercile     = ntile(region_re,  3),
    high_cdr       = cdr_tercile == 3,
    high_re        = re_tercile  == 3,
    # Mutually exclusive
    high_cdr_only  = high_cdr & !high_re,
    high_re_only   = high_re  & !high_cdr,
    # Overlapping
    high_any       = high_cdr | high_re
  ) %>%
  ungroup()

cat("  Regional tercile counts (mutually exclusive, pooled across regions):\n")
regional_terciles %>%
  group_by(Ambition) %>%
  summarise(
    high_cdr_only = sum(high_cdr_only, na.rm=TRUE),
    high_re_only  = sum(high_re_only,  na.rm=TRUE),
    .groups="drop"
  ) %>% print()

# Updated correlation builder — uses region-specific tercile classification
# rather than global pathway filter
make_reg_corr_regional <- function(data_with_terciles, filter_expr, label) {
  map_dfr(names(outcomes_f2_p4e), function(out_var) {
    if (!out_var %in% names(data_with_terciles)) return(NULL)
    map_dfr(list(list(col="region_cdr", lab="Total CDR"),
                 list(col="region_re",  lab="Renewable Capacity")), function(dep) {
                   map_dfr(c("1.5C (High-Ambition)","2C (Medium-Ambition)"), function(amb) {
                     map_dfr(regions_r10, function(reg) {
                       d <- data_with_terciles %>%
                         filter(Region == reg, Ambition == amb) %>%
                         filter(!!rlang::parse_expr(filter_expr))
                       res <- compute_sp(d, dep$col, out_var)
                       tibble(
                         outcome_lab  = outcomes_f2_p4e[out_var],
                         deployment   = dep$lab,
                         Region_label = unname(REGION_LABELS_FIG[reg]),
                         Ambition     = amb,
                         rho          = res$rho,
                         n_scen       = nrow(d),
                         cell_label   = if_else(
                           !is.na(res$rho),
                           paste0(sprintf("%.2f", res$rho),
                                  if_else(!is.na(res$pval) & res$pval < 0.05, "*", "")),
                           ""),
                         scenario_set = label
                       )
                     })
                   })
                 })
  })
}

# Version 1: Mutually exclusive (top tercile CDR only OR RE only, within region)
corr_results_reg_excl <- make_reg_corr_regional(
  regional_terciles, "high_cdr_only | high_re_only", "High (mutually exclusive)"
)

# Version 2: Overlapping (top tercile CDR or RE, within region)
corr_results_reg_overlap <- make_reg_corr_regional(
  regional_terciles, "high_any", "High (overlapping)"
)

# Version 3: All scenarios (no tercile filter)
corr_results_reg_all <- make_reg_corr_regional(
  regional_terciles, "TRUE", "All scenarios"
)

make_reg_heatmap <- function(results, title_suffix, subtitle_suffix) {
  results %>%
    mutate(
      outcome_lab  = factor(outcome_lab,  levels = unname(outcomes_f2_p4e)),
      Region_label = factor(Region_label,
                            levels = rev(unname(REGION_LABELS_FIG[regions_r10]))),
      Ambition     = factor(Ambition,
                            levels = c("1.5C (High-Ambition)",
                                       "2C (Medium-Ambition)")),
      deployment   = factor(deployment,
                            levels = c("Renewable Capacity","Total CDR"))
    ) %>%
    filter(!is.na(rho)) %>%
    ggplot(aes(x = outcome_lab, y = Region_label, fill = rho)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = cell_label), size = 2.8, colour = "black") +
    facet_grid(Ambition ~ deployment, scales = "free", space = "free") +
    scale_fill_distiller(
      palette = "RdBu", direction = 1,
      limits  = c(-1, 1), na.value = "grey92",
      name    = "Spearman ρ",
      breaks  = c(-1, -0.5, 0, 0.5, 1),
      labels  = c("-1.0","-0.5","0.0","0.5","1.0"),
      guide   = guide_colourbar(title.position = "top",
                                barwidth  = unit(6,"lines"),
                                barheight = unit(0.6,"lines"))
    ) +
    labs(
      title    = paste0("Spearman Correlations — Regional Deployment → Regional Outcomes"),
      subtitle = paste0(subtitle_suffix,
                        "  ·  * = p < 0.05  ·  1.5C = C1+C2; 2C = C3+C4"),
      x = NULL, y = NULL,
      caption  = paste0(
        "X-axis = region own cumulative deployment (CDR or RE, 2020-2100).\n",
        "Y-axis = region own wellbeing outcome.  Grey = insufficient data (n < 5).\n",
        "Each cell is a within-region Spearman correlation across scenarios."
      )
    ) +
    theme_c() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 7.5,
                                     lineheight = 1.1),
          panel.grid  = element_blank())
}

p4e_reg_excl <- make_reg_heatmap(
  corr_results_reg_excl,
  "High (mutually exclusive)",
  "COMPASS  ·  High-CDR + High-RE scenarios, mutually exclusive (top tercile)"
)
sc(p4e_reg_excl, "P4e_reg_exclusive_spearman.png", 14, 10)

p4e_reg_overlap <- make_reg_heatmap(
  corr_results_reg_overlap,
  "High (overlapping)",
  "COMPASS  ·  High-CDR or High-RE scenarios, overlapping allowed (top tercile)"
)
sc(p4e_reg_overlap, "P4e_reg_overlap_spearman.png", 14, 10)

p4e_reg_all <- make_reg_heatmap(
  corr_results_reg_all,
  "All scenarios",
  "COMPASS  ·  All scenarios"
)
sc(p4e_reg_all, "P4e_reg_all_spearman.png", 14, 10)

cat("  P4e regional heatmaps saved (exclusive, overlap, all).\n")

# =============================================================================
# P4e PER-CAPITA: Spearman heatmap — 4 outcomes, per million population
# =============================================================================
cat("Building per-capita Spearman heatmap...\n")

OUTCOMES_4_PERCAP <- c(
  "cumulative_deaths_mln"   = "Air Pollution Mortality\n(deaths per million, cumul.)",
  "mean_headcount_millions" = "Energy Deprivation\nHeadcount (per million, mean)",
  "jobs_Renewables"         = "Renewable Energy Jobs\n(per million, cumul.)",
  "jobs_Fossil"             = "Fossil Energy Jobs\n(per million, cumul.)"
)

pop_2020_r10_p4e <- pop_ts %>%
  filter(Region %in% regions_r10, Year == 2020) %>%
  group_by(Region) %>%
  summarise(pop_mln = median(Value, na.rm = TRUE), .groups = "drop")
pop_2020_agg_p4e <- sum(pop_2020_r10_p4e$pop_mln)

p4e_pooled_percap <- p4e_pooled_dat %>%
  left_join(pop_2020_r10_p4e, by = "Region") %>%
  mutate(
    pop_use                 = if_else(Region == "Aggregated R10 regions",
                                      pop_2020_agg_p4e, pop_mln),
    cumulative_deaths_mln   = (cumulative_deaths_mln * 1e6)   / pop_use,
    mean_headcount_millions = (mean_headcount_millions * 1e6) / pop_use,
    jobs_Renewables         = jobs_Renewables                 / pop_use,
    jobs_Fossil             = jobs_Fossil                     / pop_use
  )

# Spearman on per-capita outcomes — returns rho AND pval
make_p4e_percap_results <- function(data) {
  map_dfr(names(OUTCOMES_4_PERCAP), function(out_var) {
    if (!out_var %in% names(data)) return(NULL)
    map_dfr(list(list(col="world_cdr", lab="Total CDR"),
                 list(col="world_re",  lab="Renewable Capacity")), function(dep) {
                   map_dfr(c("1.5C (High-Ambition)","2C (Medium-Ambition)"), function(amb) {
                     map_dfr(regions_for_p4e, function(reg) {
                       d   <- data %>% filter(Region == reg, Ambition == amb)
                       res <- compute_sp(d, dep$col, out_var)
                       tibble(
                         outcome_lab  = OUTCOMES_4_PERCAP[out_var],
                         deployment   = dep$lab,
                         Region_label = unname(all_region_labels_p4e[reg]),
                         Ambition     = amb,
                         rho          = res$rho,
                         rho_pval     = res$pval,
                         n_scen       = nrow(d),
                         cell_label   = if_else(!is.na(res$rho),
                                                paste0(sprintf("%.2f", res$rho),
                                                       if_else(!is.na(res$pval) & res$pval < 0.05, "*", "")), "")
                       )
                     })
                   })
                 })
  })
}

corr_percap <- make_p4e_percap_results(p4e_pooled_percap)
cat("  Per-capita Spearman rows:", nrow(corr_percap), "\n")

# Heatmap
p4e_percap <- corr_percap %>%
  mutate(
    outcome_lab  = factor(outcome_lab,  levels = unname(OUTCOMES_4_PERCAP)),
    Region_label = factor(Region_label, levels = region_level_order_p4e),
    Ambition     = factor(Ambition,     levels = c("1.5C (High-Ambition)","2C (Medium-Ambition)")),
    deployment   = factor(deployment,   levels = c("Renewable Capacity","Total CDR"))
  ) %>%
  filter(!is.na(rho)) %>%
  ggplot(aes(x = outcome_lab, y = Region_label, fill = rho)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = cell_label), size = 2.8, colour = "black") +
  facet_grid(Ambition ~ deployment, scales = "free", space = "free") +
  scale_fill_distiller(
    palette = "RdBu", direction = 1, limits = c(-1, 1), na.value = "grey92",
    name    = "Spearman rho",
    breaks  = c(-1, -0.5, 0, 0.5, 1),
    labels  = c("-1.0","-0.5","0.0","0.5","1.0"),
    guide   = guide_colourbar(title.position = "top",
                              barwidth = unit(6,"lines"), barheight = unit(0.6,"lines"))
  ) +
  labs(
    title    = "P4e (per-capita): Spearman Correlations — World Deployment -> Region Outcomes",
    subtitle = paste0(
      "COMPASS  ·  Pooled High-CDR + High-RE  ·  * = p < 0.05\n",
      "Outcomes per million 2020 population  ·  World-level tercile classification"
    ),
    x = NULL, y = NULL,
    caption  = "Outcomes normalised by 2020 regional population (per million). Grey = n < 5."
  ) +
  theme_c() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 7.5, lineheight = 1.1),
        panel.grid  = element_blank())

sc(p4e_percap, "P4e_percap_spearman_4outcomes.png", 12, 9)
cat("  P4e per-capita heatmap saved.\n")

# Spearman coefficient dot plot — patchwork, one panel per outcome
make_sp_panel <- function(dat, outcome, show_legend = FALSE) {
  d <- dat %>%
    filter(outcome_lab == outcome, !is.na(rho)) %>%
    mutate(
      Region_label = factor(Region_label, levels = region_level_order_p4e),
      sig_shape    = !is.na(rho_pval) & rho_pval < 0.05
    )
  if (nrow(d) == 0) return(NULL)
  ggplot(d, aes(x = rho, y = Region_label, colour = Ambition, shape = sig_shape)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
    geom_point(size = 2.5) +
    facet_wrap(~ deployment, scales = "free_x", nrow = 1) +
    scale_x_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1)) +
    scale_colour_manual(values = ACOLS, name = "Ambition") +
    scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1),
                       labels = c("TRUE" = "p < 0.05", "FALSE" = "p >= 0.05"),
                       name   = "Significance") +
    labs(title = outcome, x = "Spearman rho", y = NULL) +
    theme_c(9) +
    theme(strip.text.x   = element_text(size = 8),
          plot.title     = element_text(size = 9, face = "bold"),
          legend.position = if (show_legend) "bottom" else "none")
}

sp_outcomes_order <- unname(OUTCOMES_4_PERCAP)

panels_sp <- map(sp_outcomes_order, ~ make_sp_panel(
  corr_percap, .x,
  show_legend = (.x == last(sp_outcomes_order))
))
panels_sp <- compact(panels_sp)

p4e_sp_coef <- wrap_plots(panels_sp, ncol = 1) +
  plot_annotation(
    title    = "P4e (Spearman coef): Correlation Coefficients — World Deployment -> Region Outcomes",
    subtitle = paste0(
      "Spearman rho  ·  Pooled High-CDR + High-RE  ·  Per-capita outcomes\n",
      "Filled = p < 0.05; open = not significant  ·  Each outcome has independent x-axis"
    ),
    caption  = "Positive rho = more deployment -> higher (worse) outcome. Exception: jobs — positive = better.",
    theme    = theme_c(9)
  )

sc(p4e_sp_coef, "P4e_spearman_coef_plot_4outcomes.png", 14, 16)
cat("  P4e Spearman coefficient plot saved.\n")

# P4f — Regression coefficient figures now generated from reg_all CSV below
# (P4f_coef_aggregated_4outcomes.png and P4f_coef_regional_4outcomes.png)

# =============================================================================
# SECTION 5: EQUITY + ROBUSTNESS + TAKEAWAYS
# =============================================================================
cat("\n=== SECTION 5: Equity + robustness ===\n")

# P5a — Gini inequality across all outcomes
gini_coef2 <- function(x, w = rep(1, length(x))) {
  x <- x[!is.na(x) & !is.na(w) & is.finite(x) & is.finite(w)]
  w <- w[!is.na(x) & !is.na(w) & is.finite(x) & is.finite(w)]
  if (length(x) < 2 || sum(x, na.rm=TRUE) == 0) return(NA_real_)
  ord <- order(x); x <- x[ord]; w <- w[ord]
  cumw <- cumsum(w)/sum(w); cumx <- cumsum(x*w)/sum(x*w)
  n    <- length(x)
  lorenz_area <- sum(diff(c(0,cumw)) * (c(0,cumx[-n]) + cumx) / 2)
  1 - 2*lorenz_area
}

gini_outcomes <- c(
  "cum_deaths_nz"           = "Air Pollution\nMortality",
  "mean_headcount_millions" = "Energy\nDeprivation\nHeadcount",
  "jobs_Fossil"             = "Fossil\nEnergy Jobs",
  "jobs_Renewables"         = "Renewable\nEnergy Jobs"
)

pop_lkp <- compass_filtered %>%
  filter(Variable == "Population", Year == 2030,
         Region %in% regions_r10) %>%
  group_by(Model, Scenario, Region) %>%
  summarise(pop = median(Value, na.rm=TRUE), .groups="drop")

gini_data <- outcomes_nz %>%
  left_join(pop_lkp, by = c("Model","Scenario","Region")) %>%
  filter(!is.na(pop)) %>%
  group_by(Model, Scenario, Pathway, Ambition, Group) %>%
  summarise(
    across(any_of(names(gini_outcomes)),
           ~ gini_coef2(pmax(.x, 0, na.rm=FALSE), pop),
           .names = "gini_{.col}"),
    n_reg = n(),
    .groups = "drop"
  ) %>%
  filter(n_reg >= 4) %>%
  pivot_longer(starts_with("gini_"),
               names_to = "metric", values_to = "gini") %>%
  filter(!is.na(gini)) %>%
  mutate(
    outcome_col = str_remove(metric, "^gini_"),
    outcome_lab = gini_outcomes[outcome_col],
    outcome_lab = factor(outcome_lab, levels = unname(gini_outcomes))
  )

p5a <- ggplot(gini_data,
              aes(x = Group, y = gini,
                  fill = Group, colour = Group)) +
  geom_violin(alpha = 0.45, colour = NA, scale = "width", trim = TRUE) +
  geom_boxplot(width = 0.12, outlier.size = 0.3,
               colour = "grey30", fill = "white", alpha = 0.8) +
  stat_summary(fun = median, geom = "text",
               aes(label = sprintf("%.2f", after_stat(y))),
               vjust = -1.0, size = 2.5, colour = "grey30") +
  facet_wrap(~ outcome_lab, ncol = 2, scales = "free_y") +
  scale_fill_manual(values   = GROUP_COLS_4) +
  scale_colour_manual(values = GROUP_COLS_4) +
  labs(
    title    = "P5a: Regional Equity — Population-Weighted Gini Coefficients",
    subtitle = paste0(
      "Higher Gini = outcome more concentrated in fewer regions\n",
      "Does High-CDR concentrate bad outcomes in the Global South?"
    ),
    x = NULL, y = "Gini coefficient (across 5 R10 regions)",
    fill = "Pathway × Ambition", colour = "Pathway × Ambition",
    caption  = paste0(
      "Gini = 0: equal distribution across regions. Gini = 1: all outcome in one region.\n",
      "Weighted by 2030 regional population. Median values labelled.\n",
      "For mortality and headcount: higher Gini = more concentrated in fewer (poorer) regions.\n",
      "For jobs: higher Gini = employment less evenly distributed across regions."
    )
  ) +
  theme_c() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

sc(p5a, "P5a_gini_equity.png", 16, 10)

# P5a_reg — Gini by region (one panel per outcome × region)
gini_data_reg <- outcomes_nz %>%
  filter(!is.na(Region_label)) %>%
  left_join(pop_lkp, by=c("Model","Scenario","Region")) %>%
  filter(!is.na(pop)) %>%
  group_by(Model, Scenario, Pathway, Ambition, Group, Region_label) %>%
  summarise(
    across(any_of(names(gini_outcomes)),
           ~ if_else(.x > 0 & !is.na(.x), .x, NA_real_),
           .names="{.col}"),
    n_obs = n(), .groups="drop"
  ) %>%
  pivot_longer(any_of(names(gini_outcomes)),
               names_to="metric", values_to="val") %>%
  filter(!is.na(val)) %>%
  mutate(outcome_lab = factor(gini_outcomes[metric],
                              levels=unname(gini_outcomes)))

p5a_reg <- ggplot(gini_data_reg,
                  aes(x=Group, y=val, fill=Group, colour=Group)) +
  geom_violin(alpha=0.40, colour=NA, scale="width", trim=TRUE) +
  geom_boxplot(width=0.10, outlier.size=0.3,
               colour="grey30", fill="white", alpha=0.8) +
  facet_grid(outcome_lab ~ Region_label, scales="free_y") +
  scale_fill_manual(values=GROUP_COLS_4) +
  scale_colour_manual(values=GROUP_COLS_4) +
  labs(title="P5a (regional): Outcome Distributions by Region and Pathway",
       subtitle="Direct outcome values by region  ·  All four pathway × ambition groups",
       x=NULL, y=NULL) +
  theme_c(8) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        strip.text.y=element_text(angle=-90,size=7))
sc(p5a_reg, "P5a_outcomes_by_region.png", 20, 16)

# =============================================================================
# P4f_coef — Regression coefficient figure
# Reads reg_all from saved CSV, averages coefficients within ambition groups
# (C1+C2 -> 1.5C; C3+C4 -> 2C), plots by region and outcome.
#
# 2020–net-zero window (1.5C=2060, 2C=2075) used in analysis:
#   C1: 2020-2050  |  C2: 2020-2060  |  C3: 2020-2070  |  C4: 2020-2075
# Coefficients averaged within ambition group for display.
# =============================================================================
cat("Building P4f coefficient figure...\n")

reg_path <- file.path(
  "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_only",
  "compass_regression_results_all.csv"
)

if (file.exists(reg_path)) {
  reg_all <- read_csv(reg_path, show_col_types = FALSE)
  cat("  reg_all rows:", nrow(reg_all), "\n")
  cat("  regression types:", paste(unique(reg_all$regression), collapse=", "), "\n")
  cat("  y_var values:", paste(unique(reg_all$y_var), collapse=", "), "\n")
} else {
  warning("compass_regression_results_all.csv not found — skipping P4f")
  reg_all <- NULL
}

if (!is.null(reg_all) && nrow(reg_all) > 0) {
  
  # Outcome display labels — filtered to 4 main outcomes
  outcome_labels <- c(
    "cumulative_deaths_mln"   = "Air Pollution\nMortality",
    "mean_headcount_millions" = "Energy Deprivation\nHeadcount",
    "jobs_Renewables"         = "Renewable\nEnergy Jobs",
    "jobs_Fossil"             = "Fossil\nEnergy Jobs"
  )
  
  # Average coefficients within C1+C2 (1.5C) and C3+C4 (2C) ambition groups
  # reg_all has one row per Region x Ambition x regression type already,
  # but Ambition is already grouped (C1+C2 / C3+C4) from the analysis script
  reg_plot <- reg_all %>%
    filter(
      regression %in% c("A_All_CDR", "B_All_RE"),
      Region %in% regions_r10,
      !is.na(estimate)
    ) %>%
    mutate(
      outcome_lab  = outcome_labels[y_var],
      outcome_lab  = factor(outcome_lab, levels = unname(outcome_labels)),
      Region_label = REGION_LABELS_FIG[Region],
      Region_label = factor(Region_label,
                            levels = rev(unname(REGION_LABELS_FIG[regions_r10]))),
      deployment   = if_else(regression == "A_All_CDR",
                             "Total CDR", "Renewable Capacity"),
      deployment   = factor(deployment,
                            levels = c("Total CDR", "Renewable Capacity"))
    ) %>%
    filter(!is.na(outcome_lab), !is.na(Region_label))
  
  cat("  reg_plot rows after filter:", nrow(reg_plot), "\n")
  cat("  Ambition groups:", paste(unique(reg_plot$Ambition), collapse=", "), "\n")
  
  # Build coefficient plot
  p4f <- ggplot(reg_plot,
                aes(x = estimate, y = Region_label,
                    colour = Ambition, shape = significant)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.6) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.25, linewidth = 0.5, alpha = 0.7) +
    geom_point(size = 2.5) +
    facet_grid(outcome_lab ~ deployment, scales = "free") +
    scale_colour_manual(values = ACOLS, name = "Ambition group") +
    scale_shape_manual(values  = c("TRUE" = 16, "FALSE" = 1),
                       labels  = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
                       name    = "Significance") +
    labs(
      title    = "P4f: Regression Coefficients — Deployment → Wellbeing Outcomes",
      subtitle = paste0(
        "Log-log OLS  ·  All scenarios  ·  By region and ambition group\n",
        "Standardised 2020–net-zero outcome window (1.5C=2060, 2C=2075)  ·  World-level tercile classification\n",
        "Coefficients averaged within 1.5C (C1+C2) and 2C (C3+C4) for display"
      ),
      x = "Log-log elasticity (95% CI)", y = NULL,
      caption = paste0(
        "Filled point = p < 0.05; open = not significant.\n",
        "Positive coefficient = more deployment associated with higher (worse) outcome.\n",
        "Exception: jobs — positive = more jobs (better)."
      )
    ) +
    theme_c() +
    theme(
      strip.text.y = element_text(angle = -90, size = 8),
      strip.text.x = element_text(size = 9)
    )
  
  sc(p4f, "P4f_loglog_coefficients.png", 14, 16)
  cat("  P4f log-log saved.\n")
  
  # P4f OLS — raw linear regression coefficients
  if (!is.null(reg_all) && any(str_detect(reg_all$regression, "_OLS$"))) {
    reg_plot_ols <- reg_all %>%
      filter(regression %in% c("A_All_CDR_OLS","B_All_RE_OLS"),
             Region %in% regions_r10, !is.na(estimate)) %>%
      mutate(
        outcome_lab  = outcome_labels[y_var],
        outcome_lab  = factor(outcome_lab, levels = unname(outcome_labels)),
        Region_label = REGION_LABELS_FIG[Region],
        Region_label = factor(Region_label,
                              levels = rev(unname(REGION_LABELS_FIG[regions_r10]))),
        deployment   = if_else(str_detect(regression, "CDR"),
                               "Total CDR", "Renewable Capacity"),
        deployment   = factor(deployment, levels = c("Total CDR","Renewable Capacity"))
      ) %>%
      filter(!is.na(outcome_lab), !is.na(Region_label))
    
    if (nrow(reg_plot_ols) > 0) {
      p4f_ols <- ggplot(reg_plot_ols,
                        aes(x = estimate, y = Region_label,
                            colour = Ambition, shape = significant)) +
        geom_vline(xintercept = 0, linetype = "dashed",
                   colour = "grey40", linewidth = 0.6) +
        geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                       height = 0.25, linewidth = 0.5, alpha = 0.7) +
        geom_point(size = 2.5) +
        facet_grid(outcome_lab ~ deployment, scales = "free") +
        scale_colour_manual(values = ACOLS, name = "Ambition group") +
        scale_shape_manual(values = c("TRUE"=16,"FALSE"=1),
                           labels = c("TRUE"="p < 0.05","FALSE"="p ≥ 0.05"),
                           name = "Significance") +
        labs(
          title    = "P4f (OLS): Raw Linear Regression Coefficients — Deployment → Outcomes",
          subtitle = paste0(
            "Raw OLS (untransformed)  ·  All scenarios  ·  By region\n",
            "2020–net-zero window (1.5C=2060, 2C=2075)  ·  World-level tercile classification"
          ),
          x = "OLS slope (95% CI)", y = NULL,
          caption = "Coefficient = change in outcome per unit increase in deployment."
        ) +
        theme_c() +
        theme(strip.text.y = element_text(angle=-90, size=8))
      sc(p4f_ols, "P4f_OLS_coefficients.png", 14, 16)
      cat("  P4f OLS saved.\n")
    }
  }
  
} else {
  cat("  Skipping P4f — reg_all not available.\n")
}

# =============================================================================
# P4f_new: Coefficient plots filtered to 4 main outcomes
#   - Aggregated across R10 (one point per ambition group per outcome)
#   - Regional (one point per region per ambition group per outcome)
# Same 4 outcomes as P4a: mortality, headcount, RE jobs, fossil jobs
# =============================================================================

COEF_OUTCOMES_4 <- c(
  "cumulative_deaths_mln"   = "Air Pollution\nMortality",
  "mean_headcount_millions" = "Energy Deprivation\nHeadcount",
  "jobs_Renewables"         = "Renewable\nEnergy Jobs",
  "jobs_Fossil"             = "Fossil\nEnergy Jobs"
)

if (!is.null(reg_all) && nrow(reg_all) > 0) {
  
  reg_4 <- reg_all %>%
    filter(
      regression %in% c("A_All_CDR", "B_All_RE"),
      y_var %in% names(COEF_OUTCOMES_4),
      !is.na(estimate)
    ) %>%
    mutate(
      outcome_lab = factor(COEF_OUTCOMES_4[y_var],
                           levels = unname(COEF_OUTCOMES_4)),
      deployment  = factor(
        if_else(regression == "A_All_CDR", "Total CDR", "Renewable Capacity"),
        levels = c("Total CDR", "Renewable Capacity")
      )
    ) %>%
    filter(!is.na(outcome_lab))
  
  # ── Aggregated: median coefficient across regions ─────────────────────────
  reg_4_agg <- reg_4 %>%
    group_by(Ambition, outcome_lab, deployment) %>%
    summarise(
      conf.low  = median(conf.low,  na.rm = TRUE),
      conf.high = median(conf.high, na.rm = TRUE),
      estimate  = median(estimate,  na.rm = TRUE),
      pct_sig   = mean(significant, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    mutate(sig_agg = pct_sig >= 0.5)
  
  p4f_agg <- ggplot(reg_4_agg,
                    aes(x = estimate, y = Ambition,
                        colour = Ambition, shape = sig_agg)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.6) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.2, linewidth = 0.6, alpha = 0.7) +
    geom_point(size = 3) +
    facet_grid(outcome_lab ~ deployment, scales = "free") +
    scale_colour_manual(values = ACOLS, name = "Ambition") +
    scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1),
                       labels = c("TRUE" = "p < 0.05 in >=50% regions",
                                  "FALSE" = "p >= 0.05"),
                       name   = "Significance") +
    labs(
      title    = "P4f (aggregated): Regression Coefficients — Deployment -> Wellbeing Outcomes",
      subtitle = "Median elasticity across R10 regions  ·  Log-log OLS  ·  High-CDR and High-RE scenarios",
      x = "Median log-log elasticity (95% CI)", y = NULL,
      caption  = paste0(
        "Filled = p < 0.05 in majority of regions; open = not significant.\n",
        "Positive = more deployment associated with higher (worse) outcome. Exception: jobs — positive = better."
      )
    ) +
    theme_c() +
    theme(strip.text.y = element_text(angle = -90, size = 9),
          strip.text.x = element_text(size = 9))
  
  sc(p4f_agg, "P4f_coef_aggregated_4outcomes.png", 12, 10)
  cat("  P4f aggregated 4-outcome coefficient plot saved.\n")
  
  # ── Regional: one point per region ────────────────────────────────────────
  reg_4_reg <- reg_4 %>%
    filter(Region %in% regions_r10) %>%
    mutate(
      Region_label = factor(REGION_LABELS_FIG[Region],
                            levels = rev(unname(REGION_LABELS_FIG[regions_r10])))
    ) %>%
    filter(!is.na(Region_label))
  
  # Patchwork approach: one panel per outcome so x-axes are fully independent
  make_coef_panel <- function(dat, outcome, xlab, show_legend = FALSE) {
    d <- dat %>% filter(outcome_lab == outcome)
    if (nrow(d) == 0) return(NULL)
    p <- ggplot(d, aes(x = estimate, y = Region_label,
                       colour = Ambition, shape = significant)) +
      geom_vline(xintercept = 0, linetype = "dashed",
                 colour = "grey40", linewidth = 0.5) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                     height = 0.25, linewidth = 0.5, alpha = 0.7) +
      geom_point(size = 2.5) +
      facet_wrap(~ deployment, scales = "free_x", nrow = 1) +
      scale_colour_manual(values = ACOLS, name = "Ambition") +
      scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1),
                         labels = c("TRUE" = "p < 0.05", "FALSE" = "p >= 0.05"),
                         name   = "Significance") +
      labs(title = outcome, x = xlab, y = NULL) +
      theme_c(9) +
      theme(strip.text.x  = element_text(size = 8),
            plot.title     = element_text(size = 9, face = "bold"),
            legend.position = if (show_legend) "bottom" else "none")
    p
  }
  
  outcomes_order <- unname(COEF_OUTCOMES_4)
  
  panels_reg <- map(outcomes_order, ~ make_coef_panel(
    reg_4_reg, .x, "Log-log elasticity (95% CI)",
    show_legend = (.x == last(outcomes_order))
  ))
  panels_reg <- compact(panels_reg)
  
  p4f_reg <- wrap_plots(panels_reg, ncol = 1) +
    plot_annotation(
      title    = "P4f (regional): Regression Coefficients by R10 Region — Deployment -> Wellbeing Outcomes",
      subtitle = "Log-log OLS  ·  By region and ambition group  ·  High-CDR and High-RE scenarios  ·  Each outcome has independent x-axis",
      caption  = paste0(
        "Filled point = p < 0.05; open = not significant.\n",
        "Positive = more deployment associated with higher (worse) outcome. Exception: jobs — positive = better."
      ),
      theme = theme_c(9)
    )
  
  sc(p4f_reg, "P4f_coef_regional_4outcomes.png", 14, 16)
  cat("  P4f regional 4-outcome coefficient plot saved.\n")
  
  # ── OLS (raw) versions ────────────────────────────────────────────────────
  if (any(str_detect(reg_all$regression, "_OLS$"))) {
    
    reg_4_ols <- reg_all %>%
      filter(
        regression %in% c("A_All_CDR_OLS", "B_All_RE_OLS"),
        y_var %in% names(COEF_OUTCOMES_4),
        !is.na(estimate)
      ) %>%
      mutate(
        outcome_lab = factor(COEF_OUTCOMES_4[y_var],
                             levels = unname(COEF_OUTCOMES_4)),
        deployment  = factor(
          if_else(str_detect(regression, "CDR"), "Total CDR", "Renewable Capacity"),
          levels = c("Total CDR", "Renewable Capacity")
        )
      ) %>%
      filter(!is.na(outcome_lab))
    
    # Aggregated OLS
    reg_4_ols_agg <- reg_4_ols %>%
      group_by(Ambition, outcome_lab, deployment) %>%
      summarise(
        conf.low  = median(conf.low,  na.rm = TRUE),
        conf.high = median(conf.high, na.rm = TRUE),
        estimate  = median(estimate,  na.rm = TRUE),
        pct_sig   = mean(significant, na.rm = TRUE),
        .groups   = "drop"
      ) %>%
      mutate(sig_agg = pct_sig >= 0.5)
    
    p4f_ols_agg <- ggplot(reg_4_ols_agg,
                          aes(x = estimate, y = Ambition,
                              colour = Ambition, shape = sig_agg)) +
      geom_vline(xintercept = 0, linetype = "dashed",
                 colour = "grey40", linewidth = 0.6) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                     height = 0.2, linewidth = 0.6, alpha = 0.7) +
      geom_point(size = 3) +
      facet_grid(outcome_lab ~ deployment, scales = "free") +
      scale_colour_manual(values = ACOLS, name = "Ambition") +
      scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1),
                         labels = c("TRUE" = "p < 0.05 in >=50% regions",
                                    "FALSE" = "p >= 0.05"),
                         name   = "Significance") +
      labs(
        title    = "P4f OLS (aggregated): Raw Regression Coefficients — Deployment -> Wellbeing Outcomes",
        subtitle = "Raw OLS (untransformed)  ·  Median slope across R10 regions  ·  High-CDR and High-RE scenarios",
        x = "OLS slope (95% CI)", y = NULL,
        caption  = paste0(
          "Filled = p < 0.05 in majority of regions; open = not significant.\n",
          "Coefficient = change in outcome per unit increase in deployment."
        )
      ) +
      theme_c() +
      theme(strip.text.y = element_text(angle = -90, size = 9),
            strip.text.x = element_text(size = 9))
    
    sc(p4f_ols_agg, "P4f_OLS_coef_aggregated_4outcomes.png", 12, 10)
    cat("  P4f OLS aggregated saved.\n")
    
    # Regional OLS
    reg_4_ols_reg <- reg_4_ols %>%
      filter(Region %in% regions_r10) %>%
      mutate(
        Region_label = factor(REGION_LABELS_FIG[Region],
                              levels = rev(unname(REGION_LABELS_FIG[regions_r10])))
      ) %>%
      filter(!is.na(Region_label))
    
    panels_ols_reg <- map(outcomes_order, ~ make_coef_panel(
      reg_4_ols_reg, .x, "OLS slope (95% CI)",
      show_legend = (.x == last(outcomes_order))
    ))
    panels_ols_reg <- compact(panels_ols_reg)
    
    p4f_ols_reg <- wrap_plots(panels_ols_reg, ncol = 1) +
      plot_annotation(
        title    = "P4f OLS (regional): Raw Regression Coefficients by R10 Region",
        subtitle = "Raw OLS (untransformed)  ·  By region and ambition group  ·  Each outcome has independent x-axis",
        caption  = paste0(
          "Filled point = p < 0.05; open = not significant.\n",
          "Coefficient = change in outcome per unit increase in deployment."
        ),
        theme = theme_c(9)
      )
    
    sc(p4f_ols_reg, "P4f_OLS_coef_regional_4outcomes.png", 14, 16)
    cat("  P4f OLS regional saved.\n")
    
    # ── Separate 1.5C and 2C versions (both log-log and OLS) ──────────────
    make_coef_reg <- function(dat, title_sfx, subtitle_sfx, xlab, fname, w=14, h=16) {
      panels <- map(outcomes_order, ~ make_coef_panel(
        dat, .x, xlab,
        show_legend = (.x == last(outcomes_order))
      ))
      panels <- compact(panels)
      p <- wrap_plots(panels, ncol = 1) +
        plot_annotation(
          title    = title_sfx,
          subtitle = subtitle_sfx,
          caption  = paste0(
            "Filled point = p < 0.05; open = not significant.\n",
            "Positive = more deployment associated with higher (worse) outcome. Exception: jobs — positive = better."
          ),
          theme = theme_c(9)
        )
      sc(p, fname, w, h)
    }
    
    for (amb_str in c("1.5C", "2C")) {
      amb_filter  <- if_else(amb_str == "1.5C", "1.5C (High-Ambition)", "2C (Medium-Ambition)")
      file_sfx    <- if_else(amb_str == "1.5C", "15C", "2C")
      
      # Log-log separate
      make_coef_reg(
        dat          = reg_4_reg %>% filter(Ambition == amb_filter),
        title_sfx    = paste0("P4f log-log (", amb_str, "): Regression Coefficients by R10 Region"),
        subtitle_sfx = paste0("Log-log OLS  ·  ", amb_str, " scenarios only  ·  High-CDR and High-RE"),
        xlab         = "Log-log elasticity (95% CI)",
        fname        = paste0("P4f_coef_regional_4outcomes_", file_sfx, ".png")
      )
      
      # OLS separate
      make_coef_reg(
        dat          = reg_4_ols_reg %>% filter(Ambition == amb_filter),
        title_sfx    = paste0("P4f OLS (", amb_str, "): Raw Regression Coefficients by R10 Region"),
        subtitle_sfx = paste0("Raw OLS (untransformed)  ·  ", amb_str, " scenarios only  ·  High-CDR and High-RE"),
        xlab         = "OLS slope (95% CI)",
        fname        = paste0("P4f_OLS_coef_regional_4outcomes_", file_sfx, ".png")
      )
    }
    cat("  Separate 1.5C and 2C coefficient plots saved.\n")
    
  } else {
    cat("  No OLS regression types in reg_all — skipping OLS plots.\n")
  }
  
} else {
  cat("  Skipping P4f new coefficient plots — reg_all not available.\n")
}

# P5b — High-CDR vs High-RE outcome scatter by region
cat("Building P5b...\n")

p5b_outcomes <- c(
  "cumulative_deaths_mln"        = "Air Pollution Mortality\n(million deaths)",
  "cumulative_gap_EJ"            = "DLE Energy Gap (EJ)",
  "mean_headcount_millions"      = "Energy Deprivation\nHeadcount (millions)",
  "cumulative_implied_CO2_GtCO2" = "CO2 Cost of DLE Gap (GtCO2)"
)

p5b_dat <- df_master %>%
  filter(Variable == "Total CDR", Region %in% regions_r10) %>%
  add_pathway() %>%
  select(Model, Scenario, Pathway, Ambition, Region,
         any_of(names(p5b_outcomes))) %>%
  pivot_longer(any_of(names(p5b_outcomes)),
               names_to = "outcome_col", values_to = "value") %>%
  filter(!is.na(value), value > 0) %>%
  mutate(
    outcome_lab  = p5b_outcomes[outcome_col],
    outcome_lab  = factor(outcome_lab, levels = unname(p5b_outcomes)),
    Region_label = REGION_LABELS_FIG[Region]
  ) %>%
  group_by(Pathway, Ambition, Region_label, outcome_lab) %>%
  summarise(med = median(value, na.rm=TRUE), .groups="drop") %>%
  pivot_wider(names_from = Pathway, values_from = med) %>%
  filter(!is.na(`High-CDR`), !is.na(`High-RE`))

p5b <- ggplot(p5b_dat,
              aes(x = `High-RE`, y = `High-CDR`,
                  colour = Region_label, shape = Ambition)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "grey50", linewidth = 0.7) +
  geom_point(size = 3.5, alpha = 0.85) +
  facet_wrap(~ outcome_lab, scales = "free", ncol = 2) +
  scale_colour_brewer(palette = "Set1", name = "Region") +
  scale_shape_manual(values = c("1.5C (High-Ambition)" = 16,
                                "2C (Medium-Ambition)"  = 17),
                     name = "Ambition") +
  labs(
    title    = "P5b: High-CDR vs High-RE Outcomes by Region",
    subtitle = "Each point = one region × ambition group  ·  Points above dashed line = CDR pathways worse",
    x = "High-RE median outcome", y = "High-CDR median outcome",
    caption  = "Dashed = equal outcomes. Above line = High-CDR worse; below = High-CDR better."
  ) +
  theme_c()

sc(p5b, "P5b_CDR_vs_RE_outcome_scatter.png", 13, 11)

# P5b_reg — faceted by region (one panel per region, all outcomes shown)
p5b_reg_dat <- p5b_dat %>% filter(!is.na(Region_label))

p5b_reg <- ggplot(p5b_reg_dat,
                  aes(x=`High-RE`, y=`High-CDR`, colour=Ambition)) +
  geom_abline(slope=1, intercept=0, linetype="dashed",
              colour="grey50", linewidth=0.6) +
  geom_point(size=2.5, alpha=0.85) +
  facet_grid(outcome_lab ~ Region_label, scales="free") +
  scale_colour_manual(values=ACOLS, name="Ambition") +
  labs(title="P5b (regional): High-CDR vs High-RE Outcomes by Region",
       subtitle="Points above dashed line = CDR pathways worse",
       x="High-RE median", y="High-CDR median") +
  theme_c(8) + theme(strip.text.y=element_text(angle=-90,size=7))
sc(p5b_reg, "P5b_CDR_vs_RE_scatter_by_region.png", 18, 14)

# =============================================================================
# P5c — Leave-one-model-out robustness
# =============================================================================
cat("Building P5c...\n")

loo_outcomes <- c(
  "cumulative_deaths_mln"   = "Mortality",
  "cumulative_gap_EJ"       = "DLE Gap",
  "jobs_Renewables"         = "RE Jobs",
  "jobs_Fossil"             = "Fossil Jobs"
)

model_families <- df_master %>%
  mutate(family = simplify_model(Model)) %>%
  filter(family != "Other") %>%
  distinct(family) %>%
  pull(family)

compute_diff <- function(data, dropped = "None (full sample)") {
  data %>%
    filter(Variable == "Total CDR", Region %in% regions_r10) %>%
    add_pathway() %>%
    select(Model, Scenario, Pathway, Ambition,
           any_of(names(loo_outcomes))) %>%
    pivot_longer(any_of(names(loo_outcomes)),
                 names_to = "outcome_col", values_to = "value") %>%
    filter(!is.na(value)) %>%
    group_by(Pathway, Ambition, outcome_col) %>%
    summarise(med = median(value, na.rm=TRUE), .groups="drop") %>%
    pivot_wider(names_from = Pathway, values_from = med) %>%
    mutate(diff = `High-CDR` - `High-RE`,
           dropped_family = dropped)
}

loo_all <- bind_rows(
  compute_diff(df_master),
  map_dfr(model_families, function(fam) {
    compute_diff(
      df_master %>% filter(simplify_model(Model) != fam),
      fam
    )
  })
) %>%
  mutate(
    outcome_lab    = loo_outcomes[outcome_col],
    outcome_lab    = factor(outcome_lab, levels = unname(loo_outcomes)),
    dropped_family = factor(dropped_family,
                            levels = c("None (full sample)", model_families)),
    is_full        = dropped_family == "None (full sample)"
  )

p5c <- ggplot(loo_all %>% filter(!is.na(diff)),
              aes(x = diff, y = dropped_family,
                  colour = is_full, size = is_full)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "grey40", linewidth = 0.7) +
  geom_point(alpha = 0.85) +
  facet_grid(outcome_lab ~ Ambition, scales = "free_x") +
  scale_colour_manual(values = c("TRUE" = "#d7191c", "FALSE" = "#525252"),
                      guide = "none") +
  scale_size_manual(values = c("TRUE" = 3.5, "FALSE" = 2), guide = "none") +
  labs(
    title    = "P5c: Leave-One-Model-Out Robustness",
    subtitle = "Red = full sample; grey = model family dropped  ·  Robust if grey points cluster near red",
    x = "Difference (High-CDR − High-RE median)", y = "Model family dropped",
    caption  = "If a grey point is far from red, that model family drives the result."
  ) +
  theme_c() +
  theme(strip.text.y = element_text(angle = -90))

sc(p5c, "P5c_LOO_robustness.png", 13, 11)

# P5c_reg — LOO robustness by region
loo_outcomes_reg <- c(
  "cumulative_deaths_mln" = "Mortality",
  "cumulative_gap_EJ"     = "DLE Gap",
  "jobs_Renewables"       = "RE Jobs"
)

compute_diff_reg <- function(data, dropped="None") {
  data %>%
    filter(Variable=="Total CDR", Region %in% regions_r10) %>%
    add_pathway() %>%
    select(Model, Scenario, Pathway, Ambition, Region,
           any_of(names(loo_outcomes_reg))) %>%
    pivot_longer(any_of(names(loo_outcomes_reg)),
                 names_to="outcome_col", values_to="value") %>%
    filter(!is.na(value)) %>%
    group_by(Pathway, Ambition, Region, outcome_col) %>%
    summarise(med=median(value,na.rm=T), .groups="drop") %>%
    pivot_wider(names_from=Pathway, values_from=med) %>%
    mutate(diff=`High-CDR`-`High-RE`, dropped_family=dropped)
}

loo_reg_all <- bind_rows(
  compute_diff_reg(df_master),
  map_dfr(model_families, ~ compute_diff_reg(
    df_master %>% filter(simplify_model(Model) != .x), .x))
) %>%
  mutate(
    outcome_lab    = loo_outcomes_reg[outcome_col],
    outcome_lab    = factor(outcome_lab, levels=unname(loo_outcomes_reg)),
    Region_label   = REGION_LABELS_FIG[Region],
    dropped_family = factor(dropped_family,
                            levels=c("None",model_families)),
    is_full        = dropped_family == "None"
  )

p5c_reg <- ggplot(loo_reg_all %>% filter(!is.na(diff)),
                  aes(x=diff, y=dropped_family,
                      colour=is_full, size=is_full)) +
  geom_vline(xintercept=0, linetype="dashed",
             colour="grey40", linewidth=0.6) +
  geom_point(alpha=0.85) +
  facet_grid(outcome_lab ~ Region_label, scales="free_x") +
  scale_colour_manual(values=c("TRUE"="#d7191c","FALSE"="#525252"),
                      guide="none") +
  scale_size_manual(values=c("TRUE"=3,"FALSE"=1.5), guide="none") +
  labs(title="P5c (regional): LOO Robustness by Region",
       subtitle="Red=full sample; grey=model family dropped",
       x="Difference (High-CDR − High-RE)", y="Model dropped") +
  theme_c(8) + theme(strip.text.y=element_text(angle=-90,size=7))
sc(p5c_reg, "P5c_LOO_robustness_by_region.png", 20, 12)

# =============================================================================
# P5d — CDR and RE deployment distributions by pathway type
# =============================================================================
cat("Building P5d...\n")

# P5d uses mutually exclusive High-CDR only and High-RE only
# Use regional classification (same as rest of analysis)
p5d_dat <- df_master %>%
  filter(Variable %in% c("Total CDR","Renewable Capacity"),
         Region %in% regions_r10) %>%
  select(Model, Scenario, Category, Ambition, Region, Variable, Total_Value) %>%
  pivot_wider(names_from = Variable, values_from = Total_Value) %>%
  rename(region_cdr = `Total CDR`, region_re = `Renewable Capacity`) %>%
  group_by(Region, Ambition) %>%
  mutate(
    high_cdr_only = ntile(region_cdr, 3) == 3 & ntile(region_re, 3) != 3,
    high_re_only  = ntile(region_re,  3) == 3 & ntile(region_cdr, 3) != 3
  ) %>%
  ungroup() %>%
  filter(high_cdr_only | high_re_only) %>%
  mutate(
    Pathway      = if_else(high_cdr_only, "High-CDR only", "High-RE only"),
    Pathway      = factor(Pathway, levels = c("High-CDR only","High-RE only")),
    CDR_TtCO2    = region_cdr / 1e3,
    RE_thou_GWyr = region_re  / 1e3
  )

p5d_cols <- c("High-CDR only" = "#2166ac", "High-RE only" = "#d6604d")

p5d_cdr <- ggplot(p5d_dat,
                  aes(x = CDR_TtCO2, fill = Pathway,
                      colour = Pathway)) +
  geom_density(alpha = 0.40, linewidth = 0.7) +
  facet_grid(Region ~ Ambition, scales = "free") +
  scale_fill_manual(values   = p5d_cols, name = "Pathway") +
  scale_colour_manual(values = p5d_cols, name = "Pathway") +
  labs(title = "Total CDR by Region",
       x = "Regional cumulative CDR 2020-2100 (TtCO2)", y = "Density") +
  theme_c(9) +
  theme(strip.text.y = element_text(angle = -90, size = 7))

p5d_re <- ggplot(p5d_dat,
                 aes(x = RE_thou_GWyr, fill = Pathway,
                     colour = Pathway)) +
  geom_density(alpha = 0.40, linewidth = 0.7) +
  facet_grid(Region ~ Ambition, scales = "free") +
  scale_fill_manual(values   = p5d_cols, name = "Pathway", guide = "none") +
  scale_colour_manual(values = p5d_cols, name = "Pathway", guide = "none") +
  labs(title = "Renewable Capacity by Region",
       x = "Regional cumulative RE 2020-2100 (thousands GW·yr)", y = "Density") +
  theme_c(9) +
  theme(strip.text.y = element_text(angle = -90, size = 7))

p5d <- p5d_cdr + p5d_re +
  plot_annotation(
    title    = "P5d: CDR and RE Deployment Distributions — High-CDR vs High-RE",
    subtitle = paste0(
      "Mutually exclusive pathways only  ·  World-level tercile classification\n",
      "High-CDR only = top tercile CDR, not RE in that region  ·  ",
      "High-RE only = top tercile RE, not CDR in that region"
    ),
    caption  = "Density per region × ambition group.  Blue = High-CDR; red = High-RE.",
    theme = theme_c()
  )

sc(p5d, "P5d_deployment_distributions.png", 14, 10)

cat("P5b, P5c, P5d complete.\n")

# =============================================================================
# SECTION 6: PATHWAY COMPARISON FIGURES
#
# P6a  — Within-ambition: High-CDR vs High-RE violins, faceted by region
# P6b  — Within-ambition: same, aggregated across all R10 regions
# P6c  — Cross-ambition:  all four groups (4 violins), faceted by region
# P6d  — Cross-ambition:  all four groups, aggregated across R10
# P6e  — Cross-ambition:  ambition-level average (collapsing pathway), by region
# P6f  — Cross-ambition:  ambition-level average, aggregated across R10
# P6g  — Coefficient figure: within-ambition, faceted by region
# P6h  — Coefficient figure: within-ambition, aggregated across R10
# P6i  — Coefficient figure: cross-ambition, faceted by region
# P6j  — Coefficient figure: cross-ambition, aggregated across R10
# =============================================================================
cat("\n=== SECTION 6: Pathway comparison figures ===\n")

# ---- Shared outcome specs for Section 6 ------------------------------------
s6_outcomes <- c(
  "cum_deaths_nz"              = "Air Pollution\nMortality\n(2020–NZ)",
  "cumulative_gap_EJ"          = "DLE Energy Gap",
  "mean_headcount_millions"    = "Energy Deprivation\nHeadcount",
  "cumulative_implied_CO2_GtCO2" = "CO2 Cost\nDLE Gap",
  "jobs_Renewables"            = "Renewable\nEnergy Jobs",
  "jobs_Fossil"                = "Fossil\nEnergy Jobs"
)

# s6_outcomes_reg: same outcomes but keyed to df_master column names (for P6g-P6j)
s6_outcomes_reg <- c(
  "cumulative_deaths_mln"        = "Air Pollution\nMortality\n(2020–NZ)",
  "cumulative_gap_EJ"            = "DLE Energy Gap",
  "mean_headcount_millions"      = "Energy Deprivation\nHeadcount",
  "cumulative_implied_CO2_GtCO2" = "CO2 Cost\nDLE Gap",
  "jobs_Renewables"              = "Renewable\nEnergy Jobs",
  "jobs_Fossil"                  = "Fossil\nEnergy Jobs"
)

# Regional pathway classification — already built earlier (before P3a_reg)
# Kept here as reference; the if(!exists) guard prevents redefinition
if (!exists("s6_reg_class")) {
  s6_reg_class <- df_master %>%
    filter(Variable %in% c("Total CDR","Renewable Capacity"),
           Region %in% regions_r10) %>%
    select(Model, Scenario, Category, Region, Ambition, Variable, Total_Value) %>%
    pivot_wider(names_from = Variable, values_from = Total_Value) %>%
    rename(region_cdr = `Total CDR`, region_re = `Renewable Capacity`) %>%
    group_by(Region, Ambition) %>%
    mutate(
      high_cdr      = ntile(region_cdr, 3) == 3,
      high_re       = ntile(region_re,  3) == 3,
      high_cdr_only = high_cdr & !high_re,
      high_re_only  = high_re  & !high_cdr,
      high_any      = high_cdr | high_re,
      Pathway_reg   = case_when(
        high_cdr_only ~ "High-CDR",
        high_re_only  ~ "High-RE",
        TRUE          ~ NA_character_
      )
    ) %>%
    ungroup()
}

# Outcomes joined with WORLD-LEVEL pathway labels (from pathway_tercile / world_cum_f2)
# This ensures P6c, P6e, P6g-P6j all use world-level tercile classification
s6_outcomes_base <- df_master %>%
  filter(Variable == "Total CDR", Region %in% regions_r10) %>%
  select(Model, Scenario, Category, Region, Ambition,
         any_of(c(names(s6_outcomes), "cumulative_deaths_mln"))) %>%
  left_join(
    world_cum_f2 %>% select(Model, Scenario,
                            high_cdr_only, high_re_only) %>%
      mutate(high_any = coalesce(high_cdr_only, FALSE) |
               coalesce(high_re_only,  FALSE),
             Pathway_reg = case_when(
               high_cdr_only ~ "High-CDR",
               high_re_only  ~ "High-RE",
               TRUE          ~ NA_character_
             )),
    by = c("Model","Scenario")
  ) %>%
  # Join cum_deaths_nz (2020-2050 mortality) from mort_to_nz
  left_join(
    mort_to_nz %>% select(Model, Scenario, Region, cum_deaths_nz),
    by = c("Model","Scenario","Region")
  ) %>%
  mutate(
    Region_label = factor(REGION_LABELS_FIG[Region],
                          levels = unname(REGION_LABELS_FIG[regions_r10]))
  )

# Mutually exclusive: High-CDR only OR High-RE only
s6_excl <- s6_outcomes_base %>%
  filter(!is.na(Pathway_reg)) %>%
  mutate(Group = paste0(Pathway_reg, "\n", Ambition),
         Group = factor(Group, levels = c(
           "High-CDR\n1.5C (High-Ambition)", "High-RE\n1.5C (High-Ambition)",
           "High-CDR\n2C (Medium-Ambition)", "High-RE\n2C (Medium-Ambition)")))

# Overlapping: top tercile CDR or RE (including both)
s6_overlap <- s6_outcomes_base %>%
  filter(high_any) %>%
  mutate(
    Pathway_reg = if_else(high_cdr_only, "High-CDR",
                          if_else(high_re_only,  "High-RE", "Both High")),
    Group = paste0(Pathway_reg, "\n", Ambition),
    Group = factor(Group, levels = c(
      "High-CDR\n1.5C (High-Ambition)", "High-RE\n1.5C (High-Ambition)",
      "Both High\n1.5C (High-Ambition)",
      "High-CDR\n2C (Medium-Ambition)", "High-RE\n2C (Medium-Ambition)",
      "Both High\n2C (Medium-Ambition)"))
  )

# Pivot to long for plotting
make_s6_long <- function(df) {
  df %>%
    pivot_longer(any_of(names(s6_outcomes)),
                 names_to = "outcome_col", values_to = "val") %>%
    filter(!is.na(val), val > 0) %>%
    mutate(outcome_lab = factor(s6_outcomes[outcome_col],
                                levels = unname(s6_outcomes)))
}

s6_long_excl    <- make_s6_long(s6_excl)
s6_long_overlap <- make_s6_long(s6_overlap)

# Aggregated versions — use outcomes_nz_world with regional pathway labels
# (outcomes_nz_world is pop-weighted mean; pathway from global classification
# since it's already at All Regions level — keep separate from regional figures)
s6_long_agg_excl <- outcomes_nz_world %>%
  filter(!is.na(Pathway)) %>%
  select(Model, Scenario, Pathway, Ambition, Region_label, Group,
         any_of(names(s6_outcomes))) %>%
  pivot_longer(any_of(names(s6_outcomes)),
               names_to = "outcome_col", values_to = "val") %>%
  filter(!is.na(val), val > 0) %>%
  mutate(outcome_lab = factor(s6_outcomes[outcome_col],
                              levels = unname(s6_outcomes)))

# Colours for 4-group and 6-group plots
GROUP_COLS_6 <- c(
  "High-CDR\n1.5C (High-Ambition)" = "#2166ac",
  "High-RE\n1.5C (High-Ambition)"  = "#d6604d",
  "Both High\n1.5C (High-Ambition)"= "#762a83",
  "High-CDR\n2C (Medium-Ambition)" = "#74add1",
  "High-RE\n2C (Medium-Ambition)"  = "#f46d43",
  "Both High\n2C (Medium-Ambition)"= "#c2a5cf"
)

# Shared violin + boxplot layer
violin_box <- function(mapping, data) {
  list(
    geom_violin(mapping, data = data,
                alpha = 0.40, colour = NA, scale = "width", trim = TRUE),
    geom_boxplot(mapping, data = data,
                 width = 0.12, outlier.size = 0.3,
                 colour = "grey30", fill = "white", alpha = 0.8)
  )
}

# ── P6a: Within-ambition, by region — two versions (excl + overlap) ─────────
cat("  P6a...\n")

make_within_violin <- function(data, ambition_str, title_sfx, subtitle_sfx) {
  ggplot(data %>% filter(str_detect(Ambition, ambition_str),
                         !is.na(Region_label)),
         aes(x = Pathway_reg, y = val,
             fill = Pathway_reg, colour = Pathway_reg)) +
    geom_violin(alpha = 0.40, colour = NA, scale = "width", trim = TRUE) +
    geom_boxplot(width = 0.12, outlier.size = 0.3,
                 colour = "grey30", fill = "white", alpha = 0.8) +
    facet_grid(outcome_lab ~ Region_label, scales = "free_y") +
    scale_fill_manual(values   = PCOLS, name = "Pathway") +
    scale_colour_manual(values = PCOLS, name = "Pathway") +
    scale_y_continuous(labels = comma_format()) +
    labs(title    = paste0("P6a: High-CDR vs High-RE — ", title_sfx),
         subtitle = subtitle_sfx,
         x = NULL, y = NULL,
         caption  = "Filled = distribution; box = IQR; line = median.  World-level tercile classification.") +
    theme_c() +
    theme(axis.text.x  = element_text(angle = 20, hjust = 1),
          strip.text.y = element_text(angle = -90, size = 7))
}

# Mutually exclusive versions
sc(make_within_violin(s6_long_excl, "1.5C",
                      "1.5C Scenarios by Region (Mutually Exclusive)",
                      "Mutually exclusive world-level terciles  ·  2020–net-zero window (1.5C=2060, 2C=2075)"),
   "P6a_excl_15C_by_region.png", 18, 14)

sc(make_within_violin(s6_long_excl, "2C",
                      "2C Scenarios by Region (Mutually Exclusive)",
                      "Mutually exclusive world-level terciles  ·  2020–net-zero window (1.5C=2060, 2C=2075)"),
   "P6a_excl_2C_by_region.png", 18, 14)

# Overlapping versions (High-CDR only shown; Both High excluded for cleaner viz)
sc(make_within_violin(
  s6_long_overlap %>% filter(Pathway_reg %in% c("High-CDR","High-RE")),
  "1.5C",
  "1.5C Scenarios by Region (Overlapping)",
  "Overlapping world-level terciles (Both High included)  ·  2020–net-zero window (1.5C=2060, 2C=2075)"),
  "P6a_overlap_15C_by_region.png", 18, 14)

sc(make_within_violin(
  s6_long_overlap %>% filter(Pathway_reg %in% c("High-CDR","High-RE")),
  "2C",
  "2C Scenarios by Region (Overlapping)",
  "Overlapping world-level terciles (Both High included)  ·  2020–net-zero window (1.5C=2060, 2C=2075)"),
  "P6a_overlap_2C_by_region.png", 18, 14)

# ── P6b: Within-ambition, aggregated ────────────────────────────────────────
cat("  P6b...\n")

p6b_dat <- s6_long_agg_excl

p6b <- ggplot(p6b_dat,
              aes(x = Pathway, y = val,
                  fill = Pathway, colour = Pathway)) +
  geom_violin(alpha = 0.40, colour = NA, scale = "width", trim = TRUE) +
  geom_boxplot(width = 0.15, outlier.size = 0.4,
               colour = "grey30", fill = "white", alpha = 0.8) +
  facet_grid(outcome_lab ~ Ambition, scales = "free_y") +
  scale_fill_manual(values   = PCOLS) +
  scale_colour_manual(values = PCOLS) +
  scale_y_continuous(labels = comma_format()) +
  labs(title    = "P6b: High-CDR vs High-RE — All Regions Aggregated",
       subtitle = "Population-weighted mean across R10  ·  2020–net-zero window (1.5C=2060, 2C=2075)",
       x = NULL, y = NULL,
       caption  = "Filled = pathway distribution; box = IQR; line = median.") +
  theme_c() +
  theme(axis.text.x  = element_text(angle = 20, hjust = 1),
        strip.text.y = element_text(angle = -90, size = 8))

sc(p6b, "P6b_within_ambition_aggregated.png", 11, 14)

# ── P6c: Cross-ambition, all four groups, by region ─────────────────────────
cat("  P6c...\n")

# P6c uses mutually exclusive regional classification (4 groups)
p6c_dat <- s6_long_excl

p6c <- ggplot(p6c_dat,
              aes(x = Group, y = val,
                  fill = Group, colour = Group)) +
  geom_violin(alpha = 0.40, colour = NA, scale = "width", trim = TRUE) +
  geom_boxplot(width = 0.12, outlier.size = 0.3,
               colour = "grey30", fill = "white", alpha = 0.8) +
  facet_grid(outcome_lab ~ Region_label, scales = "free_y") +
  scale_fill_manual(values   = GROUP_COLS_4) +
  scale_colour_manual(values = GROUP_COLS_4) +
  scale_y_continuous(labels  = comma_format()) +
  labs(title    = "P6c: All Four Pathway × Ambition Groups — By Region",
       subtitle = "High-CDR vs High-RE within and across ambition groups  ·  2020–net-zero window (1.5C=2060, 2C=2075)",
       x = NULL, y = NULL,
       caption  = "Blue tones = High-CDR; red tones = High-RE  ·  Dark = 1.5C; light = 2C") +
  theme_c() +
  theme(axis.text.x  = element_blank(), axis.ticks.x = element_blank(),
        strip.text.y = element_text(angle = -90, size = 7))

sc(p6c, "P6c_cross_ambition_4groups_by_region.png", 20, 16)

# ── P6d: Cross-ambition, all four groups, aggregated ────────────────────────
cat("  P6d...\n")

p6d_dat <- s6_long_agg_excl

p6d <- ggplot(p6d_dat,
              aes(x = Group, y = val,
                  fill = Group, colour = Group)) +
  geom_violin(alpha = 0.40, colour = NA, scale = "width", trim = TRUE) +
  geom_boxplot(width = 0.15, outlier.size = 0.4,
               colour = "grey30", fill = "white", alpha = 0.8) +
  facet_wrap(~ outcome_lab, scales = "free_y", ncol = 3) +
  scale_fill_manual(values   = GROUP_COLS_4) +
  scale_colour_manual(values = GROUP_COLS_4) +
  scale_y_continuous(labels  = comma_format()) +
  labs(title    = "P6d: All Four Pathway × Ambition Groups — Aggregated",
       subtitle = "Population-weighted mean across R10  ·  2020–net-zero window (1.5C=2060, 2C=2075)",
       x = NULL, y = NULL,
       caption  = "Blue tones = High-CDR; red tones = High-RE  ·  Dark = 1.5C; light = 2C") +
  theme_c() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

sc(p6d, "P6d_cross_ambition_4groups_aggregated.png", 14, 10)

# ── P6e: Cross-ambition, ambition-level average (collapsing pathway), by region
cat("  P6e...\n")

p6e_dat <- s6_long_excl %>%
  mutate(Ambition_short = if_else(str_detect(Ambition, "1.5C"),
                                  "1.5C", "2C"))

p6e <- ggplot(p6e_dat,
              aes(x = Ambition_short, y = val,
                  fill = Ambition, colour = Ambition)) +
  geom_violin(alpha = 0.40, colour = NA, scale = "width", trim = TRUE) +
  geom_boxplot(width = 0.15, outlier.size = 0.3,
               colour = "grey30", fill = "white", alpha = 0.8) +
  facet_grid(outcome_lab ~ Region_label, scales = "free_y") +
  scale_fill_manual(values   = ACOLS) +
  scale_colour_manual(values = ACOLS) +
  scale_y_continuous(labels  = comma_format()) +
  labs(title    = "P6e: 1.5C vs 2C Ambition — By Region (Pooled Pathways)",
       subtitle = "High-CDR + High-RE pooled within each ambition group  ·  2020–net-zero window (1.5C=2060, 2C=2075)",
       x = NULL, y = NULL,
       caption  = "Pools High-CDR and High-RE scenarios within each ambition group.") +
  theme_c() +
  theme(strip.text.y = element_text(angle = -90, size = 7))

sc(p6e, "P6e_cross_ambition_pooled_by_region.png", 18, 14)

# ── P6f: Cross-ambition, ambition-level average, aggregated ─────────────────
cat("  P6f...\n")

p6f_dat <- s6_long_agg_excl %>%
  mutate(Ambition_short = if_else(str_detect(Ambition, "1.5C"),
                                  "1.5C", "2C"))

p6f <- ggplot(p6f_dat,
              aes(x = Ambition_short, y = val,
                  fill = Ambition, colour = Ambition)) +
  geom_violin(alpha = 0.40, colour = NA, scale = "width", trim = TRUE) +
  geom_boxplot(width = 0.15, outlier.size = 0.4,
               colour = "grey30", fill = "white", alpha = 0.8) +
  facet_wrap(~ outcome_lab, scales = "free_y", ncol = 3) +
  scale_fill_manual(values   = ACOLS) +
  scale_colour_manual(values = ACOLS) +
  scale_y_continuous(labels  = comma_format()) +
  labs(title    = "P6f: 1.5C vs 2C Ambition — Aggregated (Pooled Pathways)",
       subtitle = "Population-weighted mean across R10  ·  High-CDR + High-RE pooled  ·  2020–net-zero window (1.5C=2060, 2C=2075)",
       x = NULL, y = NULL) +
  theme_c()

sc(p6f, "P6f_cross_ambition_pooled_aggregated.png", 13, 10)

# =============================================================================
# REGRESSION COEFFICIENT FIGURES (P6g–P6j)
# =============================================================================

if (!is.null(reg_all) && nrow(reg_all) > 0) {
  
  # Regional regression results — use E (overlap) and F (excl)
  # These use world-level tercile classification, x = region deployment
  prep_reg <- function(reg_types, dep_labels) {
    reg_all %>%
      filter(regression %in% reg_types,
             Region %in% regions_r10,
             !is.na(estimate)) %>%
      mutate(
        outcome_lab  = factor(s6_outcomes_reg[y_var],
                              levels = unname(s6_outcomes_reg)),
        Region_label = factor(REGION_LABELS_FIG[Region],
                              levels = rev(unname(REGION_LABELS_FIG[regions_r10]))),
        deployment   = dep_labels[regression],
        deployment   = factor(deployment,
                              levels = c("Total CDR", "Renewable Capacity"))
      ) %>%
      filter(!is.na(outcome_lab), !is.na(Region_label), !is.na(deployment))
  }
  
  excl_labels    <- c("F_Reg_Excl_CDR"    = "Total CDR",
                      "F_Reg_Excl_RE"     = "Renewable Capacity")
  overlap_labels <- c("E_Reg_Overlap_CDR" = "Total CDR",
                      "E_Reg_Overlap_RE"  = "Renewable Capacity")
  
  reg_base_excl    <- prep_reg(names(excl_labels),    excl_labels)
  reg_base_overlap <- prep_reg(names(overlap_labels), overlap_labels)
  
  # Default reg_base = exclusive for coefficient plots
  reg_base <- reg_base_excl
  
  # Aggregated: median across regions
  agg_reg <- function(df) {
    df %>%
      group_by(Ambition, outcome_lab, deployment) %>%
      summarise(
        estimate  = median(estimate,  na.rm = TRUE),
        conf.low  = median(conf.low,  na.rm = TRUE),
        conf.high = median(conf.high, na.rm = TRUE),
        n_regions = n(),
        pct_sig   = mean(significant, na.rm = TRUE),
        .groups   = "drop"
      ) %>%
      mutate(significant = pct_sig >= 0.5)
  }
  
  reg_agg         <- agg_reg(reg_base_excl)
  reg_agg_overlap <- agg_reg(reg_base_overlap)
  
  coef_theme <- theme_c() +
    theme(strip.text.y = element_text(angle = -90, size = 7),
          strip.text.x = element_text(size = 9))
  
  # ── P6g: Within-ambition coefficients, by region ────────────────────────
  cat("  P6g...\n")
  
  make_coef_plot <- function(data, title, subtitle) {
    ggplot(data, aes(x = estimate, y = Region_label,
                     colour = Ambition, shape = significant)) +
      geom_vline(xintercept = 0, linetype = "dashed",
                 colour = "grey40", linewidth = 0.6) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                     height = 0.25, linewidth = 0.5, alpha = 0.7) +
      geom_point(size = 2.5) +
      scale_colour_manual(values = ACOLS,  name = "Ambition") +
      scale_shape_manual(values  = c("TRUE" = 16, "FALSE" = 1),
                         labels  = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
                         name    = "Significance") +
      scale_y_discrete(drop = FALSE) +
      labs(title = title, subtitle = subtitle,
           x = "Log-log elasticity (95% CI)", y = NULL,
           caption = paste0(
             "Positive = more deployment → higher (worse) outcome.\n",
             "Exception: jobs — positive = more jobs (better)."
           )) +
      coef_theme
  }
  
  # P6g: exclusive and overlap versions for each ambition group
  for (vers in list(list(data=reg_base_excl,    sfx="excl",    note="Mutually exclusive"),
                    list(data=reg_base_overlap, sfx="overlap", note="Overlapping"))) {
    for (amb in list(list(str="1.5C", wnd="2020-2060"),
                     list(str="2C",   wnd="2020-2075"))) {
      p <- vers$data %>%
        filter(str_detect(Ambition, amb$str)) %>%
        make_coef_plot(
          paste0("P6g: Coefficients — ", amb$str, " by Region (", vers$note, ")"),
          paste0(vers$note, " world-level terciles  ·  ", amb$wnd, "  ·  Filled = p < 0.05")
        ) +
        facet_grid(outcome_lab ~ deployment, scales = "free")
      sc(p, paste0("P6g_coef_", amb$str, "_", vers$sfx, "_by_region.png"), 12, 14)
    }
  }
  
  # ── P6h: Within-ambition coefficients, aggregated ───────────────────────
  cat("  P6h...\n")
  
  p6h <- ggplot(reg_agg,
                aes(x = estimate, y = Ambition,
                    colour = Ambition, shape = significant)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.6) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.2, linewidth = 0.6, alpha = 0.7) +
    geom_point(size = 3) +
    facet_grid(outcome_lab ~ deployment, scales = "free") +
    scale_colour_manual(values = ACOLS,  name = "Ambition") +
    scale_shape_manual(values  = c("TRUE" = 16, "FALSE" = 1),
                       labels  = c("TRUE" = "p < 0.05 in ≥50% regions",
                                   "FALSE" = "p ≥ 0.05"),
                       name    = "Significance") +
    labs(
      title    = "P6h: Regression Coefficients — Aggregated Across R10 Regions",
      subtitle = "Median elasticity across regions  ·  Filled = majority of regions significant",
      x = "Median log-log elasticity (95% CI)", y = NULL,
      caption  = "Positive = more deployment → worse outcome (except jobs: positive = better)."
    ) +
    coef_theme
  
  sc(p6h, "P6h_coef_within_ambition_aggregated.png", 12, 12)
  
  # ── P6i: Cross-ambition coefficients, by region ──────────────────────────
  cat("  P6i...\n")
  
  p6i <- reg_base %>%
    make_coef_plot(
      "P6i: Regression Coefficients — 1.5C vs 2C, by Region",
      "Both ambition groups  ·  2020–net-zero window (1.5C=2060, 2C=2075)  ·  Filled = p < 0.05"
    ) +
    facet_grid(outcome_lab ~ deployment, scales = "free")
  
  sc(p6i, "P6i_coef_cross_ambition_by_region.png", 12, 14)
  
  # ── P6j: Cross-ambition coefficients, aggregated ─────────────────────────
  cat("  P6j...\n")
  
  p6j <- ggplot(reg_agg,
                aes(x = estimate, y = Ambition,
                    colour = Ambition, shape = significant)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.6) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.2, linewidth = 0.6, alpha = 0.7) +
    geom_point(size = 3) +
    facet_grid(outcome_lab ~ deployment, scales = "free") +
    scale_colour_manual(values = ACOLS,  name = "Ambition") +
    scale_shape_manual(values  = c("TRUE" = 16, "FALSE" = 1),
                       labels  = c("TRUE" = "p < 0.05 in ≥50% regions",
                                   "FALSE" = "p ≥ 0.05"),
                       name    = "Significance") +
    labs(
      title    = "P6j: Regression Coefficients — 1.5C vs 2C Aggregated",
      subtitle = "Median elasticity across R10  ·  Both ambition groups compared",
      x = "Median log-log elasticity (95% CI)", y = NULL
    ) +
    coef_theme
  
  sc(p6j, "P6j_coef_cross_ambition_aggregated.png", 12, 12)
  
  cat("  P6g-P6j complete.\n")
  
} else {
  cat("  Skipping P6g-P6j — reg_all not available.\n")
}

cat("\n=== SECTION 6 COMPLETE ===\n")
cat("  P6a  — Within-ambition violins by region (1.5C + 2C separate)\n")
cat("  P6b  — Within-ambition violins aggregated\n")
cat("  P6c  — Cross-ambition 4-group violins by region\n")
cat("  P6d  — Cross-ambition 4-group violins aggregated\n")
cat("  P6e  — Cross-ambition pooled-pathway violins by region\n")
cat("  P6f  — Cross-ambition pooled-pathway violins aggregated\n")
cat("  P6g  — Coefficients within-ambition by region (1.5C + 2C separate)\n")
cat("  P6h  — Coefficients within-ambition aggregated\n")
cat("  P6i  — Coefficients cross-ambition by region\n")
cat("  P6j  — Coefficients cross-ambition aggregated\n")

# =============================================================================
# SUMMARY OF ALL FIGURES PRODUCED
# =============================================================================
cat("\n=== COMMITTEE FIGURE SCRIPT COMPLETE ===\n")
cat(sprintf("Net-zero cutoffs used (snapped):  1.5C → %d  |  2C → %d\n", NZ_15_snap, NZ_2C_snap))
cat("\nSection 1 — What is in COMPASS?\n")
cat("  P1a — Scenario coverage by model family × category\n")
cat("  P1b — CDR and RE deployment by model family\n")
cat("  P1c — CDR component coverage by model family\n")
cat("\nSection 2 — Variable trends (all scenarios)\n")
cat("  P2a — Annual CDR and RE trends\n")
cat("  P2b — Air pollutant emission trends (NOx, SO2)\n")
cat("  P2c — Final energy by region\n")
cat("  P2d — Capacity additions by fuel type\n")
cat("\nSection 3 — Trends by pathway type\n")
cat("  P3a — CDR component breakdown: Fossil CCS dominates\n")
cat("  P3b — Electricity mix by pathway\n")
cat("  P3c — NOx trajectories by pathway × region\n")
cat("\nSection 4 — Wellbeing outcomes (to net-zero cutoff)\n")
cat("  P4a — Outcome violins (world average)\n")
cat("  P4b — Mortality trajectory world\n")
cat("  P4c — Mortality delta heatmap\n")
cat("  P4d — DLE + jobs difference strips\n")
cat("  P4e — Spearman correlation heatmap\n")
cat("  P4f — Regression coefficients\n")
cat("\nSection 5 — Equity + robustness\n")
cat("  P5a — Gini equity coefficients (all outcomes)\n")
cat("  P5b — Baseline vs net-zero scatter\n")
cat("  P5c — Leave-one-model-out robustness\n")
cat("  P5d — CDR effectiveness vs peak warming\n")

# =============================================================================
# NEW PER-CAPITA OUTCOME FIGURES — sourced from separate script
# Run after all objects above are built (df_master, pop_ts, etc.)
# =============================================================================
# Uncomment to run new figures:
# source(file.path(dirname(FIG_OUT), "../P_new_outcome_figures.R"))
# OR place P_new_outcome_figures.R in the same directory as this script and run:
source(file.path(COMPASS_DIR, "P_new_outcome_figures.R"))

# =============================================================================
# PATHWAY-FILTERED DIAGNOSTIC FIGURES
# High-CDR vs High-RE versions of all P1/P2 diagnostic figures
#
# Design convention (mirrors P3a/P3b):
#   colour/fill = Pathway (blue = High-CDR, red = High-RE)
#   facet cols  = Ambition (1.5C | 2C)
#   For line figures: solid = High-CDR, dashed = High-RE within each ambition facet
#   OR: colour = Pathway, linetype = Ambition — toggle via STYLE below
#
# To switch line-figure style, change PATH_LINE_STYLE:
#   "facet_ambition"  → facet by Ambition, colour = Pathway (default, matches bar figs)
#   "linetype_ambition" → colour = Pathway, linetype = Ambition, no facet
# =============================================================================
cat("\n=== PATHWAY-FILTERED DIAGNOSTIC FIGURES ===\n")

PATH_LINE_STYLE <- "facet_ambition"   # or "linetype_ambition"

# Shared helpers
PCOLS_PATH <- c("High-CDR" = "#2166ac", "High-RE" = "#d6604d")

# add_pathway_diag: like add_pathway() but preserves Category for assign_amb
add_pathway_diag <- function(df) {
  df %>%
    left_join(world_cum_f2 %>% select(Model, Scenario, high_cdr_only, high_re_only),
              by = c("Model", "Scenario")) %>%
    filter(!is.na(high_cdr_only), high_cdr_only | high_re_only) %>%
    assign_amb("Category") %>%
    filter(!is.na(Ambition)) %>%
    mutate(Pathway = if_else(high_cdr_only, "High-CDR", "High-RE"))
}

# add_pathway_diag_thresh: same but using absolute threshold classification
add_pathway_diag_thresh <- function(df) {
  df %>%
    left_join(world_cum_thresh %>% select(Model, Scenario, high_cdr_only, high_re_only),
              by = c("Model", "Scenario")) %>%
    filter(!is.na(high_cdr_only), high_cdr_only | high_re_only) %>%
    assign_amb("Category") %>%
    filter(!is.na(Ambition)) %>%
    mutate(Pathway = if_else(high_cdr_only, "High-CDR", "High-RE"))
}

# Summarise helper for line figures: median + IQR by Pathway × Ambition × Year [× group]
summarise_path <- function(df, ...) {
  df %>%
    group_by(Pathway, Ambition, Year, ...) %>%
    summarise(med = median(Value, na.rm = TRUE),
              q25 = quantile(Value, 0.25, na.rm = TRUE),
              q75 = quantile(Value, 0.75, na.rm = TRUE),
              .groups = "drop")
}

# Line plot builder — respects PATH_LINE_STYLE
plot_path_trend <- function(dat, ylab, free_y = FALSE) {
  scales_use <- if (free_y) "free_y" else "fixed"
  if (PATH_LINE_STYLE == "facet_ambition") {
    p <- ggplot(dat, aes(x = Year, colour = Pathway, fill = Pathway)) +
      geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.18, colour = NA) +
      geom_line(aes(y = med), linewidth = 1.0) +
      facet_wrap(~ Ambition, ncol = 2, scales = scales_use) +
      scale_colour_manual(values = PCOLS_PATH) +
      scale_fill_manual(values   = PCOLS_PATH)
  } else {
    p <- ggplot(dat, aes(x = Year, colour = Pathway, fill = Pathway,
                         linetype = Ambition)) +
      geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.12, colour = NA) +
      geom_line(aes(y = med), linewidth = 1.0) +
      scale_colour_manual(values = PCOLS_PATH) +
      scale_fill_manual(values   = PCOLS_PATH) +
      scale_linetype_manual(values = c("1.5C (High-Ambition)" = "solid",
                                       "2C (Medium-Ambition)"  = "dashed"))
  }
  p + scale_x_continuous(breaks = seq(2020, 2100, 20)) +
    labs(y = ylab, x = "Year", colour = "Pathway", fill = "Pathway") +
    theme_c(10)
}

# ── P1a (pathway) — now generated inline after original P1a (see line ~588) ──
cat("  P1a pathway (already generated inline)...\n")

# ── P1b (pathway) — CDR & RE deployment distributions, High-CDR vs High-RE ───
cat("  P1b pathway...\n")

p1b_path_cdr <- cdr_cumulative %>%
  filter(Variable == "Total CDR") %>%
  add_pathway_diag() %>%
  mutate(Model_family = simplify_model(Model))

p1b_path_re <- cdr_cumulative %>%
  filter(Variable == "Renewable Capacity", Region %in% regions_r10) %>%
  add_pathway_diag() %>%
  group_by(Model, Scenario, Pathway, Ambition) %>%
  summarise(Total_Value = sum(Total_Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Model_family = simplify_model(Model))

pb_path_cdr <- ggplot(p1b_path_cdr,
                      aes(x = reorder(Model_family, Total_Value, median),
                          y = Total_Value / 1e3, fill = Pathway)) +
  geom_boxplot(alpha = 0.65, outlier.size = 0.4, linewidth = 0.4) +
  coord_flip() +
  facet_grid(Pathway ~ Ambition, scales = "free_x") +
  scale_fill_manual(values = PCOLS_PATH, guide = "none") +
  labs(title = "Total CDR (World, cumul. 2020-2100)", x = NULL, y = "TtCO2") +
  theme_c(10)

pb_path_re <- ggplot(p1b_path_re,
                     aes(x = reorder(Model_family, Total_Value, median),
                         y = Total_Value / 1e3, fill = Pathway)) +
  geom_boxplot(alpha = 0.65, outlier.size = 0.4, linewidth = 0.4) +
  coord_flip() +
  facet_grid(Pathway ~ Ambition, scales = "free_x") +
  scale_fill_manual(values = PCOLS_PATH, guide = "none") +
  labs(title = "Renewable Capacity (sum R10, cumul. 2020-2100)",
       x = NULL, y = "Thousands of GW·yr") +
  theme_c(10)

p1b_path <- pb_path_cdr + pb_path_re +
  plot_annotation(
    title    = "P1b (pathway): CDR and RE Deployment by Model Family — High-CDR vs High-RE",
    subtitle = "Cumulative 2020-2100  ·  Distribution across pathway-classified scenarios",
    caption  = "Left: Total CDR. Right: Renewable Capacity, sum across R10 regions.",
    theme    = theme_c()
  )

sc(p1b_path, "P1b_pathway_deployment_by_model.png", 18, 10)

# ── P1c (pathway) — CDR component coverage, High-CDR vs High-RE ──────────────
cat("  P1c pathway...\n")

p1c_path_dat <- compass_interp %>%
  filter(Variable %in% c("Novel CDR", "Fossil CCS", "Land-based CDR"),
         Region %in% regions_r10, Year == 2050,
         !is.na(Value), Value > 0) %>%
  add_pathway_diag() %>%
  mutate(
    Model_family = simplify_model(Model),
    CDR_type     = factor(Variable, levels = c("Novel CDR","Fossil CCS","Land-based CDR"))
  ) %>%
  distinct(Model_family, CDR_type, Pathway, Ambition, Model, Scenario) %>%
  count(Model_family, CDR_type, Pathway, Ambition, name = "n_scenarios")

p1c_path <- ggplot(p1c_path_dat,
                   aes(x = reorder(Model_family, n_scenarios, sum),
                       y = n_scenarios, fill = CDR_type)) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.3) +
  coord_flip() +
  facet_grid(Pathway ~ Ambition, scales = "free_x") +
  scale_fill_manual(values = CCDR, name = "CDR type") +
  labs(
    title    = "P1c (pathway): CDR Component Coverage — High-CDR vs High-RE",
    subtitle = "Scenarios reporting non-zero values at 2050 for each CDR component type",
    x = NULL, y = "Scenarios with non-zero CDR in 2050",
    caption  = "Novel CDR = DAC + BECCS + Enhanced Weathering  ·  Fossil CCS = Fossil energy + industrial CCS  ·  Land-based = Carbon Removal|Land Use"
  ) +
  theme_c(10)

sc(p1c_path, "P1c_pathway_CDR_component_coverage.png", 16, 10)

# ── P2a (pathway) — CDR & RE annual trends, High-CDR vs High-RE ──────────────
cat("  P2a pathway...\n")

p2a_path_cdr <- compass_filtered %>%
  filter(Variable == "Total CDR", Region %in% regions_r10,
         Year >= 2020, Year <= 2100) %>%
  group_by(Model, Scenario, Category, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  add_pathway_diag() %>%
  summarise_path()

p2a_path_re <- compass_filtered %>%
  filter(Variable %in% c("Capacity|Electricity|Solar","Capacity|Electricity|Wind",
                         "Capacity|Electricity|Hydro","Capacity|Electricity|Nuclear",
                         "Capacity|Electricity|Biomass"),
         Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  group_by(Model, Scenario, Category, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  add_pathway_diag() %>%
  summarise_path()

p2a_path <-
  plot_path_trend(p2a_path_cdr, "MtCO2/yr") +
  labs(title = "Annual Total CDR (aggregated R10 regions, MtCO2/yr)") +
  plot_path_trend(p2a_path_re, "GW") +
  labs(title = "Annual Total Renewable Capacity (aggregated R10 regions, GW)") +
  plot_annotation(
    title    = "P2a (pathway): CDR and RE Annual Trends — High-CDR vs High-RE",
    subtitle = "Median ± IQR  ·  2020-2100  ·  High-CDR and High-RE scenarios only",
    caption  = "Solid line = median; shaded band = interquartile range.",
    theme    = theme_c()
  )

sc(p2a_path, "P2a_pathway_CDR_RE_annual_trends.png", 16, 7)

# ── P2a regional (pathway) ────────────────────────────────────────────────────
cat("  P2a regional pathway...\n")

p2a_path_cdr_reg <- compass_filtered %>%
  filter(Variable == "Total CDR", Region %in% regions_r10,
         Year >= 2020, Year <= 2100) %>%
  add_pathway_diag() %>%
  mutate(Region_label = REGION_LABELS_FIG[Region]) %>%
  summarise_path(Region_label) %>%
  rename(Technology_Deployment = med)

p2a_path_re_reg <- compass_filtered %>%
  filter(Variable %in% c("Capacity|Electricity|Solar","Capacity|Electricity|Wind",
                         "Capacity|Electricity|Hydro","Capacity|Electricity|Nuclear",
                         "Capacity|Electricity|Biomass"),
         Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  group_by(Model, Scenario, Category, Region, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  add_pathway_diag() %>%
  mutate(Region_label = REGION_LABELS_FIG[Region]) %>%
  summarise_path(Region_label) %>%
  rename(Technology_Deployment = med)

plot_path_trend_reg <- function(dat, ylab) {
  if (PATH_LINE_STYLE == "facet_ambition") {
    p <- ggplot(dat, aes(x = Year, colour = Pathway, fill = Pathway)) +
      geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.18, colour = NA) +
      geom_line(aes(y = Technology_Deployment), linewidth = 0.85) +
      facet_grid(Ambition ~ Region_label, scales = "free_y") +
      scale_colour_manual(values = PCOLS_PATH) +
      scale_fill_manual(values   = PCOLS_PATH)
  } else {
    p <- ggplot(dat, aes(x = Year, colour = Pathway, fill = Pathway,
                         linetype = Ambition)) +
      geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.12, colour = NA) +
      geom_line(aes(y = Technology_Deployment), linewidth = 0.85) +
      facet_wrap(~ Region_label, ncol = 5, scales = "free_y") +
      scale_colour_manual(values = PCOLS_PATH) +
      scale_fill_manual(values   = PCOLS_PATH) +
      scale_linetype_manual(values = c("1.5C (High-Ambition)" = "solid",
                                       "2C (Medium-Ambition)"  = "dashed"))
  }
  p +
    scale_x_continuous(breaks = c(2020, 2060, 2100)) +
    labs(x = "Year", y = ylab, colour = "Pathway", fill = "Pathway") +
    theme_c(9)
}

p2a_path_reg <-
  plot_path_trend_reg(p2a_path_cdr_reg, "Total CDR (MtCO2/yr)") /
  plot_path_trend_reg(p2a_path_re_reg,  "Renewable Capacity (GW)") +
  plot_annotation(
    title    = "P2a (pathway, regional): CDR and RE Trends by R10 Region — High-CDR vs High-RE",
    subtitle = "Median ± IQR  ·  2020-2100",
    theme    = theme_c()
  )

sc(p2a_path_reg, "P2a_pathway_CDR_RE_trends_by_region.png", 20, 12)

# ── P2b (pathway) — Air pollutant emissions, High-CDR vs High-RE ─────────────
cat("  P2b pathway...\n")

p2b_path_dat <- compass_filtered %>%
  filter(Variable %in% c("Emissions|NOx","Emissions|Sulfur"),
         Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  group_by(Model, Scenario, Category, Variable, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  add_pathway_diag() %>%
  mutate(Variable = recode(Variable,
                           "Emissions|NOx"    = "NOx (Mt/yr)",
                           "Emissions|Sulfur" = "SO2 (Mt/yr)")) %>%
  summarise_path(Variable)

if (PATH_LINE_STYLE == "facet_ambition") {
  p2b_path <- ggplot(p2b_path_dat,
                     aes(x = Year, colour = Pathway, fill = Pathway)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.18, colour = NA) +
    geom_line(aes(y = med), linewidth = 1.1) +
    facet_grid(Variable ~ Ambition, scales = "free_y") +
    scale_colour_manual(values = PCOLS_PATH) +
    scale_fill_manual(values   = PCOLS_PATH)
} else {
  p2b_path <- ggplot(p2b_path_dat,
                     aes(x = Year, colour = Pathway, fill = Pathway,
                         linetype = Ambition)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.12, colour = NA) +
    geom_line(aes(y = med), linewidth = 1.1) +
    facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
    scale_colour_manual(values = PCOLS_PATH) +
    scale_fill_manual(values   = PCOLS_PATH) +
    scale_linetype_manual(values = c("1.5C (High-Ambition)" = "solid",
                                     "2C (Medium-Ambition)"  = "dashed"))
}

p2b_path <- p2b_path +
  scale_x_continuous(breaks = seq(2020, 2100, 20)) +
  labs(title    = "P2b (pathway): Air Pollutant Emissions — High-CDR vs High-RE",
       subtitle = "Sum across R10 regions  ·  Median ± IQR",
       x = "Year", y = "Emissions (Mt/yr)",
       colour = "Pathway", fill = "Pathway",
       caption  = "NOx and SO2 are the primary PM2.5 precursors used by TM5-FASST to estimate mortality.") +
  theme_c()

sc(p2b_path, "P2b_pathway_emissions_trends.png", 14, 7)

# ── P2b regional (pathway) ────────────────────────────────────────────────────
cat("  P2b regional pathway...\n")

p2b_path_reg_dat <- compass_filtered %>%
  filter(Variable %in% c("Emissions|NOx","Emissions|Sulfur"),
         Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  add_pathway_diag() %>%
  mutate(Region_label = REGION_LABELS_FIG[Region],
         Variable = recode(Variable, "Emissions|NOx" = "NOx (Mt/yr)",
                           "Emissions|Sulfur" = "SO2 (Mt/yr)")) %>%
  summarise_path(Region_label, Variable)

if (PATH_LINE_STYLE == "facet_ambition") {
  p2b_path_reg <- ggplot(p2b_path_reg_dat,
                         aes(x = Year, colour = Pathway, fill = Pathway)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.18, colour = NA) +
    geom_line(aes(y = med), linewidth = 0.85) +
    facet_grid(Variable + Ambition ~ Region_label, scales = "free_y")
} else {
  p2b_path_reg <- ggplot(p2b_path_reg_dat,
                         aes(x = Year, colour = Pathway, fill = Pathway,
                             linetype = Ambition)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.12, colour = NA) +
    geom_line(aes(y = med), linewidth = 0.85) +
    facet_grid(Variable ~ Region_label, scales = "free_y") +
    scale_linetype_manual(values = c("1.5C (High-Ambition)" = "solid",
                                     "2C (Medium-Ambition)"  = "dashed"))
}

p2b_path_reg <- p2b_path_reg +
  scale_colour_manual(values = PCOLS_PATH) +
  scale_fill_manual(values   = PCOLS_PATH) +
  scale_x_continuous(breaks  = c(2020, 2060, 2100)) +
  labs(title    = "P2b (pathway, regional): Air Pollutant Emissions by R10 Region — High-CDR vs High-RE",
       subtitle = "Median ± IQR",
       x = "Year", y = "Emissions (Mt/yr)",
       colour = "Pathway", fill = "Pathway") +
  theme_c(9) + theme(strip.text.y = element_text(angle = -90))

sc(p2b_path_reg, "P2b_pathway_emissions_by_region.png", 20, 10)

# ── P2c (pathway) — Final energy by region, High-CDR vs High-RE ──────────────
cat("  P2c pathway...\n")

p2c_path_dat <- compass_filtered %>%
  filter(Variable == fe_var_use, Region %in% regions_r10,
         Year >= 2020, Year <= 2100) %>%
  add_pathway_diag() %>%
  mutate(Region_label = REGION_LABELS_FIG[Region]) %>%
  summarise_path(Region_label)

if (PATH_LINE_STYLE == "facet_ambition") {
  p2c_path <- ggplot(p2c_path_dat,
                     aes(x = Year, colour = Pathway, fill = Pathway)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.18, colour = NA) +
    geom_line(aes(y = med), linewidth = 0.95) +
    facet_grid(Ambition ~ Region_label, scales = "free_y")
} else {
  p2c_path <- ggplot(p2c_path_dat,
                     aes(x = Year, colour = Pathway, fill = Pathway,
                         linetype = Ambition)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.12, colour = NA) +
    geom_line(aes(y = med), linewidth = 0.95) +
    facet_wrap(~ Region_label, ncol = 5, scales = "free_y") +
    scale_linetype_manual(values = c("1.5C (High-Ambition)" = "solid",
                                     "2C (Medium-Ambition)"  = "dashed"))
}

p2c_path <- p2c_path +
  scale_colour_manual(values = PCOLS_PATH) +
  scale_fill_manual(values   = PCOLS_PATH) +
  scale_x_continuous(breaks  = c(2020, 2060, 2100)) +
  labs(title    = "P2c (pathway): Final Energy by Region — High-CDR vs High-RE",
       subtitle = "Compared against DLE thresholds (Kikstra et al. 2021)",
       x = "Year", y = "Final Energy (EJ/yr)",
       colour = "Pathway", fill = "Pathway",
       caption  = "DLE thresholds differ by region: Africa 24.5, India+ 22.5, China+ 37, Europe 52, N.America 63 GJ/capita/yr.") +
  theme_c(10)

sc(p2c_path, "P2c_pathway_final_energy_by_region.png", 18, 8)

# ── P2d (pathway) — Capacity additions, High-CDR vs High-RE ──────────────────
cat("  P2d pathway...\n")

p2d_path_dat <- compass_filtered %>%
  filter(Variable %in% c(
    "Capacity Additions|Electricity|Solar","Capacity Additions|Electricity|Wind",
    "Capacity Additions|Electricity|Hydro","Capacity Additions|Electricity|Nuclear",
    "Capacity Additions|Electricity|Biomass",
    "Capacity Additions|Electricity|Coal","Capacity Additions|Electricity|Gas"),
    Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  mutate(fuel       = str_remove(Variable, "Capacity Additions\\|Electricity\\|"),
         tech_group = if_else(fuel %in% c("Coal","Gas","Oil"), "Fossil","Renewables")) %>%
  group_by(Model, Scenario, Category, fuel, tech_group, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  add_pathway_diag() %>%
  group_by(Pathway, Ambition, fuel, tech_group, Year) %>%
  summarise(med = median(Value, na.rm = TRUE), .groups = "drop")

if (PATH_LINE_STYLE == "facet_ambition") {
  p2d_path <- ggplot(p2d_path_dat,
                     aes(x = Year, y = med, colour = fuel, linetype = Pathway)) +
    geom_line(linewidth = 0.9) +
    facet_grid(tech_group ~ Ambition, scales = "free_y") +
    scale_linetype_manual(values = c("High-CDR" = "solid", "High-RE" = "dashed"),
                          name = "Pathway")
} else {
  p2d_path <- ggplot(p2d_path_dat,
                     aes(x = Year, y = med, colour = fuel,
                         linetype = interaction(Pathway, Ambition, sep = " · "))) +
    geom_line(linewidth = 0.85) +
    facet_wrap(~ tech_group, scales = "free_y", ncol = 2)
}

p2d_path <- p2d_path +
  scale_colour_brewer(palette = "Set1", name = "Fuel type") +
  scale_x_continuous(breaks  = seq(2020, 2100, 20)) +
  labs(title    = "P2d (pathway): Capacity Additions by Fuel Type — High-CDR vs High-RE",
       subtitle = "Median across pathway-classified scenarios  ·  Sum across R10 regions",
       x = "Year", y = "Capacity Additions (GW/yr)",
       caption  = "Jobs = construction + O&M intensity × GW added per year.") +
  theme_c()

sc(p2d_path, "P2d_pathway_capacity_additions_trends.png", 14, 8)

# ── P2d regional (pathway) ───────────────────────────────────────────────────
cat("  P2d regional pathway...\n")

p2d_path_reg_dat <- compass_filtered %>%
  filter(Variable %in% c(
    "Capacity Additions|Electricity|Solar","Capacity Additions|Electricity|Wind",
    "Capacity Additions|Electricity|Hydro","Capacity Additions|Electricity|Nuclear",
    "Capacity Additions|Electricity|Biomass",
    "Capacity Additions|Electricity|Coal","Capacity Additions|Electricity|Gas"),
    Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  mutate(fuel       = str_remove(Variable, "Capacity Additions\\|Electricity\\|"),
         tech_group = if_else(fuel %in% c("Coal","Gas","Oil"), "Fossil","Renewables")) %>%
  add_pathway_diag() %>%
  mutate(Region_label = REGION_LABELS_FIG[Region]) %>%
  group_by(Pathway, Ambition, Region_label, fuel, tech_group, Year) %>%
  summarise(med = median(Value, na.rm = TRUE), .groups = "drop")

if (PATH_LINE_STYLE == "facet_ambition") {
  p2d_path_reg <- ggplot(p2d_path_reg_dat,
                         aes(x = Year, y = med, colour = fuel, linetype = Pathway)) +
    geom_line(linewidth = 0.65) +
    facet_grid(tech_group + Ambition ~ Region_label, scales = "free_y") +
    scale_linetype_manual(values = c("High-CDR" = "solid", "High-RE" = "dashed"),
                          name = "Pathway")
} else {
  p2d_path_reg <- ggplot(p2d_path_reg_dat,
                         aes(x = Year, y = med, colour = fuel,
                             linetype = interaction(Pathway, Ambition, sep = " · "))) +
    geom_line(linewidth = 0.65) +
    facet_grid(tech_group ~ Region_label, scales = "free_y")
}

p2d_path_reg <- p2d_path_reg +
  scale_colour_brewer(palette = "Set1", name = "Fuel") +
  scale_x_continuous(breaks  = c(2020, 2060, 2100)) +
  labs(title    = "P2d (pathway, regional): Capacity Additions by Fuel and Region — High-CDR vs High-RE",
       subtitle = "Median across pathway-classified scenarios  ·  GW/yr",
       x = "Year", y = "Capacity Additions (GW/yr)") +
  theme_c(9) + theme(strip.text.y = element_text(angle = -90))

sc(p2d_path_reg, "P2d_pathway_capacity_additions_by_region.png", 20, 12)

cat("\n=== PATHWAY-FILTERED DIAGNOSTIC FIGURES COMPLETE ===\n")
cat("Files saved to:", FIG_COMM, "\n")
cat("Figures produced:\n")
for (f in c("P1a_pathway_scenario_coverage",
            "P1b_pathway_deployment_by_model",
            "P1c_pathway_CDR_component_coverage",
            "P2a_pathway_CDR_RE_annual_trends",
            "P2a_pathway_CDR_RE_trends_by_region",
            "P2b_pathway_emissions_trends",
            "P2b_pathway_emissions_by_region",
            "P2c_pathway_final_energy_by_region",
            "P2d_pathway_capacity_additions_trends",
            "P2d_pathway_capacity_additions_by_region")) {
  cat("  ", f, ".png\n", sep="")
}
cat("\nTo switch line figure style, change PATH_LINE_STYLE at the top of this block:\n")
cat("  'facet_ambition'    → colour = Pathway, facet by Ambition (default)\n")
cat("  'linetype_ambition' → colour = Pathway, linetype = Ambition, no ambition facet\n")

# =============================================================================
# CLASSIFICATION COMPARISON FIGURES
# Side-by-side comparison of A (excl. tercile), B (overlap tercile), C (ratio)
#
# Figures produced:
#   COMP_CDR_components   — P3a-style CDR component bars for all three methods
#   COMP_CDR_RE_trends    — P2a-style CDR + RE annual trend lines
#   COMP_outcome_violins  — outcome violin plots (jobs, mortality, headcount)
#   COMP_scatter          — scatter of total_cdr vs total_re with classification
#                           overlay (most direct view of how the groups differ)
# =============================================================================
cat("\n=== CLASSIFICATION COMPARISON FIGURES ===\n")

# ── Shared helper: add_pathway_classified ────────────────────────────────────
# Joins a classification lookup (must have Model, Scenario, Pathway column)
# onto any compass_filtered-derived data frame
join_class <- function(df, lookup, pathway_col) {
  df %>%
    left_join(lookup %>% select(Model, Scenario, Ambition,
                                Pathway = !!sym(pathway_col)),
              by = c("Model", "Scenario", "Ambition")) %>%
    filter(!is.na(Pathway))
}

# Build standardised compass_filtered base with Ambition already attached
cf_amb <- compass_filtered %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition))

# ── COMP 0: Scatter — total_cdr vs total_re, coloured by classification ───────
cat("  COMP_scatter...\n")

scatter_base <- pathway_tercile %>%
  select(Model, Scenario, Category, Ambition, total_cdr, total_re) %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition), !is.na(total_cdr), !is.na(total_re)) %>%
  left_join(world_cum_f2 %>%
              mutate(Pathway_A = case_when(
                high_cdr_only ~ "High-CDR",
                high_re_only  ~ "High-RE",
                TRUE          ~ "Other")) %>%
              select(Model, Scenario, Pathway_A),
            by = c("Model","Scenario")) %>%
  left_join(world_cum_overlap %>% select(Model, Scenario, Ambition, Pathway_B),
            by = c("Model","Scenario","Ambition")) %>%
  left_join(world_cum_ratio   %>% select(Model, Scenario, Ambition, Pathway_C),
            by = c("Model","Scenario","Ambition")) %>%
  mutate(
    Pathway_A = replace_na(Pathway_A, "Other"),
    Pathway_B = replace_na(Pathway_B, "Other"),
    Pathway_C = replace_na(Pathway_C, "Other")
  ) %>%
  pivot_longer(cols = c(Pathway_A, Pathway_B, Pathway_C),
               names_to = "Method", values_to = "Pathway") %>%
  mutate(Method = recode(Method,
                         "Pathway_A" = "A: Excl. tercile",
                         "Pathway_B" = "B: Overlap tercile",
                         "Pathway_C" = "C: CDR ratio"))

PCOLS_COMP <- c("High-CDR" = "#2166ac", "High-RE" = "#d6604d", "Other" = "grey80")

p_scatter <- ggplot(scatter_base,
                    aes(x = total_re / 1e3, y = total_cdr / 1e3,
                        colour = Pathway, alpha = Pathway)) +
  geom_point(size = 0.9) +
  facet_grid(Ambition ~ Method) +
  scale_colour_manual(values = PCOLS_COMP) +
  scale_alpha_manual(values  = c("High-CDR" = 0.8, "High-RE" = 0.8,
                                 "Other" = 0.25), guide = "none") +
  labs(
    title    = "COMP: Classification Comparison — Scenario Space",
    subtitle = "Each point = one scenario. Coloured points = classified; grey = unclassified / middle group",
    x = "Cumulative Renewable Capacity (Thousands of GW·yr, 2020-2100)",
    y = "Cumulative Total CDR (TtCO2, 2020-2100)",
    colour   = "Pathway",
    caption  = paste0(
      "A: Mutually exclusive top-tercile CDR or RE (original)\n",
      "B: Overlapping top-tercile; 'Both' assigned by relative z-score\n",
      "C: Top/bottom tercile of CDR / (CDR + RE) ratio"
    )
  ) +
  theme_c()

sc(p_scatter, "COMP_scatter_classification.png", 16, 9)

# ── COMP 1: CDR component bars ────────────────────────────────────────────────
cat("  COMP_CDR_components...\n")

make_p3a_dat <- function(lookup, pathway_col) {
  cdr_cumulative %>%
    filter(Variable %in% c("Novel CDR","Fossil CCS","Land-based CDR"),
           Region %in% regions_r10) %>%
    group_by(Model, Scenario, Category, Variable) %>%
    summarise(Total_Value = sum(Total_Value, na.rm = TRUE), .groups = "drop") %>%
    assign_amb("Category") %>%
    filter(!is.na(Ambition)) %>%
    left_join(lookup %>% select(Model, Scenario, Ambition,
                                Pathway = !!sym(pathway_col)),
              by = c("Model","Scenario","Ambition")) %>%
    filter(!is.na(Pathway)) %>%
    mutate(Variable = factor(Variable,
                             levels = c("Novel CDR","Fossil CCS","Land-based CDR"))) %>%
    group_by(Pathway, Ambition, Variable) %>%
    summarise(mean_val = mean(Total_Value / 1e3, na.rm = TRUE),
              n        = n_distinct(paste(Model, Scenario)),
              .groups  = "drop")
}

p3a_A <- make_p3a_dat(
  world_cum_f2 %>% mutate(Pathway_A = case_when(
    high_cdr_only ~ "High-CDR", high_re_only ~ "High-RE", TRUE ~ NA_character_)),
  "Pathway_A") %>% mutate(Method = "A: Excl. tercile")

p3a_B <- make_p3a_dat(world_cum_overlap, "Pathway_B") %>%
  mutate(Method = "B: Overlap tercile")

p3a_C <- make_p3a_dat(world_cum_ratio, "Pathway_C") %>%
  mutate(Method = "C: CDR ratio")

p3a_comp_dat <- bind_rows(p3a_A, p3a_B, p3a_C) %>%
  mutate(Method = factor(Method, levels = c("A: Excl. tercile",
                                            "B: Overlap tercile",
                                            "C: CDR ratio")))

p_comp_cdr <- ggplot(p3a_comp_dat,
                     aes(x = Pathway, y = mean_val, fill = Variable)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
  facet_grid(Ambition ~ Method) +
  scale_fill_manual(values = CCDR, name = "CDR type") +
  scale_y_continuous(labels = comma_format(),
                     expand = expansion(mult = c(0, .08))) +
  labs(
    title    = "COMP: CDR Component Breakdown by Classification Method",
    subtitle = "Mean cumulative 2020-2100, summed across R10",
    x = NULL, y = "Mean cumulative CDR (TtCO2)",
    caption  = "A: Mutually exclusive tercile (original)  ·  B: Overlapping tercile  ·  C: CDR ratio tercile"
  ) +
  theme_c() +
  theme(axis.text.x = element_text(angle = 10, hjust = 1))

sc(p_comp_cdr, "COMP_CDR_components.png", 16, 9)

# ── COMP 2: CDR & RE annual trends ────────────────────────────────────────────
cat("  COMP_CDR_RE_trends...\n")

make_trend_dat <- function(var_filter, agg_fn, lookup, pathway_col, label) {
  cf_amb %>%
    filter(Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
    { if (!is.null(var_filter)) filter(., Variable %in% var_filter) else . } %>%
    { agg_fn(.) } %>%
    left_join(lookup %>% select(Model, Scenario, Ambition,
                                Pathway = !!sym(pathway_col)),
              by = c("Model","Scenario","Ambition")) %>%
    filter(!is.na(Pathway)) %>%
    group_by(Pathway, Ambition, Year) %>%
    summarise(med = median(Value, na.rm = TRUE),
              q25 = quantile(Value, 0.25, na.rm = TRUE),
              q75 = quantile(Value, 0.75, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(Method = label)
}

sum_regions <- function(df) {
  df %>%
    group_by(Model, Scenario, Category, Ambition, Year) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
}

cdr_vars  <- "Total CDR"
re_vars   <- c("Capacity|Electricity|Solar","Capacity|Electricity|Wind",
               "Capacity|Electricity|Hydro","Capacity|Electricity|Nuclear",
               "Capacity|Electricity|Biomass")

lookup_A <- world_cum_f2 %>%
  mutate(Pathway_A = case_when(high_cdr_only ~ "High-CDR",
                               high_re_only  ~ "High-RE",
                               TRUE          ~ NA_character_))

trend_cdr <- bind_rows(
  make_trend_dat(cdr_vars, sum_regions, lookup_A,         "Pathway_A", "A: Excl. tercile"),
  make_trend_dat(cdr_vars, sum_regions, world_cum_overlap, "Pathway_B", "B: Overlap tercile"),
  make_trend_dat(cdr_vars, sum_regions, world_cum_ratio,   "Pathway_C", "C: CDR ratio")
) %>% mutate(Panel = "Total CDR (MtCO2/yr)")

trend_re <- bind_rows(
  make_trend_dat(re_vars, sum_regions, lookup_A,          "Pathway_A", "A: Excl. tercile"),
  make_trend_dat(re_vars, sum_regions, world_cum_overlap,  "Pathway_B", "B: Overlap tercile"),
  make_trend_dat(re_vars, sum_regions, world_cum_ratio,    "Pathway_C", "C: CDR ratio")
) %>% mutate(Panel = "Renewable Capacity (GW)")

trend_comp_dat <- bind_rows(trend_cdr, trend_re) %>%
  mutate(Method = factor(Method, levels = c("A: Excl. tercile",
                                            "B: Overlap tercile",
                                            "C: CDR ratio")))

p_comp_trends <- ggplot(trend_comp_dat,
                        aes(x = Year, colour = Pathway, fill = Pathway)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.18, colour = NA) +
  geom_line(aes(y = med), linewidth = 0.9) +
  facet_grid(Panel ~ Method + Ambition, scales = "free_y") +
  scale_colour_manual(values = PCOLS_PATH) +
  scale_fill_manual(values   = PCOLS_PATH) +
  scale_x_continuous(breaks  = c(2020, 2060, 2100)) +
  labs(
    title    = "COMP: CDR and RE Annual Trends by Classification Method",
    subtitle = "Median ± IQR  ·  Aggregated R10 regions  ·  2020-2100",
    x = "Year", y = NULL,
    colour = "Pathway", fill = "Pathway",
    caption  = "Solid = median; ribbon = IQR.  A: original excl. tercile  ·  B: overlap  ·  C: CDR ratio"
  ) +
  theme_c(9)

sc(p_comp_trends, "COMP_CDR_RE_trends.png", 22, 10)

# ── COMP 3: Outcome violins ────────────────────────────────────────────────────
cat("  COMP_outcome_violins...\n")

# Build outcome data for each classification
make_outcome_dat <- function(lookup, pathway_col, method_label) {
  df_master %>%
    filter(Variable == "Total CDR", Region %in% regions_r10) %>%
    assign_amb("Category") %>%
    filter(!is.na(Ambition)) %>%
    left_join(lookup %>% select(Model, Scenario, Ambition,
                                Pathway = !!sym(pathway_col)),
              by = c("Model","Scenario","Ambition")) %>%
    filter(!is.na(Pathway)) %>%
    group_by(Model, Scenario, Pathway, Ambition) %>%
    summarise(
      jobs_RE     = sum(jobs_Renewables,          na.rm = TRUE),
      jobs_Fossil = sum(jobs_Fossil,              na.rm = TRUE),
      headcount   = sum(mean_headcount_millions,  na.rm = TRUE),
      deaths      = sum(cumulative_deaths_mln,    na.rm = TRUE),
      .groups     = "drop"
    ) %>%
    mutate(net_jobs = jobs_RE - jobs_Fossil,
           Method   = method_label)
}

outcomes_comp <- bind_rows(
  make_outcome_dat(lookup_A,          "Pathway_A", "A: Excl. tercile"),
  make_outcome_dat(world_cum_overlap,  "Pathway_B", "B: Overlap tercile"),
  make_outcome_dat(world_cum_ratio,    "Pathway_C", "C: CDR ratio")
) %>%
  mutate(Method = factor(Method, levels = c("A: Excl. tercile",
                                            "B: Overlap tercile",
                                            "C: CDR ratio"))) %>%
  pivot_longer(cols = c(net_jobs, headcount, deaths),
               names_to = "outcome", values_to = "value") %>%
  mutate(outcome_lab = recode(outcome,
                              "net_jobs"  = "Net RE Jobs (thousands)",
                              "headcount" = "Headcount below DLS (millions)",
                              "deaths"    = "Cumulative PM2.5 deaths (millions)"
  ))

p_comp_outcomes <- ggplot(outcomes_comp,
                          aes(x = Pathway, y = value,
                              fill = Pathway, colour = Pathway)) +
  geom_violin(alpha = 0.3, colour = NA, scale = "width", trim = TRUE) +
  geom_boxplot(width = 0.18, outlier.size = 0.5,
               colour = "grey20", fill = "white", alpha = 0.9) +
  stat_summary(fun = median, geom = "point",
               size = 2.5, colour = "grey10", shape = 18) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  facet_grid(outcome_lab ~ Method + Ambition, scales = "free_y") +
  scale_fill_manual(values   = PCOLS_PATH, guide = "none") +
  scale_colour_manual(values = PCOLS_PATH, guide = "none") +
  scale_y_continuous(labels  = comma_format()) +
  labs(
    title    = "COMP: Wellbeing Outcomes by Classification Method",
    subtitle = "Aggregated R10 regions  ·  Violin = distribution; box = IQR; diamond = median",
    x = NULL, y = NULL,
    caption  = "A: original excl. tercile  ·  B: overlap  ·  C: CDR ratio"
  ) +
  theme_c(9)

sc(p_comp_outcomes, "COMP_outcome_violins.png", 22, 14)

cat("\n=== CLASSIFICATION COMPARISON FIGURES COMPLETE ===\n")
cat("Files saved to:", FIG_COMM, "\n")
cat("Figures produced:\n")
cat("  COMP_scatter_classification.png  — CDR vs RE scatter, all three methods\n")
cat("  COMP_CDR_components.png          — P3a-style CDR component bars\n")
cat("  COMP_CDR_RE_trends.png           — P2a-style trend lines\n")
cat("  COMP_outcome_violins.png         — Jobs / headcount / mortality violins\n")
cat("\nScenario counts by method printed above at classification build time.\n")

# =============================================================================
# THRESHOLD SENSITIVITY DIAGNOSTIC
# Sweeps across classification thresholds (top 25%, 33%, 40%, 50%, 60%, 67%)
# and shows for each:
#   1. Model family concentration in High-CDR and High-RE
#   2. N scenarios per group
#   3. Herfindahl Index (HHI) — measure of model dominance (1 = one model, 
#      low = well distributed). HHI = sum of squared shares.
# Goal: find the lowest threshold where High-RE is no longer REMIND-dominated
# =============================================================================
cat("\n=== THRESHOLD SENSITIVITY DIAGNOSTIC ===\n")

thresholds <- c(0.25, 0.33, 0.40, 0.50, 0.60, 0.67)

threshold_results <- map_dfr(thresholds, function(thresh) {
  n_tiles <- round(1 / thresh)
  top_tile <- n_tiles
  
  pathway_tercile %>%
    select(Model, Scenario, Category, Ambition, total_cdr, total_re) %>%
    assign_amb("Category") %>%
    filter(!is.na(Ambition)) %>%
    group_by(Ambition) %>%
    mutate(
      high_cdr      = ntile(total_cdr, n_tiles) == top_tile,
      high_re       = ntile(total_re,  n_tiles) == top_tile,
      high_cdr_only = high_cdr & !high_re,
      high_re_only  = high_re  & !high_cdr
    ) %>%
    ungroup() %>%
    filter(high_cdr_only | high_re_only) %>%
    mutate(
      Pathway      = if_else(high_cdr_only, "High-CDR", "High-RE"),
      Model_family = simplify_model(Model),
      threshold    = thresh,
      threshold_label = paste0("Top ", round(thresh * 100), "%")
    )
}) %>%
  group_by(threshold, threshold_label, Ambition, Pathway, Model_family) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(threshold, threshold_label, Ambition, Pathway) %>%
  mutate(
    total_n = sum(n),
    share   = n / total_n,
    HHI     = sum(share^2)   # computed per group, same value for all rows in group
  ) %>%
  ungroup()

# Print HHI summary table — key diagnostic
cat("\nHHI by threshold, ambition, pathway (lower = more distributed):\n")
cat("HHI = 1.0 means one model dominates; HHI = 0.1 means ~10 equal models\n\n")
threshold_results %>%
  distinct(threshold, threshold_label, Ambition, Pathway, total_n, HHI) %>%
  arrange(Ambition, Pathway, threshold) %>%
  mutate(HHI = round(HHI, 3), dominant_share = round(1/HHI, 1)) %>%
  print(n = Inf)

# Also print top model family share at each threshold for High-RE
cat("\nTop model family share in High-RE by threshold:\n")
threshold_results %>%
  filter(Pathway == "High-RE") %>%
  group_by(threshold_label, Ambition) %>%
  slice_max(share, n = 1) %>%
  select(threshold_label, Ambition, Model_family, n, total_n, share) %>%
  mutate(share = scales::percent(share, accuracy = 1)) %>%
  arrange(Ambition, threshold_label) %>%
  print(n = Inf)

# ── Figure: model family concentration heatmap by threshold ──────────────────
cat("\n  Plotting threshold sensitivity figure...\n")

# Get all model families present
all_families <- threshold_results %>%
  distinct(Model_family) %>%
  pull(Model_family) %>%
  sort()

# Complete grid so missing families show as 0
threshold_plot_dat <- threshold_results %>%
  complete(threshold_label, Ambition, Pathway, Model_family,
           fill = list(n = 0, share = 0)) %>%
  mutate(
    threshold_label = factor(threshold_label,
                             levels = paste0("Top ", round(thresholds * 100), "%")),
    pct_label = if_else(share > 0.02,
                        paste0(round(share * 100), "%"), "")
  )

p_thresh_heat <- ggplot(threshold_plot_dat,
                        aes(x = threshold_label, y = Model_family,
                            fill = share)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = pct_label), size = 2.5, colour = "grey20") +
  facet_grid(Pathway ~ Ambition) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       labels  = percent_format(accuracy = 1),
                       name    = "Share of\ngroup") +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  labs(
    title    = "DIAG: Model Family Concentration by Classification Threshold",
    subtitle = "Share of High-CDR / High-RE scenarios from each model family  ·  Mutually exclusive",
    x = "Threshold (top X% classified as High)",
    y = NULL,
    caption  = paste0(
      "Each cell = % of scenarios in that pathway group from that model family at that threshold.\n",
      "Goal: find threshold where High-RE is no longer REMIND-dominated (~85% at top 33%)."
    )
  ) +
  theme_c(9) +
  theme(panel.grid = element_blank())

sc(p_thresh_heat, "DIAG_threshold_concentration.png", 16, 10)

# ── Figure: HHI line plot across thresholds ───────────────────────────────────
hhi_dat <- threshold_results %>%
  distinct(threshold, threshold_label, Ambition, Pathway, total_n, HHI) %>%
  mutate(threshold_label = factor(threshold_label,
                                  levels = paste0("Top ", round(thresholds*100), "%")))

p_hhi <- ggplot(hhi_dat,
                aes(x = threshold_label, y = HHI,
                    colour = Pathway, group = Pathway)) +
  geom_line(linewidth = 1.1) +
  geom_point(aes(size = total_n), alpha = 0.8) +
  geom_hline(yintercept = 0.25, linetype = "dashed",
             colour = "grey50", linewidth = 0.5) +
  annotate("text", x = 1, y = 0.27, label = "HHI = 0.25 (moderate concentration)",
           hjust = 0, size = 3, colour = "grey40") +
  facet_wrap(~ Ambition, ncol = 2) +
  scale_colour_manual(values = PCOLS_PATH) +
  scale_size_continuous(name = "N scenarios", range = c(2, 6)) +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  labs(
    title    = "DIAG: Model Concentration (HHI) by Threshold",
    subtitle = "Lower HHI = more evenly distributed across model families",
    x = "Threshold", y = "Herfindahl Index (HHI)",
    colour   = "Pathway",
    caption  = "Point size = number of scenarios in group. Dashed line = moderate concentration threshold."
  ) +
  theme_c()

sc(p_hhi, "DIAG_HHI_by_threshold.png", 12, 6)

cat("\n=== THRESHOLD SENSITIVITY DIAGNOSTIC COMPLETE ===\n")
cat("  DIAG_threshold_concentration.png — heatmap of model shares by threshold\n")
cat("  DIAG_HHI_by_threshold.png        — HHI concentration index across thresholds\n")
cat("\nKey question: at what threshold does High-RE HHI drop below ~0.25?\n")


# =============================================================================
# ABSOLUTE THRESHOLD CLASSIFICATION — COMPARISON FIGURES
#
# Mirrors all key pathway-dependent figures (P3a, P3b, P3c, P4a, P4b, P4d,
# P5a) using world_cum_thresh instead of world_cum_f2.
# All filenames have "_THRESH" suffix to distinguish from tercile versions.
# =============================================================================
cat("\n=== THRESHOLD CLASSIFICATION COMPARISON FIGURES ===\n")
cat("Using absolute CDR/RE thresholds instead of relative terciles.\n")
cat("See Section 6b of COMPASS_full_analysis_revised_CW_3_23.R for threshold values.\n\n")

# ── Rebuild core outcome datasets using threshold classification ──────────────

# mort_to_nz_thresh
mort_to_nz_thresh <- mortality_r10_raw %>%
  filter(Region %in% regions_r10, Year >= 2020) %>%
  left_join(
    world_cum_thresh %>%
      select(Model, Scenario, Category, Ambition, high_cdr_only, high_re_only),
    by = c("Model", "Scenario")
  ) %>%
  filter(!is.na(high_cdr_only), high_cdr_only | high_re_only, !is.na(Ambition)) %>%
  mutate(
    Pathway = if_else(high_cdr_only, "High-CDR", "High-RE"),
    cutoff  = if_else(str_detect(Ambition, "1.5C"), WINDOW_15C, WINDOW_2C)
  ) %>%
  filter(Year <= cutoff) %>%
  group_by(Model, Scenario, Pathway, Ambition, Region) %>%
  summarise(cum_deaths_nz = sum(deaths_pm25 * 5, na.rm = TRUE) / 1e6,
            .groups = "drop")

cat("mort_to_nz_thresh rows:", nrow(mort_to_nz_thresh), "\n")

# outcomes_nz_thresh (regional)
outcomes_nz_thresh <- df_master %>%
  filter(Variable == "Total CDR", Region %in% regions_r10) %>%
  add_pathway_thresh() %>%
  mutate(cutoff = get_cutoff(Ambition)) %>%
  left_join(mort_to_nz_thresh %>%
              select(Model, Scenario, Region, cum_deaths_nz),
            by = c("Model","Scenario","Region")) %>%
  select(Model, Scenario, Pathway, Ambition, Region, cutoff,
         cum_deaths_nz, cumulative_gap_EJ, mean_headcount_millions,
         cumulative_implied_CO2_GtCO2, jobs_Fossil, jobs_Renewables) %>%
  left_join(
    pop_ts %>%
      filter(Region %in% regions_r10, Year == 2020) %>%
      group_by(Region) %>%
      summarise(pop_mln = median(Value, na.rm = TRUE), .groups = "drop"),
    by = "Region"
  ) %>%
  mutate(
    cum_deaths_nz           = (cum_deaths_nz * 1e6)          / pop_mln,
    mean_headcount_millions = (mean_headcount_millions * 1e6) / pop_mln,
    jobs_Renewables         = jobs_Renewables                 / pop_mln,
    jobs_Fossil             = jobs_Fossil                     / pop_mln,
    Region_label = REGION_LABELS_FIG[Region],
    Group = paste0(Pathway, "\n", Ambition),
    Group = factor(Group, levels = c(
      "High-CDR\n1.5C (High-Ambition)", "High-RE\n1.5C (High-Ambition)",
      "High-CDR\n2C (Medium-Ambition)", "High-RE\n2C (Medium-Ambition)"
    ))
  )

# outcomes_nz_world_thresh (aggregated)
outcomes_nz_world_thresh <- df_master %>%
  filter(Variable == "Total CDR", Region == "Aggregated R10 regions") %>%
  add_pathway_thresh() %>%
  mutate(cutoff = get_cutoff(Ambition)) %>%
  left_join(mort_to_nz_thresh %>%
              group_by(Model, Scenario, Pathway, Ambition) %>%
              summarise(cum_deaths_nz = sum(cum_deaths_nz, na.rm=TRUE), .groups="drop"),
            by = c("Model","Scenario","Pathway","Ambition")) %>%
  select(Model, Scenario, Pathway, Ambition, cutoff, cum_deaths_nz,
         cumulative_gap_EJ, mean_headcount_millions,
         cumulative_implied_CO2_GtCO2, jobs_Fossil, jobs_Renewables) %>%
  left_join(
    pop_ts %>%
      filter(Region %in% regions_r10, Year == 2020) %>%
      group_by(Region) %>%
      summarise(pop_mln = median(Value, na.rm = TRUE), .groups = "drop") %>%
      summarise(pop_agg = sum(pop_mln)),
    by = character()
  ) %>%
  mutate(
    cum_deaths_nz           = (cum_deaths_nz * 1e6)          / pop_agg,
    mean_headcount_millions = (mean_headcount_millions * 1e6) / pop_agg,
    jobs_Renewables         = jobs_Renewables                 / pop_agg,
    jobs_Fossil             = jobs_Fossil                     / pop_agg,
    Region_label = "Aggregated R10 regions",
    Group = paste0(Pathway, "\n", Ambition),
    Group = factor(Group, levels = c(
      "High-CDR\n1.5C (High-Ambition)", "High-RE\n1.5C (High-Ambition)",
      "High-CDR\n2C (Medium-Ambition)", "High-RE\n2C (Medium-Ambition)"
    ))
  )

cat("outcomes_nz_thresh rows:", nrow(outcomes_nz_thresh), "\n")
cat("outcomes_nz_world_thresh rows:", nrow(outcomes_nz_world_thresh), "\n")

# ── P3a_THRESH — CDR components by pathway (threshold) ───────────────────────
p3a_thresh_dat <- cdr_cumulative %>%
  filter(Variable %in% c("Novel CDR","Fossil CCS","Land-based CDR"),
         Region %in% regions_r10) %>%
  left_join(world_cum_thresh %>% select(Model, Scenario,
                                        high_cdr_only, high_re_only),
            by = c("Model","Scenario")) %>%
  filter(high_cdr_only | high_re_only) %>%
  assign_amb("Category") %>% filter(!is.na(Ambition)) %>%
  mutate(Pathway  = if_else(high_cdr_only, "High-CDR", "High-RE"),
         Variable = factor(Variable, levels=c("Novel CDR","Fossil CCS","Land-based CDR"))) %>%
  group_by(Pathway, Ambition, Variable) %>%
  summarise(mean_val = mean(Total_Value/1e3, na.rm=TRUE),
            n = n_distinct(paste(Model,Scenario)), .groups="drop")

p3a_thresh <- ggplot(p3a_thresh_dat,
                     aes(x=Pathway, y=mean_val, fill=Variable)) +
  geom_col(width=0.7, colour="white", linewidth=0.3) +
  facet_wrap(~Ambition, ncol=2) +
  scale_fill_manual(values=CCDR) +
  scale_y_continuous(labels=comma_format()) +
  labs(title    = "P3a [THRESH]: CDR Components by Pathway — Absolute Threshold Classification",
       subtitle = "Mean cumulative 2020-2100, summed across R10  ·  World-level absolute threshold classification",
       x=NULL, y="Cumulative CDR (GtCO2, 2020-2100)", fill="CDR type",
       caption  = "Absolute threshold classification — see Section 6b of analysis script for threshold values.") +
  theme_c()
sc(p3a_thresh, "P3a_CDR_components_by_pathway_THRESH.png", 12, 7)

# ── P3a_reg_THRESH ────────────────────────────────────────────────────────────
p3a_reg_thresh_dat <- cdr_cumulative %>%
  filter(Variable %in% c("Novel CDR","Fossil CCS","Land-based CDR"),
         Region %in% regions_r10) %>%
  left_join(world_cum_thresh %>% select(Model, Scenario, high_cdr_only, high_re_only),
            by = c("Model","Scenario")) %>%
  filter(high_cdr_only | high_re_only) %>%
  assign_amb("Category") %>% filter(!is.na(Ambition)) %>%
  mutate(Pathway  = if_else(high_cdr_only, "High-CDR", "High-RE"),
         Variable = factor(Variable, levels=c("Novel CDR","Fossil CCS","Land-based CDR")),
         Region_label = REGION_LABELS_FIG[Region]) %>%
  group_by(Pathway, Ambition, Region_label, Variable) %>%
  summarise(mean_val = mean(Total_Value/1e3, na.rm=TRUE), .groups="drop")

p3a_reg_thresh <- ggplot(p3a_reg_thresh_dat,
                         aes(x=Pathway, y=mean_val, fill=Variable)) +
  geom_col(width=0.7, colour="white", linewidth=0.3) +
  facet_grid(Region_label ~ Ambition) +
  scale_fill_manual(values=CCDR) +
  scale_y_continuous(labels=comma_format()) +
  labs(title    = "P3a (regional) [THRESH]: CDR Components by Pathway and Region",
       subtitle = "Mean cumulative 2020-2100  ·  Absolute threshold classification",
       x=NULL, y="Cumulative CDR (GtCO2)", fill="CDR type") +
  theme_c(8) +
  theme(strip.text.y = element_text(angle=-90, size=7))
sc(p3a_reg_thresh, "P3a_CDR_components_by_region_THRESH.png", 13, 14)

# ── P3b_THRESH — electricity mix by pathway ───────────────────────────────────
p3b_thresh_dat <- compass_filtered %>%
  filter(Variable %in% names(elec_vars_map),
         Region %in% regions_r10, Year %in% c(2030,2050,2070)) %>%
  left_join(world_cum_thresh %>% select(Model, Scenario, high_cdr_only, high_re_only),
            by = c("Model","Scenario")) %>%
  filter(high_cdr_only | high_re_only) %>%
  mutate(Pathway = if_else(high_cdr_only,"High-CDR","High-RE"),
         fuel    = elec_vars_map[Variable]) %>%
  assign_amb("Category") %>% filter(!is.na(Ambition)) %>%
  group_by(Model, Scenario, Pathway, Ambition, Year) %>%
  mutate(share = Value/sum(Value, na.rm=TRUE)) %>%
  filter(!is.na(share), is.finite(share)) %>%
  group_by(Pathway, Ambition, Year, fuel) %>%
  summarise(med_share = median(share, na.rm=TRUE), .groups="drop") %>%
  mutate(fuel   = factor(fuel, levels=rev(names(PELEC))),
         AmbYear = factor(
           paste0(if_else(str_detect(Ambition,"1.5"),"1.5C","2C")," ",Year),
           levels = c("1.5C 2030","1.5C 2050","1.5C 2070","2C 2030","2C 2050","2C 2070")
         ))

p3b_thresh <- ggplot(p3b_thresh_dat,
                     aes(x=Pathway, y=med_share, fill=fuel)) +
  geom_col(width=0.8, colour="white", linewidth=0.15, position="fill") +
  facet_wrap(~AmbYear, nrow=1) +
  scale_fill_manual(values=PELEC, guide=guide_legend(nrow=2, reverse=TRUE)) +
  scale_y_continuous(labels=percent_format()) +
  labs(title    = "P3b [THRESH]: Electricity Mix by Pathway — Absolute Threshold Classification",
       subtitle = "Median share  ·  2030/2050/2070  ·  High-CDR vs High-RE",
       x=NULL, y="Share of electricity", fill="Fuel type") +
  theme_c(8)
sc(p3b_thresh, "P3b_electricity_mix_by_pathway_THRESH.png", 13, 9)

# ── P3c_THRESH — NOx trajectories ─────────────────────────────────────────────
p3c_thresh_dat <- compass_filtered %>%
  filter(Variable == "Emissions|NOx",
         Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  left_join(world_cum_thresh %>% select(Model, Scenario, high_cdr_only, high_re_only),
            by = c("Model","Scenario")) %>%
  filter(high_cdr_only | high_re_only) %>%
  mutate(Pathway = if_else(high_cdr_only,"High-CDR","High-RE")) %>%
  assign_amb("Category") %>% filter(!is.na(Ambition)) %>%
  mutate(Region_label = REGION_LABELS_FIG[Region]) %>%
  group_by(Pathway, Ambition, Region_label, Year) %>%
  summarise(med = median(Value, na.rm=TRUE),
            q25 = quantile(Value, 0.25, na.rm=TRUE),
            q75 = quantile(Value, 0.75, na.rm=TRUE), .groups="drop")

p3c_thresh <- ggplot(p3c_thresh_dat,
                     aes(x=Year, colour=Pathway, fill=Pathway)) +
  geom_ribbon(aes(ymin=q25, ymax=q75), alpha=0.15, colour=NA) +
  geom_line(aes(y=med), linewidth=1.0) +
  facet_grid(Region_label ~ Ambition, scales="free_y") +
  scale_colour_manual(values=PCOLS) +
  scale_fill_manual(values=PCOLS) +
  scale_x_continuous(breaks=c(2020,2050,2080)) +
  labs(title    = "P3c [THRESH]: NOx Emissions by Pathway — Absolute Threshold Classification",
       subtitle = "Median ± IQR  ·  Absolute threshold classification",
       x="Year", y="NOx emissions (Mt/yr)", colour="Pathway", fill="Pathway") +
  theme_c() +
  theme(strip.text.y=element_text(angle=-90, size=8))
sc(p3c_thresh, "P3c_NOx_by_pathway_region_THRESH.png", 14, 14)

# ── P4a_THRESH — outcome violins (aggregated) ─────────────────────────────────
p4a_long_thresh <- outcomes_nz_world_thresh %>%
  select(Model, Scenario, Group, all_of(names(OUTCOME_LABS_NZ))) %>%
  pivot_longer(cols = all_of(names(OUTCOME_LABS_NZ)),
               names_to = "outcome_col", values_to = "val") %>%
  filter(!is.na(val), val > 0) %>%
  mutate(outcome_lab = factor(OUTCOME_LABS_NZ[outcome_col],
                              levels = unname(OUTCOME_LABS_NZ)))

sc(make_p4a(p4a_long_thresh, "1.5C", " (1.5C High-Ambition) [THRESH]",
            "P4a_outcome_violins_15C_THRESH.png"),
   "P4a_outcome_violins_15C_THRESH.png", 14, 8)
sc(make_p4a(p4a_long_thresh, "2C", " (2C Medium-Ambition) [THRESH]",
            "P4a_outcome_violins_2C_THRESH.png"),
   "P4a_outcome_violins_2C_THRESH.png", 14, 8)
sc(make_p4a(p4a_long_thresh, NULL, " (1.5C and 2C Combined) [THRESH]",
            "P4a_outcome_violins_combined_THRESH.png"),
   "P4a_outcome_violins_combined_THRESH.png", 18, 8)
cat("  P4a threshold variants saved.\n")

# P4a_reg_THRESH
p4a_reg_long_thresh <- outcomes_nz_thresh %>%
  filter(!is.na(Region_label)) %>%
  select(Model, Scenario, Group, Region_label, all_of(names(OUTCOME_LABS_NZ))) %>%
  pivot_longer(all_of(names(OUTCOME_LABS_NZ)),
               names_to="outcome_col", values_to="val") %>%
  filter(!is.na(val), val > 0) %>%
  mutate(outcome_lab = factor(OUTCOME_LABS_NZ[outcome_col],
                              levels=unname(OUTCOME_LABS_NZ)))

sc(make_p4a_reg(p4a_reg_long_thresh, "1.5C", " (1.5C) [THRESH]",
                "P4a_reg_outcome_violins_15C_THRESH.png"),
   "P4a_reg_outcome_violins_15C_THRESH.png", 20, 10)
sc(make_p4a_reg(p4a_reg_long_thresh, "2C", " (2C) [THRESH]",
                "P4a_reg_outcome_violins_2C_THRESH.png"),
   "P4a_reg_outcome_violins_2C_THRESH.png", 20, 10)
sc(make_p4a_reg(p4a_reg_long_thresh, NULL, " (Combined) [THRESH]",
                "P4a_reg_outcome_violins_combined_THRESH.png"),
   "P4a_reg_outcome_violins_combined_THRESH.png", 20, 18)
cat("  P4a regional threshold variants saved.\n")

# ── P4b_THRESH — mortality trajectories ───────────────────────────────────────
p4b_thresh_dat <- mortality_r10_raw %>%
  filter(Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  left_join(world_cum_thresh %>%
              select(Model, Scenario, Ambition, high_cdr_only, high_re_only),
            by = c("Model","Scenario")) %>%
  filter(!is.na(high_cdr_only), high_cdr_only | high_re_only, !is.na(Ambition)) %>%
  mutate(Pathway = if_else(high_cdr_only, "High-CDR", "High-RE")) %>%
  group_by(Model, Scenario, Pathway, Ambition, Year) %>%
  summarise(deaths = sum(deaths_pm25, na.rm=TRUE), .groups="drop") %>%
  mutate(Group = paste0(Pathway,"\n",Ambition),
         Group = factor(Group, levels=names(GROUP_COLS_4)))

p4b_thresh <- ggplot(p4b_thresh_dat,
                     aes(x=Year, y=deaths/1e6,
                         group=interaction(Model,Scenario), colour=Group)) +
  geom_rect(data=nz_rects,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            inherit.aes=FALSE, fill="grey95", alpha=0.6) +
  geom_line(alpha=0.10, linewidth=0.3) +
  stat_summary(aes(group=Group), fun=median,
               geom="line", linewidth=1.3, na.rm=TRUE) +
  geom_vline(data=nz_rects, aes(xintercept=xmin),
             linetype="dashed", colour="grey40", linewidth=0.8, inherit.aes=FALSE) +
  facet_wrap(~Ambition, ncol=2) +
  scale_colour_manual(values=GROUP_COLS_4,
                      guide=guide_legend(title="Pathway", nrow=2,
                                         override.aes=list(alpha=1,linewidth=1.3))) +
  scale_x_continuous(breaks=seq(2020,2100,10)) +
  labs(title    = "P4b [THRESH]: Mortality Trajectories — Absolute Threshold Classification",
       subtitle = "Thin = individual scenarios  ·  Bold = median  ·  Shaded = post net-zero",
       x="Year", y="PM2.5 deaths (millions/yr)") +
  theme_c()
sc(p4b_thresh, "P4b_mortality_trajectory_world_THRESH.png", 14, 7)

# P4b_reg_THRESH
p4b_reg_thresh_dat <- mortality_r10_raw %>%
  filter(Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  left_join(world_cum_thresh %>%
              select(Model, Scenario, Ambition, high_cdr_only, high_re_only),
            by = c("Model","Scenario")) %>%
  filter(!is.na(high_cdr_only), high_cdr_only | high_re_only, !is.na(Ambition)) %>%
  mutate(Pathway = if_else(high_cdr_only, "High-CDR", "High-RE"),
         Group   = factor(paste0(Pathway,"\n",Ambition), levels=names(GROUP_COLS_4)),
         Region_label = REGION_LABELS_FIG[Region])

p4b_reg_thresh <- ggplot(p4b_reg_thresh_dat,
                         aes(x=Year, y=deaths_pm25/1e6,
                             group=interaction(Model,Scenario), colour=Group)) +
  geom_line(alpha=0.08, linewidth=0.3) +
  stat_summary(aes(group=Group), fun=median, geom="line",
               linewidth=1.1, na.rm=TRUE) +
  facet_grid(Region_label ~ Ambition, scales="free_y") +
  scale_colour_manual(values=GROUP_COLS_4,
                      guide=guide_legend(title="Pathway",nrow=2,
                                         override.aes=list(alpha=1,linewidth=1.1))) +
  scale_x_continuous(breaks=c(2020,2050,2080,2100)) +
  labs(title    = "P4b (regional) [THRESH]: Mortality Trajectories — Absolute Threshold",
       subtitle = "Thin = individual  ·  Bold = median  ·  Absolute threshold classification",
       x="Year", y="PM2.5 deaths (millions/yr)") +
  theme_c(8) +
  theme(strip.text.y=element_text(angle=-90, size=8))
sc(p4b_reg_thresh, "P4b_mortality_trajectory_by_region_THRESH.png", 14, 16)

# ── P5a_THRESH — equity: outcomes by region ───────────────────────────────────
p5a_reg_thresh_dat <- outcomes_nz_thresh %>%
  filter(!is.na(Region_label)) %>%
  select(Model, Scenario, Group, Region_label, all_of(names(OUTCOME_LABS_NZ))) %>%
  pivot_longer(all_of(names(OUTCOME_LABS_NZ)),
               names_to="outcome_col", values_to="val") %>%
  filter(!is.na(val), val > 0) %>%
  mutate(outcome_lab = factor(OUTCOME_LABS_NZ[outcome_col],
                              levels=unname(OUTCOME_LABS_NZ)),
         Region_label = factor(Region_label,
                               levels=unname(REGION_LABELS_FIG[regions_r10])))

p5a_reg_thresh <- ggplot(p5a_reg_thresh_dat,
                         aes(x=Group, y=val, fill=Group, colour=Group)) +
  geom_violin(alpha=0.40, colour=NA, scale="width", trim=TRUE) +
  geom_boxplot(width=0.10, outlier.size=0.3,
               colour="grey30", fill="white", alpha=0.8) +
  facet_grid(outcome_lab ~ Region_label, scales="free_y") +
  scale_fill_manual(values=GROUP_COLS_4) +
  scale_colour_manual(values=GROUP_COLS_4) +
  scale_y_continuous(labels=comma_format()) +
  labs(title    = "P5a (regional) [THRESH]: Wellbeing Outcomes by Region — Absolute Threshold",
       subtitle = "Per million 2020 population  ·  Absolute threshold classification",
       x=NULL, y=NULL) +
  theme_c(8) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        strip.text.y=element_text(angle=-90, size=7))
sc(p5a_reg_thresh, "P5a_outcomes_by_region_THRESH.png", 20, 16)

cat("\n=== THRESHOLD COMPARISON FIGURES COMPLETE ===\n")
cat("Files saved to:", FIG_COMM, "\n")
cat("Threshold figures produced (all with _THRESH suffix):\n")
for (f in c("P3a_CDR_components_by_pathway_THRESH.png",
            "P3a_CDR_components_by_region_THRESH.png",
            "P3b_electricity_mix_by_pathway_THRESH.png",
            "P3c_NOx_by_pathway_region_THRESH.png",
            "P4a_outcome_violins_15C_THRESH.png",
            "P4a_outcome_violins_2C_THRESH.png",
            "P4a_outcome_violins_combined_THRESH.png",
            "P4a_reg_outcome_violins_15C_THRESH.png",
            "P4a_reg_outcome_violins_2C_THRESH.png",
            "P4a_reg_outcome_violins_combined_THRESH.png",
            "P4b_mortality_trajectory_world_THRESH.png",
            "P4b_mortality_trajectory_by_region_THRESH.png",
            "P5a_outcomes_by_region_THRESH.png")) {
  cat("  ", f, "\n")
}

