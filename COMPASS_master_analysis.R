# =============================================================================
# COMPASS MASTER ANALYSIS — Dissertation Paper 1
# High-Renewables vs High-CDR mitigation pathways at 1.5C and 2C ambition
#
# PURPOSE
#   Compare TEN approaches to defining the final scenario sample set. The
#   High-CDR vs High-RE classification method is IDENTICAL across all ten;
#   three things vary between approaches:
#     (1) the scenario FILTER (SCI vetting),
#     (2) how AMBITION (1.5C vs 2C) is defined, and
#     (3) the classification cut: top TERCILE (A-E) or above MEDIAN (F-J,
#         the same 5 filter/ambition combinations at a looser 50% cut, which
#         roughly doubles group sizes for the feasibility-vetted approaches).
#
#   ┌──────┬────────────────────────────┬───────────────────┬───────────────┐
#   │ App. │ Scenario filter (vetting)  │ Ambition split     │ Top fraction  │
#   ├──────┼────────────────────────────┼───────────────────┼───────────────┤
#   │ A/F  │ none  (all scenarios)      │ AR6 category       │ 1/3   │  1/2  │
#   │ B/G  │ none  (all scenarios)      │ Peak warming       │ 1/3   │  1/2  │
#   │ C/H  │ full SCI vetting list      │ AR6 category       │ 1/3   │  1/2  │
#   │ D/I  │ full SCI vetting list      │ Peak warming       │ 1/3   │  1/2  │
#   │ E/J  │ partial SCI (tech-feas.)   │ AR6 category       │ 1/3   │  1/2  │
#   └──────┴────────────────────────────┴───────────────────┴───────────────┘
#   AR6 category: C1/C2 = 1.5C, C3/C4 = 2C. Peak warming: <=1.7C = 1.5C,
#   1.7-2.0C = 2C.
#
# ARCHITECTURE
#   STAGE 1 (run once):  Load data; build the R10 timeseries; compute the
#                        expensive ANNUAL outcome tables (rfasst mortality,
#                        DLE headcount/gap/implied-CO2, energy jobs) and the
#                        CDR/RE deployment metrics used for classification.
#   STAGE 2 (per approach): filter scenarios -> assign ambition -> classify
#                        High-CDR/High-RE at the approach's top fraction ->
#                        cumulate annual outcomes to the ambition window ->
#                        build that approach's df_master, with both absolute
#                        and population-normalised (per-capita) outcome
#                        columns, at R10-region AND aggregated ("World" =
#                        5-region sum) resolution. Save each approach to its
#                        own subfolder.
#   STAGE 3:             Cross-approach comparison tables (sample sizes,
#                        overlap of selected scenarios, pathway counts).
#
# POPULATION NORMALISATION
#   Denominator: fixed 2020 population (median across scenarios) per R10
#   region; the aggregate row uses the 5-region total. Per-capita columns
#   (suffix _per_1k / _pct / _pc) sit alongside the absolute columns in the
#   same df_master — nothing is dropped, both are always available.
#
# OUTPUTS (per approach X in {A..J}, under OUT_DIR/approach_X/):
#   compass_master_dataset_X.rds / .csv   one row per scenario x region x var;
#                                         absolute AND per-capita outcome cols;
#                                         Region includes R10 + "Aggregated R10
#                                         regions" (World-equivalent, pop-summed)
#   compass_pathway_tercile_X.rds / .csv  High-CDR/High-RE classification
#   compass_scenario_set_X.csv            the final scenario sample list
#   compass_cdr_cumulative_X.csv          deployment used for classification
#   These file names/objects mirror the originals so the figure scripts can
#   consume any single approach by pointing at its subfolder.
#
# CROSS-APPROACH (under OUT_DIR/comparison/):
#   approach_scenario_counts.csv          n scenarios per approach x ambition
#   approach_pathway_counts.csv           n per approach x ambition x pathway
#   approach_scenario_membership.csv      wide 0/1 membership matrix
#   approach_summary.csv                  one-line summary per approach
#
# INPUTS (unchanged from your existing pipeline):
#   compass_interp.rds              <- COMPASS_data_collection*.R
#   compass_r10_meta.csv            <- compass_pull.py   (AR6 cat + peak warming)
#   compass_mortality_summary.rds   <- COMPASS_rfasst*.R
#   compass_mortality_r10.csv       <- COMPASS_rfasst*.R (annual, for windowing)
#   job_factors_complete.csv        <- AR6 job intensity factors
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(zoo)
  library(scales)
  library(broom)
})


# =============================================================================
# SECTION 0: CONFIGURATION
# =============================================================================

# ---- 0a. Paths --------------------------------------------------------------
# Edit these to match your machine. All outputs go under OUT_DIR/<approach>.
COMPASS_DIR <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/COMPASS"
AR6_DIR     <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/AR6"
OUT_DIR     <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_master"

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- 0b. Analysis constants -------------------------------------------------
regions_r10 <- c("R10AFRICA", "R10CHINA+", "R10INDIA+",
                 "R10EUROPE", "R10NORTH_AM")
cats_keep   <- c("C1", "C2", "C3", "C4")
START_YEAR  <- 2020L

# Ambition-specific cumulation windows for wellbeing outcomes.
# These are keyed on AMBITION (not category), so they apply identically no
# matter whether ambition was assigned by AR6 category or by peak warming.
WINDOW_15C  <- 2060L   # 1.5C (High-Ambition)   group
WINDOW_2C   <- 2075L   # 2C   (Medium-Ambition) group

AMB_15C <- "1.5C (High-Ambition)"
AMB_2C  <- "2C (Medium-Ambition)"

# Default top fraction for High-CDR / High-RE classification. Each approach
# carries its own top_frac (1/3 = top tercile, 1/2 = above median); this global
# is only a fallback for calls that don't pass one.
TOP_FRAC <- 1/3

# ---- 0c. Peak-warming ambition split (approaches B, D) ----------------------
# Median peak warming thresholds (deg C):
#   1.5C (High-Ambition)   : peak warming <= WARMING_15C_MAX
#   2C   (Medium-Ambition) : WARMING_15C_MAX < peak warming <= WARMING_2C_MAX
WARMING_15C_MAX <- 1.7
WARMING_2C_MAX  <- 2.0
# Median (50th-percentile) peak-warming column in compass_r10_meta.csv.
# Confirmed column name from the COMPASS metadata (MAGICCv7.5.3). Additional
# candidates are tried in order in case of a version bump; first match wins.
PEAK_WARMING_COL_CANDIDATES <- c(
  "Climate Assessment|Peak Warming|Median [MAGICCv7.5.3]",
  "Climate Assessment|Peak Warming|Median [MAGICCv7.6.0]",
  "Climate Assessment|Peak Warming|Median"
)
PEAK_WARMING_COL <- NULL   # resolved in SECTION 1 from the candidates above

# ---- 0d. Full SCI vetting (approaches C, D) ---------------------------------
# The COMPASS metadata carries the authoritative SCI 2025 vetting flag directly.
# "full" vetting keeps scenarios whose flag is in SCI_VET_PASS.
#   Vetting|SCI 2025 values: "ok" (pass), "failed", "insufficient reporting", ""
SCI_VET_COL  <- "Vetting|SCI 2025"
SCI_VET_PASS <- c("ok")

# ---- 0e. Partial SCI vetting for approach E (technological feasibility) ------
# "Happy medium": stricter than none, looser than full SCI vetting. E keeps
# scenarios that are NOT flagged as high technological-feasibility concern on
# solar, wind, and CDR scale-up, ignoring the other SCI screens (historical
# calibration, sustainability, overall pass/fail).
#   Feasibility Concern|... columns are coded: "ok" / "medium" / "high" / "".
# Confirmed column names from the COMPASS metadata (World, 2030 horizon):
TECHFEAS_COL_CANDIDATES <- list(
  solar = c("Feasibility Concern|Solar PV Capacity|World|2030"),
  wind  = c("Feasibility Concern|Onshore Wind Capacity|World|2030",
            "Feasibility Concern|Wind Capacity|World|2030"),
  cdr   = c("Feasibility Concern|Carbon Capture|World|2030",
            "Feasibility Concern|Carbon Capture|World|2035",
            "Feasibility Concern|Carbon Capture|World|2040")
)
# A scenario FAILS a tech-feasibility flag if its value is in this set
# (case-insensitive). "high" = high feasibility concern = tech-infeasible.
# Everything else — "ok", "medium", and blank/NA (not assessed) — passes, so
# an unassessed scenario is not silently excluded. Add "medium" here to also
# exclude moderate-concern scenarios (stricter E).
TECHFEAS_FAIL_VALUES <- c("high")
# If TRUE, a scenario must pass ALL available tech-feasibility flags (i.e. no
# "high" concern on solar, wind, OR CDR); if FALSE, passing ANY one suffices.
TECHFEAS_REQUIRE_ALL <- TRUE

# Last-resort proxy (only used if NO tech-feasibility columns can be found):
# drop scenarios whose cumulative Novel CDR exceeds this percentile per ambition.
PARTIAL_NOVELCDR_PCTL <- 0.90

# ---- 0e. Approach definitions ----------------------------------------------
# vetting  : "none" | "full" | "partial"
# ambition : "ar6"  | "warming"
# top_frac : 1/3 (top tercile) | 1/2 (above median)
# A-E use the top-tercile cut; F-J are their median-split twins (top 50%),
# which roughly double the High-CDR / High-RE group sizes to give the
# feasibility-vetted approaches (C/D) more statistical power.
approaches <- tribble(
  ~id, ~label,                                                      ~vetting,  ~ambition, ~top_frac,
  "A", "All scenarios; AR6 ambition; top tercile",                 "none",    "ar6",     1/3,
  "B", "All scenarios; peak-warming ambition; top tercile",        "none",    "warming", 1/3,
  "C", "Full SCI vetting; AR6 ambition; top tercile",              "full",    "ar6",     1/3,
  "D", "Full SCI vetting; peak-warming ambition; top tercile",     "full",    "warming", 1/3,
  "E", "Partial SCI (tech-feas.); AR6 ambition; top tercile",      "partial", "ar6",     1/3,
  "F", "All scenarios; AR6 ambition; above median",                "none",    "ar6",     1/2,
  "G", "All scenarios; peak-warming ambition; above median",       "none",    "warming", 1/2,
  "H", "Full SCI vetting; AR6 ambition; above median",             "full",    "ar6",     1/2,
  "I", "Full SCI vetting; peak-warming ambition; above median",    "full",    "warming", 1/2,
  "J", "Partial SCI (tech-feas.); AR6 ambition; above median",     "partial", "ar6",     1/2
)


# =============================================================================
# SECTION 1: LOAD DATA + METADATA
# =============================================================================

cat("=== SECTION 1: Loading data ===\n")

compass_interp <- readRDS(file.path(COMPASS_DIR, "compass_interp.rds"))
cat("compass_interp scenarios:",
    n_distinct(paste(compass_interp$Model, compass_interp$Scenario)), "\n")

# ---- 1a. Metadata: AR6 category + median peak warming -----------------------
meta_path <- file.path(COMPASS_DIR, "compass_r10_meta.csv")
compass_meta <- read.csv(meta_path, stringsAsFactors = FALSE, check.names = FALSE)

# Normalise model/scenario column names in metadata
names(compass_meta)[tolower(names(compass_meta)) == "model"]    <- "Model"
names(compass_meta)[tolower(names(compass_meta)) == "scenario"] <- "Scenario"

# AR6 category column
cat_col <- names(compass_meta)[
  str_detect(names(compass_meta), "Climate Category\\|AR6 \\[Name\\]")
][1]
if (is.na(cat_col)) {
  # fall back to any column literally named/containing "AR6"
  cat_col <- names(compass_meta)[str_detect(names(compass_meta), "AR6")][1]
}
cat("AR6 category column:", cat_col, "\n")

# Helper: resolve the first candidate column that exists in the metadata,
# else auto-detect by keyword patterns.
resolve_col <- function(candidates, detect_patterns = NULL, meta = compass_meta) {
  hit <- candidates[candidates %in% names(meta)]
  if (length(hit) > 0) return(hit[1])
  if (!is.null(detect_patterns)) {
    nm <- names(meta)
    ok <- rep(TRUE, length(nm))
    for (p in detect_patterns) ok <- ok & str_detect(tolower(nm), p)
    det <- nm[ok]
    if (length(det) > 0) return(det[1])
  }
  NA_character_
}

# Peak-warming column: hardcoded candidates first, then detect median peak warming
PEAK_WARMING_COL <- resolve_col(
  PEAK_WARMING_COL_CANDIDATES,
  detect_patterns = c("peak", "warming")   # then prefer median below
)
if (!is.na(PEAK_WARMING_COL) &&
    !PEAK_WARMING_COL %in% PEAK_WARMING_COL_CANDIDATES) {
  # if we fell back to detection, prefer a median/50th variant when present
  cand <- names(compass_meta)[
    str_detect(tolower(names(compass_meta)), "peak") &
    str_detect(tolower(names(compass_meta)), "warming")
  ]
  cand_med <- cand[str_detect(tolower(cand), "median|50")]
  if (length(cand_med) > 0) PEAK_WARMING_COL <- cand_med[1]
}
cat("Peak-warming column:", PEAK_WARMING_COL, "\n")
if (is.na(PEAK_WARMING_COL)) {
  warning("No peak-warming column found. Approaches B and D (peak-warming ",
          "ambition) will fall back to AR6-category ambition. Add your exact ",
          "column name to PEAK_WARMING_COL_CANDIDATES at the top of the script.")
}

# Technological-feasibility columns (solar / wind / CDR) for approach E
TECHFEAS_COLS <- imap_chr(TECHFEAS_COL_CANDIDATES, function(cands, tech) {
  resolve_col(cands, detect_patterns = c("feasib", tech))
})
# CDR detection needs an extra try (keyword "carbon"/"removal")
if (is.na(TECHFEAS_COLS[["cdr"]])) {
  TECHFEAS_COLS[["cdr"]] <- resolve_col(
    character(0), detect_patterns = c("feasib", "carbon"))
  if (is.na(TECHFEAS_COLS[["cdr"]]))
    TECHFEAS_COLS[["cdr"]] <- resolve_col(
      character(0), detect_patterns = c("feasib", "removal"))
}
cat("Tech-feasibility columns resolved:\n")
for (tech in names(TECHFEAS_COLS))
  cat(sprintf("  %-6s -> %s\n", tech,
              ifelse(is.na(TECHFEAS_COLS[[tech]]), "(none found)",
                     TECHFEAS_COLS[[tech]])))
TECHFEAS_COLS_FOUND <- TECHFEAS_COLS[!is.na(TECHFEAS_COLS)]
if (length(TECHFEAS_COLS_FOUND) == 0)
  warning("No technological-feasibility columns found for approach E. Add your ",
          "exact column names to TECHFEAS_COL_CANDIDATES; the script will use ",
          "the novel-CDR percentile proxy in the meantime.")

# SCI 2025 vetting flag column (full vetting for approaches C, D)
SCI_VET_COL <- resolve_col(SCI_VET_COL, detect_patterns = c("vetting", "sci"))
cat("SCI vetting column:", SCI_VET_COL, "\n")

# Build a tidy metadata lookup: Model, Scenario, Category, peak_warming,
# SCI vetting flag, and the resolved tech-feasibility flags.
meta_lookup <- compass_meta %>%
  transmute(
    Model, Scenario,
    Category = case_when(
      str_detect(.data[[cat_col]], "^C1") ~ "C1",
      str_detect(.data[[cat_col]], "^C2") ~ "C2",
      str_detect(.data[[cat_col]], "^C3") ~ "C3",
      str_detect(.data[[cat_col]], "^C4") ~ "C4",
      TRUE ~ NA_character_
    ),
    peak_warming = if (!is.na(PEAK_WARMING_COL))
                     suppressWarnings(as.numeric(.data[[PEAK_WARMING_COL]]))
                   else NA_real_,
    sci_vet = if (!is.na(SCI_VET_COL)) as.character(.data[[SCI_VET_COL]])
              else NA_character_
  )

# Attach the resolved tech-feasibility flag columns (raw values) as techfeas_<tech>
for (tech in names(TECHFEAS_COLS_FOUND)) {
  col <- TECHFEAS_COLS_FOUND[[tech]]
  meta_lookup[[paste0("techfeas_", tech)]] <- as.character(compass_meta[[col]])
}
meta_lookup <- meta_lookup %>% distinct(Model, Scenario, .keep_all = TRUE)

cat("Metadata scenarios with AR6 category:",
    sum(!is.na(meta_lookup$Category)), "\n")
if (!is.na(PEAK_WARMING_COL))
  cat("Metadata scenarios with peak warming:",
      sum(!is.na(meta_lookup$peak_warming)), "\n")
if (!is.na(SCI_VET_COL)) {
  cat("SCI vetting flag distribution:\n")
  print(table(meta_lookup$sci_vet, useNA = "ifany"))
}


# =============================================================================
# SECTION 2: VETTING DEFINITIONS
# =============================================================================
# "none"    -> keep all scenarios present in compass_interp (C1-C4 universe)
# "full"    -> keep scenarios passing the SCI 2025 vetting flag (metadata
#              column `Vetting|SCI 2025` == "ok"); the hardcoded name list below
#              is only a fallback used if that column is missing.
# "partial" -> technological-feasibility screen (SECTION 0e), resolved in Stage 2.

# Fallback SCI-vetted scenario name list (used only if the metadata SCI vetting
# column is unavailable). Superseded by the `Vetting|SCI 2025` flag.
vetted_scenarios <- c(
  "SDI-2.5°C", "SDI-Baseline",
  "SSP1-19", "SSP1-26", "SSP1-34", "SSP1-45", "SSP1-Baseline", "SSP3-Baseline",
  "NGFS Phase 2-Below 2°C", "NGFS Phase 2-Current Policies",
  "NGFS Phase 2-Delayed Transition",
  "NGFS Phase 2-Nationally Determined Contributions (NDCs)",
  "NGFS Phase 5-Below 2°C", "NGFS Phase 5-Current Policies",
  "NGFS Phase 5-Delayed Transition", "NGFS Phase 5-Fragmented World",
  "NGFS Phase 5-Low Demand",
  "NGFS Phase 5-Nationally Determined Contributions (NDCs)",
  "NGFS Phase 5-Net-Zero 2050",
  "COMMIT-2°C-2030", "COMMIT-Current-Policies", "COMMIT-NDCplus",
  "ENGAGE-INDCi2030-1000", "ENGAGE-INDCi2030-1000f", "ENGAGE-INDCi2030-1200",
  "ENGAGE-INDCi2030-1200f", "ENGAGE-INDCi2030-1400", "ENGAGE-INDCi2030-1400f",
  "ENGAGE-INDCi2030-3000", "ENGAGE-INDCi2030-3000f", "ENGAGE-INDCi2030-800f",
  "ENGAGE-INDCi2100", "ENGAGE-NPi2020-1000", "ENGAGE-NPi2020-1200",
  "ENGAGE-NPi2020-1400", "ENGAGE-NPi2020-1400f", "ENGAGE-NPi2020-3000",
  "ENGAGE-NPi2020-3000f", "ENGAGE-NPi2100",
  "SSP2021-SSP1-Baseline", "SSP2021-SSP1-SPA1-19-Default",
  "SSP2021-SSP1-SPA1-19-Default-LowBiomass", "SSP2021-SSP1-SPA1-19-Lifestyle",
  "SSP2021-SSP1-SPA1-19-Lifestyle-Renewables",
  "SSP2021-SSP1-SPA1-19-Renewables",
  "SSP2021-SSP1-SPA1-19-Renewables-LowBiomass",
  "SSP2021-SSP1-SPA1-26-Default", "SSP2021-SSP1-SPA1-26-Lifestyle",
  "SSP2021-SSP1-SPA1-26-Lifestyle-Renewables",
  "SSP2021-SSP1-SPA1-26-Renewables", "SSP2021-SSP1-SPA1-34-Default",
  "SSP2021-SSP1-SPA1-34-Lifestyle",
  "SSP2021-SSP1-SPA1-34-Lifestyle-Renewables",
  "SSP2021-SSP1-SPA1-34-Renewables", "SSP2021-SSP2-Baseline",
  "SSP2021-SSP2-SPA0-26-Default",
  "SSP2021-SSP2-SPA1-19-Default-LowBiomass", "SSP2021-SSP2-SPA2-19-Default",
  "SSP2021-SSP2-SPA2-19-Lifestyle",
  "SSP2021-SSP2-SPA2-19-Lifestyle-Renewables",
  "SSP2021-SSP2-SPA2-19-Renewables", "SSP2021-SSP2-SPA2-26-Default",
  "SSP2021-SSP2-SPA2-26-Lifestyle",
  "SSP2021-SSP2-SPA2-26-Lifestyle-Renewables",
  "SSP2021-SSP2-SPA2-26-Renewables", "SSP2021-SSP2-SPA2-34-Default",
  "SSP2021-SSP2-SPA2-34-Lifestyle",
  "SSP2021-SSP2-SPA2-34-Lifestyle-Renewables",
  "SSP2021-SSP2-SPA2-34-Renewables", "SSP2021-SSP2-SPA2-45-Default",
  "SSP2021-SSP2-SPA2-45-Lifestyle",
  "SSP2021-SSP2-SPA2-45-Lifestyle-Renewables",
  "SSP2021-SSP2-SPA2-45-Renewables", "SSP2021-SSP3-Baseline",
  "SSP2021-SSP4-Baseline", "SSP2021-SSP5-Baseline", "ECEMF-DIAG-NPi",
  "NAVIGATE Demand-1.5°C-ele_u", "NAVIGATE Demand-1.5°C-tec_u",
  "NAVIGATE Demand-2.0°C-ele_u", "NAVIGATE Demand-2.0°C-ref",
  "NAVIGATE Demand-2.0°C-tec_u", "NAVIGATE Demand-NPi-ele",
  "NAVIGATE Demand-NPi-ref", "NAVIGATE Demand-NPi-tec", "ENGAGE-NoPolicy",
  "NAVIGATE Demand-2.0°C-act_u", "NAVIGATE Demand-2.0°C-all_u",
  "NAVIGATE Demand-NPi-act", "NAVIGATE Demand-NPi-all",
  "COVID-Shift-GreenPush_max_GDP", "COVID-Shift-NoPolicyNoCOVID",
  "COVID-Shift-Restore", "COVID-Shift-SelfReliance",
  "COVID-Shift-SelfReliance_max_GDP", "COVID-Shift-SmartUse",
  "COMMIT-2°C-2020", "COMMIT-Baseline", "COMMIT-Bridge",
  "COMMIT-Bridge-No-Tax", "COMMIT-GPP", "COMMIT-GPP-No-Tax",
  "COMMIT-NDC-2050-Convergence", "ENGAGE-INDCi2030-1000-COV",
  "ENGAGE-INDCi2030-1000-COV-NDCp", "ENGAGE-INDCi2030-1000-NDCp",
  "ENGAGE-INDCi2030-1000f-COV", "ENGAGE-INDCi2030-1000f-COV-NDCp",
  "ENGAGE-INDCi2030-1000f-NDCp", "ENGAGE-INDCi2030-1600",
  "ENGAGE-INDCi2030-1600f", "ENGAGE-INDCi2030-1800", "ENGAGE-INDCi2030-1800f",
  "ENGAGE-INDCi2030-2000", "ENGAGE-INDCi2030-2000f", "ENGAGE-INDCi2030-2500",
  "ENGAGE-INDCi2030-2500f", "ENGAGE-INDCi2030-300f", "ENGAGE-INDCi2030-400f",
  "ENGAGE-INDCi2030-500f", "ENGAGE-INDCi2030-600f", "ENGAGE-INDCi2030-600f-COV",
  "ENGAGE-INDCi2030-600f-COV-NDCp", "ENGAGE-INDCi2030-600f-NDCp",
  "ENGAGE-INDCi2030-700f", "ENGAGE-INDCi2030-900", "ENGAGE-INDCi2030-900f",
  "ENGAGE-INDCi2100-COV", "ENGAGE-INDCi2100-COV-NDCp", "ENGAGE-INDCi2100-NDCp",
  "ENGAGE-NPi2020-1000f", "ENGAGE-NPi2020-1000f-COV", "ENGAGE-NPi2020-1200f",
  "ENGAGE-NPi2020-1600", "ENGAGE-NPi2020-1600f", "ENGAGE-NPi2020-1800",
  "ENGAGE-NPi2020-1800f", "ENGAGE-NPi2020-2000", "ENGAGE-NPi2020-2000f",
  "ENGAGE-NPi2020-2500", "ENGAGE-NPi2020-2500f", "ENGAGE-NPi2020-900f",
  "ENGAGE-NPi2100-COV", "EMF30-BCOC-EndU", "EMF30-Baseline", "EMF30-CH4-Only",
  "EMF30-D-BCOC-Red", "EMF30-D-CH4-ClimatePolicy", "EMF30-D-Frozen-CH4",
  "EMF30-D-Frozen-EF", "EMF30-D-Frozen-EF-EndU", "EMF30-D-Frozen-EF-SLCF",
  "EMF30-SLCF", "EMF30-Slower-Action", "EMF30-Slower-to-Faster",
  "ADVANCE-NoPolicy", "ADVANCE-Reference", "CEMICS-Ref",
  "LeastTotalCost-Base-brkLR15-SSP1-P50", "LeastTotalCost-Base-brkLR15-SSP2-P50",
  "LeastTotalCost-Base-brkLR15-SSP5-P50", "LeastTotalCost-Base-brkSR15-SSP1-P50",
  "LeastTotalCost-Base-brkSR15-SSP2-P50", "LeastTotalCost-Base-brkSR15-SSP5-P50",
  "R2p1-SSP1-Baseline", "R2p1-SSP2-Baseline", "R2p1-SSP5-Baseline",
  "Rescuing-1.5°C-Highest-Possible-Ambition", "BEG-Baseline",
  "BEG-Efficiency", "CD-LINKS-No-Policy", "EMF33-Baseline",
  "EMF33-medium-2°C-cost100", "EMF33-medium-2°C-full",
  "EMF33-medium-2°C-limbio", "EMF33-medium-2°C-nofuel",
  "EMF33-tax-hi-full", "EMF33-tax-hi-none", "EMF33-tax-lo-full",
  "EMF33-tax-lo-none", "PEP-NPi", "PEP-NoPolicy", "SMP-Reference-Default",
  "SMP-Reference-Sustainable", "Diff-NoPolicy-Baseline",
  "NGFS Phase 2-Current Policies [IPD 95th]",
  "NGFS Phase 2-Current Policies [IPD Median]",
  "DeepElectrification-SSP2-Baseline", "DeepElectrification-SSP2-NPi",
  "SHAPE-SSP2-NPi", "SHAPE-SSP2-NPi [with Climate Change Impacts]",
  "RESCUE-End-of-Century-Budget-1150",
  "RESCUE-End-of-Century-Budget-1150-with-OAE",
  "RESCUE-End-of-Century-Budget-500",
  "RESCUE-End-of-Century-Budget-500-with-OAE", "RESCUE-Peak-Budget-1150",
  "RESCUE-Peak-Budget-1150-with-OAE", "ENGAGE-NPi2020-1000-COV",
  "ENGAGE-NPi2020-800", "ENGAGE-NPi2020-800f", "ENGAGE-NPi2020-900",
  "ENGAGE-INDCi2030-1200-NDCp", "ENGAGE-INDCi2030-1200f-NDCp",
  "ENGAGE-INDCi2030-1400-NDCp", "ENGAGE-INDCi2030-1400f-NDCp",
  "ENGAGE-INDCi2030-1600-NDCp", "ENGAGE-INDCi2030-1600f-NDCp",
  "ENGAGE-INDCi2030-1800-NDCp", "ENGAGE-INDCi2030-1800f-NDCp",
  "ENGAGE-INDCi2030-2000-NDCp", "ENGAGE-INDCi2030-2000f-NDCp",
  "ENGAGE-INDCi2030-2500-NDCp", "ENGAGE-INDCi2030-2500f-NDCp",
  "ENGAGE-INDCi2030-3000-NDCp", "ENGAGE-INDCi2030-3000f-NDCp",
  "ENGAGE-INDCi2030-700f-NDCp", "ENGAGE-INDCi2030-800",
  "ENGAGE-INDCi2030-800-NDCp", "ENGAGE-INDCi2030-800f-NDCp",
  "ENGAGE-INDCi2030-900-NDCp", "ENGAGE-INDCi2030-900f-NDCp"
)
vetted_scenarios <- unique(vetted_scenarios)
cat("Full SCI-vetted list size:", length(vetted_scenarios), "\n")


# =============================================================================
# SECTION 3: BUILD TIMESERIES + CDR/RE DEPLOYMENT  (run once)
# =============================================================================

cat("\n=== SECTION 3: Building timeseries + deployment ===\n")

# R10 timeseries for all scenarios in the 4 categories (no vetting yet).
compass_ts <- compass_interp %>%
  filter(Region %in% regions_r10,
         Category %in% cats_keep,
         Year >= 2015, Year <= 2100,
         !is.na(Value)) %>%
  mutate(Model_Group         = "COMPASS",
         ModelGroup_Scenario = paste("COMPASS", Scenario, sep = "_"))

# ---- 3a. CDR component timeseries (MtCO2/yr) --------------------------------
# Prefer pre-computed composite variables; otherwise build from raw components.
cdr_present <- compass_ts %>%
  filter(Variable %in% c("Novel CDR", "Fossil CCS", "Land-based CDR", "Total CDR"),
         Value > 0) %>%
  distinct(Variable) %>% pull(Variable)

sum_component <- function(vars, out_name) {
  compass_ts %>%
    filter(Variable %in% vars) %>%
    group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
             Region, Category, Year) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    mutate(Variable = out_name)
}

novel_cdr_ts <- if ("Novel CDR" %in% cdr_present)
  filter(compass_ts, Variable == "Novel CDR") else
  sum_component(c("Carbon Removal|Geological Storage|Direct Air Capture",
                  "Carbon Capture|Geological Storage|Biomass",
                  "Carbon Removal|Enhanced Weathering"), "Novel CDR")

fossil_ccs_ts <- if ("Fossil CCS" %in% cdr_present)
  filter(compass_ts, Variable == "Fossil CCS") else
  sum_component(c("Carbon Capture|Energy|Fossil",
                  "Carbon Capture|Industrial Processes"), "Fossil CCS")

land_cdr_ts <- if ("Land-based CDR" %in% cdr_present)
  filter(compass_ts, Variable == "Land-based CDR") else
  (compass_ts %>% filter(Variable == "Carbon Removal|Land Use") %>%
     mutate(Variable = "Land-based CDR"))

total_cdr_ts <- if ("Total CDR" %in% cdr_present)
  filter(compass_ts, Variable == "Total CDR") else
  bind_rows(novel_cdr_ts, fossil_ccs_ts, land_cdr_ts) %>%
    group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
             Region, Category, Year) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    mutate(Variable = "Total CDR")

# ---- 3b. Renewable capacity (GW) --------------------------------------------
# ---- What counts as "renewable" for the High-RE classification --------------
# RE_SPEC selects the definition; everything downstream (World-level metric,
# classification, all approaches) follows from it.
#
#   "renewables"  (DEFAULT) Solar, Wind, Hydro, Geothermal
#   "low_carbon"            + Nuclear
#   "with_biomass"          + Nuclear + Biomass   (the original definition)
#
# WHY BIOMASS IS EXCLUDED BY DEFAULT: biomass is the physical substrate of the
# dominant CDR technology in these scenarios. Its generating capacity counted
# toward Renewable Capacity while its captured carbon counts toward CDR
# ("Carbon Capture|Geological Storage|Biomass"), so a BECCS-heavy scenario
# scored on BOTH classification axes at once -- contaminating the very contrast
# being measured. Excluding it also removes the bioenergy air-quality signal
# (bioenergy combustion is a major BC/OC source), which is plausibly a symptom
# of the same overlap.
#
# WHY NUCLEAR IS EXCLUDED BY DEFAULT: it is low-carbon but not renewable, and
# the contrast of interest is renewables vs carbon removal. Unlike biomass it is
# non-combustion, so it introduces no air-quality or CDR overlap -- it is a
# clean definitional sensitivity rather than a correctness issue.
#
# Re-run with RE_SPEC set to each value to produce the definition-sensitivity
# table for the SI.
RE_SPEC <- "renewables"

.re_sets <- list(
  renewables   = c("Solar", "Wind", "Hydro", "Geothermal"),
  low_carbon   = c("Solar", "Wind", "Hydro", "Geothermal", "Nuclear"),
  with_biomass = c("Solar", "Wind", "Hydro", "Geothermal", "Nuclear", "Biomass")
)
if (!RE_SPEC %in% names(.re_sets))
  stop("RE_SPEC must be one of: ", paste(names(.re_sets), collapse = ", "))
re_vars <- paste0("Capacity|Electricity|", .re_sets[[RE_SPEC]])
cat("RE classification spec:", RE_SPEC, "->",
    paste(.re_sets[[RE_SPEC]], collapse = ", "), "\n")
re_total_ts <- compass_ts %>%
  filter(Variable %in% re_vars) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Category, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Variable = "Renewable Capacity")

# ---- 3c. Cumulative CDR/RE 2020-2100 (R10) for classification ---------------
cdr_cumulative_full <- bind_rows(
  novel_cdr_ts, fossil_ccs_ts, land_cdr_ts, total_cdr_ts, re_total_ts
) %>%
  filter(Region %in% regions_r10, Year >= 2020, Year <= 2100) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Category, Variable) %>%
  summarise(Total_Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  filter(Total_Value > 0) %>%
  mutate(proxy = FALSE)

# ---- 3d. World-level deployment for classification (prefer real World rows) --
world_re_cap_vars <- re_vars
world_cdr_component_vars <- c(
  "Carbon Removal|Geological Storage|Direct Air Capture",
  "Carbon Capture|Geological Storage|Biomass",
  "Carbon Removal|Enhanced Weathering",
  "Carbon Capture|Energy|Fossil",
  "Carbon Capture|Industrial Processes",
  "Carbon Removal|Land Use"
)

world_ts_raw <- compass_interp %>%
  filter(Region == "World", Category %in% cats_keep,
         Year >= 2020, Year <= 2100, !is.na(Value)) %>%
  mutate(Model_Group = "COMPASS",
         ModelGroup_Scenario = paste("COMPASS", Scenario, sep = "_"))

world_cdr_direct <- world_ts_raw %>% filter(Variable == "Total CDR", Value > 0)
world_cdr_computed <- world_ts_raw %>%
  filter(Variable %in% world_cdr_component_vars) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario, Category, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  filter(Value > 0) %>%
  mutate(Region = "World", Variable = "Total CDR")

world_total_cdr_ts <- {
  if (nrow(world_cdr_direct) > 0) world_cdr_direct
  else if (nrow(world_cdr_computed) > 0) world_cdr_computed
  else tibble()
}

# Also keep world Novel CDR (for the partial-vetting proxy in approach E)
world_novel_cdr_ts <- world_ts_raw %>%
  filter(Variable %in% c("Carbon Removal|Geological Storage|Direct Air Capture",
                         "Carbon Capture|Geological Storage|Biomass",
                         "Carbon Removal|Enhanced Weathering")) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario, Category, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = "World", Variable = "Novel CDR")

world_re_ts <- world_ts_raw %>%
  filter(Variable %in% world_re_cap_vars, Value > 0) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario, Category, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = "World", Variable = "Renewable Capacity")

world_cumulative_direct <- bind_rows(world_total_cdr_ts, world_re_ts,
                                     world_novel_cdr_ts) %>%
  filter(Year >= 2020, Year <= 2100) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario, Category, Variable) %>%
  summarise(Total_Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  filter(Total_Value > 0)

# Summed-R10 fallback
world_cumulative_sumR10 <- cdr_cumulative_full %>%
  filter(Variable %in% c("Total CDR", "Renewable Capacity")) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario, Category, Variable) %>%
  summarise(Total_Value = sum(Total_Value, na.rm = TRUE), .groups = "drop") %>%
  filter(Total_Value > 0)

n_r10_scens <- n_distinct(paste(compass_ts$Model, compass_ts$Scenario))
n_world_cdr <- world_cumulative_direct %>% filter(Variable == "Total CDR") %>%
  { n_distinct(paste(.$Model, .$Scenario)) }
coverage_pct <- if (n_r10_scens > 0) 100 * n_world_cdr / n_r10_scens else 0
USE_WORLD_REGION <- coverage_pct >= 50
cat(sprintf("World-region CDR coverage: %.1f%% -> use World rows: %s\n",
            coverage_pct, USE_WORLD_REGION))

# Deployment table used for classification: one row per Model x Scenario x Variable
if (USE_WORLD_REGION) {
  deploy_metrics <- world_cumulative_sumR10 %>%
    select(Model, Scenario, Category, Variable, total_sumR10 = Total_Value) %>%
    full_join(world_cumulative_direct %>%
                select(Model, Scenario, Category, Variable, total_world = Total_Value),
              by = c("Model", "Scenario", "Category", "Variable")) %>%
    mutate(Total_Value = coalesce(total_world, total_sumR10)) %>%
    select(Model, Scenario, Category, Variable, Total_Value)
  # add Novel CDR (world) explicitly for approach E proxy
  deploy_metrics <- bind_rows(
    deploy_metrics,
    world_cumulative_direct %>%
      filter(Variable == "Novel CDR") %>%
      select(Model, Scenario, Category, Variable, Total_Value)
  ) %>% distinct(Model, Scenario, Variable, .keep_all = TRUE)
} else {
  novel_sumR10 <- cdr_cumulative_full %>%
    filter(Variable == "Novel CDR") %>%
    group_by(Model, Scenario, Category, Variable) %>%
    summarise(Total_Value = sum(Total_Value, na.rm = TRUE), .groups = "drop")
  deploy_metrics <- bind_rows(
    world_cumulative_sumR10 %>%
      select(Model, Scenario, Category, Variable, Total_Value),
    novel_sumR10
  ) %>% distinct(Model, Scenario, Variable, .keep_all = TRUE)
}

cat("deploy_metrics rows:", nrow(deploy_metrics),
    "| scenarios:", n_distinct(paste(deploy_metrics$Model, deploy_metrics$Scenario)), "\n")


# =============================================================================
# SECTION 4: ANNUAL WELLBEING OUTCOMES  (run once)
# =============================================================================
# Computes annual (per-year) outcome tables so that each approach can cumulate
# to its own ambition window cheaply in Stage 2:
#   mortality_annual  : deaths per year (from rfasst outputs)
#   dle_annual        : DLE gap, headcount, implied CO2 per year
#   jobs_annual       : RE / fossil jobs (thousands) per year
# =============================================================================

cat("\n=== SECTION 4: Annual wellbeing outcomes ===\n")

# ---- 4a. Air-pollution mortality (annual) -----------------------------------
# The rfasst script writes compass_mortality_r10.csv to its own output folder
# (Outputs/COMPASS_mortality) AND to COMPASS_DIR. Look in both and use the most
# recently modified, so a stale copy in COMPASS_DIR can't silently win. Add
# more candidate paths here if your rfasst OUT_DIR differs.
MORT_R10_CANDIDATES <- c(
  file.path(COMPASS_DIR, "compass_mortality_r10.csv"),
  file.path(dirname(COMPASS_DIR), "..", "Outputs", "COMPASS_mortality",
            "compass_mortality_r10.csv"),
  "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_mortality/compass_mortality_r10.csv"
)
mort_found <- MORT_R10_CANDIDATES[file.exists(MORT_R10_CANDIDATES)]
mort_r10_path <- if (length(mort_found) > 0)
  mort_found[which.max(file.mtime(mort_found))] else MORT_R10_CANDIDATES[1]
mortality_annual <- NULL
if (file.exists(mort_r10_path)) {
  cat("Reading annual mortality from:", mort_r10_path,
      "(modified", format(file.mtime(mort_r10_path)), ")\n")
  ma <- read_csv(mort_r10_path, show_col_types = FALSE)
  if ("model" %in% names(ma) && !"Model" %in% names(ma))
    ma <- rename(ma, Model = model, Scenario = scenario)
  if ("year" %in% names(ma) && !"Year" %in% names(ma))
    ma <- rename(ma, Year = year)
  if ("r10_region" %in% names(ma) && !"Region" %in% names(ma))
    ma <- rename(ma, Region = r10_region)
  ma <- ma %>% select(-any_of(c("model", "scenario", "year", "r10_region")))
  deaths_col <- intersect(names(ma), c("deaths_pm25", "FUSION", "deaths_total"))[1]
  cat("Mortality annual column:", deaths_col, "\n")
  mortality_annual <- ma %>%
    filter(Region %in% regions_r10) %>%
    transmute(Model, Scenario, Region, Year = as.integer(Year),
              deaths_annual = .data[[deaths_col]])
} else {
  warning("compass_mortality_r10.csv not found; will fall back to scaled ",
          "cumulative summary for mortality.")
  ms <- readRDS(file.path(COMPASS_DIR, "compass_mortality_summary.rds"))
  if ("model" %in% names(ms))      ms <- rename(ms, Model = model, Scenario = scenario)
  if ("r10_region" %in% names(ms)) ms <- rename(ms, Region = r10_region)
  ms <- ms %>% select(-any_of(c("model", "scenario", "r10_region")))
  mort_col <- intersect(names(ms),
                        c("cumulative_deaths_mln_pm25", "cumulative_deaths_mln"))[1]
  # crude annualisation: spread cumulative evenly across 2020-2100
  mortality_annual <- ms %>%
    filter(Region %in% regions_r10) %>%
    transmute(Model, Scenario, Region,
              cum_deaths_mln = .data[[mort_col]]) %>%
    crossing(Year = seq(2020, 2100, 5)) %>%
    mutate(deaths_annual = cum_deaths_mln * 1e6 / 17) %>%   # 17 five-year steps
    select(Model, Scenario, Region, Year, deaths_annual)
}

# ---- 4b. Energy jobs (annual) -----------------------------------------------
job_factors_complete <- read.csv(file.path(AR6_DIR, "job_factors_complete.csv"))

# ---- Geothermal job factors (absent from the source table) ------------------
# job_factors_complete carries no geothermal factor at all, so geothermal
# capacity was silently dropped from the jobs calc (~7% of GW-rows), which
# understates RENEWABLE jobs. Add geothermal for every phase used by the total-
# employment calc below (construction, manufacturing, O&M; geothermal has no
# fuel so extraction/refinery stay 0). Method: anchor the most-cited global
# geothermal employment factors -- O&M 1.17 jobs/MW = 1170 jobs/GW; construction
# 3.1 = 3100; manufacturing 3.3 = 3300 (Geothermal Energy Assoc. 2015 / GEA
# geothermal-econ) -- and distribute each with ONE technology-independent
# regional labour multiplier derived from the table's own O&M factors (geometric
# mean across low-carbon generation techs). This mirrors the Rutovitz et al.
# (2015) structure (global employment factor x regional labour multiplier) and
# avoids importing any single technology's regional quirks (e.g. the China hydro
# O&M spike, or biomass's fuel-driven pattern).
if (!any(job_factors_complete$fuel == "geothermal")) {
  .geo_global <- tribble(~category,        ~ef_global,
                         "oem",            1170,
                         "construction",   3100,
                         "manufacturing",  3300)
  .donors <- c("biomass", "hydro", "wind_on", "solar_pv", "nuclear")
  .gm <- function(x) exp(mean(log(x)))
  .region_mult <- job_factors_complete %>%
    filter(category == "oem", fuel %in% .donors,
           region %in% regions_r10, job_intensity > 0) %>%
    group_by(fuel) %>%
    mutate(ratio = job_intensity / .gm(job_intensity)) %>%
    group_by(region) %>%
    summarise(mult = .gm(ratio), .groups = "drop")
  geothermal_factors <- expand_grid(region = regions_r10,
                                    category = .geo_global$category) %>%
    left_join(.geo_global, by = "category") %>%
    left_join(.region_mult, by = "region") %>%
    transmute(region, fuel = "geothermal", category,
              job_intensity = ef_global * mult)
  job_factors_complete <- bind_rows(job_factors_complete, geothermal_factors)
  cat("Added geothermal job factors (jobs/GW) by phase:\n")
  print(as.data.frame(geothermal_factors %>%
                        pivot_wider(names_from = category, values_from = job_intensity) %>%
                        mutate(across(where(is.numeric), ~round(.x, 0)))))
}

# Jobs are grouped FOUR ways -- Renewables / Nuclear / Bioenergy / Fossil --
# rather than the previous two. Nuclear and bioenergy are reported separately so
# the jobs grouping does not have to be re-litigated whenever the RE_SPEC
# classification changes, and so the (large) bioenergy employment block is
# visible instead of being absorbed into "Renewables". Biomass carries the
# highest employment factors of any technology (build ~20,400 jobs/GW, O&M
# ~2,840 jobs/GW), so where it sits materially moves the totals.
# jobs_re_group() aggregates these to match the active RE_SPEC.
cap_additions_fuel_map <- tribble(
  ~Variable,                                    ~fuel,        ~tech_group,
  "Capacity Additions|Electricity|Solar",       "solar_pv",   "Renewables",
  "Capacity Additions|Electricity|Wind",        "wind_on",    "Renewables",
  "Capacity Additions|Electricity|Hydro",       "hydro",      "Renewables",
  "Capacity Additions|Electricity|Geothermal",  "geothermal", "Renewables",
  "Capacity Additions|Electricity|Nuclear",     "nuclear",    "Nuclear",
  "Capacity Additions|Electricity|Biomass",     "biomass",    "Bioenergy",
  "Capacity Additions|Electricity|Coal",        "coal",       "Fossil",
  "Capacity Additions|Electricity|Gas",         "gas",        "Fossil",
  "Capacity Additions|Electricity|Oil",         "oil",        "Fossil"
)
cap_stock_fuel_map <- tribble(
  ~Variable,                          ~fuel,        ~tech_group,
  "Capacity|Electricity|Solar",       "solar_pv",   "Renewables",
  "Capacity|Electricity|Wind",        "wind_on",    "Renewables",
  "Capacity|Electricity|Hydro",       "hydro",      "Renewables",
  "Capacity|Electricity|Geothermal",  "geothermal", "Renewables",
  "Capacity|Electricity|Nuclear",     "nuclear",    "Nuclear",
  "Capacity|Electricity|Biomass",     "biomass",    "Bioenergy",
  "Capacity|Electricity|Coal",        "coal",       "Fossil",
  "Capacity|Electricity|Gas",         "gas",        "Fossil",
  "Capacity|Electricity|Oil",         "oil",        "Fossil"
)

# Which job groups count as "renewable" for the ACTIVE classification spec, so
# jobs_Renewables always matches how High-RE was defined.
jobs_re_group <- switch(RE_SPEC,
  renewables   = c("Renewables"),
  low_carbon   = c("Renewables", "Nuclear"),
  with_biomass = c("Renewables", "Nuclear", "Bioenergy"))
cat("Jobs counted as renewable under RE_SPEC='", RE_SPEC, "': ",
    paste(jobs_re_group, collapse = ", "), "\n", sep = "")

cap_additions_ts <- compass_ts %>% filter(Variable %in% cap_additions_fuel_map$Variable)

scens_with_additions <- cap_additions_ts %>%
  filter(Value > 0, Year >= START_YEAR) %>%
  distinct(Model, Scenario, Region)
scens_needing_stockdiff <- compass_ts %>%
  filter(Variable %in% cap_stock_fuel_map$Variable, Year >= START_YEAR) %>%
  distinct(Model, Scenario, Region) %>%
  anti_join(scens_with_additions, by = c("Model", "Scenario", "Region"))

# TOTAL energy-sector employment (Rutovitz-style). The three job streams have
# DIFFERENT dimensional drivers, so they are applied to different quantities:
#   * build  (construction + manufacturing) -> ONE-TIME per GW ADDED   (a flow)
#   * O&M + fuel (oem + extraction + refinery) -> ONGOING per GW INSTALLED (stock)
# The previous version applied only the O&M factor to additions -- an ongoing
# per-stock rate multiplied by a one-time flow -- which is dimensionally wrong
# and captured neither build employment nor true O&M/fuel employment. See the
# methods note. ASSUMPTION: fuel/extraction employment is proxied per installed
# GW (constant capacity-factor approximation), consistent with the per-GW
# encoding of job_factors_complete; construction/manufacturing are per GW built.
job_ef <- job_factors_complete %>%
  filter(category %in% c("construction", "manufacturing",
                         "oem", "extraction", "refinery")) %>%
  pivot_wider(names_from = category, values_from = job_intensity,
              values_fn = mean, values_fill = 0) %>%
  mutate(build_ef   = construction + manufacturing,   # per GW added  (one-time)
         ongoing_ef = oem + extraction + refinery) %>% # per GW stock (per year)
  select(region, fuel, build_ef, ongoing_ef)

# ---- installed CAPACITY STOCK by tech -> drives O&M + fuel (ongoing) --------
stock_ts <- compass_ts %>%
  filter(Variable %in% cap_stock_fuel_map$Variable, Year >= START_YEAR) %>%
  inner_join(cap_stock_fuel_map, by = "Variable") %>%
  transmute(Model, Scenario, Region, Category, Year, fuel, tech_group,
            stock_GW = pmax(0, Value))

# ---- capacity ADDITIONS by tech -> drives build (one-time) ------------------
# Direct where reported; else implied from the annual stock change (year-gap,
# = 1 on the annual grid; see the fallback-divisor fix note).
add_direct <- cap_additions_ts %>%
  semi_join(scens_with_additions, by = c("Model", "Scenario", "Region")) %>%
  filter(Year >= START_YEAR) %>%
  inner_join(cap_additions_fuel_map, by = "Variable") %>%
  transmute(Model, Scenario, Region, Category, Year, fuel, tech_group,
            add_GW = pmax(0, Value))
add_fallback <- compass_ts %>%
  filter(Variable %in% cap_stock_fuel_map$Variable, Year >= START_YEAR) %>%
  semi_join(scens_needing_stockdiff, by = c("Model", "Scenario", "Region")) %>%
  inner_join(cap_stock_fuel_map, by = "Variable") %>%
  arrange(Model, Scenario, Region, fuel, Year) %>%
  group_by(Model, Scenario, Region, Category, fuel, tech_group) %>%
  mutate(add_GW = pmax(0, (Value - lag(Value)) / (Year - lag(Year)))) %>%
  ungroup() %>%
  filter(!is.na(add_GW)) %>%
  transmute(Model, Scenario, Region, Category, Year, fuel, tech_group, add_GW)
additions_ts <- bind_rows(add_direct, add_fallback)

# ---- three streams -> total jobs per Model/Scenario/Region/Year/tech_group --
jobs_build <- additions_ts %>%
  left_join(job_ef, by = c("Region" = "region", "fuel")) %>%
  transmute(Model, Scenario, Region, Category, Year, tech_group,
            jobs_thousands = add_GW * build_ef / 1000)
jobs_ongoing <- stock_ts %>%
  left_join(job_ef, by = c("Region" = "region", "fuel")) %>%
  transmute(Model, Scenario, Region, Category, Year, tech_group,
            jobs_thousands = stock_GW * ongoing_ef / 1000)

jobs_annual <- bind_rows(jobs_build, jobs_ongoing) %>%
  filter(!is.na(jobs_thousands)) %>%
  group_by(Model, Scenario, Region, Category, Year, tech_group) %>%
  summarise(jobs_thousands = sum(jobs_thousands, na.rm = TRUE), .groups = "drop")

# ---- 4c. DLE gap / headcount / implied CO2 (annual) -------------------------
# DLE FIX 1 (see dle_fix.R / methods note): decent-living final-energy
# thresholds (GJ/capita/yr).
#
# SOURCED (default): regional totals read from Kikstra et al. 2021, ERL 16
# 095006, figure 1A ("Threshold for DLE", black lines), mapped from the IIASA
# regions to our five R10 regions:
#     R10NORTH_AM <- NAM 37 | R10EUROPE <- WEU/EEU 28 | R10CHINA+ <- CPA 15
#     R10INDIA+   <- SAS 10 | R10AFRICA <- AFR 17      (global mean 17 GJ/cap)
# Sector split uses DESIRE's published global shares (Kikstra et al. 2025:
# res_comm 6.7, transport 11.8, industry 3.8 GJ/cap -> 30.0 / 52.9 / 17.0 %),
# which keeps transport the largest component in every region.
#
# This REPLACES an earlier interpolated table (18.0/19.0/22.0/25.5/34.5) that we
# had placed inside DESIRE's per-sector ranges by climate/settlement judgement.
# That interpolation compressed the true regional spread badly: published
# NAM:SAS is ~3.7x, our interpolation was only 1.9x. Net effect of the switch:
# INDIA+ -44%, CHINA+ -32%, AFRICA -11%, EUROPE +10%, NORTH_AM +7%.
#
# NOTE ON VINTAGE: Kikstra 2021 puts the global mean DLE threshold at 17
# GJ/cap; DESIRE (2025) reports 22.3 [17-35]. We use the 2021 values directly
# because they are the published *regional* thresholds. Set DLE_RESCALE_TO_DESIRE
# to TRUE to keep the 2021 regional pattern but rescale its level to DESIRE's
# 22.3 global mean (x 1.31).
DLE_RESCALE_TO_DESIRE <- FALSE
.dle_totals <- c("R10INDIA+" = 10, "R10AFRICA" = 17, "R10CHINA+" = 15,
                 "R10EUROPE" = 28, "R10NORTH_AM" = 37)          # Kikstra 2021 fig 1A
if (DLE_RESCALE_TO_DESIRE) .dle_totals <- .dle_totals * (22.3 / 17)
.dle_shares <- c(res_comm = 6.7, transport = 11.8, industry = 3.8)
.dle_shares <- .dle_shares / sum(.dle_shares)                    # DESIRE sector split
dle_thresholds <- tibble(
  Region        = names(.dle_totals),
  res_comm_GJ   = round(unname(.dle_totals) * .dle_shares[["res_comm"]],  2),
  transport_GJ  = round(unname(.dle_totals) * .dle_shares[["transport"]], 2),
  industry_GJ   = round(unname(.dle_totals) * .dle_shares[["industry"]],  2)
)
cat("DLE thresholds (GJ/cap/yr), source = Kikstra et al. 2021 fig 1A",
    if (DLE_RESCALE_TO_DESIRE) "rescaled to DESIRE 22.3\n" else "(global mean 17)\n")
print(as.data.frame(dle_thresholds %>%
        mutate(total = res_comm_GJ + transport_GJ + industry_GJ)))
# DLE FIX 2 (see dle_fix.R): steepen provisioning-efficiency to match DESIRE's
# ~-30% to -46% by 2040. 1.9%/yr lands at ~-38% by 2040, then holds a floor.
# (was sector-specific 1.0-1.5%/yr, giving only ~-24% by 2040.)
SEF_RATE  <- 0.019
SEF_FLOOR <- 0.5
sef_lookup <- expand_grid(
  Year = unique(compass_ts$Year),
  sector = c("res_comm", "industry", "transport")
) %>%
  mutate(SEF = pmax(SEF_FLOOR, 1 - SEF_RATE * (Year - 2020)))
dle_thresholds_total <- dle_thresholds %>%
  mutate(total_GJ = res_comm_GJ + industry_GJ + transport_GJ) %>%
  select(Region, total_GJ)
sef_total <- tibble(Year = unique(compass_ts$Year)) %>%
  mutate(SEF_total = pmax(SEF_FLOOR, 1 - SEF_RATE * (Year - 2020)))
dle_thresh_long <- dle_thresholds %>%
  pivot_longer(c(res_comm_GJ, industry_GJ, transport_GJ),
               names_to = "sector", values_to = "threshold_GJ_base") %>%
  mutate(sector = str_remove(sector, "_GJ"))

energy_ts <- compass_ts %>%
  filter(Variable %in% c("Final Energy", "Final Energy|Industry",
                         "Final Energy|Transportation"))
pop_ts <- compass_ts %>% filter(Variable == "Population")

fe_total_r10 <- energy_ts %>%
  filter(Variable == "Final Energy", Region %in% regions_r10) %>%
  select(Model_Group, Model, Scenario, ModelGroup_Scenario,
         Region, Year, Category, fe_total = Value)
fe_sectors_r10 <- energy_ts %>%
  filter(Variable %in% c("Final Energy|Industry", "Final Energy|Transportation"),
         Region %in% regions_r10) %>%
  mutate(sector = if_else(Variable == "Final Energy|Industry",
                          "industry", "transport")) %>%
  select(Model_Group, Model, Scenario, ModelGroup_Scenario,
         Region, Year, Category, sector, energy_EJ = Value)
fe_wide <- fe_sectors_r10 %>%
  pivot_wider(names_from = sector, values_from = energy_EJ) %>%
  left_join(fe_total_r10, by = c("Model_Group", "Model", "Scenario",
                                 "ModelGroup_Scenario", "Region", "Year", "Category")) %>%
  mutate(res_comm = pmax(fe_total - coalesce(industry, 0) - coalesce(transport, 0), 0))
energy_by_sector <- fe_wide %>%
  select(Model_Group, Model, Scenario, ModelGroup_Scenario,
         Region, Year, Category, industry, transport, res_comm) %>%
  pivot_longer(c(res_comm, industry, transport),
               names_to = "sector", values_to = "energy_EJ") %>%
  filter(!is.na(energy_EJ))
pop_r10 <- pop_ts %>%
  filter(Region %in% regions_r10) %>%
  select(Model_Group, Model, Scenario, ModelGroup_Scenario,
         Region, Year, Category, pop_millions = Value)

# 3-sector track
evt_3s <- energy_by_sector %>%
  left_join(pop_r10, by = c("Model_Group", "Model", "Scenario",
                            "ModelGroup_Scenario", "Region", "Year", "Category")) %>%
  left_join(dle_thresh_long, by = c("Region", "sector")) %>%
  left_join(sef_lookup, by = c("Year", "sector")) %>%
  filter(!is.na(pop_millions), pop_millions > 0,
         !is.na(threshold_GJ_base), !is.na(energy_EJ)) %>%
  mutate(energy_GJ_pc = (energy_EJ * 1e9) / (pop_millions * 1e6),
         threshold_GJ_pc = threshold_GJ_base * SEF,
         gap_GJ_pc = pmax(0, threshold_GJ_pc - energy_GJ_pc),
         gap_EJ_total = gap_GJ_pc * (pop_millions * 1e6) / 1e9)
complete_3s <- evt_3s %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Year, Category) %>%
  summarise(n = n_distinct(sector), .groups = "drop") %>%
  filter(n == 3) %>% select(-n)
evt_3s <- semi_join(evt_3s, complete_3s,
                    by = c("Model_Group", "Model", "Scenario",
                           "ModelGroup_Scenario", "Region", "Year", "Category"))

# 1-sector fallback
evt_1s <- fe_total_r10 %>%
  anti_join(complete_3s, by = c("Model_Group", "Model", "Scenario",
                                "ModelGroup_Scenario", "Region", "Year", "Category")) %>%
  left_join(pop_r10, by = c("Model_Group", "Model", "Scenario",
                            "ModelGroup_Scenario", "Region", "Year", "Category")) %>%
  left_join(dle_thresholds_total, by = "Region") %>%
  left_join(sef_total, by = "Year") %>%
  filter(!is.na(pop_millions), pop_millions > 0, !is.na(fe_total), !is.na(total_GJ)) %>%
  mutate(sector = "total",
         energy_GJ_pc = (fe_total * 1e9) / (pop_millions * 1e6),
         threshold_GJ_pc = total_GJ * SEF_total,
         gap_GJ_pc = pmax(0, threshold_GJ_pc - energy_GJ_pc),
         gap_EJ_total = gap_GJ_pc * (pop_millions * 1e6) / 1e9)

evt <- bind_rows(
  evt_3s %>% select(Model_Group, Model, Scenario, ModelGroup_Scenario,
                    Region, Year, Category, pop_millions, sector,
                    energy_GJ_pc, threshold_GJ_pc, gap_GJ_pc, gap_EJ_total),
  evt_1s %>% select(Model_Group, Model, Scenario, ModelGroup_Scenario,
                    Region, Year, Category, pop_millions, sector,
                    energy_GJ_pc, threshold_GJ_pc, gap_GJ_pc, gap_EJ_total)
)

energy_gini <- tribble(
  ~Region,        ~gini,
  "R10AFRICA",    0.45, "R10CHINA+", 0.38, "R10EUROPE", 0.25,
  "R10INDIA+",    0.42, "R10NORTH_AM", 0.28
) %>% mutate(sigma_ln = sqrt(2) * qnorm((gini + 1) / 2))

# DLE FIX 3 (see dle_fix.R): compute the energy GAP distributionally on the SAME
# region-total lognormal as the headcount, instead of the old mean-based
# (threshold - mean) shortfall. The old gap read ZERO whenever mean >= threshold
# (ignoring the poor tail) and was inconsistent with the headcount. The DESIRE gap
# is the energy to lift everyone below the threshold up to it = partial
# expectation of (threshold - energy) over the below-threshold tail:
#   m  = ln(E) - s^2/2 ;  d1 = (ln(T) - m)/s ;  d2 = d1 - s
#   headcount rate = Phi(d1)             (unchanged)
#   gap per capita = T*Phi(d1) - E*Phi(d2)
dle_headcount_annual <- evt %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Year, Category, pop_millions) %>%
  summarise(energy_GJ_pc_total = sum(energy_GJ_pc, na.rm = TRUE),
            threshold_GJ_pc_total = sum(threshold_GJ_pc, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(energy_gini, by = "Region") %>%     # sigma_ln from energy Gini
  mutate(s  = sigma_ln,
         mu_ln = log(pmax(energy_GJ_pc_total, 0.01)) - s^2 / 2,
         d1 = (log(pmax(threshold_GJ_pc_total, 0.01)) - mu_ln) / s,
         d2 = d1 - s,
         deprivation_rate  = pnorm(d1),          # headcount (unchanged)
         headcount_millions = deprivation_rate * pop_millions,
         gap_GJ_pc = pmax(0, threshold_GJ_pc_total * pnorm(d1) -
                             energy_GJ_pc_total   * pnorm(d2)),   # distributional
         gap_EJ_total = gap_GJ_pc * (pop_millions * 1e6) / 1e9)

# implied CO2 (annual)
emissions_intensity <- compass_ts %>%
  filter(Variable %in% c("Emissions|CO2|Energy", "Final Energy"),
         Region %in% regions_r10) %>%
  pivot_wider(id_cols = c(Model_Group, Model, Scenario, ModelGroup_Scenario,
                          Region, Year, Category),
              names_from = Variable, values_from = Value, values_fn = mean) %>%
  rename(any_of(c(co2_energy = "Emissions|CO2|Energy", fe_EJ = "Final Energy"))) %>%
  filter(!is.na(co2_energy), !is.na(fe_EJ), fe_EJ > 0) %>%
  mutate(ei_MtCO2_per_EJ = (co2_energy * 1000) / fe_EJ)

dle_annual <- dle_headcount_annual %>%
  left_join(emissions_intensity, by = c("Model_Group", "Model", "Scenario",
                                        "ModelGroup_Scenario", "Region", "Year", "Category")) %>%
  mutate(implied_CO2_GtCO2 = gap_EJ_total * ei_MtCO2_per_EJ / 1000) %>%
  select(Model, Scenario, Region, Category, Year,
         gap_EJ_total, headcount_millions, implied_CO2_GtCO2)

cat("Annual outcome tables built:\n")
cat("  mortality_annual rows:", nrow(mortality_annual), "\n")
cat("  jobs_annual rows:     ", nrow(jobs_annual), "\n")
cat("  dle_annual rows:      ", nrow(dle_annual), "\n")

# ---- 4d. Mortality coverage ceiling (diagnostic) ----------------------------
# How many CLASSIFIED (deployment) scenarios actually have mortality output?
# This is the ceiling on mortality coverage regardless of vetting: mortality
# exists only for scenarios present in the rfasst run (i.e. with emissions data).
# If this is well below 100%, re-running COMPASS_rfasst_full.R will NOT reach
# full coverage — the gap is emissions-data availability, not the vetted filter.
{
  deploy_scen <- deploy_metrics %>% distinct(Model, Scenario)
  mort_scen   <- mortality_annual %>% distinct(Model, Scenario)
  n_deploy <- nrow(deploy_scen)
  n_both   <- deploy_scen %>% semi_join(mort_scen, by = c("Model", "Scenario")) %>% nrow()
  cat(sprintf(
    "  MORTALITY COVERAGE CEILING: %d / %d classified scenarios have mortality (%.0f%%)\n",
    n_both, n_deploy, if (n_deploy > 0) 100 * n_both / n_deploy else 0))
  cat(sprintf("    (rfasst produced mortality for %d scenarios total)\n",
              nrow(mort_scen)))
  if (n_deploy > 0 && n_both / n_deploy < 0.9)
    cat("    -> Coverage < 90%: gap is emissions-data availability, not vetting.\n",
        "      Check that compass_emissions_raw.csv covers the full scenario set.\n")
}


# =============================================================================
# SECTION 5: STAGE-2 FUNCTIONS  (filter -> ambition -> classify -> outcomes)
# =============================================================================

cat("\n=== SECTION 5: Defining per-approach functions ===\n")

window_for_ambition <- function(amb) {
  case_when(amb == AMB_15C ~ WINDOW_15C,
            amb == AMB_2C  ~ WINDOW_2C,
            TRUE ~ NA_integer_)
}

# ---- 5.0 Population for per-capita normalisation -----------------------------
# Fixed 2020 population per R10 region (median across scenarios), matching the
# denominator used in P_new_outcome_figures. pop2020_total is the 5-region sum,
# used to normalise the aggregate ("World") row.
pop2020_r10 <- pop_ts %>%
  filter(Region %in% regions_r10, Year == 2020) %>%
  group_by(Region) %>%
  summarise(pop_mln = median(Value, na.rm = TRUE), .groups = "drop")
pop2020_total <- sum(pop2020_r10$pop_mln, na.rm = TRUE)
cat("2020 population (mln) by region:\n"); print(as.data.frame(pop2020_r10))
cat("5-region total (mln):", round(pop2020_total, 0), "\n")

# Add population-normalised outcome columns given a `pop_mln` column already
# joined on. Units: deaths & jobs per 1,000 people; headcount as % of pop;
# DLE gap in GJ/capita; implied CO2 in tCO2/capita. Absolute columns are kept.
add_percapita <- function(df) {
  df %>% mutate(
    mort_per_1k        = if ("cumulative_deaths_mln"        %in% names(.)) cumulative_deaths_mln        / pop_mln * 1000 else NA_real_,
    headcount_pct      = if ("mean_headcount_millions"      %in% names(.)) mean_headcount_millions      / pop_mln * 100  else NA_real_,
    re_jobs_per_1k     = if ("jobs_Renewables"              %in% names(.)) jobs_Renewables              / pop_mln       else NA_real_,
    fossil_jobs_per_1k = if ("jobs_Fossil"                  %in% names(.)) jobs_Fossil                  / pop_mln       else NA_real_,
    net_re_jobs_per_1k = if (all(c("jobs_Renewables","jobs_Fossil") %in% names(.))) (jobs_Renewables - jobs_Fossil) / pop_mln else NA_real_,
    gap_GJ_pc          = if ("cumulative_gap_EJ"            %in% names(.)) cumulative_gap_EJ            * 1000 / pop_mln else NA_real_,
    implied_CO2_tpc    = if ("cumulative_implied_CO2_GtCO2" %in% names(.)) cumulative_implied_CO2_GtCO2 * 1000 / pop_mln else NA_real_
  )
}

# ---- 5a. Ambition assignment -------------------------------------------------
assign_ambition <- function(df, method) {
  if (method == "ar6") {
    df %>% left_join(meta_lookup %>% select(Model, Scenario, Category),
                     by = c("Model", "Scenario"),
                     suffix = c("", ".meta")) %>%
      mutate(Category = coalesce(Category, Category.meta)) %>%
      select(-any_of("Category.meta")) %>%
      mutate(Ambition = case_when(
        Category %in% c("C1", "C2") ~ AMB_15C,
        Category %in% c("C3", "C4") ~ AMB_2C,
        TRUE ~ NA_character_))
  } else if (method == "warming") {
    if (is.na(PEAK_WARMING_COL)) {
      # graceful fallback to AR6 if peak warming unavailable
      return(assign_ambition(df, "ar6"))
    }
    df %>% left_join(meta_lookup %>% select(Model, Scenario, peak_warming),
                     by = c("Model", "Scenario")) %>%
      mutate(Ambition = case_when(
        !is.na(peak_warming) & peak_warming <= WARMING_15C_MAX ~ AMB_15C,
        !is.na(peak_warming) & peak_warming >  WARMING_15C_MAX &
          peak_warming <= WARMING_2C_MAX                       ~ AMB_2C,
        TRUE ~ NA_character_))
  } else stop("Unknown ambition method: ", method)
}

# ---- 5b. Scenario selection (vetting) ---------------------------------------
# Returns a distinct Model x Scenario tibble of the surviving sample.
select_scenarios <- function(vetting) {
  all_scens <- deploy_metrics %>% distinct(Model, Scenario, Category)
  if (vetting == "none") {
    return(all_scens %>% select(Model, Scenario))
  }
  if (vetting == "full") {
    # Primary: authoritative SCI 2025 vetting flag from metadata.
    if (!is.na(SCI_VET_COL)) {
      pass_set <- tolower(trimws(as.character(SCI_VET_PASS)))
      keep <- meta_lookup %>%
        filter(tolower(trimws(sci_vet)) %in% pass_set) %>%
        select(Model, Scenario)
      return(all_scens %>% semi_join(keep, by = c("Model", "Scenario")) %>%
               select(Model, Scenario))
    }
    # Fallback: hardcoded SCI-vetted scenario-name list.
    return(all_scens %>% filter(Scenario %in% vetted_scenarios) %>%
             select(Model, Scenario))
  }
  if (vetting == "partial") {
    # MODE 1 (primary): SCI technological-feasibility flags for solar/wind/CDR.
    if (length(TECHFEAS_COLS_FOUND) > 0) {
      fail_set <- tolower(as.character(TECHFEAS_FAIL_VALUES))
      flag_cols <- paste0("techfeas_", names(TECHFEAS_COLS_FOUND))
      tf <- meta_lookup %>% select(Model, Scenario, all_of(flag_cols))
      # per-flag pass: TRUE unless the value is in the fail set (NA passes)
      pass_mat <- sapply(flag_cols, function(cc) {
        v <- tolower(trimws(as.character(tf[[cc]])))
        !(v %in% fail_set)          # NA -> "na" -> not in fail_set -> TRUE (pass)
      })
      pass_mat <- matrix(pass_mat, nrow = nrow(tf))
      tf$keep <- if (TECHFEAS_REQUIRE_ALL) apply(pass_mat, 1, all)
                 else apply(pass_mat, 1, any)
      keep <- tf %>% filter(keep) %>% select(Model, Scenario)
      out <- all_scens %>% semi_join(keep, by = c("Model", "Scenario")) %>%
        select(Model, Scenario)
      cat(sprintf("  [partial/tech-feas] flags used: %s | require_all=%s\n",
                  paste(names(TECHFEAS_COLS_FOUND), collapse = "+"),
                  TECHFEAS_REQUIRE_ALL))
      return(out)
    }
    # MODE 2 (fallback proxy): drop top novel-CDR reliance within ambition group.
    cat("  [partial] no tech-feas columns found — using novel-CDR proxy\n")
    novel <- deploy_metrics %>%
      filter(Variable == "Novel CDR") %>%
      transmute(Model, Scenario, Category, novel_cdr = Total_Value) %>%
      mutate(Ambition = case_when(Category %in% c("C1", "C2") ~ AMB_15C,
                                  Category %in% c("C3", "C4") ~ AMB_2C,
                                  TRUE ~ NA_character_))
    drop <- novel %>%
      filter(!is.na(Ambition)) %>%
      group_by(Ambition) %>%
      mutate(thr = quantile(novel_cdr, PARTIAL_NOVELCDR_PCTL, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(novel_cdr > thr) %>%
      select(Model, Scenario)
    return(all_scens %>% anti_join(drop, by = c("Model", "Scenario")) %>%
             select(Model, Scenario))
  }
  stop("Unknown vetting: ", vetting)
}

# ---- 5c. Pathway classification (top fraction within ambition) --------------
# High-CDR / High-RE from WORLD-level deployment, top `top_frac` within Ambition
# (1/3 = top tercile, 1/2 = above median).
classify_pathways <- function(scen_set, ambition_method, top_frac = TOP_FRAC) {
  wcdr <- deploy_metrics %>%
    filter(Variable == "Total CDR") %>%
    semi_join(scen_set, by = c("Model", "Scenario")) %>%
    group_by(Model, Scenario, Category) %>%
    summarise(total_cdr = sum(Total_Value, na.rm = TRUE), .groups = "drop")
  wre <- deploy_metrics %>%
    filter(Variable == "Renewable Capacity") %>%
    semi_join(scen_set, by = c("Model", "Scenario")) %>%
    group_by(Model, Scenario, Category) %>%
    summarise(total_re = sum(Total_Value, na.rm = TRUE), .groups = "drop")

  full_join(wcdr, wre, by = c("Model", "Scenario", "Category")) %>%
    assign_ambition(ambition_method) %>%
    filter(!is.na(Ambition)) %>%
    group_by(Ambition) %>%
    mutate(
      cdr_thresh = quantile(total_cdr, 1 - top_frac, na.rm = TRUE),
      re_thresh  = quantile(total_re,  1 - top_frac, na.rm = TRUE),
      high_cdr   = total_cdr >= cdr_thresh,
      high_re    = total_re  >= re_thresh,
      Pathway_overlap = case_when(
        high_cdr & high_re  ~ "Both High",
        high_cdr & !high_re ~ "High-CDR only",
        !high_cdr & high_re ~ "High-RE only",
        TRUE                ~ "Low (both)"),
      high_cdr_only = high_cdr & !high_re,
      high_re_only  = high_re  & !high_cdr,
      Pathway_excl  = case_when(high_cdr_only ~ "High-CDR",
                                high_re_only  ~ "High-RE",
                                TRUE ~ NA_character_),
      threshold_label = paste0("top_", round(top_frac * 100), "pct")
    ) %>%
    ungroup()
}

# ---- 5d. Cumulate annual outcomes to ambition window ------------------------
build_df_master <- function(scen_set, pathway_df, ambition_method) {
  # ambition + window per scenario (Model x Scenario -> Ambition)
  amb_map <- pathway_df %>% distinct(Model, Scenario, Category, Ambition) %>%
    mutate(window_end = window_for_ambition(Ambition))

  # CDR/RE cumulative (R10) restricted to the sample, with ambition attached
  cdr_cum <- cdr_cumulative_full %>%
    semi_join(scen_set, by = c("Model", "Scenario")) %>%
    inner_join(amb_map %>% select(Model, Scenario, Ambition),
               by = c("Model", "Scenario"))

  # mortality cumulated to window
  mort_cum <- mortality_annual %>%
    inner_join(amb_map, by = c("Model", "Scenario")) %>%
    filter(Year >= START_YEAR, Year <= window_end) %>%
    group_by(Model, Scenario, Region) %>%
    summarise(cumulative_deaths_mln = sum(deaths_annual * 10, na.rm = TRUE) / 1e6,
              .groups = "drop")
  # (x10: annual deaths carried over 10-yr blocks between 5-yr steps as in source;
  #  adjust the multiplier to match your rfasst timestep convention if needed.)

  # jobs cumulated to window, wide by tech_group
  # Cumulate by the four tech groups, then fold them into the renewable /
  # fossil split implied by the active RE_SPEC (see jobs_re_group). Nuclear and
  # bioenergy are also kept as their own columns so they can be reported or
  # re-grouped without re-running.
  jobs_cum <- jobs_annual %>%
    inner_join(amb_map, by = c("Model", "Scenario", "Category")) %>%
    filter(Year >= START_YEAR, Year <= window_end) %>%
    group_by(Model, Scenario, Region, tech_group) %>%
    summarise(total_jobs = sum(jobs_thousands, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = tech_group, values_from = total_jobs,
                names_prefix = "jobs_", values_fill = 0)
  for (g in c("Renewables", "Nuclear", "Bioenergy", "Fossil")) {
    cn <- paste0("jobs_", g)
    if (!cn %in% names(jobs_cum)) jobs_cum[[cn]] <- 0
  }
  jobs_cum <- jobs_cum %>%
    mutate(jobs_Renewables = rowSums(across(all_of(paste0("jobs_", jobs_re_group))),
                                     na.rm = TRUE))

  # DLE cumulated to window
  dle_cum <- dle_annual %>%
    inner_join(amb_map, by = c("Model", "Scenario", "Category")) %>%
    filter(Year >= START_YEAR, Year <= window_end) %>%
    group_by(Model, Scenario, Region) %>%
    summarise(cumulative_gap_EJ = sum(gap_EJ_total, na.rm = TRUE),
              mean_headcount_millions = mean(headcount_millions, na.rm = TRUE),
              cumulative_implied_CO2_GtCO2 = sum(implied_CO2_GtCO2, na.rm = TRUE),
              .groups = "drop")

  # assemble per-region master (one row per scenario x region x deployment-var),
  # absolute outcomes, then attach 2020 population and per-capita columns.
  dfm <- cdr_cum %>%
    select(-proxy) %>%
    left_join(mort_cum, by = c("Model", "Scenario", "Region")) %>%
    left_join(jobs_cum %>% select(Model, Scenario, Region,
                                  jobs_Renewables, jobs_Fossil,
                                  jobs_Nuclear, jobs_Bioenergy),
              by = c("Model", "Scenario", "Region")) %>%
    left_join(dle_cum, by = c("Model", "Scenario", "Region")) %>%
    left_join(pop2020_r10, by = "Region") %>%
    add_percapita()

  # Aggregate ("World" = 5-region sum) row: SUM absolute outcomes across the R10
  # regions (deaths, jobs, headcount, gap, CO2 are all additive), then normalise
  # by the 5-region total population. Total_Value (deployment) is summed too.
  outcome_cols <- c("cumulative_deaths_mln", "jobs_Renewables", "jobs_Fossil",
                    "jobs_Nuclear", "jobs_Bioenergy",
                    "cumulative_gap_EJ", "mean_headcount_millions",
                    "cumulative_implied_CO2_GtCO2")
  dfm_agg <- dfm %>%
    group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
             Category, Ambition, Variable) %>%
    summarise(
      Total_Value = sum(Total_Value, na.rm = TRUE),
      across(any_of(outcome_cols),
             ~ if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)),
      .groups = "drop") %>%
    mutate(Region = "Aggregated R10 regions", pop_mln = pop2020_total) %>%
    add_percapita()

  bind_rows(dfm, dfm_agg)
}


# =============================================================================
# SECTION 6: RUN ALL APPROACHES (A-E top tercile, F-J above median)
# =============================================================================

cat("\n=== SECTION 6: Running approaches A-J ===\n")

run_approach <- function(row) {
  id <- row$id; vetting <- row$vetting; ambition_method <- row$ambition
  top_frac <- row$top_frac
  cat(sprintf("\n--- Approach %s: %s ---\n", id, row$label))

  scen_set   <- select_scenarios(vetting)
  pathway_df <- classify_pathways(scen_set, ambition_method, top_frac)
  df_master  <- build_df_master(scen_set, pathway_df, ambition_method)

  # per-approach scenario sample summary (one row per scenario)
  scenario_set <- pathway_df %>%
    transmute(Model, Scenario, Category, Ambition,
              total_cdr, total_re,
              Pathway_overlap, Pathway_excl,
              high_cdr_only, high_re_only,
              approach = id, vetting = vetting, ambition_method = ambition_method,
              threshold_label = threshold_label,
              top_frac = top_frac)

  n_scen <- n_distinct(paste(pathway_df$Model, pathway_df$Scenario))
  cat(sprintf("  scenarios in sample: %d | with ambition: %d\n",
              nrow(scen_set), n_scen))
  cat("  pathway counts (mutually exclusive):\n")
  pathway_df %>% filter(!is.na(Pathway_excl)) %>%
    count(Ambition, Pathway_excl) %>% as.data.frame() %>% print()

  # outcome coverage: % non-NA per outcome on the aggregated-R10 Total-CDR rows.
  # Mortality dropping below the others flags the emissions-coverage ceiling.
  cov <- df_master %>%
    filter(Variable == "Total CDR", Region == "Aggregated R10 regions") %>%
    summarise(n = n(),
              across(any_of(c("cumulative_deaths_mln", "jobs_Renewables", "jobs_Fossil",
                              "cumulative_gap_EJ", "mean_headcount_millions",
                              "cumulative_implied_CO2_GtCO2")),
                     ~ round(100 * mean(!is.na(.x)), 0), .names = "pct_{.col}"))
  cat("  outcome coverage (% non-NA, agg R10 Total CDR rows):\n")
  print(as.data.frame(cov))

  # save per-approach outputs
  adir <- file.path(OUT_DIR, paste0("approach_", id))
  dir.create(adir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(df_master,  file.path(adir, paste0("compass_master_dataset_", id, ".rds")))
  write.csv(df_master, file.path(adir, paste0("compass_master_dataset_", id, ".csv")), row.names = FALSE)
  saveRDS(pathway_df, file.path(adir, paste0("compass_pathway_tercile_", id, ".rds")))
  write.csv(pathway_df, file.path(adir, paste0("compass_pathway_tercile_", id, ".csv")), row.names = FALSE)
  write.csv(scenario_set, file.path(adir, paste0("compass_scenario_set_", id, ".csv")), row.names = FALSE)
  write.csv(
    cdr_cumulative_full %>% semi_join(scen_set, by = c("Model", "Scenario")),
    file.path(adir, paste0("compass_cdr_cumulative_", id, ".csv")), row.names = FALSE)

  list(id = id, scenario_set = scenario_set, pathway = pathway_df,
       df_master = df_master, n_sample = nrow(scen_set), n_ambition = n_scen)
}

results <- lapply(seq_len(nrow(approaches)), function(i) run_approach(approaches[i, ]))
names(results) <- approaches$id


# =============================================================================
# SECTION 7: CROSS-APPROACH COMPARISON
# =============================================================================

cat("\n=== SECTION 7: Cross-approach comparison ===\n")

comp_dir <- file.path(OUT_DIR, "comparison")
dir.create(comp_dir, showWarnings = FALSE, recursive = TRUE)

all_scenario_sets <- map_dfr(results, "scenario_set")

# 7a. Scenario counts per approach x ambition (and classified pathways)
approach_scenario_counts <- all_scenario_sets %>%
  group_by(approach, vetting, ambition_method, Ambition) %>%
  summarise(n_scenarios   = n_distinct(paste(Model, Scenario)),
            n_high_cdr     = sum(high_cdr_only, na.rm = TRUE),
            n_high_re      = sum(high_re_only,  na.rm = TRUE),
            .groups = "drop") %>%
  arrange(approach, Ambition)
write.csv(approach_scenario_counts,
          file.path(comp_dir, "approach_scenario_counts.csv"), row.names = FALSE)
cat("\nScenario counts per approach x ambition:\n")
print(as.data.frame(approach_scenario_counts))

# 7b. Pathway counts per approach x ambition x pathway (mutually exclusive)
approach_pathway_counts <- all_scenario_sets %>%
  filter(!is.na(Pathway_excl)) %>%
  group_by(approach, Ambition, Pathway_excl) %>%
  summarise(n = n_distinct(paste(Model, Scenario)), .groups = "drop") %>%
  arrange(approach, Ambition, Pathway_excl)
write.csv(approach_pathway_counts,
          file.path(comp_dir, "approach_pathway_counts.csv"), row.names = FALSE)

# 7c. Scenario membership matrix (which approaches select each scenario)
approach_membership <- all_scenario_sets %>%
  distinct(approach, Model, Scenario) %>%
  mutate(in_sample = 1L) %>%
  pivot_wider(names_from = approach, values_from = in_sample,
              names_prefix = "approach_", values_fill = 0L) %>%
  arrange(Model, Scenario)
write.csv(approach_membership,
          file.path(comp_dir, "approach_scenario_membership.csv"), row.names = FALSE)

# 7d. One-line summary per approach
approach_summary <- approaches %>%
  left_join(
    tibble(id = names(results),
           n_sample   = map_int(results, "n_sample"),
           n_ambition = map_int(results, "n_ambition")),
    by = "id") %>%
  left_join(
    all_scenario_sets %>%
      filter(!is.na(Pathway_excl)) %>%
      group_by(id = approach) %>%
      summarise(n_high_cdr = sum(high_cdr_only, na.rm = TRUE),
                n_high_re  = sum(high_re_only,  na.rm = TRUE),
                .groups = "drop"),
    by = "id")
write.csv(approach_summary,
          file.path(comp_dir, "approach_summary.csv"), row.names = FALSE)
cat("\nApproach summary:\n")
print(as.data.frame(approach_summary))

# 7e. Pairwise overlap of selected scenario sets (Jaccard on Model|Scenario)
set_list <- map(results, ~ unique(paste(.x$scenario_set$Model,
                                        .x$scenario_set$Scenario, sep = "||")))
overlap <- expand_grid(a = names(set_list), b = names(set_list)) %>%
  rowwise() %>%
  mutate(
    n_a = length(set_list[[a]]),
    n_b = length(set_list[[b]]),
    n_shared = length(intersect(set_list[[a]], set_list[[b]])),
    jaccard  = n_shared / length(union(set_list[[a]], set_list[[b]]))
  ) %>%
  ungroup()
write.csv(overlap, file.path(comp_dir, "approach_set_overlap.csv"), row.names = FALSE)
cat("\nPairwise selected-scenario overlap (Jaccard):\n")
overlap %>%
  select(a, b, jaccard) %>%
  pivot_wider(names_from = b, values_from = jaccard) %>%
  as.data.frame() %>% print()

cat("\n=== COMPASS MASTER ANALYSIS COMPLETE ===\n")
cat("Per-approach outputs:  ", file.path(OUT_DIR, "approach_<A-J>"), "\n")
cat("Comparison outputs:    ", comp_dir, "\n")
cat("\nNext step: point the figure scripts at one approach's subfolder, e.g.\n")
cat('  df_master <- readRDS(file.path(OUT_DIR, "approach_C",\n')
cat('                                 "compass_master_dataset_C.rds"))\n')
