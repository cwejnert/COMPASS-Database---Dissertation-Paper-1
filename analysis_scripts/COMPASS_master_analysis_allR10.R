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
#                        10-region sum) resolution. Save each approach to its
#                        own subfolder.
#   STAGE 3:             Cross-approach comparison tables (sample sizes,
#                        overlap of selected scenarios, pathway counts).
#
# POPULATION NORMALISATION
#   Denominator: fixed 2020 population (median across scenarios) per R10
#   region; the aggregate row uses the 10-region total. Per-capita columns
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
#   compass_mortality_r10_noNH3.csv <- COMPASS_rfasst*.R (annual PM2.5 mortality)
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
# Override only for isolated sensitivity runs; the submission release preserves
# the canonical COMPASS_master directory unchanged.
OUT_DIR     <- Sys.getenv("COMPASS_OUT_DIR",
                          "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_master")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- 0b. Analysis constants -------------------------------------------------
# ALL TEN R10 regions (was five: AFRICA, CHINA+, INDIA+, EUROPE, NORTH_AM,
# which covered ~63% of world population). Codes match COMPASS_rfasst_full.R's
# fasst_to_r10 mapping exactly.
regions_r10 <- c("R10AFRICA", "R10CHINA+", "R10EUROPE", "R10INDIA+",
                 "R10LATIN_AM", "R10MIDDLE_EAST", "R10NORTH_AM",
                 "R10PAC_OECD", "R10REF_ECON", "R10REST_ASIA")
cats_keep   <- c("C1", "C2", "C3", "C4")
START_YEAR  <- 2020L

# FINAL RELEASE: the primary classification and wellbeing outcomes use the
# common 2020-2100 horizon. Set COMPASS_OUTCOME_WINDOW_END=2050 only when
# intentionally generating the documented near-term sensitivity.
OUTCOME_WINDOW_END <- as.integer(Sys.getenv("COMPASS_OUTCOME_WINDOW_END", "2100"))
if (!OUTCOME_WINDOW_END %in% c(2050L, 2100L)) {
  stop("COMPASS_OUTCOME_WINDOW_END must be 2050 or 2100")
}
ANALYSIS_RELEASE <- paste0("paper1-final_2020-", OUTCOME_WINDOW_END,
                           "_nh3-harmonised")
OUTCOME_WINDOW_TAG <- paste0(START_YEAR, "_", OUTCOME_WINDOW_END)
WINDOW_15C  <- OUTCOME_WINDOW_END   # 1.5C (High-Ambition)   group
WINDOW_2C   <- OUTCOME_WINDOW_END   # 2C   (Medium-Ambition) group

AMB_15C <- "1.5C (High-Ambition)"
AMB_2C  <- "2C (Medium-Ambition)"

# Default top fraction for High-CDR / High-RE classification. Each approach
# carries its own top_frac (1/3 = top tercile, 1/2 = above median); this global
# is only a fallback for calls that don't pass one.
TOP_FRAC <- 1/3

# Carbon-management axis used for pathway classification.  `total_cdr` retains
# the all-removals definition for sensitivity work. `engineered_cmt` is the
# energy-system definition: novel/geologically stored CDR plus fossil/industrial
# CCS, explicitly excluding land-based CDR.
CMT_SPEC <- Sys.getenv("COMPASS_CMT_SPEC", "total_cdr")
if (!CMT_SPEC %in% c("total_cdr", "engineered_cmt")) {
  stop("COMPASS_CMT_SPEC must be 'total_cdr' or 'engineered_cmt'")
}
CMT_AXIS_VAR <- if (CMT_SPEC == "engineered_cmt") "Engineered CMT" else "Total CDR"

# `exclusive_upper` is the legacy broad-archetype rule. `opposing_terciles`
# defines genuinely opposing portfolios: high CMT / low RE versus high RE / low
# CMT, with both thresholds calculated within the selected ambition band.
PORTFOLIO_RULE <- Sys.getenv("COMPASS_PORTFOLIO_RULE", "exclusive_upper")
if (!PORTFOLIO_RULE %in% c("exclusive_upper", "opposing_terciles")) {
  stop("COMPASS_PORTFOLIO_RULE must be 'exclusive_upper' or 'opposing_terciles'")
}
cat("CMT classification spec:", CMT_SPEC, "(", CMT_AXIS_VAR,
    ") | portfolio rule:", PORTFOLIO_RULE, "\n")

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
# top_frac : 1/5 (top quintile) | 1/3 (top tercile) | 1/2 (above median)
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
  "J", "Partial SCI (tech-feas.); AR6 ambition; above median",     "partial", "ar6",     1/2,
  # Stricter classification sensitivity. It improves contrast extremity but
  # can alter exclusive-arm membership and reduce cross-model overlap.
  "M", "All scenarios; AR6 ambition; top quintile sensitivity",    "none",    "ar6",     1/5,
  "N", "All scenarios; peak-warming; top quintile sensitivity",    "none",    "warming", 1/5,
  "O", "Full SCI vetting; AR6; top quintile sensitivity",          "full",    "ar6",     1/5,
  "P", "Full SCI vetting; peak-warming; top quintile sensitivity", "full",    "warming", 1/5,
  "Q", "Partial SCI; AR6; top quintile sensitivity",               "partial", "ar6",     1/5,
  "K", "All scenarios; SCI GW-tier ambition; top tercile",         "none",    "sci_gw",  1/3,
  "L", "Full SCI vetting; SCI GW-tier ambition; top tercile",      "full",    "sci_gw",  1/3
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

# SCI 2025 Global Warming (GW0-GW8) category, Tier I -- SCI's OWN peak-warming
# classification, introduced as its replacement for AR6's C1-C8. Sourced
# directly from SCI (same provenance as SCI_VET_COL), unlike WARMING_15C_MAX/
# WARMING_2C_MAX below, which are our own chosen peak-warming cutoffs. Where
# both are present, GW3 spans the AR6 C2/C3/C4 boundary region (SCI's own
# scientists did not draw the 1.5-vs-2C line exactly at AR6 C2|C3), which is
# the same ambiguity the self-chosen 1.7/2.0 cutoffs were trying to resolve.
# Mapping: GW0-GW2 -> 1.5C (peak <~1.7C), GW3 -> 2C ("likely below 2C");
# GW4+ excluded, matching the exclusion of >2.0C peak warming elsewhere.
SCI_GW_COL <- resolve_col(
  c("Climate Category|SCI 2025 [Tier I]"),
  detect_patterns = c("sci", "tier i\\]")
)
cat("SCI GW tier column:", SCI_GW_COL, "\n")

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
              else NA_character_,
    sci_gw = if (!is.na(SCI_GW_COL)) as.character(.data[[SCI_GW_COL]])
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

# Engineered carbon management deliberately excludes land-based removal. It is
# appropriate where the paper's estimand is an energy-system portfolio rather
# than every pathway that reports large net removal.
engineered_cmt_ts <- bind_rows(novel_cdr_ts, fossil_ccs_ts) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
           Region, Category, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Variable = "Engineered CMT")

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
RE_SPEC <- Sys.getenv("COMPASS_RE_SPEC", "renewables")

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
  novel_cdr_ts, fossil_ccs_ts, land_cdr_ts, total_cdr_ts, engineered_cmt_ts, re_total_ts
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
  filter(Variable %in% c("Total CDR", "Engineered CMT", "Renewable Capacity")) %>%
  group_by(Model_Group, Model, Scenario, ModelGroup_Scenario, Category, Variable) %>%
  summarise(Total_Value = sum(Total_Value, na.rm = TRUE), .groups = "drop") %>%
  filter(Total_Value > 0)

r10_scenario_keys <- cdr_cumulative_full %>%
  distinct(Model, Scenario)
world_cdr_scenario_keys <- world_cumulative_direct %>%
  filter(Variable == "Total CDR") %>%
  distinct(Model, Scenario)
n_r10_scens <- nrow(r10_scenario_keys)
n_world_cdr <- world_cdr_scenario_keys %>%
  inner_join(r10_scenario_keys, by = c("Model", "Scenario")) %>%
  nrow()
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
    # NOTE: with all ten R10 regions in scope, the sum-R10 fallback is now a
    # genuine global total, so the two branches of this coalesce are finally on
    # the same geography. Previously World CDR (all ten regions) was mixed with
    # a five-region renewables sum, putting the two axes of the ranking on
    # different footprints.
    # BOTH AXES ON ONE GEOGRAPHY. coalesce() resolved CDR to the database
    # World row (which includes the Other (R10) residual) while renewables fell
    # back to the ten-region sum, because total_world exists for one variable
    # and not the other. Ranking a scenario on a wider footprint for CDR than
    # for RE is not a like-for-like tercile. The ten-region sum exists for both
    # and matches every regional outcome, so use it for both.
    mutate(Total_Value = total_sumR10) %>%
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
# Do not select the newest generic mortality file. NH3 coverage and spatial
# allocation materially change interpretation, so the release is explicit.
# The default is the legacy NH3-fixed diagnostic only for reproduction; a
# submission run must set COMPASS_MORTALITY_FILE to the complete-precursor
# linked/world-fallback, base-emission-allocation output.
MORTALITY_RELEASE <- Sys.getenv("COMPASS_MORTALITY_RELEASE", "legacy-nh3-fixed diagnostic")
MORTALITY_FILE <- Sys.getenv("COMPASS_MORTALITY_FILE", "compass_mortality_r10_noNH3.csv")
mort_r10_path <- file.path(COMPASS_DIR, MORTALITY_FILE)
mortality_annual <- NULL
if (file.exists(mort_r10_path)) {
  cat("Reading annual mortality from:", mort_r10_path,
      "(release", MORTALITY_RELEASE, "; modified",
      format(file.mtime(mort_r10_path)), ")\n")
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
  stop("Submission mortality file missing: ", mort_r10_path,
       "\nRun COMPASS_rfasst_full_allR10.R with the matching NH3/allocation ",
       "configuration and rebuild results; do not substitute a different ",
       "mortality convention or an annualised cumulative summary.")
}

# ---- 4b. Energy jobs (annual) -----------------------------------------------
# Ten-region employment factors. Built by build_job_factors_FINAL.R from the
# original Emmerling source (github.com/witch-team/energy-jobs-dataset,
# FinalAllcountries) with Rutovitz 2015 Table 6 multipliers as the documented
# fallback. Also corrects the manufacturing unit error for coal/gas/oil/nuclear
# (published per-MW values had been entered into a per-GW table).
# Per-cell provenance: job_factors_allR10_provenance.csv
job_factors_complete <- read.csv(file.path(AR6_DIR, "job_factors_allR10.csv"))

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

cap_additions_ts <- compass_ts %>%
  filter(Variable %in% cap_additions_fuel_map$Variable) %>%
  inner_join(cap_additions_fuel_map, by = "Variable")

# Choose the additions driver PER FUEL, not per scenario-region. Some IAMs
# report direct additions for only a subset of technologies. Treating the
# presence of one additions variable as coverage for every fuel silently
# dropped construction jobs for unreported technologies. Any reported series,
# including an all-zero series, counts as direct coverage; otherwise additions
# are inferred from positive annual stock changes.
direct_addition_keys <- cap_additions_ts %>%
  filter(Year >= START_YEAR) %>%
  distinct(Model, Scenario, Region, fuel)

# Keep 2019 solely to form the opening 2020 stock balance. The outcome window
# still begins in 2020.
stock_balance_ts <- compass_ts %>%
  filter(Variable %in% cap_stock_fuel_map$Variable,
         Year >= START_YEAR - 1L) %>%
  inner_join(cap_stock_fuel_map, by = "Variable") %>%
  transmute(Model, Scenario, Region, Category, Year, fuel, tech_group,
            stock_GW = pmax(0, Value))

stock_keys_needing_diff <- stock_balance_ts %>%
  filter(Year >= START_YEAR) %>%
  distinct(Model, Scenario, Region, fuel) %>%
  anti_join(direct_addition_keys,
            by = c("Model", "Scenario", "Region", "fuel"))

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

# Same factors kept BY CATEGORY rather than rolled into the two streams, so the
# temporal / by-technology analysis can separate construction from manufacturing
# and O&M from extraction/refinery. `basis` records which driver each category
# scales with, matching the build/ongoing split above exactly: summing these
# five by stream reproduces build_ef and ongoing_ef.
job_ef_long <- job_factors_complete %>%
  filter(category %in% c("construction", "manufacturing",
                         "oem", "extraction", "refinery")) %>%
  group_by(region, fuel, category) %>%
  summarise(ef = mean(job_intensity), .groups = "drop") %>%
  mutate(basis = if_else(category %in% c("construction", "manufacturing"),
                         "added", "stock"))

# ---- installed CAPACITY STOCK by tech -> drives O&M + fuel (ongoing) --------
stock_ts <- stock_balance_ts %>% filter(Year >= START_YEAR)

# ---- capacity ADDITIONS by tech -> drives build (one-time) ------------------
# Direct where reported; else implied from the annual stock change (year-gap,
# = 1 on the annual grid; see the fallback-divisor fix note).
add_direct <- cap_additions_ts %>%
  filter(Year >= START_YEAR) %>%
  transmute(Model, Scenario, Region, Category, Year, fuel, tech_group,
            add_GW = pmax(0, Value))
add_fallback <- stock_balance_ts %>%
  semi_join(stock_keys_needing_diff,
            by = c("Model", "Scenario", "Region", "fuel")) %>%
  arrange(Model, Scenario, Region, fuel, Year) %>%
  group_by(Model, Scenario, Region, Category, fuel, tech_group) %>%
  mutate(add_GW = pmax(0, (stock_GW - lag(stock_GW)) /
                              (Year - lag(Year)))) %>%
  ungroup() %>%
  filter(Year >= START_YEAR, !is.na(add_GW)) %>%
  transmute(Model, Scenario, Region, Category, Year, fuel, tech_group, add_GW)
additions_ts <- bind_rows(add_direct, add_fallback)

# ---- inferred gross retirements ---------------------------------------------
# Capacity Additions is GW/year and compass_interp is annual, so the stock-flow
# identity is evaluated one year at a time:
#   retired_t = max(0, stock_{t-1} + additions_t - stock_t).
# The residual fields expose non-closing IAM series rather than hiding them.
# They are especially important when additions and stock were interpolated
# independently. Retirements are a transition diagnostic; the associated lost
# O&M/fuel jobs are NOT subtracted again from cumulative employment because the
# lower installed stock already removes those future job-years.
additions_balance <- additions_ts %>%
  group_by(Model, Scenario, Region, Category, Year, fuel, tech_group) %>%
  summarise(gross_additions_GW = sum(add_GW, na.rm = TRUE), .groups = "drop")

retirement_ts <- stock_balance_ts %>%
  arrange(Model, Scenario, Region, fuel, Year) %>%
  group_by(Model, Scenario, Region, Category, fuel, tech_group) %>%
  mutate(previous_stock_GW = lag(stock_GW),
         year_gap = Year - lag(Year)) %>%
  ungroup() %>%
  filter(Year >= START_YEAR, year_gap == 1L) %>%
  left_join(additions_balance,
            by = c("Model", "Scenario", "Region", "Category", "Year",
                   "fuel", "tech_group")) %>%
  mutate(gross_additions_GW = coalesce(gross_additions_GW, 0),
         stock_change_GW = stock_GW - previous_stock_GW,
         inferred_retirements_GW = pmax(
           0, previous_stock_GW + gross_additions_GW - stock_GW),
         unexplained_additions_GW = pmax(
           0, stock_GW - previous_stock_GW - gross_additions_GW),
         balance_residual_GW = previous_stock_GW + gross_additions_GW -
           inferred_retirements_GW - stock_GW)

# JGCRI's open GCAMUSAJobs package derives decommissioning employment factors
# from NREL JEDI project models and links them to retired capacity. These are
# medians across its 52 U.S. state/DC rows (job-years/MW), downloaded 2026-08-31
# from data/EF.JEDI.rda. They are a transparent U.S.-anchored proxy, not a claim
# of globally observed R10 factors:
#   https://jgcri.github.io/GCAMUSAJobs/articles/methods.html
#   https://github.com/JGCRI/GCAMUSAJobs/blob/main/data/EF.JEDI.rda
# A no-decommissioning sensitivity is available through the environment flag.
DECOMMISSIONING_MODE <- Sys.getenv("COMPASS_DECOMMISSIONING_MODE", "jedi_proxy")
if (!DECOMMISSIONING_MODE %in% c("jedi_proxy", "none"))
  stop("COMPASS_DECOMMISSIONING_MODE must be 'jedi_proxy' or 'none'")

jedi_decommission_anchor <- tribble(
  ~fuel,        ~decommission_jobyears_per_MW_us_median,
  "solar_pv",   0.62978903,
  "wind_on",    0.10159004,
  "hydro",      0.74615629,
  "geothermal", 1.35736336,
  "nuclear",    3.21283205,
  "biomass",    1.67724748,
  "coal",       2.36290997,
  "gas",        0.62078255,
  "oil",        2.36290997
)

# Transfer the U.S. anchors to R10 using a technology-independent regional
# labour multiplier derived from this analysis's existing construction factors.
# This mirrors the regionalisation already used for geothermal and avoids
# pretending that a U.S. state factor is directly global. Clamp only guards
# against pathological sparse-factor ratios; the unclamped value is retained.
.decom_donors <- intersect(jedi_decommission_anchor$fuel,
                           unique(job_factors_complete$fuel))
decommission_region_mult <- job_factors_complete %>%
  filter(category == "construction", fuel %in% .decom_donors,
         region %in% regions_r10, job_intensity > 0) %>%
  group_by(fuel) %>%
  mutate(relative_labour = job_intensity /
           exp(mean(log(job_intensity), na.rm = TRUE))) %>%
  group_by(region) %>%
  summarise(regional_labour_multiplier_raw =
              exp(mean(log(relative_labour), na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(regional_labour_multiplier = pmin(
    8, pmax(0.25, regional_labour_multiplier_raw)))

decommission_ef <- crossing(
  region = regions_r10,
  jedi_decommission_anchor
) %>%
  left_join(decommission_region_mult, by = "region") %>%
  mutate(regional_labour_multiplier = coalesce(regional_labour_multiplier, 1),
         decommission_jobyears_per_GW =
           as.numeric(DECOMMISSIONING_MODE == "jedi_proxy") *
           decommission_jobyears_per_MW_us_median * 1000 *
           regional_labour_multiplier)

# Gross worker displacement at closure is reported separately from total
# job-years. Plant jobs use O&M only. Extraction and refinery are upstream
# regional effects and are not described as workers located at the plant.
retirement_jobs_annual <- retirement_ts %>%
  left_join(job_ef_long %>%
              filter(basis == "stock") %>%
              select(region, fuel, category, ef) %>%
              pivot_wider(names_from = category, values_from = ef,
                          values_fill = 0),
            by = c("Region" = "region", "fuel")) %>%
  left_join(decommission_ef,
            by = c("Region" = "region", "fuel")) %>%
  mutate(across(any_of(c("oem", "extraction", "refinery")),
                ~ coalesce(.x, 0)),
         plant_jobs_displaced_thousands =
           inferred_retirements_GW * oem / 1000,
         upstream_jobs_displaced_thousands =
           inferred_retirements_GW * (extraction + refinery) / 1000,
         decommission_jobyears_thousands =
           inferred_retirements_GW *
             coalesce(decommission_jobyears_per_GW, 0) / 1000)

# ---- three streams -> total jobs per Model/Scenario/Region/Year/tech_group --
# Keep `fuel` and tag which stream the jobs came from, so the temporal and
# by-technology analysis can be done downstream. `jobs_annual` is then
# aggregated back to exactly its previous columns, so nothing else changes.
jobs_build <- additions_ts %>%
  left_join(job_ef, by = c("Region" = "region", "fuel")) %>%
  transmute(Model, Scenario, Region, Category, Year, fuel, tech_group,
            stream = "build",
            jobs_thousands = add_GW * build_ef / 1000)
jobs_ongoing <- stock_ts %>%
  left_join(job_ef, by = c("Region" = "region", "fuel")) %>%
  transmute(Model, Scenario, Region, Category, Year, fuel, tech_group,
            stream = "ongoing",
            jobs_thousands = stock_GW * ongoing_ef / 1000)

jobs_by_fuel <- bind_rows(jobs_build, jobs_ongoing) %>%
  filter(!is.na(jobs_thousands))

# unchanged downstream object
jobs_annual <- jobs_by_fuel %>%
  group_by(Model, Scenario, Region, Category, Year, tech_group) %>%
  summarise(jobs_thousands = sum(jobs_thousands, na.rm = TRUE), .groups = "drop")

# --- the same jobs, split by the FIVE employment categories ------------------
# construction / manufacturing scale with capacity ADDED; oem / extraction /
# refinery scale with capacity INSTALLED. Summing `category` within `stream`
# reproduces jobs_by_fuel exactly -- checked below.
jobs_build_cat <- additions_ts %>%
  inner_join(filter(job_ef_long, basis == "added"),
             by = c("Region" = "region", "fuel"), relationship = "many-to-many") %>%
  transmute(Model, Scenario, Region, Category, Year, fuel, tech_group,
            stream = "build", job_type = category,
            jobs_thousands = add_GW * ef / 1000)
jobs_ongoing_cat <- stock_ts %>%
  inner_join(filter(job_ef_long, basis == "stock"),
             by = c("Region" = "region", "fuel"), relationship = "many-to-many") %>%
  transmute(Model, Scenario, Region, Category, Year, fuel, tech_group,
            stream = "ongoing", job_type = category,
            jobs_thousands = stock_GW * ef / 1000)

jobs_by_type <- bind_rows(jobs_build_cat, jobs_ongoing_cat) %>%
  filter(!is.na(jobs_thousands))

# consistency check: the five categories must sum back to the two streams
.chk <- jobs_by_type %>%
  group_by(Model, Scenario, Region, Year, fuel, stream) %>%
  summarise(a = sum(jobs_thousands), .groups = "drop") %>%
  inner_join(jobs_by_fuel %>%
               group_by(Model, Scenario, Region, Year, fuel, stream) %>%
               summarise(b = sum(jobs_thousands), .groups = "drop"),
             by = c("Model", "Scenario", "Region", "Year", "fuel", "stream"))
stopifnot(nrow(.chk) > 0, max(abs(.chk$a - .chk$b)) < 1e-8)
cat("job-type split reconciles with stream totals (max diff",
    signif(max(abs(.chk$a - .chk$b)), 3), ")\n")

# --- SAVE the temporal / by-fuel / by-type jobs data ------------------------
# Jobs depend only on the scenario data, not on the pathway classification, so
# this is written once here rather than inside the per-approach loop.
# Thinned to decade years: nothing in the planned temporal analysis needs
# 5-year steps, and the full grid is large.
.jobs_out <- jobs_by_type %>% filter(Year %% 10 == 0)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
saveRDS(.jobs_out, file.path(OUT_DIR, "compass_jobs_by_fuel_type_year.rds"),
        compress = "xz")
cat("wrote compass_jobs_by_fuel_type_year.rds:", nrow(.jobs_out), "rows |",
    dplyr::n_distinct(.jobs_out$fuel), "fuels |",
    dplyr::n_distinct(.jobs_out$job_type), "job types |",
    dplyr::n_distinct(.jobs_out$Region), "regions |",
    dplyr::n_distinct(.jobs_out$Year), "years\n")

saveRDS(retirement_jobs_annual,
        file.path(OUT_DIR, "compass_jobs_retirements_decommissioning_annual.rds"),
        compress = "xz")
.retirement_summary <- retirement_jobs_annual %>%
  filter(Year >= START_YEAR, Year <= OUTCOME_WINDOW_END) %>%
  group_by(Model, Scenario, Region, Category, fuel, tech_group) %>%
  summarise(
    inferred_retirements_GW = sum(inferred_retirements_GW, na.rm = TRUE),
    gross_additions_GW = sum(gross_additions_GW, na.rm = TRUE),
    plant_jobs_displaced_thousands =
      sum(plant_jobs_displaced_thousands, na.rm = TRUE),
    upstream_jobs_displaced_thousands =
      sum(upstream_jobs_displaced_thousands, na.rm = TRUE),
    decommission_jobyears_thousands =
      sum(decommission_jobyears_thousands, na.rm = TRUE),
    absolute_balance_residual_GW = sum(abs(balance_residual_GW), na.rm = TRUE),
    unexplained_additions_GW = sum(unexplained_additions_GW, na.rm = TRUE),
    .groups = "drop")
write_csv(.retirement_summary,
          file.path(OUT_DIR, paste0(
            "compass_jobs_retirements_decommissioning_",
            OUTCOME_WINDOW_TAG, ".csv")))
write_csv(decommission_ef,
          file.path(OUT_DIR, "decommissioning_factors_r10_jedi_proxy.csv"))

# Compact scenario-region totals used by the revised employment outcome and
# transition figures. Unlike net_re_jobs_per_1k, jobs_EnergyTotal is an actual
# total employment measure: every technology's positive job-years are added.
.jobs_group_cumulative <- jobs_annual %>%
  filter(Year >= START_YEAR, Year <= OUTCOME_WINDOW_END) %>%
  group_by(Model, Scenario, Region, Category, tech_group) %>%
  summarise(jobyears_thousands = sum(jobs_thousands, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider(names_from = tech_group, values_from = jobyears_thousands,
              names_prefix = "jobs_", values_fill = 0)
for (g in c("Renewables", "Nuclear", "Bioenergy", "Fossil")) {
  cn <- paste0("jobs_", g)
  if (!cn %in% names(.jobs_group_cumulative))
    .jobs_group_cumulative[[cn]] <- 0
}
.jobs_transition_cumulative <- retirement_jobs_annual %>%
  filter(Year >= START_YEAR, Year <= OUTCOME_WINDOW_END) %>%
  group_by(Model, Scenario, Region, Category) %>%
  summarise(
    jobs_Decommission = sum(decommission_jobyears_thousands, na.rm = TRUE),
    gross_plant_jobs_displaced =
      sum(plant_jobs_displaced_thousands, na.rm = TRUE),
    gross_upstream_jobs_displaced =
      sum(upstream_jobs_displaced_thousands, na.rm = TRUE),
    inferred_retirements_GW = sum(inferred_retirements_GW, na.rm = TRUE),
    absolute_balance_residual_GW = sum(abs(balance_residual_GW), na.rm = TRUE),
    unexplained_additions_GW = sum(unexplained_additions_GW, na.rm = TRUE),
    .groups = "drop")
jobs_cumulative <- .jobs_group_cumulative %>%
  left_join(.jobs_transition_cumulative,
            by = c("Model", "Scenario", "Region", "Category")) %>%
  mutate(across(c(jobs_Decommission, gross_plant_jobs_displaced,
                  gross_upstream_jobs_displaced, inferred_retirements_GW,
                  absolute_balance_residual_GW, unexplained_additions_GW),
                ~ coalesce(.x, 0)),
         jobs_EnergyTotal_no_decommission = jobs_Renewables + jobs_Nuclear +
           jobs_Bioenergy + jobs_Fossil,
         jobs_EnergyTotal = jobs_EnergyTotal_no_decommission +
           jobs_Decommission,
         # Retained only as a portfolio-composition diagnostic.
         jobs_RE_minus_fossil = jobs_Renewables - jobs_Fossil)
saveRDS(jobs_cumulative,
        file.path(OUT_DIR, paste0("compass_jobs_cumulative_",
                                  OUTCOME_WINDOW_TAG, ".rds")),
        compress = "xz")
write_csv(jobs_cumulative,
          file.path(OUT_DIR, paste0("compass_jobs_cumulative_",
                                    OUTCOME_WINDOW_TAG, ".csv")))
cat("wrote retirement/decommissioning outputs:",
    nrow(retirement_jobs_annual), "annual fuel-region rows | mode",
    DECOMMISSIONING_MODE, "\n")

if (identical(Sys.getenv("COMPASS_JOBS_ONLY"), "1")) {
  cat("COMPASS_JOBS_ONLY=1: jobs outputs complete; stopping before DLE and ",
      "cross-approach rebuild.\n", sep = "")
  quit(save = "no", status = 0)
}

# ---- 4c. DLE gap / headcount / implied CO2 (annual) -------------------------
# DLE FIX 1: use the official DESIRE country-level IAM energy-threshold mapping,
# aggregated to this project's R10 concordance with documented population
# weights. This replaces pixel-read 2021 regional totals and a global sector
# split. Build/refresh this input with work/build_desire_r10_inputs.R.
DLE_INPUT_PATH <- file.path(COMPASS_DIR, "05_inputs", "desire_r10_dle_inputs.csv")
if (!file.exists(DLE_INPUT_PATH)) stop("Missing official DESIRE R10 input: ", DLE_INPUT_PATH)
DLE_THRESHOLD_SCALE <- as.numeric(Sys.getenv("COMPASS_DLE_THRESHOLD_SCALE", "1"))
if (!is.finite(DLE_THRESHOLD_SCALE) || DLE_THRESHOLD_SCALE <= 0) {
  stop("COMPASS_DLE_THRESHOLD_SCALE must be a positive number")
}
dle_inputs <- read.csv(DLE_INPUT_PATH, stringsAsFactors = FALSE) %>%
  filter(Region %in% regions_r10)
stopifnot(nrow(dle_inputs) == length(regions_r10),
          all(c("res_comm_GJ", "transport_GJ", "industry_GJ",
                "desire_energy_gini") %in% names(dle_inputs)))
dle_thresholds <- dle_inputs %>%
  transmute(Region, res_comm_GJ = res_comm_GJ * DLE_THRESHOLD_SCALE,
            transport_GJ = transport_GJ * DLE_THRESHOLD_SCALE,
            industry_GJ = industry_GJ * DLE_THRESHOLD_SCALE)
cat("DLE thresholds (GJ/cap/yr), official DESIRE country mapping; scale = ",
    DLE_THRESHOLD_SCALE, "\n", sep = "")
print(as.data.frame(dle_thresholds %>%
        mutate(total = res_comm_GJ + transport_GJ + industry_GJ)))
# DLE FIX 2 (see dle_fix.R): steepen provisioning-efficiency to match DESIRE's
# ~-30% to -46% by 2040. 1.9%/yr lands at ~-38% by 2040, then holds a floor.
# (was sector-specific 1.0-1.5%/yr, giving only ~-24% by 2040.)
SEF_RATE  <- as.numeric(Sys.getenv("COMPASS_DLE_SEF_RATE", "0.019"))
SEF_FLOOR <- as.numeric(Sys.getenv("COMPASS_DLE_SEF_FLOOR", "0.5"))
if (!is.finite(SEF_RATE) || SEF_RATE < 0 || !is.finite(SEF_FLOOR) ||
    SEF_FLOOR <= 0 || SEF_FLOOR > 1) {
  stop("DLE SEF settings must satisfy rate >= 0 and 0 < floor <= 1")
}
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

# DESIRE's country-level final-energy Gini estimates (observed where available,
# otherwise model-filled by the authors) are population-weighted to the same
# project R10 concordance in `desire_r10_dle_inputs.csv`. This replaces the
# earlier income/consumption-Gini proxy. Absolute levels remain conditional on
# the lognormal approximation, so threshold/Gini sensitivity remains required.
DLE_GINI_SHIFT <- as.numeric(Sys.getenv("COMPASS_DLE_GINI_SHIFT", "0"))
if (!is.finite(DLE_GINI_SHIFT)) stop("COMPASS_DLE_GINI_SHIFT must be numeric")
desire_energy_gini <- dle_inputs %>%
  transmute(Region, gini = pmin(0.75, pmax(0.15, desire_energy_gini + DLE_GINI_SHIFT)),
            sigma_ln = sqrt(2) * qnorm((gini + 1) / 2))

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
  left_join(desire_energy_gini, by = "Region") %>% # DESIRE final-energy distribution
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


# ---- 4c-bis. PERSIST THE ANNUAL DLE TABLE ---------------------------------
# Written so the deprivation gap can be re-cumulated to any window (e.g. the
# 2020-2050 net-zero window) without re-running the master. Mirrors what
# compass_mortality_r10.csv already provides for mortality.
saveRDS(dle_annual, file.path(COMPASS_DIR, "compass_dle_annual.rds"))
cat("Saved: compass_dle_annual.rds (", nrow(dle_annual), " rows )\n")

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
# denominator used in P_new_outcome_figures. pop2020_total is the 10-region sum,
# used to normalise the aggregate ("World") row.
pop2020_r10 <- pop_ts %>%
  filter(Region %in% regions_r10, Year == 2020) %>%
  group_by(Region) %>%
  summarise(pop_mln = median(Value, na.rm = TRUE), .groups = "drop")
pop2020_total <- sum(pop2020_r10$pop_mln, na.rm = TRUE)
cat("2020 population (mln) by region:\n"); print(as.data.frame(pop2020_r10))
cat("10-region total (mln):", round(pop2020_total, 0), "\n")

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
    total_energy_jobyears_per_1k = if ("jobs_EnergyTotal" %in% names(.)) jobs_EnergyTotal / pop_mln else NA_real_,
    total_energy_jobyears_no_decommission_per_1k = if ("jobs_EnergyTotal_no_decommission" %in% names(.)) jobs_EnergyTotal_no_decommission / pop_mln else NA_real_,
    decommission_jobyears_per_1k = if ("jobs_Decommission" %in% names(.)) jobs_Decommission / pop_mln else NA_real_,
    gross_plant_jobs_displaced_per_1k = if ("gross_plant_jobs_displaced" %in% names(.)) gross_plant_jobs_displaced / pop_mln else NA_real_,
    gross_upstream_jobs_displaced_per_1k = if ("gross_upstream_jobs_displaced" %in% names(.)) gross_upstream_jobs_displaced / pop_mln else NA_real_,
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
  } else if (method == "sci_gw") {
    # SCI's OWN peak-warming scheme (Tier I: GW0-GW8), sourced directly from
    # SCI rather than a self-chosen cutoff. See SCI_GW_COL note above.
    if (is.na(SCI_GW_COL)) {
      warning("No SCI GW-tier column found; falling back to AR6 ambition.")
      return(assign_ambition(df, "ar6"))
    }
    df %>% left_join(meta_lookup %>% select(Model, Scenario, sci_gw),
                     by = c("Model", "Scenario")) %>%
      mutate(Ambition = case_when(
        sci_gw %in% c("GW0", "GW1", "GW2") ~ AMB_15C,
        sci_gw == "GW3"                    ~ AMB_2C,
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
# High-CMT / High-RE from World-equivalent deployment, top `top_frac` within
# ambition. The primary engineered-CMT run may instead impose bottom-tercile
# ceilings on the opposing axis, creating genuinely opposing portfolios.
classify_pathways <- function(scen_set, ambition_method, top_frac = TOP_FRAC) {
  wcmt <- deploy_metrics %>%
    filter(Variable == CMT_AXIS_VAR) %>%
    semi_join(scen_set, by = c("Model", "Scenario")) %>%
    group_by(Model, Scenario, Category) %>%
    summarise(cmt_score = sum(Total_Value, na.rm = TRUE), .groups = "drop")
  wre <- deploy_metrics %>%
    filter(Variable == "Renewable Capacity") %>%
    semi_join(scen_set, by = c("Model", "Scenario")) %>%
    group_by(Model, Scenario, Category) %>%
    summarise(total_re = sum(Total_Value, na.rm = TRUE), .groups = "drop")

  full_join(wcmt, wre, by = c("Model", "Scenario", "Category")) %>%
    assign_ambition(ambition_method) %>%
    filter(!is.na(Ambition)) %>%
    group_by(Ambition) %>%
    mutate(
      cmt_thresh = quantile(cmt_score, 1 - top_frac, na.rm = TRUE),
      cmt_floor  = quantile(cmt_score, top_frac, na.rm = TRUE),
      re_thresh  = quantile(total_re,  1 - top_frac, na.rm = TRUE),
      re_floor   = quantile(total_re, top_frac, na.rm = TRUE),
      high_cdr   = cmt_score >= cmt_thresh,
      high_re    = total_re  >= re_thresh,
      low_cdr = cmt_score <= cmt_floor,
      low_re  = total_re <= re_floor,
      high_cdr_only = if (PORTFOLIO_RULE == "opposing_terciles")
        high_cdr & low_re else high_cdr & !high_re,
      high_re_only  = if (PORTFOLIO_RULE == "opposing_terciles")
        high_re & low_cdr else high_re & !high_cdr,
      Pathway_overlap = case_when(
        high_cdr_only ~ "High-CMT only",
        high_re_only  ~ "High-RE only",
        high_cdr & high_re ~ "Both High",
        low_cdr & low_re ~ "Both Low",
        TRUE ~ "Intermediate / mixed"),
      Pathway_excl  = case_when(high_cdr_only ~ "High-CMT",
                                high_re_only  ~ "High-RE",
                                TRUE ~ NA_character_),
      threshold_label = if (PORTFOLIO_RULE == "opposing_terciles")
        paste0("opposing_", round(top_frac * 100), "pct") else
        paste0("top_", round(top_frac * 100), "pct"),
      cmt_axis = CMT_AXIS_VAR,
      portfolio_rule = PORTFOLIO_RULE
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

  # Mortality is an ANNUAL flow reported by rfasst at decadal scenario years.
  # Integrate linearly between the sampled annual values.  This gives the area
  # under the 2020--2050 mortality trajectory (30 year-equivalents), instead of
  # multiplying all four endpoints by ten (40 year-equivalents).  A missing
  # annual estimate remains missing; it must never become a zero-death result.
  mort_cum <- mortality_annual %>%
    inner_join(amb_map, by = c("Model", "Scenario")) %>%
    filter(Year >= START_YEAR, Year <= window_end) %>%
    group_by(Model, Scenario, Region) %>%
    summarise(cumulative_deaths_mln = {
      ord <- order(Year)
      yr  <- Year[ord]
      dth <- deaths_annual[ord]
      if (length(dth) < 2L || anyNA(dth)) NA_real_
      else sum((dth[-1L] + dth[-length(dth)]) / 2 * diff(yr)) / 1e6
    },
              .groups = "drop")

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
    mutate(
      # Preserve the physical four-way grouping before jobs_Renewables is
      # redefined to match the active pathway-classification specification.
      jobs_BaseRenewables = jobs_Renewables,
      jobs_EnergyTotal_no_decommission = jobs_BaseRenewables +
        jobs_Nuclear + jobs_Bioenergy + jobs_Fossil,
      jobs_Renewables = rowSums(
        across(all_of(paste0("jobs_", jobs_re_group))), na.rm = TRUE))

  # Decommissioning is a genuine positive, one-time employment stream.
  # Displaced jobs are gross transition counts, not additional negative
  # job-years; lower installed stock already reduces ongoing employment.
  transition_cum <- retirement_jobs_annual %>%
    inner_join(amb_map, by = c("Model", "Scenario", "Category")) %>%
    filter(Year >= START_YEAR, Year <= window_end) %>%
    group_by(Model, Scenario, Region) %>%
    summarise(
      jobs_Decommission = sum(decommission_jobyears_thousands, na.rm = TRUE),
      gross_plant_jobs_displaced =
        sum(plant_jobs_displaced_thousands, na.rm = TRUE),
      gross_upstream_jobs_displaced =
        sum(upstream_jobs_displaced_thousands, na.rm = TRUE),
      inferred_retirements_GW =
        sum(inferred_retirements_GW, na.rm = TRUE),
      additions_stock_balance_residual_GW =
        sum(abs(balance_residual_GW), na.rm = TRUE),
      unexplained_additions_GW =
        sum(unexplained_additions_GW, na.rm = TRUE),
      .groups = "drop")

  jobs_cum <- jobs_cum %>%
    left_join(transition_cum,
              by = c("Model", "Scenario", "Region")) %>%
    mutate(across(c(jobs_Decommission, gross_plant_jobs_displaced,
                    gross_upstream_jobs_displaced, inferred_retirements_GW,
                    additions_stock_balance_residual_GW,
                    unexplained_additions_GW),
                  ~ coalesce(.x, 0)),
           jobs_EnergyTotal = jobs_EnergyTotal_no_decommission +
             jobs_Decommission)

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
                                  jobs_Nuclear, jobs_Bioenergy,
                                  jobs_BaseRenewables,
                                  jobs_EnergyTotal_no_decommission,
                                  jobs_Decommission, jobs_EnergyTotal,
                                  gross_plant_jobs_displaced,
                                  gross_upstream_jobs_displaced,
                                  inferred_retirements_GW,
                                  additions_stock_balance_residual_GW,
                                  unexplained_additions_GW),
              by = c("Model", "Scenario", "Region")) %>%
    left_join(dle_cum, by = c("Model", "Scenario", "Region")) %>%
    left_join(pop2020_r10, by = "Region") %>%
    add_percapita()

  # ---------------------------------------------------------------------------
  # Aggregate ("World" = 10-region sum) row — STRICT TEN-REGION RULE.
  #
  # WHY THIS IS NOT A PLAIN group_by(...) %>% summarise(sum). The previous
  # version grouped by `Variable` and summed the outcome columns inside each
  # group. Because `dfm` carries one row per scenario x region x DEPLOYMENT
  # VARIABLE, that made a scenario's World outcome inherit the regional coverage
  # of whichever CDR variable the row belonged to. COFFEE 1.1 / COMMIT-Baseline
  # reports Renewable Capacity for ten regions and Total CDR for nine, so its
  # World jobs read 765,457 on one row and 684,824 on the other. Every one of the
  # 288 affected scenario-regions showed exactly that pattern.
  #
  # The rule now is: a World value exists only when ALL TEN R10 regions are
  # present for THAT outcome, and each outcome is gated INDEPENDENTLY — missing
  # mortality must not blank jobs or deprivation, since their coverage differs
  # substantially (at 1.5C, 53 scenarios have complete deprivation against 42
  # with complete jobs). Partial sums are set to NA rather than reported.
  #
  # Coverage is carried explicitly (n_regions_* and world_complete_*) so that
  # downstream code and the paper can report the coverage flow rather than
  # silently inheriting whatever survived.
  # ---------------------------------------------------------------------------
  n_r10 <- length(regions_r10)   # the ten R10 regions, defined at the top of the script

  # Sum a column only if it is present in all ten regions; otherwise NA.
  strict10 <- function(x) if (sum(!is.na(x)) == n_r10) sum(x) else NA_real_

  # 1. OUTCOME-ONLY R10 TABLE — one row per scenario x region, no Variable.
  #    The outcome columns are constant across a scenario-region's Variable rows
  #    (they are attached by left_join before this point), so de-duplicating is
  #    lossless. Asserted below rather than assumed.
  outcome_cols <- c("cumulative_deaths_mln", "jobs_Renewables", "jobs_Fossil",
                    "jobs_Nuclear", "jobs_Bioenergy", "jobs_BaseRenewables",
                    "jobs_EnergyTotal_no_decommission", "jobs_Decommission",
                    "jobs_EnergyTotal", "gross_plant_jobs_displaced",
                    "gross_upstream_jobs_displaced", "inferred_retirements_GW",
                    "additions_stock_balance_residual_GW",
                    "unexplained_additions_GW",
                    "cumulative_gap_EJ", "mean_headcount_millions",
                    "cumulative_implied_CO2_GtCO2")
  keys <- c("Model_Group", "Model", "Scenario", "ModelGroup_Scenario",
            "Category", "Ambition")
  dfm_out <- dfm %>%
    select(all_of(keys), Region, any_of(outcome_cols)) %>%
    distinct()
  n_dup <- dfm_out %>% count(Model, Scenario, Region) %>% filter(n > 1) %>% nrow()
  if (n_dup > 0)
    stop("build_df_master: ", n_dup, " scenario-regions carry conflicting ",
         "outcome values across Variable rows. The World aggregation cannot be ",
         "made well-defined until that is resolved upstream.")

  # 2. PER-OUTCOME STRICT AGGREGATION, with coverage fields.
  #    Jobs requires BOTH the renewable and fossil components, because the
  #    headline measure is their difference.
  dfm_world_out <- dfm_out %>%
    group_by(across(all_of(keys))) %>%
    summarise(
      n_regions_jobs      = sum(!is.na(jobs_Renewables) & !is.na(jobs_Fossil)),
      n_regions_gap       = sum(!is.na(cumulative_gap_EJ)),
      n_regions_headcount = sum(!is.na(mean_headcount_millions)),
      n_regions_mortality = sum(!is.na(cumulative_deaths_mln)),
      n_regions_co2       = sum(!is.na(cumulative_implied_CO2_GtCO2)),
      jobs_ok = n_regions_jobs == n_r10,
      across(c(jobs_Renewables, jobs_Fossil, jobs_Nuclear, jobs_Bioenergy,
               jobs_BaseRenewables, jobs_EnergyTotal_no_decommission,
               jobs_Decommission, jobs_EnergyTotal,
               gross_plant_jobs_displaced,
               gross_upstream_jobs_displaced, inferred_retirements_GW,
               additions_stock_balance_residual_GW,
               unexplained_additions_GW),
             ~ if (jobs_ok) sum(.x, na.rm = TRUE) else NA_real_),
      cumulative_gap_EJ            = strict10(cumulative_gap_EJ),
      mean_headcount_millions      = strict10(mean_headcount_millions),
      cumulative_deaths_mln        = strict10(cumulative_deaths_mln),
      cumulative_implied_CO2_GtCO2 = strict10(cumulative_implied_CO2_GtCO2),
      .groups = "drop") %>%
    mutate(world_complete_jobs      = n_regions_jobs      == n_r10,
           world_complete_gap       = n_regions_gap       == n_r10,
           world_complete_headcount = n_regions_headcount == n_r10,
           world_complete_mortality = n_regions_mortality == n_r10) %>%
    select(-jobs_ok)

  # 3. WORLD DEPLOYMENT, gated the same way. A World deployment total summed
  #    over nine regions is the same class of error one level up, and pairing it
  #    with a true ten-region World outcome would be worse than either alone.
  dfm_world_dep <- dfm %>%
    group_by(across(all_of(keys)), Variable) %>%
    summarise(n_regions_deployment = sum(!is.na(Total_Value)),
              Total_Value = strict10(Total_Value), .groups = "drop") %>%
    mutate(world_complete_deployment = n_regions_deployment == n_r10)

  # 4. JOIN, and normalise by the ten-region population total.
  dfm_agg <- dfm_world_dep %>%
    left_join(dfm_world_out, by = keys) %>%
    mutate(Region = "Aggregated R10 regions", pop_mln = pop2020_total) %>%
    add_percapita()

  cat(sprintf(
    "  World rows: %d | complete jobs %d, deprivation %d, mortality %d, deployment %d\n",
    nrow(dfm_agg), sum(dfm_agg$world_complete_jobs, na.rm = TRUE),
    sum(dfm_agg$world_complete_gap, na.rm = TRUE),
    sum(dfm_agg$world_complete_mortality, na.rm = TRUE),
    sum(dfm_agg$world_complete_deployment, na.rm = TRUE)))

  bind_rows(dfm, dfm_agg)
}


# =============================================================================
# SECTION 6: RUN ALL APPROACHES (A-E tercile, F-J median, M-Q quintile,
# K-L SCI GW-tier tercile)
# =============================================================================

cat("\n=== SECTION 6: Running configured approaches ===\n")

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
              cmt_score, total_re, cmt_axis, portfolio_rule,
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
cat("Per-approach outputs:  ", file.path(OUT_DIR, "approach_<id>"), "\n")
cat("Comparison outputs:    ", comp_dir, "\n")
cat("\nNext step: point the figure scripts at one approach's subfolder, e.g.\n")
cat('  df_master <- readRDS(file.path(OUT_DIR, "approach_C",\n')
cat('                                 "compass_master_dataset_C.rds"))\n')
