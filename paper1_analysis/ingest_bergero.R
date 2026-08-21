# =============================================================================
# INGEST BERGERO / STATE OF CDR SCENARIOS INTO compass_interp.rds
#
# The master reads a single assembled file, compass_interp.rds, with columns
#   Model | Scenario | Region | Category | Variable | Year | Value
# built upstream by COMPASS_data_collection*.R (not in this repo). To add a new
# scenario set we append to that file rather than re-running the collection.
#
# INPUT you supply:
#   1. BERGERO_CSV  — an IAMC-format export (wide years), the standard download
#      from ixmp4 / pyam / a Scenario Explorer. Columns:
#         Model, Scenario, Region, Variable, Unit, 2005, 2010, ... 2100
#   2. BERGERO_META — optional CSV with Model, Scenario and an AR6 climate
#      category column. Without it the scenarios cannot be assigned an ambition
#      class and will be dropped by the master; see the note under step 5.
#
# WHAT THIS SCRIPT DOES NOT DO, deliberately:
#   It does not overwrite compass_interp.rds. It writes compass_interp_plus.rds
#   and leaves the original alone, so the added scenarios can be run as a
#   SEPARATE APPROACH rather than silently folded into approach A.
#
# WHY THAT MATTERS — READ BEFORE USING THE OUTPUT:
#   Classification is a TERCILE WITHIN THE POOLED SAMPLE. Adding a set of
#   scenarios that are CDR-focused by construction raises the carbon-management
#   threshold and reclassifies scenarios already in the sample. The addition is
#   therefore not free: it changes the labels of scenarios it has nothing to do
#   with. Run it as an approach alongside A and report the difference, rather
#   than replacing A.
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(purrr)
})
# base read.csv, not readr: check.names = FALSE is essential here because both
# the year columns ("2020") and the IAMC variable names ("Capacity|Electricity|
# Solar") are mangled by R's default name repair.
read_iamc <- function(path) read.csv(path, stringsAsFactors = FALSE,
                                     check.names = FALSE, na.strings = c("", "NA"))

COMPASS_DIR  <- Sys.getenv("COMPASS_DIR",
                  "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/COMPASS")
BERGERO_CSV  <- Sys.getenv("BERGERO_CSV",  "bergero_scenarios.csv")
BERGERO_META <- Sys.getenv("BERGERO_META", "bergero_meta.csv")   # optional
OUT_FILE     <- file.path(COMPASS_DIR, "compass_interp_plus.rds")

line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

R10 <- c("R10AFRICA","R10CHINA+","R10EUROPE","R10INDIA+","R10LATIN_AM",
         "R10MIDDLE_EAST","R10NORTH_AM","R10PAC_OECD","R10REF_ECON","R10REST_ASIA")

# Everything the master and its two satellite modules need. A scenario missing
# an ESSENTIAL variable cannot be classified; missing an OPTIONAL one simply
# drops out of that one outcome.
ESSENTIAL <- c(
  # renewable-capacity axis (RE_SPEC = "renewables")
  "Capacity|Electricity|Solar", "Capacity|Electricity|Wind",
  "Capacity|Electricity|Hydro", "Capacity|Electricity|Geothermal",
  # carbon-management axis
  "Carbon Removal|Geological Storage|Direct Air Capture",
  "Carbon Capture|Geological Storage|Biomass",
  "Carbon Removal|Enhanced Weathering",
  "Carbon Capture|Energy|Fossil",
  "Carbon Capture|Industrial Processes",
  "Carbon Removal|Land Use",
  # denominators
  "Population"
)
OPTIONAL <- c(
  # jobs needs the full capacity stack, not just the renewable axis
  "Capacity|Electricity|Nuclear", "Capacity|Electricity|Biomass",
  "Capacity|Electricity|Coal", "Capacity|Electricity|Gas", "Capacity|Electricity|Oil",
  # decent living energy
  "Final Energy", "Final Energy|Industry", "Final Energy|Transportation",
  "Emissions|CO2|Energy",
  # TM5-FASST precursors
  "Emissions|Sulfur", "Emissions|NOx", "Emissions|BC", "Emissions|OC",
  "Emissions|CO", "Emissions|NH3", "Emissions|VOC", "Emissions|CH4"
)

# ---------------------------------------------------------------- 1. read ----
line("1. READ THE IAMC EXPORT")
if (!file.exists(BERGERO_CSV))
  stop("Not found: ", BERGERO_CSV,
       "\nSet BERGERO_CSV to an IAMC-format export (wide years).")
raw <- read_iamc(BERGERO_CSV)
names(raw)[tolower(names(raw)) == "model"]    <- "Model"
names(raw)[tolower(names(raw)) == "scenario"] <- "Scenario"
names(raw)[tolower(names(raw)) == "region"]   <- "Region"
names(raw)[tolower(names(raw)) == "variable"] <- "Variable"
names(raw)[tolower(names(raw)) == "unit"]     <- "Unit"
need <- c("Model","Scenario","Region","Variable")
if (!all(need %in% names(raw)))
  stop("Missing IAMC columns: ", paste(setdiff(need, names(raw)), collapse=", "))

year_cols <- grep("^[12][0-9]{3}$", names(raw), value = TRUE)
if (!length(year_cols))
  stop("No year columns found. Expected a WIDE IAMC export with columns like 2020, 2030, ...")
cat("rows", nrow(raw), "| scenarios", n_distinct(paste(raw$Model, raw$Scenario)),
    "| variables", n_distinct(raw$Variable),
    "| years", min(year_cols), "-", max(year_cols), "\n")

long <- raw %>%
  mutate(across(all_of(year_cols), as.numeric)) %>%
  pivot_longer(all_of(year_cols), names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.integer(Year)) %>%
  filter(!is.na(Value))

# ------------------------------------------------------------- 2. regions ----
line("2. REGION COVERAGE")
cat("regions present:\n"); print(sort(unique(long$Region)))
hit <- intersect(unique(long$Region), R10)
cat("\nR10 regions matched:", length(hit), "of 10\n")
if (length(hit) < 10) {
  cat("\nMISSING:", paste(setdiff(R10, hit), collapse = ", "), "\n")
  cat("\nThe analysis is regional. A World-only scenario set cannot enter it.\n")
  cat("If the export uses different region labels, add a mapping below and\n")
  cat("re-run; if it genuinely has no R10 detail, these scenarios can only\n")
  cat("support a World-level sensitivity, not the regional scorecard.\n")
}
# If the export uses its own region names, map them here, e.g.
#   long <- long %>% mutate(Region = recode(Region, "Sub-Saharan Africa" = "R10AFRICA"))
long <- long %>% filter(Region %in% c(R10, "World"))

# ------------------------------------------------------------ 3. coverage ----
line("3. VARIABLE COVERAGE, PER SCENARIO")
cov <- long %>%
  filter(Region %in% R10) %>%
  distinct(Model, Scenario, Variable) %>%
  mutate(kind = case_when(Variable %in% ESSENTIAL ~ "essential",
                          Variable %in% OPTIONAL  ~ "optional",
                          TRUE ~ "other"))
per_scen <- cov %>% filter(kind != "other") %>%
  count(Model, Scenario, kind) %>%
  pivot_wider(names_from = kind, values_from = n, values_fill = 0)
if (!"essential" %in% names(per_scen)) per_scen$essential <- 0L
if (!"optional"  %in% names(per_scen)) per_scen$optional  <- 0L
per_scen <- per_scen %>%
  mutate(essential_pct = round(100*essential/length(ESSENTIAL)),
         optional_pct  = round(100*optional /length(OPTIONAL)),
         usable = essential == length(ESSENTIAL))
cat("scenarios:", nrow(per_scen), "| fully covering the ESSENTIAL set:",
    sum(per_scen$usable), "\n\n")
print(as.data.frame(head(per_scen %>% arrange(essential_pct), 20)))

cat("\nWhich ESSENTIAL variables are missing, and from how many scenarios:\n")
miss <- expand_grid(distinct(per_scen, Model, Scenario), Variable = ESSENTIAL) %>%
  anti_join(cov, by = c("Model","Scenario","Variable")) %>%
  count(Variable, name = "scenarios_missing") %>% arrange(desc(scenarios_missing))
print(if (nrow(miss)) as.data.frame(miss) else "  none - full essential coverage")

cat("\nNOTE ON NOVEL CDR: the carbon-management axis sums land CDR, novel CDR\n")
cat("and fossil CCS with na.rm = TRUE, so a MISSING novel-CDR row is scored as\n")
cat("a real zero. In the existing database only 29% of scenarios report novel\n")
cat("CDR at all, and among those that do it is 43% of the axis. Check the rows\n")
cat("above: if these scenarios report novel CDR and the incumbents do not, the\n")
cat("addition will push them up the carbon-management axis for a reporting\n")
cat("reason rather than a deployment one.\n")

# --------------------------------------------------------- 4. interpolate ----
line("4. INTERPOLATE TO ANNUAL")
# compass_interp is annual; IAMC exports are usually 5- or 10-yearly.
interp_one <- function(d) {
  d <- d %>% arrange(Year)
  if (nrow(d) < 2) return(d)
  yrs <- seq(min(d$Year), max(d$Year))
  tibble(Year = yrs, Value = approx(d$Year, d$Value, xout = yrs)$y)
}
interp <- long %>%
  group_by(Model, Scenario, Region, Variable) %>%
  group_modify(~interp_one(.x)) %>%
  ungroup()
cat("rows after interpolation:", nrow(interp),
    "| years", min(interp$Year), "-", max(interp$Year), "\n")

# -------------------------------------------------------------- 5. category --
line("5. AR6 CLIMATE CATEGORY")
# The master assigns ambition from Category (C1-C4). Without it these scenarios
# are dropped, so this is the step most likely to block the whole ingestion.
if (file.exists(BERGERO_META)) {
  meta <- read_iamc(BERGERO_META)
  names(meta)[tolower(names(meta)) == "model"]    <- "Model"
  names(meta)[tolower(names(meta)) == "scenario"] <- "Scenario"
  cat_col <- names(meta)[str_detect(names(meta), regex("categor", ignore_case = TRUE))][1]
  if (is.na(cat_col)) stop("No category column found in ", BERGERO_META)
  cat("using category column:", cat_col, "\n")
  meta <- meta %>% transmute(Model, Scenario, Category = .data[[cat_col]])
  interp <- interp %>% left_join(meta, by = c("Model","Scenario"))
  n_nocat <- n_distinct(paste(interp$Model, interp$Scenario)[is.na(interp$Category)])
  cat("scenarios with a category:",
      n_distinct(paste(interp$Model, interp$Scenario)) - n_nocat,
      "| without:", n_nocat, "\n")
  print(interp %>% distinct(Model, Scenario, Category) %>% count(Category) %>% as.data.frame())
} else {
  interp$Category <- NA_character_
  cat("NO METADATA FILE. Category is NA for every added scenario, so the master\n")
  cat("will drop them all at the ambition-assignment step.\n\n")
  cat("These scenarios need an AR6 climate category (C1-C4), which comes from a\n")
  cat("climate assessment (MAGICC/FaIR), not from the scenario data itself. Two\n")
  cat("ways to supply it:\n")
  cat("  (a) if the scenarios were AR6-assessed, export the category alongside\n")
  cat("      them and pass it as BERGERO_META;\n")
  cat("  (b) if not, they must be climate-assessed first, or assigned by an\n")
  cat("      explicit rule that is stated in the paper. Do NOT infer C1-C4 from\n")
  cat("      cumulative CO2 without saying so.\n")
}

# ------------------------------------------------------------- 6. append -----
line("6. APPEND")
src <- file.path(COMPASS_DIR, "compass_interp.rds")
if (!file.exists(src)) {
  cat("compass_interp.rds not found at", src, "\n")
  cat("Writing the prepared Bergero block on its own to bergero_interp.rds so it\n")
  cat("can be merged wherever the database actually lives.\n")
  saveRDS(interp %>% mutate(source = "bergero"), "bergero_interp.rds")
} else {
  base <- readRDS(src)
  keep <- intersect(names(base), c("Model","Scenario","Region","Category","Variable","Year","Value"))
  cat("existing scenarios:", n_distinct(paste(base$Model, base$Scenario)), "\n")

  dup <- interp %>% semi_join(distinct(base, Model, Scenario), by = c("Model","Scenario"))
  if (nrow(dup)) {
    cat("WARNING:", n_distinct(paste(dup$Model, dup$Scenario)),
        "added scenarios already exist in the database and will be skipped.\n")
    interp <- interp %>% anti_join(distinct(base, Model, Scenario), by = c("Model","Scenario"))
  }
  out <- bind_rows(
    base   %>% mutate(source = "compass"),
    interp %>% select(any_of(keep)) %>% mutate(source = "bergero")
  )
  cat("combined scenarios:", n_distinct(paste(out$Model, out$Scenario)),
      "(+", n_distinct(paste(interp$Model, interp$Scenario)), ")\n")
  saveRDS(out, OUT_FILE)
  cat("written:", OUT_FILE, "\n")
  cat("\nThe original compass_interp.rds is untouched. To run the added set,\n")
  cat("point COMPASS_master_analysis.R at compass_interp_plus.rds and compare\n")
  cat("the result against approach A rather than replacing it -- the tercile\n")
  cat("thresholds move, so existing scenarios get reclassified too.\n")
}
