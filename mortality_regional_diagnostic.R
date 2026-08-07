# =============================================================================
# mortality_regional_diagnostic.R  —  RUN LOCALLY
#
# WHY: PM2.5 mortality shows High-CDR clearly worse in Europe (+36%), China+
#      (+15%) and India+ (+7%), but a NULL result in Africa (-3%, p=0.14) and
#      North America (-3%, p=0.28) -- despite High-CDR carrying 1.4-2.2x more
#      fossil capacity in EVERY region. This script tests why.
#
# HYPOTHESES TESTED:
#   H1 pollutant-pathway composition: is the null an O3-vs-PM2.5 mix effect?
#   H2 emissions decoupling: do the pathways actually differ in the pollutants
#      that drive mortality in Africa / North America?
#   H3 scale confounder: cor(RE capacity, mortality) is POSITIVE in Africa
#      (+0.24) and N.America (+0.35) -- is that just total-energy scale?
#
# INPUTS (edit paths below):
#   compass_mortality_r10.csv        <- COMPASS_rfasst_full.R
#   compass_emissions_raw.csv        <- COMPASS_data_collection*.R
#   compass_pathway_tercile_A.csv    <- COMPASS_master_analysis.R (approach A)
#   compass_interp.rds               (optional; for the scale test)
#
# OUTPUT: mortality_diagnostic_summary.csv  (small -- paste/attach this back)
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
})

# ---- PATHS -------------------------------------------------------------------
# The inputs live in THREE different folders, so this script can be run from any
# working directory -- the absolute paths below are what matter. They default to
# the same directories used by COMPASS_master_analysis.R / COMPASS_rfasst_full.R.
COMPASS_DIR     <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/COMPASS"
MASTER_OUT_DIR  <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_master"
MORT_OUT_DIR    <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_mortality"

# compass_mortality_r10.csv is written to BOTH COMPASS_DIR and the rfasst OUT_DIR;
# take whichever exists and is newest, so a stale copy can't silently win.
.mort_candidates <- c(file.path(COMPASS_DIR,  "compass_mortality_r10.csv"),
                      file.path(MORT_OUT_DIR, "compass_mortality_r10.csv"))
.mort_found <- .mort_candidates[file.exists(.mort_candidates)]
MORT_CSV   <- if (length(.mort_found))
                .mort_found[which.max(file.mtime(.mort_found))] else .mort_candidates[1]

EM_CSV     <- file.path(COMPASS_DIR, "compass_emissions_raw.csv")
PATHWAY_A  <- file.path(MASTER_OUT_DIR, "approach_A", "compass_pathway_tercile_A.csv")
INTERP     <- file.path(COMPASS_DIR, "compass_interp.rds")  # optional; NA to skip H3
# -----------------------------------------------------------------------------

# fail early and clearly if an input is missing, rather than deep in a join
{
  .req <- c(MORT_CSV = MORT_CSV, EM_CSV = EM_CSV, PATHWAY_A = PATHWAY_A)
  .missing <- .req[!file.exists(.req)]
  if (length(.missing)) {
    stop("Missing required input file(s):\n",
         paste0("  ", names(.missing), ": ", .missing, collapse = "\n"),
         "\nEdit the path variables at the top of this script.", call. = FALSE)
  }
  cat("Inputs resolved:\n")
  cat("  mortality :", MORT_CSV, "\n")
  cat("  emissions :", EM_CSV, "\n")
  cat("  pathways  :", PATHWAY_A, "\n")
  cat("  interp    :", if (file.exists(INTERP)) INTERP else "(not found - H3 skipped)", "\n\n")
}

regions_r10 <- c("R10AFRICA","R10CHINA+","R10EUROPE","R10INDIA+","R10NORTH_AM")
# Single common cumulation window for every ambition, matching the master's
# OUTCOME_WINDOW_END (2100) -- see COMPASS_master_analysis.R for rationale.
WIN <- c(`1.5C (High-Ambition)` = 2100L, `2C (Medium-Ambition)` = 2100L)

# helpers -----------------------------------------------------------------
cliffs <- function(x, y) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  if (!length(x) || !length(y)) return(NA_real_)
  (sum(outer(x, y, ">")) - sum(outer(x, y, "<"))) / (length(x) * length(y))
}
mwp <- function(x, y) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  if (length(x) > 1 && length(y) > 1) suppressWarnings(wilcox.test(x, y)$p.value) else NA_real_
}
norm_names <- function(d) {
  n <- names(d)
  n[tolower(n) == "model"]      <- "Model"
  n[tolower(n) == "scenario"]   <- "Scenario"
  n[tolower(n) == "year"]       <- "Year"
  n[tolower(n) == "region"]     <- "Region"
  n[tolower(n) == "r10_region"] <- "Region"
  names(d) <- n; d
}

# ---- pathway labels (approach A) ----------------------------------------
pw_raw <- read.csv(PATHWAY_A, stringsAsFactors = FALSE, check.names = FALSE) %>%
  norm_names()
cat("pathway file columns:", paste(names(pw_raw), collapse = ", "), "\n")

# Pathway label column: prefer the mutually-exclusive one, else fall back
.path_col <- intersect(c("Pathway_excl", "Pathway", "Pathway_overlap"), names(pw_raw))[1]
if (is.na(.path_col))
  stop("No pathway column found (looked for Pathway_excl / Pathway / Pathway_overlap). ",
       "Columns present: ", paste(names(pw_raw), collapse = ", "), call. = FALSE)

# Ambition column: derive from Category if the file doesn't carry it
if (!"Ambition" %in% names(pw_raw)) {
  if (!"Category" %in% names(pw_raw))
    stop("Pathway file has neither 'Ambition' nor 'Category'. Columns: ",
         paste(names(pw_raw), collapse = ", "), call. = FALSE)
  cat("note: 'Ambition' absent - deriving from Category (C1/C2 = 1.5C, C3/C4 = 2C)\n")
  pw_raw$Ambition <- ifelse(pw_raw$Category %in% c("C1","C2"), "1.5C (High-Ambition)",
                     ifelse(pw_raw$Category %in% c("C3","C4"), "2C (Medium-Ambition)", NA))
}

pw <- pw_raw %>%
  transmute(Model, Scenario, Ambition, Pathway_excl = .data[[.path_col]]) %>%
  filter(!is.na(Pathway_excl), Pathway_excl != "",
         Pathway_excl %in% c("High-CDR", "High-RE"))
cat("pathway column used:", .path_col, "| labelled scenarios:", nrow(pw), "\n")
if (nrow(pw) == 0) stop("No High-CDR/High-RE labelled scenarios found.", call. = FALSE)
print(table(pw$Ambition, pw$Pathway_excl))

# =========================================================================
# H1 — PM2.5 vs O3 composition by region x pathway
# =========================================================================
mort <- read_csv(MORT_CSV, show_col_types = FALSE) %>% norm_names()
stopifnot(all(c("Model","Scenario","Region","Year") %in% names(mort)))
has_split <- all(c("deaths_pm25","deaths_o3") %in% names(mort))
cat("pollutant split available:", has_split, "\n")

mort_w <- mort %>%
  filter(Region %in% regions_r10) %>%
  inner_join(pw, by = c("Model","Scenario")) %>%
  mutate(window_end = WIN[Ambition]) %>%
  filter(!is.na(window_end), Year >= 2020, Year <= window_end)

# cumulate (rfasst is decadal -> x10), then per-1000 population later if desired
mort_cum <- mort_w %>%
  group_by(Model, Scenario, Ambition, Pathway_excl, Region) %>%
  summarise(across(any_of(c("deaths_pm25","deaths_o3","deaths_total")),
                   ~ sum(.x * 10, na.rm = TRUE)), .groups = "drop")

h1 <- list()
for (rg in regions_r10) for (amb in names(WIN)) {
  for (v in intersect(c("deaths_pm25","deaths_o3","deaths_total"), names(mort_cum))) {
    s <- mort_cum %>% filter(Region == rg, Ambition == amb)
    hc <- s[[v]][s$Pathway_excl == "High-CDR"]
    hr <- s[[v]][s$Pathway_excl == "High-RE"]
    if (!length(hc) || !length(hr)) next
    h1[[length(h1)+1]] <- data.frame(
      test = "H1_pollutant_pathway", region = sub("R10","",rg),
      ambition = if (grepl("1.5", amb)) "1.5C" else "2C",
      metric = v, CDR_mean = mean(hc, na.rm=TRUE), RE_mean = mean(hr, na.rm=TRUE),
      pct_diff = round(100*(mean(hc,na.rm=TRUE)-mean(hr,na.rm=TRUE))/mean(hr,na.rm=TRUE)),
      cliff = round(cliffs(hc,hr),3), p = mwp(hc,hr), n_CDR = length(hc), n_RE = length(hr))
  }
}
h1 <- bind_rows(h1)
cat("\n=== H1: PM2.5 vs O3 by region (2C) ===\n")
print(as.data.frame(h1 %>% filter(ambition=="2C") %>%
  mutate(across(c(CDR_mean,RE_mean), ~signif(.x,3))) %>%
  select(region, metric, CDR_mean, RE_mean, pct_diff, cliff, p)), row.names = FALSE)

# =========================================================================
# H2 — which POLLUTANTS differ between pathways, per region
# =========================================================================
em <- read_csv(EM_CSV, show_col_types = FALSE) %>% norm_names()
polls <- c("Emissions|Sulfur","Emissions|NOx","Emissions|BC","Emissions|OC",
           "Emissions|CO","Emissions|NH3","Emissions|VOC","Emissions|CH4")
em_w <- em %>%
  filter(Region %in% regions_r10, variable %in% polls) %>%
  inner_join(pw, by = c("Model","Scenario")) %>%
  mutate(Year = as.integer(Year), window_end = WIN[Ambition]) %>%
  filter(!is.na(window_end), Year >= 2020, Year <= window_end) %>%
  group_by(Model, Scenario, Ambition, Pathway_excl, Region, variable) %>%
  summarise(cum_em = sum(as.numeric(value), na.rm = TRUE), .groups = "drop")

h2 <- list()
for (rg in regions_r10) for (pl in polls) {
  s <- em_w %>% filter(Region == rg, variable == pl, grepl("2C", Ambition))
  hc <- s$cum_em[s$Pathway_excl == "High-CDR"]; hr <- s$cum_em[s$Pathway_excl == "High-RE"]
  if (length(hc) < 2 || length(hr) < 2) next
  h2[[length(h2)+1]] <- data.frame(
    test = "H2_emissions", region = sub("R10","",rg), ambition = "2C",
    metric = sub("Emissions\\|","",pl),
    CDR_mean = mean(hc,na.rm=TRUE), RE_mean = mean(hr,na.rm=TRUE),
    pct_diff = round(100*(mean(hc,na.rm=TRUE)-mean(hr,na.rm=TRUE))/mean(hr,na.rm=TRUE)),
    cliff = round(cliffs(hc,hr),3), p = mwp(hc,hr), n_CDR = length(hc), n_RE = length(hr))
}
h2 <- bind_rows(h2)
cat("\n=== H2: pollutant emissions, High-CDR vs High-RE (2C, % diff) ===\n")
print(as.data.frame(h2 %>% select(region, metric, pct_diff, cliff, p) %>%
  pivot_wider(names_from = metric, values_from = c(pct_diff), id_cols = region)), row.names = FALSE)

# correlation of each pollutant with PM2.5 deaths, per region
pm_col <- if ("deaths_pm25" %in% names(mort_cum)) "deaths_pm25" else "deaths_total"
corr <- em_w %>% filter(grepl("2C", Ambition)) %>%
  inner_join(mort_cum %>% filter(grepl("2C", Ambition)) %>%
               select(Model, Scenario, Region, deaths = all_of(pm_col)),
             by = c("Model","Scenario","Region")) %>%
  group_by(Region, variable) %>%
  filter(sum(!is.na(cum_em) & !is.na(deaths)) > 5) %>%
  summarise(cor_with_deaths = round(cor(cum_em, deaths, use = "complete.obs"), 3),
            .groups = "drop") %>%
  mutate(region = sub("R10","",Region), metric = sub("Emissions\\|","",variable))
cat("\n=== H2b: correlation of each pollutant with PM2.5 deaths, by region ===\n")
print(as.data.frame(corr %>% select(region, metric, cor_with_deaths) %>%
  pivot_wider(names_from = metric, values_from = cor_with_deaths)), row.names = FALSE)

# =========================================================================
# H3 — scale confounder: does total final energy drive mortality?
# =========================================================================
h3 <- data.frame()
if (!is.na(INTERP) && file.exists(INTERP)) {
  ci <- readRDS(INTERP)
  fe <- ci %>%
    filter(Region %in% regions_r10, Variable == "Final Energy",
           Year >= 2020, Year <= 2100, !is.na(Value)) %>%
    inner_join(pw, by = c("Model","Scenario")) %>%
    filter(grepl("2C", Ambition)) %>%
    group_by(Model, Scenario, Region, Pathway_excl) %>%
    summarise(cum_fe = sum(Value, na.rm = TRUE), .groups = "drop")
  h3 <- fe %>%
    inner_join(mort_cum %>% filter(grepl("2C", Ambition)) %>%
                 select(Model, Scenario, Region, deaths = all_of(pm_col)),
               by = c("Model","Scenario","Region")) %>%
    group_by(Region) %>%
    summarise(cor_FE_deaths = round(cor(cum_fe, deaths, use="complete.obs"),3),
              FE_CDR = mean(cum_fe[Pathway_excl=="High-CDR"], na.rm=TRUE),
              FE_RE  = mean(cum_fe[Pathway_excl=="High-RE"],  na.rm=TRUE),
              .groups="drop") %>%
    mutate(region = sub("R10","",Region),
           FE_pct_diff = round(100*(FE_CDR-FE_RE)/FE_RE),
           test = "H3_scale", ambition = "2C", metric = "cum_final_energy")
  cat("\n=== H3: total final energy vs mortality (scale confounder) ===\n")
  print(as.data.frame(h3 %>% select(region, cor_FE_deaths, FE_pct_diff)), row.names = FALSE)
} else {
  cat("\n[H3 skipped: compass_interp.rds not found]\n")
}

# ---- write compact summary ----------------------------------------------
out <- bind_rows(
  h1 %>% mutate(across(everything(), as.character)),
  h2 %>% mutate(across(everything(), as.character)),
  corr %>% transmute(test="H2b_cor_pollutant_deaths", region, ambition="2C",
                     metric, cliff=as.character(cor_with_deaths)) %>%
    mutate(across(everything(), as.character)),
  if (nrow(h3)) h3 %>% transmute(test, region, ambition, metric,
      cliff=as.character(cor_FE_deaths), pct_diff=as.character(FE_pct_diff)) %>%
      mutate(across(everything(), as.character)) else NULL
)
OUT_CSV <- file.path(MASTER_OUT_DIR, "mortality_diagnostic_summary.csv")
ok <- tryCatch({ write_csv(out, OUT_CSV); TRUE },
               error = function(e) { write_csv(out, "mortality_diagnostic_summary.csv"); FALSE })
if (!ok) OUT_CSV <- file.path(getwd(), "mortality_diagnostic_summary.csv")
cat("\n===========================================================\n")
cat("WROTE:", normalizePath(OUT_CSV, winslash = "/", mustWork = FALSE), "\n")
cat("rows:", nrow(out), " -- small file, attach or paste this back\n")
cat("===========================================================\n")
