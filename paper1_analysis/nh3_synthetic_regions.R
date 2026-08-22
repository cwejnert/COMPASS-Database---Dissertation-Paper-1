# =============================================================================
# THE LAST OPEN RISK ON MORTALITY — which scenarios have REAL regional
# emissions, and which had their regional detail manufactured from a World
# total by population weight?
#
# WHY THIS MATTERS, AND ONLY FOR MORTALITY. The rfasst script disaggregates
# World-level emissions to R10 by population weight when a scenario reports
# emissions only at World. Reasonable for a global total, but it means that
# scenario's REGIONAL emissions carry no pathway information: every region gets
# the same per-capita intensity by construction. Feed that into TM5-FASST and
# the regional mortality contrast is partly an artefact of population shares.
# Classification, jobs and deprivation are not exposed -- all 590 classified
# scenarios were verified to carry genuine varying R10 deployment and final
# energy. Emissions are the one input with a World fallback.
#
# ------------------------------------------------------------------ VERSION 2
# The first version looked for a pre-disaggregation frame by name and filtered
# it on short pollutant codes. It returned "not in emissions" for all 590,
# which means the filter matched no rows -- the raw frame almost certainly
# stores AR6 variable names ("Emissions|Sulfur") rather than codes ("SO2").
# Rather than guess at names, this version tests em_clean itself, which is the
# frame that actually feeds rfasst and is guaranteed to exist.
#
# THE TEST, and why it cannot give a false positive. If a scenario's regional
# emissions were produced by splitting one World total on population weights,
# then EVERY pollutant is split on the SAME weights -- so the vector of
# regional shares is IDENTICAL for SO2, NOX, BC, OM, NH3, VOC, CH4 and CO, to
# machine precision. Genuinely reported regional emissions never do that: a
# region's share of global SO2 differs from its share of global black carbon,
# because the underlying sectors differ. So:
#
#     max spread of regional shares across pollutants ~ 0  ->  World-derived
#     visibly non-zero spread                             ->  real regions
#
# No external population data is needed and no object has to be found by name.
#
# USAGE: setwd() to the folder holding the rfasst script (the 1,363-line copy),
# then
#   source("nh3_synthetic_regions.R")
# Reads only. Runs no rfasst. Seconds.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr)})
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

COMPASS_DIR <- Sys.getenv("COMPASS_DIR",
                 "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/COMPASS")
CANDIDATES  <- c("COMPASS_rfasst_full_allR10.R", "COMPASS_rfasst_full.R")
RFASST      <- CANDIDATES[file.exists(CANDIDATES)][1]
if (is.na(RFASST)) stop("No rfasst script found in ", getwd())

line("1. LOADING DEFINITIONS")
src <- readLines(RFASST, warn = FALSE)
cat("script:", normalizePath(RFASST), "| lines:", length(src), "\n")
cut <- grep("SECTION 5", src)
if (!length(cut)) stop("No 'SECTION 5' marker.")
prefix <- tempfile(fileext = ".R")
writeLines(src[seq_len(cut[1] - 1)], prefix)
source(prefix, local = FALSE)
if (!exists("em_clean")) stop("em_clean not created by the prefix.")

have <- intersect(COMPASS_R10_REGIONS, unique(em_clean$region))
cat("em_clean rows:", nrow(em_clean), "| R10 regions:", length(have), "of 10\n")
if (length(have) < 10) stop("wrong copy of the script -- see nh3_harmonised_run.R")

# Show the disaggregation code, so the test below is checked against what the
# script actually does rather than against my description of it.
i <- grep("pop_share|pop_weight|World", src)
cat("\nlines mentioning World or population weighting:\n")
for (x in head(i, 20)) cat(sprintf("  %5d | %s\n", x, substr(trimws(src[x]), 1, 110)))

line("2. WHAT em_clean ACTUALLY HOLDS")
cat("columns:", paste(names(em_clean), collapse = ", "), "\n")
POLCOL <- intersect(c("pollutant","variable","Variable"), names(em_clean))[1]
VALCOL <- intersect(c("value_kt","value","emissions"), names(em_clean))[1]
cat("pollutant column:", POLCOL, "| value column:", VALCOL, "\n")
cat("distinct pollutants:\n"); print(sort(unique(em_clean[[POLCOL]])))
cat("years:", paste(sort(unique(em_clean$year)), collapse = ", "), "\n")

line("3. REAL REGIONS OR A POPULATION SPLIT?")
# Regional shares, per scenario x pollutant x year. A scenario built from a
# World total by population weight gives the SAME share vector for every
# pollutant; real reported data does not.
TESTYR <- 2030
E <- em_clean %>%
  filter(region %in% COMPASS_R10_REGIONS, year == TESTYR) %>%
  mutate(val = .data[[VALCOL]], pol = .data[[POLCOL]]) %>%
  group_by(model, scenario, pol) %>%
  mutate(tot = sum(val, na.rm = TRUE)) %>%
  filter(tot > 0) %>%
  mutate(share = val / tot) %>%
  ungroup()
cat("scenarios with any non-zero", TESTYR, "emissions:",
    n_distinct(paste(E$model, E$scenario)), "\n")

per_scen <- E %>%
  group_by(model, scenario, region) %>%
  summarise(spread = diff(range(share)), n_pol = n(), .groups = "drop") %>%
  group_by(model, scenario) %>%
  summarise(max_spread = max(spread, na.rm = TRUE),
            n_pol = max(n_pol), n_reg = n(), .groups = "drop") %>%
  mutate(source = case_when(
    n_pol < 2       ~ "untestable (one pollutant)",
    max_spread < 1e-9 ~ "World-derived",
    TRUE            ~ "real regions"))

cat("\ndistribution of max spread across pollutants (0 = population split):\n")
print(summary(per_scen$max_spread))
cat("\nall scenarios in em_clean:\n")
print(as.data.frame(per_scen %>% count(source)))

line("4. HOW IT LANDS ACROSS THE TWO ARMS")
pw_path <- file.path(COMPASS_DIR, "compass_pathway_tercile_A.rds")
if (!file.exists(pw_path)) stop("Not found: ", pw_path)
LAB <- readRDS(pw_path) %>% filter(!is.na(Pathway_excl)) %>%
  distinct(model = Model, scenario = Scenario,
           Pathway = Pathway_excl, Ambition) %>%
  mutate(amb = ifelse(grepl("^1\\.5", Ambition), "1.5C", "2C"))

J <- LAB %>% left_join(per_scen, by = c("model","scenario")) %>%
  mutate(source = ifelse(is.na(source), "not in emissions", source))
cat("classified scenarios:", nrow(J), "\n\n")
print(as.data.frame(J %>% count(Pathway, source) %>%
      pivot_wider(names_from = source, values_from = n, values_fill = 0)))
cat("\nby ambition:\n")
print(as.data.frame(J %>% count(amb, Pathway, source) %>%
      pivot_wider(names_from = source, values_from = n, values_fill = 0)))

# THE NUMBER THAT DECIDES IT. Equal shares across arms = noise. Unequal = bias.
sh <- J %>% filter(source %in% c("real regions","World-derived")) %>%
  group_by(Pathway) %>%
  summarise(n = n(), synthetic = sum(source == "World-derived"),
            pct = round(100*mean(source == "World-derived"), 1), .groups = "drop")
cat("\nshare of each arm with manufactured regional detail:\n")
print(as.data.frame(sh))
if (nrow(sh) == 2) {
  gap <- abs(diff(sh$pct))
  cat(sprintf("\ndifference between arms: %.1f percentage points\n", gap))
  if (gap < 5)
    cat("[ok] near-balanced. Synthetic regions add noise, not bias.\n") else
    cat("[ATTENTION] the arms are NOT balanced. Re-cut the regional mortality\n",
        "            cells on real-region scenarios only.\n", sep="")
}

line("5. WRITING THE FLAG")
out <- file.path(COMPASS_DIR, "nh3_scenario_region_source.csv")
write.csv(J %>% select(model, scenario, Pathway, amb, source,
                       max_spread, n_pol, n_reg), out, row.names = FALSE)
cat("written:", out, "\n")
cat("\nIf 'not in emissions' is still large, print head(em_clean) and the first\n")
cat("few rows of the pathway file -- that is a key-format mismatch, not a\n")
cat("coverage problem, and it is fixed in one line.\n")
cat("\nSend back this output and nh3_scenario_region_source.csv.\n")
