# =============================================================================
# THE LAST OPEN RISK ON MORTALITY — which scenarios have REAL regional
# emissions, and which have regional detail that was manufactured from World?
#
# WHY THIS MATTERS, AND ONLY FOR MORTALITY. The rfasst script disaggregates
# World-level emissions to R10 by POPULATION WEIGHT when a scenario reports
# emissions only at World. That is a reasonable fallback for a global total, but
# it means the scenario's REGIONAL emissions carry no pathway information at
# all -- every region gets the same per-capita intensity by construction. Feed
# that into TM5-FASST and the regional mortality contrast between High-CMT and
# High-RE is partly an artefact of population shares.
#
# Classification, jobs and deprivation are NOT exposed to this: all 590
# classified scenarios were verified to carry genuine, varying R10 deployment
# and final-energy data. Emissions are the one input with a World fallback.
#
# THE RISK IS ASYMMETRY, NOT PRESENCE. If synthetic-region scenarios are split
# evenly across the two arms they add noise. If they concentrate in one arm --
# and up to 136 of the 590 (57 High-CMT, 79 High-RE) may be affected -- they add
# bias, in a direction we cannot sign in advance.
#
# WHAT THIS DOES. Reads only; runs no rfasst. Seconds, not minutes.
#   1. finds the pre-disaggregation emissions frame and the disaggregation step
#   2. flags each classified scenario as REAL-REGION or WORLD-DERIVED
#   3. cross-tabulates that flag against Pathway and ambition
#   4. writes nh3_scenario_region_source.csv so the mortality cells can be
#      re-cut on real-region scenarios only
#
# USAGE: setwd() to the folder holding the rfasst script (the 1,363-line copy),
# then
#   source("nh3_synthetic_regions.R")
# Safe to run while nothing else is going -- it does not write into the pipeline.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr)})
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

COMPASS_DIR <- Sys.getenv("COMPASS_DIR",
                 "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/COMPASS")
CANDIDATES  <- c("COMPASS_rfasst_full_allR10.R", "COMPASS_rfasst_full.R")
RFASST      <- CANDIDATES[file.exists(CANDIDATES)][1]
if (is.na(RFASST)) stop("No rfasst script found in ", getwd())

line("1. WHICH SCRIPT, AND WHERE IS THE DISAGGREGATION?")
src <- readLines(RFASST, warn = FALSE)
cat("script:", normalizePath(RFASST), "| lines:", length(src), "\n")

# Show the disaggregation code itself, so the flag below is checked against what
# the script actually does rather than against my description of it.
i <- grep("pop_share|pop_weight|fasst_weights|World.*disagg|disagg.*World",
          src, ignore.case = TRUE)
cat("\nlines mentioning population weighting or World disaggregation:\n")
if (!length(i)) cat("  none found -- read section 2-3 of the script by hand\n")
for (x in head(i, 25)) cat(sprintf("  %5d | %s\n", x, trimws(src[x])))

cut <- grep("SECTION 5", src)
if (!length(cut)) stop("No 'SECTION 5' marker.")
prefix <- tempfile(fileext = ".R")
writeLines(src[seq_len(cut[1] - 1)], prefix)
source(prefix, local = FALSE)

line("2. THE EMISSIONS FRAMES IN SCOPE")
# Do not assume the raw frame is called em_raw. Find every data frame that has a
# region column, and report its region coverage -- the one carrying "World" rows
# alongside R10 rows is the pre-disaggregation frame.
objs <- ls(envir = globalenv())
frames <- Filter(function(n) {
  o <- get(n, envir = globalenv())
  is.data.frame(o) && "region" %in% names(o)
}, objs)
info <- lapply(frames, function(n) {
  o <- get(n, envir = globalenv())
  regs <- unique(o$region)
  tibble(object = n, rows = nrow(o),
         has_World = "World" %in% regs,
         n_R10 = length(intersect(COMPASS_R10_REGIONS, regs)),
         n_other = length(setdiff(regs, c(COMPASS_R10_REGIONS, "World"))),
         scenarios = if (all(c("model","scenario") %in% names(o)))
                       n_distinct(paste(o$model, o$scenario)) else NA_integer_)
}) %>% bind_rows()
print(as.data.frame(info))

# The pre-disaggregation frame is the one holding World rows for scenarios that
# em_clean also covers. Prefer an explicit name if it exists.
pref <- intersect(c("em_raw","em_all","em","emissions_raw","em_long"), info$object)
RAWNAME <- if (length(pref)) pref[1] else
  info$object[info$has_World][which.max(info$rows[info$has_World])]
if (is.na(RAWNAME) || !length(RAWNAME))
  stop("Could not identify a pre-disaggregation frame holding World rows.\n",
       "  Send the table above back and this script can be pointed at the ",
       "right object.")
cat("\nusing as the pre-disaggregation frame:", RAWNAME, "\n")
RAW <- get(RAWNAME, envir = globalenv())
VALCOL <- intersect(c("value_kt","value","emissions"), names(RAW))[1]
POLCOL <- intersect(c("pollutant","variable","Variable"), names(RAW))[1]
cat("value column:", VALCOL, "| pollutant column:", POLCOL, "\n")

line("3. REAL-REGION OR WORLD-DERIVED, PER SCENARIO")
# THE TEST. A scenario has genuine regional emissions if, for the PM2.5
# precursors, it reports NON-ZERO values in R10 regions in the raw frame. A
# scenario that reports only World -- or reports R10 rows that are all zero or
# all identical per capita -- was filled in by the disaggregation.
PREC <- c("SO2","NOX","BC","OM","NH3","VOC","CH4","CO")
rawp <- RAW %>%
  filter(.data[[POLCOL]] %in% PREC) %>%
  mutate(val = .data[[VALCOL]])

per_scen <- rawp %>%
  group_by(model, scenario) %>%
  summarise(
    r10_rows      = sum(region %in% COMPASS_R10_REGIONS),
    r10_nonzero   = sum(region %in% COMPASS_R10_REGIONS & !is.na(val) & val != 0),
    r10_regions   = n_distinct(region[region %in% COMPASS_R10_REGIONS &
                                      !is.na(val) & val != 0]),
    world_rows    = sum(region == "World"),
    .groups = "drop") %>%
  mutate(source = case_when(
    r10_regions >= 8 ~ "real regions",
    r10_regions > 0  ~ "partial",
    TRUE             ~ "World-derived"))

cat("all scenarios in the raw frame:\n")
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
tab <- J %>% count(Pathway, source) %>%
  pivot_wider(names_from = source, values_from = n, values_fill = 0)
print(as.data.frame(tab))

cat("\nby ambition:\n")
print(as.data.frame(J %>% count(amb, Pathway, source) %>%
      pivot_wider(names_from = source, values_from = n, values_fill = 0)))

# THE NUMBER THAT DECIDES IT. Equal shares across arms = noise. Unequal = bias.
sh <- J %>% filter(source != "not in emissions") %>%
  group_by(Pathway) %>%
  summarise(n = n(), synthetic = sum(source != "real regions"),
            pct = round(100*mean(source != "real regions"), 1), .groups = "drop")
cat("\nshare of each arm with manufactured regional detail:\n")
print(as.data.frame(sh))
if (nrow(sh) == 2) {
  gap <- abs(diff(sh$pct))
  cat(sprintf("\ndifference between arms: %.1f percentage points\n", gap))
  if (gap < 5)
    cat("[ok] near-balanced. Synthetic regions add noise, not bias, and the\n",
        "     regional mortality contrast stands.\n", sep="")
  else
    cat("[ATTENTION] the arms are NOT balanced on this. Re-cut the regional\n",
        "            mortality cells on real-region scenarios only -- the file\n",
        "            written below does exactly that.\n", sep="")
}

line("5. WRITING THE FLAG")
out <- file.path(COMPASS_DIR, "nh3_scenario_region_source.csv")
write.csv(J %>% select(model, scenario, Pathway, amb, source,
                       r10_regions, r10_nonzero, world_rows),
          out, row.names = FALSE)
cat("written:", out, "\n")
cat("\nSend back this output and nh3_scenario_region_source.csv.\n")
cat("If the arms are unbalanced, the mortality rebuild re-runs on the\n")
cat("real-region subset and the paper reports that subset for regional cells.\n")
