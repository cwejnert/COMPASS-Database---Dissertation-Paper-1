# =============================================================================
# HARMONISED (NO-AMMONIA) MORTALITY RUN — replaces nh3_run_checked.R
#
# WHY THIS EXISTS. nh3_run_checked.R failed at:
#     Error in flip("TRUE") : no unique 'DROP_NH3 <- ...' line found
# because it inherited the old design: edit a DROP_NH3 flag into the rfasst
# script (patch 01d), flip the text of that line, re-source the whole file.
# That flag is not present, so patch 01d never took on this copy.
#
# THAT IS ALSO THE LIKELIEST EXPLANATION OF THE BYTE-IDENTICAL FILES. If 01d
# never applied, the original 03_nh3_run.R hit the same error, and whatever
# produced compass_mortality_summary_noNH3.rds was not a no-ammonia run.
#
# So this version drops the flag entirely. The probe already proved we can zero
# ammonia directly in em_clean and drive rfasst ourselves -- no patching, no
# text-editing of source files, nothing to leave in a half-flipped state.
#
# It is also much faster: it runs only the CLASSIFIED scenarios (the ~590 the
# paper actually compares) rather than all 1,543, at roughly 0.15 min each.
# Budget about 75 minutes rather than five hours.
#
# It writes compass_mortality_r10_noNH3.csv in EXACTLY the schema the master
# produces -- model, scenario, Category, r10_region, year, deaths_pm25 -- by
# reusing the master's own aggregation (FUSION summed over fasst_to_r10).
# It never touches the main outputs.
#
# USAGE: setwd() to the folder holding the rfasst script, then
#   source("nh3_harmonised_run.R")
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr)})
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")
t_start <- Sys.time()

COMPASS_DIR <- Sys.getenv("COMPASS_DIR",
                 "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/COMPASS")
CANDIDATES  <- c("COMPASS_rfasst_full_allR10.R", "COMPASS_rfasst_full.R")
RFASST      <- CANDIDATES[file.exists(CANDIDATES)][1]
if (is.na(RFASST)) stop("No rfasst script found in ", getwd())
OUT_CSV <- file.path(COMPASS_DIR, "compass_mortality_r10_noNH3.csv")

# ------------------------------- 1. definitions, not the batch loop ----------
line("1. LOADING DEFINITIONS")
src <- readLines(RFASST, warn = FALSE)
cut <- grep("SECTION 5", src)
if (!length(cut)) stop("No 'SECTION 5' marker; refusing to run the whole batch.")
prefix <- tempfile(fileext = ".R")
writeLines(src[seq_len(cut[1] - 1)], prefix)
source(prefix, local = FALSE)
for (need in c("em_clean","build_em_list","run_rfasst_for_scenario",
               "COMPASS_R10_REGIONS","FASST_YEARS","fasst_to_r10"))
  if (!exists(need)) stop("missing after sourcing the prefix: ", need)
cat("script:", normalizePath(RFASST), "\n")
cat("lines:", length(src), "| scenarios available:",
    n_distinct(paste(em_clean$model, em_clean$scenario)), "\n")

# THE INVARIANT THAT MATTERS. Two copies of this script exist in different
# folders -- a 1,363-line version that produced the main mortality file (10 R10
# regions, 1,337 scenarios) and a 1,237-line version in the COMPASS data
# directory that yields half the em_clean rows and only five regions downstream.
# Line counts are a symptom; region coverage in em_clean is the thing to assert.
have <- intersect(COMPASS_R10_REGIONS, unique(em_clean$region))
cat("R10 regions present in em_clean:", length(have), "of 10\n")
if (length(have) < 10)
  stop("em_clean covers only ", length(have), " R10 regions (missing: ",
       paste(setdiff(COMPASS_R10_REGIONS, have), collapse = ", "), ").\n",
       "  You are almost certainly running from the wrong folder. Use the copy ",
       "of the rfasst script that produced compass_mortality_r10.csv -- the one ",
       "with ~1,363 lines and ~714,400 em_clean rows, NOT the copy in the ",
       "COMPASS data directory.")
cat("em_clean rows:", nrow(em_clean), "(the correct script gives ~714,400)\n")

# ------------------------------- 2. which scenarios do we actually need? -----
line("2. RESTRICTING TO THE CLASSIFIED SCENARIOS")
pw_path <- file.path(COMPASS_DIR, "compass_pathway_tercile_A.rds")
if (!file.exists(pw_path)) stop("Not found: ", pw_path)
want <- readRDS(pw_path) %>% filter(!is.na(Pathway_excl)) %>%
  distinct(model = Model, scenario = Scenario)
todo <- em_clean %>% distinct(model, scenario) %>% semi_join(want, by = c("model","scenario"))
cat("classified scenarios:", nrow(want), "| present in em_clean:", nrow(todo), "\n")
cat("estimated runtime:", round(nrow(todo) * 0.15), "minutes\n")
if (!nrow(todo)) stop("No classified scenario is present in em_clean.")

# ------------------------------- 3. zero ammonia, and PROVE it ---------------
line("3. ZEROING AMMONIA FOR EVERY MODEL")
nh3_before <- sum(em_clean$value_kt[em_clean$pollutant == "NH3"], na.rm = TRUE)
em_clean <- em_clean %>%
  mutate(value_kt = ifelse(pollutant == "NH3", 0, value_kt))
nh3_after <- sum(em_clean$value_kt[em_clean$pollutant == "NH3"], na.rm = TRUE)
cat(sprintf("total NH3 before: %.4g kg | after: %.4g kg\n", nh3_before, nh3_after))
if (!(nh3_before > 0 && nh3_after == 0))
  stop("ammonia was not zeroed. Refusing to run -- this is exactly the silent ",
       "no-op that produced two identical files last time.")
cat("[ok] ammonia removed uniformly, for every model\n")

# ------------------------------- 4. run ---------------------------------------
line("4. RUNNING")
res <- list(); errs <- character()
for (i in seq_len(nrow(todo))) {
  mod <- todo$model[i]; scen <- todo$scenario[i]
  if (i %% 25 == 0 || i == 1)
    cat(sprintf("  [%d/%d] %.0f min elapsed\n", i, nrow(todo),
                as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
  em <- em_clean %>% filter(model == mod, scenario == scen)
  if (!nrow(em)) { errs <- c(errs, paste(mod, scen, "no emissions")); next }
  req  <- c("SO2","NOX","BC","OM","NH3","VOC","CH4","CO")
  miss <- setdiff(req, unique(em$pollutant))
  if (length(miss)) em <- bind_rows(em, crossing(
    region = COMPASS_R10_REGIONS, pollutant = miss,
    year = as.integer(FASST_YEARS)) %>%
    mutate(value_kt = 0, model = mod, scenario = scen))
  L <- tryCatch(build_em_list(em), error = function(e) NULL)
  if (is.null(L)) { errs <- c(errs, paste(mod, scen, "em.list")); next }
  r <- tryCatch(run_rfasst_for_scenario(L, scen), error = function(e) NULL)
  if (is.null(r) || is.null(r$pm25)) { errs <- c(errs, paste(mod, scen, "rfasst")); next }
  d <- bind_rows(r$pm25)
  if (!"FUSION" %in% names(d)) { errs <- c(errs, paste(mod, scen, "no FUSION")); next }
  res[[paste(mod, scen, sep = "||")]] <- d %>% mutate(model = mod, scenario = scen)
}
cat("\nsucceeded:", length(res), "| failed:", length(errs), "\n")
if (length(errs)) print(utils::head(errs, 10))
if (!length(res)) stop("nothing to write.")

# ------------------------------- 5. aggregate exactly as the master does -----
line("5. AGGREGATING TO R10")
# Save the RAW per-FASST-region output first. Aggregation is cheap and easy to
# get wrong; the 36-minute rfasst run is neither. With this on disk, a mapping
# mistake costs seconds to fix instead of another full run.
saveRDS(bind_rows(res), file.path(COMPASS_DIR, "noNH3_raw_fasst.rds"))
cat("raw per-FASST-region output saved: noNH3_raw_fasst.rds\n")

# DIAGNOSE the mapping before trusting it. The first attempt produced only five
# R10 regions and mortality that ROSE in three of them -- impossible for removing
# a precursor -- which means the region mapping, not the run, was at fault.
cat("\nfasst_to_r10: rows", nrow(fasst_to_r10),
    "| distinct fasst_region", n_distinct(fasst_to_r10$fasst_region),
    "| distinct r10_region", n_distinct(fasst_to_r10$r10_region), "\n")
dupes <- fasst_to_r10 %>% count(fasst_region) %>% filter(n > 1)
if (nrow(dupes)) {
  cat("[FAIL]", nrow(dupes), "fasst regions map to MORE THAN ONE R10 region.\n")
  cat("       A many-to-many join duplicates rows and inflates the sums.\n")
  print(as.data.frame(dupes))
}
raw_regs <- sort(unique(bind_rows(res)$region))
cat("regions returned by rfasst:", length(raw_regs), "\n")
unmapped <- setdiff(raw_regs, fasst_to_r10$fasst_region)
if (length(unmapped)) {
  cat("[FAIL]", length(unmapped), "returned regions are NOT in fasst_to_r10 and\n")
  cat("       are silently dropped by the filter:\n")
  print(unmapped)
}
# The master uses deaths_pm25 = sum(FUSION) over fasst regions mapped to R10.
# Reproduced verbatim so the output drops into the existing pipeline.
cat_lookup <- em_clean %>% distinct(model, scenario) %>%
  left_join(readRDS(pw_path) %>% distinct(model = Model, scenario = Scenario,
                                          Category), by = c("model","scenario"))
out <- bind_rows(res) %>%
  left_join(fasst_to_r10, by = c("region" = "fasst_region")) %>%
  filter(!is.na(r10_region), r10_region %in% COMPASS_R10_REGIONS) %>%
  group_by(model, scenario, r10_region, year) %>%
  summarise(deaths_pm25 = sum(FUSION, na.rm = TRUE), .groups = "drop") %>%
  left_join(cat_lookup, by = c("model","scenario")) %>%
  select(model, scenario, Category, r10_region, year, deaths_pm25)

cat("rows:", nrow(out), "| scenarios:", n_distinct(paste(out$model, out$scenario)),
    "| regions:", n_distinct(out$r10_region), "\n")
if (n_distinct(out$r10_region) != 10) {
  cat("\n[FAIL] expected 10 R10 regions, got", n_distinct(out$r10_region), "-",
      paste(sort(unique(out$r10_region)), collapse = ", "), "\n")
  cat("The raw output is saved, so this is fixable without re-running rfasst.\n")
  stop("aggregation produced the wrong number of regions; refusing to write.")
}
write.csv(out, OUT_CSV, row.names = FALSE)
cat("written:", OUT_CSV, "\n")

# ------------------------------- 6. sanity check against the main run --------
line("6. DID IT ACTUALLY COME OUT DIFFERENT?")
main <- file.path(COMPASS_DIR, "compass_mortality_r10.csv")
if (file.exists(main)) {
  M <- read.csv(main, stringsAsFactors = FALSE)
  cmp <- out %>% rename(no_nh3 = deaths_pm25) %>%
    inner_join(M %>% select(model, scenario, r10_region, year,
                            with_nh3 = deaths_pm25),
               by = c("model","scenario","r10_region","year")) %>%
    filter(with_nh3 > 0) %>%
    mutate(pct = 100*(no_nh3 - with_nh3)/with_nh3)
  cat("comparable rows:", nrow(cmp), "\n")
  cat(sprintf("median change: %+.2f%% | IQR %+.2f to %+.2f\n",
              median(cmp$pct), quantile(cmp$pct,.25), quantile(cmp$pct,.75)))
  # TM5-FASST sums non-negative source-receptor contributions, so removing a
  # precursor can only LOWER PM2.5. Any increase means the region labels are
  # scrambled, not that ammonia helps.
  up <- mean(cmp$pct > 0.5)
  cat(sprintf("rows where mortality ROSE by >0.5%%: %.1f%%\n", 100*up))
  if (up > 0.02) {
    cat("\n[FAIL] removing ammonia cannot raise PM2.5 mortality. The region\n")
    cat("       mapping is wrong. Do NOT use this file.\n")
  } else if (abs(median(cmp$pct)) < 0.01) {
    cat("\n[WARNING] essentially no change. Investigate before using this file.\n")
  } else {
    cat("\n[ok] the harmonised run differs from the main run, and in the only\n")
    cat("     direction physically possible.\n")
  }
} else cat("main file not found; skipping the comparison.\n")

cat(sprintf("\ntotal runtime: %.0f minutes\n",
            as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
cat("\nSend back compass_mortality_r10_noNH3.csv.\n")
cat("The main outputs were never touched -- nothing to restore.\n")
