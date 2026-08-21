# =============================================================================
# NH3 PROBE — settle the ammonia sensitivity in minutes, not 90.
#
# THE SYMPTOM. compass_mortality_summary.rds and compass_mortality_summary_
# noNH3.rds came back BYTE-IDENTICAL (MD5 bbaa7f67...). A sensitivity that
# changes nothing at all is not a result, it is a failed run.
#
# WHAT THE CODE SAYS SHOULD HAPPEN. Tracing DROP_NH3 through the pipeline:
#   1. patch 01d removes "Emissions|NH3" from pollutant_map
#   2. so NH3 never enters em_clean
#   3. BUT the per-scenario block re-adds it, because required_pols still
#      hardcodes "NH3" and missing pollutants are filled with value_kt = 0
#   4. build_em_list() then completes the grid with 0 for anything absent
# So NH3 does end up at zero, and PM2.5 mortality SHOULD move. It didn't.
#
# Only two explanations survive:
#   (A) the no-NH3 run never actually re-ran, and 03_nh3_run.R copied the
#       untouched main file to the _noNH3 name -- it does that without ever
#       checking the file changed; or
#   (B) NH3 genuinely does not move PM2.5 mortality through this code path,
#       in which case the sensitivity is moot and that is the finding.
#
# THIS SCRIPT DISTINGUISHES THEM. It runs ONE scenario twice -- NH3 as
# reported, then NH3 forced to zero -- and compares. Two scenarios, ~4 minutes
# each, no editing of the rfasst script, no 90-minute batch.
#
# It asserts, before running anything, that the two emission lists ACTUALLY
# DIFFER in NH3. That assertion is the guard that was missing from the original
# run: it makes the silent no-op impossible.
#
# USAGE
#   setwd() to the folder holding the rfasst script, then
#     source("nh3_probe.R")
#   It sources the rfasst script only UP TO Section 5, so all the helpers
#   (em_clean, build_em_list, run_rfasst_for_scenario, fasst_weights) are
#   defined without the full batch loop ever starting.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr)})
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

# ---------------------------------------------------- 0. locate the script ---
CANDIDATES <- c("COMPASS_rfasst_full_allR10.R", "COMPASS_rfasst_full.R")
RFASST <- CANDIDATES[file.exists(CANDIDATES)][1]
if (is.na(RFASST))
  stop("No rfasst script found. Looked for: ", paste(CANDIDATES, collapse=", "),
       "\nsetwd() to the folder that holds it.")
cat("using:", RFASST, "\n")

# ------------------------------------ 1. source the definitions, not the run --
line("1. LOADING DEFINITIONS (everything before the batch loop)")
src  <- readLines(RFASST, warn = FALSE)
cut  <- grep("SECTION 5", src)
if (!length(cut))
  stop("Could not find the 'SECTION 5' marker that starts the batch loop.\n",
       "Without it this probe would run all scenarios. Aborting.")
cut <- cut[1]
cat("truncating at line", cut, "of", length(src), "\n")
prefix <- tempfile(fileext = ".R")
writeLines(src[seq_len(cut - 1)], prefix)
source(prefix, local = FALSE)

for (need in c("em_clean", "build_em_list", "run_rfasst_for_scenario",
               "COMPASS_R10_REGIONS", "FASST_YEARS")) {
  if (!exists(need)) stop("definition missing after sourcing the prefix: ", need)
}
cat("definitions loaded. scenarios available:",
    n_distinct(paste(em_clean$model, em_clean$scenario)), "\n")

# ------------------------------------------------- 2. pick probe scenarios ---
line("2. PICKING TWO SCENARIOS")
# One REMIND and one non-REMIND. REMIND is the family whose NH3 looked ~3% of
# the rfasst base, is 40% of the classified sample, and is ~99% High-RE -- so it
# is exactly where an NH3 problem would flatter High-RE.
avail <- em_clean %>%
  filter(pollutant == "NH3") %>%
  group_by(model, scenario) %>%
  summarise(nh3_total = sum(value_kt, na.rm = TRUE),
            n_reg = n_distinct(region), .groups = "drop") %>%
  filter(n_reg >= 10, nh3_total > 0) %>%
  mutate(fam = sub("[ /].*$", "", model))
if (!nrow(avail)) stop("No scenario reports non-zero NH3 in em_clean. ",
                       "If DROP_NH3 is currently TRUE in the script, set it ",
                       "back to FALSE before probing.")
# a single rfasst pair takes ~0.1 min, so take two from each side rather than one
pick <- bind_rows(
  avail %>% filter(grepl("^REMIND", fam))  %>% slice_max(nh3_total, n = 2),
  avail %>% filter(!grepl("^REMIND", fam)) %>% slice_max(nh3_total, n = 2)
)
print(as.data.frame(pick))
if (!nrow(pick)) stop("Could not select probe scenarios.")

# --------------------------------------------------------- 3. the probe -----
line("3. RUNNING EACH SCENARIO WITH AND WITHOUT NH3")
probe_one <- function(mod, scen) {
  cat("\n---", mod, "|", scen, "---\n")
  em_scen <- em_clean %>% filter(model == mod, scenario == scen)

  # backfill exactly as the main loop does, so the probe and the batch agree
  required_pols <- c("SO2","NOX","BC","OM","NH3","VOC","CH4","CO")
  missing_pols  <- setdiff(required_pols, unique(em_scen$pollutant))
  if (length(missing_pols)) {
    em_scen <- bind_rows(em_scen, crossing(
      region = COMPASS_R10_REGIONS, pollutant = missing_pols,
      year = as.integer(FASST_YEARS)) %>%
      mutate(value_kt = 0, model = mod, scenario = scen))
  }
  em_zero <- em_scen %>% mutate(value_kt = ifelse(pollutant == "NH3", 0, value_kt))

  L_with <- build_em_list(em_scen)
  L_zero <- build_em_list(em_zero)

  # ---- THE ASSERTION THAT WAS MISSING FROM THE ORIGINAL RUN ----------------
  nh3_of <- function(L) sum(vapply(L, function(d)
    sum(d$value[d$pollutant == "NH3"], na.rm = TRUE), numeric(1)))
  a <- nh3_of(L_with); b <- nh3_of(L_zero)
  cat(sprintf("  NH3 in em.list: with = %.4g kg | zeroed = %.4g kg\n", a, b))
  if (!(a > 0 && b == 0)) {
    cat("  [FAIL] the two emission lists do not differ in NH3.\n")
    cat("         Nothing downstream can differ either. Stop here and fix the\n")
    cat("         input before running anything longer.\n")
    return(NULL)
  }
  cat("  [ok]   inputs genuinely differ - any null result below is REAL\n")

  t0 <- Sys.time()
  r_with <- run_rfasst_for_scenario(L_with, paste0(scen, "_withNH3"))
  r_zero <- run_rfasst_for_scenario(L_zero, paste0(scen, "_zeroNH3"))
  cat(sprintf("  ran in %.1f min\n",
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))

  # rfasst::m3_get_mort_pm25 returns one row per region x year x age x disease,
  # with a COLUMN PER CONCENTRATION-RESPONSE FUNCTION: GBD, GEMM and FUSION.
  # There is no single "mortality" column, which is why the first version of
  # this probe found nothing to compare. Sum over age and disease and keep all
  # three CRFs -- the master uses deaths_pm25, which derives from FUSION.
  grab <- function(r) {
    if (is.null(r$pm25)) return(NULL)
    d <- bind_rows(r$pm25)
    rc   <- intersect(c("region","fasst_region","subRegion"), names(d))[1]
    crfs <- intersect(c("GBD","GEMM","FUSION","mort_pm25","value","mort","deaths"),
                      names(d))
    if (is.na(rc) || !length(crfs)) {
      cat("  columns returned:", paste(names(d), collapse=", "), "\n")
      return(NULL)
    }
    d %>% group_by(region = .data[[rc]]) %>%
      summarise(across(all_of(crfs), ~sum(as.numeric(.x), na.rm = TRUE)),
                .groups = "drop") %>%
      pivot_longer(all_of(crfs), names_to = "crf", values_to = "v")
  }
  A <- grab(r_with); B <- grab(r_zero)
  if (is.null(A) || is.null(B)) { cat("  [!] no PM2.5 output returned\n"); return(NULL) }

  cmp <- A %>% rename(with_nh3 = v) %>%
    inner_join(B %>% rename(zero_nh3 = v), by = c("region","crf")) %>%
    mutate(pct = ifelse(with_nh3 > 0, 100*(zero_nh3 - with_nh3)/with_nh3, NA_real_))
  glob <- cmp %>% group_by(crf) %>%
    summarise(w = sum(with_nh3), z = sum(zero_nh3), .groups = "drop") %>%
    mutate(pct = 100*(z - w)/w)
  for (k in seq_len(nrow(glob)))
    cat(sprintf("  %-7s global with NH3: %12.0f | zeroed: %12.0f | change %+7.2f%%\n",
                glob$crf[k], glob$w[k], glob$z[k], glob$pct[k]))

  cmp %>% mutate(model = mod, scenario = scen)
}

OUT <- bind_rows(lapply(seq_len(nrow(pick)),
                        function(i) probe_one(pick$model[i], pick$scenario[i])))

# ---------------------------------------------------------- 4. verdict ------
line("4. VERDICT")
if (!nrow(OUT)) {
  cat("No comparison completed. The [FAIL]/[!] lines above say why.\n")
} else {
  saveRDS(OUT, "nh3_probe_result.rds")
  print(OUT %>% group_by(crf) %>%
        summarise(cells = n(),
                  median_pct  = round(median(pct, na.rm = TRUE), 3),
                  max_abs_pct = round(max(abs(pct), na.rm = TRUE), 3),
                  .groups = "drop") %>% as.data.frame())
  cat("\nby scenario (FUSION, the CRF behind deaths_pm25):\n")
  print(OUT %>% filter(crf == "FUSION") %>% group_by(model, scenario) %>%
        summarise(regions = n(),
                  median_pct  = round(median(pct, na.rm = TRUE), 3),
                  max_abs_pct = round(max(abs(pct), na.rm = TRUE), 3),
                  .groups = "drop") %>% as.data.frame())

  worst <- max(abs(OUT$pct), na.rm = TRUE)
  cat("\nlargest regional change from removing NH3, any CRF:",
      round(worst, 3), "%\n\n")
  if (worst < 0.01) {
    cat("VERDICT: NH3 does not move PM2.5 mortality through this code path.\n")
    cat("The byte-identical files were CORRECT, not a failed run. The ammonia\n")
    cat("sensitivity is moot and should be reported as such: rfasst as invoked\n")
    cat("here is insensitive to NH3, so the REMIND NH3 under-reporting cannot\n")
    cat("be biasing the mortality result. That is a clean answer to the\n")
    cat("question -- but it is also a LIMITATION worth stating, because a PM2.5\n")
    cat("estimate that ignores ammonium nitrate understates secondary aerosol\n")
    cat("everywhere, not just in REMIND.\n")
  } else {
    cat("VERDICT: NH3 DOES move mortality, by up to", round(worst, 2), "% regionally.\n")
    cat("So the batch run is at fault, not the science -- the old 03_nh3_run.R\n")
    cat("copies compass_mortality_summary.rds to the _noNH3 name WITHOUT EVER\n")
    cat("CHECKING IT CHANGED, so a run that silently did nothing produces two\n")
    cat("identical files. Re-run with nh3_run_checked.R, which verifies the\n")
    cat("output was regenerated and refuses to write an identical copy.\n")
  }
}
cat("\nwritten: nh3_probe_result.rds (send this back)\n")
