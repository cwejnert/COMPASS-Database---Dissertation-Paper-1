# =============================================================================
# DLE THRESHOLD SENSITIVITY HARNESS
#   Shows how robust the High-CDR vs High-RE contrast is to the DLE threshold
#   level. Re-derives the (distributional) DLE gap and the deprivation headcount
#   under several threshold sets, and recomputes the contrast for each.
#
# WHERE THIS RUNS: paste at the END of COMPASS_master_analysis_2.R, i.e. AFTER
#   Section 4 (annual outcome tables built -> object `evt` exists) and AFTER you
#   have applied dle_fix.R (recalibrated `dle_thresholds`, steeper `sef_lookup`,
#   and the distributional gap). It reuses these existing objects:
#     evt                 - per Model/Scenario/Region/Year/Category/sector rows
#                           with energy_GJ_pc and pop_millions   (Section 4c)
#     energy_gini         - Region, gini, sigma_ln                (Section 4c)
#     sef_lookup          - sector, Year, SEF                     (dle_fix.R)
#     window_for_ambition - ambition -> window end year           (Section 5)
#     results[["A"]]$pathway - High-CDR/High-RE classification for approach A
#   Swap approach "A" for whichever sample you want to stress-test.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(purrr); library(stringr)})

# ---- 1. threshold sets to test --------------------------------------------
thr_desire <- dle_thresholds   # DESIRE-based baseline (from dle_fix.R)
thr_old <- tibble::tribble(     # original residential-led table, as an alternative source
  ~Region,        ~res_comm_GJ, ~transport_GJ, ~industry_GJ,
  "R10AFRICA",          12.0,  8.0,  4.5, "R10CHINA+",  18.0, 14.0, 5.0,
  "R10EUROPE",          28.0, 16.0,  8.0, "R10INDIA+",  10.0,  8.5, 4.0,
  "R10NORTH_AM",        35.0, 18.0, 10.0)
scale_thr <- function(t, f) t %>% mutate(across(ends_with("_GJ"), ~ .x * f))
threshold_sets <- list(
  "DESIRE x0.75"             = scale_thr(thr_desire, 0.75),
  "DESIRE x1.00 (baseline)"  = thr_desire,
  "DESIRE x1.25"             = scale_thr(thr_desire, 1.25),
  "Alt: original (residential-led)" = thr_old
)

# ---- 2. energy side (threshold-independent), taken straight from `evt` -----
energy_side <- evt %>%
  distinct(Model, Scenario, Region, Year, Category, sector, energy_GJ_pc, pop_millions)

# ---- 3. recompute annual gap + headcount for ONE threshold table ----------
#     (region-total lognormal, identical to dle_fix.R: distributional gap +
#      headcount share below threshold)
dle_annual_for <- function(thr_tbl) {
  thr_long <- thr_tbl %>%
    pivot_longer(ends_with("_GJ"), names_to = "sector", values_to = "thr_base") %>%
    mutate(sector = str_remove(sector, "_GJ"))
  energy_side %>%
    left_join(thr_long, by = c("Region", "sector")) %>%
    left_join(sef_lookup, by = c("sector", "Year")) %>%
    mutate(threshold_GJ_pc = thr_base * SEF) %>%
    group_by(Model, Scenario, Region, Year, Category, pop_millions) %>%
    summarise(E = sum(energy_GJ_pc, na.rm = TRUE),
              Tt = sum(threshold_GJ_pc, na.rm = TRUE), .groups = "drop") %>%
    left_join(energy_gini, by = "Region") %>%
    mutate(s  = sigma_ln,
           m  = log(pmax(E, 0.01)) - s^2 / 2,
           d1 = (log(pmax(Tt, 0.01)) - m) / s,
           d2 = d1 - s,
           headcount_millions = pnorm(d1) * pop_millions,
           gap_EJ_total = pmax(0, Tt * pnorm(d1) - E * pnorm(d2)) * (pop_millions * 1e6) / 1e9)
}

# ---- 4. cumulate to window + compute the High-CDR vs High-RE contrast ------
cliffs_delta <- function(x, y) {                # x = High-CDR, y = High-RE
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  if (length(x) == 0 || length(y) == 0) return(NA_real_)
  (sum(outer(x, y, ">")) - sum(outer(x, y, "<"))) / (length(x) * length(y))
}

contrast_for <- function(dle_annual_v, pathway_df) {
  amb <- pathway_df %>%
    distinct(Model, Scenario, Ambition, Pathway_excl) %>%
    filter(!is.na(Pathway_excl)) %>%
    mutate(window_end = window_for_ambition(Ambition))
  cum <- dle_annual_v %>%
    inner_join(amb, by = c("Model", "Scenario")) %>%
    filter(Year >= 2020, Year <= window_end) %>%
    # per-scenario: cumulative gap (sum) and mean headcount, pop-weighted across R10
    group_by(Model, Scenario, Ambition, Pathway_excl) %>%
    summarise(gap      = sum(gap_EJ_total, na.rm = TRUE),
              headcount = mean(headcount_millions, na.rm = TRUE), .groups = "drop")
  cum %>% group_by(Ambition) %>% group_modify(~{
    hc <- .x %>% filter(Pathway_excl == "High-CDR")
    hr <- .x %>% filter(Pathway_excl == "High-RE")
    safe_p <- function(a, b) if (length(a) > 1 && length(b) > 1)
      suppressWarnings(wilcox.test(a, b)$p.value) else NA_real_
    tibble(
      n_CDR = nrow(hc), n_RE = nrow(hr),
      gap_delta       = cliffs_delta(hc$gap, hr$gap),
      gap_p           = safe_p(hc$gap, hr$gap),
      headcount_delta = cliffs_delta(hc$headcount, hr$headcount),
      headcount_p     = safe_p(hc$headcount, hr$headcount))
  }) %>% ungroup()
}

# ---- 5. run the sweep -----------------------------------------------------
pathway_A <- results[["A"]]$pathway     # <- choose the sample/approach to stress-test
sweep <- imap_dfr(threshold_sets, function(thr, nm)
  contrast_for(dle_annual_for(thr), pathway_A) %>% mutate(threshold_set = nm, .before = 1))

cat("\n=== DLE threshold sensitivity: High-CDR vs High-RE contrast ===\n")
cat("(gap_delta / headcount_delta = Cliff's delta; >0 High-CDR worse, <0 High-RE worse; * p<0.05)\n\n")
sweep %>%
  mutate(gap = sprintf("%+.2f%s", gap_delta, ifelse(gap_p < 0.05, "*", "")),
         headcount = sprintf("%+.2f%s", headcount_delta, ifelse(headcount_p < 0.05, "*", ""))) %>%
  select(threshold_set, Ambition, n_CDR, n_RE, gap, headcount) %>%
  as.data.frame() %>% print(row.names = FALSE)

# ROBUSTNESS READ: if the SIGN (and ideally significance) of gap_delta and
# headcount_delta is stable across the four threshold sets, the contrast is
# robust to the threshold level and the interpolation uncertainty is not
# driving the result — report this table (or a heatmap of it) in the SI.
readr::write_csv(sweep, "dle_threshold_sensitivity.csv")
cat("\nwrote dle_threshold_sensitivity.csv\n")
