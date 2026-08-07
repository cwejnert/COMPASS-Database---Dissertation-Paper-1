# =============================================================================
# warming_threshold_check.R  —  RUN LOCALLY (fast, metadata only)
#
# QUESTION: should WARMING_2C_MAX be raised from 2.0 to ~2.2, to give the 2C
#   group the same overshoot headroom the 1.5C group gets (1.5 -> 1.7)?
#
# WHY IT MATTERS: scenarios peaking above WARMING_2C_MAX are dropped entirely
#   (Ambition = NA), not just relabelled. Approaches B and D use the
#   peak-warming rule, and D is our smallest, least-powered cell -- the one
#   where the DLE gap result flips. Raising the cap would add scenarios there.
#
# THE TEST: which AR6 categories actually occupy the 2.0-2.2 band?
#   - mostly C3/C4  -> raising the cap RECOVERS genuinely 2C-consistent
#                      scenarios that the warming rule wrongly excluded. Good.
#   - mostly C5+    -> raising the cap CONTAMINATES the 2C group with
#                      "limit to 2.5C" pathways. Bad -- keep 2.0.
#
#   (AR6 defines C1/C2 as *returning to* 1.5C, so peak warming legitimately
#    overshoots -- hence 1.7. C3/C4 are defined as *limiting* warming to 2C,
#    so the headroom is not symmetric by design. This script checks whether
#    that theoretical asymmetry actually bites in the COMPASS sample.)
#
# OUTPUT: warming_threshold_check.csv (small -- attach or paste back)
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
})

COMPASS_DIR    <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/COMPASS"
MASTER_OUT_DIR <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_master"
META <- file.path(COMPASS_DIR, "compass_r10_meta.csv")
stopifnot(file.exists(META))

meta <- read.csv(META, stringsAsFactors = FALSE, check.names = FALSE)
names(meta)[tolower(names(meta)) == "model"]    <- "Model"
names(meta)[tolower(names(meta)) == "scenario"] <- "Scenario"

# resolve the same columns the master uses
cat_col <- names(meta)[str_detect(names(meta), "Climate Category\\|AR6 \\[Name\\]")][1]
if (is.na(cat_col)) cat_col <- names(meta)[str_detect(names(meta), "AR6")][1]
pw_cands <- c("Climate Assessment|Peak Warming|Median [MAGICCv7.5.3]",
              "Climate Assessment|Peak Warming|Median [MAGICCv7.6.0]",
              "Climate Assessment|Peak Warming|Median")
pw_col <- pw_cands[pw_cands %in% names(meta)][1]
if (is.na(pw_col)) {
  cand <- names(meta)[str_detect(tolower(names(meta)), "peak") &
                      str_detect(tolower(names(meta)), "warming")]
  pw_col <- cand[str_detect(tolower(cand), "median|50")][1]
  if (is.na(pw_col)) pw_col <- cand[1]
}
cat("AR6 category column :", cat_col, "\n")
cat("peak warming column :", pw_col, "\n\n")
stopifnot(!is.na(cat_col), !is.na(pw_col))

d <- meta %>%
  transmute(Model, Scenario,
            Category = .data[[cat_col]],
            peak = suppressWarnings(as.numeric(.data[[pw_col]]))) %>%
  filter(!is.na(peak)) %>%
  mutate(Category = str_trim(Category),
         band = cut(peak, breaks = c(-Inf, 1.7, 2.0, 2.1, 2.2, 2.3, 2.5, Inf),
                    labels = c("<=1.7", "1.7-2.0", "2.0-2.1", "2.1-2.2",
                               "2.2-2.3", "2.3-2.5", ">2.5"),
                    right = TRUE))

cat("=== AR6 category x peak-warming band (scenario counts) ===\n")
tab <- d %>% count(band, Category) %>%
  pivot_wider(names_from = Category, values_from = n, values_fill = 0) %>%
  arrange(band)
print(as.data.frame(tab), row.names = FALSE)

cat("\n=== THE DECISIVE ROW: what is in 2.0-2.2 (the band a 2.2 cap would add)? ===\n")
add <- d %>% filter(peak > 2.0, peak <= 2.2)
if (nrow(add) == 0) {
  cat("  no scenarios in 2.0-2.2 -- raising the cap would change nothing.\n")
} else {
  comp <- add %>% count(Category) %>% mutate(pct = round(100 * n / sum(n))) %>%
    arrange(desc(n))
  print(as.data.frame(comp), row.names = FALSE)
  c34 <- sum(comp$n[comp$Category %in% c("C3", "C4")])
  cat(sprintf("\n  total scenarios gained: %d\n", nrow(add)))
  cat(sprintf("  of which C3/C4 (2C-consistent): %d (%.0f%%)\n",
              c34, 100 * c34 / nrow(add)))
  cat("  VERDICT: ",
      if (100 * c34 / nrow(add) >= 70)
        "mostly C3/C4 -> raising to 2.2 recovers 2C-consistent scenarios. Defensible.\n"
      else if (100 * c34 / nrow(add) <= 40)
        "mostly non-C3/C4 -> raising to 2.2 contaminates the 2C group. Keep 2.0.\n"
      else "mixed -> judgement call; report both as a sensitivity.\n", sep = "")
}

cat("\n=== how the 2C group size changes with the cap ===\n")
sz <- bind_rows(lapply(c(2.0, 2.1, 2.2, 2.3), function(cap) {
  data.frame(cap = cap,
             n_15C = sum(d$peak <= 1.7),
             n_2C  = sum(d$peak > 1.7 & d$peak <= cap),
             n_dropped = sum(d$peak > cap))
}))
print(as.data.frame(sz), row.names = FALSE)

cat("\n=== for reference: peak warming distribution within each AR6 category ===\n")
print(as.data.frame(d %>% group_by(Category) %>%
  summarise(n = n(), median_peak = round(median(peak), 2),
            p10 = round(quantile(peak, .1), 2),
            p90 = round(quantile(peak, .9), 2), .groups = "drop") %>%
  arrange(Category)), row.names = FALSE)

OUT <- file.path(MASTER_OUT_DIR, "warming_threshold_check.csv")
ok <- tryCatch({ write_csv(d %>% count(band, Category), OUT); TRUE },
               error = function(e) { write_csv(d %>% count(band, Category),
                                               "warming_threshold_check.csv"); FALSE })
if (!ok) OUT <- file.path(getwd(), "warming_threshold_check.csv")
cat("\nWROTE:", normalizePath(OUT, winslash = "/", mustWork = FALSE), "\n")
