# =============================================================================
# W14 — DOES THE POOLED DIRECTION SURVIVE INSIDE A MODEL?
#       (the all-CDR axis, repaired keys — the replacement for W2)
#
# WHY THIS REPLACES W2 RATHER THAN RE-RUNNING IT. W2_within_model.R answers the
# right question but on the superseded design, and it cannot simply be pointed
# at new inputs:
#
#   * it sources stratified.R.fns, which sets WINDOW <- "2020-2050"
#   * load_frame() filters ds_*.rds to Variable == "Total CDR" -- the OLD axis
#   * it takes labels from pw_*.rds$Pathway_excl -- the OLD classification
#   * it compares against FINAL_RESULTS_NH3.rds -- the OLD pooled grid
#   * all five of those .rds inputs are gitignored and live on the analysis
#     machine, so the script is not runnable from a clean checkout at all
#
# This rebuilds the same test on LAND_PRIMARY.rds, which means it needs only
# files that are already in the repository.
#
# THE QUESTION, unchanged from W2. The arms are badly unbalanced by model:
# REMIND is 73% of High-RE and under 1% of High-CDR. So every pooled cell is
# partly a model contrast. Where a family holds BOTH arms with at least MINN
# scenarios each, we can ask it directly: inside that model, holding the
# modelling framework fixed, which pathway does better?
#
# THIS IS NOT THE STRATIFIED TEST. Z2 showed a properly weighted stratified
# estimator is hopelessly underpowered here. This is the weaker but answerable
# question: of the families that can be asked, how many point the same way as
# the pooled result?
#
# Effect size is CLIFF'S DELTA, as in W2. That is deliberate: inside a single
# family the samples are tiny, and the question is genuinely about rank overlap
# ("does this model put High-RE above High-CDR?") rather than about the size of
# the gap. The pooled tables report raw differences because that is what they
# print; this reports delta because that is what it asks.
#
# USAGE: Rscript W14_within_model_landprimary.R      (run from the repo root)
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(purrr)})
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

MINN  <- 3
AXIS  <- "with land"          # the primary axis; set "engineered" for the sensitivity
R10   <- c("R10AFRICA","R10CHINA+","R10EUROPE","R10INDIA+","R10LATIN_AM",
           "R10MIDDLE_EAST","R10NORTH_AM","R10PAC_OECD","R10REF_ECON","R10REST_ASIA")
WORLD <- "Aggregated R10 regions"
ALLR  <- c(WORLD, R10)
DROP  <- "R10PAC_OECD"
OUTS  <- c(net_re_jobs_per_1k="Jobs", gap_GJ_pc="Deprivation", mort_per_1k="Health")
LOWER <- c("gap_GJ_pc","headcount_pct","mort_per_1k")

# Key normalisation: see V6_key_repair.R. Do NOT add enc2utf8() -- in a C locale
# it re-encodes already-UTF-8 bytes as latin1 and breaks the correct side.
DEG  <- "°"
norm <- function(x) {
  x <- gsub("<U+00B0>", DEG, x, fixed = TRUE)
  x <- gsub("\\u00b0",  DEG, x, fixed = TRUE)
  x <- gsub("�",   DEG, x, fixed = TRUE)
  Encoding(x) <- "UTF-8"
  x
}
add_pc <- function(df) df %>% mutate(
  mort_per_1k        = cumulative_deaths_mln   / pop_mln * 1000,
  headcount_pct      = mean_headcount_millions / pop_mln * 100,
  net_re_jobs_per_1k = (jobs_Renewables - jobs_Fossil) / pop_mln,
  gap_GJ_pc          = cumulative_gap_EJ * 1000 / pop_mln)

# Cliff's delta: P(b > a) - P(a > b). Positive means the second sample sits
# above the first. Kept identical to cliff_d() in stratified.R.fns.
cliff_d <- function(a, b) {
  a <- a[!is.na(a)]; b <- b[!is.na(b)]
  if (!length(a) || !length(b)) return(NA_real_)
  m <- outer(b, a, "-")
  (sum(m > 0) - sum(m < 0)) / (length(a) * length(b))
}

# =============================================================================
line("1. THE SCENARIO-LEVEL FRAME, ON THE SAME FOOTING AS THE POOLED GRID")
# =============================================================================
LP  <- readRDS("LAND_PRIMARY.rds")
LAB <- (if (AXIS == "with land") LP$labels_land else LP$labels_eng) %>%
  filter(approach == "A") %>% select(Model, Scenario, Pathway, amb)
cat("axis:", AXIS, "| classified scenarios (approach A):", nrow(LAB), "\n")

ABS <- c("jobs_Renewables","jobs_Fossil","cumulative_gap_EJ",
         "mean_headcount_millions","cumulative_deaths_mln")
RO <- read.csv("master_outputs/approach_A/compass_master_dataset_A.csv",
               stringsAsFactors = FALSE) %>%
  mutate(Model = norm(Model), Scenario = norm(Scenario)) %>%
  filter(Region %in% R10) %>%
  distinct(Model, Scenario, Region, .keep_all = TRUE) %>%
  select(Model, Scenario, Region, pop_mln, all_of(ABS))

# The World row is taken from LAND_PRIMARY's strict table, so the within-model
# test and the pooled tables are looking at exactly the same World values.
WLD <- LP$world %>% filter(approach == "A") %>%
  select(Model, Scenario, Region, net_re_jobs_per_1k, gap_GJ_pc, headcount_pct)

MORT <- read.csv("final_outcomes/mortality_reporting_complete_scenario_values_2020_2100.csv",
                 stringsAsFactors = FALSE) %>%
  filter(approach == "A") %>%
  mutate(Model = norm(Model), Scenario = norm(Scenario)) %>%
  transmute(Model, Scenario, Region, mort_per_1k = cumulative_pm25_deaths_mln)

F <- bind_rows(RO %>% add_pc() %>%
                 select(Model, Scenario, Region, net_re_jobs_per_1k,
                        gap_GJ_pc, headcount_pct),
               WLD) %>%
  full_join(MORT, by = c("Model","Scenario","Region")) %>%
  inner_join(LAB, by = c("Model","Scenario")) %>%
  mutate(fam = sub("[ /-].*$", "", Model))
cat("scenario-region rows:", nrow(F), "| model families:", n_distinct(F$fam), "\n")

# The frame must reproduce the pooled grid, or the two are not comparable.
chk <- expand_grid(Region = ALLR, amb = c("1.5C","2C"), outcome = names(OUTS)) %>%
  pmap_dfr(function(Region, amb, outcome) {
    d <- F[F$Region == Region & F$amb == amb, ]
    a <- d[[outcome]][d$Pathway == "High-CMT"]; b <- d[[outcome]][d$Pathway == "High-RE"]
    a <- a[!is.na(a)]; b <- b[!is.na(b)]
    sgn <- ifelse(outcome %in% LOWER, -1, 1)
    tibble(Region, amb, outcome, n_cmt = length(a), n_re = length(b),
           mine = if (length(a) >= 5 && length(b) >= 5) sgn*(median(b)-median(a)) else NA_real_)
  })
POOL <- LP$grid %>% filter(axis == AXIS, approach == "A", outcome %in% names(OUTS)) %>%
  select(Region, amb, outcome, pooled = gap, sig, pn_cmt = n_cmt, pn_re = n_re)
V <- chk %>% inner_join(POOL, by = c("Region","amb","outcome"))
d <- with(V, ifelse(is.na(mine) & is.na(pooled), 0, abs(mine - pooled)))
cat("reproduces the pooled grid — max |difference|:", signif(max(d, na.rm = TRUE), 3),
    "| arm sizes identical:", all(V$n_cmt == V$pn_cmt & V$n_re == V$pn_re), "\n")
if (max(d, na.rm = TRUE) > 1e-9)
  stop("the within-model frame does not reproduce the pooled grid; stopping rather ",
       "than comparing a within-model result against a differently-built pooled one")
cat("[ok] the two are built on the same numbers.\n")

# =============================================================================
line("2. THE WITHIN-MODEL EFFECT, FAMILY BY FAMILY")
# =============================================================================
within <- expand_grid(Region = ALLR, amb = c("1.5C","2C"), outcome = names(OUTS)) %>%
  pmap_dfr(function(Region, amb, outcome) {
    sgn <- ifelse(outcome %in% LOWER, -1, 1)
    F[F$Region == Region & F$amb == amb, ] %>%
      group_by(fam) %>%
      summarise(na = sum(Pathway == "High-CMT" & !is.na(.data[[outcome]])),
                nb = sum(Pathway == "High-RE"  & !is.na(.data[[outcome]])),
                dlt = if (na >= MINN && nb >= MINN)
                        sgn * cliff_d(.data[[outcome]][Pathway == "High-CMT"],
                                      .data[[outcome]][Pathway == "High-RE"])
                      else NA_real_, .groups = "drop") %>%
      filter(!is.na(dlt)) %>%
      mutate(Region = Region, amb = amb, outcome = outcome)
  })

J <- within %>% inner_join(POOL, by = c("Region","amb","outcome")) %>%
  filter(!is.na(pooled)) %>%
  mutate(agrees = sign(dlt) == sign(pooled))

S <- J %>% group_by(Region, amb, outcome) %>%
  summarise(families = n(), agree = sum(agrees), med_within = median(dlt),
            pooled = pooled[1], sig = sig[1], .groups = "drop") %>%
  mutate(family = OUTS[outcome],
         reg = ifelse(Region == WORLD, "WORLD", sub("^R10", "", Region)),
         conflict = sign(med_within) != sign(pooled))
saveRDS(list(within = J, summary = S, axis = AXIS), "W14_WITHIN.rds")
cat("written: W14_WITHIN.rds\n")

# =============================================================================
line("3. HOW OFTEN CAN THE QUESTION EVEN BE ASKED?")
# =============================================================================
cat("cells where at least one family holds both arms with >= ", MINN, " each: ",
    nrow(S), " of 66\n\n", sep = "")
print(as.data.frame(S %>% count(family, families) %>%
      pivot_wider(names_from = families, values_from = n, values_fill = 0)))
cat("\n(columns are the number of model families available in that cell)\n")

# =============================================================================
line("4. THE HEADLINE — DOES THE POOLED DIRECTION SURVIVE?")
# =============================================================================
print(as.data.frame(S %>% filter(Region != DROP) %>% group_by(family) %>%
  summarise(cells = n(),
            family_comparisons = sum(families),
            agree_rate_pct = round(100 * sum(agree) / sum(families)),
            cells_within_favours_RE = sum(med_within > 0),
            cells_pooled_favours_RE = sum(pooled > 0),
            conflicts = sum(conflict), .groups = "drop")))
cat("\nagree_rate_pct is the share of INDIVIDUAL family-cell comparisons pointing\n")
cat("the same way as the pooled result. conflicts counts cells where the MEDIAN\n")
cat("family disagrees with the pooled direction.\n")

# =============================================================================
line("5. WHERE THE WITHIN-MODEL DIRECTION CONTRADICTS THE POOLED DIRECTION")
# =============================================================================
cf <- S %>% filter(Region != DROP, conflict)
cat(nrow(cf), "of", sum(S$Region != DROP), "cells\n\n")
if (nrow(cf)) print(as.data.frame(cf %>% mutate(across(where(is.numeric), ~round(., 2))) %>%
  select(reg, amb, family, families, agree, med_within, pooled, sig) %>%
  arrange(family, reg)))

# =============================================================================
line("6. THE SIGNIFICANT CELLS — THE ONES THE PAPER ACTUALLY CLAIMS")
# =============================================================================
sg <- S %>% filter(Region != DROP, sig)
cat("significant pooled cells that can be asked within-model:", nrow(sg), "\n")
cat("of which the median family AGREES:", sum(!sg$conflict), "\n\n")
print(as.data.frame(sg %>% mutate(across(where(is.numeric), ~round(., 2))) %>%
  select(reg, amb, family, families, agree, med_within, pooled) %>%
  arrange(family, reg)))

# =============================================================================
line("7. PER-FAMILY VIEW — WHO DISAGREES WITH WHOM")
# =============================================================================
print(as.data.frame(J %>% filter(Region != DROP) %>%
  group_by(fam, outcome) %>%
  summarise(cells = n(), favours_RE = sum(dlt > 0),
            median_delta = round(median(dlt), 2), .groups = "drop") %>%
  mutate(family = OUTS[outcome]) %>% select(-outcome) %>%
  arrange(family, desc(cells))))
cat("\nA family that holds both arms in many cells and points consistently one way\n")
cat("is the strongest available evidence on this question; one that appears in a\n")
cat("handful of cells with saturated deltas (+/-1) is not.\n")
