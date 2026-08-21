# =============================================================================
# Ten-region analysis engine — COMPASS Paper 1        [v5, corrected-data build]
#
# WHAT CHANGED FROM v4
#   * "High-CDR" -> "High-CMT". The CDR axis is Land-based + Novel + Fossil CCS,
#     and fossil CCS is the median 54% of it. It is carbon MANAGEMENT, not
#     carbon removal, and the label now says so.
#   * The mortality gate is rebuilt on the run's own coverage file. v4 carried
#     three workarounds for a bug that no longer exists: patch 01e stopped a
#     crashed PM2.5 run from being written as zero, so exact-zero detection
#     (1,155 cells), mortflag_*.rds and em_cov.csv are all obsolete. The gate is
#     now one rule applied to two columns the run itself reports.
#   * Both ranking axes are the ten-region sum (master patch 01c), so the World
#     row and the classification axes finally describe the same geography.
#
# JOBS DEFINITION. The pipeline's `net_re_jobs_per_1k` is (Renewables - Fossil)
# only — it drops nuclear and bioenergy, exactly the two blocks High-CMT builds.
# That is a partial net. The reported measures are built here instead.
# =============================================================================

suppressPackageStartupMessages({library(dplyr); library(tidyr); library(purrr)})

R10_TEN <- c("R10AFRICA","R10CHINA+","R10EUROPE","R10INDIA+","R10LATIN_AM",
             "R10MIDDLE_EAST","R10NORTH_AM","R10PAC_OECD","R10REF_ECON","R10REST_ASIA")

PATHWAYS <- c("High-CMT", "High-RE")

OUTCOMES <- c(
  net_jobs_per_1k     = "NET energy jobs per 1,000 (RE + fossil + nuclear + bioenergy)",
  re_jobs_per_1k      = "Renewable jobs per 1,000",
  fossil_jobs_per_1k  = "Fossil jobs per 1,000",
  nuclear_jobs_per_1k = "Nuclear jobs per 1,000",
  bio_jobs_per_1k     = "Bioenergy jobs per 1,000",
  partial_net_per_1k  = "Partial net (RE minus fossil only) — legacy measure",
  mort_per_1k         = "Mortality (cum. deaths per 1,000)",
  gap_GJ_pc           = "Energy deprivation gap (GJ/capita)",
  headcount_pct       = "Deprivation headcount (% below DLE)")

LOWER_BETTER <- c("mort_per_1k","gap_GJ_pc","headcount_pct","fossil_jobs_per_1k")

# -----------------------------------------------------------------------------
# MORTALITY GATE
#
# A scenario's mortality is usable when both hold:
#   (a) no R10 cell is NA — a crashed TM5-FASST PM2.5 solve now propagates as NA
#       rather than zero, and one missing region makes the scenario total wrong;
#   (b) all six PM2.5 precursors are reported AND non-zero. Reporting a
#       precursor as a column of zeros is not the same as emitting nothing, and
#       an absent precursor understates concentrations without any signal in the
#       output — it produces a plausible, non-constant, wrong number.
#
# min_nonzero = 5 is the sensitivity partner. It readmits the 54 scenarios that
# report five of six (GCAM 36, GCAM-PR 3, REMIND 15) and is the coverage rule
# the DROP_NH3 run would need, since dropping NH3 makes five the ceiling.
# -----------------------------------------------------------------------------
mort_gate <- function(mort_summary, mort_coverage, min_nonzero = 6L) {
  na_ok <- mort_summary %>%
    group_by(Model, Scenario) %>%
    summarise(n_na = sum(is.na(cumulative_deaths_mln_total)), .groups = "drop") %>%
    transmute(Model, Scenario, pass_na = n_na == 0)
  mort_coverage %>%
    transmute(Model = model, Scenario = scenario, n_pm_nonzero) %>%
    right_join(na_ok, by = c("Model","Scenario")) %>%
    transmute(Model, Scenario,
              mort_ok = pass_na & !is.na(n_pm_nonzero) & n_pm_nonzero >= min_nonzero)
}

load_approach <- function(id, dir = ".", min_nonzero = 6L, classify = TRUE,
                          mort_file = "mort_summary.rds") {
  ds <- readRDS(file.path(dir, paste0("ds_", id, ".rds")))
  pw <- readRDS(file.path(dir, paste0("pw_", id, ".rds")))
  ms <- readRDS(file.path(dir, mort_file))
  mc <- readRDS(file.path(dir, "mort_coverage.rds"))

  # WORLD ROW, TEN-REGION RESTRICTION. The 'Aggregated R10 regions' row sums its
  # numerator over only the regions a scenario reports but divides by FULL world
  # population, so a scenario covering 6 of 10 regions is understated per capita.
  # Only scenarios reporting all ten regions give a World row that means the same
  # thing twice. Per-region cells are unaffected and keep the full sample.
  complete <- ds %>%
    filter(Variable == "Total CDR", Region %in% R10_TEN) %>%
    count(Model, Scenario, name = "n_reg") %>%
    filter(n_reg == length(R10_TEN)) %>%
    select(Model, Scenario)
  ds <- ds %>%
    filter(Region != "Aggregated R10 regions" |
           paste(Model, Scenario) %in% paste(complete$Model, complete$Scenario))

  base <- ds %>%
    filter(Variable == "Total CDR") %>%
    mutate(
      nuclear_jobs_per_1k = jobs_Nuclear   / pop_mln,
      bio_jobs_per_1k     = jobs_Bioenergy / pop_mln,
      partial_net_per_1k  = net_re_jobs_per_1k,
      net_jobs_per_1k     = (jobs_Renewables + jobs_Fossil +
                             jobs_Nuclear   + jobs_Bioenergy) / pop_mln) %>%
    left_join(mort_gate(ms, mc, min_nonzero), by = c("Model","Scenario")) %>%
    mutate(mort_per_1k = ifelse(is.na(mort_ok) | !mort_ok, NA_real_, mort_per_1k)) %>%
    select(Model, Scenario, Model_Group, Region, Category, Ambition,
           all_of(names(OUTCOMES)), pop_mln)

  cls <- pw %>%
    filter(!is.na(Pathway_excl)) %>%
    select(Model, Scenario, Pathway = Pathway_excl)

  # classify = FALSE returns every scenario with Pathway = NA, for the
  # alternative classification designs, which label scenarios the global
  # tercile leaves unlabelled and so cannot be built from an inner join.
  out <- if (classify) inner_join(base, cls, by = c("Model","Scenario"))
         else          left_join (base, cls, by = c("Model","Scenario"))
  out %>%
    mutate(Pathway = factor(Pathway, levels = PATHWAYS),
           amb = ifelse(grepl("^1\\.5", Ambition), "1.5C", "2C"),
           # model FAMILY is the stratum: REMIND 2.1 and REMIND 3.0 are versions
           # of one model, and treating them separately would shrink already
           # thin strata for no gain.
           fam = sub("[ /].*$", "", Model))
}

# Cliff's delta: non-parametric effect size, comparable across outcomes whose
# units are not. Signed so positive = High-RE higher (not yet "better").
cliff_d <- function(a, b) {
  a <- a[!is.na(a)]; b <- b[!is.na(b)]
  if (!length(a) || !length(b)) return(NA_real_)
  (sum(outer(b, a, ">")) - sum(outer(b, a, "<"))) / (length(a) * length(b))
}

compare_cell <- function(d, outcome) {
  x <- d[[outcome]]
  a <- x[d$Pathway == "High-CMT"]; b <- x[d$Pathway == "High-RE"]
  a <- a[!is.na(a)]; b <- b[!is.na(b)]
  if (length(a) < 3 || length(b) < 3)
    return(tibble(n_cmt = length(a), n_re = length(b), med_cmt = NA_real_,
                  med_re = NA_real_, pct_diff = NA_real_, p = NA_real_,
                  cliff = NA_real_))
  w <- suppressWarnings(wilcox.test(a, b))
  mc <- median(a); mr <- median(b)
  tibble(n_cmt = length(a), n_re = length(b), med_cmt = mc, med_re = mr,
         pct_diff = if (mc != 0) 100 * (mr - mc) / abs(mc) else NA_real_,
         p = w$p.value, cliff = cliff_d(a, b))
}

compare_all <- function(d, regions = c("Aggregated R10 regions", R10_TEN)) {
  expand_grid(Region = regions, Ambition = sort(unique(d$Ambition)),
              outcome = names(OUTCOMES)) %>%
    pmap_dfr(function(Region, Ambition, outcome) {
      sub <- d[d$Region == Region & d$Ambition == Ambition, ]
      bind_cols(tibble(Region = Region, Ambition = Ambition, outcome = outcome),
                compare_cell(sub, outcome))
    }) %>%
    mutate(label = OUTCOMES[outcome])
}

# Direction follows Cliff's delta, not the medians: the Wilcoxon p-value tests
# rank dominance, so a median-based direction can report a "significant" win for
# the group the test says is worse.
add_fdr <- function(res) {
  res %>%
    mutate(p_fdr = p.adjust(p, method = "BH"),
           favours = case_when(
             is.na(p_fdr) | p_fdr >= 0.05 ~ "ns",
             outcome %in% LOWER_BETTER ~ ifelse(cliff < 0, "High-RE", "High-CMT"),
             TRUE ~ ifelse(cliff > 0, "High-RE", "High-CMT")),
           # DIRECTION, independent of significance. The user's framing: the sign
           # is the finding, the p-value is the robustness check. `favours` above
           # collapses to "ns" and hides the sign; `leans` never does.
           leans = case_when(
             is.na(cliff) ~ NA_character_,
             outcome %in% LOWER_BETTER ~ ifelse(cliff < 0, "High-RE", "High-CMT"),
             TRUE ~ ifelse(cliff > 0, "High-RE", "High-CMT")))
}
