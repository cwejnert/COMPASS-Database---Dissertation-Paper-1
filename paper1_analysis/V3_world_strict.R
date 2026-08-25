# =============================================================================
# V3 — STRICT WORLD AGGREGATION (option C), AND THE RESULT GRID REBUILT ON IT
#
# THE DEFECT BEING FIXED. COMPASS_master_analysis_allR10.R builds the World row
# by summing regional outcomes WITHIN each deployment-variable group:
#
#     dfm_agg <- dfm %>% group_by(..., Variable) %>% summarise(across(outcome_cols, sum))
#
# so a scenario's World jobs total inherits the regional coverage of whichever
# CDR variable that row belongs to. COFFEE 1.1 / COMMIT-Baseline reports
# Renewable Capacity for ten regions and Total CDR for nine, and its World jobs
# reads 765,457 on one row and 684,824 on the other. All 288 discrepant
# scenario-regions have exactly this pattern -- 288 of 288.
#
# THE R10 ROWS ARE CLEAN. Verified: zero scenario-regions show any variation in
# jobs, gap or mortality across Variable rows. The corruption is created
# entirely in the aggregation step, so World can be rebuilt correctly from the
# existing outputs without re-running the master.
#
# WHAT THIS IMPLEMENTS (option C, per the agreed spec):
#
#   1. An outcome-only R10 table, independent of deployment-variable rows.
#   2. Each outcome aggregated SEPARATELY and only when all ten R10 values are
#      present. Missing mortality must not blank jobs or deprivation, and vice
#      versa -- the three have different coverage and are gated independently.
#   3. Strict World outcomes joined back onto the deployment-variable rows.
#   4. Explicit coverage fields: n_regions_jobs / _gap / _headcount / _mortality
#      and world_complete_* for each.
#   5. The World DEPLOYMENT total also requires ten regions, or is set NA --
#      otherwise a row can pair a true World outcome with a partial World
#      deployment, which is the same class of error one level up.
#
# It then reports the coverage flow the paper should carry, and rebuilds the
# result grid with the cluster bootstrap so the corrected World figures can
# replace the current ones.
#
# USAGE: Rscript V3_world_strict.R      (run from the repo root)
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(purrr)})
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")
set.seed(20260825)

B    <- 2000
ROOT <- "."
R10  <- c("R10AFRICA","R10CHINA+","R10EUROPE","R10INDIA+","R10LATIN_AM",
          "R10MIDDLE_EAST","R10NORTH_AM","R10PAC_OECD","R10REF_ECON","R10REST_ASIA")
WORLD <- "Aggregated R10 regions"
ALLR  <- c(WORLD, R10)
SH   <- c(`Aggregated R10 regions`="WORLD", R10AFRICA="Africa", `R10CHINA+`="China+",
          R10EUROPE="Europe", `R10INDIA+`="India+", R10LATIN_AM="Latin America",
          R10MIDDLE_EAST="Middle East", R10NORTH_AM="North America",
          R10PAC_OECD="Pacific OECD", R10REF_ECON="Reforming econ.",
          R10REST_ASIA="Rest of Asia")
OUTS  <- c(net_re_jobs_per_1k="Jobs", gap_GJ_pc="Deprivation",
           headcount_pct="Deprivation headcount", mort_per_1k="Health")
LOWER <- c("gap_GJ_pc","headcount_pct","mort_per_1k")
norm  <- function(x) iconv(x, from = "", to = "UTF-8", sub = "")

# Per-capita formulas taken verbatim from add_percapita() in the master script.
add_pc <- function(df) df %>% mutate(
  mort_per_1k        = cumulative_deaths_mln   / pop_mln * 1000,
  headcount_pct      = mean_headcount_millions / pop_mln * 100,
  net_re_jobs_per_1k = (jobs_Renewables - jobs_Fossil) / pop_mln,
  gap_GJ_pc          = cumulative_gap_EJ * 1000 / pop_mln)

# =============================================================================
line("1. OUTCOME-ONLY R10 TABLE  [spec 1]")
# =============================================================================
# Absolute (additive) outcome columns, one row per Model x Scenario x Region,
# with no reference to Variable at all.
ABS <- c("jobs_Renewables","jobs_Fossil","cumulative_gap_EJ",
         "mean_headcount_millions","cumulative_deaths_mln")

r10_outcomes <- function(id) {
  read.csv(file.path(ROOT, sprintf("master_outputs/approach_%s/compass_master_dataset_%s.csv", id, id)),
           stringsAsFactors = FALSE) %>%
    mutate(Model = norm(Model), Scenario = norm(Scenario), approach = id) %>%
    filter(Region %in% R10) %>%
    distinct(approach, Model, Scenario, Region, .keep_all = TRUE) %>%
    select(approach, Model, Scenario, Region, Ambition, Category, pop_mln, all_of(ABS))
}
RO <- bind_rows(r10_outcomes("A"), r10_outcomes("C"))
cat("R10 outcome rows:", nrow(RO), "| scenarios:",
    n_distinct(paste(RO$approach, RO$Model, RO$Scenario)), "\n")

# The R10 rows must be unique per scenario-region for this to be safe.
dupe <- RO %>% count(approach, Model, Scenario, Region) %>% filter(n > 1)
cat("duplicate scenario-region rows after dedupe:", nrow(dupe),
    ifelse(nrow(dupe) == 0, "  [ok]", "  [FAIL]"), "\n")

# =============================================================================
line("2. PER-OUTCOME STRICT AGGREGATION  [spec 2, 4]")
# =============================================================================
# Each outcome gated INDEPENDENTLY on having all ten regions. Missing mortality
# must not blank jobs or deprivation.
pop_r10 <- RO %>% distinct(Region, pop_mln) %>% group_by(Region) %>%
  summarise(pop_mln = median(pop_mln), .groups = "drop")
POP_TOT <- sum(pop_r10$pop_mln)
cat("ten-region population total (mln):", round(POP_TOT), "\n\n")

strict_sum <- function(v, reg) if (sum(!is.na(v)) == 10) sum(v) else NA_real_

WLD <- RO %>% group_by(approach, Model, Scenario, Ambition, Category) %>%
  summarise(
    n_regions              = n_distinct(Region),
    # jobs needs BOTH components in all ten regions
    n_regions_jobs         = sum(!is.na(jobs_Renewables) & !is.na(jobs_Fossil)),
    n_regions_gap          = sum(!is.na(cumulative_gap_EJ)),
    n_regions_headcount    = sum(!is.na(mean_headcount_millions)),
    n_regions_mortality    = sum(!is.na(cumulative_deaths_mln)),
    jobs_Renewables        = if (n_regions_jobs      == 10) sum(jobs_Renewables[!is.na(jobs_Renewables) & !is.na(jobs_Fossil)]) else NA_real_,
    jobs_Fossil            = if (n_regions_jobs      == 10) sum(jobs_Fossil[!is.na(jobs_Renewables) & !is.na(jobs_Fossil)])     else NA_real_,
    cumulative_gap_EJ      = if (n_regions_gap       == 10) sum(cumulative_gap_EJ, na.rm = TRUE)       else NA_real_,
    mean_headcount_millions= if (n_regions_headcount == 10) sum(mean_headcount_millions, na.rm = TRUE) else NA_real_,
    cumulative_deaths_mln  = if (n_regions_mortality == 10) sum(cumulative_deaths_mln, na.rm = TRUE)   else NA_real_,
    .groups = "drop") %>%
  mutate(Region = WORLD, pop_mln = POP_TOT,
         world_complete_jobs      = n_regions_jobs      == 10,
         world_complete_gap       = n_regions_gap       == 10,
         world_complete_headcount = n_regions_headcount == 10,
         world_complete_mortality = n_regions_mortality == 10) %>%
  add_pc()

cat("World rows built:", nrow(WLD), "\n")
print(as.data.frame(WLD %>% summarise(
  complete_jobs      = sum(world_complete_jobs),
  complete_gap       = sum(world_complete_gap),
  complete_headcount = sum(world_complete_headcount),
  complete_mortality = sum(world_complete_mortality))))
cat("\nNote the three differ — which is exactly why they must be gated separately.\n")

# =============================================================================
line("3. WHAT THE FIX CHANGES AGAINST THE PUBLISHED WORLD ROW  [spec 3]")
# =============================================================================
OLD <- bind_rows(lapply(c("A","C"), function(id)
  read.csv(file.path(ROOT, sprintf("master_outputs/approach_%s/compass_master_dataset_%s.csv", id, id)),
           stringsAsFactors = FALSE) %>%
    mutate(Model = norm(Model), Scenario = norm(Scenario), approach = id) %>%
    filter(Region == WORLD) %>%
    distinct(approach, Model, Scenario, .keep_all = TRUE) %>%
    select(approach, Model, Scenario, old_jobs = net_re_jobs_per_1k,
           old_gap = gap_GJ_pc, old_mort = mort_per_1k)))
CMP <- WLD %>% select(approach, Model, Scenario, new_jobs = net_re_jobs_per_1k,
                      new_gap = gap_GJ_pc, new_mort = mort_per_1k,
                      world_complete_jobs) %>%
  inner_join(OLD, by = c("approach","Model","Scenario"))
cat("scenarios compared:", nrow(CMP), "\n")
cat("jobs value changed  :", sum(abs(CMP$new_jobs - CMP$old_jobs) > 1e-6, na.rm=TRUE), "\n")
cat("jobs newly set to NA:", sum(is.na(CMP$new_jobs) & !is.na(CMP$old_jobs)), "\n")
cat("  (these are scenarios whose World jobs was a PARTIAL-region sum)\n")

# =============================================================================
line("4. THE COVERAGE FLOW FOR THE PAPER  [spec: report a compact flow]")
# =============================================================================
LAB <- read.csv(file.path(ROOT,"final_outcomes/engineered_cmt_century_broad_labels.csv"),
                stringsAsFactors = FALSE) %>%
  mutate(Model = norm(Model), Scenario = norm(Scenario)) %>%
  filter(!is.na(Pathway), Pathway != "") %>%
  transmute(approach, Model, Scenario, Pathway,
            amb = ifelse(grepl("^1\\.5", Ambition), "1.5C", "2C"))

FLOW <- LAB %>% left_join(WLD %>% select(approach, Model, Scenario,
                            starts_with("world_complete_")),
                          by = c("approach","Model","Scenario"))
cat("Scenarios classified, then surviving each World completeness gate:\n\n")
print(as.data.frame(FLOW %>% group_by(approach, amb, Pathway) %>%
  summarise(classified = n(),
            complete_jobs      = sum(world_complete_jobs, na.rm = TRUE),
            complete_gap       = sum(world_complete_gap, na.rm = TRUE),
            complete_mortality = sum(world_complete_mortality, na.rm = TRUE),
            .groups = "drop")))

# =============================================================================
line("5. THE RESULT GRID REBUILT ON STRICT WORLD")
# =============================================================================
# MORTALITY IS NOT REBUILT HERE, AND DOES NOT NEED TO BE. The published
# mortality comes from the reporting-complete run, not from the master
# dataset's cumulative_deaths_mln, and that pipeline ALREADY implements option
# C: verified that all 516 scenarios carrying a World row have exactly ten
# regions and World equals sum(R10) to 1e-13. Its input rule -- all five
# precursors reported directly at R10 -- enforces completeness upstream. So the
# strict-World fix applies to jobs and deprivation only.
JOBDEP <- c("net_re_jobs_per_1k","gap_GJ_pc","headcount_pct")
REG <- RO %>% add_pc() %>%
  select(approach, Model, Scenario, Region, all_of(JOBDEP))
MORT <- read.csv(file.path(ROOT,"final_outcomes/mortality_reporting_complete_scenario_values_2020_2100.csv"),
                 stringsAsFactors = FALSE) %>%
  mutate(Model = norm(Model), Scenario = norm(Scenario)) %>%
  transmute(approach, Model, Scenario, Region,
            mort_per_1k = cumulative_pm25_deaths_mln)   # million cumulative deaths
D <- bind_rows(REG, WLD %>% select(approach, Model, Scenario, Region, all_of(JOBDEP))) %>%
  full_join(MORT, by = c("approach","Model","Scenario","Region")) %>%
  inner_join(LAB, by = c("approach","Model","Scenario")) %>%
  mutate(stem = gsub("[-_ ]?[0-9]+(\\.[0-9]+)?[a-z]?$", "", Scenario),
         stem = sub("/.*$", "", stem), clus = paste(Model, stem))

cell <- function(d, out) {
  sgn <- ifelse(out %in% LOWER, -1, 1)
  a <- d[[out]][d$Pathway=="High-engineered-CMT"]; ca <- d$clus[d$Pathway=="High-engineered-CMT"]
  b <- d[[out]][d$Pathway=="High-RE"];             cb <- d$clus[d$Pathway=="High-RE"]
  ka <- !is.na(a); kb <- !is.na(b); a<-a[ka]; ca<-ca[ka]; b<-b[kb]; cb<-cb[kb]
  if (length(a) < 5 || length(b) < 5)
    return(tibble(n_cmt=length(a), n_re=length(b), raw_cmt=NA_real_, raw_re=NA_real_,
                  gap=NA_real_, lo=NA_real_, hi=NA_real_, pct=NA_real_))
  ua <- unique(ca); ub <- unique(cb)
  ia <- split(seq_along(a), ca); ib <- split(seq_along(b), cb)
  reps <- vapply(seq_len(B), function(i) {
    sa <- unlist(ia[sample(ua, length(ua), TRUE)], use.names=FALSE)
    sb <- unlist(ib[sample(ub, length(ub), TRUE)], use.names=FALSE)
    sgn*(median(b[sb]) - median(a[sa]))
  }, numeric(1))
  q <- quantile(reps, c(.025,.975), na.rm=TRUE)
  ma <- median(a); mb <- median(b)
  tibble(n_cmt=length(a), n_re=length(b), raw_cmt=ma, raw_re=mb,
         gap = sgn*(mb-ma), lo=q[[1]], hi=q[[2]], pct = sgn*100*(mb-ma)/abs(ma))
}
GRID <- expand_grid(approach=c("A","C"), Region=ALLR, amb=c("1.5C","2C"),
                    outcome=names(OUTS)) %>%
  pmap_dfr(function(approach, Region, amb, outcome) {
    d <- D[D$approach==approach & D$Region==Region & D$amb==amb, ]
    bind_cols(tibble(approach, Region, amb, outcome), cell(d, outcome))
  }) %>%
  mutate(family = OUTS[outcome], sig = !is.na(lo) & (lo>0 | hi<0),
         reg = SH[Region], primary = outcome != "headcount_pct")
saveRDS(list(grid=GRID, world=WLD, flow=FLOW), "STRICT_WORLD.rds")
cat("written: STRICT_WORLD.rds\n")

line("WORLD — CORRECTED, against the currently published figures")
OLDG <- readRDS("CENTURY_RESULTS.rds") %>%
  filter(approach=="A", Region==WORLD) %>%
  select(amb, outcome, old_cmt=raw_cmt, old_re=raw_re, old_gap=gap, old_sig=sig)
print(GRID %>% filter(approach=="A", Region==WORLD) %>%
      left_join(OLDG, by=c("amb","outcome")) %>%
      transmute(family, amb, n=paste0(n_cmt,"v",n_re),
                published_re = round(old_re,2), corrected_re = round(raw_re,2),
                published_gap = round(old_gap,2), corrected_gap = round(gap,2),
                CI = sprintf("[%+.2f,%+.2f]", lo, hi),
                sig = ifelse(sig,"YES","no")) %>%
      arrange(family, amb) %>% as.data.frame())

line("SCORECARD ON STRICT WORLD (nine regions + World, primary measures)")
H <- GRID %>% filter(approach=="A", primary, Region!="R10PAC_OECD", !is.na(gap))
print(as.data.frame(H %>% group_by(family) %>%
      summarise(cells=n(), favour_RE=sum(gap>0), sig_for=sum(gap>0 & sig),
                sig_against=sum(gap<0 & sig), .groups="drop")))
cat("\ntotal:", sum(H$gap>0), "of", nrow(H), "| significant for", sum(H$gap>0 & H$sig),
    "| against", sum(H$gap<0 & H$sig), "\n")
cat("\nRegional cells are unchanged by construction — the fix touches only the\n")
cat("World row. Any movement in the regional counts would indicate a bug.\n")
