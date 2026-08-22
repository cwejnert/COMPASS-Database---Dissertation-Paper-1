# =============================================================================
# W5 — DOES THE ORDER OF AGGREGATION MATTER?
#
# THE QUESTION. Two ways to turn an annual outcome series into one number per
# arm:
#
#   A (published)  per SCENARIO, cumulate over 2020-2050 -> one value per
#                  scenario -> compare the DISTRIBUTIONS between arms.
#
#   B (proposed)   per YEAR, take the median across scenarios within an arm ->
#                  one median trajectory per arm -> cumulate that.
#
# These are not the same. The median is not linear, so median(sum) != sum(median).
#
# THE DECISIVE ARGUMENT IS NOT ABOUT THE POINT ESTIMATE.
#
#   1. B DESTROYS INFERENCE. It collapses each arm to a SINGLE number. There is
#      no distribution left, so no Cliff's delta, no cluster bootstrap, no
#      confidence interval, no significance -- and no within-model test, which
#      is the check the paper now rests on. Every robustness result in the study
#      requires scenario-level values.
#
#   2. B INVENTS A TRAJECTORY NO MODEL PRODUCED. The median in 2030 and the
#      median in 2045 can come from different scenarios with different
#      assumptions, so the "median path" need not be internally consistent --
#      it can imply a capacity build no scenario contains. This is the standard
#      objection to reporting a median scenario in scenario analysis: the
#      envelope of medians is not a member of the ensemble.
#
#   3. THE SCENARIO IS THE UNIT OF ANALYSIS. Each scenario is one internally
#      consistent projection. Cliff's delta asks "if I draw one scenario from
#      each arm, how often is High-RE better?" -- a question that only has
#      meaning at the scenario level.
#
# SO WHY RUN THIS AT ALL? Because the size of the discrepancy is worth knowing.
# If A and B give similar arm medians, the choice is immaterial to the levels
# reported in the paper and point 1 settles it cleanly. If they diverge, that
# divergence is itself informative -- it means the arms have skewed
# within-year distributions, which is worth a sentence in the methods.
#
# Runs all three outcome families on the same footing.
#
# USAGE: Rscript W5_aggregation_order.R [compass_mortality_r10_noNH3.csv]
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

args   <- commandArgs(trailingOnly = TRUE)
NO_NH3 <- if (length(args)) args[1] else NA_character_
Y0 <- 2020; Y1 <- 2050
DROP <- "R10PAC_OECD"

LAB <- readRDS("pw_A.rds") %>% filter(!is.na(Pathway_excl)) %>%
  distinct(Model, Scenario, Pathway = Pathway_excl, Ambition) %>%
  mutate(amb = ifelse(grepl("^1\\.5", Ambition), "1.5C", "2C"))
POPR <- readRDS("ds_A.rds") %>% filter(Variable == "Total CDR") %>%
  distinct(Model, Scenario, Region, pop_mln) %>% filter(Region %in% R10_TEN)

# =============================================================================
# Build a per-scenario ANNUAL per-capita series for each outcome, so that the
# only thing differing between A and B is the ORDER of the two operations.
# =============================================================================
SER <- list()

# ---- mortality --------------------------------------------------------------
if (!is.na(NO_NH3) && file.exists(NO_NH3)) {
  mc <- readRDS("mort_coverage.rds")
  ORIG <- read.csv("mort_annual.csv", stringsAsFactors = FALSE)
  gate <- ORIG %>% filter(year>=2020, year<=2100) %>%
    group_by(Model=model, Scenario=scenario, Region=r10_region) %>%
    summarise(na=all(is.na(deaths_pm25)), .groups="drop") %>%
    group_by(Model, Scenario) %>% summarise(n_na=sum(na), n_reg=n(), .groups="drop") %>%
    left_join(mc %>% transmute(Model=model, Scenario=scenario, n_pm_nonzero),
              by=c("Model","Scenario")) %>%
    filter(n_na==0, n_reg==10, !is.na(n_pm_nonzero), n_pm_nonzero>=6) %>%
    select(Model, Scenario)
  SER$Health <- read.csv(NO_NH3, stringsAsFactors = FALSE) %>%
    filter(year >= Y0, year <= Y1) %>%
    transmute(Model=model, Scenario=scenario, Region=r10_region, Year=year,
              v = deaths_pm25 * 10) %>%          # decadal step, as published
    semi_join(gate, by=c("Model","Scenario")) %>%
    inner_join(POPR, by=c("Model","Scenario","Region")) %>%
    transmute(Model, Scenario, Region, Year, v = 1000*(v/1e6)/pop_mln)
  cat("mortality series rows:", nrow(SER$Health), "\n")
} else cat("[!] no harmonised mortality file - Health skipped\n")

# ---- deprivation ------------------------------------------------------------
SER$Deprivation <- readRDS("dle_annual.rds") %>%
  filter(Year >= Y0, Year <= Y1, Region %in% R10_TEN) %>%
  transmute(Model, Scenario, Region, Year, gap_EJ_total) %>%
  inner_join(POPR, by=c("Model","Scenario","Region")) %>%
  # EJ -> GJ per capita: 1 EJ = 1e9 GJ, pop in millions
  transmute(Model, Scenario, Region, Year,
            v = (gap_EJ_total * 1e9) / (pop_mln * 1e6))
cat("deprivation series rows:", nrow(SER$Deprivation), "\n")

# ---- jobs -------------------------------------------------------------------
gp <- function(tg) dplyr::case_when(tg=="Fossil"~"Fossil", tg=="Nuclear"~"Nuclear",
                                    tg=="Bioenergy"~"Bioenergy", TRUE~"Renewables")
SER$Jobs <- readRDS("jobs_type.rds") %>%
  filter(Year >= Y0, Year <= Y1, Region %in% R10_TEN) %>%
  mutate(g = gp(tech_group)) %>%
  group_by(Model, Scenario, Region, Year, g) %>%
  summarise(k = sum(jobs_thousands, na.rm=TRUE), .groups="drop") %>%
  pivot_wider(names_from=g, values_from=k, values_fill=0) %>%
  inner_join(POPR, by=c("Model","Scenario","Region")) %>%
  transmute(Model, Scenario, Region, Year,
            v = 1000*(Renewables - Fossil)/(pop_mln*1000) * 10)  # x10 = job-years
cat("jobs series rows:", nrow(SER$Jobs), "\n")

# =============================================================================
# A and B, side by side
# =============================================================================
compare <- function(nm) {
  S <- SER[[nm]] %>% inner_join(LAB, by=c("Model","Scenario"))

  # A — published: cumulate per scenario, then take the median of those totals
  A <- S %>% group_by(Model, Scenario, Region, amb, Pathway) %>%
    summarise(tot = sum(v, na.rm=TRUE), .groups="drop") %>%
    group_by(Region, amb, Pathway) %>%
    summarise(A = median(tot), n = n(), .groups="drop")

  # B — proposed: median across scenarios within each YEAR, then cumulate
  B <- S %>% group_by(Region, amb, Pathway, Year) %>%
    summarise(med = median(v, na.rm=TRUE), .groups="drop") %>%
    group_by(Region, amb, Pathway) %>%
    summarise(B = sum(med), .groups="drop")

  A %>% inner_join(B, by=c("Region","amb","Pathway")) %>%
    mutate(outcome = nm, diff_pct = 100*(B-A)/abs(A))
}
R <- bind_rows(lapply(names(SER), compare))
saveRDS(R, "W5_AGGORDER.rds")

line("HOW FAR APART ARE THE TWO ORDERS, AT ARM LEVEL?")
print(R %>% group_by(outcome) %>%
      summarise(cells = n(),
                med_abs_diff = round(median(abs(diff_pct)), 2),
                p90_abs_diff = round(quantile(abs(diff_pct), .9), 2),
                max_abs_diff = round(max(abs(diff_pct)), 2), .groups="drop") %>%
      as.data.frame())
cat("\ndiff_pct is (B - A) / |A| for one arm in one region at one ambition level.\n")

line("WORLD-EQUIVALENT VIEW — the ten regions summed, both orders")
print(R %>% filter(Region != DROP) %>% group_by(outcome, amb, Pathway) %>%
      summarise(A = round(sum(A), 2), B = round(sum(B), 2),
                diff_pct = round(100*(sum(B)-sum(A))/abs(sum(A)), 2), .groups="drop") %>%
      as.data.frame())

line("DOES THE ARM GAP ITSELF CHANGE? — this is what would matter")
G <- R %>% select(outcome, Region, amb, Pathway, A, B) %>%
  pivot_wider(names_from=Pathway, values_from=c(A,B)) %>%
  mutate(gapA = `A_High-RE` - `A_High-CMT`,
         gapB = `B_High-RE` - `B_High-CMT`,
         same_sign = sign(gapA) == sign(gapB),
         gap_shift_pct = 100*(gapB-gapA)/abs(gapA))
print(G %>% filter(Region != DROP) %>% group_by(outcome) %>%
      summarise(cells = n(), same_direction = sum(same_sign),
                med_gap_shift = round(median(abs(gap_shift_pct)), 1),
                max_gap_shift = round(max(abs(gap_shift_pct)), 1), .groups="drop") %>%
      as.data.frame())
cat("\nsame_direction counts cells where the ARM ORDERING is unchanged. That is\n")
cat("what the paper's claims rest on -- not the absolute level.\n")

fl <- G %>% filter(Region != DROP, !same_sign)
cat("\ncells where the two orders disagree on which arm is better:", nrow(fl), "\n")
if (nrow(fl)) print(fl %>% mutate(across(where(is.numeric), ~round(.,3))) %>%
      select(outcome, Region, amb, gapA, gapB) %>% as.data.frame())

line("WHY B CANNOT BE USED, IN ONE NUMBER")
cat("Approach A gives one value per SCENARIO:\n")
print(R %>% group_by(outcome) %>% summarise(scenario_values = sum(n), .groups="drop") %>%
      as.data.frame())
cat("\nApproach B gives one value per ARM per region per ambition level:\n")
print(R %>% group_by(outcome) %>% summarise(arm_values = n(), .groups="drop") %>%
      as.data.frame())
cat("\nWith one value per arm there is nothing to bootstrap, nothing to rank, and\n")
cat("no way to run the within-model test. Every inferential result in the paper\n")
cat("requires the scenario-level distribution that B discards.\n")
