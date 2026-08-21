# =============================================================================
# T4 — AUDIT SECTION 4: THE THREE THINGS T3 TURNED UP
#
#   (i)   pop_mln is IDENTICAL in every scenario (range = median = 7,625 mln).
#         So it is a fixed exogenous denominator, not a scenario projection.
#         Which year, and does that matter?
#   (ii)  Median FOSSIL jobs collapse 5.6 -> 0.6 -> 0.1 -> 0.1 (k) across
#         2020-2050 while renewables climb 5.2 -> 19.6. If fossil employment is
#         near zero from 2030, "renewables minus fossil" is really just
#         "renewables", and the contrast is not doing the work its name claims.
#   (iii) The two deprivation outcomes correlate rho = 0.99. Are the two JOBS
#         outcomes the same story twice as well? If so the 110-cell scorecard
#         over-counts, and the honest denominator is smaller.
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

ds <- readRDS("ds_A.rds"); F <- load_frame("A")

# =============================================================================
line("(i) THE POPULATION DENOMINATOR")
# =============================================================================
p <- ds %>% filter(Variable == "Total CDR") %>% distinct(Model, Scenario, Region, pop_mln)
v <- p %>% group_by(Region) %>%
  summarise(n_scen = n(), n_distinct_values = n_distinct(round(pop_mln, 6)),
            value = round(median(pop_mln)), .groups = "drop")
print(as.data.frame(v))
cat("\nIf n_distinct_values == 1 everywhere, population is a CONSTANT lookup:\n")
cat("  the same denominator for every scenario, so per-capita differences are\n")
cat("  pure numerator differences. That is a FEATURE (no demographic confound),\n")
cat("  but it means these are not really scenario-specific per-capita numbers.\n")
cat("  all constant:", all(v$n_distinct_values == 1), "\n")

cat("\nWhich year does 7,625 mln correspond to? (UN WPP world totals)\n")
cat("  2010 = 6,986 | 2015 = 7,380 | 2018 = 7,631 | 2020 = 7,841 | 2050 = 9,709\n")
cat("  -> the vector is a ~2018 snapshot, i.e. the database base year, NOT 2050.\n")
cat("\nCONSEQUENCE: cumulative 2020-2050 outcomes are expressed per BASE-YEAR head.\n")
cat("Africa's per-capita burden is therefore UNDERSTATED relative to a 2050\n")
cat("denominator (its population roughly doubles), and Europe's is overstated.\n")
cat("This affects LEVELS across regions. It does NOT affect the pathway contrast\n")
cat("within a region, because both arms share the same denominator.\n")

# proof that it cannot touch the contrast
cat("\nPROOF the contrast is denominator-free: Cliff's delta on RAW jobs totals\n")
cat("vs on per-capita jobs, same cells:\n")
raw <- F %>% mutate(REFOSS_raw = Renewables - Fossil)
chk <- expand_grid(Region = c("Aggregated R10 regions", R10_TEN), amb = c("1.5C","2C")) %>%
  pmap_dfr(function(Region, amb) {
    d <- raw[raw$Region == Region & raw$amb == amb, ]
    tibble(Region, amb,
           pc  = cell5(d, "REFOSS")$adv,
           rawd = cell5(d, "REFOSS_raw")$adv)
  })
cat("  max |difference| across", nrow(chk), "cells:",
    signif(max(abs(chk$pc - chk$rawd), na.rm = TRUE), 3), "\n")

# =============================================================================
line("(ii) DOES FOSSIL EMPLOYMENT ACTUALLY VANISH BY 2030?")
# =============================================================================
jt <- readRDS("jobs_type.rds")
fo <- jt %>% filter(tech_group == "Fossil", Region %in% R10_TEN, Year <= 2050)
cat("fossil job rows:", nrow(fo), "| streams:", paste(unique(fo$stream), collapse=" | "),
    "| job types:", paste(unique(fo$job_type), collapse=" | "), "\n")
cat("\nfossil jobs by year -- median hides a lot, so show the whole distribution\n")
cat("(thousand jobs, summed over the ten regions, per scenario):\n")
fs <- fo %>% group_by(Model, Scenario, Year) %>%
  summarise(k = sum(jobs_thousands, na.rm = TRUE), .groups = "drop")
print(fs %>% group_by(Year) %>%
      summarise(n = n(), pct_zero = round(100*mean(k == 0)),
                q25 = round(quantile(k,.25)), median = round(median(k)),
                q75 = round(quantile(k,.75)), max = round(max(k)), .groups="drop") %>%
      as.data.frame())
cat("\nby stream (construction/manufacturing vs O&M behave differently):\n")
print(fo %>% group_by(stream, Year) %>%
      summarise(median_k = round(median(jobs_thousands, na.rm=TRUE),2), .groups="drop") %>%
      pivot_wider(names_from = Year, values_from = median_k) %>% as.data.frame())
cat("\nSHARE OF THE CONTRAST carried by the fossil term:\n")
sh <- F %>% filter(Region %in% R10_TEN) %>%
  mutate(re_part = Renewables, fo_part = Fossil,
         share_fossil = Fossil/(Renewables + Fossil)) %>%
  group_by(Region) %>%
  summarise(median_RE_k = round(median(Renewables)), median_fossil_k = round(median(Fossil)),
            fossil_share_pct = round(100*median(share_fossil)), .groups="drop")
print(as.data.frame(sh))
cat("\nIf the fossil share is small, 'renewables minus fossil' is dominated by the\n")
cat("renewables term and should be described as a NET position, not as a\n")
cat("displacement result. The honest reading: High-RE wins because it BUILDS more\n")
cat("labour-intensive capacity, not because High-CMT keeps more fossil workers.\n")

cat("\nDecomposition: does High-RE win on the RE term, the fossil term, or both?\n")
dec <- expand_grid(Region = c("Aggregated R10 regions", R10_TEN), amb = c("1.5C","2C")) %>%
  pmap_dfr(function(Region, amb) {
    d <- F[F$Region == Region & F$amb == amb, ]
    g <- function(col, flip = 1) {
      a <- d[[col]][d$Pathway=="High-CMT"]; b <- d[[col]][d$Pathway=="High-RE"]
      a<-a[!is.na(a)]; b<-b[!is.na(b)]
      if (length(a)<5||length(b)<5) NA_real_ else flip*cliff_d(a,b)
    }
    tibble(Region, amb, adv_RE_jobs = g("Renewables"),
           adv_fossil_jobs = g("Fossil", -1),   # fewer fossil jobs = High-RE "wins"
           adv_contrast = g("REFOSS"))
  })
print(dec %>% mutate(across(where(is.numeric), ~round(.,2))) %>% as.data.frame())
cat("\nadv_fossil_jobs is signed so POSITIVE = High-RE has FEWER fossil jobs.\n")
cat("A positive contrast built on a positive RE term and a NEGATIVE fossil term\n")
cat("means High-RE gains on renewables while ALSO retaining more fossil jobs.\n")

# =============================================================================
line("(iii) HOW MANY INDEPENDENT RESULTS ARE THERE REALLY?")
# =============================================================================
cat("Spearman correlation between the five outcomes, pooled over scenario-regions:\n")
M <- F %>% filter(Region %in% R10_TEN) %>% select(all_of(names(OUT5)))
cm <- cor(M, method = "spearman", use = "pairwise.complete.obs")
dimnames(cm) <- list(OUT5[rownames(cm)], OUT5[colnames(cm)])
print(round(cm, 3))

cat("\nWithin region (the level the scorecard counts at):\n")
wr <- F %>% filter(Region %in% R10_TEN) %>% group_by(Region) %>%
  summarise(jobs_pair = round(cor(REFOSS, LOWC, method="spearman", use="complete.obs"),3),
            depr_pair = round(cor(gap_GJ_pc, headcount_pct, method="spearman", use="complete.obs"),3),
            .groups="drop")
print(as.data.frame(wr))
cat("\nmedian rho -- jobs pair:", round(median(wr$jobs_pair),3),
    "| deprivation pair:", round(median(wr$depr_pair),3), "\n")
cat("\nIf both pairs are ~0.99 the scorecard has 3 independent OUTCOME FAMILIES,\n")
cat("not 5 outcomes: jobs, deprivation, mortality. 110 cells is really 11 regions\n")
cat("x 2 ambitions x 3 families = 66 independent cells.\n")

fam3 <- F %>% mutate(dummy = 1)
score3 <- expand_grid(Region = c("Aggregated R10 regions", R10_TEN), amb = c("1.5C","2C"),
                      outcome = c("REFOSS","gap_GJ_pc","mort_per_1k")) %>%
  pmap_dfr(function(Region, amb, outcome)
    bind_cols(tibble(Region, amb, outcome), cell5(F[F$Region==Region & F$amb==amb,], outcome))) %>%
  mutate(win = adv > 0)
cat("\nSCORECARD ON ONE REPRESENTATIVE PER FAMILY (jobs = RE-fossil,\n")
cat("deprivation = gap, mortality):\n")
print(score3 %>% filter(!is.na(win)) %>% mutate(label = OUT5[outcome]) %>%
      group_by(label) %>% summarise(cells=n(), RE=sum(win), .groups="drop") %>% as.data.frame())
cat("total:", sum(score3$win, na.rm=TRUE), "of", sum(!is.na(score3$win)),
    sprintf(" (%.0f%%)\n", 100*mean(score3$win, na.rm=TRUE)))
saveRDS(score3, "T4_SCORE3.rds")
