# =============================================================================
# Z1 — READINESS AUDIT. One pass, no new threads.
#
# For each of the three outcome families, answer the same five questions:
#   1. Is the grid COMPLETE? (11 regions x 2 ambitions x 2 databases x 2 samples)
#   2. Are LEVELS reportable? (medians for both arms, both ambitions)
#   3. Is SIGNIFICANCE established? (cluster-robust intervals)
#   4. Does it survive VETTING? (full database vs SCI 2025)
#   5. Does it survive the MODEL-COMPOSITION problem? -- the guard that has
#      already sunk mortality. Two tests: within-model variance share, and
#      the pooled contrast against a within-model-stratified one.
#
# Question 5 has never been run for DEPRIVATION. That is the one real gap
# between "we have a number" and "we can defend the number".
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

FIN <- readRDS("FINAL_RESULTS.rds")
F   <- load_frame("A")
ALLR <- c("Aggregated R10 regions", R10_TEN)
FAM <- c(REFOSS="Jobs", LOWC="Jobs", gap_GJ_pc="Deprivation",
         headcount_pct="Deprivation", mort_per_1k="Health")
PRIMARY <- c("REFOSS","gap_GJ_pc","mort_per_1k")

# =============================================================================
line("1. IS THE GRID COMPLETE?")
# =============================================================================
grid <- FIN %>% mutate(family = FAM[outcome]) %>%
  group_by(family, approach, sample) %>%
  summarise(cells = n(), computed = sum(!is.na(adv)),
            pct = round(100*mean(!is.na(adv))), .groups="drop") %>%
  pivot_wider(names_from = c(approach, sample), values_from = c(cells, computed, pct))
print(FIN %>% mutate(family = FAM[outcome]) %>%
      group_by(family, approach, sample) %>%
      summarise(computed = sum(!is.na(adv)), of = n(), .groups="drop") %>%
      mutate(v = paste0(computed,"/",of)) %>% select(-computed,-of) %>%
      pivot_wider(names_from = c(approach, sample), values_from = v) %>%
      as.data.frame())
cat("\nA blank cell means fewer than 5 scenarios in one arm, so no comparison.\n")

# =============================================================================
line("2-4. LEVELS, SIGNIFICANCE, VETTING  (primary measure per family)")
# =============================================================================
P <- FIN %>% filter(outcome %in% PRIMARY) %>% mutate(family = FAM[outcome])
print(P %>% filter(!is.na(adv)) %>% group_by(family, approach, sample) %>%
      summarise(cells = n(), RE_wins = sum(adv>0),
                sig = sum(sig), sig_RE = sum(sig & adv>0), sig_CMT = sum(sig & adv<0),
                .groups="drop") %>%
      mutate(score = paste0(RE_wins,"/",cells)) %>%
      select(family, approach, sample, score, sig, sig_RE, sig_CMT) %>%
      as.data.frame())

cat("\nDoes the DIRECTION agree between the full database and SCI vetting?\n")
agree <- P %>% filter(sample == "all scenarios", !is.na(adv)) %>%
  select(family, Region, amb, approach, adv) %>%
  pivot_wider(names_from = approach, values_from = adv) %>%
  filter(!is.na(`A full database`), !is.na(`C SCI-vetted`)) %>%
  mutate(same = sign(`A full database`) == sign(`C SCI-vetted`))
print(agree %>% group_by(family) %>%
      summarise(cells = n(), agree = sum(same), pct = round(100*mean(same)),
                .groups="drop") %>% as.data.frame())

# =============================================================================
line("5. THE MODEL-COMPOSITION GUARD  (never run for deprivation until now)")
# =============================================================================
vshare <- function(col) {
  F %>% filter(Region %in% R10_TEN, !is.na(.data[[col]])) %>%
    group_by(Region) %>%
    summarise(within = mean(tapply(.data[[col]], fam, var, na.rm=TRUE), na.rm=TRUE),
              total  = var(.data[[col]], na.rm=TRUE), .groups="drop") %>%
    mutate(share = within/total, outcome = col)
}
V <- bind_rows(lapply(PRIMARY, vshare)) %>%
  mutate(family = FAM[outcome])
cat("Share of outcome variance that is WITHIN a model family.\n")
cat("Below 0.10 the pooled comparison is reading model inventories, not pathways.\n\n")
print(V %>% select(Region, family, share) %>%
      mutate(share = round(share,3)) %>%
      pivot_wider(names_from = family, values_from = share) %>% as.data.frame())
print(V %>% group_by(family) %>%
      summarise(min = round(min(share),3), median = round(median(share),3),
                max = round(max(share),3), n_below_10pct = sum(share < 0.10),
                .groups="drop") %>% as.data.frame())

cat("\n--- pooled contrast vs WITHIN-MODEL stratified contrast ---\n")
cat("If a family's advantage is real it should survive being computed only\n")
cat("inside models that hold BOTH arms.\n\n")
strat <- expand_grid(Region = ALLR, amb = c("1.5C","2C"), outcome = PRIMARY) %>%
  pmap_dfr(function(Region, amb, outcome) {
    d <- F[F$Region == Region & F$amb == amb, ]
    d <- d[!is.na(d[[outcome]]), ]
    sgn <- ifelse(outcome %in% LOWER5, -1, 1)
    pooled <- if (sum(d$Pathway=="High-CMT") >= 5 && sum(d$Pathway=="High-RE") >= 5)
      sgn*cliff_d(d[[outcome]][d$Pathway=="High-CMT"],
                  d[[outcome]][d$Pathway=="High-RE"]) else NA_real_
    # keep only model families holding both arms with >=3 each
    ok <- d %>% group_by(fam) %>%
      summarise(a = sum(Pathway=="High-CMT"), b = sum(Pathway=="High-RE"),
                .groups="drop") %>% filter(a >= 3, b >= 3)
    ds <- d[d$fam %in% ok$fam, ]
    within <- if (nrow(ok) >= 1)
      sgn*cliff_strat(ds[[outcome]], ds$Pathway, ds$fam) else NA_real_
    tibble(Region, amb, outcome, family = FAM[outcome], pooled, within,
           n_fam_both = nrow(ok), n_strat = nrow(ds))
  })
saveRDS(strat, "Z1_STRAT.rds")
print(strat %>% filter(!is.na(pooled)) %>% group_by(family) %>%
      summarise(cells = n(),
                testable_within = sum(!is.na(within)),
                pooled_RE = sum(pooled > 0),
                within_RE = sum(within > 0, na.rm=TRUE),
                sign_flips = sum(!is.na(within) & sign(pooled) != sign(within)),
                median_gap = round(median(abs(pooled - within), na.rm=TRUE), 3),
                .groups="drop") %>% as.data.frame())

cat("\ncells where pooled and within-model DISAGREE ON SIGN:\n")
fl <- strat %>% filter(!is.na(within), !is.na(pooled), sign(pooled) != sign(within))
print(if (nrow(fl)) fl %>% mutate(across(where(is.numeric), ~round(.,3))) %>%
      select(Region, amb, family, pooled, within, n_fam_both) %>% as.data.frame()
      else "  none")

# =============================================================================
line("VERDICT PER FAMILY")
# =============================================================================
for (fm in c("Jobs","Deprivation","Health")) {
  v  <- V %>% filter(family == fm)
  st <- strat %>% filter(family == fm, !is.na(pooled))
  p  <- P %>% filter(FAM[outcome] == fm, approach=="A full database",
                     sample=="all scenarios", !is.na(adv))
  cv <- P %>% filter(FAM[outcome] == fm, approach=="C SCI-vetted",
                     sample=="all scenarios", !is.na(adv))
  cat(sprintf("\n%-12s  full DB %s (%d sig) | vetted %s | var-share median %.2f, %d region(s) <0.10\n",
      fm, paste0(sum(p$adv>0),"/",nrow(p)), sum(p$sig),
      paste0(sum(cv$adv>0),"/",nrow(cv)),
      median(v$share), sum(v$share < 0.10)))
  cat(sprintf("%-12s  within-model testable in %d of %d cells | sign flips %d\n", "",
      sum(!is.na(st$within)), nrow(st),
      sum(!is.na(st$within) & sign(st$pooled) != sign(st$within))))
}
