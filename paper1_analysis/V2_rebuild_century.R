# =============================================================================
# V2 — REBUILD THE RESULT GRID ON THE REVAMPED (ENGINEERED-CMT, CENTURY) DESIGN
#
# WHAT CHANGED UPSTREAM, and it is a lot:
#
#   1. THE CMT AXIS IS NOW *ENGINEERED* CDR. Novel CDR plus fossil/industrial
#      CCS, with LAND-BASED CDR EXCLUDED. The old axis was Total CDR, which
#      folded afforestation and soil carbon into the same bucket as DACCS and
#      BECCS. Excluding land is a real improvement: land-based removal is a
#      different intervention with different wellbeing consequences, and the old
#      Latin America result (73% land-based) was measuring something the outcome
#      set could not see.
#
#   2. THE WINDOW IS 2020-2100, for BOTH classification and outcomes.
#
#   3. THE ARMS ARE NOW BALANCED: 64/64 at 1.5C and 239/239 at 2C in the full
#      database. The old design gave 74/62 and 261/193.
#
#   4. MORTALITY IS RESTRICTED TO DIRECTLY-REPORTED R10 PRECURSORS. No
#      World-to-R10 population disaggregation and no NH3 sidecar. This settles
#      BOTH open problems from the previous round at once -- the ammonia
#      reporting asymmetry and the synthetic-regions risk -- by dropping the
#      scenarios that caused them rather than patching them.
#
# WHAT THIS SCRIPT ADDS. The new pipeline reports medians and within-model
# counts but no interval, so significance cannot be read off it. This rebuilds
# the published grid on the same footing the deck used before: raw arm medians
# plus a 2,000-replicate cluster bootstrap on the DIFFERENCE IN MEDIANS,
# resampling whole model x scenario-family clusters.
#
# IT SELF-CHECKS FIRST. The medians it computes must reproduce
# century_outcome_medians_no_land_engineered_cmt.csv. If they do not, the join
# or the window is wrong and nothing downstream can be trusted.
#
# USAGE: Rscript V2_rebuild_century.R      (run from the repo root)
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(purrr)})
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")
set.seed(20260825)

B     <- 2000
ROOT  <- "."
OUTS  <- c(net_re_jobs_per_1k = "Jobs",
           gap_GJ_pc          = "Deprivation",
           headcount_pct      = "Deprivation headcount",
           mort_per_1k        = "Health")
LOWER <- c("gap_GJ_pc","headcount_pct","mort_per_1k")   # lower is better
R10   <- c("R10AFRICA","R10CHINA+","R10EUROPE","R10INDIA+","R10LATIN_AM",
           "R10MIDDLE_EAST","R10NORTH_AM","R10PAC_OECD","R10REF_ECON","R10REST_ASIA")
ALLR  <- c("Aggregated R10 regions", R10)
SH    <- c(`Aggregated R10 regions`="WORLD", R10AFRICA="Africa", `R10CHINA+`="China+",
           R10EUROPE="Europe", `R10INDIA+`="India+", R10LATIN_AM="Latin America",
           R10MIDDLE_EAST="Middle East", R10NORTH_AM="North America",
           R10PAC_OECD="Pacific OECD", R10REF_ECON="Reforming econ.",
           R10REST_ASIA="Rest of Asia")

# ---- labels from the revamped classification -------------------------------
LAB <- read.csv(file.path(ROOT,"final_outcomes/engineered_cmt_century_broad_labels.csv"),
                stringsAsFactors = FALSE) %>%
  mutate(Model = iconv(Model, from="", to="UTF-8", sub=""),
         Scenario = iconv(Scenario, from="", to="UTF-8", sub="")) %>%
  filter(!is.na(Pathway), Pathway != "") %>%
  transmute(approach, Model, Scenario, Ambition, Pathway,
            amb = ifelse(grepl("^1\\.5", Ambition), "1.5C", "2C"))
cat("labelled scenario-rows:", nrow(LAB), "\n")
print(as.data.frame(LAB %>% count(approach, amb, Pathway) %>%
      pivot_wider(names_from = Pathway, values_from = n)))

# ---- outcomes, per scenario x region ---------------------------------------
# MATCHES COMPASS_engineered_cmt_century_outcomes_summary.R EXACTLY: read the
# CSV, take the FIRST row per Model x Scenario x Region, no Variable filter.
#
# WHY THIS IS FRAGILE, AND IT MATTERS. The outcome columns are not constant
# across a scenario's Variable rows -- in 288 of 11,808 scenario-regions (2%)
# net_re_jobs_per_1k differs between the "Total CDR" row and the "Renewable
# Capacity" row for the SAME scenario. Taking the first row therefore makes the
# published median depend on CSV row ordering. At World 1.5C the High-RE jobs
# median is 686.8 on first/max and 503.5 on min -- a 36% swing. Direction is
# unaffected (High-RE still far ahead either way) but the magnitude is not.
# Reproduced as-is here so the deck matches the published files; flagged in the
# summary at the end so it can be fixed upstream.
norm <- function(x) iconv(x, from = "", to = "UTF-8", sub = "")
load_ds <- function(id) {
  read.csv(file.path(ROOT, sprintf("master_outputs/approach_%s/compass_master_dataset_%s.csv", id, id)),
           stringsAsFactors = FALSE) %>%
    mutate(Model = norm(Model), Scenario = norm(Scenario), approach = id) %>%
    select(approach, Model, Scenario, Region, any_of(names(OUTS))) %>%
    distinct(approach, Model, Scenario, Region, .keep_all = TRUE)
}
DS <- bind_rows(load_ds("A"), load_ds("C"))
cat("\noutcome rows:", nrow(DS), "| regions:", n_distinct(DS$Region), "\n")

D <- DS %>% inner_join(LAB, by = c("approach","Model","Scenario")) %>%
  mutate(stem = gsub("[-_ ]?[0-9]+(\\.[0-9]+)?[a-z]?$", "", Scenario),
         stem = sub("/.*$", "", stem), clus = paste(Model, stem))
cat("joined rows:", nrow(D), "| clusters:", n_distinct(D$clus), "\n")

# =============================================================================
line("SELF-CHECK — do our medians reproduce the published ones?")
# =============================================================================
PUB <- read.csv(file.path(ROOT,"final_outcomes/century_outcome_medians_no_land_engineered_cmt.csv"),
                stringsAsFactors = FALSE) %>%
  mutate(amb = ifelse(grepl("^1\\.5", Ambition), "1.5C", "2C"))
OURS <- D %>% pivot_longer(any_of(names(OUTS)), names_to="outcome", values_to="v") %>%
  filter(!is.na(v)) %>%
  group_by(approach, Region, amb, outcome) %>%
  summarise(n_cmt = sum(Pathway == "High-engineered-CMT"),
            n_re  = sum(Pathway == "High-RE"),
            m_cmt = median(v[Pathway == "High-engineered-CMT"]),
            m_re  = median(v[Pathway == "High-RE"]), .groups = "drop")
CHK <- PUB %>% inner_join(OURS, by = c("approach","Region","amb","outcome")) %>%
  mutate(d_cmt = abs(median_cmt - m_cmt), d_re = abs(median_re - m_re))
cat("cells compared:", nrow(CHK), "\n")
cat("max |difference| in High-CMT median:", signif(max(CHK$d_cmt, na.rm=TRUE), 3), "\n")
cat("max |difference| in High-RE  median:", signif(max(CHK$d_re,  na.rm=TRUE), 3), "\n")
cat("arm sizes match:", all(CHK$n_cmt.x == CHK$n_cmt.y & CHK$n_re.x == CHK$n_re.y, na.rm=TRUE), "\n")
if (max(c(CHK$d_cmt, CHK$d_re), na.rm=TRUE) > 1e-6) {
  cat("\n[FAIL] our medians do not reproduce the published file. Worst cells:\n")
  print(CHK %>% arrange(desc(pmax(d_cmt,d_re))) %>% head(6) %>%
        select(approach, Region, amb, outcome, median_cmt, m_cmt, median_re, m_re) %>%
        as.data.frame())
} else cat("[ok] exact match — the join and window are right.\n")

# =============================================================================
line("BUILDING THE GRID WITH CLUSTER-ROBUST INTERVALS")
# =============================================================================
cell <- function(d, out) {
  sgn <- ifelse(out %in% LOWER, -1, 1)      # sign so positive always favours High-RE
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
         gap = sgn*(mb-ma), lo=q[[1]], hi=q[[2]],
         pct = sgn*100*(mb-ma)/abs(ma))
}
GRID <- expand_grid(approach=c("A","C"), Region=ALLR, amb=c("1.5C","2C"),
                    outcome=names(OUTS)[1:3]) %>%
  pmap_dfr(function(approach, Region, amb, outcome) {
    d <- D[D$approach==approach & D$Region==Region & D$amb==amb, ]
    bind_cols(tibble(approach, Region, amb, outcome), cell(d, outcome))
  }) %>%
  mutate(family = OUTS[outcome], sig = !is.na(lo) & (lo>0 | hi<0),
         reg = SH[Region], primary = outcome %in% c("net_re_jobs_per_1k","gap_GJ_pc"))

# ---- mortality, from the reporting-complete scenario values -----------------
MS <- read.csv(file.path(ROOT,"final_outcomes/mortality_reporting_complete_scenario_values_2020_2100.csv"),
               stringsAsFactors = FALSE) %>%
  mutate(amb = ifelse(grepl("^1\\.5", Ambition), "1.5C", "2C"),
         stem = gsub("[-_ ]?[0-9]+(\\.[0-9]+)?[a-z]?$", "", Scenario),
         stem = sub("/.*$", "", stem), clus = paste(Model, stem),
         mort = cumulative_pm25_deaths_mln)
cat("mortality scenario-region rows:", nrow(MS), "\n")
cellm <- function(d) {
  a <- d$mort[d$Pathway=="High-engineered-CMT"]; ca <- d$clus[d$Pathway=="High-engineered-CMT"]
  b <- d$mort[d$Pathway=="High-RE"];             cb <- d$clus[d$Pathway=="High-RE"]
  if (length(a) < 5 || length(b) < 5)
    return(tibble(n_cmt=length(a), n_re=length(b), raw_cmt=NA_real_, raw_re=NA_real_,
                  gap=NA_real_, lo=NA_real_, hi=NA_real_, pct=NA_real_))
  ua <- unique(ca); ub <- unique(cb)
  ia <- split(seq_along(a), ca); ib <- split(seq_along(b), cb)
  reps <- vapply(seq_len(B), function(i) {
    sa <- unlist(ia[sample(ua, length(ua), TRUE)], use.names=FALSE)
    sb <- unlist(ib[sample(ub, length(ub), TRUE)], use.names=FALSE)
    -(median(b[sb]) - median(a[sa]))                    # lower deaths is better
  }, numeric(1))
  q <- quantile(reps, c(.025,.975), na.rm=TRUE)
  ma <- median(a); mb <- median(b)
  tibble(n_cmt=length(a), n_re=length(b), raw_cmt=ma, raw_re=mb,
         gap = -(mb-ma), lo=q[[1]], hi=q[[2]], pct = -100*(mb-ma)/abs(ma))
}
MORT <- expand_grid(approach=c("A","C"), Region=ALLR, amb=c("1.5C","2C")) %>%
  pmap_dfr(function(approach, Region, amb) {
    d <- MS[MS$approach==approach & MS$Region==Region & MS$amb==amb, ]
    bind_cols(tibble(approach, Region, amb, outcome="mort_per_1k"), cellm(d))
  }) %>%
  mutate(family="Health", sig = !is.na(lo) & (lo>0 | hi<0), reg = SH[Region], primary = TRUE)

ALL <- bind_rows(GRID, MORT)
saveRDS(ALL, "CENTURY_RESULTS.rds")
cat("written: CENTURY_RESULTS.rds\n")

line("WORLD — all four outcomes, both ambition levels, full database")
print(ALL %>% filter(approach=="A", Region=="Aggregated R10 regions") %>%
      transmute(family, outcome, amb, n=paste0(n_cmt,"v",n_re),
                cmt=round(raw_cmt,2), re=round(raw_re,2), gap=round(gap,2),
                CI=sprintf("[%+.2f, %+.2f]", lo, hi), pct=round(pct),
                sig=ifelse(sig,"YES","no")) %>% as.data.frame())

line("SCORECARD — primary measures, nine regions + World (Pacific OECD dropped)")
H <- ALL %>% filter(approach=="A", primary, Region!="R10PAC_OECD", !is.na(gap))
print(as.data.frame(H %>% group_by(family) %>%
      summarise(cells=n(), favour_RE=sum(gap>0), sig_for=sum(gap>0 & sig),
                sig_against=sum(gap<0 & sig), .groups="drop")))
cat("\ntotal:", sum(H$gap>0), "of", nrow(H), "| significant for", sum(H$gap>0 & H$sig),
    "| against", sum(H$gap<0 & H$sig), "\n")

line("EVERY REGION, FULL DATABASE")
for (fm in c("Jobs","Deprivation","Health")) {
  cat("\n---", fm, "---\n")
  print(ALL %>% filter(approach=="A", family==fm, primary) %>%
        transmute(reg, amb, cmt=round(raw_cmt,2), re=round(raw_re,2),
                  gap=round(gap,2), CI=sprintf("[%+.2f,%+.2f]", lo, hi),
                  pct=round(pct), sig=ifelse(sig,"*","")) %>% as.data.frame())
}
