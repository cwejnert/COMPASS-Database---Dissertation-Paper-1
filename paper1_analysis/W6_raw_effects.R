# =============================================================================
# W6 — RESULTS AS RAW LEVELS, WITH A CONFIDENCE INTERVAL ON THE RAW DIFFERENCE
#
# WHAT CHANGES AND WHY IT MATTERS.
#
# The published tables report Cliff's delta with a cluster-bootstrap interval on
# DELTA. Delta measures rank overlap: "draw one scenario from each arm, how
# often is High-RE better?" It says nothing about how large the gap is.
#
# Reporting a raw gap ("13.96 -> 10.01 GJ/cap, -28%") next to a significance
# star computed on delta is a mismatch. The two can disagree, and in a
# predictable direction: an outcome where the arms barely overlap but differ by
# a trivial amount gets a large significant delta and a meaningless gap; an
# outcome with a big level difference but heavy overlap gets the reverse.
#
# So this recomputes the interval on the quantity actually being reported: the
# DIFFERENCE IN MEDIANS between the arms, bootstrapped over the same 312 model
# x scenario-family clusters, 2,000 replicates. Significance means that
# interval excludes zero.
#
# Signed as ADVANTAGE throughout: positive always means High-RE is better, so
# for the two "lower is better" outcomes (deprivation gap, mortality) the sign
# is flipped. Units are the natural units of each outcome:
#
#   Jobs         job-years per 1,000 people   (higher better)
#   Deprivation  GJ per capita of gap         (lower better)
#   Mortality    PM2.5 deaths per 1,000       (lower better)
#
# It also reports where the two bases for significance DISAGREE, which is the
# thing worth knowing before switching the tables over.
#
# USAGE: Rscript W6_raw_effects.R [compass_mortality_r10_noNH3.csv]
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")
set.seed(20260821)

args   <- commandArgs(trailingOnly = TRUE)
NO_NH3 <- if (length(args)) args[1] else NA_character_
B      <- 2000
DROP   <- "R10PAC_OECD"
ALLR   <- c("Aggregated R10 regions", R10_TEN)
OUTS   <- c(REFOSS = "Jobs", gap_GJ_pc = "Energy deprivation", mort_per_1k = "Health")
UNIT   <- c(Jobs = "job-years per 1,000",
            `Energy deprivation` = "GJ per capita",
            Health = "deaths per 1,000")

# ---- harmonised mortality ---------------------------------------------------
MORT <- NULL
if (!is.na(NO_NH3) && file.exists(NO_NH3)) {
  mc <- readRDS("mort_coverage.rds"); ORIG <- read.csv("mort_annual.csv", stringsAsFactors=FALSE)
  gate <- ORIG %>% filter(year>=2020, year<=2100) %>%
    group_by(Model=model, Scenario=scenario, Region=r10_region) %>%
    summarise(na=all(is.na(deaths_pm25)), .groups="drop") %>%
    group_by(Model, Scenario) %>% summarise(n_na=sum(na), n_reg=n(), .groups="drop") %>%
    left_join(mc %>% transmute(Model=model, Scenario=scenario, n_pm_nonzero),
              by=c("Model","Scenario")) %>%
    filter(n_na==0, n_reg==10, !is.na(n_pm_nonzero), n_pm_nonzero>=6) %>%
    select(Model, Scenario)
  pop <- readRDS("ds_A.rds") %>% filter(Variable=="Total CDR") %>%
    distinct(Model, Scenario, Region, pop_mln) %>% filter(Region %in% R10_TEN)
  POP <- bind_rows(pop, pop %>% group_by(Model,Scenario) %>% filter(n()==10) %>%
    summarise(pop_mln=sum(pop_mln), .groups="drop") %>%
    mutate(Region="Aggregated R10 regions"))
  r <- read.csv(NO_NH3, stringsAsFactors=FALSE) %>% filter(year>=2020, year<=2050) %>%
    group_by(Model=model, Scenario=scenario, Region=r10_region) %>%
    summarise(dm=sum(deaths_pm25*10, na.rm=TRUE)/1e6, .groups="drop")
  w <- r %>% group_by(Model,Scenario) %>% filter(n()==10) %>%
    summarise(dm=sum(dm), .groups="drop") %>% mutate(Region="Aggregated R10 regions")
  MORT <- bind_rows(r,w) %>% semi_join(gate, by=c("Model","Scenario")) %>%
    inner_join(POP, by=c("Model","Scenario","Region")) %>%
    transmute(Model, Scenario, Region, mort_h = 1000*dm/pop_mln)
  cat("harmonised mortality rows:", nrow(MORT), "\n")
} else cat("[!] no harmonised mortality file - Health uses the published (uncorrected) values\n")

build <- function(id) {
  F <- load_frame(id) %>%
    mutate(stem = gsub("[-_ ]?[0-9]+(\\.[0-9]+)?[a-z]?$", "", Scenario),
           stem = sub("/.*$", "", stem), clus = paste(Model, stem))
  if (!is.null(MORT)) F <- F %>% select(-mort_per_1k) %>%
    left_join(MORT, by=c("Model","Scenario","Region")) %>% rename(mort_per_1k = mort_h)
  F
}

# =============================================================================
# THE ESTIMATOR. Difference in medians, cluster-bootstrapped.
#
# Both arms are resampled at the CLUSTER level, exactly as the delta bootstrap
# does, so the two are directly comparable and the dependence structure is
# handled identically. Cliff's delta is retained alongside as a supporting
# effect size, not as the basis for the star.
# =============================================================================
cliff_fast <- function(a, b) {
  n1 <- length(a); n2 <- length(b); if (!n1 || !n2) return(NA_real_)
  r <- rank(c(a, b)); 2*((sum(r[(n1+1):(n1+n2)]) - n2*(n2+1)/2)/(n1*n2)) - 1
}
cell <- function(d, out) {
  sgn <- ifelse(out %in% LOWER5, -1, 1)
  a <- d[[out]][d$Pathway=="High-CMT"]; b <- d[[out]][d$Pathway=="High-RE"]
  ca <- d$clus[d$Pathway=="High-CMT"];  cb <- d$clus[d$Pathway=="High-RE"]
  ka <- !is.na(a); kb <- !is.na(b); a<-a[ka]; ca<-ca[ka]; b<-b[kb]; cb<-cb[kb]
  if (length(a) < 5 || length(b) < 5)
    return(tibble(n_cmt=length(a), n_re=length(b), raw_cmt=NA_real_, raw_re=NA_real_,
                  gap=NA_real_, gap_lo=NA_real_, gap_hi=NA_real_, pct=NA_real_,
                  adv=NA_real_, d_lo=NA_real_, d_hi=NA_real_))
  ua <- unique(ca); ub <- unique(cb)
  ia <- split(seq_along(a), ca); ib <- split(seq_along(b), cb)
  reps <- vapply(seq_len(B), function(i) {
    sa <- unlist(ia[sample(ua, length(ua), TRUE)], use.names=FALSE)
    sb <- unlist(ib[sample(ub, length(ub), TRUE)], use.names=FALSE)
    c(sgn*(median(b[sb]) - median(a[sa])), sgn*cliff_fast(a[sa], b[sb]))
  }, numeric(2))
  qg <- quantile(reps[1,], c(.025,.975), na.rm=TRUE)
  qd <- quantile(reps[2,], c(.025,.975), na.rm=TRUE)
  ma <- median(a); mb <- median(b)
  tibble(n_cmt=length(a), n_re=length(b), raw_cmt=ma, raw_re=mb,
         gap = sgn*(mb-ma), gap_lo=qg[[1]], gap_hi=qg[[2]],
         pct = sgn*100*(mb-ma)/abs(ma),
         adv = sgn*cliff_fast(a,b), d_lo=qd[[1]], d_hi=qd[[2]])
}

matched <- function(d) d %>% filter(Region %in% R10_TEN) %>%
  distinct(Model,Scenario,Region) %>% count(Model,Scenario) %>% filter(n==10) %>%
  transmute(k = paste(Model,Scenario))
sweep <- function(d, approach, samp)
  expand_grid(Region=ALLR, amb=c("1.5C","2C"), outcome=names(OUTS)) %>%
    pmap_dfr(function(Region, amb, outcome)
      bind_cols(tibble(approach, sample=samp, Region, amb, outcome),
                cell(d[d$Region==Region & d$amb==amb, ], outcome)))

A <- build("A"); C <- build("C")
R <- bind_rows(
  sweep(A, "A full database", "all scenarios"),
  sweep(A %>% filter(paste(Model,Scenario) %in% matched(A)$k), "A full database", "matched only"),
  sweep(C, "C SCI-vetted",    "all scenarios"),
  sweep(C %>% filter(paste(Model,Scenario) %in% matched(C)$k), "C SCI-vetted", "matched only")
) %>% mutate(
  family = OUTS[outcome], unit = UNIT[family],
  sig_raw   = !is.na(gap_lo) & (gap_lo > 0 | gap_hi < 0),
  sig_delta = !is.na(d_lo)   & (d_lo   > 0 | d_hi   < 0),
  reg = ifelse(Region=="Aggregated R10 regions","WORLD", sub("^R10","",Region)))
saveRDS(R, "RAW_RESULTS.rds")
cat("written: RAW_RESULTS.rds\n")

H <- R %>% filter(approach=="A full database", sample=="all scenarios")

line("WORLD — raw levels and the interval on the raw difference")
print(H %>% filter(Region=="Aggregated R10 regions") %>%
      transmute(family, amb, unit, n=paste0(n_cmt,"v",n_re),
                `High-CMT`=round(raw_cmt,2), `High-RE`=round(raw_re,2),
                gap=round(gap,2), CI=sprintf("[%+.2f, %+.2f]", gap_lo, gap_hi),
                pct=round(pct), sig=ifelse(sig_raw,"YES","no")) %>%
      as.data.frame())

line("ALL REGIONS — raw levels, both ambition levels")
for (fm in c("Jobs","Energy deprivation","Health")) {
  cat("\n---", fm, "(", UNIT[fm], ") ---\n")
  print(H %>% filter(family==fm) %>%
        transmute(reg, amb, `High-CMT`=round(raw_cmt,2), `High-RE`=round(raw_re,2),
                  gap=round(gap,2),
                  CI=sprintf("[%+.2f, %+.2f]", gap_lo, gap_hi),
                  pct=round(pct), sig=ifelse(sig_raw,"*","")) %>%
        arrange(match(reg,c("WORLD","AFRICA","CHINA+","EUROPE","INDIA+","LATIN_AM",
                            "MIDDLE_EAST","NORTH_AM","REF_ECON","REST_ASIA","PAC_OECD")),
                amb) %>% as.data.frame())
}

line("DOES THE STAR MOVE? — raw-difference CI against delta CI")
cmp <- R %>% filter(!is.na(gap), Region != DROP)
print(cmp %>% group_by(approach, sample) %>%
      summarise(cells=n(), sig_raw=sum(sig_raw), sig_delta=sum(sig_delta),
                disagree=sum(sig_raw != sig_delta), .groups="drop") %>% as.data.frame())
d <- cmp %>% filter(sig_raw != sig_delta, approach=="A full database",
                    sample=="all scenarios")
cat("\nfull database, all scenarios — the cells that change:\n")
print(if (nrow(d)) d %>% transmute(reg, amb, family,
        raw=round(gap,3), raw_CI=sprintf("[%+.3f, %+.3f]", gap_lo, gap_hi), sig_raw,
        delta=round(adv,2), sig_delta) %>% as.data.frame() else "  none")

line("HEADLINE COUNTS ON THE RAW BASIS (nine regions + World)")
print(H %>% filter(Region != DROP, !is.na(gap)) %>% group_by(family) %>%
      summarise(cells=n(), favour_RE=sum(gap>0), sig_for=sum(gap>0 & sig_raw),
                sig_against=sum(gap<0 & sig_raw), .groups="drop") %>% as.data.frame())
cat("\ntotal:", sum(H$gap>0 & H$Region!=DROP, na.rm=TRUE), "of",
    sum(!is.na(H$gap) & H$Region!=DROP), "| significant for",
    sum(H$gap>0 & H$sig_raw & H$Region!=DROP, na.rm=TRUE), "| against",
    sum(H$gap<0 & H$sig_raw & H$Region!=DROP, na.rm=TRUE), "\n")
