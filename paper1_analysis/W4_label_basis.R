# =============================================================================
# W4 — GLOBAL TERCILE OR PER-REGION TERCILE?
#
# THE PUBLISHED DESIGN uses ONE global classification: total_cdr and total_re
# are summed over the ten R10 regions, cut at the top tercile within an
# ambition class, and the resulting labels are applied UNCHANGED in every
# region. So "High-RE" means "this scenario leans renewable globally", and the
# question the paper answers is: do globally renewable-leaning pathways deliver
# better outcomes everywhere?
#
# THE ALTERNATIVE computes the terciles on each region's OWN deployment. Then
# "High-RE" means "this scenario leans renewable IN THIS REGION", and the
# question becomes: within a region, does leaning renewable locally do better?
#
# THESE ARE DIFFERENT QUESTIONS, not a right and a wrong answer. Which one the
# paper wants depends on what it claims. The global label supports a statement
# about pathway ARCHETYPES; the per-region label supports a statement about
# local deployment choices. The global label is also what makes the World row
# meaningful -- World IS the global aggregate, so a per-region label does not
# exist there and those rows are necessarily dropped from this comparison.
#
# WHY IT MATTERS HERE. Z3 showed the global label describes regional behaviour
# well in most regions (delta 0.7-0.9 on their own renewable deployment) and
# badly in Pacific OECD (-0.19). If the result depends on which basis is used,
# that is a real fragility. If it does not, the classification choice is not
# load-bearing and one paragraph in the SI settles it.
#
# This runs BOTH bases through the full machinery -- three families, both
# ambition levels, cluster-robust intervals, and the within-model test that is
# the binding constraint on the paper -- and reports them side by side.
#
# USAGE: Rscript W4_label_basis.R [compass_mortality_r10_noNH3.csv]
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")
set.seed(20260821)

args   <- commandArgs(trailingOnly = TRUE)
NO_NH3 <- if (length(args)) args[1] else NA_character_
B      <- 2000
DROP   <- "R10PAC_OECD"
OUTS   <- c(REFOSS = "Jobs", gap_GJ_pc = "Energy deprivation", mort_per_1k = "Health")

# ---- harmonised mortality, so both bases are scored on the corrected data ---
MORT <- NULL
if (!is.na(NO_NH3) && file.exists(NO_NH3)) {
  NA_  <- read.csv(NO_NH3, stringsAsFactors = FALSE)
  mc   <- readRDS("mort_coverage.rds")
  ORIG <- read.csv("mort_annual.csv", stringsAsFactors = FALSE)
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
  MORT <- NA_ %>% filter(year>=2020, year<=2050) %>%
    group_by(Model=model, Scenario=scenario, Region=r10_region) %>%
    summarise(dm = sum(deaths_pm25*10, na.rm=TRUE)/1e6, .groups="drop") %>%
    semi_join(gate, by=c("Model","Scenario")) %>%
    inner_join(pop, by=c("Model","Scenario","Region")) %>%
    transmute(Model, Scenario, Region, mort_h = 1000*dm/pop_mln)
  cat("harmonised mortality rows:", nrow(MORT), "\n")
} else cat("[!] no harmonised mortality file - Health will be skipped\n")

# ---- the two labellings -----------------------------------------------------
ds <- readRDS("ds_A.rds")
reg <- ds %>% filter(Variable %in% c("Total CDR","Renewable Capacity"),
                     Region %in% R10_TEN) %>%
  select(Model, Scenario, Region, Variable, Total_Value) %>%
  pivot_wider(names_from = Variable, values_from = Total_Value) %>%
  rename(cmt = `Total CDR`, re = `Renewable Capacity`)

glob <- readRDS("pw_A.rds") %>% filter(!is.na(Pathway_excl)) %>%
  distinct(Model, Scenario, Pathway_glob = Pathway_excl, Ambition) %>%
  mutate(amb = ifelse(grepl("^1\\.5", Ambition), "1.5C", "2C"))

# Per-region terciles are computed on the SAME ambition strata and the SAME
# exclusion rule (high on one axis and not the other), so the only thing that
# changes is the sample the quantile is taken on.
tc <- function(x) x >= quantile(x, 2/3, na.rm = TRUE)
perreg <- reg %>% inner_join(glob, by = c("Model","Scenario")) %>%
  group_by(Region, amb) %>%
  mutate(hr = tc(re), hc = tc(cmt),
         Pathway_reg = ifelse(hc & !hr, "High-CMT",
                       ifelse(hr & !hc, "High-RE", NA_character_))) %>%
  ungroup() %>%
  select(Model, Scenario, Region, amb, Pathway_glob, Pathway_reg)

line("1. HOW MUCH DOES THE LABEL ACTUALLY MOVE?")
cat("Per-region labels are defined only on the ten R10 rows -- World IS the\n")
cat("global aggregate, so there is no per-region label there.\n\n")
print(perreg %>% group_by(Region) %>%
      summarise(rows = n(),
                same    = sum(!is.na(Pathway_reg) & Pathway_reg == Pathway_glob),
                flipped = sum(!is.na(Pathway_reg) & Pathway_reg != Pathway_glob),
                dropped = sum(is.na(Pathway_reg)),
                pct_same_of_labelled =
                  round(100*same/pmax(1, same+flipped)), .groups="drop") %>%
      as.data.frame())
cat("\nOverall: of", sum(!is.na(perreg$Pathway_reg)), "region-rows that the ",
    "per-region rule labels,\n", sep="")
cat(sum(!is.na(perreg$Pathway_reg) & perreg$Pathway_reg != perreg$Pathway_glob),
    "carry the OPPOSITE label to the global rule (",
    round(100*mean(perreg$Pathway_reg[!is.na(perreg$Pathway_reg)] !=
          perreg$Pathway_glob[!is.na(perreg$Pathway_reg)]), 1), "%).\n", sep="")

line("2. DOES IT FIX THE MODEL-COMPOSITION PROBLEM?")
# This is the question worth asking. The binding constraint on the paper is
# that REMIND is 85% of High-RE and 1% of High-CMT. If a per-region tercile
# rebalanced the arms it would be worth serious consideration on those grounds
# alone, whatever it did to the point estimates.
compo <- function(d, col) d %>% filter(!is.na(.data[[col]])) %>%
  mutate(fam = sub("[ /-].*$", "", Model)) %>%
  count(fam, arm = .data[[col]]) %>%
  pivot_wider(names_from = arm, values_from = n, values_fill = 0)
cat("GLOBAL labels, share of each arm that is REMIND:\n")
g <- compo(perreg, "Pathway_glob")
cat(sprintf("  High-RE %.0f%%  |  High-CMT %.0f%%\n",
    100*sum(g$`High-RE`[grepl("REMIND",g$fam)])/sum(g$`High-RE`),
    100*sum(g$`High-CMT`[grepl("REMIND",g$fam)])/sum(g$`High-CMT`)))
cat("PER-REGION labels, share of each arm that is REMIND:\n")
r <- compo(perreg, "Pathway_reg")
cat(sprintf("  High-RE %.0f%%  |  High-CMT %.0f%%\n",
    100*sum(r$`High-RE`[grepl("REMIND",r$fam)])/sum(r$`High-RE`),
    100*sum(r$`High-CMT`[grepl("REMIND",r$fam)])/sum(r$`High-CMT`)))
cat("\nfull composition under per-region labels:\n")
print(as.data.frame(r %>% mutate(tot = `High-CMT`+`High-RE`) %>% arrange(desc(tot))))

# ---- score both bases -------------------------------------------------------
F <- load_frame("A") %>%
  mutate(stem = gsub("[-_ ]?[0-9]+(\\.[0-9]+)?[a-z]?$", "", Scenario),
         stem = sub("/.*$", "", stem), clus = paste(Model, stem),
         fam  = sub("[ /-].*$", "", Model))
if (!is.null(MORT)) F <- F %>% select(-mort_per_1k) %>%
  left_join(MORT, by = c("Model","Scenario","Region")) %>% rename(mort_per_1k = mort_h)
OUTUSE <- if (is.null(MORT)) names(OUTS)[1:2] else names(OUTS)

D <- F %>% filter(Region %in% R10_TEN) %>%
  inner_join(perreg %>% select(Model,Scenario,Region,Pathway_reg),
             by = c("Model","Scenario","Region"))

cliff_fast <- function(a, b) {
  n1 <- length(a); n2 <- length(b); if (!n1 || !n2) return(NA_real_)
  r <- rank(c(a, b)); 2*((sum(r[(n1+1):(n1+n2)]) - n2*(n2+1)/2)/(n1*n2)) - 1
}
cell <- function(d, out, pcol) {
  sgn <- ifelse(out %in% LOWER5, -1, 1)
  keep <- !is.na(d[[pcol]]) & !is.na(d[[out]])
  d <- d[keep, ]
  a <- d[[out]][d[[pcol]]=="High-CMT"]; b <- d[[out]][d[[pcol]]=="High-RE"]
  ca <- d$clus[d[[pcol]]=="High-CMT"];  cb <- d$clus[d[[pcol]]=="High-RE"]
  if (length(a) < 5 || length(b) < 5)
    return(tibble(n_cmt=length(a), n_re=length(b), adv=NA_real_, lo=NA_real_, hi=NA_real_))
  ua <- unique(ca); ub <- unique(cb)
  ia <- split(seq_along(a), ca); ib <- split(seq_along(b), cb)
  reps <- vapply(seq_len(B), function(i) {
    sa <- unlist(ia[sample(ua, length(ua), TRUE)], use.names=FALSE)
    sb <- unlist(ib[sample(ub, length(ub), TRUE)], use.names=FALSE)
    sgn*cliff_fast(a[sa], b[sb])
  }, numeric(1))
  q <- quantile(reps, c(.025,.975), na.rm=TRUE)
  tibble(n_cmt=length(a), n_re=length(b), adv=sgn*cliff_fast(a,b), lo=q[[1]], hi=q[[2]])
}
R2 <- expand_grid(basis=c("global","per-region"), Region=R10_TEN,
                  amb=c("1.5C","2C"), outcome=OUTUSE) %>%
  pmap_dfr(function(basis, Region, amb, outcome) {
    pcol <- ifelse(basis=="global", "Pathway", "Pathway_reg")
    d <- D[D$Region==Region & D$amb==amb, ]
    bind_cols(tibble(basis, Region, amb, outcome), cell(d, outcome, pcol))
  }) %>%
  mutate(sig = !is.na(lo) & (lo>0 | hi<0), family = OUTS[outcome],
         reg = sub("^R10","",Region))
saveRDS(R2, "W4_LABELS.rds")

line("3. THE HEADLINE UNDER EACH BASIS (nine regions, no World row)")
print(R2 %>% filter(Region != DROP, !is.na(adv)) %>% group_by(family, basis) %>%
      summarise(cells=n(), RE=sum(adv>0), sig_for=sum(adv>0 & sig),
                sig_against=sum(adv<0 & sig), med=round(median(adv),3),
                .groups="drop") %>%
      pivot_wider(names_from=basis, values_from=c(cells,RE,sig_for,sig_against,med)) %>%
      as.data.frame())
cat("\ntotals:\n")
print(R2 %>% filter(Region != DROP, !is.na(adv)) %>% group_by(basis) %>%
      summarise(cells=n(), RE=sum(adv>0), sig_for=sum(adv>0 & sig),
                sig_against=sum(adv<0 & sig), .groups="drop") %>% as.data.frame())

line("4. CELLS THAT CHANGE SIGN BETWEEN THE TWO BASES")
CP <- R2 %>% select(Region, reg, amb, outcome, family, basis, adv, sig, n_cmt, n_re) %>%
  pivot_wider(names_from=basis, values_from=c(adv,sig,n_cmt,n_re))
fl <- CP %>% filter(Region != DROP, !is.na(adv_global), !is.na(`adv_per-region`),
                    sign(adv_global) != sign(`adv_per-region`))
cat(nrow(fl), "of", sum(CP$Region != DROP & !is.na(CP$adv_global) &
                        !is.na(CP$`adv_per-region`)), "comparable cells\n\n")
if (nrow(fl)) print(fl %>% mutate(across(where(is.numeric), ~round(.,3))) %>%
      select(reg, amb, family, adv_global, sig_global,
             `adv_per-region`, `sig_per-region`) %>% as.data.frame())

line("5. WITHIN-MODEL AGREEMENT UNDER EACH BASIS")
wm <- expand_grid(basis=c("global","per-region"), Region=R10_TEN,
                  amb=c("1.5C","2C"), outcome=OUTUSE) %>%
  pmap_dfr(function(basis, Region, amb, outcome) {
    pcol <- ifelse(basis=="global", "Pathway", "Pathway_reg")
    d <- D[D$Region==Region & D$amb==amb, ]
    d <- d[!is.na(d[[pcol]]), ]
    sgn <- ifelse(outcome %in% LOWER5, -1, 1)
    d %>% group_by(fam) %>%
      summarise(na=sum(.data[[pcol]]=="High-CMT" & !is.na(.data[[outcome]])),
                nb=sum(.data[[pcol]]=="High-RE"  & !is.na(.data[[outcome]])),
                dlt = if (na>=3 && nb>=3)
                        sgn*cliff_d(.data[[outcome]][.data[[pcol]]=="High-CMT"],
                                    .data[[outcome]][.data[[pcol]]=="High-RE"])
                      else NA_real_, .groups="drop") %>%
      filter(!is.na(dlt)) %>%
      mutate(basis=basis, Region=Region, amb=amb, outcome=outcome)
  }) %>% inner_join(R2 %>% select(basis,Region,amb,outcome,pooled=adv),
                    by=c("basis","Region","amb","outcome")) %>%
  mutate(agrees = sign(dlt)==sign(pooled), family = OUTS[outcome])
print(wm %>% filter(Region != DROP) %>% group_by(family, basis) %>%
      summarise(families=n(), agree_rate=round(100*mean(agrees)), .groups="drop") %>%
      pivot_wider(names_from=basis, values_from=c(families, agree_rate)) %>%
      as.data.frame())
cat("\nIf the per-region basis does not raise agreement, it does not address the\n")
cat("constraint that actually limits the paper, whatever it does to the counts.\n")
