# =============================================================================
# REBUILD THE MORTALITY CELLS FROM A HARMONISED (NO-AMMONIA) rfasst RUN
#
# Consumes compass_mortality_r10_noNH3.csv from nh3_harmonised_run.R and
# produces the mortality half of the scorecard on the same basis as everything
# else: cumulative 2020-2050, advantage-signed Cliff's delta, cluster-robust
# intervals over model x scenario-family clusters.
#
# TWO DESIGN CHOICES WORTH KNOWING ABOUT:
#
# 1. THE QUALITY GATE IS TAKEN FROM THE ORIGINAL RUN, NOT RECOMPUTED.
#    The gate requires >= 6 non-zero PM2.5 precursors, and the precursor set
#    includes NH3. Recomputing it on a run where ammonia is deliberately zero
#    would fail every scenario. Holding the original gate fixed also keeps the
#    SAME scenarios in both runs, which is what makes the comparison clean.
#
# 2. IT IS RUN ON BOTH FILES AND THE TWO ARE REPORTED SIDE BY SIDE.
#    Feeding it the ORIGINAL mortality file must reproduce the published
#    mortality cells exactly. That is the built-in check: if the with-NH3 pass
#    does not match FINAL_RESULTS, the re-cut is wrong and the no-NH3 numbers
#    cannot be trusted either.
#
# USAGE (from the scratchpad holding the analysis .rds files):
#   Rscript nh3_mortality_rebuild.R  [no_nh3.csv]  [nh3_scenario_region_source.csv]
#   With no argument it runs the self-check on the original file only.
#   The optional second argument is the flag file from nh3_synthetic_regions.R.
#   Supplying it adds a third pass restricted to scenarios whose REGIONAL
#   emissions are real rather than manufactured from a World total by population
#   weight -- the only remaining known threat to the regional mortality cells.
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")
set.seed(20260821)

args   <- commandArgs(trailingOnly = TRUE)
NO_NH3 <- if (length(args) >= 1) args[1] else NA_character_
SRCFLG <- if (length(args) >= 2) args[2] else NA_character_
B      <- 2000
ALLR   <- c("Aggregated R10 regions", R10_TEN)
DROP   <- "R10PAC_OECD"

# ---- the fixed pieces: gate, population, labels, clusters ------------------
mc  <- readRDS("mort_coverage.rds")
ORIG <- read.csv("mort_annual.csv", stringsAsFactors = FALSE)

pop <- readRDS("ds_A.rds") %>% filter(Variable == "Total CDR") %>%
  distinct(Model, Scenario, Region, pop_mln) %>% filter(Region %in% R10_TEN)
POP <- bind_rows(pop, pop %>% group_by(Model, Scenario) %>% filter(n() == 10) %>%
  summarise(pop_mln = sum(pop_mln), .groups = "drop") %>%
  mutate(Region = "Aggregated R10 regions"))

LAB <- readRDS("pw_A.rds") %>% filter(!is.na(Pathway_excl)) %>%
  distinct(Model, Scenario, Pathway = Pathway_excl, Ambition) %>%
  mutate(amb = ifelse(grepl("^1\\.5", Ambition), "1.5C", "2C"),
         fam = sub("[ /].*$", "", Model),
         stem = gsub("[-_ ]?[0-9]+(\\.[0-9]+)?[a-z]?$", "", Scenario),
         stem = sub("/.*$", "", stem), clus = paste(Model, stem))

# gate computed ONCE, on the original run -- see note 1 above
cum <- function(MA) MA %>% filter(year >= 2020, year <= 2050) %>%
  group_by(Model = model, Scenario = scenario, Region = r10_region) %>%
  summarise(deaths_mln = if (all(is.na(deaths_pm25))) NA_real_
                         else sum(deaths_pm25 * 10, na.rm = TRUE)/1e6, .groups = "drop")
gate <- ORIG %>% filter(year >= 2020, year <= 2100) %>%
  group_by(Model = model, Scenario = scenario, Region = r10_region) %>%
  summarise(na = all(is.na(deaths_pm25)), .groups = "drop") %>%
  group_by(Model, Scenario) %>%
  summarise(n_na = sum(na), n_reg = n(), .groups = "drop") %>%
  left_join(mc %>% transmute(Model = model, Scenario = scenario, n_pm_nonzero),
            by = c("Model","Scenario")) %>%
  transmute(Model, Scenario,
            mort_ok = n_na == 0 & n_reg == 10 &
                      !is.na(n_pm_nonzero) & n_pm_nonzero >= 6)
cat("scenarios passing the (original) mortality gate:", sum(gate$mort_ok), "\n")

build <- function(MA, tag) {
  r <- cum(MA)
  w <- r %>% group_by(Model, Scenario) %>% filter(n() == 10) %>%
    summarise(deaths_mln = if (any(is.na(deaths_mln))) NA_real_ else sum(deaths_mln),
              .groups = "drop") %>% mutate(Region = "Aggregated R10 regions")
  bind_rows(r, w) %>%
    inner_join(POP,  by = c("Model","Scenario","Region")) %>%
    inner_join(gate, by = c("Model","Scenario")) %>%
    inner_join(LAB,  by = c("Model","Scenario")) %>%
    transmute(Model, Scenario, Region, amb, Pathway, fam, clus, run = tag,
              mort = ifelse(mort_ok, 1000*deaths_mln/pop_mln, NA_real_))
}

cliff_fast <- function(a, b) {
  n1 <- length(a); n2 <- length(b); if (!n1 || !n2) return(NA_real_)
  r <- rank(c(a, b)); 2*((sum(r[(n1+1):(n1+n2)]) - n2*(n2+1)/2)/(n1*n2)) - 1
}
cell <- function(d) {
  a <- d$mort[d$Pathway=="High-CMT"]; b <- d$mort[d$Pathway=="High-RE"]
  ca <- d$clus[d$Pathway=="High-CMT"]; cb <- d$clus[d$Pathway=="High-RE"]
  ka <- !is.na(a); kb <- !is.na(b); a<-a[ka]; ca<-ca[ka]; b<-b[kb]; cb<-cb[kb]
  if (length(a) < 5 || length(b) < 5)
    return(tibble(n_cmt=length(a), n_re=length(b), med_cmt=NA_real_, med_re=NA_real_,
                  pct=NA_real_, adv=NA_real_, lo=NA_real_, hi=NA_real_))
  ua <- unique(ca); ub <- unique(cb)
  ia <- split(seq_along(a), ca); ib <- split(seq_along(b), cb)
  reps <- vapply(seq_len(B), function(i) {
    sa <- unlist(ia[sample(ua, length(ua), TRUE)], use.names=FALSE)
    sb <- unlist(ib[sample(ub, length(ub), TRUE)], use.names=FALSE)
    -cliff_fast(a[sa], b[sb])
  }, numeric(1))
  q <- quantile(reps, c(.025,.975), na.rm = TRUE)
  tibble(n_cmt=length(a), n_re=length(b), med_cmt=median(a), med_re=median(b),
         pct = -100*(median(b)-median(a))/abs(median(a)),
         adv = -cliff_fast(a,b), lo = q[[1]], hi = q[[2]])
}
sweep <- function(D, tag) expand_grid(Region = ALLR, amb = c("1.5C","2C")) %>%
  pmap_dfr(function(Region, amb)
    bind_cols(tibble(run = tag, Region, amb),
              cell(D[D$Region == Region & D$amb == amb, ]))) %>%
  mutate(sig = !is.na(lo) & (lo > 0 | hi < 0))

# =============================================================================
line("SELF-CHECK — does the re-cut reproduce the published mortality cells?")
# =============================================================================
W <- sweep(build(ORIG, "with NH3"), "with NH3")
pub <- readRDS("FINAL_RESULTS.rds") %>%
  filter(outcome == "mort_per_1k", approach == "A full database",
         sample == "all scenarios") %>%
  select(Region, amb, pub_adv = adv, pub_n_cmt = n_cmt)
chk <- W %>% inner_join(pub, by = c("Region","amb")) %>%
  mutate(d = abs(adv - pub_adv))
cat("cells compared:", sum(!is.na(chk$d)), "| max |difference| in Cliff's delta:",
    signif(max(chk$d, na.rm = TRUE), 3), "\n")
cat("sample sizes match:", all(chk$n_cmt == chk$pub_n_cmt, na.rm = TRUE), "\n")
if (max(chk$d, na.rm = TRUE) > 0.01)
  cat("\n[FAIL] the re-cut does not reproduce the published cells. Stop here.\n") else
  cat("[ok] re-cut reproduces the published mortality exactly.\n")

if (is.na(NO_NH3)) {
  cat("\nNo harmonised file supplied - self-check only.\n")
  cat("Re-run as: Rscript nh3_mortality_rebuild.R <compass_mortality_r10_noNH3.csv>\n")
  quit(save = "no")
}

# =============================================================================
line("HARMONISED RUN — ammonia removed for every model")
# =============================================================================
if (!file.exists(NO_NH3)) stop("Not found: ", NO_NH3)
NA_ <- read.csv(NO_NH3, stringsAsFactors = FALSE)
cat("rows", nrow(NA_), "| scenarios", nrow(distinct(NA_, model, scenario)), "\n")
Z <- sweep(build(NA_, "no NH3"), "no NH3")

cmp <- W %>% select(Region, amb, adv_with = adv, sig_with = sig,
                    med_cmt_w = med_cmt, med_re_w = med_re) %>%
  inner_join(Z %>% select(Region, amb, adv_no = adv, sig_no = sig,
                          med_cmt_n = med_cmt, med_re_n = med_re),
             by = c("Region","amb")) %>%
  mutate(shift = adv_no - adv_with,
         flip = !is.na(adv_with) & !is.na(adv_no) & sign(adv_with) != sign(adv_no),
         shown = Region != DROP)

cat("\nlevel change from removing ammonia (median across cells):",
    sprintf("%+.1f%%\n", 100*median((cmp$med_cmt_n - cmp$med_cmt_w)/cmp$med_cmt_w,
                                    na.rm = TRUE)))
print(cmp %>% mutate(reg = ifelse(Region=="Aggregated R10 regions","WORLD",
                                  sub("^R10","",Region)),
                     across(where(is.numeric), ~round(.,3))) %>%
      select(reg, amb, adv_with, sig_with, adv_no, sig_no, shift, flip) %>%
      as.data.frame())

line("THE MORTALITY LINE FOR THE PAPER")
sh <- cmp %>% filter(shown, !is.na(adv_no))
cat("cells shown (nine regions + World):", nrow(sh), "\n")
cat("  with ammonia as reported : ", sum(sh$adv_with > 0), " favour High-RE, ",
    sum(sh$adv_with > 0 & sh$sig_with), " significantly\n", sep="")
cat("  ammonia harmonised       : ", sum(sh$adv_no > 0), " favour High-RE, ",
    sum(sh$adv_no > 0 & sh$sig_no), " significantly\n", sep="")
cat("  sign changes:", sum(sh$flip), "| median shift:", round(median(sh$shift),3), "\n")
saveRDS(cmp, "NH3_MORT_REBUILD.rds")
cat("\nwritten: NH3_MORT_REBUILD.rds\n")

# =============================================================================
# THIRD PASS — real regional emissions only
#
# Scenarios that report emissions only at World have their R10 detail filled in
# by population weight inside the rfasst script. Their regional PM2.5 therefore
# carries no pathway information: every region gets the same per-capita
# intensity by construction. That is harmless if the two arms hold equal shares
# of such scenarios and biasing if they do not. This pass drops them and asks
# whether the harmonised regional result survives on genuine regional data.
#
# The WORLD row is unaffected either way -- a World total is a World total,
# however it was later split -- so it is reported from the full sample.
# =============================================================================
if (!is.na(SRCFLG)) {
  line("REAL-REGION SUBSET — dropping World-derived regional emissions")
  if (!file.exists(SRCFLG)) stop("Not found: ", SRCFLG)
  FL <- read.csv(SRCFLG, stringsAsFactors = FALSE)
  cat("flag file rows:", nrow(FL), "\n")
  print(as.data.frame(table(FL$Pathway, FL$source)))

  keep <- FL %>% filter(source == "real regions") %>%
    distinct(Model = model, Scenario = scenario)
  cat("\nscenarios with real regional emissions:", nrow(keep), "of", nrow(FL), "\n")

  D  <- build(NA_, "no NH3") %>% semi_join(keep, by = c("Model","Scenario"))
  RR <- sweep(D, "no NH3, real regions")

  cmp2 <- cmp %>% select(Region, amb, adv_no, sig_no, shown) %>%
    inner_join(RR %>% select(Region, amb, adv_rr = adv, sig_rr = sig,
                             n_cmt_rr = n_cmt, n_re_rr = n_re),
               by = c("Region","amb")) %>%
    mutate(flip = !is.na(adv_no) & !is.na(adv_rr) & sign(adv_no) != sign(adv_rr))
  print(cmp2 %>% mutate(reg = ifelse(Region=="Aggregated R10 regions","WORLD",
                                     sub("^R10","",Region)),
                        across(where(is.numeric), ~round(.,3))) %>%
        select(reg, amb, n_cmt_rr, n_re_rr, adv_no, sig_no, adv_rr, sig_rr, flip) %>%
        as.data.frame())

  s2 <- cmp2 %>% filter(shown, !is.na(adv_rr))
  cat("\ncells retaining >= 5 per arm after the restriction:", nrow(s2), "of 20\n")
  cat("  favouring High-RE:", sum(s2$adv_rr > 0), "|",
      sum(s2$adv_rr > 0 & s2$sig_rr), "significantly\n")
  cat("  sign changes against the full harmonised sample:", sum(s2$flip), "\n")
  saveRDS(cmp2, "NH3_MORT_REALREGION.rds")
  cat("written: NH3_MORT_REALREGION.rds\n")
  cat("\nIf cells are lost to sample size rather than flipped, the restriction\n")
  cat("costs power, not direction -- report the full sample and this as a check.\n")
}
