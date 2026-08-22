# =============================================================================
# THE FINAL GRID WITH AMMONIA HARMONISED
#
# nh3_mortality_rebuild.R re-cut mortality on ONE of the four design cells
# (full database, all scenarios). The paper reports all four -- full database
# and SCI-vetted, crossed with all-scenarios and the depth-matched subsample --
# so mortality has to be re-cut on all four before the scorecard can be
# recounted. That is what this does.
#
# METHOD. The harmonised run gives a new deaths_pm25 per model x scenario x
# region x year. This rebuilds mort_per_1k for the 2020-2050 window exactly as
# load_frame() does, substitutes it into frames A and C, and re-runs U1's own
# sweep for mort_per_1k alone. Jobs and deprivation are copied through
# untouched -- ammonia cannot reach them.
#
# THE GATE IS HELD AT THE ORIGINAL RUN, deliberately: it requires >= 6 non-zero
# PM2.5 precursors and ammonia is one of them, so recomputing it on a run where
# NH3 is zero by construction would fail every scenario. Holding it fixed also
# keeps the two runs on the SAME scenarios, which is what makes them comparable.
#
# USAGE:
#   Rscript nh3_final_grid.R compass_mortality_r10_noNH3.csv
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")
set.seed(20260821)

args   <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("supply compass_mortality_r10_noNH3.csv")
NO_NH3 <- args[1]
B      <- 2000
ALLR   <- c("Aggregated R10 regions", R10_TEN)
DROP   <- "R10PAC_OECD"

# ---- 1. harmonised mort_per_1k, built the way load_frame() builds it --------
line("1. REBUILDING mort_per_1k WITH AMMONIA REMOVED")
NA_ <- read.csv(NO_NH3, stringsAsFactors = FALSE)
mc  <- readRDS("mort_coverage.rds")
ORIG <- read.csv("mort_annual.csv", stringsAsFactors = FALSE)
cat("harmonised rows:", nrow(NA_), "| scenarios:", nrow(distinct(NA_, model, scenario)),
    "| regions:", n_distinct(NA_$r10_region), "\n")

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

pop <- readRDS("ds_A.rds") %>% filter(Variable == "Total CDR") %>%
  distinct(Model, Scenario, Region, pop_mln) %>% filter(Region %in% R10_TEN)
POP <- bind_rows(pop, pop %>% group_by(Model, Scenario) %>% filter(n() == 10) %>%
  summarise(pop_mln = sum(pop_mln), .groups = "drop") %>%
  mutate(Region = "Aggregated R10 regions"))

r <- NA_ %>% filter(year >= 2020, year <= 2050) %>%
  group_by(Model = model, Scenario = scenario, Region = r10_region) %>%
  summarise(deaths_mln = if (all(is.na(deaths_pm25))) NA_real_
                         else sum(deaths_pm25 * 10, na.rm = TRUE)/1e6, .groups = "drop")
w <- r %>% group_by(Model, Scenario) %>% filter(n() == 10) %>%
  summarise(deaths_mln = if (any(is.na(deaths_mln))) NA_real_ else sum(deaths_mln),
            .groups = "drop") %>% mutate(Region = "Aggregated R10 regions")
MORT_NH3 <- bind_rows(r, w) %>%
  inner_join(POP,  by = c("Model","Scenario","Region")) %>%
  inner_join(gate, by = c("Model","Scenario")) %>%
  transmute(Model, Scenario, Region,
            mort_nh3 = ifelse(mort_ok, 1000*deaths_mln/pop_mln, NA_real_))
cat("harmonised mortality values:", sum(!is.na(MORT_NH3$mort_nh3)),
    "over", n_distinct(paste(MORT_NH3$Model, MORT_NH3$Scenario)), "scenarios\n")

# ---- 2. U1's machinery, verbatim -------------------------------------------
cliff_fast <- function(a, b) {
  n1 <- length(a); n2 <- length(b); if (!n1 || !n2) return(NA_real_)
  r <- rank(c(a, b)); 2*((sum(r[(n1+1):(n1+n2)]) - n2*(n2+1)/2)/(n1*n2)) - 1
}
cell_full <- function(d, out) {
  sgn <- ifelse(out %in% LOWER5, -1, 1)
  a <- d[[out]][d$Pathway=="High-CMT"]; b <- d[[out]][d$Pathway=="High-RE"]
  ca <- d$clus[d$Pathway=="High-CMT"];  cb <- d$clus[d$Pathway=="High-RE"]
  ka <- !is.na(a); kb <- !is.na(b); a<-a[ka]; ca<-ca[ka]; b<-b[kb]; cb<-cb[kb]
  na <- length(a); nb <- length(b)
  if (na < 5 || nb < 5)
    return(tibble(n_cmt=na, n_re=nb, n_clus=NA_integer_, med_cmt=NA_real_,
                  med_re=NA_real_, pct=NA_real_, adv=NA_real_, lo=NA_real_,
                  hi=NA_real_, p_wilcox=NA_real_))
  ua <- unique(ca); ub <- unique(cb)
  ia <- split(seq_len(na), ca); ib <- split(seq_len(nb), cb)
  reps <- vapply(seq_len(B), function(i) {
    sa <- unlist(ia[sample(ua, length(ua), TRUE)], use.names=FALSE)
    sb <- unlist(ib[sample(ub, length(ub), TRUE)], use.names=FALSE)
    cliff_fast(a[sa], b[sb])
  }, numeric(1))
  q <- quantile(sgn*reps, c(.025,.975), na.rm=TRUE)
  tibble(n_cmt=na, n_re=nb, n_clus=length(ua)+length(ub),
         med_cmt=median(a), med_re=median(b),
         pct = sgn*100*(median(b)-median(a))/abs(median(a)),
         adv = sgn*cliff_fast(a,b), lo = q[[1]], hi = q[[2]],
         p_wilcox = suppressWarnings(wilcox.test(a,b))$p.value)
}
build <- function(id) load_frame(id) %>%
  mutate(stem = gsub("[-_ ]?[0-9]+(\\.[0-9]+)?[a-z]?$", "", Scenario),
         stem = sub("/.*$", "", stem), clus = paste(Model, stem)) %>%
  select(-mort_per_1k) %>%
  left_join(MORT_NH3, by = c("Model","Scenario","Region")) %>%
  rename(mort_per_1k = mort_nh3)
matched <- function(d) d %>% filter(Region %in% R10_TEN) %>%
  distinct(Model,Scenario,Region) %>% count(Model,Scenario) %>% filter(n==10) %>%
  transmute(k = paste(Model,Scenario))
sweep1 <- function(d, approach, sample_tag)
  expand_grid(Region=ALLR, amb=c("1.5C","2C")) %>%
    pmap_dfr(function(Region, amb)
      bind_cols(tibble(approach, sample=sample_tag, Region, amb,
                       outcome="mort_per_1k"),
                cell_full(d[d$Region==Region & d$amb==amb, ], "mort_per_1k")))

line("2. RE-SWEEPING MORTALITY ON ALL FOUR DESIGN CELLS")
A <- build("A"); C <- build("C")
M <- bind_rows(
  sweep1(A, "A full database", "all scenarios"),
  sweep1(A %>% filter(paste(Model,Scenario) %in% matched(A)$k), "A full database", "matched only"),
  sweep1(C, "C SCI-vetted",    "all scenarios"),
  sweep1(C %>% filter(paste(Model,Scenario) %in% matched(C)$k), "C SCI-vetted", "matched only")
) %>% mutate(family = "Health", primary = TRUE, label = OUT5["mort_per_1k"],
             win = ifelse(is.na(adv), NA, adv > 0),
             sig = !is.na(lo) & (lo > 0 | hi < 0),
             is_world = Region == "Aggregated R10 regions", window = "2020-2050")

# ---- 3. splice into FINAL_RESULTS ------------------------------------------
line("3. THE FINAL GRID")
OLD <- readRDS("FINAL_RESULTS.rds")
NEW <- bind_rows(OLD %>% filter(outcome != "mort_per_1k"),
                 M %>% mutate(p_fdr = NA_real_, sig_naive = NA)) %>%
  group_by(approach, sample) %>%
  mutate(p_fdr = p.adjust(p_wilcox, "BH"),
         sig_naive = !is.na(p_fdr) & p_fdr < 0.05) %>% ungroup()
saveRDS(NEW, "FINAL_RESULTS_NH3.rds")
cat("written: FINAL_RESULTS_NH3.rds\n")

# THE SCORECARD. Nine regions plus World, primary measure per family, so 20
# cells per family and 60 in total -- the same denominator the deck reports.
score <- function(D, tag) {
  s <- D %>% filter(primary, Region != DROP, !is.na(adv))
  cat("\n", tag, "\n", sep="")
  print(s %>% group_by(family, approach, sample) %>%
        summarise(v = paste0(sum(adv>0), "/", n()), .groups="drop") %>%
        pivot_wider(names_from = c(approach, sample), values_from = v) %>%
        as.data.frame())
  h <- s %>% filter(approach=="A full database", sample=="all scenarios")
  cat(sprintf("\n  headline: %d of %d favour High-RE | %d significantly for | %d significantly against\n",
      sum(h$adv>0), nrow(h), sum(h$adv>0 & h$sig), sum(h$adv<0 & h$sig)))
}
score(OLD, "BEFORE — ammonia as each model reports it")
score(NEW, "AFTER  — ammonia harmonised across all models")

line("MORTALITY CELL BY CELL, HARMONISED")
print(M %>% filter(approach=="A full database", sample=="all scenarios") %>%
      mutate(reg = ifelse(is_world,"WORLD",sub("^R10","",Region)),
             across(where(is.numeric), ~round(.,3))) %>%
      select(reg, amb, n_cmt, n_re, med_cmt, med_re, pct, adv, lo, hi, sig) %>%
      as.data.frame())

line("DOES THE HARMONISED MORTALITY HOLD ACROSS THE FOUR DESIGNS?")
print(M %>% filter(Region != DROP, !is.na(adv)) %>% group_by(approach, sample) %>%
      summarise(cells = n(), RE = sum(adv>0), sig_for = sum(adv>0 & sig),
                sig_against = sum(adv<0 & sig), med = round(median(adv),3),
                .groups="drop") %>% as.data.frame())
