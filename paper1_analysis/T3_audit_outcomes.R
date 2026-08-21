# =============================================================================
# T3 — AUDIT SECTION 3: HOW THE FIVE OUTCOMES ARE BUILT
#
# Things that would quietly invalidate a result and are invisible in the output:
#   1. pop_mln — WHICH year? A 2020-2050 cumulative divided by a single
#      population number is only meaningful if we can say which one.
#   2. Jobs — are employment factors constant? If jobs = capacity x a fixed
#      coefficient, the jobs outcome is the RE axis rescaled, not a result.
#   3. Mortality — what does the n_pm_nonzero >= 6 gate actually remove, and
#      is it removing scenarios non-randomly with respect to pathway?
#   4. Deprivation — gap is a SUM over the window, headcount is a MEAN. Those
#      two answer different questions and must not be described as one thing.
#   5. Do any outcomes have zero/degenerate variance in a region (which would
#      make Cliff's delta meaningless there)?
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

ds <- readRDS("ds_A.rds")
F  <- load_frame("A")

# =============================================================================
line("1. WHAT IS pop_mln?")
# =============================================================================
p <- ds %>% filter(Variable == "Total CDR") %>%
  distinct(Model, Scenario, Region, pop_mln)
cat("one pop_mln per scenario-region?",
    nrow(p) == nrow(distinct(p, Model, Scenario, Region)), "\n")
cat("World total implied by the ten-region sum, median across scenarios:\n")
w <- p %>% filter(Region %in% R10_TEN) %>% group_by(Model, Scenario) %>%
  filter(n() == 10) %>% summarise(tot = sum(pop_mln), .groups = "drop")
cat("  median", round(median(w$tot)), "mln | IQR", round(quantile(w$tot,.25)),
    "-", round(quantile(w$tot,.75)), "| range", round(min(w$tot)), "-", round(max(w$tot)), "\n")
cat("\nBENCHMARK world population: 2020 = 7,800 mln | 2050 = 9,700 mln\n")
cat("  8,000-10,000  -> a SINGLE year's population (a snapshot)\n")
cat("  >100,000      -> a person-years integral over the window\n")
cat("VERDICT: ", if (median(w$tot) > 5e4) "PERSON-YEARS integral"
              else if (median(w$tot) > 6000 & median(w$tot) < 12000) "SINGLE-YEAR snapshot"
              else "neither - investigate", "\n")
cat("\nregional populations, median:\n")
print(p %>% filter(Region %in% R10_TEN) %>% group_by(Region) %>%
      summarise(med_pop_mln = round(median(pop_mln)),
                share_pct = NA_real_, .groups = "drop") %>%
      mutate(share_pct = round(100*med_pop_mln/sum(med_pop_mln), 1)) %>% as.data.frame())
cat("\nBENCHMARK 2050 shares: Africa ~25%, India+ ~21%, China+ ~16%, Rest Asia ~11%,\n")
cat("  Latin Am ~8%, Europe ~7%, North Am ~6%, Middle East ~5%, REF ~4%, Pac OECD ~2%\n")

# =============================================================================
line("2. JOBS — are employment factors constant?")
# =============================================================================
jt <- readRDS("jobs_type.rds")
cat("columns:", paste(names(jt), collapse = ", "), "\n")
cat("tech groups:", paste(sort(unique(jt$tech_group)), collapse = " | "), "\n")
cat("years:", paste(sort(unique(jt$Year)), collapse = " "), "\n")

# If jobs were capacity x a fixed factor, jobs/capacity would be constant.
cap <- ds %>% filter(Variable == "Renewable Capacity", Region %in% R10_TEN) %>%
  group_by(Model, Scenario, Region) %>% summarise(cap = sum(Total_Value, na.rm=TRUE), .groups="drop")
rej <- jt %>% filter(Year >= 2020, Year <= 2050, Region %in% R10_TEN,
                     !tech_group %in% c("Fossil","Nuclear","Bioenergy")) %>%
  group_by(Model, Scenario, Region) %>%
  summarise(rej = sum(jobs_thousands, na.rm=TRUE), .groups="drop")
cc <- cap %>% inner_join(rej, by=c("Model","Scenario","Region")) %>% filter(cap > 0, rej > 0) %>%
  mutate(ratio = rej/cap)
cat("\njobs-per-unit-capacity ratio, by region (if near-constant, jobs is capacity rescaled):\n")
print(cc %>% group_by(Region) %>%
      summarise(n=n(), med = signif(median(ratio),3),
                iqr_over_med = round((quantile(ratio,.75)-quantile(ratio,.25))/median(ratio), 2),
                .groups="drop") %>% as.data.frame())
cat("\nIQR/median near 0 = a fixed coefficient. Above ~0.3 = real variation.\n")
cat("overall spearman(RE capacity, RE jobs):", round(cor(cc$cap, cc$rej, method="spearman"), 3), "\n")

cat("\nDoes the employment factor DECLINE over time (learning)?  jobs per decade:\n")
print(jt %>% filter(Region %in% R10_TEN, Year >= 2020, Year <= 2050) %>%
      mutate(g = case_when(tech_group=="Fossil"~"Fossil", tech_group=="Nuclear"~"Nuclear",
                           tech_group=="Bioenergy"~"Bioenergy", TRUE~"Renewables")) %>%
      group_by(g, Year) %>% summarise(median_k = round(median(jobs_thousands, na.rm=TRUE),1),
                                      .groups="drop") %>%
      pivot_wider(names_from=Year, values_from=median_k) %>% as.data.frame())

cat("\nCIRCULARITY CHECK — correlation of each axis with each jobs contrast:\n")
ax <- readRDS("pw_A.rds") %>% select(Model, Scenario, total_cdr, total_re)
cj <- F %>% filter(Region %in% R10_TEN) %>% inner_join(ax, by=c("Model","Scenario"))
print(cj %>% group_by(Region) %>%
      summarise(rho_RE_axis_vs_REFOSS  = round(cor(total_re, REFOSS, method="spearman", use="complete.obs"),2),
                rho_CMT_axis_vs_REFOSS = round(cor(total_cdr, REFOSS, method="spearman", use="complete.obs"),2),
                .groups="drop") %>% as.data.frame())
cat("\nThe RE axis is global (ten-region sum) but jobs here are REGIONAL, so a high\n")
cat("rho is transmission, not tautology -- a global RE build has to land somewhere.\n")
cat("The CMT column is the control: near zero means the axes are not interchangeable.\n")

# =============================================================================
line("3. MORTALITY — what does the coverage gate remove?")
# =============================================================================
mc <- readRDS("mort_coverage.rds")
pwl <- readRDS("pw_A.rds") %>% filter(!is.na(Pathway_excl)) %>%
  distinct(Model, Scenario, Pathway = Pathway_excl)
g <- mc %>% transmute(Model=model, Scenario=scenario, n_pm_nonzero) %>%
  right_join(pwl, by=c("Model","Scenario")) %>%
  mutate(pass = !is.na(n_pm_nonzero) & n_pm_nonzero >= 6,
         fam = sub("[ /].*$","",Model))
cat("classified scenarios:", nrow(g), "| pass the >=6 precursor gate:", sum(g$pass),
    sprintf(" (%.0f%%)\n", 100*mean(g$pass)))
cat("\nIS THE GATE BALANCED ACROSS PATHWAYS?\n")
print(g %>% count(Pathway, pass) %>% pivot_wider(names_from=pass, values_from=n, values_fill=0,
      names_prefix="pass_") %>% mutate(pct_pass = round(100*pass_TRUE/(pass_TRUE+pass_FALSE))) %>%
      as.data.frame())
cat("\nby family:\n")
print(g %>% group_by(fam) %>% summarise(n=n(), n_pass=sum(pass), .groups="drop") %>%
      mutate(pct=round(100*n_pass/n)) %>% arrange(pct) %>% as.data.frame())
cat("\nA gate that drops High-CMT and High-RE at different rates changes WHICH\n")
cat("scenarios are compared, not just how many.\n")

# =============================================================================
line("4. DEPRIVATION — the gap is a SUM, the headcount is a MEAN")
# =============================================================================
DA <- readRDS("dle_annual.rds")
cat("gap_EJ_total is a FLOW (EJ/yr), so summing it over 31 years gives EJ of\n")
cat("cumulative shortfall -- a stock. headcount_millions is a STOCK (people\n")
cat("deprived in that year), so summing it would be person-years; the master\n")
cat("takes a MEAN instead, giving the average number deprived over the window.\n")
cat("These are different quantities and must be labelled as such.\n\n")
cat("Do the two move together?  correlation of the two per-capita outcomes:\n")
print(F %>% filter(Region %in% R10_TEN) %>% group_by(Region) %>%
      summarise(rho = round(cor(gap_GJ_pc, headcount_pct, method="spearman", use="complete.obs"),3),
                .groups="drop") %>% as.data.frame())
cat("\nIf rho ~ 1 everywhere the two outcomes are ONE result counted twice, and the\n")
cat("scorecard should say so rather than treating them as independent evidence.\n")

# =============================================================================
line("5. DEGENERATE CELLS — is there anything to compare?")
# =============================================================================
deg <- expand_grid(Region = c("Aggregated R10 regions", R10_TEN), amb = c("1.5C","2C"),
                   outcome = names(OUT5)) %>%
  pmap_dfr(function(Region, amb, outcome) {
    x <- F[[outcome]][F$Region==Region & F$amb==amb]; x <- x[!is.na(x)]
    tibble(Region, amb, outcome, n = length(x),
           pct_zero = ifelse(length(x), round(100*mean(x == 0)), NA_real_),
           n_unique = n_distinct(x),
           cv = ifelse(length(x) && median(x) != 0,
                       round(IQR(x)/abs(median(x)), 2), NA_real_))
  })
cat("cells where more than 10% of values are exactly zero, or fewer than 20 unique values:\n")
bad <- deg %>% filter(pct_zero > 10 | n_unique < 20)
if (nrow(bad)) print(bad %>% mutate(label = OUT5[outcome]) %>%
                     select(Region, amb, label, n, pct_zero, n_unique, cv) %>% as.data.frame()) else
  cat("  none\n")
cat("\nspread (IQR/|median|) by outcome -- a near-zero spread means the pathway\n")
cat("contrast is riding on noise:\n")
print(deg %>% mutate(label = OUT5[outcome]) %>% group_by(label) %>%
      summarise(min_cv = min(cv, na.rm=TRUE), med_cv = round(median(cv, na.rm=TRUE),2),
                max_cv = max(cv, na.rm=TRUE), .groups="drop") %>% as.data.frame())
