# =============================================================================
# S2 — deprivation re-cut to the net-zero window.
#
# The DLE annual file is TRULY annual (2020-2100, every year), unlike the
# decadal mortality file, so the master's sum(gap_EJ_total) is already a real
# 81-year integral and needs no interval multiplier. Headcount is a MEAN, not a
# sum, so it is re-cut as a mean over the window.
#
# Master (Section 5): cumulative_gap_EJ = sum(gap_EJ_total)
#                     mean_headcount_millions = mean(headcount_millions)
#          per-capita: gap_GJ_pc = cumulative_gap_EJ / pop_mln
#                      headcount_pct = mean_headcount_millions / pop_mln * 100
# =============================================================================
source("stratified.R.fns")
options(width = 180)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

DA <- readRDS("dle_annual.rds")
line("1. THE ANNUAL FILE")
cat("rows", nrow(DA), "| scenarios", nrow(distinct(DA, Model, Scenario)),
    "| years", min(DA$Year), "-", max(DA$Year), "(", n_distinct(DA$Year), "distinct )\n")

cum_window <- function(y_max) {
  r <- DA %>% filter(Year >= 2020, Year <= y_max) %>%
    group_by(Model, Scenario, Region) %>%
    summarise(gap_EJ = sum(gap_EJ_total, na.rm = TRUE),
              hc_mln = mean(headcount_millions, na.rm = TRUE), .groups = "drop")
  w <- r %>% group_by(Model, Scenario) %>% filter(n() == 10) %>%
    summarise(gap_EJ = sum(gap_EJ), hc_mln = sum(hc_mln), .groups = "drop") %>%
    mutate(Region = "Aggregated R10 regions")
  bind_rows(r, w)
}
D50 <- cum_window(2050); D00 <- cum_window(2100)

line("2. DOES THE 2020-2100 RE-CUT REPRODUCE THE MASTER?")
ds <- readRDS("ds_A.rds") %>% filter(Variable == "Total CDR") %>%
  select(Model, Scenario, Region, pop_mln,
         m_gap = cumulative_gap_EJ, m_hc = mean_headcount_millions,
         m_gap_pc = gap_GJ_pc, m_hc_pct = headcount_pct)
chk <- D00 %>% inner_join(ds, by = c("Model","Scenario","Region")) %>%
  filter(!is.na(m_gap), m_gap > 0)
cat("comparable cells:", nrow(chk), "\n")
cat("  gap       median ratio", round(median(chk$gap_EJ / chk$m_gap), 5),
    "| cor", round(cor(chk$gap_EJ, chk$m_gap), 6), "\n")
cat("  headcount median ratio", round(median(chk$hc_mln / chk$m_hc, na.rm=TRUE), 5),
    "| cor", round(cor(chk$hc_mln, chk$m_hc, use="complete.obs"), 6), "\n")
cat("\nper-capita convention check (master gap_GJ_pc vs cumulative_gap_EJ/pop):\n")
cat("  median ratio", round(median(chk$m_gap_pc / (chk$m_gap/chk$pop_mln)), 5), "\n")
cat("  headcount_pct vs mean_hc/pop*100 median ratio",
    round(median(chk$m_hc_pct / (chk$m_hc/chk$pop_mln*100), na.rm=TRUE), 5), "\n")

line("3. BUILD PER-CAPITA, BOTH WINDOWS")
pop <- ds %>% distinct(Model, Scenario, Region, pop_mln) %>% filter(Region %in% R10_TEN)
popw <- pop %>% group_by(Model, Scenario) %>% filter(n() == 10) %>%
  summarise(pop_mln = sum(pop_mln), .groups = "drop") %>%
  mutate(Region = "Aggregated R10 regions")
POP <- bind_rows(pop, popw)
# x1000: the master reports gap in GJ/capita from a cumulative total in EJ over a
# population in millions, so EJ/mln -> GJ/cap carries a factor of 1000. Verified
# against the master above (median ratio exactly 1000).
mk <- function(D, tag) D %>% inner_join(POP, by = c("Model","Scenario","Region")) %>%
  transmute(Model, Scenario, Region, window = tag,
            gap_GJ_pc = gap_EJ / pop_mln * 1000,
            headcount_pct = hc_mln / pop_mln * 100)
DD <- bind_rows(mk(D50, "2020-2050"), mk(D00, "2020-2100"))
saveRDS(DD, "DLE_WINDOWS.rds")
cat("rows written:", nrow(DD), "| scenarios", nrow(distinct(DD, Model, Scenario)), "\n")

line("4. DOES THE WINDOW CHANGE THE DEPRIVATION VERDICT?")
LAB <- readRDS("pw_A.rds") %>% filter(!is.na(Pathway_excl)) %>%
  distinct(Model, Scenario, Pathway = Pathway_excl)
AMB <- ds %>% distinct(Model, Scenario) %>%
  left_join(readRDS("ds_A.rds") %>% filter(Variable=="Total CDR") %>%
              distinct(Model, Scenario, Ambition), by=c("Model","Scenario")) %>%
  mutate(amb = ifelse(grepl("^1\\.5", Ambition), "1.5C", "2C"))
X <- DD %>% inner_join(LAB, by=c("Model","Scenario")) %>%
  inner_join(AMB, by=c("Model","Scenario")) %>%
  mutate(Pathway = factor(Pathway, levels = PATHWAYS))

res <- expand_grid(Region = c("Aggregated R10 regions", R10_TEN),
                   amb = c("1.5C","2C"), window = c("2020-2050","2020-2100"),
                   outcome = c("gap_GJ_pc","headcount_pct")) %>%
  pmap_dfr(function(Region, amb, window, outcome) {
    s <- X[X$Region==Region & X$amb==amb & X$window==window, ]
    a <- s[[outcome]][s$Pathway=="High-CMT"]; b <- s[[outcome]][s$Pathway=="High-RE"]
    a<-a[!is.na(a)]; b<-b[!is.na(b)]
    if (length(a)<5 || length(b)<5)
      return(tibble(Region,amb,window,outcome,adv=NA_real_,pct=NA_real_,p=NA_real_,
                    med_cmt=NA_real_,med_re=NA_real_,n_cmt=length(a),n_re=length(b)))
    tibble(Region, amb, window, outcome, adv = -cliff_d(a,b),
           pct = -100*(median(b)-median(a))/abs(median(a)),
           p = suppressWarnings(wilcox.test(a,b))$p.value,
           med_cmt=median(a), med_re=median(b), n_cmt=length(a), n_re=length(b))
  }) %>% group_by(window) %>% mutate(p_fdr = p.adjust(p,"BH")) %>% ungroup() %>%
  mutate(sig = !is.na(p_fdr) & p_fdr<0.05, win = adv>0)
saveRDS(res, "DLE_WINDOW_RES.rds")

cat("\ncells won by High-RE:\n")
print(res %>% filter(!is.na(win)) %>% group_by(window, outcome) %>%
      summarise(cells=n(), RE=sum(win), sig_RE=sum(win&sig), sig_CMT=sum(!win&sig),
                .groups="drop") %>% as.data.frame())

cat("\nWORLD:\n")
print(res %>% filter(Region=="Aggregated R10 regions") %>%
      select(amb,outcome,window,med_cmt,med_re,pct,adv,p_fdr,sig) %>%
      mutate(across(where(is.numeric),~round(.,3))) %>% as.data.frame())

cat("\nsign changes between windows (gap):\n")
fl <- res %>% filter(outcome=="gap_GJ_pc") %>% select(Region,amb,window,adv) %>%
  pivot_wider(names_from=window, values_from=adv) %>%
  filter(!is.na(`2020-2050`), !is.na(`2020-2100`)) %>%
  mutate(flip = sign(`2020-2050`) != sign(`2020-2100`))
cat(sum(fl$flip), "of", nrow(fl), "\n")
print(fl %>% mutate(across(where(is.numeric),~round(.,2))) %>% as.data.frame())
