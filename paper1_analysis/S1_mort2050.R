# =============================================================================
# S1 — mortality re-cut to the net-zero window, from the annual file.
#
# Matches the master script's convention exactly: annual deaths are carried over
# 10-year blocks, sum(deaths * 10) / 1e6, and an all-NA scenario-region stays NA
# rather than becoming zero. Same mortality quality gate as before: all ten R10
# cells non-NA AND all six PM2.5 precursors reported non-zero.
#
# The annual file is decadal (2010, 2020, ... 2100), so 2020-2050 is four
# timesteps. Note the convention counts 40 years for a 31-year window, exactly
# as 2020-2100 counts 90 for 81 -- an overcount, but identical for both arms.
# =============================================================================
source("stratified.R.fns")
options(width = 180)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

MA <- read.csv("mort_annual.csv", stringsAsFactors = FALSE)
mc <- readRDS("mort_coverage.rds")

line("1. THE ANNUAL FILE")
cat("rows", nrow(MA), "| scenarios", nrow(distinct(MA, model, scenario)),
    "| years", paste(sort(unique(MA$year)), collapse=", "), "\n")

cum_window <- function(y_max) {
  r <- MA %>% filter(year >= 2020, year <= y_max) %>%
    group_by(Model = model, Scenario = scenario, Region = r10_region) %>%
    # deaths_pm25, NOT deaths_total: the master picks the first of
    # (deaths_pm25, FUSION, deaths_total) that exists, i.e. PM2.5 only. Using
    # deaths_total would add ozone and inflate every level by ~7%.
    summarise(deaths_mln = if (all(is.na(deaths_pm25))) NA_real_
                           else sum(deaths_pm25 * 10, na.rm = TRUE) / 1e6,
              .groups = "drop")
  # World = ten-region sum, only where all ten are present and none is NA
  w <- r %>% group_by(Model, Scenario) %>%
    filter(n() == 10) %>%
    summarise(deaths_mln = if (any(is.na(deaths_mln))) NA_real_ else sum(deaths_mln),
              .groups = "drop") %>%
    mutate(Region = "Aggregated R10 regions")
  bind_rows(r, w)
}

M50 <- cum_window(2050); M00 <- cum_window(2100)

line("2. DOES THE 2020-2100 RE-CUT REPRODUCE THE MASTER?")
ds <- readRDS("ds_A.rds") %>% filter(Variable=="Total CDR") %>%
  select(Model, Scenario, Region, master = cumulative_deaths_mln)
chk <- M00 %>% inner_join(ds, by=c("Model","Scenario","Region")) %>%
  filter(!is.na(deaths_mln), !is.na(master), master > 0)
cat("comparable cells:", nrow(chk), "| median ratio recut/master:",
    round(median(chk$deaths_mln/chk$master), 4),
    "| correlation:", round(cor(chk$deaths_mln, chk$master), 5), "\n")

line("3. GATE AND PER-CAPITA, BOTH WINDOWS")
pop <- readRDS("ds_A.rds") %>% filter(Variable=="Total CDR") %>%
  distinct(Model, Scenario, Region, pop_mln) %>% filter(Region %in% R10_TEN)
popw <- pop %>% group_by(Model,Scenario) %>% filter(n()==10) %>%
  summarise(pop_mln=sum(pop_mln), .groups="drop") %>%
  mutate(Region="Aggregated R10 regions")
POP <- bind_rows(pop, popw)

gate <- M00 %>% filter(Region %in% R10_TEN) %>%
  group_by(Model, Scenario) %>%
  summarise(n_na = sum(is.na(deaths_mln)), n_reg = n(), .groups="drop") %>%
  left_join(mc %>% transmute(Model=model, Scenario=scenario, n_pm_nonzero),
            by=c("Model","Scenario")) %>%
  transmute(Model, Scenario,
            mort_ok = n_na == 0 & n_reg == 10 &
                      !is.na(n_pm_nonzero) & n_pm_nonzero >= 6)
cat("scenarios passing the gate:", sum(gate$mort_ok), "of", nrow(gate), "\n")

mk <- function(M, tag) M %>% inner_join(POP, by=c("Model","Scenario","Region")) %>%
  inner_join(gate, by=c("Model","Scenario")) %>%
  transmute(Model, Scenario, Region,
            mort = ifelse(mort_ok, 1000*deaths_mln/pop_mln, NA_real_), window = tag)
MM <- bind_rows(mk(M50,"2020-2050"), mk(M00,"2020-2100"))
saveRDS(MM, "MORT_WINDOWS.rds")

line("4. DOES THE WINDOW CHANGE THE MORTALITY VERDICT?")
LAB <- readRDS("pw_A.rds") %>% filter(!is.na(Pathway_excl)) %>%
  distinct(Model, Scenario, Pathway = Pathway_excl)
AMB <- readRDS("ds_A.rds") %>% filter(Variable=="Total CDR") %>%
  distinct(Model, Scenario, Ambition) %>%
  mutate(amb = ifelse(grepl("^1\\.5", Ambition), "1.5C", "2C"))
D <- MM %>% inner_join(LAB, by=c("Model","Scenario")) %>%
  inner_join(AMB, by=c("Model","Scenario")) %>%
  mutate(Pathway = factor(Pathway, levels=PATHWAYS))

res <- expand_grid(Region=c("Aggregated R10 regions",R10_TEN),
                   amb=c("1.5C","2C"), window=c("2020-2050","2020-2100")) %>%
  pmap_dfr(function(Region,amb,window){
    s <- D[D$Region==Region & D$amb==amb & D$window==window, ]
    a <- s$mort[s$Pathway=="High-CMT"]; b <- s$mort[s$Pathway=="High-RE"]
    a<-a[!is.na(a)]; b<-b[!is.na(b)]
    if(length(a)<5||length(b)<5) return(tibble(Region,amb,window,adv=NA_real_,
      pct=NA_real_,p=NA_real_,n_cmt=length(a),n_re=length(b)))
    tibble(Region, amb, window, adv = -cliff_d(a,b),
           pct = -100*(median(b)-median(a))/abs(median(a)),
           p = suppressWarnings(wilcox.test(a,b))$p.value,
           n_cmt=length(a), n_re=length(b))
  }) %>% group_by(window) %>% mutate(p_fdr=p.adjust(p,"BH")) %>% ungroup() %>%
  mutate(sig = !is.na(p_fdr) & p_fdr<0.05, win = adv>0)
saveRDS(res, "MORT_WINDOW_RES.rds")

print(res %>% select(Region,amb,window,adv,pct,sig) %>%
      mutate(across(where(is.numeric),~round(.,2))) %>%
      pivot_wider(names_from=window, values_from=c(adv,pct,sig)) %>% as.data.frame())

cat("\ncells won by High-RE:\n")
print(res %>% filter(!is.na(win)) %>% group_by(window) %>%
      summarise(cells=n(), RE=sum(win), sig_RE=sum(win&sig), sig_CMT=sum(!win&sig),
                .groups="drop") %>% as.data.frame())
cat("\nsign changes between windows:\n")
fl <- res %>% select(Region,amb,window,adv) %>% pivot_wider(names_from=window, values_from=adv) %>%
  filter(!is.na(`2020-2050`), !is.na(`2020-2100`)) %>%
  mutate(flip = sign(`2020-2050`) != sign(`2020-2100`))
cat(sum(fl$flip), "of", nrow(fl), "\n")
print(fl %>% filter(flip) %>% mutate(across(where(is.numeric),~round(.,2))) %>% as.data.frame())

line("5. WORLD, BOTH WINDOWS")
print(res %>% filter(Region=="Aggregated R10 regions") %>%
      select(amb,window,n_cmt,n_re,adv,pct,p_fdr,sig) %>%
      mutate(across(where(is.numeric),~round(.,3))) %>% as.data.frame())
