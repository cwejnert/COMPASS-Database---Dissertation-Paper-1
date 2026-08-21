# =============================================================================
# S4 — the 2010 base year as an independent quality check.
#
# 2010 is HISTORY. Every scenario should reproduce roughly the same global
# PM2.5 mortality in 2010, because they all start from the same world. The
# observed spread is 82.7% (2.0 to 6.7 mln), which means some scenarios are
# reporting far too little precursor emission in the base year.
#
# This is a scenario-level quality signal INDEPENDENT of the precursor-count
# gate, and possibly a better one: it tests the output, not the input coverage.
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

MA <- read.csv("mort_annual.csv", stringsAsFactors = FALSE)
mc <- readRDS("mort_coverage.rds")

base <- MA %>% filter(year == 2010, !is.na(deaths_pm25)) %>%
  group_by(Model = model, Scenario = scenario) %>%
  summarise(n_reg = n(), base2010_mln = sum(deaths_pm25)/1e6, .groups="drop") %>%
  filter(n_reg == 10) %>%
  mutate(fam = sub("[ /].*$","",Model))
ref <- median(base$base2010_mln)

line("1. THE 2010 BASE YEAR SHOULD BE THE SAME FOR EVERYONE")
cat("scenarios with all ten regions in 2010:", nrow(base), "\n")
cat("median", round(ref,2), "mln | IQR", round(quantile(base$base2010_mln,.25),2),
    "-", round(quantile(base$base2010_mln,.75),2),
    "| range", round(min(base$base2010_mln),2), "-", round(max(base$base2010_mln),2), "\n")
cat("\nHow far below the median does each family sit?\n")
print(base %>% group_by(fam) %>%
      summarise(n = n(), median_2010 = round(median(base2010_mln),2),
                pct_of_ref = round(100*median(base2010_mln)/ref),
                min = round(min(base2010_mln),2), .groups="drop") %>%
      arrange(pct_of_ref) %>% as.data.frame())

line("2. DOES THE PRECURSOR-COUNT GATE ALREADY CATCH THESE?")
b <- base %>% left_join(mc %>% transmute(Model=model, Scenario=scenario,
                                         n_pm_nonzero), by=c("Model","Scenario")) %>%
  mutate(pct = 100*base2010_mln/ref,
         base_ok = pct >= 70,
         gate_ok = !is.na(n_pm_nonzero) & n_pm_nonzero >= 6)
print(b %>% count(gate_ok, base_ok) %>%
      mutate(meaning = c("both fail","precursor gate fails only",
                         "BASE YEAR fails only","both pass")[1+2*gate_ok+base_ok]) %>%
      as.data.frame())
cat("\nScenarios the precursor gate PASSES but the base year rejects:\n")
miss <- b %>% filter(gate_ok, !base_ok)
print(miss %>% count(fam, n_pm_nonzero) %>% as.data.frame())
cat("\nTheir base-year levels:\n")
print(miss %>% group_by(fam) %>%
      summarise(n=n(), median_pct = round(median(pct)), min_pct = round(min(pct)),
                .groups="drop") %>% as.data.frame())

line("3. DOES ADDING THE BASE-YEAR CHECK CHANGE THE MORTALITY RESULT?")
LAB <- readRDS("pw_A.rds") %>% filter(!is.na(Pathway_excl)) %>%
  distinct(Model, Scenario, Pathway = Pathway_excl)
F <- load_frame("A")
Fb <- F %>% left_join(b %>% select(Model,Scenario,base_ok), by=c("Model","Scenario")) %>%
  mutate(mort_strict = ifelse(!is.na(base_ok) & base_ok, mort_per_1k, NA_real_))

cat("scenario-region cells with mortality: current gate",
    sum(!is.na(F$mort_per_1k)), "| plus base-year check", sum(!is.na(Fb$mort_strict)), "\n")

cmp <- expand_grid(Region=c("Aggregated R10 regions",R10_TEN), amb=c("1.5C","2C")) %>%
  pmap_dfr(function(Region,amb){
    s <- Fb[Fb$Region==Region & Fb$amb==amb, ]
    g <- function(v){ a<-s[[v]][s$Pathway=="High-CMT"]; bb<-s[[v]][s$Pathway=="High-RE"]
      a<-a[!is.na(a)];bb<-bb[!is.na(bb)]
      if(length(a)<5||length(bb)<5) return(c(NA,NA,NA))
      c(-cliff_d(a,bb), length(a), length(bb)) }
    c1 <- g("mort_per_1k"); c2 <- g("mort_strict")
    tibble(Region, amb, adv_now=c1[1], n_now=c1[2]+c1[3],
                        adv_strict=c2[1], n_strict=c2[2]+c2[3])
  })
print(cmp %>% mutate(across(where(is.numeric),~round(.,2)),
                     flip = !is.na(adv_now)&!is.na(adv_strict)&sign(adv_now)!=sign(adv_strict)) %>%
      as.data.frame())
cat("\nsign changes:", sum(cmp$flip, na.rm=TRUE), "of",
    sum(!is.na(cmp$adv_now) & !is.na(cmp$adv_strict)), "\n")
cat("cells won by High-RE -- current:", sum(cmp$adv_now>0, na.rm=TRUE),
    "| with base-year check:", sum(cmp$adv_strict>0, na.rm=TRUE), "\n")
saveRDS(b, "BASEYEAR.rds")
