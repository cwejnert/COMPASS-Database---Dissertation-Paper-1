# =============================================================================
# Z3 — DOES THE GLOBAL LABEL DESCRIBE WHAT HAPPENS IN EACH REGION?
#
# The classification axes are GLOBAL: total_cdr and total_re are sums over the
# ten R10 regions, cut at the top tercile within an ambition class. One fixed
# set of labels is then applied unchanged in every region.
#
# That is a deliberate choice -- it answers "do scenarios that globally lean
# renewable do better everywhere" -- but it carries an assumption: that a
# globally High-RE scenario is also renewables-leaning IN THE REGION whose
# outcome we are scoring. If a scenario builds its renewables in Asia and its
# CDR in Africa, then in Africa the High-RE label describes nothing.
#
# Three tests:
#   1. Within each region, do High-RE scenarios actually deploy more renewables
#      than High-CMT ones? And less carbon management?
#   2. What fraction of scenarios would carry a DIFFERENT label if the tercile
#      were computed on that region's own deployment?
#   3. Does the deprivation result change under per-region labels?
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

ds <- readRDS("ds_A.rds")
F  <- load_frame("A")
cliff <- function(a, b) { a<-a[!is.na(a)]; b<-b[!is.na(b)]
  if (length(a)<3 || length(b)<3) return(NA_real_)
  r <- rank(c(a,b)); n1<-length(a); n2<-length(b)
  2*((sum(r[(n1+1):(n1+n2)]) - n2*(n2+1)/2)/(n1*n2)) - 1 }

# regional deployment on each axis
reg <- ds %>%
  filter(Variable %in% c("Total CDR","Renewable Capacity"), Region %in% R10_TEN) %>%
  select(Model, Scenario, Region, Variable, Total_Value) %>%
  pivot_wider(names_from = Variable, values_from = Total_Value) %>%
  rename(cmt = `Total CDR`, re = `Renewable Capacity`)

lab <- readRDS("pw_A.rds") %>% filter(!is.na(Pathway_excl)) %>%
  distinct(Model, Scenario, Pathway = Pathway_excl, Ambition) %>%
  mutate(amb = ifelse(grepl("^1\\.5", Ambition), "1.5C", "2C"))
R <- reg %>% inner_join(lab, by = c("Model","Scenario"))

line("1. DOES THE GLOBAL LABEL HOLD INSIDE EACH REGION?")
cat("Cliff's delta on REGIONAL deployment, High-RE vs High-CMT.\n")
cat("re_delta should be strongly POSITIVE (High-RE builds more renewables here)\n")
cat("cmt_delta should be strongly NEGATIVE (High-RE does less carbon mgmt here)\n\n")
coh <- R %>% group_by(Region, amb) %>%
  summarise(re_delta  = cliff(re[Pathway=="High-CMT"],  re[Pathway=="High-RE"]),
            cmt_delta = cliff(cmt[Pathway=="High-CMT"], cmt[Pathway=="High-RE"]),
            n_cmt = sum(Pathway=="High-CMT"), n_re = sum(Pathway=="High-RE"),
            .groups="drop")
print(coh %>% mutate(across(where(is.numeric), ~round(.,2))) %>%
      select(Region, amb, re_delta, cmt_delta, n_cmt, n_re) %>%
      pivot_wider(names_from = amb, values_from = c(re_delta, cmt_delta, n_cmt, n_re)) %>%
      as.data.frame())
cat("\nweakest regions on re_delta:\n")
print(coh %>% arrange(re_delta) %>% head(4) %>%
      mutate(across(where(is.numeric), ~round(.,2))) %>% as.data.frame())

line("2. HOW MANY SCENARIOS WOULD BE LABELLED DIFFERENTLY PER REGION?")
tc <- function(x) x >= quantile(x, 2/3, na.rm = TRUE)
per_reg <- R %>% group_by(Region, amb) %>%
  mutate(hr = tc(re), hc = tc(cmt),
         lab_reg = ifelse(hc & !hr, "High-CMT", ifelse(hr & !hc, "High-RE", NA))) %>%
  ungroup()
cat("Comparing the GLOBAL label with a label computed on that region alone:\n\n")
print(per_reg %>% group_by(Region) %>%
      summarise(n = n(),
                same      = sum(!is.na(lab_reg) & lab_reg == Pathway),
                flipped   = sum(!is.na(lab_reg) & lab_reg != Pathway),
                unlabelled= sum(is.na(lab_reg)),
                pct_same  = round(100*same/n), .groups="drop") %>%
      as.data.frame())

line("3. DOES DEPRIVATION CHANGE UNDER PER-REGION LABELS?")
DD <- F %>% filter(!is.na(gap_GJ_pc), Region %in% R10_TEN) %>%
  inner_join(per_reg %>% select(Model, Scenario, Region, lab_reg),
             by = c("Model","Scenario","Region"))
cmp <- expand_grid(Region = R10_TEN, amb = c("1.5C","2C")) %>%
  pmap_dfr(function(Region, amb) {
    d <- DD %>% filter(Region == !!Region, amb == !!amb)
    g <- function(x, col) {
      a <- x$gap_GJ_pc[x[[col]] == "High-CMT"]; b <- x$gap_GJ_pc[x[[col]] == "High-RE"]
      a<-a[!is.na(a)]; b<-b[!is.na(b)]
      if (length(a)<5 || length(b)<5) NA_real_ else -cliff(a, b)
    }
    tibble(Region, amb,
           global_label = g(d, "Pathway"),
           region_label = g(d %>% filter(!is.na(lab_reg)), "lab_reg"))
  }) %>% mutate(flip = !is.na(global_label) & !is.na(region_label) &
                       sign(global_label) != sign(region_label))
print(cmp %>% mutate(across(where(is.numeric), ~round(.,3))) %>% as.data.frame())
cat("\nHigh-RE wins -- global labels:", sum(cmp$global_label > 0, na.rm=TRUE),
    "| per-region labels:", sum(cmp$region_label > 0, na.rm=TRUE),
    "of", sum(!is.na(cmp$region_label)), "\n")
cat("sign changes:", sum(cmp$flip, na.rm=TRUE), "\n")
saveRDS(cmp, "Z3_LABELS.rds")
