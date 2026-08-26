# =============================================================================
# W13 — (A) ARE THE ZERO-RENEWABLE SCENARIOS TRUE ZEROS OR MISSING DATA?
#       (B) DOES MORTALITY MOVE WHEN LAND-BASED CDR IS PUT BACK IN THE CMT AXIS?
#
# (A) WHY IT MATTERS. The classification pivots renewable capacity with
# values_fill = 0, so a scenario that never reports Renewable Capacity is
# treated as deploying none. That puts it at the bottom of the RE distribution
# and makes it eligible for the High-CMT arm. At 1.5C, 16 of the 64 High-CMT
# scenarios (a quarter of the arm) sit at exactly zero renewables. If those are
# genuinely zero the treatment is right; if they are simply unreported, the arm
# is partly populated by scenarios about which we know nothing on that axis,
# and "High-CMT" is doing work that "did not report renewables" should be doing.
#
# The test is simple and decisive: does the scenario have a Renewable Capacity
# ROW in the cumulative CDR file at all? A row carrying 0 is a reported zero. No
# row is missing data that values_fill invented a zero for.
#
# (B) The land sensitivity (W12) covered jobs and deprivation but not mortality,
# because mortality comes from a separate reporting-complete run whose scenario
# list was drawn against the ENGINEERED labels. The mortality values themselves
# are label-independent, so the land question can be answered by re-joining the
# same deaths to the land-inclusive labels -- with the caveat that the
# reporting-complete target list was selected under the engineered axis, so
# scenarios the land axis newly classifies may have no mortality run.
#
# USAGE: Rscript W13_zeros_and_land_mortality.R      (run from the repo root)
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(purrr)})
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")
set.seed(20260825)

B     <- 2000
R10   <- c("R10AFRICA","R10CHINA+","R10EUROPE","R10INDIA+","R10LATIN_AM",
           "R10MIDDLE_EAST","R10NORTH_AM","R10PAC_OECD","R10REF_ECON","R10REST_ASIA")
WORLD <- "Aggregated R10 regions"
SH    <- c(`Aggregated R10 regions`="WORLD", R10AFRICA="Africa", `R10CHINA+`="China+",
           R10EUROPE="Europe", `R10INDIA+`="India+", R10LATIN_AM="Latin America",
           R10MIDDLE_EAST="Middle East", R10NORTH_AM="North America",
           R10PAC_OECD="Pacific OECD", R10REF_ECON="Reforming econ.",
           R10REST_ASIA="Rest of Asia")
norm <- function(x) x %>% enc2utf8() %>% gsub("\\u00b0", "°", .) %>%
  gsub("�", "°", ., fixed = TRUE)

# =============================================================================
line("A. ZERO RENEWABLES — REPORTED ZERO, OR NO ROW AT ALL?")
# =============================================================================
LAND_OBJ <- readRDS("W12_LAND.rds")

check_zeros <- function(id) {
  cdr <- read.csv(sprintf("master_outputs/approach_%s/compass_cdr_cumulative_%s.csv", id, id),
                  stringsAsFactors = FALSE) %>%
    mutate(Model = norm(Model), Scenario = norm(Scenario))
  # Does the scenario have ANY Renewable Capacity row, and is it non-zero?
  re_rows <- cdr %>% filter(Variable == "Renewable Capacity") %>%
    group_by(Model, Scenario) %>%
    summarise(n_re_rows = n(),
              n_regions = n_distinct(Region),
              sum_re = sum(Total_Value, na.rm = TRUE),
              all_na = all(is.na(Total_Value)), .groups = "drop")
  L <- LAND_OBJ$labels_eng %>% filter(approach == id) %>%
    select(Model, Scenario, amb, Pathway, renewables)
  L %>% left_join(re_rows, by = c("Model","Scenario")) %>%
    mutate(approach = id,
           status = case_when(
             is.na(n_re_rows)              ~ "NO Renewable Capacity row at all",
             all_na                        ~ "row present, all values NA",
             sum_re > 0                    ~ "reported, non-zero",
             TRUE                          ~ "reported, genuinely zero"))
}
Z <- bind_rows(check_zeros("A"), check_zeros("C"))

cat("Every scenario in the classification frame, by renewables status:\n\n")
print(as.data.frame(Z %>% count(approach, status) %>%
      pivot_wider(names_from = approach, values_from = n, values_fill = 0)))

cat("\nOf the scenarios with renewables == 0 on the classification axis:\n\n")
ZERO <- Z %>% filter(renewables == 0)
print(as.data.frame(ZERO %>% count(approach, amb, status, Pathway) %>%
      arrange(approach, amb, status)))

cat("\nHOW MANY OF EACH ARM ARE ZERO-RENEWABLE, AND WHY:\n\n")
print(as.data.frame(Z %>% filter(!is.na(Pathway)) %>%
      group_by(approach, amb, Pathway) %>%
      summarise(arm_n = n(),
                zero_re = sum(renewables == 0),
                of_which_no_row = sum(renewables == 0 &
                                      status == "NO Renewable Capacity row at all"),
                of_which_true_zero = sum(renewables == 0 &
                                         status == "reported, genuinely zero"),
                .groups = "drop")))

cat("\nWhich model families are the zero-renewable scenarios?\n")
print(as.data.frame(ZERO %>% filter(!is.na(Pathway)) %>%
      mutate(fam = sub("[ /-].*$","",Model)) %>%
      count(approach, fam, status) %>% arrange(desc(n))))

# What would happen if they were dropped rather than treated as zero?
cat("\n--- IF ZERO-RENEWABLE SCENARIOS WERE EXCLUDED FROM THE CLASSIFICATION ---\n")
for (id in c("A")) for (a in c("1.5C","2C")) {
  d <- Z %>% filter(approach == id, amb == a)
  keep <- d %>% filter(renewables > 0)
  cat(sprintf("%s %s: %d scenarios -> %d after dropping %d zero-renewable\n",
              id, a, nrow(d), nrow(keep), sum(d$renewables == 0)))
}

# =============================================================================
line("B. DOES MORTALITY CHANGE WHEN LAND IS PUT BACK IN THE CMT AXIS?")
# =============================================================================
MS <- read.csv("final_outcomes/mortality_reporting_complete_scenario_values_2020_2100.csv",
               stringsAsFactors = FALSE) %>%
  mutate(Model = norm(Model), Scenario = norm(Scenario),
         mort = cumulative_pm25_deaths_mln,
         stem = gsub("[-_ ]?[0-9]+(\\.[0-9]+)?[a-z]?$", "", Scenario),
         stem = sub("/.*$", "", stem), clus = paste(Model, stem)) %>%
  select(approach, Model, Scenario, Region, mort, clus)
cat("mortality scenario-region rows available:", nrow(MS), "\n")

cellm <- function(d) {
  a <- d$mort[d$Pathway=="High-CMT"]; ca <- d$clus[d$Pathway=="High-CMT"]
  b <- d$mort[d$Pathway=="High-RE"];  cb <- d$clus[d$Pathway=="High-RE"]
  ka <- !is.na(a); kb <- !is.na(b); a<-a[ka]; ca<-ca[ka]; b<-b[kb]; cb<-cb[kb]
  if (length(a) < 5 || length(b) < 5)
    return(tibble(n_cmt=length(a), n_re=length(b), raw_cmt=NA_real_, raw_re=NA_real_,
                  gap=NA_real_, lo=NA_real_, hi=NA_real_))
  ua <- unique(ca); ub <- unique(cb)
  ia <- split(seq_along(a), ca); ib <- split(seq_along(b), cb)
  reps <- vapply(seq_len(B), function(i) {
    sa <- unlist(ia[sample(ua, length(ua), TRUE)], use.names=FALSE)
    sb <- unlist(ib[sample(ub, length(ub), TRUE)], use.names=FALSE)
    -(median(b[sb]) - median(a[sa]))          # positive = High-RE avoids deaths
  }, numeric(1))
  q <- quantile(reps, c(.025,.975), na.rm=TRUE)
  tibble(n_cmt=length(a), n_re=length(b), raw_cmt=median(a), raw_re=median(b),
         gap = -(median(b)-median(a)), lo=q[[1]], hi=q[[2]])
}
score_m <- function(L, nm) {
  d <- MS %>% inner_join(L %>% filter(approach=="A", !is.na(Pathway)) %>%
                           select(Model, Scenario, Pathway, amb),
                         by = c("Model","Scenario")) %>% filter(approach == "A")
  expand_grid(Region = c(WORLD, R10), amb = c("1.5C","2C")) %>%
    pmap_dfr(function(Region, amb)
      bind_cols(tibble(axis=nm, Region, amb), cellm(d[d$Region==Region & d$amb==amb, ]))) %>%
    mutate(sig = !is.na(lo) & (lo>0 | hi<0), reg = SH[Region])
}
SM <- bind_rows(score_m(LAND_OBJ$labels_eng, "engineered"),
                score_m(LAND_OBJ$labels_land, "with land"))
saveRDS(list(zeros = Z, mort = SM), "W13_ZEROS_LANDMORT.rds")

cat("\nCOVERAGE. The reporting-complete mortality target list was drawn against\n")
cat("the ENGINEERED labels, so the land axis can only be scored on scenarios\n")
cat("that already have a mortality run:\n\n")
print(as.data.frame(SM %>% filter(reg=="WORLD") %>%
      transmute(axis, amb, n_cmt, n_re)))

cat("\nWORLD mortality, both axes (million cumulative deaths, positive = High-RE avoids):\n\n")
print(as.data.frame(SM %>% filter(reg=="WORLD") %>%
      transmute(amb, axis, cmt=round(raw_cmt,2), re=round(raw_re,2),
                gap=round(gap,2), CI=sprintf("[%+.2f,%+.2f]",lo,hi),
                sig=ifelse(sig,"YES","no")) %>% arrange(amb, axis)))

cat("\nALL REGIONS, both axes:\n\n")
print(as.data.frame(SM %>% filter(Region != "R10PAC_OECD") %>%
      select(reg, amb, axis, gap, sig) %>%
      mutate(gap = round(gap,2)) %>%
      pivot_wider(names_from=axis, values_from=c(gap,sig))))

cat("\nscorecard (nine regions + World):\n")
print(as.data.frame(SM %>% filter(Region != "R10PAC_OECD", !is.na(gap)) %>%
      group_by(axis) %>%
      summarise(cells=n(), favour_RE=sum(gap>0), sig_for=sum(gap>0 & sig),
                sig_against=sum(gap<0 & sig), .groups="drop")))

fl <- SM %>% filter(Region != "R10PAC_OECD", !is.na(gap)) %>%
  select(reg, amb, axis, gap) %>% pivot_wider(names_from=axis, values_from=gap) %>%
  filter(!is.na(engineered), !is.na(`with land`),
         sign(engineered) != sign(`with land`))
cat("\ncells changing sign between the two axes:", nrow(fl), "\n")
if (nrow(fl)) print(as.data.frame(fl %>% mutate(across(where(is.numeric), ~round(.,2)))))
