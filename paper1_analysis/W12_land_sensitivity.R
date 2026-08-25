# =============================================================================
# W12 — LAND-BASED CDR IN OR OUT: THE SENSITIVITY THAT IS NOT YET IN THE PAPER
#
# The revamp moved the CMT axis from Total CDR to ENGINEERED CDR (Novel CDR +
# fossil/industrial CCS, land-based removal excluded). That is a defensible
# choice -- land-based removal is a different intervention whose main welfare
# channel is land competition, which this outcome set cannot see -- but it was
# made without reporting what it does. The published run exists only in the
# land-excluded form, so a reader cannot tell whether the results are a property
# of the pathways or of where the CMT boundary was drawn.
#
# Land is not a rounding error. Across 1,117 scenarios the median cumulative
# land-based CDR is 123,900 against 182,102 for fossil CCS and 0 for novel CDR
# (only 28% of scenarios report novel CDR at all). Adding land back roughly
# DOUBLES the CMT axis for a typical scenario, so it should move the tercile
# cut and reshuffle membership substantially.
#
# THIS SCRIPT builds both classifications with the published rule, verifies the
# engineered one reproduces the published labels exactly, and then asks four
# questions:
#
#   1. How much does membership actually change?
#   2. Does the model composition of the arms change -- i.e. does including
#      land make the confound better or worse?
#   3. Do the wellbeing results change, and in which direction?
#   4. What kind of scenario switches sides, and does that tell a story?
#
# The classification rule is taken verbatim from
# COMPASS_engineered_cmt_century_broad_targets.R: within each ambition band,
# top tercile on the focal axis and NOT top tercile on the opposing axis,
# quantile type 7, cumulative 2020-2100 deployment summed over all regions.
#
# USAGE: Rscript W12_land_sensitivity.R      (run from the repo root)
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
OUTS  <- c(net_re_jobs_per_1k="Jobs", gap_GJ_pc="Deprivation")
LOWER <- c("gap_GJ_pc","headcount_pct","mort_per_1k")
# normalise_id() verbatim from COMPASS_engineered_cmt_century_broad_targets.R.
# Using iconv(sub="") instead STRIPS the degree symbol where this CONVERTS it,
# which silently mismatched 71 scenario keys on the first run.
norm <- function(x) x %>% enc2utf8() %>% gsub("\\u00b0", "\u00b0", .) %>%
  gsub("\ufffd", "\u00b0", ., fixed = TRUE)

# =============================================================================
line("1. BUILDING BOTH CLASSIFICATIONS")
# =============================================================================
# Verbatim replication of build_labels() in the published targets script, with
# the CMT axis switchable.
build_labels <- function(approach, with_land) {
  cdr <- read.csv(sprintf("master_outputs/approach_%s/compass_cdr_cumulative_%s.csv",
                          approach, approach), stringsAsFactors = FALSE)
  sset <- read.csv(sprintf("master_outputs/approach_%s/compass_scenario_set_%s.csv",
                           approach, approach), stringsAsFactors = FALSE)
  # THE SAMPLE MUST BE HELD FIXED so that only the AXIS differs between the two
  # runs. The published script filters exactly three variables, and that filter
  # defines which scenarios enter the quantile. Admitting a fourth (land) would
  # add scenarios that report land but neither novel CDR nor fossil CCS, which
  # shifts the tercile cut and makes the comparison partly a sample change.
  # So: build the frame on their three variables, then JOIN land on separately.
  agg <- function(vars) cdr %>% filter(Variable %in% vars) %>%
    group_by(Model, Scenario, Category, Variable) %>%
    summarise(value = sum(Total_Value, na.rm = TRUE), .groups = "drop")
  metrics <- agg(c("Novel CDR","Fossil CCS","Renewable Capacity")) %>%
    pivot_wider(names_from = Variable, values_from = value, values_fill = 0) %>%
    rename(novel_cdr = `Novel CDR`, fossil_ccs = `Fossil CCS`,
           renewables = `Renewable Capacity`) %>%
    left_join(agg("Land-based CDR") %>%
                select(Model, Scenario, Category, land_cdr = value),
              by = c("Model","Scenario","Category")) %>%
    mutate(land_cdr = ifelse(is.na(land_cdr), 0, land_cdr),
           cmt_axis_value = novel_cdr + fossil_ccs + if (with_land) land_cdr else 0)

  sset %>% select(Model, Scenario, Category, Ambition) %>% distinct() %>%
    inner_join(metrics, by = c("Model","Scenario","Category")) %>%
    group_by(Ambition) %>%
    mutate(cmt_high = quantile(cmt_axis_value, 2/3, na.rm = TRUE, type = 7),
           re_high  = quantile(renewables,     2/3, na.rm = TRUE, type = 7),
           high_cmt = cmt_axis_value >= cmt_high,
           high_re  = renewables     >= re_high,
           Pathway  = case_when(high_cmt & !high_re ~ "High-CMT",
                                high_re & !high_cmt ~ "High-RE",
                                TRUE ~ NA_character_)) %>%
    ungroup() %>%
    mutate(approach = approach, Model = norm(Model), Scenario = norm(Scenario),
           amb = ifelse(grepl("^1\\.5", Ambition), "1.5C", "2C"))
}
ENG  <- bind_rows(build_labels("A", FALSE), build_labels("C", FALSE))
LAND <- bind_rows(build_labels("A", TRUE),  build_labels("C", TRUE))

# ---- self-check against the published labels -------------------------------
PUB <- read.csv("final_outcomes/engineered_cmt_century_broad_labels.csv",
                stringsAsFactors = FALSE) %>%
  mutate(Model = norm(Model), Scenario = norm(Scenario)) %>%
  transmute(approach, Model, Scenario,
            pub = ifelse(is.na(Pathway), NA_character_,
                         ifelse(Pathway == "High-engineered-CMT", "High-CMT", "High-RE")))
CHK <- ENG %>% select(approach, Model, Scenario, ours = Pathway) %>%
  inner_join(PUB, by = c("approach","Model","Scenario"))
same  <- (!is.na(CHK$ours) & !is.na(CHK$pub) & CHK$ours == CHK$pub) |
          (is.na(CHK$ours) & is.na(CHK$pub))
agree <- sum(same)
cat("scenarios compared against the published labels:", nrow(CHK), "\n")
cat("identical labels:", agree, sprintf("(%.1f%%)", 100*agree/nrow(CHK)),
    ifelse(agree == nrow(CHK), " [ok]", " [FAIL]"), "\n")
if (agree < nrow(CHK)) {
  cat("\nmismatches:\n")
  print(head(as.data.frame(CHK[!same, ]), 8))
}

cat("\narm counts under each axis (approach A):\n")
print(as.data.frame(bind_rows(
  ENG  %>% filter(approach=="A", !is.na(Pathway)) %>% count(amb, Pathway) %>% mutate(axis="engineered"),
  LAND %>% filter(approach=="A", !is.na(Pathway)) %>% count(amb, Pathway) %>% mutate(axis="with land")) %>%
  pivot_wider(names_from=c(axis,Pathway), values_from=n)))

# =============================================================================
line("2. HOW MUCH DOES LAND CHANGE THE AXIS AND THE CUT?")
# =============================================================================
AX <- ENG %>% select(approach, Model, Scenario, amb, novel_cdr, fossil_ccs, land_cdr,
                     eng = cmt_axis_value, eng_cut = cmt_high) %>%
  inner_join(LAND %>% select(approach, Model, Scenario,
                             tot = cmt_axis_value, tot_cut = cmt_high),
             by = c("approach","Model","Scenario"))
cat("median cumulative deployment, approach A (arbitrary deployment units):\n")
print(as.data.frame(AX %>% filter(approach=="A") %>% group_by(amb) %>%
      summarise(novel_cdr = round(median(novel_cdr)),
                fossil_ccs = round(median(fossil_ccs)),
                land_cdr = round(median(land_cdr)),
                engineered_axis = round(median(eng)),
                with_land_axis = round(median(tot)),
                land_share_of_axis = sprintf("%.0f%%", 100*median(land_cdr/pmax(tot,1))),
                .groups="drop")))
cat("\ntercile cut on each axis:\n")
print(as.data.frame(AX %>% filter(approach=="A") %>% group_by(amb) %>%
      summarise(engineered_cut = round(median(eng_cut)),
                with_land_cut = round(median(tot_cut)), .groups="drop")))
cat("\ncorrelation between the two axes (Spearman):",
    round(cor(AX$eng, AX$tot, method="spearman", use="complete.obs"), 3), "\n")

# =============================================================================
line("3. MEMBERSHIP CHURN — who changes side?")
# =============================================================================
M <- ENG %>% select(approach, Model, Scenario, amb, eng = Pathway) %>%
  inner_join(LAND %>% select(approach, Model, Scenario, land = Pathway),
             by = c("approach","Model","Scenario")) %>%
  mutate(eng2  = ifelse(is.na(eng),  "unclassified", eng),
         land2 = ifelse(is.na(land), "unclassified", land))
cat("approach A, all scenarios in the classification frame:\n\n")
print(as.data.frame(M %>% filter(approach=="A") %>% count(eng2, land2) %>%
      pivot_wider(names_from = land2, values_from = n, values_fill = 0)))
cat("\nrows = label with land EXCLUDED (published); columns = with land INCLUDED\n")

both <- M %>% filter(approach=="A", !is.na(eng), !is.na(land))
cat("\nscenarios classified under BOTH axes:", nrow(both), "\n")
cat("keeping the same label:", sum(both$eng == both$land),
    sprintf("(%.0f%%)", 100*mean(both$eng == both$land)), "\n")
cat("SWITCHING SIDES:", sum(both$eng != both$land), "\n")
if (sum(both$eng != both$land) > 0) {
  cat("\nwho switches:\n")
  print(as.data.frame(both %>% filter(eng != land) %>%
        mutate(fam = sub("[ /-].*$","",Model)) %>%
        count(amb, eng, land, fam) %>% arrange(desc(n))))
}

# =============================================================================
line("4. DOES INCLUDING LAND HELP OR HURT THE MODEL-COMPOSITION PROBLEM?")
# =============================================================================
compo <- function(L, nm) L %>% filter(approach=="A", !is.na(Pathway)) %>%
  mutate(fam = sub("[ /-].*$","",Model)) %>% count(fam, Pathway) %>%
  pivot_wider(names_from=Pathway, values_from=n, values_fill=0) %>%
  mutate(axis = nm, both = `High-CMT` > 0 & `High-RE` > 0)
CO <- bind_rows(compo(ENG,"engineered"), compo(LAND,"with land"))
print(as.data.frame(CO %>% select(axis, fam, `High-CMT`, `High-RE`) %>%
      pivot_wider(names_from=axis, values_from=c(`High-CMT`,`High-RE`), values_fill=0)))
cat("\nsummary:\n")
print(as.data.frame(CO %>% group_by(axis) %>%
      summarise(families = n(), holding_both = sum(both),
                remind_share_of_RE = sprintf("%.0f%%",
                  100*sum(`High-RE`[grepl("REMIND",fam)])/sum(`High-RE`)),
                remind_share_of_CMT = sprintf("%.0f%%",
                  100*sum(`High-CMT`[grepl("REMIND",fam)])/pmax(sum(`High-CMT`),1)),
                .groups="drop")))

# =============================================================================
line("5. DO THE WELLBEING RESULTS CHANGE?")
# =============================================================================
# Outcomes rebuilt on the strict-World basis from V3, so both axes are scored
# identically and only the labels differ.
SW <- readRDS("STRICT_WORLD.rds")
RO <- read.csv("master_outputs/approach_A/compass_master_dataset_A.csv", stringsAsFactors=FALSE) %>%
  mutate(Model = norm(Model), Scenario = norm(Scenario), approach = "A") %>%
  filter(Region %in% R10) %>%
  distinct(approach, Model, Scenario, Region, .keep_all = TRUE) %>%
  select(approach, Model, Scenario, Region, all_of(names(OUTS)))
WW <- SW$world %>% filter(approach=="A") %>%
  select(approach, Model, Scenario, Region, all_of(names(OUTS)))
OUTC <- bind_rows(RO, WW)

cliff_cell <- function(d, out) {
  sgn <- ifelse(out %in% LOWER, -1, 1)
  a <- d[[out]][d$Pathway=="High-CMT"]; ca <- d$clus[d$Pathway=="High-CMT"]
  b <- d[[out]][d$Pathway=="High-RE"];  cb <- d$clus[d$Pathway=="High-RE"]
  ka <- !is.na(a); kb <- !is.na(b); a<-a[ka]; ca<-ca[ka]; b<-b[kb]; cb<-cb[kb]
  if (length(a) < 5 || length(b) < 5)
    return(tibble(n_cmt=length(a), n_re=length(b), raw_cmt=NA_real_, raw_re=NA_real_,
                  gap=NA_real_, lo=NA_real_, hi=NA_real_))
  ua <- unique(ca); ub <- unique(cb)
  ia <- split(seq_along(a), ca); ib <- split(seq_along(b), cb)
  reps <- vapply(seq_len(B), function(i) {
    sa <- unlist(ia[sample(ua, length(ua), TRUE)], use.names=FALSE)
    sb <- unlist(ib[sample(ub, length(ub), TRUE)], use.names=FALSE)
    sgn*(median(b[sb]) - median(a[sa]))
  }, numeric(1))
  q <- quantile(reps, c(.025,.975), na.rm=TRUE)
  tibble(n_cmt=length(a), n_re=length(b), raw_cmt=median(a), raw_re=median(b),
         gap = sgn*(median(b)-median(a)), lo=q[[1]], hi=q[[2]])
}
score <- function(L, nm) {
  d <- OUTC %>% inner_join(L %>% filter(approach=="A", !is.na(Pathway)) %>%
                             select(approach, Model, Scenario, Pathway, amb),
                           by = c("approach","Model","Scenario")) %>%
    mutate(stem = gsub("[-_ ]?[0-9]+(\\.[0-9]+)?[a-z]?$","",Scenario),
           stem = sub("/.*$","",stem), clus = paste(Model, stem))
  expand_grid(Region = c(WORLD, R10), amb = c("1.5C","2C"), outcome = names(OUTS)) %>%
    pmap_dfr(function(Region, amb, outcome)
      bind_cols(tibble(axis=nm, Region, amb, outcome),
                cliff_cell(d[d$Region==Region & d$amb==amb, ], outcome))) %>%
    mutate(sig = !is.na(lo) & (lo>0 | hi<0), reg = SH[Region],
           family = OUTS[outcome])
}
S <- bind_rows(score(ENG,"engineered"), score(LAND,"with land"))
saveRDS(list(labels_eng=ENG, labels_land=LAND, churn=M, scores=S), "W12_LAND.rds")

cat("WORLD, both axes:\n\n")
print(as.data.frame(S %>% filter(reg=="WORLD") %>%
      transmute(family, amb, axis, n=paste0(n_cmt,"v",n_re),
                cmt=round(raw_cmt,2), re=round(raw_re,2), gap=round(gap,2),
                CI=sprintf("[%+.2f,%+.2f]",lo,hi), sig=ifelse(sig,"YES","no")) %>%
      arrange(family, amb, axis)))

cat("\nscorecard, nine regions + World (Pacific OECD dropped):\n")
print(as.data.frame(S %>% filter(Region!="R10PAC_OECD", !is.na(gap)) %>%
      group_by(axis, family) %>%
      summarise(cells=n(), favour_RE=sum(gap>0), sig_for=sum(gap>0 & sig),
                sig_against=sum(gap<0 & sig), .groups="drop")))

cat("\ncells that change sign between the two axes:\n")
FL <- S %>% filter(Region!="R10PAC_OECD", !is.na(gap)) %>%
  select(reg, amb, family, axis, gap, sig) %>%
  pivot_wider(names_from=axis, values_from=c(gap,sig))
names(FL) <- gsub("gap_|sig_","",names(FL)) %>% make.unique()
fl <- S %>% filter(Region!="R10PAC_OECD", !is.na(gap)) %>%
  select(reg, amb, family, axis, gap) %>%
  pivot_wider(names_from=axis, values_from=gap) %>%
  filter(!is.na(engineered), !is.na(`with land`),
         sign(engineered) != sign(`with land`))
print(if (nrow(fl)) as.data.frame(fl %>% mutate(across(where(is.numeric), ~round(.,2)))) else "  none")

line("THE READING")
cat("If membership churn is small and no cell changes sign, the land boundary is\n")
cat("not what the result rests on, and one paragraph settles it. If churn is\n")
cat("large, the two axes are answering different questions and BOTH belong in\n")
cat("the paper -- engineered removal as the primary contrast, total CDR as the\n")
cat("portfolio a real government actually chooses between.\n")
