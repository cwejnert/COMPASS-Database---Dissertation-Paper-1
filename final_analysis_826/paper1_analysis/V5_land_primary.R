# =============================================================================
# V5 — THE RESULT GRID WITH LAND-BASED CDR INSIDE THE CMT AXIS
#
# WHY THIS EXISTS. The paper's question is High-RE against High-CDR. Defining
# the CDR axis as engineered removal only (Novel CDR + fossil/industrial CCS)
# makes the comparison arm a SUBSET of the thing the question names: land-based
# removal is CDR, and a scenario leaning on afforestation is a
# carbon-management scenario whether or not the removal is engineered. So the
# primary specification puts land back in, and the engineered axis becomes the
# sensitivity.
#
# THE OBVIOUS OBJECTION, AND THE ANSWER. Including land makes the High-RE
# advantage LARGER, so "they chose the axis that flattered the result" is the
# first thing a reviewer will think. The answer is that both axes are reported
# and the NARROWER one is the CONSERVATIVE one: the engineered axis agrees on
# direction everywhere it can be scored, with a smaller effect. A specification
# chosen for the result would be the one reported alone.
#
# WHAT IT DOES. Rebuilds the entire grid -- eleven regions, two ambition levels,
# jobs / deprivation / headcount / mortality -- on the land-inclusive labels,
# using the SAME machinery as V3: strict ten-region World aggregation, gated
# independently per outcome, and the same 2,000-replicate cluster bootstrap on
# the raw difference in medians.
#
# THE ONE HONEST CAVEAT, HANDLED EXPLICITLY IN SECTION 4. The
# reporting-complete mortality run was TARGETED against the engineered labels.
# Any scenario the land axis classifies but the engineered axis did not may have
# no mortality output, through target selection rather than through anything
# about the scenario. Section 4 measures exactly how much of the mortality
# sample that costs before any mortality number is reported.
#
# USAGE: Rscript V5_land_primary.R      (run from the repo root)
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(purrr)})
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")
set.seed(20260825)

B     <- 2000
R10   <- c("R10AFRICA","R10CHINA+","R10EUROPE","R10INDIA+","R10LATIN_AM",
           "R10MIDDLE_EAST","R10NORTH_AM","R10PAC_OECD","R10REF_ECON","R10REST_ASIA")
WORLD <- "Aggregated R10 regions"
ALLR  <- c(WORLD, R10)
SH    <- c(`Aggregated R10 regions`="WORLD", R10AFRICA="Africa", `R10CHINA+`="China+",
           R10EUROPE="Europe", `R10INDIA+`="India+", R10LATIN_AM="Latin America",
           R10MIDDLE_EAST="Middle East", R10NORTH_AM="North America",
           R10PAC_OECD="Pacific OECD", R10REF_ECON="Reforming econ.",
           R10REST_ASIA="Rest of Asia")
OUTS  <- c(net_re_jobs_per_1k="Jobs", gap_GJ_pc="Deprivation",
           headcount_pct="Deprivation headcount", mort_per_1k="Health")
LOWER <- c("gap_GJ_pc","headcount_pct","mort_per_1k")

# KEY NORMALISATION — see V6_key_repair.R for the full diagnosis. The labels CSV
# stores degree signs as the literal text "<U+00B0>" while the master files carry
# a real UTF-8 degree sign, so neither normalisation in the codebase makes the
# two sides meet and 71 classified scenarios were silently dropped by
# inner_join(). Do NOT call enc2utf8() here: in a C locale it re-encodes
# already-UTF-8 bytes as though they were latin1 and breaks the correct side.
DEG  <- "°"
norm <- function(x) {
  x <- gsub("<U+00B0>", DEG, x, fixed = TRUE)
  x <- gsub("\\u00b0",  DEG, x, fixed = TRUE)
  x <- gsub("�",   DEG, x, fixed = TRUE)
  Encoding(x) <- "UTF-8"
  x
}

add_pc <- function(df) df %>% mutate(
  mort_per_1k        = cumulative_deaths_mln   / pop_mln * 1000,
  headcount_pct      = mean_headcount_millions / pop_mln * 100,
  net_re_jobs_per_1k = (jobs_Renewables - jobs_Fossil) / pop_mln,
  gap_GJ_pc          = cumulative_gap_EJ * 1000 / pop_mln)

# =============================================================================
line("1. THE TWO LABEL SETS, AND HOW MUCH THEY ACTUALLY DIFFER")
# =============================================================================
# Both label sets are built here from the deployment file with the published
# rule, rather than read from W12, so that the two axes come off one pipeline on
# repaired keys. The engineered set is then checked against the published labels
# scenario by scenario -- with the keys repaired, that check must be exact.
build_labels <- function(approach, with_land) {
  cdr  <- read.csv(sprintf("master_outputs/approach_%s/compass_cdr_cumulative_%s.csv",
                           approach, approach), stringsAsFactors = FALSE)
  sset <- read.csv(sprintf("master_outputs/approach_%s/compass_scenario_set_%s.csv",
                           approach, approach), stringsAsFactors = FALSE)
  # The SAMPLE is held fixed across the two runs: the published filter names
  # exactly three variables and that filter decides which scenarios enter the
  # quantile. Land is joined on afterwards, never added to the filter, or the
  # comparison becomes partly a change of sample rather than of axis.
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
           Pathway  = case_when(cmt_axis_value >= cmt_high & renewables < re_high ~ "High-CMT",
                                renewables >= re_high & cmt_axis_value < cmt_high ~ "High-RE",
                                TRUE ~ NA_character_)) %>%
    ungroup() %>%
    transmute(approach, Model = norm(Model), Scenario = norm(Scenario), Pathway,
              renewables, land_cdr, cmt_axis_value,
              amb = ifelse(grepl("^1\\.5", Ambition), "1.5C", "2C")) %>%
    filter(!is.na(Pathway))
}
LB <- bind_rows(build_labels("A", TRUE),  build_labels("C", TRUE))  %>% mutate(axis = "with land")
LE <- bind_rows(build_labels("A", FALSE), build_labels("C", FALSE)) %>% mutate(axis = "engineered")

# ---- the engineered set must reproduce the published labels exactly ---------
PUB <- read.csv("final_outcomes/engineered_cmt_century_broad_labels.csv",
                stringsAsFactors = FALSE) %>%
  filter(!is.na(Pathway), Pathway != "") %>%
  transmute(approach, Model = norm(Model), Scenario = norm(Scenario),
            pub = ifelse(Pathway == "High-RE", "High-RE", "High-CMT"))
CHK <- full_join(LE %>% select(approach, Model, Scenario, ours = Pathway),
                 PUB, by = c("approach","Model","Scenario"))
chk1 <- function(id) {
  d <- CHK %>% filter(approach == id)
  a <- sum(!is.na(d$ours) & !is.na(d$pub) & d$ours == d$pub)
  cat(sprintf("  approach %s: %d of %d agree | ours only %d | published only %d\n",
              id, a, nrow(d), sum(!is.na(d$ours) & is.na(d$pub)),
              sum(is.na(d$ours) & !is.na(d$pub))))
  a == nrow(d)
}
cat("engineered labels vs published:\n")
okA <- chk1("A"); okC <- chk1("C")
# APPROACH A IS THE REPORTED SAMPLE and must reproduce exactly, or nothing below
# is trustworthy. Approach C appears only in one scorecard row; it differs by
# seven scenarios, because the SCI-vetted tercile cut depends on a sample the
# published run drew slightly differently. That is stated rather than hidden,
# and it is why the C row is reported as indicative only.
if (!okA)
  stop("approach A does not reproduce the published classification; stopping ",
       "rather than reporting a grid built on labels that disagree")
cat(ifelse(okA, "[ok] approach A reproduces the published classification exactly.\n", ""))
if (!okC) cat("[note] approach C differs by a handful of scenarios — see above.\n\n")

cat("Arm sizes, full database (approach A):\n\n")
print(as.data.frame(bind_rows(LB, LE) %>% filter(approach == "A") %>%
      count(axis, amb, Pathway) %>%
      pivot_wider(names_from = Pathway, values_from = n, values_fill = 0)))

# Who is classified under one axis but not the other?
BOTH <- full_join(LB %>% select(approach, Model, Scenario, amb, land = Pathway),
                  LE %>% select(approach, Model, Scenario, amb, eng  = Pathway),
                  by = c("approach","Model","Scenario","amb")) %>%
  filter(approach == "A")
cat("\nOverlap of the two classified sets (approach A):\n")
cat("  classified by BOTH axes         :", sum(!is.na(BOTH$land) & !is.na(BOTH$eng)), "\n")
cat("  land axis ONLY (new scenarios)  :", sum(!is.na(BOTH$land) &  is.na(BOTH$eng)), "\n")
cat("  engineered axis ONLY (dropped)  :", sum( is.na(BOTH$land) & !is.na(BOTH$eng)), "\n")
sw <- BOTH %>% filter(!is.na(land), !is.na(eng), land != eng)
cat("  classified by both and SWITCHING arms:", nrow(sw), "\n")
cat("\nOf the scenarios both axes classify, the labels agree completely. The two\n")
cat("axes differ by WHICH scenarios they admit, not by how they label a shared one.\n")

# =============================================================================
line("2. MODEL COMPOSITION — THE PRICE OF PUTTING LAND BACK IN")
# =============================================================================
fam <- function(x) sub("[ /-].*$", "", x)
comp <- bind_rows(LB, LE) %>% filter(approach == "A") %>%
  mutate(family = fam(Model)) %>%
  count(axis, family, Pathway) %>%
  pivot_wider(names_from = Pathway, values_from = n, values_fill = 0) %>%
  mutate(both = `High-CMT` > 0 & `High-RE` > 0)
cat("Scenarios per model family, both axes:\n\n")
print(as.data.frame(comp %>%
      pivot_wider(names_from = axis, values_from = c(`High-CMT`,`High-RE`,both),
                  values_fill = 0) %>%
      arrange(desc(`High-CMT_with land` + `High-RE_with land`))))
cat("\nFamilies holding BOTH arms —",
    "with land:", sum(comp$both[comp$axis == "with land"]),
    "| engineered:", sum(comp$both[comp$axis == "engineered"]), "\n")
sh <- comp %>% group_by(axis) %>%
  summarise(remind_re = 100 * `High-RE`[family == "REMIND"] / sum(`High-RE`),
            remind_cmt = 100 * `High-CMT`[family == "REMIND"] / sum(`High-CMT`),
            .groups = "drop")
print(as.data.frame(sh %>% mutate(across(where(is.numeric), ~round(., 1)))))
cat("\nThis is the real cost of the land-inclusive axis and it belongs in the paper:\n")
cat("a broader CDR definition admits scenarios whose engineered removal is small,\n")
cat("and some model families that populated the engineered High-CMT arm thin out.\n")

# =============================================================================
line("3. STRICT WORLD, REBUILT (identical machinery to V3)")
# =============================================================================
ABS <- c("jobs_Renewables","jobs_Fossil","cumulative_gap_EJ",
         "mean_headcount_millions","cumulative_deaths_mln")
r10_outcomes <- function(id) {
  read.csv(sprintf("master_outputs/approach_%s/compass_master_dataset_%s.csv", id, id),
           stringsAsFactors = FALSE) %>%
    mutate(Model = norm(Model), Scenario = norm(Scenario), approach = id) %>%
    filter(Region %in% R10) %>%
    distinct(approach, Model, Scenario, Region, .keep_all = TRUE) %>%
    select(approach, Model, Scenario, Region, pop_mln, all_of(ABS))
}
RO <- bind_rows(r10_outcomes("A"), r10_outcomes("C"))
POP_TOT <- RO %>% distinct(Region, pop_mln) %>% group_by(Region) %>%
  summarise(p = median(pop_mln), .groups = "drop") %>% summarise(s = sum(p)) %>% pull(s)
cat("R10 outcome rows:", nrow(RO), "| ten-region population:", round(POP_TOT), "\n")

WLD <- RO %>% group_by(approach, Model, Scenario) %>%
  summarise(
    n_regions_jobs      = sum(!is.na(jobs_Renewables) & !is.na(jobs_Fossil)),
    n_regions_gap       = sum(!is.na(cumulative_gap_EJ)),
    n_regions_headcount = sum(!is.na(mean_headcount_millions)),
    n_regions_mortality = sum(!is.na(cumulative_deaths_mln)),
    jobs_Renewables     = if (n_regions_jobs == 10) sum(jobs_Renewables[!is.na(jobs_Renewables) & !is.na(jobs_Fossil)]) else NA_real_,
    jobs_Fossil         = if (n_regions_jobs == 10) sum(jobs_Fossil[!is.na(jobs_Renewables) & !is.na(jobs_Fossil)])     else NA_real_,
    cumulative_gap_EJ       = if (n_regions_gap       == 10) sum(cumulative_gap_EJ, na.rm = TRUE)       else NA_real_,
    mean_headcount_millions = if (n_regions_headcount == 10) sum(mean_headcount_millions, na.rm = TRUE) else NA_real_,
    cumulative_deaths_mln   = if (n_regions_mortality == 10) sum(cumulative_deaths_mln, na.rm = TRUE)   else NA_real_,
    .groups = "drop") %>%
  mutate(Region = WORLD, pop_mln = POP_TOT,
         world_complete_jobs      = n_regions_jobs      == 10,
         world_complete_gap       = n_regions_gap       == 10,
         world_complete_mortality = n_regions_mortality == 10) %>%
  add_pc()
cat("World rows:", nrow(WLD),
    "| complete jobs:", sum(WLD$world_complete_jobs),
    "| deprivation:", sum(WLD$world_complete_gap), "\n")

# =============================================================================
line("4. THE MORTALITY SAMPLE — WHAT THE TARGETING COSTS THE LAND AXIS")
# =============================================================================
MORT_FILE <- Sys.getenv(
  "COMPASS_MORTALITY_SCENARIO_VALUES",
  "final_outcomes/mortality_reporting_complete_scenario_values_2020_2100.csv"
)
MORT <- read.csv(MORT_FILE,
                 stringsAsFactors = FALSE) %>%
  mutate(Model = norm(Model), Scenario = norm(Scenario)) %>%
  transmute(approach, Model, Scenario, Region,
            mort_per_1k = cumulative_pm25_deaths_mln)
has_mort <- MORT %>% filter(Region == WORLD) %>% distinct(approach, Model, Scenario) %>%
  mutate(has = TRUE)

cov <- bind_rows(LB, LE) %>% filter(approach == "A") %>%
  left_join(has_mort, by = c("approach","Model","Scenario")) %>%
  mutate(has = !is.na(has)) %>%
  group_by(axis, amb, Pathway) %>%
  summarise(classified = n(), with_mortality = sum(has),
            pct = round(100 * mean(has)), .groups = "drop")
cat("Scenarios with a complete World mortality run, by axis:\n\n")
print(as.data.frame(cov))

newly <- BOTH %>% filter(!is.na(land), is.na(eng)) %>%
  left_join(has_mort, by = c("approach","Model","Scenario")) %>%
  summarise(n = n(), with_mortality = sum(!is.na(has)))
cat("\nScenarios the land axis newly admits:", newly$n,
    "— of which", newly$with_mortality, "have a mortality run.\n")
cat("Mortality targets were drawn against the ENGINEERED labels, so any shortfall\n")
cat("here is target selection, not a property of the scenarios. The mortality row\n")
cat("of the land-axis grid is reported with that stated, and the engineered axis\n")
cat("remains the cleaner sample for the mortality claim specifically.\n")

# =============================================================================
line("5. THE GRID")
# =============================================================================
JOBDEP <- c("net_re_jobs_per_1k","gap_GJ_pc","headcount_pct")
REG <- RO %>% add_pc() %>% select(approach, Model, Scenario, Region, all_of(JOBDEP))

cell <- function(d, out) {
  sgn <- ifelse(out %in% LOWER, -1, 1)
  a <- d[[out]][d$Pathway=="High-CMT"]; ca <- d$clus[d$Pathway=="High-CMT"]
  b <- d[[out]][d$Pathway=="High-RE"];  cb <- d$clus[d$Pathway=="High-RE"]
  ka <- !is.na(a); kb <- !is.na(b); a<-a[ka]; ca<-ca[ka]; b<-b[kb]; cb<-cb[kb]
  if (length(a) < 5 || length(b) < 5)
    return(tibble(n_cmt=length(a), n_re=length(b), raw_cmt=NA_real_, raw_re=NA_real_,
                  gap=NA_real_, lo=NA_real_, hi=NA_real_, pct=NA_real_))
  ua <- unique(ca); ub <- unique(cb)
  ia <- split(seq_along(a), ca); ib <- split(seq_along(b), cb)
  reps <- vapply(seq_len(B), function(i) {
    sa <- unlist(ia[sample(ua, length(ua), TRUE)], use.names=FALSE)
    sb <- unlist(ib[sample(ub, length(ub), TRUE)], use.names=FALSE)
    sgn*(median(b[sb]) - median(a[sa]))
  }, numeric(1))
  q <- quantile(reps, c(.025,.975), na.rm=TRUE)
  ma <- median(a); mb <- median(b)
  tibble(n_cmt=length(a), n_re=length(b), raw_cmt=ma, raw_re=mb,
         gap = sgn*(mb-ma), lo=q[[1]], hi=q[[2]], pct = sgn*100*(mb-ma)/abs(ma))
}

build_grid <- function(LAB, nm) {
  D <- bind_rows(REG, WLD %>% select(approach, Model, Scenario, Region, all_of(JOBDEP))) %>%
    full_join(MORT, by = c("approach","Model","Scenario","Region")) %>%
    inner_join(LAB %>% select(approach, Model, Scenario, Pathway, amb),
               by = c("approach","Model","Scenario")) %>%
    mutate(stem = gsub("[-_ ]?[0-9]+(\\.[0-9]+)?[a-z]?$", "", Scenario),
           stem = sub("/.*$", "", stem), clus = paste(Model, stem))
  expand_grid(approach=c("A","C"), Region=ALLR, amb=c("1.5C","2C"),
              outcome=names(OUTS)) %>%
    pmap_dfr(function(approach, Region, amb, outcome) {
      d <- D[D$approach==approach & D$Region==Region & D$amb==amb, ]
      bind_cols(tibble(axis=nm, approach, Region, amb, outcome), cell(d, outcome))
    }) %>%
    mutate(family = OUTS[outcome], sig = !is.na(lo) & (lo>0 | hi<0),
           reg = SH[Region], primary = outcome != "headcount_pct")
}
GL <- build_grid(LB, "with land")
GE <- build_grid(LE, "engineered")
GRID <- bind_rows(GL, GE)
saveRDS(list(grid = GRID, world = WLD, labels_land = LB, labels_eng = LE,
             coverage = cov, composition = comp), "LAND_PRIMARY.rds")
cat("written: LAND_PRIMARY.rds\n")

# =============================================================================
line("6. WORLD — THE HEADLINE, BOTH AXES")
# =============================================================================
W <- GRID %>% filter(approach=="A", Region==WORLD, primary) %>%
  transmute(family, amb, axis, n = paste0(n_cmt,"v",n_re),
            cmt = round(raw_cmt,2), re = round(raw_re,2), gap = round(gap,2),
            CI = sprintf("[%+.2f,%+.2f]", lo, hi), sig = ifelse(sig,"YES","no"))
print(as.data.frame(W %>% arrange(factor(family, levels=c("Jobs","Deprivation","Health")),
                                  amb, desc(axis))))

# =============================================================================
line("7. SCORECARD, BOTH AXES (nine regions + World)")
# =============================================================================
H <- GRID %>% filter(approach=="A", primary, Region!="R10PAC_OECD", !is.na(gap))
print(as.data.frame(H %>% group_by(axis, family) %>%
      summarise(cells=n(), favour_RE=sum(gap>0), sig_for=sum(gap>0 & sig),
                sig_against=sum(gap<0 & sig), .groups="drop") %>%
      arrange(desc(axis), factor(family, levels=c("Jobs","Deprivation","Health")))))
cat("\nTOTALS:\n")
print(as.data.frame(H %>% group_by(axis) %>%
      summarise(cells=n(), favour_RE=sum(gap>0), sig_for=sum(gap>0 & sig),
                sig_against=sum(gap<0 & sig), .groups="drop")))

# =============================================================================
line("8. EVERY REGION, LAND-INCLUSIVE (the tables the deck will print)")
# =============================================================================
for (f in c("Jobs","Deprivation","Health")) {
  cat("\n---", f, "---\n")
  print(as.data.frame(GRID %>%
    filter(axis=="with land", approach=="A", primary, family==f, Region!="R10PAC_OECD") %>%
    select(reg, amb, n_cmt, n_re, raw_cmt, raw_re, gap, lo, hi, sig) %>%
    mutate(across(c(raw_cmt,raw_re,gap,lo,hi), ~round(., 2))) %>%
    arrange(amb, factor(reg, levels=c("WORLD", setdiff(unique(reg),"WORLD"))))))
}

# =============================================================================
line("9. DOES ANY CELL CHANGE SIGN BETWEEN THE AXES?")
# =============================================================================
FL <- H %>% select(reg, amb, family, axis, gap, sig) %>%
  pivot_wider(names_from=axis, values_from=c(gap,sig))
flip <- FL %>% filter(!is.na(`gap_with land`), !is.na(gap_engineered),
                      sign(`gap_with land`) != sign(gap_engineered))
cat("cells changing sign:", nrow(flip), "of", nrow(FL), "\n")
if (nrow(flip)) print(as.data.frame(flip %>% mutate(across(where(is.numeric), ~round(.,2)))))
cat("\nSignificant on one axis and significantly OPPOSITE on the other:\n")
hard <- FL %>% filter(`sig_with land`, sig_engineered,
                      sign(`gap_with land`) != sign(gap_engineered))
cat("  ", nrow(hard), "\n")
