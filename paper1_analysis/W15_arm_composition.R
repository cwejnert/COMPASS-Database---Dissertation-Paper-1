# =============================================================================
# W15 — WHAT IS EACH ARM MADE OF, AND DOES THE RESULT SURVIVE ITS BIGGEST MODEL?
#
# WHY THIS IS THE RIGHT DIAGNOSTIC. W14 asks whether a family that holds BOTH
# arms agrees with the pooled direction. That is a useful question but it is not
# the one that worries a reader of these tables. The worry is simpler: the two
# arms are not built from the same mix of models, so a "pathway effect" could be
# a "REMIND effect" wearing a pathway label.
#
#   effective number of models = 1 / sum(share^2), the inverse Herfindahl index.
#   It answers "how many models is this arm really made of?" -- an arm that is
#   88% one model has an effective count near 1 however many families appear in
#   the tail.
#
#           1.5C High-CDR  3.6      1.5C High-RE  1.3
#           2C   High-CDR  4.7      2C   High-RE  2.0
#
# The High-CDR arm is a genuine multi-model ensemble at both ambition levels.
# The High-RE arm is REMIND with a fringe. That asymmetry -- not the overlap
# between arms -- is what the pooled comparison has to survive.
#
# SO THIS DOES TWO THINGS:
#   1. Reports the composition of each arm, by ambition, with concentration.
#   2. LEAVE-ONE-FAMILY-OUT: drops each family from BOTH arms in turn and
#      recomputes the cell. If the result is a REMIND effect, dropping REMIND
#      removes it. If it survives, the pathway reading holds on the rest of the
#      ensemble, which is the strongest thing that can be said short of a
#      balanced sample.
#
# WHAT A DROP DOES AND DOES NOT PROVE. Dropping the dominant family shrinks the
# arm and widens the interval, so losing significance is expected and is NOT by
# itself evidence of a confound -- the sign and the magnitude are the things to
# read. A SIGN FLIP would be damning; a wider interval on the same estimate is
# just less data.
#
# USAGE: Rscript W15_arm_composition.R      (run from the repo root)
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(purrr)})
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")
set.seed(20260825)

B     <- 2000
AXIS  <- "with land"
R10   <- c("R10AFRICA","R10CHINA+","R10EUROPE","R10INDIA+","R10LATIN_AM",
           "R10MIDDLE_EAST","R10NORTH_AM","R10PAC_OECD","R10REF_ECON","R10REST_ASIA")
WORLD <- "Aggregated R10 regions"
ALLR  <- c(WORLD, R10)
DROP  <- "R10PAC_OECD"
SH    <- c(`Aggregated R10 regions`="WORLD", R10AFRICA="Africa", `R10CHINA+`="China+",
           R10EUROPE="Europe", `R10INDIA+`="India+", R10LATIN_AM="Latin America",
           R10MIDDLE_EAST="Middle East", R10NORTH_AM="North America",
           R10PAC_OECD="Pacific OECD", R10REF_ECON="Reforming econ.",
           R10REST_ASIA="Rest of Asia")
OUTS  <- c(net_re_jobs_per_1k="Jobs", gap_GJ_pc="Deprivation", mort_per_1k="Health")
LOWER <- c("gap_GJ_pc","mort_per_1k")

DEG  <- "°"
norm <- function(x) {
  x <- gsub("<U+00B0>", DEG, x, fixed = TRUE)
  x <- gsub("\\u00b0",  DEG, x, fixed = TRUE)
  x <- gsub("�",   DEG, x, fixed = TRUE)
  Encoding(x) <- "UTF-8"; x
}
add_pc <- function(df) df %>% mutate(
  net_re_jobs_per_1k = (jobs_Renewables - jobs_Fossil) / pop_mln,
  gap_GJ_pc          = cumulative_gap_EJ * 1000 / pop_mln,
  headcount_pct      = mean_headcount_millions / pop_mln * 100)

# =============================================================================
line("1. WHAT EACH ARM IS MADE OF")
# =============================================================================
LP  <- readRDS("LAND_PRIMARY.rds")
LAB <- LP$labels_land %>% filter(approach == "A") %>%
  transmute(Model, Scenario, Pathway, amb, fam = sub("[ /-].*$", "", Model))

COMP <- LAB %>% count(amb, Pathway, fam) %>%
  group_by(amb, Pathway) %>%
  mutate(arm_n = sum(n), share = n / arm_n) %>%
  arrange(amb, Pathway, desc(n)) %>% ungroup()

CONC <- COMP %>% group_by(amb, Pathway) %>%
  summarise(arm_n = arm_n[1], families = n(),
            top_family = fam[1], top_share = round(100 * share[1], 1),
            hhi = round(sum(share^2), 3),
            eff_models = round(1 / sum(share^2), 2), .groups = "drop") %>%
  mutate(arm = ifelse(Pathway == "High-RE", "High-RE", "High-CDR"))

for (a in c("1.5C","2C")) {
  cat("\n--- ", a, " ---\n", sep = "")
  print(as.data.frame(COMP %>% filter(amb == a) %>%
    mutate(arm = ifelse(Pathway == "High-RE", "High-RE", "High-CDR"),
           pct = round(100 * share, 1)) %>%
    select(arm, fam, n, pct) %>%
    pivot_wider(names_from = arm, values_from = c(n, pct), values_fill = 0) %>%
    arrange(desc(`n_High-CDR` + `n_High-RE`))))
}
cat("\nCONCENTRATION — how many models is each arm really made of?\n\n")
print(as.data.frame(CONC %>% select(amb, arm, arm_n, families, top_family,
                                    top_share, eff_models)))
cat("\neff_models is 1/sum(share^2). The High-CDR arm is a multi-model ensemble;\n")
cat("the High-RE arm is REMIND with a fringe. That asymmetry is what the pooled\n")
cat("comparison has to survive, and section 3 tests it directly.\n")

# =============================================================================
line("2. THE SCENARIO FRAME (same construction as the pooled grid)")
# =============================================================================
ABS <- c("jobs_Renewables","jobs_Fossil","cumulative_gap_EJ","mean_headcount_millions")
RO <- read.csv("master_outputs/approach_A/compass_master_dataset_A.csv",
               stringsAsFactors = FALSE) %>%
  mutate(Model = norm(Model), Scenario = norm(Scenario)) %>%
  filter(Region %in% R10) %>%
  distinct(Model, Scenario, Region, .keep_all = TRUE) %>%
  select(Model, Scenario, Region, pop_mln, all_of(ABS))
WLD <- LP$world %>% filter(approach == "A") %>%
  select(Model, Scenario, Region, net_re_jobs_per_1k, gap_GJ_pc)
MORT <- read.csv("final_outcomes/mortality_reporting_complete_scenario_values_2020_2100.csv",
                 stringsAsFactors = FALSE) %>%
  filter(approach == "A") %>%
  mutate(Model = norm(Model), Scenario = norm(Scenario)) %>%
  transmute(Model, Scenario, Region, mort_per_1k = cumulative_pm25_deaths_mln)

F <- bind_rows(RO %>% add_pc() %>% select(Model, Scenario, Region,
                                          net_re_jobs_per_1k, gap_GJ_pc),
               WLD) %>%
  full_join(MORT, by = c("Model","Scenario","Region")) %>%
  inner_join(LAB, by = c("Model","Scenario")) %>%
  mutate(stem = gsub("[-_ ]?[0-9]+(\\.[0-9]+)?[a-z]?$", "", Scenario),
         stem = sub("/.*$", "", stem), clus = paste(Model, stem))

cell <- function(d, out) {
  sgn <- ifelse(out %in% LOWER, -1, 1)
  a <- d[[out]][d$Pathway=="High-CMT"]; ca <- d$clus[d$Pathway=="High-CMT"]
  b <- d[[out]][d$Pathway=="High-RE"];  cb <- d$clus[d$Pathway=="High-RE"]
  ka <- !is.na(a); kb <- !is.na(b); a<-a[ka]; ca<-ca[ka]; b<-b[kb]; cb<-cb[kb]
  if (length(a) < 5 || length(b) < 5)
    return(tibble(n_cmt=length(a), n_re=length(b), gap=NA_real_, lo=NA_real_, hi=NA_real_))
  ua <- unique(ca); ub <- unique(cb)
  ia <- split(seq_along(a), ca); ib <- split(seq_along(b), cb)
  reps <- vapply(seq_len(B), function(i) {
    sa <- unlist(ia[sample(ua, length(ua), TRUE)], use.names=FALSE)
    sb <- unlist(ib[sample(ub, length(ub), TRUE)], use.names=FALSE)
    sgn*(median(b[sb]) - median(a[sa]))
  }, numeric(1))
  q <- quantile(reps, c(.025,.975), na.rm=TRUE)
  tibble(n_cmt=length(a), n_re=length(b),
         gap = sgn*(median(b)-median(a)), lo=q[[1]], hi=q[[2]])
}

# reproduce the published grid before trusting any drop
POOL <- LP$grid %>% filter(axis == AXIS, approach == "A", outcome %in% names(OUTS)) %>%
  select(Region, amb, outcome, p_gap = gap, p_sig = sig)
base <- expand_grid(Region = ALLR, amb = c("1.5C","2C"), outcome = names(OUTS)) %>%
  pmap_dfr(function(Region, amb, outcome)
    bind_cols(tibble(Region, amb, outcome),
              cell(F[F$Region==Region & F$amb==amb, ], outcome)))
V <- base %>% inner_join(POOL, by = c("Region","amb","outcome"))
dd <- with(V, ifelse(is.na(gap) & is.na(p_gap), 0, abs(gap - p_gap)))
cat("reproduces the pooled grid — max |difference|:", signif(max(dd, na.rm=TRUE), 3), "\n")
if (max(dd, na.rm = TRUE) > 1e-9)
  stop("frame does not reproduce the pooled grid; stopping")
cat("[ok]\n")

# =============================================================================
line("3. LEAVE ONE FAMILY OUT — WORLD")
# =============================================================================
fams <- sort(unique(LAB$fam))
loo_world <- expand_grid(drop = c("(none)", fams), amb = c("1.5C","2C"),
                         outcome = names(OUTS)) %>%
  pmap_dfr(function(drop, amb, outcome) {
    d <- F[F$Region == WORLD & F$amb == amb, ]
    if (drop != "(none)") d <- d[d$fam != drop, ]
    bind_cols(tibble(drop, amb, outcome), cell(d, outcome))
  }) %>%
  mutate(family = OUTS[outcome], sig = !is.na(lo) & (lo > 0 | hi < 0))

for (a in c("1.5C","2C")) {
  cat("\n--- WORLD, ", a, " (positive favours High-RE) ---\n", sep = "")
  print(as.data.frame(loo_world %>% filter(amb == a) %>%
    transmute(drop, family, n = paste0(n_cmt, "v", n_re), gap = round(gap, 2),
              CI = ifelse(is.na(lo), "—", sprintf("[%+.2f,%+.2f]", lo, hi)),
              sig = ifelse(is.na(gap), "—", ifelse(sig, "YES", "no"))) %>%
    pivot_wider(names_from = family, values_from = c(n, gap, CI, sig)) %>%
    select(drop, starts_with("n_"), starts_with("gap_"), starts_with("sig_"))))
}

# =============================================================================
line("4. THE ONE THAT MATTERS — DROP REMIND, EVERY REGION")
# =============================================================================
loo_reg <- expand_grid(drop = c("(none)","REMIND"), Region = ALLR,
                       amb = c("1.5C","2C"), outcome = names(OUTS)) %>%
  pmap_dfr(function(drop, Region, amb, outcome) {
    d <- F[F$Region == Region & F$amb == amb, ]
    if (drop != "(none)") d <- d[d$fam != drop, ]
    bind_cols(tibble(drop, Region, amb, outcome), cell(d, outcome))
  }) %>%
  mutate(family = OUTS[outcome], sig = !is.na(lo) & (lo > 0 | hi < 0),
         reg = SH[Region])
saveRDS(list(composition = COMP, concentration = CONC,
             loo_world = loo_world, loo_region = loo_reg), "W15_ARMS.rds")
cat("written: W15_ARMS.rds\n\n")

W <- loo_reg %>% filter(Region != DROP) %>%
  select(reg, amb, family, drop, gap, sig) %>%
  pivot_wider(names_from = drop, values_from = c(gap, sig)) %>%
  rename(base = `gap_(none)`, noREMIND = gap_REMIND,
         base_sig = `sig_(none)`, noR_sig = sig_REMIND) %>%
  filter(!is.na(base), !is.na(noREMIND))

cat("cells scoreable both with and without REMIND:", nrow(W), "\n")
cat("SIGN FLIPS when REMIND is dropped:", sum(sign(W$base) != sign(W$noREMIND)), "\n")
cat("lose significance:", sum(W$base_sig & !W$noR_sig),
    "| gain significance:", sum(!W$base_sig & W$noR_sig), "\n\n")
print(as.data.frame(W %>% group_by(family) %>%
  summarise(cells = n(),
            favour_RE_base = sum(base > 0), favour_RE_noREMIND = sum(noREMIND > 0),
            sig_base = sum(base_sig), sig_noREMIND = sum(noR_sig),
            sign_flips = sum(sign(base) != sign(noREMIND)), .groups = "drop")))

fl <- W %>% filter(sign(base) != sign(noREMIND))
if (nrow(fl)) {
  cat("\nthe cells that flip:\n")
  print(as.data.frame(fl %>% mutate(across(where(is.numeric), ~round(., 2)))))
}

cat("\n--- every region, jobs, with and without REMIND ---\n")
print(as.data.frame(W %>% filter(family == "Jobs") %>%
  transmute(reg, amb, base = round(base, 1), noREMIND = round(noREMIND, 1),
            change_pct = round(100 * (noREMIND - base) / abs(base)),
            base_sig = ifelse(base_sig, "YES", "no"),
            noR_sig = ifelse(noR_sig, "YES", "no")) %>%
  arrange(amb, reg)))
