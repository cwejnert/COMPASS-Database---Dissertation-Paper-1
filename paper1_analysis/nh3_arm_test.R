# =============================================================================
# NH3 ARM TEST — does the ammonia problem actually move the PAPER'S RESULT?
#
# The probe settled that NH3 matters: removing it cuts global PM2.5 mortality by
# about 11% (FUSION), consistently across REMIND-MAgPIE and MESSAGE-GLOBIOM.
# So the byte-identical files were a failed run, not a null finding.
#
# BUT "NH3 matters" is not the question the paper asks. The paper reports
# Cliff's delta, which is RANK-BASED. If removing NH3 scales every scenario's
# mortality by roughly the same factor, the ranks are unchanged and the pathway
# contrast is IDENTICAL. The contrast only moves if the NH3 effect differs
# systematically BETWEEN THE ARMS.
#
# The probe already hints it does not:
#     REMIND-MAgPIE  (99% High-RE)   -11.1%, -10.7%
#     MESSAGE-GLOBIOM (mostly CMT)   -10.9%, -12.4%
# Those overlap. This script tests it properly on a real sample of both arms.
#
# It answers the question in ~15 minutes instead of a five-hour batch, and it
# answers it DIRECTLY: it recomputes Cliff's delta with and without NH3 on the
# same scenarios, which is the number that would actually appear in the paper.
#
# USAGE: setwd() to the folder holding the rfasst script, then
#   source("nh3_arm_test.R")
# Set N_PER_ARM lower for a quicker look, higher for a tighter answer.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr)})
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")
set.seed(20260821)

N_PER_ARM   <- as.integer(Sys.getenv("N_PER_ARM", "20"))
COMPASS_DIR <- Sys.getenv("COMPASS_DIR",
                 "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/COMPASS")

CANDIDATES <- c("COMPASS_rfasst_full_allR10.R", "COMPASS_rfasst_full.R")
RFASST <- CANDIDATES[file.exists(CANDIDATES)][1]
if (is.na(RFASST)) stop("No rfasst script found in ", getwd())

# ---------------------------------------- 1. definitions, not the batch loop --
line("1. LOADING DEFINITIONS")
src <- readLines(RFASST, warn = FALSE)
cut <- grep("SECTION 5", src)
if (!length(cut)) stop("No 'SECTION 5' marker; refusing to run the whole batch.")
prefix <- tempfile(fileext = ".R")
writeLines(src[seq_len(cut[1] - 1)], prefix)
source(prefix, local = FALSE)
for (need in c("em_clean","build_em_list","run_rfasst_for_scenario",
               "COMPASS_R10_REGIONS","FASST_YEARS","fasst_weights"))
  if (!exists(need)) stop("missing after sourcing the prefix: ", need)

# fasst_region -> r10_region is many-to-one (weights sum to 1 within each R10),
# so mortality can be aggregated straight back up by summing.
f2r <- fasst_weights %>% distinct(fasst_region, r10_region)
dup <- f2r %>% count(fasst_region) %>% filter(n > 1)
if (nrow(dup)) {
  cat("  [!]", nrow(dup), "fasst regions map to more than one R10 region;\n")
  cat("      falling back to GLOBAL totals only.\n")
  f2r <- NULL
}

# ------------------------------------------------- 2. the classified arms ----
line("2. SAMPLING BOTH ARMS")
pw_path <- file.path(COMPASS_DIR, "compass_pathway_tercile_A.rds")
if (!file.exists(pw_path)) stop("Not found: ", pw_path)
lab <- readRDS(pw_path) %>%
  filter(!is.na(Pathway_excl)) %>%
  distinct(model = Model, scenario = Scenario, arm = Pathway_excl)
cat("classified scenarios:", nrow(lab), "\n")

# only scenarios that actually reach rfasst with non-zero NH3 can be tested
usable <- em_clean %>%
  filter(pollutant == "NH3") %>%
  group_by(model, scenario) %>%
  summarise(nh3 = sum(value_kt, na.rm = TRUE), n_reg = n_distinct(region),
            .groups = "drop") %>%
  filter(nh3 > 0, n_reg >= 10) %>%
  inner_join(lab, by = c("model","scenario"))
print(usable %>% count(arm) %>% as.data.frame())
if (n_distinct(usable$arm) < 2)
  stop("Only one arm has usable scenarios; cannot compare.")

pick <- usable %>% group_by(arm) %>%
  slice_sample(n = min(N_PER_ARM, max(table(usable$arm)))) %>% ungroup()
cat("\nsampled", nrow(pick), "scenarios (", N_PER_ARM, "requested per arm )\n")
print(pick %>% count(arm, fam = sub("[ /].*$","",model)) %>% as.data.frame())

# ---------------------------------------------------------- 3. run pairs -----
line("3. RUNNING EACH SAMPLED SCENARIO WITH AND WITHOUT NH3")
cat("about", round(nrow(pick) * 0.35, 0), "minutes\n")

one <- function(mod, scen) {
  em <- em_clean %>% filter(model == mod, scenario == scen)
  req <- c("SO2","NOX","BC","OM","NH3","VOC","CH4","CO")
  miss <- setdiff(req, unique(em$pollutant))
  if (length(miss)) em <- bind_rows(em, crossing(
    region = COMPASS_R10_REGIONS, pollutant = miss,
    year = as.integer(FASST_YEARS)) %>%
    mutate(value_kt = 0, model = mod, scenario = scen))
  L1 <- build_em_list(em)
  L0 <- build_em_list(em %>% mutate(
          value_kt = ifelse(pollutant == "NH3", 0, value_kt)))

  nh3_of <- function(L) sum(vapply(L, function(d)
    sum(d$value[d$pollutant == "NH3"], na.rm = TRUE), numeric(1)))
  if (!(nh3_of(L1) > 0 && nh3_of(L0) == 0)) return(NULL)   # never compare a no-op

  grab <- function(r) {
    if (is.null(r$pm25)) return(NULL)
    d <- bind_rows(r$pm25)
    if (!"FUSION" %in% names(d) || !"region" %in% names(d)) return(NULL)
    d %>% group_by(fasst_region = region) %>%
      summarise(v = sum(as.numeric(FUSION), na.rm = TRUE), .groups = "drop")
  }
  A <- grab(run_rfasst_for_scenario(L1, paste0(scen, "_w")))
  B <- grab(run_rfasst_for_scenario(L0, paste0(scen, "_z")))
  if (is.null(A) || is.null(B)) return(NULL)

  j <- A %>% rename(with_nh3 = v) %>%
    inner_join(B %>% rename(zero_nh3 = v), by = "fasst_region")
  if (!is.null(f2r)) j <- j %>% inner_join(f2r, by = "fasst_region") %>%
    group_by(r10_region) %>%
    summarise(with_nh3 = sum(with_nh3), zero_nh3 = sum(zero_nh3), .groups = "drop")
  else j <- j %>% summarise(r10_region = "GLOBAL",
                            with_nh3 = sum(with_nh3), zero_nh3 = sum(zero_nh3))
  j %>% mutate(model = mod, scenario = scen)
}

RES <- bind_rows(lapply(seq_len(nrow(pick)), function(i) {
  if (i %% 5 == 0 || i == 1) cat("  [", i, "/", nrow(pick), "]\n", sep="")
  one(pick$model[i], pick$scenario[i])
}))
if (!nrow(RES)) stop("No scenario produced a comparable pair.")
RES <- RES %>% inner_join(lab, by = c("model","scenario")) %>%
  mutate(pct = 100*(zero_nh3 - with_nh3)/with_nh3)
saveRDS(RES, "nh3_arm_test_result.rds")

# ---------------------------------------------------------- 4. the answer ----
line("4. DOES THE NH3 EFFECT DIFFER BETWEEN THE ARMS?")
sc <- RES %>% group_by(model, scenario, arm) %>%
  summarise(with_nh3 = sum(with_nh3), zero_nh3 = sum(zero_nh3), .groups = "drop") %>%
  mutate(pct = 100*(zero_nh3 - with_nh3)/with_nh3)
print(sc %>% group_by(arm) %>%
      summarise(n = n(), median_pct = round(median(pct), 2),
                q25 = round(quantile(pct, .25), 2), q75 = round(quantile(pct, .75), 2),
                .groups = "drop") %>% as.data.frame())
if (n_distinct(sc$arm) == 2) {
  w <- suppressWarnings(wilcox.test(pct ~ arm, data = sc))
  cat("\nWilcoxon on the per-scenario % change, High-CMT vs High-RE: p =",
      signif(w$p.value, 3), "\n")
  cat("A LARGE p means NH3 hits both arms equally, so it cannot move a\n")
  cat("rank-based contrast.\n")
}

line("5. THE NUMBER THAT WOULD APPEAR IN THE PAPER")
cliff <- function(a, b) {
  a <- a[!is.na(a)]; b <- b[!is.na(b)]
  if (length(a) < 3 || length(b) < 3) return(NA_real_)
  r <- rank(c(a, b)); n1 <- length(a); n2 <- length(b)
  2*((sum(r[(n1+1):(n1+n2)]) - n2*(n2+1)/2)/(n1*n2)) - 1
}
# advantage-signed: mortality is lower-is-better, so flip
adv <- RES %>% group_by(r10_region) %>%
  summarise(
    adv_with = -cliff(with_nh3[arm=="High-CMT"], with_nh3[arm=="High-RE"]),
    adv_zero = -cliff(zero_nh3[arm=="High-CMT"], zero_nh3[arm=="High-RE"]),
    n_cmt = sum(arm=="High-CMT"), n_re = sum(arm=="High-RE"), .groups = "drop") %>%
  mutate(shift = adv_zero - adv_with,
         flips = !is.na(adv_with) & !is.na(adv_zero) & sign(adv_with) != sign(adv_zero))
print(adv %>% mutate(across(where(is.numeric), ~round(., 3))) %>% as.data.frame())
cat("\ncells changing sign:", sum(adv$flips, na.rm = TRUE), "of",
    sum(!is.na(adv$shift)), "\n")
cat("largest shift in Cliff's delta:", round(max(abs(adv$shift), na.rm = TRUE), 3), "\n\n")

biggest <- max(abs(adv$shift), na.rm = TRUE)
if (sum(adv$flips, na.rm = TRUE) == 0 && biggest < 0.10) {
  cat("VERDICT: NH3 changes the LEVEL of mortality by ~11% but not the\n")
  cat("CONTRAST. It hits both arms alike, so the rank-based comparison the\n")
  cat("paper reports is unaffected. Report the level caveat in the SI and keep\n")
  cat("the mortality result as it stands -- no full re-run needed.\n")
} else {
  cat("VERDICT: NH3 DOES move the contrast. Run the full batch with\n")
  cat("nh3_run_checked.R and re-run the mortality analysis against it.\n")
}
cat("\nwritten: nh3_arm_test_result.rds (send this back)\n")
