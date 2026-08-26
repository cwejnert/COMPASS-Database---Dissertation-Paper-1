# =============================================================================
# V6 — THE MANGLED SCENARIO KEYS, AND WHAT REPAIRING THEM DOES
#
# THE DEFECT. engineered_cmt_century_broad_labels.csv stores degree signs as the
# LITERAL SEVEN-CHARACTER TEXT "<U+00B0>":
#
#     label  bytes: 43 4f 4d 4d 49 54 2d 32 3c 55 2b 30 30 42 30 3e 43 ...
#                                            <  U  +  0  0  B  0  >
#     master bytes: 43 4f 4d 4d 49 54 2d 32 c2 b0 43 ...
#                                            ^^^^^ a real UTF-8 degree sign
#
# so "COMMIT-2<U+00B0>C-2020" never equals "COMMIT-2°C-2020". Neither
# normalisation in the codebase repairs this: iconv(sub="") deletes the real
# degree sign on the master side and leaves the literal text alone on the label
# side, and normalise_id() converts a replacement character but has never seen
# this escape. The two sides simply do not meet.
#
# WHY IT MATTERS. COMPASS_engineered_cmt_century_outcomes_summary.R joins them
# with inner_join(), which drops non-matching rows SILENTLY. 71 classified
# scenarios never reach the outcome tables. The published medians -- and
# therefore every figure and slide built on them -- are computed on a sample
# that lost those scenarios to a text-encoding accident rather than to any
# property of the scenarios.
#
# The same 71 also make the coverage flow wrong in the opposite direction:
# V3_world_strict.R counts them with sum(world_complete_jobs, na.rm = TRUE),
# and a failed join produces NA, so every one was counted as "World incomplete".
# That is where "42 of 64" came from.
#
# WHAT THIS DOES. Repairs the keys, re-joins, and reports exactly what moves.
# The repair is textual and reversible: translate the escape back to the
# character it stands for.
#
# USAGE: Rscript V6_key_repair.R      (run from the repo root)
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

# The normalisation actually in use upstream, kept so the defect is reproducible.
old_norm <- function(x) iconv(x, from = "", to = "UTF-8", sub = "")

# The repair. Every escape form seen in these files is mapped to the character
# it denotes, THEN the string is normalised, so both sides land in one encoding.
#   "<U+00B0>"  the literal escape written into the labels CSV
#   "°"    the six-character backslash form
#   ""         a replacement character from a lossy round-trip
#
# NOTE ON enc2utf8. Do NOT call it here. This session's locale is C, and
# enc2utf8() on a string whose bytes are ALREADY UTF-8 (c2 b0) re-encodes them
# as though they were latin1, giving c3 82 c2 b0 -- so the "normalisation"
# breaks the side that was correct. The repair is purely textual: replace the
# escape sequences with the byte sequence the master file actually contains,
# and declare the encoding without converting.
DEG <- "°"                                   # R parses this to the character
fix_keys <- function(x) {
  x <- gsub("<U+00B0>", DEG, x, fixed = TRUE)     # the labels CSV's mangling
  x <- gsub("\\u00b0",  DEG, x, fixed = TRUE)     # the backslash-escape form
  x <- gsub("�",   DEG, x, fixed = TRUE)     # a replacement char from a lossy round-trip
  Encoding(x) <- "UTF-8"
  x
}

# =============================================================================
line("1. HOW MANY KEYS ARE AFFECTED, AND DOES THE REPAIR CLOSE THE GAP?")
# =============================================================================
LABraw <- read.csv("final_outcomes/engineered_cmt_century_broad_labels.csv",
                   stringsAsFactors = FALSE) %>% filter(!is.na(Pathway), Pathway != "")
MASraw <- bind_rows(lapply(c("A","C"), function(id)
  read.csv(sprintf("master_outputs/approach_%s/compass_master_dataset_%s.csv", id, id),
           stringsAsFactors = FALSE) %>% mutate(approach = id)))

cat("label rows (classified):", nrow(LABraw),
    "| labels containing the literal escape:",
    sum(grepl("<U+00B0>", LABraw$Scenario, fixed = TRUE)), "\n")

key <- function(d, f) paste(d$approach, f(d$Model), f(d$Scenario))
mk  <- unique(key(MASraw, old_norm))
cat("\nUsing the CURRENT normalisation, labels that find a master row:",
    sum(key(LABraw, old_norm) %in% mk), "of", nrow(LABraw), "\n")
mk2 <- unique(key(MASraw, fix_keys))
cat("Using the REPAIRED keys,           labels that find a master row:",
    sum(key(LABraw, fix_keys) %in% mk2), "of", nrow(LABraw), "\n")

miss <- LABraw[!(key(LABraw, fix_keys) %in% mk2), ]
cat("\nstill unmatched after repair:", nrow(miss), "\n")
if (nrow(miss)) print(head(as.data.frame(miss %>% select(approach, Model, Scenario)), 10))

lost <- LABraw[!(key(LABraw, old_norm) %in% mk) & (key(LABraw, fix_keys) %in% mk2), ]
cat("\nRECOVERED BY THE REPAIR:", nrow(lost), "scenarios. By arm and ambition:\n\n")
print(as.data.frame(lost %>%
  mutate(amb = ifelse(grepl("^1\\.5", Ambition), "1.5C", "2C")) %>%
  count(approach, amb, Pathway)))
cat("\nBy model family:\n")
print(as.data.frame(lost %>% mutate(fam = sub("[ /-].*$","",Model)) %>% count(fam) %>%
      arrange(desc(n))))

# =============================================================================
line("2. REBUILD THE GRID ON REPAIRED KEYS")
# =============================================================================
ABS <- c("jobs_Renewables","jobs_Fossil","cumulative_gap_EJ",
         "mean_headcount_millions","cumulative_deaths_mln")
add_pc <- function(df) df %>% mutate(
  mort_per_1k        = cumulative_deaths_mln   / pop_mln * 1000,
  headcount_pct      = mean_headcount_millions / pop_mln * 100,
  net_re_jobs_per_1k = (jobs_Renewables - jobs_Fossil) / pop_mln,
  gap_GJ_pc          = cumulative_gap_EJ * 1000 / pop_mln)

RO <- MASraw %>%
  mutate(Model = fix_keys(Model), Scenario = fix_keys(Scenario)) %>%
  filter(Region %in% R10) %>%
  distinct(approach, Model, Scenario, Region, .keep_all = TRUE) %>%
  select(approach, Model, Scenario, Region, pop_mln, all_of(ABS))
POP_TOT <- RO %>% distinct(Region, pop_mln) %>% group_by(Region) %>%
  summarise(p = median(pop_mln), .groups="drop") %>% summarise(s = sum(p)) %>% pull(s)

WLD <- RO %>% group_by(approach, Model, Scenario) %>%
  summarise(
    n_regions_jobs      = sum(!is.na(jobs_Renewables) & !is.na(jobs_Fossil)),
    n_regions_gap       = sum(!is.na(cumulative_gap_EJ)),
    n_regions_headcount = sum(!is.na(mean_headcount_millions)),
    n_regions_mortality = sum(!is.na(cumulative_deaths_mln)),
    jobs_Renewables     = if (n_regions_jobs == 10) sum(jobs_Renewables[!is.na(jobs_Renewables) & !is.na(jobs_Fossil)]) else NA_real_,
    jobs_Fossil         = if (n_regions_jobs == 10) sum(jobs_Fossil[!is.na(jobs_Renewables) & !is.na(jobs_Fossil)])     else NA_real_,
    cumulative_gap_EJ       = if (n_regions_gap       == 10) sum(cumulative_gap_EJ, na.rm=TRUE)       else NA_real_,
    mean_headcount_millions = if (n_regions_headcount == 10) sum(mean_headcount_millions, na.rm=TRUE) else NA_real_,
    cumulative_deaths_mln   = if (n_regions_mortality == 10) sum(cumulative_deaths_mln, na.rm=TRUE)   else NA_real_,
    .groups="drop") %>%
  mutate(Region = WORLD, pop_mln = POP_TOT,
         world_complete_jobs = n_regions_jobs == 10,
         world_complete_gap  = n_regions_gap  == 10,
         world_complete_mortality = n_regions_mortality == 10) %>%
  add_pc()

MORT <- read.csv("final_outcomes/mortality_reporting_complete_scenario_values_2020_2100.csv",
                 stringsAsFactors = FALSE) %>%
  mutate(Model = fix_keys(Model), Scenario = fix_keys(Scenario)) %>%
  transmute(approach, Model, Scenario, Region, mort_per_1k = cumulative_pm25_deaths_mln)

JOBDEP <- c("net_re_jobs_per_1k","gap_GJ_pc","headcount_pct")
REG <- RO %>% add_pc() %>% select(approach, Model, Scenario, Region, all_of(JOBDEP))

cell <- function(d, out) {
  sgn <- ifelse(out %in% LOWER, -1, 1)
  a <- d[[out]][d$arm=="cmt"]; ca <- d$clus[d$arm=="cmt"]
  b <- d[[out]][d$arm=="re" ]; cb <- d$clus[d$arm=="re" ]
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

grid_on <- function(LAB, nm) {
  D <- bind_rows(REG, WLD %>% select(approach, Model, Scenario, Region, all_of(JOBDEP))) %>%
    full_join(MORT, by = c("approach","Model","Scenario","Region")) %>%
    inner_join(LAB, by = c("approach","Model","Scenario")) %>%
    mutate(stem = gsub("[-_ ]?[0-9]+(\\.[0-9]+)?[a-z]?$", "", Scenario),
           stem = sub("/.*$", "", stem), clus = paste(Model, stem))
  expand_grid(approach=c("A","C"), Region=ALLR, amb=c("1.5C","2C"), outcome=names(OUTS)) %>%
    pmap_dfr(function(approach, Region, amb, outcome) {
      d <- D[D$approach==approach & D$Region==Region & D$amb==amb, ]
      bind_cols(tibble(keys=nm, approach, Region, amb, outcome), cell(d, outcome))
    }) %>%
    mutate(family = OUTS[outcome], sig = !is.na(lo) & (lo>0 | hi<0),
           reg = SH[Region], primary = outcome != "headcount_pct")
}

mklab <- function(f) LABraw %>%
  transmute(approach, Model = f(Model), Scenario = f(Scenario),
            arm = ifelse(Pathway == "High-RE", "re", "cmt"),
            amb = ifelse(grepl("^1\\.5", Ambition), "1.5C", "2C"))
# The broken side must also read the master through the broken normalisation,
# otherwise it is not a like-for-like reproduction of the defect.
RO_old  <- MASraw %>% mutate(Model=old_norm(Model), Scenario=old_norm(Scenario)) %>%
  filter(Region %in% R10) %>% distinct(approach, Model, Scenario, Region, .keep_all=TRUE)

GFIX <- grid_on(mklab(fix_keys), "repaired")
saveRDS(list(grid = GFIX, world = WLD, labels = mklab(fix_keys),
             recovered = lost), "KEYS_REPAIRED.rds")
cat("written: KEYS_REPAIRED.rds\n")

# =============================================================================
line("3. WHAT MOVED — WORLD")
# =============================================================================
OLD <- readRDS("STRICT_WORLD.rds")$grid %>%
  filter(approach=="A", Region==WORLD, primary) %>%
  select(family, amb, o_ncmt=n_cmt, o_nre=n_re, o_cmt=raw_cmt, o_re=raw_re,
         o_gap=gap, o_lo=lo, o_hi=hi, o_sig=sig)
NEW <- GFIX %>% filter(approach=="A", Region==WORLD, primary) %>%
  select(family, amb, n_cmt, n_re, raw_cmt, raw_re, gap, lo, hi, sig)
print(as.data.frame(OLD %>% inner_join(NEW, by=c("family","amb")) %>%
  transmute(family, amb,
            n_before = paste0(o_ncmt,"v",o_nre), n_after = paste0(n_cmt,"v",n_re),
            gap_before = round(o_gap,2), gap_after = round(gap,2),
            CI_after = sprintf("[%+.2f,%+.2f]", lo, hi),
            sig_before = ifelse(o_sig,"YES","no"), sig_after = ifelse(sig,"YES","no")) %>%
  arrange(factor(family, levels=c("Jobs","Deprivation","Health")), amb)))

# =============================================================================
line("4. WHAT MOVED — SCORECARD, AND DID ANY CELL FLIP?")
# =============================================================================
scr <- function(g, nm) g %>% filter(approach=="A", primary, Region!="R10PAC_OECD", !is.na(gap)) %>%
  summarise(keys=nm, cells=n(), favour_RE=sum(gap>0), sig_for=sum(gap>0 & sig),
            sig_against=sum(gap<0 & sig))
print(as.data.frame(bind_rows(scr(readRDS("STRICT_WORLD.rds")$grid, "as published"),
                              scr(GFIX, "repaired"))))

cmp <- readRDS("STRICT_WORLD.rds")$grid %>%
  filter(approach=="A", primary, Region!="R10PAC_OECD") %>%
  select(reg, amb, family, b_gap=gap, b_sig=sig) %>%
  inner_join(GFIX %>% filter(approach=="A", primary, Region!="R10PAC_OECD") %>%
               select(reg, amb, family, a_gap=gap, a_sig=sig),
             by=c("reg","amb","family"))
fl <- cmp %>% filter(!is.na(b_gap), !is.na(a_gap), sign(b_gap) != sign(a_gap))
cat("\ncells changing SIGN:", nrow(fl), "\n")
if (nrow(fl)) print(as.data.frame(fl %>% mutate(across(where(is.numeric), ~round(.,2)))))
sg <- cmp %>% filter(!is.na(b_gap), !is.na(a_gap), b_sig != a_sig)
cat("\ncells changing SIGNIFICANCE:", nrow(sg), "\n")
if (nrow(sg)) print(as.data.frame(sg %>% mutate(across(where(is.numeric), ~round(.,2)))))
