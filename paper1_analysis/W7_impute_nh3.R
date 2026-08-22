# =============================================================================
# W7 — WHAT WOULD PUTTING AMMONIA *BACK* DO?
#
# "Add ammonia back" is ambiguous, and the ambiguity matters, because the two
# things it can mean move the result in OPPOSITE directions:
#
#   (A) REVERT — restore each model's ammonia AS REPORTED.
#       High-CMT regains ~8% of its mortality; High-RE regains ~0.4%, because
#       REMIND never reported any. High-RE therefore looks BETTER. This is the
#       uncorrected published number, and it is the artefact.
#
#   (B) IMPUTE — give REMIND the agricultural ammonia it is missing, so that
#       BOTH arms carry ammonia on a common basis. High-RE gains what it never
#       reported. High-RE therefore looks WORSE.
#
# Only (B) is a correction. (A) is simply not correcting.
#
# THIS SCRIPT QUANTIFIES (B), because the paper claims the harmonised null is an
# UPPER BOUND on the High-RE advantage and that claim should carry a number.
#
# THE ARITHMETIC. Let M0 be ammonia-free mortality (what the harmonised run
# produced) and f the fraction of total PM2.5 mortality that ammonia
# contributes when a model reports it properly. Then total = M0 / (1 - f).
#
#   If f is the SAME in both arms, imputation multiplies both arms by the same
#   constant. The gap scales but its SIGN cannot change -- so a null stays a
#   null. Deletion and imputation agree.
#
#   Imputation only differs from deletion if f genuinely DIFFERS between the
#   arms for pathway reasons -- e.g. if BECCS-heavy High-CMT scenarios carried
#   more fertiliser ammonia.
#
# SO THE WHOLE QUESTION REDUCES TO: is f higher in High-CMT or in High-RE?
#
# The only within-model evidence available is the probe: MESSAGEix is the one
# family holding both arms while reporting ammonia. Its eight High-CMT runs sit
# at f = 5.21-6.55% (median 6.37%). Its single High-RE run sits at f = 10.12% --
# HIGHER than every one of them. n=1 on that side, so this is a bound rather
# than an estimate, but it is the only evidence there is and it points against
# the BECCS-fertiliser story rather than for it.
#
# USAGE: Rscript W7_impute_nh3.R
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

DROP <- "R10PAC_OECD"
C <- readRDS("NH3_MORT_REBUILD.rds")          # med_*_w = as reported, med_*_n = ammonia removed
A <- readRDS("ARM.rds") %>% mutate(fam = sub("[ /-].*$", "", model))

line("1. THE AMMONIA FRACTION, WHERE A MODEL REPORTS IT AND HOLDS BOTH ARMS")
sc <- A %>% group_by(fam, model, scenario, arm) %>%
  summarise(w = sum(with_nh3), z = sum(zero_nh3), .groups = "drop") %>%
  mutate(f = (w - z)/w)
print(as.data.frame(sc %>% group_by(fam, arm) %>%
      summarise(n = n(), median_f = round(100*median(f), 2), .groups = "drop") %>%
      pivot_wider(names_from = arm, values_from = c(n, median_f), values_fill = 0)))
mess <- sc %>% filter(fam == "MESSAGEix")
F_CMT <- median(mess$f[mess$arm == "High-CMT"])
F_RE  <- median(mess$f[mess$arm == "High-RE"])
cat(sprintf("\nMESSAGEix, the only family with both arms reporting ammonia:\n"))
cat(sprintf("  High-CMT f = %.2f%%  (n = %d)\n", 100*F_CMT, sum(mess$arm=="High-CMT")))
cat(sprintf("  High-RE  f = %.2f%%  (n = %d)\n", 100*F_RE,  sum(mess$arm=="High-RE")))
if (F_RE > F_CMT)
  cat("\n  High-RE's ammonia fraction is HIGHER. Imputing the missing ammonia\n",
      "  therefore penalises High-RE more than High-CMT.\n", sep="")

line("2. THREE VERSIONS OF THE SAME COMPARISON")
# (A) reverted / as reported            -> med_cmt_w, med_re_w
# (B) harmonised / ammonia deleted      -> med_cmt_n, med_re_n   [published]
# (C) imputed: both arms carry ammonia at the within-model observed fractions
imp <- C %>% filter(shown) %>%
  transmute(Region, amb,
            reg = ifelse(Region=="Aggregated R10 regions","WORLD", sub("^R10","",Region)),
            rev_cmt = med_cmt_w, rev_re = med_re_w,
            har_cmt = med_cmt_n, har_re = med_re_n,
            imp_cmt = med_cmt_n/(1 - F_CMT),
            imp_re  = med_re_n /(1 - F_RE),
            gap_rev = rev_cmt - rev_re,      # + = High-RE has fewer deaths
            gap_har = har_cmt - har_re,
            gap_imp = imp_cmt - imp_re)
print(imp %>% mutate(across(where(is.numeric), ~round(.,2))) %>%
      select(reg, amb, gap_rev, gap_har, gap_imp) %>% as.data.frame())

cat("\ngap is deaths per 1,000 AVOIDED by High-RE. Positive favours High-RE.\n")
cat("\nsummary across the 20 shown cells:\n")
print(data.frame(
  version = c("(A) reverted - ammonia as reported",
              "(B) harmonised - ammonia deleted  [published]",
              "(C) imputed - both arms carry ammonia"),
  cells_favouring_RE = c(sum(imp$gap_rev>0), sum(imp$gap_har>0), sum(imp$gap_imp>0)),
  median_gap = round(c(median(imp$gap_rev), median(imp$gap_har), median(imp$gap_imp)), 2)))

line("3. WORLD, THE THREE VERSIONS SIDE BY SIDE")
print(imp %>% filter(reg=="WORLD") %>%
      transmute(amb,
                `A reverted`  = sprintf("%.2f v %.2f  ->  %+.2f", rev_cmt, rev_re, gap_rev),
                `B harmonised`= sprintf("%.2f v %.2f  ->  %+.2f", har_cmt, har_re, gap_har),
                `C imputed`   = sprintf("%.2f v %.2f  ->  %+.2f", imp_cmt, imp_re, gap_imp)) %>%
      as.data.frame())

line("THE POINT")
cat("Reverting to the reported data hands High-RE a ", sprintf("%.2f", imp$gap_rev[imp$reg=="WORLD" & imp$amb=="1.5C"]),
    " deaths-per-1,000 advantage.\n", sep="")
cat("Imputing the ammonia REMIND never filed hands it a ",
    sprintf("%.2f", -imp$gap_imp[imp$reg=="WORLD" & imp$amb=="1.5C"]),
    " deaths-per-1,000 DISADVANTAGE.\n", sep="")
cat("The published harmonised number sits between them at ",
    sprintf("%+.2f", imp$gap_har[imp$reg=="WORLD" & imp$amb=="1.5C"]), ".\n\n", sep="")
cat("So the harmonised null is not a pessimistic reading -- it is the middle of\n")
cat("the defensible range, and the only correction that does not require an\n")
cat("external ammonia dataset. Version (C) rests on n=1 for the High-RE\n")
cat("fraction and is reported as a bound, not as a result.\n")

saveRDS(imp, "W7_IMPUTE.rds")
cat("\nwritten: W7_IMPUTE.rds\n")
