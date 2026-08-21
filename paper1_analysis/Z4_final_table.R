# =============================================================================
# Z4 — THE FINAL TABLE.
#
# Two changes from Z1, both decided rather than discovered:
#
# 1. PACIFIC OECD IS DROPPED FROM THE BY-REGION RESULTS. Z3 showed the global
#    label does not describe it: Cliff's delta on that region's own renewable
#    deployment between the two arms is -0.19 (1.5C) and -0.07 (2C), i.e.
#    High-RE builds NO MORE renewables in Pacific OECD than High-CMT does.
#    Scoring a "renewables vs carbon management" contrast in a region where the
#    two arms deploy the same renewables is not a weak result, it is not the
#    comparison we claim to be making. It stays inside the World aggregate,
#    which is a ten-region sum, and comes out of the regional display.
#
#    Reforming economies is weak but real (+0.26, +0.45) and is KEPT, flagged.
#
# 2. Everything is reported on nine regions plus World.
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

DROP_REGION <- "R10PAC_OECD"
FIN <- readRDS("FINAL_RESULTS.rds")
FAM <- c(REFOSS="Jobs", LOWC="Jobs", gap_GJ_pc="Deprivation",
         headcount_pct="Deprivation", mort_per_1k="Health")
PRIMARY <- c("REFOSS","gap_GJ_pc","mort_per_1k")

P <- FIN %>% mutate(family = FAM[outcome]) %>%
  filter(outcome %in% PRIMARY, !is.na(adv))
keep <- P %>% filter(Region != DROP_REGION)

line("HEADLINE — nine regions plus World, primary measure per family")
h <- keep %>% filter(approach == "A full database", sample == "all scenarios")
cat("cells:", nrow(h), "| favour High-RE:", sum(h$adv > 0),
    sprintf(" (%.0f%%)", 100*mean(h$adv > 0)),
    "| significant for High-RE:", sum(h$adv > 0 & h$sig),
    "| significant against:", sum(h$adv < 0 & h$sig), "\n\n")
print(h %>% group_by(family) %>%
      summarise(cells = n(), RE = sum(adv > 0),
                sig_RE = sum(adv > 0 & sig), sig_CMT = sum(adv < 0 & sig),
                med_adv = round(median(adv), 2), med_pct = round(median(pct)),
                .groups = "drop") %>% as.data.frame())

cat("\nEFFECT OF DROPPING PACIFIC OECD:\n")
print(bind_rows(
  P %>% filter(approach=="A full database", sample=="all scenarios") %>%
    group_by(family) %>% summarise(v = paste0(sum(adv>0),"/",n()), .groups="drop") %>%
    mutate(set = "all 11 regions"),
  h %>% group_by(family) %>% summarise(v = paste0(sum(adv>0),"/",n()), .groups="drop") %>%
    mutate(set = "10 shown (Pac OECD out)")) %>%
  pivot_wider(names_from = set, values_from = v) %>% as.data.frame())

line("THE FULL GRID — what goes in the paper")
g <- h %>% mutate(reg = sub("^R10","", Region),
                  reg = ifelse(Region=="Aggregated R10 regions","WORLD",reg),
                  cell = sprintf("%+.2f%s", adv, ifelse(sig,"*","")))
print(g %>% select(reg, amb, family, cell) %>%
      pivot_wider(names_from = c(family, amb), values_from = cell) %>%
      as.data.frame())
cat("\n* = clears a cluster-robust 95% interval\n")

line("ROBUSTNESS OF THE HEADLINE ACROSS ALL FOUR SAMPLES")
print(keep %>% group_by(approach, sample) %>%
      summarise(cells = n(), RE = sum(adv > 0), pct = round(100*mean(adv > 0)),
                sig_RE = sum(adv > 0 & sig), sig_CMT = sum(adv < 0 & sig),
                .groups = "drop") %>% as.data.frame())

line("BY FAMILY, ACROSS SAMPLES")
print(keep %>% group_by(family, approach, sample) %>%
      summarise(v = paste0(sum(adv > 0),"/",n()), .groups="drop") %>%
      pivot_wider(names_from = c(approach, sample), values_from = v) %>%
      as.data.frame())

line("WHAT STILL FAVOURS High-CMT, on the nine kept regions plus World")
print(h %>% filter(adv < 0) %>%
      mutate(reg = sub("^R10","", Region), across(where(is.numeric), ~round(.,2))) %>%
      select(reg, amb, family, med_cmt, med_re, pct, adv, lo, hi, sig) %>%
      arrange(family, reg) %>% as.data.frame())
saveRDS(keep, "Z4_FINAL.rds")
