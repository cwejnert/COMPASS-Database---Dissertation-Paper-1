# =============================================================================
# NH3 ARM ANALYSIS — reading nh3_arm_test_result.rds.
#
# The arm test came back with the OPPOSITE of the predicted result: removing
# NH3 cuts High-CMT mortality by ~8.7% and High-RE by ~0.15% (Wilcoxon
# p = 7.7e-08, Cliff's delta between the two %-change distributions = 0.912).
# So NH3 is NOT a uniform level shift and it DOES move the contrast.
#
# This script asks the only question that then matters: is that a property of
# the PATHWAYS, or of the MODELS that happen to populate them?
# =============================================================================
suppressPackageStartupMessages({library(dplyr);library(tidyr)}); options(width=175)
line <- function(s) cat("\n", strrep("=",76), "\n", s, "\n", strrep("=",76), "\n", sep="")
d  <- readRDS("ARM.rds") %>% mutate(fam = sub("[ /].*$","",model))
sc <- d %>% group_by(model, scenario, arm, fam) %>%
  summarise(with_nh3 = sum(with_nh3), zero_nh3 = sum(zero_nh3), .groups="drop") %>%
  mutate(pct = 100*(zero_nh3 - with_nh3)/with_nh3)

line("ARM AND MODEL FAMILY ARE NEARLY THE SAME VARIABLE HERE")
print(with(sc, table(fam, arm)))

line("THE ONE FAMILY HOLDING BOTH ARMS")
both <- sc %>% group_by(fam) %>% filter(n_distinct(arm) == 2) %>% ungroup()
print(both %>% group_by(fam, arm) %>%
      summarise(n = n(), median_pct = round(median(pct),2),
                min = round(min(pct),2), max = round(max(pct),2), .groups="drop") %>%
      as.data.frame())
cat("\nOnly ONE High-RE scenario exists inside a family that also holds High-CMT,\n")
cat("so this is suggestive rather than conclusive. But it points the OPPOSITE way\n")
cat("to the pooled arm pattern: that single High-RE run is MORE ammonia-sensitive\n")
cat("(-10.1%) than its High-CMT siblings (-6.4%), not less. If the arm gap were a\n")
cat("pathway property it should reproduce inside a model. It does not.\n")

line("HOW MUCH OF EACH FAMILY'S PM2.5 MORTALITY IS AMMONIA-DRIVEN?")
print(sc %>% group_by(fam) %>%
      summarise(n = n(), nh3_share_pct = round(-median(pct),2), .groups="drop") %>%
      arrange(desc(nh3_share_pct)) %>% as.data.frame())
r  <- -median(sc$pct[grepl("^REMIND", sc$fam)])
nr <- -median(sc$pct[!grepl("^REMIND", sc$fam)])
cat(sprintf("\nREMIND family: %.2f%%   |   everyone else: %.2f%%   |   ratio %.0fx\n",
            r, nr, nr/r))
cat("\nAmmonium nitrate and sulfate are typically 20-50% of PM2.5 mass in\n")
cat("industrialised regions. A model whose NH3 contributes 0.15% is not\n")
cat("describing a cleaner world -- it is not reporting agricultural ammonia.\n")

line("WHAT THIS DOES TO THE PUBLISHED MORTALITY CELLS")
cliff <- function(a, b) { a<-a[!is.na(a)]; b<-b[!is.na(b)]
  if (length(a)<3 || length(b)<3) return(NA_real_)
  r <- rank(c(a,b)); n1<-length(a); n2<-length(b)
  2*((sum(r[(n1+1):(n1+n2)]) - n2*(n2+1)/2)/(n1*n2)) - 1 }
adv <- d %>% group_by(r10_region) %>%
  summarise(adv_with = -cliff(with_nh3[arm=="High-CMT"], with_nh3[arm=="High-RE"]),
            adv_zero = -cliff(zero_nh3[arm=="High-CMT"], zero_nh3[arm=="High-RE"]),
            .groups="drop") %>%
  mutate(shift = adv_zero - adv_with,
         flips = sign(adv_with) != sign(adv_zero))
print(adv %>% mutate(across(where(is.numeric), ~round(.,3))) %>% as.data.frame())
cat("\nHigh-RE wins  -- as published:", sum(adv$adv_with>0), "of 10",
    "|  NH3 removed from everyone:", sum(adv$adv_zero>0), "of 10\n")
cat("sign changes:", sum(adv$flips), "| largest shift:",
    round(max(abs(adv$shift)),3), "\n")
