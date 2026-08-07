# =============================================================================
# within_model_family_check.R  —  RUN LOCALLY
#
# RESOLUTION of the North America / Africa PM2.5 mortality anomaly.
#
# Prior diagnostics (mortality_regional_diagnostic.R, na_mortality_mechanism.R,
# na_mortality_mechanism2.R) refuted biomass capacity as the mechanism and
# surfaced a severe MODEL-COMPOSITION CONFOUND (na_mechanism2_model_composition.csv):
# in approach A at 2C, REMIND-family models (REMIND + REMIND-MAgPIE +
# REMIND-Transport, 69% of the High-RE sample) have ZERO High-CDR scenarios,
# and POLES-JRC + GEM-E3 (29% of High-CDR) have ZERO High-RE scenarios. Only
# AIM, MESSAGEix-GLOBIOM and WITCH have >=10 scenarios on BOTH sides.
#
# THIS SCRIPT tests whether the "High-RE worse" mortality result in North
# America and Africa survives comparing High-CDR vs High-RE WITHIN a single
# model family (removing the between-model confound), for the three usable
# families.
#
# RESULT (already run once on the uploaded data): the anomaly did NOT survive.
# Within every one of the 6 family x region combinations, High-CDR is worse or
# not significantly different -- never significantly High-RE-worse. The pooled
# "High-RE worse" result is a Simpson's-paradox artifact of model composition,
# not a real technology effect: REMIND's baseline NA/Africa pollutant
# assumptions likely differ from IMAGE/POLES-JRC/GEM-E3's, independent of any
# CDR-vs-RE technology story, and pooling attributes that model-level
# difference to the pathway label.
#
# OUTPUT: within_model_family_summary.csv (small -- attach or paste back)
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr)
})

MASTER_OUT_DIR <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_master"
DS_A <- file.path(MASTER_OUT_DIR, "approach_A", "compass_master_dataset_A.csv")
PW_A <- file.path(MASTER_OUT_DIR, "approach_A", "compass_pathway_tercile_A.csv")
stopifnot(file.exists(DS_A), file.exists(PW_A))

r10 <- c("R10AFRICA","R10CHINA+","R10EUROPE","R10INDIA+","R10NORTH_AM")
cliffs <- function(x,y){x<-x[!is.na(x)];y<-y[!is.na(y)];if(!length(x)||!length(y))return(NA_real_)
  (sum(outer(x,y,">"))-sum(outer(x,y,"<")))/(length(x)*length(y))}
mwp <- function(x,y){x<-x[!is.na(x)];y<-y[!is.na(y)]
  if(length(x)>1&&length(y)>1) suppressWarnings(wilcox.test(x,y)$p.value) else NA_real_}
famof <- function(m) sub("[ /].*", "", m)

pw <- read.csv(PW_A, stringsAsFactors=FALSE) %>%
  select(Model, Scenario, Ambition, Pathway_excl) %>%
  filter(Pathway_excl %in% c("High-CDR","High-RE"))

# ---- 0. which families are actually usable (>=10 scenarios on EACH side)? --
comp <- pw %>% filter(grepl("2C", Ambition)) %>% mutate(fam = famof(Model)) %>%
  count(Pathway_excl, fam) %>%
  pivot_wider(names_from = Pathway_excl, values_from = n, values_fill = 0)
cat("=== model-family composition (2C, approach A) ===\n")
print(as.data.frame(comp %>% mutate(both_sides = `High-CDR`>=10 & `High-RE`>=10) %>%
  arrange(desc(`High-CDR`+`High-RE`))), row.names=FALSE)
usable_fams <- comp %>% filter(`High-CDR`>=10, `High-RE`>=10) %>% pull(fam)
cat("\nusable for a within-model test (>=10 both sides):", paste(usable_fams, collapse=", "), "\n")

# ---- 1. within-model mortality comparison, all R10 regions ------------------
d <- read.csv(DS_A, check.names=FALSE) %>% filter(Region %in% r10) %>%
  distinct(Model, Scenario, Ambition, Region, mort_per_1k)
j <- inner_join(d, pw, by = c("Model","Scenario","Ambition")) %>%
  mutate(fam = famof(Model)) %>% filter(fam %in% usable_fams, grepl("2C", Ambition))

cat("\n=== WITHIN-MODEL-FAMILY mortality: High-CDR vs High-RE, all R10 regions (2C) ===\n")
out <- list()
for (rg in r10) for (fam_ in usable_fams) {
  s <- j %>% filter(Region == rg, fam == fam_)
  hc <- s$mort_per_1k[s$Pathway_excl == "High-CDR"]; hr <- s$mort_per_1k[s$Pathway_excl == "High-RE"]
  if (length(hc) < 3 || length(hr) < 3) next
  out[[length(out)+1]] <- data.frame(
    region = sub("R10","",rg), family = fam_, n_CDR = length(hc), n_RE = length(hr),
    CDR_mean = round(mean(hc,na.rm=TRUE),1), RE_mean = round(mean(hr,na.rm=TRUE),1),
    higher = ifelse(mean(hc,na.rm=TRUE) > mean(hr,na.rm=TRUE), "High-CDR", "High-RE"),
    pct = round(100*(mean(hc,na.rm=TRUE)-mean(hr,na.rm=TRUE))/mean(hr,na.rm=TRUE)),
    cliff = round(cliffs(hc,hr),3), p = round(mwp(hc,hr),4))
}
out <- bind_rows(out)
print(as.data.frame(out), row.names=FALSE)
cat("\nREAD: compare 'higher' here to the POOLED (across-model) direction for the\n",
    "  same region. If pooled says High-RE worse but every within-model row here\n",
    "  says High-CDR worse (or ns), the pooled result is a model-composition\n",
    "  (Simpson's paradox) artifact, not a real regional technology effect.\n", sep="")

OUT <- file.path(MASTER_OUT_DIR, "within_model_family_summary.csv")
ok <- tryCatch({ write_csv(out, OUT); TRUE },
               error=function(e){write_csv(out,"within_model_family_summary.csv"); FALSE})
if (!ok) OUT <- file.path(getwd(), "within_model_family_summary.csv")
cat("\nWROTE:", normalizePath(OUT, winslash="/", mustWork=FALSE), "\n")
