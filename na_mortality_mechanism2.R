# =============================================================================
# na_mortality_mechanism2.R  —  RUN LOCALLY
#
# HYPOTHESIS 1 (biomass ELECTRICITY capacity) IS REFUTED: na_mortality_mechanism.R
#   showed High-RE has 50-92% LESS biomass capacity than High-CDR, in every
#   region and ambition (High-CDR runs BECCS). Where biomass correlates
#   positively with BC in North America, the direction is backwards: less
#   biomass (High-RE) + positive correlation would predict LESS BC in High-RE,
#   but High-RE actually has MORE BC and OC there. Biomass power capacity is
#   not the mechanism.
#
# TWO NEXT HYPOTHESES:
#   H1' bioenergy shows up as TOTAL PRIMARY ENERGY (industrial heat, transport
#       biofuels), not power capacity -- High-RE may shift biomass use OUT of
#       electricity (where it competes with cheap solar/wind) and INTO
#       harder-to-electrify sectors, still producing BC/OC.
#   H2  MODEL-COMPOSITION CONFOUND -- the High-CDR/High-RE split in North
#       America might draw disproportionately on different model families,
#       which could have different baseline pollutant assumptions unrelated
#       to the technology story at all.
#
# OUTPUT: na_mechanism2_summary.csv (small -- attach or paste back)
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
})

COMPASS_DIR    <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/COMPASS"
MASTER_OUT_DIR <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_master"
INTERP    <- file.path(COMPASS_DIR, "compass_interp.rds")
EM_CSV    <- file.path(COMPASS_DIR, "compass_emissions_raw.csv")
PATHWAY_A <- file.path(MASTER_OUT_DIR, "approach_A", "compass_pathway_tercile_A.csv")
stopifnot(file.exists(INTERP), file.exists(EM_CSV), file.exists(PATHWAY_A))

regions_r10 <- c("R10AFRICA","R10CHINA+","R10EUROPE","R10INDIA+","R10NORTH_AM")
WIN <- c(`1.5C (High-Ambition)` = 2100L, `2C (Medium-Ambition)` = 2100L)  # matches master
norm_names <- function(d) {
  n <- names(d); n[tolower(n)=="model"]<-"Model"; n[tolower(n)=="scenario"]<-"Scenario"
  n[tolower(n)=="year"]<-"Year"; n[tolower(n)=="region"]<-"Region"; names(d)<-n; d
}
cliffs <- function(x,y){x<-x[!is.na(x)];y<-y[!is.na(y)];if(!length(x)||!length(y))return(NA_real_)
  (sum(outer(x,y,">"))-sum(outer(x,y,"<")))/(length(x)*length(y))}
mwp <- function(x,y){x<-x[!is.na(x)];y<-y[!is.na(y)]
  if(length(x)>1&&length(y)>1) suppressWarnings(wilcox.test(x,y)$p.value) else NA_real_}

pw <- read.csv(PATHWAY_A, stringsAsFactors=FALSE) %>% norm_names() %>%
  select(Model, Scenario, Ambition, Pathway_excl) %>%
  filter(Pathway_excl %in% c("High-CDR","High-RE"))

ci <- readRDS(INTERP)

# ---- H1': total bioenergy PRIMARY ENERGY (all sectors, not just power) ------
be_vars <- c("Primary Energy|Biomass", "Final Energy|Solids|Biomass",
            "Final Energy|Industry|Biomass", "Final Energy|Transportation|Biomass",
            "Final Energy|Residential and Commercial|Biomass")
present <- ci %>% filter(Variable %in% be_vars) %>% distinct(Variable) %>% pull(Variable)
cat("bioenergy variables found in interp:", paste(present, collapse=", "), "\n")

if (length(present) > 0) {
  be <- ci %>% filter(Region %in% regions_r10, Variable %in% present,
                      Year >= 2020, !is.na(Value)) %>%
    inner_join(pw, by = c("Model","Scenario")) %>%
    mutate(window_end = WIN[Ambition]) %>%
    filter(!is.na(window_end), Year <= window_end) %>%
    group_by(Model, Scenario, Ambition, Pathway_excl, Region, Variable) %>%
    summarise(cum_val = sum(Value, na.rm=TRUE), .groups="drop")

  cat("\n=== H1': total bioenergy PRIMARY/FINAL ENERGY, High-CDR vs High-RE ===\n")
  res_be <- list()
  for (rg in regions_r10) for (am in names(WIN)) for (v in present) {
    s <- be %>% filter(Region==rg, Ambition==am, Variable==v)
    hc <- s$cum_val[s$Pathway_excl=="High-CDR"]; hr <- s$cum_val[s$Pathway_excl=="High-RE"]
    if (length(hc)<3 || length(hr)<3) next
    res_be[[length(res_be)+1]] <- data.frame(region=sub("R10","",rg),
      ambition=if(grepl("1.5",am))"1.5C" else "2C", variable=v,
      CDR_mean=mean(hc,na.rm=TRUE), RE_mean=mean(hr,na.rm=TRUE),
      pct_RE_vs_CDR=round(100*(mean(hr,na.rm=TRUE)-mean(hc,na.rm=TRUE))/mean(hc,na.rm=TRUE)),
      cliff_REvsCDR=round(cliffs(hr,hc),3), p=mwp(hc,hr))
  }
  res_be <- bind_rows(res_be)
  print(as.data.frame(res_be %>% mutate(across(c(CDR_mean,RE_mean),~signif(.x,3)),
    sig=ifelse(!is.na(p)&p<.05,"*",""))), row.names=FALSE)
  cat("(+pct/cliff>0 = High-RE uses MORE total bioenergy than High-CDR)\n")
} else {
  cat("none of the candidate bioenergy variables exist in interp -- skip H1', see full Variable list below\n")
  cat(paste(sort(unique(ci$Variable[grepl("Biomass|Bioenergy", ci$Variable, ignore.case=TRUE)])), collapse="\n"), "\n")
  res_be <- tibble()
}

# ---- H2: model-family composition of High-CDR vs High-RE in NA/Africa -------
famof <- function(m) sub("[ /].*", "", m)
comp <- pw %>% filter(grepl("2C", Ambition)) %>%   # composition is region-agnostic in pw; report once
  mutate(fam = famof(Model)) %>%
  count(Pathway_excl, fam) %>%
  group_by(Pathway_excl) %>% mutate(pct = round(100*n/sum(n))) %>% ungroup()

cat("\n=== H2: model-family composition, High-CDR vs High-RE (2C, approach A) ===\n")
print(as.data.frame(comp %>% pivot_wider(names_from=Pathway_excl, values_from=c(n,pct),
  values_fill=0) %>% arrange(desc(`n_High-CDR` + `n_High-RE`))), row.names=FALSE)
cat("\nREAD: if one pathway draws heavily on a model family the other barely uses,\n",
    "  region-specific pollutant differences could reflect that model's baseline\n",
    "  emissions assumptions rather than the CDR-vs-RE technology choice itself.\n", sep="")

OUT1 <- file.path(MASTER_OUT_DIR, "na_mechanism2_bioenergy.csv")
OUT2 <- file.path(MASTER_OUT_DIR, "na_mechanism2_model_composition.csv")
ok1 <- tryCatch({ write_csv(res_be, OUT1); TRUE },
                error=function(e){write_csv(res_be,"na_mechanism2_bioenergy.csv"); FALSE})
ok2 <- tryCatch({ write_csv(comp, OUT2); TRUE },
                error=function(e){write_csv(comp,"na_mechanism2_model_composition.csv"); FALSE})
if (!ok1) OUT1 <- file.path(getwd(), "na_mechanism2_bioenergy.csv")
if (!ok2) OUT2 <- file.path(getwd(), "na_mechanism2_model_composition.csv")
cat("\nWROTE:", normalizePath(OUT1, winslash="/", mustWork=FALSE), "\n")
cat("WROTE:", normalizePath(OUT2, winslash="/", mustWork=FALSE), "\n")
