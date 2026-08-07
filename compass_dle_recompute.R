# =============================================================================
# compass_dle_recompute.R  —  RUN LOCALLY (needs compass_interp.rds in memory)
# Nothing is uploaded. It prints the answer and writes a tiny results CSV.
#
# What it does:
#   1. classifies High-CDR / High-RE (tercile) for full database (A) and
#      SCI-vetted (C), AR6 ambition  -> prints counts so you can check the
#      vintage against the deck (expect A@2C ~209/227, C@2C ~40/35)
#   2. computes the CORRECTED DLE gap (distributional) + deprivation headcount
#      with DESIRE-recalibrated thresholds and steeper efficiency
#   3. reports the High-CDR vs High-RE contrast (Cliff's delta + Mann-Whitney)
#      -> answers: does the energy-gap reversal survive the fix?
#   4. runs a threshold sensitivity sweep (x0.75 / x1.0 / x1.25 / old table)
#
# EDIT THE TWO PATHS BELOW, then: source("compass_dle_recompute.R")
# =============================================================================
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(readr);library(stringr);library(purrr);library(tibble)})

INTERP <- "compass_interp.rds"       # <-- path to your interp file
META   <- "compass_r10_meta.csv"     # <-- path to compass_r10_meta.csv

regions_r10 <- c("R10AFRICA","R10CHINA+","R10INDIA+","R10EUROPE","R10NORTH_AM")
cats_keep   <- c("C1","C2","C3","C4")
# Single common cumulation window for every ambition, matching the master's
# OUTCOME_WINDOW_END (2100) -- see COMPASS_master_analysis.R for rationale.
WIN <- c(`1.5C`=2100L, `2C`=2100L); START <- 2020L; TOP_FRAC <- 1/3

ci <- readRDS(INTERP)
cat("interp columns:", paste(names(ci), collapse=", "), "\n")
# expects columns: Model, Scenario, Region, Variable, Year, Value, Category
stopifnot(all(c("Model","Scenario","Region","Variable","Year","Value") %in% names(ci)))
if(!"Category" %in% names(ci)) stop("no Category column in interp; add it or map from meta")

# ---------- 1. DEPLOYMENT + CLASSIFICATION --------------------------------
cdr_components <- c("Carbon Removal|Geological Storage|Direct Air Capture",
  "Carbon Capture|Geological Storage|Biomass","Carbon Removal|Enhanced Weathering",
  "Carbon Capture|Energy|Fossil","Carbon Capture|Industrial Processes","Carbon Removal|Land Use")
re_vars <- paste0("Capacity|Electricity|",c("Solar","Wind","Hydro","Nuclear","Biomass","Geothermal"))

cum_deploy <- function(region_set){
  d <- ci %>% filter(Region %in% region_set, Category %in% cats_keep,
                     Year>=2020, Year<=2100, !is.na(Value))
  cdr_direct <- d %>% filter(Variable=="Total CDR", Value>0) %>%
    group_by(Model,Scenario,Category,Region,Year) %>% summarise(v=sum(Value),.groups="drop")
  cdr_comp <- d %>% filter(Variable %in% cdr_components) %>%
    group_by(Model,Scenario,Category,Region,Year) %>% summarise(v=sum(Value,na.rm=TRUE),.groups="drop")
  cdr <- (if(nrow(cdr_direct)>0) cdr_direct else cdr_comp) %>% filter(v>0) %>%
    group_by(Model,Scenario,Category) %>% summarise(cdr=sum(v,na.rm=TRUE),.groups="drop")
  re <- d %>% filter(Variable %in% re_vars, Value>0) %>%
    group_by(Model,Scenario,Category) %>% summarise(re=sum(Value,na.rm=TRUE),.groups="drop")
  full_join(cdr, re, by=c("Model","Scenario","Category"))
}
# prefer World deployment, fall back to summed-R10 where World missing
dep_world <- cum_deploy("World")
dep_r10   <- cum_deploy(regions_r10)
deploy <- full_join(dep_world, dep_r10, by=c("Model","Scenario","Category"), suffix=c(".w",".r")) %>%
  transmute(Model,Scenario,Category,
            cdr=coalesce(cdr.w,cdr.r), re=coalesce(re.w,re.r)) %>%
  filter(!is.na(cdr)|!is.na(re))

# meta: AR6 category (for ambition) + SCI vetting flag (for sample C)
meta <- read.csv(META, stringsAsFactors=FALSE, check.names=FALSE)
names(meta)[tolower(names(meta))=="model"]    <- "Model"
names(meta)[tolower(names(meta))=="scenario"] <- "Scenario"
sci_col <- names(meta)[str_detect(names(meta),"Vetting\\|SCI 2025")][1]
cat("SCI vetting column:", sci_col, "\n")
sci_ok <- meta %>% transmute(Model,Scenario, ok=tolower(trimws(.data[[sci_col]]))=="ok") %>%
  filter(ok) %>% distinct(Model,Scenario)

classify <- function(scen_keep=NULL){
  d <- deploy
  if(!is.null(scen_keep)) d <- d %>% semi_join(scen_keep, by=c("Model","Scenario"))
  d %>% mutate(Amb=case_when(Category%in%c("C1","C2")~"1.5C",Category%in%c("C3","C4")~"2C",TRUE~NA_character_)) %>%
    filter(!is.na(Amb)) %>% group_by(Amb) %>%
    mutate(ct=quantile(cdr,1-TOP_FRAC,na.rm=TRUE), rt=quantile(re,1-TOP_FRAC,na.rm=TRUE),
           high_cdr=cdr>=ct, high_re=re>=rt,
           Pathway=case_when(high_cdr&!high_re~"High-CDR", high_re&!high_cdr~"High-RE", TRUE~NA_character_)) %>%
    ungroup()
}
cls_A <- classify(NULL)      # full database
cls_C <- classify(sci_ok)    # SCI-vetted

cat("\n=== CLASSIFICATION COUNTS (vintage check vs deck: A@2C ~209/227, C@2C ~40/35) ===\n")
count_tab <- function(cls,lbl) cls %>% filter(!is.na(Pathway)) %>% count(Amb,Pathway) %>%
  pivot_wider(names_from=Pathway,values_from=n) %>% mutate(sample=lbl,.before=1)
print(as.data.frame(bind_rows(count_tab(cls_A,"A full DB"), count_tab(cls_C,"C SCI-vetted"))))

# ---------- 2. CORRECTED DLE (distributional gap + headcount) --------------
dle_thr <- tribble(~Region,~res,~tra,~ind,
  "R10AFRICA",5.0,11.0,3.0, "R10CHINA+",6.5,11.5,4.0, "R10EUROPE",9.0,12.0,4.5,
  "R10INDIA+",4.5,10.5,3.0, "R10NORTH_AM",11.0,18.0,5.5) %>%
  mutate(Ttbase=res+tra+ind) %>% select(Region,Ttbase)
gini <- tribble(~Region,~g, "R10AFRICA",0.45,"R10CHINA+",0.38,"R10EUROPE",0.25,
  "R10INDIA+",0.42,"R10NORTH_AM",0.28) %>% mutate(sigma=sqrt(2)*qnorm((g+1)/2))
SEF_RATE <- 0.019; SEF_FLOOR <- 0.5

fe  <- ci %>% filter(Region %in% regions_r10, Category %in% cats_keep, Variable=="Final Energy",
                     Year>=2020,Year<=2100,!is.na(Value)) %>% select(Model,Scenario,Region,Year,Category,fe=Value)
pop <- ci %>% filter(Region %in% regions_r10, Variable=="Population",
                     Year>=2020,Year<=2100,!is.na(Value)) %>% select(Model,Scenario,Region,Year,pop=Value)

# per scenario cumulative gap (GJ/cap, pop-weighted across regions) + headcount (millions)
dle_per_scenario <- function(thr_mult=1){
  fe %>% left_join(pop,by=c("Model","Scenario","Region","Year")) %>%
    filter(!is.na(pop), pop>0) %>%
    left_join(dle_thr,by="Region") %>% left_join(gini,by="Region") %>%
    mutate(Amb=case_when(Category%in%c("C1","C2")~"1.5C",Category%in%c("C3","C4")~"2C",TRUE~NA_character_)) %>%
    filter(!is.na(Amb), Year<=WIN[Amb]) %>%
    mutate(E=fe*1000/pop, SEF=pmax(SEF_FLOOR,1-SEF_RATE*(Year-2020)), Tt=Ttbase*thr_mult*SEF,
           m=log(pmax(E,0.01))-sigma^2/2, d1=(log(pmax(Tt,0.01))-m)/sigma, d2=d1-sigma,
           gap_pc=pmax(0, Tt*pnorm(d1)-E*pnorm(d2)), hc=pnorm(d1)*pop) %>%
    group_by(Model,Scenario,Amb,Region) %>%
    summarise(cum_gap=sum(gap_pc,na.rm=TRUE), mean_hc=mean(hc,na.rm=TRUE), w=mean(pop,na.rm=TRUE),.groups="drop") %>%
    group_by(Model,Scenario,Amb) %>%
    summarise(gap=weighted.mean(cum_gap,w,na.rm=TRUE), headcount=sum(mean_hc,na.rm=TRUE),.groups="drop")
}

# ---------- 3. CONTRAST --------------------------------------------------
cliffs <- function(x,y){ x<-x[!is.na(x)]; y<-y[!is.na(y)]
  if(length(x)==0||length(y)==0) return(NA_real_); (sum(outer(x,y,">"))-sum(outer(x,y,"<")))/(length(x)*length(y)) }
safep <- function(a,b) if(length(a)>1&&length(b)>1) suppressWarnings(wilcox.test(a,b)$p.value) else NA_real_
contrast <- function(per_scen, cls, lbl){
  per_scen %>% inner_join(cls %>% select(Model,Scenario,Amb,Pathway),by=c("Model","Scenario","Amb")) %>%
    filter(!is.na(Pathway)) %>% group_by(Amb) %>% group_modify(~{
      hc<-.x%>%filter(Pathway=="High-CDR"); hr<-.x%>%filter(Pathway=="High-RE")
      tibble(sample=lbl, n_CDR=nrow(hc), n_RE=nrow(hr),
        gap_delta=cliffs(hc$gap,hr$gap), gap_p=safep(hc$gap,hr$gap),
        hc_delta=cliffs(hc$headcount,hr$headcount), hc_p=safep(hc$headcount,hr$headcount))
    }) %>% ungroup()
}
ps <- dle_per_scenario(1)
main <- bind_rows(contrast(ps, cls_A, "A full DB"), contrast(ps, cls_C, "C SCI-vetted"))
cat("\n=== CORRECTED DLE CONTRAST (High-CDR vs High-RE; delta>0 = High-CDR worse; * p<0.05) ===\n")
main %>% mutate(gap=sprintf("%+.2f%s",gap_delta,ifelse(gap_p<.05,"*","")),
                headcount=sprintf("%+.2f%s",hc_delta,ifelse(hc_p<.05,"*",""))) %>%
  select(sample,Amb,n_CDR,n_RE,gap,headcount) %>% as.data.frame() %>% print(row.names=FALSE)
cat("\n>> Does the gap reversal survive? Look at the 'gap' sign for SCI-vetted (C):",
    "\n   if it is still NEGATIVE at 2C it reverses; if POSITIVE/ns it does not.\n")

# ---------- 4. THRESHOLD SENSITIVITY (full database A) --------------------
sweep <- imap_dfr(c(`x0.75`=0.75,`x1.00`=1.0,`x1.25`=1.25), function(mult,nm)
  contrast(dle_per_scenario(mult), cls_A, paste0("A ",nm)))
cat("\n=== THRESHOLD SENSITIVITY (full database A) ===\n")
sweep %>% mutate(gap=sprintf("%+.2f%s",gap_delta,ifelse(gap_p<.05,"*","")),
                 headcount=sprintf("%+.2f%s",hc_delta,ifelse(hc_p<.05,"*",""))) %>%
  select(sample,Amb,gap,headcount) %>% as.data.frame() %>% print(row.names=FALSE)

out <- bind_rows(main %>% mutate(set="baseline"), sweep %>% mutate(set="sensitivity"))
write_csv(out, "dle_recompute_results.csv")
cat("\nwrote dle_recompute_results.csv  (tiny — attach or paste this)\n")
