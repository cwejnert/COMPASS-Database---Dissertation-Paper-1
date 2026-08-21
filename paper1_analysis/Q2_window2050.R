# =============================================================================
# Q2 — HEADLINE WINDOW = 2020-2050, motivated by net zero by mid-century.
#
# Jobs are rebuilt on the net-zero window from the annual employment file.
# ALL FIVE OUTCOMES are now cut to the same window:
#   jobs        <- jobs_type.rds            (decadal, summed)
#   mortality   <- compass_mortality_r10.csv (decadal, x10 rectangles)
#   deprivation <- compass_dle_annual.rds    (truly annual, summed)
# Each re-cut was verified to reproduce the master exactly at 2020-2100.
#
# Two jobs contrasts only:
#   REFOSS = renewables - fossil
#   LOWC   = renewables + bioenergy + nuclear - fossil
# =============================================================================
source("stratified.R.fns")
options(width = 182)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

OUTS <- c(REFOSS="RE - fossil jobs", LOWC="Low-carbon - fossil jobs",
          gap_GJ_pc="Energy deprivation gap", headcount_pct="Deprivation headcount",
          mort_per_1k="PM2.5 mortality")
JOBS    <- c("REFOSS","LOWC")
LOWER_O <- c("mort_per_1k","gap_GJ_pc","headcount_pct")
ALLR    <- c("Aggregated R10 regions", R10_TEN)

jt  <- readRDS("jobs_type.rds")
grp <- function(tg) case_when(tg=="Fossil"~"Fossil", tg=="Nuclear"~"Nuclear",
                              tg=="Bioenergy"~"Bioenergy", TRUE~"Renewables")

# ---- jobs on a chosen window, regions + a World row ------------------------
jobs_window <- function(y_max) {
  jr <- jt %>% filter(Year>=2020, Year<=y_max, Region %in% R10_TEN) %>%
    mutate(g=grp(tech_group)) %>%
    group_by(Model,Scenario,Region,g) %>%
    summarise(k=sum(jobs_thousands,na.rm=TRUE), .groups="drop")
  comp <- jr %>% distinct(Model,Scenario,Region) %>% count(Model,Scenario) %>%
    filter(n==10) %>% select(Model,Scenario)
  jw <- jr %>% semi_join(comp, by=c("Model","Scenario")) %>%
    group_by(Model,Scenario,g) %>% summarise(k=sum(k), .groups="drop") %>%
    mutate(Region="Aggregated R10 regions")
  bind_rows(jr, jw) %>% pivot_wider(names_from=g, values_from=k, values_fill=0)
}

build <- function(id, y_max) {
  ds <- readRDS(paste0("ds_", id, ".rds")) %>% filter(Variable=="Total CDR")
  pw <- readRDS(paste0("pw_", id, ".rds")) %>% filter(!is.na(Pathway_excl)) %>%
    distinct(Model, Scenario, Pathway = Pathway_excl)
  # ds carries its OWN World row; drop it and rebuild World as the ten-region sum
  # so the population denominator matches the geography the jobs sum covers.
  # Binding both would duplicate every World row and double the sample size.
  popr <- ds %>% distinct(Model,Scenario,Region,pop_mln) %>% filter(Region %in% R10_TEN)
  popw <- popr %>%
    group_by(Model,Scenario) %>% filter(n()==10) %>%
    summarise(pop_mln=sum(pop_mln), .groups="drop") %>%
    mutate(Region="Aggregated R10 regions")
  # EVERY outcome is now cut to the same window: mortality from the annual
  # rfasst file (S1), deprivation from the annual DLE table (S2).
  W <- paste0("2020-", y_max)
  MW <- readRDS("MORT_WINDOWS.rds") %>% filter(window == W) %>%
    select(Model, Scenario, Region, mort_per_1k = mort)
  DW <- readRDS("DLE_WINDOWS.rds") %>% filter(window == W) %>%
    select(Model, Scenario, Region, gap_GJ_pc, headcount_pct)
  other <- load_approach(id, classify=FALSE) %>%
    select(Model, Scenario, Region, Ambition) %>%
    left_join(DW, by=c("Model","Scenario","Region")) %>%
    left_join(MW, by=c("Model","Scenario","Region"))
  jobs_window(y_max) %>%
    inner_join(bind_rows(popr,popw), by=c("Model","Scenario","Region")) %>%
    mutate(REFOSS = 1000*(Renewables-Fossil)/(pop_mln*1000),
           LOWC   = 1000*((Renewables+Bioenergy+Nuclear)-Fossil)/(pop_mln*1000)) %>%
    inner_join(other, by=c("Model","Scenario","Region")) %>%
    inner_join(pw, by=c("Model","Scenario")) %>%
    mutate(Pathway=factor(Pathway, levels=PATHWAYS),
           amb=ifelse(grepl("^1\\.5",Ambition),"1.5C","2C"),
           fam=sub("[ /].*$","",Model))
}

cell <- function(d,out){
  a<-d[[out]][d$Pathway=="High-CMT"]; b<-d[[out]][d$Pathway=="High-RE"]
  a<-a[!is.na(a)];b<-b[!is.na(b)]
  if(length(a)<5||length(b)<5) return(tibble(n_cmt=length(a),n_re=length(b),
    med_cmt=NA_real_,med_re=NA_real_,pct=NA_real_,adv=NA_real_,p=NA_real_))
  sgn <- ifelse(out %in% LOWER_O,-1,1)
  tibble(n_cmt=length(a),n_re=length(b),med_cmt=median(a),med_re=median(b),
         pct=sgn*100*(median(b)-median(a))/abs(median(a)),
         adv=sgn*cliff_d(a,b), p=suppressWarnings(wilcox.test(a,b))$p.value)
}
sweep <- function(d,tag,sm){
  expand_grid(Region=ALLR, amb=c("1.5C","2C"), outcome=names(OUTS)) %>%
    pmap_dfr(function(Region,amb,outcome)
      bind_cols(tibble(approach=tag,sample=sm,Region,amb,outcome),
                cell(d[d$Region==Region & d$amb==amb,], outcome)))
}

A50 <- build("A",2050); C50 <- build("C",2050)
mk <- function(d) d %>% filter(Region %in% R10_TEN) %>% distinct(Model,Scenario,Region) %>%
  count(Model,Scenario) %>% filter(n==10) %>% transmute(k=paste(Model,Scenario))

R <- bind_rows(
  sweep(A50,"A full","all scenarios"),
  sweep(A50 %>% filter(paste(Model,Scenario) %in% mk(A50)$k), "A full","matched only"),
  sweep(C50,"C vetted","all scenarios"),
  sweep(C50 %>% filter(paste(Model,Scenario) %in% mk(C50)$k), "C vetted","matched only")
) %>%
  group_by(approach,sample) %>% mutate(p_fdr=p.adjust(p,"BH")) %>% ungroup() %>%
  mutate(sig=!is.na(p_fdr)&p_fdr<0.05, win=ifelse(is.na(adv),NA,adv>0),
         label=OUTS[outcome], is_world=Region=="Aggregated R10 regions",
         window="2020-2050")
saveRDS(R, "DEEPDIVE.rds"); saveRDS(A50, "MAIN50.rds")

P <- R %>% filter(approach=="A full", sample=="all scenarios")
line("HEADLINE — jobs on the net-zero window")
cat("cells:", sum(!is.na(P$win)), "| High-RE wins:", sum(P$win,na.rm=TRUE),
    sprintf(" (%.0f%%)", 100*mean(P$win,na.rm=TRUE)),
    "| sig for High-RE:", sum(P$win&P$sig,na.rm=TRUE),
    "| for High-CMT:", sum(!P$win&P$sig,na.rm=TRUE), "\n")

cat("\nWORLD:\n")
print(P %>% filter(is_world) %>%
      select(amb,label,window,n_cmt,n_re,med_cmt,med_re,pct,adv,p_fdr,sig) %>%
      mutate(across(where(is.numeric),~round(.,2))) %>% as.data.frame())

cat("\nREGIONAL TALLY:\n")
print(P %>% filter(!is_world,!is.na(win)) %>% group_by(label,window,amb) %>%
      summarise(w=sum(win), of=n(), sig=sum(win&sig), med_pct=round(median(pct)), .groups="drop") %>%
      mutate(cell=paste0(w,"/",of)) %>% select(label,window,amb,cell,sig,med_pct) %>%
      pivot_wider(names_from=amb, values_from=c(cell,sig,med_pct)) %>% as.data.frame())

line("JOBS: 2020-2050 vs the old 2020-2100 framing")
old <- readRDS("WINDOW.rds")$res %>% filter(outcome!="NET", !is.na(adv)) %>%
  mutate(label=recode(outcome, LOWC="Low-carbon - fossil jobs", REFOSS="RE - fossil jobs"))
cat("2020-2100 regional cells won:", sum(old$adv[old$window=="2020-2100"]>0),
    "of", sum(old$window=="2020-2100"), "\n")
cat("2020-2050 regional cells won:", sum(old$adv[old$window=="2020-2050"]>0),
    "of", sum(old$window=="2020-2050"), "\n")
cat("\nmedian % gap by measure and window:\n")
print(old %>% group_by(label, window, amb) %>%
      summarise(med_pct=round(median(pct)), .groups="drop") %>%
      pivot_wider(names_from=window, values_from=med_pct) %>% as.data.frame())

line("EVERY REVERSAL under the net-zero-window headline")
fl <- P %>% filter(!is.na(win), !win) %>%
  select(Region,amb,label,window,adv,pct,sig) %>% arrange(label,Region,amb)
cat(nrow(fl), "of", sum(!is.na(P$win)), "cells favour High-CMT\n\n")
print(fl %>% mutate(across(where(is.numeric),~round(.,2))) %>% as.data.frame())

line("PER-REGION JOBS SCORE (four cells: two measures x two ambitions)")
print(P %>% filter(outcome %in% JOBS, !is.na(win)) %>% group_by(Region) %>%
      summarise(jobs=paste0(sum(win),"/",n()), med_adv=round(median(adv),2),
                med_pct=round(median(pct)), .groups="drop") %>% as.data.frame())
