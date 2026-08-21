# =============================================================================
# T1b — AUDIT SECTION 1 (CORRECTED): THE CLASSIFICATION
#
# T1 reported "100% agreement" for both the component check and the cut
# sensitivity. Both were VACUOUS, for the same reason:
#
#   lab = High-CMT  requires  hi_cmt & !hi_re
#   lab = High-RE   requires  hi_re  & !hi_cmt
#
#   (a) Component check: lab_total and lab_fccs share the SAME hi_re. If both
#       are non-NA, both took the same branch of hi_re, so they cannot differ.
#   (b) Cut check: quantiles are monotone in q. A scenario labelled High-RE at
#       the 2/3 cut is above the 0.60 cut too, so it can never come back
#       High-CMT. Disagreement is impossible by construction.
#
#   The information is entirely in the NA transitions, which the old code
#   filtered out before comparing. So: measure SET MEMBERSHIP, not agreement
#   among survivors -- and then measure what actually matters, whether the
#   RESULT moves.
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

ds <- readRDS("ds_A.rds")
pw <- readRDS("pw_A.rds")

# ---- the deployment table the labels are built from ------------------------
axes <- ds %>%
  filter(Variable %in% c("Total CDR","Renewable Capacity","Fossil CCS",
                         "Land-based CDR","Novel CDR"), Region %in% R10_TEN) %>%
  select(Model,Scenario,Ambition,Region,Variable,Total_Value) %>%
  pivot_wider(names_from=Variable, values_from=Total_Value) %>%
  rename(cmt=`Total CDR`, re=`Renewable Capacity`, land=`Land-based CDR`,
         novel=`Novel CDR`, fccs=`Fossil CCS`) %>%
  filter(!is.na(cmt), !is.na(re)) %>%
  group_by(Model,Scenario,Ambition) %>%
  summarise(across(c(cmt,re,land,novel,fccs), ~sum(.x,na.rm=TRUE)), .groups="drop")

# label under an arbitrary CMT axis and an arbitrary cut
label_by <- function(d, cmt_col, q = 2/3) {
  d %>% group_by(Ambition) %>%
    mutate(hr = re >= quantile(re, q, na.rm=TRUE),
           hc = .data[[cmt_col]] >= quantile(.data[[cmt_col]], q, na.rm=TRUE),
           lab = ifelse(hc & !hr, "High-CMT", ifelse(hr & !hc, "High-RE", NA_character_))) %>%
    ungroup() %>% select(Model,Scenario,Ambition,lab)
}
base <- label_by(axes, "cmt", 2/3)

# churn between two labellings, counting NA moves
churn <- function(new, tag) {
  j <- base %>% rename(old = lab) %>%
    left_join(new %>% rename(new = lab), by=c("Model","Scenario","Ambition")) %>%
    mutate(old = ifelse(is.na(old),"unclassified",old),
           new = ifelse(is.na(new),"unclassified",new))
  moved   <- sum(j$old != j$new)
  flipped <- sum(j$old %in% PATHWAYS & j$new %in% PATHWAYS & j$old != j$new)
  lost    <- sum(j$old %in% PATHWAYS & j$new == "unclassified")
  gained  <- sum(j$old == "unclassified" & j$new %in% PATHWAYS)
  cat(sprintf("  %-24s n_CMT %3d  n_RE %3d | moved %3d (%4.1f%%) | flipped %2d | dropped %3d | added %3d\n",
      tag, sum(j$new=="High-CMT"), sum(j$new=="High-RE"),
      moved, 100*moved/nrow(j), flipped, lost, gained))
  invisible(j)
}

line("Q1. IS THE CARBON-MANAGEMENT AXIS ONE CONSTRUCT?")
comp <- ds %>%
  filter(Variable %in% c("Land-based CDR","Novel CDR","Fossil CCS"), Region %in% R10_TEN) %>%
  select(Model,Scenario,Region,Variable,Total_Value) %>%
  pivot_wider(names_from=Variable, values_from=Total_Value) %>%
  rename(land=`Land-based CDR`, novel=`Novel CDR`, fccs=`Fossil CCS`) %>%
  filter(!is.na(land),!is.na(novel),!is.na(fccs))
cat("Spearman correlation between the three components, within region:\n")
print(comp %>% group_by(Region) %>%
      summarise(land_novel = round(cor(land,novel,method="spearman"),2),
                land_fccs  = round(cor(land,fccs, method="spearman"),2),
                novel_fccs = round(cor(novel,fccs,method="spearman"),2),
                .groups="drop") %>% as.data.frame())

cat("\nShare of the ten-region CMT total contributed by each component:\n")
print(axes %>% mutate(tot = land+novel+fccs) %>% filter(tot>0) %>%
      summarise(land = round(100*median(land/tot)), novel = round(100*median(novel/tot)),
                fccs = round(100*median(fccs/tot))) %>% as.data.frame())

cat("\nMEMBERSHIP CHURN if the CMT axis were ONE component instead of the sum\n")
cat("(baseline: 2/3 cut on Land+Novel+FossilCCS)\n")
churn(base, "baseline (total CMT)")
for (v in c("fccs","land","novel")) churn(label_by(axes, v, 2/3), paste0("axis = ", v, " only"))

line("Q2. WHO GETS EXCLUDED?")
ex <- pw %>% mutate(status = ifelse(is.na(Pathway_excl),
                     ifelse(high_cdr & high_re, "both-high", "neither-high"), Pathway_excl))
print(ex %>% count(Ambition, status) %>%
      pivot_wider(names_from=status, values_from=n, values_fill=0) %>% as.data.frame())
cat("\nby model family:\n")
print(ex %>% mutate(fam=sub("[ /].*$","",Model)) %>% count(fam, status) %>%
      pivot_wider(names_from=status, values_from=n, values_fill=0) %>%
      mutate(total=`both-high`+`neither-high`+`High-CMT`+`High-RE`,
             pct_excluded=round(100*(`both-high`+`neither-high`)/total)) %>%
      arrange(desc(total)) %>% as.data.frame())
cat("\nDeployment levels by status (2C, median of the ten-region sum):\n")
print(axes %>% left_join(ex %>% distinct(Model,Scenario,status), by=c("Model","Scenario")) %>%
      filter(grepl("^2C", Ambition), !is.na(status)) %>%
      group_by(status) %>%
      summarise(n=n(), med_cmt=round(median(cmt)), med_re=round(median(re)),
                .groups="drop") %>% as.data.frame())

line("Q3. DOES THE RESULT MOVE WHEN THE CUT MOVES?")
cat("Agreement among survivors is guaranteed by monotonicity, so measure the two\n")
cat("things that can actually change: who is in the sample, and what they show.\n\n")
for (q in c(0.50, 0.60, 2/3, 0.75)) churn(label_by(axes,"cmt",q), sprintf("cut at %.2f", q))

# --- and the part that matters: re-run the headline under each cut ----------
F <- load_frame("A")                      # carries the 2/3 labels
key <- F %>% select(-Pathway)
ALLR <- c("Aggregated R10 regions", R10_TEN)

sweep_cut <- function(q) {
  lab <- label_by(axes, "cmt", q) %>% filter(!is.na(lab)) %>%
    distinct(Model, Scenario, Pathway = lab)
  d <- key %>% inner_join(lab, by=c("Model","Scenario")) %>%
    mutate(Pathway = factor(Pathway, levels=PATHWAYS))
  expand_grid(Region=ALLR, amb=c("1.5C","2C"), outcome=names(OUT5)) %>%
    pmap_dfr(function(Region,amb,outcome)
      bind_cols(tibble(cut=q,Region,amb,outcome),
                cell5(d[d$Region==Region & d$amb==amb,], outcome)))
}
CUT <- map_dfr(c(0.50,0.60,2/3,0.75), sweep_cut) %>%
  mutate(win = ifelse(is.na(adv), NA, adv>0))
saveRDS(CUT, "CUT_SENS.rds")

cat("\nHEADLINE under each cut (all 11 regions x 2 ambitions x 5 outcomes):\n")
print(CUT %>% filter(!is.na(win)) %>% group_by(cut) %>%
      summarise(cells=n(), RE_wins=sum(win), pct=round(100*mean(win)),
                med_adv=round(median(adv),3), .groups="drop") %>% as.data.frame())

cat("\nBy outcome:\n")
print(CUT %>% filter(!is.na(win)) %>%
      mutate(label=OUT5[outcome]) %>% group_by(label,cut) %>%
      summarise(cell=paste0(sum(win),"/",n()), .groups="drop") %>%
      pivot_wider(names_from=cut, values_from=cell) %>% as.data.frame())

cat("\nCells that CHANGE SIGN between the 0.50 cut and the 0.75 cut:\n")
sg <- CUT %>% filter(cut %in% c(0.50,0.75)) %>% select(Region,amb,outcome,cut,adv) %>%
  pivot_wider(names_from=cut, values_from=adv, names_prefix="q") %>%
  filter(!is.na(q0.5), !is.na(q0.75), sign(q0.5)!=sign(q0.75))
cat(nrow(sg), "of", sum(!is.na(CUT$adv[CUT$cut==0.75])), "\n")
if (nrow(sg)) print(sg %>% mutate(label=OUT5[outcome], across(where(is.numeric),~round(.,2))) %>%
                    select(Region,amb,label,q0.5,q0.75) %>% as.data.frame())

line("Q4. ARE SCENARIOS INDEPENDENT?")
cl <- pw %>% filter(!is.na(Pathway_excl)) %>%
  mutate(fam=sub("[ /].*$","",Model),
         stem = gsub("[-_ ]?[0-9]+(\\.[0-9]+)?[a-z]?$", "", Scenario),
         stem = sub("/.*$","",stem))
eff <- nrow(distinct(cl, Model, stem))
cat("classified scenarios:", nrow(cl), "| distinct models:", n_distinct(cl$Model),
    "| families:", n_distinct(cl$fam), "\n")
cat("distinct model x stem clusters:", eff,
    sprintf("| design effect roughly %.1fx\n", nrow(cl)/eff))
print(cl %>% count(Model, stem, sort=TRUE) %>% head(10) %>% as.data.frame())
