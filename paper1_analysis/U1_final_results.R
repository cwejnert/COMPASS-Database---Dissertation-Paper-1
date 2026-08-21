# =============================================================================
# U1 — THE FINAL RESULT TABLE, incorporating every audit decision.
#
# What the audit (T1b-T4) changed:
#   1. OUTCOME FAMILIES, not outcomes. The two jobs measures correlate 0.97 and
#      the two deprivation measures 0.996. Counting all five treats one result
#      as two. Headline counts THREE families (jobs / deprivation / mortality);
#      the second measure in each family is reported as a within-family check.
#   2. CLUSTER-ROBUST significance. 590 scenarios sit in 312 model x stem
#      clusters. Wilcoxon p-values assume 590 independent draws. Significance
#      is now a cluster-bootstrap 95% CI on Cliff's delta.
#   3. COMMON-SUPPORT labelling reported as a sensitivity. The published
#      classification computes the two terciles on different samples (367
#      scenarios report no renewable capacity), which is why the arms are
#      335/255 rather than equal.
#   4. Per-capita is a FIXED base-period denominator, identical in every
#      scenario, so it rescales levels and cannot touch any contrast.
#
# Window 2020-2050 throughout. Positive advantage ALWAYS means High-RE better.
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")
set.seed(20260821)

ALLR   <- c("Aggregated R10 regions", R10_TEN)
FAMILY <- c(REFOSS = "Jobs", LOWC = "Jobs",
            gap_GJ_pc = "Energy deprivation", headcount_pct = "Energy deprivation",
            mort_per_1k = "Health")
PRIMARY <- c(REFOSS = TRUE, LOWC = FALSE, gap_GJ_pc = TRUE,
             headcount_pct = FALSE, mort_per_1k = TRUE)
B <- 2000

cliff_fast <- function(a, b) {
  n1 <- length(a); n2 <- length(b)
  if (!n1 || !n2) return(NA_real_)
  r <- rank(c(a, b)); U <- sum(r[(n1+1):(n1+n2)]) - n2*(n2+1)/2
  2*U/(n1*n2) - 1
}

build <- function(id) {
  load_frame(id) %>%
    mutate(stem = gsub("[-_ ]?[0-9]+(\\.[0-9]+)?[a-z]?$", "", Scenario),
           stem = sub("/.*$", "", stem), clus = paste(Model, stem))
}

cell_full <- function(d, out) {
  sgn <- ifelse(out %in% LOWER5, -1, 1)
  a <- d[[out]][d$Pathway=="High-CMT"]; b <- d[[out]][d$Pathway=="High-RE"]
  ca <- d$clus[d$Pathway=="High-CMT"];  cb <- d$clus[d$Pathway=="High-RE"]
  ka <- !is.na(a); kb <- !is.na(b)
  a<-a[ka]; ca<-ca[ka]; b<-b[kb]; cb<-cb[kb]
  na <- length(a); nb <- length(b)
  if (na < 5 || nb < 5)
    return(tibble(n_cmt=na, n_re=nb, n_clus=NA_integer_, med_cmt=NA_real_, med_re=NA_real_,
                  pct=NA_real_, adv=NA_real_, lo=NA_real_, hi=NA_real_, p_wilcox=NA_real_))
  ua <- unique(ca); ub <- unique(cb)
  ia <- split(seq_len(na), ca); ib <- split(seq_len(nb), cb)
  reps <- vapply(seq_len(B), function(i) {
    sa <- unlist(ia[sample(ua, length(ua), TRUE)], use.names=FALSE)
    sb <- unlist(ib[sample(ub, length(ub), TRUE)], use.names=FALSE)
    cliff_fast(a[sa], b[sb])
  }, numeric(1))
  q <- quantile(sgn*reps, c(.025,.975), na.rm=TRUE)
  tibble(n_cmt=na, n_re=nb, n_clus=length(ua)+length(ub),
         med_cmt=median(a), med_re=median(b),
         pct = sgn*100*(median(b)-median(a))/abs(median(a)),
         adv = sgn*cliff_fast(a,b), lo = q[[1]], hi = q[[2]],
         p_wilcox = suppressWarnings(wilcox.test(a,b))$p.value)
}

sweep <- function(d, approach, sample_tag) {
  expand_grid(Region=ALLR, amb=c("1.5C","2C"), outcome=names(OUT5)) %>%
    pmap_dfr(function(Region, amb, outcome)
      bind_cols(tibble(approach, sample=sample_tag, Region, amb, outcome),
                cell_full(d[d$Region==Region & d$amb==amb, ], outcome)))
}

A <- build("A"); C <- build("C")
matched <- function(d) d %>% filter(Region %in% R10_TEN) %>%
  distinct(Model,Scenario,Region) %>% count(Model,Scenario) %>% filter(n==10) %>%
  transmute(k = paste(Model,Scenario))

FIN <- bind_rows(
  sweep(A, "A full database", "all scenarios"),
  sweep(A %>% filter(paste(Model,Scenario) %in% matched(A)$k), "A full database", "matched only"),
  sweep(C, "C SCI-vetted",    "all scenarios"),
  sweep(C %>% filter(paste(Model,Scenario) %in% matched(C)$k), "C SCI-vetted", "matched only")
) %>%
  group_by(approach, sample) %>% mutate(p_fdr = p.adjust(p_wilcox, "BH")) %>% ungroup() %>%
  mutate(family  = FAMILY[outcome], primary = PRIMARY[outcome],
         label   = OUT5[outcome],
         win     = ifelse(is.na(adv), NA, adv > 0),
         sig_naive = !is.na(p_fdr) & p_fdr < 0.05,
         sig     = !is.na(lo) & (lo > 0 | hi < 0),
         is_world = Region == "Aggregated R10 regions",
         window  = "2020-2050")
saveRDS(FIN, "FINAL_RESULTS.rds")

H <- FIN %>% filter(approach=="A full database", sample=="all scenarios")
HP <- H %>% filter(primary)

line("HEADLINE — three outcome families, 2020-2050, cluster-robust")
cat("cells (11 regions x 2 ambitions x 3 families):", sum(!is.na(HP$win)), "\n")
cat("favour High-RE:", sum(HP$win, na.rm=TRUE),
    sprintf(" (%.0f%%)", 100*mean(HP$win, na.rm=TRUE)),
    "| significantly so:", sum(HP$win & HP$sig, na.rm=TRUE),
    "| significantly favour High-CMT:", sum(!HP$win & HP$sig, na.rm=TRUE), "\n")
cat("\nall five measures, for comparison:", sum(H$win, na.rm=TRUE), "of", sum(!is.na(H$win)),
    sprintf(" (%.0f%%)\n", 100*mean(H$win, na.rm=TRUE)))

cat("\nBY FAMILY (primary measure):\n")
print(HP %>% filter(!is.na(win)) %>% group_by(family, label) %>%
      summarise(cells=n(), RE=sum(win), sig_RE=sum(win&sig), sig_CMT=sum(!win&sig),
                med_adv=round(median(adv),2), med_pct=round(median(pct)), .groups="drop") %>%
      as.data.frame())
cat("\nWITHIN-FAMILY CHECK (second measure agrees?):\n")
print(H %>% filter(!is.na(win)) %>% group_by(label, primary) %>%
      summarise(cell=paste0(sum(win),"/",n()), .groups="drop") %>%
      arrange(desc(primary)) %>% as.data.frame())

line("WORLD")
print(H %>% filter(is_world) %>%
      select(amb, label, n_cmt, n_re, n_clus, med_cmt, med_re, pct, adv, lo, hi, sig) %>%
      mutate(across(where(is.numeric), ~round(.,2))) %>% as.data.frame())

line("REGIONAL GRID (primary measures, advantage with cluster-robust CI)")
print(HP %>% filter(!is_world, !is.na(adv)) %>%
      mutate(cellv = sprintf("%+.2f%s", adv, ifelse(sig,"*",""))) %>%
      select(Region, amb, family, cellv) %>%
      pivot_wider(names_from=c(family,amb), values_from=cellv) %>% as.data.frame())

line("ROBUSTNESS — does the verdict survive the four alternative samples?")
print(FIN %>% filter(primary, !is.na(win)) %>% group_by(approach, sample) %>%
      summarise(cells=n(), RE=sum(win), pct=round(100*mean(win)),
                sig_RE=sum(win&sig), sig_CMT=sum(!win&sig),
                n_scen=NA_integer_, .groups="drop") %>%
      select(-n_scen) %>% as.data.frame())
cat("\nscenario counts behind each:\n")
print(bind_rows(
  tibble(approach="A full database", sample="all scenarios",  n=n_distinct(paste(A$Model,A$Scenario))),
  tibble(approach="A full database", sample="matched only",   n=nrow(matched(A))),
  tibble(approach="C SCI-vetted",    sample="all scenarios",  n=n_distinct(paste(C$Model,C$Scenario))),
  tibble(approach="C SCI-vetted",    sample="matched only",   n=nrow(matched(C)))) %>% as.data.frame())

line("EVERY CELL THAT FAVOURS High-CMT (primary measures)")
print(HP %>% filter(!is.na(win), !win) %>%
      select(Region, amb, label, med_cmt, med_re, pct, adv, lo, hi, sig) %>%
      mutate(across(where(is.numeric), ~round(.,2))) %>% arrange(label, Region) %>% as.data.frame())
