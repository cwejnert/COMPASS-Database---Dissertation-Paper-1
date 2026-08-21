# =============================================================================
# T2 — AUDIT SECTION 2: THE SAMPLE AND THE INFERENCE
#
# Two weaknesses found in T1b that need testing, not just noting.
#
# (A) SPLIT THRESHOLD SAMPLE. The two terciles are computed on DIFFERENT
#     scenario sets. 367 of 1425 scenarios (26%) report no Renewable Capacity
#     at all, so re_thresh is a tercile of 1058 scenarios while cdr_thresh is a
#     tercile of 1394. That is why the arms are 335 / 255 rather than equal:
#     the High-CMT pool is drawn from a larger base. Entire families are
#     missing RE (MESSAGE-GLOBIOM, POLES, WITCH-GLOBIOM 100%; GCAM 74%).
#     FIX TO TEST: recompute both thresholds on the common-support subset.
#
# (B) NON-INDEPENDENCE. 590 classified scenarios sit in 312 model x stem
#     clusters (design effect ~1.9x). Every Wilcoxon p-value treats sibling
#     runs of one scenario family as independent draws. FIX TO TEST: a cluster
#     bootstrap, resampling whole model x stem clusters.
#
# The question for both is the same: does the CONCLUSION move, or only the
# decoration?
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")
set.seed(20260821)

ALLR <- c("Aggregated R10 regions", R10_TEN)
F    <- load_frame("A")
key  <- F %>% select(-Pathway)

# rank-based Cliff's delta: identical to the outer() version, O(n log n)
cliff_fast <- function(a, b) {
  n1 <- length(a); n2 <- length(b)
  if (!n1 || !n2) return(NA_real_)
  r <- rank(c(a, b))
  U <- sum(r[(n1+1):(n1+n2)]) - n2*(n2+1)/2
  2*U/(n1*n2) - 1
}
stopifnot(abs(cliff_fast(c(1,2,3,4),c(2,3,3,9)) - cliff_d(c(1,2,3,4),c(2,3,3,9))) < 1e-12)

# =============================================================================
line("(A) SPLIT THRESHOLD SAMPLE — relabel on common support")
# =============================================================================
pw <- readRDS("pw_A.rds")
cat("scenarios with an Ambition:", nrow(pw),
    "| missing CDR:", sum(is.na(pw$total_cdr)),
    "| missing RE:", sum(is.na(pw$total_re)),
    "| BOTH present:", sum(!is.na(pw$total_cdr) & !is.na(pw$total_re)), "\n\n")

CS <- pw %>% filter(!is.na(total_cdr), !is.na(total_re)) %>%
  group_by(Ambition) %>%
  mutate(hc = total_cdr >= quantile(total_cdr, 2/3),
         hr = total_re  >= quantile(total_re,  2/3),
         lab = ifelse(hc & !hr, "High-CMT", ifelse(hr & !hc, "High-RE", NA_character_))) %>%
  ungroup()
cat("arm sizes -- current labelling vs common support:\n")
print(bind_rows(
  pw %>% filter(!is.na(Pathway_excl)) %>% count(Ambition, Pathway_excl) %>%
    rename(lab = Pathway_excl) %>% mutate(design = "current (split sample)"),
  CS %>% filter(!is.na(lab)) %>% count(Ambition, lab) %>%
    mutate(design = "common support")) %>%
  pivot_wider(names_from = lab, values_from = n) %>% as.data.frame())

lab_cs <- CS %>% filter(!is.na(lab)) %>% distinct(Model, Scenario, Pathway = lab)
sweep <- function(d, tag) {
  expand_grid(Region = ALLR, amb = c("1.5C","2C"), outcome = names(OUT5)) %>%
    pmap_dfr(function(Region, amb, outcome)
      bind_cols(tibble(design = tag, Region, amb, outcome),
                cell5(d[d$Region == Region & d$amb == amb, ], outcome)))
}
D_cs <- key %>% inner_join(lab_cs, by = c("Model","Scenario")) %>%
  mutate(Pathway = factor(Pathway, levels = PATHWAYS))
CMP <- bind_rows(sweep(F, "current"), sweep(D_cs, "common support")) %>%
  mutate(win = ifelse(is.na(adv), NA, adv > 0))
saveRDS(CMP, "T2_COMMON_SUPPORT.rds")

cat("\nheadline:\n")
print(CMP %>% filter(!is.na(win)) %>% group_by(design) %>%
      summarise(cells = n(), RE_wins = sum(win), pct = round(100*mean(win)),
                med_adv = round(median(adv), 3), .groups = "drop") %>% as.data.frame())
cat("\nby outcome:\n")
print(CMP %>% filter(!is.na(win)) %>% mutate(label = OUT5[outcome]) %>%
      group_by(label, design) %>% summarise(cell = paste0(sum(win),"/",n()), .groups="drop") %>%
      pivot_wider(names_from = design, values_from = cell) %>% as.data.frame())
sg <- CMP %>% select(Region, amb, outcome, design, adv) %>%
  pivot_wider(names_from = design, values_from = adv) %>%
  filter(!is.na(current), !is.na(`common support`),
         sign(current) != sign(`common support`))
cat("\ncells that change sign:", nrow(sg), "\n")
if (nrow(sg)) print(sg %>% mutate(label = OUT5[outcome], across(where(is.numeric), ~round(.,2))) %>%
                    select(Region, amb, label, current, `common support`) %>% as.data.frame())

# =============================================================================
line("(B) NON-INDEPENDENCE — cluster bootstrap over model x stem")
# =============================================================================
FC <- F %>% mutate(stem = gsub("[-_ ]?[0-9]+(\\.[0-9]+)?[a-z]?$", "", Scenario),
                   stem = sub("/.*$", "", stem),
                   clus = paste(Model, stem))
cat("clusters in the analysis frame:", n_distinct(FC$clus),
    "| scenarios:", n_distinct(paste(FC$Model, FC$Scenario)), "\n")

B <- 600
boot_cell <- function(d, out) {
  a <- d[[out]][d$Pathway == "High-CMT"]; b <- d[[out]][d$Pathway == "High-RE"]
  ca <- d$clus[d$Pathway == "High-CMT"];  cb <- d$clus[d$Pathway == "High-RE"]
  ka <- !is.na(a); kb <- !is.na(b); a <- a[ka]; ca <- ca[ka]; b <- b[kb]; cb <- cb[kb]
  if (length(a) < 5 || length(b) < 5) return(c(NA, NA, NA, NA))
  obs <- cliff_fast(a, b)
  ua <- unique(ca); ub <- unique(cb)
  ia <- split(seq_along(a), ca); ib <- split(seq_along(b), cb)
  reps <- vapply(seq_len(B), function(i) {
    sa <- unlist(ia[sample(ua, length(ua), TRUE)], use.names = FALSE)
    sb <- unlist(ib[sample(ub, length(ub), TRUE)], use.names = FALSE)
    if (!length(sa) || !length(sb)) return(NA_real_)
    cliff_fast(a[sa], b[sb])
  }, numeric(1))
  c(obs, quantile(reps, .025, na.rm = TRUE), quantile(reps, .975, na.rm = TRUE),
    length(ua) + length(ub))
}
BOOT <- expand_grid(Region = ALLR, amb = c("1.5C","2C"), outcome = names(OUT5)) %>%
  pmap_dfr(function(Region, amb, outcome) {
    d <- FC[FC$Region == Region & FC$amb == amb, ]
    v <- boot_cell(d, outcome)
    sgn <- ifelse(outcome %in% LOWER5, -1, 1)
    tibble(Region, amb, outcome, adv = sgn*v[1],
           lo = sgn*ifelse(sgn > 0, v[2], v[3]), hi = sgn*ifelse(sgn > 0, v[3], v[2]),
           n_clus = v[4])
  })
NAIVE <- sweep(F, "current") %>%
  mutate(p_fdr = p.adjust(p, "BH"), sig_naive = !is.na(p_fdr) & p_fdr < 0.05) %>%
  select(Region, amb, outcome, adv, n_cmt, n_re, p_fdr, sig_naive)
CB <- BOOT %>% inner_join(NAIVE %>% select(-adv), by = c("Region","amb","outcome")) %>%
  mutate(sig_clus = !is.na(lo) & (lo > 0 | hi < 0), win = adv > 0)
saveRDS(CB, "T2_CLUSTER_BOOT.rds")

cat("\nSIGNIFICANCE, naive Wilcoxon+BH vs cluster bootstrap 95% CI:\n")
print(CB %>% filter(!is.na(adv)) %>%
      # NB: summarise() evaluates sequentially, so a later expression would see
      # the SCALAR sig_naive, not the column. Compute the crosstab instead.
      summarise(cells = n(), n_sig_naive = sum(sig_naive), n_sig_cluster = sum(sig_clus),
                lost = sum(sig_naive & !sig_clus, na.rm = TRUE),
                gained = sum((!sig_naive) & sig_clus, na.rm = TRUE), .groups = "drop") %>%
      as.data.frame())
print(with(CB, table(naive = sig_naive, cluster = sig_clus)))
cat("\nby outcome:\n")
print(CB %>% filter(!is.na(adv)) %>% mutate(label = OUT5[outcome]) %>%
      group_by(label) %>%
      summarise(cells = n(), RE_wins = sum(win),
                sig_naive = sum(sig_naive), sig_cluster = sum(sig_clus),
                sig_RE_cluster = sum(sig_clus & win),
                sig_CMT_cluster = sum(sig_clus & !win), .groups = "drop") %>%
      as.data.frame())
cat("\nWORLD, with cluster-robust intervals:\n")
print(CB %>% filter(Region == "Aggregated R10 regions") %>%
      mutate(label = OUT5[outcome], across(where(is.numeric), ~round(., 3))) %>%
      select(amb, label, adv, lo, hi, n_clus, sig_naive, sig_clus) %>% as.data.frame())
cat("\nCells significant under the naive test but NOT under clustering:\n")
print(CB %>% filter(sig_naive, !sig_clus) %>%
      mutate(label = OUT5[outcome], across(where(is.numeric), ~round(., 2))) %>%
      select(Region, amb, label, adv, lo, hi) %>% as.data.frame())
