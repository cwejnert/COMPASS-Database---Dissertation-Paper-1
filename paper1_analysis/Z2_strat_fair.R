# =============================================================================
# Z2 — IS THE WITHIN-MODEL TEST A FAIR REFEREE FOR DEPRIVATION?
#
# Z1 reported that deprivation's pooled and within-model contrasts disagree in
# 12 of 22 cells, and I called the result "not robust" on the strength of it.
# Before that goes to a supervisor it needs one more check, because the
# stratified test does not just change HOW the arms are compared -- it changes
# WHICH SCENARIOS are in them.
#
# The worry: models that are overwhelmingly High-CMT contribute only a handful
# of High-RE scenarios, and those are likely to be scenarios that BARELY cleared
# the renewables tercile. REMIND's High-RE scenarios sit deep in the corner;
# MESSAGEix's four might sit right on the line. If so the stratified estimate is
# comparing a WEAKER treatment, and a flip would say nothing about robustness.
#
# Three tests:
#   1. How deep in the renewables corner is each arm, inside vs outside the
#      "both arms" families?
#   2. Is the stratified estimate dominated by one thin stratum?
#   3. Does the flip survive if the comparison is restricted to scenarios
#      matched on how far past the threshold they sit?
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

F  <- load_frame("A")
pw <- readRDS("pw_A.rds") %>%
  select(Model, Scenario, total_cdr, total_re, cdr_thresh, re_thresh, Ambition)

# how far past its own ambition-class threshold does a scenario sit?
depth <- pw %>%
  mutate(re_depth  = total_re  / re_thresh,
         cdr_depth = total_cdr / cdr_thresh) %>%
  select(Model, Scenario, re_depth, cdr_depth)

D <- F %>% filter(!is.na(gap_GJ_pc)) %>% inner_join(depth, by = c("Model","Scenario"))

# families holding both arms with >=3 each, per region-ambition cell
qual <- function(d) d %>% group_by(fam) %>%
  summarise(a = sum(Pathway=="High-CMT"), b = sum(Pathway=="High-RE"), .groups="drop") %>%
  filter(a >= 3, b >= 3) %>% pull(fam)

line("1. IS THE STRATIFIED SUBSET COMPARING A WEAKER TREATMENT?")
cat("re_depth = a scenario's renewable total divided by its ambition class's\n")
cat("threshold. 1.0 means it sat exactly on the line; 3.0 means it is three\n")
cat("times past it. Shown for HIGH-RE scenarios only.\n\n")
res <- lapply(c("1.5C","2C"), function(a) {
  d <- D %>% filter(Region == "R10AFRICA", amb == a)
  q <- qual(d)
  d %>% filter(Pathway == "High-RE") %>%
    mutate(inside = fam %in% q) %>%
    group_by(inside) %>%
    summarise(amb = a, n = n(), median_re_depth = round(median(re_depth), 2),
              q25 = round(quantile(re_depth,.25),2),
              q75 = round(quantile(re_depth,.75),2), .groups="drop")
}) %>% bind_rows() %>%
  mutate(group = ifelse(inside, "inside a both-arms family", "outside")) %>%
  select(amb, group, n, median_re_depth, q25, q75)
print(as.data.frame(res))
cat("\nIf the 'inside' group sits much closer to 1.0, the within-model test is\n")
cat("comparing High-CMT against scenarios that only just qualified as High-RE.\n")

line("2. IS THE STRATIFIED ESTIMATE DOMINATED BY ONE THIN STRATUM?")
for (a in c("1.5C","2C")) {
  d <- D %>% filter(Region == "R10AFRICA", amb == a)
  q <- qual(d)
  w <- d %>% filter(fam %in% q) %>% group_by(fam) %>%
    summarise(n_cmt = sum(Pathway=="High-CMT"), n_re = sum(Pathway=="High-RE"),
              .groups="drop") %>%
    mutate(weight = n_cmt*n_re, wt_pct = round(100*weight/sum(weight)))
  cat("\n--", a, "--\n"); print(as.data.frame(w))
  cat("   cliff_strat weights each family by n_cmt * n_re, so a family with a\n")
  cat("   large CMT arm and a tiny RE arm still carries most of the estimate.\n")
}

line("3. DOES THE FLIP SURVIVE A DEPTH-MATCHED COMPARISON?")
cat("Restrict BOTH arms to scenarios within a comparable distance of their\n")
cat("threshold (re_depth <= 2 for High-RE), so the stratified and pooled\n")
cat("estimates describe the same strength of treatment.\n\n")
cliff <- function(a, b) { a<-a[!is.na(a)]; b<-b[!is.na(b)]
  if (length(a)<3 || length(b)<3) return(NA_real_)
  r <- rank(c(a,b)); n1<-length(a); n2<-length(b)
  2*((sum(r[(n1+1):(n1+n2)]) - n2*(n2+1)/2)/(n1*n2)) - 1 }

out <- expand_grid(Region = c("Aggregated R10 regions", R10_TEN),
                   amb = c("1.5C","2C")) %>%
  pmap_dfr(function(Region, amb) {
    d <- D %>% filter(Region == !!Region, amb == !!amb)
    q <- qual(d)
    ds <- d %>% filter(fam %in% q)
    # pooled, but restricted to the SAME depth band as the stratified subset
    band <- if (nrow(ds)) quantile(ds$re_depth[ds$Pathway=="High-RE"], .95) else NA
    dm <- d %>% filter(Pathway == "High-CMT" |
                       (Pathway == "High-RE" & re_depth <= band))
    g <- function(x) if (sum(x$Pathway=="High-CMT") >= 5 &&
                         sum(x$Pathway=="High-RE") >= 5)
      -cliff(x$gap_GJ_pc[x$Pathway=="High-CMT"],
             x$gap_GJ_pc[x$Pathway=="High-RE"]) else NA_real_
    tibble(Region, amb,
           pooled_all   = g(d),
           pooled_depth = g(dm),
           within       = if (length(q))
             -cliff_strat(ds$gap_GJ_pc, ds$Pathway, ds$fam) else NA_real_,
           n_re_all = sum(d$Pathway=="High-RE"),
           n_re_band = sum(dm$Pathway=="High-RE"),
           depth_cut = round(band, 2))
  })
print(out %>% mutate(across(where(is.numeric), ~round(.,3))) %>% as.data.frame())

cat("\nsign disagreements with the FULL pooled estimate:\n")
cat("  within-model  :", sum(!is.na(out$within) & !is.na(out$pooled_all) &
                             sign(out$within) != sign(out$pooled_all)), "\n")
cat("  depth-matched :", sum(!is.na(out$pooled_depth) & !is.na(out$pooled_all) &
                             sign(out$pooled_depth) != sign(out$pooled_all)), "\n")
cat("\nIf the depth-matched pooled estimate ALSO flips, the driver is treatment\n")
cat("strength, not model composition -- and the within-model test was unfair.\n")
cat("If it does NOT flip, model composition is the real driver and the\n")
cat("deprivation result genuinely is fragile.\n")
saveRDS(out, "Z2_STRAT_FAIR.rds")
