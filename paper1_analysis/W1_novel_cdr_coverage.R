# =============================================================================
# W1 — NOVEL CDR IS A REPORTING-COVERAGE PROBLEM INSIDE THE CLASSIFICATION AXIS
#
# The audit reported "Novel CDR contributes 0% of carbon management at the
# median". That was WRONG, and wrong in an instructive way:
#
#   * only 318 of 1,086 scenarios report a Novel CDR row at all (29%)
#   * of those that DO report it, NONE is zero -- median 106,249
#   * among scenarios reporting all three components, Novel CDR is 42.9% of the
#     total, the LARGEST component
#
# The median across all scenarios is zero because 71% have no row, and
# sum(Value, na.rm = TRUE) silently turns "not reported" into zero.
#
# THE CONSEQUENCE IS NOT COSMETIC. The classification axis is
#     total_cdr = land + novel + fossil CCS
# so a scenario that reports Novel CDR carries ~43% more axis than an otherwise
# identical scenario that does not. If reporting correlates with model family --
# and in this database everything does -- then the High-CMT arm is partly
# selecting on REPORTING COMPLETENESS rather than on deployment.
#
# Four questions:
#   1. Who reports Novel CDR? Is it a model-family fingerprint?
#   2. Does reporting it predict landing in the High-CMT arm?
#   3. If the axis is rebuilt on components everyone reports, who moves?
#   4. Does the headline move?
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

ds <- readRDS("ds_A.rds")
pw <- readRDS("pw_A.rds")
ALLR <- c("Aggregated R10 regions", R10_TEN)

# component totals, keeping NA distinct from a reported zero
comp <- ds %>%
  filter(Variable %in% c("Land-based CDR","Novel CDR","Fossil CCS","Renewable Capacity"),
         Region %in% R10_TEN) %>%
  group_by(Model, Scenario, Variable) %>%
  summarise(v = sum(Total_Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Variable, values_from = v) %>%
  rename(land = `Land-based CDR`, novel = `Novel CDR`,
         fccs = `Fossil CCS`, re = `Renewable Capacity`) %>%
  mutate(reports_novel = !is.na(novel), fam = sub("[ /].*$", "", Model))

line("1. WHO REPORTS NOVEL CDR?")
print(comp %>% group_by(fam) %>%
      summarise(n = n(), n_reporting = sum(reports_novel), .groups = "drop") %>%
      mutate(pct = round(100*n_reporting/n)) %>% arrange(desc(n)) %>% as.data.frame())
cat("\nIf this is 0% or 100% by family, reporting is a MODEL FINGERPRINT and any\n")
cat("axis built on it inherits the model-composition problem wholesale.\n")

line("2. DOES REPORTING NOVEL CDR PREDICT THE HIGH-CMT ARM?")
lab <- pw %>% filter(!is.na(Pathway_excl)) %>%
  distinct(Model, Scenario, Pathway = Pathway_excl)
j <- comp %>% inner_join(lab, by = c("Model","Scenario"))
print(j %>% count(Pathway, reports_novel) %>%
      pivot_wider(names_from = reports_novel, values_from = n, values_fill = 0,
                  names_prefix = "reports_") %>%
      mutate(pct_reporting = round(100*reports_TRUE/(reports_TRUE+reports_FALSE))) %>%
      as.data.frame())
tb <- with(j, table(Pathway, reports_novel))
cat("\nFisher exact p:", signif(fisher.test(tb)$p.value, 3),
    "| odds ratio:", round(fisher.test(tb)$estimate, 2), "\n")
cat("An odds ratio far from 1 means the axis is partly measuring who filled in\n")
cat("the DACCS and enhanced-weathering rows.\n")

line("3. REBUILD THE AXIS THREE WAYS")
# A: as published (missing -> 0)
# B: land + fossil CCS only, the two components nearly everyone reports
# C: all three, restricted to scenarios that report all three
amb <- pw %>% distinct(Model, Scenario, Ambition) %>% filter(!is.na(Ambition))
base <- comp %>% inner_join(amb, by = c("Model","Scenario")) %>%
  mutate(cmt_pub  = coalesce(land,0) + coalesce(novel,0) + coalesce(fccs,0),
         cmt_lf   = coalesce(land,0) + coalesce(fccs,0),
         cmt_all3 = ifelse(!is.na(land) & !is.na(novel) & !is.na(fccs),
                           land + novel + fccs, NA_real_)) %>%
  filter(!is.na(re))

mklab <- function(d, col) d %>% filter(!is.na(.data[[col]])) %>%
  group_by(Ambition) %>%
  mutate(hr = re >= quantile(re, 2/3, na.rm=TRUE),
         hc = .data[[col]] >= quantile(.data[[col]], 2/3, na.rm=TRUE),
         lab = ifelse(hc & !hr, "High-CMT", ifelse(hr & !hc, "High-RE", NA_character_))) %>%
  ungroup() %>% select(Model, Scenario, lab)

L <- list(published = mklab(base,"cmt_pub"),
          land_fccs = mklab(base,"cmt_lf"),
          all_three = mklab(base,"cmt_all3"))
for (nm in names(L)) {
  k <- L[[nm]]
  cat(sprintf("  %-10s scenarios ranked %4d | High-CMT %3d | High-RE %3d\n",
      nm, nrow(k), sum(k$lab=="High-CMT",na.rm=TRUE), sum(k$lab=="High-RE",na.rm=TRUE)))
}
cat("\nchurn against the published labelling (NA moves counted):\n")
for (nm in c("land_fccs","all_three")) {
  m <- L$published %>% rename(old=lab) %>%
    full_join(L[[nm]] %>% rename(new=lab), by=c("Model","Scenario")) %>%
    mutate(old=ifelse(is.na(old),"unclassified",old), new=ifelse(is.na(new),"unclassified",new))
  cat(sprintf("  %-10s moved %3d of %4d (%.0f%%) | flipped arm %2d\n", nm,
      sum(m$old!=m$new), nrow(m), 100*mean(m$old!=m$new),
      sum(m$old %in% PATHWAYS & m$new %in% PATHWAYS & m$old!=m$new)))
}

line("4. DOES THE HEADLINE MOVE?")
F <- load_frame("A"); key <- F %>% select(-Pathway)
sweep <- function(lab, tag) {
  d <- key %>% inner_join(lab %>% filter(!is.na(lab)) %>%
                          distinct(Model, Scenario, Pathway = lab),
                          by = c("Model","Scenario")) %>%
    mutate(Pathway = factor(Pathway, levels = PATHWAYS))
  expand_grid(Region = ALLR, amb = c("1.5C","2C"), outcome = names(OUT5)) %>%
    pmap_dfr(function(Region, amb, outcome)
      bind_cols(tibble(design = tag, Region, amb, outcome),
                cell5(d[d$Region==Region & d$amb==amb, ], outcome)))
}
AX <- bind_rows(lapply(names(L), function(nm) sweep(L[[nm]], nm))) %>%
  mutate(win = ifelse(is.na(adv), NA, adv > 0),
         family = c(REFOSS="Jobs", LOWC="Jobs", gap_GJ_pc="Deprivation",
                    headcount_pct="Deprivation", mort_per_1k="Health")[outcome],
         primary = outcome %in% c("REFOSS","gap_GJ_pc","mort_per_1k"))
saveRDS(AX, "W1_AXIS_SENS.rds")

cat("all five measures (110 cells):\n")
print(AX %>% filter(!is.na(win)) %>% group_by(design) %>%
      summarise(cells=n(), RE=sum(win), pct=round(100*mean(win)),
                med_adv=round(median(adv),3), .groups="drop") %>% as.data.frame())
cat("\nthree families (66 cells) -- the headline:\n")
print(AX %>% filter(primary, !is.na(win)) %>% group_by(design) %>%
      summarise(cells=n(), RE=sum(win), pct=round(100*mean(win)), .groups="drop") %>%
      as.data.frame())
cat("\nby family:\n")
print(AX %>% filter(primary, !is.na(win)) %>% group_by(family, design) %>%
      summarise(cell=paste0(sum(win),"/",n()), .groups="drop") %>%
      pivot_wider(names_from=design, values_from=cell) %>% as.data.frame())

cat("\ncells changing sign, published vs land+fossilCCS:\n")
sg <- AX %>% filter(design %in% c("published","land_fccs")) %>%
  select(Region, amb, outcome, design, adv) %>%
  pivot_wider(names_from=design, values_from=adv) %>%
  filter(!is.na(published), !is.na(land_fccs), sign(published)!=sign(land_fccs))
cat(nrow(sg), "of", sum(!is.na(AX$adv[AX$design=="land_fccs"])), "\n")
if (nrow(sg)) print(sg %>% mutate(label=OUT5[outcome], across(where(is.numeric),~round(.,2))) %>%
                    select(Region, amb, label, published, land_fccs) %>% as.data.frame())
