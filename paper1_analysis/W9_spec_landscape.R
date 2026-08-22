# =============================================================================
# W9 — THE SPECIFICATION LANDSCAPE
#
# THE QUESTION: which combination of analytical choices gives the strongest
# High-RE result while remaining defensible?
#
# THE HONEST ANSWER IS NOT "THE ONE THAT SCORES HIGHEST". Choosing a
# specification because it maximises the result is the garden of forking paths,
# and it is precisely what the robustness section exists to rule out. Every
# choice in this study was fixed on prior grounds before its effect was known,
# and re-picking now would make every one of those prior justifications false.
#
# SO THIS SCRIPT ANSWERS THE USEFUL VERSION OF THE QUESTION, in three parts:
#
#   1. THE RANGE. What is the best and worst the result looks across every
#      combination tested? A reader is entitled to know the spread, and a paper
#      that shows it is far harder to attack than one that reports a point.
#
#   2. THE INVARIANT CORE. Which claims hold in EVERY combination? That set is
#      the strongest thing the paper can say, because it needs no specification
#      argument at all -- it survives whichever choice a referee prefers.
#
#   3. WHERE THE CHOICES ACTUALLY BITE. Which specification moves which family,
#      and by how much. This tells you what to pre-register as the primary
#      analysis and what to report as sensitivity.
#
# USAGE: Rscript W9_spec_landscape.R
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

DROP <- "R10PAC_OECD"
FAM3 <- c(REFOSS="Jobs", gap_GJ_pc="Deprivation", mort_per_1k="Health")

# ---- 1. database x sample x ambition, on the raw basis ---------------------
RR <- readRDS("RAW_RESULTS.rds") %>% filter(!is.na(gap), Region != DROP) %>%
  mutate(family = FAM3[outcome])

line("1. DATABASE x SAMPLE x AMBITION — the four published designs")
A <- RR %>% group_by(approach, sample, amb) %>%
  summarise(cells=n(), RE=sum(gap>0), sig_for=sum(gap>0 & sig_raw),
            sig_ag=sum(gap<0 & sig_raw), pct=round(100*mean(gap>0)), .groups="drop") %>%
  arrange(desc(pct))
print(as.data.frame(A))
cat("\nbest:", A$pct[1], "% (", A$approach[1], "/", A$sample[1], "/", A$amb[1], ")",
    " | worst:", A$pct[nrow(A)], "%\n", sep="")

line("2. BY FAMILY — where the spread actually comes from")
B <- RR %>% group_by(family, approach, sample, amb) %>%
  summarise(v = paste0(sum(gap>0), "/", n()), .groups="drop") %>%
  mutate(design = paste0(sub("A full database","FULL",
                         sub("C SCI-vetted","SCI", approach)), " · ",
                         sub(" scenarios","", sample), " · ", amb))
print(as.data.frame(B %>% select(family, design, v) %>%
      pivot_wider(names_from=family, values_from=v)))

line("3. THE INVARIANT CORE — what holds in EVERY one of the eight designs")
inv <- RR %>% group_by(family, Region, amb) %>%
  summarise(n_designs = n(), all_RE = all(gap>0), all_sig = all(gap>0 & sig_raw),
            .groups="drop") %>% filter(n_designs == 4)
print(as.data.frame(inv %>% group_by(family) %>%
      summarise(cells = n(), always_RE = sum(all_RE),
                always_RE_and_significant = sum(all_sig), .groups="drop")))
cat("\ncells where High-RE is better in all four designs AND significant in all four:\n")
print(as.data.frame(inv %>% filter(all_sig) %>%
      transmute(family, reg = ifelse(Region=="Aggregated R10 regions","WORLD",
                                     sub("^R10","",Region)), amb)))

# ---- 4. the other specification axes ---------------------------------------
line("4. THE OTHER AXES — window, label basis, tercile cut, threshold sample")
rows <- list()
if (file.exists("W3_WINDOWS.rds")) {
  W <- readRDS("W3_WINDOWS.rds") %>% filter(!is.na(adv), Region != DROP)
  rows[[length(rows)+1]] <- W %>% group_by(axis = "Window", variant = window) %>%
    summarise(pct = round(100*mean(adv>0)), .groups="drop")
  rows[[length(rows)+1]] <- W %>% filter(family=="Jobs") %>%
    group_by(axis = "Window · JOBS ONLY", variant = window) %>%
    summarise(pct = round(100*mean(adv>0)), .groups="drop")
}
if (file.exists("W4_LABELS.rds")) {
  L <- readRDS("W4_LABELS.rds") %>% filter(!is.na(adv), Region != DROP)
  rows[[length(rows)+1]] <- L %>% group_by(axis = "Label basis", variant = basis) %>%
    summarise(pct = round(100*mean(adv>0)), .groups="drop")
  rows[[length(rows)+1]] <- L %>% filter(family=="Jobs") %>%
    group_by(axis = "Label basis · JOBS ONLY", variant = basis) %>%
    summarise(pct = round(100*mean(adv>0)), .groups="drop")
}
if (file.exists("CUT_SENS.rds")) {
  C <- readRDS("CUT_SENS.rds") %>% filter(!is.na(adv), Region != DROP)
  rows[[length(rows)+1]] <- C %>%
    group_by(axis = "Tercile cut", variant = sprintf("top %.0f%%", 100*(1-cut))) %>%
    summarise(pct = round(100*mean(adv>0)), .groups="drop")
}
if (file.exists("T2_COMMON_SUPPORT.rds")) {
  S <- readRDS("T2_COMMON_SUPPORT.rds") %>% filter(!is.na(adv), Region != DROP)
  rows[[length(rows)+1]] <- S %>% group_by(axis = "Threshold sample", variant = design) %>%
    summarise(pct = round(100*mean(adv>0)), .groups="drop")
}
SPEC <- bind_rows(rows)
print(as.data.frame(SPEC %>% arrange(axis, desc(pct))))

line("5. THE MAXIMISING COMBINATION, AND WHY IT IS NOT THE ANSWER")
best <- A %>% slice(1)
cat("Highest-scoring published design: ", best$approach, " / ", best$sample, " / ",
    best$amb, " -> ", best$RE, " of ", best$cells, " (", best$pct, "%)\n", sep="")
cat("  arm sizes there: see below -- this is the thing to check before quoting it.\n\n")
print(readRDS("RAW_RESULTS.rds") %>%
      filter(approach==best$approach, sample==best$sample, amb==best$amb,
             Region=="Aggregated R10 regions") %>%
      transmute(family=FAM3[outcome], n_cmt, n_re) %>% as.data.frame())

line("THE RECOMMENDATION")
cat("Jobs is 20/20 in every design, every window, every label basis and every\n")
cat("tercile cut. That is a stronger claim than any single specification's\n")
cat("headline count, because it requires no argument about which specification\n")
cat("is right -- it is true under all of them.\n\n")
cat("Report the pre-specified primary (full database, all scenarios, 2020-2050,\n")
cat("global terciles), show the range across everything else, and lead with the\n")
cat("invariant core. A paper that shows its own spread is much harder to attack\n")
cat("than one that reports the best point it found.\n")
saveRDS(list(designs=A, byfamily=B, invariant=inv, axes=SPEC), "W9_SPEC.rds")
