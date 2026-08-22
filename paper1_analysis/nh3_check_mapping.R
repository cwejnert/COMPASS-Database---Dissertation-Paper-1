# =============================================================================
# ONE-MINUTE PRE-CHECK — inspect the FASST -> R10 mapping before re-running.
#
# The last run was fine (590 succeeded, 0 failed). The aggregation was not: it
# produced 5 R10 regions instead of 10, and mortality ROSE in three of them,
# which is impossible when a precursor is removed. Both point at the mapping.
#
# This sources only the definitions (seconds, no rfasst runs) and prints what
# fasst_to_r10 actually contains. Run it, send the output, and the 36-minute
# re-run can be made right first time.
#
# USAGE: setwd() to the folder holding the rfasst script, then
#   source("nh3_check_mapping.R")
# =============================================================================
suppressPackageStartupMessages({library(dplyr)})

CANDIDATES <- c("COMPASS_rfasst_full_allR10.R", "COMPASS_rfasst_full.R")
RFASST <- CANDIDATES[file.exists(CANDIDATES)][1]
if (is.na(RFASST)) stop("No rfasst script found in ", getwd())
cat("using:", RFASST, "\n")

src <- readLines(RFASST, warn = FALSE)
cut <- grep("SECTION 5", src)[1]
prefix <- tempfile(fileext = ".R")
writeLines(src[seq_len(cut - 1)], prefix)
source(prefix, local = FALSE)

# THE FILTER THAT ACTUALLY GATES EVERYTHING. em_clean is filtered on
# `region %in% c(COMPASS_R10_REGIONS, "World")`, and the R10 aggregation is
# filtered on `r10_region %in% COMPASS_R10_REGIONS`. If this vector is short,
# both the input and the output shrink -- and the World-only disaggregation
# spreads each World total over FEWER regions, inflating every one of them.
cat("\n--- COMPASS_R10_REGIONS (the gate on everything) ---\n")
cat("length:", length(COMPASS_R10_REGIONS), "\n")
print(COMPASS_R10_REGIONS)
if (length(COMPASS_R10_REGIONS) != 10) {
  cat("\n[FAIL] this must contain all TEN R10 regions.\n")
  cat("Missing:", paste(setdiff(unique(fasst_to_r10$r10_region),
                                COMPASS_R10_REGIONS), collapse = ", "), "\n")
  cat("Fix the definition in the rfasst script before re-running anything.\n")
} else cat("[ok] all ten present\n")

cat("\n--- fasst_to_r10 ---\n")
cat("rows:", nrow(fasst_to_r10),
    "| distinct fasst_region:", n_distinct(fasst_to_r10$fasst_region),
    "| distinct r10_region:", n_distinct(fasst_to_r10$r10_region), "\n")
cat("columns:", paste(names(fasst_to_r10), collapse = ", "), "\n\n")
print(as.data.frame(fasst_to_r10 %>% count(r10_region)))

cat("\nfasst regions mapping to MORE THAN ONE R10 region (should be none):\n")
d <- fasst_to_r10 %>% count(fasst_region) %>% filter(n > 1)
print(if (nrow(d)) as.data.frame(d) else "  none")

cat("\nR10 regions in COMPASS_R10_REGIONS but MISSING from fasst_to_r10:\n")
miss <- setdiff(COMPASS_R10_REGIONS, unique(fasst_to_r10$r10_region))
print(if (length(miss)) miss else "  none")

cat("\nAny NA in the mapping:", sum(is.na(fasst_to_r10$r10_region)), "\n")

cat("\n--- what fasst_weights says (this one worked in the arm test) ---\n")
cat("rows:", nrow(fasst_weights),
    "| distinct r10_region:", n_distinct(fasst_weights$r10_region), "\n")
print(as.data.frame(fasst_weights %>% count(r10_region)))

cat("\n--- do the two mappings agree? ---\n")
a <- fasst_to_r10 %>% select(fasst_region, r10_region) %>% arrange(fasst_region)
b <- fasst_weights %>% distinct(fasst_region, r10_region) %>% arrange(fasst_region)
cat("identical:", isTRUE(all.equal(as.data.frame(a), as.data.frame(b))), "\n")
only_b <- anti_join(b, a, by = c("fasst_region","r10_region"))
if (nrow(only_b)) {
  cat("pairs in fasst_weights but NOT in fasst_to_r10:\n")
  print(as.data.frame(only_b))
}
cat("\nSend this whole output back.\n")
