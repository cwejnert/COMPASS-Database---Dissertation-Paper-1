# =============================================================================
# check_sci_gw_column.R  —  RUN LOCALLY (instant, metadata only)
#
# QUESTION: does compass_r10_meta.csv already carry a native SCI "Global
#   Warming" category (GW1-GW8), separate from the AR6 C1-C8 category we
#   currently use? SCI's 2025 release introduced this scheme as a replacement
#   for AR6 categories; if the column is in our metadata already, it is a
#   better-sourced ambition rule than our self-chosen 1.7/2.0 peak-warming
#   cutoffs -- no digitizing a paywalled paper required.
#
# OUTPUT: prints every column name containing "GW" or "Global Warming" or
#   "SCI", plus a sample of values, so we can tell at a glance whether it's
#   there and what it looks like.
# =============================================================================
COMPASS_DIR <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/COMPASS"
META <- file.path(COMPASS_DIR, "compass_r10_meta.csv")
stopifnot(file.exists(META))

meta <- read.csv(META, stringsAsFactors = FALSE, check.names = FALSE)
cat("total columns:", ncol(meta), "\n\n")

hits <- names(meta)[grepl("GW[0-9]|Global Warming|SCI|Category", names(meta), ignore.case = TRUE)]
cat("=== candidate columns ===\n")
print(hits)

for (col in hits) {
  vals <- unique(meta[[col]])
  vals <- vals[!is.na(vals) & vals != ""]
  cat("\n---", col, "---\n")
  cat("n distinct:", length(vals), "\n")
  print(head(sort(vals), 15))
}

cat("\n=== full column list (in case the GW column has an unexpected name) ===\n")
print(names(meta))
