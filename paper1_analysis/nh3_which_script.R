# =============================================================================
# WHICH rfasst SCRIPT IS BEING USED? — reads files only, runs nothing.
#
# The console text itself changed between runs, which means the SCRIPT changed,
# not the data:
#
#   during the probe:  "RUN_ALL_SCENARIOS: TRUE (TRUE = all emissions scenarios,
#                       no category pre-filter)"   Category table had <NA> 596
#                       Scenarios to run: 1543   em_clean rows: 714400
#
#   just now:          "RUN_ALL_SCENARIOS: TRUE"
#                       Category table had NO <NA> row
#                       Scenarios to run: 947    em_clean rows: 357640
#
# em_clean halved while the SCENARIO count stayed at 1143, so the loss is rows
# per scenario, and the input file is byte-identical (1,300,316 rows both times).
# Patch 01d also wrote .bak_nh3_<timestamp> backups, so a restored or duplicate
# copy is the obvious suspect.
#
# USAGE: setwd() to the folder holding the rfasst script, then
#   source("nh3_which_script.R")
# =============================================================================
cat("working directory:\n  ", getwd(), "\n\n")

f <- list.files(".", pattern = "rfasst", ignore.case = TRUE)
if (!length(f)) stop("no file with 'rfasst' in the name here")
info <- file.info(f)
cat("--- every rfasst-ish file here ---\n")
print(data.frame(file = f, bytes = info$size, modified = format(info$mtime)),
      row.names = FALSE)

CANDIDATES <- c("COMPASS_rfasst_full_allR10.R", "COMPASS_rfasst_full.R")
USED <- CANDIDATES[file.exists(CANDIDATES)][1]
cat("\nthe scripts pick up:", USED, "\n")

s <- readLines(USED, warn = FALSE)
cat("lines:", length(s), "  (the probe reported 1363)\n")

cat("\n--- COMPASS_R10_REGIONS as written in the file ---\n")
i <- grep("^\\s*COMPASS_R10_REGIONS\\s*<-", s)
if (!length(i)) cat("  NOT FOUND\n") else {
  j <- i[1]; k <- j
  while (k < length(s) && !grepl("\\)\\s*$", s[k])) k <- k + 1
  cat(paste(s[j:k], collapse = "\n"), "\n")
  cat("R10 codes on those lines:",
      length(unlist(regmatches(paste(s[j:k], collapse=" "),
             gregexpr('"R10[^"]*"', paste(s[j:k], collapse=" "))))), "\n")
}

cat("\n--- the em_clean region filter ---\n")
i <- grep("region\\s*%in%\\s*c\\(COMPASS_R10_REGIONS", s)
if (length(i)) for (x in i) cat("  line", x, ":", trimws(s[x]), "\n") else
  cat("  NOT FOUND - the filter may have been rewritten\n")

cat("\n--- the RUN_ALL_SCENARIOS line (this is what visibly differs) ---\n")
i <- grep("RUN_ALL_SCENARIOS", s)
for (x in i) cat("  line", x, ":", trimws(s[x]), "\n")

cat("\n--- any year filter on em_clean ---\n")
i <- grep("year\\s*%in%\\s*FASST_YEARS|filter\\(.*year\\s*[<>=]", s)
for (x in head(i, 6)) cat("  line", x, ":", trimws(s[x]), "\n")

cat("\n--- backups patch 01d may have left ---\n")
b <- list.files(".", pattern = "\\.bak")
if (length(b)) {
  bi <- file.info(b)
  print(data.frame(file = b, bytes = bi$size, modified = format(bi$mtime)),
        row.names = FALSE)
  cat("\nIf one of these is 1363 lines and the live script is not, the live\n")
  cat("script is a different version and should be restored from the backup.\n")
} else cat("  none\n")

cat("\nSend this whole output back.\n")
