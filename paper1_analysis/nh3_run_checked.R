# =============================================================================
# NH3 SENSITIVITY RUN — hardened replacement for 03_nh3_run.R
#
# WHAT WENT WRONG THE FIRST TIME. The original step 4 was:
#
#     if (ok && file.exists(p("compass_mortality_summary.rds"))) {
#       file.copy(p("compass_mortality_summary.rds"),
#                 p("compass_mortality_summary_noNH3.rds"), overwrite = TRUE)
#     }
#
# `ok` is only FALSE if source() threw. But the rfasst loop wraps every scenario
# in tryCatch, so a run in which nothing succeeded still returns cleanly. And
# step 1 has already copied the MAIN outputs into place. So if the run produced
# nothing new, step 4 copies the UNTOUCHED MAIN FILE to the _noNH3 name and
# reports success. That is exactly how two byte-identical files appear.
#
# THIS VERSION CANNOT DO THAT. It:
#   * records the size, mtime and md5 of the summary BEFORE the run
#   * asserts DROP_NH3 is actually TRUE in the file it is about to source
#   * asserts NH3 is absent from em_clean once the run has loaded it
#   * after the run, REFUSES to write _noNH3 if the summary is unchanged
#   * restores the MAIN outputs whether or not the run succeeded
#
# RUN nh3_probe.R FIRST. It answers in ~10 minutes whether NH3 moves mortality
# at all. If it does not, this 90-minute run has nothing to find and you should
# not spend the time.
#
# USAGE: setwd() to the folder holding the rfasst script, then
#   source("nh3_run_checked.R")
# =============================================================================
suppressPackageStartupMessages({library(dplyr)})

CANDIDATES  <- c("COMPASS_rfasst_full_allR10.R", "COMPASS_rfasst_full.R")
RFASST      <- CANDIDATES[file.exists(CANDIDATES)][1]
COMPASS_DIR <- Sys.getenv("COMPASS_DIR",
                 "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/COMPASS")
if (is.na(RFASST)) stop("No rfasst script found in ", getwd())
if (!dir.exists(COMPASS_DIR)) stop("COMPASS_DIR not found: ", COMPASS_DIR)
p <- function(f) file.path(COMPASS_DIR, f)

have_md5 <- requireNamespace("tools", quietly = TRUE)
fp <- function(f) {
  if (!file.exists(f)) return(NA_character_)
  paste(file.info(f)$size,
        if (have_md5) unname(tools::md5sum(f)) else format(file.info(f)$mtime))
}

MAIN <- c("compass_mortality_summary.rds", "compass_mortality_r10.csv",
          "compass_mortality_coverage.rds")

flip <- function(to) {
  txt <- readLines(RFASST, warn = FALSE)
  i   <- grep("^DROP_NH3 <- (TRUE|FALSE)\\s*$", txt)
  if (length(i) != 1)
    stop("no unique 'DROP_NH3 <- ...' line found; apply patch 01d first")
  txt[i] <- paste0("DROP_NH3 <- ", to)
  writeLines(txt, RFASST)
  chk <- grep(paste0("^DROP_NH3 <- ", to, "\\s*$"), readLines(RFASST, warn = FALSE))
  if (!length(chk)) stop("wrote DROP_NH3 <- ", to, " but it did not stick")
  cat("  DROP_NH3 = ", to, " (line ", i, ", verified on re-read)\n", sep = "")
}

restore <- function() {
  cat("\n=== restoring the MAIN outputs ===\n")
  for (f in MAIN) {
    bak <- p(sub("\\.(rds|csv)$", "_MAIN.\\1", f))
    if (file.exists(bak)) { file.copy(bak, p(f), overwrite = TRUE)
                            cat("  restored", f, "\n") }
  }
  flip("FALSE")
}

# ---- 1. back up, and fingerprint what we are about to replace --------------
cat("\n=== 1. backing up the MAIN outputs ===\n")
before <- setNames(vapply(MAIN, function(f) fp(p(f)), character(1)), MAIN)
for (f in MAIN) {
  if (!file.exists(p(f))) { cat("  [!] missing:", f, "\n"); next }
  file.copy(p(f), p(sub("\\.(rds|csv)$", "_MAIN.\\1", f)), overwrite = TRUE)
  cat("  saved", f, "\n")
}
if (is.na(before[["compass_mortality_summary.rds"]]))
  stop("compass_mortality_summary.rds does not exist. Do the MAIN run first.")
cat("  fingerprint before:", before[["compass_mortality_summary.rds"]], "\n")

# ---- 2. flip, and assert it took ------------------------------------------
cat("\n=== 2. setting DROP_NH3 ===\n")
flip("TRUE")

# ---- 3. run ---------------------------------------------------------------
cat("\n=== 3. running rfasst without NH3 (30-90 min) ===\n")
ok <- tryCatch({ source(RFASST); TRUE },
               error = function(e) { cat("\nRUN FAILED:", conditionMessage(e), "\n"); FALSE })

# did the flag reach the data?
if (ok && exists("em_clean")) {
  n_nh3 <- sum(em_clean$pollutant == "NH3")
  cat("\n  NH3 rows in em_clean during the run:", n_nh3, "\n")
  if (n_nh3 > 0) {
    cat("  [FAIL] NH3 was still present. DROP_NH3 did not reach pollutant_map.\n")
    ok <- FALSE
  } else cat("  [ok]   NH3 excluded from em_clean\n")
}

# ---- 4. only write _noNH3 if something actually changed --------------------
cat("\n=== 4. checking the output actually changed ===\n")
after <- fp(p("compass_mortality_summary.rds"))
cat("  fingerprint after: ", after, "\n")
if (!ok) {
  cat("  [FAIL] run did not complete cleanly - writing nothing.\n")
} else if (identical(after, before[["compass_mortality_summary.rds"]])) {
  cat("  [FAIL] the summary is UNCHANGED. This is the failure mode that\n")
  cat("         produced two identical files last time. Writing nothing.\n")
  cat("         Run nh3_probe.R to find out whether NH3 moves mortality at\n")
  cat("         all before spending another 90 minutes here.\n")
} else {
  file.copy(p("compass_mortality_summary.rds"),
            p("compass_mortality_summary_noNH3.rds"), overwrite = TRUE)
  if (file.exists(p("compass_mortality_r10.csv")))
    file.copy(p("compass_mortality_r10.csv"),
              p("compass_mortality_r10_noNH3.csv"), overwrite = TRUE)
  cat("  [ok]   wrote compass_mortality_summary_noNH3.rds\n")
  cat("  [ok]   wrote compass_mortality_r10_noNH3.csv\n")
}

# ---- 5. always restore ----------------------------------------------------
restore()
cat("\n=== DONE ===\n")
cat("Send back compass_mortality_summary_noNH3.rds and\n")
cat("compass_mortality_r10_noNH3.csv IF step 4 said [ok]. If it said [FAIL],\n")
cat("send the console output instead - the run, not the analysis, is the problem.\n")
