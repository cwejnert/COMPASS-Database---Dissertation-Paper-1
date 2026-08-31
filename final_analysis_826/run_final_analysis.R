run <- function(path) {
  cat("\n===", path, "===\n")
  status <- system2(file.path(R.home("bin"), "Rscript"), path)
  if (status != 0) stop("Failed: ", path)
}

truthy <- function(name) {
  tolower(Sys.getenv(name, "false")) %in% c("true", "1", "yes")
}

required <- c(
  "LAND_PRIMARY.rds",
  "master_outputs/approach_A/compass_master_dataset_A.rds",
  "master_outputs/approach_C/compass_master_dataset_C.rds",
  "final_outcomes/jobs_revision/compass_jobs_cumulative_2020_2100.rds",
  "final_outcomes/jobs_revision/compass_jobs_retirements_decommissioning_2020_2100.csv",
  "final_outcomes/mortality_472/compass_mortality_r10_allcdr_reporting_complete_472.csv"
)
missing <- required[!file.exists(required)]
if (length(missing)) {
  stop("Missing required input(s): ", paste(missing, collapse = ", "))
}

Sys.setenv(
  COMPASS_FINAL_ANALYSIS_DIR = normalizePath(".", winslash = "/"),
  COMPASS_OUTCOME_WINDOW_END = "2100",
  COMPASS_REVISED_JOBS_DIR = "final_outcomes/jobs_revision",
  COMPASS_REVISED_JOBS_RESULTS = "final_outcomes/jobs_revision"
)

if (truthy("COMPASS_REBUILD_PRIMARY")) {
  run("paper1_analysis/V5_land_primary.R")
}

if (truthy("COMPASS_REBUILD_JOBS")) {
  Sys.setenv(
    COMPASS_OUT_DIR = normalizePath(
      "final_outcomes/jobs_revision", winslash = "/", mustWork = FALSE
    ),
    COMPASS_JOBS_ONLY = "1"
  )
  run("analysis_scripts/COMPASS_master_analysis_allR10.R")
  Sys.unsetenv(c("COMPASS_OUT_DIR", "COMPASS_JOBS_ONLY"))
}

# The expensive targeted RFASST run is optional. The validated 472-scenario
# release is frozen in final_outcomes/mortality_472 for ordinary reproduction.
if (truthy("COMPASS_RUN_TARGETED_RFASST")) {
  run("paper1_analysis/W26_targeted_rfasst_rerun.R")
}

run("paper1_analysis/W27_regenerate_mortality_472.R")
run("paper1_analysis/W19_jobs_retirement_decommissioning.R")
run("paper1_analysis/W29_employment_family_mechanisms.R")
run("paper1_analysis/W28_integrated_tiered_analysis.R")

cat(
  "\nFinal 2020-2100 analysis complete. Jobs were ",
  ifelse(truthy("COMPASS_REBUILD_JOBS"), "rebuilt", "read from the frozen compact release"),
  "; targeted RFASST was ",
  ifelse(truthy("COMPASS_RUN_TARGETED_RFASST"), "rerun", "not rerun"),
  ".\n",
  sep = ""
)
