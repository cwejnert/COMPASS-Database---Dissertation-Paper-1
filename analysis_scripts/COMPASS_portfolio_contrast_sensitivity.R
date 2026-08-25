# COMPASS Paper 1: opposing-portfolio sensitivity and within-model diagnostic
#
# Purpose
# -------
# The primary paper labels pathways High-CMT when total CDR is in the top
# tercile and renewable capacity is *not* in the top tercile; the converse
# defines High-RE.  This script adds two deliberately stricter diagnostics:
#
# 1. Global polar archetypes: top-tercile focal deployment AND bottom-tercile
#    opposing deployment, calculated separately within each ambition band.
# 2. Within-model polar archetypes: the same rule, but terciles are calculated
#    separately within Model x ambition.  This answers a different question:
#    does an IAM family that spans both portfolio extremes show the same
#    outcome direction internally?
#
# These are construct-validity / identification diagnostics.  They are not
# replacements for the frozen primary release unless they retain sufficient
# same-model overlap and their estimand is explicitly changed in the paper.
#
# Inputs are the frozen Approach-A output.  This script is isolated and never
# overwrites the primary release.

options(stringsAsFactors = FALSE)

MASTER_OUT <- Sys.getenv(
  "COMPASS_MASTER_OUT",
  "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_master"
)
OUT_DIR <- Sys.getenv(
  "COMPASS_PORTFOLIO_SENS_OUT",
  file.path(MASTER_OUT, "portfolio_contrast_sensitivity")
)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

approach_dir <- file.path(MASTER_OUT, "approach_A")
pathway_file <- file.path(approach_dir, "compass_pathway_tercile_A.rds")
master_file  <- file.path(approach_dir, "compass_master_dataset_A.rds")
if (!file.exists(pathway_file) || !file.exists(master_file)) {
  stop("Expected frozen Approach-A RDS files are missing under: ", approach_dir)
}

pathways <- readRDS(pathway_file)
master   <- readRDS(master_file)

id <- c("Model", "Scenario", "Category", "Ambition")
pathways <- pathways[complete.cases(pathways[, c(id, "total_cdr", "total_re")]), ]

quantile_or_na <- function(x, p) {
  if (sum(is.finite(x)) < 3L) return(NA_real_)
  as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE))
}

add_polar_labels <- function(d, by_columns, prefix, use_existing_high = FALSE) {
  key <- interaction(d[, by_columns], drop = TRUE, lex.order = TRUE)
  d$cdr_low  <- ave(d$total_cdr, key, FUN = function(x) quantile_or_na(x, 1 / 3))
  d$re_low   <- ave(d$total_re,  key, FUN = function(x) quantile_or_na(x, 1 / 3))
  if (use_existing_high) {
    # Preserve the frozen paper's top-tercile membership exactly.  Only the
    # newly added opposing-axis lower ceiling is calculated here.
    d$cdr_high <- d$cdr_thresh
    d$re_high <- d$re_thresh
  } else {
    d$cdr_high <- ave(d$total_cdr, key, FUN = function(x) quantile_or_na(x, 2 / 3))
    d$re_high  <- ave(d$total_re,  key, FUN = function(x) quantile_or_na(x, 2 / 3))
  }

  label <- ifelse(
    d$total_cdr >= d$cdr_high & d$total_re <= d$re_low,
    "CMT-high_RE-low",
    ifelse(
      d$total_re >= d$re_high & d$total_cdr <= d$cdr_low,
      "RE-high_CMT-low",
      NA_character_
    )
  )
  d[[prefix]] <- label
  d[, setdiff(names(d), c("cdr_low", "cdr_high", "re_low", "re_high"))]
}

global_polar <- add_polar_labels(pathways, "Ambition", "global_polar", use_existing_high = TRUE)
within_polar <- add_polar_labels(pathways, c("Model", "Ambition"), "within_model_polar")
labels <- merge(
  global_polar[, c(id, "global_polar")],
  within_polar[, c(id, "within_model_polar")],
  by = id, all = TRUE
)
write.csv(labels, file.path(OUT_DIR, "portfolio_contrast_membership.csv"), row.names = FALSE)

# Outcomes are repeated once for each deployment variable.  Collapse each
# scenario-region to one row while preserving a missing outcome as missing.
first_finite <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) x[[1L]] else NA_real_
}

world_label <- "Aggregated R10 regions"
if (!any(master$Region == world_label)) {
  stop("World aggregate 'Aggregated R10 regions' is absent.")
}

outcome_key <- interaction(master[, c(id, "Region")], drop = TRUE, lex.order = TRUE)
outcomes <- data.frame(
  Model = tapply(master$Model, outcome_key, function(x) x[[1L]]),
  Scenario = tapply(master$Scenario, outcome_key, function(x) x[[1L]]),
  Category = tapply(master$Category, outcome_key, function(x) x[[1L]]),
  Ambition = tapply(master$Ambition, outcome_key, function(x) x[[1L]]),
  Region = tapply(master$Region, outcome_key, function(x) x[[1L]]),
  REFOSS = tapply(master$net_re_jobs_per_1k, outcome_key, first_finite),
  gap_GJ_pc = tapply(master$gap_GJ_pc, outcome_key, first_finite),
  mort_per_1k = tapply(master$mort_per_1k, outcome_key, first_finite),
  row.names = NULL,
  check.names = FALSE
)
rownames(outcomes) <- NULL
outcomes <- merge(outcomes, labels, by = id, all.x = TRUE)

outcome_names <- c("REFOSS", "gap_GJ_pc", "mort_per_1k")

summarise_groups <- function(d, group_col, split_by = c("Ambition")) {
  d <- d[!is.na(d[[group_col]]), ]
  if (!nrow(d)) return(data.frame())
  results <- list()
  at <- 1L
  split_key <- interaction(d[, split_by], drop = TRUE, lex.order = TRUE)
  for (group_data in split(d, split_key)) {
    for (outcome in outcome_names) {
      cmt <- group_data[group_data[[group_col]] == "CMT-high_RE-low", outcome]
      re  <- group_data[group_data[[group_col]] == "RE-high_CMT-low", outcome]
      cmt <- cmt[is.finite(cmt)]
      re  <- re[is.finite(re)]
      results[[at]] <- data.frame(
        group_data[1L, split_by, drop = FALSE],
        outcome = outcome,
        n_cmt = length(cmt),
        n_re = length(re),
        median_cmt = if (length(cmt)) median(cmt) else NA_real_,
        median_re = if (length(re)) median(re) else NA_real_,
        re_minus_cmt = if (length(cmt) && length(re)) median(re) - median(cmt) else NA_real_,
        stringsAsFactors = FALSE
      )
      at <- at + 1L
    }
  }
  do.call(rbind, results)
}

global_summary <- summarise_groups(outcomes, "global_polar", split_by = c("Region", "Ambition"))
write.csv(global_summary, file.path(OUT_DIR, "global_polar_all_regions_medians.csv"), row.names = FALSE)

# Keep only IAM x ambition cells that contain both polar portfolios.  This is
# the estimable within-model subset; raw model coverage alone is not enough.
within_membership <- outcomes[!is.na(outcomes$within_model_polar), ]
coverage_key <- interaction(within_membership$Model, within_membership$Ambition,
                            drop = TRUE, lex.order = TRUE)
coverage_input <- within_membership[within_membership$Region == world_label, ]
coverage_key <- interaction(coverage_input$Model, coverage_input$Ambition,
                            drop = TRUE, lex.order = TRUE)
coverage <- do.call(rbind, lapply(split(coverage_input, coverage_key), function(x) {
  data.frame(
    Model = x$Model[[1L]],
    Ambition = x$Ambition[[1L]],
    n_cmt = sum(x$within_model_polar == "CMT-high_RE-low"),
    n_re = sum(x$within_model_polar == "RE-high_CMT-low"),
    estimable_within_model = all(c("CMT-high_RE-low", "RE-high_CMT-low") %in% x$within_model_polar),
    stringsAsFactors = FALSE
  )
}))
write.csv(coverage, file.path(OUT_DIR, "within_model_polar_coverage.csv"), row.names = FALSE)

within_estimable <- merge(
  within_membership,
  coverage[coverage$estimable_within_model, c("Model", "Ambition")],
  by = c("Model", "Ambition")
)
within_by_model <- summarise_groups(
  within_estimable,
  "within_model_polar",
  split_by = c("Model", "Region", "Ambition")
)
write.csv(within_by_model, file.path(OUT_DIR, "within_model_polar_all_regions_medians.csv"), row.names = FALSE)

# Unweighted median of the estimable within-model median differences prevents
# scenario-rich IAMs from defining this diagnostic's headline.
within_unweighted <- aggregate(
  re_minus_cmt ~ Region + Ambition + outcome,
  data = within_by_model,
  FUN = function(x) c(
    n_models = sum(is.finite(x)),
    median_model_difference = median(x, na.rm = TRUE),
    p25 = quantile(x, 0.25, na.rm = TRUE),
    p75 = quantile(x, 0.75, na.rm = TRUE)
  )
)
write.csv(
  within_unweighted,
  file.path(OUT_DIR, "within_model_polar_all_regions_unweighted_summary.csv"),
  row.names = FALSE
)

message("Wrote opposing-portfolio sensitivity outputs to: ", OUT_DIR)
message("Global polar labels: ", paste(table(labels$global_polar, useNA = "ifany"), collapse = "; "))
message("Within-model estimable cells: ", sum(coverage$estimable_within_model),
        " of ", nrow(coverage))
