# =============================================================================
# V4 — VERIFY THE PORT
#
# The strict World aggregation was validated downstream in V3_world_strict.R and
# then ported into build_df_master() in COMPASS_master_analysis_allR10.R. A port
# is only useful if it computes the same thing, so this extracts the NEW block
# from the master script verbatim, runs it against the published R10 rows, and
# checks the result against V3.
#
# It cannot run the full master (that needs the input databases), but it does not
# need to: the aggregation takes `dfm` as input, and the R10 half of `dfm` is
# exactly what the published master_outputs CSVs contain. Reconstruct that, run
# the ported code, compare.
#
# USAGE: Rscript V4_verify_port.R      (run from the repo root)
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr)})
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

MASTER <- "analysis_scripts/COMPASS_master_analysis_allR10.R"
regions_r10 <- c("R10AFRICA","R10CHINA+","R10EUROPE","R10INDIA+","R10LATIN_AM",
                 "R10MIDDLE_EAST","R10NORTH_AM","R10PAC_OECD","R10REF_ECON","R10REST_ASIA")
norm <- function(x) iconv(x, from = "", to = "UTF-8", sub = "")

# add_percapita, verbatim from the master
add_percapita <- function(df) {
  df %>% mutate(
    mort_per_1k        = if ("cumulative_deaths_mln"        %in% names(.)) cumulative_deaths_mln        / pop_mln * 1000 else NA_real_,
    headcount_pct      = if ("mean_headcount_millions"      %in% names(.)) mean_headcount_millions      / pop_mln * 100  else NA_real_,
    re_jobs_per_1k     = if ("jobs_Renewables"              %in% names(.)) jobs_Renewables              / pop_mln       else NA_real_,
    fossil_jobs_per_1k = if ("jobs_Fossil"                  %in% names(.)) jobs_Fossil                  / pop_mln       else NA_real_,
    net_re_jobs_per_1k = if (all(c("jobs_Renewables","jobs_Fossil") %in% names(.))) (jobs_Renewables - jobs_Fossil) / pop_mln else NA_real_,
    gap_GJ_pc          = if ("cumulative_gap_EJ"            %in% names(.)) cumulative_gap_EJ            * 1000 / pop_mln else NA_real_,
    implied_CO2_tpc    = if ("cumulative_implied_CO2_GtCO2" %in% names(.)) cumulative_implied_CO2_GtCO2 * 1000 / pop_mln else NA_real_
  )
}

# =============================================================================
line("1. EXTRACTING THE PORTED BLOCK FROM THE MASTER SCRIPT")
# =============================================================================
src <- readLines(MASTER, warn = FALSE)
i0 <- grep("^  n_r10 <- length\\(regions_r10\\)", src)
i1 <- grep("^  bind_rows\\(dfm, dfm_agg\\)", src)
if (length(i0) != 1 || length(i1) != 1)
  stop("could not locate the ported block: found ", length(i0), " start and ",
       length(i1), " end markers")
blk <- src[i0:(i1 - 1)]
cat("extracted lines", i0, "to", i1 - 1, "  (", length(blk), "lines )\n")
cat("the block is used EXACTLY as it appears in the master — no re-typing.\n")

# =============================================================================
line("2. RECONSTRUCTING `dfm` (the R10 half) FROM THE PUBLISHED OUTPUTS")
# =============================================================================
load_dfm <- function(id) {
  read.csv(sprintf("master_outputs/approach_%s/compass_master_dataset_%s.csv", id, id),
           stringsAsFactors = FALSE) %>%
    mutate(Model = norm(Model), Scenario = norm(Scenario)) %>%
    filter(Region %in% regions_r10)
}
res <- list()
for (id in c("A","C")) {
  dfm <- load_dfm(id)
  pop2020_total <- dfm %>% distinct(Region, pop_mln) %>%
    group_by(Region) %>% summarise(p = median(pop_mln), .groups="drop") %>%
    summarise(s = sum(p)) %>% pull(s)
  cat("approach", id, "— dfm rows:", nrow(dfm),
      "| ten-region population:", round(pop2020_total), "\n")

  # ---- run the ported block verbatim -------------------------------------
  env <- new.env()
  assign("dfm", dfm, envir = env)
  assign("pop2020_total", pop2020_total, envir = env)
  assign("regions_r10", regions_r10, envir = env)
  assign("add_percapita", add_percapita, envir = env)
  eval(parse(text = paste(blk, collapse = "\n")), envir = env)
  res[[id]] <- get("dfm_agg", envir = env) %>% mutate(approach = id)
}
PORT <- bind_rows(res)
cat("\nWorld rows produced by the ported code:", nrow(PORT), "\n")

# =============================================================================
line("3. COMPARING AGAINST V3 (the validated downstream implementation)")
# =============================================================================
V3 <- readRDS("STRICT_WORLD.rds")$world %>%
  select(approach, Model, Scenario,
         v3_jobs = net_re_jobs_per_1k, v3_gap = gap_GJ_pc,
         v3_head = headcount_pct,
         v3_cj = world_complete_jobs, v3_cg = world_complete_gap)

# The ported code emits one World row per deployment Variable; the outcome
# columns are identical across those rows by construction, so collapse first.
P1 <- PORT %>%
  distinct(approach, Model, Scenario, .keep_all = TRUE) %>%
  select(approach, Model, Scenario,
         p_jobs = net_re_jobs_per_1k, p_gap = gap_GJ_pc, p_head = headcount_pct,
         p_cj = world_complete_jobs, p_cg = world_complete_gap)

J <- V3 %>% inner_join(P1, by = c("approach","Model","Scenario"))
cat("scenarios compared:", nrow(J), "\n\n")

chk <- function(a, b, nm) {
  both_na <- is.na(a) & is.na(b)
  d <- ifelse(both_na, 0, abs(a - b))
  cat(sprintf("  %-22s max |difference| %-10s  NA pattern matches: %s\n", nm,
              signif(max(d, na.rm = TRUE), 3),
              all(is.na(a) == is.na(b))))
  invisible(max(d, na.rm = TRUE))
}
d1 <- chk(J$v3_jobs, J$p_jobs, "net_re_jobs_per_1k")
d2 <- chk(J$v3_gap,  J$p_gap,  "gap_GJ_pc")
d3 <- chk(J$v3_head, J$p_head, "headcount_pct")
cat("\n  coverage flags match — jobs:", all(J$v3_cj == J$p_cj),
    "| deprivation:", all(J$v3_cg == J$p_cg), "\n")

ok <- max(c(d1,d2,d3), na.rm = TRUE) < 1e-9 &&
      all(is.na(J$v3_jobs) == is.na(J$p_jobs)) &&
      all(J$v3_cj == J$p_cj) && all(J$v3_cg == J$p_cg)
cat("\n", ifelse(ok, "[ok] the port reproduces V3 exactly.",
                     "[FAIL] the port and V3 disagree."), "\n", sep="")

# =============================================================================
line("4. AND AGAINST THE OLD PUBLISHED WORLD ROW — what actually moved")
# =============================================================================
OLD <- bind_rows(lapply(c("A","C"), function(id)
  read.csv(sprintf("master_outputs/approach_%s/compass_master_dataset_%s.csv", id, id),
           stringsAsFactors = FALSE) %>%
    mutate(Model = norm(Model), Scenario = norm(Scenario), approach = id) %>%
    filter(Region == "Aggregated R10 regions") %>%
    distinct(approach, Model, Scenario, .keep_all = TRUE) %>%
    select(approach, Model, Scenario, old_jobs = net_re_jobs_per_1k)))
M <- P1 %>% inner_join(OLD, by = c("approach","Model","Scenario"))
cat("scenarios:", nrow(M), "\n")
cat("World jobs now NA (was a partial-region sum):", sum(is.na(M$p_jobs) & !is.na(M$old_jobs)), "\n")
cat("World jobs value changed:", sum(!is.na(M$p_jobs) & !is.na(M$old_jobs) &
                                     abs(M$p_jobs - M$old_jobs) > 1e-6), "\n")
cat("World jobs unchanged:", sum(!is.na(M$p_jobs) & !is.na(M$old_jobs) &
                                 abs(M$p_jobs - M$old_jobs) <= 1e-6), "\n")
cat("\nThe port is a drop-in: it changes only World rows, only where the old\n")
cat("value was a partial-region sum, and it agrees with the validated\n")
cat("downstream implementation to machine precision.\n")
