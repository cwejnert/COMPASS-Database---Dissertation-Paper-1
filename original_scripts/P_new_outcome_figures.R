# =============================================================================
# NEW OUTCOME FIGURES — COMPASS only
# Three main outcomes, population-normalised, per ambition level:
#   1. Air pollution mortality    → deaths per million 2020 population
#   2. Energy deprivation         → % of 2020 population below DLS
#   3. Net RE jobs (RE − Fossil)  → thousands of jobs per million 2020 pop
#
# For each outcome:
#   Fig A — aggregated across all R10 regions (single violin per pathway)
#   Fig B — faceted by 5 R10 regions
#
# Triangle / ternary chart (separate for 1.5C and 2C):
#   Individual scenario points coloured by pathway + median markers
#   Axes: mortality (per million), headcount (%), net RE jobs (per million)
#
# RUN AFTER: paper_1_figs_combined_CW_3_23.R
# Requires: df_master, pathway_tercile, pop_ts, sc(), theme_c(), regions_r10,
#           REGION_LABELS_FIG, COMPASS_DIR, FIG_OUT, add_pathway()
# =============================================================================

library(tidyverse)
library(scales)
library(ggtern)       # for ternary chart

cat("\n=== NEW PER-CAPITA OUTCOME FIGURES ===\n")

# Load threshold classification if not already in environment
if (!exists("world_cum_thresh")) {
  if (!exists("pathway_threshold")) {
    pathway_threshold <- readRDS(file.path(COMPASS_DIR, "compass_pathway_threshold.rds"))
  }
  world_cum_thresh <- pathway_threshold %>%
    select(Model, Scenario, Category, Ambition,
           total_cdr, total_re, high_cdr_only, high_re_only,
           Pathway_overlap, Pathway_excl, threshold_label)
  cat("world_cum_thresh loaded:", nrow(world_cum_thresh), "rows\n")
}

if (!exists("add_pathway_thresh")) {
  add_pathway_thresh <- function(df) {
    df %>%
      left_join(world_cum_thresh %>%
                  select(Model, Scenario, high_cdr_only, high_re_only),
                by = c("Model", "Scenario")) %>%
      filter(!is.na(high_cdr_only), high_cdr_only | high_re_only) %>%
      assign_amb("Category") %>%
      filter(!is.na(Ambition)) %>%
      mutate(Pathway = if_else(high_cdr_only, "High-CDR", "High-RE"))
  }
}

# ── 1. Build 2020 population lookup ──────────────────────────────────────────
# Median 2020 population per region across all scenarios (millions)
pop_2020_r10 <- pop_ts %>%
  filter(Region %in% regions_r10, Year == 2020) %>%
  group_by(Region) %>%
  summarise(pop_2020_mln = median(Value, na.rm = TRUE), .groups = "drop")

# Aggregated: sum across the 5 R10 regions
pop_2020_agg <- sum(pop_2020_r10$pop_2020_mln)

cat("2020 population by region (millions):\n")
print(pop_2020_r10)
cat("Aggregated 5-region total:", round(pop_2020_agg, 0), "million\n")

# ── 2. Build normalised outcome dataset ──────────────────────────────────────
# Start from df_master (already has Aggregated R10 row via pop-weighted mean)
# Filter to Total CDR rows (one row per scenario × region)
# Join pathway classification
# Compute per-capita metrics

# Per-region raw outcomes (needs regional pop for normalisation)
base_regional <- df_master %>%
  filter(Variable == "Total CDR", Region %in% regions_r10) %>%
  left_join(
    world_cum_f2 %>%
      select(Model, Scenario, Category, high_cdr_only, high_re_only),
    by = c("Model", "Scenario", "Category")
  ) %>%
  filter(!is.na(high_cdr_only), high_cdr_only | high_re_only) %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition)) %>%
  mutate(Pathway = if_else(high_cdr_only, "High-CDR", "High-RE")) %>%
  left_join(pop_2020_r10, by = "Region") %>%
  mutate(
    # deaths per million 2020 population (cumulative_deaths_mln is already in millions)
    mort_per_mln      = (cumulative_deaths_mln * 1e6) / pop_2020_mln,
    # headcount as % of 2020 population
    headcount_pct     = (mean_headcount_millions / pop_2020_mln) * 100,
    # jobs per million 2020 population (jobs in thousands / pop millions)
    re_jobs_per_mln     = jobs_Renewables / pop_2020_mln,
    fossil_jobs_per_mln = jobs_Fossil     / pop_2020_mln,
    total_jobs_per_mln  = (jobs_Renewables + jobs_Fossil) / pop_2020_mln,
    net_re_jobs_per_mln = ((jobs_Renewables - jobs_Fossil) / pop_2020_mln),
    Region_label      = REGION_LABELS_FIG[Region],
    PathAmb           = paste0(Pathway, "\n", if_else(str_detect(Ambition,"1.5C"),"1.5C","2C"))
  ) %>%
  filter(!is.na(mort_per_mln) | !is.na(headcount_pct) | !is.na(net_re_jobs_per_mln))

# Aggregated across R10 — sum raw outcomes then normalise by total pop
# (summing deaths/headcount is additive; jobs are additive)
base_agg <- df_master %>%
  filter(Variable == "Total CDR", Region %in% regions_r10) %>%
  left_join(
    world_cum_f2 %>%
      select(Model, Scenario, Category, high_cdr_only, high_re_only),
    by = c("Model", "Scenario", "Category")
  ) %>%
  filter(!is.na(high_cdr_only), high_cdr_only | high_re_only) %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition)) %>%
  mutate(Pathway = if_else(high_cdr_only, "High-CDR", "High-RE")) %>%
  group_by(Model, Scenario, Pathway, Ambition) %>%
  summarise(
    deaths_mln_sum    = sum(cumulative_deaths_mln,  na.rm = TRUE),
    headcount_mln_sum = sum(mean_headcount_millions, na.rm = TRUE),
    jobs_RE_sum       = sum(jobs_Renewables,          na.rm = TRUE),
    jobs_Fossil_sum   = sum(jobs_Fossil,              na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mort_per_mln        = (deaths_mln_sum * 1e6) / pop_2020_agg,
    headcount_pct       = (headcount_mln_sum / pop_2020_agg) * 100,
    re_jobs_per_mln     = jobs_RE_sum                            / pop_2020_agg,
    fossil_jobs_per_mln = jobs_Fossil_sum                        / pop_2020_agg,
    total_jobs_per_mln  = (jobs_RE_sum + jobs_Fossil_sum)        / pop_2020_agg,
    net_re_jobs_per_mln = (jobs_RE_sum - jobs_Fossil_sum)        / pop_2020_agg,
    Region_label      = "Aggregated R10 regions"
  )

cat("Regional base rows:", nrow(base_regional), "\n")
cat("Aggregated base rows:", nrow(base_agg), "\n")

# ── 3. Colour/shape constants ─────────────────────────────────────────────────
PATH_COLS <- c("High-CDR" = "#2166ac", "High-RE" = "#d6604d")
AMB_SHAPES <- c("1.5C (High-Ambition)" = 16, "2C (Medium-Ambition)" = 17)

# Helper: save figure
sc_new <- function(p, name, w = 14, h = 8) {
  ggsave(file.path(dirname(FIG_OUT), name), p,
         width = w, height = h, dpi = 300, bg = "white")
  message("Saved: ", name)
}

theme_out <- theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "#1c3a5e", colour = NA),
    strip.text       = element_text(colour = "white", face = "bold"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", hjust = 0, size = 13),
    plot.subtitle    = element_text(hjust = 0, colour = "grey40", size = 10),
    plot.caption     = element_text(colour = "grey50", hjust = 0, size = 8)
  )

# ── 4. Figure generator functions ─────────────────────────────────────────────

# 4a. Aggregated violin + box per ambition
make_agg_violin <- function(dat_agg, y_col, y_lab, title_sfx, subtitle, amb) {
  d <- dat_agg %>% filter(str_detect(Ambition, amb))
  if (nrow(d) == 0) return(NULL)
  
  ggplot(d, aes(x = Pathway, y = .data[[y_col]], fill = Pathway, colour = Pathway)) +
    geom_violin(alpha = 0.35, colour = NA, scale = "width", trim = TRUE) +
    geom_boxplot(width = 0.18, outlier.size = 0.8,
                 colour = "grey20", fill = "white", alpha = 0.9) +
    stat_summary(fun = median, geom = "point", size = 3, colour = "grey10", shape = 18) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
    scale_fill_manual(values = PATH_COLS, guide = "none") +
    scale_colour_manual(values = PATH_COLS, guide = "none") +
    scale_y_continuous(labels = comma_format()) +
    labs(
      title    = paste0(title_sfx, " — Aggregated R10 Regions (",
                        if_else(str_detect(amb,"1.5"), "1.5C High-Ambition", "2C Medium-Ambition"), ")"),
      subtitle = subtitle,
      x = NULL, y = y_lab,
      caption  = "Aggregated across 5 R10 regions. Violin = distribution; box = IQR; diamond = median."
    ) +
    theme_out
}

# 4b. Regional faceted violin per ambition
make_reg_violin <- function(dat_reg, y_col, y_lab, title_sfx, subtitle, amb) {
  d <- dat_reg %>%
    filter(str_detect(Ambition, amb), !is.na(Region_label)) %>%
    mutate(Region_label = factor(Region_label,
                                 levels = unname(REGION_LABELS_FIG[regions_r10])))
  if (nrow(d) == 0) return(NULL)
  
  ggplot(d, aes(x = Pathway, y = .data[[y_col]], fill = Pathway, colour = Pathway)) +
    geom_violin(alpha = 0.35, colour = NA, scale = "width", trim = TRUE) +
    geom_boxplot(width = 0.18, outlier.size = 0.5,
                 colour = "grey20", fill = "white", alpha = 0.9) +
    stat_summary(fun = median, geom = "point", size = 2.5, colour = "grey10", shape = 18) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
    facet_wrap(~ Region_label, nrow = 1, scales = "free_y") +
    scale_fill_manual(values = PATH_COLS, guide = "none") +
    scale_colour_manual(values = PATH_COLS, guide = "none") +
    scale_y_continuous(labels = comma_format()) +
    labs(
      title    = paste0(title_sfx, " by R10 Region (",
                        if_else(str_detect(amb,"1.5"), "1.5C High-Ambition", "2C Medium-Ambition"), ")"),
      subtitle = subtitle,
      x = NULL, y = y_lab,
      caption  = "Violin = distribution; box = IQR; diamond = median."
    ) +
    theme_out
}

# ── 5. Generate all outcome figures ──────────────────────────────────────────

for (amb_tag in c("1.5C", "2C")) {
  amb_str   <- if_else(amb_tag == "1.5C", "1.5C", "2C")
  file_sfx  <- if_else(amb_tag == "1.5C", "15C", "2C")
  
  # ── Mortality ──────────────────────────────────────────────────────────────
  p <- make_agg_violin(
    base_agg, "mort_per_mln",
    "PM2.5 deaths per million population (cumul. 2020–net-zero)",
    "Air Pollution Mortality",
    "Cumulative PM2.5 premature deaths  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_mort_agg_", file_sfx, ".png"), 8, 7)
  
  p <- make_reg_violin(
    base_regional, "mort_per_mln",
    "PM2.5 deaths per million (cumul.)",
    "Air Pollution Mortality",
    "Cumulative PM2.5 premature deaths  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_mort_regional_", file_sfx, ".png"), 18, 7)
  
  # ── Headcount deprivation ──────────────────────────────────────────────────
  p <- make_agg_violin(
    base_agg, "headcount_pct",
    "Population below DLS threshold (% of 2020 population)",
    "Energy Deprivation Headcount",
    "Population below Decent Living Standards energy threshold  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_headcount_agg_", file_sfx, ".png"), 8, 7)
  
  p <- make_reg_violin(
    base_regional, "headcount_pct",
    "Population below DLS (% of 2020 pop.)",
    "Energy Deprivation Headcount",
    "Population below Decent Living Standards energy threshold  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_headcount_regional_", file_sfx, ".png"), 18, 7)
  
  # ── Net RE jobs ────────────────────────────────────────────────────────────
  p <- make_agg_violin(
    base_agg, "net_re_jobs_per_mln",
    "Net RE jobs per million population (thousands, cumul. 2020–net-zero)",
    "Net Renewable Energy Jobs (RE minus Fossil)",
    "Cumulative (RE jobs − Fossil jobs)  ·  Positive = net RE advantage  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_netjobs_agg_", file_sfx, ".png"), 8, 7)
  
  p <- make_reg_violin(
    base_regional, "net_re_jobs_per_mln",
    "Net RE jobs per million pop. (thousands)",
    "Net Renewable Energy Jobs (RE minus Fossil)",
    "Cumulative (RE jobs − Fossil jobs)  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_netjobs_regional_", file_sfx, ".png"), 18, 7)
  
  # ── RE jobs ────────────────────────────────────────────────────────────────
  p <- make_agg_violin(
    base_agg, "re_jobs_per_mln",
    "RE jobs per million population (thousands, cumul. 2020–net-zero)",
    "Renewable Energy Jobs",
    "Cumulative renewable energy jobs  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_rejobs_agg_", file_sfx, ".png"), 8, 7)
  
  p <- make_reg_violin(
    base_regional, "re_jobs_per_mln",
    "RE jobs per million pop. (thousands)",
    "Renewable Energy Jobs",
    "Cumulative renewable energy jobs  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_rejobs_regional_", file_sfx, ".png"), 18, 7)
  
  # ── Fossil jobs ────────────────────────────────────────────────────────────
  p <- make_agg_violin(
    base_agg, "fossil_jobs_per_mln",
    "Fossil jobs per million population (thousands, cumul. 2020–net-zero)",
    "Fossil Fuel Jobs",
    "Cumulative fossil fuel jobs  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_fossiljobs_agg_", file_sfx, ".png"), 8, 7)
  
  p <- make_reg_violin(
    base_regional, "fossil_jobs_per_mln",
    "Fossil jobs per million pop. (thousands)",
    "Fossil Fuel Jobs",
    "Cumulative fossil fuel jobs  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_fossiljobs_regional_", file_sfx, ".png"), 18, 7)
  
  # ── Total jobs (RE + Fossil) ───────────────────────────────────────────────
  p <- make_agg_violin(
    base_agg, "total_jobs_per_mln",
    "Total energy jobs per million population (thousands, cumul. 2020–net-zero)",
    "Total Energy Jobs (RE + Fossil)",
    "Cumulative total energy jobs (RE + fossil)  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_totaljobs_agg_", file_sfx, ".png"), 8, 7)
  
  p <- make_reg_violin(
    base_regional, "total_jobs_per_mln",
    "Total energy jobs per million pop. (thousands)",
    "Total Energy Jobs (RE + Fossil)",
    "Cumulative total energy jobs (RE + fossil)  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_totaljobs_regional_", file_sfx, ".png"), 18, 7)
}

# ── 6. Triangle / Ternary chart ───────────────────────────────────────────────
cat("\nBuilding ternary charts...\n")

# Ternary charts need all three axes positive and ideally on comparable scales.
# Strategy:
#   T axis: mortality per million (higher = worse, so invert: T = max - mort)
#   L axis: headcount % (higher = worse, invert: L = max - headcount)
#   R axis: net RE jobs per million (higher = better, keep as-is but rescale)
# BUT true ternary (summing to 100%) distorts the data.
# Better approach: use ggtern with raw values, interpreting as a scatter on
# a ternary canvas. Values don't need to sum to 1 — ggtern handles this.

# For readability, rescale each axis to 0-100 within the plotting data
# so all three axes have similar range.

for (amb_tag in c("1.5C", "2C")) {
  amb_str  <- if_else(amb_tag == "1.5C", "1.5C", "2C")
  file_sfx <- if_else(amb_tag == "1.5C", "15C", "2C")
  
  tri_dat <- base_agg %>%
    filter(str_detect(Ambition, amb_str)) %>%
    filter(!is.na(mort_per_mln), !is.na(headcount_pct), !is.na(net_re_jobs_per_mln)) %>%
    mutate(
      # Rescale each axis to 0-100 for ternary display
      # Mortality: higher = worse (↑ is bad)
      T_mort    = scales::rescale(mort_per_mln,      to = c(1, 99)),
      # Headcount: higher = worse (↑ is bad)
      L_head    = scales::rescale(headcount_pct,     to = c(1, 99)),
      # Net RE jobs: higher = better, but we invert so higher = more fossil dominance
      # Actually: keep positive = RE advantage. Rescale from min to max.
      R_jobs    = scales::rescale(net_re_jobs_per_mln, to = c(1, 99)),
      Pathway   = factor(Pathway, levels = c("High-CDR", "High-RE"))
    )
  
  if (nrow(tri_dat) < 5) {
    cat("  Skipping ternary for", amb_str, "— insufficient data\n")
    next
  }
  
  # Pathway medians
  tri_med <- tri_dat %>%
    group_by(Pathway) %>%
    summarise(
      T_mort = median(T_mort, na.rm = TRUE),
      L_head = median(L_head, na.rm = TRUE),
      R_jobs = median(R_jobs, na.rm = TRUE),
      .groups = "drop"
    )
  
  p_tri <- ggtern(tri_dat,
                  aes(x = L_head, y = T_mort, z = R_jobs, colour = Pathway)) +
    geom_point(alpha = 0.25, size = 1.8) +
    geom_point(data = tri_med, aes(colour = Pathway),
               size = 6, shape = 18, alpha = 1) +
    scale_colour_manual(values = PATH_COLS, name = "Pathway") +
    theme_bw() +
    theme_showarrows() +
    labs(
      title    = paste0("Ternary Tradeoffs: Mortality · Deprivation · Net RE Jobs\n",
                        if_else(str_detect(amb_str,"1.5"), "1.5C (High-Ambition)", "2C (Medium-Ambition)"),
                        "  ·  Aggregated R10 regions"),
      subtitle = "Each point = one scenario. Diamond = pathway median.  All axes rescaled to 0–100 for display.",
      Tarrow   = "PM2.5\nMortality\n(per million)\n↑ worse",
      Larrow   = "Deprivation\nHeadcount\n(% pop.)\n↑ worse",
      Rarrow   = "Net RE Jobs\n(per million)\n↑ better",
      caption  = paste0(
        "High-CDR: blue  ·  High-RE: red  ·  ",
        "Net RE jobs = RE jobs − Fossil jobs per million 2020 population.\n",
        "Mortality and headcount: cumulative 2020–net-zero. ",
        "Axes rescaled within this ambition group; values are not proportions.")
    ) +
    theme(
      legend.position = "bottom",
      plot.title      = element_text(face = "bold", size = 12),
      plot.subtitle   = element_text(colour = "grey40", size = 9),
      plot.caption    = element_text(colour = "grey50", size = 8)
    )
  
  sc_new(p_tri, paste0("NEW_P_triangle_", file_sfx, ".png"), 10, 9)
}

cat("\n=== NEW OUTCOME FIGURES COMPLETE ===\n")
cat("Files saved to:", dirname(FIG_OUT), "\n")
cat("Figures produced:\n")
for (f in c("mort","headcount","netjobs","rejobs","fossiljobs","totaljobs")) {
  for (v in c("agg","regional")) {
    for (a in c("15C","2C")) {
      cat("  NEW_P_", f, "_", v, "_", a, ".png\n", sep="")
    }
  }
}
cat("  NEW_P_triangle_15C.png\n")
cat("  NEW_P_triangle_2C.png\n")


# =============================================================================
# ABSOLUTE THRESHOLD CLASSIFICATION — NEW_P COMPARISON FIGURES
#
# Mirrors all NEW_P figures using world_cum_thresh instead of world_cum_f2.
# All filenames have "_THRESH" suffix.
# =============================================================================
cat("\n=== NEW PER-CAPITA OUTCOME FIGURES — THRESHOLD CLASSIFICATION ===\n")

# ── Build normalised outcome datasets using threshold classification ──────────

base_regional_thresh <- df_master %>%
  filter(Variable == "Total CDR", Region %in% regions_r10) %>%
  left_join(
    world_cum_thresh %>%
      select(Model, Scenario, Category, high_cdr_only, high_re_only),
    by = c("Model", "Scenario", "Category")
  ) %>%
  filter(!is.na(high_cdr_only), high_cdr_only | high_re_only) %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition)) %>%
  mutate(Pathway = if_else(high_cdr_only, "High-CDR", "High-RE")) %>%
  left_join(pop_2020_r10, by = "Region") %>%
  mutate(
    mort_per_mln        = (cumulative_deaths_mln * 1e6)  / pop_2020_mln,
    headcount_pct       = (mean_headcount_millions / pop_2020_mln) * 100,
    re_jobs_per_mln     = jobs_Renewables                / pop_2020_mln,
    fossil_jobs_per_mln = jobs_Fossil                    / pop_2020_mln,
    total_jobs_per_mln  = (jobs_Renewables + jobs_Fossil)/ pop_2020_mln,
    net_re_jobs_per_mln = (jobs_Renewables - jobs_Fossil)/ pop_2020_mln,
    Region_label        = REGION_LABELS_FIG[Region],
    PathAmb             = paste0(Pathway, "\n",
                                 if_else(str_detect(Ambition,"1.5C"),"1.5C","2C"))
  ) %>%
  filter(!is.na(mort_per_mln) | !is.na(headcount_pct) | !is.na(net_re_jobs_per_mln))

base_agg_thresh <- df_master %>%
  filter(Variable == "Total CDR", Region %in% regions_r10) %>%
  left_join(
    world_cum_thresh %>%
      select(Model, Scenario, Category, high_cdr_only, high_re_only),
    by = c("Model", "Scenario", "Category")
  ) %>%
  filter(!is.na(high_cdr_only), high_cdr_only | high_re_only) %>%
  assign_amb("Category") %>%
  filter(!is.na(Ambition)) %>%
  mutate(Pathway = if_else(high_cdr_only, "High-CDR", "High-RE")) %>%
  group_by(Model, Scenario, Pathway, Ambition) %>%
  summarise(
    deaths_mln_sum    = sum(cumulative_deaths_mln,   na.rm = TRUE),
    headcount_mln_sum = sum(mean_headcount_millions,  na.rm = TRUE),
    jobs_RE_sum       = sum(jobs_Renewables,           na.rm = TRUE),
    jobs_Fossil_sum   = sum(jobs_Fossil,               na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mort_per_mln        = (deaths_mln_sum * 1e6)                   / pop_2020_agg,
    headcount_pct       = (headcount_mln_sum / pop_2020_agg) * 100,
    re_jobs_per_mln     = jobs_RE_sum                              / pop_2020_agg,
    fossil_jobs_per_mln = jobs_Fossil_sum                          / pop_2020_agg,
    total_jobs_per_mln  = (jobs_RE_sum + jobs_Fossil_sum)          / pop_2020_agg,
    net_re_jobs_per_mln = (jobs_RE_sum - jobs_Fossil_sum)          / pop_2020_agg,
    Region_label        = "Aggregated R10 regions"
  )

cat("Threshold base_regional rows:", nrow(base_regional_thresh), "\n")
cat("Threshold base_agg rows:",      nrow(base_agg_thresh), "\n")

# ── Generate all threshold violin figures ─────────────────────────────────────
for (amb_tag in c("1.5C", "2C")) {
  amb_str  <- if_else(amb_tag == "1.5C", "1.5C", "2C")
  file_sfx <- if_else(amb_tag == "1.5C", "15C", "2C")
  
  # Mortality
  p <- make_agg_violin(
    base_agg_thresh, "mort_per_mln",
    "PM2.5 deaths per million population (cumul. 2020–net-zero)",
    "Air Pollution Mortality [THRESH]",
    "Absolute threshold classification  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_mort_agg_", file_sfx, "_THRESH.png"), 8, 7)
  
  p <- make_reg_violin(
    base_regional_thresh, "mort_per_mln",
    "PM2.5 deaths per million population",
    "Air Pollution Mortality [THRESH]",
    "Absolute threshold classification  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_mort_regional_", file_sfx, "_THRESH.png"), 18, 7)
  
  # Headcount
  p <- make_agg_violin(
    base_agg_thresh, "headcount_pct",
    "Population below DLS (% of 2020 pop., mean)",
    "Energy Deprivation Headcount [THRESH]",
    "Absolute threshold classification  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_headcount_agg_", file_sfx, "_THRESH.png"), 8, 7)
  
  p <- make_reg_violin(
    base_regional_thresh, "headcount_pct",
    "Population below DLS (% of 2020 pop.)",
    "Energy Deprivation Headcount [THRESH]",
    "Absolute threshold classification  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_headcount_regional_", file_sfx, "_THRESH.png"), 18, 7)
  
  # Net RE jobs
  p <- make_agg_violin(
    base_agg_thresh, "net_re_jobs_per_mln",
    "Net RE jobs per million population (thousands, cumul. 2020–net-zero)",
    "Net Renewable Energy Jobs (RE minus Fossil) [THRESH]",
    "Absolute threshold classification  ·  Positive = net RE advantage",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_netjobs_agg_", file_sfx, "_THRESH.png"), 8, 7)
  
  p <- make_reg_violin(
    base_regional_thresh, "net_re_jobs_per_mln",
    "Net RE jobs per million pop. (thousands)",
    "Net Renewable Energy Jobs (RE minus Fossil) [THRESH]",
    "Absolute threshold classification  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_netjobs_regional_", file_sfx, "_THRESH.png"), 18, 7)
  
  # RE jobs
  p <- make_agg_violin(
    base_agg_thresh, "re_jobs_per_mln",
    "RE jobs per million population (thousands, cumul. 2020–net-zero)",
    "Renewable Energy Jobs [THRESH]",
    "Absolute threshold classification  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_rejobs_agg_", file_sfx, "_THRESH.png"), 8, 7)
  
  p <- make_reg_violin(
    base_regional_thresh, "re_jobs_per_mln",
    "RE jobs per million pop. (thousands)",
    "Renewable Energy Jobs [THRESH]",
    "Absolute threshold classification  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_rejobs_regional_", file_sfx, "_THRESH.png"), 18, 7)
  
  # Fossil jobs
  p <- make_agg_violin(
    base_agg_thresh, "fossil_jobs_per_mln",
    "Fossil jobs per million population (thousands, cumul. 2020–net-zero)",
    "Fossil Fuel Jobs [THRESH]",
    "Absolute threshold classification  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_fossiljobs_agg_", file_sfx, "_THRESH.png"), 8, 7)
  
  p <- make_reg_violin(
    base_regional_thresh, "fossil_jobs_per_mln",
    "Fossil jobs per million pop. (thousands)",
    "Fossil Fuel Jobs [THRESH]",
    "Absolute threshold classification  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_fossiljobs_regional_", file_sfx, "_THRESH.png"), 18, 7)
  
  # Total jobs
  p <- make_agg_violin(
    base_agg_thresh, "total_jobs_per_mln",
    "Total energy jobs per million population (thousands, cumul. 2020–net-zero)",
    "Total Energy Jobs (RE + Fossil) [THRESH]",
    "Absolute threshold classification  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_totaljobs_agg_", file_sfx, "_THRESH.png"), 8, 7)
  
  p <- make_reg_violin(
    base_regional_thresh, "total_jobs_per_mln",
    "Total energy jobs per million pop. (thousands)",
    "Total Energy Jobs (RE + Fossil) [THRESH]",
    "Absolute threshold classification  ·  High-CDR vs High-RE",
    amb_str)
  if (!is.null(p)) sc_new(p, paste0("NEW_P_totaljobs_regional_", file_sfx, "_THRESH.png"), 18, 7)
}

# ── Ternary charts (threshold) ────────────────────────────────────────────────
cat("\nBuilding ternary charts (threshold classification)...\n")

for (amb_tag in c("1.5C", "2C")) {
  amb_str  <- if_else(amb_tag == "1.5C", "1.5C", "2C")
  file_sfx <- if_else(amb_tag == "1.5C", "15C", "2C")
  
  tri_dat_thresh <- base_agg_thresh %>%
    filter(str_detect(Ambition, amb_str)) %>%
    filter(!is.na(mort_per_mln), !is.na(headcount_pct),
           !is.na(net_re_jobs_per_mln)) %>%
    mutate(
      T_mort  = scales::rescale(mort_per_mln,       to = c(1, 99)),
      L_head  = scales::rescale(headcount_pct,      to = c(1, 99)),
      R_jobs  = scales::rescale(net_re_jobs_per_mln,to = c(1, 99)),
      Pathway = factor(Pathway, levels = c("High-CDR", "High-RE"))
    )
  
  if (nrow(tri_dat_thresh) < 5) {
    cat("  Skipping ternary [THRESH] for", amb_str, "— insufficient data\n")
    next
  }
  
  tri_med_thresh <- tri_dat_thresh %>%
    group_by(Pathway) %>%
    summarise(T_mort = median(T_mort, na.rm=TRUE),
              L_head = median(L_head, na.rm=TRUE),
              R_jobs = median(R_jobs, na.rm=TRUE),
              .groups = "drop")
  
  p_tri_thresh <- ggtern(tri_dat_thresh,
                         aes(x=L_head, y=T_mort, z=R_jobs, colour=Pathway))+
                         geom_point(alpha=0.25, size=1.8) +
    geom_point(data=tri_med_thresh, aes(colour=Pathway),
               size=6, shape=18, alpha=1) +
    scale_colour_manual(values=PATH_COLS, name="Pathway") +
    theme_bw() +
    theme_showarrows() +
    labs(
      title    = paste0("Ternary Tradeoffs [THRESH]: Mortality · Deprivation · Net RE Jobs\n",
                        if_else(str_detect(amb_str,"1.5"), "1.5C (High-Ambition)",
                                "2C (Medium-Ambition)"),
                        "  ·  Absolute threshold classification"),
      subtitle = "Each point = one scenario. Diamond = pathway median. Axes rescaled 0–100.",
      Tarrow   = "PM2.5\nMortality\n(per million)\n↑ worse",
      Larrow   = "Deprivation\nHeadcount\n(% pop.)\n↑ worse",
      Rarrow   = "Net RE Jobs\n(per million)\n↑ better",
      caption  = "Absolute threshold classification — see Section 6b of analysis script for threshold values."
    ) +
    theme(legend.position="bottom",
          plot.title   = element_text(face="bold", size=12),
          plot.subtitle= element_text(colour="grey40", size=9),
          plot.caption = element_text(colour="grey50", size=8))
  
  sc_new(p_tri_thresh, paste0("NEW_P_triangle_", file_sfx, "_THRESH.png"), 10, 9)
}

cat("\n=== NEW OUTCOME FIGURES (THRESHOLD) COMPLETE ===\n")
cat("Files saved to:", dirname(FIG_OUT), "\n")
cat("All threshold figures have _THRESH suffix.\n")
cat("  NEW_P_triangle_15C.png\n")
cat("  NEW_P_triangle_2C.png\n")

