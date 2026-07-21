# =============================================================================
# COMPASS — CROSS-APPROACH COMPARISON FIGURES (Paper 1)
#
# Compares the TEN scenario-selection approaches (A-J) produced by
# COMPASS_master_analysis.R — the five filter/ambition combinations (A-E) at
# the top-tercile cut, and the same five combinations (F-J) at an above-median
# cut — on two fronts:
#
#   PART 1  SAMPLE-SET SIZE
#     S1  Selection funnel: universe -> after vetting -> classified
#         High-CDR-only / High-RE-only, per approach x ambition.
#     S2  Pathway counts (High-CDR vs High-RE) per approach x ambition.
#     S3  Approach overlap: Jaccard heatmap of selected scenario sets.
#     S4  Ambition-strategy shift: AR6-category vs peak-warming sample split.
#     S5  Tercile vs median: paired group-size comparison (A vs F, B vs G, ...).
#
#   PART 2  RESULTS (population-normalised; figures use per-capita units,
#           the exported CSVs carry both absolute and per-capita columns)
#     R1  Outcome distributions by Pathway (High-CDR vs High-RE), faceted by
#         ambition, panelled across approaches — per outcome. Produced as two
#         batches (tercile A-E, median F-J) so the cut can be compared directly.
#         World (5-region population-weighted aggregate) level.
#     R2  Contrast forest plot: median(High-CDR) - median(High-RE) per
#         outcome x ambition x approach (all 10), with Mann-Whitney
#         significance and Cliff's delta. World level.
#     R3  Regional breakdown: for a curated set of headline outcomes, one
#         figure per approach faceted by Ambition (rows) x Region (cols,
#         5 R10 regions + World), so you can see which regions drive the
#         High-CDR vs High-RE contrast.
#
# INPUTS (written by COMPASS_master_analysis.R):
#   OUT_DIR/comparison/approach_scenario_counts.csv
#   OUT_DIR/comparison/approach_pathway_counts.csv
#   OUT_DIR/comparison/approach_set_overlap.csv
#   OUT_DIR/comparison/approach_summary.csv
#   OUT_DIR/approach_<X>/compass_master_dataset_<X>.csv   (X in A..J)
#   OUT_DIR/approach_<X>/compass_pathway_tercile_<X>.csv
#
# OUTPUT:  OUT_DIR/comparison/Figs/*.png
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(patchwork)
})

# ---- Paths ------------------------------------------------------------------
OUT_DIR  <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_master"
COMP_DIR <- file.path(OUT_DIR, "comparison")
FIG_DIR  <- file.path(COMP_DIR, "Figs")
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

APPROACHES          <- c("A","B","C","D","E","F","G","H","I","J")
TERCILE_APPROACHES  <- c("A","B","C","D","E")
MEDIAN_APPROACHES   <- c("F","G","H","I","J")
# Pairing of each tercile approach with its above-median twin (same
# filter/ambition combination, different cut) — used by S5.
TWIN_OF <- c(A="F", B="G", C="H", D="I", E="J",
            F="A", G="B", H="C", I="D", J="E")

APPROACH_LABELS <- c(
  A = "A: all·AR6·1/3",       F = "F: all·AR6·1/2",
  B = "B: all·warm·1/3",      G = "G: all·warm·1/2",
  C = "C: SCI·AR6·1/3",       H = "H: SCI·AR6·1/2",
  D = "D: SCI·warm·1/3",      I = "I: SCI·warm·1/2",
  E = "E: part·AR6·1/3",      J = "J: part·AR6·1/2"
)
PATH_COLS  <- c("High-CDR" = "#2166ac", "High-RE" = "#d6604d")
AMB_ORDER  <- c("1.5C (High-Ambition)", "2C (Medium-Ambition)")

REGION_LABELS <- c(
  R10AFRICA               = "Africa",
  `R10CHINA+`              = "China+",
  R10EUROPE                = "Europe",
  `R10INDIA+`               = "India+",
  R10NORTH_AM               = "North America",
  `Aggregated R10 regions`  = "World (5-region sum)"
)
REGION_ORDER <- names(REGION_LABELS)

save_fig <- function(p, name, w = 12, h = 7) {
  ggsave(file.path(FIG_DIR, name), p, width = w, height = h, dpi = 300, bg = "white")
  message("Saved: Figs/", name)
}

theme_cmp <- function(base = 11) {
  theme_bw(base_size = base) +
    theme(strip.background = element_rect(fill = "#1c3a5e", colour = NA),
          strip.text = element_text(colour = "white", face = "bold"),
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0),
          plot.subtitle = element_text(colour = "grey40"),
          plot.caption = element_text(colour = "grey50", hjust = 0, size = 8))
}

amb_short <- function(x) if_else(str_detect(x, "1.5"), "1.5C", "2C")

# =============================================================================
# LOAD
# =============================================================================
cat("Loading comparison tables and per-approach outputs...\n")

scenario_counts <- read_csv(file.path(COMP_DIR, "approach_scenario_counts.csv"),
                            show_col_types = FALSE)
pathway_counts  <- read_csv(file.path(COMP_DIR, "approach_pathway_counts.csv"),
                            show_col_types = FALSE)
set_overlap     <- read_csv(file.path(COMP_DIR, "approach_set_overlap.csv"),
                            show_col_types = FALSE)
approach_summary <- read_csv(file.path(COMP_DIR, "approach_summary.csv"),
                             show_col_types = FALSE)

# Per-approach master datasets (ALL regions kept: 5 R10 + Aggregated/"World")
# + pathway classification, stacked with an `approach` column.
load_master <- function(id) {
  path_m <- file.path(OUT_DIR, paste0("approach_", id),
                      paste0("compass_master_dataset_", id, ".csv"))
  path_p <- file.path(OUT_DIR, paste0("approach_", id),
                      paste0("compass_pathway_tercile_", id, ".csv"))
  if (!file.exists(path_m)) {
    warning("Missing master dataset for approach ", id, " — skipping. ",
            "Re-run COMPASS_master_analysis.R if this approach is new.")
    return(NULL)
  }
  m <- read_csv(path_m, show_col_types = FALSE)
  p <- read_csv(path_p, show_col_types = FALSE) %>%
    select(Model, Scenario, Pathway_excl, high_cdr_only, high_re_only)
  m %>%
    filter(Variable == "Total CDR") %>%
    left_join(p, by = c("Model", "Scenario")) %>%
    mutate(approach = id)
}
master_all <- map_dfr(APPROACHES, load_master)
cat("master_all rows:", nrow(master_all), "| approaches loaded:",
    paste(sort(unique(master_all$approach)), collapse = ","), "\n")

# Derived absolute net RE jobs (per-capita net_re_jobs_per_1k already in master)
master_all <- master_all %>%
  mutate(net_re_jobs = jobs_Renewables - jobs_Fossil)

# Outcome definitions: absolute column + per-capita column + labels.
# Figures use the per-capita column; CSVs carry both.
OUTCOMES <- tribble(
  ~col_abs,                        ~col_pc,               ~label_pc,                                      ~label_abs,                          ~better,
  "cumulative_deaths_mln",         "mort_per_1k",          "PM2.5 mortality (per 1,000 pop, cumul.)",     "PM2.5 mortality (mln, cumul.)",        "lower",
  "mean_headcount_millions",       "headcount_pct",        "Energy deprivation (% of population)",        "Energy deprivation (mln)",              "lower",
  "cumulative_gap_EJ",             "gap_GJ_pc",             "DLE energy gap (GJ/capita, cumul.)",          "DLE energy gap (EJ, cumul.)",           "lower",
  "cumulative_implied_CO2_GtCO2",  "implied_CO2_tpc",       "Implied CO2 of DLE gap (tCO2/capita)",        "Implied CO2 of DLE gap (GtCO2)",        "lower",
  "jobs_Renewables",               "re_jobs_per_1k",        "Renewable energy jobs (per 1,000 pop)",       "Renewable energy jobs (000s)",          "higher",
  "jobs_Fossil",                   "fossil_jobs_per_1k",    "Fossil energy jobs (per 1,000 pop)",          "Fossil energy jobs (000s)",             "higher",
  "net_re_jobs",                   "net_re_jobs_per_1k",    "Net RE jobs (per 1,000 pop)",                 "Net RE jobs (000s)",                    "higher"
)


# =============================================================================
# PART 1 — SAMPLE-SET SIZE
# =============================================================================

# ---- S1: selection funnel ---------------------------------------------------
funnel <- scenario_counts %>%
  transmute(approach, Ambition, ambition_method, vetting,
            `In sample`     = n_scenarios,
            `High-CDR only` = n_high_cdr,
            `High-RE only`  = n_high_re) %>%
  pivot_longer(c(`In sample`, `High-CDR only`, `High-RE only`),
               names_to = "stage", values_to = "n") %>%
  mutate(stage = factor(stage,
                        levels = c("In sample", "High-CDR only", "High-RE only")),
         Ambition = factor(Ambition, levels = AMB_ORDER),
         approach_lab = factor(APPROACH_LABELS[approach],
                               levels = APPROACH_LABELS[APPROACHES]))

fig_S1 <- funnel %>%
  ggplot(aes(x = stage, y = n, fill = stage)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n), vjust = -0.3, size = 2.6) +
  facet_grid(Ambition ~ approach_lab, scales = "free_x", switch = "y") +
  scale_fill_manual(values = c("In sample" = "#9ecae1",
                               "High-CDR only" = "#2166ac",
                               "High-RE only" = "#d6604d"),
                    guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Scenario selection funnel by approach and ambition",
       subtitle = "In-sample (after vetting) then top-fraction High-CDR-only / High-RE-only classification",
       x = NULL, y = "Number of scenarios",
       caption = "Vetting: A/B/F/G none · C/D/H/I full SCI · E/J partial (tech-feasibility). Ambition: A/C/E/F/H/J AR6 category · B/D/G/I median peak warming. Cut: A-E top tercile (1/3) · F-J above median (1/2).") +
  theme_cmp() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 7))
save_fig(fig_S1, "S1_selection_funnel.png", 24, 8)

# ---- S2: pathway counts (High-CDR vs High-RE) -------------------------------
fig_S2 <- pathway_counts %>%
  mutate(Ambition = factor(Ambition, levels = AMB_ORDER),
         approach_lab = factor(APPROACH_LABELS[approach],
                               levels = APPROACH_LABELS[APPROACHES])) %>%
  ggplot(aes(x = approach_lab, y = n, fill = Pathway_excl)) +
  geom_col(position = position_dodge(0.75), width = 0.7) +
  geom_text(aes(label = n), position = position_dodge(0.75),
            vjust = -0.3, size = 2.8) +
  facet_wrap(~ Ambition, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = PATH_COLS, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "High-CDR vs High-RE scenario counts by approach",
       subtitle = "Mutually-exclusive pathways, within each ambition group. A-E = top tercile, F-J = above median.",
       x = NULL, y = "Number of scenarios",
       caption = "Small cells are underpowered for results inference; check n before trusting a contrast.") +
  theme_cmp() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8))
save_fig(fig_S2, "S2_pathway_counts.png", 16, 8)

# ---- S3: approach overlap (Jaccard heatmap) ---------------------------------
fig_S3 <- set_overlap %>%
  mutate(a = factor(a, levels = APPROACHES),
         b = factor(b, levels = rev(APPROACHES))) %>%
  ggplot(aes(x = a, y = b, fill = jaccard)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", jaccard)), size = 2.8) +
  scale_fill_gradient(low = "#f7fbff", high = "#08519c", name = "Jaccard",
                      limits = c(0, 1)) +
  coord_equal() +
  labs(title = "Overlap of selected scenario sets across approaches",
       subtitle = "Jaccard index (shared / union) of the Model×Scenario sets kept by each approach",
       x = NULL, y = NULL) +
  theme_cmp() +
  theme(panel.grid = element_blank())
save_fig(fig_S3, "S3_set_overlap.png", 10, 9)

# ---- S4: ambition-strategy shift (AR6 vs peak-warming) ----------------------
fig_S4 <- scenario_counts %>%
  filter(approach %in% c("A", "B", "C", "D")) %>%
  mutate(pair = if_else(approach %in% c("A", "B"),
                        "All scenarios", "Full SCI vetting"),
         strategy = if_else(ambition_method == "ar6",
                            "AR6 category", "Median peak warming"),
         Ambition = factor(amb_short(Ambition), levels = c("1.5C", "2C"))) %>%
  ggplot(aes(x = strategy, y = n_scenarios, fill = Ambition)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n_scenarios), position = position_stack(vjust = 0.5),
            size = 3.2, colour = "white") +
  facet_wrap(~ pair, scales = "free_y") +
  scale_fill_manual(values = c("1.5C" = "#1a9641", "2C" = "#d7191c"),
                    name = "Ambition") +
  labs(title = "Ambition-classification strategy: AR6 category vs median peak warming",
       subtitle = "Same scenario pool, re-split into 1.5C / 2C by each strategy (top-tercile approaches shown)",
       x = NULL, y = "Number of scenarios",
       caption = "Peak-warming re-classification moves low-peak C3/C4 into 1.5C and drops scenarios peaking >2.0C.") +
  theme_cmp()
save_fig(fig_S4, "S4_ambition_strategy.png", 11, 6)

# ---- S5: tercile vs median — paired group-size comparison -------------------
s5_dat <- pathway_counts %>%
  mutate(cut = if_else(approach %in% TERCILE_APPROACHES, "Tercile (1/3)", "Median (1/2)"),
         pair = if_else(approach %in% TERCILE_APPROACHES, approach, TWIN_OF[approach]),
         pair_lab = paste0(pair, "/", TWIN_OF[pair]),
         Ambition = factor(Ambition, levels = AMB_ORDER))

fig_S5 <- s5_dat %>%
  ggplot(aes(x = cut, y = n, fill = Pathway_excl)) +
  geom_col(position = position_dodge(0.75), width = 0.7) +
  geom_text(aes(label = n), position = position_dodge(0.75), vjust = -0.3, size = 2.8) +
  facet_grid(Ambition ~ pair_lab, scales = "free_y") +
  scale_fill_manual(values = PATH_COLS, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Tercile vs above-median cut: paired group-size comparison",
       subtitle = "Same filter/ambition combination at two classification thresholds (e.g. A vs F)",
       x = NULL, y = "Number of scenarios",
       caption = "Above-median (F-J) roughly doubles group size vs top tercile (A-E), most useful where tercile n is small (C/D).") +
  theme_cmp() +
  theme(axis.text.x = element_text(size = 8))
save_fig(fig_S5, "S5_tercile_vs_median.png", 14, 8)


# =============================================================================
# PART 2 — RESULTS (per-capita in figures, absolute + per-capita in CSVs)
# =============================================================================

path_all <- master_all %>%
  filter(!is.na(Pathway_excl)) %>%
  mutate(Ambition = factor(Ambition, levels = AMB_ORDER),
         approach_lab = factor(APPROACH_LABELS[approach],
                               levels = APPROACH_LABELS[APPROACHES]),
         Region_lab = factor(REGION_LABELS[Region], levels = REGION_LABELS[REGION_ORDER]))

path_world <- path_all %>% filter(Region == "Aggregated R10 regions")

# ---- R1: outcome distributions by pathway (World level), tercile vs median --
make_R1 <- function(ycol, ylab, approach_ids) {
  d <- path_world %>% filter(approach %in% approach_ids, !is.na(.data[[ycol]]))
  if (nrow(d) == 0) return(invisible(NULL))
  d <- d %>% mutate(approach_lab = droplevels(approach_lab))
  ncell <- d %>% count(approach_lab, Ambition, Pathway_excl, name = "n")
  thin <- d %>% group_by(approach_lab, Ambition, Pathway_excl) %>%
    filter(n() < 10) %>% ungroup()
  ggplot(d, aes(x = Pathway_excl, y = .data[[ycol]], fill = Pathway_excl)) +
    geom_violin(alpha = 0.35, colour = NA, scale = "width", trim = TRUE) +
    geom_boxplot(width = 0.18, outlier.size = 0.5, alpha = 0.9,
                 colour = "grey25", fill = "white") +
    geom_jitter(data = thin, width = 0.08, height = 0, size = 1,
                colour = "grey20", alpha = 0.7) +
    stat_summary(fun = median, geom = "point", shape = 18, size = 2.4, colour = "grey10") +
    geom_text(data = ncell, aes(x = Pathway_excl, y = Inf, label = paste0("n=", n)),
              inherit.aes = FALSE, vjust = 1.4, size = 2.9, colour = "grey35") +
    facet_grid(Ambition ~ approach_lab, scales = "free_y") +
    scale_fill_manual(values = PATH_COLS, guide = "none") +
    scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0.05, 0.12))) +
    labs(title = ylab,
         subtitle = "High-CDR vs High-RE · World (5-region pop-normalised) · by ambition (rows) and approach (cols)",
         x = NULL, y = ylab,
         caption = "Violin = distribution; box = IQR; diamond = median; n labelled per cell. Raw points overlaid where n < 10.") +
    theme_cmp()
}
for (i in seq_len(nrow(OUTCOMES))) {
  oc <- OUTCOMES[i, ]
  p_t <- make_R1(oc$col_pc, oc$label_pc, TERCILE_APPROACHES)
  if (!is.null(p_t)) save_fig(p_t, paste0("R1_tercile_", oc$col_pc, ".png"), 15, 7)
  p_m <- make_R1(oc$col_pc, oc$label_pc, MEDIAN_APPROACHES)
  if (!is.null(p_m)) save_fig(p_m, paste0("R1_median_", oc$col_pc, ".png"), 15, 7)
}

# ---- R2: contrast forest plot (World level, all 10 approaches) --------------
# Per-capita medians drive the figure and significance test; absolute medians
# are computed in parallel and attached to the exported CSV only.
MIN_N_TEST <- 4

long_pc  <- path_world %>%
  select(approach, approach_lab, Ambition, Pathway_excl, all_of(OUTCOMES$col_pc)) %>%
  pivot_longer(all_of(OUTCOMES$col_pc), names_to = "col_pc", values_to = "val") %>%
  filter(!is.na(val))
long_abs <- path_world %>%
  select(approach, Ambition, Pathway_excl, all_of(OUTCOMES$col_abs)) %>%
  pivot_longer(all_of(OUTCOMES$col_abs), names_to = "col_abs", values_to = "val_abs") %>%
  filter(!is.na(val_abs))

stat_tests <- long_pc %>%
  group_by(approach, Ambition, col_pc) %>%
  summarise(
    n_cdr = sum(Pathway_excl == "High-CDR"),
    n_re  = sum(Pathway_excl == "High-RE"),
    p_value = {
      x <- val[Pathway_excl == "High-CDR"]; y <- val[Pathway_excl == "High-RE"]
      if (min(length(x), length(y)) >= MIN_N_TEST)
        suppressWarnings(wilcox.test(x, y)$p.value) else NA_real_
    },
    cliffs_delta = {
      x <- val[Pathway_excl == "High-CDR"]; y <- val[Pathway_excl == "High-RE"]
      if (length(x) > 0 && length(y) > 0)
        (sum(outer(x, y, ">")) - sum(outer(x, y, "<"))) / (length(x) * length(y))
      else NA_real_
    },
    .groups = "drop") %>%
  mutate(sig = !is.na(p_value) & p_value < 0.05,
         p_lab = case_when(is.na(p_value) ~ "ns (n<4)",
                           p_value < 0.001 ~ "***",
                           p_value < 0.01  ~ "**",
                           p_value < 0.05  ~ "*",
                           TRUE            ~ "ns"))

contrast_pc <- long_pc %>%
  group_by(approach, approach_lab, Ambition, col_pc, Pathway_excl) %>%
  summarise(med = median(val), n = n(), .groups = "drop") %>%
  pivot_wider(names_from = Pathway_excl, values_from = c(med, n)) %>%
  mutate(diff = `med_High-CDR` - `med_High-RE`,
         rel  = 100 * diff / abs(`med_High-RE`),
         n_min = pmin(`n_High-CDR`, `n_High-RE`, na.rm = TRUE)) %>%
  left_join(stat_tests, by = c("approach", "Ambition", "col_pc")) %>%
  left_join(OUTCOMES %>% select(col_pc, col_abs, outcome_lab = label_pc),
            by = "col_pc")

contrast_abs <- long_abs %>%
  group_by(approach, Ambition, col_abs, Pathway_excl) %>%
  summarise(med_abs = median(val_abs), .groups = "drop") %>%
  pivot_wider(names_from = Pathway_excl, values_from = med_abs,
              names_prefix = "med_abs_") %>%
  mutate(diff_abs = `med_abs_High-CDR` - `med_abs_High-RE`,
         rel_abs  = 100 * diff_abs / abs(`med_abs_High-RE`))

contrast <- contrast_pc %>%
  left_join(contrast_abs, by = c("approach", "Ambition", "col_abs"))

fig_R2 <- contrast %>%
  filter(!is.na(rel)) %>%
  mutate(Ambition = factor(Ambition, levels = AMB_ORDER),
         Significance = if_else(sig, "p < 0.05", "ns / n<4")) %>%
  ggplot(aes(x = rel, y = approach_lab, colour = Ambition)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(aes(size = n_min, shape = Significance), alpha = 0.9,
             position = position_dodge(width = 0.5)) +
  facet_wrap(~ outcome_lab, scales = "free_x", ncol = 3) +
  scale_colour_manual(values = c("1.5C (High-Ambition)" = "#1a9641",
                                 "2C (Medium-Ambition)" = "#d7191c"),
                      name = NULL) +
  scale_shape_manual(values = c("p < 0.05" = 16, "ns / n<4" = 1), name = NULL) +
  scale_size_continuous(range = c(1.2, 4.5), name = "min group n") +
  labs(title = "High-CDR minus High-RE: outcome contrast across approaches",
       subtitle = "Relative difference in per-capita medians (% of High-RE median). Right of 0 = higher under High-CDR.",
       x = "Median(High-CDR) − Median(High-RE), % of High-RE (per-capita)", y = NULL,
       caption = paste("Filled = Mann-Whitney U p < 0.05; hollow = not significant or n < 4 per group.",
                       "A-E = top tercile, F-J = above median. Size = smaller group n.")) +
  theme_cmp() +
  theme(axis.text.y = element_text(size = 8))
save_fig(fig_R2, "R2_contrast_forest.png", 16, 12)

write.csv(contrast %>%
            transmute(approach, Ambition, outcome = outcome_lab,
                      med_HighCDR_percapita = `med_High-CDR`,
                      med_HighRE_percapita  = `med_High-RE`,
                      diff_percapita = diff, rel_pct_percapita = rel,
                      med_HighCDR_abs = `med_abs_High-CDR`,
                      med_HighRE_abs  = `med_abs_High-RE`,
                      diff_abs, rel_pct_abs = rel_abs,
                      n_HighCDR = `n_High-CDR`, n_HighRE = `n_High-RE`,
                      p_value, p_lab, cliffs_delta, significant = sig) %>%
            arrange(outcome, Ambition, approach),
          file.path(COMP_DIR, "approach_outcome_contrast.csv"), row.names = FALSE)

robustness <- contrast %>%
  filter(!is.na(diff)) %>%
  group_by(outcome_lab, Ambition) %>%
  summarise(n_approaches   = n(),
            n_significant  = sum(sig, na.rm = TRUE),
            n_cdr_higher   = sum(diff > 0, na.rm = TRUE),
            n_re_higher    = sum(diff < 0, na.rm = TRUE),
            consistent_dir = max(sum(diff > 0, na.rm = TRUE),
                                 sum(diff < 0, na.rm = TRUE)),
            .groups = "drop") %>%
  arrange(outcome_lab, Ambition)
write.csv(robustness,
          file.path(COMP_DIR, "approach_robustness_summary.csv"), row.names = FALSE)
cat("\nRobustness (significant & same-signed across all 10 approaches):\n")
print(as.data.frame(robustness))

# ---- R3: regional breakdown, one figure per approach, per headline outcome --
# Curated to 4 outcomes to keep the file count manageable (10 approaches x 4 =
# 40 figures). Add rows to KEY_OUTCOMES to extend.
KEY_OUTCOMES <- OUTCOMES %>%
  filter(col_pc %in% c("mort_per_1k", "headcount_pct",
                       "gap_GJ_pc", "net_re_jobs_per_1k"))

make_R3 <- function(id, ycol, ylab) {
  d <- path_all %>% filter(approach == id, !is.na(.data[[ycol]]))
  if (nrow(d) == 0) return(invisible(NULL))
  ncell <- d %>% count(Region_lab, Ambition, Pathway_excl, name = "n")
  ggplot(d, aes(x = Pathway_excl, y = .data[[ycol]], fill = Pathway_excl)) +
    geom_violin(alpha = 0.35, colour = NA, scale = "width", trim = TRUE) +
    geom_boxplot(width = 0.18, outlier.size = 0.4, alpha = 0.9,
                 colour = "grey25", fill = "white") +
    stat_summary(fun = median, geom = "point", shape = 18, size = 2, colour = "grey10") +
    geom_text(data = ncell, aes(x = Pathway_excl, y = Inf, label = paste0("n=", n)),
              inherit.aes = FALSE, vjust = 1.4, size = 2.5, colour = "grey35") +
    facet_grid(Ambition ~ Region_lab, scales = "free_y") +
    scale_fill_manual(values = PATH_COLS, guide = "none") +
    scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0.05, 0.12))) +
    labs(title = paste0(ylab, " — Approach ", id, " (", APPROACH_LABELS[id], ")"),
         subtitle = "High-CDR vs High-RE, by R10 region and World (5-region sum), across ambition",
         x = NULL, y = ylab,
         caption = "Violin = distribution; box = IQR; diamond = median; n labelled per cell.") +
    theme_cmp() +
    theme(strip.text.x = element_text(size = 8))
}

cat("\nGenerating regional breakdown figures (R3)...\n")
for (id in APPROACHES) {
  for (i in seq_len(nrow(KEY_OUTCOMES))) {
    oc <- KEY_OUTCOMES[i, ]
    p <- make_R3(id, oc$col_pc, oc$label_pc)
    if (!is.null(p))
      save_fig(p, paste0("R3_", oc$col_pc, "_", id, ".png"), 18, 7)
  }
}

cat("\n=== CROSS-APPROACH FIGURES COMPLETE ===\n")
cat("Figures: ", FIG_DIR, "\n")
cat("Contrast table: ", file.path(COMP_DIR, "approach_outcome_contrast.csv"), "\n")
cat("Robustness table: ", file.path(COMP_DIR, "approach_robustness_summary.csv"), "\n")
