# =============================================================================
# COMPASS — CROSS-APPROACH COMPARISON FIGURES (Paper 1)
#
# Compares the five scenario-selection approaches (A-E) produced by
# COMPASS_master_analysis.R, on two fronts:
#
#   PART 1  SAMPLE-SET SIZE
#     S1  Selection funnel: universe -> after vetting -> classified
#         High-CDR-only / High-RE-only, per approach x ambition.
#     S2  Pathway counts (High-CDR vs High-RE) per approach x ambition.
#     S3  Approach overlap: Jaccard heatmap of selected scenario sets.
#     S4  Ambition-strategy shift: AR6-category vs peak-warming sample split.
#
#   PART 2  RESULTS
#     R1  Outcome distributions by Pathway (High-CDR vs High-RE), faceted by
#         ambition, panelled across approaches — per outcome.
#     R2  Contrast forest plot: median(High-CDR) - median(High-RE) per
#         outcome x ambition x approach, so you can see whether the sign and
#         size of the CDR-vs-RE difference is robust to the sampling choice.
#
# INPUTS (written by COMPASS_master_analysis.R):
#   OUT_DIR/comparison/approach_scenario_counts.csv
#   OUT_DIR/comparison/approach_pathway_counts.csv
#   OUT_DIR/comparison/approach_set_overlap.csv
#   OUT_DIR/comparison/approach_summary.csv
#   OUT_DIR/approach_<X>/compass_master_dataset_<X>.csv
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

APPROACHES <- c("A", "B", "C", "D", "E")
APPROACH_LABELS <- c(
  A = "A: all · AR6",
  B = "B: all · peak-warming",
  C = "C: full SCI · AR6",
  D = "D: full SCI · peak-warming",
  E = "E: partial SCI · AR6"
)
PATH_COLS <- c("High-CDR" = "#2166ac", "High-RE" = "#d6604d")
AMB_ORDER <- c("1.5C (High-Ambition)", "2C (Medium-Ambition)")

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

# Per-approach master datasets + pathway classification, stacked with an
# `approach` column. Master rows are one per scenario x region x deployment-var;
# we keep Total CDR rows on the Aggregated R10 region for the outcome figures,
# and join the pathway (High-CDR/High-RE) classification.
load_master <- function(id) {
  m <- read_csv(file.path(OUT_DIR, paste0("approach_", id),
                          paste0("compass_master_dataset_", id, ".csv")),
                show_col_types = FALSE)
  p <- read_csv(file.path(OUT_DIR, paste0("approach_", id),
                          paste0("compass_pathway_tercile_", id, ".csv")),
                show_col_types = FALSE) %>%
    select(Model, Scenario, Pathway_excl, high_cdr_only, high_re_only)
  m %>%
    filter(Variable == "Total CDR") %>%
    left_join(p, by = c("Model", "Scenario")) %>%
    mutate(approach = id)
}
master_all <- map_dfr(APPROACHES, load_master)
cat("master_all rows:", nrow(master_all), "\n")

OUTCOMES <- tribble(
  ~col,                            ~label,                              ~better,
  "cumulative_deaths_mln",         "PM2.5 mortality (mln, cumul.)",     "lower",
  "mean_headcount_millions",       "Energy deprivation (mln)",          "lower",
  "cumulative_gap_EJ",             "DLE energy gap (EJ, cumul.)",       "lower",
  "cumulative_implied_CO2_GtCO2",  "Implied CO2 of DLE gap (GtCO2)",    "lower",
  "jobs_Renewables",               "Renewable energy jobs (000s)",      "higher",
  "jobs_Fossil",                   "Fossil energy jobs (000s)",         "higher"
)
# Net RE jobs as a derived outcome
master_all <- master_all %>%
  mutate(net_re_jobs = jobs_Renewables - jobs_Fossil)


# =============================================================================
# PART 1 — SAMPLE-SET SIZE
# =============================================================================

# ---- S1: selection funnel ---------------------------------------------------
# universe (n_scenarios) -> classified High-CDR-only + High-RE-only
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
         approach_lab = APPROACH_LABELS[approach])

fig_S1 <- funnel %>%
  ggplot(aes(x = stage, y = n, fill = stage)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n), vjust = -0.3, size = 3) +
  facet_grid(Ambition ~ approach_lab, scales = "free_x", switch = "y") +
  scale_fill_manual(values = c("In sample" = "#9ecae1",
                               "High-CDR only" = "#2166ac",
                               "High-RE only" = "#d6604d"),
                    guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Scenario selection funnel by approach and ambition",
       subtitle = "In-sample (after vetting) then top-tercile High-CDR-only / High-RE-only classification",
       x = NULL, y = "Number of scenarios",
       caption = "Vetting: A/B none · C/D full SCI · E partial (tech-feasibility). Ambition: A/C/E AR6 category · B/D median peak warming.") +
  theme_cmp() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 8))
save_fig(fig_S1, "S1_selection_funnel.png", 15, 8)

# ---- S2: pathway counts (High-CDR vs High-RE) -------------------------------
fig_S2 <- pathway_counts %>%
  mutate(Ambition = factor(Ambition, levels = AMB_ORDER),
         approach_lab = APPROACH_LABELS[approach]) %>%
  ggplot(aes(x = approach_lab, y = n, fill = Pathway_excl)) +
  geom_col(position = position_dodge(0.75), width = 0.7) +
  geom_text(aes(label = n), position = position_dodge(0.75),
            vjust = -0.3, size = 3) +
  facet_wrap(~ Ambition, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = PATH_COLS, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "High-CDR vs High-RE scenario counts by approach",
       subtitle = "Top-tercile mutually-exclusive pathways, within each ambition group",
       x = NULL, y = "Number of scenarios",
       caption = "Small cells (e.g. C/D at 1.5C, n=5-7) are underpowered for results inference.") +
  theme_cmp() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
save_fig(fig_S2, "S2_pathway_counts.png", 12, 8)

# ---- S3: approach overlap (Jaccard heatmap) ---------------------------------
fig_S3 <- set_overlap %>%
  mutate(a = factor(a, levels = APPROACHES),
         b = factor(b, levels = rev(APPROACHES))) %>%
  ggplot(aes(x = a, y = b, fill = jaccard)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", jaccard)), size = 3.4) +
  scale_fill_gradient(low = "#f7fbff", high = "#08519c", name = "Jaccard",
                      limits = c(0, 1)) +
  coord_equal() +
  labs(title = "Overlap of selected scenario sets across approaches",
       subtitle = "Jaccard index (shared / union) of the Model×Scenario sets kept by each approach",
       x = NULL, y = NULL) +
  theme_cmp() +
  theme(panel.grid = element_blank())
save_fig(fig_S3, "S3_set_overlap.png", 8, 7)

# ---- S4: ambition-strategy shift (AR6 vs peak-warming) ----------------------
# Compare how the "all scenarios" sample splits into 1.5C/2C under the two
# ambition strategies: A (AR6) vs B (peak-warming); and C (AR6) vs D (warming).
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
       subtitle = "Same scenario pool, re-split into 1.5C / 2C by each strategy",
       x = NULL, y = "Number of scenarios",
       caption = "Peak-warming re-classification moves low-peak C3/C4 into 1.5C and drops scenarios peaking >2.0C.") +
  theme_cmp()
save_fig(fig_S4, "S4_ambition_strategy.png", 11, 6)


# =============================================================================
# PART 2 — RESULTS
# =============================================================================

path_long <- master_all %>%
  filter(Region == "Aggregated R10 regions",
         !is.na(Pathway_excl)) %>%
  mutate(Ambition = factor(Ambition, levels = AMB_ORDER),
         approach_lab = APPROACH_LABELS[approach])

# ---- R1: outcome distributions by pathway, per outcome ----------------------
make_R1 <- function(ycol, ylab, add_net = FALSE) {
  d <- path_long %>% filter(!is.na(.data[[ycol]]))
  if (nrow(d) == 0) return(invisible(NULL))
  # annotate n per cell
  ncell <- d %>% count(approach_lab, Ambition, Pathway_excl)
  p <- d %>%
    ggplot(aes(x = Pathway_excl, y = .data[[ycol]], fill = Pathway_excl)) +
    geom_violin(alpha = 0.35, colour = NA, scale = "width", trim = TRUE) +
    geom_boxplot(width = 0.18, outlier.size = 0.5, alpha = 0.9,
                 colour = "grey25", fill = "white") +
    stat_summary(fun = median, geom = "point", shape = 18, size = 2.4,
                 colour = "grey10") +
    facet_grid(Ambition ~ approach_lab, scales = "free_y") +
    scale_fill_manual(values = PATH_COLS, guide = "none") +
    scale_y_continuous(labels = comma_format()) +
    labs(title = ylab,
         subtitle = "High-CDR vs High-RE · Aggregated R10 regions · by ambition (rows) and approach (cols)",
         x = NULL, y = ylab,
         caption = "Violin = distribution; box = IQR; diamond = median.") +
    theme_cmp()
  p
}
for (i in seq_len(nrow(OUTCOMES))) {
  oc <- OUTCOMES[i, ]
  p <- make_R1(oc$col, oc$label)
  if (!is.null(p))
    save_fig(p, paste0("R1_", oc$col, ".png"), 15, 7)
}
# Net RE jobs (derived)
p_net <- make_R1("net_re_jobs", "Net RE jobs (RE - Fossil, 000s)")
if (!is.null(p_net)) save_fig(p_net, "R1_net_re_jobs.png", 15, 7)

# ---- R2: contrast forest plot ------------------------------------------------
# median(High-CDR) - median(High-RE) per outcome x ambition x approach.
contrast <- path_long %>%
  select(approach, approach_lab, Ambition, Pathway_excl,
         all_of(OUTCOMES$col), net_re_jobs) %>%
  pivot_longer(c(all_of(OUTCOMES$col), net_re_jobs),
               names_to = "outcome", values_to = "val") %>%
  filter(!is.na(val)) %>%
  group_by(approach, approach_lab, Ambition, outcome, Pathway_excl) %>%
  summarise(med = median(val), n = n(), .groups = "drop") %>%
  pivot_wider(names_from = Pathway_excl, values_from = c(med, n)) %>%
  mutate(
    diff = `med_High-CDR` - `med_High-RE`,
    # relative difference (% of High-RE median) for comparability across outcomes
    rel  = 100 * diff / abs(`med_High-RE`),
    n_min = pmin(`n_High-CDR`, `n_High-RE`, na.rm = TRUE),
    outcome_lab = c(OUTCOMES$label, "Net RE jobs (000s)")[
      match(outcome, c(OUTCOMES$col, "net_re_jobs"))]
  )

fig_R2 <- contrast %>%
  filter(!is.na(rel)) %>%
  mutate(Ambition = factor(Ambition, levels = AMB_ORDER)) %>%
  ggplot(aes(x = rel, y = approach_lab, colour = Ambition)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(aes(size = n_min), alpha = 0.85,
             position = position_dodge(width = 0.5)) +
  facet_wrap(~ outcome_lab, scales = "free_x", ncol = 3) +
  scale_colour_manual(values = c("1.5C (High-Ambition)" = "#1a9641",
                                 "2C (Medium-Ambition)" = "#d7191c"),
                      name = NULL) +
  scale_size_continuous(range = c(1.5, 5), name = "min group n") +
  labs(title = "High-CDR minus High-RE: outcome contrast across approaches",
       subtitle = "Relative difference in medians (% of High-RE median). Right of 0 = higher under High-CDR.",
       x = "Median(High-CDR) − Median(High-RE), % of High-RE", y = NULL,
       caption = "Robust conclusions = points that stay on the same side of 0 across approaches (A-E). Point size = smaller of the two group sizes.") +
  theme_cmp()
save_fig(fig_R2, "R2_contrast_forest.png", 15, 9)

# Also write the contrast table for the paper
write.csv(contrast %>%
            select(approach, Ambition, outcome = outcome_lab,
                   med_HighCDR = `med_High-CDR`, med_HighRE = `med_High-RE`,
                   diff, rel_pct = rel, n_HighCDR = `n_High-CDR`,
                   n_HighRE = `n_High-RE`),
          file.path(COMP_DIR, "approach_outcome_contrast.csv"), row.names = FALSE)

cat("\n=== CROSS-APPROACH FIGURES COMPLETE ===\n")
cat("Figures: ", FIG_DIR, "\n")
cat("Contrast table: ", file.path(COMP_DIR, "approach_outcome_contrast.csv"), "\n")
