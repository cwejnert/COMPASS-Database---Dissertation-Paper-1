# =============================================================================
# Z9 — FIGURE SET FOR THE CENTURY DESIGN, HIGH-RE AGAINST HIGH-CDR
#
# The primary axis is ALL CDR -- engineered removal plus land-based removal --
# because the paper asks whether renewables-led mitigation beats CDR-led
# mitigation, and a CDR axis that excludes land names a subset of what it
# claims. The engineered-only axis is carried throughout as the sensitivity.
#
# Visual language of the August deck is kept: gold = High-RE, blue = High-CDR,
# positive always favours High-RE.
#
#   F1  scorecard          % change in the median, 9 regions + World
#   F2  World detail       raw gap with its 95% interval, native units
#   F3  axis sensitivity   all-CDR vs engineered-only, all three outcomes
#   F4  coverage flow      classified -> complete jobs / deprivation / mortality
#   F5  the zero problem   what the non-reporting scenarios do to the arms
#
# USAGE: Rscript Z9_century_figs.R      (run from the repo root)
#
# Figures are written to paper1_analysis/figures_century/ rather than the
# working directory, so running from the repo root does not scatter PNGs there.
# =============================================================================
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(ggplot2);library(purrr)})
options(width=170)
CMT<-"#2F79BF"; RE<-"#C68A20"; INK<-"#1F2A36"; MUTE<-"#5B6B7B"
LINE<-"#D9E1E8"; SURF<-"#FCFCFB"; GREY<-"#9AA7B4"; RED<-"#A33A3A"; GREEN<-"#1B7A4B"
th <- theme_minimal(base_size=9) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_line(colour="#EDF1F4",linewidth=0.4),
        plot.background=element_rect(fill=SURF,colour=NA),
        panel.background=element_rect(fill=SURF,colour=NA),
        strip.text=element_text(face="bold",colour=INK,size=8.5),
        axis.text=element_text(colour=INK,size=7.6),
        axis.title=element_text(colour=MUTE,size=8),
        legend.text=element_text(colour=INK,size=7.6),
        plot.title=element_text(face="bold",colour=INK,size=11.5),
        plot.subtitle=element_text(colour=MUTE,size=8.2,lineheight=1.15),
        plot.caption=element_text(colour=MUTE,size=7,hjust=0,lineheight=1.25))
FIGDIR <- "paper1_analysis/figures_century"
dir.create(FIGDIR, showWarnings = FALSE, recursive = TRUE)
fig <- function(name) file.path(FIGDIR, name)

ORD9 <- c("WORLD","Africa","China+","Europe","India+","Latin America",
          "Middle East","North America","Reforming econ.","Rest of Asia")
DROP <- "R10PAC_OECD"
amb_lab <- function(x) factor(x, levels=c("1.5C","2C"),
                              labels=c("1.5C high ambition","2C medium ambition"))

# EVERYTHING COMES FROM LAND_PRIMARY.rds (V5_land_primary.R), which carries both
# axes on one pipeline: repaired scenario keys, labels rebuilt with the published
# rule and checked exactly against the published classification for approach A,
# and the strict ten-region World aggregation.
#
# The PRIMARY axis is now "with land" -- all CDR, engineered and land-based --
# because the paper's question is High-RE against High-CDR and a CDR axis that
# excludes land names a subset of what it claims. The engineered axis is retained
# throughout as the sensitivity, and it is the CONSERVATIVE one: it agrees on
# direction and reports a smaller advantage.
#
# Do NOT go back to CENTURY_RESULTS or STRICT_WORLD here. Both were built with a
# normalisation that could not match the labels file's mangled degree signs, so
# both silently dropped 71 classified scenarios. See V6_key_repair.R.
WKEY  <- "Aggregated R10 regions"
PRIM  <- "with land"
LP    <- readRDS("LAND_PRIMARY.rds")$grid
R <- LP %>% filter(axis == PRIM, approach == "A", primary, !is.na(gap)) %>%
  mutate(fam = factor(family, levels=c("Jobs","Deprivation","Health")))
stopifnot(sum(R$Region == WKEY) == 6)   # 3 outcomes x 2 ambition levels
H <- R %>% filter(Region != DROP)
cat("cells:", nrow(H), "| favour High-RE:", sum(H$gap>0),
    "| significant for:", sum(H$gap>0 & H$sig), "| against:", sum(H$gap<0 & H$sig), "\n")

# ------------------------------------------------------------- F1 scorecard --
d1 <- H %>% mutate(regf = factor(reg, levels=rev(ORD9)), ambl = amb_lab(amb),
                   cellv = case_when(gap>0 & sig ~"High-RE, significant",
                                     gap>0       ~"High-RE (n.s.)",
                                     gap<0 & sig ~"High-CMT, significant",
                                     TRUE        ~"High-CMT (n.s.)"),
                   txt = ifelse(abs(pct) < 1, sprintf("%+.1f%%", pct),
                                              sprintf("%+.0f%%", pct)))
p1 <- ggplot(d1, aes(fam, regf, fill=cellv)) +
  geom_tile(colour=SURF, linewidth=1.6) +
  geom_text(aes(label=txt, colour=cellv), size=2.55, fontface="bold", show.legend=FALSE) +
  geom_hline(yintercept=9.5, colour=INK, linewidth=0.7) +
  facet_wrap(~ambl) +
  scale_fill_manual(values=c(`High-RE, significant`=RE, `High-RE (n.s.)`="#F0DFB8",
                             `High-CMT, significant`=CMT, `High-CMT (n.s.)`="#CBDCF0"),
                    breaks=c("High-RE, significant","High-RE (n.s.)",
                             "High-CMT (n.s.)","High-CMT, significant")) +
  scale_colour_manual(values=c(`High-RE, significant`="#3B2B08",`High-RE (n.s.)`=INK,
                               `High-CMT, significant`="#FFFFFF",`High-CMT (n.s.)`=INK)) +
  labs(title=sprintf("High-RE is better in %d of %d comparisons (%.0f%%)",
                     sum(H$gap>0), nrow(H), 100*mean(H$gap>0)),
       subtitle=paste0("Percentage change in the median outcome, High-CDR to High-RE, ",
                       "signed so positive favours High-RE.\nSaturated cells clear a ",
                       "cluster-robust 95% interval on the raw difference. CDR is ALL ",
                       "removal,\nengineered and land-based. Cumulative 2020-2100."),
       x=NULL, y=NULL, fill=NULL,
       caption=with(H %>% group_by(fam) %>%
                      summarise(n=n(), f=sum(gap>0), s=sum(gap>0 & sig),
                                a=sum(gap<0 & sig), .groups="drop"),
         paste0("Jobs ", f[fam=="Jobs"], " of ", n[fam=="Jobs"],
                ", all ", s[fam=="Jobs"], " significant — the only outcome of which that ",
                "is true.\nDeprivation leads in ", f[fam=="Deprivation"], " of ",
                n[fam=="Deprivation"], " (", s[fam=="Deprivation"], " significant, ",
                a[fam=="Deprivation"], " against). Health ", f[fam=="Health"], " of ",
                n[fam=="Health"], " (", s[fam=="Health"], " significant, ",
                a[fam=="Health"], " against).\nPacific OECD is excluded from the regional ",
                "rows and retained inside the World aggregate."))) +
  th + theme(legend.position="top", panel.grid=element_blank())
ggsave(fig("F1_scorecard.png"), p1, width=8.4, height=4.8, dpi=210)

# ---------------------------------------------------------- F2 World detail --
d2 <- R %>% filter(Region=="Aggregated R10 regions") %>%
  mutate(ambl = amb_lab(amb),
         unit = recode(as.character(fam),
                       Jobs="job-years per 1,000 people",
                       Deprivation="GJ per capita of gap",
                       Health="million cumulative deaths"),
         panel = factor(paste0(fam, "\n", unit), levels=unique(paste0(fam,"\n",unit))),
         lbl = sprintf("%.1f  vs  %.1f", raw_cmt, raw_re))
p2 <- ggplot(d2, aes(gap, ambl)) +
  geom_vline(xintercept=0, colour=INK, linewidth=0.5) +
  geom_errorbarh(aes(xmin=lo, xmax=hi, colour=sig), height=0, linewidth=0.9) +
  geom_point(aes(colour=sig), size=3) +
  geom_text(aes(label=lbl), vjust=-1.5, size=2.5, colour=MUTE) +
  facet_wrap(~panel, scales="free_x") +
  scale_colour_manual(values=c(`TRUE`=RE, `FALSE`=GREY), guide="none") +
  scale_y_discrete(expand=expansion(add=c(0.55,0.85))) +
  labs(title="World: how big is the gap, and does it clear zero?",
       subtitle=paste0("Raw difference between the arm medians in each outcome's own units, ",
                       "with a cluster-robust 95% interval.\nGrey text is the two arm medians, ",
                       "High-CDR then High-RE. Positive favours High-RE."),
       x="raw difference (High-RE advantage, native units)", y=NULL,
       caption=with(d2, paste0(
         "Jobs clears by a wide margin at both ambition levels (+",
         sprintf("%.0f", gap[fam=="Jobs" & amb=="1.5C"]), " and +",
         sprintf("%.0f", gap[fam=="Jobs" & amb=="2C"]),
         " job-years per 1,000).\nDeprivation closes the gap by ",
         sprintf("%.2f", gap[fam=="Deprivation" & amb=="1.5C"]), " GJ per capita at 1.5C, ",
         "which clears, and ", sprintf("%.2f", gap[fam=="Deprivation" & amb=="2C"]),
         " at 2C, which does not.\nMortality clears at 1.5C only — ",
         sprintf("%.1f", gap[fam=="Health" & amb=="1.5C"]),
         " million deaths avoided — and Europe supplies most of it."))) + th
ggsave(fig("F2_world.png"), p2, width=8.4, height=3.5, dpi=210)

# ------------------------------------------------------- F3 axis sensitivity --
# Both axes come from LAND_PRIMARY, so this is a like-for-like comparison on
# identical outcome data: only the labels differ. The grey point is the PRIMARY
# all-CDR result and the arrow head is the engineered-only sensitivity, so the
# arrow shows what NARROWING the CDR definition does to each cell.
d3 <- LP %>% filter(approach=="A", primary, Region != DROP, !is.na(gap)) %>%
  select(axis, reg, amb, family, gap, sig) %>%
  pivot_wider(names_from=axis, values_from=c(gap,sig)) %>%
  rename(land=`gap_with land`, eng=gap_engineered) %>%
  filter(!is.na(eng), !is.na(land)) %>%
  mutate(famf = factor(family, levels=c("Jobs","Deprivation","Health")),
         ambl = amb_lab(amb),
         moved = ifelse(sign(eng)!=sign(land), "changes sign", "same sign"))
p3 <- ggplot(d3, aes(y=reorder(reg, land))) +
  annotate("rect", xmin=-Inf, xmax=0, ymin=-Inf, ymax=Inf, fill="#EAF0F7", alpha=0.85) +
  geom_vline(xintercept=0, colour=INK, linewidth=0.5) +
  geom_segment(aes(x=land, xend=eng, yend=reorder(reg, land)),
               colour=GREY, linewidth=0.7,
               arrow=arrow(length=unit(0.05,"in"), type="closed")) +
  geom_point(aes(x=land), colour=INK, size=2.3) +
  geom_point(aes(x=eng, colour=moved), size=1.9) +
  facet_grid(ambl ~ famf, scales="free_x") +
  scale_colour_manual(values=c(`same sign`=GREY, `changes sign`=RED), name=NULL) +
  labs(title="Narrowing the CDR axis to engineered removal only",
       subtitle=paste0("Dark point = the PRIMARY all-CDR result. Arrow head = the ",
                       "engineered-only sensitivity.\nPositive favours High-RE. Both axes ",
                       "are scored on identical outcome data, so only the labels differ."),
       x="High-RE advantage (each outcome in its own units)", y=NULL,
       caption=paste0("Of the ", nrow(d3), " cells scored on both axes, ",
                      sum(d3$moved=="changes sign"), " change sign. The engineered axis is ",
                      "the CONSERVATIVE one:\nit reports a SMALLER High-RE jobs advantage ",
                      "at World 1.5C (+405 against +464), because land-heavy CDR\nscenarios ",
                      "carry fewer energy jobs. The headline is not resting on the wider ",
                      "definition.")) +
  th + theme(legend.position="top")
ggsave(fig("F3_land.png"), p3, width=8.4, height=5.0, dpi=210)

# ---------------------------------------------------------- F4 coverage flow --
# Built straight off the primary grid's own arm sizes, so the bars are by
# construction the samples the result tables were computed on -- there is no
# second join that could disagree with them.
d4 <- LP %>% filter(axis==PRIM, approach=="A", reg=="WORLD",
                    outcome %in% c("net_re_jobs_per_1k","gap_GJ_pc","mort_per_1k")) %>%
  select(amb, family, n_cmt, n_re) %>%
  pivot_longer(c(n_cmt, n_re), names_to="k", values_to="n") %>%
  mutate(arm = ifelse(k=="n_cmt", "High-CDR", "High-RE"),
         stage = recode(family, Jobs="Complete jobs",
                        Deprivation="Complete deprivation",
                        Health="Complete mortality")) %>%
  select(amb, arm, stage, n) %>%
  bind_rows(readRDS("LAND_PRIMARY.rds")$labels_land %>% filter(approach=="A") %>%
              count(amb, Pathway) %>%
              transmute(amb, arm = ifelse(Pathway=="High-RE","High-RE","High-CDR"),
                        stage = "Classified", n)) %>%
  mutate(stage = factor(stage, levels=c("Classified","Complete jobs",
                                        "Complete deprivation","Complete mortality")),
         ambl = amb_lab(amb))
stopifnot(!any(is.na(d4$n)), nrow(d4) == 16)   # 4 stages x 2 arms x 2 ambitions
p4 <- ggplot(d4, aes(stage, n, fill=arm)) +
  geom_col(position=position_dodge(width=0.72), width=0.66) +
  geom_text(aes(label=n), position=position_dodge(width=0.72),
            vjust=-0.45, size=2.5, colour=INK) +
  facet_wrap(~ambl, scales="free_y") +
  scale_fill_manual(values=c(`High-CDR`=CMT, `High-RE`=RE), name=NULL) +
  scale_y_continuous(expand=expansion(mult=c(0,0.14))) +
  labs(title="How many scenarios each outcome can actually use",
       subtitle=paste0("A World figure is computed only when all ten R10 regions are present ",
                       "for THAT outcome.\nThe three are gated independently — missing ",
                       "mortality does not blank jobs or deprivation."),
       x=NULL, y="scenarios",
       caption=paste0("Mortality is always the binding constraint, because it additionally ",
                      "requires all five PM2.5 precursors\nreported directly at R10 level — ",
                      "AND because its target list was drawn against the engineered axis, so ",
                      "the 80\nscenarios the all-CDR axis newly admits have no mortality run ",
                      "at all. Jobs and deprivation retain different\nnumbers of scenarios, ",
                      "which is exactly why the outcomes must be gated separately.")) +
  th + theme(legend.position="top", axis.text.x=element_text(size=7.2))
ggsave(fig("F4_coverage.png"), p4, width=8.4, height=4.2, dpi=210)

# ------------------------------------------------------------ F5 the zeros ---
# On the PRIMARY axis, using its own labels and its own strict World table.
# The renewables axis is identical under both CDR definitions, so the
# zero-renewables problem is a property of the RE axis and applies either way;
# what changes is which scenarios the CDR axis then admits into the arm.
LPO <- readRDS("LAND_PRIMARY.rds")
Z  <- LPO$labels_land %>% filter(approach=="A")
SW <- LPO$world %>% filter(approach=="A") %>%
  select(Model, Scenario, jobs=net_re_jobs_per_1k, gap=gap_GJ_pc)
d5 <- Z %>% inner_join(SW, by=c("Model","Scenario")) %>%
  filter(Pathway=="High-CMT", !is.na(jobs)) %>%
  mutate(grp = ifelse(renewables==0, "renewables NOT reported\n(filled with zero)",
                                     "renewables reported"),
         ambl = amb_lab(amb))
z5 <- d5 %>% mutate(zero = renewables == 0) %>%
  group_by(amb) %>% summarise(n = n(), z = sum(zero), .groups="drop")
z5cap <- paste0("On the all-CDR axis: ", z5$z[z5$amb=="1.5C"], " of ", z5$n[z5$amb=="1.5C"],
                " High-CDR scenarios at 1.5C and ", z5$z[z5$amb=="2C"], " of ",
                z5$n[z5$amb=="2C"], " at 2C.\nNONE in the High-RE arm, by construction: a ",
                "scenario cannot be top-tercile on renewables while reporting none.\n",
                "SCI vetting already excludes every one of them.")

p5 <- ggplot(d5, aes(grp, jobs, fill=grp)) +
  geom_hline(yintercept=0, colour=INK, linewidth=0.4, linetype="dashed") +
  geom_boxplot(outlier.shape=NA, width=0.5, colour=GREY, linewidth=0.4, alpha=0.85) +
  # The non-reporting group is a flat line at zero, which a boxplot alone renders
  # almost invisibly -- exactly the group the figure exists to show. The points
  # give it presence and show how many scenarios sit there.
  geom_jitter(width=0.13, height=0, shape=21, size=1.25, stroke=0.3,
              colour=INK, fill="#FFFFFF", alpha=0.7) +
  facet_wrap(~ambl) +
  scale_fill_manual(values=c(`renewables reported`="#CBDCF0",
                             `renewables NOT reported\n(filled with zero)`="#F2C9C9"),
                    guide="none") +
  labs(title="The High-CDR arm contains scenarios that never reported renewables",
       subtitle=paste0("World net energy employment, job-years per 1,000, for the High-CDR ",
                       "arm only.\nEvery zero-renewable scenario has NO Renewable Capacity ",
                       "row at all — not one is a reported zero."),
       x=NULL, y="job-years per 1,000 people",
       caption=z5cap) + th
ggsave(fig("F5_zeros.png"), p5, width=8.4, height=4.2, dpi=210)

cat("\nwritten to", FIGDIR, ":\n  F1_scorecard  F2_world  F3_land  F4_coverage  F5_zeros\n")
