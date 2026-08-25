# =============================================================================
# Z9 — FIGURE SET FOR THE REVAMPED (ENGINEERED-CMT, CENTURY) DESIGN
#
# The previous Y-series was built on the 2020-2050 Total-CDR design and none of
# it is valid now. This rebuilds the figures the deck needs, on CENTURY_RESULTS
# and the new sensitivity outputs, keeping the visual language of the August
# deck: gold = High-RE, blue = High-CMT, positive always favours High-RE.
#
#   F1  scorecard          % change in the median, 9 regions + World
#   F2  World detail       raw gap with its 95% interval, native units
#   F3  land sensitivity   engineered vs with-land, all three outcomes
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

R <- readRDS("CENTURY_RESULTS.rds") %>%
  filter(approach=="A", primary, !is.na(gap)) %>%
  mutate(fam = factor(family, levels=c("Jobs","Deprivation","Health")))
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
       subtitle=paste0("Percentage change in the median outcome, High-engineered-CMT to ",
                       "High-RE, signed so positive favours High-RE.\nBold cells clear a ",
                       "cluster-robust 95% interval on the raw difference. Cumulative ",
                       "2020-2100."),
       x=NULL, y=NULL, fill=NULL,
       caption=paste0("Jobs is unanimous AND fully significant — 20 of 20 cells clear the ",
                      "interval, the only outcome of which that is true.\nDeprivation leads ",
                      "in 13 of 20 with three reversals. Health is scattered, with Europe ",
                      "carrying the World result.\nPacific OECD is excluded from the regional ",
                      "rows and retained inside the World aggregate.")) +
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
                       "High-engineered-CMT then High-RE. Positive favours High-RE."),
       x="raw difference (High-RE advantage, native units)", y=NULL,
       caption=paste0("Jobs clears by a wide margin at both ambition levels. Deprivation is ",
                      "large (-5.7 and -4.3 GJ per capita) but its\ninterval touches zero. ",
                      "Mortality clears at 1.5C only, and Europe supplies most of that ",
                      "8.7 million.")) + th
ggsave(fig("F2_world.png"), p2, width=8.4, height=3.5, dpi=210)

# ------------------------------------------------------- F3 land sensitivity --
if (file.exists("W12_LAND.rds") && file.exists("W13_ZEROS_LANDMORT.rds")) {
  S  <- readRDS("W12_LAND.rds")$scores %>% filter(Region != DROP, !is.na(gap))
  SM <- readRDS("W13_ZEROS_LANDMORT.rds")$mort %>% filter(Region != DROP, !is.na(gap)) %>%
    mutate(family = "Health")
  d3 <- bind_rows(S %>% select(axis, reg, amb, family, gap, sig),
                  SM %>% select(axis, reg, amb, family, gap, sig)) %>%
    pivot_wider(names_from=axis, values_from=c(gap,sig)) %>%
    rename(eng=`gap_engineered`, land=`gap_with land`) %>%
    filter(!is.na(eng), !is.na(land)) %>%
    mutate(famf = factor(family, levels=c("Jobs","Deprivation","Health")),
           ambl = amb_lab(amb),
           moved = ifelse(sign(eng)!=sign(land), "changes sign", "same sign"))
  p3 <- ggplot(d3, aes(y=reorder(reg, eng))) +
    annotate("rect", xmin=-Inf, xmax=0, ymin=-Inf, ymax=Inf, fill="#EAF0F7", alpha=0.85) +
    geom_vline(xintercept=0, colour=INK, linewidth=0.5) +
    geom_segment(aes(x=eng, xend=land, yend=reorder(reg, eng)),
                 colour=GREY, linewidth=0.7,
                 arrow=arrow(length=unit(0.05,"in"), type="closed")) +
    geom_point(aes(x=eng), colour=GREY, size=1.9) +
    geom_point(aes(x=land, colour=moved), size=2.3) +
    facet_grid(ambl ~ famf, scales="free_x") +
    scale_colour_manual(values=c(`same sign`=INK, `changes sign`=RED), name=NULL) +
    labs(title="Putting land-based CDR back into the carbon-management axis",
         subtitle=paste0("Grey = engineered CMT (published). Arrow head = engineered plus ",
                         "land-based removal.\nPositive favours High-RE. Both axes scored on ",
                         "identical outcome data, so only the labels differ."),
         x="High-RE advantage (each outcome in its own units)", y=NULL,
         caption=paste0("NO SCENARIO SWITCHES ARMS: of 530 classified under both axes, 100% ",
                        "keep the same label. The axes correlate 0.898.\nIncluding land makes ",
                        "the High-RE jobs advantage LARGER (World 1.5C +396 to +456) because ",
                        "land-heavy carbon-management\nscenarios carry fewer energy jobs. ",
                        "Excluding land is therefore the conservative choice.")) +
    th + theme(legend.position="top")
  ggsave(fig("F3_land.png"), p3, width=8.4, height=5.0, dpi=210)
}

# ---------------------------------------------------------- F4 coverage flow --
FL <- readRDS("STRICT_WORLD.rds")$flow %>% filter(approach=="A") %>%
  group_by(amb, Pathway) %>%
  summarise(Classified = n(),
            `Complete jobs`        = sum(world_complete_jobs, na.rm=TRUE),
            `Complete deprivation` = sum(world_complete_gap, na.rm=TRUE),
            .groups="drop")
MC <- readRDS("W13_ZEROS_LANDMORT.rds")$mort %>%
  filter(axis=="engineered", reg=="WORLD") %>%
  select(amb, n_cmt, n_re) %>%
  pivot_longer(c(n_cmt,n_re), names_to="k", values_to="Complete mortality") %>%
  mutate(Pathway = ifelse(k=="n_cmt","High-CMT","High-RE")) %>% select(-k)
d4 <- FL %>% left_join(MC, by=c("amb","Pathway")) %>%
  pivot_longer(-c(amb,Pathway), names_to="stage", values_to="n") %>%
  mutate(stage = factor(stage, levels=c("Classified","Complete jobs",
                                        "Complete deprivation","Complete mortality")),
         ambl = amb_lab(amb),
         arm = ifelse(Pathway=="High-RE","High-RE","High-engineered-CMT"))
p4 <- ggplot(d4, aes(stage, n, fill=arm)) +
  geom_col(position=position_dodge(width=0.72), width=0.66) +
  geom_text(aes(label=n), position=position_dodge(width=0.72),
            vjust=-0.45, size=2.5, colour=INK) +
  facet_wrap(~ambl, scales="free_y") +
  scale_fill_manual(values=c(`High-engineered-CMT`=CMT, `High-RE`=RE), name=NULL) +
  scale_y_continuous(expand=expansion(mult=c(0,0.14))) +
  labs(title="How many scenarios each outcome can actually use",
       subtitle=paste0("A World figure is computed only when all ten R10 regions are present ",
                       "for THAT outcome.\nThe three are gated independently — missing ",
                       "mortality does not blank jobs or deprivation."),
       x=NULL, y="scenarios",
       caption=paste0("Mortality is always the binding constraint, because it additionally ",
                      "requires all five PM2.5 precursors reported\ndirectly at R10 level. ",
                      "Deprivation retains MORE High-CMT scenarios than jobs at 1.5C (53 ",
                      "against 42), which is\nexactly why the outcomes must be gated ",
                      "separately rather than jointly.")) +
  th + theme(legend.position="top", axis.text.x=element_text(size=7.2))
ggsave(fig("F4_coverage.png"), p4, width=8.4, height=4.2, dpi=210)

# ------------------------------------------------------------ F5 the zeros ---
Z <- readRDS("W13_ZEROS_LANDMORT.rds")$zeros %>% filter(approach=="A", !is.na(Pathway))
SW <- readRDS("STRICT_WORLD.rds")$world %>% filter(approach=="A") %>%
  select(Model, Scenario, jobs=net_re_jobs_per_1k, gap=gap_GJ_pc)
d5 <- Z %>% inner_join(SW, by=c("Model","Scenario")) %>%
  filter(Pathway=="High-CMT", !is.na(jobs)) %>%
  mutate(grp = ifelse(renewables==0, "renewables NOT reported\n(filled with zero)",
                                     "renewables reported"),
         ambl = amb_lab(amb))
p5 <- ggplot(d5, aes(grp, jobs, fill=grp)) +
  geom_boxplot(outlier.shape=21, outlier.size=1.2, width=0.5,
               colour=GREY, linewidth=0.4, alpha=0.85) +
  geom_hline(yintercept=0, colour=INK, linewidth=0.4, linetype="dashed") +
  facet_wrap(~ambl) +
  scale_fill_manual(values=c(`renewables reported`="#CBDCF0",
                             `renewables NOT reported\n(filled with zero)`="#F2C9C9"),
                    guide="none") +
  labs(title="The High-engineered-CMT arm contains scenarios that never reported renewables",
       subtitle=paste0("World net energy employment, job-years per 1,000, for the High-CMT ",
                       "arm only.\nAll 50 zero-renewable scenarios have NO Renewable Capacity ",
                       "row at all — not one is a reported zero."),
       x=NULL, y="job-years per 1,000 people",
       caption=paste0("16 of 64 High-CMT scenarios at 1.5C (a quarter of the arm) and 29 of ",
                      "239 at 2C. NONE in the High-RE arm, by construction.\nConcentrated in ",
                      "GCAM (33 of 50). SCI vetting already excludes every one of them.\n",
                      "Dropping them takes World deprivation from non-significant to ",
                      "significant at BOTH ambition levels.")) + th
ggsave(fig("F5_zeros.png"), p5, width=8.4, height=4.2, dpi=210)

cat("\nwritten to", FIGDIR, ":\n  F1_scorecard  F2_world  F3_land  F4_coverage  F5_zeros\n")
