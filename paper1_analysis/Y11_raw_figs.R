# =============================================================================
# Y11 — THE HEADLINE FIGURES, REBUILT ON THE RAW BASIS
#
# The results are now reported as raw levels with a cluster-bootstrap interval
# on the raw DIFFERENCE, so the figures that carried Cliff's delta have to be
# rebuilt on the same footing. Delta is retained only where the question is
# genuinely about rank overlap (the within-model test, Y10).
#
#   Y1  scorecard          % change in the raw median, 9 regions + World
#   Y2  World detail       raw gap with its 95% interval, native units
#   Y8  ammonia correction raw mortality gap before and after harmonising
#
# Y3 (robustness), Y4 (jobs decomposition), Y5 (label coherence), Y6 (NH3 gap),
# Y7 (variance) and Y9 (NH3 asymmetry) are unchanged -- none of them reports an
# effect size that the change of basis touches.
#
# USAGE: Rscript Y11_raw_figs.R
# =============================================================================
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(ggplot2);library(purrr)})
source("stratified.R.fns")
options(width=170)
CMT<-"#2F79BF"; RE<-"#C68A20"; INK<-"#1F2A36"; MUTE<-"#5B6B7B"
LINE<-"#D9E1E8"; SURF<-"#FCFCFB"; GREY<-"#9AA7B4"; RED<-"#A33A3A"
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
SH <- c(`Aggregated R10 regions`="WORLD",R10AFRICA="Africa",`R10CHINA+`="China+",
        R10EUROPE="Europe",`R10INDIA+`="India+",R10LATIN_AM="Latin America",
        R10MIDDLE_EAST="Middle East",R10NORTH_AM="North America",R10PAC_OECD="Pacific OECD",
        R10REF_ECON="Reforming econ.",R10REST_ASIA="Rest of Asia")
ORD9 <- c("WORLD","Africa","China+","Europe","India+","Latin America",
          "Middle East","North America","Reforming econ.","Rest of Asia")
DROP <- "R10PAC_OECD"
amb_lab <- function(x) factor(x, levels=c("1.5C","2C"),
                              labels=c("1.5C high ambition","2C medium ambition"))

R <- readRDS("RAW_RESULTS.rds") %>%
  filter(approach=="A full database", sample=="all scenarios", !is.na(gap)) %>%
  mutate(fam = factor(family, levels=c("Jobs","Energy deprivation","Health"),
                      labels=c("Jobs","Deprivation","Health")))
H <- R %>% filter(Region != DROP)
cat("cells:", nrow(H), "| favour High-RE:", sum(H$gap>0),
    "| significant for:", sum(H$gap>0 & H$sig_raw),
    "| against:", sum(H$gap<0 & H$sig_raw), "\n")

# ------------------------------------------------------------- Y1 scorecard --
# Percent change in the raw median. Where a baseline sits near zero a percentage
# is meaningless, so those cells print the absolute gap instead -- Europe's
# SCI jobs baseline of -0.05 would otherwise read +7542%.
FLOOR <- c(Jobs=0.5, Deprivation=0.30, Health=1.0)
d1 <- H %>% mutate(
  reg  = factor(SH[Region], levels=rev(ORD9)),
  ambl = amb_lab(amb),
  usep = abs(raw_cmt) >= FLOOR[as.character(fam)],
  # "-0%" is an artefact of rounding a sub-1% change; show a decimal there.
  txt  = ifelse(!usep, sprintf("%+.2f", gap),
         ifelse(abs(pct) < 1, sprintf("%+.1f%%", pct), sprintf("%+.0f%%", pct))),
  cellv= case_when(gap>0 & sig_raw ~"High-RE, significant",
                   gap>0           ~"High-RE (n.s.)",
                   gap<0 & sig_raw ~"High-CMT, significant",
                   TRUE            ~"High-CMT (n.s.)"))
p1 <- ggplot(d1, aes(fam, reg, fill=cellv)) +
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
       subtitle=paste0("Percentage change in the median outcome, High-CMT to High-RE, ",
                       "signed so positive always favours High-RE.\nBold cells clear a ",
                       "cluster-robust 95% interval on the RAW DIFFERENCE over 312 model ",
                       "x scenario-family clusters."),
       x=NULL, y=NULL, fill=NULL,
       caption=paste0("Cumulative 2020-2050, 590 scenarios. ",
                      sum(H$gap>0 & H$sig_raw), " cells significantly favour High-RE, ",
                      sum(H$gap<0 & H$sig_raw), " significantly favour High-CMT.\nJobs is ",
                      "unanimous; deprivation follows with three exceptions; health does not ",
                      "separate the pathways once ammonia is on a common basis.\nPacific OECD ",
                      "is excluded from the regional rows and retained inside the World aggregate.")) +
  th + theme(legend.position="top", panel.grid=element_blank())
ggsave("Y1_scorecard.png", p1, width=8.4, height=4.8, dpi=210)

# ---------------------------------------------------------- Y2 World detail --
# Native units per family, so three panels with free scales rather than one
# axis pretending job-years and deaths are commensurable.
d2 <- R %>% filter(Region=="Aggregated R10 regions") %>%
  mutate(ambl = amb_lab(amb),
         unit = recode(as.character(fam),
                       Jobs="job-years per 1,000 people",
                       Deprivation="GJ per capita of gap",
                       Health="PM2.5 deaths per 1,000"),
         panel = factor(paste0(fam, "\n", unit),
                        levels=unique(paste0(fam, "\n", unit))),
         lbl = sprintf("%.2f  vs  %.2f", raw_cmt, raw_re))
p2 <- ggplot(d2, aes(gap, ambl)) +
  geom_vline(xintercept=0, colour=INK, linewidth=0.5) +
  geom_errorbarh(aes(xmin=gap_lo, xmax=gap_hi, colour=sig_raw), height=0, linewidth=0.9) +
  geom_point(aes(colour=sig_raw), size=3) +
  geom_text(aes(label=lbl), vjust=-1.5, size=2.5, colour=MUTE) +
  facet_wrap(~panel, scales="free_x") +
  scale_colour_manual(values=c(`TRUE`=RE, `FALSE`=GREY), guide="none") +
  scale_y_discrete(expand=expansion(add=c(0.55,0.85))) +
  labs(title="World: how big is the gap, and does it clear zero?",
       subtitle=paste0("Raw difference between the arm medians in each outcome's own units, ",
                       "with a cluster-robust 95% interval.\nGrey text is the two arm ",
                       "medians, High-CMT then High-RE. Positive means High-RE is better."),
       x="raw difference (High-RE advantage, native units)", y=NULL,
       caption=paste0("Jobs +7.39 and +6.44 job-years per 1,000, both clearing zero comfortably. ",
                      "Deprivation closes the gap by 3.95 and 4.27 GJ per capita.\nHealth is ",
                      "-0.08 [-0.90, +1.19] and +0.74 [-3.11, +1.87] deaths per 1,000 -- ",
                      "intervals straddling zero in both directions.")) +
  th
ggsave("Y2_world_forest.png", p2, width=8.4, height=3.5, dpi=210)

# --------------------------------------------- Y8 ammonia correction, raw ----
C <- readRDS("NH3_MORT_REBUILD.rds") %>% filter(shown) %>%
  transmute(reg = SH[Region], ambl = amb_lab(amb),
            before = med_cmt_w - med_re_w,     # + = High-RE has fewer deaths
            after  = med_cmt_n - med_re_n) %>%
  mutate(moved = ifelse(sign(before)!=sign(after), "sign changes", "same sign"))
p8 <- ggplot(C, aes(y=reorder(reg, after))) +
  annotate("rect", xmin=-Inf, xmax=0, ymin=-Inf, ymax=Inf, fill="#EAF0F7", alpha=0.85) +
  geom_vline(xintercept=0, colour=INK, linewidth=0.5) +
  geom_segment(aes(x=before, xend=after, yend=reorder(reg, after)),
               colour=GREY, linewidth=0.7,
               arrow=arrow(length=unit(0.055,"in"), type="closed")) +
  geom_point(aes(x=before), colour=GREY, size=2.1) +
  geom_point(aes(x=after, colour=moved), size=2.6) +
  facet_wrap(~ambl) +
  scale_colour_manual(values=c(`same sign`=INK, `sign changes`=RED), name=NULL) +
  labs(title="Harmonising ammonia removes the mortality advantage",
       subtitle=paste0("Raw gap in PM2.5 deaths per 1,000, High-CMT minus High-RE. ",
                       "Positive means High-RE has fewer deaths.\nGrey = ammonia as each model ",
                       "reports it. Arrow head = ammonia removed for every model."),
       x="deaths per 1,000 avoided by High-RE", y=NULL,
       caption=paste0("At World the gap falls from 1.68 to -0.08 deaths per 1,000 at 1.5C ",
                      "and from 3.02 to 0.74 at 2C.\nEighteen of twenty cells move toward ",
                      "High-CMT; four change sign and none changes sign toward High-RE.\n",
                      "North America's remaining -2.71 rests on 47 versus 17 scenarios with ",
                      "no model overlap, and is reported as unresolvable.")) +
  th + theme(legend.position="top")
ggsave("Y8_nh3_correction.png", p8, width=8.4, height=4.4, dpi=210)

cat("\nwritten: Y1_scorecard.png  Y2_world_forest.png  Y8_nh3_correction.png\n")

# =============================================================================
# NUMBERS FOR THE DECK — printed so the slides can quote them exactly.
# =============================================================================
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")
line("WORLD, RAW")
print(R %>% filter(Region=="Aggregated R10 regions") %>%
      transmute(fam, amb, n=paste0(n_cmt,"v",n_re), cmt=round(raw_cmt,2), re=round(raw_re,2),
                gap=round(gap,2), CI=sprintf("[%+.2f, %+.2f]", gap_lo, gap_hi),
                pct=round(pct), sig=sig_raw) %>% as.data.frame())
line("EVERY REGION, RAW, FULL DATABASE")
print(H %>% transmute(reg=SH[Region], fam, amb, cmt=round(raw_cmt,2), re=round(raw_re,2),
                      gap=round(gap,2), pct=round(pct), sig=sig_raw) %>%
      arrange(fam, match(reg, ORD9), amb) %>% as.data.frame())
line("NH3 CORRECTION IN RAW UNITS")
print(readRDS("NH3_MORT_REBUILD.rds") %>% filter(shown) %>%
      transmute(reg=SH[Region], amb,
                before=round(med_cmt_w-med_re_w,2), after=round(med_cmt_n-med_re_n,2),
                cmt_drop=sprintf("%.1f%%", 100*(med_cmt_n-med_cmt_w)/med_cmt_w),
                re_drop =sprintf("%.1f%%", 100*(med_re_n -med_re_w )/med_re_w)) %>%
      as.data.frame())
