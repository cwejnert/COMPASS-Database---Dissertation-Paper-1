# =============================================================================
# Y8 — FIGURES FOR THE AMMONIA CORRECTION, and the three headline figures
#      regenerated off the harmonised grid.
#
# The mortality family changed after harmonising ammonia, so every figure that
# counts cells has to be rebuilt: Y1 (scorecard), Y2 (World forest) and Y3
# (robustness) are regenerated from FINAL_RESULTS_NH3.rds. Y4-Y7 do not touch
# mortality cells and are left alone.
#
# TWO NEW FIGURES CARRY THE CORRECTION ITSELF:
#   Y8  what harmonising did to every cell -- the dumbbell
#   Y9  WHY it did it -- the level change is 22x larger in one arm than the other
#
# Y9 is the one that makes the argument. If ammonia were noise it would move
# both arms alike. It moves High-CMT by -8.0% and High-RE by -0.36%, because
# the High-RE arm is ~95% REMIND and REMIND's agricultural ammonia lives in
# MAgPIE, where Emissions|NH3 never sees it. That asymmetry IS the result the
# uncorrected analysis was reporting.
#
# USAGE: Rscript Y8_nh3_figs.R
# =============================================================================
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(ggplot2);library(purrr)})
source("stratified.R.fns")
options(width=170)
CMT<-"#2F79BF"; RE<-"#C68A20"; INK<-"#1F2A36"; MUTE<-"#5B6B7B"
LINE<-"#D9E1E8"; SURF<-"#FCFCFB"; GREY<-"#9AA7B4"; GREEN<-"#1B7A4B"; RED<-"#A33A3A"
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
FIN <- readRDS("FINAL_RESULTS_NH3.rds")
FAM <- c(REFOSS="Jobs", LOWC="Jobs", gap_GJ_pc="Deprivation",
         headcount_pct="Deprivation", mort_per_1k="Health")
P <- FIN %>% mutate(family=FAM[outcome]) %>%
  filter(outcome %in% c("REFOSS","gap_GJ_pc","mort_per_1k"), !is.na(adv))
H <- P %>% filter(approach=="A full database", sample=="all scenarios", Region != DROP)
NTOT <- nrow(H); NRE <- sum(H$adv>0)
NSF <- sum(H$adv>0 & H$sig); NSA <- sum(H$adv<0 & H$sig)
cat("headline:", NRE, "of", NTOT, "| sig for", NSF, "| sig against", NSA, "\n")

# ------------------------------------------------------------- Y1 scorecard --
d1 <- H %>% mutate(reg=factor(SH[Region], levels=rev(ORD9)),
                   fam=factor(family, levels=c("Jobs","Deprivation","Health")),
                   ambl=amb_lab(amb),
                   cellv=case_when(adv>0 & sig ~"High-RE, significant",
                                   adv>0 ~"High-RE (n.s.)",
                                   adv<0 & sig ~"High-CMT, significant",
                                   TRUE~"High-CMT (n.s.)"),
                   txt=sprintf("%+.2f", adv))
p1 <- ggplot(d1, aes(fam, reg, fill=cellv)) +
  geom_tile(colour=SURF, linewidth=1.6) +
  geom_text(aes(label=txt, colour=cellv), size=2.6, fontface="bold", show.legend=FALSE) +
  geom_hline(yintercept=9.5, colour=INK, linewidth=0.7) +
  facet_wrap(~ambl) +
  scale_fill_manual(values=c(`High-RE, significant`=RE, `High-RE (n.s.)`="#F0DFB8",
                             `High-CMT, significant`=CMT, `High-CMT (n.s.)`="#CBDCF0"),
                    breaks=c("High-RE, significant","High-RE (n.s.)",
                             "High-CMT (n.s.)","High-CMT, significant")) +
  scale_colour_manual(values=c(`High-RE, significant`="#3B2B08",`High-RE (n.s.)`=INK,
                               `High-CMT, significant`="#FFFFFF",`High-CMT (n.s.)`=INK)) +
  labs(title=sprintf("High-RE beats High-CMT in %d of %d cells (%.0f%%)",
                     NRE, NTOT, 100*NRE/NTOT),
       subtitle=paste0("Cliff's delta, signed so positive always means High-RE is better. ",
                       "One primary measure per outcome family.\nBold cells clear a ",
                       "cluster-robust 95% interval over 312 model x scenario-family clusters. ",
                       "Ammonia harmonised across all models."),
       x=NULL, y=NULL, fill=NULL,
       caption=paste0("Cumulative 2020-2050, 590 scenarios. ", NSF, " cells significantly ",
                      "favour High-RE, ", NSA, " significantly favour High-CMT.\nJobs is ",
                      "unanimous; deprivation follows; health does not separate the pathways ",
                      "once the two arms report emissions on the same basis.\nPacific OECD is ",
                      "excluded from the regional rows and remains inside the World aggregate.")) +
  th + theme(legend.position="top", panel.grid=element_blank())
ggsave("Y1_scorecard.png", p1, width=8.4, height=4.8, dpi=210)

# ---------------------------------------------------------- Y2 World forest --
d2 <- P %>% filter(approach=="A full database", sample=="all scenarios",
                   Region=="Aggregated R10 regions") %>%
  mutate(fam=factor(family, levels=c("Jobs","Deprivation","Health")),
         ambl=amb_lab(amb),
         lb=sprintf("%s  ·  %s", fam, sub("C high.*|C medium.*","C",as.character(ambl))))
p2 <- ggplot(d2, aes(adv, reorder(lb, adv))) +
  annotate("rect", xmin=-Inf, xmax=0, ymin=-Inf, ymax=Inf, fill="#EAF0F7", alpha=0.9) +
  geom_vline(xintercept=0, colour=INK, linewidth=0.5) +
  geom_errorbarh(aes(xmin=lo, xmax=hi, colour=sig), height=0, linewidth=0.9) +
  geom_point(aes(colour=sig), size=2.8) +
  geom_text(aes(label=sprintf("%+.2f", adv)), vjust=-1.1, size=2.5, colour=INK) +
  scale_colour_manual(values=c(`TRUE`=RE, `FALSE`=GREY), guide="none") +
  scale_x_continuous(limits=c(-0.5,1.05), breaks=seq(-0.5,1,0.25)) +
  labs(title="At World, jobs and deprivation separate the pathways. Health does not.",
       subtitle=paste0("Cliff's delta with a cluster-robust 95% interval, ",
                       "resampling whole model x scenario-family clusters.\nGold clears zero; ",
                       "grey does not. Positive always means High-RE is better."),
       x="advantage to High-RE (Cliff's delta)", y=NULL,
       caption=paste0("Health sits on zero at both ambition levels (+0.06, +0.03) once ",
                      "ammonia is put on a common basis across models.\nBefore that ",
                      "correction it read +0.47 and +0.33, of which 86% and 90% was ",
                      "ammonia that only one arm reported.")) +
  th
ggsave("Y2_world_forest.png", p2, width=8.4, height=4.0, dpi=210)

# ---------------------------------------------------------- Y3 robustness ----
rb <- P %>% filter(Region != DROP) %>% group_by(approach, sample) %>%
  summarise(pct=100*mean(adv>0), .groups="drop") %>%
  transmute(test="Database & vetting",
            variant=paste0(sub("A full database","Full DB",
                           sub("C SCI-vetted","SCI-vetted",approach)),
                           ", ", sub(" scenarios","",sample)), pct)
cut <- readRDS("CUT_SENS.rds") %>% filter(!is.na(adv), Region != DROP) %>%
  group_by(cut) %>% summarise(pct=100*mean(adv>0), .groups="drop") %>%
  transmute(test="Tercile cut", variant=sprintf("top %.0f%%",100*(1-cut)), pct)
cs <- readRDS("T2_COMMON_SUPPORT.rds") %>% filter(!is.na(adv), Region != DROP) %>%
  group_by(design) %>% summarise(pct=100*mean(adv>0), .groups="drop") %>%
  transmute(test="Threshold sample",
            variant=ifelse(design=="current","published (split)","common support"), pct)
lb <- readRDS("Z3_LABELS.rds") %>% filter(Region != DROP) %>%
  summarise(g=100*mean(global_label>0, na.rm=TRUE),
            r=100*mean(region_label>0, na.rm=TRUE)) %>%
  pivot_longer(everything()) %>%
  transmute(test="Label basis",
            variant=ifelse(name=="g","global tercile","per-region tercile"), pct=value)
d3 <- bind_rows(cut, cs, lb, rb) %>%
  mutate(test=factor(test, levels=c("Tercile cut","Threshold sample","Label basis",
                                    "Database & vetting")))
p3 <- ggplot(d3, aes(pct, reorder(variant, pct))) +
  geom_vline(xintercept=50, colour=GREY, linetype="dashed", linewidth=0.4) +
  geom_col(fill=RE, width=0.62) +
  geom_text(aes(label=sprintf("%.0f%%", pct)), hjust=-0.25, size=2.7, colour=INK) +
  facet_grid(test ~ ., scales="free_y", space="free_y", switch="y") +
  scale_x_continuous(limits=c(0,100), breaks=c(0,25,50,75,100)) +
  labs(title="No single design choice carries the result",
       subtitle="Share of cells favouring High-RE under each alternative. Dashed line is a coin flip.",
       x="cells favouring High-RE (%)", y=NULL,
       caption=paste0("Database rows use the ammonia-harmonised mortality. Tercile-cut, ",
                      "threshold-sample and label-basis rows are computed\non all five ",
                      "measures. Only SCI vetting moves the answer meaningfully, and that is ",
                      "a power effect concentrated in deprivation.")) +
  th + theme(strip.placement="outside", strip.text.y.left=element_text(angle=0,hjust=1),
             panel.grid.major.y=element_blank())
ggsave("Y3_robustness.png", p3, width=8.4, height=4.6, dpi=210)

# =============================================================================
# Y8 — WHAT HARMONISING AMMONIA DID TO EVERY MORTALITY CELL
# =============================================================================
C <- readRDS("NH3_MORT_REBUILD.rds") %>% filter(shown, !is.na(adv_no)) %>%
  mutate(reg=SH[Region], ambl=amb_lab(amb),
         moved=ifelse(sign(adv_with)!=sign(adv_no), "sign changes", "same sign"))
p8 <- ggplot(C, aes(y=reorder(reg, adv_no))) +
  annotate("rect", xmin=-Inf, xmax=0, ymin=-Inf, ymax=Inf, fill="#EAF0F7", alpha=0.85) +
  geom_vline(xintercept=0, colour=INK, linewidth=0.5) +
  geom_segment(aes(x=adv_with, xend=adv_no, yend=reorder(reg, adv_no)),
               colour=GREY, linewidth=0.7,
               arrow=arrow(length=unit(0.055,"in"), type="closed")) +
  geom_point(aes(x=adv_with), colour=GREY, size=2.1) +
  geom_point(aes(x=adv_no, colour=moved), size=2.6) +
  facet_wrap(~ambl) +
  scale_colour_manual(values=c(`same sign`=INK, `sign changes`=RED), name=NULL) +
  scale_x_continuous(limits=c(-1.05,1.05), breaks=seq(-1,1,0.5)) +
  labs(title="Harmonising ammonia moves 18 of 20 mortality cells toward High-CMT",
       subtitle=paste0("Grey = ammonia as each model reports it. Arrow head = ammonia ",
                       "removed for every model.\nPositive means High-RE is better. ",
                       "Four cells change sign; none changes sign toward High-RE."),
       x="advantage to High-RE (Cliff's delta)", y=NULL,
       caption=paste0("At World the advantage falls from +0.47 to +0.06 (1.5C) and +0.33 to ",
                      "+0.03 (2C) -- 86% and 90% of the effect eliminated.\nNorth America ",
                      "reaches -1.00 with a zero-width interval: complete separation on ",
                      "47 vs 17 scenarios, which is a\nmodel-composition signature rather ",
                      "than a pathway effect, and is reported as such.")) +
  th + theme(legend.position="top")
ggsave("Y8_nh3_correction.png", p8, width=8.4, height=4.4, dpi=210)

# =============================================================================
# Y9 — WHY: THE CORRECTION IS 22x LARGER IN ONE ARM THAN THE OTHER
# =============================================================================
D <- readRDS("NH3_MORT_REBUILD.rds") %>% filter(shown) %>%
  transmute(reg=SH[Region], ambl=amb_lab(amb),
            `High-CMT`=100*(med_cmt_n-med_cmt_w)/med_cmt_w,
            `High-RE` =100*(med_re_n -med_re_w )/med_re_w) %>%
  pivot_longer(c(`High-CMT`,`High-RE`), names_to="arm", values_to="chg")
med <- D %>% group_by(arm) %>% summarise(m=median(chg), .groups="drop")
cat("median level change: ", paste(sprintf("%s %.2f%%", med$arm, med$m), collapse=" | "), "\n")
p9 <- ggplot(D, aes(chg, reorder(reg, chg), colour=arm, shape=arm)) +
  geom_vline(xintercept=0, colour=INK, linewidth=0.45) +
  geom_line(aes(group=interaction(reg, ambl)), colour=LINE, linewidth=0.8) +
  geom_point(size=2.4) +
  facet_wrap(~ambl) +
  scale_colour_manual(values=c(`High-CMT`=CMT, `High-RE`=RE), name=NULL) +
  scale_shape_manual(values=c(`High-CMT`=16, `High-RE`=17), name=NULL) +
  labs(title="Removing ammonia empties one arm and barely touches the other",
       subtitle=paste0("Change in median PM2.5 mortality when ammonia is removed. ",
                       "If ammonia were noise, both arms would move alike.\nMedian across ",
                       "cells: High-CMT -8.0%, High-RE -0.36% -- a 22-fold asymmetry."),
       x="change in median mortality when ammonia is removed (%)", y=NULL,
       caption=paste0("The High-RE arm barely moves because it never had ammonia in it. ",
                      "REMIND is ~95% of that arm, and REMIND's\nagricultural emissions live ",
                      "in MAgPIE, so agricultural NH3 -- roughly 85% of the global total -- ",
                      "never reaches Emissions|NH3.\nEurope -36.0% against -0.7%, China+ ",
                      "-18.5% against -1.8%, North America -16.3% against -0.35%.")) +
  th + theme(legend.position="top")
ggsave("Y9_nh3_asymmetry.png", p9, width=8.4, height=4.4, dpi=210)

cat("\nwritten: Y1_scorecard.png Y2_world_forest.png Y3_robustness.png\n")
cat("         Y8_nh3_correction.png Y9_nh3_asymmetry.png\n")
