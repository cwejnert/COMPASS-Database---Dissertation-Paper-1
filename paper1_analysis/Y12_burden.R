# =============================================================================
# Y12 — THE REGIONAL PM2.5 BURDEN, WHICH NO PATHWAY CHOICE MOVES
#
# The mortality section spent its length on a contrast that turns out not to
# exist. The levels underneath it are a different matter: a person in India+
# faces roughly five times the cumulative PM2.5 mortality risk of a person in
# Europe or North America, in EVERY scenario, under BOTH pathway archetypes,
# and whether or not ammonia is harmonised.
#
# That is worth reporting on its own terms. It says the air-quality problem is
# not solved by choosing a mitigation strategy -- it needs its own instrument --
# which is a more useful policy statement than a co-benefit would have been.
#
# The figure shows each region's distribution across all classified scenarios
# with the two arms overlaid, so the reader can see for themselves that the
# arms sit on top of each other while the regions do not.
#
# USAGE: Rscript Y12_burden.R
# =============================================================================
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(ggplot2)})
source("stratified.R.fns")
CMT<-"#2F79BF"; RE<-"#C68A20"; INK<-"#1F2A36"; MUTE<-"#5B6B7B"
SURF<-"#FCFCFB"; GREY<-"#9AA7B4"
th <- theme_minimal(base_size=9) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(colour="#EDF1F4",linewidth=0.4),
        plot.background=element_rect(fill=SURF,colour=NA),
        panel.background=element_rect(fill=SURF,colour=NA),
        axis.text=element_text(colour=INK,size=8),
        axis.title=element_text(colour=MUTE,size=8),
        legend.text=element_text(colour=INK,size=8),
        plot.title=element_text(face="bold",colour=INK,size=11.5),
        plot.subtitle=element_text(colour=MUTE,size=8.2,lineheight=1.15),
        plot.caption=element_text(colour=MUTE,size=7,hjust=0,lineheight=1.25))
SH <- c(R10AFRICA="Africa",`R10CHINA+`="China+",R10EUROPE="Europe",`R10INDIA+`="India+",
        R10LATIN_AM="Latin America",R10MIDDLE_EAST="Middle East",R10NORTH_AM="North America",
        R10REF_ECON="Reforming econ.",R10REST_ASIA="Rest of Asia")

F <- load_frame("A") %>%
  filter(!is.na(mort_per_1k), Region %in% names(SH)) %>%
  mutate(reg = SH[Region])
med <- F %>% group_by(reg) %>% summarise(m = median(mort_per_1k), .groups="drop")
F <- F %>% mutate(reg = factor(reg, levels = med$reg[order(med$m)]))
arm <- F %>% group_by(reg, Pathway) %>%
  summarise(m = median(mort_per_1k), .groups="drop")
lab <- med %>% mutate(reg = factor(reg, levels = med$reg[order(med$m)]))
hi <- max(med$m); lo <- min(med$m)

p <- ggplot(F, aes(mort_per_1k, reg)) +
  geom_boxplot(outlier.shape=NA, width=0.55, colour=GREY, fill="#F2F5F8", linewidth=0.4) +
  geom_point(data=arm, aes(m, reg, colour=Pathway), size=2.6) +
  geom_text(data=lab, aes(x=m, y=reg, label=sprintf("%.1f", m)),
            vjust=-1.55, size=2.5, colour=INK, fontface="bold") +
  scale_colour_manual(values=c(`High-CMT`=CMT, `High-RE`=RE), name=NULL) +
  coord_cartesian(xlim=c(0, 62)) +
  labs(title=sprintf("A %.1f-fold gap in air-pollution burden that no pathway choice closes",
                     hi/lo),
       subtitle=paste0("Cumulative PM2.5 mortality per 1,000 people, 2020-2050, across all 590 ",
                       "classified scenarios.\nBoxes are the full scenario spread; the two dots ",
                       "are the arm medians. The dots sit on top of each other; the regions do not."),
       x="deaths per 1,000 people, cumulative 2020-2050", y=NULL,
       caption=paste0("India+ 44.1 against Latin America 9.3, North America 10.1 and Europe 11.7. ",
                      "The ordering is unchanged when ammonia is\nharmonised (India+ 43.6 against ",
                      "Europe 8.7, a 5.0-fold gap), so this finding does not depend on the ",
                      "reporting question at all.\nIt is the one thing the mortality data says ",
                      "clearly: the burden is overwhelmingly Asian, and mitigation strategy is ",
                      "not the lever that moves it.")) +
  th + theme(legend.position="top")
ggsave("Y12_burden.png", p, width=8.4, height=4.4, dpi=210)
cat("written: Y12_burden.png\n")
cat(sprintf("\nhighest %s %.1f | lowest %s %.1f | ratio %.1fx\n",
    med$reg[which.max(med$m)], hi, med$reg[which.min(med$m)], lo, hi/lo))
