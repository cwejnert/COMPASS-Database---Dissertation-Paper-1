# =============================================================================
# V1 — FINAL FIGURES, all built from FINAL_RESULTS.rds (2020-2050,
#      cluster-robust, three outcome families).
#
#   V1  scorecard: 11 regions x 2 ambitions x 3 families, World separated
#   V2  World forest plot with cluster-robust 95% intervals
#   V3  robustness: tercile cut, common support, vetting, clustering
#   V4  jobs decomposition: RE term vs fossil term, by region
#   V5  the deprivation-mortality trade-off
#   V6  why mortality is the weak family
# =============================================================================
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(ggplot2);library(purrr)})
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
        legend.title=element_text(colour=MUTE,size=7.6),
        plot.title=element_text(face="bold",colour=INK,size=11.5),
        plot.subtitle=element_text(colour=MUTE,size=8.2,lineheight=1.15),
        plot.caption=element_text(colour=MUTE,size=7,hjust=0,lineheight=1.25))
SH <- c(`Aggregated R10 regions`="WORLD",R10AFRICA="Africa",`R10CHINA+`="China+",
        R10EUROPE="Europe",`R10INDIA+`="India+",R10LATIN_AM="Latin America",
        R10MIDDLE_EAST="Middle East",R10NORTH_AM="North America",R10PAC_OECD="Pacific OECD",
        R10REF_ECON="Reforming econ.",R10REST_ASIA="Rest of Asia")
REG_ORD <- c("WORLD","Africa","China+","Europe","India+","Latin America","Middle East",
             "North America","Pacific OECD","Reforming econ.","Rest of Asia")
FIN <- readRDS("FINAL_RESULTS.rds")
H  <- FIN %>% filter(approach=="A full database", sample=="all scenarios")
HP <- H %>% filter(primary)
amb_lab <- function(x) factor(x, levels=c("1.5C","2C"),
                              labels=c("1.5C high ambition","2C medium ambition"))

# =========================================================== V1 scorecard ===
FAM_ORD <- c("Jobs","Energy deprivation","Health")
d1 <- HP %>% filter(!is.na(win)) %>%
  mutate(reg = factor(SH[Region], levels=rev(REG_ORD)),
         fam = factor(family, levels=FAM_ORD),
         ambl = amb_lab(amb),
         cellv = case_when(win & sig ~ "High-RE, significant", win ~ "High-RE (n.s.)",
                           !win & sig ~ "High-CMT, significant", TRUE ~ "High-CMT (n.s.)"),
         txt = sprintf("%+.2f", adv))
p1 <- ggplot(d1, aes(fam, reg, fill=cellv)) +
  geom_tile(colour=SURF, linewidth=1.6) +
  geom_text(aes(label=txt, colour=cellv), size=2.6, fontface="bold", show.legend=FALSE) +
  geom_hline(yintercept=10.5, colour=INK, linewidth=0.7) +
  facet_wrap(~ambl) +
  scale_fill_manual(values=c(`High-RE, significant`=RE, `High-RE (n.s.)`="#F0DFB8",
                             `High-CMT, significant`=CMT, `High-CMT (n.s.)`="#CBDCF0"),
                    breaks=c("High-RE, significant","High-RE (n.s.)",
                             "High-CMT (n.s.)","High-CMT, significant")) +
  scale_colour_manual(values=c(`High-RE, significant`="#3B2B08", `High-RE (n.s.)`=INK,
                               `High-CMT, significant`="#FFFFFF", `High-CMT (n.s.)`=INK)) +
  labs(title="High-RE beats High-CMT in 53 of 66 cells (80%)",
       subtitle=paste0("Cliff's delta, signed so positive always means High-RE is better. ",
                       "One primary measure per outcome family.\nBold cells clear a ",
                       "cluster-robust 95% interval (2,000 bootstrap replicates over 312 model x scenario-family clusters)."),
       x=NULL, y=NULL, fill=NULL,
       caption=paste0("Cumulative 2020-2050. Jobs = renewable minus fossil employment; ",
                      "deprivation = cumulative energy gap; health = PM2.5 mortality.\n",
                      "590 scenarios, AR6 R10. 41 cells significantly favour High-RE, 7 significantly favour High-CMT. High-CMT = top tercile of land CDR + novel CDR + fossil CCS.")) +
  th + theme(legend.position="top", panel.grid=element_blank())
ggsave("V1_scorecard.png", p1, width=8.4, height=5.0, dpi=210)

# ======================================================== V2 world forest ===
ORD5 <- c("RE - fossil jobs","Low-carbon - fossil jobs","Energy deprivation gap",
          "Deprivation headcount","PM2.5 mortality")
d2 <- H %>% filter(is_world, !is.na(adv)) %>%
  mutate(lab = factor(label, levels=rev(ORD5)), ambl = amb_lab(amb),
         col = ifelse(sig, "significant", "not significant"))
p2 <- ggplot(d2, aes(adv, lab, colour=col)) +
  geom_vline(xintercept=0, colour=INK, linewidth=0.5) +
  geom_errorbarh(aes(xmin=lo, xmax=hi), height=0.16, linewidth=0.6) +
  geom_point(size=2.4) +
  geom_text(aes(label=sprintf("%+.0f%%", pct)), vjust=-1.1, size=2.5,
            colour=MUTE, show.legend=FALSE) +
  facet_wrap(~ambl) +
  scale_colour_manual(values=c(significant=RE, `not significant`=GREY)) +
  scale_x_continuous(limits=c(-0.15,1.05), breaks=seq(0,1,0.25)) +
  labs(title="World: nine of ten cells favour High-RE with a clear interval",
       subtitle=paste0("Cliff's delta with a cluster-robust 95% bootstrap interval. ",
                       "Labels give the median percentage gap.\nThe one exception is 2C ",
                       "PM2.5 mortality, whose interval crosses zero."),
       x="High-RE advantage (Cliff's delta)", y=NULL, colour=NULL,
       caption="World = the ten-region sum, restricted to scenarios reporting all ten regions.") +
  th + theme(legend.position="top")
ggsave("V2_world_forest.png", p2, width=8.4, height=3.9, dpi=210)

# ========================================================= V3 robustness ====
CUT <- readRDS("CUT_SENS.rds") %>% filter(!is.na(adv)) %>%
  group_by(cut) %>% summarise(pct=100*mean(adv>0), .groups="drop") %>%
  transmute(test="Tercile cut", variant=sprintf("top %.0f%%", 100*(1-cut)), pct)
CS <- readRDS("T2_COMMON_SUPPORT.rds") %>% filter(!is.na(adv)) %>%
  group_by(design) %>% summarise(pct=100*mean(adv>0), .groups="drop") %>%
  transmute(test="Threshold sample",
            variant=ifelse(design=="current","published (split)","common support"), pct)
SM <- FIN %>% filter(!is.na(adv)) %>% group_by(approach, sample) %>%
  summarise(pct=100*mean(adv>0), .groups="drop") %>%
  transmute(test="Database & vetting",
            variant=paste0(sub(" database","",sub("A full","Full DB",sub("C SCI-vetted","SCI-vetted",approach))),
                           ", ", sub(" scenarios","",sample)), pct)
d3 <- bind_rows(CUT, CS, SM) %>%
  mutate(test=factor(test, levels=c("Tercile cut","Threshold sample","Database & vetting")))
p3 <- ggplot(d3, aes(pct, reorder(variant, pct))) +
  geom_vline(xintercept=50, colour=GREY, linetype="dashed", linewidth=0.4) +
  geom_vline(xintercept=80, colour=LINE, linewidth=0.4) +
  geom_col(fill=RE, width=0.62) +
  geom_text(aes(label=sprintf("%.0f%%", pct)), hjust=-0.25, size=2.7, colour=INK) +
  facet_grid(test ~ ., scales="free_y", space="free_y", switch="y") +
  scale_x_continuous(limits=c(0,100), breaks=c(0,25,50,75,100)) +
  labs(title="The direction does not depend on any single design choice",
       subtitle=paste0("Share of cells favouring High-RE under each alternative. ",
                       "The dashed line is a coin flip."),
       x="cells favouring High-RE (%)", y=NULL,
       caption=paste0("Every row is computed on all five measures (110 cells) so the bars are ",
                      "directly comparable; the three-family headline is 80%.\nSCI-vetted rests on ",
                      "137 scenarios rather than 590, so the direction holds while significance thins.")) +
  th + theme(strip.placement="outside", strip.text.y.left=element_text(angle=0, hjust=1),
             panel.grid.major.y=element_blank())
ggsave("V3_robustness.png", p3, width=8.4, height=4.4, dpi=210)

# ================================================= V4 jobs decomposition ====
source("stratified.R.fns")
F <- load_frame("A")
dec <- expand_grid(Region=c("Aggregated R10 regions", R10_TEN), amb=c("1.5C","2C")) %>%
  pmap_dfr(function(Region, amb) {
    d <- F[F$Region==Region & F$amb==amb, ]
    g <- function(col) { a<-d[[col]][d$Pathway=="High-CMT"]; b<-d[[col]][d$Pathway=="High-RE"]
      a<-a[!is.na(a)]; b<-b[!is.na(b)]
      if (length(a)<5||length(b)<5) NA_real_ else cliff_d(a,b) }
    tibble(Region, amb, re_term=g("Renewables"), fo_term=g("Fossil"))
  }) %>% filter(!is.na(re_term)) %>%
  mutate(reg=SH[Region], ambl=amb_lab(amb), is_world=Region=="Aggregated R10 regions") %>%
  # no ggrepel here, so stagger labels: neighbours in x get opposite vertical offsets
  group_by(ambl) %>% arrange(re_term, .by_group=TRUE) %>%
  mutate(vj = ifelse(row_number() %% 2 == 0, -1.05, 1.95)) %>% ungroup()
saveRDS(dec, "V4_DECOMP.rds")
p4 <- ggplot(dec, aes(re_term, fo_term)) +
  annotate("rect", xmin=-Inf, xmax=0.35, ymin=-Inf, ymax=Inf, fill="#F4E9E9", alpha=0.85) +
  geom_hline(yintercept=0, colour=INK, linewidth=0.45) +
  geom_point(aes(colour=is_world, size=is_world)) +
  geom_text(aes(label=reg, vjust=vj), size=2.4, colour=INK) +
  facet_wrap(~ambl) +
  scale_colour_manual(values=c(`FALSE`=RE, `TRUE`=INK), guide="none") +
  scale_size_manual(values=c(`FALSE`=1.9, `TRUE`=2.9), guide="none") +
  scale_x_continuous(limits=c(-0.02,1.06), breaks=seq(0,1,0.25)) +
  scale_y_continuous(limits=c(-0.92,0.40), breaks=seq(-0.75,0.25,0.25)) +
  labs(title="Where the jobs advantage comes from: building, not demolishing",
       subtitle=paste0("Horizontal: does High-RE create more renewable jobs? ",
                       "Vertical: does it hold more or fewer fossil jobs?\n",
                       "India+, Rest of Asia and the Middle East sit ABOVE zero - ",
                       "High-RE wins the contrast while retaining MORE fossil workers."),
       x="Cliff's delta on renewable jobs  (right = High-RE creates more)",
       y="Cliff's delta on fossil jobs\n(up = High-RE retains more)",
       caption=paste0("Shaded band: cells whose jobs 'win' rests on fossil job destruction ",
                      "rather than renewable job creation\n(Reforming economies 1.5C, ",
                      "Pacific OECD). Those should not be presented as employment gains.")) +
  th
ggsave("V4_jobs_decomposition.png", p4, width=8.4, height=4.6, dpi=210)

# =============================================== V5 deprivation vs health ===
# Built from the SAME advantage values as the scorecard, so the two figures
# cannot disagree. Cliff's delta is bounded, which also avoids the percentage
# blow-ups that afflict regions with a near-zero deprivation base.
#
# Note on the pooled correlation: gap and mortality correlate -0.58 across
# scenario-regions, but that is a BETWEEN-region level effect (poorer,
# lower-energy regions have both a larger gap and less combustion). WITHIN a
# region the median correlation is only -0.07, so there is no systematic
# trade-off that a pathway has to buy its way out of.
wr <- F %>% filter(Region %in% R10_TEN, !is.na(gap_GJ_pc), !is.na(mort_per_1k)) %>%
  group_by(Region, amb) %>%
  summarise(r = cor(gap_GJ_pc, mort_per_1k, method="spearman"), .groups="drop")
d5 <- HP %>% filter(family %in% c("Energy deprivation","Health"), !is.na(adv)) %>%
  select(Region, amb, family, adv) %>%
  pivot_wider(names_from=family, values_from=adv) %>%
  rename(dep=`Energy deprivation`, hea=Health) %>%
  filter(!is.na(dep), !is.na(hea)) %>%
  mutate(reg = SH[Region], ambl = amb_lab(amb),
         is_world = Region == "Aggregated R10 regions",
         quad = case_when(dep > 0 & hea > 0 ~ "High-RE better on both",
                          dep > 0 | hea > 0 ~ "High-RE better on one",
                          TRUE ~ "High-CMT better on both"))
saveRDS(d5, "V5_QUAD.rds")
p5 <- ggplot(d5, aes(dep, hea)) +
  annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=Inf, fill="#EAF2EC", alpha=0.9) +
  geom_hline(yintercept=0, colour=INK, linewidth=0.45) +
  geom_vline(xintercept=0, colour=INK, linewidth=0.45) +
  geom_point(aes(colour=quad, size=is_world)) +
  geom_text(aes(label=reg, vjust=ifelse(hea > 0, -1.05, 1.85)), size=2.35, colour=INK) +
  facet_wrap(~ambl) +
  scale_colour_manual(values=c(`High-RE better on both`=GREEN,
                               `High-RE better on one`=RE,
                               `High-CMT better on both`=RED)) +
  scale_size_manual(values=c(`FALSE`=1.9, `TRUE`=3.0), guide="none") +
  scale_x_continuous(limits=c(-0.62,1.0), breaks=seq(-0.5,1,0.5)) +
  scale_y_continuous(limits=c(-0.85,1.05), breaks=seq(-0.75,1,0.25)) +
  labs(title="Eleven of 22 cells improve on both; only two are worse on both",
       subtitle=paste0("Cliff's delta on each family, same values as the scorecard. ",
                       "Green quadrant = High-RE better on both.\nJobs is omitted because ",
                       "it favours High-RE in all 22 cells, so it separates nothing."),
       x="energy deprivation advantage (right = High-RE better)",
       y="health advantage\n(up = High-RE better)", colour=NULL,
       caption=paste0("Gap and mortality correlate -0.58 POOLED across scenario-regions, ",
                      "but only -0.07 (median) WITHIN a region: the pooled figure is a\n",
                      "level effect across regions, not a trade-off any one region must make.")) +
  th + theme(legend.position="top")
ggsave("V5_tradeoff.png", p5, width=8.4, height=4.8, dpi=210)
cat("\nquadrant counts:\n"); print(table(d5$quad))

# ============================================= V6 why mortality is weak =====
vp <- F %>% filter(!is.na(mort_per_1k), Region %in% R10_TEN) %>%
  group_by(Region) %>%
  summarise(within = mean(tapply(mort_per_1k, fam, var, na.rm=TRUE), na.rm=TRUE),
            total = var(mort_per_1k, na.rm=TRUE), .groups="drop") %>%
  mutate(share = within/total, reg=SH[Region], outcome="PM2.5 mortality")
vj <- F %>% filter(!is.na(REFOSS), Region %in% R10_TEN) %>%
  group_by(Region) %>%
  summarise(within = mean(tapply(REFOSS, fam, var, na.rm=TRUE), na.rm=TRUE),
            total = var(REFOSS, na.rm=TRUE), .groups="drop") %>%
  mutate(share = within/total, reg=SH[Region], outcome="RE - fossil jobs")
d6 <- bind_rows(vp, vj) %>%
  mutate(reg = factor(reg, levels = vp$reg[order(vp$share)]))
p6 <- ggplot(d6, aes(share, reg, colour=outcome)) +
  geom_vline(xintercept=0.10, colour=RED, linetype="dashed", linewidth=0.45) +
  geom_line(aes(group=reg), colour=LINE, linewidth=0.8) +
  geom_point(size=2.2) +
  scale_colour_manual(values=c(`PM2.5 mortality`=CMT, `RE - fossil jobs`=RE)) +
  scale_x_continuous(labels=scales::percent, limits=c(0,1)) +
  labs(title="Why mortality is the weakest family: the arms barely meet inside a model",
       subtitle=paste0("Share of outcome variance that is WITHIN a model family. ",
                       "Below the dashed line (10%) a pooled comparison is\ncomparing ",
                       "model inventories, not pathways. Jobs clear it everywhere; ",
                       "mortality does not."),
       x="within-model-family share of total variance", y=NULL, colour=NULL,
       caption=paste0("POLES-JRC is 100% High-CMT and the REMIND family ~95% High-RE, so in ",
                      "low-share regions the two arms are\nnever observed inside the same ",
                      "model. This is the Simpson risk behind the three reversed mortality cells.")) +
  th + theme(legend.position="top")
ggsave("V6_mortality_variance.png", p6, width=8.4, height=4.4, dpi=210)

cat("written:\n"); print(list.files(".", "^V[0-9].*png$"))
print(d6 %>% select(reg, outcome, share) %>%
      mutate(share=round(share,3)) %>%
      pivot_wider(names_from=outcome, values_from=share) %>% as.data.frame())
