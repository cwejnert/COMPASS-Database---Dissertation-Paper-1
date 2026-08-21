# =============================================================================
# Y1 — FINAL FIGURE SET. Nine regions plus World (Pacific OECD out of the
#      regional display, retained inside the World aggregate).
#
#   Y1  scorecard, 9 regions + World, 3 families
#   Y2  World forest, cluster-robust intervals            (unchanged from V2)
#   Y3  robustness across every alternative specification
#   Y4  jobs decomposition: build vs demolish
#   Y5  why Pacific OECD is out: label coherence by region      NEW
#   Y6  why mortality is not yet reportable: the NH3 reporting gap   NEW
#   Y7  within-model variance: which families can be pooled at all
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
FIN <- readRDS("FINAL_RESULTS.rds")
FAM <- c(REFOSS="Jobs", LOWC="Jobs", gap_GJ_pc="Deprivation",
         headcount_pct="Deprivation", mort_per_1k="Health")
P <- FIN %>% mutate(family=FAM[outcome]) %>%
  filter(outcome %in% c("REFOSS","gap_GJ_pc","mort_per_1k"), !is.na(adv))
H <- P %>% filter(approach=="A full database", sample=="all scenarios",
                  Region != DROP)

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
  labs(title="High-RE beats High-CMT in 48 of 60 cells (80%)",
       subtitle=paste0("Cliff's delta, signed so positive always means High-RE is better. ",
                       "One primary measure per outcome family.\nBold cells clear a ",
                       "cluster-robust 95% interval over 312 model x scenario-family clusters."),
       x=NULL, y=NULL, fill=NULL,
       caption=paste0("Cumulative 2020-2050, 590 scenarios. 38 cells significantly favour ",
                      "High-RE, 6 significantly favour High-CMT.\nPacific OECD is excluded ",
                      "from the regional rows (see the label-coherence figure) but remains ",
                      "inside the World aggregate.")) +
  th + theme(legend.position="top", panel.grid=element_blank())
ggsave("Y1_scorecard.png", p1, width=8.4, height=4.8, dpi=210)

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
       caption=paste0("Tercile-cut, threshold-sample and label-basis rows are computed on all ",
                      "five measures; database rows on three families.\nOnly SCI vetting moves ",
                      "the answer meaningfully, and that is a power effect concentrated in deprivation.")) +
  th + theme(strip.placement="outside", strip.text.y.left=element_text(angle=0,hjust=1),
             panel.grid.major.y=element_blank())
ggsave("Y3_robustness.png", p3, width=8.4, height=4.6, dpi=210)

# ------------------------------------------------- Y5 label coherence (NEW) --
ds <- readRDS("ds_A.rds")
reg <- ds %>% filter(Variable %in% c("Total CDR","Renewable Capacity"),
                     Region %in% R10_TEN) %>%
  select(Model,Scenario,Region,Variable,Total_Value) %>%
  pivot_wider(names_from=Variable, values_from=Total_Value) %>%
  rename(cmt=`Total CDR`, re=`Renewable Capacity`)
lab <- readRDS("pw_A.rds") %>% filter(!is.na(Pathway_excl)) %>%
  distinct(Model,Scenario,Pathway=Pathway_excl,Ambition) %>%
  mutate(amb=ifelse(grepl("^1\\.5",Ambition),"1.5C","2C"))
coh <- reg %>% inner_join(lab, by=c("Model","Scenario")) %>%
  group_by(Region, amb) %>%
  summarise(re_delta=cliff_d(re[Pathway=="High-CMT"], re[Pathway=="High-RE"]),
            .groups="drop") %>%
  mutate(reg=SH[Region], ambl=amb_lab(amb), drop=Region==DROP)
p5 <- ggplot(coh, aes(re_delta, reorder(reg, re_delta), colour=drop)) +
  annotate("rect", xmin=-Inf, xmax=0.5, ymin=-Inf, ymax=Inf, fill="#F7E7E7", alpha=0.75) +
  geom_vline(xintercept=0, colour=INK, linewidth=0.45) +
  geom_line(aes(group=reg), colour=LINE, linewidth=0.8) +
  geom_point(size=2.3) +
  scale_colour_manual(values=c(`FALSE`=RE, `TRUE`=RED), guide="none") +
  scale_x_continuous(limits=c(-0.35,1.05), breaks=seq(-0.25,1,0.25)) +
  labs(title="Does the global label describe what happens inside the region?",
       subtitle=paste0("Cliff's delta on that region's OWN renewable deployment, ",
                       "High-RE against High-CMT.\nNear 1.0 means the label holds there. ",
                       "Near 0 means the two arms build the same renewables locally."),
       x="regional renewable-deployment gap between the arms", y=NULL,
       caption=paste0("Both ambition levels shown per region. Pacific OECD (red) is -0.19 and ",
                      "-0.07: High-RE builds NO MORE renewables there,\nso the contrast the ",
                      "paper claims to measure does not exist in that region. It is dropped ",
                      "from the regional results and kept in the World sum.\nReforming ",
                      "economies is weak (+0.26, +0.45) and is retained with a flag.")) +
  th
ggsave("Y5_label_coherence.png", p5, width=8.4, height=4.4, dpi=210)

# ------------------------------------------------------ Y6 NH3 gap (NEW) -----
A <- readRDS("ARM.rds") %>% mutate(fam=sub("[ /].*$","",model))
sc <- A %>% group_by(model, scenario, arm, fam) %>%
  summarise(w=sum(with_nh3), z=sum(zero_nh3), .groups="drop") %>%
  mutate(share=100*(w-z)/w)
p6 <- ggplot(sc, aes(share, reorder(fam, share), colour=arm)) +
  geom_vline(xintercept=0, colour=INK, linewidth=0.4) +
  geom_point(size=2, alpha=0.75,
             position=position_jitter(height=0.13, width=0, seed=1)) +
  scale_colour_manual(values=c(`High-CMT`=CMT, `High-RE`=RE)) +
  scale_x_continuous(limits=c(-0.5,15), breaks=seq(0,15,5)) +
  labs(title="Why the mortality comparison is not yet apples-to-apples",
       subtitle=paste0("Share of each scenario's PM2.5 mortality that comes from ammonia. ",
                       "Agricultural NH3 lives in MAgPIE,\nso REMIND does not report it -- ",
                       "and REMIND is ~95% of the High-RE arm."),
       x="share of PM2.5 mortality attributable to NH3 (%)", y=NULL, colour=NULL,
       caption=paste0("37 scenarios run twice each, with NH3 as reported and with NH3 forced ",
                      "to zero. IMAGE 12.4%, POLES-JRC 9.4%, AIM 8.9%,\nMESSAGEix-GLOBIOM ",
                      "6.4% against REMIND-MAgPIE 0.16% and REMIND 0.14% -- a 58-fold gap. ",
                      "High-CMT therefore carries ~9% extra\nmortality that is an accounting ",
                      "difference, not a pathway effect, and it flatters High-RE.")) +
  th + theme(legend.position="top")
ggsave("Y6_nh3_gap.png", p6, width=8.4, height=4.2, dpi=210)

# ------------------------------------------- Y4 jobs decomposition (9 reg) ---
F <- load_frame("A")
dec <- expand_grid(Region=c("Aggregated R10 regions", R10_TEN), amb=c("1.5C","2C")) %>%
  pmap_dfr(function(Region, amb) {
    d <- F[F$Region==Region & F$amb==amb, ]
    g <- function(col){a<-d[[col]][d$Pathway=="High-CMT"]; b<-d[[col]][d$Pathway=="High-RE"]
      a<-a[!is.na(a)];b<-b[!is.na(b)]
      if(length(a)<5||length(b)<5) NA_real_ else cliff_d(a,b)}
    tibble(Region, amb, re_term=g("Renewables"), fo_term=g("Fossil"))
  }) %>% filter(!is.na(re_term)) %>%
  mutate(reg=SH[Region], ambl=amb_lab(amb),
         kind=case_when(Region=="Aggregated R10 regions"~"World",
                        Region==DROP~"excluded from regional results",
                        TRUE~"region")) %>%
  group_by(ambl) %>% arrange(re_term, .by_group=TRUE) %>%
  mutate(vj=ifelse(row_number()%%2==0,-1.05,1.95)) %>% ungroup()
p4 <- ggplot(dec, aes(re_term, fo_term)) +
  annotate("rect", xmin=-Inf, xmax=0.35, ymin=-Inf, ymax=Inf, fill="#F4E9E9", alpha=0.85) +
  geom_hline(yintercept=0, colour=INK, linewidth=0.45) +
  geom_point(aes(colour=kind, size=kind)) +
  geom_text(aes(label=reg, vjust=vj), size=2.35, colour=INK) +
  facet_wrap(~ambl) +
  scale_colour_manual(values=c(region=RE, World=INK,
                               `excluded from regional results`=RED)) +
  scale_size_manual(values=c(region=1.9, World=2.9,
                             `excluded from regional results`=1.9), guide="none") +
  scale_x_continuous(limits=c(-0.02,1.06), breaks=seq(0,1,0.25)) +
  scale_y_continuous(limits=c(-0.92,0.40), breaks=seq(-0.75,0.25,0.25)) +
  labs(title="Where the jobs advantage comes from: building, not demolishing",
       subtitle=paste0("Horizontal: does High-RE create more renewable jobs? ",
                       "Vertical: does it hold more or fewer fossil jobs?\n",
                       "India+, Rest of Asia and the Middle East sit ABOVE zero - High-RE ",
                       "wins while retaining MORE fossil workers."),
       x="Cliff's delta on renewable jobs  (right = High-RE creates more)",
       y="Cliff's delta on fossil jobs\n(up = High-RE retains more)", colour=NULL,
       caption=paste0("Shaded band: the jobs win rests on fossil job destruction rather than ",
                      "renewable job creation. Reforming economies at 1.5C\nposts a ",
                      "renewable-jobs delta of only +0.07 and should not be presented as an ",
                      "employment gain.")) +
  th + theme(legend.position="top")
ggsave("Y4_jobs_decomposition.png", p4, width=8.4, height=4.6, dpi=210)

# --------------------------------------------- Y7 within-model variance ------
vs <- function(col) F %>% filter(Region %in% R10_TEN, !is.na(.data[[col]])) %>%
  group_by(Region) %>%
  summarise(within=mean(tapply(.data[[col]], fam, var, na.rm=TRUE), na.rm=TRUE),
            total=var(.data[[col]], na.rm=TRUE), .groups="drop") %>%
  mutate(share=within/total, outcome=col)
d7 <- bind_rows(vs("REFOSS"), vs("gap_GJ_pc"), vs("mort_per_1k")) %>%
  mutate(family=FAM[outcome], reg=SH[Region]) %>% filter(Region != DROP)
p7 <- ggplot(d7, aes(share, reorder(reg, share), colour=family)) +
  geom_vline(xintercept=0.10, colour=RED, linetype="dashed", linewidth=0.45) +
  geom_line(aes(group=reg), colour=LINE, linewidth=0.8) +
  geom_point(size=2.2) +
  scale_colour_manual(values=c(Jobs=RE, Deprivation=GREEN, Health=CMT)) +
  scale_x_continuous(labels=scales::percent, limits=c(0,1)) +
  labs(title="Which outcomes can be pooled across models at all",
       subtitle=paste0("Share of outcome variance that is WITHIN a model family. Below the ",
                       "dashed line (10%) a pooled\ncomparison is reading model inventories ",
                       "rather than pathways."),
       x="within-model-family share of total variance", y=NULL, colour=NULL,
       caption=paste0("Jobs 30-63% and deprivation 24-64% clear it in every region. ",
                      "Mortality falls below in four of the nine shown,\nwhich is the ",
                      "structural reason it cannot carry a pooled claim.")) +
  th + theme(legend.position="top")
ggsave("Y7_variance.png", p7, width=8.4, height=4.4, dpi=210)

cat("written:\n"); print(list.files(".", "^Y[0-9].*png$"))
