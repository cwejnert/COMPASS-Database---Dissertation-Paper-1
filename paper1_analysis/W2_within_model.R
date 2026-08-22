# =============================================================================
# W2 — DOES THE POOLED DIRECTION SURVIVE INSIDE A MODEL?
#
# WHY THIS EXISTS. The arms are badly unbalanced by model: REMIND is 85% of
# High-RE and 1% of High-CMT. So every pooled cell is partly a model contrast.
# Where a family holds BOTH arms, we can ask the question directly: inside that
# model, holding the modelling framework fixed, which pathway does better?
#
# THIS IS NOT THE STRATIFIED TEST. Z2 showed that a properly weighted
# stratified estimator is hopelessly underpowered here (at 1.5C a single family
# carries 100% of the weight). This is the weaker but answerable question: of
# the model families that hold both arms with at least MINN scenarios each,
# how many point the same way as the pooled result?
#
# WHAT IT FOUND, and it cuts both ways.
#
#   JOBS SURVIVES. 82% of individual model families point to High-RE, and 20 of
#   22 cells have a within-model median favouring it. Pooled and within-model
#   tell the same story, so the jobs result is not an artefact of which models
#   populate which arm. This is the finding the paper can carry.
#
#   DEPRIVATION DOES NOT SEPARATE FROM MODEL COMPOSITION. Simpson's paradox
#   runs in BOTH directions depending on region. Rest of Asia and Middle East
#   read against High-RE when pooled but favour it inside almost every model.
#   World, Africa, Europe, Latin America and North America at 2C read FOR
#   High-RE when pooled but AGAINST it inside the models. Only 42% of families
#   agree with the pooled direction -- barely better than a coin flip. The
#   deprivation cells are substantially "what does REMIND say about this region
#   relative to other models", not "what does the pathway do".
#
#   HEALTH CANNOT BE ADJUDICATED. Only two families hold both arms, and one of
#   them (GCAM, 3 vs 4 scenarios) saturates at +/-1 in every cell.
#
# The asymmetry is the point: the same test that clears jobs is what convicts
# deprivation, so it cannot be dismissed as an unfair standard.
#
# USAGE: Rscript W2_within_model.R
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

MINN <- 3
DROP <- "R10PAC_OECD"
ALLR <- c("Aggregated R10 regions", R10_TEN)
OUTS <- c(REFOSS="Jobs", gap_GJ_pc="Energy deprivation", mort_per_1k="Health")

F <- load_frame("A") %>% mutate(fam = sub("[ /-].*$", "", Model))
cat("model families:", n_distinct(F$fam), "\n")

within <- expand_grid(Region = ALLR, amb = c("1.5C","2C"), outcome = names(OUTS)) %>%
  pmap_dfr(function(Region, amb, outcome) {
    d <- F[F$Region == Region & F$amb == amb, ]
    sgn <- ifelse(outcome %in% LOWER5, -1, 1)
    d %>% group_by(fam) %>%
      summarise(na = sum(Pathway=="High-CMT" & !is.na(.data[[outcome]])),
                nb = sum(Pathway=="High-RE"  & !is.na(.data[[outcome]])),
                dlt = if (na >= MINN && nb >= MINN)
                        sgn*cliff_d(.data[[outcome]][Pathway=="High-CMT"],
                                    .data[[outcome]][Pathway=="High-RE"])
                      else NA_real_, .groups = "drop") %>%
      filter(!is.na(dlt)) %>%
      mutate(Region = Region, amb = amb, outcome = outcome)
  })

FIN <- readRDS("FINAL_RESULTS_NH3.rds") %>%
  filter(approach=="A full database", sample=="all scenarios",
         outcome %in% names(OUTS)) %>%
  select(Region, amb, outcome, pooled = adv, sig)

J <- within %>% inner_join(FIN, by = c("Region","amb","outcome")) %>%
  mutate(agrees = sign(dlt) == sign(pooled))

S <- J %>% group_by(Region, amb, outcome) %>%
  summarise(families = n(), agree = sum(agrees),
            med_within = median(dlt), pooled = pooled[1], sig = sig[1],
            .groups = "drop") %>%
  mutate(family = OUTS[outcome],
         reg = ifelse(Region=="Aggregated R10 regions","WORLD", sub("^R10","",Region)),
         conflict = sign(med_within) != sign(pooled))
saveRDS(S, "W2_WITHIN.rds")

line("HOW OFTEN CAN THE QUESTION EVEN BE ASKED?")
cat("cells where at least one family holds both arms with >=", MINN, "each: ",
    nrow(S), " of 66\n", sep="")
print(as.data.frame(S %>% count(family, families) %>%
      pivot_wider(names_from = families, values_from = n, values_fill = 0)))
cat("\n(columns are the number of model families available in that cell)\n")

line("WHERE THE WITHIN-MODEL DIRECTION CONTRADICTS THE POOLED DIRECTION")
cf <- S %>% filter(Region != DROP, conflict)
cat(nrow(cf), "of", sum(S$Region != DROP), "cells\n\n")
print(cf %>% mutate(across(where(is.numeric), ~round(.,2))) %>%
      select(reg, amb, family, families, agree, med_within, pooled, sig) %>%
      as.data.frame())

line("THE CELLS THAT GO AGAINST HIGH-RE IN THE POOLED DATA")
ag <- S %>% filter(Region != DROP, pooled < 0)
print(ag %>% mutate(across(where(is.numeric), ~round(.,2))) %>%
      select(reg, amb, family, families, agree, med_within, pooled, sig) %>%
      arrange(family, reg) %>% as.data.frame())
cat("\nOf", nrow(ag), "cells that favour High-CMT when pooled,", sum(ag$med_within > 0),
    "favour High-RE inside the median model family.\n")

line("BY FAMILY — does pooling change the story?")
print(S %>% filter(Region != DROP) %>% group_by(family) %>%
      summarise(cells = n(),
                pooled_RE = sum(pooled > 0),
                within_RE = sum(med_within > 0),
                agree_rate = round(100*sum(agree)/sum(families)),
                .groups = "drop") %>% as.data.frame())
cat("\nagree_rate is the share of individual model families pointing the same\n")
cat("way as the pooled result in that outcome family.\n")

line("THE DEPRIVATION REVERSALS, FAMILY BY FAMILY")
print(J %>% filter(outcome == "gap_GJ_pc", pooled < 0, Region != DROP) %>%
      mutate(reg = sub("^R10","",Region), across(where(is.numeric), ~round(.,2))) %>%
      select(reg, amb, fam, na, nb, dlt, pooled) %>% as.data.frame())
cat("\nIf these are positive while pooled is negative, the reversal is model\n")
cat("composition, not pathway: the High-RE arm is dominated by a model that\n")
cat("reports larger deprivation gaps in that region whatever the pathway.\n")

# =============================================================================
# Y10 — THE FIGURE. Pooled against within-model, one point per cell per family.
#
# This is now the most important robustness figure in the paper. Points in the
# upper-right and lower-left quadrants agree with the pooled result; points in
# the off-diagonal quadrants are Simpson reversals. Jobs sits almost entirely
# in the agreeing quadrant. Deprivation does not.
# =============================================================================
suppressPackageStartupMessages(library(ggplot2))
CMT<-"#2F79BF"; RE<-"#C68A20"; INK<-"#1F2A36"; MUTE<-"#5B6B7B"
SURF<-"#FCFCFB"; GREY<-"#9AA7B4"; RED<-"#A33A3A"
th <- theme_minimal(base_size=9) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_line(colour="#EDF1F4",linewidth=0.4),
        plot.background=element_rect(fill=SURF,colour=NA),
        panel.background=element_rect(fill=SURF,colour=NA),
        strip.text=element_text(face="bold",colour=INK,size=8.5),
        axis.text=element_text(colour=INK,size=7.6),
        axis.title=element_text(colour=MUTE,size=8),
        plot.title=element_text(face="bold",colour=INK,size=11.5),
        plot.subtitle=element_text(colour=MUTE,size=8.2,lineheight=1.15),
        plot.caption=element_text(colour=MUTE,size=7,hjust=0,lineheight=1.25))

PLOT <- J %>% filter(Region != DROP) %>%
  mutate(fam2 = factor(OUTS[outcome],
                       levels=c("Jobs","Energy deprivation","Health")),
         agree2 = ifelse(sign(dlt)==sign(pooled), "agrees with pooled",
                                                  "reverses"))
rate <- PLOT %>% group_by(fam2) %>%
  summarise(lab=sprintf("%.0f%% of model families agree", 100*mean(agree2=="agrees with pooled")),
            .groups="drop")
p10 <- ggplot(PLOT, aes(pooled, dlt)) +
  annotate("rect", xmin=0, xmax=Inf, ymin=-Inf, ymax=0, fill="#F7E7E7", alpha=0.7) +
  annotate("rect", xmin=-Inf, xmax=0, ymin=0, ymax=Inf, fill="#F7E7E7", alpha=0.7) +
  geom_hline(yintercept=0, colour=INK, linewidth=0.4) +
  geom_vline(xintercept=0, colour=INK, linewidth=0.4) +
  geom_abline(slope=1, intercept=0, colour=GREY, linetype="dashed", linewidth=0.35) +
  geom_point(aes(colour=agree2), size=1.9, alpha=0.85) +
  geom_text(data=rate, aes(x=0, y=-1.28, label=lab), inherit.aes=FALSE,
            size=2.5, colour=INK, fontface="bold") +
  facet_wrap(~fam2) +
  scale_colour_manual(values=c(`agrees with pooled`=INK, reverses=RED), name=NULL) +
  scale_x_continuous(limits=c(-1.05,1.05), breaks=seq(-1,1,0.5)) +
  scale_y_continuous(limits=c(-1.35,1.05), breaks=seq(-1,1,0.5)) +
  labs(title="Does the result survive inside a single modelling framework?",
       subtitle=paste0("One point per region x ambition x model family holding both arms ",
                       "with at least 3 scenarios each.\nShaded quadrants are reversals: ",
                       "the model disagrees with the pooled answer."),
       x="pooled advantage to High-RE (Cliff's delta)",
       y="advantage inside that model family",
       caption=paste0("The arms are 85% REMIND against 1% REMIND, so every pooled cell is ",
                      "partly a model contrast. Jobs holds inside models.\nDeprivation does ",
                      "not: at 2C the within-model direction reverses in World, Africa, ",
                      "Europe, Latin America and North America.\nHealth has only two ",
                      "families available and cannot adjudicate either way.")) +
  th + theme(legend.position="top")
ggsave("Y10_within_model.png", p10, width=8.4, height=4.3, dpi=210)
cat("\nwritten: Y10_within_model.png\n")
