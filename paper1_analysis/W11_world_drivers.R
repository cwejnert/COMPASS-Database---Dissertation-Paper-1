# =============================================================================
# W11 — WHICH REGIONS DRIVE THE WORLD RESULT (all three outcomes),
#       AND WHICH SECTOR DRIVES DEPRIVATION
#
# TWO QUESTIONS THE NARRATIVE NEEDS.
#
# (A) THE WORLD NUMBER IS NOT A REGION. It is a population-weighted aggregate of
#     ten regions, so a region contributes to it in proportion to BOTH its own
#     gap AND its share of world population. A large per-capita effect in a
#     small region moves the World number very little; a moderate effect in
#     India+ or China+ moves it a lot. The paper should say which regions the
#     World headline actually rests on, because "High-RE is better globally"
#     means something different if it is carried by two regions than if it is
#     spread evenly.
#
# (B) WHICH SECTOR CARRIES DEPRIVATION. W10 found the deprivation result
#     correlates +0.63 with the INDUSTRIAL final-energy gap and -0.15 with the
#     residential and commercial gap. That is the opposite of what a household
#     energy-poverty measure should do, and it needs pinning down: the DESIRE
#     sector weights put 53% on transport, 30% on residential/commercial and
#     17% on industry, so transport is the obvious third candidate.
#
# USAGE: Rscript W11_world_drivers.R
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

DROP <- "R10PAC_OECD"
SH <- c(R10AFRICA="Africa",`R10CHINA+`="China+",R10EUROPE="Europe",`R10INDIA+`="India+",
        R10LATIN_AM="Latin America",R10MIDDLE_EAST="Middle East",R10NORTH_AM="North America",
        R10PAC_OECD="Pacific OECD",R10REF_ECON="Reforming econ.",R10REST_ASIA="Rest of Asia")

POPR <- readRDS("ds_A.rds") %>% filter(Variable=="Total CDR") %>%
  distinct(Model, Scenario, Region, pop_mln) %>% filter(Region %in% R10_TEN)
popshare <- POPR %>% group_by(Region) %>% summarise(pop = median(pop_mln), .groups="drop") %>%
  mutate(share = pop/sum(pop), reg = SH[Region])

line("0. POPULATION WEIGHTS — the reason World is not an average of regions")
print(as.data.frame(popshare %>% transmute(reg, pop_mln = round(pop),
      share = sprintf("%.1f%%", 100*share)) %>% arrange(desc(pop_mln))))

# =============================================================================
line("A. REGIONAL CONTRIBUTIONS TO THE WORLD GAP")
# =============================================================================
# World per-capita gap = sum over regions of (regional gap x regional pop share).
# So contribution_r = gap_r * share_r, and the shares sum to the World gap.
R <- readRDS("RAW_RESULTS.rds") %>%
  filter(approach=="A full database", sample=="all scenarios",
         outcome %in% c("REFOSS","gap_GJ_pc","mort_per_1k"),
         Region %in% R10_TEN, !is.na(gap)) %>%
  select(-any_of("reg")) %>%      # RAW_RESULTS already carries a reg column
  left_join(popshare %>% select(Region, share, reg), by="Region") %>%
  mutate(contrib = gap * share,
         famshort = c(REFOSS="Jobs", gap_GJ_pc="Deprivation",
                      mort_per_1k="Mortality")[outcome])

for (fm in c("Jobs","Deprivation","Mortality")) for (a in c("1.5C","2C")) {
  d <- R %>% filter(famshort==fm, amb==a) %>% arrange(desc(contrib))
  tot <- sum(d$contrib)
  cat("\n---", fm, a, "--- implied World gap from the ten regions:",
      round(tot,2), "\n")
  print(as.data.frame(d %>% transmute(reg,
        regional_gap = round(gap,2),
        pop_share = sprintf("%.1f%%", 100*share),
        contribution = round(contrib,3),
        pct_of_world = sprintf("%+.0f%%", 100*contrib/tot))))
  top2 <- d %>% slice_max(contrib, n=2)
  cat(sprintf("  top two regions (%s, %s) carry %.0f%% of the World gap\n",
      top2$reg[1], top2$reg[2], 100*sum(top2$contrib)/tot))
  # How concentrated is it? Share of the total carried by the largest two, and
  # whether any region pulls the other way.
  cat(sprintf("  regions pulling AGAINST the World direction: %s\n",
      ifelse(any(sign(d$contrib) != sign(tot)),
             paste(d$reg[sign(d$contrib) != sign(tot)], collapse=", "), "none")))
}

# =============================================================================
line("B. WHICH SECTOR CARRIES THE DEPRIVATION RESULT?")
# =============================================================================
LAB <- readRDS("pw_A.rds") %>% filter(!is.na(Pathway_excl)) %>%
  distinct(Model, Scenario, Pathway=Pathway_excl, Ambition) %>%
  mutate(amb = ifelse(grepl("^1\\.5",Ambition),"1.5C","2C"))
FE <- readRDS("fe_pop.rds") %>%
  filter(Variable %in% c("Final Energy","Final Energy|Industry","Final Energy|Transportation"),
         Year>=2020, Year<=2050) %>%
  group_by(Model, Scenario, Region, Variable) %>%
  summarise(v = mean(Value, na.rm=TRUE), .groups="drop") %>%
  pivot_wider(names_from=Variable, values_from=v) %>%
  rename(total=`Final Energy`, industry=`Final Energy|Industry`,
         transport=`Final Energy|Transportation`) %>%
  mutate(resid_comm = total - industry - transport) %>%
  filter(Region %in% R10_TEN, !is.na(resid_comm)) %>%
  inner_join(LAB, by=c("Model","Scenario"))

sect <- FE %>%
  pivot_longer(c(total, industry, transport, resid_comm),
               names_to="sector", values_to="gj") %>%
  group_by(Region, amb, sector) %>%
  summarise(cmt = median(gj[Pathway=="High-CMT"], na.rm=TRUE),
            re  = median(gj[Pathway=="High-RE"],  na.rm=TRUE), .groups="drop") %>%
  mutate(pct = 100*(re-cmt)/cmt, reg = SH[Region])

DEP <- readRDS("RAW_RESULTS.rds") %>%
  filter(approach=="A full database", sample=="all scenarios",
         outcome=="gap_GJ_pc", Region %in% R10_TEN, !is.na(gap)) %>%
  transmute(Region, amb, dep = gap)

W <- sect %>% select(Region, amb, sector, pct) %>%
  pivot_wider(names_from=sector, values_from=pct) %>%
  inner_join(DEP, by=c("Region","amb")) %>% filter(Region != DROP) %>%
  mutate(reg = SH[Region])

cat("DESIRE sector weights in the decent-living threshold:\n")
cat("  transport 11.8, residential/commercial 6.7, industry 3.8 (normalised:",
    "53% / 30% / 17%)\n\n")
cat("correlation of each sector's final-energy gap with the deprivation gap closed:\n")
for (v in c("resid_comm","transport","industry","total"))
  cat(sprintf("  %-12s %+.2f\n", v, cor(W[[v]], W$dep, use="complete.obs")))

cat("\nregion by region (positive pct = High-RE delivers MORE of that sector):\n")
print(as.data.frame(W %>% transmute(reg, amb,
      resid = round(resid_comm), transp = round(transport),
      indus = round(industry), total = round(total),
      dep_closed = round(dep,2)) %>% arrange(desc(dep_closed))))

cat("\nINTERPRETATION. The threshold weights transport most heavily, so if the\n")
cat("transport correlation is the strongest, the measure is largely tracking\n")
cat("mobility energy rather than household energy access -- which is a real\n")
cat("property of the Kikstra/DESIRE operationalisation and belongs in the\n")
cat("limitations, not a bug in this analysis.\n")
saveRDS(list(pop=popshare, contrib=R, sector=W), "W11_DRIVERS.rds")
