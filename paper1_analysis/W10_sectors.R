# =============================================================================
# W10 — WHICH SECTORS AND TECHNOLOGIES ACTUALLY CARRY THE RESULT?
#
# The wellbeing results say WHAT differs between the arms. This says WHERE
# inside the energy system the difference sits, which is what turns a
# statistical result into a mechanism a reader can argue with.
#
# THREE QUESTIONS:
#
#   1. JOBS — which technologies? The headline is "renewables minus fossil",
#      but that hides whether the gain is solar, wind or hydro, and whether the
#      fossil loss is coal, oil or gas. Those have completely different
#      political economies: a coal region and an oil exporter do not face the
#      same transition even when the net number is identical.
#
#   2. DEPRIVATION — which sectors? The decent-living gap is applied per sector
#      and truncated at zero, so a pathway can close the gap by delivering more
#      residential energy while delivering LESS industrial energy. Final Energy
#      is reported split into Industry, Transportation and (by subtraction)
#      Residential & Commercial, so the question is answerable.
#
#   3. Does the sectoral story differ between the regions where deprivation
#      holds and the regions where it reverses? If Rest of Asia's reversal is
#      concentrated in one sector, that is a mechanism; if it is spread evenly,
#      it is more consistent with the model-composition explanation.
#
# USAGE: Rscript W10_sectors.R
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

DROP <- "R10PAC_OECD"
SH <- c(`Aggregated R10 regions`="WORLD",R10AFRICA="Africa",`R10CHINA+`="China+",
        R10EUROPE="Europe",`R10INDIA+`="India+",R10LATIN_AM="Latin America",
        R10MIDDLE_EAST="Middle East",R10NORTH_AM="North America",R10PAC_OECD="Pacific OECD",
        R10REF_ECON="Reforming econ.",R10REST_ASIA="Rest of Asia")
LAB <- readRDS("pw_A.rds") %>% filter(!is.na(Pathway_excl)) %>%
  distinct(Model, Scenario, Pathway = Pathway_excl, Ambition) %>%
  mutate(amb = ifelse(grepl("^1\\.5", Ambition), "1.5C", "2C"))
POPR <- readRDS("ds_A.rds") %>% filter(Variable == "Total CDR") %>%
  distinct(Model, Scenario, Region, pop_mln) %>% filter(Region %in% R10_TEN)

# =============================================================================
line("1. JOBS BY FUEL — where the work is gained and where it is lost")
# =============================================================================
J <- readRDS("jobs_type.rds") %>%
  filter(Year >= 2020, Year <= 2050, Region %in% R10_TEN) %>%
  group_by(Model, Scenario, Region, fuel, tech_group) %>%
  summarise(k = sum(jobs_thousands, na.rm = TRUE) * 10, .groups = "drop") %>%  # job-years
  inner_join(POPR, by = c("Model","Scenario","Region")) %>%
  mutate(per1k = 1000*k/(pop_mln*1000)) %>%
  inner_join(LAB, by = c("Model","Scenario"))

world_fuel <- J %>% group_by(Model, Scenario, amb, Pathway, fuel, tech_group) %>%
  summarise(per1k = sum(per1k), .groups = "drop") %>%
  group_by(amb, Pathway, fuel, tech_group) %>%
  summarise(m = median(per1k), .groups = "drop") %>%
  pivot_wider(names_from = Pathway, values_from = m, values_fill = 0) %>%
  mutate(gap = `High-RE` - `High-CMT`) %>% arrange(amb, desc(gap))
cat("job-years per 1,000 people, cumulative 2020-2050, World\n\n")
print(as.data.frame(world_fuel %>%
      transmute(amb, fuel, tech_group,
                `High-CMT`=round(`High-CMT`,2), `High-RE`=round(`High-RE`,2),
                gap=round(gap,2))))
cat("\nshare of the TOTAL jobs gap carried by each fuel (1.5C):\n")
w15 <- world_fuel %>% filter(amb == "1.5C")
print(as.data.frame(w15 %>% transmute(fuel, gap = round(gap,2),
      share = sprintf("%+.0f%%", 100*gap/sum(gap[gap>0])))))

# =============================================================================
line("2. DEPRIVATION BY SECTOR — which end-use carries the difference")
# =============================================================================
FE <- readRDS("fe_pop.rds") %>%
  filter(Variable %in% c("Final Energy","Final Energy|Industry",
                         "Final Energy|Transportation"),
         Year >= 2020, Year <= 2050) %>%
  group_by(Model, Scenario, Region, Variable) %>%
  summarise(v = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Variable, values_from = v) %>%
  rename(total = `Final Energy`, industry = `Final Energy|Industry`,
         transport = `Final Energy|Transportation`) %>%
  # residential & commercial is what is left once industry and transport are out
  mutate(resid_comm = total - industry - transport) %>%
  filter(Region %in% R10_TEN, !is.na(total)) %>%
  inner_join(LAB, by = c("Model","Scenario"))
cat("scenarios with a full sector split:",
    n_distinct(paste(FE$Model, FE$Scenario)[!is.na(FE$resid_comm)]), "\n\n")

sect <- FE %>% filter(!is.na(resid_comm)) %>%
  pivot_longer(c(total, industry, transport, resid_comm),
               names_to = "sector", values_to = "gj_pc") %>%
  group_by(Region, amb, sector) %>%
  summarise(cmt = median(gj_pc[Pathway=="High-CMT"], na.rm=TRUE),
            re  = median(gj_pc[Pathway=="High-RE"],  na.rm=TRUE),
            d   = cliff_d(gj_pc[Pathway=="High-CMT"], gj_pc[Pathway=="High-RE"]),
            .groups = "drop") %>%
  mutate(pct = 100*(re-cmt)/cmt, reg = SH[Region])
cat("Final energy per capita by sector, GJ/cap, mean over 2020-2050.\n")
cat("Positive pct = High-RE delivers MORE of that sector's energy.\n\n")
print(as.data.frame(sect %>% filter(Region != DROP) %>%
      transmute(reg, amb, sector, cmt=round(cmt,1), re=round(re,1),
                pct=round(pct), d=round(d,2)) %>%
      arrange(sector, reg, amb)))

# =============================================================================
line("3. DOES THE SECTOR STORY EXPLAIN THE DEPRIVATION REVERSALS?")
# =============================================================================
DEP <- readRDS("RAW_RESULTS.rds") %>%
  filter(approach=="A full database", sample=="all scenarios",
         outcome=="gap_GJ_pc", !is.na(gap)) %>%
  transmute(Region, amb, dep_gap = gap, dep_sig = sig_raw)
K <- sect %>% filter(sector == "resid_comm") %>%
  select(Region, amb, reg, rc_pct = pct, rc_d = d) %>%
  inner_join(sect %>% filter(sector=="industry") %>% select(Region, amb, ind_pct = pct),
             by=c("Region","amb")) %>%
  inner_join(DEP, by = c("Region","amb")) %>%
  filter(Region != DROP)
cat("Residential & commercial is the sector decent living depends on most.\n")
cat("If High-RE delivers more of it, deprivation should fall.\n\n")
print(as.data.frame(K %>% transmute(reg, amb,
      resid_comm_pct = round(rc_pct), industry_pct = round(ind_pct),
      deprivation_gap_closed = round(dep_gap,2), sig = dep_sig) %>%
      arrange(desc(deprivation_gap_closed))))
cat("\ncorrelation, residential/commercial energy gap vs deprivation gap closed: ",
    round(cor(K$rc_pct, K$dep_gap, use="complete.obs"), 2), "\n", sep="")
cat("correlation, industrial energy gap vs deprivation gap closed:            ",
    round(cor(K$ind_pct, K$dep_gap, use="complete.obs"), 2), "\n", sep="")
cat("\nIf the residential correlation is strong and positive, the deprivation\n")
cat("result has a genuine sectoral mechanism. If it is weak, the reversals are\n")
cat("more consistent with the model-composition explanation.\n")
saveRDS(list(fuel=world_fuel, sector=sect, link=K), "W10_SECTORS.rds")
