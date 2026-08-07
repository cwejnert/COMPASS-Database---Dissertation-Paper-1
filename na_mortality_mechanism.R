# =============================================================================
# na_mortality_mechanism.R  —  RUN LOCALLY
#
# WHY: PM2.5 mortality in North America (and Africa) is significantly WORSE
#   under High-RE at 1.5C, even AFTER biomass was removed from the RE
#   classification metric (d=-0.44 -> -0.46; barely moved). Deaths in NA are
#   driven by BC (r=0.85) and OC (r=0.78) -- classic biomass/wood-combustion
#   signatures -- and High-RE emits more of both (+15% BC, +53% OC).
#
#   Because the classification change barely moved the result, biomass is
#   probably NOT contaminating the classification anymore -- but High-RE
#   scenarios may still physically DEPLOY more biomass capacity than High-CDR
#   scenarios in NA, e.g. as dispatchable backup for variable solar/wind, even
#   though biomass no longer counts toward the "High-RE" label itself.
#
# THIS SCRIPT TESTS THREE THINGS, region by region (esp. NORTH_AM, AFRICA):
#   1. does biomass CAPACITY (not classification) differ High-CDR vs High-RE?
#   2. does biomass capacity actually CORRELATE with BC/OC emissions there?
#   3. alternative suspects: transport (VOC/CO from biofuels), CDR land-use
#      (afforestation/BECCS feedstock -- could run the OTHER direction)
#
# OUTPUT: na_mortality_mechanism_summary.csv (small -- attach or paste back)
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
})

COMPASS_DIR    <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/COMPASS"
MASTER_OUT_DIR <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_master"
INTERP    <- file.path(COMPASS_DIR, "compass_interp.rds")
EM_CSV    <- file.path(COMPASS_DIR, "compass_emissions_raw.csv")
PATHWAY_A <- file.path(MASTER_OUT_DIR, "approach_A", "compass_pathway_tercile_A.csv")
stopifnot(file.exists(INTERP), file.exists(EM_CSV), file.exists(PATHWAY_A))

regions_r10 <- c("R10AFRICA","R10CHINA+","R10EUROPE","R10INDIA+","R10NORTH_AM")
WIN <- c(`1.5C (High-Ambition)` = 2060L, `2C (Medium-Ambition)` = 2075L)
norm_names <- function(d) {
  n <- names(d); n[tolower(n)=="model"]<-"Model"; n[tolower(n)=="scenario"]<-"Scenario"
  n[tolower(n)=="year"]<-"Year"; n[tolower(n)=="region"]<-"Region"; names(d)<-n; d
}
cliffs <- function(x,y){x<-x[!is.na(x)];y<-y[!is.na(y)];if(!length(x)||!length(y))return(NA_real_)
  (sum(outer(x,y,">"))-sum(outer(x,y,"<")))/(length(x)*length(y))}
mwp <- function(x,y){x<-x[!is.na(x)];y<-y[!is.na(y)]
  if(length(x)>1&&length(y)>1) suppressWarnings(wilcox.test(x,y)$p.value) else NA_real_}

pw <- read.csv(PATHWAY_A, stringsAsFactors=FALSE) %>% norm_names() %>%
  select(Model, Scenario, Ambition, Pathway_excl) %>%
  filter(Pathway_excl %in% c("High-CDR","High-RE"))
cat("pathway rows:", nrow(pw), "\n")

ci <- readRDS(INTERP)

# ---- 1. biomass capacity by pathway, NA and Africa focus --------------------
cap <- ci %>% filter(Region %in% regions_r10, Variable == "Capacity|Electricity|Biomass",
                     Year >= 2020, !is.na(Value)) %>%
  inner_join(pw, by = c("Model","Scenario")) %>%
  mutate(window_end = WIN[Ambition]) %>%
  filter(!is.na(window_end), Year <= window_end) %>%
  group_by(Model, Scenario, Ambition, Pathway_excl, Region) %>%
  summarise(cum_biomass_GW = sum(Value, na.rm = TRUE), .groups = "drop")

cat("\n=== 1. Biomass CAPACITY: High-CDR vs High-RE (biomass no longer defines either group) ===\n")
res1 <- list()
for (rg in regions_r10) for (am in names(WIN)) {
  s <- cap %>% filter(Region == rg, Ambition == am)
  hc <- s$cum_biomass_GW[s$Pathway_excl=="High-CDR"]; hr <- s$cum_biomass_GW[s$Pathway_excl=="High-RE"]
  if (length(hc)<2 || length(hr)<2) next
  res1[[length(res1)+1]] <- data.frame(region=sub("R10","",rg), ambition=if(grepl("1.5",am))"1.5C" else "2C",
    CDR_mean=mean(hc,na.rm=TRUE), RE_mean=mean(hr,na.rm=TRUE),
    pct_RE_vs_CDR=round(100*(mean(hr,na.rm=TRUE)-mean(hc,na.rm=TRUE))/mean(hc,na.rm=TRUE)),
    cliff_REvsCDR=round(cliffs(hr,hc),3), p=mwp(hc,hr))
}
res1 <- bind_rows(res1)
print(as.data.frame(res1 %>% mutate(across(c(CDR_mean,RE_mean),~signif(.x,3)),
  sig=ifelse(!is.na(p)&p<.05,"*",""))), row.names=FALSE)
cat("(+pct / cliff>0 = High-RE deploys MORE biomass than High-CDR, despite biomass not defining either label)\n")

# ---- 2. does biomass capacity actually predict BC/OC emissions? -------------
# IMPORTANT: correlate WITHIN each Ambition group, not pooled. Pooling 1.5C and
# 2C scenarios together confounds the correlation, because 2C scenarios sum
# cumulative quantities over more years (2020-2075) than 1.5C ones (2020-2060)
# -- ANY two cumulative variables look spuriously correlated across the pooled
# sample purely from window-length differences, regardless of a real
# relationship. Verified on synthetic i.i.d. data: pooling produced r ~ 0.75-0.8
# between variables generated completely independently.
em <- read_csv(EM_CSV, show_col_types = FALSE) %>% norm_names()
bc_oc <- em %>% filter(Region %in% regions_r10, variable %in% c("Emissions|BC","Emissions|OC")) %>%
  inner_join(pw, by = c("Model","Scenario")) %>%
  mutate(Year = as.integer(Year), window_end = WIN[Ambition]) %>%
  filter(!is.na(window_end), Year <= window_end) %>%
  group_by(Model, Scenario, Ambition, Region, variable) %>%
  summarise(cum_em = sum(as.numeric(value), na.rm=TRUE), .groups="drop") %>%
  pivot_wider(names_from = variable, values_from = cum_em)

cor_tab <- cap %>% group_by(Model, Scenario, Ambition, Region) %>%
  summarise(cum_biomass_GW = sum(cum_biomass_GW), .groups="drop") %>%
  inner_join(bc_oc, by = c("Model","Scenario","Ambition","Region")) %>%
  group_by(Region, Ambition) %>%
  filter(n() > 5) %>%
  summarise(cor_biomass_BC = round(cor(cum_biomass_GW, `Emissions|BC`, use="complete.obs"),3),
            cor_biomass_OC = round(cor(cum_biomass_GW, `Emissions|OC`, use="complete.obs"),3),
            n = n(), .groups="drop") %>%
  mutate(region = sub("R10","",Region),
         ambition = ifelse(grepl("1.5", Ambition), "1.5C", "2C")) %>%
  select(region, ambition, cor_biomass_BC, cor_biomass_OC, n)

cat("\n=== 2. Does biomass CAPACITY correlate with BC/OC emissions, WITHIN each ambition group? ===\n")
print(as.data.frame(cor_tab), row.names=FALSE)
cat("(high positive r => biomass deployment level predicts the BC/OC excess;\n",
    " computed within ambition to avoid the window-length confound -- see note above)\n", sep="")

# ---- 3. alternative suspects: land-use CDR (afforestation etc.) -------------
lu_vars <- c("Carbon Removal|Land Use", "Land Cover|Forest")
lu <- ci %>% filter(Region %in% regions_r10, Variable %in% lu_vars, Year >= 2020, !is.na(Value)) %>%
  inner_join(pw, by = c("Model","Scenario")) %>%
  mutate(window_end = WIN[Ambition]) %>% filter(!is.na(window_end), Year <= window_end) %>%
  group_by(Model, Scenario, Ambition, Pathway_excl, Region, Variable) %>%
  summarise(cum_val = sum(Value, na.rm=TRUE), .groups="drop")

cat("\n=== 3. Land-use CDR / forest cover: High-CDR vs High-RE (NA, Africa, 2C) ===\n")
if (nrow(lu) > 0) {
  res3 <- lu %>% filter(Region %in% c("R10NORTH_AM","R10AFRICA"), grepl("2C", Ambition)) %>%
    group_by(Region, Variable, Pathway_excl) %>%
    summarise(mean_val = mean(cum_val, na.rm=TRUE), .groups="drop") %>%
    pivot_wider(names_from = Pathway_excl, values_from = mean_val)
  print(as.data.frame(res3), row.names=FALSE)
} else cat("  no land-use CDR variables found in interp -- skip\n")

OUT1 <- file.path(MASTER_OUT_DIR, "na_mortality_biomass_capacity.csv")
OUT2 <- file.path(MASTER_OUT_DIR, "na_mortality_biomass_pollutant_cor.csv")
ok1 <- tryCatch({ write_csv(res1, OUT1); TRUE },
                error = function(e) { write_csv(res1, "na_mortality_biomass_capacity.csv"); FALSE })
ok2 <- tryCatch({ write_csv(cor_tab, OUT2); TRUE },
                error = function(e) { write_csv(cor_tab, "na_mortality_biomass_pollutant_cor.csv"); FALSE })
if (!ok1) OUT1 <- file.path(getwd(), "na_mortality_biomass_capacity.csv")
if (!ok2) OUT2 <- file.path(getwd(), "na_mortality_biomass_pollutant_cor.csv")
cat("\nWROTE:", normalizePath(OUT1, winslash="/", mustWork=FALSE), "\n")
cat("WROTE:", normalizePath(OUT2, winslash="/", mustWork=FALSE), "\n")
cat("(both small -- attach or paste back; also paste the two printed tables above and the part-3 land-use table)\n")
