# =============================================================================
# biomass_check.R  —  RUN LOCALLY (quick, ~1 min)
#
# WHY: PM2.5 mortality is significantly WORSE under High-RE in North America
#      (-15%, Cliff's d -0.44, p=1.5e-05) and Africa (d -0.23, p=0.026) at
#      1.5C. The emissions diagnostic showed why: in those two regions deaths
#      are driven by BC and OC (r = 0.85-0.95), and High-RE emits MORE of both
#      (North America: +53% OC, +15% BC). Elsewhere (Europe, China+) deaths are
#      driven by NH3, which High-CDR emits far more of (+210-255%).
#
#      LEADING HYPOTHESIS: the BC/OC excess under High-RE is BIOENERGY
#      combustion -- biomass counts as "renewable" in our classification, and
#      biomass burning is a major primary carbonaceous-aerosol source.
#
# THIS SCRIPT TESTS IT: does High-RE actually deploy more biomass capacity than
#      High-CDR, and is that gap largest in the regions where mortality flips?
#
# OUTPUT: biomass_check_summary.csv  (small -- attach or paste back)
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr)
})

COMPASS_DIR    <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Data/COMPASS"
MASTER_OUT_DIR <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_master"
INTERP    <- file.path(COMPASS_DIR, "compass_interp.rds")
PATHWAY_A <- file.path(MASTER_OUT_DIR, "approach_A", "compass_pathway_tercile_A.csv")

stopifnot(file.exists(INTERP), file.exists(PATHWAY_A))
regions_r10 <- c("R10AFRICA","R10CHINA+","R10EUROPE","R10INDIA+","R10NORTH_AM")
WIN <- c(`1.5C (High-Ambition)` = 2060L, `2C (Medium-Ambition)` = 2075L)

cliffs <- function(x, y) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  if (!length(x) || !length(y)) return(NA_real_)
  (sum(outer(x, y, ">")) - sum(outer(x, y, "<"))) / (length(x) * length(y))
}
mwp <- function(x, y) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  if (length(x) > 1 && length(y) > 1) suppressWarnings(wilcox.test(x, y)$p.value) else NA_real_
}

pw <- read.csv(PATHWAY_A, stringsAsFactors = FALSE) %>%
  select(Model, Scenario, Ambition, Pathway_excl) %>%
  filter(Pathway_excl %in% c("High-CDR", "High-RE"))

ci <- readRDS(INTERP)

# Renewable capacity by technology -- is biomass over-represented in High-RE?
re_techs <- c("Solar","Wind","Hydro","Nuclear","Biomass","Geothermal")
cap <- ci %>%
  filter(Region %in% regions_r10,
         Variable %in% paste0("Capacity|Electricity|", re_techs),
         Year >= 2020, !is.na(Value)) %>%
  mutate(tech = sub("Capacity\\|Electricity\\|", "", Variable)) %>%
  inner_join(pw, by = c("Model","Scenario")) %>%
  mutate(window_end = WIN[Ambition]) %>%
  filter(!is.na(window_end), Year <= window_end) %>%
  group_by(Model, Scenario, Ambition, Pathway_excl, Region, tech) %>%
  summarise(cum_GW = sum(Value, na.rm = TRUE), .groups = "drop")

# biomass share of total renewable capacity, per scenario x region
share <- cap %>%
  group_by(Model, Scenario, Ambition, Pathway_excl, Region) %>%
  summarise(biomass_GW = sum(cum_GW[tech == "Biomass"], na.rm = TRUE),
            total_re_GW = sum(cum_GW, na.rm = TRUE), .groups = "drop") %>%
  filter(total_re_GW > 0) %>%
  mutate(biomass_share = 100 * biomass_GW / total_re_GW)

res <- list()
for (rg in regions_r10) for (amb in names(WIN)) {
  s <- share %>% filter(Region == rg, Ambition == amb)
  for (v in c("biomass_GW","biomass_share")) {
    hc <- s[[v]][s$Pathway_excl == "High-CDR"]; hr <- s[[v]][s$Pathway_excl == "High-RE"]
    if (length(hc) < 2 || length(hr) < 2) next
    res[[length(res)+1]] <- data.frame(
      region = sub("R10","",rg), ambition = if (grepl("1.5", amb)) "1.5C" else "2C",
      metric = v, CDR_mean = mean(hc, na.rm=TRUE), RE_mean = mean(hr, na.rm=TRUE),
      pct_diff_RE_vs_CDR = round(100*(mean(hr,na.rm=TRUE)-mean(hc,na.rm=TRUE))/mean(hc,na.rm=TRUE)),
      cliff = round(cliffs(hr, hc), 3),   # >0 = High-RE has MORE
      p = mwp(hc, hr), n_CDR = length(hc), n_RE = length(hr))
  }
}
res <- bind_rows(res)

cat("\n=== Does High-RE deploy more BIOMASS? (+ = High-RE more; cliff>0 = High-RE more) ===\n")
print(as.data.frame(res %>% filter(metric == "biomass_share") %>%
  transmute(region, ambition, CDR_pct = round(CDR_mean,1), RE_pct = round(RE_mean,1),
            diff_pct = pct_diff_RE_vs_CDR, cliff, p = signif(p,2))), row.names = FALSE)

cat("\n=== absolute cumulative biomass capacity ===\n")
print(as.data.frame(res %>% filter(metric == "biomass_GW") %>%
  transmute(region, ambition, CDR = signif(CDR_mean,3), RE = signif(RE_mean,3),
            diff_pct = pct_diff_RE_vs_CDR, cliff, p = signif(p,2))), row.names = FALSE)

cat("\nREAD: if biomass share/capacity is higher under High-RE (positive diff, cliff>0),\n",
    "     and largest in AFRICA / NORTH_AM, the BC+OC mortality penalty is bioenergy.\n")

OUT <- file.path(MASTER_OUT_DIR, "biomass_check_summary.csv")
ok <- tryCatch({ write_csv(res, OUT); TRUE },
               error = function(e) { write_csv(res, "biomass_check_summary.csv"); FALSE })
if (!ok) OUT <- file.path(getwd(), "biomass_check_summary.csv")
cat("\nWROTE:", normalizePath(OUT, winslash = "/", mustWork = FALSE), "\n")
