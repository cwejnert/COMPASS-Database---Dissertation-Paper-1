# =============================================================================
# S3 — is deaths_pm25 an ANNUAL RATE or an ALREADY-SUMMED DECADAL TOTAL?
#
# The master multiplies by 10 on the assumption it is an annual rate. If it is
# already a decadal total the multiplier double-counts by a factor of ten and
# every absolute mortality figure in the paper is wrong by that factor.
# (The pathway CONTRAST is unaffected either way -- it is a constant scale --
# but the levels, and the GBD/GEMM validation, are not.)
#
# Four independent tests.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr)})
options(width = 175)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

MA <- read.csv("mort_annual.csv", stringsAsFactors = FALSE)

line("TEST 1 — global total in a SINGLE year, against known benchmarks")
g <- MA %>% filter(!is.na(deaths_pm25)) %>%
  group_by(model, scenario, year) %>%
  summarise(n_reg = n(), global = sum(deaths_pm25), .groups="drop") %>%
  filter(n_reg == 10)
print(g %>% group_by(year) %>%
      summarise(scenarios = n(),
                median_mln = round(median(global)/1e6, 2),
                q25 = round(quantile(global,.25)/1e6, 2),
                q75 = round(quantile(global,.75)/1e6, 2), .groups="drop") %>%
      as.data.frame())
cat("\nBENCHMARK: PM2.5-attributable deaths are ~4.1 mln/yr (GBD) to ~8.9 mln/yr (GEMM).\n")
cat("FUSION sits between them, so an ANNUAL RATE in 2020 should be roughly 5-9 mln.\n")
cat("A DECADAL TOTAL would be roughly 50-90 mln.\n")

y2020 <- median(g$global[g$year==2020])/1e6
cat("\nOBSERVED 2020 median:", round(y2020,2), "mln\n")
cat("VERDICT:", if (y2020 > 3 & y2020 < 15) "ANNUAL RATE -- the x10 multiplier is correct"
             else if (y2020 > 30) "DECADAL TOTAL -- the x10 multiplier DOUBLE-COUNTS"
             else "ambiguous, check the other tests", "\n")

line("TEST 2 — 2010 is in the file. Does it match observed 2010 PM2.5 mortality?")
y2010 <- g %>% filter(year==2010)
cat("2010 global, median across", nrow(y2010), "scenarios:",
    round(median(y2010$global)/1e6, 2), "mln\n")
cat("All scenarios share a 2010 base year, so the spread should be near zero:\n")
cat("  min", round(min(y2010$global)/1e6,2), "| max", round(max(y2010$global)/1e6,2),
    "| relative spread", round(100*(max(y2010$global)-min(y2010$global))/median(y2010$global),1), "%\n")
cat("Observed global PM2.5 mortality around 2010 was ~4-5 mln/yr (GBD).\n")

line("TEST 3 — per-capita rate implied by each reading")
pop <- readRDS("ds_A.rds") %>% dplyr::filter(Variable=="Total CDR") %>%
  distinct(Region, pop_mln) %>% group_by(Region) %>%
  summarise(pop_mln = median(pop_mln), .groups="drop")
r2020 <- MA %>% filter(year==2020, !is.na(deaths_pm25)) %>%
  group_by(Region = r10_region) %>%
  summarise(d = median(deaths_pm25), .groups="drop") %>%
  inner_join(pop, by="Region") %>%
  mutate(as_annual  = round(d/(pop_mln*1e6)*1e5, 1),
         as_decadal = round(d/(pop_mln*1e6)*1e5/10, 1))
print(as.data.frame(r2020 %>% select(Region, as_annual, as_decadal)))
cat("\nBENCHMARK deaths per 100,000 per year, around 2020:\n")
cat("  India ~90-100 | China ~80-90 | E.Europe/Russia ~50-70 | W.Europe ~25-40\n")
cat("  Japan ~35-45  | USA ~20-25   | Sub-Saharan Africa ~60-80\n")
cat("The column that lands in those ranges is the correct reading.\n")

line("TEST 4 — does the rfasst summary equal a PLAIN sum of these values?")
ms <- readRDS("mort_summary.rds")
plain <- MA %>% filter(year>=2020, year<=2100) %>%
  group_by(model, scenario, r10_region) %>%
  summarise(s = sum(deaths_pm25, na.rm=TRUE), .groups="drop")
cmp <- ms %>% select(model, scenario, r10_region,
                     summary_deaths = cumulative_deaths_pm25_2020_2100) %>%
  inner_join(plain, by=c("model","scenario","r10_region")) %>%
  filter(summary_deaths > 0)
cat("comparable cells:", nrow(cmp), "| median ratio plain-sum / rfasst-summary:",
    round(median(cmp$s/cmp$summary_deaths), 4), "\n")
cat("If 1.00, the rfasst summary is a PLAIN SUM of these nine timesteps, i.e. the\n")
cat("annual file holds per-year rates and the summary is NOT an 81-year integral.\n")
