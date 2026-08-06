# =============================================================================
# DLE — full correction to match Kikstra et al. (Decent Living Energy)
#   Sources: Kikstra et al. 2025, "Closing decent living gaps ... DESIRE",
#            Environ. Res. Lett. 20 054038  (doi:10.1088/1748-9326/adc3ad)
#            Kikstra et al. 2021, Environ. Res. Lett. 16 095006
#            (doi:10.1088/1748-9326/ac1c27)
#
# This patches three DLE problems in Section 4c of COMPASS_master_analysis_2.R:
#   FIX 1 — thresholds recalibrated to DESIRE (levels, sector split, needs-floor)
#   FIX 2 — provisioning-efficiency (SEF) steepened to match DESIRE (~-38% by 2040)
#   FIX 3 — the energy GAP computed distributionally (lognormal), consistent with
#           the headcount and with DESIRE ("lift everyone below the threshold up
#           to it"), instead of the mean-based (threshold - mean) shortfall.
#
# SCOPE NOTE: COMPASS gives us top-down IAM final energy, so we keep the
#   top-down "threshold vs modelled energy" approach (as DESIRE does when it
#   overlays DLE on IAM output) rather than rebuilding DESIRE's full bottom-up
#   service accounting (pkm, m2, materials). The point here is to make that
#   overlay DESIRE-consistent in its thresholds, efficiency and gap integral.
# =============================================================================

# -----------------------------------------------------------------------------
# FIX 1 — DESIRE-calibrated DLE final-energy thresholds  (GJ/capita/yr)
# -----------------------------------------------------------------------------
# DESIRE global average ~22 GJ/cap [country range 17-35], with TRANSPORT the
# largest sector (~11.8 [10-18]), residential/commercial ~6.7 [4-12], industry
# ~3.8 [3-7]. DLE is a NEEDS floor: it varies mainly with climate (heating ->
# res_comm) and settlement/geography (distance -> transport), NOT with wealth.
#
# The values below map those published sectoral means/ranges onto the 5 R10
# regions used here, keeping every sector inside DESIRE's published range,
# transport largest in every region, and totals within DESIRE's 17-35 span.
# They REPLACE the old table (res_comm-largest, totals up to 63 GJ/cap).
#   >> For a final dissertation number, obtain DESIRE's country-level thresholds
#      and population-weight them to R10 (or ask the authors); treat these as a
#      documented, DESIRE-consistent best estimate until then. <<
dle_thresholds <- tibble::tribble(
  ~Region,        ~res_comm_GJ, ~transport_GJ, ~industry_GJ,   # total  (rationale)
  "R10INDIA+",           4.5,          10.5,         3.0,       # 18.0   warm, dense; DESIRE low end
  "R10AFRICA",           5.0,          11.0,         3.0,       # 19.0   warm; some long-distance transport
  "R10CHINA+",           6.5,          11.5,         4.0,       # 22.0   ~ DESIRE global average
  "R10EUROPE",           9.0,          12.0,         4.5,       # 25.5   cold -> heating raises res_comm
  "R10NORTH_AM",        11.0,          18.0,         5.5        # 34.5   cold+hot, car-dependent; DESIRE high end
)
# (Old table for reference — DO NOT USE:
#   AFRICA 12/8/4.5=24.5 | CHINA+ 18/14/5=37 | EUROPE 28/16/8=52 |
#   INDIA+ 10/8.5/4=22.5 | NORTH_AM 35/18/10=63  -> res_comm largest, rich-skewed)

# -----------------------------------------------------------------------------
# FIX 2 — provisioning-efficiency (SEF): steepen to DESIRE's ~30-46% by 2040
# -----------------------------------------------------------------------------
# DESIRE lowers DLE needs ~30-46% by 2040 via end-use efficiency, structural
# shift and fuel-switching. The old SEF (1.0-1.5%/yr) gives only ~-24% by 2040.
# ~1.9%/yr lands at ~-38% by 2040 (midpoint), then holds a floor.
SEF_RATE  <- 0.019     # per year (was 0.010-0.015, sector-specific)
SEF_FLOOR <- 0.5
sef_lookup <- tidyr::expand_grid(
  Year   = unique(compass_ts$Year),
  sector = c("res_comm", "industry", "transport")
) %>%
  dplyr::mutate(SEF = pmax(SEF_FLOOR, 1 - SEF_RATE * (Year - 2020)))
sef_total <- tibble::tibble(Year = unique(compass_ts$Year)) %>%
  dplyr::mutate(SEF_total = pmax(SEF_FLOOR, 1 - SEF_RATE * (Year - 2020)))
# (thr_pc = base(region,sector) * SEF, exactly as before — only the rate changed)

# -----------------------------------------------------------------------------
# FIX 3 — distributional energy gap (replaces the mean-based sector gap)
# -----------------------------------------------------------------------------
# Old (WRONG): gap_pc = max(0, threshold - MEAN energy_pc); summed over sectors.
#   -> reads ZERO whenever mean >= threshold, ignoring the poor tail; and it is
#      inconsistent with the headcount, which integrates the lognormal.
# DESIRE gap = energy to lift everyone BELOW the threshold up to it
#   = partial expectation of (threshold - energy) over the below-threshold tail.
# For lognormal energy X with mean E and log-sd s (from the energy Gini):
#   m  = ln(E) - s^2/2
#   d1 = (ln(T) - m)/s ;  d2 = d1 - s
#   headcount rate = Phi(d1)                 (unchanged)
#   gap per capita = T*Phi(d1) - E*Phi(d2)   (NEW — distributional)
#
# Compute the gap on the SAME region-total lognormal as the headcount. In
# Section 4c, DELETE the sector-level gap_GJ_pc / gap_EJ_total lines (in evt_3s
# and evt_1s), and replace the `dle_headcount_annual` mutate with this:

dle_headcount_annual <- evt %>%
  dplyr::group_by(Model_Group, Model, Scenario, ModelGroup_Scenario,
                  Region, Year, Category, pop_millions) %>%
  dplyr::summarise(energy_GJ_pc_total    = sum(energy_GJ_pc,    na.rm = TRUE),
                   threshold_GJ_pc_total = sum(threshold_GJ_pc, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::left_join(energy_gini, by = "Region") %>%     # sigma_ln from energy Gini
  dplyr::mutate(
    s   = sigma_ln,
    m   = log(pmax(energy_GJ_pc_total,    0.01)) - s^2 / 2,
    d1  = (log(pmax(threshold_GJ_pc_total, 0.01)) - m) / s,
    d2  = d1 - s,
    deprivation_rate   = pnorm(d1),                    # headcount (unchanged)
    headcount_millions = deprivation_rate * pop_millions,
    gap_GJ_pc          = pmax(0, threshold_GJ_pc_total * pnorm(d1) -
                                  energy_GJ_pc_total   * pnorm(d2)),   # NEW
    gap_EJ_total       = gap_GJ_pc * (pop_millions * 1e6) / 1e9
  )
# implied CO2 is unchanged in FORM and now consumes the corrected gap_EJ_total,
# so it becomes distributional automatically.

# -----------------------------------------------------------------------------
# FIX 4 (verify, not auto-changed) — energy Gini coefficients
# -----------------------------------------------------------------------------
# DESIRE builds the lognormal from within-country ENERGY Gini coefficients
# (Oswald et al. 2021, "Global redistribution of income and household energy
# footprints", Nature Energy). The current per-R10 values
#   AFRICA 0.45 | CHINA+ 0.38 | EUROPE 0.25 | INDIA+ 0.42 | NORTH_AM 0.28
# are plausible but should be population-weighted from Oswald's energy Ginis for
# the countries in each R10 region. Left unchanged here pending that source.
#
# energy_gini <- tibble::tribble(
#   ~Region,        ~gini,
#   "R10AFRICA",    0.45, "R10CHINA+", 0.38, "R10EUROPE", 0.25,
#   "R10INDIA+",    0.42, "R10NORTH_AM", 0.28
# ) %>% dplyr::mutate(sigma_ln = sqrt(2) * qnorm((gini + 1) / 2))
