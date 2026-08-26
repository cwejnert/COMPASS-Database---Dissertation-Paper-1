import ixmp4
import pandas as pd

PLATFORM = "scenariocompass"

# Exact region names as they appear in the COMPASS database
REGIONS = [
    "World",
    "Africa (R10)",
    "China+ (R10)",
    "India+ (R10)",
    "North America (R10)",
    "Europe (R10)",
    "Latin America (R10)",
    "Middle East (R10)",
    "Pacific OECD (R10)",
    "Reforming Economies (R10)",
    "Rest of Asia (R10)",
]

VARIABLES = [
    # ---- Emissions -----------------------------------------------------------
    "Emissions|CO2",
    "Emissions|CO2|Energy",

    # ---- Final Energy --------------------------------------------------------
    "Final Energy",
    "Final Energy|Industry",
    "Final Energy|Transportation",

    # ---- Population ----------------------------------------------------------
    "Population",

    # ---- Secondary Energy|Electricity ----------------------------------------
    "Secondary Energy|Electricity",
    "Secondary Energy|Electricity|Coal",
    "Secondary Energy|Electricity|Coal|w/ CCS",
    "Secondary Energy|Electricity|Gas",
    "Secondary Energy|Electricity|Gas|w/ CCS",
    "Secondary Energy|Electricity|Oil",
    "Secondary Energy|Electricity|Biomass",
    "Secondary Energy|Electricity|Biomass|w/ CCS",
    "Secondary Energy|Electricity|Nuclear",
    "Secondary Energy|Electricity|Wind",
    "Secondary Energy|Electricity|Solar",
    "Secondary Energy|Electricity|Hydro",

    # ---- CDR components (COMPASS native variable names) ---------------------
    # Mapped to three CDR types:
    #
    # Novel CDR:
    "Carbon Capture|Direct Air Capture",
    "Carbon Removal|Geological Storage|Direct Air Capture",
    "Carbon Removal|Enhanced Weathering",
    "Carbon Capture|Energy|Biomass",
    #
    # Fossil CCS:
    "Carbon Capture|Energy|Fossil",
    "Carbon Capture|Geological Storage|Fossil",
    "Carbon Capture|Industrial Processes",
    "Carbon Capture|Geological Storage|Industrial Processes",
    #
    # Land-based CDR:
    "Carbon Removal|Land Use",
    #
    # Parent totals (cross-check):
    "Carbon Capture",
    "Carbon Removal",

    # ---- Capacity stock (GW) -----------------------------------------------
    "Capacity|Electricity|Biomass",
    "Capacity|Electricity|Biomass|w/ CCS",
    "Capacity|Electricity|Biomass|w/o CCS",
    "Capacity|Electricity|Coal",
    "Capacity|Electricity|Coal|w/ CCS",
    "Capacity|Electricity|Coal|w/o CCS",
    "Capacity|Electricity|Gas",
    "Capacity|Electricity|Gas|w/ CCS",
    "Capacity|Electricity|Gas|w/o CCS",
    "Capacity|Electricity|Geothermal",
    "Capacity|Electricity|Hydro",
    "Capacity|Electricity|Hydrogen",
    "Capacity|Electricity|Non-Biomass Renewables",
    "Capacity|Electricity|Nuclear",
    "Capacity|Electricity|Ocean",
    "Capacity|Electricity|Oil",
    "Capacity|Electricity|Oil|w/ CCS",
    "Capacity|Electricity|Oil|w/o CCS",
    "Capacity|Electricity|Other",
    "Capacity|Electricity|Solar",
    "Capacity|Electricity|Solar|CSP",
    "Capacity|Electricity|Solar|PV",
    "Capacity|Electricity|Solar|PV|Commercial",
    "Capacity|Electricity|Solar|PV|Residential",
    "Capacity|Electricity|Wind",
    "Capacity|Electricity|Wind|Offshore",
    "Capacity|Electricity|Wind|Onshore",

    # ---- Capacity Additions (GW/yr) -----------------------------------------
    "Capacity Additions|Electricity|Biomass",
    "Capacity Additions|Electricity|Biomass|w/ CCS",
    "Capacity Additions|Electricity|Biomass|w/o CCS",
    "Capacity Additions|Electricity|Coal",
    "Capacity Additions|Electricity|Coal|w/ CCS",
    "Capacity Additions|Electricity|Coal|w/o CCS",
    "Capacity Additions|Electricity|Gas",
    "Capacity Additions|Electricity|Gas|w/ CCS",
    "Capacity Additions|Electricity|Gas|w/o CCS",
    "Capacity Additions|Electricity|Geothermal",
    "Capacity Additions|Electricity|Hydro",
    "Capacity Additions|Electricity|Hydrogen",
    "Capacity Additions|Electricity|Non-Biomass Renewables",
    "Capacity Additions|Electricity|Nuclear",
    "Capacity Additions|Electricity|Oil",
    "Capacity Additions|Electricity|Oil|w/ CCS",
    "Capacity Additions|Electricity|Oil|w/o CCS",
    "Capacity Additions|Electricity|Solar",
    "Capacity Additions|Electricity|Solar|CSP",
    "Capacity Additions|Electricity|Solar|PV",
    "Capacity Additions|Electricity|Solar|PV|Commercial",
    "Capacity Additions|Electricity|Solar|PV|Residential",
    "Capacity Additions|Electricity|Wind",
    "Capacity Additions|Electricity|Wind|Offshore",
    "Capacity Additions|Electricity|Wind|Onshore",

    # ---- Air pollutant emissions (total aggregates) -------------------------
    # Included here so they sit alongside energy/CDR variables for diagnostics.
    # Also pulled separately (with World region) in the rfasst emissions file.
    "Emissions|Sulfur",
    "Emissions|NOx",
    "Emissions|BC",
    "Emissions|OC",
    "Emissions|CO",
    "Emissions|NH3",
    "Emissions|VOC",
    "Emissions|CH4",
]

OUT_DATA = r"C:\Users\camwe\OneDrive\Documents\YSSP_CDR_wellbeing\Data\COMPASS\compass_r10_raw.csv"
OUT_META = r"C:\Users\camwe\OneDrive\Documents\YSSP_CDR_wellbeing\Data\COMPASS\compass_r10_meta.csv"

print("Connecting to scenariocompass...")
platform = ixmp4.Platform(PLATFORM)

# Split VARIABLES into smaller batches to avoid network timeout
# during pagination of large requests
BATCH_SIZE = 15
var_batches = [VARIABLES[i:i+BATCH_SIZE] for i in range(0, len(VARIABLES), BATCH_SIZE)]
print(f"Fetching timeseries data in {len(var_batches)} batches of up to {BATCH_SIZE} variables...")

region_map = {
    "World":               "World",
    "Africa (R10)":        "R10AFRICA",
    "China+ (R10)":        "R10CHINA+",
    "India+ (R10)":        "R10INDIA+",
    "North America (R10)": "R10NORTH_AM",
    "Europe (R10)":        "R10EUROPE",
}

all_batches = []
for b, batch in enumerate(var_batches):
    print(f"  Batch {b+1}/{len(var_batches)}: {len(batch)} variables...")
    try:
        batch_df = platform.iamc.tabulate(
            region=REGIONS,
            variable=batch,
        )
        print(f"    Rows returned: {len(batch_df)}")
        all_batches.append(batch_df)
    except Exception as e:
        print(f"    ERROR in batch {b+1}: {e}")
        print("    Retrying batch once...")
        try:
            batch_df = platform.iamc.tabulate(
                region=REGIONS,
                variable=batch,
            )
            all_batches.append(batch_df)
            print(f"    Retry succeeded: {len(batch_df)} rows")
        except Exception as e2:
            print(f"    Retry failed: {e2} — skipping batch")

if all_batches:
    ts = pd.concat(all_batches, ignore_index=True)
    print(f"\nTotal rows combined: {len(ts)}")

    # Report found/missing
    found   = set(ts["variable"].unique())
    missing = sorted(set(VARIABLES) - found)
    print(f"Variables returned: {len(found)} of {len(VARIABLES)} requested")
    if missing:
        print(f"Variables NOT found in COMPASS ({len(missing)}):")
        for v in missing:
            print(f"  - {v}")
    else:
        print("All requested variables returned")

    # Rename regions
    ts["region"] = ts["region"].replace(region_map)

    ts.to_csv(OUT_DATA, index=False)
    print(f"\nTimeseries saved to: {OUT_DATA}")
    print(f"Rows: {len(ts)}")
else:
    print("ERROR: All batches failed — check API connection and variable names")


# =============================================================================
# METADATA
# =============================================================================
print("\nFetching metadata...")
meta = platform.meta.tabulate()

meta_wide = meta.pivot_table(
    index=["model", "scenario", "version"],
    columns="key",
    values="value",
    aggfunc="first"
).reset_index()
meta_wide.columns.name = None

print(f"Metadata keys: {[c for c in meta_wide.columns if c not in ['model','scenario','version']][:20]}")
meta_wide.to_csv(OUT_META, index=False)
print(f"Metadata saved to: {OUT_META}")


# =============================================================================
# AIR POLLUTANT EMISSIONS (separate file for rfasst — includes World region)
# =============================================================================
EMISSION_VARS = [
    # Required by rfasst for PM2.5 and O3 concentration modelling
    "Emissions|Sulfur",
    "Emissions|NOx",
    "Emissions|BC",
    "Emissions|OC",
    "Emissions|CO",
    "Emissions|NH3",
    "Emissions|VOC",
    "Emissions|CH4",
    # Note: COMPASS does not report Emissions|PM2.5 directly.
    # rfasst derives PM2.5 concentration from BC + OC + SO2 + NOx precursors.
]

EMISSION_REGIONS = [
    "Africa (R10)",
    "China+ (R10)",
    "India+ (R10)",
    "North America (R10)",
    "Europe (R10)",
    "World",
]

OUT_EMISSIONS = r"C:\Users\camwe\OneDrive\Documents\YSSP_CDR_wellbeing\Data\COMPASS\compass_emissions_raw.csv"

print("\nFetching air pollutant emissions for rfasst...")
em = platform.iamc.tabulate(
    variable=EMISSION_VARS,
    region=EMISSION_REGIONS,
)

region_map_em = {
    "Africa (R10)":        "R10AFRICA",
    "China+ (R10)":        "R10CHINA+",
    "India+ (R10)":        "R10INDIA+",
    "North America (R10)": "R10NORTH_AM",
    "Europe (R10)":        "R10EUROPE",
    "World":               "World",
}
em["region"] = em["region"].replace(region_map_em)

print(f"Emissions rows:      {len(em)}")
print(f"Variables returned:  {sorted(em['variable'].unique())}")
print(f"Regions returned:    {sorted(em['region'].unique())}")
print(f"Scenarios:           {em[['model','scenario']].drop_duplicates().shape[0]}")

em.to_csv(OUT_EMISSIONS, index=False)
print(f"Saved to: {OUT_EMISSIONS}")
print("\nDone.")
