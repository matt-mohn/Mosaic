
# Notes on Adding Your Own Shapefiles

![ ](../assets/icon_shape.png)

A Mosaic shapefile should include the following attributes:

- **GEOID20** (or `GEOID`, `Precinct`, `PCT`) - a unique identifier string for each precinct
- **CTY** - a string column with county identifiers
- **DEM** - a numeric column with Democratic votes for your chosen election
- **REP** - a numeric column with Republican votes
- **POP** - a numeric column with total population in each precinct

### Handling Idiosyncrasies

Mosaic includes some automatic handling for common shapefile issues:

- **Lowercase column names**: Columns like `pop`, `dem`, `rep`, `cty` are automatically normalized to uppercase
- **Character columns**: If `POP`, `DEM`, or `REP` are stored as strings (e.g., `"1234"`), Mosaic will parse them to numeric with a warning
- **Integer county codes**: `CTY` can be numeric county FIPS codes (e.g., `37001`) or string names (e.g., `"WAKE"`)
- **Duplicate GEOIDs**: Mosaic will warn you if duplicate precinct IDs are detected

If validation fails, Mosaic will provide a clear error message indicating what needs to be fixed.

---

## Guide: Importing from Dave's Redistricting App

This guide walks through creating a Mosaic-ready shapefile using Census TIGER/Line data and election results from Dave's Redistricting App.

**Step 1** - Determine the state you'd like to use. Here, we choose Minnesota.

**Step 2** - Download Minnesota's 2020 VTD shapefile from the [U.S. Census TIGER/Line repository](https://catalog.data.gov/dataset/tiger-line-shapefile-2020-state-minnesota-voting-districts). Download the `.zip` file (e.g., `tl_2020_27_vtd20.zip` - the `27` is Minnesota's FIPS code) and extract it.

**Step 3** - Navigate to [Dave's Redistricting App](https://davesredistricting.org) and start a map for Minnesota. In the Data Selector sidebar, select the election you want to use (e.g., "President 2024"). Then click Export Map and download the "Precinct Data" CSV.

**Step 4** - Place the unzipped Census shapefile and the DRA CSV in the same folder.

**Step 5** - Join the data and rename columns to Mosaic's format. You can do this in QGIS or R. Here is example R code:

```r
library(dplyr)
library(sf)

# === CONFIGURATION ===
# Set these paths to match your files

SHAPEFILE_PATH <- "tl_2020_27_vtd20.shp"   # Census TIGER/Line shapefile
DATA_PATH      <- "precinct-data.csv"      # DRA precinct data export
SAVE_NAME      <- "Minnesota_Ready.shp"    # Output filename

# === COLUMN MAPPINGS ===
# Find your column names by running: colnames(read_sf(SHAPEFILE_PATH))
#                               and: colnames(read.csv(DATA_PATH))

id_column         <- "GEOID20"              # Shared ID column (in both files)
population_column <- "T_20_CENS_Total"      # Population (in DRA data)
democrat_column   <- "E_24_PRES_Dem"        # Dem votes (in DRA data)
republican_column <- "E_24_PRES_Rep"        # Rep votes (in DRA data)
county_column     <- "COUNTYFP20"           # County FIPS (in shapefile)

# === LOAD DATA ===

shapefile <- read_sf(SHAPEFILE_PATH)
dra_data  <- read.csv(DATA_PATH)

# Ensure ID columns are character type for reliable joining
shapefile[[id_column]] <- as.character(shapefile[[id_column]])
dra_data[[id_column]]  <- as.character(dra_data[[id_column]])

# === EXTRACT AND RENAME DRA COLUMNS ===

extracted <- dra_data |>
  select(
    GEOID20 = all_of(id_column),
    POP     = all_of(population_column),
    DEM     = all_of(democrat_column),
    REP     = all_of(republican_column)
  )

# === JOIN TO SHAPEFILE ===

shapefile <- shapefile |>
  rename(GEOID20 = all_of(id_column)) |>
  left_join(extracted, by = "GEOID20")

# === ADD COUNTY COLUMN ===

shapefile$CTY <- as.character(shapefile[[county_column]])

# === FINALIZE AND SAVE ===

shapefile <- shapefile |>
  select(GEOID20, POP, DEM, REP, CTY, geometry) |>
  filter(!is.na(POP))  # Remove precincts with no population data

cat("Rows:", nrow(shapefile), "\n")
cat("Columns:", paste(names(shapefile), collapse = ", "), "\n")

st_write(shapefile, SAVE_NAME, delete_layer = TRUE)
cat("Saved to:", SAVE_NAME, "\n")
```

**Step 6** - Move the output shapefile (and its associated `.shx`, `.dbf`, `.prj` files) to Mosaic's `shapefiles/` folder. You're ready to go!

---

## Additional Guide: Rejoining to DRA

If you run a Mosaic chain on data from DRA (or a Census 2020 VTD shapefile), you can easily import the results back into DRA.

1. Find the `final_assignment_*.csv` file in Mosaic's `output/` folder
2. In DRA, use the "Color Map from File" sidebar panel
3. Upload the CSV and DRA will match precincts by GEOID

**Troubleshooting**: If DRA can't match precincts, the GEOID column may have been corrupted. Common issues:
- Leading zeros dropped (e.g., `"001"` became `1`) - often caused by Excel
- Type conversion from string to number and back
- Ensure the GEOID column is stored as text, not numeric

---

## Additional Guide: Simplifying in QGIS

Large shapefiles can slow down Mosaic's plotting and GIF functions. Here's how to simplify a shapefile in QGIS while preserving accuracy:

1. **Fix geometries** - Run "Fix geometries" or "Repair shapefile" from the Processing Toolbox to clean up any corrupted edges

2. **Generalize** - Run `v.generalize` from GRASS GIS (available in QGIS). Keep `v.in.ogr snap` at `-1` (default) to preserve geographic precision

3. **Fix again** - Run "Fix geometries" on the generalized result to repair any issues introduced by simplification

4. **Dissolve** - Dissolve by the ID column (e.g., `GEOID20`) to merge any accidentally-split precincts

5. **Save** - Export as a new shapefile with a descriptive name (e.g., `Minnesota_Simplified.shp`)

The `v.generalize` function does the heavy lifting - the other steps help avoid errors that can arise from simplification.
