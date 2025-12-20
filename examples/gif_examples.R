# ============================================
# GIF EXAMPLES
# ============================================
# Two examples demonstrating Mosaic's GIF animation capabilities
# See docs/tutorials/gif-examples.md for full tutorial

source("mosaic.R")
load_mosaic()

SHAPEFILE <- "shapefiles/North_Carolina_Simplified.shp"

# ============================================
# EXAMPLE 1: Simple Compactness
# ============================================

cat("\n=== Example 1: Simple Compactness ===\n\n")

# Run the chain with snapshots enabled
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  num_steps = 2000,
  seed = 123456,
  county_bias = 10,
  weight_county_splits = 10,
  snapshot_assignments = TRUE,
  snapshot_interval = 50,
  verbose_console = FALSE
)

# Create the GIF
mosaic_gif(
  shapefile_path = SHAPEFILE,
  type = "simple",
  output_path = "example1_simple.gif",
  fps = 10,
  border_outline = TRUE,
  district_outline = TRUE,
  county_outline = TRUE,
  precinct_outline = FALSE,
  width = 700,
  height = 400
)

cat("\n✓ Example 1 GIF saved to: example1_simple.gif\n\n")

# ============================================
# EXAMPLE 2: Partisan Fairness
# ============================================

cat("\n=== Example 2: Partisan Fairness ===\n\n")

# Run the chain with partisan objective
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  num_steps = 2000,
  seed = 654321,
  county_bias = 5,
  weight_county_splits = 5,
  target_mean_median = 0,
  weight_mean_median = 100,
  snapshot_assignments = TRUE,
  snapshot_interval = 50,
  verbose_console = FALSE
)

# Create the GIF with partisan coloring
mosaic_gif(
  shapefile_path = SHAPEFILE,
  type = "partisan",
  output_path = "example2_partisan.gif",
  fps = 10,
  border_outline = TRUE,
  district_outline = TRUE,
  county_outline = TRUE,
  precinct_outline = FALSE,
  width = 700,
  height = 400
)

cat("\n✓ Example 2 GIF saved to: example2_partisan.gif\n\n")

cat("========================================\n")
cat("Both GIFs created successfully!\n")
cat("========================================\n")