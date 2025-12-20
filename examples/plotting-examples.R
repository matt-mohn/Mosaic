# ============================================
# PLOTTING EXAMPLES
# ============================================
# Four examples demonstrating Mosaic's plotting capabilities
# See docs/guides/plotting.md for full tutorial

source("mosaic.R")
load_mosaic()

SHAPEFILE <- "shapefiles/North_Carolina_Simplified.shp"

# ============================================
# RUN CHAIN (shared for all examples)
# ============================================

cat("\n=== Running Chain for Plotting Examples ===\n\n")

results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  seed = 123456,
  county_bias = 5,
  weight_county_splits = 10,
  target_mean_median = 0,
  weight_mean_median = 150,
  weight_competitiveness = 10,
  num_steps = 3000,
  verbose_console = FALSE
)

# ============================================
# EXAMPLE 1: Simple Map
# ============================================

cat("\n=== Example 1: Simple Map ===\n\n")

mosaic_plot(
  shapefile_path = SHAPEFILE,
  type = "simple"
)

# ============================================
# EXAMPLE 2: Customizing Visual Elements
# ============================================

cat("\n=== Example 2: Customizing Visual Elements ===\n\n")

mosaic_plot(
  shapefile_path = SHAPEFILE,
  type = "simple",
  title = "North Carolina Competitive Map",
  subtitle = "14 Congressional Districts",
  border_outline = TRUE,
  district_outline = TRUE,
  county_outline = TRUE,
  number_labels = TRUE
)

# ============================================
# EXAMPLE 3: Partisan Coloring
# ============================================

cat("\n=== Example 3: Partisan Coloring ===\n\n")

mosaic_plot(
  shapefile_path = SHAPEFILE,
  type = "partisan",
  title = "North Carolina Partisan Lean",
  subtitle = "Colored by Democratic vote share",
  border_outline = TRUE,
  district_outline = TRUE,
  county_outline = TRUE,
  number_labels = TRUE
)

# ============================================
# EXAMPLE 4: Partisan Analysis Graph
# ============================================

cat("\n=== Example 4: Partisan Analysis Graph ===\n\n")

mosaic_partisan_plot(
  title = "District Partisan Lean",
  subtitle = "Ranked by Democratic vote share"
)

cat("\n========================================\n")
cat("All examples completed!\n")
cat("========================================\n")