# ============================================
# NORTH CAROLINA EXAMPLES
# ============================================
# Seven examples demonstrating Mosaic's capabilities
# See docs/tutorials/nc-examples.md for full tutorial

source("mosaic.R")
load_mosaic()

SHAPEFILE <- "shapefiles/North_Carolina_Simplified.shp"

# ============================================
# EXAMPLE 1: Simple Redistricting
# ============================================

results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  seed = 123456
)

# View the map
mosaic_plot(
  shapefile_path = SHAPEFILE,
  type = "simple",
  title = "Example 1: Simple Redistricting",
  border_outline = TRUE,
  district_outline = TRUE,
  county_outline = TRUE,
  number_labels = TRUE
)

# ============================================
# EXAMPLE 2: Preserving Counties
# ============================================

results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  seed = 12345,
  county_bias = 10,
  weight_county_splits = 15
)

mosaic_plot(
  shapefile_path = SHAPEFILE,
  type = "simple",
  title = "Example 2: County Preservation",
  border_outline = TRUE,
  district_outline = TRUE,
  county_outline = TRUE,
  number_labels = TRUE
)

# ============================================
# EXAMPLE 3: Republican Gerrymander
# ============================================

results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  seed = 123456,
  county_bias = 2,
  weight_county_splits = 5,
  target_efficiency_gap = 0.5,
  weight_efficiency_gap = 100,
  target_mean_median = 0.15,
  weight_mean_median = 50,
  num_steps = 3000,
  target_dem_seats = 2,
  weight_dem_seats = 250
)

mosaic_plot(
  shapefile_path = SHAPEFILE,
  type = "partisan",
  title = "Example 3: Republican Gerrymander",
  border_outline = TRUE,
  district_outline = TRUE,
  county_outline = TRUE,
  number_labels = TRUE
)

mosaic_partisan_plot(
  title = "Example 3: Republican Gerrymander"
)

# ============================================
# EXAMPLE 4: Democratic Gerrymander
# ============================================

results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  seed = 123456,
  county_bias = 2,
  weight_county_splits = 5,
  target_efficiency_gap = -0.3,
  weight_efficiency_gap = 100,
  target_mean_median = -0.1,
  weight_mean_median = 50,
  num_steps = 3000,
  target_dem_seats = 11,
  weight_dem_seats = 250,
  verbose_console = FALSE
)

mosaic_plot(
  shapefile_path = SHAPEFILE,
  type = "partisan",
  title = "Example 4: Democratic Gerrymander",
  border_outline = TRUE,
  district_outline = TRUE,
  county_outline = TRUE,
  number_labels = TRUE
)

mosaic_partisan_plot(
  title = "Example 4: Democratic Gerrymander"
)

# ============================================
# EXAMPLE 5: Fair State Senate Map
# ============================================

results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 50,
  seed = 123456,
  county_bias = 5,
  weight_county_splits = 10,
  target_mean_median = 0,
  weight_mean_median = 150,
  target_efficiency_gap = 0,
  weight_efficiency_gap = 100,
  weight_competitiveness = 5,
  num_steps = 5000,
  verbose_console = FALSE
)

mosaic_plot(
  shapefile_path = SHAPEFILE,
  type = "partisan",
  title = "Example 5: Fair State Senate",
  border_outline = TRUE,
  district_outline = TRUE,
  county_outline = TRUE,
  number_labels = FALSE
)

mosaic_partisan_plot(
  title = "Example 5: Fair NC State Senate"
)

# ============================================
# EXAMPLE 6: Anti-Bunking (Incumbents)
# ============================================

NC_incumbent_list <- c(
  1000, 1,
  "3707900BEAR", "37183001-27", "3714701504A", "3713500000H",
  "37011000002", "37059000013", "37129000W25", "37179000014",
  "37125000SSP", "37035000013", "370890000FR", "37119000011",
  "37183001-36", "3704500KM-N"
)

results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  seed = 123456,
  county_bias = 3,
  weight_county_splits = 15,
  target_mean_median = 0,
  weight_mean_median = 500,
  target_efficiency_gap = 0,
  weight_efficiency_gap = 100,
  num_steps = 2500,
  verbose_console = FALSE,
  bunking_lists = create_bunking_lists(
    anti_bunking = list(NC_incumbent_list)
  )
)

mosaic_plot(
  shapefile_path = SHAPEFILE,
  bunking_lists = create_bunking_lists(
    anti_bunking = list(NC_incumbent_list)
  ),
  type = "partisan",
  title = "Example 6: Fair Map Without Double-Bunking",
  subtitle = "Yellow diamonds: incumbent locations",
  district_outline = TRUE,
  number_labels = TRUE,
  county_outline = TRUE,
  border_outline = TRUE
)

mosaic_partisan_plot(
  title = "Example 6: Fair Map (Anti-Bunking)"
)

# ============================================
# EXAMPLE 7: Pro-Bunking (UNC System)
# ============================================

NC_university_list <- c(
  5000, 1,
  "37183004-05", "37063000047", "37063000007", "37135000UNC",
  "37081000G44", "37081000G64", "37067000403", "37067000705",
  "37189000012", "370210010.1", "37099000RIV", "37119000126",
  "37155000017", "3705100CC34", "37129000W31", "37147001506",
  "3713900EAST"
)

results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  seed = 123456,
  county_bias = 1.5,
  weight_county_splits = 15,
  target_mean_median = 0,
  weight_mean_median = 500,
  num_steps = 2500,
  verbose_console = FALSE,
  bunking_lists = create_bunking_lists(
    pro_bunking = list(NC_university_list)
  )
)

mosaic_plot(
  shapefile_path = SHAPEFILE,
  bunking_lists = create_bunking_lists(
    pro_bunking = list(NC_university_list)
  ),
  type = "partisan",
  title = "Example 7: Grouping UNC System Campuses",
  subtitle = "Blue diamonds: university locations",
  district_outline = TRUE,
  number_labels = TRUE,
  county_outline = TRUE,
  border_outline = TRUE
)