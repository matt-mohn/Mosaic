
cleanup_globals <- function() {
  # Remove bunking variables
  rm(list = ls(pattern = "^ANTI_BUNKING_", envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = "^PRO_BUNKING_", envir = .GlobalEnv), envir = .GlobalEnv)
  
  # Remove bunking lists object
  if (exists("BUNKING_LISTS", envir = .GlobalEnv)) {
    rm(BUNKING_LISTS, envir = .GlobalEnv)
  }
  
  # Remove weight variables
  weight_vars <- c("WEIGHT_CUT_EDGES", "WEIGHT_MEAN_MEDIAN", "WEIGHT_EFFICIENCY_GAP", 
                   "WEIGHT_DEM_SEATS", "WEIGHT_COUNTY_SPLITS", "WEIGHT_COMPETITIVENESS")
  for (var in weight_vars) {
    if (exists(var, envir = .GlobalEnv)) {
      rm(list = var, envir = .GlobalEnv)
    }
  }
  
  # Remove exponent variables
  exponent_vars <- c("EXPONENT_CUT_EDGES", "EXPONENT_COUNTY_SPLITS", "EXPONENT_MEAN_MEDIAN",
                     "EXPONENT_EFFICIENCY_GAP", "EXPONENT_DEM_SEATS", "EXPONENT_COMPETITIVENESS")
  for (var in exponent_vars) {
    if (exists(var, envir = .GlobalEnv)) {
      rm(list = var, envir = .GlobalEnv)
    }
  }
  
  # Remove target variables
  target_vars <- c("TARGET_MEAN_MEDIAN", "TARGET_EFFICIENCY_GAP", "TARGET_DEM_SEATS", 
                   "ELECTION_WIN_PROB_AT_55", "USE_ROBUST_EFFICIENCY_GAP", "COMPETITIVENESS_MAXIMIZE")
  for (var in target_vars) {
    if (exists(var, envir = .GlobalEnv)) {
      rm(list = var, envir = .GlobalEnv)
    }
  }
  
  # Remove county lookup cache
  cache_vars <- c(".county_lookup_cache", ".county_pops_cache", ".county_allowances_cache")
  for (var in cache_vars) {
    if (exists(var, envir = .GlobalEnv)) {
      rm(list = var, envir = .GlobalEnv)
    }
  }
  
  invisible(NULL)
}