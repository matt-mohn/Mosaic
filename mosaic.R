# ============================================
# MOSAIC LOADER
# ============================================


# Usage:
#   setwd("path/to/mosaic")
#   source("mosaic.R")
#   load_mosaic()

# ============================================
# ENVIRONMENT SETUP
# ============================================

.mosaic_env <- new.env()

initialize_mosaic <- function() {
  # Find where mosaic.R lives
  mosaic_path <- if (sys.nframe() > 0) {
    tryCatch({
      dirname(sys.frame(1)$ofile)
    }, error = function(e) {
      getwd()
    })
  } else {
    getwd()
  }
  
  .mosaic_env$mosaic_root <- mosaic_path
  
  # Create required directories if they don't exist
  dirs_to_create <- c("output", "gifs")
  for (dir in dirs_to_create) {
    full_path <- file.path(mosaic_path, dir)
    if (!dir.exists(full_path)) {
      dir.create(full_path, recursive = TRUE, showWarnings = FALSE)
      cat(sprintf("✓ Created %s/\n", dir))
    }
  }
  
  cat(sprintf("Mosaic root: %s\n", mosaic_path))
}

# Helper function to get paths relative to mosaic root
mosaic_path <- function(...) {
  file.path(.mosaic_env$mosaic_root, ...)
}

# ============================================
# MAIN LOADER
# ============================================

load_mosaic <- function() {
  cat("\n")
  cat("========================================\n")
  cat("LOADING MOSAIC\n")
  cat("========================================\n")
  
  initialize_mosaic()
  
  # Load required packages
  cat("\nLoading required packages:\n")
  required_packages <- c("sf", "tidyverse", "igraph", "readr", 
                         "ggplot2", "dplyr", "shadowtext", "scales")
  
  for (pkg in required_packages) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    cat(sprintf("  ✓ %s\n", pkg))
  }
  
  files <- c(
    "functions/config.R",
    "functions/timing_functions.R",
    "functions/scoring_functions.R",
    "functions/tree_functions.R",
    "functions/partition_functions.R",
    "functions/recom_functions.R",
    "functions/graph_functions.R",
    "functions/output_functions.R",
    "functions/chain_runner.R",
    "functions/graphics.R"
  )
  
  cat("\nLoading components:\n")
  for (f in files) {
    full_path <- mosaic_path(f)
    if (!file.exists(full_path)) {
      stop(sprintf("Required file not found: %s\nMake sure you're in the mosaic directory.", f))
    }
    source(full_path)
    cat(sprintf("  ✓ %s\n", f))
  }
  
  cat("\n✓ Mosaic loaded successfully!\n")
  cat("========================================\n\n")
}

# ============================================
# DEPENDENCY CHECK
# ============================================

check_mosaic_dependencies <- function() {
  cat("\n")
  cat("========================================\n")
  cat("CHECKING DEPENDENCIES\n")
  cat("========================================\n")
  
  required_packages <- c(
    "sf", "tidyverse", "igraph", "readr", 
    "ggplot2", "dplyr", "shadowtext", "scales"
  )
  
  missing <- c()
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing <- c(missing, pkg)
      cat(sprintf("  ✗ %s (missing)\n", pkg))
    } else {
      cat(sprintf("  ✓ %s\n", pkg))
    }
  }
  
  cat("========================================\n")
  
  if (length(missing) > 0) {
    cat("\nMissing packages detected!\n")
    cat("Install with:\n")
    cat(sprintf('  install.packages(c("%s"))\n\n', paste(missing, collapse = '", "')))
    return(FALSE)
  } else {
    cat("\n✓ All dependencies installed!\n\n")
    return(TRUE)
  }
}