# ============================================
# INPUT VALIDATION
# ============================================

# Normalize column names (handle lowercase variants)
normalize_column_names <- function(shp) {
  col_map <- c(
    "pop" = "POP", "dem" = "DEM", "rep" = "REP", "cty" = "CTY",
    "geoid" = "GEOID", "geoid20" = "GEOID20", "precinct" = "PRECINCT", "pct" = "PCT"
  )
  current_names <- names(shp)
  lower_names <- tolower(current_names)
  renamed <- c()

  for (i in seq_along(current_names)) {
    if (lower_names[i] %in% names(col_map) && current_names[i] != col_map[lower_names[i]]) {
      renamed <- c(renamed, sprintf("%s -> %s", current_names[i], col_map[lower_names[i]]))
      names(shp)[i] <- col_map[lower_names[i]]
    }
  }

  if (length(renamed) > 0) {
    cat("\n=== COLUMN NAMES NORMALIZED ===\n")
    cat(sprintf("  %s\n", renamed))
    cat("===============================\n\n")
  }

  return(shp)
}

validate_shapefile_data <- function(shp, num_districts, use_partisan = FALSE, county_bias = 1.0) {
  warnings <- c()
  errors <- c()

  n <- nrow(shp)

  # Check for empty shapefile
  if (n == 0) {
    errors <- c(errors, "Shapefile contains 0 precincts")
  }

  # Check num_districts vs num_precincts
  if (num_districts > n) {
    errors <- c(errors, sprintf("num_districts (%d) > num_precincts (%d)", num_districts, n))
  }

  # Check for duplicate column names
  col_names <- names(shp)
  if (any(duplicated(col_names))) {
    dup_cols <- unique(col_names[duplicated(col_names)])
    errors <- c(errors, sprintf("Duplicate column names detected: %s", paste(dup_cols, collapse = ", ")))
  }

  # Check county data if county_bias is being used
  if (county_bias != 1.0) {
    if (!"CTY" %in% names(shp)) {
      warnings <- c(warnings, "county_bias is set but CTY column is missing - county_bias will be ignored")
    } else {
      n_counties <- length(unique(shp$CTY))
      if (n_counties == 1) {
        warnings <- c(warnings, sprintf("county_bias is set but all precincts are in the same county (%s) - county_bias has no effect", shp$CTY[1]))
      }
    }
  }

  # Check for GEOID column and duplicates
  geoid_col <- NULL
  for (col in c("GEOID20", "GEOID", "PRECINCT", "PCT")) {
    if (col %in% names(shp)) {
      geoid_col <- col
      break
    }
  }
  if (!is.null(geoid_col)) {
    geoids <- shp[[geoid_col]]
    n_unique <- length(unique(geoids))
    if (n_unique < n) {
      warnings <- c(warnings, sprintf("Duplicate %s values detected: %d unique out of %d rows",
                                       geoid_col, n_unique, n))
    }
  }

  # Check POP column
  if (!"POP" %in% names(shp)) {
    errors <- c(errors, "Missing required column: POP")
  } else {
    pop <- shp$POP

    # Try to parse if character
    if (is.character(pop)) {
      parsed <- suppressWarnings(as.numeric(pop))
      if (all(is.na(parsed))) {
        errors <- c(errors, "POP column contains unparseable text values")
      } else if (any(is.na(parsed) & !is.na(pop))) {
        errors <- c(errors, "POP column contains some unparseable text values")
      } else {
        warnings <- c(warnings, "POP column was character type - converted to numeric")
        shp$POP <- parsed
        pop <- parsed
      }
    }

    # Check for NA values
    if (all(is.na(pop))) {
      errors <- c(errors, "POP column is entirely NA")
    } else if (any(is.na(pop))) {
      warnings <- c(warnings, sprintf("POP column contains %d NA values", sum(is.na(pop))))
    }

    # Check for negative values
    if (!all(is.na(pop)) && any(pop < 0, na.rm = TRUE)) {
      errors <- c(errors, sprintf("POP column contains %d negative values", sum(pop < 0, na.rm = TRUE)))
    }

    # Check for all zeros
    if (!all(is.na(pop)) && all(pop == 0, na.rm = TRUE)) {
      errors <- c(errors, "POP column is entirely zero")
    }
  }

  # Check DEM/REP if partisan optimization is used
  if (use_partisan) {
    for (col in c("DEM", "REP")) {
      if (!col %in% names(shp)) {
        errors <- c(errors, sprintf("Missing required column for partisan optimization: %s", col))
      } else {
        vals <- shp[[col]]

        # Try to parse if character
        if (is.character(vals)) {
          parsed <- suppressWarnings(as.numeric(vals))
          if (all(is.na(parsed))) {
            errors <- c(errors, sprintf("%s column contains unparseable text values", col))
          } else if (any(is.na(parsed) & !is.na(vals))) {
            errors <- c(errors, sprintf("%s column contains some unparseable text values", col))
          } else {
            warnings <- c(warnings, sprintf("%s column was character type - converted to numeric", col))
            shp[[col]] <- parsed
            vals <- parsed
          }
        }

        # Check for all NA
        if (all(is.na(vals))) {
          errors <- c(errors, sprintf("%s column is entirely NA", col))
        }
      }
    }
  }

  # Print warnings
  if (length(warnings) > 0) {
    cat("\n=== DATA WARNINGS ===\n")
    for (w in warnings) {
      cat(sprintf("  * %s\n", w))
    }
    cat("=====================\n\n")
  }

  # Stop if errors
  if (length(errors) > 0) {
    error_msg <- paste(c("\n=== DATA VALIDATION ERRORS ===",
                         paste("  *", errors),
                         "===============================\n"),
                       collapse = "\n")
    stop(error_msg)
  }

  return(shp)
}

# ============================================
# BUNKING LISTS HELPER FUNCTION
# ============================================

create_bunking_lists <- function(anti_bunking = list(), pro_bunking = list()) {
  structure <- list(
    anti_bunking_raw = anti_bunking,
    pro_bunking_raw = pro_bunking
  )
  class(structure) <- "bunking_lists_raw"
  return(structure)
}

# ============================================
# MAIN CHAIN RUNNER
# ============================================

run_chain <- function(shapefile_path, 
                      num_districts = 5, 
                      num_steps = 1000,
                      pdev_tolerance = 0.05,
                      seed = NULL,
                      assignment = NULL,
                      use_optimization = TRUE,
                      initial_temp_factor = 0.2,
                      temp_mode = "PROPORTIONAL",
                      temp_nominal = NULL,
                      cooling_mode = "GUIDED",
                      cooling_guided_params = list(guidepoint_iteration_p = 0.9, guidepoint_temperature = 1),
                      cooling_rate = 0.999,
                      weight_cut_edges = 1,
                      weight_county_splits = NA,
                      weight_mean_median = NA,
                      weight_efficiency_gap = NA,
                      weight_dem_seats = NA,
                      weight_competitiveness = NA,
                      exponent_cut_edges = 1,
                      exponent_county_splits = 1,
                      exponent_mean_median = 2,
                      exponent_efficiency_gap = 2,
                      exponent_dem_seats = 2,
                      exponent_competitiveness = 1,
                      target_mean_median = 0,
                      target_efficiency_gap = 0,
                      target_dem_seats = NA,
                      use_robust_efficiency_gap = TRUE,
                      election_win_prob_at_55 = 0.9,
                      competitiveness_maximize = TRUE,
                      county_bias = 1.0,
                      bunking_lists = NULL,
                      output_dir = NULL,
                      save_final_assignment = TRUE,
                      capture_full_assignments = FALSE,
                      snapshot_assignments = TRUE,
                      snapshot_interval = 50,
                      verbose_console = TRUE,
                      verbose_initialization = FALSE,
                      timing_analysis = FALSE,
                      final_analysis = TRUE) {
  # ============================================
  # INITIALIZATION & DATA PREPARATION
  # ============================================
  
  # Set default output_dir relative to mosaic root if not provided
  if (is.null(output_dir)) {
    if (exists("mosaic_path", mode = "function")) {
      output_dir <- mosaic_path("output")
    } else {
      output_dir <- "output"
    }
  }
  
  # Clear scoring cache from any previous runs
  clear_scoring_cache()
  
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Load and prepare shapefile data
  cat("Loading shapefile:", shapefile_path, "\n")
  data <- load_or_build_graph(shapefile_path)
  shp <- data$shp
  graph <- data$graph
  cat("Loaded", nrow(shp), "precincts\n")

  # Normalize column names (handle lowercase variants like 'pop' -> 'POP')
  shp <- normalize_column_names(shp)

  # Determine if partisan optimization is being used
  use_partisan <- !is.na(weight_mean_median) || !is.na(weight_efficiency_gap) ||
                  !is.na(weight_dem_seats) || !is.na(weight_competitiveness)

  # Validate input data
  shp <- validate_shapefile_data(shp, num_districts, use_partisan, county_bias)

  # Extract population data
  node_pops <- shp$POP
  total_pop <- sum(node_pops)
  ideal_pop <- total_pop / num_districts
  
  cat("Total population:", format(total_pop, big.mark=","), "\n")
  cat("Ideal per district:", format(round(ideal_pop), big.mark=","), "\n")
  cat("Population tolerance:", sprintf("%.1f%%", pdev_tolerance * 100), "\n\n")
  
  # Extract counties if available
  counties <- NULL
  if ("CTY" %in% names(shp)) {
    counties <- shp$CTY
    cat("County data found:", length(unique(counties)), "unique counties\n")
  }
  
  # Create initial partition if not provided
  if (is.null(assignment)) {
    cat("\nCreating initial partition...\n")
    assignment <- create_initial_partition(graph, node_pops, num_districts, pdev_tolerance, 
                                           ifelse(is.null(seed), sample.int(1e6, 1), seed),
                                           counties = counties, county_bias = county_bias,
                                           verbose = verbose_initialization)
    
    if (verbose_initialization) {
      district_pops <- get_district_populations(node_pops, assignment)
      cat("\nInitial district populations:\n")
      for (i in 1:num_districts) {
        dev <- (district_pops[i] - ideal_pop) / ideal_pop * 100
        cat(sprintf("  District %2d: %s (deviation: %+.2f%%)\n", 
                    i, format(round(district_pops[i]), big.mark=","), dev))
      }
      cat("\n")
    }
  }
  
  # Process bunking lists if provided
  if (!is.null(bunking_lists)) {
    # If user passed raw format, process it now that we have shp loaded
    if (inherits(bunking_lists, "bunking_lists_raw")) {
      processed <- list(anti_bunking = list(), pro_bunking = list())
      
      # Detect ID column
      id_col <- NULL
      for (col in c("GEOID20", "GEOID", "PRECINCT", "PCT")) {
        if (col %in% names(shp)) { id_col <- col; break }
      }
      if (is.null(id_col)) stop("Shapefile missing ID column (need GEOID20, GEOID, PRECINCT, or PCT)")
      shp_ids <- as.character(shp[[id_col]])

      # Process anti-bunking
      for (i in seq_along(bunking_lists$anti_bunking_raw)) {
        vec <- bunking_lists$anti_bunking_raw[[i]]
        weight <- as.numeric(vec[1])
        exponent <- as.numeric(vec[2])
        geoids <- as.character(vec[3:length(vec)])

        precinct_indices <- integer(length(geoids))
        for (j in seq_along(geoids)) {
          idx <- which(shp_ids == geoids[j])
          if (length(idx) == 0) stop(paste("GEOID not found:", geoids[j]))
          precinct_indices[j] <- idx[1]
        }
        
        processed$anti_bunking[[i]] <- list(
          weight = weight, exponent = exponent, geoids = geoids,
          precinct_indices = precinct_indices, name = paste("Anti-Bunking List", i)
        )
        
        cat(sprintf("Anti-Bunking List %d: weight=%.1f, exponent=%.1f, %d precincts\n", 
                    i, weight, exponent, length(precinct_indices)))
      }
      
      # Process pro-bunking
      for (i in seq_along(bunking_lists$pro_bunking_raw)) {
        vec <- bunking_lists$pro_bunking_raw[[i]]
        weight <- as.numeric(vec[1])
        exponent <- as.numeric(vec[2])
        geoids <- as.character(vec[3:length(vec)])

        precinct_indices <- integer(length(geoids))
        for (j in seq_along(geoids)) {
          idx <- which(shp_ids == geoids[j])
          if (length(idx) == 0) stop(paste("GEOID not found:", geoids[j]))
          precinct_indices[j] <- idx[1]
        }
        
        processed$pro_bunking[[i]] <- list(
          weight = weight, exponent = exponent, geoids = geoids,
          precinct_indices = precinct_indices, name = paste("Pro-Bunking List", i)
        )
        
        cat(sprintf("Pro-Bunking List %d: weight=%.1f, exponent=%.1f, %d precincts\n", 
                    i, weight, exponent, length(precinct_indices)))
      }
      
      bunking_lists <- processed
    }
  }
  
  # Create scoring configuration object
  config <- create_scoring_config(
    weight_cut_edges = weight_cut_edges,
    weight_county_splits = weight_county_splits,
    weight_mean_median = weight_mean_median,
    weight_efficiency_gap = weight_efficiency_gap,
    weight_dem_seats = weight_dem_seats,
    weight_competitiveness = weight_competitiveness,
    exponent_cut_edges = exponent_cut_edges,
    exponent_county_splits = exponent_county_splits,
    exponent_mean_median = exponent_mean_median,
    exponent_efficiency_gap = exponent_efficiency_gap,
    exponent_dem_seats = exponent_dem_seats,
    exponent_competitiveness = exponent_competitiveness,
    target_mean_median = target_mean_median,
    target_efficiency_gap = target_efficiency_gap,
    target_dem_seats = target_dem_seats,
    use_robust_efficiency_gap = use_robust_efficiency_gap,
    election_win_prob_at_55 = election_win_prob_at_55,
    competitiveness_maximize = competitiveness_maximize,
    bunking_lists = bunking_lists
  )
  
  # Determine which tracking modules are active
  tracking <- determine_active_tracking(config)
  
  # Validate that required data exists for active tracking
  validate_tracking(tracking, shp, counties, config)
  
  # ============================================
  # MAIN CHAIN EXECUTION
  # ============================================
  
  n <- vcount(graph)
  assignments_all <- matrix(0, nrow = n, ncol = num_steps + 1)
  assignments_all[, 1] <- assignment
  
  cut_edges_all <- integer(num_steps + 1)
  cut_edges_all[1] <- nrow(get_cut_edges(graph, assignment))
  
  scores_all <- numeric(num_steps + 1)
  county_splits_all <- numeric(num_steps + 1)
  
  t0 <- start_timer()
  timers <- if (timing_analysis) init_timing() else NULL
  result <- calculate_map_metrics(graph, assignment, node_pops, config, shp, counties, ideal_pop, timers)
  current_metrics <- result$metrics
  timers <- result$timers
  timers <- add_time(timers, "score_calculation", t0)
  
  current_score <- calculate_map_score(current_metrics, config)
  scores_all[1] <- current_score
  county_splits_all[1] <- current_metrics$county_splits
  
  mean_dem_share_all <- numeric(num_steps + 1)
  median_dem_share_all <- numeric(num_steps + 1)
  competitiveness_all <- numeric(num_steps + 1)
  dem_share_by_district <- NULL
  if (tracking$partisan) {
    mean_dem_share_all[1] <- current_metrics$mean_dem_share
    median_dem_share_all[1] <- current_metrics$median_dem_share
    competitiveness_all[1] <- current_metrics$competitiveness
    dem_share_by_district <- matrix(0, nrow = num_steps + 1, ncol = num_districts)
    dem_share_by_district[1, ] <- current_metrics$dem_share
  }
  
  annealing_state <- NULL
  recent_accepts <- integer(0)
  
  if (use_optimization) {
    annealing_state <- init_annealing(current_score, initial_temp_factor, cooling_rate,
                                      temp_mode, temp_nominal, 
                                      cooling_mode, cooling_guided_params, num_steps)
    cat(sprintf("Optimization enabled: Initial score=%.1f, Temp=%.1f\n", 
                current_score, annealing_state$temperature))
  }
  
  if (!is.null(counties) && county_bias != 1.0) {
    cat(sprintf("County-aware mode: bias factor = %.1f\n", county_bias))
  }
  
  if(!verbose_console) {
    print_interval <- 500
  } else {
    print_interval <- 100
  }
  
  print_header_interval <- 500  # Print header every 500 iterations
  
  cat("\nRunning", num_steps, "ReCom steps...\n")
  start_time <- Sys.time()
  
  for (step in 1:num_steps) {
    should_print <- (step %% print_interval == 0 || step == num_steps)
    should_print_header <- (step %% print_header_interval == 0 || step == print_interval)
    
    result <- recom_step(graph, assignment, node_pops, ideal_pop, pdev_tolerance, 
                         verbose = FALSE, timers = timers, counties = counties, 
                         county_bias = county_bias)
    
    proposed_assignment <- result$assignment
    timers <- result$timers
    
    t0 <- start_timer()
    result <- calculate_map_metrics(graph, proposed_assignment, node_pops, config, shp, counties, ideal_pop, timers)
    proposed_metrics <- result$metrics
    timers <- result$timers
    timers <- add_time(timers, "score_calculation", t0)
    
    proposed_score <- calculate_map_score(proposed_metrics, config)
    
    accept <- TRUE
    if (use_optimization) {
      accept <- accept_proposal(current_score, proposed_score, annealing_state)
      
      recent_accepts <- c(recent_accepts, as.integer(accept))
      if (length(recent_accepts) > 50) {
        recent_accepts <- recent_accepts[-1]
      }
      
      if (accept) {
        annealing_state$accepts <- annealing_state$accepts + 1
      } else {
        annealing_state$rejects <- annealing_state$rejects + 1
      }
      
      annealing_state <- cool_temperature(annealing_state)
    }
    
    if (accept) {
      assignment <- proposed_assignment
      current_score <- proposed_score
      current_metrics <- proposed_metrics
    }
    
    assignments_all[, step + 1] <- assignment
    cut_edges_all[step + 1] <- current_metrics$cut_edges
    scores_all[step + 1] <- current_score
    county_splits_all[step + 1] <- if (!is.na(current_metrics$county_splits)) current_metrics$county_splits else NA
    
    if (tracking$partisan) {
      mean_dem_share_all[step + 1] <- current_metrics$mean_dem_share
      median_dem_share_all[step + 1] <- current_metrics$median_dem_share
      competitiveness_all[step + 1] <- if (!is.na(current_metrics$competitiveness)) current_metrics$competitiveness else NA
      dem_share_by_district[step + 1, ] <- current_metrics$dem_share
    }
    
    if (should_print) {
      if (!verbose_console) {
        # Simple iteration counter
        cat(sprintf("\rIter: %d/%d \n", step, num_steps))
        
      } else if (use_optimization) {
        # Print header every print_header_interval iterations
        if (should_print_header) {
          cat("\n")
          # Dynamic header based on EG mode
          eg_header <- if (!is.null(current_metrics$is_robust_eg) && current_metrics$is_robust_eg) {
            "EG ADJ"
          } else {
            "EF GAP"
          }
          cat(sprintf("%-9s %-9s %-8s %-6s %-6s %-8s %-9s %-9s %-5s %-5s %-6s %-8s\n",
                      "ITER", "SCORE", "TEMP", "CUTS", "CNTY", "BUNK", "MM DIFF", eg_header, "D", "R", "C", "% ACC"))
        }
        
        # Calculate metrics for display
        n_recent <- sum(recent_accepts)
        accept_pct <- 100 * n_recent / length(recent_accepts)
        
        mm_val <- current_metrics$mean_median_diff
        if (!is.na(mm_val)) {
          mm_pct <- 100 * mm_val
          mm_str <- sprintf("%+.1f", mm_pct)
        } else {
          mm_str <- "--"
        }
        
        eg_val <- current_metrics$efficiency_gap
        if (!is.na(eg_val)) {
          eg_pct <- 100 * eg_val
          eg_str <- sprintf("%+.1f", eg_pct)
        } else {
          eg_str <- "--"
        }
        
        dem_seats <- current_metrics$expected_dem_seats
        rep_seats <- if (!is.na(dem_seats)) num_districts - dem_seats else NA
        
        # Competitiveness
        comp_val <- current_metrics$competitiveness
        if (!is.na(comp_val)) {
          comp_str <- sprintf("%.1f", comp_val)
        } else {
          comp_str <- "--"
        }
        
        # Calculate unweighted bunking score (sum of all raw metrics)
        bunk_score <- 0
        if (!is.null(config$bunking_lists)) {
          bunking_lists <- config$bunking_lists
          
          # Sum anti-bunking raw metrics
          if (!is.null(bunking_lists$anti_bunking) && length(bunking_lists$anti_bunking) > 0) {
            for (i in seq_along(bunking_lists$anti_bunking)) {
              prefix <- paste0("anti_bunking_", i)
              raw_metric <- current_metrics[[paste0(prefix, "_raw")]]
              if (!is.null(raw_metric)) {
                bunk_score <- bunk_score + raw_metric
              }
            }
          }
          
          # Sum pro-bunking raw metrics
          if (!is.null(bunking_lists$pro_bunking) && length(bunking_lists$pro_bunking) > 0) {
            for (i in seq_along(bunking_lists$pro_bunking)) {
              prefix <- paste0("pro_bunking_", i)
              raw_metric <- current_metrics[[paste0(prefix, "_raw")]]
              if (!is.null(raw_metric)) {
                bunk_score <- bunk_score + raw_metric
              }
            }
          }
        }
        
        bunk_str <- if (bunk_score > 0) sprintf("%.2f", bunk_score) else "-"
        
        # Handle NA values in display
        cuts_str <- if(!is.na(current_metrics$cut_edges)) sprintf("%d", current_metrics$cut_edges) else "--"
        cnty_str <- if(!is.na(current_metrics$county_splits)) sprintf("%.2f", current_metrics$county_splits) else "--"
        d_str <- if(!is.na(dem_seats)) sprintf("%.1f", dem_seats) else "--"
        r_str <- if(!is.na(rep_seats)) sprintf("%.1f", rep_seats) else "--"
        
        cat(sprintf("%-9d %-9.0f %-8.1f %-6s %-6s %-8s %-9s %-9s %-5s %-5s %-6s %-8.1f\n",
                    step, current_score, annealing_state$temperature, 
                    cuts_str, cnty_str, 
                    bunk_str, mm_str, eg_str, d_str, r_str, comp_str, accept_pct))
      } else {
        if (verbose_console) {
          cat(sprintf("Step %d/%d (cuts:%d score:%.1f)\n", 
                      step, num_steps, current_metrics$cut_edges, current_score))
        }
      }
    }
    
    if (!check_all_contiguous(graph, assignment)) {
      cat("\nERROR: Contiguity violated!\n")
      stop("Contiguity violated!")
    }
  }
  
  if (!verbose_console) {
    cat("\n")  # Newline after progress counter
  }
  
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  time_per_step <- elapsed / num_steps
  iters_per_sec <- num_steps / elapsed
  if (timing_analysis) {
    timers$total_steps <- elapsed
  }
  
  cat(sprintf("\nCompleted in %.1fs (%.1fms/step, %.1f iter/s)\n", 
              elapsed, time_per_step * 1000, iters_per_sec))
  
  if (use_optimization) {
    cat(sprintf("Optimization stats: %d accepts, %d rejects (%.1f%% accept rate)\n",
                annealing_state$accepts, annealing_state$rejects,
                100 * annealing_state$accepts / (annealing_state$accepts + annealing_state$rejects)))
    cat(sprintf("Score: %.1f to %.1f (Δ=%.1f)\n", 
                scores_all[1], current_score, current_score - scores_all[1]))
  }
  
  print_timing_report(timers, num_steps)
  
  if (final_analysis) {
    print_final_summary(current_metrics, tracking)
  }
  
  # ============================================
  # SAVE RESULTS
  # ============================================
  
  timestamp <- save_results(
    assignments_all, cut_edges_all, scores_all, county_splits_all,
    dem_share_by_district, shp, output_dir, num_steps,
    capture_full_assignments, save_final_assignment,
    snapshot_assignments, snapshot_interval
  )
  
  cat("\n")
  cat("========================================\n")
  cat("RESULTS SAVED\n")
  cat("========================================\n")
  if (save_final_assignment) {
    cat(sprintf("Final assignment: %s/final_assignment_%s.csv\n", output_dir, timestamp))
  }
  cat(sprintf("Metrics: %s/metrics_%s.csv\n", output_dir, timestamp))
  if (snapshot_assignments) {
    cat(sprintf("Snapshots: %s/snapshot_assignments_%s.csv\n", output_dir, timestamp))
  }
  cat("========================================\n\n")
  
  list(assignments = assignments_all, final = assignment, time_per_step = time_per_step, 
       cut_edges = cut_edges_all, scores = scores_all, county_splits = county_splits_all,
       mean_dem_share = mean_dem_share_all, median_dem_share = median_dem_share_all,
       competitiveness = competitiveness_all,
       dem_share_by_district = dem_share_by_district,
       annealing_state = annealing_state, final_metrics = current_metrics,
       timestamp = timestamp, config = config)
}