# ============================================
# BUNKING SYSTEM FUNCTIONS
# ============================================

calculate_anti_bunking_metric <- function(assignment, bunking_list) {
  # Get districts for bunked precincts
  districts <- assignment[bunking_list$precinct_indices]
  
  # Count unique districts
  unique_districts <- length(unique(districts))
  num_precincts <- length(bunking_list$precinct_indices)
  
  # Part 1: Base metric - how many districts are shared
  # (num_precincts - unique_districts) / (num_precincts - 1)
  # 0 = all in different districts (best)
  # 1 = all in same district (worst)
  if (num_precincts == 1) {
    base_metric <- 0  # Single precinct edge case
  } else {
    base_metric <- (num_precincts - unique_districts) / (num_precincts - 1)
  }
  
  # Part 2: Smoothness penalty - penalize larger clusters
  # Calculate fraction of precincts in each district
  smoothness_penalty <- 0
  if (unique_districts < num_precincts && unique_districts > 0) {
    district_counts <- table(districts)
    fractions <- as.numeric(district_counts) / num_precincts
    
    # For anti-bunking, we want precincts spread out
    # Penalize clusters: sum fractions of districts with MORE than 1 precinct
    min_fraction <- 1 / num_precincts
    smoothness_penalty <- sum(fractions[fractions > min_fraction + 0.001])
  }
  
  # Combine: base metric (0-1 range) + smoothness (0-1 range, scaled down)
  # Scale smoothness by 0.5 so base metric is primary, smoothness is tiebreaker
  raw_metric <- base_metric + (smoothness_penalty * 0.5)
  
  return(list(
    raw_metric = raw_metric,
    unique_districts = unique_districts,
    num_precincts = num_precincts
  ))
}

calculate_pro_bunking_metric <- function(assignment, bunking_list) {
  # Get districts for bunked precincts
  districts <- assignment[bunking_list$precinct_indices]
  
  # Count unique districts
  unique_districts <- length(unique(districts))
  num_precincts <- length(bunking_list$precinct_indices)
  
  # Part 1: Base metric - how many districts are used
  # (unique_districts - 1) / (num_precincts - 1)
  # 0 = all in same district (best)
  # 1 = all in different districts (worst)
  if (num_precincts == 1) {
    base_metric <- 0  # Single precinct edge case
  } else {
    base_metric <- (unique_districts - 1) / (num_precincts - 1)
  }
  
  # Part 2: Smoothness penalty - penalize fragmentation
  # Calculate fraction of precincts in each district
  smoothness_penalty <- 0
  if (unique_districts > 1) {
    district_counts <- table(districts)
    fractions <- as.numeric(district_counts) / num_precincts
    fractions_sorted <- sort(fractions, decreasing = TRUE)
    
    # For pro-bunking, we want precincts together
    # Penalize minority pieces (everything except largest cluster)
    # Sum everything except the largest piece
    smoothness_penalty <- sum(fractions_sorted[2:length(fractions_sorted)])
  }
  
  # Combine: base metric (0-1 range) + smoothness (0-1 range, scaled down)
  # Scale smoothness by 0.5 so base metric is primary, smoothness is tiebreaker
  raw_metric <- base_metric + (smoothness_penalty * 0.5)
  
  return(list(
    raw_metric = raw_metric,
    unique_districts = unique_districts,
    num_precincts = num_precincts
  ))
}

# ============================================
# MODULAR TRACKING SYSTEM
# ============================================

# Determine which tracking modules are active based on config
determine_active_tracking <- function(config) {
  tracking <- list(
    county = FALSE,
    bunking = FALSE,
    partisan = FALSE,
    victory = FALSE
  )
  
  # County tracking: active if weight is not NA
  tracking$county <- !is.na(config$weights$county_splits)
  
  # Bunking tracking: active if bunking_lists exists and has non-NA weights
  bunking_active <- FALSE
  if (!is.null(config$bunking_lists)) {
    bunking_lists <- config$bunking_lists
    
    # Check anti-bunking lists
    if (!is.null(bunking_lists$anti_bunking) && length(bunking_lists$anti_bunking) > 0) {
      for (i in seq_along(bunking_lists$anti_bunking)) {
        weight <- bunking_lists$anti_bunking[[i]]$weight
        if (!is.na(weight)) {
          bunking_active <- TRUE
          break
        }
      }
    }
    
    # Check pro-bunking lists if not already active
    if (!bunking_active && !is.null(bunking_lists$pro_bunking) && length(bunking_lists$pro_bunking) > 0) {
      for (i in seq_along(bunking_lists$pro_bunking)) {
        weight <- bunking_lists$pro_bunking[[i]]$weight
        if (!is.na(weight)) {
          bunking_active <- TRUE
          break
        }
      }
    }
  }
  
  tracking$bunking <- bunking_active
  
  # Partisan tracking: active if any partisan weights are not NA
  tracking$partisan <- !is.na(config$weights$mean_median) || 
    !is.na(config$weights$efficiency_gap) ||
    !is.na(config$weights$dem_seats) ||
    !is.na(config$weights$competitiveness)
  
  # Victory tracking: subset of partisan
  tracking$victory <- !is.na(config$weights$dem_seats) || 
    !is.na(config$weights$competitiveness)
  
  return(tracking)
}

# Validate required data exists for active tracking
validate_tracking <- function(tracking, shp, counties, config) {
  errors <- character(0)
  
  if (tracking$county) {
    if (is.null(counties)) {
      errors <- c(errors, "County tracking enabled (weight_county_splits not NA) but no 'CTY' column in shapefile")
    } else if (length(unique(counties)) < 2) {
      errors <- c(errors, "County tracking enabled but only 1 county in data")
    }
  }
  
  if (tracking$bunking) {
    if (is.null(config$bunking_lists)) {
      errors <- c(errors, "Bunking tracking enabled but bunking_lists not provided")
    } else if (length(config$bunking_lists$anti_bunking) + length(config$bunking_lists$pro_bunking) == 0) {
      errors <- c(errors, "Bunking tracking enabled but no valid bunking lists found")
    }
  }
  
  if (tracking$partisan) {
    if (is.null(shp)) {
      errors <- c(errors, "Partisan tracking enabled but no shapefile provided")
    } else {
      has_dem <- "DEM" %in% names(shp)
      has_rep <- "REP" %in% names(shp)
      
      if (!has_dem || !has_rep) {
        missing <- c(if(!has_dem) "DEM", if(!has_rep) "REP")
        errors <- c(errors, sprintf("Partisan tracking enabled but missing columns: %s", 
                                    paste(missing, collapse = ", ")))
      } else if (sum(shp$DEM, na.rm = TRUE) == 0 && sum(shp$REP, na.rm = TRUE) == 0) {
        errors <- c(errors, "Partisan tracking enabled but all votes are zero")
      }
    }
  }
  
  if (length(errors) > 0) {
    stop(paste("\n=== TRACKING VALIDATION ERRORS ===\n",
               paste("  *", errors, collapse = "\n"), 
               "\n=================================\n", sep = ""))
  }
  
  invisible(TRUE)
}

# Print tracking status
print_tracking_status <- function(tracking) {
  cat("\n========================================\n")
  cat("TRACKING MODULES\n")
  cat("========================================\n")
  cat(sprintf("County:     %s\n", if(tracking$county) "ON" else "OFF"))
  cat(sprintf("Bunking:    %s\n", if(tracking$bunking) "ON" else "OFF"))
  cat(sprintf("Partisan:   %s\n", if(tracking$partisan) "ON" else "OFF"))
  cat(sprintf("Victory:    %s\n", if(tracking$victory) "ON" else "OFF"))
  cat("========================================\n\n")
}

# ============================================
# PRIVATE CACHE ENVIRONMENT
# ============================================

.scoring_cache <- new.env(parent = emptyenv())

clear_scoring_cache <- function() {
  rm(list = ls(envir = .scoring_cache), envir = .scoring_cache)
}

# ============================================
# SCORING CALCULATION FUNCTIONS
# ============================================

calculate_expected_dem_seats <- function(dem_share, config) {
  k <- get_election_volatility_k(config)
  prob_win <- 1 / (1 + exp(-(dem_share - 0.5) / k))
  return(sum(prob_win))
}

calculate_efficiency_gap <- function(dem_share) {
  if (length(dem_share) == 0) return(NA_real_)

  wasted_dem <- 0
  wasted_rep <- 0

  for (share in dem_share) {
    if (share > 0.5) {
      # Democrats win
      wasted_dem <- wasted_dem + (share - 0.5)
      wasted_rep <- wasted_rep + (1 - share)
    } else {
      # Republicans win
      wasted_dem <- wasted_dem + share
      wasted_rep <- wasted_rep + ((1 - share) - 0.5)
    }
  }
  
  (wasted_dem - wasted_rep) / length(dem_share)
}

calculate_robust_efficiency_gap <- function(dem_share) {
  if (length(dem_share) == 0) return(NA_real_)

  # Swing scenarios and weights (normal distribution, σ=3%)
  swings  <- c(-0.08, -0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06, 0.08)
  weights <- c(0.007, 0.037, 0.108, 0.218, 0.272, 0.218, 0.108, 0.037, 0.007)
  
  eg_sum <- 0
  
  for (i in seq_along(swings)) {
    # Apply swing with bounds
    dem_share_swung <- pmin(pmax(dem_share + swings[i], 0), 1)
    
    wasted_dem <- 0
    wasted_rep <- 0
    
    for (share in dem_share_swung) {
      if (share > 0.5) {
        # Democrats win
        wasted_dem <- wasted_dem + (share - 0.5)
        wasted_rep <- wasted_rep + (1 - share)
      } else {
        # Republicans win
        wasted_dem <- wasted_dem + share
        wasted_rep <- wasted_rep + ((1 - share) - 0.5)
      }
    }
    
    eg_scenario <- (wasted_dem - wasted_rep) / length(dem_share_swung)
    eg_sum <- eg_sum + eg_scenario * weights[i]
  }
  
  eg_sum
}

get_election_volatility_k <- function(config) {
  target_prob <- config$behavior$election_win_prob_at_55
  
  if (target_prob <= 0 || target_prob >= 1) {
    stop("election_win_prob_at_55 must be between 0 and 1 (exclusive)")
  }
  
  k <- -0.05 / log((1 / target_prob) - 1)
  return(k)
}

calculate_county_splits_score <- function(assignment, counties, node_pops, ideal_pop) {
  if (is.null(counties)) {
    return(0)
  }
  
  # Check for cached county lookup
  if (!exists("county_lookup", envir = .scoring_cache)) {
    # First time: build lookup table
    unique_counties <- unique(counties)
    county_lookup <- vector("list", length(unique_counties))
    names(county_lookup) <- unique_counties
    
    for (county_name in unique_counties) {
      county_lookup[[county_name]] <- which(counties == county_name)
    }
    
    # Cache it
    assign("county_lookup", county_lookup, envir = .scoring_cache)
    assign("county_pops", sapply(county_lookup, function(idx) sum(node_pops[idx])), envir = .scoring_cache)
    assign("county_allowances", ceiling(get("county_pops", envir = .scoring_cache) / ideal_pop), envir = .scoring_cache)
  }
  
  # Use cached lookups
  county_lookup <- get("county_lookup", envir = .scoring_cache)
  county_pops <- get("county_pops", envir = .scoring_cache)
  allowances <- get("county_allowances", envir = .scoring_cache)
  
  count_penalty <- 0
  smoothness_penalty <- 0
  
  for (county_name in names(county_lookup)) {
    precinct_indices <- county_lookup[[county_name]]
    county_pop <- county_pops[county_name]
    
    if (county_pop == 0) next
    
    # Get districts in this county
    districts_in_county <- unique(assignment[precinct_indices])
    num_splits <- length(districts_in_county)
    
    allowance <- allowances[county_name]
    
    # Part 1: Count penalty
    excess_splits <- max(0, num_splits - allowance)
    count_penalty <- count_penalty + excess_splits
    
    # Part 2: Smoothness penalty - only for counties split beyond allowance
    if (num_splits > allowance) {
      # Calculate fraction of county in each district
      fractions <- numeric(num_splits)
      for (j in seq_along(districts_in_county)) {
        district <- districts_in_county[j]
        district_mask <- assignment[precinct_indices] == district
        pop_in_district <- sum(node_pops[precinct_indices[district_mask]])
        fractions[j] <- pop_in_district / county_pop
      }
      
      # Sort fractions to identify minority pieces
      fractions_sorted <- sort(fractions, decreasing = TRUE)
      
      # Sum everything except the largest piece (minority pieces)
      minority_fraction <- sum(fractions_sorted[2:length(fractions_sorted)])
      
      smoothness_penalty <- smoothness_penalty + minority_fraction
    }
  }
  
  # Combine: integer count + decimal smoothness
  return(count_penalty + smoothness_penalty)
}

calculate_map_metrics <- function(graph, assignment, node_pops, config,
                                  shp = NULL, counties = NULL, ideal_pop = NULL,
                                  timers = NULL, tracking = NULL) {
  metrics <- list()
  
  # Determine tracking if not provided
  if (is.null(tracking)) {
    tracking <- determine_active_tracking(config)
  }
  
  t0 <- start_timer()
  metrics$cut_edges <- nrow(get_cut_edges(graph, assignment))
  timers <- add_time(timers, "score_cut_edges", t0)
  
  # County splits score - only if tracking active
  if (tracking$county && !is.null(counties) && !is.null(ideal_pop)) {
    t0 <- start_timer()
    metrics$county_splits <- calculate_county_splits_score(assignment, counties, node_pops, ideal_pop)
    timers <- add_time(timers, "score_county_splits", t0)
  } else {
    metrics$county_splits <- NA
  }
  
  # Bunking metrics - only if tracking active
  if (tracking$bunking && !is.null(config$bunking_lists)) {
    t0 <- start_timer()
    bunking_lists <- config$bunking_lists
    
    # Anti-bunking metrics
    if (!is.null(bunking_lists$anti_bunking) && length(bunking_lists$anti_bunking) > 0) {
      for (i in seq_along(bunking_lists$anti_bunking)) {
        result <- calculate_anti_bunking_metric(assignment, bunking_lists$anti_bunking[[i]])
        
        prefix <- paste0("anti_bunking_", i)
        metrics[[paste0(prefix, "_raw")]] <- result$raw_metric
        metrics[[paste0(prefix, "_unique_districts")]] <- result$unique_districts
        metrics[[paste0(prefix, "_num_precincts")]] <- result$num_precincts
        
        # Calculate scored metric: raw_metric ^ exponent
        exponent <- bunking_lists$anti_bunking[[i]]$exponent
        metrics[[paste0(prefix, "_metric")]] <- result$raw_metric ^ exponent
      }
    }
    
    # Pro-bunking metrics
    if (!is.null(bunking_lists$pro_bunking) && length(bunking_lists$pro_bunking) > 0) {
      for (i in seq_along(bunking_lists$pro_bunking)) {
        result <- calculate_pro_bunking_metric(assignment, bunking_lists$pro_bunking[[i]])
        
        prefix <- paste0("pro_bunking_", i)
        metrics[[paste0(prefix, "_raw")]] <- result$raw_metric
        metrics[[paste0(prefix, "_unique_districts")]] <- result$unique_districts
        metrics[[paste0(prefix, "_num_precincts")]] <- result$num_precincts
        
        # Calculate scored metric: raw_metric ^ exponent
        exponent <- bunking_lists$pro_bunking[[i]]$exponent
        metrics[[paste0(prefix, "_metric")]] <- result$raw_metric ^ exponent
      }
    }
    timers <- add_time(timers, "score_bunking", t0)
  }
  
  # Partisan metrics - only if tracking active
  if (tracking$partisan && !is.null(shp)) {
    t0 <- start_timer()
    num_districts <- max(assignment)
    dem_votes <- numeric(num_districts)
    rep_votes <- numeric(num_districts)
    
    for (d in 1:num_districts) {
      nodes <- which(assignment == d)
      dem_votes[d] <- sum(shp$DEM[nodes])
      rep_votes[d] <- sum(shp$REP[nodes])
    }
    timers <- add_time(timers, "score_vote_aggregation", t0)
    
    t0 <- start_timer()
    total_votes <- dem_votes + rep_votes
    dem_share <- dem_votes / total_votes
    
    metrics$dem_votes <- dem_votes
    metrics$rep_votes <- rep_votes
    metrics$dem_share <- dem_share
    metrics$mean_dem_share <- mean(dem_share)
    metrics$median_dem_share <- median(dem_share)
    
    mean_median_diff <- metrics$mean_dem_share - metrics$median_dem_share
    metrics$mean_median_diff <- mean_median_diff
    metrics$mean_median_metric <- (abs(mean_median_diff - config$targets$mean_median))^config$exponents$mean_median * 100
    
    # Efficiency Gap (standard or robust based on setting)
    if (config$behavior$use_robust_efficiency_gap) {
      eg <- calculate_robust_efficiency_gap(dem_share)
      metrics$is_robust_eg <- TRUE
    } else {
      eg <- calculate_efficiency_gap(dem_share)
      metrics$is_robust_eg <- FALSE
    }
    metrics$efficiency_gap <- eg
    metrics$efficiency_gap_metric <- (abs(eg - config$targets$efficiency_gap))^config$exponents$efficiency_gap * 100
    
    # Victory tracking - only if active (subset of partisan)
    if (tracking$victory) {
      k <- get_election_volatility_k(config)
      dem_win_prob <- 1 / (1 + exp(-(dem_share - 0.5) / k))
      
      # Competitiveness
      minority_win_prob <- pmin(dem_win_prob, 1 - dem_win_prob)
      metrics$competitiveness <- sum(minority_win_prob * 2)
      
      comp_frac <- metrics$competitiveness / num_districts
      if (config$behavior$competitiveness_maximize) {
        metrics$competitiveness_metric <- ((1 - comp_frac)^config$exponents$competitiveness) * 100
      } else {
        metrics$competitiveness_metric <- (comp_frac^config$exponents$competitiveness) * 100
      }
      
      # Expected Dem seats
      expected_dem_seats <- sum(dem_win_prob)
      metrics$expected_dem_seats <- expected_dem_seats
      
      target_dem_seats <- config$targets$dem_seats
      if (!is.na(target_dem_seats)) {
        metrics$dem_seats_metric <- (abs(expected_dem_seats - target_dem_seats))^config$exponents$dem_seats
      } else {
        metrics$dem_seats_metric <- 0
      }
    } else {
      # Victory tracking disabled
      metrics$competitiveness <- NA
      metrics$competitiveness_metric <- NA
      metrics$expected_dem_seats <- NA
      metrics$dem_seats_metric <- NA
    }
    
    timers <- add_time(timers, "score_partisan_metrics", t0)
  } else {
    # Partisan tracking disabled - set all to NA
    metrics$mean_median_diff <- NA
    metrics$mean_median_metric <- NA
    metrics$efficiency_gap <- NA
    metrics$efficiency_gap_metric <- NA
    metrics$competitiveness <- NA
    metrics$competitiveness_metric <- NA
    metrics$expected_dem_seats <- NA
    metrics$dem_seats_metric <- NA
  }
  
  return(list(metrics = metrics, timers = timers))
}

calculate_map_score <- function(metrics, config) {
  weights <- get_weights_from_config(config)
  
  score <- 0
  
  for (metric_name in names(weights)) {
    metric_key <- paste0(metric_name, "_metric")
    
    # Check if pre-computed metric exists (e.g., mean_median_metric)
    if (!is.null(metrics[[metric_key]]) && !is.na(metrics[[metric_key]])) {
      score <- score + weights[[metric_name]] * metrics[[metric_key]]
    } 
    # Otherwise use raw metric and apply exponent
    else if (!is.null(metrics[[metric_name]]) && !is.na(metrics[[metric_name]])) {
      # Get exponent from config
      exponent <- config$exponents[[metric_name]]
      if (is.null(exponent)) exponent <- 1
      score <- score + weights[[metric_name]] * (metrics[[metric_name]] ^ exponent)
    }
  }
  
  return(score)
}

# ============================================
# SIMULATED ANNEALING FUNCTIONS
# ============================================

init_annealing <- function(initial_score, initial_temp_factor = 0.2, cooling_rate = 0.999,
                           temp_mode = "PROPORTIONAL", temp_nominal = NULL,
                           cooling_mode = "NOMINAL", cooling_guided_params = NULL,
                           num_steps = NULL) {
  
  # === TEMPERATURE INITIALIZATION ===
  if (temp_mode == "NOMINAL") {
    if (is.null(temp_nominal)) {
      stop("temp_mode='NOMINAL' requires temp_nominal to be specified")
    }
    temperature <- temp_nominal
  } else if (temp_mode == "PROPORTIONAL") {
    temperature <- initial_score * initial_temp_factor
  } else {
    stop("temp_mode must be 'NOMINAL' or 'PROPORTIONAL'")
  }
  
  # === COOLING RATE CALCULATION ===
  if (cooling_mode == "NOMINAL") {
    final_cooling_rate <- cooling_rate
  } else if (cooling_mode == "GUIDED") {
    if (is.null(cooling_guided_params)) {
      # Default guided parameters
      cooling_guided_params <- list(
        guidepoint_iteration_p = 0.9,
        guidepoint_temperature = 1
      )
    }
    if (is.null(num_steps)) {
      stop("cooling_mode='GUIDED' requires num_steps to be specified")
    }
    
    # Extract parameters
    iteration_p <- cooling_guided_params$guidepoint_iteration_p
    target_temp <- cooling_guided_params$guidepoint_temperature
    
    # Calculate cooling rate: (target_temp / initial_temp)^(1 / (iteration_p * num_steps))
    guidepoint_iteration <- iteration_p * num_steps
    final_cooling_rate <- (target_temp / temperature)^(1 / guidepoint_iteration)
    
    cat(sprintf("GUIDED cooling: T0=%.1f to T=%.1f at iteration %.0f (%.0f%%) at rate=%.6f\n",
                temperature, target_temp, guidepoint_iteration, iteration_p * 100, final_cooling_rate))
  } else {
    stop("cooling_mode must be 'NOMINAL' or 'GUIDED'")
  }
  
  list(
    temperature = temperature,
    cooling_rate = final_cooling_rate,
    initial_temperature = temperature,
    temp_mode = temp_mode,
    cooling_mode = cooling_mode,
    accepts = 0,
    rejects = 0
  )
}

cool_temperature <- function(annealing_state) {
  annealing_state$temperature <- annealing_state$temperature * annealing_state$cooling_rate
  return(annealing_state)
}

accept_proposal <- function(current_score, proposed_score, annealing_state) {
  delta <- proposed_score - current_score
  
  if (delta <= 0) {
    return(TRUE)
  }
  
  acceptance_prob <- exp(-delta / annealing_state$temperature)
  
  return(runif(1) < acceptance_prob)
}

# ============================================
# REPORTING FUNCTIONS
# ============================================

print_final_summary <- function(metrics, tracking = NULL) {
  cat("\n========================================\n")
  cat("FINAL ANALYSIS\n")
  cat("========================================\n")
  
  # Print partisan table if partisan tracking is ON
  if (!is.null(tracking) && tracking$partisan && !is.null(metrics$dem_votes)) {
    num_districts <- length(metrics$dem_votes)
    total_votes <- metrics$dem_votes + metrics$rep_votes
    dem_pct <- 100 * metrics$dem_votes / total_votes
    rep_pct <- 100 * metrics$rep_votes / total_votes
    margin <- abs(dem_pct - rep_pct)
    winner <- ifelse(dem_pct > rep_pct, "DEM", "REP")
    
    # Determine if we should show P(DEM) column
    show_pdem <- !is.null(tracking) && tracking$victory
    
    if (show_pdem) {
      cat(sprintf("%-10s %10s %10s %8s %8s %8s %8s %s\n", 
                  "District", "DEM", "REP", "DEM%", "REP%", "Margin", "P(DEM)", "Winner"))
      cat(strrep("-", 78), "\n")
    } else {
      cat(sprintf("%-10s %10s %10s %8s %8s %8s %s\n", 
                  "District", "DEM", "REP", "DEM%", "REP%", "Margin", "Winner"))
      cat(strrep("-", 70), "\n")
    }
    
    # Calculate win probabilities if victory tracking active
    if (show_pdem && !is.na(metrics$expected_dem_seats)) {
      dem_share <- metrics$dem_share
      # Reconstruct k from metrics (we need config for this - pass it through)
      # For now, use a reasonable default
      k <- 0.05 / log((1 / 0.9) - 1)  # Assumes default election_win_prob_at_55
      dem_win_prob <- 1 / (1 + exp(-(dem_share - 0.5) / k))
    } else {
      dem_win_prob <- NULL
    }
    
    for (d in 1:num_districts) {
      if (show_pdem && !is.null(dem_win_prob)) {
        prob_str <- sprintf("%7.1f%%", 100 * dem_win_prob[d])
        cat(sprintf("%-10s %10.0f %10.0f %7.1f%% %7.1f%% %7.1f%% %8s %s\n",
                    paste0("D", d), 
                    metrics$dem_votes[d], 
                    metrics$rep_votes[d],
                    dem_pct[d], 
                    rep_pct[d], 
                    margin[d],
                    prob_str,
                    winner[d]))
      } else {
        cat(sprintf("%-10s %10.0f %10.0f %7.1f%% %7.1f%% %7.1f%% %s\n",
                    paste0("D", d), 
                    metrics$dem_votes[d], 
                    metrics$rep_votes[d],
                    dem_pct[d], 
                    rep_pct[d], 
                    margin[d],
                    winner[d]))
      }
    }
    
    cat(strrep("-", if(show_pdem) 78 else 70), "\n")
    total_dem <- sum(metrics$dem_votes)
    total_rep <- sum(metrics$rep_votes)
    total_all <- total_dem + total_rep
    
    cat(sprintf("%-10s %10.0f %10.0f %7.1f%% %7.1f%%\n",
                "TOTAL",
                total_dem,
                total_rep,
                100 * total_dem / total_all,
                100 * total_rep / total_all))
    
    cat(sprintf("\nMean Dem Share:   %.1f%%\n", 100 * metrics$mean_dem_share))
    cat(sprintf("Median Dem Share: %.1f%%\n", 100 * metrics$median_dem_share))
    
    mm_diff <- metrics$mean_median_diff
    mm_pct <- 100 * abs(mm_diff)
    if (mm_diff < 0) {
      mm_label <- sprintf("D+%.1f%%", mm_pct)
    } else {
      mm_label <- sprintf("R+%.1f%%", mm_pct)
    }
    
    eg <- metrics$efficiency_gap
    eg_pct <- 100 * abs(eg)
    if (eg < 0) {
      eg_label <- sprintf("D+%.1f%%", eg_pct)
    } else {
      eg_label <- sprintf("R+%.1f%%", eg_pct)
    }
    
    cat(sprintf("MM: %s | MM Score: %.0f\n", mm_label, metrics$mean_median_metric))
    cat(sprintf("EG: %s | EG Score: %.0f\n", eg_label, metrics$efficiency_gap_metric))
    
    expected_dem <- metrics$expected_dem_seats
    if (!is.na(expected_dem)) {
      expected_rep <- num_districts - expected_dem
      cat(sprintf("Expected Seats: D %.1f | R %.1f\n", expected_dem, expected_rep))
    }
  }
  
  # Print county splits if county tracking is ON
  if (!is.null(tracking) && tracking$county && !is.null(metrics$county_splits) && !is.na(metrics$county_splits)) {
    cat("\n")
    cat(sprintf("County Splits Score: %.2f\n", metrics$county_splits))
  }
  
  # Print bunking metrics if bunking tracking is ON
  if (!is.null(tracking) && tracking$bunking) {
    # Check if we have any bunking metrics
    has_bunking <- any(grepl("^(anti|pro)_bunking_", names(metrics)))
    
    if (has_bunking) {
      cat("\n")
      cat(strrep("-", 70), "\n")
      
      # Anti-bunking metrics
      anti_indices <- grep("^anti_bunking_[0-9]+_raw$", names(metrics))
      if (length(anti_indices) > 0) {
        cat("Anti-Bunking Metrics:\n")
        for (idx_name in names(metrics)[anti_indices]) {
          i <- as.integer(sub("^anti_bunking_([0-9]+)_raw$", "\\1", idx_name))
          prefix <- paste0("anti_bunking_", i)
          raw <- metrics[[paste0(prefix, "_raw")]]
          unique_dist <- metrics[[paste0(prefix, "_unique_districts")]]
          num_prec <- metrics[[paste0(prefix, "_num_precincts")]]
          metric_val <- metrics[[paste0(prefix, "_metric")]]
          
          cat(sprintf("  List %d: %d precincts in %d districts | metric=%.3f\n",
                      i, num_prec, unique_dist, raw))
        }
      }
      
      # Pro-bunking metrics
      pro_indices <- grep("^pro_bunking_[0-9]+_raw$", names(metrics))
      if (length(pro_indices) > 0) {
        cat("\nPro-Bunking Metrics:\n")
        for (idx_name in names(metrics)[pro_indices]) {
          i <- as.integer(sub("^pro_bunking_([0-9]+)_raw$", "\\1", idx_name))
          prefix <- paste0("pro_bunking_", i)
          raw <- metrics[[paste0(prefix, "_raw")]]
          unique_dist <- metrics[[paste0(prefix, "_unique_districts")]]
          num_prec <- metrics[[paste0(prefix, "_num_precincts")]]
          metric_val <- metrics[[paste0(prefix, "_metric")]]
          
          cat(sprintf("  List %d: %d precincts in %d districts | metric=%.3f\n",
                      i, num_prec, unique_dist, raw))
        }
      }
    }
  }
  
  cat("========================================\n\n")
}