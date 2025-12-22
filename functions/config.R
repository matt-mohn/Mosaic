# ============================================
# SCORING CONFIGURATION OBJECT
# ============================================

create_scoring_config <- function(
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
    bunking_lists = NULL
) {
  # Validate election_win_prob_at_55
  if (!is.na(election_win_prob_at_55) && (election_win_prob_at_55 <= 0 || election_win_prob_at_55 >= 1)) {
    stop("election_win_prob_at_55 must be between 0 and 1 (exclusive)")
  }

  # Validate weights are non-negative
  weights_to_check <- list(
    weight_cut_edges = weight_cut_edges,
    weight_county_splits = weight_county_splits,
    weight_mean_median = weight_mean_median,
    weight_efficiency_gap = weight_efficiency_gap,
    weight_dem_seats = weight_dem_seats,
    weight_competitiveness = weight_competitiveness
  )
  for (name in names(weights_to_check)) {
    val <- weights_to_check[[name]]
    if (!is.na(val) && val < 0) {
      stop(sprintf("%s cannot be negative (got %.2f)", name, val))
    }
  }

  # Validate exponents are positive
  exponents_to_check <- list(
    exponent_cut_edges = exponent_cut_edges,
    exponent_county_splits = exponent_county_splits,
    exponent_mean_median = exponent_mean_median,
    exponent_efficiency_gap = exponent_efficiency_gap,
    exponent_dem_seats = exponent_dem_seats,
    exponent_competitiveness = exponent_competitiveness
  )
  for (name in names(exponents_to_check)) {
    val <- exponents_to_check[[name]]
    if (!is.na(val) && val <= 0) {
      stop(sprintf("%s must be positive (got %.2f)", name, val))
    }
  }

  config <- list(
    weights = list(
      cut_edges = weight_cut_edges,
      county_splits = weight_county_splits,
      mean_median = weight_mean_median,
      efficiency_gap = weight_efficiency_gap,
      dem_seats = weight_dem_seats,
      competitiveness = weight_competitiveness
    ),
    exponents = list(
      cut_edges = exponent_cut_edges,
      county_splits = exponent_county_splits,
      mean_median = exponent_mean_median,
      efficiency_gap = exponent_efficiency_gap,
      dem_seats = exponent_dem_seats,
      competitiveness = exponent_competitiveness
    ),
    targets = list(
      mean_median = target_mean_median,
      efficiency_gap = target_efficiency_gap,
      dem_seats = target_dem_seats
    ),
    behavior = list(
      use_robust_efficiency_gap = use_robust_efficiency_gap,
      election_win_prob_at_55 = election_win_prob_at_55,
      competitiveness_maximize = competitiveness_maximize
    ),
    bunking_lists = bunking_lists
  )
  
  class(config) <- "scoring_config"
  return(config)
}

# Helper to get weights from config
get_weights_from_config <- function(config) {
  weights <- list()
  
  # Standard weights - only include if not NA
  if (!is.na(config$weights$cut_edges)) {
    weights$cut_edges <- config$weights$cut_edges
  }
  if (!is.na(config$weights$county_splits)) {
    weights$county_splits <- config$weights$county_splits
  }
  if (!is.na(config$weights$mean_median)) {
    weights$mean_median <- config$weights$mean_median
  }
  if (!is.na(config$weights$efficiency_gap)) {
    weights$efficiency_gap <- config$weights$efficiency_gap
  }
  if (!is.na(config$weights$dem_seats)) {
    weights$dem_seats <- config$weights$dem_seats
  }
  if (!is.na(config$weights$competitiveness)) {
    weights$competitiveness <- config$weights$competitiveness
  }
  
  # Default weights if none specified
  if (length(weights) == 0) {
    weights$cut_edges <- 1.0
  }
  
  # Add bunking weights if present
  if (!is.null(config$bunking_lists)) {
    bunking_lists <- config$bunking_lists
    
    # Anti-bunking weights
    if (!is.null(bunking_lists$anti_bunking) && length(bunking_lists$anti_bunking) > 0) {
      for (i in seq_along(bunking_lists$anti_bunking)) {
        weight <- bunking_lists$anti_bunking[[i]]$weight
        if (!is.na(weight)) {
          weight_name <- paste0("anti_bunking_", i)
          weights[[weight_name]] <- weight
        }
      }
    }
    
    # Pro-bunking weights
    if (!is.null(bunking_lists$pro_bunking) && length(bunking_lists$pro_bunking) > 0) {
      for (i in seq_along(bunking_lists$pro_bunking)) {
        weight <- bunking_lists$pro_bunking[[i]]$weight
        if (!is.na(weight)) {
          weight_name <- paste0("pro_bunking_", i)
          weights[[weight_name]] <- weight
        }
      }
    }
  }
  
  return(weights)
}