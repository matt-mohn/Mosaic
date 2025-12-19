# ============================================
# PARTITION FUNCTIONS
# ============================================

create_initial_partition <- function(graph, node_pops, num_districts, pdev_tolerance, seed, 
                                     counties = NULL, county_bias = 1.0, verbose = TRUE) {
  set.seed(seed)
  n <- vcount(graph)
  total_pop <- sum(node_pops)
  ideal_pop <- total_pop / num_districts
  
  assignment <- rep(0, n)
  remaining_nodes <- 1:n
  
  for (district in 1:(num_districts - 1)) {
    if (verbose) cat(sprintf("\nCreating district %d/%d...\n", district, num_districts))
    
    subgraph <- induced_subgraph(graph, remaining_nodes)
    sub_node_pops <- node_pops[remaining_nodes]
    sub_counties <- if (!is.null(counties)) counties[remaining_nodes] else NULL
    sub_total_pop <- sum(sub_node_pops)
    
    remaining_districts <- num_districts - district + 1
    target_pop <- ideal_pop
    
    if (verbose) {
      cat(sprintf("  Subgraph: %.0f total pop for %d districts\n", 
                  sub_total_pop, remaining_districts))
    }
    
    max_attempts <- 10000
    found_valid <- FALSE
    
    for (attempt in 1:max_attempts) {
      result <- bipartition_tree(subgraph, sub_node_pops, target_pop, pdev_tolerance, 
                                 max_attempts = 100, one_sided = TRUE, timers = NULL,
                                 counties = sub_counties, county_bias = county_bias)
      subset_local <- result$subset
      
      if (!is.null(V(subgraph)$name)) {
        subset_global <- as.integer(V(subgraph)$name[subset_local])
      } else {
        subset_global <- remaining_nodes[subset_local]
      }
      
      subset_pop <- sum(node_pops[subset_global])
      remaining_pop <- sub_total_pop - subset_pop
      remaining_ideal <- ideal_pop * (remaining_districts - 1)
      
      subset_dev <- abs(subset_pop - ideal_pop) / ideal_pop
      remaining_dev <- abs(remaining_pop - remaining_ideal) / remaining_ideal
      
      if (subset_dev <= pdev_tolerance && remaining_dev <= pdev_tolerance) {
        found_valid <- TRUE
        assignment[subset_global] <- district
        remaining_nodes <- setdiff(remaining_nodes, subset_global)
        
        if (verbose) {
          cat(sprintf("District %d: %.0f pop (%.1f%%), remaining: %.0f for %d districts (%.1f%%)\n", 
                      district, subset_pop, subset_dev * 100, 
                      remaining_pop, remaining_districts - 1, remaining_dev * 100))
        }
        break
      }
      
      if (verbose && attempt %% 1000 == 0) {
        cat(sprintf("  Attempt %d: subset dev %.1f%%, remaining dev %.1f%%\n", 
                    attempt, subset_dev * 100, remaining_dev * 100))
      }
    }
    
    if (!found_valid) {
      stop("Could not find valid partition after ", max_attempts, " attempts")
    }
  }
  
  assignment[remaining_nodes] <- num_districts
  final_pop <- sum(node_pops[remaining_nodes])
  if (verbose) {
    cat(sprintf("District %d (final): %.0f pop (%.1f%% from ideal)\n", 
                num_districts, final_pop, (final_pop - ideal_pop) / ideal_pop * 100))
  }
  
  return(assignment)
}

# ============================================
# DISTRICT UTILITY FUNCTIONS
# ============================================

get_district_nodes <- function(assignment, district) {
  return(which(assignment == district))
}

get_district_populations <- function(node_pops, assignment) {
  districts <- sort(unique(assignment))
  pops <- sapply(districts, function(d) sum(node_pops[assignment == d]))
  names(pops) <- districts
  return(pops)
}

# ============================================
# EDGE AND CONTIGUITY FUNCTIONS
# ============================================

get_cut_edges <- function(graph, assignment) {
  edges <- as_edgelist(graph, names = FALSE)
  cut_edge_mask <- assignment[edges[,1]] != assignment[edges[,2]]
  return(edges[cut_edge_mask, , drop = FALSE])
}

check_contiguity <- function(graph, assignment, district) {
  nodes <- get_district_nodes(assignment, district)
  if (length(nodes) == 0) return(FALSE)
  
  subgraph <- induced_subgraph(graph, nodes)
  return(is_connected(subgraph))
}

check_all_contiguous <- function(graph, assignment) {
  districts <- unique(assignment)
  for (district in districts) {
    if (!check_contiguity(graph, assignment, district)) {
      return(FALSE)
    }
  }
  return(TRUE)
}