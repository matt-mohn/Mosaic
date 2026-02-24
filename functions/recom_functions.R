# ============================================
# DISTRICT ADJACENCY (for n>2 recombination)
# ============================================

build_district_adjacency_list <- function(precinct_adj, assignment, n_districts) {
  district_adj <- vector("list", n_districts)
  for (d in 1:n_districts) district_adj[[d]] <- integer(0)
  for (p in seq_along(assignment)) {
    p_dist <- assignment[p]
    for (neighbor in precinct_adj[[p]]) {
      n_dist <- assignment[neighbor]
      if (p_dist != n_dist && !(n_dist %in% district_adj[[p_dist]])) {
        district_adj[[p_dist]] <- c(district_adj[[p_dist]], n_dist)
      }
    }
  }
  return(district_adj)
}

update_district_adjacency_list <- function(district_adj, precinct_adj, assignment, changed_districts) {
  for (d in changed_districts) district_adj[[d]] <- integer(0)
  for (d in seq_along(district_adj)) {
    district_adj[[d]] <- setdiff(district_adj[[d]], changed_districts)
  }
  changed_precincts <- which(assignment %in% changed_districts)
  for (p in changed_precincts) {
    p_dist <- assignment[p]
    for (neighbor in precinct_adj[[p]]) {
      n_dist <- assignment[neighbor]
      if (p_dist != n_dist) {
        if (!(n_dist %in% district_adj[[p_dist]])) {
          district_adj[[p_dist]] <- c(district_adj[[p_dist]], n_dist)
        }
        if (!(p_dist %in% district_adj[[n_dist]])) {
          district_adj[[n_dist]] <- c(district_adj[[n_dist]], p_dist)
        }
      }
    }
  }
  return(district_adj)
}

select_n_districts <- function(district_adj, n) {
  n_districts <- length(district_adj)
  max_attempts <- 100
  for (attempt in 1:max_attempts) {
    start <- sample(n_districts, 1)
    selected <- start
    while (length(selected) < n) {
      neighbors <- unique(unlist(district_adj[selected]))
      candidates <- setdiff(neighbors, selected)
      if (length(candidates) == 0) break
      selected <- c(selected, sample(candidates, 1))
    }
    if (length(selected) == n) return(selected)
  }
  return(NULL)
}

# ============================================
# RECOM STEP FUNCTION (n=2, original)
# ============================================

recom_step <- function(graph, assignment, node_pops, ideal_pop, pdev_tolerance, verbose = TRUE,
                       timers = NULL, counties = NULL, county_bias = 1.0) {
  t0 <- start_timer()
  cut_edges <- get_cut_edges(graph, assignment)
  timers <- add_time(timers, "cut_edges", t0)
  
  if (nrow(cut_edges) == 0) {
    if (verbose) cat("  WARNING: No cut edges\n")
    return(list(assignment = assignment, timers = timers))
  }
  
  edge_idx <- sample(nrow(cut_edges), 1)
  edge <- cut_edges[edge_idx, ]
  
  district_a <- assignment[edge[1]]
  district_b <- assignment[edge[2]]
  
  nodes_a <- get_district_nodes(assignment, district_a)
  nodes_b <- get_district_nodes(assignment, district_b)
  merged_nodes <- c(nodes_a, nodes_b)
  
  t0 <- start_timer()
  test_merged <- induced_subgraph(graph, merged_nodes)
  timers <- add_time(timers, "subgraph_create", t0)
  
  t0 <- start_timer()
  is_conn <- is_connected(test_merged)
  timers <- add_time(timers, "merge_check", t0)
  
  if (!is_conn) {
    if (verbose) cat(sprintf("  ERROR: D%d+D%d not connected\n", district_a, district_b))
    return(list(assignment = assignment, timers = timers))
  }
  
  subgraph <- test_merged
  
  if (!is.null(V(subgraph)$name)) {
    subgraph_vertex_ids <- as.integer(V(subgraph)$name)
    sub_node_pops <- node_pops[subgraph_vertex_ids]
    sub_counties <- if (!is.null(counties)) counties[subgraph_vertex_ids] else NULL
  } else {
    sub_node_pops <- node_pops[merged_nodes]
    sub_counties <- if (!is.null(counties)) counties[merged_nodes] else NULL
  }
  
  result <- tryCatch({
    bipartition_tree(subgraph, sub_node_pops, ideal_pop, pdev_tolerance, one_sided = FALSE, 
                     timers = timers, counties = sub_counties, county_bias = county_bias)
  }, error = function(e) {
    if (verbose) cat(sprintf("  Failed: %s\n", e$message))
    return(list(subset = NULL, timers = timers))
  })
  
  subset_local <- result$subset
  timers <- result$timers
  
  if (is.null(subset_local)) {
    return(list(assignment = assignment, timers = timers))
  }
  
  t0 <- start_timer()
  if (!is.null(V(subgraph)$name)) {
    subset_global <- as.integer(V(subgraph)$name[subset_local])
  } else {
    subset_global <- merged_nodes[subset_local]
  }
  
  remaining_global <- setdiff(merged_nodes, subset_global)
  
  pop1 <- sum(node_pops[subset_global])
  pop2 <- sum(node_pops[remaining_global])
  
  new_assignment <- assignment
  new_assignment[subset_global] <- district_a
  new_assignment[remaining_global] <- district_b
  timers <- add_time(timers, "partition_apply", t0)
  
  t0 <- start_timer()
  contig_a <- check_contiguity(graph, new_assignment, district_a)
  contig_b <- check_contiguity(graph, new_assignment, district_b)
  timers <- add_time(timers, "contiguity_check", t0)
  
  if (!contig_a || !contig_b) {
    if (verbose) cat(sprintf("  ERROR: Contiguity violated\n"))
    return(list(assignment = assignment, timers = timers))
  }
  
  if (verbose) {
    cat(sprintf("D%d+D%d → D%d:%.0fk D%d:%.0fk\n",
                district_a, district_b, district_a, pop1/1000, district_b, pop2/1000))
  }

  return(list(assignment = new_assignment, timers = timers))
}

# ============================================
# RECOM STEP FUNCTION (n>2)
# ============================================

recom_step_n <- function(graph, assignment, node_pops, ideal_pop, pdev_tolerance,
                         district_adj, precinct_adj, n = 3, verbose = FALSE, timers = NULL,
                         counties = NULL, county_bias = 1.0) {

  t0 <- start_timer()

  # Select n connected districts
  selected_districts <- select_n_districts(district_adj, n)
  if (is.null(selected_districts)) {
    return(list(assignment = assignment, changed = NULL, timers = timers))
  }

  timers <- add_time(timers, "district_selection", t0)

  t0 <- start_timer()

  # Merge nodes from all selected districts
  merged_nodes <- which(assignment %in% selected_districts)
  subgraph <- induced_subgraph(graph, merged_nodes)

  timers <- add_time(timers, "subgraph_create", t0)

  t0 <- start_timer()

  if (!is_connected(subgraph)) {
    return(list(assignment = assignment, changed = NULL, timers = timers))
  }

  timers <- add_time(timers, "merge_check", t0)

  # Get vertex IDs and populations for subgraph
  if (!is.null(V(subgraph)$name)) {
    subgraph_vertex_ids <- as.integer(V(subgraph)$name)
  } else {
    subgraph_vertex_ids <- merged_nodes
  }
  sub_node_pops <- node_pops[subgraph_vertex_ids]
  sub_counties <- if (!is.null(counties)) counties[subgraph_vertex_ids] else NULL

  # Sequential bipartition: carve off (n-1) districts one at a time
  new_assignment <- assignment
  remaining_subgraph <- subgraph
  remaining_node_ids <- subgraph_vertex_ids
  remaining_pops <- sub_node_pops
  remaining_counties <- sub_counties

  for (i in 1:(n - 1)) {
    t0 <- start_timer()

    remaining_districts <- n - i + 1
    sub_total_pop <- sum(remaining_pops)

    found_valid <- FALSE
    for (outer_attempt in 1:50) {
      result <- tryCatch({
        bipartition_tree(remaining_subgraph, remaining_pops, ideal_pop, pdev_tolerance,
                         max_attempts = 50, one_sided = TRUE, timers = NULL,
                         counties = remaining_counties, county_bias = county_bias)
      }, error = function(e) list(subset = NULL))

      if (is.null(result$subset)) next

      subset_local <- result$subset
      if (!is.null(V(remaining_subgraph)$name)) {
        subset_global <- as.integer(V(remaining_subgraph)$name[subset_local])
      } else {
        subset_global <- remaining_node_ids[subset_local]
      }

      # Dual validation: check both subset AND remainder
      subset_pop <- sum(node_pops[subset_global])
      remaining_pop <- sub_total_pop - subset_pop
      remaining_ideal <- ideal_pop * (remaining_districts - 1)

      subset_dev <- abs(subset_pop - ideal_pop) / ideal_pop
      remaining_dev <- abs(remaining_pop - remaining_ideal) / remaining_ideal

      if (subset_dev <= pdev_tolerance && remaining_dev <= pdev_tolerance) {
        found_valid <- TRUE
        new_assignment[subset_global] <- selected_districts[i]

        remaining_local <- setdiff(1:vcount(remaining_subgraph), subset_local)
        remaining_subgraph <- induced_subgraph(remaining_subgraph, remaining_local)

        if (!is.null(V(remaining_subgraph)$name)) {
          remaining_node_ids <- as.integer(V(remaining_subgraph)$name)
        } else {
          remaining_node_ids <- remaining_node_ids[remaining_local]
        }
        remaining_pops <- node_pops[remaining_node_ids]
        remaining_counties <- if (!is.null(counties)) counties[remaining_node_ids] else NULL
        break
      }
    }

    timers <- add_time(timers, "bipartition", t0)

    if (!found_valid) {
      return(list(assignment = assignment, changed = NULL, timers = timers))
    }
  }

  # Assign remaining nodes to the last district
  new_assignment[remaining_node_ids] <- selected_districts[n]

  t0 <- start_timer()

  # Verify contiguity of all new districts
  for (d in selected_districts) {
    if (!check_contiguity(graph, new_assignment, d)) {
      return(list(assignment = assignment, changed = NULL, timers = timers))
    }
  }

  timers <- add_time(timers, "contiguity_check", t0)

  if (verbose) {
    cat(sprintf("Recombined D%s\n", paste(selected_districts, collapse = "+")))
  }

  return(list(assignment = new_assignment, changed = selected_districts, timers = timers))
}
