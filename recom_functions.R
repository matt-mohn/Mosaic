# ============================================
# RECOM STEP FUNCTION
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