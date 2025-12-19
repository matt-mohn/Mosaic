random_spanning_tree <- function(graph, counties = NULL, county_bias = 1.0) {
  if (vcount(graph) == 0) {
    stop("Cannot create spanning tree of empty graph")
  }
  
  if (ecount(graph) == 0) {
    stop("Cannot create spanning tree of graph with no edges")
  }
  
  n_edges <- ecount(graph)
  base_weights <- runif(n_edges)
  
  # Apply county penalty if counties provided
  if (!is.null(counties) && county_bias != 1.0) {
    edge_list <- as_edgelist(graph, names = FALSE)
    
    # For subgraphs, counties vector corresponds to subgraph vertices in order
    # edge_list uses 1-indexed positions in the subgraph
    edge_counties_from <- counties[edge_list[,1]]
    edge_counties_to <- counties[edge_list[,2]]
    
    # Apply penalty to cross-county edges
    crosses_county <- edge_counties_from != edge_counties_to
    penalty_multiplier <- ifelse(crosses_county, county_bias, 1.0)
    E(graph)$weight <- base_weights * penalty_multiplier
  } else {
    E(graph)$weight <- base_weights
  }
  
  spanning_tree <- mst(graph, weights = E(graph)$weight)
  return(spanning_tree)
}

get_tree_structure <- function(graph, root) {
  n <- vcount(graph)
  
  bfs_result <- bfs(graph, root = root, unreachable = FALSE, 
                    parent = TRUE, order = TRUE, rank = FALSE, dist = FALSE)
  
  pred <- as.integer(bfs_result$parent)
  order <- as.integer(bfs_result$order)
  
  succ <- vector("list", n)
  
  for (node in 1:n) {
    parent <- pred[node]
    if (!is.na(parent) && parent > 0 && parent <= n) {
      succ[[parent]] <- c(succ[[parent]], node)
    }
  }
  
  return(list(pred = pred, succ = succ))
}

get_predecessors <- function(graph, root) {
  n <- vcount(graph)
  if (root < 1 || root > n) {
    stop(sprintf("Invalid root %d for graph with %d vertices", root, n))
  }
  bfs_result <- bfs(graph, root = root, unreachable = FALSE, parent = TRUE)
  return(bfs_result$parent)
}

get_successors <- function(graph, root) {
  n <- vcount(graph)
  adj_list <- as_adj_list(graph, mode = "all")
  visited <- rep(FALSE, n)
  succ <- vector("list", n)
  
  queue <- c(root)
  visited[root] <- TRUE
  
  while (length(queue) > 0) {
    node <- queue[1]
    queue <- queue[-1]
    
    neighbors <- adj_list[[node]]
    for (neighbor in neighbors) {
      if (!visited[neighbor]) {
        visited[neighbor] <- TRUE
        succ[[node]] <- c(succ[[node]], neighbor)
        queue <- c(queue, neighbor)
      }
    }
  }
  
  return(succ)
}

calc_subtree_pops <- function(succ, root, node_pops) {
  n <- length(succ)
  subtree_pops <- numeric(n)
  
  stack <- integer(n * 2)
  stack[1] <- root
  top <- 1
  processed <- rep(FALSE, n)
  
  while (top > 0) {
    node <- stack[top]
    
    if (processed[node]) {
      top <- top - 1
      pop <- node_pops[node]
      children <- succ[[node]]
      if (!is.null(children)) {
        for (child in children) {
          pop <- pop + subtree_pops[child]
        }
      }
      subtree_pops[node] <- pop
    } else {
      processed[node] <- TRUE
      children <- succ[[node]]
      if (!is.null(children)) {
        for (child in children) {
          top <- top + 1
          stack[top] <- child
        }
      }
      top <- top + 1
      stack[top] <- node
    }
  }
  
  return(subtree_pops)
}

get_subtree_nodes <- function(node, succ) {
  max_size <- length(succ)
  nodes <- integer(max_size)
  visited <- rep(FALSE, max_size)
  
  n_nodes <- 0
  n_queue <- 1
  queue <- integer(max_size)
  queue[1] <- node
  visited[node] <- TRUE
  head <- 1
  
  while (head <= n_queue) {
    current <- queue[head]
    head <- head + 1
    
    n_nodes <- n_nodes + 1
    nodes[n_nodes] <- current
    
    children <- succ[[current]]
    if (!is.null(children) && length(children) > 0) {
      for (child in children) {
        if (!visited[child]) {
          visited[child] <- TRUE
          n_queue <- n_queue + 1
          queue[n_queue] <- child
        }
      }
    }
  }
  
  return(nodes[1:n_nodes])
}

bipartition_tree <- function(graph, node_pops, pop_target, pdev_tolerance, max_attempts = 1000, 
                             one_sided = FALSE, timers = NULL, counties = NULL, county_bias = 1.0) {
  total_pop <- sum(node_pops)
  min_pop <- pop_target * (1 - pdev_tolerance)
  max_pop <- pop_target * (1 + pdev_tolerance)
  
  for (attempt in 1:max_attempts) {
    t0 <- start_timer()
    spanning_tree <- random_spanning_tree(graph, counties, county_bias)
    timers <- add_time(timers, "spanning_tree", t0)
    
    n <- vcount(spanning_tree)
    nodes_with_degree_gt_1 <- which(degree(spanning_tree) > 1)
    
    if (length(nodes_with_degree_gt_1) == 0) {
      next
    }
    
    root <- sample(nodes_with_degree_gt_1, 1)
    
    t0 <- start_timer()
    tree_struct <- get_tree_structure(spanning_tree, root)
    pred <- tree_struct$pred
    succ <- tree_struct$succ
    timers <- add_time(timers, "bfs_traversal", t0)
    
    t0 <- start_timer()
    subtree_pops <- calc_subtree_pops(succ, root, node_pops)
    timers <- add_time(timers, "subtree_pops", t0)
    
    t0 <- start_timer()
    for (node in 1:n) {
      if (node == root) next
      if (pred[node] == 0) next
      
      tree_pop <- subtree_pops[node]
      other_pop <- total_pop - tree_pop
      
      valid <- if (one_sided) {
        (tree_pop >= min_pop && tree_pop <= max_pop) ||
          (other_pop >= min_pop && other_pop <= max_pop)
      } else {
        (tree_pop >= min_pop && tree_pop <= max_pop) &&
          (other_pop >= min_pop && other_pop <= max_pop)
      }
      
      if (valid) {
        subset_indices <- get_subtree_nodes(node, succ)
        
        if (length(subset_indices) == 0 || any(subset_indices < 1) || any(subset_indices > n)) {
          next
        }
        
        actual_subset_pop <- sum(node_pops[subset_indices])
        actual_other_pop <- total_pop - actual_subset_pop
        
        if (abs(actual_subset_pop - tree_pop) > 1) {
          next
        }
        
        if (actual_subset_pop < min_pop || actual_subset_pop > max_pop) {
          next
        }
        
        if (!one_sided && (actual_other_pop < min_pop || actual_other_pop > max_pop)) {
          next
        }
        
        timers <- add_time(timers, "find_cut", t0)
        return(list(subset = subset_indices, timers = timers))
      }
    }
    timers <- add_time(timers, "find_cut", t0)
    
    if (attempt %% 100 == 0) {
      cat(sprintf("  Attempt %d: no cuts found yet\n", attempt))
    }
  }
  
  stop("Could not find balanced cut after ", max_attempts, " attempts")
}