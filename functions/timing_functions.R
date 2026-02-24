TIMING_ENABLED <- FALSE  # Set TRUE only when timing_analysis = TRUE in run_chain

init_timing <- function() {
  TIMING_ENABLED <<- TRUE  # Enable timing when init is called
  list(
    cut_edges = 0,
    merge_check = 0,
    subgraph_create = 0,
    spanning_tree = 0,
    bfs_traversal = 0,
    subtree_pops = 0,
    find_cut = 0,
    partition_apply = 0,
    contiguity_check = 0,
    score_calculation = 0,
    score_cut_edges = 0,
    score_county_splits = 0,
    score_bunking = 0,
    score_vote_aggregation = 0,
    score_partisan_metrics = 0,
    total_steps = 0
  )
}

start_timer <- function() {
  if (!TIMING_ENABLED) return(NULL)
  proc.time()[3]
}

add_time <- function(timers, category, start_time) {
  if (!TIMING_ENABLED || is.null(start_time) || is.null(timers)) return(timers)
  elapsed <- proc.time()[3] - start_time
  if (is.null(timers[[category]])) {
    timers[[category]] <- 0
  }
  timers[[category]] <- timers[[category]] + elapsed
  timers
}

print_timing_report <- function(timers, num_steps) {
  if (!TIMING_ENABLED || is.null(timers)) return()
  
  cat("\n========================================\n")
  cat("PERFORMANCE TIMING REPORT\n")
  cat("========================================\n")
  cat(sprintf("Total steps: %d\n", num_steps))
  cat(sprintf("Total time: %.2fs\n\n", timers$total_steps))
  
  categories <- c(
    "cut_edges", "merge_check", "subgraph_create", 
    "spanning_tree", "bfs_traversal", "subtree_pops",
    "find_cut", "partition_apply", "contiguity_check",
    "score_calculation"
  )
  
  labels <- c(
    "Get Cut Edges", "Merge Connectivity", "Subgraph Creation",
    "Spanning Tree Gen", "BFS Traversal", "Subtree Pops Calc",
    "Find Valid Cut", "Apply Partition", "Contiguity Check",
    "Score Calculation"
  )
  
  total_tracked <- sum(sapply(categories, function(c) timers[[c]]))
  
  cat("Per-Category Breakdown:\n")
  cat(sprintf("%-25s %8s %8s %8s %10s\n", "Category", "Total", "Avg/Step", "%Time", "ms/Step"))
  cat(strrep("-", 70), "\n")
  
  for (i in 1:length(categories)) {
    cat_time <- timers[[categories[i]]]
    avg_time <- cat_time / num_steps
    pct_time <- (cat_time / total_tracked) * 100
    
    cat(sprintf("%-25s %7.2fs %7.2fs %7.1f%% %9.1fms\n",
                labels[i], cat_time, avg_time, pct_time, avg_time * 1000))
  }
  
  cat(strrep("-", 70), "\n")
  cat(sprintf("%-25s %7.2fs %7.2fs %7.1f%% %9.1fms\n",
              "TOTAL TRACKED", total_tracked, total_tracked/num_steps, 
              100.0, (total_tracked/num_steps) * 1000))
  
  untracked <- timers$total_steps - total_tracked
  if (untracked > 0.01) {
    cat(sprintf("%-25s %7.2fs %7.2fs %7.1f%% %9.1fms\n",
                "Untracked Overhead", untracked, untracked/num_steps,
                (untracked/timers$total_steps)*100, (untracked/num_steps)*1000))
  }
  
  # Print granular score breakdown
  score_categories <- c("score_cut_edges", "score_county_splits", "score_bunking", 
                        "score_vote_aggregation", "score_partisan_metrics")
  score_labels <- c("  Cut Edges Calc", "  County Splits", "  Bunking Metrics",
                    "  Vote Aggregation", "  Partisan Metrics")
  
  has_score_detail <- any(sapply(score_categories, function(c) !is.null(timers[[c]]) && timers[[c]] > 0))
  
  if (has_score_detail) {
    cat("\nScore Calculation Breakdown:\n")
    cat(strrep("-", 70), "\n")
    
    score_total <- sum(sapply(score_categories, function(c) if(!is.null(timers[[c]])) timers[[c]] else 0))
    
    for (i in 1:length(score_categories)) {
      cat_time <- if(!is.null(timers[[score_categories[i]]])) timers[[score_categories[i]]] else 0
      if (cat_time > 0) {
        avg_time <- cat_time / num_steps
        pct_time <- (cat_time / score_total) * 100
        
        cat(sprintf("%-25s %7.2fs %7.2fs %7.1f%% %9.1fms\n",
                    score_labels[i], cat_time, avg_time, pct_time, avg_time * 1000))
      }
    }
  }
  
  cat("========================================\n\n")
}