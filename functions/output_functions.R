# ============================================
# RESULTS SAVING FUNCTIONS
# ============================================

save_results <- function(assignments_all, cut_edges_all, scores_all, county_splits_all, 
                         dem_share_by_district, shp, output_dir, num_steps,
                         capture_full_assignments, save_final_assignment, 
                         snapshot_assignments, snapshot_interval) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  timestamp <- format(Sys.time(), "%m%d%Y_%H%M")
  
  # Detect ID column and extract values
  id_col_name <- NULL
  if ("GEOID20" %in% names(shp)) {
    id_col_name <- "GEOID20"
  } else if ("GEOID" %in% names(shp)) {
    id_col_name <- "GEOID"
  } else if ("PRECINCT" %in% names(shp)) {
    id_col_name <- "PRECINCT"
  } else if ("PCT" %in% names(shp)) {
    id_col_name <- "PCT"
  } else {
    stop("Shapefile must have one of: GEOID20, GEOID, PRECINCT, or PCT column")
  }
  
  # Extract ID values as character (ensures join compatibility)
  precinct_ids <- as.character(shp[[id_col_name]])
  
  # Save full assignments if requested
  if (capture_full_assignments) {
    output_df <- as.data.frame(assignments_all)
    colnames(output_df) <- paste0("iteration_", 0:num_steps)
    output_df <- data.frame(precinct_ids, output_df, stringsAsFactors = FALSE)
    names(output_df)[1] <- id_col_name
    
    output_file <- file.path(output_dir, paste0("full_assignments_", timestamp, ".csv"))
    write.csv(output_df, output_file, row.names = FALSE, quote = TRUE)
  }
  
  # Save snapshot assignments if requested
  if (snapshot_assignments) {
    # Determine snapshot iterations: 0, snapshot_interval, 2*snapshot_interval, ..., num_steps
    snapshot_iters <- seq(0, num_steps, by = snapshot_interval)
    if (snapshot_iters[length(snapshot_iters)] != num_steps) {
      snapshot_iters <- c(snapshot_iters, num_steps)
    }
    
    # Extract snapshot columns (add 1 because matrix is 0-indexed in colnames but 1-indexed in R)
    snapshot_cols <- snapshot_iters + 1
    snapshot_assignments <- assignments_all[, snapshot_cols, drop = FALSE]
    
    output_df <- as.data.frame(snapshot_assignments)
    colnames(output_df) <- paste0("iteration_", snapshot_iters)
    output_df <- data.frame(precinct_ids, output_df, stringsAsFactors = FALSE)
    names(output_df)[1] <- id_col_name
    
    output_file <- file.path(output_dir, paste0("snapshot_assignments_", timestamp, ".csv"))
    write.csv(output_df, output_file, row.names = FALSE, quote = TRUE)
  }
  
  # Save final assignment if requested
  if (save_final_assignment) {
    final_df <- data.frame(
      precinct_ids,
      assignment = assignments_all[, num_steps + 1],
      stringsAsFactors = FALSE
    )
    names(final_df)[1] <- id_col_name
    final_file <- file.path(output_dir, paste0("final_assignment_", timestamp, ".csv"))
    write.csv(final_df, final_file, row.names = FALSE, quote = TRUE)
  }
  
  # Create metrics dataframe
  metrics_df <- data.frame(
    iteration = 0:num_steps,
    score = scores_all
  )
  
  # Add cut_edges only if not all NA
  if (!all(is.na(cut_edges_all))) {
    metrics_df$cut_edges <- cut_edges_all
  }
  
  # Add county_splits only if not all NA
  if (!all(is.na(county_splits_all))) {
    metrics_df$county_splits <- county_splits_all
  }
  
  # Add district-level partisan data if available
  if (!is.null(dem_share_by_district)) {
    num_districts <- ncol(dem_share_by_district)
    for (d in 1:num_districts) {
      col_name <- paste0("dem_share_d", d)
      metrics_df[[col_name]] <- dem_share_by_district[, d]
    }
  }
  
  metrics_file <- file.path(output_dir, paste0("metrics_", timestamp, ".csv"))
  write.csv(metrics_df, metrics_file, row.names = FALSE, quote = FALSE)
  
  return(timestamp)
}