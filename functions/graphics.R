# ============================================
# Mosaic Plot Function
# ============================================

# Minimal validation for graphics input
validate_graphics_shp <- function(shp, csv_geoids = NULL) {
  if (nrow(shp) == 0) stop("Shapefile contains 0 precincts")

  # Normalize CTY if lowercase
  if ("cty" %in% tolower(names(shp)) && !"CTY" %in% names(shp)) {
    idx <- which(tolower(names(shp)) == "cty")
    names(shp)[idx] <- "CTY"
  }

  # Check GEOID column exists
  id_col <- NULL
  for (col in c("GEOID20", "GEOID", "PRECINCT", "PCT")) {
    if (col %in% names(shp)) { id_col <- col; break }
  }
  if (is.null(id_col)) stop("Shapefile missing ID column (need GEOID20, GEOID, PRECINCT, or PCT)")

  # Check CSV GEOID match if provided
  if (!is.null(csv_geoids)) {
    shp_geoids <- as.character(shp[[id_col]])
    csv_geoids <- as.character(csv_geoids)
    missing <- setdiff(csv_geoids, shp_geoids)
    extra <- setdiff(shp_geoids, csv_geoids)
    if (length(missing) > 0 || length(extra) > 0) {
      stop(sprintf("GEOID mismatch: %d in CSV not in shapefile, %d in shapefile not in CSV",
                   length(missing), length(extra)))
    }
  }

  return(shp)
}

DISTRICT_COLORS <- c(
  "#b86e6e", "#6e6ec2", "#bbffad", "#ff6e6e", "#ffe86e",
  "#6eb7b7", "#e7ac80", "#aca3e5", "#6effff", "#ff79c2",
  "#b6ff6e", "#a7c3f5", "#f2c3b3", "#b9b96f", "#ffbe6e",
  "#6eff6e", "#9791bd", "#ffff6e", "#c6e38a", "#ffdbe1",
  "#b76e6e", "#ca9d88", "#b8ffea", "#996eb8", "#ebaec2",
  "#6e6eb7", "#6effb6", "#ecc9ec", "#bfd9bf", "#f6b7b7",
  "#f6f1be", "#ff956e", "#93aaee", "#8ae38a", "#c58ae2",
  "#ffcc6e", "#7fc0ff", "#a8e3cf", "#fff0d5", "#d38181",
  "#d2ddec", "#b5fd6e", "#ff6eff", "#df7aba", "#92ede4",
  "#ffa696", "#e9dbe9", "#f9cba5", "#e3a2a2", "#d2edf1"
)

PARTISAN_COLORS <- c(
  "#A80000", "#C21B18", "#D72F30", "#D75D5D", "#E27F7F", "#FFB2B2",
  "#D3D9FF", "#7996E2", "#6674DE", "#584CDE", "#3933E5", "#252198"
)

PARTISAN_BREAKS <- c(0, 0.1, 0.2, 0.3, 0.4, 0.45, 0.5, 0.55, 0.6, 0.7, 0.8, 0.9)

mosaic_plot <- function(shapefile_path, csv_path = NULL, metrics_path = NULL,
                        output_dir = "output", type = "simple",
                        border_outline = FALSE, district_outline = FALSE, 
                        county_outline = FALSE, number_labels = FALSE,
                        precinct_outline = FALSE,
                        bunking_lists = NULL,
                        title = "", subtitle = "", caption = "") {
  
  
  if (is.null(csv_path)) {
    csv_files <- list.files(output_dir, "^final_assignment.*\\.csv$", full.names = TRUE)
    csv_path <- csv_files[which.max(file.info(csv_files)$mtime)]
    cat("Using most recent assignment CSV:", csv_path, "\n")
  }
  
  cat("Loading data...\n")
  csv_data <- read_csv(csv_path, col_types = "cd", show_col_types = FALSE)
  map_data <- read_sf(shapefile_path)

  # Detect ID column from CSV (first column)
  csv_id_col <- names(csv_data)[1]
  map_data <- validate_graphics_shp(map_data, csv_data[[csv_id_col]])

  # Detect ID column from shapefile for join
  shp_id_col <- NULL
  for (col in c("GEOID20", "GEOID", "PRECINCT", "PCT")) {
    if (col %in% names(map_data)) { shp_id_col <- col; break }
  }
  if (is.null(shp_id_col)) shp_id_col <- csv_id_col

  # Ensure both are character for reliable join
  csv_data[[csv_id_col]] <- as.character(csv_data[[csv_id_col]])
  map_data[[shp_id_col]] <- as.character(map_data[[shp_id_col]])

  # Join with explicit column mapping
  map_data <- left_join(map_data, csv_data, by = setNames(csv_id_col, shp_id_col))

  # Validate join succeeded
  if (all(is.na(map_data$assignment))) {
    stop("Join failed: no matching IDs between shapefile and CSV. Check that you're using the same shapefile that was used for run_chain().")
  }
  na_count <- sum(is.na(map_data$assignment))
  if (na_count > 0) {
    warning(sprintf("%d precincts have no assignment (IDs not found in CSV)", na_count))
  }

  num_districts <- max(map_data$assignment, na.rm = TRUE)
  
  # Build base map
  if (type == "simple") {
    colors <- DISTRICT_COLORS[((1:num_districts - 1) %% 50) + 1]
    names(colors) <- as.character(1:num_districts)
    
    cat("Building base map (simple)...\n")
    p <- ggplot() +
      geom_sf(data = map_data, aes(fill = factor(assignment)), color = NA) +
      scale_fill_manual(values = colors, name = "District") +
      theme_void() +
      theme(legend.position = "none")
    
  } else if (type == "partisan") {
    if (is.null(metrics_path)) {
      metrics_files <- list.files(output_dir, "^metrics.*\\.csv$", full.names = TRUE)
      metrics_path <- metrics_files[which.max(file.info(metrics_files)$mtime)]
      cat("Using most recent metrics CSV:", metrics_path, "\n")
    }
    
    cat("Loading metrics for partisan coloring...\n")
    metrics <- read_csv(metrics_path, show_col_types = FALSE)
    
    final_iter <- max(metrics$iteration)
    final_metrics <- metrics[metrics$iteration == final_iter, ]
    
    dem_share_cols <- grep("^dem_share_d", names(final_metrics), value = TRUE)
    
    if (length(dem_share_cols) == 0) {
      cat("WARNING: No partisan data found in metrics file. Reverting to simple mode.\n")
      type <- "simple"
      colors <- DISTRICT_COLORS[((1:num_districts - 1) %% 50) + 1]
      names(colors) <- as.character(1:num_districts)
      
      p <- ggplot() +
        geom_sf(data = map_data, aes(fill = factor(assignment)), color = NA) +
        scale_fill_manual(values = colors, name = "District") +
        theme_void() +
        theme(legend.position = "none")
    } else {
      dem_shares <- as.numeric(final_metrics[1, dem_share_cols])
      
      district_colors <- character(nrow(map_data))
      for (d in 1:num_districts) {
        share <- dem_shares[d]
        # Bounds check for color index
        if (is.na(share)) {
          color <- "#CCCCCC"  # Gray for missing data
        } else {
          share <- pmax(0, pmin(1, share))  # Clamp to [0,1]
          color_idx <- max(which(PARTISAN_BREAKS <= share))
          color <- PARTISAN_COLORS[color_idx]
        }
        district_colors[map_data$assignment == d] <- color
      }
      
      map_data$fill_color <- district_colors
      
      cat("Building base map (partisan)...\n")
      p <- ggplot() +
        geom_sf(data = map_data, aes(fill = fill_color), color = NA) +
        scale_fill_identity() +
        theme_void() +
        theme(legend.position = "none")
    }
  }
  
  # Add precinct boundaries
  if (precinct_outline) {
    cat("Adding precinct boundaries...\n")
    p <- p + geom_sf(data = map_data, fill = NA, color = alpha("white", 0.15), linewidth = 0.1)
  }
  
  # Add title/subtitle/caption
  if (title != "" || subtitle != "" || caption != "") {
    p <- p + labs(title = title, subtitle = subtitle, caption = caption)
  }
  
  # Add county boundaries
  if (county_outline && "CTY" %in% names(map_data)) {
    cat("Adding county boundaries...\n")
    dissolved_counties <- map_data |>
      group_by(CTY) |>
      summarize(geometry = st_union(geometry), .groups = "drop")
    p <- p + geom_sf(data = dissolved_counties, fill = NA, color = "navy", linewidth = 0.5)
  }
  
  # Pre-dissolve districts if needed for either outline or labels
  dissolved_districts <- NULL
  if (district_outline || number_labels) {
    cat("Dissolving districts...\n")
    dissolved_districts <- map_data |>
      group_by(assignment) |>
      summarize(geometry = st_union(geometry), .groups = "drop")
  }
  
  # Add district boundaries
  if (district_outline) {
    cat("Adding district boundaries...\n")
    p <- p + geom_sf(data = dissolved_districts, fill = NA, color = "black", linewidth = 1)
  }
  
  # Add border outline
  if (border_outline) {
    cat("Adding border outline...\n")
    dissolved_border <- map_data |>
      summarize(geometry = st_union(geometry))
    p <- p + geom_sf(data = dissolved_border, fill = NA, color = "black", linewidth = 1.5)
  }
  
  # Add bunking markers (BEFORE number labels so they're below)
  if (!is.null(bunking_lists)) {
    cat("Adding bunking markers...\n")
    
    # Collect all bunking precincts
    all_bunking_geoids <- character(0)
    bunking_types <- character(0)  # "anti" or "pro"
    
    # Handle raw format (from create_bunking_lists)
    if (inherits(bunking_lists, "bunking_lists_raw")) {
      # Extract GEOIDs from raw vectors
      if (!is.null(bunking_lists$anti_bunking_raw) && length(bunking_lists$anti_bunking_raw) > 0) {
        for (i in seq_along(bunking_lists$anti_bunking_raw)) {
          vec <- bunking_lists$anti_bunking_raw[[i]]
          geoids <- as.character(vec[3:length(vec)])
          all_bunking_geoids <- c(all_bunking_geoids, geoids)
          bunking_types <- c(bunking_types, rep("anti", length(geoids)))
        }
      }
      
      if (!is.null(bunking_lists$pro_bunking_raw) && length(bunking_lists$pro_bunking_raw) > 0) {
        for (i in seq_along(bunking_lists$pro_bunking_raw)) {
          vec <- bunking_lists$pro_bunking_raw[[i]]
          geoids <- as.character(vec[3:length(vec)])
          all_bunking_geoids <- c(all_bunking_geoids, geoids)
          bunking_types <- c(bunking_types, rep("pro", length(geoids)))
        }
      }
    } else {
      # Handle processed format (from chain_runner)
      if (!is.null(bunking_lists$anti_bunking) && length(bunking_lists$anti_bunking) > 0) {
        for (i in seq_along(bunking_lists$anti_bunking)) {
          geoids <- bunking_lists$anti_bunking[[i]]$geoids
          all_bunking_geoids <- c(all_bunking_geoids, geoids)
          bunking_types <- c(bunking_types, rep("anti", length(geoids)))
        }
      }
      
      if (!is.null(bunking_lists$pro_bunking) && length(bunking_lists$pro_bunking) > 0) {
        for (i in seq_along(bunking_lists$pro_bunking)) {
          geoids <- bunking_lists$pro_bunking[[i]]$geoids
          all_bunking_geoids <- c(all_bunking_geoids, geoids)
          bunking_types <- c(bunking_types, rep("pro", length(geoids)))
        }
      }
    }
    
    if (length(all_bunking_geoids) > 0) {
      # Find precincts in map_data
      id_col <- if ("GEOID20" %in% names(map_data)) "GEOID20" else "GEOID"
      
      bunking_precincts <- suppressWarnings(
        map_data |>
          filter(!!sym(id_col) %in% all_bunking_geoids) |>
          st_point_on_surface() |>
          st_coordinates() |>
          as.data.frame()
      )
      
      bunking_precincts$type <- bunking_types[match(
        map_data |> filter(!!sym(id_col) %in% all_bunking_geoids) |> pull(!!sym(id_col)),
        all_bunking_geoids
      )]
      
      
      # Add diamond markers
      p <- p + geom_point(
        data = bunking_precincts,
        aes(x = X, y = Y, shape = type, color = type),
        size = 2,
        fill = "white",  # Inner fill of diamond
        stroke = 1.2
      ) +
        scale_shape_manual(
          values = c("anti" = 23, "pro" = 23),  # 23 = filled diamond
          guide = "none"
        ) +
        scale_color_manual(
          values = c("anti" = "yellow", "pro" = "blue"),
          guide = "none"
        )
    }
  }
  
  # Add district labels LAST so they render on top
  if (number_labels) {
    cat("Adding district labels...\n")
    district_centers <- suppressWarnings(
      dissolved_districts |>
        st_point_on_surface() |>
        st_coordinates() |>
        as.data.frame()
    )
    district_centers$assignment <- dissolved_districts$assignment
    
    p <- p + geom_shadowtext(
      data = district_centers, 
      aes(x = X, y = Y, label = assignment),
      size = 3, color = "white", bg.color = "black", fontface = "bold"
    )
  }
  
  cat("Rendering plot...\n")
  p
}

# ============================================
# Mosaic GIF Function
# ============================================

calculate_stable_color_mapping <- function(initial_assignment, current_assignment, num_districts) {
  # Build overlap matrix: current districts (rows) vs initial districts (cols)
  overlap_matrix <- matrix(0, nrow = num_districts, ncol = num_districts)
  
  for (curr_d in 1:num_districts) {
    precincts_in_curr <- which(current_assignment == curr_d)
    
    if (length(precincts_in_curr) == 0) {
      next  # Empty district - will handle below
    }
    
    # Count overlaps with each initial district
    init_districts <- initial_assignment[precincts_in_curr]
    for (init_d in 1:num_districts) {
      overlap_matrix[curr_d, init_d] <- sum(init_districts == init_d)
    }
  }
  
  # Calculate confidence for each current district
  # Confidence = difference between best and second-best overlap
  confidence <- numeric(num_districts)
  
  for (curr_d in 1:num_districts) {
    overlaps <- overlap_matrix[curr_d, ]
    sorted_overlaps <- sort(overlaps, decreasing = TRUE)
    
    if (sorted_overlaps[1] == 0) {
      # Empty district
      confidence[curr_d] <- -1
    } else {
      confidence[curr_d] <- sorted_overlaps[1] - sorted_overlaps[2]
    }
  }
  
  # Sort districts by confidence (highest first)
  processing_order <- order(confidence, decreasing = TRUE)
  
  # Greedy assignment with conflict resolution
  color_map <- integer(num_districts)
  taken_colors <- logical(num_districts)  # Track which colors are assigned
  
  for (curr_d in processing_order) {
    # Get preference list (sorted by overlap, descending)
    preferences <- order(overlap_matrix[curr_d, ], decreasing = TRUE)
    
    # Find first available color from preference list
    assigned <- FALSE
    for (init_d in preferences) {
      if (!taken_colors[init_d]) {
        color_map[curr_d] <- init_d
        taken_colors[init_d] <- TRUE
        assigned <- TRUE
        break
      }
    }
    
    # Fallback: if all preferences taken (shouldn't happen), assign to self
    if (!assigned) {
      color_map[curr_d] <- curr_d
    }
  }
  
  return(color_map)
}

mosaic_gif <- function(shapefile_path, assignments_csv = NULL, metrics_csv = NULL,
                       output_dir = "output", output_path = NULL,
                       type = "simple", fps = 10,
                       border_outline = FALSE, district_outline = FALSE,
                       county_outline = FALSE, precinct_outline = FALSE,
                       bunking_lists = NULL,
                       width = 500, height = 500) {
  library(sf)
  library(readr)
  library(ggplot2)
  library(dplyr)
  
  # Load assignments
  if (is.null(assignments_csv)) {
    csv_files <- list.files(output_dir, "^snapshot_assignments.*\\.csv$", full.names = TRUE)
    assignments_csv <- csv_files[which.max(file.info(csv_files)$mtime)]
    cat("Using snapshot assignments CSV:", assignments_csv, "\n")
  }
  
  cat("Loading shapefile and assignments...\n")
  shp <- read_sf(shapefile_path)
  assignments_df <- read_csv(assignments_csv, col_types = cols(.default = "c"), show_col_types = FALSE)

  geoid_col <- names(assignments_df)[1]
  shp <- validate_graphics_shp(shp, assignments_df[[geoid_col]])
  iteration_cols <- grep("^iteration_", names(assignments_df), value = TRUE)

  num_iterations <- length(iteration_cols)

  # Detect ID column from shapefile
  shp_id_col <- NULL
  for (col in c("GEOID20", "GEOID", "PRECINCT", "PCT")) {
    if (col %in% names(shp)) { shp_id_col <- col; break }
  }
  if (is.null(shp_id_col)) shp_id_col <- geoid_col

  # Ensure both are character for reliable join
  assignments_df[[geoid_col]] <- as.character(assignments_df[[geoid_col]])
  shp[[shp_id_col]] <- as.character(shp[[shp_id_col]])

  # Join assignments to shapefile by GEOID
  cat("Joining assignments to shapefile by GEOID...\n")
  shp <- shp |>
    left_join(assignments_df, by = setNames(geoid_col, shp_id_col))

  # Validate join succeeded
  first_iter <- iteration_cols[1]
  if (all(is.na(shp[[first_iter]]))) {
    stop("Join failed: no matching IDs between shapefile and CSV. Check that you're using the same shapefile that was used for run_chain().")
  }
  na_count <- sum(is.na(shp[[first_iter]]))
  if (na_count > 0) {
    warning(sprintf("%d precincts have no assignment (IDs not found in CSV)", na_count))
  }

  # Convert iteration columns back to integer after join
  for (col in iteration_cols) {
    shp[[col]] <- as.integer(shp[[col]])
  }
  
  initial_assignment <- shp[[iteration_cols[1]]]
  num_districts <- max(initial_assignment, na.rm = TRUE)
  
  cat(sprintf("Found %d snapshots with %d districts\n", num_iterations, num_districts))
  
  # Pre-dissolve county and border (once)
  dissolved_counties <- NULL
  if (county_outline && "CTY" %in% names(shp)) {
    cat("Pre-dissolving county boundaries...\n")
    dissolved_counties <- shp |>
      group_by(CTY) |>
      summarize(geometry = st_union(geometry), .groups = "drop")
  }
  
  dissolved_border <- NULL
  if (border_outline) {
    cat("Pre-dissolving state border...\n")
    dissolved_border <- shp |>
      summarize(geometry = st_union(geometry))
  }
  
  # Load metrics if partisan
  metrics_df <- NULL
  dem_share_cols <- NULL
  if (type == "partisan") {
    if (is.null(metrics_csv)) {
      metrics_files <- list.files(output_dir, "^metrics.*\\.csv$", full.names = TRUE)
      metrics_csv <- metrics_files[which.max(file.info(metrics_files)$mtime)]
      cat("Using metrics CSV:", metrics_csv, "\n")
    }
    
    metrics_df <- read_csv(metrics_csv, show_col_types = FALSE)
    dem_share_cols <- grep("^dem_share_d", names(metrics_df), value = TRUE)
    
    if (length(dem_share_cols) == 0) {
      cat("WARNING: No partisan data found. Switching to simple mode.\n")
      type <- "simple"
    }
  }
  
  # Setup output
  temp_dir <- file.path("gifs", "temp_frames")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  
  if (is.null(output_path)) {
    timestamp <- format(Sys.time(), "%m%d%Y_%H%M")
    output_path <- file.path("gifs", paste0("mosaic_", type, "_", timestamp, ".gif"))
  }
  
  # Setup colors for simple mode
  colors <- NULL
  if (type == "simple") {
    colors <- DISTRICT_COLORS[((1:num_districts - 1) %% 50) + 1]
    names(colors) <- as.character(1:num_districts)
  }
  
  # Pre-process bunking markers if provided
  bunking_geoids <- NULL
  if (!is.null(bunking_lists)) {
    cat("Pre-processing bunking markers...\n")
    all_bunking_geoids <- character(0)
    all_bunking_types <- character(0)
    
    # Handle raw format (from create_bunking_lists)
    if (inherits(bunking_lists, "bunking_lists_raw")) {
      if (!is.null(bunking_lists$anti_bunking_raw) && length(bunking_lists$anti_bunking_raw) > 0) {
        for (i in seq_along(bunking_lists$anti_bunking_raw)) {
          vec <- bunking_lists$anti_bunking_raw[[i]]
          geoids <- as.character(vec[3:length(vec)])
          all_bunking_geoids <- c(all_bunking_geoids, geoids)
          all_bunking_types <- c(all_bunking_types, rep("anti", length(geoids)))
        }
      }
      
      if (!is.null(bunking_lists$pro_bunking_raw) && length(bunking_lists$pro_bunking_raw) > 0) {
        for (i in seq_along(bunking_lists$pro_bunking_raw)) {
          vec <- bunking_lists$pro_bunking_raw[[i]]
          geoids <- as.character(vec[3:length(vec)])
          all_bunking_geoids <- c(all_bunking_geoids, geoids)
          all_bunking_types <- c(all_bunking_types, rep("pro", length(geoids)))
        }
      }
    } else {
      # Handle processed format
      if (!is.null(bunking_lists$anti_bunking) && length(bunking_lists$anti_bunking) > 0) {
        for (i in seq_along(bunking_lists$anti_bunking)) {
          geoids <- bunking_lists$anti_bunking[[i]]$geoids
          all_bunking_geoids <- c(all_bunking_geoids, geoids)
          all_bunking_types <- c(all_bunking_types, rep("anti", length(geoids)))
        }
      }
      
      if (!is.null(bunking_lists$pro_bunking) && length(bunking_lists$pro_bunking) > 0) {
        for (i in seq_along(bunking_lists$pro_bunking)) {
          geoids <- bunking_lists$pro_bunking[[i]]$geoids
          all_bunking_geoids <- c(all_bunking_geoids, geoids)
          all_bunking_types <- c(all_bunking_types, rep("pro", length(geoids)))
        }
      }
    }
    
    if (length(all_bunking_geoids) > 0) {
      id_col <- if ("GEOID20" %in% names(shp)) "GEOID20" else "GEOID"
      
      bunking_precincts <- suppressWarnings(
        shp |>
          filter(!!sym(id_col) %in% all_bunking_geoids) |>
          st_point_on_surface() |>
          st_coordinates() |>
          as.data.frame()
      )
      
      bunking_precincts$type <- all_bunking_types[match(
        shp |> filter(!!sym(id_col) %in% all_bunking_geoids) |> pull(!!sym(id_col)),
        all_bunking_geoids
      )]
      
      bunking_geoids <- bunking_precincts
    }
  }
  
  frame_files <- character(num_iterations)
  
  cat(sprintf("Rendering %d frames...\n", num_iterations))
  
  for (i in 1:num_iterations) {
    if (i %% 20 == 0) {
      cat(sprintf("  Frame %d/%d\n", i, num_iterations))
    }
    
    current_assignment <- shp[[iteration_cols[i]]]
    iter_name <- iteration_cols[i]
    step_num <- as.numeric(sub("iteration_", "", iter_name))
    
    # Build base map
    if (type == "simple") {
      # Apply smart renumbering
      color_map <- calculate_stable_color_mapping(initial_assignment, current_assignment, num_districts)
      stable_colors <- color_map[current_assignment]
      
      shp$district <- factor(stable_colors)
      
      p <- ggplot() +
        geom_sf(data = shp, aes(fill = district), color = NA) +
        scale_fill_manual(values = colors) +
        theme_void() +
        theme(legend.position = "none")
      
    } else if (type == "partisan") {
      # Get dem shares for this iteration
      score_idx <- which(metrics_df$iteration == step_num)
      
      district_colors <- character(nrow(shp))
      for (d in 1:num_districts) {
        if (length(score_idx) > 0) {
          dem_share <- metrics_df[[dem_share_cols[d]]][score_idx[1]]
        } else {
          dem_share <- NA
        }
        
        if (is.na(dem_share)) {
          color <- "#CCCCCC"
        } else {
          dem_share <- pmax(0, pmin(1, dem_share))  # Clamp to [0,1]
          color_idx <- max(which(PARTISAN_BREAKS <= dem_share))
          color <- PARTISAN_COLORS[color_idx]
        }

        district_colors[current_assignment == d] <- color
      }
      
      shp$fill_color <- district_colors
      
      p <- ggplot() +
        geom_sf(data = shp, aes(fill = fill_color), color = NA) +
        scale_fill_identity() +
        theme_void() +
        theme(legend.position = "none")
    }
    
    # Add precinct outlines
    if (precinct_outline) {
      p <- p + geom_sf(data = shp, fill = NA, color = alpha("white", 0.2), linewidth = 0.15)
    }
    
    # Add title
    p <- p + labs(title = sprintf("Iteration %d", step_num)) +
      theme(plot.title = element_text(size = 10, hjust = 0.5, margin = margin(t = 5, b = 5)))
    
    # Add county boundaries
    if (!is.null(dissolved_counties)) {
      p <- p + geom_sf(data = dissolved_counties, fill = NA, color = "navy", linewidth = 0.5)
    }
    
    # Add district boundaries
    if (district_outline) {
      dissolved_districts <- shp |>
        mutate(assignment = current_assignment) |>
        group_by(assignment) |>
        summarize(geometry = st_union(geometry), .groups = "drop")
      
      p <- p + geom_sf(data = dissolved_districts, fill = NA, color = "black", linewidth = 1)
    }
    
    # Add border outline
    if (!is.null(dissolved_border)) {
      p <- p + geom_sf(data = dissolved_border, fill = NA, color = "black", linewidth = 2)
    }
    
    # Add bunking markers
    if (!is.null(bunking_geoids)) {
      p <- p + geom_point(
        data = bunking_geoids,
        aes(x = X, y = Y, shape = type, color = type),
        size = 2,
        fill = "white",
        stroke = 1
      ) +
        scale_shape_manual(
          values = c("anti" = 23, "pro" = 23),
          guide = "none"
        ) +
        scale_color_manual(
          values = c("anti" = "yellow", "pro" = "blue"),
          guide = "none"
        )
    }
    
    # Save frame
    frame_file <- file.path(temp_dir, sprintf("frame_%03d.png", i))
    png(frame_file, width = width, height = height, bg = "white")
    print(p)
    dev.off()
    
    frame_files[i] <- frame_file
  }
  
  cat(sprintf("Creating GIF from %d frames...\n", length(frame_files)))
  
  if (requireNamespace("magick", quietly = TRUE)) {
    cat("  Using magick package...\n")
    img <- magick::image_read(frame_files)
    
    n_frames <- length(img)
    delays <- rep(100/fps, n_frames)
    delays[1] <- 200  # 2 seconds
    delays[n_frames] <- 200  # 2 seconds
    
    img <- magick::image_animate(img, fps = fps, delay = delays)
    magick::image_write(img, output_path)
    
  } else {
    cat("  Using ImageMagick convert...\n")
    
    cmd_parts <- c("convert")
    for (i in seq_along(frame_files)) {
      if (i == 1 || i == length(frame_files)) {
        cmd_parts <- c(cmd_parts, "-delay", "200", frame_files[i])
      } else {
        cmd_parts <- c(cmd_parts, "-delay", as.character(100/fps), frame_files[i])
      }
    }
    cmd_parts <- c(cmd_parts, "-loop", "0", output_path)
    
    cmd <- paste(cmd_parts, collapse = " ")
    result <- system(cmd, ignore.stderr = TRUE)
    
    if (result != 0) {
      stop("GIF creation failed with ImageMagick")
    }
  }
  
  cat("Cleaning up temp frames...\n")
  unlink(temp_dir, recursive = TRUE)
  
  cat("Saved:", output_path, "\n")
  invisible(output_path)
}



mosaic_partisan_plot <- function( metrics_path = NULL,
                                  output_dir = "output",
                                  title = "", 
                                  subtitle = "",
                                  caption = "") {
  
  
  cat("Loading data...\n")
  
  if (is.null(metrics_path)) {
    metrics_files <- list.files(output_dir, "^metrics.*\\.csv$", full.names = TRUE)
    metrics_path <- metrics_files[which.max(file.info(metrics_files)$mtime)]
    cat("Using most recent metrics CSV:", metrics_path, "\n")
  }
  
  cat("Loading metrics for partisan plot...\n")
  metrics <- read_csv(metrics_path, show_col_types = FALSE)
  
  final_iter <- max(metrics$iteration)
  final_metrics <- metrics[metrics$iteration == final_iter, ]
  
  dem_share_cols <- grep("^dem_share_d", names(final_metrics), value = TRUE)
  
  if (length(dem_share_cols) == 0) {
    stop("WARNING: No partisan data found in metrics file. Partisan data can't be plotted\n")
    
  } else {
    
    ### build p
    
    df <- metrics |>
      slice_max(iteration, n = 1) |>
      pivot_longer(starts_with("dem_share_d"),
                   names_to = "district",
                   values_to = "dem_share") |>
      arrange(dem_share) |>
      mutate(
        rank = row_number(),
        party = if_else(dem_share >= 0.5, "D", "R"),
        in_comp = dem_share >= 0.45 & dem_share <= 0.55
      )
    
    y_min <- max(min(df$dem_share) - 0.05, 0)
    y_max <- min(max(df$dem_share) + 0.05, 1)
    
    p <- ggplot(df, aes(rank, dem_share)) +
      annotate(
        "rect",
        xmin = -Inf, xmax = Inf,
        ymin = 0.45, ymax = 0.55,
        fill = "#FFF3AA", alpha = 0.6
      ) +
      geom_hline(yintercept = c(0.45, 0.55), linetype = "dashed", color = "lightgray") +
      geom_hline(yintercept = 0.5, color = "black") +
      geom_vline(xintercept = median(df$rank), linetype = "dashed") +
      geom_point(
        aes(color = party, alpha = in_comp),
        shape = 15,
        size = 3
      ) +
      scale_color_manual(
        values = c(D = "blue", R = "red"),
        guide = "none"
      ) +
      scale_alpha_manual(values = c(`TRUE` = 0.7, `FALSE` = 1), guide = "none") +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        limits = c(y_min, y_max)
      ) +
      labs(x = NULL, y = "Dem share",
           title = title,
           subtitle = subtitle,
           caption = caption) +
      theme_minimal() +
      theme(
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    
    
  }
  
  cat("Rendering partisan graph plot...\n")
  p
  
}