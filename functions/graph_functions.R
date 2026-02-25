build_graph <- function(shp) {
  n <- nrow(shp)
  touches <- st_touches(shp)
  edge_list <- data.frame(from = integer(), to = integer())

  for (i in 1:n) {
    neighbors <- touches[[i]]
    if (length(neighbors) > 0) {
      edge_list <- rbind(edge_list, data.frame(from = i, to = neighbors))
    }
  }

  graph <- graph_from_data_frame(edge_list, directed = FALSE, vertices = 1:n)
  simplify(graph)
}

# ============================================
# POLSBY-POPPER PRECOMPUTATION
# ============================================

precompute_pp_data <- function(shp, touches = NULL) {
  # Precompute edge lengths and exterior boundaries for Polsby-Popper calculation
  # This is expensive (~5 min for 2500 precincts) but only done once per shapefile

  cat("Precomputing Polsby-Popper data (this may take several minutes)...\n")

  # Disable s2 for planar geometry operations
  old_s2 <- sf_use_s2()
  sf_use_s2(FALSE)
  on.exit(sf_use_s2(old_s2))

  n <- nrow(shp)

  # Get touching pairs if not provided
  if (is.null(touches)) {
    touches <- st_touches(shp)
  }

  # Estimate number of edges for preallocation
  n_edges_estimate <- sum(sapply(touches, length)) / 2
  edge_from <- integer(n_edges_estimate)
  edge_to <- integer(n_edges_estimate)
  edge_len <- numeric(n_edges_estimate)
  edge_idx <- 0

  # Compute shared boundary lengths
  t0 <- Sys.time()
  for (i in seq_along(touches)) {
    neighbors <- touches[[i]]
    neighbors <- neighbors[neighbors > i]  # Only process i < j to avoid duplicates

    for (j in neighbors) {
      shared <- tryCatch({
        suppressWarnings(st_intersection(st_geometry(shp)[i], st_geometry(shp)[j]))
      }, error = function(e) NULL)

      if (is.null(shared) || length(shared) == 0) {
        len <- 0
      } else {
        len <- as.numeric(st_length(shared))
        if (length(len) == 0 || is.na(len)) len <- 0
      }

      edge_idx <- edge_idx + 1
      edge_from[edge_idx] <- i
      edge_to[edge_idx] <- j
      edge_len[edge_idx] <- len
    }

    if (i %% 500 == 0) {
      cat(sprintf("  Edge lengths: %d/%d precincts\n", i, n))
    }
  }

  # Trim to actual size
  edge_from <- edge_from[1:edge_idx]
  edge_to <- edge_to[1:edge_idx]
  edge_len <- edge_len[1:edge_idx]

  # Compute precinct areas and total perimeters
  areas <- as.numeric(st_area(shp))
  total_perimeters <- as.numeric(suppressWarnings(
    st_length(st_cast(st_geometry(shp), "MULTILINESTRING"))
  ))

  # Compute shared perimeters per precinct
  shared_perimeters <- numeric(n)
  for (i in seq_along(edge_from)) {
    shared_perimeters[edge_from[i]] <- shared_perimeters[edge_from[i]] + edge_len[i]
    shared_perimeters[edge_to[i]] <- shared_perimeters[edge_to[i]] + edge_len[i]
  }

  # Exterior = total - shared
  exterior_perimeters <- total_perimeters - shared_perimeters

  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("  Polsby-Popper precompute complete: %.1fs (%d edges)\n", elapsed, edge_idx))

  list(
    edge_from = edge_from,
    edge_to = edge_to,
    edge_len = edge_len,
    areas = areas,
    exterior_perimeters = exterior_perimeters
  )
}

load_or_build_graph <- function(shapefile_path, precompute_polsby_popper = FALSE) {
  # Determine cache directory - use mosaic_path if available
  cache_dir <- if (exists("mosaic_path", mode = "function")) {
    mosaic_path("cache")
  } else {
    "cache"
  }

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  shp_basename <- tools::file_path_sans_ext(basename(shapefile_path))
  cache_file <- file.path(cache_dir, paste0("cache_", shp_basename, ".rds"))

  if (file.exists(cache_file)) {
    cat("Loading cached graph from:", cache_file, "\n")
    cached <- readRDS(cache_file)

    # Check if PP data is needed but not cached
    if (precompute_polsby_popper && is.null(cached$pp_data)) {
      cat("Polsby-Popper data not in cache, computing...\n")
      pp_data <- precompute_pp_data(cached$shp)
      cached$pp_data <- pp_data
      saveRDS(cached, cache_file)
      cat("Updated cache with PP data\n")
    }

    return(list(shp = cached$shp, graph = cached$graph, pp_data = cached$pp_data))
  }

  cat("Building graph (not in cache)...\n")
  shp <- st_read(shapefile_path, quiet = TRUE)

  # Check for empty shapefile
  if (nrow(shp) == 0) {
    stop("Shapefile contains 0 precincts")
  }

  graph <- build_graph(shp)

  # Check for disconnected graph
  if (!is_connected(graph)) {
    components <- components(graph)
    largest <- which.max(components$csize)
    disconnected_count <- sum(components$membership != largest)
    stop(sprintf(
      "Shapefile contains %d disconnected precincts (not touching any others). All precincts must form a single connected region. Check for islands, enclaves, or gaps in your shapefile.",
      disconnected_count
    ))
  }

  # Optionally precompute Polsby-Popper data
  pp_data <- NULL
  if (precompute_polsby_popper) {
    pp_data <- precompute_pp_data(shp)
  }

  cat("Saving to cache:", cache_file, "\n")
  saveRDS(list(shp = shp, graph = graph, pp_data = pp_data), cache_file)

  return(list(shp = shp, graph = graph, pp_data = pp_data))
}