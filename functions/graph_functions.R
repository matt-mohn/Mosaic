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

load_or_build_graph <- function(shapefile_path) {
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
    return(list(shp = cached$shp, graph = cached$graph))
  }
  
  cat("Building graph (not in cache)...\n")
  shp <- st_read(shapefile_path, quiet = TRUE)

  # Check for empty shapefile
  if (nrow(shp) == 0) {
    stop("Shapefile contains 0 precincts")
  }

  graph <- build_graph(shp)
  
  cat("Saving to cache:", cache_file, "\n")
  saveRDS(list(shp = shp, graph = graph), cache_file)
  
  return(list(shp = shp, graph = graph))
}