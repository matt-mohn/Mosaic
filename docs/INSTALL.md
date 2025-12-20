# Mosaic Installation Guide

**Note:** Mosaic is easiest for users with experience in R and RStudio. If you're new to R, I recommend familiarizing yourself with the basics before diving into Mosaic.

---

## Requirements

- **R** ≥ 4.0
- **R Packages**: `sf`, `tidyverse`, `igraph`, `readr`, `ggplot2`, `dplyr`, `shadowtext`, `scales`, `magick`
- **System**: GDAL, GEOS, PROJ (usually installed automatically with `sf`)

---

## Installation

The following code chunks can be run from a `.R` file you create in the folder where you downloaded Mosaic.

### 1. Install R Packages

```r
install.packages(c("sf", "tidyverse", "igraph", "readr", 
                   "ggplot2", "dplyr", "shadowtext", "scales", "magick"))
```

### 2. Download Mosaic

**Option 1: Clone with Git**

```bash
git clone https://github.com/matt-mohn/Mosaic.git
cd Mosaic
```

**Option 2: Download ZIP**

1. Go to the [Mosaic GitHub repository](https://github.com/matt-mohn/Mosaic)
2. Click the green "Code" button
3. Click "Download ZIP"
4. Extract the ZIP file to your desired location

### 3. Load Mosaic

```r
setwd("path/to/mosaic")
source("mosaic.R")
load_mosaic()
```

You should see confirmation that all components loaded successfully.

### 4. Test It

Let's run the first example from the [North Carolina Examples tutorial](tutorials/nc-examples.md). This will run an extremely brief, simple 14-district map optimizing for compactness:

```r
# Run a quick 500-step optimization
results <- run_chain(
  shapefile_path = "shapefiles/North_Carolina_Simplified.shp",
  num_districts = 14,
  num_steps = 500,
  verbose_initialization = TRUE,
  seed = 123456
)

# Visualize the result
p <- mosaic_plot(
  shapefile_path = "shapefiles/North_Carolina_Simplified.shp",
  type = "simple",
  border_outline = FALSE,
  district_outline = FALSE,
  
)

p
```

You should see console output as the annealing runs, and finally, a map of the state divided into 14 colored districts. 

---

## Platform-Specific Notes

### macOS

The `sf` package requires spatial libraries. If installation fails:

```bash
brew install gdal geos proj
```

Then reinstall `sf` in R.

### Windows

The `sf` package typically works out of the box. If you encounter issues, install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) and try again.

---

## Troubleshooting

**`sf` won't install:** Install system spatial libraries (GDAL, GEOS, PROJ). On macOS use Homebrew as shown above.

**"No permission to create directories":** Ensure you have write permissions in your mosaic folder.

**Very slow performance:** On first run with a new shapefile, Mosaic builds a graph cache (saved as `.rds` file) which takes a minute or two. This cache stores the shapefile data and the adjacency graph showing which precincts touch each other. Subsequent runs are much faster because Mosaic already knows to look for a cache with the same name as your shapefile.

---

## Next Steps

**Tutorials:**
- **[Mosaic in Action: North Carolina Examples](tutorials/nc-examples.md)** - Seven comprehensive examples covering:

- **[Creating Animated GIFs](tutorials/gif-examples.md)** - Learn to create GIF animations showing how districts evolve during optimization

**Reference Guides:**
- **[Plot Guide](guides/plotting.md)** - Complete reference for `mosaic_plot()`, `mosaic_gif()`, and `mosaic_partisan_plot()`
- **[Scoring & Optimization](guides/scoring.md)** - Deep dive into `run_chain()` parameters and how to tune your redistricting objectives
- **[How Mosaic Works](guides/algorithm.md)** - Understanding the ReCom algorithm and simulated annealing