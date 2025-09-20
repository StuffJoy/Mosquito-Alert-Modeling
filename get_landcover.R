library(geodata)
library(terra)
library(zoo)


# Landcover Data

# 1) Read the GeoTIFF
lc <- rast("data/landcover/Spain_Catalu_a_Catalunya_Barcelona_WorldCover_2021.tif")   # path to your file
plot(lc)
#> SpatRaster with 1 layer named "Map" (ESA WorldCover class codes)

# 2) Define class codes, names, and colors (matching ESA WorldCover)
codes <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 100)
classes <- c("Tree cover","Shrubland","Grassland","Cropland","Built-up",
             "Bare/sparse","Snow/ice","Water","Herbaceous wetland",
             "Mangroves","Moss/lichen")
cols <- c("#006400","#ffbb22","#ffff4c","#f096ff","#fa0000",
          "#b4b4b4","#f0f0f0","#0064c8","#0096a0","#00cf75","#fae6a0")

# 3) Attach a raster attribute table (so plots show class names)
rat <- data.frame(ID = codes, class = classes)
levels(lc) <- rat

# 4) Quick look
plot(lc, col = cols, type = "classes", plg = list(title = "ESA WorldCover 2021"))

# 5) Basic counts by class (pixels)
freq(lc)  # table of code -> pixel count present in your raster

# 6) Area by class (hectares) computed in R (optional; you already exported a CSV)
#    Uses pixel area from the raster's CRS/resolution
ha_per_cell <- prod(res(lc)) * 111320 * 111320 / 10000
# ^ ONLY if lc is in degrees; but your export used EPSG:4326, so this would be rough.
# A better way: reproject to an equal-area CRS first, then sum pixelArea.
lc_eq <- project(lc, "EPSG:6933")  # World Cylindrical Equal Area
cell_ha <- cellSize(lc_eq, unit = "ha")
#area_tab <- as.data.frame.zoo(zonal(cell_ha, lc_eq, "sum"))
area_tab <- as.data.frame(zonal(cell_ha, lc_eq, "sum"))
colnames(area_tab) <- c("class_code","area_ha")
merge(area_tab, data.frame(class_code = codes, class_name = classes), by = "class_code", all.x = TRUE)


# Core metadata
meta <- list(
  file       = sources(lc),
  layers     = nlyr(lc),
  names      = names(lc),
  dims       = c(nrow = nrow(lc), ncol = ncol(lc), ncell = ncell(lc)),
  extent     = ext(lc),
  resolution = res(lc),           # pixel size (units depend on CRS)
  crs        = crs(lc),           # coordinate reference system
  is_lonlat  = is.lonlat(lc),     # TRUE if degrees (EPSG:4326, etc.)
  datatype   = datatype(lc),
  minmax     = global(lc, c("min","max"), na.rm = TRUE)
)
meta

# Class codes present and their counts (fast)
freq(lc)  # value -> pixel count


