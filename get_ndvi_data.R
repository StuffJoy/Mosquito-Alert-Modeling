devtools::install_github("Mosquito-Alert/mosquitoR", ref = "feature/modularize-modeling-data")

library(mosquitoR)
library(geodata)
library(terra)
library(zoo)
library(sf)
library(readr)

# Landcover Data

# 1) Read the GeoTIFF
ndvi <- rast("data/NDVI/Spain_Catalu_a_Catalunya_Barcelona_NDVI_2024-01-31_2024-12-31.tif")   # path to your file
plot(ndvi)
lc_sf <- as.polygons(lc, dissolve = FALSE) |> st_as_sf()
ndvi_strip = process_ndvi_data(
  ndvi,
  "esp",
  "4",
  "Barcelona"
)

ndvi_new = rast("data/proc/spatial_esp_4_barcelona_ndvi.tif")
plot(ndvi_new)
plot(ndvi_strip$raster)

summary(ndvi_new)
head(ndvi_new)

r
lc <- terra::rast("data/proc/spatial_esp_4_barcelona_landcover.tif")
names(ndvi_new)          # layer names / column labels
terra::cats(ndvi_new)    # category table if it’s attached
unique(terra::values(ndvi_new[[1]]))  # actual stored codes (sample or wrap in na.rm=TRUE)



build_location_identifiers <- function(iso3, admin_level, admin_name) {
  if (length(iso3) != 1 || nchar(iso3) != 3) {
    stop("`iso3` must be a single three-character ISO3 code.", call. = FALSE)
  }
  
  iso3_slug <- tolower(iso3)
  admin_level_slug <- as.character(admin_level)
  admin_name_slug <- tolower(admin_name)
  admin_name_slug <- gsub("[^a-z0-9]+", "_", admin_name_slug)
  admin_name_slug <- gsub("^_+|_+$", "", admin_name_slug)
  
  list(
    iso3 = iso3_slug,
    admin_level = admin_level_slug,
    admin_name = admin_name_slug,
    slug = paste(iso3_slug, admin_level_slug, admin_name_slug, sep = "_")
  )
}



#' Add high-NDVI proximity metrics to Mosquito Alert model inputs
#'
#' Loads the land-cover enriched model-preparation dataset produced by
#' [add_landcover_features()], identifies raster cells whose NDVI exceeds a
#' supplied threshold, and measures each report's distance to the nearest
#' qualifying cell. Results are saved as `model_prep_*_wx_lc_ndvi.Rds`.
#'
#' @param iso3 Three-letter ISO3 country code.
#' @param admin_level Administrative level used to build the dataset slug.
#' @param admin_name Administrative unit name used in the file naming scheme.
#' @param data_dir Directory containing processed datasets. Defaults to
#'   `"data/proc"`.
#' @param ndvi_threshold Numeric cutoff applied to the NDVI raster. Defaults to
#'   `0.3`.
#' @param decay_alpha Optional exponential decay coefficient used when
#'   deriving proximity; set to `NULL` to skip. Defaults to `0.01`.
#' @param decay_beta Exponent applied to the distance term in the decay
#'   function. Ignored when `decay_alpha` is `NULL`. Defaults to `1`.
#' @param verbose Logical; if `TRUE`, prints progress messages.
#'
#' @return A tibble/data frame with the augmented variables. Attributes from
#'   the source dataset are preserved and augmented with `ndvi_source`,
#'   `ndvi_threshold`, and `output_path` metadata.
#' @export
#' @importFrom terra rast ifel as.points
#' @importFrom sf st_as_sf st_nearest_feature st_distance
add_ndvi_features <- function(
    iso3,
    admin_level,
    admin_name,
    data_dir = "data/proc",
    ndvi_threshold = 0.3,
    decay_alpha = 0.01,
    decay_beta = 1,
    verbose = TRUE
) {
  ids <- build_location_identifiers(iso3, admin_level, admin_name)
  location_slug <- ids$slug
  
  dataset_path <- file.path(data_dir, paste0("model_prep_", location_slug, "_wx_lc.Rds"))
  if (!file.exists(dataset_path)) {
    stop("Land-cover enriched dataset not found at ", dataset_path, call. = FALSE)
  }
  
  ndvi_path <- file.path(data_dir, paste0("spatial_", location_slug, "_ndvi.tif"))
  if (!file.exists(ndvi_path)) {
    stop("NDVI raster not found at ", ndvi_path, call. = FALSE)
  }
  
  if (isTRUE(verbose)) message("Reading land-cover dataset from ", dataset_path)
  enriched <- readRDS(dataset_path)
  base_attrs <- attributes(enriched)
  
  if (!is.null(decay_alpha)) {
    if (!is.numeric(decay_alpha) || length(decay_alpha) != 1 || is.na(decay_alpha)) {
      stop("`decay_alpha` must be a single numeric value or NULL.", call. = FALSE)
    }
    if (!is.numeric(decay_beta) || length(decay_beta) != 1 || is.na(decay_beta)) {
      stop("`decay_beta` must be a single numeric value.", call. = FALSE)
    }
  }
  
  if (!all(c("lon", "lat") %in% names(enriched))) {
    stop("Input dataset must contain `lon` and `lat` columns.", call. = FALSE)
  }
  
  valid_points <- stats::complete.cases(enriched[c("lon", "lat")])
  if (!any(valid_points)) {
    stop("No valid lon/lat coordinates available to compute NDVI distances.", call. = FALSE)
  }
  
  if (isTRUE(verbose)) message("Loading NDVI raster from ", ndvi_path)
  ndvi_raster <- terra::rast(ndvi_path)
  
  if (isTRUE(verbose)) {
    message("Filtering NDVI values using threshold >= ", format(ndvi_threshold))
  }
  ndvi_filtered <- terra::ifel(ndvi_raster >= ndvi_threshold, ndvi_raster, NA_real_)
  
  ndvi_points <- terra::as.points(ndvi_filtered, na.rm = TRUE)
  if (terra::nrow(ndvi_points) == 0L) {
    stop("No NDVI pixels met the threshold of ", ndvi_threshold, ".", call. = FALSE)
  }
  
  ndvi_points_sf <- sf::st_as_sf(ndvi_points)
  
  points_sf <- sf::st_as_sf(enriched[valid_points, , drop = FALSE], coords = c("lon", "lat"), crs = terra::crs(ndvi_raster), remove = FALSE)
  
  nearest_idx <- sf::st_nearest_feature(points_sf, ndvi_points_sf)
  nearest_points <- ndvi_points_sf[nearest_idx, , drop = FALSE]
  
  distances <- sf::st_distance(points_sf, nearest_points, by_element = TRUE)
  distances_numeric <- as.numeric(distances)
  
  ndvi_col <- names(nearest_points)[1]
  ndvi_values <- as.numeric(nearest_points[[ndvi_col]])
  
  ndvi_distance_full <- rep(NA_real_, nrow(enriched))
  ndvi_distance_full[valid_points] <- distances_numeric
  
  ndvi_value_full <- rep(NA_real_, nrow(enriched))
  ndvi_value_full[valid_points] <- ndvi_values
  
  enriched$ndvi_distance_m <- ndvi_distance_full
  enriched$ndvi_value_nearest <- ndvi_value_full
  
  if (!is.null(decay_alpha)) {
    decay_full <- rep(NA_real_, nrow(enriched))
    decay_full[valid_points] <- exp(-decay_alpha * (distances_numeric ^ decay_beta))
    enriched$ndvi_ddf_proximity <- decay_full
  }
  
  enriched_out <- enriched
  
  output_filename <- paste0("model_prep_", location_slug, "_wx_lc_ndvi.Rds")
  output_path <- file.path(data_dir, output_filename)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  
  preserve <- base_attrs[setdiff(names(base_attrs), c("names", "row.names", "class"))]
  for (nm in names(preserve)) {
    attr(enriched_out, nm) <- preserve[[nm]]
  }
  
  attr(enriched_out, "ndvi_source") <- ndvi_path
  attr(enriched_out, "ndvi_threshold") <- ndvi_threshold
  attr(enriched_out, "output_path") <- output_path
  
  if (isTRUE(verbose)) message("Saving NDVI-enriched dataset to ", output_path)
  saveRDS(enriched_out, output_path)
  
  enriched_out
}


xxx = add_ndvi_features("esp",
    "4",
    "Barcelona")




#' Process NDVI raster by clipping to an administrative boundary
#'
#' Loads an NDVI raster (e.g., a Sentinel-2 composite), aligns it with a target
#' boundary, crops and masks pixels outside the polygon, writes the masked
#' output if requested, and computes simple global summary statistics.
#'
#' @param ndvi A `terra::SpatRaster` or path to an NDVI GeoTIFF file.
#' @param iso3 Three-letter ISO3 country code used to locate the boundary file.
#' @param admin_level Administrative level associated with the boundary.
#' @param admin_name Administrative unit name used in the file naming scheme.
#' @param write_raster Logical. If `TRUE`, save the masked raster via
#'   [terra::writeRaster()].
#' @param write_stats Logical. If `TRUE`, persist the summary statistics to
#'   disk (RDS).
#' @param proc_dir Directory used for any outputs. Created when missing.
#' @param datatype GDAL datatype passed to [terra::writeRaster()]. Defaults to
#'   `"FLT4S"` for floating-point NDVI values.
#' @param verbose Logical. If `TRUE`, prints progress messages.
#'
#' @return A list with the masked NDVI raster (`raster`) and a tibble of
#'   summary statistics (`stats`).
#' @export
#' @importFrom terra rast crop mask writeRaster global
#' @importFrom sf st_transform st_make_valid
#' @importFrom tibble tibble
#' @importFrom readr write_rds
process_ndvi_data <- function(
    ndvi,
    iso3,
    admin_level,
    admin_name,
    write_raster = TRUE,
    write_stats = TRUE,
    proc_dir = "data/proc",
    datatype = "FLT4S",
    verbose = TRUE
) {
  ids <- build_location_identifiers(iso3, admin_level, admin_name)
  location_slug <- ids$slug
  
  boundary_path <- file.path(proc_dir, paste0("spatial_", location_slug, "_adm.Rds"))
  if (!file.exists(boundary_path)) {
    stop(
      "Boundary file not found at ", boundary_path,
      ". Generate it first with the spatial grid tooling.",
      call. = FALSE
    )
  }
  if (isTRUE(verbose)) message("Loading boundary from ", boundary_path)
  boundary <- readRDS(boundary_path)
  
  if (!inherits(boundary, "sf")) {
    stop("`boundary` must be an sf object. Did you call get_gadm_data()?", call. = FALSE)
  }
  
  boundary <- sf::st_make_valid(boundary)
  
  
  ndvi_raster <- if (inherits(ndvi, "SpatRaster")) {
    ndvi
  } else if (is.character(ndvi) && length(ndvi) == 1) {
    if (!file.exists(ndvi)) {
      stop("NDVI file does not exist: ", ndvi)
    }
    terra::rast(ndvi)
  } else {
    stop("`ndvi` must be a terra::SpatRaster or a path to a raster file.")
  }
  
  if (!inherits(ndvi_raster, "SpatRaster")) {
    stop("Unable to create a SpatRaster from `ndvi`.")
  }
  
  if (isTRUE(verbose)) message("Aligning CRS between NDVI raster and boundary ...")
  boundary_aligned <- sf::st_transform(boundary, terra::crs(ndvi_raster))
  
  if (isTRUE(verbose)) message("Cropping NDVI raster to boundary extent ...")
  ndvi_cropped <- terra::crop(ndvi_raster, boundary_aligned)
  
  if (isTRUE(verbose)) message("Masking NDVI raster to boundary ...")
  ndvi_masked <- terra::mask(ndvi_cropped, boundary_aligned)
  
  if (terra::nlyr(ndvi_masked) > 1L) {
    warning("NDVI raster has multiple bands; processing all bands together.")
  }
  
  summarise_vals <- function(vals) {
    vals <- vals[!is.na(vals)]
    if (!length(vals)) {
      return(c(mean = NA_real_, sd = NA_real_, min = NA_real_, max = NA_real_, pixels = 0))
    }
    c(
      mean = mean(vals),
      sd = stats::sd(vals),
      min = min(vals),
      max = max(vals),
      pixels = length(vals)
    )
  }
  
  stats_raw <- terra::global(ndvi_masked, fun = summarise_vals)
  stats_tbl <- as.data.frame(stats_raw)
  stats_tbl <- tibble::tibble(
    mean = stats_tbl$mean,
    sd = stats_tbl$sd,
    min = stats_tbl$min,
    max = stats_tbl$max,
    pixels = stats_tbl$pixels
  )
  
  if (isTRUE(write_raster) || isTRUE(write_stats)) {
    dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  if (isTRUE(write_raster)) {
    output_path <- file.path(proc_dir, paste0("spatial_", location_slug, "_ndvi.tif"))
    if (is.null(datatype)) {
      terra::writeRaster(ndvi_masked, output_path, overwrite = TRUE)
    } else {
      terra::writeRaster(ndvi_masked, output_path, datatype = datatype, overwrite = TRUE)
    }
    if (isTRUE(verbose)) message("Saved masked NDVI raster to ", output_path)
  }
  
  if (isTRUE(write_stats)) {
    stats_path <- file.path(proc_dir, paste0("spatial_", location_slug, "_ndvi_stats.rds"))
    readr::write_rds(stats_tbl, stats_path)
    if (isTRUE(verbose)) message("Saved NDVI summary statistics to ", stats_path)
  }
  
  list(raster = ndvi_masked, stats = stats_tbl)
}
