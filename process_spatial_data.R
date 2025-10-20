library(geodata)
library(terra)
library(zoo)
library(sf)
library(dplyr)
library(mosquitoR)
library(data.table)

source("helpers/parameters.R")
# Landcover Data


level4_test = mosquitoR::get_gadm_names(country = "ESP", level = 4, view = "table")

# 1) Read the GeoTIFF
lc <- rast("data/landcover/Spain_Catalu_a_Catalunya_Barcelona_WorldCover_2021.tif")   # path to your file
plot(lc)
# Administrative Data 
bcn_map <- get_gadm_data(target_country_iso3, name=target_city, rds = FALSE, level = target_level, perimeter = FALSE)
plot(bcn_map)
# Elevation data
elavation_data <- get_elevation_data("ESP", level = 4, name_value = "Barcelona")

# align CRS
bcn_map <- sf::st_transform(bcn_map, crs(lc))

# crop (reduce raster extent to bounding box)
lc_crop <- terra::crop(lc, bcn_map)
plot(lc_crop)

# mask (keep only pixels inside boundary)
lc_bcn <- terra::mask(lc_crop, bcn_map)
plot(lc_bcn)

lc_bcn_sf <- as.polygons(lc_bcn, dissolve = FALSE) |> st_as_sf()
lc_points <- as.points(lc_bcn) |> st_as_sf()
plot(lc_bcn_sf)
lc_bcn_sf <- lc_bcn_sf %>%
  mutate(
    class_name = case_when(
      Map == 10  ~ "Tree cover",
      Map == 20  ~ "Shrubland",
      Map == 30  ~ "Grassland",
      Map == 40  ~ "Cropland",
      Map == 50  ~ "Built-up",
      Map == 60  ~ "Bare/sparse",
      Map == 70  ~ "Snow/ice",
      Map == 80  ~ "Water",
      Map == 90  ~ "Herbaceous wetland",
      Map == 95  ~ "Mangroves",
      Map == 100 ~ "Moss/lichen",
      TRUE       ~ "Unknown"
    )
  )
plot(lc_bcn_sf)
bcn_grid <- build_tigacell_grid(iso3 = target_country_iso3, gadm_level = target_level, admin_name = target_city, cellsize_deg = 0.025)
plot(bcn_grid)

bcn_cells = bcn_grid$TigacellID
View(bcn_cells)




write_rds(elavation_data, "data/proc/elavation_data.Rds")

write_rds(lc_bcn_sf, "data/proc/lc_bcn_sf.Rds")

write_rds(bcn_grid, "data/proc/bcn_grid.Rds")

write_rds(bcn_cells, "data/proc/bcn_cells.Rds")

write_rds(bcn_map, "data/proc/bcn_map.Rds")