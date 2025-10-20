devtools::install_github("Mosquito-Alert/mosquitoR", ref = "feature/modularize-modeling-data")

library(mosquitoR)
library(sf)
library(readr)
library(dplyr)

source('helpers/parameters.R')

# Administrative Units

bcn_map <- get_gadm_data("ESP", level = 4, perimeter = FALSE, rds = FALSE)
summary(bcn_map)

lvl4 = bcn_map$NAME_4
summary(lvl4)
list(lvl4)

#perim_sf <- get_adm_perimeter(bcn_map, output_filename = "perim_barcelona.rds")

bcn_map = read_rds("data/proc/spatial_esp_4_barcelona_adm.rds") 
bcn_perimeter = read_rds("data/proc/spatial_esp_4_barcelona_perimeter.rds")

# Grid

bcn_grid <- build_tigacell_grid(iso3 = target_country_iso3, gadm_level = target_level, admin_name = target_city, cellsize_deg = 0.025)
bcn_grid = read_rds("data/proc/spatial_esp_4_barcelona_grid.rds")
bcn_cells = read_rds("data/proc/spatial_esp_4_barcelona_cells.rds")



# landcover

lc <- rast("data/landcover/Spain_Catalu_a_Catalunya_Barcelona_WorldCover_2021.tif")   # path to your file

lc = process_landcover_data(
  lc,
  bcn_map,
  write_raster= TRUE,
  output_filename = "spatial_esp_4_barcelona_landcover.tif",
  proc_dir = "data/proc",
  verbose = TRUE
)

bcn_landcover <- rast("data/proc/spatial_esp_4_barcelona_landcover.tif")  

# Elevation data
bcn_elevation <- get_elevation_data("ESP", level = 4, name_value = "Barcelona")
bcn_elevation = read_rds("data/proc/spatial_esp_4_barcelona_grid.tif")
plot(bcn_elevation)
