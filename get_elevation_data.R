devtools::install_github("Mosquito-Alert/mosquitoR", ref = "feature/modularize-modeling-data")

library(mosquitoR)
library(terra)

dem <- get_elevation_data("BGD", level = 2, name_value = "Dhaka")
plot(dem)


#Basic version of the function above
# library(elevatr)
# library(sf)
# library(terra)
# 
# # Get Barcelona province boundary
# esp <- geodata::gadm("ESP", level = 2, path = "data/")
# barca <- esp[esp$NAME_2 == "Barcelona", ]
# 
# # Convert to sf for elevatr
# barca_sf <- st_as_sf(barca)
# 
# # Download 30m DEM (zoom ~11–12)
# dem_30m <- get_elev_raster(
#   loc = barca_sf,
#   z = 11,                # z=11 ~ 30m
#   clip = "locations"
# )
# 
# # Convert to terra
# dem_30m <- rast(dem_30m)
# plot(dem_30m)

#Option to download directly from geodata (very coarse, 1km)
# library(geodata)
# 
# # Elevation Data
# elevation_data <- geodata::elevation_30s(country="ESP", path="data/")
# 
# plot(elevation_data)