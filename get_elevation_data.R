library(geodata)

# Elevation Data
elevation_data <- geodata::elevation_30s(country="ESP", path="data/")

plot(elevation_data)


library(terra)
library(geodata)

# 1) Elevation for Spain
elevation_data <- geodata::elevation_30s(country="ESP", path="data/")

# 2) Get GADM boundaries (level 1 = provinces, level 2 = municipalities)
esp_admin <- geodata::gadm("ESP", level = 2, path="data/")

# 3) Extract Barcelona municipality polygon
barcelona <- esp_admin[esp_admin$NAME_2 == "Barcelona", ]

# 4) Crop and mask the elevation raster
barcelona_elev <- crop(elevation_data, barcelona) |> mask(barcelona)

# 5) Plot
plot(barcelona_elev, main = "Elevation in Barcelona")
lines(barcelona, col = "red")



####

install.packages("elevatr")

library(terra); library(geodata); 
library(elevatr); 
library(tmap)

# Barcelona province polygon
esp_l1 <- geodata::gadm("ESP", level=1, path="data/")
barca  <- esp_l1[esp_l1$NAME_1 == "Barcelona", ]

barca_sf <- sf::st_as_sf(barca)

dem_hr <- elevatr::get_elev_raster(
  loc  = barca_sf,
  z    = 11,             # zoom level ~30 m res
  clip = "locations"
)


# Get ~30 m DEM clipped to Barcelona (zoom 11–12 ≈ 30 m; tweak if needed)
dem_hr <- elevatr::get_elev_raster(loc = vect(barca), z = 11, clip = "locations")
dem_hr <- rast(dem_hr)  # to terra SpatRaster

# Project to UTM 31N for correct meters & hillshade
dem_hr <- project(dem_hr, "EPSG:25831")
dem_hr <- mask(crop(dem_hr, vect(barca)), vect(barca))

# (Optional) Light smoothing to reduce tile seams
# dem_hr <- focal(dem_hr, w=3, fun="mean", na.policy="omit")

# Hillshade
slope  <- terrain(dem_hr, v="slope", unit="radians")
aspect <- terrain(dem_hr, v="aspect", unit="radians")
hs     <- shade(slope, aspect, angle=45, direction=315)

# Map
tmap_mode("plot")
m <- tm_shape(hs) + tm_raster(palette=gray.colors(256), legend.show=FALSE) +
  tm_shape(dem_hr) + tm_raster(alpha=0.55, style="cont", n=16, palette=terrain.colors(16),
                               title="Elevation (m)") +
  tm_shape(barca) + tm_borders(lwd=1.2, col="white") +
  tm_layout(title="Barcelona — High-res Elevation (≈30 m)", frame=FALSE)

m
tmap_save(m, "barcelona_elevation_30m_hillshade.png", dpi=400, width=2800, height=3000, units="px")
