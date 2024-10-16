#https://public.opendatasoft.com/explore/dataset/geonames-all-cities-with-a-population-1000/table/?disjunctive.cou_name_en&sort=name
install.packages("devtools")
install.packages("sf")
install.packages("dplyr")
devtools::install_github("Mosquito-Alert/mosquitoR")

library(dplyr)
library(sf)
library(mosquitoR)

malerts_reports_github = get_malert_data(source = "github")

#geo_data <- st_read("Downloads/geonames-all-cities-with-a-population-1000.geojson")

spain_data <- malerts_reports_github %>%
  filter(country == "ESP")

# Define the point with given longitude and latitude
point <- st_point(c(-4.0616770, 36.74924), dim = "XY")

# Create an sf object
point_sf <- st_sfc(point, crs = st_crs(geo_data))


# Calculate distances from the point to all cities in geo_data
distances <- st_distance(point_sf, geo_data$geometry)

# Find the index of the minimum distance
min_index <- which.min(distances)

# Subset the closest city
closest_city <- geo_data[min_index, ]

print(closest_city$name)

malerts_reports_github <- malerts_reports_github %>%
  filter(!is.na(lon) & !is.na(lat))

malerts_reports_github <- st_as_sf(malerts_reports_github, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

geo_data <- st_as_sf(geo_data, wkt = "geometry", crs = 4326)

# Perform a spatial join to find the nearest town for each point in malerts_reports_github
malerts_reports_github <- malerts_reports_github %>%
  st_join(geo_data, join = st_nearest_feature)

# Assuming 'town_name' is the column in 'geo_data' that has the town names
malerts_reports_github$town <- geo_data$name[st_nearest_feature(malerts_reports_github, geo_data)]


town_counts <- malerts_reports_github %>%
  group_by(town) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

bcn_reports <- malerts_reports_github %>%
  filter(town == "Barcelona")

country_counts <- malerts_reports_github %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

esp_reports <- malerts_reports_github %>%
  filter(country == "ESP")




# Load polygon shapefile (e.g., administrative boundaries)

polygon_sf <- st_read("/Downloads/gadm41_ESP_shp/gadm41_ESP_4.shp)



# Load or create point data (e.g., a CSV with coordinates)

points_df <- read.csv("path_to_your_points.csv")

points_sf 
<- st_as_sf(points_df,
            coords 
            = c("longitude",
                "latitude"), 
            crs = 
              4326)



# Perform spatial join or intersection to extract polygon data based on points

extracted_polygons 
<- st_join(points_sf,
           polygon_sf)