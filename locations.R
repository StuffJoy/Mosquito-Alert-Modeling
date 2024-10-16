#https://public.opendatasoft.com/explore/dataset/geonames-all-cities-with-a-population-1000/table/?disjunctive.cou_name_en&sort=name
install.packages("devtools")
install.packages("sf")
install.packages("dplyr")
devtools::install_github("Mosquito-Alert/mosquitoR")

library(dplyr)
library(sf)
library(mosquitoR)

malerts_reports_github = get_malert_data(source = "github")

spain_data <- malerts_reports_github %>%
  filter(country == "ESP")

colnames(spain_data)

# Load polygon shapefile
polygon_sf <- st_read("gadm41_ESP_shp/gadm41_ESP_4.shp")


spain_sf <- st_as_sf(spain_data, 
                     coords = c("lon", "lat"), # your lat/long column names
                     crs = 4326)

# Perform spatial join or intersection to extract polygon data based on points

extracted_polygons <- st_join(spain_sf,polygon_sf)

colnames(extracted_polygons)

grouped_counts <- extracted_polygons %>%
  group_by(NAME_4) %>%
  summarise(count = n())

