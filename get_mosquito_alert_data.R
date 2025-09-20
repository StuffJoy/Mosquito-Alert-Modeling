library(mosquitoR)
library(dplyr)

# Mosquito Alert Data
malerts_reports_github = mosquitoR::get_malert_data(source = "github")

vector_data <- malerts_reports_github %>%
  filter(country == "ESP")

colnames(vector_data)

#show counts from each country
grouped_countries <- malerts_reports_github %>%
  group_by(country) %>%
  summarise(count = n())



#https://public.opendatasoft.com/explore/dataset/geonames-all-cities-with-a-population-1000/table/?disjunctive.cou_name_en&sort=name
install.packages("devtools")
install.packages("stringr")
install.packages("dplyr")
devtools::install_github("Mosquito-Alert/mosquitoR")

library(dplyr)
library(stringr)
library(mosquitoR)






# Load polygon shapefile for Spain
spain_polygon_sf <- st_read("gadm41_ESP_shp/gadm41_ESP_4.shp")




last_digit <- str_extract("gadm41_ESP_shp/gadm41_ESP_4.shp", "_\\d+\\.") %>%
  str_extract("\\d+")

print(last_digit)

st_layers("gadm41_ESP.gpkg") #gets the layer names
spain_polygon_geo <- st_read("gadm41_ESP.gpkg", layer = "ADM_ADM_4")


spain_sf <- st_as_sf(spain_data, 
                     coords = c("lon", "lat"), # your lat/long column names
                     crs = 4326)

# Perform spatial join or intersection to extract polygon data based on points

spain_extracted_polygons <- st_join(spain_sf,spain_polygon_sf)
spain_extracted_geo_polygons <- st_join(spain_sf,spain_polygon_geo)

colnames(extracted_polygons)

grouped_counts <- spain_extracted_polygons %>%
  group_by(NAME_4) %>%
  summarise(count = n())

grouped_counts <- spain_extracted_geo_polygons %>%
  group_by(NAME_4) %>%
  summarise(count = n())


ita_polygon_geo <- st_read("gadm41_ITA.gpkg", layer = "ADM_ADM_3")

ITA_data <- malerts_reports_github %>%
  filter(country == "ITA")

ita_sf <- st_as_sf(ITA_data, 
                   coords = c("lon", "lat"), # your lat/long column names
                   crs = 4326)

ita_extracted_geo_polygons <- st_join(ita_sf,ita_polygon_geo)

grouped_counts <- ita_extracted_geo_polygons %>%
  group_by(NAME_3) %>%
  summarise(count = n())



