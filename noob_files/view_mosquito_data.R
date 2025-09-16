#https://public.opendatasoft.com/explore/dataset/geonames-all-cities-with-a-population-1000/table/?disjunctive.cou_name_en&sort=name
install.packages("devtools")
install.packages("stringr")
install.packages("dplyr")
devtools::install_github("Mosquito-Alert/mosquitoR")

library(dplyr)
library(stringr)
library(mosquitoR)

malerts_reports_github = get_malert_data(source = "github")

spain_data <- malerts_reports_github %>%
  filter(country == "ESP")

colnames(spain_data)

#show counts from each country
grouped_countries <- malerts_reports_github %>%
  group_by(country) %>%
  summarise(count = n())

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








get_malert_aggregates <- function(aggregate_type, filter_year, file_path, country_code, file_layer) {
  
  malerts_reports_github = get_malert_data(source = "github")
  
  filter_year = "2014,2015,2018,2020"
  
  # Handle multiple years or year range
  if (!is.null(filter_year)) {
    
    if (grepl("-", filter_year)) { # Check if it's a range (e.g., "2011-2015")
      years <- as.numeric(unlist(strsplit(filter_year, "-")))
      filter_year <- seq(years[1], years[2])
      
    } else if (grepl(",", filter_year)) { # Check if it's a comma-separated list (e.g., "2021,2024,2023,2022")
      filter_year <- as.numeric(unlist(strsplit(filter_year, ",")))
    } else {
      filter_year <- as.numeric(filter_year) # Single year
    }
    
    
  }
  
  # Apply filtering for the year or range of years
  filtereed_malerts_reports_github <- malerts_reports_github %>%
    filter(creation_year %in% filter_year)
  
  if(aggregate_type == "country") {
    
    aggregated_data <- malerts_reports_github %>%
      group_by(country) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
  } else if (aggregate_type == "city") {
    
    malerts_reports_github <- malerts_reports_github %>%
      filter(country == country_code)
    
    file_ext <- tools::file_ext(file_path)
    
    # Test if the file is a shapefile (.shp) or a GPKG file (.gpkg)
    if (file_ext == "shp") {
      polygon_file <- st_read(file_path)
    } else if (file_ext == "gpkg") {
      polygon_file <- st_read(file_path, layer = file_layer)
    } else {
      return("Unknown file type.")
    }
    
    malerts_reports_github <- st_as_sf(malerts_reports_github,
                                       coords = c("lon", "lat"),
                                       crs = 4326)
    
    malerts_reports_github <- st_join(malerts_reports_github, polygon_file)
    
    aggregated_data <- malerts_reports_github %>%
      group_by(layer) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
  }
  
  return(aggregated_data)
}

# Install necessary packages for raster handling
# install.packages("terra")

library(sf)

# Step 1: List all layers in the GPKG
gpkg_layers <- st_layers("gadm41_ESP.gpkg")
print(gpkg_layers)

num_rows <- nrow(gpkg_layers)-1

# Step 2: Loop through each layer and find the minimum elevation
lowest_layer <- NULL
min_elevation <- Inf  # Initialize a very high value for comparison

# Loop through the layers and check for elevation data
for (layer_name in gpkg_layers$name) {
  layer_data <- rast("gadm41_ESP.gpkg", layer = "ADM_ADM_5")  # Load the raster layer
  if (is.raster(layer_data)) {
    min_layer_elevation <- min(values(layer_data), na.rm = TRUE)  # Get min elevation
    if (min_layer_elevation < min_elevation) {
      min_elevation <- min_layer_elevation
      lowest_layer <- layer_name
    }
  }
}

if (!is.null(lowest_layer)) {
  print(paste("The layer with the lowest elevation is:", lowest_layer, "with an elevation of", min_elevation))
} else {
  print("No raster layers with elevation found.")
}
