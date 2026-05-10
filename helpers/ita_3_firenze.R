target_country = "Italy"
target_country_iso3 = "ITA"
target_country_iso2 = "IT"
target_level = 3
target_city = "Firenze"

android_start_date = as.POSIXct(strptime('2014-06-14 00:00:00.000000+0000', format="%Y-%m-%d %H:%M:%S%OS%z", tz="gmt"))

raw_pop_density = "data/population/Italy_Toscana_Firenze_WorldPop_2020_population_100m.tif"
raw_ndvi = "data/ndvi/Italy_Toscana_Firenze_NDVI_2024-01-31_2024-12-31.tif"
raw_landcover = "data/landcover/Italy_Toscana_Firenze_WorldCover_2021.tif"

# key settings ####
available_cores = parallel::detectCores()
ncores = available_cores # change this in case fewer cores are preferred
nchains = 6
threads_per_chain = ncores/nchains

era5_single_level <- c(
  "10m_u_component_of_wind",
  "10m_v_component_of_wind",
  "2m_dewpoint_temperature",
  "2m_temperature",
  "surface_pressure",
  "total_precipitation"
)

era5_land_to_single_level <- c(
  "10_m_u_wind_component" = "10m_u_component_of_wind",
  "10_m_v_wind_component" = "10m_v_component_of_wind",
  "2_m_dewpoint_temperature" = "2m_dewpoint_temperature",
  "2_m_temperature" = "2m_temperature",
  "surface_pressure" = "surface_pressure",
  "total_precipitation" = "total_precipitation"
)

era5_land <- c(
  "2_m_dewpoint_temperature",
  "2_m_temperature",
  "10_m_u_wind_component",
  "10_m_v_wind_component",
  "surface_pressure",
  "total_precipitation" 
)

gbif_desired_cols <- c(
  "gbifID",
  "species",
  "occurrenceStatus",
  "decimalLatitude",
  "decimalLongitude",
  "eventDate",
  "year",
  "occurrenceID",
  "collectionCode"
)

malert_desired_cols <- c(
  "version_UUID", 
  "creation_time", 
  "creation_date", 
  "creation_year", 
  "type", 
  "lon", 
  "lat", 
  "movelab_annotation_euro.class_name"
)

malert_filters = list(
  "movelab_annotation_euro.class_id" = 4,
  "type" = "adult"
)

taxon_keys <- c(
  1651430
)

# 1651430 #Aedes albopictus (Skuse, 1894)
# movelab_annotation_euro.class_id - 4
# movelab_annotation_euro.class_label - aedes-albopictus
# movelab_annotation_euro.class_name - Aedes albopictus

# 1651891 #Aedes aegypti (Linnaeus, 1762)
# movelab_annotation_euro.class_id - 5
# movelab_annotation_euro.class_label - aedes-aegypti
# movelab_annotation_euro.class_name - Aedes aegypti

# taxon_keys <- c(
#   7924646, # Aedes Meigen, 1818
#   1650098, # Anopheles Meigen, 1818
#   1497010  # Culex Linnaeus, 1758
# )