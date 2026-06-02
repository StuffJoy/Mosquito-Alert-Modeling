target_country = "Greece"
target_country_iso3 = "GRC"
target_country_iso2 = "GR"
target_level = 2
target_city = "Attica"

android_start_date = as.POSIXct(strptime('2014-06-14 00:00:00.000000+0000', format="%Y-%m-%d %H:%M:%S%OS%z", tz="gmt"))

raw_pop_density = "data/population/Netherlands_Noord_holland_WorldPop_2020_population_100m.tif"
raw_ndvi = "data/ndvi/Netherlands_Noord_holland_NDVI_2024-01-31_2024-12-31.tif"
raw_landcover = "data/landcover/Netherlands_Noord_holland_WorldCover_2021.tif"

# key settings ####
available_cores = parallel::detectCores()
ncores = available_cores # change this in case fewer cores are preferred
nchains = 6
threads_per_chain = ncores/nchains

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

taxon_keys <- c(
  7924646, # Aedes Meigen, 1818
  1650098, # Anopheles Meigen, 1818
  1497010  # Culex Linnaeus, 1758
)