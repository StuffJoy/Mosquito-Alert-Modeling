target_country = "Italy"
target_country_iso3 = "ITA"
target_level = 3
target_city = "Roma"
ecmwfr_key = "95690cbe-f660-4dbb-b7a1-9a8d9355ffe6"
android_start_date = as.POSIXct(strptime('2014-06-14 00:00:00.000000+0000', format="%Y-%m-%d %H:%M:%S%OS%z", tz="gmt"))

raw_pop_density = "data/population/Italy_Lazio_Roma_WorldPop_2020_population_100m.tif"
raw_ndvi = "data/ndvi/Italy_Lazio_Roma_NDVI_2024-01-31_2024-12-31.tif"
raw_landcover = "data/landcover/Italy_Lazio_Roma_WorldCover_2021.tif"

# key settings ####
available_cores = parallel::detectCores()
ncores = available_cores # change this in case fewer cores are preferred
nchains = 6
threads_per_chain = ncores/nchains
