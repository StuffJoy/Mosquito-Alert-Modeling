target_country = "Bangladesh"
target_country_iso3 = "BGD"
target_level = 2
target_city = "Dhaka"
ecmwfr_key = "95690cbe-f660-4dbb-b7a1-9a8d9355ffe6"
android_start_date = as.POSIXct(strptime('2014-06-14 00:00:00.000000+0000', format="%Y-%m-%d %H:%M:%S%OS%z", tz="gmt"))

raw_pop_density = "data/population/Bangladesh_Dhaka_Dhaka_WorldPop_2020_population_100m.tif"
raw_ndvi = "data/ndvi/Bangladesh_Dhaka_Dhaka_NDVI_2024-01-31_2024-12-31.tif"
raw_landcover = "data/landcover/Bangladesh_Dhaka_Dhaka_WorldCover_2021.tif"

# key settings ####
available_cores = parallel::detectCores()
ncores = available_cores # change this in case fewer cores are preferred
nchains = 6
threads_per_chain = ncores/nchains
