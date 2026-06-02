devtools::install_github("Mosquito-Alert/mosquitoR")
devtools::install_github("Mosquito-Alert/vectormodelR", ref="feature/implement-bym2-modeling")
devtools::install_github("Mosquito-Alert/vectormodelR")

library(vectormodelR)

#source('helpers/keys.R')
#https://gee-community-catalog.org/projects/gaul/
#https://cds.climate.copernicus.eu/
source('helpers/ita_3_roma.R')
source('helpers/ita_3_trento.R')
source('helpers/bra_1_minasgerais.R')
source('helpers/ita_3_firenze.R') attica
source('helpers/esp_4_barcelona.R')
#source('helpers/bgd_2_dhaka.R')
source('helpers/mex_2_leon.R')
source('helpers/nld_2_amsterdam.R')
source('helpers/col_2_miranda.R')
source('helpers/grc_2_attica.R')

ma_count_country = mosquitoR::get_malert_aggregates(aggregate_type = "country", filter_year = "2014-2026")
ma_count_city_GRC = vectormodelR::get_malert_aggregates(aggregate_type = "city", filter_year = "2014-2026", country_code = "GRC", file_layer = "2")
ma_counts = mosquitoR::get_malert_aggregates(aggregate_type = "city", filter_year = "2014-2025", country_code = target_country_iso3, file_layer = target_level)


vectormodelR::get_gadm_names(country = "GRC", level=3, view ="datatable")

counts <- vectormodelR::get_vector_counts(iso3 = target_country_iso3, level = target_level,taxon_key = taxon_keys)
counts_LVL1_BRA <- get_vector_counts(iso3 = "BRA", level = 1,taxon_key = taxon_keys)
counts_LVL2_BRA <- get_vector_counts(iso3 = "BRA", level = 2,taxon_key = taxon_keys)
counts_LVL1_COL <- get_vector_counts(iso3 = "COL", level = 1,taxon_key = taxon_keys)
counts_LVL2_COL <- get_vector_counts(iso3 = "COL", level = 2,taxon_key = taxon_keys)

counts <- vectormodelR(iso3 = target_country_iso3, level = target_level)

# Download Era5 Weather data

# ecmwfr::wf_transfer(
#   url = "https://cds.climate.copernicus.eu/api/retrieve/v1/jobs/e52f3dd2-9ce9-4b88-8e29-0975f334e808",
#   path = "data/weather/grib/esp_4_barcelona",
#   filename = "era5land_esp_4_barcelona_2023_11.zip"
# )


vectormodelR::get_era5_data(
  iso3 = target_country_iso3,
  dataset = "reanalysis-era5-land",
  #dataset = "reanalysis-era5-single-levels",
  admin_level = target_level,
  admin_name = target_city,
  start_ym = "2026_01",
  end_ym = "2026_04",
  verbose = TRUE
)


vectormodelR::get_era5_logged_jobs(
  iso3 = target_country_iso3,,
  admin_level = target_level,
  admin_name = target_city,
  start_ym = "2016_01",
  end_ym = "2017_12",
    verbose = TRUE,
  dataset = "reanalysis-era5-land"
) 


vectormodelR::compile_era5_data(
  iso3 = target_country_iso3,
  #dataset = "reanalysis-era5-single-levels",
  dataset = "reanalysis-era5-land",
  admin_level = target_level,
  admin_name = target_city,
  recent_n  = 1,
  verbose = TRUE
)


vectormodelR::process_era5_data(
  iso3 = target_country_iso3,
  admin_level = target_level,
  admin_name = target_city,
  #dataset = "reanalysis-era5-single-levels",
  dataset = "reanalysis-era5-land",
  aggregation_unit = "cell",
  attach_to_global = FALSE
  )



# Administrative Units
map <- vectormodelR::get_gadm_data(iso3 = target_country_iso3, name=target_city, level = target_level, perimeter = TRUE, rds = TRUE)

hex_grid400 <- vectormodelR::build_spatial_grid(iso3 = target_country_iso3, admin_level = target_level, admin_name = target_city)

hex_grid800 <- vectormodelR::build_spatial_grid(cellsize_m =800, iso3 = target_country_iso3, admin_level = target_level, admin_name = target_city)

hex_grid1200 <- vectormodelR::build_spatial_grid(cellsize_m =1200, iso3 = target_country_iso3, admin_level = target_level, admin_name = target_city)

hex_grid1600 <- vectormodelR::build_spatial_grid(cellsize_m =1600, iso3 = target_country_iso3, admin_level = target_level, admin_name = target_city)

# plot(hex_grid400["GID_3"])
# plot(hex_grid400)

# landcover

lc <- terra::rast(raw_landcover) 

processed_lc = vectormodelR::process_landcover_data( 
  lc,
  iso3 = target_country_iso3,
  admin_level = target_level,
  admin_name = target_city,
  proc_dir = "data/proc",
  verbose = TRUE
)

# Elevation data
elevation <- vectormodelR::get_elevation_data(target_country_iso3, level = target_level, name_value = target_city)

terra::plot(elevation)

# NDVI
ndvi <- terra::rast(raw_ndvi) 

processed_ndvi = vectormodelR::process_ndvi_data(
  ndvi,
  target_country_iso3,
  target_level,
  target_city
)
#terra::plot(processed_ndvi$raster)

# POPULATION
pop_density <- terra::rast(raw_pop_density) 

processed_pop_density = vectormodelR::process_popdensity_data(
  pop_density,
  target_country_iso3,
  target_level,
  target_city
)

# upper <- terra::global(
#   processed_pop_density,
#   quantile,
#   probs = 0.98,
#   na.rm = TRUE
# )[1,1]
# 
# terra::plot(
#   processed_pop_density,
#   range = c(0, upper)
# )

# Vector data
malert_vector_data = vectormodelR::get_malert_data(
  source = "github",
  iso3 = target_country_iso3, 
  admin_level = target_level,
  admin_name = target_city 
  #desired_cols = malert_desired_cols
  #filters = malert_filters
)

gbif_vector_data <- vectormodelR::get_gbif_data(
  taxon_key = taxon_keys,
  iso3 = target_country_iso3,
  admin_level = target_level,
  admin_name = target_city,
  desired_cols = gbif_desired_cols,
  out_dir = "data/proc",
  verbose = TRUE
)

#test = readRDS("data/proc/model_prep_esp_4_barcelona_trs_daily.Rds")

# vector = readRDS("data/vector/vector_global_malert.Rds")
# writexl::write_xlsx(vector, "vector_global_malert.xlsx")

trs_daily = vectormodelR::build_trs_daily(iso3 = target_country_iso3, admin_level = target_level, admin_name = target_city) 
tgb_daily = vectormodelR::build_tgb_daily(iso3 = target_country_iso3, admin_level = target_level, admin_name = target_city, overwrite = TRUE) 

initialized_data = vectormodelR::initialize_vector_dataset(iso3 = target_country_iso3, admin_level = target_level,admin_name = target_city, overwrite = TRUE)

enriched_data_hourly = vectormodelR::add_features(
  target_country_iso3,target_level,weather_resolution="hourly",target_city,vector_sources = c("malert", "gbif"),"se,el,pd,wx_land,ndvi,lc,hex,hex_800,hex_1200")

enriched_data_daily = vectormodelR::add_features(
  target_country_iso3,target_level,weather_resolution="daily",target_city,vector_sources = c("malert", "gbif"),"se,el,pd,wx_land,ndvi,lc,hex,hex_800,hex_1200")


brms_dataset_daily <- vectormodelR::prepare_brms_data(
  dataset = enriched_data_daily,
  cellsize_m = 400,
  temporal_resolution = "daily",
  base_required_cols = base_required_cols,
  vars_to_check = daily_vars_to_check,
  scale_specs = daily_scale_specs,
  aggregation_specs = daily_aggregation_specs,
  factor_cols = daily_factor_cols,
  iso3 = target_country_iso3,
  admin_level = target_level,
  admin_name = target_city,
  write = TRUE,
  verbose = TRUE
)

brms_dataset_daily_data = brms_dataset_daily$model_data

brms_dataset_hourly <- vectormodelR::prepare_brms_data(
  dataset = enriched_data_hourly,
  cellsize_m = 400,
  temporal_resolution = "hourly",
  base_required_cols = base_required_cols,
  vars_to_check = hourly_vars_to_check,
  scale_specs = hourly_scale_specs,
  aggregation_specs = hourly_aggregation_specs,
  factor_cols = hourly_factor_cols,
  iso3 = target_country_iso3,
  admin_level = target_level,
  admin_name = target_city,
  write = TRUE,
  verbose = TRUE
)

brms_dataset_hourly_data = brms_dataset_hourly$model_data



daily_formula <- c(
  "presence ~ s(sea_days, bs = \"cc\", k = 12)",
  "s(maxTM_z, k = 6)",
  "ppt_30d_lag7_z + ndvi_z + elev_z + pop_z + landcover_class",
  "(1 | year)"
)

hourly_formula <- c(
  "presence ~ s(hour, bs = \"cc\", k = 8)",
  "s(sea_days, bs = \"cc\", k = 12)",
  "s(t2m_C_hour_z, k = 6)",
  "ppt_mm_prev_24h_z + RH_mean_prev_6h_z + ws10_hour_z + ndvi_z + elev_z + pop_z + landcover_class",
  "(1 | year)"
)

daily_formula_bym2_test <- c(
  "presence ~ s(sea_days, bs = \"cc\", k = 8)",
  "maxTM_z",
  "ppt_30d_lag7_z",
  "(1 | year)",
  "car(W, gr = grid_id_800, type = \"bym2\")"
)


brms_daily_model <- vectormodelR::run_brms_model(
  iso3 = target_country_iso3,
  formula = daily_formula,
  temporal_resolution = "daily",
  #priors = simple_priors,
  admin_level = target_level,
  admin_name = target_city,
  adapt_delta = 0.95,    # <--- Start here. Only go to 0.99 if you see "divergent transitions".
  max_treedepth = 15,    # <--- 20 is overkill unless needed.
  nchains = nchains,
  threads_per_chain = threads_per_chain
) 
summary(brms_daily_model)

brms_hourly_model <- vectormodelR::run_brms_model(
  iso3 = target_country_iso3,
  formula = hourly_formula,
  temporal_resolution = "hourly",
  #priors = simple_priors,
  admin_level = target_level,
  admin_name = target_city,
  adapt_delta = 0.95,    # <--- Start here. Only go to 0.99 if you see "divergent transitions".
  max_treedepth = 15,    # <--- 20 is overkill unless needed.
  nchains = nchains,
  threads_per_chain = threads_per_chain
) 
summary(brms_hourly_model)

brms_bym2_model <- vectormodelR::run_brms_bym2_model(
  iso3 = target_country_iso3,
  formula = daily_formula,
  admin_level = target_level,
  admin_name = target_city,
  adapt_delta = 0.995,    # <--- Start here. Only go to 0.99 if you see "divergent transitions".
  max_treedepth = 15,    # <--- 20 is overkill unless needed.
  nchains = nchains,
  threads_per_chain = threads_per_chain
) 

