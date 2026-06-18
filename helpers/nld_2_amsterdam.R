target_country = "Netherlands"
target_country_iso3 = "NLD"
target_country_iso2 = "NL"
target_level = 2
target_city = "Amsterdam"

raw_pop_density = "data/population/Netherlands_Noord_holland_WorldPop_2020_population_100m.tif"
raw_ndvi = "data/ndvi/Netherlands_Noord_holland_NDVI_2024-01-31_2024-12-31.tif"
raw_landcover = "data/landcover/Netherlands_Noord_holland_WorldCover_2021.tif"



android_start_date = as.POSIXct(strptime('2014-06-14 00:00:00.000000+0000', format="%Y-%m-%d %H:%M:%S%OS%z", tz="gmt"))


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

malert_desired_cols  <- c(
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

factor_cols <- c(
  "year",
  "landcover_code",
  "source"
)

#daily defaults for prepping data
base_required_cols <- c(
  "date",
  "presence",
  "source"
)

daily_vars_to_check <- c(
  "presence",
  "sea_days",
  "ndvi_ddf_proximity",
  "elevation_m",
  "source",
  "year",
  "landcover_code",
  "maxTM",
  "PPT_3d_lag7",
  "PPT_7d_lag7",
  "PPT_14d_lag7",
  "PPT_21d_lag7",
  "PPT_30d_lag7"
)

daily_scale_specs <- list(
  ndvi = list(
    input = "ndvi_ddf_proximity",
    output = "ndvi_z",
    scale_name = "ndvi"
  ),
  popdensity = list(
    input = "popdensity_km2",
    output = "pop_z",
    scale_name = "popdensity"
  ),
  elevation = list(
    input = "elevation_m",
    output = "elev_z",
    scale_name = "elevation"
  ),
  max_temperature = list(
    input = "maxTM",
    output = "maxTM_z",
    scale_name = "maxTM"
  ),
  ppt_3d_lag7 = list(
    input = "PPT_3d_lag7",
    output = "ppt_3d_lag7_z",
    transform = "log1p",
    scale_name = "log1p_PPT_3d_lag7"
  ),
  ppt_7d_lag7 = list(
    input = "PPT_7d_lag7",
    output = "ppt_7d_lag7_z",
    transform = "log1p",
    scale_name = "log1p_PPT_7d_lag7"
  ),
  ppt_14d_lag7 = list(
    input = "PPT_14d_lag7",
    output = "ppt_14d_lag7_z",
    transform = "log1p",
    scale_name = "log1p_PPT_14d_lag7"
  ),
  ppt_21d_lag7 = list(
    input = "PPT_21d_lag7",
    output = "ppt_21d_lag7_z",
    transform = "log1p",
    scale_name = "log1p_PPT_21d_lag7"
  ),
  ppt_30d_lag7 = list(
    input = "PPT_30d_lag7",
    output = "ppt_30d_lag7_z",
    transform = "log1p",
    scale_name = "log1p_PPT_30d_lag7"
  )
)

daily_aggregation_specs <- c(
  presence = "any",
  sea_days = "first",
  year = "first",
  landcover_code = "mode",
  maxTM = "first",
  PPT_30d_lag7 = "first",
  ndvi_ddf_proximity = "mean",
  elevation_m = "mean"
)

#hourly defaults for prepping data
hourly_vars_to_check <- c(
  "presence",
  "sea_days",
  "ndvi_ddf_proximity",
  "elevation_m",
  "source",
  "year",
  "landcover_code",
  "hour",
  "t2m_C_hour",
  "RH_mean_prev_6h",
  "ws10_hour",
  "ppt_mm_prev_24h"
)

hourly_scale_specs <- list(
  ndvi = list(
    input = "ndvi_ddf_proximity",
    output = "ndvi_z",
    scale_name = "ndvi"
  ),
  popdensity = list(
    input = "popdensity_km2",
    output = "pop_z",
    scale_name = "popdensity"
  ),
  elevation = list(
    input = "elevation_m",
    output = "elev_z",
    scale_name = "elevation"
  ),
  t2m_hour = list(
    input = "t2m_C_hour",
    output = "t2m_C_hour_z",
    scale_name = "t2m_C_hour"
  ),
  rh_mean_prev_6h = list(
    input = "RH_mean_prev_6h",
    output = "RH_mean_prev_6h_z",
    scale_name = "RH_mean_prev_6h"
  ),
  wind_hour = list(
    input = "ws10_hour",
    output = "ws10_hour_z",
    scale_name = "ws10_hour"
  ),
  ppt_prev_24h = list(
    input = "ppt_mm_prev_24h",
    output = "ppt_mm_prev_24h_z",
    transform = "log1p",
    scale_name = "log1p_ppt_mm_prev_24h"
  )
)

hourly_aggregation_specs <- c(
  presence = "any",
  sea_days = "first",
  year = "first",
  landcover_code = "mode",
  t2m_C_hour = "first",
  RH_mean_prev_6h = "first",
  ws10_hour = "first",
  ppt_mm_prev_24h = "first",
  ndvi_ddf_proximity = "mean",
  elevation_m = "mean"
)


#Model formulas
daily_formula_3d <- c(
  "presence ~ s(sea_days, bs = \"cc\", k = 8)",
  "s(maxTM_z, k = 5)",
  "ppt_3d_lag7_z + ndvi_z + elev_z + pop_z + landcover_class",
  "(1 | year)"
)

daily_formula_7d <- c(
  "presence ~ s(sea_days, bs = \"cc\", k = 8)",
  "s(maxTM_z, k = 5)",
  "ppt_7d_lag7_z + ndvi_z + elev_z + pop_z + landcover_class",
  "(1 | year)"
)

daily_formula_14d <- c(
  "presence ~ s(sea_days, bs = \"cc\", k = 8)",
  "s(maxTM_z, k = 5)",
  "ppt_14d_lag7_z + ndvi_z + elev_z + pop_z + landcover_class",
  "(1 | year)"
)

daily_formula_21d <- c(
  "presence ~ s(sea_days, bs = \"cc\", k = 8)",
  "s(maxTM_z, k = 6)",
  "ppt_21d_lag7_z + ndvi_z + elev_z + pop_z + landcover_class",
  "(1 | year)"
)

daily_formula_30d <- c(
  "presence ~ s(sea_days, bs = \"cc\", k = 8)",
  "s(maxTM_z, k = 5)",
  "ppt_30d_lag7_z + ndvi_z + elev_z + pop_z + landcover_class",
  "(1 | year)"
)

hourly_formula <- c(
  "presence ~ s(hour, bs = \"cc\", k = 6)",
  "s(sea_days, bs = \"cc\", k = 8)",
  "s(t2m_C_hour_z, k = 5)",
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

