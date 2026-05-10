devtools::install_github("Mosquito-Alert/mosquitoR")
devtools::install_github("Mosquito-Alert/vectormodelR", ref="feature/implement-hourly-models")
library(vectormodelR)

#source('helpers/keys.R')
#https://gee-community-catalog.org/projects/gaul/
#https://cds.climate.copernicus.eu/
source('helpers/ita_3_roma.R')
source('helpers/ita_3_trento.R')
source('helpers/ita_3_firenze.R')
source('helpers/esp_4_barcelona.R')
#source('helpers/bgd_2_dhaka.R')
source('helpers/mex_2_leon.R')
source('helpers/nld_2_amsterdam.R')


ma_count_country = mosquitoR::get_malert_aggregates(aggregate_type = "country", filter_year = "2014-2026")
ma_count_city_URY = vectormodelR::get_malert_aggregates(aggregate_type = "city", filter_year = "2014-2026", country_code = "NLD", file_layer = "2")
ma_counts = mosquitoR::get_malert_aggregates(aggregate_type = "city", filter_year = "2014-2025", country_code = target_country_iso3, file_layer = target_level)


vectormodelR::get_gadm_names(country = "URY", level=2, view ="datatable")

counts <- vectormodelR::get_vector_counts(iso3 = target_country_iso3, level = target_level,taxon_key = taxon_keys)
counts <- vectormodelR::get_vector_counts(iso3 = target_country_iso3, level = target_level,taxon_key = taxon_keys)
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
  start_ym = "2026_01",
  end_ym = "2026_04",
    verbose = TRUE,
  dataset = "reanalysis-era5-land"
) 


vectormodelR::compile_era5_data(
  iso3 = target_country_iso3,
  #dataset = "reanalysis-era5-single-levels",
  dataset = "reanalysis-era5-land",
  admin_level = target_level,
  admin_name = target_city,
  recent_n  = 3,
  verbose = TRUE
)


vectormodelR::process_era5_data(
  iso3 = target_country_iso3,
  admin_level = target_level,
  admin_name = target_city,
  #dataset = "reanalysis-era5-single-levels",
  dataset = "reanalysis-era5-land",
  aggregation_unit = "hourly",
  attach_to_global = TRUE
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
  admin_name = target_city, 
  desired_cols = malert_desired_cols,
  filters = malert_filters
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

initialized_dataROME = vectormodelR::initialize_vector_dataset(iso3 = target_country_iso3, admin_level = target_level,admin_name = target_city, overwrite = TRUE)

enriched_data_hourly = vectormodelR::add_features(
  target_country_iso3,target_level,weather_resolution="hourly",target_city,vector_sources = c("malert", "gbif"),"se,el,pd,wx_land,ndvi,lc,hex,hex_800,hex_1200")

enriched_data_hourly = vectormodelR::add_features(
  target_country_iso3,target_level,weather_resolution="hourly",target_city,vector_sources = c("malert", "gbif"),"se,el,pd,lc,wx_land")



enriched_data_daily = vectormodelR::add_features(
  target_country_iso3,target_level,weather_resolution="daily",target_city,vector_sources = c("malert", "gbif"),"se,el,pd,wx_land,lc,hex,hex_800,hex_1200")

brms_dataset = vectormodelR::prepare_brms_data(
  cellsize_m = 400,
  temporal_resolution = "hourly",
  dataset = enriched_data_hourly,
  iso3 = target_country_iso3,
  admin_level = target_level,
  admin_name = target_city,
  write = TRUE
)

#model_data =brms_dataset$model_data


summary(brms_bym2_model)
brms::rhat(brms_bym2_model)
brms::neff_ratio(brms_bym2_model)


daily_formula <- c(
  "presence ~ s(sea_days, bs = \"cc\", k = 12)",
  "s(maxTM_z, k = 6)",
  "ppt_14d_lag7_z + ndvi_z + elev_z + pop_z + landcover_class",
  "(1 | year)"
)

hourly_formula <- c(
  "presence ~ s(hour, bs = \"cc\", k = 8)",
  "s(sea_days, bs = \"cc\", k = 12)",
  "s(t2m_C_hour_z, k = 6)",
  "ppt_mm_prev_24h_z + RH_mean_prev_6h_z + ws10_hour_z + ndvi_z + elev_z + pop_z + landcover_class",
  "(1 | year)"
)




#md <- readRDS("data/proc/model_prep_esp_4_barcelona_data.rds")$model_data


brms_model <- vectormodelR::run_brms_model(
  iso3 = target_country_iso3,
  formula = hourly_formula,
  temporal_resolution = "hourly",
  admin_level = target_level,
  admin_name = target_city,
  adapt_delta = 0.999,    # <--- Start here. Only go to 0.99 if you see "divergent transitions".
  max_treedepth = 15,    # <--- 20 is overkill unless needed.
  nchains = nchains,
  threads_per_chain = threads_per_chain
) 
summary(brms_model)

brms_bym2_model_xxx <- vectormodelR::run_brms_bym2_model(
  iso3 = target_country_iso3,
  formula = my_custom_formula,
  admin_level = target_level,
  admin_name = target_city,
  adapt_delta = 0.995,    # <--- Start here. Only go to 0.99 if you see "divergent transitions".
  max_treedepth = 20,    # <--- 20 is overkill unless needed.
  nchains = nchains,
  threads_per_chain = threads_per_chain
) 

summary(brms_bym2_model)






mex = readRDS("data/proc/model_prep_mex_2_leon_data.rds")

mex = mex$model_data
c(mu = mean(df$elevation_m, na.rm=TRUE),
  sd = sd(df$elevation_m, na.rm=TRUE),
  min = min(df$elevation_m, na.rm=TRUE),
  max = max(df$elevation_m, na.rm=TRUE))

quantile(df$elevation_m, c(.5,.9,.95,.99,.995,.999), na.rm=TRUE)


check_brms_data("data/proc/model_prep_esp_4_barcelona_data.rds")
check_brms_data("data/proc/model_prep_mex_2_leon_data.rds")
check_brms_data("data/proc/model_prep_ita_3_roma_data.rds")
check_brms_data("data/proc/model_prep_nld_2_amsterdam_data.rds")


library(ggplot2)
library(brms)

# 1. Extract the conditional effects for just pop_z
ce_pop <- conditional_effects(brms_bym2_model_xxx, effects = "pop_z")

# 2. Plot it with custom ggplot layers
plot(ce_pop, plot = FALSE)[[1]] + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") + # 50% probability line
  labs(
    title = "How Population Density affects Mosquito Detection",
    subtitle = "Notice the non-linear shape (Boom and Bust)",
    x = "Population Density (Standardized Z-score)",
    y = "Probability of Presence"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 12))

ee = xxx$model_data


brms_model = mosquitoR::run_brms_model(
  enriched_data,
  nchains = nchains,
  threads_per_chain = threads_per_chain,
  backend = "cmdstanr",
  iso3 = target_country_iso3,
  admin_level = target_level,
  admin_name = target_city,
  write_output = TRUE,
  output_path = "data/proc",
  verbose = TRUE
)

summary(brms_model)

brms_model_fit = mosquitoR::interpret_brms_model(brms_model)


ranef(model)

model = readRDS("data/proc/model_ita_3_roma_brms.Rds")

max_model = readRDS("data/proc/model_esp_4_barcelona_maxent_maxnet.Rds")


names(max_model)
max_model$betas

mx_fit <- run_maxent_model(
  dataset = bcn,
  iso3 = target_country_iso3, admin_level = target_level, admin_name = target_city,
  # optional tweaks:
  feature_classes = "lqh",
  regmult = 1,
  n_background = 20000
)



W_grid <- mosquitoR::build_grid_adjacency(
  iso3 = target_country_iso3,
  admin_level = target_level,
  admin_name  = target_city,
  cellsize_m = 800,
  model = enriched_data, 
  sparse = TRUE
)
library(dplyr)
# 2) Prep data: grid_id must match W
D2 <- enriched_data %>%
  mutate(
    year = factor(year),
    landcover_code = factor(landcover_code),
    grid_id_800 = as.character(grid_id_800),
    
    # scaled predictors (same as your baseline)
    maxTM_z = as.numeric(scale(maxTM)),
    ppt_z   = as.numeric(scale(log1p(meanPPT24H))),
    ndvi_z  = as.numeric(scale(ndvi_ddf_proximity)),
    elev_z  = as.numeric(scale(elevation_m)),
    pop_z   = as.numeric(scale(log1p(popdensity_km2)))
  ) %>%
  filter(grid_id_800 %in% rownames(W_grid))

D2 <- enriched_data %>%
  mutate(
    year = factor(year),
    landcover_code = factor(landcover_code),
    source = factor(source),
    grid_id_800 = as.character(grid_id_800),
    
    maxTM_z = as.numeric(scale(maxTM)),
    ppt_z   = as.numeric(scale(log1p(meanPPT24H))),
    ndvi_z  = as.numeric(scale(ndvi_ddf_proximity)),
    elev_z  = as.numeric(scale(elevation_m)),
    pop_z   = as.numeric(scale(log1p(popdensity_km2))),
    
    w = dplyr::case_when(
      presence ~ 1,
      source == "gbif" & !presence ~ 0.3,   # downweight gbif pseudo-absences
      TRUE ~ 1
    )
  ) %>%
  filter(grid_id_800 %in% rownames(W_grid))

D2 <- D2 %>%
  group_by(source, grid_id_800, date, presence) %>%
  slice(1) %>%
  ungroup()

# 3) IMPORTANT: reorder W to match data's grid_id universe
ids <- sort(unique(D2$grid_id_800))
W2 <- W_grid[ids, ids]

priors <- c(
  brms::set_prior("normal(0, 1)", class = "b"),
  brms::set_prior("student_t(3, 0, 2.5)", class = "Intercept")
)





