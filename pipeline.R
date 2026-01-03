devtools::install_github("Mosquito-Alert/mosquitoR", ref = "feature/modularize-modeling-data")

library(readr)

source('helpers/ita_3_roma.R')
source('helpers/esp_4_barcelona.R')
#source('helpers/bgd_2_dhaka.R')

#ma_counts = mosquitoR::get_malert_aggregates(aggregate_type = "city", filter_year = "2014-2025", country_code = target_country_iso3, file_layer = target_level)

#mosquitoR::get_gadm_names(country = target_country, level=target_level, view ="datatable")


bg_data = readr::read_rds("data/bg-data/bg_counts.Rds")
bcn = readr::read_rds("data/proc/model_prep_esp_4_barcelona_base_se_ndvi_el_pd_wx_lc_hex.Rds")

# Ensure it's a data.frame (in case it's a tibble or data.table)
df <- as.data.frame(bg_data)

write.csv(
  df,
  file = "bgd_bg_data.csv",
  row.names = FALSE,
  na = ""
)

# Download Era5 Weather data
mosquitoR::get_era5_data(country_iso3 = target_country_iso3,
                         start_year = 2014, end_year = 2025,
                         ecmwfr_key = ecmwfr_key,
                         write_key  = TRUE,
                         data_format = "grib",
                         verbose = TRUE)

mosquitoR::compile_era5_data_v2(
  iso3 = target_country_iso3,
  recent_n  = 12,
  verbose   = TRUE
)


mosquitoR::process_era5_data(
iso3 = target_country_iso3,
admin_level = target_level,
admin_name = target_city,
aggregation_unit = "cell")


# Administrative Units
map <- mosquitoR::get_gadm_data(iso3 = target_country_iso3, name=target_city, level = target_level, perimeter = TRUE, rds = TRUE)
hex_grid <- mosquitoR::build_spatial_grid(iso3 = target_country_iso3, admin_level = target_level, admin_name = target_city)


# landcover

lc <- terra::rast(raw_landcover) 

processed_lc = mosquitoR::process_landcover_data( 
  lc,
  iso3 = target_country_iso3,
  admin_level = target_level,
  admin_name = target_city,
  proc_dir = "data/proc",
  verbose = TRUE
)

# Elevation data
elevation <- mosquitoR::get_elevation_data(target_country_iso3, level = target_level, name_value = target_city)

# NDVI
ndvi <- terra::rast(raw_ndvi) 

processed_ndvi = mosquitoR::process_ndvi_data(
  ndvi,
  target_country_iso3,
  target_level,
  target_city
)
#terra::plot(processed_ndvi$raster)

# POPULATION
pop_density <- terra::rast(raw_pop_density) 

processed_pop_density = mosquitoR::process_popdensity_data(
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

initialized_data = mosquitoR::initialize_ma_dataset(target_country_iso3,target_level,target_city)

enriched_data = mosquitoR::add_features(target_country_iso3,target_level,target_city,"se,el,ndvi,pd,wx,lc,hex")

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

summary(fit)

brms_model_fit = mosquitoR::interpret_brms_model(brms_model)




model = readRDS("data/proc/model_ita_3_roma_brms.Rds")

summary(model)

mx_fit <- run_maxent_model(
  dataset = bcn,
  iso3 = target_country_iso3, admin_level = target_level, admin_name = target_city,
  # optional tweaks:
  feature_classes = "lqh",
  regmult = 1,
  n_background = 20000
)


# 1) Build adjacency W (sparse is fine)
W_grid <- mosquitoR::build_grid_adjacency(
  iso3 = target_country_iso3,
  admin_level = target_level,
  admin_name  = target_city,
  model = enriched_data, #"data/proc/model_prep_ita_3_roma_base_se_el_ndvi_pd_wx_lc_hex.Rds",       # or path to model_prep_*.Rds
  sparse = TRUE
)
library(dplyr)
# 2) Prep data: grid_id must match W
D2 <- enriched_data %>%
  mutate(
    year = factor(year),
    landcover_code = factor(landcover_code),
    grid_id = as.character(grid_id),
    
    # scaled predictors (same as your baseline)
    maxTM_z = as.numeric(scale(maxTM)),
    ppt_z   = as.numeric(scale(log1p(meanPPT24H))),
    ndvi_z  = as.numeric(scale(ndvi_ddf_proximity)),
    elev_z  = as.numeric(scale(elevation_m)),
    pop_z   = as.numeric(scale(log1p(popdensity_km2)))
  ) %>%
  filter(grid_id %in% rownames(W_grid))

# 3) IMPORTANT: reorder W to match data's grid_id universe
ids <- sort(unique(D2$grid_id))
W2 <- W_grid[ids, ids]

priors <- c(
  brms::set_prior("normal(0, 1)", class = "b"),
  brms::set_prior("student_t(3, 0, 2.5)", class = "Intercept")
)

# 4) Fit ICAR model
M_icar_scaled <- brms::brm(
  presence ~
    s(sea_days) +
    s(maxTM_z) +
    ppt_z + ndvi_z + elev_z + pop_z +
    (1 | year) +
    (1 | landcover_code) +
    (1 | grid_id) +
    car(W, gr = grid_id, type = "bym2"),
  data  = D2,
  data2 = list(W2 = W2),   # note: name must match the term's first arg (W2)
  family  = brms::bernoulli(),
  backend = "cmdstanr",
  chains = nchains, cores = nchains, threads = brms::threading(threads_per_chain),
  prior = priors,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  save_pars = brms::save_pars(latent = TRUE)
)

#D2 <- D2 %>% mutate(eff_z = as.numeric(scale(log1p(sampling_effort))))

M_pres <- brms::brm(
  presence ~
    s(sea_days, bs="cc", k=12) +
    s(maxTM_z, k=6) +
    ppt_z + ndvi_z + elev_z + pop_z + #eff_z +
    (1 | year) +
    (1 | landcover_code) +
    car(W2, gr = grid_id, type = "bym2"),
  data  = D2,
  data2 = list(W2 = W2),
  family  = brms::bernoulli(),
  backend = "cmdstanr",
  chains = nchains, cores = nchains,
  threads = brms::threading(threads_per_chain),
  prior = priors,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  save_pars = brms::save_pars(latent = TRUE)
)

summary(M_pres)




