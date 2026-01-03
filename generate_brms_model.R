library(mosquitoR)
library(dplyr)

# key settings ####
available_cores = parallel::detectCores()
ncores = available_cores # change this in case fewer cores are preferred
nchains = 6
threads_per_chain = ncores/nchains

D_mod_dfb = mosquitoR::add_features("esp","4","Barcelona","se,ndvi,el,pd,wx,lc,hex")





D_mod_df2 = add_features_new("esp","4","Barcelona","wx,lc,ndvi,el,pd,se")
D_mod_df3 = add_features("esp","4","Barcelona","se")

process_era5_data2(
iso3 = "ESP",
admin_level = 4,
admin_name = "Barcelona",
processed_dir = "data/weather/grib/esp/processed",
out_dir = "data/proc",
aggregation_unit = "cell"
)

model_prep_esp_4_barcelona_trs_daily.Rds
trs = read_rds("data/proc/model_prep_esp_4_barcelona_trs_daily.Rds") 

xxx = interpret_brms_model(fit)
summary(fit)
D_mod_df_M10 = hexy %>% 
  dplyr::select(date, 
                grid_id, 
                grid_lon, 
                grid_lat, 
                tigacell_lon,
                tigacell_lat,
                TigacellID, 
                TigacellID_small,
                sea_days,
                biweek,
                presence,
                year, 
                presence,ndvi_distance_m, 
                ndvi_value_nearest, 
                ndvi_ddf_proximity,
                elevation_m, 
                popdensity_km2, 
                landcover_code, 
                landcover_class, 
                mwi, 
                maxTM, 
                meanPPT24H)

vars_needed <- c("presence","sea_days","maxTM","meanPPT24H","ndvi_ddf_proximity",
                 "elevation_m","popdensity_km2","year","landcover_code","grid_id")
D_mod_df_M10_cc <- D_mod_df_M10 %>%
  mutate(grid_id = as.character(grid_id),
         year = factor(year),
         landcover_code = factor(landcover_code),
         sea_days = lubridate::yday(as.Date(date))) %>%
  tidyr::drop_na(dplyr::all_of(vars_needed)) %>%
  filter(grid_id %in% rownames(W_grid))

W_grid <- build_grid_adjacency(
  iso3 = "ESP",
  admin_level = 4,
  admin_name = "Barcelona",
  model = D_mod_df_M10
)




M_base <- brm(
  presence ~
    s(sea_days, bs = "cc", k = 12) +
    s(maxTM, k = 6) +
    log1p(meanPPT24H) +
    ndvi_ddf_proximity +
    elevation_m +
    log1p(popdensity_km2) +
    (1 | year) +
    (1 | landcover_code),
  data = D_mod_df_M10,
  family = bernoulli(),
  backend = "cmdstanr",
  chains=nchains, cores=nchains, threads = threading(threads_per_chain),
  control = list(adapt_delta = 0.95)
)

priors <- c(
  prior(normal(0, 1), class = "b"),
  prior(student_t(3, 0, 2.5), class = "Intercept")
)

M_base2 <- update(
  M_base,
  prior = priors,
  control = list(adapt_delta = 0.99, max_treedepth = 12)
)


# sampler diagnostics
nuts_params(M_scaled) %>%
  count(Parameter, Value) %>%
  filter(Parameter %in% c("divergent__", "treedepth__"))

# quick overall health
summary(M_scaled)
pp_check(M_scaled, ndraws = 200)



D2 <- D_mod_df_M10 %>%
  mutate(
    year = factor(year),
    landcover_code = factor(landcover_code),
    maxTM_z = as.numeric(scale(maxTM)),
    ppt_z   = as.numeric(scale(log1p(meanPPT24H))),
    ndvi_z  = as.numeric(scale(ndvi_ddf_proximity)),
    elev_z  = as.numeric(scale(elevation_m)),
    pop_z   = as.numeric(scale(log1p(popdensity_km2)))
  )

M_scaled <- brm(
  presence ~ s(sea_days, bs="cc", k=12) +
    s(maxTM_z, k=6) +
    ppt_z + ndvi_z + elev_z + pop_z +
    (1 | year) + (1 | landcover_code),
  data = D2,
  family = bernoulli(),
  backend = "cmdstanr",
  chains = nchains, cores = nchains, threads = threading(threads_per_chain),
  prior = priors,
  control = list(adapt_delta = 0.99, max_treedepth = 12)
)

yrep <- posterior_predict(M_scaled, ndraws = 500)  # matrix: draws x N
obs  <- mean(D2$presence)
rep  <- rowMeans(yrep)

hist(rep, breaks = 30, main = "Posterior predictive prevalence", xlab = "mean(yrep)")
abline(v = obs, lwd = 2)
obs




library(dplyr)
library(tidybayes)

pp_year <- add_epred_draws(M_scaled, newdata = D2, ndraws = 400) %>%
  group_by(.draw, year) %>%
  summarise(p_hat = mean(.epred), .groups = "drop")

obs_year <- D2 %>%
  group_by(year) %>%
  summarise(p_obs = mean(presence), .groups="drop")

summ_year <- pp_year %>%
  group_by(year) %>%
  summarise(lo = quantile(p_hat, .05), md = median(p_hat), hi = quantile(p_hat, .95),
            .groups="drop") %>%
  left_join(obs_year, by="year")

summ_year


library(brms)


library(dplyr)
library(tidyr)

na_summary <- D_mod_df_M10 %>%
  summarise(across(
    everything(),
    ~sum(is.na(.)),
    .names = "na_{.col}"
  )) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "na_count"
  ) %>%
  mutate(
    variable = sub("^na_", "", variable),
    na_percent = 100 * na_count / nrow(D_mod_df_M10)
  ) %>%
  arrange(desc(na_count))

print(na_summary)


model_vars <- c(
  "presence",
  "sea_days",
  "maxTM",
  "meanPPT24H",
  "ndvi_ddf_proximity",
  "elevation_m",
  "popdensity_km2",
  "year",
  "landcover_code",
  "grid_id"
)

na_model_vars <- D_mod_df_M10 %>%
  summarise(across(
    all_of(model_vars),
    ~sum(is.na(.)),
    .names = "na_{.col}"
  )) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "na_count"
  ) %>%
  mutate(
    variable = sub("^na_", "", variable),
    na_percent = 100 * na_count / nrow(D_mod_df_M10)
  ) %>%
  arrange(desc(na_count))

print(na_model_vars)


empty_rows <- D_mod_df_M10 %>%
  filter(
    is.na(maxTM) |
      is.na(meanPPT24H) |
      is.na(popdensity_km2)
  ) %>%
  arrange(year, date) %>%
  select(
    date,
    year,
    grid_id,
    maxTM,
    meanPPT24H,
    popdensity_km2,
    ndvi_ddf_proximity,
    elevation_m,
    landcover_code,
    presence
  )

print(empty_rows, n = Inf)

M_icar <- brm(
  presence ~
    s(sea_days, bs="cc", k=20) +
    poly(maxTM, 2) +
    log1p(meanPPT24H) +
    ndvi_ddf_proximity + elevation_m + log1p(popdensity_km2) +
    (1 | year) +
    (1 | landcover_code) +
    car(W, gr = grid_id, type = "icar"),
  data = D_mod_df_M10_cc,
  data2 = list(W = W_grid),
  family = bernoulli(link="logit"),
  backend = "cmdstanr",
  chains=nchains, cores=nchains, threads = threading(threads_per_chain),
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  save_pars = save_pars(latent = TRUE)
)


table(D_mod_df_M10_cc$presence)
mean(D_mod_df_M10_cc$presence)

library(sf)
library(dplyr)

tmp_sf <- st_as_sf(D_mod_df_M10_cc, coords = c("grid_lon","grid_lat"), crs = 4326)
tmp_sf <- st_transform(tmp_sf, 3857)   # for Europe; for global use 3857 (see below)

xy <- st_coordinates(tmp_sf)
D_mod_df_M10_cc$grid_x <- xy[,1]
D_mod_df_M10_cc$grid_y <- xy[,2]

grid_xy <- D_mod_df_M10_cc %>%
  group_by(grid_id) %>%
  summarise(grid_x = first(grid_x), grid_y = first(grid_y), .groups="drop")

D_mod_df_M10_cc <- D_mod_df_M10_cc %>%
  select(-grid_x, -grid_y) %>%
  left_join(grid_xy, by="grid_id")

M_gp2 <- brm(
  presence ~
    s(sea_days, bs="cc", k=12) +
    s(maxTM, k=6) +
    log1p(meanPPT24H) +
    ndvi_ddf_proximity + elevation_m + log1p(popdensity_km2) +
    (1 | year) +
    landcover_code +
    gp(grid_x, grid_y, k = 100, scale = TRUE),
  data = D_mod_df_M10_cc,
  family = bernoulli(),
  backend = "cmdstanr",
  chains = nchains, cores = nchains,
  threads = threading(threads_per_chain),
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

M_gp <- brm(
  presence ~
    s(sea_days, bs="cc", k=12) +
    s(maxTM, k=6) +
    log1p(meanPPT24H) +
    ndvi_ddf_proximity + elevation_m + log1p(popdensity_km2) +
    (1 | year) +
    landcover_code +
    gp(grid_lon, grid_lat, k = 100, scale = FALSE),
  data = D_mod_df_M10_cc,
  family = bernoulli(),
  backend = "cmdstanr",
  chains=nchains, cores=nchains, threads = threading(threads_per_chain)
)

M_icar_stable <- brm(
  presence ~
    s(sea_days, bs="cc", k=12) +
    s(maxTM, k=6) +
    meanPPT24H +
    ndvi_ddf_proximity + elevation_m + log1p(popdensity_km2) +
    (1 | year) +
    (1 | landcover_code) +
    car(W, gr = grid_id, type = "icar"),
  data  = D_mod_df_M10_cc,
  data2 = list(W = W_grid),
  family = bernoulli(),
  backend = "cmdstanr",
  chains = 4, cores = 4,
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  save_pars = save_pars(latent = TRUE)
)

library(dplyr)

D_mod_df_M10_cc2 <- D_mod_df_M10_cc %>%
  mutate(
    sea_days = lubridate::yday(as.Date(date)),
    year = factor(year),
    landcover_code = factor(landcover_code),
    grid_id = as.character(grid_id),
    
    # transforms first, then scale
    maxTM_z   = as.numeric(scale(maxTM)),
    ppt_z     = as.numeric(scale(log1p(meanPPT24H))),
    ndvi_z    = as.numeric(scale(ndvi_ddf_proximity)),
    elev_z    = as.numeric(scale(elevation_m)),
    pop_z     = as.numeric(scale(log1p(popdensity_km2)))
  ) %>%
  filter(grid_id %in% rownames(W_grid))

M_icar_scaled <- brm(
  presence ~
    s(sea_days, bs="cc", k=12) +
    s(maxTM_z, k=6) +
    ppt_z +
    ndvi_z + elev_z + pop_z +
    (1 | year) +
    (1 | landcover_code) +
    car(W, gr = grid_id, type = "icar"),
  data  = D_mod_df_M10_cc2,
  data2 = list(W = W_grid),
  family = bernoulli(),
  backend = "cmdstanr",
  chains = 4, cores = 4,
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  save_pars = save_pars(latent = TRUE)
)

M_bym2_scaled <- brm(
  presence ~
    s(sea_days, bs="cc", k=12) +
    s(maxTM_z, k=6) +
    ppt_z +
    ndvi_z + elev_z + pop_z +
    (1 | year) +
    (1 | landcover_code) +
    car(W, gr = grid_id, type = "bym2"),
  data  = D_mod_df_M10_cc2,
  data2 = list(W = W_grid),
  family = bernoulli(),
  backend = "cmdstanr",
  chains = 4, cores = 4,
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  save_pars = save_pars(latent = TRUE)
)

table(D_mod_df_M10_cc2$presence)
mean(D_mod_df_M10_cc2$presence)

#' Build adjacency matrix for a grid (global-safe)
#'
#' Reads a polygon grid (`spatial_*_hex_grid.Rds`) and prepared model data
#' (`model_prep_*_wx_lc_ndvi_elev.Rds`), keeps only cells present in model data,
#' reprojects (only if needed) to a locally appropriate projected CRS, and
#' returns a binary queen-contiguity adjacency matrix suitable for `brms::car()`.
#'
#' @param iso3 Three-letter ISO3 country code used in the slug.
#' @param admin_level Administrative level tied to the boundary/grid slug.
#' @param admin_name Administrative unit name used in the file naming scheme.
#' @param data_dir Directory holding processed outputs. Defaults to `"data/proc"`.
#' @param target_crs Optional. EPSG code or `sf::st_crs()` object. If `NULL`,
#'   a suitable CRS is chosen automatically (UTM if input is lon/lat).
#' @param sparse Logical. If TRUE (default), return a sparse Matrix.
#' @param model Either the in-memory model dataset or a path to a prepared
#'   `model_prep_*` RDS file. This must be supplied; the function no longer
#'   derives the model path automatically.
#' @return An adjacency matrix (dense base matrix if `sparse = FALSE`,
#'   otherwise a `Matrix::dgCMatrix`) with row/col names = `grid_id`.
#' @export
build_grid_adjacency <- function(
    iso3,
    admin_level,
    admin_name,
    data_dir = "data/proc",
    target_crs = NULL,
    sparse = TRUE,
    model
) {
  stopifnot(requireNamespace("sf", quietly = TRUE))
  stopifnot(requireNamespace("Matrix", quietly = TRUE))
  
  location <- build_location_identifiers(iso3, admin_level, admin_name)
  location_slug <- location$slug
  
  grid_path <- file.path(data_dir, paste0("spatial_", location_slug, "_hex_grid.Rds"))
  if (!file.exists(grid_path)) stop("Hex grid not found at ", grid_path, call. = FALSE)
  grid <- readRDS(grid_path)
  if (!inherits(grid, "sf")) stop("Hex grid must be an sf object.", call. = FALSE)
  
  if (missing(model)) {
    stop("`model` must be provided as an object or file path.", call. = FALSE)
  }
  
  if (is.character(model) && length(model) == 1L) {
    if (!file.exists(model)) stop("Model dataset not found at ", model, call. = FALSE)
    model_data <- readRDS(model)
  } else {
    model_data <- model
  }
  
  if (!("grid_id" %in% names(grid))) stop("Grid sf object must contain a `grid_id` column.", call. = FALSE)
  if (!("grid_id" %in% names(model_data))) stop("Model data must contain a `grid_id` column.", call. = FALSE)
  
  # keep only ids in model data
  model_ids <- unique(as.character(model_data$grid_id))
  grid$grid_id <- as.character(grid$grid_id)
  
  if (!all(model_ids %in% grid$grid_id)) {
    missing <- setdiff(model_ids, grid$grid_id)
    warning("Some model grid_id not found in grid (showing up to 20): ",
            paste(head(missing, 20), collapse = ", "), call. = FALSE)
  }
  
  grid_sub <- grid[grid$grid_id %in% model_ids, , drop = FALSE]
  if (nrow(grid_sub) < 2) stop("Not enough grid cells after subsetting to build adjacency.", call. = FALSE)
  if (anyDuplicated(grid_sub$grid_id)) stop("Duplicate `grid_id` found in grid.", call. = FALSE)
  
  grid_sub <- grid_sub[order(grid_sub$grid_id), , drop = FALSE]
  
  # ---- CRS handling (global-safe) ----
  is_longlat <- sf::st_is_longlat(grid_sub)
  
  if (!is.null(target_crs)) {
    grid_sub <- sf::st_transform(grid_sub, target_crs)
  } else if (is_longlat) {
    # choose UTM zone from centroid (works globally)
    cxy <- sf::st_coordinates(sf::st_centroid(sf::st_union(sf::st_geometry(grid_sub))))
    lon <- cxy[1]; lat <- cxy[2]
    zone <- floor((lon + 180) / 6) + 1
    epsg <- if (lat >= 0) 32600 + zone else 32700 + zone
    grid_sub <- sf::st_transform(grid_sub, epsg)
  }
  # If already projected and target_crs is NULL, leave as-is.
  
  # ---- Queen contiguity adjacency ----
  if (sparse) {
    rel <- sf::st_relate(grid_sub, grid_sub, pattern = "F***T****", sparse = TRUE)
    n <- nrow(grid_sub)
    i <- rep(seq_len(n), lengths(rel))
    j <- unlist(rel, use.names = FALSE)
    
    W <- Matrix::sparseMatrix(i = i, j = j, x = 1, dims = c(n, n))
    dimnames(W) <- list(grid_sub$grid_id, grid_sub$grid_id)
    
    # ensure symmetry + diagonal
    W <- (W + Matrix::t(W)) > 0
    W <- Matrix::Matrix(W * 1, sparse = TRUE)
    Matrix::diag(W) <- 1
  } else {
    W <- 1 * sf::st_relate(grid_sub, grid_sub, pattern = "F***T****", sparse = FALSE)
    rownames(W) <- grid_sub$grid_id
    colnames(W) <- grid_sub$grid_id
    diag(W) <- 1
  }
  
  # sanity: isolated nodes check
  rs <- Matrix::rowSums(W)
  if (any(rs <= 1)) {
    iso <- names(rs)[rs <= 1]
    warning("Some grid cells appear isolated (no neighbors besides self). ",
            "This can be okay at borders, but check if unexpected. ",
            "Example ids: ", paste(head(iso, 10), collapse = ", "),
            call. = FALSE)
  }
  
  W
}

build_location_identifiers <- function(iso3, admin_level, admin_name) {
  if (length(iso3) != 1 || nchar(iso3) != 3) {
    stop("`iso3` must be a single three-character ISO3 code.", call. = FALSE)
  }
  
  iso3_slug <- tolower(iso3)
  admin_level_slug <- as.character(admin_level)
  admin_name_slug <- tolower(admin_name)
  admin_name_slug <- gsub("[^a-z0-9]+", "_", admin_name_slug)
  admin_name_slug <- gsub("^_+|_+$", "", admin_name_slug)
  
  list(
    iso3 = iso3_slug,
    admin_level = admin_level_slug,
    admin_name = admin_name_slug,
    slug = paste(iso3_slug, admin_level_slug, admin_name_slug, sep = "_")
  )
}
