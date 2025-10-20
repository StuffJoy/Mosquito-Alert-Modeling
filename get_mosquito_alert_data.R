devtools::install_github("Mosquito-Alert/mosquitoR", ref = "feature/modularize-modeling-data")



library(tidyverse)
library(sf)
library(terra)
# library(rgdal)
library(ggplot2)
library(lubridate)
library(readxl)
library(janitor)
library(RcppRoll)
library(parallel)
#library(RSocrata)
library(data.table)
library(jsonlite)
library(mosquitoR)
library(brms)
library(dplyr)
library(stringr)

source('helpers/parameters.R')


bcn_perimeter = read_rds("data/proc/spatial_esp_4_barcelona_perimeter.rds")  %>% st_set_crs(3035) # resetting CRS to deal with issue of data created on different machines (ignore warning)

bcn_grid = read_rds("data/proc/spatial_esp_4_barcelona_perimeter.rds") %>% st_as_sf() %>% st_set_crs(3035) # resetting CRS to deal with issue of data created on different machines (ignore warning)

bcn_cells = read_rds("data/proc/spatial_esp_4_barcelona_cells.rds")

bcn_map = read_rds("data/proc/spatial_esp_4_barcelona_adm.Rds") %>% st_set_crs(3035) # resetting CRS to deal with issue of data created on different machines (ignore warning)

bcn_landcover = rast("data/proc/spatial_esp_4_barcelona_landcover.tif")

bcn_elevation = rast("data/proc/spatial_esp_4_barcelona_elevation.tif")



# Weather ####

weather_esp_lvl2_barcelona_daily = read_rds("data/proc/weather_esp_lvl2_barcelona_daily.rds")

weather_esp_lvl2_barcelona_lags_7d = read_rds("data/proc/weather_esp_lvl2_barcelona_lags_7d.Rds")

weather_esp_lvl2_barcelona_lags_14d = read_rds("data/proc/weather_esp_lvl2_barcelona_lags_14d.Rds")

weather_esp_lvl2_barcelona_lags_30d = read_rds("data/proc/weather_esp_lvl2_barcelona_lags_30d.Rds")

weather_esp_lvl2_barcelona_lags_21d_lag7 = read_rds("data/proc/weather_esp_lvl2_barcelona_lags_21d_lag7.Rds")

weather_esp_lvl2_barcelona_ppt_lags = read_rds("data/proc/weather_esp_lvl2_barcelona_ppt_lags.Rds")


sampling_effort = read_csv("https://github.com/Mosquito-Alert/sampling_effort_data/raw/main/sampling_effort_daily_cellres_025.csv.gz") 

vrs <- get_malert_data(source = "github", clean = TRUE) %>%
  filter(country == "ESP", report_type == "adult")

D = vrs %>% mutate(year = year(date), sea_days = yday(date), TigacellID_small = make_samplingcell_ids(lon, lat, .025)) %>% 
  filter(date > as_date(android_start_date)) %>% 
  left_join(vrs %>% group_by(user) %>% 
              summarise(mean_score = mean(movelab_certainty_category, na.rm=TRUE))) %>% 
  mutate(reliable_report = (movelab_certainty_category>0 | ( (is.na(movelab_certainty_category) | movelab_certainty_category >=0) & mean_score>0))) %>% 
  filter(reliable_report) %>% filter(TigacellID_small %in% bcn_fua_cells) %>% 
  left_join(weather_esp_lvl2_barcelona_daily %>%dplyr::select(date, mwi, maxTM, meanPPT24H, mwi_zeros_past_14d, FH_zeros_past_14d)) %>% 
  left_join(weather_esp_lvl2_barcelona_ppt_lags %>% dplyr::select(date, PPT_7d_8daysago)) %>% 
  left_join(weather_esp_lvl2_barcelona_lags_30d %>% dplyr::select(date, FW30, FH30, FT30, mwi30)) %>% 
  left_join(weather_esp_lvl2_barcelona_lags_14d %>% dplyr::select(date, FW14, FH14, FT14, mwi14)) %>% 
  left_join(weather_esp_lvl2_barcelona_lags_21d_lag7 %>% dplyr::select(date, FW21, FH21, FT21, mwi21))
