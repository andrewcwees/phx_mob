# libs ####
library(sf)
library(httr)
library(dplyr)
library(skimr)
library(ggpubr)
library(data.table)
library(terra)
library(readr)
library(purrr)
library(jsonlite)
library(glue)
library(mapview)
library(tidyr)
library(osrm)
options(java.parameters = "-Xmx4G")
library(rJava)
library(r5r)
library(terra)
library(tidyverse)
library(openrouteservice)
library(ggspatial)
library(tidycensus)
library(tigris)
options(tigris_use_cache = T)

# load data and network ####
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')

origins <- st_read("origins.gpkg") %>% 
  mutate(id = as.character(row_number())) 
destinations <- st_read("destinations.gpkg") %>%
  rename('cat' = 'category')

# load network
deptime <- as.POSIXct(
  "26-11-2023 11:00:00", 
  format = "%d-%m-%Y %H:%M:%S")

r5r_core <- setup_r5(
  data_path = "D:/research/phxmob/data/gtfs",
  verbose = F)

cutoffs <- c(15,20,30,40,50,60)

modes <- list(
  walk = "WALK",
  #bus = c("WALK", "TRANSIT"),
  bike = "BICYCLE")

cats <- c(
  "grocery", 
  "park", 
  "pharmacy", 
  "schools", 
  "library",
  "transit")

# define comp_access function
comp_access <- function(dest_cat, mode_name, mode_value) {
  message(glue::glue("Processing: {dest_cat} - {mode_name}"))
  
  dest_subset <- destinations %>%
    dplyr::filter(cat == dest_cat) %>%
    mutate(id = as.character(row_number()), n_dest = 1)
  
  acc <- accessibility(
    r5r_core = r5r_core,
    origins = origins,
    destinations = dest_subset,
    opportunities_colnames = "n_dest",
    mode = mode_value,
    departure_datetime = deptime,
    decay_function = "step",
    cutoffs = cutoffs,
    max_trip_duration = 60,
    time_window = 120,
    progress = T) %>%
    dplyr::select(id, cutoff, accessibility) %>%
    rename(!!paste0("n_", mode_name) := accessibility) %>%
    pivot_wider(
      id_cols = id,
      names_from = cutoff,
      values_from = paste0("n_", mode_name),
      names_glue = paste0("n_{cutoff}_", mode_name))
  
  return(acc)
}

# loop through cat/mode
access <- origins %>% dplyr::select(id) 

for (cat in cats) {
  acc_list <- map2(names(modes), modes, function(mode_name, mode_value) {
    comp_access(cat, mode_name, mode_value)
  })
  
  acc_all_modes <- reduce(acc_list, left_join, by = "id")
  
  names(acc_all_modes)[-1] <- paste0(names(acc_all_modes)[-1], "_", cat)
  
  access <- left_join(access, acc_all_modes, by = "id")
}

# L pivot -> create 'cut' 'mode' 'cat' 'count' 
access_long <- access %>%
  st_drop_geometry() %>%
  pivot_longer(
    cols = starts_with("n_"),
    names_to = c("cut", "mode", "cat"),
    names_pattern = "^n_(\\d+)_(\\w+)_(.+)$",
    values_to = "count") %>% 
  na.omit() %>%
  mutate(
    cut = factor(
      cut, levels = sort(
        unique(as.numeric(cut))), ordered = T))

# + mode 'any'
any_mode <- access_long %>%
  group_by(id, cut, cat) %>%
  summarise(count = max(count, na.rm = T), .groups = "drop") %>%
  mutate(mode = "any") %>%
  select(names(access_long))

access_long <- bind_rows(access_long, any_mode)

# + cat 'total'
total_cat <- access_long %>%
  group_by(id, cut, mode) %>%
  summarise(count = sum(count, na.rm = T), .groups = "drop") %>%
  mutate(cat = "total") %>%
  select(names(access_long))

access_long <- bind_rows(access_long, total_cat)

# + access_long$ttnearest (min cut to nearest per cat/mode)
access_long <- access_long %>%
  group_by(id, mode, cat) %>%
  mutate(
    ttnearest = if (any(count > 0)) {
      min(cut[count > 0])} else {
        NA}) %>%
  ungroup() %>%
  mutate(
    ttnearest = factor(ttnearest, 
                       levels = c(levels(cut), NA), 
                       ordered = T))

# + ACS estimates
access_allvars <- left_join(access_long, origins, by= 'id')

# save
st_write(access_allvars, 'access.gpkg', append = F)





