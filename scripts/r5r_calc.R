# libs ####
library(sf)
library(httr)
library(dplyr)
library(skimr)
library(ggpubr)
library(data.table)
library(terra)
library(readr)
library(jsonlite)
library(mapview)
library(tidyr)
library(osrm)
options(java.parameters = "-Xmx2G")
library(r5r)
library(rJava)
library(terra)
library(tidyverse)
library(openrouteservice)
library(ggspatial)
library(tidycensus)
library(tigris)
options(tigris_use_cache = T)

# proximity ####
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')

# load O/D data
origins <- fread("origins_f.csv") %>%
  mutate(
    id = as.character(row_number()))

destinations <- fread("destinations_f.csv") %>% 
  mutate(
    id = as.character(row_number()),
    n_dest = 1)

# load routing network
deptime <- as.POSIXct("15-11-2023 10:00:00", format = "%d-%m-%Y %H:%M:%S") # wednesday nov 15 2023 @ 10am
valleymetro <- "D:/research/phxmob/data/valleymetro"

r5r_core <- setup_r5(
  data_path = valleymetro,
  verbose = T)

# walk
walk_count <- accessibility(
  r5r_core = r5r_core,
  origins = origins,
  destinations = destinations,
  opportunities_colnames = c("n_dest"),
  mode = "WALK",
  departure_datetime = deptime,
  decay_function = "step",
  cutoffs = 15) %>% 
  dplyr::select(id, accessibility) %>%
  rename(n_stores_walk = accessibility)

# bike
bike_count <- accessibility(
  r5r_core = r5r_core,
  origins = origins,
  destinations = destinations,
  opportunities_colnames = c("n_dest"),
  mode = "BICYCLE",
  departure_datetime = deptime,
  decay_function = "step",
  cutoffs = 15) %>% 
  dplyr::select(id, accessibility) %>%
  rename(n_stores_bike = accessibility)

# car
car_count <- accessibility(
  r5r_core = r5r_core,
  origins = origins,
  destinations = destinations,
  opportunities_colnames = c("n_dest"),
  mode = "CAR",
  departure_datetime = deptime,
  decay_function = "step",
  cutoffs = 10) %>% 
  dplyr::select(id, accessibility) %>%
  rename(n_stores_car = accessibility)

# bus
bus_count <- accessibility(
  r5r_core = r5r_core,
  origins = origins,
  destinations = destinations,
  opportunities_colname = c("n_dest"),
  mode = c("WALK", "TRANSIT"),
  decay_function = "step",
  cutoffs = 15,
  departure_datetime = deptime,
  time_window = 60) %>% 
  dplyr::select(id, accessibility) %>%
  rename(n_stores_bus = accessibility)

# add n_stores variables back to origins -> save proximity.csv
prox <- left_join(origins, walk_count)
prox <- left_join(prox, bike_count)
prox <- left_join(prox, car_count) 
prox <- left_join(prox, bus_count) %>% dplyr::select(-id)

mean(prox$n_stores_car > 10) * 100
mean(prox$n_stores_bus > 10) * 100

fwrite(prox, 'proximity.csv')

# utility ####
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')

# load O/D data
origins <- fread("origins_f.csv") %>%
  mutate(
    id = as.character(row_number()))

destinations <- fread("destinations_f.csv") %>% 
  mutate(
    id = as.character(row_number()),
    n_dest = 1)

# load routing network
deptime <- as.POSIXct("15-11-2023 10:00:00", format = "%d-%m-%Y %H:%M:%S") # wednesday nov 15 2023 @ 10am
valleymetro <- "D:/research/phxmob/data/valleymetro"

r5r_core <- setup_r5(
  data_path = valleymetro,
  verbose = T)

# walk
walk_times <- travel_time_matrix(
  r5r_core = r5r_core,
  origins = origins,
  destinations = destinations,
  mode = "WALK",
  departure_datetime = deptime,
  max_trip_duration = 15) %>%
  rename(
    og_id = from_id,
    dest_id = to_id,
    time = travel_time_p50) %>%
  mutate(local_walk = 1)

# bike
bike_times <- travel_time_matrix(
  r5r_core = r5r_core,
  origins = origins,
  destinations = destinations,
  mode = "BICYCLE",
  departure_datetime = deptime,
  max_trip_duration = 15) %>%
  rename(
    og_id = from_id,
    dest_id = to_id,
    time = travel_time_p50) %>%
  mutate(local_bike = 1)

# car
car_times <- travel_time_matrix(
  r5r_core = r5r_core,
  origins = origins,
  destinations = destinations,
  mode = "CAR",
  departure_datetime = deptime,
  max_trip_duration = 10) %>%
  rename(
    og_id = from_id,
    dest_id = to_id,
    time = travel_time_p50) %>%
  mutate(local_car = 1)

# bus
bus_times <- travel_time_matrix(
  r5r_core = r5r_core,
  origins = origins,
  destinations = destinations,
  mode = c("WALK", "TRANSIT"),
  departure_datetime = deptime,
  max_trip_duration = 15,
  time_window = 60,
  percentiles = 50,
  verbose = T) %>%
  rename(
    og_id = from_id,
    dest_id = to_id,
    time = travel_time_p50) %>%
  mutate(local_bus = 1)

# create grid
grid <- expand.grid(
  og_id = origins$id,
  dest_id = destinations$id,
  stringsAsFactors = F)

# add matrix results
grid <- grid %>%
  left_join(walk_times, by = c("og_id", "dest_id")) %>% # + walk
  mutate(local_walk = if_else(!is.na(time) & time <= 15, 1, 0)) %>%
  dplyr::select(-time) %>%
  
  left_join(bike_times, by = c("og_id", "dest_id")) %>% # + bike
  mutate(local_bike = if_else(!is.na(time) & time <= 15, 1, 0)) %>%
  dplyr::select(-time) %>%
  
  left_join(car_times, by = c("og_id", "dest_id")) %>% # + car
  mutate(local_car = if_else(!is.na(time) & time <= 15, 1, 0)) %>%
  dplyr::select(-time) %>%
  
  left_join(bus_times, by = c("og_id", "dest_id")) %>% # + bus
  mutate(local_bus = if_else(!is.na(time) & time <= 15, 1, 0)) %>%
  dplyr::select(-time)

# load trips -> add trips$total_stops
trips <- fread('trips_f.csv')

total_stops <- trips %>% 
  group_by(GEOID) %>%
  summarise(
    stops_total = sum(stops, na.rm = T),
    .groups = 'drop')

trips <- trips %>% left_join(total_stops, by = 'GEOID')

# add trips$og_id
id_link <- origins %>% 
  dplyr::select(GEOID, id)
id_link$GEOID <- as.numeric(id_link$GEOID)

id_link$GEOID <- as.character(id_link$GEOID)
trips$GEOID <- as.character(trips$GEOID)

trips <- trips %>%
  left_join(id_link, by = 'GEOID') %>% 
  rename(og_id = id)

# fix trips$dest_id
id_link <- destinations %>% 
  dplyr::select(destid, id)

trips <- trips %>% 
  left_join(id_link, by = 'destid') %>%
  rename(dest_id = id)

# add trips$local_walk and trips$local_bike using grid OD pairs
# -> multiply trips$stops * trips$local_x = trips$n_stops_local_x
trips <- trips %>% 
  left_join(grid, by = c('og_id', 'dest_id')) %>%
  mutate(
    stops_local_walk = stops * local_walk,
    stops_local_bike = stops * local_bike,
    stops_local_car = stops * local_car,
    stops_local_bus = stops * local_bus)

# create df for utility
utility <- trips %>% 
  group_by(GEOID) %>%
  summarise(
    stops_total_local_walk = sum(stops_local_walk, na.rm = T),
    stops_total_local_bike = sum(stops_local_bike, na.rm = T),
    stops_total_local_car = sum(stops_local_car, na.rm = T),
    stops_total_local_bus = sum(stops_local_bus, na.rm = T),
    .groups = 'drop')

total_stops$GEOID <- as.character(total_stops$GEOID)

test <- left_join(total_stops, utility, by = 'GEOID') %>%
  mutate(
    perc_local_use_walk = stops_total_local_walk / stops_total,
    perc_local_use_bike = stops_total_local_bike / stops_total,
    perc_local_use_car = stops_total_local_car / stops_total,
    perc_local_use_bus = stops_total_local_bus / stops_total)

fwrite(test, 'utility.csv')

# join ####
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')

prox <- fread('proximity.csv') %>%
  mutate(GEOID = as.character(as.numeric(GEOID))) %>%
  st_drop_geometry() 

util <- fread('utility.csv') %>%
  mutate(
    GEOID = as.character(as.numeric(GEOID)),
    across(where(is.numeric), ~ round(.x, 2)))

test <- left_join(util, prox) %>% na.omit()

fwrite(test, 'util_and_prox.csv')



