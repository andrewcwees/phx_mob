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
library(terra)
library(tidyverse)
library(openrouteservice)
library(ggspatial)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)

#####

# clear environment
rm(list = ls())
gc()

# load files
setwd('D:/research/phxmob/data')
trips <- st_read('trips/trips.shp') %>% st_transform(crs = 4326) %>%
  # exclude hospitals before calculating walk times to reduce API calls
  dplyr::filter(TOP_CAT != 'General Medical and Surgical Hospitals')
cbg <- st_read('acs/acs.shp') %>% st_transform(crs = 4326) %>% st_centroid()

categories <- trips %>% count(TOP_CAT, sort = T)

# set token
mapbox_token <- "pk.eyJ1IjoiYW5kcmV3Y3dlZXMiLCJhIjoiY21hdTJpdTFvMTIwNTJqb3RtaWp0ZWhuayJ9.cFrjd6jGuVVxTazBj5I5rA"

# define function for calculating walk time between two points
get_walk_time <- function(origin_lon, origin_lat, dest_lon, dest_lat, token) {
  
  base_url <- "https://api.mapbox.com/directions/v5/mapbox/walking"
  coords <- paste0(origin_lon, ",", origin_lat, ";", dest_lon, ",", dest_lat)
  url <- paste0(base_url, "/", coords, "?access_token=", token, "&overview=false")
  
  response <- httr::GET(url)
  
  if (response$status_code == 200) {
    res <- jsonlite::fromJSON(httr::content(response, as = "text"), simplifyVector = FALSE)
    
    if (length(res$routes) > 0 && !is.null(res$routes[[1]]$duration)) {
      return(res$routes[[1]]$duration / 60)  # minutes
    } else {
      warning("No route found for coordinates: ", coords)
      return(NA)
    }
  } else {
    warning("API call failed (", response$status_code, ") for coordinates: ", coords)
    return(NA)
  }
}

# define function for locating nearest POI by amenity category
nearest <- function(categories, cbg, trips, token) {
  
  poi_subset   <- trips %>% dplyr::filter(TOP_CAT == categories)
  nearest_idx  <- st_nearest_feature(cbg, poi_subset)
  nearest_poi  <- poi_subset[nearest_idx, ]                
  
  cbg_xy       <- st_coordinates(cbg)
  poi_xy       <- st_coordinates(nearest_poi)
  
  n            <- nrow(cbg)
  walk_minutes <- numeric(n)
  
  for (i in seq_len(n)) {
    walk_minutes[i] <- get_walk_time(
      origin_lon = cbg_xy[i, "X"],
      origin_lat = cbg_xy[i, "Y"],
      dest_lon   = poi_xy[i, "X"],
      dest_lat   = poi_xy[i, "Y"],
      token      = token
    )
    
    # ping every 100 calls
    if (i %% 100 == 0) message("Checkpoint ", i, " (", categories, ")")
    Sys.sleep(0.15)   # be considerate :)
  }
  
  tibble(
    !!paste0("min_", categories) := walk_minutes,
    !!paste0("loc_", categories) := nearest_poi$NAME
  )
}


all_cats <- sort(unique(trips$TOP_CAT))

walk_times <- cbg %>% bind_cols(map_dfc(
  all_cats, 
  nearest,  
  cbg = cbg,
  trips = trips,
  token = mapbox_token))

# simplify values & variable names
save <- walk_times %>% 
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>% 
  rename(
    # grocery
    min_groc = `min_Grocery Stores`,
    loc_groc = `loc_Grocery Stores`,
    # public transit
    min_tran = `min_Urban Transit Systems`,
    loc_tran = `loc_Urban Transit Systems`,
    # recreation
    min_park = `min_Museums, Historical Sites, and Similar Institutions`,
    loc_park = `loc_Museums, Historical Sites, and Similar Institutions`,
    # gas station
    min_gas = `min_Gasoline Stations`,
    loc_gas = `loc_Gasoline Stations`,
    # restaurants
    min_res = `min_Restaurants and Other Eating Places`,
    loc_res = `loc_Restaurants and Other Eating Places`) %>% 
  dplyr::select(-NAM)

# save
st_write(save, 'iso/iso.shp', delete_layer = T)

