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

rm(list = ls())
gc()

poi <- st_read('D:/research/phxmob/data/maricopa_poi/maricopa_poi.shp') %>% 
  st_transform(crs = 4326) %>%
  dplyr::select(-REGION) %>%
  rename(NAME = LOCATIO, ADDRESS = STREET_)
cbg <- st_read('D:/research/phxmob/data/maricopa_cbg/cbg_points.shp') %>% 
  st_transform(crs = 4326)

cat_counts <- poi %>% count(TOP_CAT, sort = T)
print(cat_counts)

mapview(poi, zcol = 'TOP_CAT')

# exclude transit & refine grocery list == poi_test
grocery_chains_list <- c(
  'Food City Arizona',
  'Safeway',
  'Sprouts',
  "Fry's Food & Drug Stores",
  "Trader Joe's",
  "Bashas' Supermarkets",
  'Whole Foods Market')

groceries <- poi %>% dplyr::filter(
  TOP_CAT == 'Grocery Stores',
  NAME %in% grocery_chains_list)

schools <- poi %>% dplyr::filter(
  TOP_CAT %in% c('Elementary and Secondary Schools', 'Child Day Care Services'))

transit <- poi %>% dplyr::filter(
  TOP_CAT == 'Urban Transit Systems')

poi_test <- rbind(schools, groceries)
poi_test <- rbind(poi_test, transit)

mapview(poi_test, zcol = 'TOP_CAT')


# calc iso by TOP_CAT ####
mapbox_token <- "pk.eyJ1IjoiYW5kcmV3Y3dlZXMiLCJhIjoiY21hdTJpdTFvMTIwNTJqb3RtaWp0ZWhuayJ9.cFrjd6jGuVVxTazBj5I5rA"

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

nearest <- function(cat_label, cbg, poi_test, token) {
  
  poi_subset   <- poi_test %>% filter(TOP_CAT == cat_label)
  nearest_idx  <- st_nearest_feature(cbg, poi_subset)
  nearest_poi  <- poi_subset[nearest_idx, ]                
  
  cbg_xy       <- st_coordinates(cbg)
  poi_xy       <- st_coordinates(nearest_poi)
  
  # pre‑allocate vectors
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
    
    # notif every 100 calls
    if (i %% 100 == 0) message("Checkpoint ", i, " (", cat_label, ")")
    Sys.sleep(0.15)   # :)
  }
  
  tibble(
    !!paste0("min_", cat_label) := walk_minutes,
    !!paste0("loc_", cat_label) := nearest_poi$NAME
  )
}

# ------------------------------------------------------------------
all_cats <- sort(unique(poi_test$TOP_CAT))

cbg2 <- cbg %>%                       
  bind_cols(
    map_dfc(all_cats, nearest,  
            cbg       = cbg,
            poi_test  = poi_test,
            token     = mapbox_token))

# simplify var names & save
save <- cbg2 %>% mutate(across(where(is.numeric), ~ round(.x, 2))) %>% 
  rename(
    min_ccare = `min_Child Day Care Services`,
    loc_ccare = `loc_Child Day Care Services`,
    min_schl = `min_Elementary and Secondary Schools`,
    loc_schl = `loc_Elementary and Secondary Schools`,
    min_groc = `min_Grocery Stores`,
    loc_groc = `loc_Grocery Stores`,
    min_tran = `min_Urban Transit Systems`,
    loc_tran = `loc_Urban Transit Systems`)

st_write(save, 'D:/research/phxmob/data/iso/iso.shp', delete_layer = T)

