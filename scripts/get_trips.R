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

# define paths
key <- "cS69Lg4d.OoXMpWPlV9ZlQmtt597fJQHZyhfw3DwtzBV8qG3vzT0QGgYxmZIckakY"
download_dir <- "D:/research/phxmob/data/advan_mpat"
meta_url <- "https://app.deweydata.io/external-api/v3/products/5acc9f39-1ca6-4535-b3ff-38f6b9baf85e/files/metadata"
base_url <- "https://app.deweydata.io/external-api/v3/products/5acc9f39-1ca6-4535-b3ff-38f6b9baf85e/files"

# get metadata
response <- GET(meta_url, add_headers(`x-api-key` = key))
metadata <- content(response, as = "parsed", type = "application/json") 

# get file_list
date_start <- "2023-01-01"
date_end <- "2025-01-01"

response <- GET(base_url,add_headers(`x-api-key` = key),
                query = list(
                  partition_key_after = date_start,
                  partition_key_before = date_end))
file_list <- content(response, as = "parsed", type = "application/json")

# download
download_links <- file_list$download_links

walk(download_links, function(file) {
  file_url <- file$link
  file_name <- file$file_name
  file_path <- file.path(download_dir, file_name)
  
  if (!is.null(file_url)) {
    GET(file_url, write_disk(file_path, overwrite = TRUE))
    message("Downloaded: ", file_name)
  } else {
    warning("Missing download URL for: ", file_name)
  }
})

# filter for trips in AZ & rbind()
rm(list = ls())
gc()

load_comb <- function(path = "D:/research/phxmob/data/advan_mpat/") {
  files <- list.files(path, pattern = "\\.csv\\.gz$", full.names = T)
  
  comb <- rbindlist(lapply(files, function(file) {
    dt <- fread(file)
    dt <- dt[REGION == "AZ"]  
    return(dt)
  }), use.names = T, fill = T)
  
  return(comb)
}

az_data <- load_comb()

# define variables to keep
keep_vars <- c(
  'LOCATION_NAME',
  'TOP_CATEGORY',
  'SUB_CATEGORY',
  'NAICS_CODE',
  'LATITUDE',
  'LONGITUDE',
  'STREET_ADDRESS',
  'CITY',
  'REGION',
  'CATEGORY_TAGS',
  'POI_CBG')

# define NAICS codes to keep
keep_naics <- c(
  
  # hospitals
  622110, 622210, 622310,
  # parks
  712190, 713990,
  # public transport
  485111, 485113, 485210, 485310, 485510, 485991, 485999,
  # restaurants
  722511, 722513, 722514, 722515,
  # gas stations
  447110, 447190,
  # grocery
  445110, 445120, 445230)

fdata <- az_data %>% 
  dplyr::select(all_of(keep_vars)) %>%
  dplyr::filter(NAICS_CODE %in% keep_naics) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) 

# filter for county_bound
bound <- st_read('county_bound/county_bound.shp') %>% st_transform(crs = 4326)

trips <- st_filter(fdata, bound, .predicate = st_within)

mapview(trips) + mapview(bound)

categories <- as.list(unique(trips$TOP_CATEGORY))
codes <- as.list(unique(trips$NAICS_CODE))

# filter out irrelevant categories in trips$TOP_CATEGORY -> save trips.shp

cat_filt <- c(
  'Specialty (except Psychiatric and Substance Abuse) Hospitals',
  'Other Transit and Ground Passenger Transportation',
  'Taxi and Limousine Service',
  'Specialty Food Stores',
  'Psychiatric and Substance Abuse Hospitals',
  'Interurban and Rural Bus Transportation',
  'Other Amusement and Recreation Industries')

ftrips <- trips %>% dplyr::filter(!TOP_CATEGORY %in% cat_filt) %>% 
  dplyr::select(-SUB_CAT, -CATEGOR, -REGION, -STREET_) %>%
  rename(
    NAME = LOCATIO,
    NAICS = NAICS_C)

st_write(ftrips, 'D:/research/phxmob/data/trips/trips.shp')



