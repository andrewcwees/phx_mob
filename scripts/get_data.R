# libs ####
library(sf)
library(httr)
library(dplyr)
library(FSA)
library(skimr)
library(ggpubr)
library(data.table)
library(deweydatar)
library(terra)
library(readr)
library(jsonlite)
library(mapview)
library(tidyr)
library(osrm)
library(terra)
library(tidyverse)
library(ggspatial)
library(tidycensus)
library(tigris)
options(tigris_use_cache = T)

# get origins ####
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')
#census_api_key("7fa16e3f5ceb46bb8e23071c2a86bc0127b228fb", install = T)

data <- get_acs(
  geography = "block group",
  variables = c(
    total = "B02001_001", 
    medinc = "B19013_001",
    medage = "B01002_001"),
  state = "AZ",
  county = "Maricopa",
  year = 2023,
  survey = "acs5",
  geometry = T,
  output = "wide")  %>% 
  dplyr::select(GEOID, geometry, matches("E$")) %>%
  rename_with(~ str_remove(., "E$"), matches("E$")) %>% 
  mutate(
    area = as.numeric(st_area(geometry)),
    pop_density = total / area) %>%
  dplyr::select(-NAM) %>% 
  st_transform(4326) %>%
  mutate(
    tile_pop = ntile(total, 10),
    tile_inc = ntile(medinc, 10),
    tile_popd = ntile(pop_density, 10),
    tile_age = ntile(medage, 10),
    across(where(is.numeric), ~ round(.x, 2)))
bound <- summarise(data)
mapview(bound)

# st_centroid(blocks) -> origins
origins <- data %>% 
  na.omit() %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]) %>%
  st_drop_geometry() 

# save
st_write(data, 'origins_sf.gpkg', append = F)
st_write(bound, 'boundary.gpkg', append = F)
fwrite(origins, 'origins.csv')

# get advan ####
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')

# define paths
key <- "zIQjCicP.f5XRF7rLlqtTqoaOIiaiFaf4gVCmgCFwei0voAGbA2ba8JxCL16lTZ3S"
download_dir <- "D:/research/phxmob/data/mpat"
meta_url <- "https://app.deweydata.io/external-api/v3/products/5acc9f39-1ca6-4535-b3ff-38f6b9baf85e/files/metadata"
base_url <- "https://app.deweydata.io/external-api/v3/products/5acc9f39-1ca6-4535-b3ff-38f6b9baf85e/files"

# get metadata
response <- GET(meta_url, add_headers(`x-api-key` = key))
metadata <- content(response, as = "parsed", type = "application/json") 

# get file_list
date_start <- "2023-11-01"
date_end <- "2023-11-30"

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

# unpack AZ advan files into 'trips.csv' and 'destinations.csv' ####
rm(list = ls())
gc()

# function to load AZ files
load_comb <- function(path = "mpat/") {
  files <- list.files(path, pattern = "\\.csv\\.gz$", full.names = T)
  
  comb <- rbindlist(lapply(files, function(file) {
    dt <- fread(file)
    dt <- dt[REGION == "AZ"]  
    return(dt)
  }), use.names = T, fill = T)
  
  return(comb)
}
az_data <- load_comb()

# define filters
var_filt <- c(
  'name',
  'type',
  'lon',
  'lat',
  'vizcbg')

code_filt <- c(
  445110, # grocery
  446110, # pharmacy
  712190  # park
  )

# clean/filter AZ Advan Mpat
destinations <- az_data %>% 
  rename(
    name = LOCATION_NAME,
    vizcbg = VISITOR_HOME_CBGS,
    code = NAICS_CODE,
    lon = LONGITUDE,
    lat = LATITUDE) %>% 
  dplyr::filter(code %in% code_filt) %>%
  dplyr::filter(vizcbg != "") %>%
  mutate(
    type = case_when(
      code == 445110 ~ "grocery",
      code == 446110 ~ "pharmacy",
      code == 712190 ~ "park",
      T ~ "other")) %>%
  dplyr::select(all_of(var_filt))

table(destinations$type) # get count by amenity type 

dest_parsed <- destinations %>% 
  mutate(
    clean_json = gsub('\\"{2}', '"', vizcbg),
    parsed = map(clean_json, safely(fromJSON))) %>% 
  mutate(
    vizcbg_parsed = map(parsed, "result")) %>%
  dplyr::select(-parsed, -clean_json)

trips <- dest_parsed %>%
  unnest_longer(
    vizcbg_parsed, 
    values_to = "stops", 
    indices_to = "GEOID") %>%
  dplyr::select(-vizcbg)

# create destination ID variable as destid = name_lon_lat
trips$destid <- paste(trips$name, 
      as.character(trips$lon), as.character(trips$lat), sep = '_')

destinations$destid <- paste(destinations$name, 
      as.character(destinations$lon), as.character(destinations$lat), sep = '_')

length(unique(trips$destid))  
length(unique(destinations$destid)) 

destinations$vizcbg <- NULL

fwrite(trips, 'trips.csv')
fwrite(destinations, 'destinations.csv')

































