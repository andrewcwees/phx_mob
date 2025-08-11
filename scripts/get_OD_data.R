# libs ####
library(sf)
library(httr)
library(dplyr)
library(FSA)
library(skimr)
library(ggpubr)
library(data.table)
library(deweydatar)
library(units)
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

# origins ####
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')
#census_api_key("7fa16e3f5ceb46bb8e23071c2a86bc0127b228fb", install = T)

# block level population data
blocks <- blocks("AZ", county = "Maricopa", year = 2020, class = "sf")

pop20 <- get_decennial(
  geography = "block",
  variables = "P1_001N",
  state = "AZ",
  county = "Maricopa",
  year = 2020,
  geometry = F) %>%
  rename(GEOID20 = GEOID)

blocks <- blocks %>%
  left_join(pop20, by = "GEOID20") %>%
  mutate(pop = as.numeric(value)) %>%
  dplyr::filter(pop > 0) %>%
  mutate(centroid = st_centroid(geometry),
         x = st_coordinates(centroid)[, 1],
         y = st_coordinates(centroid)[, 2],
         bg = substr(GEOID20, 1, 12))

bg_pw_centroids <- blocks %>%
  group_by(bg) %>%
  summarise(
    pw_x = sum(pop * x, na.rm = T) / sum(pop, na.rm = T),
    pw_y = sum(pop * y, na.rm = T) / sum(pop, na.rm = T)) %>%
  st_as_sf(coords = c("pw_x", "pw_y"), crs = 4326)

# cbg level acs
vars <- c(
  inc           = "B19013_001",
  age           = "B01002_001",
  hh_size       = "B25010_001",
  hh_total      = "B25003_001",
  hh_own        = "B25003_002",
  hh_rent       = "B25003_003",
  pop_total    = "B02001_001",
  pop_wht     = "B02001_002",
  pop_blk     = "B02001_003",
  pop_asn     = "B02001_005",
  pop_hisp      = "B03003_003")

acs_data <- get_acs(
  geography = "block group",
  variables = vars,
  state = "AZ",
  county = "Maricopa",
  year = 2023,
  survey = "acs5",
  geometry = T) %>% 
  dplyr::select(GEOID, variable, estimate) %>%
  pivot_wider(
    names_from = variable, 
    values_from = estimate) %>%
  mutate(
    pct_wht = pop_wht / pop_total,
    pct_blk = pop_blk / pop_total,
    pct_asn = pop_asn / pop_total,
    pct_hisp = pop_hisp / pop_total,
    pct_own = hh_own / hh_total,
    pct_rent = hh_rent / hh_total,
    area = as.numeric(st_area(geometry)),
    density = (pop_total / area) * 1000000) %>%
  st_drop_geometry()

# merge acs estimates to weighted centroids at cbg level
origins_pw <- bg_pw_centroids %>%
  rename(GEOID = bg) %>%
  left_join(acs_data, by = "GEOID") %>%
  na.omit() %>%
  mutate(
    across(starts_with("pct_"), ~ round(.x, 2)),
    age = round(age),
    area = round(area),
    population = pop_total) %>%
  dplyr::select(-starts_with("hh_"), -starts_with("pop_"))

mapview(origins_pw)

# spatial filter by distance from center
origins_urban <- origins_pw %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c('pw_x', 'pw_y'), crs = 4326)

mean_center <- st_coordinates(origins_urban) %>%
  as.data.frame() %>%
  mutate(pop = origins_urban$population) %>%
  summarise(
    lon = sum(X * pop) / sum(pop),
    lat = sum(Y * pop) / sum(pop)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

mapview(mean_center, col.regions = 'red') +
  mapview(origins_urban)

filter_by_distance <- function(blocks, center, max_km) {
  blocks %>%
    mutate(dist_to_center = st_distance(geometry, center)) %>%
    mutate(dist_to_center = set_units(dist_to_center, "km")) %>%
    filter(dist_to_center <= set_units(max_km, "km"))
}

origins_20km <- filter_by_distance(origins_urban, mean_center, 20)
origins_16km <- filter_by_distance(origins_urban, mean_center, 16)
mapview(mean_center, col.regions = 'red') +
  mapview(origins_20km, col.regions = 'blue') +
  mapview(origins_16km, col.regions = 'green')

origins_urban <- origins_16km


# destinations ####
#rm(list = ls())
#gc()
#setwd('D:/research/phxmob/data')

# define paths
#key <- "zIQjCicP.f5XRF7rLlqtTqoaOIiaiFaf4gVCmgCFwei0voAGbA2ba8JxCL16lTZ3S"
#download_dir <- "D:/research/phxmob/data/mpat"
#meta_url <- "https://app.deweydata.io/external-api/v3/products/5acc9f39-1ca6-4535-b3ff-38f6b9baf85e/files/metadata"
#base_url <- "https://app.deweydata.io/external-api/v3/products/5acc9f39-1ca6-4535-b3ff-38f6b9baf85e/files"

# get metadata
#response <- GET(meta_url, add_headers(`x-api-key` = key))
#metadata <- content(response, as = "parsed", type = "application/json") 

# get file_list
#date_start <- "2023-11-01"
#date_end <- "2023-11-30"

#response <- GET(base_url,add_headers(`x-api-key` = key),
#                query = list(
#                  partition_key_after = date_start,
#                  partition_key_before = date_end))
#file_list <- content(response, as = "parsed", type = "application/json")

# download
#download_links <- file_list$download_links

#walk(download_links, function(file) {
#  file_url <- file$link
#  file_name <- file$file_name
#  file_path <- file.path(download_dir, file_name)
#  
#  if (!is.null(file_url)) {
#    GET(file_url, write_disk(file_path, overwrite = TRUE))
#    message("Downloaded: ", file_name)
#  } else {
#    warning("Missing download URL for: ", file_name)
#  }
#})
#####
# unpack AZ advan files
setwd('D:/research/phxmob/data')

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
  'category',
  #'vizcbg',
  'lon',
  'lat')

code_filt <- c(
  445110,  # grocery
  446110,  # pharmacy
  712190,  # park
  611110,  # school
  519120)   # library

blacklist <- c(
  # general terms
  "Liquor", 
  "Gas", 
  "Marijuana", 
  "Cannabis", 
  "Dispensary", 
  "CBD",
  "Zen",
  "Infusion", 
  "Medical", 
  "Clinic",
  
  # specific businesses
  "Encanto Green Cross", 
  "Zenleaf", 
  "Wickenburg Alternative Medicine", 
  "YiLo", 
  "Wendy Wells NMD Wellsource Naturopathic Medical Center",
  "Alcoholism Treatment and Drug Abuse Program", 
  "All Greens Clinic", 
  "Zoomies RX", 
  "Trew Balance", 
  "Theresa N Losi DPM", 
  "The Tactical Medic", 
  "Dr Reeferalz")

whitelist <- c(
  "High",
  "Elementary",
  "Primary",
  "Secondary")

blacklist_regex <- paste(blacklist, collapse = "|")
whitelist_regex <- paste(whitelist, collapse = "|")

dest <- az_data %>% 
  rename(
    name = LOCATION_NAME,
    #vizcbg = VISITOR_HOME_CBGS,
    code = NAICS_CODE,
    lon = LONGITUDE,
    lat = LATITUDE) %>% 
  mutate(
    category = case_when(
      code == 445110 ~ "grocery",
      code == 446110 ~ "pharmacy",
      code == 712190 ~ "park",
      code == 611110 ~ "schools",
      code == 519120 ~ "library",
      T ~ "other")) %>%
  dplyr::filter(code %in% code_filt) %>%
  dplyr::filter(
    !grepl(blacklist_regex, name, ignore.case = T),
    if_else(
      category == "schools",
      grepl(whitelist_regex, name, ignore.case = T), T)) %>%
  #dplyr::filter(vizcbg != "") %>%
  dplyr::select(all_of(var_filt)) %>%             
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

# add transit stops from valley metro GTFS
stops <- fread("valleymetro/stops.txt") %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
  dplyr::select(stop_name) %>%
  mutate(category = 'transit') %>%
  rename(name = stop_name)
dest <- rbind(dest, stops)

# filter dest by proximity to origins_urban
filter_by_distance <- function(points, reference, max_km) {
  
  dist_matrix <- st_distance(points, reference)
  
  min_dist <- apply(dist_matrix, 1, min)
  min_dist <- set_units(min_dist, 'm')
  
  points[min_dist <= set_units(max_km, 'km'), ]
}

dest_urban <- filter_by_distance(dest, origins_urban, 10)

# count and map
table(dest_urban$category)
nrow(dest_urban)
nrow(origins_urban)
mapview(mean_center, col.regions = 'red') +
  mapview(origins_urban, col.regions = 'blue') +
  mapview(dest, zcol = 'category') +
  mapview(dest_urban, zcol = 'category')

# save
st_write(origins_urban, 'origins.gpkg', append = F)
st_write(dest_urban, 'destinations.gpkg', append = F)


