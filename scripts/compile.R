### libs #######################################################################
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
library(exactextractr)
library(stars)
library(raster)
library(tidyverse)
library(ggspatial)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)

#census_api_key("7fa16e3f5ceb46bb8e23071c2a86bc0127b228fb", install = TRUE)

#######

rm(list = ls())
gc()
setwd('D:/research/phxmob/data/')


# census ####
# decennial for population by race/ethnicity
vars <- c(
  total = "P1_001N",
  white = "P1_003N",
  black = "P1_004N",
  native = "P1_005N",
  asian = "P1_006N",
  pacific = "P1_007N",
  other = "P1_008N",
  two_or_more = "P1_009N",
  hispanic_any_race = "P2_002N")

maricopa_dec <- get_decennial(
  geography = "block group",
  variables = vars,
  state = "AZ",
  county = "Maricopa",
  year = 2020,
  geometry = T,
  output = 'wide')

# save maricopa county boundary as .shp
#maricopa_bound <- summarise(maricopa_dec)
#st_write(maricopa_bound, 'D:/research/phxmob/data/maricopa_bound/maricopa_bound.shp')

# acs for median income estimates
acs_income <- get_acs(
  geography = "block group",
  survey = 'acs5',
  variables = 'B19013_001',
  state = 'AZ',
  county = 'Maricopa',
  year = 2020,
  geometry = F) %>% dplyr::select('GEOID', 'estimate') %>% rename(medinc = estimate)

# merge
combined <- left_join(maricopa_dec, acs_income, by = 'GEOID') %>% na.omit()
mapview(combined)

# save cbg polygons
st_write(combined, 'D:/research/phxmob/data/maricopa_cbg/cbg_polygons.shp')

# save cbg centroids
cbg_centroids <- st_centroid(combined)
st_write(cbg_centroids, 'D:/research/phxmob/data/maricopa_cbg/cbg_points.shp')


# POI ####

# download all safegraph POI files (200 csv.gz folders)
rm(list = ls())
gc()

apikey = "cS69Lg4d.OoXMpWPlV9ZlQmtt597fJQHZyhfw3DwtzBV8qG3vzT0QGgYxmZIckakY"

pp_poi = "https://app.deweydata.io/external-api/v3/products/f8b3db87-71a0-4c67-840b-82acb088bc0a/files"

meta = get_meta(apikey, pp_poi, print_meta = TRUE)

files_df = get_file_list(apikey, pp_poi, print_info = T)

download_files(files_df, "D:/research/phxmob/data/poi_data", skip_exists = TRUE)

# filter for POI in Maricopa County
rm(list = ls())
gc()

poi_files <- list.files("D:/research/phxmob/data/poi_data", full.names = TRUE)

poi_data <- lapply(poi_files, function(file) {
  dt <- fread(file)
  dt <- dt[REGION == "AZ"]
  dt <- dt[, lapply(.SD, as.character)]
  return(dt)}) %>% bind_rows() %>%
  dplyr::select(
    'LOCATION_NAME',
    'TOP_CATEGORY',
    'SUB_CATEGORY',
    'LATITUDE',
    'LONGITUDE',
    'STREET_ADDRESS',
    'CITY',
    'REGION',
    'CATEGORY_TAGS')
poi_sf <- st_as_sf(poi_data, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

maricopa_bound <- st_read('D:/research/phxmob/data/maricopa_bound/maricopa_bound.shp') %>% st_transform(crs = 4326)

remove_list <- c(
  'Technical and Trade Schools',
  'Business Schools and Computer and Management Training',
  'Other Schools and Instruction',
  'Colleges, Universities, and Professional Schools',
  'Grocery and Related Product Merchant Wholesalers',
  'Transit and Ground Passenger Transportation',
  'Other Transit and Ground Passenger Transportation')

maricopa_poi <- st_filter(poi_sf, maricopa_bound) %>%
  dplyr::filter(grepl("schools|grocery|child|transit", TOP_CATEGORY, ignore.case = T)) %>%
  dplyr::filter(!(TOP_CATEGORY %in% remove_list))

cat_counts <- maricopa_poi %>% count(TOP_CATEGORY, sort = T)
 
# save
st_write(maricopa_poi, 'D:/research/phxmob/data/maricopa_poi/maricopa_poi.shp')


