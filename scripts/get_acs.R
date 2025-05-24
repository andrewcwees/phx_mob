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

#census_api_key("7fa16e3f5ceb46bb8e23071c2a86bc0127b228fb", install = T)

# 5-year ACS for population/income ####

# clear environment
rm(list = ls())
gc()

# define variable list
vars <- c(
  total = "B02001_001",       
  white = "B02001_002",       
  black = "B02001_003",       
  native = "B02001_004",      
  asian = "B02001_005",       
  pacific = "B02001_006",     
  other = "B02001_007",       
  two_r = "B02001_008", 
  hisp = "B03002_012", 
  medinc = "B19013_001")

# get ACS
acs <- get_acs(
  geography = "block group",
  variables = vars,
  state = "AZ",
  county = "Maricopa",
  year = 2020,
  survey = "acs5",
  geometry = T,
  output = "wide")  %>% dplyr::select(GEOID, geometry, matches("E$")) %>%
  rename_with(~ str_remove(., "E$"), matches("E$")) %>% 
  mutate(
    area = as.numeric(st_area(geometry)),
    pop_density = total / area)

# unify block geometries for boundary file
county_bound <- summarise(acs)

# set density threshold -> filter
density_filter <- 0.0001 

dense_blocks <- acs %>% dplyr::filter(pop_density >= density_filter)

# check drop percentage + map
count_after <- nrow(dense_blocks)
count_before <- nrow(acs)

percent_drop <- (count_before - count_after) / count_before
print(percent_drop)

mapview(acs) + mapview(dense_blocks) + mapview(county_bound)

# save
st_write(dense_blocks, 'D:/research/phxmob/data/acs/acs.shp')
st_write(county_bound, 'D:/research/phxmob/data/county_bound/county_bound.shp')