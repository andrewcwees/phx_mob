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
options(tigris_use_cache = TRUE)


# spatial filter by county boundary ####
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')

destinations <- fread('destinations.csv') %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)
bound <- st_read('boundary.gpkg') %>% 
  st_transform(crs = 4326)

test <- destinations %>% st_filter(bound, .predicate = st_within)
mapview(test) + mapview(destinations) + mapview(bound)

# ID filters ####
origins <- fread('origins.csv') %>%          # n = 2,659
  mutate(GEOID = as.character(as.numeric(GEOID)))
trips <- fread('trips.csv') %>%              # n = 140,007
  mutate(GEOID = as.character(as.numeric(GEOID)))
destinations <- fread('destinations.csv')    # n = 2,647

# 1.) filter trips$GEOID and origins$GEOID by common strings
common_geoid <- intersect(origins$GEOID, trips$GEOID)

origins <- origins %>% filter(GEOID %in% common_geoid) 
trips  <- trips  %>% filter(GEOID %in% common_geoid) 

# 2.) filter trips$destid and destinations$destid by common strings
common_destid <- intersect(destinations$destid, trips$destid)

destinations <- destinations %>% filter(destid %in% common_destid)
trips <- trips %>% filter(destid %in% common_destid)

# 3.) checks 
length(unique(trips$destid)) # should = nrow(destinations)
length(unique(trips$GEOID)) # should = nrow(origins)

# save filtered sets
fwrite(origins, 'origins_f.csv') # n = 2,084
fwrite(trips, 'trips_f.csv')   # n = 21,846
fwrite(destinations, 'destinations_f.csv') # n = 646



# business filters ####
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')

destinations <- fread('destinations_f.csv')
table(destinations$type)

# pharmacy filters
destinations <- destinations %>% 
  dplyr::filter(
    
    # general terms
    !grepl("Marijuana", name, ignore.case = T),
    !grepl("Cannabis", name, ignore.case = T),
    !grepl("Dispensary", name, ignore.case = T),
    !grepl("CBD", name, ignore.case = T),
    !grepl("Infusion", name, ignore.case = T),
    !grepl("Medical", name, ignore.case = T),
    !grepl("Clinic", name, ignore.case = T),
    
    # specific stores
    !grepl("Encanto Green Cross", name, ignore.case = T),
    !grepl("Zen Medical", name, ignore.case = T),
    !grepl("Zen Leaf", name, ignore.case = T),
    !grepl("Zenleaf", name, ignore.case = T),
    !grepl("Wickenburg Alternative Medicine", name, ignore.case = T),
    !grepl("YiLo", name, ignore.case = T),
    !grepl("Wendy Wells NMD Wellsource Naturopathic Medical Center", name, ignore.case = T),
    !grepl("Alcoholism Treatment and Drug Abuse Program", name, ignore.case = T),
    !grepl("All Greens Clinic", name, ignore.case = T),
    !grepl("Zoomies RX", name, ignore.case = T),
    !grepl("Trew Balance", name, ignore.case = T),
    !grepl("Theresa N Losi DPM", name, ignore.case = T),
    !grepl("The Tactical Medic", name, ignore.case = T),
    !grepl("Dr Reeferalz", name, ignore.case = T),)

table(destinations$type)










