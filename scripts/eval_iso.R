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
options(tigris_use_cache = T)

#####

rm(list = ls())
gc()

data <- st_read('D:/research/phxmob/data/iso/iso.shp') %>%
  mutate(
    ntile_pop = ntile(total, 5),
    ntile_inc = ntile(medinc, 5)) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 2)))

avg_min_pop <- data %>%
  st_drop_geometry() %>%
  group_by(ntile_pop) %>%
  summarise(
    avg_min_groc = mean(min_groc, na.rm = T),
    avg_min_schl = mean(min_schl, na.rm = T),
    avg_min_ccare = mean(min_ccare, na.rm = T))

avg_min_inc <- data %>%
  st_drop_geometry() %>%
  group_by(ntile_inc) %>%
  summarise(
    avg_min_groc = mean(min_groc, na.rm = T),
    avg_min_schl = mean(min_schl, na.rm = T),
    avg_min_ccare = mean(min_ccare, na.rm = T))

# export tables
fwrite(avg_min_pop, 'D:/research/phxmob/data/avg_min_pop.csv')
fwrite(avg_min_inc, 'D:/research/phxmob/data/avg_min_inc.csv')