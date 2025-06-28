# libs ####
library(sf)
library(httr)
library(dplyr)
library(skimr)
library(ggpubr)
library(data.table)
library(terra)
library(readr)
library(leaflet)
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
setwd('D:/research/phxmob/data')

bound <- st_read('boundary.gpkg') %>% 
  st_transform(crs = 4326)

destinations <- fread('destinations_f.csv') %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

# return point geometries to polygon for origins
origins <- fread('origins_f.csv') %>%
  mutate(GEOID = as.character(GEOID))
blocks <- st_read('blocks.gpkg') %>%
  mutate(GEOID = as.character(as.numeric(GEOID)))
origins <- inner_join(origins, blocks) %>% st_as_sf()


# mapview ####

mapview(origins, zcol = 'tile_popd') + 
  mapview(origins, zcol = 'tile_pop') +
  mapview(origins, zcol = 'tile_inc') +
  mapview(destinations, col.regions = 'red')


# test leaflet() ####
colorpal <- colorNumeric(palette = "YlOrRd", domain = origins$tile_popd)

leaflet() %>% addTiles() %>%
  # + origins
  addPolygons(
    data = origins,
    fillColor = ~colorpal(tile_popd),
    color = "black",
    weight = 1,
    fillOpacity = 0.7,
    opacity = 1,
    label = ~tile_popd,
    group = "Population Density Decile") %>%
  # + destinations
  addCircleMarkers(
    data = destinations,
    stroke = T,
    radius = 1,
    fillOpacity = 1,
    opacity = 1,
    group = "Amenities") %>%
  # + boundary line
  addPolygons(
   data = bound,
   color = "red",
    weight = 2,
    fill = F,
    fillOpacity = 0,
    opacity = 1,
    group = "Boundary") %>%
  # + legend
  addLegend(
    "bottomright",
    pal = colorpal,
    values = origins$tile_popd,
    title = "Population Density",
    opacity = 1)












