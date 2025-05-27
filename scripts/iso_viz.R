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
library(scales)
library(tidyr)
library(osrm)
library(terra)
library(tidyverse)
library(openrouteservice)
library(ggspatial)
library(tidycensus)
library(tigris)
options(tigris_use_cache = T)

# line plots ####

# clear environment
rm(list = ls())
gc()

# load data -> deciles for pop/density/income
data <- st_read('D:/research/phxmob/data/iso/iso.shp') %>%
  mutate(
    tile_population = ntile(total, 10),
    tile_income = ntile(medinc, 10),
    tile_pdensity = ntile(pp_dnst, 10)) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  st_drop_geometry()

# plot population
plot_pop <- data %>% group_by(tile_population) %>%
  summarise(across(starts_with("min_"), mean, na.rm = T), .groups = "drop") %>% 
  rename(decile = tile_population) %>% 
  pivot_longer(
    cols = starts_with("min_"),
    names_to = "cat",
    values_to = "walktime") %>% 
  mutate(cat = case_when(
    cat == "min_groc" ~ "Grocery Stores",
    cat == "min_gas" ~ "Gas Stations",
    cat == "min_park" ~ "Parks",
    cat == "min_res" ~ "Restaurants",
    cat == "min_tran" ~ "Urban Transit"))

ggplot(plot_pop, aes(x = decile, y = walktime)) +
  geom_line(aes(color = cat), size = 1.2) +
  geom_point(aes(color = cat), size = 3) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(
    breaks = plot_pop$walktime,
    labels = comma) +
  labs(
    title = "Average Walk Time to Nearest POI by Population Count Decile",
    x = "Density Decile",
    y = "Average Walk Time",
    color = "Amenity Category") +
  theme(
    panel.grid.major.x = element_line(color = "gray80"),
    panel.grid.major.y = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1))

# plot density
plot_dens <- data %>% group_by(tile_pdensity) %>%
  summarise(across(starts_with("min_"), mean, na.rm = T), .groups = "drop") %>% 
  rename(decile = tile_pdensity) %>% 
  pivot_longer(
    cols = starts_with("min_"),
    names_to = "cat",
    values_to = "walktime") %>% 
  mutate(cat = case_when(
    cat == "min_groc" ~ "Grocery Stores",
    cat == "min_gas" ~ "Gas Stations",
    cat == "min_park" ~ "Parks",
    cat == "min_res" ~ "Restaurants",
    cat == "min_tran" ~ "Urban Transit"))

ggplot(plot_dens, aes(x = decile, y = walktime)) +
  geom_line(aes(color = cat), size = 1.2) +
  geom_point(aes(color = cat), size = 3) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(
    breaks = plot_dens$walktime,
    labels = comma) +
  labs(
    title = "Average Walk Time to Nearest POI by Population Density Decile",
    x = "Density Decile",
    y = "Average Walk Time",
    color = "Amenity Category") +
  theme(
    panel.grid.major.x = element_line(color = "gray80"),
    panel.grid.major.y = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1))

# plot income
plot_inc <- data %>% group_by(tile_income) %>%
  summarise(across(starts_with("min_"), mean, na.rm = T), .groups = "drop") %>% 
  na.omit() %>% # omit NA column for income (missing data) 
  rename(decile = tile_income) %>% 
  pivot_longer(
    cols = starts_with("min_"),
    names_to = "cat",
    values_to = "walktime") %>% 
  mutate(cat = case_when(
    cat == "min_groc" ~ "Grocery Stores",
    cat == "min_gas" ~ "Gas Stations",
    cat == "min_park" ~ "Parks",
    cat == "min_res" ~ "Restaurants",
    cat == "min_tran" ~ "Urban Transit"))

ggplot(plot_inc, aes(x = decile, y = walktime)) +
  geom_line(aes(color = cat), size = 1.2) +
  geom_point(aes(color = cat), size = 3) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(
    breaks = plot_inc$walktime,
    labels = comma) +
  labs(
    title = "Average Walk Time to Nearest POI by Income Decile",
    x = "Income Decile",
    y = "Average Walk Time",
    color = "Amenity Category") +
  theme(
    panel.grid.major.x = element_line(color = "gray80"),
    panel.grid.major.y = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1))

  
  
  
  
  
  
  
  
  
  
  
  