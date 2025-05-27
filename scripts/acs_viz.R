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
library(scales) 
library(openrouteservice)
library(ggspatial)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)

# load data & calculate deciles for pop/density/income ####

# clear environment
rm(list = ls())
gc()

# load data
data <- st_read('D:/research/phxmob/data/acs/acs.shp') %>%
  mutate(
    tile_population = ntile(total, 10),
    tile_income = ntile(medinc, 10),
    tile_pdensity = ntile(pp_dnst, 10)) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# map blocks by decile ####

# income
ggplot(data) + geom_sf(aes(fill = tile_income)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(fill = "Income")

# total
ggplot(data) + geom_sf(aes(fill = tile_population)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(fill = "Population")

# density
ggplot(data) + geom_sf(aes(fill = tile_pdensity)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(fill = "Density")


# density plots ####

# income
ggplot(data, aes(x = medinc)) +
  geom_density(
    fill = "steelblue", 
    alpha = 0.7, 
    color = "darkblue", 
    linewidth = 1) +
  geom_vline(xintercept = 0, color = "black", linetype = "solid", linewidth = 0.5) +  
  geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.5) + 
  scale_x_continuous(
    labels = label_dollar(prefix = "$", big.mark = ","),
    expand = c(0, 0)) +  
  scale_y_continuous(
    labels = label_comma(),
    expand = c(0, 0)) + 
  labs(
    x = "Median Income (USD)",
    y = "Density") +
  theme_minimal() +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    panel.border = element_rect(color = "black", fill = NA, size = 1))

# population
ggplot(data, aes(x = total)) +
  geom_density(
    fill = "steelblue", 
    alpha = 0.7, 
    color = "darkblue", 
    linewidth = 1) +
  geom_vline(xintercept = 0, color = "black", linetype = "solid", linewidth = 0.5) +  
  geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.5) + 
  scale_x_continuous(expand = c(0, 0)) +  
  scale_y_continuous(
    labels = label_comma(),
    expand = c(0, 0)) + 
  labs(
    x = "Population Count",
    y = "Density") +
  theme_minimal() +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    panel.border = element_rect(color = "black", fill = NA, size = 1))

# density
ggplot(data, aes(x = pp_dnst)) +
  geom_density(
    fill = "steelblue", 
    alpha = 0.7, 
    color = "darkblue", 
    linewidth = 1) +
  geom_vline(xintercept = 0, color = "black", linetype = "solid", linewidth = 0.5) +  
  geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.5) + 
  scale_x_continuous(expand = c(0, 0)) +  
  scale_y_continuous(
    labels = label_comma(),
    expand = c(0, 0)) + 
  labs(
    x = "Population Density",
    y = "Density") +
  theme_minimal() +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    panel.border = element_rect(color = "black", fill = NA, size = 1))

# line plots with deciles ####

# population
pop_summary <- data %>%
  group_by(tile_population) %>%
  summarise(avg_population = mean(total, na.rm = TRUE)) %>%
  mutate(avg_population = round(avg_population))

pop_plot <- pop_summary %>%
  ggplot(aes(x = tile_population, y = avg_population)) +
  geom_line(color = "#69b3a2", size = 1.2) +
  geom_point(color = "#69b3a2", size = 3) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(
    breaks = pop_summary$avg_population,
    labels = comma) +
  labs(
    x = "Population Decile",
    y = "Average Population") +
  theme(
    panel.grid.major.x = element_line(color = "gray80"),
    panel.grid.major.y = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1))
pop_plot

# density
density_summary <- data %>%
  group_by(tile_pdensity) %>%
  summarise(avg_density = mean(pp_dnst, na.rm = TRUE)) %>%
  mutate(avg_density = round(avg_density))

density_plot <- density_summary %>%
  ggplot(aes(x = tile_pdensity, y = avg_density)) +
  geom_line(color = "#ffb347", size = 1.2) +
  geom_point(color = "#ffb347", size = 3) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(
    breaks = density_summary$avg_density,
    labels = comma) +
  labs(
    x = "Population Density Decile",
    y = "Average Density") +
  theme(
    panel.grid.major.x = element_line(color = "gray80"),
    panel.grid.major.y = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1))
density_plot

# income
income_summary <- data %>%
  group_by(tile_income) %>%
  summarise(avg_income = mean(medinc, na.rm = TRUE)) %>%
  mutate(avg_income = round(avg_income))

income_plot <- income_summary %>%
  ggplot(aes(x = tile_income, y = avg_income)) +
  geom_line(color = "#8470ff", size = 1.2) +
  geom_point(color = "#8470ff", size = 3) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(
    breaks = income_summary$avg_income,
    labels = comma) +
  labs(
    x = "Income Decile",
    y = "Average Median Household Income") +
  theme(
    panel.grid.major.x = element_line(color = "gray80"),
    panel.grid.major.y = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1))
income_plot
















