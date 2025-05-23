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

#####

rm(list = ls())
gc()

data <- st_read('D:/research/phxmob/data/iso/iso.shp')

# map + density plot income ####
mapview(data, zcol = 'medinc')

ggplot(data, aes(x = medinc)) +
  geom_density(
    fill = "steelblue", 
    alpha = 0.7, 
    color = "darkblue", 
    linewidth = 1) +
  geom_vline(xintercept = 0, color = "black", linetype = "solid", linewidth = 0.5) +  
  geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.5) + 
  scale_x_continuous(labels = label_dollar(prefix = "$", big.mark = ",")) +  
  scale_y_continuous(labels = label_comma()) + 
  labs(
    title = "Distribution of Median Household Income in Maricopa County, AZ",
    subtitle = "Estimates from 2020 5yr ACS estimates",
    x = "Median Income (USD)",
    y = "Density") +
  theme_minimal(base_size = 14) +  
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, color = "gray40"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),  
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"))

# density plot population totals ####
ggplot(data, aes(x = total)) +
  geom_density(
    fill = "steelblue", 
    alpha = 0.7, 
    color = "darkblue", 
    linewidth = 1) +
  geom_vline(xintercept = 0, color = "black", linetype = "solid", linewidth = 0.5) +  # y-axis line
  geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.5) +  # x-axis line
  scale_y_continuous(labels = comma_format()) +  
  labs(
    title = "Distribution of Total Population by CBG in Maricopa County, AZ",
    subtitle = "Counts from 2020 Decennial Census",
    x = "Total Population",
    y = "Density") +
  theme_minimal(base_size = 14) +  
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, color = "gray40"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"))

# histogram population by race/ethnicity ####
race_vars <- c(
  'white',
  'black',
  'native',
  'asian',
  'pacific',
  'other',
  'tw_r_mr',
  'hspnc__')

pop_counts <- data %>%
  st_drop_geometry() %>%
  pivot_longer(
    cols = all_of(race_vars),
    names_to = "race",
    values_to = "count") %>%
  dplyr::select(-GEOID,-NAME, -total, -medinc) %>%
  group_by(race) %>%
  summarise(count = sum(count, na.rm = TRUE)) %>% ungroup()

pop_counts$race <- factor(pop_counts$race, levels = pop_counts$race[order(-pop_counts$count)])

axis_labels <- c(
  "white" = "White",
  "black" = "Black",
  "asian" = "Asian",
  "native" = "Native",
  "pacific" = "NHPI",
  "other" = "Other",
  "tw_r_mr" = "Two or More Races",
  "hspnc__" = "Hispanic Any Race")

ggplot(pop_counts, aes(x = race, y = count, fill = race)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(
    title = "CBG Population Count by Race/Ethnicity in Maricopa County, AZ",
    subtitle = "Counts from 2020 Decennial Census",
    x = "Race",
    y = "Population Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(labels = axis_labels) +  
  scale_y_continuous(labels = scales::comma) +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1) )


