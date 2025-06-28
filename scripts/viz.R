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

# explore demographics ####
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')

data <- fread('origins_f.csv')



# density plots

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
    x = "Income",
    y = "Density") +
  theme_minimal() +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    panel.border = element_rect(color = "black", fill = NA, size = 1))

ggplot(data, aes(x = pop_density)) +
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



# line plots

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




# travel time by mode/deciles to nearest destination ####
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')

nearest <- st_read('iso.gpkg') %>%
  mutate(
    tile_pop = ntile(total, 10),
    tile_inc = ntile(medinc, 10),
    tile_popd = ntile(pop_density, 10),
    across(where(is.numeric), ~ round(.x, 2))) %>%
  st_drop_geometry()

sum_travel_times <- function(data, decile_var, decile_label) {
  data %>%
    group_by(decile = .data[[decile_var]]) %>%
    summarise(
      across(starts_with("minutes_"), mean, na.rm = TRUE),
      .groups = "drop") %>%
    pivot_longer(
      cols = starts_with("minutes_"),
      names_to = "mode",
      values_to = "travel_time") %>%
    mutate(decile_type = decile_label)
}

plot_data <- bind_rows(
  sum_travel_times(nearest, "tile_pop", "Population"),
  sum_travel_times(nearest, "tile_inc", "Income"),
  sum_travel_times(nearest, "tile_popd", "Density")) %>%
  mutate(mode = recode(mode,
                       minutes_walk = "Walk",
                       minutes_bike = "Bike"))

ggplot(plot_data, aes(x = decile, y = travel_time, color = mode)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ decile_type, scales = "fixed") +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Estimated Travel Time to Nearest Destination by Decile Type",
    x = "Decile",
    y = "Travel Time (Minutes)",
    color = "Mode") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80"),
    panel.border = element_rect(color = "black", fill = NA),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom")

# proximity ####
# = n destinations reachable within 15min travel time
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')

prox <- fread('proximity.csv')

# summary function
summarize_by_decile <- function(data, decile_var, decile_label) {
  data %>%
    group_by(decile = .data[[decile_var]]) %>%
    summarise(
      across(starts_with("n_stores_"), mean, na.rm = TRUE),
      .groups = "drop") %>%
    pivot_longer(
      cols = starts_with("n_stores_"),
      names_to = "mode",
      values_to = "n_destinations") %>%
    mutate(decile_type = decile_label)
}

# combine summary data
plot_data <- bind_rows(
  summarize_by_decile(prox, "tile_pop", "Population"),
  summarize_by_decile(prox, "tile_inc", "Income"),
  summarize_by_decile(prox, "tile_popd", "Density"),
  summarize_by_decile(prox, "tile_age", "Age")) %>%
  mutate(mode = recode(mode, 
      n_stores_walk = "Walk", 
      n_stores_bike = "Bike",
      n_stores_car = "Car")) #%>% dplyr::filter(mode != 'Car')

# save proximity plot data
fwrite(plot_data, 'plot_data_proximity.csv')

# plot proximity by deciles
ggplot(plot_data, aes(x = decile, y = n_destinations, color = mode)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ decile_type, scales = "fixed") +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = 'Proximity',
    subtitle = "# Destinations Reachable Within 15 min by Mode and Decile Type",
    x = "Decile",
    y = "Reachable Destinations",
    color = "Mode") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80"),
    panel.border = element_rect(color = "black", fill = NA),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom")

# utility ####
# = % trips made within 15min travel time by mode/deciles
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')

test <- fread('util_and_prox.csv')

# summary function
summarize_by_decile <- function(data, decile_var, decile_label) {
  data %>%
    group_by(decile = .data[[decile_var]]) %>%
    summarise(
      across(starts_with("perc_local_use_"), mean, na.rm = TRUE),
      .groups = "drop") %>%
    pivot_longer(
      cols = starts_with("perc_local_use_"),
      names_to = "mode",
      values_to = "percentage") %>%
    mutate(decile_type = decile_label)
}

# combine summary data
plot_data <- bind_rows(
  summarize_by_decile(test, "tile_popd", "Density"),
  summarize_by_decile(test, "tile_pop", "Population"),
  summarize_by_decile(test, "tile_inc", "Income"),
  summarize_by_decile(test, "tile_age", "Age")) %>%
  mutate(
    mode = recode(mode, 
                  perc_local_use_walk = "Walk", 
                  perc_local_use_bike = "Bike",
                  perc_local_use_car = "Car"))

# save utility plot data
fwrite(plot_data, 'plot_data_utility.csv')

# plot utility by deciles
ggplot(plot_data, aes(x = decile, y = percentage, color = mode)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ decile_type, scales = "fixed") +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = 'Utility',
    subtitle = "Avg % of Trips Made Within 15min Travel Time by Decile/Mode",
    x = "Decile",
    y = "Average % of Trips Local",
    color = "") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80"),
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "bottom")

# utility vs proximity ####
# = % local trips against destinations reachable
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')

util <- fread('plot_data_utility.csv')
prox <- fread('plot_data_proximity.csv')

test <- left_join(util, prox) %>%
  rename(
    proximity = n_destinations,
    utility = percentage) %>% 
  na.omit()

# loess regression 
ggplot(test, aes(x = proximity, y = utility)) +
  geom_point(aes(color = as.factor(decile), shape = mode), alpha = 0.8) +  
  geom_smooth(method = "loess", se = T, color = "black") +  
  facet_wrap(~ decile_type, scales = "free") +  
  labs(
    title = "Proximity vs Utility of Grocery Stores by Deciles",
    subtitle = 'Model: Loess Regression',
    x = "Proximity (Number of Destinations Reachable Within 15min)",
    y = "Utility (Percentage of Reachable Destinations Visited)",
    color = "Decile",
    shape = 'Mode') +
  theme_minimal()

# linear OLS
ggplot(test, aes(x = proximity, y = utility)) +
  geom_point(aes(color = as.factor(decile), shape = mode), alpha = 0.8) +  
  geom_smooth(method = "lm", se = T, color = "black") +  
  facet_wrap(~ decile_type, scales = "free") +  
  labs(
    title = "Proximity vs Utility of Grocery Stores by Deciles",
    subtitle = 'Model: Linear OLS Regression',
    x = "Proximity (Number of Destinations Reachable Within 15min)",
    y = "Utility (Percentage of Reachable Destinations Visited)",
    color = "Decile",
    shape = 'Mode') +
  theme_minimal()












