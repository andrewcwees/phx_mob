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
library(viridis)
library(tidyr)
library(osrm)
library(terra)
library(tidyverse)
library(openrouteservice)
library(ggspatial)
library(tidycensus)
library(tigris)
options(tigris_use_cache = T)

# density plot showing total reachable across/within cat by mode/cut ####
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')
output_folder <- 'D:/research/phxmob/output/plots'

origins <- st_read('origins.gpkg') %>% 
  st_drop_geometry() %>% 
  mutate(id = as.character(row_number())) 

access <- st_read("access.gpkg") %>% 
  left_join(origins, by = 'id') %>% 
  filter(mode != 'any')

for (this_cat in unique(access$cat)) {
  
  df_cat <- access %>%
    filter(cat == this_cat)
  
  p <- ggplot(df_cat, aes(x = count, fill = cut)) +
    geom_density(alpha = 0.4) +
    facet_wrap(~ mode, scales = "free") +
    labs(
      x = paste(this_cat, "- Destinations Reachable"),
      y = "Density",
      fill = "Cut (min)") +
    theme_minimal()
  
  file_name <- paste0(gsub("[^a-zA-Z0-9]", "_", this_cat), "_density.png")
  file_path <- file.path(output_folder, file_name)
  
  ggsave(filename = file_path, plot = p, width = 8, height = 6)

  print(p)
}









# bar plot % population with complete access any mode by cutoff ####

for (cutoff in cutoffs) {
  access <- access %>%
    rowwise() %>%
    mutate(!!paste0("access_full_any_", cutoff) := {
      all(sapply(categories, function(category) {
        # Get all columns for this category across all modes
        cols <- paste0("n_", modes, "_", cutoff, "_", category)
        any(c_across(all_of(cols)) > 0)
      })) * 1
    }) %>%
    ungroup()
}

mean(access$access_full_any_15 > 0) * 100
mean(access$access_full_any_20 > 0) * 100
mean(access$access_full_any_30 > 0) * 100
mean(access$access_full_any_40 > 0) * 100
mean(access$access_full_any_50 > 0) * 100
mean(access$access_full_any_60 > 0) * 100

access_summary <- data.frame(
  cutoff = factor(cutoffs, levels = cutoffs),
  percent_access = sapply(cutoffs, function(cutoff) {
    access_flag <- access[[paste0("access_full_any_", cutoff)]]
    weighted_sum <- sum(access$population * access_flag, na.rm = T)
    total_pop <- sum(access$population, na.rm = T)
    (weighted_sum / total_pop) * 100}))

ggplot(access_summary, aes(x = percent_access, y = cutoff, fill = percent_access)) +
  geom_bar(stat = "identity", position = 'dodge') +
  scale_fill_gradient(low = "darkred", high = "darkgreen", name = "% Access") +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)) +
  labs(
    x = "% of Population with Complete Access Through Any Mode",
    y = "Time threshold (minutes)") +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_line(color = "black"))

# bar plot % population with complete access by mode by cutoff ####

for (cutoff in cutoffs) {
  for (mode in modes) {
    access <- access %>%
      rowwise() %>%
      mutate(!!paste0("access_full_", mode, "_", cutoff) := {
        all(sapply(categories, function(category) {
          get(paste0("n_", mode, "_", cutoff, "_", category)) > 0
        })) * 1
      }) %>%
      ungroup()
  }
}

access_summary_mode <- expand.grid(cutoff = cutoffs, mode = modes) %>%
  rowwise() %>%
  mutate(
    access_flag = paste0("access_full_", mode, "_", cutoff),
    percent_access = {
      flag <- access[[access_flag]]
      pop_weighted <- sum(access$population * flag, na.rm = TRUE)
      total_pop <- sum(access$population, na.rm = TRUE)
      (pop_weighted / total_pop) * 100
    }) %>%
  ungroup() %>%
  mutate(cutoff = factor(cutoff, levels = cutoffs))

ggplot(access_summary_mode, aes(x = percent_access, y = cutoff, fill = mode)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_brewer(palette = "Set2", name = "Mode") +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)) +
  labs(
    y = "Time Threshold (minutes)",
    x = "% of Population with Complete Access by Mode") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_line(color = "black"))


















