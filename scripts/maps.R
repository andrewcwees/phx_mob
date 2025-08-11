# libs ####
library(sf)
library(httr)
library(dplyr)
library(skimr)
library(ggpubr)
library(data.table)
library(terra)
library(readr)
library(patchwork)
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

# mapview of study area ####
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')

origins <- st_read('origins.gpkg') %>%
  st_drop_geometry()

polygons <- get_acs(
  geography = "block group",
  variables = 'B02001_001',
  state = "AZ",
  county = "Maricopa",
  year = 2023,
  survey = "acs5",
  geometry = T) %>% 
  filter(GEOID %in% origins$GEOID) %>%
  select(GEOID)

plotdat <- left_join(polygons, origins, by = 'GEOID') 

plotdat_unify <- st_as_sf(st_union(plotdat))

mean_center <- st_coordinates(plotdat) %>%
  as.data.frame() %>%
  summarise(
    lon = mean(X),
    lat = mean(Y)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

mapview(
  plotdat,
  col.regions = 'blue',
  color = 'white',
  lwd = 0.5,
  alpha = 0.5) + 
  mapview(
    plotdat_unify, 
    col.regions = NA,
    lwd = 2, 
    color = 'red') +
  mapview(
    mean_center,
    col.regions = 'black')

# plot destination locations by cat ####
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')

# create map boundary
origins <- st_read('origins.gpkg') %>%
  st_drop_geometry()

polygons <- get_acs(
  geography = "block group",
  variables = 'B02001_001',
  state = "AZ",
  county = "Maricopa",
  year = 2023,
  survey = "acs5",
  geometry = T) %>% 
  filter(GEOID %in% origins$GEOID)
polygons <- left_join(polygons, origins, by = 'GEOID')
bound <- st_as_sf(st_union(polygons))
mapview(bound)

dest <- st_read('destinations.gpkg') %>%
  rename('cat' = 'category')

# plot
cat_colors <- c(
  "grocery" = "#1b9e77",
  "park"    = "#d95f02",
  "pharmacy" = "#7570b3",
  "schools" = "#e7298a",
  "library" = "darkred",
  "transit" = "#e6ab02")

p <- ggplot() +
  geom_sf(data = bound, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = dest, aes(color = cat), size = 1, alpha = 0.7) +
  facet_wrap(~ cat) +
  scale_color_manual(
    values = cat_colors,
    name = "Category"  ) +
  theme_void() +
  theme(
    legend.key.size = unit(3, "mm"),  
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    strip.text = element_blank(),     
    legend.position = "right") +
  guides(color = guide_legend(
    override.aes = list(size = 4)))
print(p)

ggsave(
  filename = "destination_locations.png",
  plot = p,
  path = "D:/research/phxmob/output/maps",
  width = 10,
  height = 8,
  dpi = 300)


# plot total reachable destinations at 15min by mode ####
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')

access_df <- st_read('access.gpkg') %>%
  st_drop_geometry()

polygons <- get_acs(
  geography = "block group",
  variables = 'B02001_001',
  state = "AZ",
  county = "Maricopa",
  year = 2023,
  survey = "acs5",
  geometry = T) %>% 
  filter(GEOID %in% access_df$GEOID) %>%
  select(GEOID)

plotdat <- left_join(polygons, access_df, by = 'GEOID')

modes <- c("walk", "bike")

for (m in modes) {
  p <- plotdat %>%
    filter(cat == "total", mode == m, cut == 15) %>%
    ggplot() +
    geom_sf(aes(fill = count), color = "white", size = 0.1) +
    scale_fill_viridis_c(option = "plasma") +
    theme_void() +
    ggtitle(str_to_title(m))
  
  ggsave(
    filename = paste0("plot_", m, "_cut15.png"),
    plot = p,
    path = "D:/research/phxmob/output/maps",
    width = 6,
    height = 6,
    dpi = 300)
}

# plot yes/no access and ttnearest by cat/cut/mode ####
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')

access_df <- st_read('access.gpkg') %>%
  st_drop_geometry()

polygons <- get_acs(
  geography = "block group",
  variables = 'B02001_001',
  state = "AZ",
  county = "Maricopa",
  year = 2023,
  survey = "acs5",
  geometry = TRUE) %>% 
  filter(GEOID %in% access_df$GEOID) %>%
  select(GEOID)

plotdat <- left_join(polygons, access_df, by = 'GEOID') %>%
  filter(mode != 'any') %>%
  mutate(has_dest = as.integer(count > 0))

cats <- plotdat %>%
  st_drop_geometry() %>%
  distinct(cat) %>%
  pull(cat)

# yes/no access
for (cat_val in cats) {
  
  p <- plotdat %>%
    filter(cat == cat_val) %>%
    ggplot() +
    geom_sf(aes(fill = factor(has_dest)), color = NA, size = 0.1) +
    scale_fill_manual(
      values = c("0" = "red", "1" = "green"),
      name = "N > 0",
      labels = c("No", "Yes")) +
    facet_grid(cut ~ mode, switch = "y") +
    theme_void() +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside") +
    ggtitle(paste("Destination Category:", str_to_title(cat_val)))
  
  ggsave(
    filename = paste0("map_", cat_val, "_bycut.png"),
    plot = p,
    path = "D:/research/phxmob/output/maps",
    width = 12,
    height = 6,
    dpi = 300)
}

# ttnearest
for (cat_val in cats) {
  
  p <- plotdat %>%
    filter(cat == cat_val) %>%
    ggplot() +
    geom_sf(aes(fill = ttnearest), color = NA, size = 0.1) +
    scale_fill_viridis_d(
      na.value = "grey90",
      option = "plasma",
      direction = -1,
      name = "Time to Nearest (min)") +
    facet_grid(mode ~ ., switch = "y") +
    theme_void() +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside") +
    ggtitle(paste("Destination Category:", str_to_title(cat_val)))
  
  ggsave(
    filename = paste0("map_ttnearest_", cat_val, "_bycutmode.png"),
    plot = p,
    path = "D:/research/phxmob/output/maps",
    width = 12,
    height = 6,
    dpi = 300)
}


# plot n categories reachable by cutoff/mode ####
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')

access_df <- st_read('access.gpkg') %>%
  st_drop_geometry()

polygons <- get_acs(
  geography = "block group",
  variables = 'B02001_001',
  state = "AZ",
  county = "Maricopa",
  year = 2023,
  survey = "acs5",
  geometry = T) %>% 
  filter(GEOID %in% access_df$GEOID) %>%
  select(GEOID)

access_long <- access_df %>%
  pivot_longer(
    cols = starts_with("n_"),
    names_to = c("mode", "cutoff", "category"),
    names_pattern = "n_(.*)_(\\d+)_(.*)",
    values_to = "count") %>%
  mutate(
    cutoff = as.numeric(cutoff),
    reachable = count > 0)

modes <- c("walk", "bike", "bus")

plot_mode_facets <- function(
    mode_name, 
    access_long, 
    polygons, 
    save_plot = F, 
    out_dir = "D:/research/phxmob/output/maps") {
  
  plotdat <- access_long %>%
    filter(mode == mode_name) %>%
    group_by(GEOID, cutoff) %>%
    summarise(
      n_categories = sum(reachable, na.rm = T),
      .groups = "drop") %>%
    left_join(polygons, by = "GEOID") %>%
    st_as_sf()
  
  p <- ggplot(plotdat) +
    geom_sf(aes(fill = n_categories), color = "white", size = 0.1) +
    facet_wrap(~ cutoff, nrow = 2) +
    scale_fill_viridis_c(
      option = "plasma",
      limits = c(0, 7),
      breaks = 0:7,
      name = paste0("Categories Reachable\n(", mode_name, ")"),
      guide = guide_colorbar(barwidth = 10, barheight = 0.8)) +
    theme_minimal() +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      panel.grid = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8))
  
  print(p)
  
  if (save_plot) {
    if (!dir.exists(out_dir)) dir.create(out_dir)
    ggsave(
      filename = file.path(
        out_dir, paste0("reachable_categories_", mode_name, ".png")),
      plot = p, width = 10, height = 6)}
}

for (m in modes) {
  plot_mode_facets(
    mode_name = m, 
    access_long = access_long, 
    polygons = polygons, 
    save_plot = T)
}




