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
library(scales)
library(terra)
library(tidyverse)
library(ggspatial)
library(tidycensus)
library(flextable)
library(officer)
library(tigris)
options(tigris_use_cache = T)

# summary table for full observation set ####
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')

origins <- st_read('origins.gpkg') %>%
  st_drop_geometry() %>%
  select(-GEOID)

sumtab <- origins %>%
  summarise(across(everything(), list(
    Mean = ~ mean(.x),
    Median = ~ median(.x),
    SD = ~ sd(.x),
    Min = ~ min(.x),
    Max = ~ max(.x)), 
    .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(),
               names_to = c("Variable", ".value"),
               names_pattern = "^(.*)_(Mean|Median|SD|Min|Max)$") %>%
  mutate(
    Mean = case_when(
      str_detect(Variable, "^pct_") ~ percent(Mean, accuracy = 0.1),
      T ~ comma(round(Mean, 0))),
    Median = case_when(
      str_detect(Variable, "^pct_") ~ percent(Median, accuracy = 0.1),
      T ~ comma(round(Median, 0))),
    SD = case_when(
      str_detect(Variable, "^pct_") ~ percent(SD, accuracy = 0.1),
      T ~ comma(round(SD, 0))),
    Min = case_when(
      str_detect(Variable, "^pct_") ~ percent(Min, accuracy = 0.1),
      T ~ comma(round(Min, 0))),
    Max = case_when(
      str_detect(Variable, "^pct_") ~ percent(Max, accuracy = 0.1),
      T ~ comma(round(Max, 0))),
    Variable = case_when(
      Variable == "age" ~ "Median Age",
      Variable == "inc" ~ "Median Household Income",
      Variable == "pct_wht" ~ "% White NH",
      Variable == "pct_blk" ~ "% Black NH",
      Variable == "pct_asn" ~ "% Asian NH",
      Variable == "pct_hisp" ~ "% Hispanic Any Race",
      Variable == "pct_own" ~ "% Owner-Occupied",
      Variable == "pct_rent" ~ "% Renter-Occupied",
      Variable == "density" ~ "Population Density",
      T ~ Variable))

ft <- flextable(sumtab) %>%
  set_header_labels(
    Variable = "Variable",
    Mean = "Mean",
    Median = "Median",
    SD = "Std. Dev.",
    Min = "Minimum",
    Max = "Maximum") %>%
  autofit() %>%
  theme_booktabs()

doc <- read_docx() %>%
  body_add_par("Summary Statistics: Sociodemographic Variables", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "D:/research/phxmob/output/tables/sumtab_fullset.docx")

# % of population with access by cat/mode/cut + full access ####
rm(list = ls())
gc()
setwd('D:/research/phxmob/data')

access <- st_read('access.gpkg') %>% 
  st_drop_geometry() %>%
  filter(
    mode != 'any',
    cat != 'total')

# full access per id/GEOID/mode/cut
access_full <- access %>%
  group_by(id, GEOID, mode, cut) %>%
  summarise(
    full_access = as.integer(all(count > 0)),
    population = first(population),
    .groups = "drop")

# pop % with any access by mode/cut/cat
pct_any_access <- access %>%
  group_by(mode, cut, cat) %>%
  summarise(
    pct_with_access = round(
      sum(
        population[count > 0], na.rm = T) / 
        sum(population, na.rm = T) * 100, 2),
    .groups = "drop") 

pct_any_access_wide <- pct_any_access %>%
  pivot_wider(
    names_from = cat,
    values_from = pct_with_access)

complete <- access_full %>%
  group_by(mode, cut) %>%
  summarise(
    complete = round(
      sum(population[full_access == 1], na.rm = T) / sum(population, na.rm = T) * 100, 2),
    .groups = "drop")

full_table <- pct_any_access_wide %>%
  left_join(complete, by = c("mode", "cut")) %>%
  arrange(mode, cut)

select_cols <- c(
  "mode",
  "cut", 
  "grocery", 
  "library", 
  "park", 
  "pharmacy", 
  "schools", 
  "transit", 
  "complete")
full_table <- full_table %>%
  select(any_of(select_cols))

combined_split <- full_table %>%
  group_split(mode)

for(tbl in combined_split) {
  mode_name <- unique(tbl$mode)
  tbl_no_mode <- select(tbl, -mode)
  
  ft <- flextable(tbl_no_mode) %>%
    add_header_row(
      values = paste0("% of Population Covered - ", mode_name),
      colwidths = ncol(tbl_no_mode)) %>%
    autofit() %>%
    theme_box()
  
  save_as_image(
    x = ft,
    path = paste0("D:/research/phxmob/output/tables/access_pctpop_by_mode_", mode_name, ".png")
  )
}







