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
options(tigris_use_cache = TRUE)

# download advan files 2023-2024 ####
rm(list = ls())
gc()

# define paths
key <- "cS69Lg4d.OoXMpWPlV9ZlQmtt597fJQHZyhfw3DwtzBV8qG3vzT0QGgYxmZIckakY"
download_dir <- "D:/research/phxmob/data/advan"
meta_url <- "https://app.deweydata.io/external-api/v3/products/2dfcb598-6e30-49f1-bdba-1deae113a951/files/metadata"
base_url <- "https://app.deweydata.io/external-api/v3/products/2dfcb598-6e30-49f1-bdba-1deae113a951/files"

# get metadata
response <- GET(meta_url, add_headers(`x-api-key` = key))
metadata <- content(response, as = "parsed", type = "application/json") 

# get file_list
date_start <- "2023-01-01"
date_end <- "2025-01-01"

response <- GET(base_url,add_headers(`x-api-key` = key),
  query = list(
    partition_key_after = date_start,
    partition_key_before = date_end))
file_list <- content(response, as = "parsed", type = "application/json")

# download
download_links <- file_list$download_links

walk(download_links, function(file) {
  file_url <- file$link
  file_name <- file$file_name
  file_path <- file.path(download_dir, file_name)
  
  if (!is.null(file_url)) {
    GET(file_url, write_disk(file_path, overwrite = TRUE))
    message("Downloaded: ", file_name)
  } else {
    warning("Missing download URL for: ", file_name)
  }
})

# apply spatial filter ####
rm(list = ls())
gc()

setwd('D:/research/phxmob/data/advan/')
output_dir <- "D:/research/phxmob/data/advan_maricopa"

id_list <- st_read('D:/research/phxmob/data/iso/iso.shp') %>% 
  st_drop_geometry() %>% dplyr::select(GEOID)
id_list$GEOID <- as.numeric(id_list$GEOID)

files <- list.files(pattern = "\\.csv\\.gz$", full.names = T)

process <- function(file_path) {
  
  file_name <- basename(file_path)
  message("Processing: ", file_name)
  
  data <- fread(file_path)
  data$GEOID <- data$AREA
  data$GEOID <- as.numeric(data$GEOID)
  data$AREA <- NULL

  save <- dplyr::filter(data, GEOID %in% id_list$GEOID)
  
  output_file <- file.path(output_dir, basename(file_path))
  fwrite(save, output_file)
  
  message("Saved: ", output_file, " (", nrow(save), " rows)")
  
}

walk(files, process)
message("All files processed.")






















