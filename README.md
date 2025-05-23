# phx_mob
In R, combine US census & smartphone GPS data to evaluate how access and usage of local amenities in Maricopa County, Arizona varies across income and racial/ethnic demographics.

(mobility data obtained from [Advan](https://app.deweydata.io/products/2dfcb598-6e30-49f1-bdba-1deae113a951/package/))








### project: redenv
Using R and public data, evaluate how historically 'redlined' neighborhoods across >200 cities in the U.S. are associated with higher average levels of exposure to noise and air pollution across transportation sectors (railroads, roadways, and aviation) in the present day.

### SETUP

## required packages
library(sf)
library(httr)
library(skimr)
library(ggpubr)
library(data.table)
library(jsonlite)
library(mapview)
library(terra)
library(exactextractr)
library(stars)
library(tidyverse)
library(ggspatial)
library(tidycensus)
library(tigris)

## scripts
1. compile
2. map_poi
3. eval_iso
4. census_viz
5. get_advan


## USAGE
- Install/load all required packages
- Run scripts in order listed above
- Not all data used in this project is publicly available. Dewey data subscription required to access Advan smartphone GPS data

















