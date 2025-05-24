# phx_mob
In R, combine US census & smartphone GPS data to evaluate how access and usage of local amenities in Maricopa County, Arizona varies across income and racial/ethnic demographics.

## SETUP

### required packages
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

### scripts
1. get_acs
2. census_viz
3. get_npat
4. refine_npat

### data
- mobility data obtained from [Advan](https://app.deweydata.io/products/2dfcb598-6e30-49f1-bdba-1deae113a951/package/)
- census data through API call

## USAGE
- Install/load all required packages
- Run scripts in order listed above
- Not all data used in this project is publicly available (Dewey data subscription is required to access Advan 'Neighborhood Patterns' dataset)

















