# phx_mob
With R, compile and analyze US census & phone GPS-based mobility data to evaluate how proximity relates to utilization for a set of crucial public amenities across sociodemographics in the Phoenix metropolitan area.

## SETUP/RUN

### install/load packages
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

### run scripts in order (after adjusting paths)
1. get_data
2. filters
3. r5r_calc
4. maps
5. viz

### data
- mobility data obtained from [Advan](https://app.deweydata.io/products/2dfcb598-6e30-49f1-bdba-1deae113a951/package/) (Monthly Patterns set)
- demographic data from recent 5-year American Community Survey estimates

## ANALYSIS/RESULTS
- maps, plots, graphs, tables etc. stored [here](https://github.com/andrewcwees/phx_mob/output) 
- learn more about the concept, methodology, and results of this project [here](https://github.com/andrewcwees/phx_mob/paper_draft.pdf) 

















