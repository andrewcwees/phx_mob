# project: phx_mob
Identify opportunities for maximizing improvements in amenity accessibility in Phoenix, AZ through multi-modal, time-denominated route modeling with r5r package and OSM street mapping

## SETUP

### required packages
install.packages("data.table")
install.packages("deweydatar")
install.packages("dplyr")
install.packages("FSA")
install.packages("ggpubr")
install.packages("ggspatial")
install.packages("httr")
install.packages("jsonlite")
install.packages("mapview")
install.packages("openrouteservice")
install.packages("osrm")
install.packages("r5r")
install.packages("rJava")
install.packages("readr")
install.packages("scales")
install.packages("sf")
install.packages("skimr")
install.packages("terra")
install.packages("tibble")
install.packages("tidycensus")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("tigris")

### scripts
1. compile
   a. get_OD_data
   b. calc_access
2. analysis
   a. maps
   b. plots
   c. tables

### data sources
- destination locations obtained from [Advan Monthly Patterns](https://app.deweydata.io/products/2dfcb598-6e30-49f1-bdba-1deae113a951/package/)
- demographic data from 5yr (2019-2023) American Community Survey estimates

## USAGE

Start by installing and loading all required packages. Modify directory paths and run scripts in the order listed.











