# phx_mob
With R, compile and analyze US census & phone GPS-based mobility data to evaluate how proximity relates to utilization for a set of crucial public amenities across sociodemographics in the Phoenix metropolitan area.

## SETUP/RUN

### install/load packages
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

















