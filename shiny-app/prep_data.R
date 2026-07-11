# prep_data.R ---------------------------------------------------------------
# One-time build: bake a slim, self-contained dataset for the Shiny app.
# Reads the canonical analytic frame (D:/research/phxmob/data/access.gpkg),
# the amenity point locations (D:/research/phxmob/data/destinations.gpkg), and
# block-group boundaries (tigris, no API key), and writes phxmob_data.rds next
# to app.R. After this runs, the app needs no Census key and no D:\ access.
#
# Bundle contents (list):
#   bg   - block-group polygons + ACS sociodemographics (one row per GEOID)
#   acc  - single-amenity time-to-nearest (GEOID x mode x cat -> ttnearest)
#   cum  - cumulative opportunity per travel-time budget
#          (GEOID x mode x cut -> n_types reachable, n_dest total destinations)
#   dest - amenity point locations (cat, name, lng, lat) for the map overlay
#
# Source data are unpublished HUM Lab work; externalizing this derived subset
# was signed off by Chris Lim (2026-07-08).

suppressWarnings(suppressMessages({
  library(sf); library(dplyr); library(tidyr); library(tigris)
}))
options(tigris_use_cache = TRUE)

app_dir <- "C:/Users/andre/Desktop/box/projects/life/job-search/portfolio-shiny-phxmob"

cats6 <- c("grocery", "pharmacy", "park", "schools", "library", "transit")

# 1. access measures (drop point geometry) -----------------------------------
acc_raw <- st_read("D:/research/phxmob/data/access.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  filter(mode %in% c("walk", "bike")) |>
  mutate(cut = suppressWarnings(as.integer(as.character(cut))))

# 1a. single-amenity time-to-nearest (constant across cutoffs -> collapse) ----
acc <- acc_raw |>
  filter(cat %in% cats6) |>
  mutate(ttnearest = suppressWarnings(as.integer(as.character(ttnearest)))) |>
  distinct(GEOID, mode, cat, ttnearest)

# 1b. cumulative opportunity per (GEOID, mode, travel-time budget) ------------
# n_types = how many of the 6 amenity types have >=1 reachable destination
# n_dest  = total count of reachable destinations summed across the 6 types
cum <- acc_raw |>
  filter(cat %in% cats6) |>
  group_by(GEOID, mode, cut) |>
  summarise(n_types = sum(count > 0, na.rm = TRUE),
            n_dest  = sum(count, na.rm = TRUE), .groups = "drop")

# per-block-group sociodemographics (one row per GEOID)
demo <- acc_raw |>
  distinct(GEOID, age, inc, pct_wht, pct_blk, pct_asn, pct_hisp,
           pct_own, pct_rent, density, population)

# 2. amenity point locations (for the toggle overlay) ------------------------
dest_sf <- st_read("D:/research/phxmob/data/destinations.gpkg", quiet = TRUE) |>
  rename(cat = category) |>
  filter(cat %in% cats6) |>
  st_transform(4326)
coords <- st_coordinates(dest_sf)
dest <- tibble(cat  = dest_sf$cat,
               name = dest_sf$name,
               lng  = round(coords[, 1], 5),
               lat  = round(coords[, 2], 5))
cat(sprintf("destinations: %d points across %d types\n",
            nrow(dest), dplyr::n_distinct(dest$cat)))

# 3. block-group polygons (generalized cartographic boundaries; no API key) ---
geoids <- unique(acc$GEOID)
bg <- tryCatch(
  block_groups(state = "AZ", county = "Maricopa", year = 2023, cb = TRUE),
  error = function(e) block_groups(state = "AZ", county = "Maricopa", cb = TRUE)
)
bg <- bg |>
  filter(GEOID %in% geoids) |>
  select(GEOID) |>
  st_transform(4326) |>
  st_make_valid() |>
  st_simplify(dTolerance = 30, preserveTopology = TRUE) |>
  left_join(demo, by = "GEOID")

cat(sprintf("polygons: %d / %d GEOIDs matched\n", nrow(bg), length(geoids)))

# 4. save bundle -------------------------------------------------------------
saveRDS(list(bg = bg, acc = acc, cum = cum, dest = dest),
        file.path(app_dir, "phxmob_data.rds"))
sz <- file.info(file.path(app_dir, "phxmob_data.rds"))$size
cat(sprintf("wrote phxmob_data.rds (%.2f MB)\n", sz / 1024^2))
