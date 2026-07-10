# prep_data.R ---------------------------------------------------------------
# One-time build: bake a slim, self-contained dataset for the Shiny app.
# Reads the canonical analytic frame (D:/research/phxmob/data/access.gpkg) and
# block-group boundaries (tigris, no API key), and writes phxmob_data.rds next
# to app.R. After this runs, the app needs no Census key and no D:\ access.
#
# Source data are unpublished HUM Lab work; externalizing this derived subset
# was signed off by Chris Lim (2026-07-08).

suppressWarnings(suppressMessages({
  library(sf); library(dplyr); library(tidyr); library(tigris)
}))
options(tigris_use_cache = TRUE)

app_dir <- "C:/Users/andre/Desktop/box/projects/life/job-search/portfolio-shiny-phxmob"

# 1. access measures (drop point geometry; collapse to time-to-nearest) --------
acc_raw <- st_read("D:/research/phxmob/data/access.gpkg", quiet = TRUE) |>
  st_drop_geometry()

# ttnearest is constant across cutoffs within GEOID x mode x cat -> collapse.
acc <- acc_raw |>
  mutate(ttnearest = suppressWarnings(as.integer(as.character(ttnearest)))) |>
  distinct(GEOID, mode, cat, ttnearest) |>
  filter(mode %in% c("walk", "bike", "any"))

# per-block-group sociodemographics (one row per GEOID)
demo <- acc_raw |>
  distinct(GEOID, age, inc, pct_wht, pct_blk, pct_asn, pct_hisp,
           pct_own, pct_rent, density, population)

# 2. block-group polygons (generalized cartographic boundaries; no API key) ----
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

# 3. save bundle ---------------------------------------------------------------
saveRDS(list(bg = bg, acc = acc), file.path(app_dir, "phxmob_data.rds"))
sz <- file.info(file.path(app_dir, "phxmob_data.rds"))$size
cat(sprintf("wrote phxmob_data.rds (%.2f MB)\n", sz / 1024^2))
