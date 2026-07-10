# Phoenix Multi-Modal Amenity Access — interactive Shiny app

An interactive R Shiny dashboard built from this **phx_mob** project. It maps
**time-denominated, multi-modal access** to six essential amenities (grocery,
pharmacy, park, school, library, transit stop) across ~835 Maricopa County
census block groups, and lets the viewer probe **sociodemographic disparities**
in that access.

**Live app:** https://connect.posit.cloud/andrewcwees/content/019f4dab-5684-7130-b404-1e7165bdbd19
**Code:** this folder; underlying analysis lives in the repository root.

## What it does
- **Interactive choropleth** (`leaflet`): travel time from each block group to
  the nearest amenity of the selected type, by **walking** or **biking**
  network routing. Hover for per-block-group detail; grey = not reachable
  within 60 min.
- **Equity chart** (`ggplot2`): the distribution of a chosen sociodemographic
  variable (median income, % White, % Hispanic, % renter, population density)
  across travel-time bins — the disparity story.
- **Value boxes**: share of block groups reachable within 15 min, median time
  to nearest, and n mapped.

## Files
- `app.R` — the Shiny app. Self-contained; reads only `phxmob_data.rds`.
- `phxmob_data.rds` — baked bundle (block-group polygons + access measures +
  ACS sociodemographics). ~0.1 MB; **no Census API key or external data needed
  at runtime.**
- `manifest.json` — dependency + R-version lockfile for Posit Connect Cloud
  (generated with `rsconnect::writeManifest()`).
- `prep_data.R` — one-time build script that regenerates the bundle from the
  canonical access frame + `tigris` boundaries. Only needed to refresh the data.

## Run locally
```r
# from this folder
shiny::runApp()
```
Requires: `shiny`, `bslib`, `leaflet`, `dplyr`, `sf`, `ggplot2`, `scales`.

## Deploy a live link (Posit Connect Cloud)
This app deploys to [Posit Connect Cloud](https://connect.posit.cloud) directly
from GitHub — no local publish step:
1. Sign in to Connect Cloud with your GitHub account.
2. **Publish → Shiny**, select this repository (`andrewcwees/phx_mob`), and set
   the primary file to **`shiny-app/app.R`**.
3. Connect Cloud reads `shiny-app/manifest.json` for the R version and packages,
   builds, and publishes. Paste the resulting URL into the **Live app** line
   above.

To refresh dependencies later, re-run `rsconnect::writeManifest()` in this
folder and push the updated `manifest.json`.

## Provenance / data note
Derived from **unpublished HUM Lab data**; external sharing of this derived,
aggregated subset was signed off by **Chris Lim (2026-07-08)**. Figures are a
work sample, not a published result. Underlying method: `r5r` network routing;
2020 Census geography, ACS 2019–2023 sociodemographics.
