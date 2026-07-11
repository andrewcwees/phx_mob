# Phoenix Multi-Modal Amenity Access — Shiny work sample

An interactive R Shiny dashboard built from the **phxmob** research project
(Weesner & Lim, UA Zuckerman College of Public Health). It maps
**time-denominated, multi-modal access** to six essential amenities (grocery,
pharmacy, park, school, library, transit stop) across ~835 Maricopa County
census block groups, and lets the viewer probe **sociodemographic disparities**
in that access.

Purpose: a job-search work sample demonstrating R + Shiny + `leaflet` +
spatial data + reproducible dashboarding.

**Live app:** _(add your shinyapps.io URL after deploying — see below)_
**Code:** this repository.

## What it does
Two map modes, chosen from the **Amenity / measure** selector:

- **Single amenity** — an interactive choropleth (`leaflet`) of travel time
  from each block group to the **nearest** amenity of the selected type, by
  **walking** or **biking** network routing. Hover for per-block-group detail;
  grey = not reachable within 60 min. A **toggleable point layer** overlays
  every destination of the selected type (marker-clustered).
- **Cumulative opportunity** — within a chosen **travel-time budget**
  (15–60 min), how much each block group can reach: **amenity types
  reachable** (of 6) or **total destinations reachable** across types.
- **Equity chart** (`ggplot2`): the distribution of a chosen sociodemographic
  variable (median income, % White, % Hispanic, % renter, population density)
  across access bins — the disparity story. Bins are travel-time (single
  amenity) or opportunity level (cumulative).
- **Value boxes** adapt to the measure — e.g. share reachable within 15 min /
  median types reachable / median destinations reachable, plus n mapped.

## Files
- `app.R` — the Shiny app. Self-contained; reads only `phxmob_data.rds`.
- `phxmob_data.rds` — baked bundle (block-group polygons + ACS
  sociodemographics + single-amenity time-to-nearest + cumulative-opportunity
  measures + amenity point locations). ~0.26 MB; **no Census API key or `D:\`
  data needed at runtime.**
- `prep_data.R` — one-time build script that regenerates the bundle from the
  canonical frame (`D:/research/phxmob/data/access.gpkg`) + `tigris`
  boundaries. Only needed to refresh the data.

## Run locally
```r
# from this folder
shiny::runApp()
```
Requires: `shiny`, `bslib`, `leaflet`, `dplyr`, `sf`, `ggplot2`, `scales`.

## Deploy a live link
Run `deploy.R` (it wraps `rsconnect::deployApp()`): create a free
shinyapps.io account, paste your token from Account > Tokens, and run the
script. Redeploys reuse the same URL. Then paste that URL into the **Live app**
line above.

## Provenance / data note
Derived from **unpublished HUM Lab data**; external sharing of this derived,
aggregated subset was signed off by **Chris Lim (2026-07-08)**. Figures are a
work sample, not a published result. Underlying method: `r5r` network routing;
2020 Census geography, ACS 2019–2023 sociodemographics.
