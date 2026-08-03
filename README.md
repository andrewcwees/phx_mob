# Phoenix Multi-Modal Amenity Access

An interactive R Shiny map built from the **phxmob** research project
(Weesner & Lim, UA Zuckerman College of Public Health). It maps
**time-denominated, multi-modal access** to eleven essential amenities across
~835 Maricopa County census block groups for walking, biking, and driving.

Purpose: a job-search work sample demonstrating R + Shiny + `leaflet` +
spatial data.

**Live app:** <https://019f4dab-5684-7130-b404-1e7165bdbd19.share.connect.posit.cloud/>

## What it does

Full-window `leaflet` choropleth with a collapsible About banner and two
floating control panels. Three map views:

- **Single amenity** -- travel time from each block group to the nearest
  amenity of the selected type (grocery, park, pharmacy, school, library,
  childcare, bank, restaurant, fitness center, place of worship, transit stop)
  by walking or biking. Toggleable point layer overlays every destination
  (marker-clustered).
- **Cumulative opportunity** -- within a travel-time budget (15--60 min), how
  many amenity types or total destinations each block group can reach.
- **Car dependence** -- where active-mode access fails but driving reaches.
  Single amenity: best active-mode time, color-graded from green (reachable on
  foot/bike) through dark maroon (car only). Cumulative: the gap between car
  and active-mode opportunity counts.

Basemap toggle (Light / Satellite), layer checkboxes, and opacity slider on
the right panel.

## Files

- `app.R` -- the Shiny app. Self-contained; reads only `phxmob_data.rds`.
- `phxmob_data.rds` -- baked data bundle (block-group polygons + ACS
  sociodemographics + single-amenity time-to-nearest + cumulative-opportunity
  measures + amenity point locations). No Census API key or external data
  needed at runtime.
- `prep_data.R` -- one-time build script that regenerates the bundle from the
  canonical data on `D:\`. Only needed to refresh the data.

## Run locally

```r
shiny::runApp()
```

Requires: `shiny`, `bslib`, `leaflet`, `dplyr`, `sf`, `ggplot2`, `scales`.

## Provenance

Derived from unpublished HUM Lab data; external sharing of this derived,
aggregated subset signed off by Chris Lim. Figures are a work sample, not a
published result. Method: `r5r` network routing on OpenStreetMap; 2020 Census
geography; ACS 2019--2023 sociodemographics; Advan Monthly Patterns (Dewey
Data) for amenity locations.
