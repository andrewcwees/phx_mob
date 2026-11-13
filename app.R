# app.R ----------------------------------------------------------------------
# Phoenix Multi-Modal Amenity Access -- interactive map
#
# A work-sample Shiny app built from the phxmob research project (Weesner &
# Lim, HUM.Lab, University of Arizona). It maps time-denominated access
# to eleven essential amenities across Maricopa County block groups and lets the
# user explore the access gradient by mode, amenity type, and travel-time
# budget.
#
# Three map modes:
#   * Walk / Bike     -> travel time to nearest, or cumulative opportunity.
#   * Car dependence  -> where active-mode access fails but driving reaches.
#                        Single amenity: best active-mode time, with "car only"
#                        for block groups unreachable on foot/bike. Cumulative:
#                        the gap between car and active-mode opportunity counts.
#
# Self-contained: reads only phxmob_data.rds (baked by prep_data.R). No Census
# API key, no external data. Run with:  shiny::runApp()
#
# Derived from unpublished HUM.Lab data; shared with the PI's permission.

library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)
library(scales)
library(tidygeocoder)

d    <- readRDS("phxmob_data.rds")
bg   <- d$bg
acc  <- d$acc
cum  <- d$cum
dest <- d$dest

# ---- lookups ----------------------------------------------------------------
amenities <- list(
  "Single amenity" = c("Grocery"           = "grocery",
                       "Park"              = "park",
                       "Pharmacy"          = "pharmacy",
                       "School"            = "schools",
                       "Library"           = "library",
                       "Childcare"         = "childcare",
                       "Bank"              = "bank",
                       "Restaurant"        = "restaurant",
                       "Fitness center"    = "fitness",
                       "Place of worship"  = "worship",
                       "Transit stop"      = "transit"),
  "Cumulative opportunity" = c("Amenity types reachable"      = "n_types",
                               "Total destinations reachable" = "n_dest")
)
cum_measures <- c("n_types", "n_dest")

labels <- c(grocery = "grocery", park = "park", pharmacy = "pharmacy",
            schools = "school", library = "library", childcare = "childcare",
            bank = "bank", restaurant = "restaurant",
            fitness = "fitness center", worship = "place of worship",
            transit = "transit stop",
            n_types = "amenity types reachable",
            n_dest  = "total destinations reachable")

pt_cols <- c(grocery = "#2c7fb8", park = "#31a354", pharmacy = "#d95f0e",
             schools = "#756bb1", library = "#c51b8a", childcare = "#e6ab02",
             bank = "#543005", restaurant = "#e31a1c", fitness = "#1b9e77",
             worship = "#6a3d9a", transit = "#636363")

modes <- c("Walking" = "walk", "Biking" = "bike", "Car dependence" = "cardep")
budgets <- c("15 min" = "15", "20 min" = "20", "30 min" = "30",
             "40 min" = "40", "50 min" = "50", "60 min" = "60")

demos <- list(
  inc      = list(label = "Median household income", fmt = label_dollar(accuracy = 1)),
  pct_wht  = list(label = "% White (non-Hispanic)",  fmt = label_percent(accuracy = 1)),
  pct_hisp = list(label = "% Hispanic",              fmt = label_percent(accuracy = 1)),
  pct_rent = list(label = "% Renter-occupied",       fmt = label_percent(accuracy = 1)),
  density  = list(label = "Population density (/km2)", fmt = label_comma(accuracy = 1))
)

pal_bins  <- c(15, 20, 30, 40, 50, 60)
time_pal  <- colorBin("YlOrRd", domain = pal_bins, bins = c(15, 20, 30, 40, 50, 60, 61),
                      na.color = "#cccccc")
types_pal <- colorBin(
  colorRampPalette(RColorBrewer::brewer.pal(9, "YlGnBu"))(11),
  domain = c(0, 11), bins = 0:11, na.color = "#cccccc")

# car-dependence palette for single amenity: green (good active access) through
# red (poor) to dark maroon (car only). v is best active-mode time (0-60) or 99
# (car only); NA = no access at all.
cardep_pal <- colorBin(
  c("#1a9850", "#91cf60", "#fee08b", "#fc8d59", "#d73027", "#67001f"),
  domain = c(0, 100), bins = c(0, 21, 31, 41, 51, 61, 100),
  na.color = "#cccccc")

# car-dependence palette for cumulative n_types: gap 0-11
cardep_types_pal <- colorBin(
  c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59", "#d73027"),
  domain = c(0, 12), bins = c(0, 2, 4, 6, 8, 10, 12),
  na.color = "#cccccc")

ext_link <- function(href, label) {
  sprintf("<a href='%s' target='_blank' rel='noopener'>%s</a>", href, label)
}

# Basemap tiles are Esri's keyless services. CARTO's basemaps now require an
# API key and serve an "API key required" placeholder tile without one.
esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/%s/MapServer/tile/{z}/{y}/{x}"
esri_attr <- "Tiles &copy; Esri &mdash; Esri, HERE, Garmin, &copy; OpenStreetMap contributors, and the GIS user community"
# The gray canvas is cached to z16; past that leaflet upscales its tiles.
gray_opts <- tileOptions(maxNativeZoom = 16, maxZoom = 19)

basemaps <- c("Light" = "light", "Satellite" = "sat")
tiles_for <- function(map, which) {
  if (identical(which, "sat"))
    map |>
      addProviderTiles(providers$Esri.WorldImagery, group = "basemap") |>
      addTiles(sprintf(esri_url, "Reference/World_Boundaries_and_Places"),
               attribution = esri_attr, group = "basemap")
  else
    map |>
      addTiles(sprintf(esri_url, "Canvas/World_Light_Gray_Base"),
               attribution = esri_attr, options = gray_opts, group = "basemap") |>
      addTiles(sprintf(esri_url, "Canvas/World_Light_Gray_Reference"),
               attribution = esri_attr, options = gray_opts, group = "basemap")
}

basemap_control <- function(sel) {
  if (is.null(sel)) sel <- "light"
  tags$div(
    class = "mapseg", `data-input` = "basemap",
    tags$div(class = "mapseg-head", "Background"),
    tags$div(
      class = "mapseg-btns",
      lapply(seq_along(basemaps), function(i) {
        v <- unname(basemaps)[i]
        tags$button(type = "button", `data-val` = v,
                    class = paste("mapseg-btn", if (identical(v, sel)) "active"),
                    names(basemaps)[i])
      })
    )
  )
}

# ---- UI ---------------------------------------------------------------------
ui <- page_fillable(
  padding = 0,
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  tags$head(tags$style(HTML(
    "/* Banner */
     .app-banner { padding: 1.4rem 1.5rem 1.2rem; background: rgba(246,246,244,0.97);
                   position: relative; z-index: 20;
                   box-shadow: 0 1px 4px rgba(0,0,0,0.08); }
     .app-brand-title { font-size: 1.8rem; font-weight: 700;
                        line-height: 1.15; letter-spacing: -.02em;
                        text-wrap: balance; margin: 0;
                        flex: 1 1 auto; min-width: 0; }
     .app-brand-lede { font-size: .9rem; line-height: 1.5; opacity: .85;
                       max-width: 95ch; margin-bottom: .4rem; }
     .app-brand-lede:last-child { margin-bottom: 0; }
     .app-brand-fine { font-size: .75rem; line-height: 1.45; opacity: .6;
                       max-width: 95ch; margin: .6rem 0 0; }
     .fine-h { font-weight: 700; letter-spacing: .06em; font-size: .68rem;
               text-transform: uppercase; }
     .fine-src { margin: .2rem 0 0; padding-left: 1.1rem; }
     .fine-src > li { break-inside: avoid; margin-bottom: .15rem; }
     .fine-src b { font-weight: 600; opacity: .9; }
     .app-brand-source { font-size: .72rem; line-height: 1.4; opacity: .5;
                          max-width: 95ch; margin: .5rem 0 0; }
     @media (max-width: 700px) {
       .app-brand-title { font-size: 1.5rem; }
       .app-banner { padding: 1.1rem 1rem .9rem; }
     }
     /* Collapse toggle */
     .brand-head { display: flex; align-items: center;
                   justify-content: space-between; gap: 1.4rem; }
     #brand-body { margin-top: .85rem; }
     .brand-actions { display: flex; align-items: center; gap: .85rem;
                      flex: 0 0 auto; }
     .collapse-toggle { border: 1px solid rgba(0,0,0,.18); background: #fff;
                        border-radius: 999px; padding: .16rem .6rem;
                        font-size: .74rem; font-weight: 600; color: #33383d;
                        display: inline-flex; align-items: center; gap: .35rem;
                        flex: 0 0 auto; }
     .collapse-toggle:hover { background: #f0f0ee; }
     .ct-chev { font-size: .58rem; transition: transform .18s ease;
                display: inline-block; }
     .collapse-toggle[aria-expanded='false'] .ct-chev { transform: rotate(180deg); }
     /* Map selectors (top-left of map) */
     .map-selectors { position: absolute; top: 10px; left: 10px; z-index: 1000;
                      background: rgba(255,255,255,0.96); border-radius: 5px;
                      padding: .6rem .8rem; box-shadow: 0 1px 5px rgba(0,0,0,0.25);
                      width: 220px; }
     .map-selectors .form-group { margin-bottom: .5rem; }
     .map-selectors .form-group:last-child { margin-bottom: 0; }
     .map-selectors .control-label { font-size: .78rem; font-weight: 600;
                                     margin-bottom: .15rem; }
     /* Map controls (top-right of map) */
     .map-controls { position: absolute; top: 10px; right: 10px; z-index: 1000;
                     background: #fff; border-radius: 5px;
                     padding: 6px; box-shadow: 0 1px 5px rgba(0,0,0,.4);
                     width: 168px; }
     .map-controls .form-group { margin-bottom: .15rem; }
     .map-controls .control-label { display: none; }
     .map-controls .checkbox { margin-top: 0; margin-bottom: .1rem; }
     .map-controls .checkbox label { font-size: .75rem; font-weight: 500;
                                     padding-left: 20px; }
     .map-controls .irs { margin-top: 0; }
     .map-controls .irs--shiny .irs-bar { height: 3px; top: 30px; }
     .map-controls .irs--shiny .irs-line { height: 3px; top: 30px; }
     .map-controls .irs--shiny .irs-handle { width: 14px; height: 14px; top: 24px; }
     .map-controls .ctrl-sep { border: 0; border-top: 1px solid #e9ecef;
                                margin: 5px 1px; }
     /* Segmented controls and section headers (matches redenv) */
     .mapseg-btns { display: flex; gap: 3px; }
     .mapseg-head,
     .ctrl-head { display: block; font-size: .6rem; font-weight: 700;
                  letter-spacing: .04em; text-transform: uppercase;
                  color: #6c757d; margin: 0 0 3px 1px; line-height: 1.2; }
     .mapseg-btn { border: 0; border-radius: 3px; cursor: pointer;
                   padding: .18rem .5rem; font-size: .72rem; font-weight: 600;
                   line-height: 1.45; color: #495057; background: #eef0f2;
                   transition: background-color .15s ease, color .15s ease; }
     .mapseg-btn:hover { background: #e0e4e8; color: #1d2124; }
     .mapseg-btn.active { background: #2780e3; color: #fff; }
     .mapseg-btn:focus-visible { outline: 2px solid #1a5fa8; outline-offset: 1px; }
     /* Map title chip (bottom-left of map) */
     .map-title-chip { position: absolute; bottom: 30px; left: 10px;
                       z-index: 1000; background: rgba(255,255,255,0.92);
                       padding: .3rem .7rem; border-radius: .35rem;
                       font-size: .82rem; font-weight: 600;
                       box-shadow: 0 1px 4px rgba(0,0,0,0.12);
                       pointer-events: none; }
     /* Address search */
     .addr-wrap { position: relative; }
     .addr-row { display: flex; gap: 4px; align-items: start; }
     .addr-row .form-group { flex: 1; margin-bottom: 0; }
     .addr-row input[type='text'] { font-size: .78rem; padding: .25rem .4rem; height: auto; }
     .addr-row .btn { flex: 0 0 auto; font-size: .72rem; padding: .25rem .5rem;
                      line-height: 1.4; }
     .addr-suggestions { position: absolute; top: 100%; left: 0; right: 0;
                         background: #fff; border: 1px solid #ddd; border-top: 0;
                         border-radius: 0 0 4px 4px; max-height: 180px;
                         overflow-y: auto; z-index: 1100;
                         box-shadow: 0 2px 6px rgba(0,0,0,.15); display: none; }
     .addr-item { padding: .35rem .5rem; font-size: .75rem; cursor: pointer;
                  border-bottom: 1px solid #f0f0f0; line-height: 1.35; }
     .addr-item:last-child { border-bottom: 0; }
     .addr-item:hover { background: #e8f0fe; }"))),
  tags$head(tags$script(HTML(
    "$(document).on('shown.bs.collapse hidden.bs.collapse', function() {
       window.dispatchEvent(new Event('resize'));
     });
     $(document).on('click', '.mapseg-btn', function() {
       var btn = $(this), seg = btn.closest('.mapseg');
       seg.find('.mapseg-btn').removeClass('active');
       btn.addClass('active');
       Shiny.setInputValue(seg.data('input'), btn.data('val'));
     });
     $(document).on('shiny:connected', function() {
       Shiny.setInputValue('basemap', 'light');
     });
     $(document).on('keydown', '#addr', function(e) {
       if (e.key === 'Enter') {
         e.preventDefault(); $('#addr_go').click(); $('#addr-suggestions').hide();
       }
     });
     (function() {
       var timer = null;
       $(document).on('input', '#addr', function() {
         clearTimeout(timer);
         var q = $(this).val().trim(), $s = $('#addr-suggestions');
         if (q.length < 3) { $s.hide().empty(); return; }
         timer = setTimeout(function() {
           var url = 'https://nominatim.openstreetmap.org/search?format=json&limit=5' +
             '&countrycodes=us&viewbox=-112.8,33.0,-111.2,33.9&bounded=0&q=' +
             encodeURIComponent(q);
           fetch(url).then(function(r) { return r.json(); }).then(function(data) {
             $s.empty();
             if (!data.length) { $s.hide(); return; }
             data.forEach(function(d) {
               $('<div class=\"addr-item\"></div>').text(d.display_name)
                 .on('click', function() {
                   $('#addr').val(d.display_name);
                   $s.hide().empty();
                   Shiny.setInputValue('addr_geo',
                     {lat: parseFloat(d.lat), lng: parseFloat(d.lon)},
                     {priority: 'event'});
                 }).appendTo($s);
             });
             $s.show();
           });
         }, 500);
       });
       $(document).on('click', function(e) {
         if (!$(e.target).closest('.addr-wrap').length) $('#addr-suggestions').hide();
       });
     })();"))),

  # ---- banner ----
  div(class = "app-banner",
    div(class = "brand-head",
      div(class = "app-brand-title", "Amenity Access in Phoenix, AZ"),
      div(class = "brand-actions",
        tags$button(
          class = "btn btn-sm collapse-toggle", type = "button",
          `data-bs-toggle` = "collapse", `data-bs-target` = "#brand-body",
          `aria-expanded` = "true", `aria-controls` = "brand-body",
          span(class = "ct-label", "About"), span(class = "ct-chev", "▲")))),
    div(
      id = "brand-body", class = "collapse show",
      p(class = "app-brand-lede",
        sprintf(paste(
          "This app maps time-denominated (15–60 min), multi-modal",
          "(walk, bike, car) access to 11 amenity categories across the",
          "Phoenix metro (%s block groups)."),
          format(nrow(bg), big.mark = ","))),
      div(class = "app-brand-fine",
        span(class = "fine-h", "Data"),
        tags$ul(
          class = "fine-src",
          tags$li(HTML(paste0(
            "<b>Amenity locations.</b> Advan Monthly Patterns (Dewey Data),",
            " 11 categories by NAICS code. Transit stops from Valley Metro",
            " GTFS."))),
          tags$li(HTML(paste0(
            "<b>Travel time.</b> ",
            ext_link("https://ipeagit.github.io/r5r/", "r5r"),
            " routing on the ",
            ext_link("https://www.openstreetmap.org/", "OpenStreetMap"),
            " road network; walking, biking, and driving modes."))),
          tags$li(HTML(paste0(
            "<b>Block-group origins.</b> Population-weighted centroids from the ",
            ext_link("https://www.census.gov/programs-surveys/decennial-census.html",
                     "2020 decennial census"),
            ", US Census Bureau."))),
          tags$li(HTML(paste0(
            "<b>Sociodemographics.</b> ",
            ext_link("https://www.census.gov/programs-surveys/acs",
                     "American Community Survey"),
            " 2019&ndash;2023, US Census Bureau."))))),
      div(class = "app-brand-source",
          HTML("HUM.Lab, University of Arizona.<br/>Work sample, not a published result.")))
  ),

  # ---- full-height map ----
  as_fill_item(div(style = "position: relative; overflow: hidden;",

    leafletOutput("map", width = "100%", height = "100%"),

    div(class = "map-selectors",
      selectInput("cat", "Destination", choices = amenities, selected = "grocery"),
      selectInput("mode", "Map view", choices = modes, selected = "walk"),
      conditionalPanel(
        "input.cat == 'n_types' || input.cat == 'n_dest'",
        selectInput("budget", "Travel-time budget", choices = budgets, selected = "30")
      ),
      tags$hr(class = "ctrl-sep"),
      span(class = "ctrl-head", "Find address"),
      div(class = "addr-wrap",
        div(class = "addr-row",
          textInput("addr", NULL, placeholder = "Street address"),
          actionButton("addr_go", "Go", class = "btn-sm btn-primary")
        ),
        div(id = "addr-suggestions", class = "addr-suggestions")
      )
    ),

    div(class = "map-controls",
      basemap_control("light"),
      tags$hr(class = "ctrl-sep"),
      span(class = "ctrl-head", "Layers"),
      checkboxInput("show_bg", "Census blocks", TRUE),
      conditionalPanel(
        condition = "input.cat !== 'n_types' && input.cat !== 'n_dest'",
        checkboxInput("show_pts", "Destinations", FALSE)
      ),
      tags$hr(class = "ctrl-sep"),
      span(class = "ctrl-head", "Opacity"),
      sliderInput("fillop", NULL, min = 0.2, max = 1, value = 0.8,
                  step = 0.05, ticks = FALSE, width = "100%")
    ),

    div(class = "map-title-chip",
      textOutput("map_title", inline = TRUE)
    )
  ))
)

# ---- server -----------------------------------------------------------------
server <- function(input, output, session) {

  is_cum    <- reactive(input$cat %in% cum_measures)
  is_cardep <- reactive(input$mode == "cardep")

  sel <- reactive({
    if (is_cardep()) {
      if (is_cum()) {
        bud     <- as.integer(input$budget)
        measure <- if (input$cat == "n_types") "n_types" else "n_dest"
        w  <- cum |> filter(mode == "walk", cut == bud) |>
          transmute(GEOID, v_w = .data[[measure]])
        b  <- cum |> filter(mode == "bike", cut == bud) |>
          transmute(GEOID, v_b = .data[[measure]])
        cr <- cum |> filter(mode == "car", cut == bud) |>
          transmute(GEOID, v_c = .data[[measure]])
        m <- w |> full_join(b, by = "GEOID") |> full_join(cr, by = "GEOID") |>
          mutate(
            active_v = ifelse(is.na(v_w) & is.na(v_b), NA_real_,
                              pmax(v_w, v_b, na.rm = TRUE)),
            v = ifelse(is.na(v_c) | is.na(active_v), NA_real_,
                       pmax(v_c - active_v, 0))
          ) |> select(GEOID, v)
      } else {
        w  <- acc |> filter(cat == input$cat, mode == "walk") |>
          transmute(GEOID, tt_w = ttnearest)
        b  <- acc |> filter(cat == input$cat, mode == "bike") |>
          transmute(GEOID, tt_b = ttnearest)
        cr <- acc |> filter(cat == input$cat, mode == "car") |>
          transmute(GEOID, tt_c = ttnearest)
        m <- w |> full_join(b, by = "GEOID") |> full_join(cr, by = "GEOID") |>
          mutate(
            ab = ifelse(is.na(tt_w) & is.na(tt_b), NA_real_,
                        pmin(tt_w, tt_b, na.rm = TRUE)),
            v = case_when(
              is.na(ab) & !is.na(tt_c) ~ 99L,
              is.na(ab) & is.na(tt_c)  ~ NA_integer_,
              TRUE                     ~ as.integer(ab)
            )
          ) |> select(GEOID, v)
      }
    } else if (is_cum()) {
      m <- cum |>
        filter(mode == input$mode, cut == as.integer(input$budget)) |>
        mutate(v = if (input$cat == "n_types") n_types else n_dest) |>
        select(GEOID, v)
    } else {
      m <- acc |>
        filter(cat == input$cat, mode == input$mode) |>
        transmute(GEOID, v = ttnearest)
    }
    bg |> left_join(m, by = "GEOID")
  })

  amenity_label <- reactive(labels[[input$cat]])
  mode_label    <- reactive(names(modes)[modes == input$mode])

  spec <- reactive({
    if (is_cardep() && !is_cum()) {
      list(pal = cardep_pal, type = "cardep_single")
    } else if (is_cardep() && input$cat == "n_types") {
      list(pal = cardep_types_pal, type = "cardep_types", values = 0:11,
           legend_title = sprintf("Car advantage (types, ≤ %s min)", input$budget))
    } else if (is_cardep() && input$cat == "n_dest") {
      v <- sel()$v
      pal <- colorQuantile("YlOrRd", domain = v, n = 6, na.color = "#cccccc")
      list(pal = pal, type = "cardep_dest", values = v,
           legend_title = sprintf("Car advantage (destinations, ≤ %s min)", input$budget))
    } else if (input$cat == "n_types") {
      list(pal = types_pal, type = "cum", values = 0:11,
           legend_title = sprintf("Types (≤ %s min)", input$budget))
    } else if (input$cat == "n_dest") {
      v <- sel()$v
      pal <- colorQuantile("YlGnBu", domain = v, n = 6, na.color = "#cccccc")
      list(pal = pal, type = "cum", values = v,
           legend_title = sprintf("Destinations (≤ %s min)", input$budget))
    } else {
      list(pal = time_pal, type = "single", values = pal_bins,
           legend_title = "Min to nearest")
    }
  })

  output$map_title <- renderText({
    if (is_cardep()) {
      if (input$cat == "n_types")
        sprintf("Car dependence: amenity types within %s min", input$budget)
      else if (input$cat == "n_dest")
        sprintf("Car dependence: destinations within %s min", input$budget)
      else
        sprintf("Car dependence: %s", amenity_label())
    } else if (input$cat == "n_types") {
      sprintf("Amenity types reachable within %s min by %s",
              input$budget, tolower(mode_label()))
    } else if (input$cat == "n_dest") {
      sprintf("Total destinations reachable within %s min by %s",
              input$budget, tolower(mode_label()))
    } else {
      sprintf("Time to nearest %s by %s", amenity_label(), tolower(mode_label()))
    }
  })

  # ---- interactive choropleth ----
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 8, zoomControl = FALSE)) |>
      addMapPane("bg_pane", zIndex = 410) |>
      addMapPane("pts_pane", zIndex = 450) |>
      tiles_for("light") |>
      setView(lng = -112.0, lat = 33.45, zoom = 9)
  })

  observeEvent(input$basemap, {
    bm <- input$basemap %||% "light"
    leafletProxy("map") |> clearTiles() |> tiles_for(bm)
  }, ignoreInit = TRUE)

  observe({
    p <- leafletProxy("map") |> clearShapes() |> clearControls()
    if (!isTRUE(input$show_bg)) return()

    s  <- sel()
    sp <- spec()
    sat  <- identical(input$basemap %||% "light", "sat")
    bw   <- if (sat) 0.8 else 0.4
    hcol <- if (sat) "#ffff00" else "#333"

    if (is_cardep() && !is_cum()) {
      detail <- sprintf("Best active mode: <b>%s</b>",
                        ifelse(s$v == 99, "car only",
                               ifelse(is.na(s$v), "no access",
                                      paste0(s$v, " min"))))
    } else if (is_cardep()) {
      detail <- sprintf("Car advantage: <b>%s</b> more %s",
                        format(round(s$v), big.mark = ","),
                        if (input$cat == "n_types") "types" else "destinations")
    } else if (is_cum()) {
      detail <- sprintf("Reachable: <b>%s</b>", format(s$v, big.mark = ","))
    } else {
      detail <- sprintf("Time to nearest: <b>%s</b>",
                        ifelse(is.na(s$v), "not within 60 min", paste0(s$v, " min")))
    }

    labs <- sprintf(
      "<b>Block group</b> %s<br/>%s<br/>%s: <b>%s</b>",
      s$GEOID, detail,
      demos[["inc"]]$label, demos[["inc"]]$fmt(s[["inc"]])
    ) |> lapply(htmltools::HTML)

    p <- p |>
      addPolygons(
        data = s,
        fillColor = ~sp$pal(v),
        fillOpacity = input$fillop, weight = bw, color = "white",
        highlightOptions = highlightOptions(weight = 2, color = hcol,
                                             bringToFront = TRUE),
        label = labs,
        options = pathOptions(pane = "bg_pane")
      )

    if (sp$type == "cardep_single") {
      p <- p |> addLegend("bottomright",
        colors = c("#1a9850", "#91cf60", "#fee08b", "#fc8d59",
                   "#d73027", "#67001f", "#cccccc"),
        labels = c("≤ 20 min", "21-30", "31-40",
                   "41-50", "51-60", "Car only", "No access"),
        title = "Car Dependence<br/><small>(best active-mode time)</small>",
        opacity = 0.9)
    } else if (sp$type == "single") {
      p <- p |> addLegend("bottomright",
        colors = c(time_pal(pal_bins), "#cccccc"),
        labels = c("15", "20", "30", "40", "50", "60", "&gt; 60"),
        title = "Time to Nearest<br/><small>(in minutes)</small>",
        opacity = 0.9)
    } else {
      p <- p |> addLegend("bottomright", pal = sp$pal, values = sp$values,
                     title = sp$legend_title, opacity = 0.9)
    }

    p
  })

  # amenity-location overlay (single-amenity mode only, toggled on)
  observe({
    proxy <- leafletProxy("map") |> clearGroup("pts")
    if (!is_cum() && isTRUE(input$show_pts)) {
      pts <- dest |> filter(cat == input$cat)
      cr  <- col2rgb(pt_cols[[input$cat]])
      icf <- htmlwidgets::JS(
        gsub("__B__", as.character(cr[3]),
        gsub("__G__", as.character(cr[2]),
        gsub("__R__", as.character(cr[1]), "
          function(cluster) {
            var n = cluster.getChildCount();
            var f = 0.82 * Math.min(1, Math.log(n) / Math.log(200));
            var base = [__R__, __G__, __B__];
            var r = Math.round(base[0] + (255 - base[0]) * f);
            var g = Math.round(base[1] + (255 - base[1]) * f);
            var b = Math.round(base[2] + (255 - base[2]) * f);
            var lum = 0.299 * r + 0.587 * g + 0.114 * b;
            var txt = lum > 165 ? '#222' : '#fff';
            var size = Math.round(28 + 16 * f);
            var html = '<div style=\"width:' + size + 'px;height:' + size +
              'px;border-radius:50%;display:flex;align-items:center;' +
              'justify-content:center;font-weight:600;font-size:12px;' +
              'box-shadow:0 0 0 1px rgba(0,0,0,.25);background:rgba(' +
              r + ',' + g + ',' + b + ',0.9);color:' + txt + ';\">' + n + '</div>';
            return new L.DivIcon({ html: html, className: 'cluster-shade',
              iconSize: new L.Point(size, size) });
          }", fixed = TRUE), fixed = TRUE), fixed = TRUE))
      proxy |> addCircleMarkers(
        data = pts, lng = ~lng, lat = ~lat, group = "pts",
        radius = 6, stroke = TRUE, weight = 1, color = "#ffffff", opacity = 0.6,
        fillColor = pt_cols[[input$cat]], fillOpacity = 0.85,
        label = ~name,
        options = pathOptions(pane = "pts_pane"),
        clusterOptions = markerClusterOptions(
          disableClusteringAtZoom = 13,
          iconCreateFunction = icf)
      )
    }
  })

  observeEvent(input$addr_go, {
    req(nzchar(trimws(input$addr)))
    result <- tidygeocoder::geo(trimws(input$addr), method = "osm", quiet = TRUE)
    if (is.na(result$lat) || is.na(result$long)) {
      showNotification("Address not found.", type = "warning")
      return()
    }
    leafletProxy("map") |>
      clearGroup("addr_pin") |>
      addMarkers(lng = result$long, lat = result$lat, group = "addr_pin",
                 popup = htmltools::htmlEscape(trimws(input$addr))) |>
      setView(lng = result$long, lat = result$lat, zoom = 13)
  })

  observeEvent(input$addr_geo, {
    geo <- input$addr_geo
    leafletProxy("map") |>
      clearGroup("addr_pin") |>
      addMarkers(lng = geo$lng, lat = geo$lat, group = "addr_pin",
                 popup = htmltools::htmlEscape(trimws(input$addr))) |>
      setView(lng = geo$lng, lat = geo$lat, zoom = 13)
  })
}

shinyApp(ui, server)
