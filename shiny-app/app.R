# app.R ----------------------------------------------------------------------
# Phoenix Multi-Modal Amenity Access — interactive dashboard
#
# A work-sample Shiny app built from the phxmob research project (Weesner &
# Lim, UA Zuckerman College of Public Health). It maps time-denominated access
# to six essential amenities across Maricopa County block groups and lets the
# user probe sociodemographic disparities in that access.
#
# Two map modes:
#   * Single amenity  -> travel time to the NEAREST destination of that type,
#                        with an optional overlay of every destination point.
#   * Cumulative       -> opportunity within a chosen travel-time budget: how
#                        many amenity TYPES are reachable, or how many total
#                        DESTINATIONS across types.
#
# Self-contained: reads only phxmob_data.rds (baked by prep_data.R). No Census
# API key, no external data. Run with:  shiny::runApp()
#
# Derived from unpublished HUM Lab data; external sharing signed off by
# Chris Lim (2026-07-08).

library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)
library(scales)

d    <- readRDS("phxmob_data.rds")
bg   <- d$bg
acc  <- d$acc
cum  <- d$cum
dest <- d$dest

# ---- lookups ----------------------------------------------------------------
# Amenity selector: six single types, plus two cumulative-opportunity measures.
amenities <- list(
  "Single amenity" = c("Grocery"      = "grocery",
                       "Pharmacy"     = "pharmacy",
                       "Park"         = "park",
                       "School"       = "schools",
                       "Library"      = "library",
                       "Transit stop" = "transit"),
  "Cumulative opportunity" = c("Amenity types reachable"      = "n_types",
                               "Total destinations reachable" = "n_dest")
)
cum_measures <- c("n_types", "n_dest")

# short labels used in titles
labels <- c(grocery = "grocery", pharmacy = "pharmacy", park = "park",
            schools = "school", library = "library", transit = "transit stop",
            n_types = "amenity types reachable",
            n_dest  = "total destinations reachable")

# colour used for each amenity type's point overlay
pt_cols <- c(grocery = "#2c7fb8", pharmacy = "#d95f0e", park = "#31a354",
             schools = "#756bb1", library = "#c51b8a", transit = "#636363")

modes <- c("Walk" = "walk", "Bike" = "bike")
budgets <- c("15 min" = "15", "20 min" = "20", "30 min" = "30",
             "40 min" = "40", "50 min" = "50", "60 min" = "60")

demos <- list(
  inc      = list(label = "Median household income", fmt = label_dollar(accuracy = 1)),
  pct_wht  = list(label = "% White (non-Hispanic)",  fmt = label_percent(accuracy = 1)),
  pct_hisp = list(label = "% Hispanic",              fmt = label_percent(accuracy = 1)),
  pct_rent = list(label = "% Renter-occupied",       fmt = label_percent(accuracy = 1)),
  density  = list(label = "Population density (/km2)", fmt = label_comma(accuracy = 1))
)
demo_choices <- setNames(names(demos), vapply(demos, `[[`, "", "label"))

# time-to-nearest palette (high time = bad = red)
tt_levels <- c("15", "20", "30", "40", "50", "60", "Not within 60 min")
pal_bins  <- c(15, 20, 30, 40, 50, 60)
time_pal  <- colorBin("YlOrRd", domain = pal_bins, bins = c(15, 20, 30, 40, 50, 60, 61),
                      na.color = "#cccccc")
# opportunity palette (more reachable = good = blue-green)
types_pal <- colorBin("YlGnBu", domain = c(0, 6), bins = 0:6, na.color = "#cccccc")

# ---- UI ---------------------------------------------------------------------
ui <- page_sidebar(
  title = "Phoenix Multi-Modal Amenity Access",
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  fillable = FALSE,
  sidebar = sidebar(
    width = 320,
    selectInput("cat", "Amenity / measure", choices = amenities, selected = "grocery"),
    radioButtons("mode", "Travel mode", choices = modes, inline = TRUE),
    conditionalPanel(
      "input.cat == 'n_types' || input.cat == 'n_dest'",
      selectInput("budget", "Travel-time budget", choices = budgets, selected = "30")
    ),
    conditionalPanel(
      "input.cat != 'n_types' && input.cat != 'n_dest'",
      checkboxInput("show_pts", "Show amenity locations on map", FALSE)
    ),
    selectInput("demo", "Compare access against", choices = demo_choices,
                selected = "inc"),
    hr(),
    helpText(
      "Single amenity: travel time (min) from each Maricopa County block group",
      "to the nearest amenity of that type, by walking or biking network",
      "routing. Grey = not reachable within 60 minutes. Toggle the point",
      "layer to see every destination of the selected type."
    ),
    helpText(
      "Cumulative opportunity: within the chosen travel-time budget, how many",
      "amenity types (of 6) — or how many total destinations across types —",
      "each block group can reach."
    ),
    helpText(
      HTML("<b>Chart:</b> the sociodemographic profile of block groups by",
           "their access — the disparity story.")
    ),
    helpText(HTML("<small>Source: phxmob (Weesner &amp; Lim, UA). r5r network",
                  "routing; 2020 Census / ACS 2019&ndash;2023. Work sample —",
                  "not a published result.</small>"))
  ),
  layout_columns(
    fill = FALSE,
    value_box(title = textOutput("vb1_title"),
              value = textOutput("vb1"), theme = "primary"),
    value_box(title = textOutput("vb2_title"),
              value = textOutput("vb2"), theme = "secondary"),
    value_box(title = "Block groups mapped",
              value = textOutput("vb_n"), theme = "light")
  ),
  card(
    full_screen = TRUE, height = 540,
    card_header(textOutput("map_title", inline = TRUE)),
    leafletOutput("map", height = 500)
  ),
  card(
    height = 400,
    card_header(textOutput("chart_title", inline = TRUE)),
    plotOutput("equity", height = 340)
  )
)

# ---- server -----------------------------------------------------------------
server <- function(input, output, session) {

  is_cum <- reactive(input$cat %in% cum_measures)

  # per-GEOID value (v) joined to geometry + demographics.
  #   single amenity -> v = travel time to nearest (NA if > 60 min)
  #   cumulative     -> v = count (types reachable, or destinations reachable)
  sel <- reactive({
    if (is_cum()) {
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

  # palette + legend spec for the current measure
  spec <- reactive({
    if (input$cat == "n_types") {
      list(pal = types_pal, values = 0:6,
           legend_title = sprintf("Types (≤ %s min)", input$budget))
    } else if (input$cat == "n_dest") {
      v <- sel()$v
      pal <- colorQuantile("YlGnBu", domain = v, n = 6, na.color = "#cccccc")
      list(pal = pal, values = v,
           legend_title = sprintf("Destinations (≤ %s min)", input$budget))
    } else {
      list(pal = time_pal, values = pal_bins, legend_title = "Min to nearest")
    }
  })

  output$map_title <- renderText({
    if (input$cat == "n_types")
      sprintf("Amenity types reachable within %s min by %s",
              input$budget, tolower(mode_label()))
    else if (input$cat == "n_dest")
      sprintf("Total destinations reachable within %s min by %s",
              input$budget, tolower(mode_label()))
    else
      sprintf("Time to nearest %s by %s", amenity_label(), tolower(mode_label()))
  })
  output$chart_title <- renderText({
    if (is_cum())
      sprintf("%s vs. %s (%s, ≤ %s min)", demos[[input$demo]]$label,
              amenity_label(), tolower(mode_label()), input$budget)
    else
      sprintf("%s vs. time to nearest %s (%s)", demos[[input$demo]]$label,
              amenity_label(), tolower(mode_label()))
  })

  # ---- value boxes (titles + values adapt to the measure) ----
  output$vb1_title <- renderText({
    if (input$cat == "n_types") "Median types reachable"
    else if (input$cat == "n_dest") "Median destinations reachable"
    else "Reachable within 15 min"
  })
  output$vb1 <- renderText({
    v <- sel()$v
    if (input$cat == "n_types") sprintf("%g of 6", median(v, na.rm = TRUE))
    else if (input$cat == "n_dest") format(round(median(v, na.rm = TRUE)), big.mark = ",")
    else percent(mean(!is.na(v) & v <= 15), accuracy = 1)
  })
  output$vb2_title <- renderText({
    if (input$cat == "n_types") "Reach all 6 types"
    else if (input$cat == "n_dest") "Reach ≥ 1 destination"
    else "Median time to nearest"
  })
  output$vb2 <- renderText({
    v <- sel()$v
    if (input$cat == "n_types") percent(mean(v == 6, na.rm = TRUE), accuracy = 1)
    else if (input$cat == "n_dest") percent(mean(v > 0, na.rm = TRUE), accuracy = 1)
    else if (all(is.na(v))) "—" else paste0(median(v, na.rm = TRUE), " min")
  })
  output$vb_n <- renderText(format(nrow(bg), big.mark = ","))

  # ---- interactive choropleth ----
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 8)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -112.0, lat = 33.45, zoom = 9)
  })

  # redraw polygons + legend on any measure change
  observe({
    s  <- sel()
    sp <- spec()
    if (is_cum()) {
      detail <- sprintf("Reachable: <b>%s</b>", format(s$v, big.mark = ","))
    } else {
      detail <- sprintf("Time to nearest: <b>%s</b>",
                        ifelse(is.na(s$v), "not within 60 min", paste0(s$v, " min")))
    }
    labs <- sprintf(
      "<b>Block group</b> %s<br/>%s<br/>%s: <b>%s</b>",
      s$GEOID, detail,
      demos[[input$demo]]$label, demos[[input$demo]]$fmt(s[[input$demo]])
    ) |> lapply(htmltools::HTML)

    leafletProxy("map", data = s) |>
      clearShapes() |>
      clearControls() |>
      addPolygons(
        fillColor = ~sp$pal(v),
        fillOpacity = 0.8, weight = 0.4, color = "white",
        highlightOptions = highlightOptions(weight = 2, color = "#333",
                                             bringToFront = TRUE),
        label = labs
      ) |>
      addLegend("bottomright", pal = sp$pal, values = sp$values,
                title = sp$legend_title, opacity = 0.9)
  })

  # amenity-location overlay (single-amenity mode only, toggled on)
  observe({
    proxy <- leafletProxy("map") |> clearGroup("pts")
    if (!is_cum() && isTRUE(input$show_pts)) {
      pts <- dest |> filter(cat == input$cat)
      proxy |> addCircleMarkers(
        data = pts, lng = ~lng, lat = ~lat, group = "pts",
        radius = 3, stroke = FALSE,
        fillColor = pt_cols[[input$cat]], fillOpacity = 0.75,
        label = ~name,
        clusterOptions = markerClusterOptions(disableClusteringAtZoom = 13)
      )
    }
  })

  # ---- equity chart: demographic distribution across access bins ----
  output$equity <- renderPlot({
    s  <- st_drop_geometry(sel())
    dm <- input$demo

    if (input$cat == "n_types") {
      s$bin <- factor(s$v, levels = 0:6)
      fills <- setNames(colorRampPalette(c("#ffffcc", "#253494"))(7), levels(s$bin))
      xlab  <- sprintf("Amenity types reachable (≤ %s min)", input$budget)
    } else if (input$cat == "n_dest") {
      br <- unique(quantile(s$v, probs = seq(0, 1, 0.2), na.rm = TRUE))
      s$bin <- cut(s$v, breaks = br, include.lowest = TRUE, dig.lab = 5)
      k     <- nlevels(s$bin)
      fills <- setNames(colorRampPalette(c("#ffffcc", "#253494"))(k), levels(s$bin))
      xlab  <- sprintf("Destinations reachable (≤ %s min, quintiles)", input$budget)
    } else {
      s$bin <- factor(ifelse(is.na(s$v), "Not within 60 min", as.character(s$v)),
                      levels = tt_levels)
      fills <- setNames(c(colorRampPalette(c("#ffffb2", "#bd0026"))(6), "#cccccc"),
                        tt_levels)
      xlab  <- "Time to nearest amenity (min)"
    }

    ggplot(s, aes(x = bin, y = .data[[dm]], fill = bin)) +
      geom_boxplot(outlier.size = 0.6, width = 0.65, alpha = 0.9) +
      scale_fill_manual(values = fills, guide = "none", drop = FALSE) +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(labels = demos[[dm]]$fmt) +
      labs(x = xlab, y = demos[[dm]]$label) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.x = element_blank(),
            axis.text.x = element_text(angle = if (input$cat == "n_dest") 20 else 0,
                                       hjust = if (input$cat == "n_dest") 1 else 0.5))
  })
}

shinyApp(ui, server)
