# app.R ----------------------------------------------------------------------
# Phoenix Multi-Modal Amenity Access — interactive dashboard
#
# A work-sample Shiny app built from the phxmob research project (Weesner &
# Lim, UA Zuckerman College of Public Health). It maps time-denominated access
# to six essential amenities across Maricopa County block groups and lets the
# user probe sociodemographic disparities in that access.
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

d   <- readRDS("phxmob_data.rds")
bg  <- d$bg
acc <- d$acc

# ---- lookups ----------------------------------------------------------------
amenities <- c("Any amenity (nearest of all six)" = "total",
               "Grocery"  = "grocery",
               "Pharmacy" = "pharmacy",
               "Park"     = "park",
               "School"   = "schools",
               "Library"  = "library",
               "Transit stop" = "transit")

modes <- c("Walk" = "walk", "Bike" = "bike")

demos <- list(
  inc      = list(label = "Median household income", fmt = label_dollar(accuracy = 1)),
  pct_wht  = list(label = "% White (non-Hispanic)",  fmt = label_percent(accuracy = 1)),
  pct_hisp = list(label = "% Hispanic",              fmt = label_percent(accuracy = 1)),
  pct_rent = list(label = "% Renter-occupied",       fmt = label_percent(accuracy = 1)),
  density  = list(label = "Population density (/km2)", fmt = label_comma(accuracy = 1))
)
demo_choices <- setNames(names(demos), vapply(demos, `[[`, "", "label"))

tt_levels <- c("15", "20", "30", "40", "50", "60", "Not within 60 min")
pal_bins  <- c(15, 20, 30, 40, 50, 60)
map_pal   <- colorBin("YlOrRd", domain = pal_bins, bins = c(15, 20, 30, 40, 50, 61),
                      na.color = "#cccccc")

# ---- UI ---------------------------------------------------------------------
ui <- page_sidebar(
  title = "Phoenix Multi-Modal Amenity Access",
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  fillable = FALSE,
  sidebar = sidebar(
    width = 320,
    selectInput("cat", "Amenity", choices = amenities, selected = "total"),
    radioButtons("mode", "Travel mode", choices = modes, inline = TRUE),
    selectInput("demo", "Compare access against", choices = demo_choices,
                selected = "inc"),
    hr(),
    helpText(
      "Map: travel time (min) from each Maricopa County block group to the",
      "nearest amenity of the selected type, by walking or biking network",
      "routing. Grey = not reachable within 60 minutes."
    ),
    helpText(
      HTML("<b>Chart:</b> the sociodemographic profile of block groups by how",
           "far they sit from that amenity — the disparity story.")
    ),
    helpText(HTML("<small>Source: phxmob (Weesner &amp; Lim, UA). r5r network",
                  "routing; 2020 Census / ACS 2019–2023. Work sample —",
                  "not a published result.</small>"))
  ),
  layout_columns(
    fill = FALSE,
    value_box(title = "Reachable within 15 min",
              value = textOutput("vb_pct15"), theme = "primary"),
    value_box(title = "Median time to nearest",
              value = textOutput("vb_median"), theme = "secondary"),
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

  # selected access measure joined to geometry + demographics
  sel <- reactive({
    a <- acc |>
      filter(cat == input$cat, mode == input$mode) |>
      select(GEOID, ttnearest)
    bg |>
      left_join(a, by = "GEOID") |>
      mutate(
        tt_lab = factor(ifelse(is.na(ttnearest), "Not within 60 min",
                               as.character(ttnearest)),
                        levels = tt_levels)
      )
  })

  amenity_label <- reactive(names(amenities)[amenities == input$cat])
  mode_label    <- reactive(names(modes)[modes == input$mode])

  output$map_title <- renderText(
    sprintf("Time to nearest %s by %s", tolower(amenity_label()),
            tolower(mode_label())))
  output$chart_title <- renderText(
    sprintf("%s vs. time to nearest %s (%s)", demos[[input$demo]]$label,
            tolower(amenity_label()), tolower(mode_label())))

  # value boxes
  output$vb_pct15 <- renderText({
    x <- sel()$ttnearest
    percent(mean(!is.na(x) & x <= 15), accuracy = 1)
  })
  output$vb_median <- renderText({
    x <- sel()$ttnearest
    if (all(is.na(x))) "—" else paste0(median(x, na.rm = TRUE), " min")
  })
  output$vb_n <- renderText(format(nrow(bg), big.mark = ","))

  # interactive choropleth
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 8)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -112.0, lat = 33.45, zoom = 9)
  })

  observe({
    s <- sel()
    labs <- sprintf(
      "<b>Block group</b> %s<br/>Time to nearest: <b>%s</b><br/>%s: <b>%s</b>",
      s$GEOID,
      ifelse(is.na(s$ttnearest), "not within 60 min", paste0(s$ttnearest, " min")),
      demos[[input$demo]]$label,
      demos[[input$demo]]$fmt(s[[input$demo]])
    ) |> lapply(htmltools::HTML)

    leafletProxy("map", data = s) |>
      clearShapes() |>
      clearControls() |>
      addPolygons(
        fillColor = ~map_pal(ttnearest),
        fillOpacity = 0.8, weight = 0.4, color = "white",
        highlightOptions = highlightOptions(weight = 2, color = "#333",
                                             bringToFront = TRUE),
        label = labs
      ) |>
      addLegend("bottomright", pal = map_pal, values = pal_bins,
                title = "Min to nearest", opacity = 0.9,
                na.label = "> 60 min")
  })

  # equity chart: demographic distribution across travel-time bins
  output$equity <- renderPlot({
    s <- st_drop_geometry(sel())
    dm <- input$demo
    ggplot(s, aes(x = tt_lab, y = .data[[dm]], fill = tt_lab)) +
      geom_boxplot(outlier.size = 0.6, width = 0.65, alpha = 0.9) +
      scale_fill_manual(
        values = setNames(
          c(colorRampPalette(c("#ffffb2", "#bd0026"))(6), "#cccccc"),
          tt_levels),
        guide = "none", drop = FALSE) +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(labels = demos[[dm]]$fmt) +
      labs(x = "Time to nearest amenity (min)", y = demos[[dm]]$label) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.x = element_blank())
  })
}

shinyApp(ui, server)
