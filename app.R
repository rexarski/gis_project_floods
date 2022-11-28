
# setup ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(sf)
library(tmap)
library(tidyverse)
library(gt)
library(gtExtras)


# load data and source script -----------------------------------------

tmap_mode("view")

rasters <- terra::rast("data/processed/rasters.tif")
hwm <- read_csv("data/processed/hwm.csv")

rasters$perm_water <-
  rasters$perm_water %>%
  terra::classify(
    cbind(0, NA)
  )

rasters$flooded <-
  rasters$flooded %>%
  terra::classify(
    cbind(0, NA)
  )

rasters$duration <-
  rasters$duration %>%
  terra::classify(
    cbind(0, NA)
  )

source("script/custom-ggplot-theme-pulp-fiction.R")
source("script/custom-shinydashboard-theme.R")

tmap_options(
  basemaps = c(
    "CartoDB.DarkMatter",
    "Stamen.Toner"
  )
)

set.seed(2022)

samples <-
  rasters$pop_density %>%
  terra::spatSample(
    size = 1000,
    method = "regular",
    as.point = TRUE
  ) %>%
  st_as_sf() %>%
  drop_na()

samples <-
  samples %>%
  mutate(
    pop_density = round(pop_density),
    dist_to_hwm_km = samples %>%
      st_distance(
        # unionized hwm
        hwm %>%
          st_as_sf(
            coords = c(
              "longitude",
              "latitude"
            ),
            crs = 4326
          ) %>%
          st_union()
      ) %>%
      units::set_units("km") %>%
      as.double()
  )

# ui ------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "ANLY-585 GIS"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "High-water mark",
        tabName = "high-water-mark",
        icon = icon("ruler")
      ),
      menuItem(
        "Duration",
        tabName = "interactive-map-1",
        icon = icon("clock")
      ),
      menuItem(
        "Population density",
        tabName = "interactive-map-2",
        icon = icon("user")
      ),
      menuItem(
        "Elevation",
        tabName = "interactive-map-3",
        icon = icon("mountain")
      ),
      menuItem(
        "Precipitation",
        tabName = "interactive-map-4",
        icon = icon("tint")
      ),
      menuItem(
        "Summary",
        tabName = "summary",
        icon = icon("smile")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "style.css"
      )
    ),

    # Use custom theme loaded from script

    use_theme(fresh_shiny_theme),
    tabItems(


      ## 1st tab ----------------------------------------------------------
      tabItem(
        tabName = "high-water-mark",
        tags$br(),
        fluidRow(
          box(
            width = 12,
            includeMarkdown("text/tab1.md")
          )
        ),
        fluidRow(
          column(
            width = 6,
            box(tmapOutput("fig1"),
              width = "100%",
              height = 6
            )
          ),
          column(
            width = 6,
            box(gt_output("fig2"),
              width = "100%"
            )
          )
        )
      ),

      ## 2nd tab ----------------------------------------------------------

      ### 2-1 map ---------------------------------------------------------


      tabItem(
        tabName = "interactive-map-1",
        tags$br(),
        h2("High-water mark vs duration"),
        fluidRow(
          column(
            width = 3,
            includeMarkdown("text/tab2-map-1.md")
          ),
          column(
            width = 9,
            box(tmapOutput("fig3"), width = "100%"),
            box(plotOutput("fig4"), width = "100%")
          )
        )
      ),


      ### 2-2 map ---------------------------------------------------------

      tabItem(
        tabName = "interactive-map-2",
        tags$br(),
        h2("High-water mark vs population density"),
        fluidRow(
          column(
            width = 3,
            includeMarkdown("text/tab2-map-2.md")
          ),
          column(
            width = 9,
            box(tmapOutput("fig5"), width = "100%"),
            box(plotOutput("fig6"), width = "100%")
          )
        )
      ),


      ### 2-3map ---------------------------------------------------------

      tabItem(
        tabName = "interactive-map-3",
        tags$br(),
        h2("Height vs elevation"),
        fluidRow(
          column(
            width = 3,
            includeMarkdown("text/tab2-map-3.md")
          ),
          column(
            width = 9,
            box(tmapOutput("fig7"), width = "100%"),
            box(plotOutput("fig8"), width = "100%")
          )
        )
      ),


      ### 2-4 map --------------------------------------------------------

      tabItem(
        tabName = "interactive-map-4",
        tags$br(),
        h2("Height vs precipitation"),
        fluidRow(
          column(
            width = 3,
            includeMarkdown("text/tab2-map-4.md")
          ),
          column(
            width = 9,
            box(tmapOutput("fig9"), width = "100%"),
            box(plotOutput("fig10"), width = "100%")
          )
        )
      ),


      ## 3rd tab ---------------------------------------------------------

      tabItem(
        tabName = "summary",
        tags$br(),
        h2("Summary"),
        fluidRow(
          column(
            width = 12,
            includeMarkdown("text/tab3.md")
          )
        )
      )
    )
  )
)


# server --------------------------------------------------------------

server <- function(input, output) {
  outputOptions(
    output,
    suspendWhenHidden = FALSE
  )

  ## reactive -----------------------------------------------------------

  hwm_reactive <- reactive({
    hwm
  })

  hwm_sfc <- reactive({
    hwm_reactive() %>%
      st_as_sf(
        coords = c("longitude", "latitude"),
        crs = 4326
      )
  })

  hwm_mean_duration_5km <- reactive({
    hwm_sfc() %>%
      mutate(
        mean_duration_5km =
          hwm %>%
            st_as_sf(
              coords = c("longitude", "latitude"),
              crs = 4326
            ) %>%
            st_buffer(5000) %>%
            terra::vect() %>%
            terra::extract(
              rasters$duration %>%
                terra::classify(
                  cbind(NA, 0)
                ),
              .,
              mean,
              na.rm = TRUE
            ) %>%
            pull() %>%
            round(2)
      )
  })

  rasters_reactive <- reactive({
    rasters
  })

  rasters_flooded <- reactive({
    rasters_reactive()$flooded
  })

  rasters_duration <- reactive({
    rasters_reactive()$duration
  })

  rasters_elevation <- reactive({
    rasters_reactive()$elevation
  })

  rasters_hillshade <- reactive({
    rasters_reactive()$hillshade
  })

  rasters_pdensity <- reactive({
    rasters_reactive()$pop_density
  })

  rasters_precip <- reactive({
    rasters_reactive()$percent_of_normal_precip
  })


  ## fig1-2 ----------------------------------------------------------

  output$fig1 <- renderTmap({
    tm_shape(
      rasters_flooded(),
      name = "Flooded area"
    ) +
      tm_raster(
        style = "cat",
        alpha = 1,
        palette = "Blues",
        legend.show = FALSE
      ) +
      tm_shape(
        hwm_sfc(),
        name = "High-water marks"
      ) +
      tm_dots(
        title = "High-water mark type",
        col = "hwm_environment",
        size = 0.05,
        border.alpha = 0,
        palette = met.brewer("Hokusai2",
          n = 2
        ),
        style = "cat",
        popup.vars = c(
          "ID" = "hwm_id",
          "State" = "stateName",
          "County" = "countyName",
          "Heigth above ground (ft)" = "height_above_gnd",
          "Elevation (ft)" = "elev_ft",
          "Type" = "hwm_environment"
        )
      ) +
      tm_layout(
        title =
          "High-water mark locations<br>after Hurricane Florence (2018)"
      )
  })

  output$fig2 <- render_gt({
    hwm_reactive() %>%
      slice_max(
        n = 15,
        elev_ft
      ) %>%
      gt() %>%
      cols_label(
        hwm_id = "ID",
        stateName = "State",
        countyName = "County",
        height_above_gnd = "Height above ground (ft)",
        elev_ft = "Elevation (ft)",
        hwm_environment = "Environment",
        latitude = "Latitude",
        longitude = "Longitude"
      ) %>%
      tab_header(
        title = md("**Top 15 High-water marks after Hurricane Florence (2018)**"),
        subtitle = md("in North Carolina and South Carolina")
      ) %>%
      tab_footnote(
        footnote = md(
          "Data source: **[USGS](https://stn.wim.usgs.gov/FEV/#2018Florence). [Download](data/processed/hwm.csv).**"
        ),
        placement = "right"
      ) %>%
      tab_style(
        locations = cells_column_labels(
          columns = everything()
        ),
        style = list(
          cell_borders(
            sides = "bottom",
            weight = px(3)
          ),
          cell_text(
            weight = "bold",
            size = 24
          )
        )
      ) %>%
      data_color(
        columns = elev_ft,
        colors = scales::col_numeric(
          c(
            "#FEF0D9",
            "#990000"
          ),
          domain = c(
            min(hwm$elev_ft),
            max(hwm$elev_ft)
          ),
          alpha = 0.75
        )
      ) %>%
      data_color(
        columns = height_above_gnd,
        colors = scales::col_numeric(
          c(
            "#fde7b9",
            "#f8b425"
          ),
          domain = c(
            min(hwm$height_above_gnd),
            max(hwm$height_above_gnd)
          ),
          alpha = 0.75
        )
      ) %>%
      data_color(
        columns = hwm_environment,
        colors = scales::col_factor(
          c(
            "#6fafb7",
            "#2b70a4"
          ),
          domain = c(
            "Coastal",
            "Riverine"
          )
        ),
        alpha = 0.75
      ) %>%
      opt_all_caps() %>%
      opt_table_font(
        font = list(
          google_font("Chivo"),
          default_fonts()
        )
      ) %>%
      tab_options(
        column_labels.border.top.width = px(3),
        column_labels.border.top.color = "transparent",
        table.border.top.color = "transparent",
        table.border.bottom.color = "transparent",
        data_row.padding = px(3),
        source_notes.font.size = 12,
        heading.align = "left"
      )
  })


  ## fig3-4 ----------------------------------------------------------

  output$fig3 <- renderTmap({
    tm_shape(
      rasters_duration(),
      name = "Flooded duration"
    ) +
      tm_raster(
        title = "Flooded duration (days)",
        style = "pretty",
        alpha = 0.8,
        palette = "Reds",
        legend.show = TRUE
      ) +
      tm_shape(
        hwm_mean_duration_5km(),
        name = "High-water marks"
      ) +
      tm_dots(
        title = "Avg flooded duration<br>within 5km (days)",
        size = 0.05,
        border.alpha = 0,
        col = "mean_duration_5km",
        pal = "plasma",
        style = "cont",
        popup.vars = c(
          "ID" = "hwm_id",
          "State" = "stateName",
          "County" = "countyName",
          "Heigth above ground (ft)" = "height_above_gnd",
          "Elevation (ft)" = "elev_ft",
          "Type" = "hwm_environment",
          "Average flooded duration (days) within 5km radius" =
            "mean_duration_5km"
        )
      ) +
      tm_layout(
        title =
          glue(
            "High-water mark vs flooded durations<br>",
            "after Hurricane Florence (2018)"
          )
      )
  })

  output$fig4 <- renderPlot({
    hwm_mean_duration_5km() %>%
      ggplot(aes(
        x = mean_duration_5km,
        fill = hwm_environment,
        y = sqrt(..count..)
      )) +
      geom_histogram(
        bins = 20,
        color = "#e9ecef",
        alpha = 0.8,
        position = "identity"
      ) +
      scale_fill_manual(
        values =
          met.brewer("Austria",
            n = 2
          )
      ) +
      scale_x_continuous(breaks = 0:5) +
      scale_y_sqrt() +
      theme_pulp_fiction() +
      labs(
        title = "How flooded it is around a high-water mark?",
        subtitle = glue(
          "y-axis is scaled by taking square root <br>",
          "as the distribution is inflated with zeros"
        ),
        caption = "Data source: USGS",
        x = "Avg flooded duration within 5km (days)",
        y = "Sqrt of high-water mark count",
        fill = "High-water mark type"
      )
  })


  ## fig5-6 ----------------------------------------------------------

  output$fig5 <- renderTmap({
    tm_shape(rasters_pdensity(),
      name = "Population density"
    ) +
      tm_raster(
        title = "Population density (ppl/km^2)",
        style = "kmeans",
        alpha = 0.6,
        palette = "Greens"
      ) +
      tm_shape(
        hwm_sfc(),
        name = "High-water mark"
      ) +
      tm_dots(
        border.alpha = 0,
        col = "#f8b425",
        size = 0.05,
        popup.vars = c(
          "ID" = "hwm_id",
          "State" = "stateName",
          "County" = "countyName",
          "Heigth above ground (ft)" = "height_above_gnd",
          "Elevation (ft)" = "elev_ft",
          "Type" = "hwm_environment"
        ),
        alpha = 0.5
      ) +
      tm_layout(
        title = glue(
          "High-water mark vs population density<br>",
          "after Hurricane Florence (2018)"
        )
      )
  })

  output$fig6 <- renderPlot({
    samples %>%
      as_tibble() %>%
      ggplot(aes(
        x = dist_to_hwm_km,
        y = sqrt(pop_density)
      )) +
      geom_point(
        alpha = 0.5,
        size = 2,
        color = "#cd2327"
      ) +
      geom_smooth(
        method = lm,
        size = 2
      ) +
      theme_pulp_fiction() +
      labs(
        title = "More high-water marks in urban areas than rural areas?",
        subtitle = glue(
          "Population density of randomly sampled 1,000 ",
          "geospatial points vs<br>distances to",
          "their closest high-water marks"
        ),
        caption = "Data source: USGS",
        x = "Distance to closest high-water mark (km)",
        y = "Sqrt of population denstiy (ppl/km^2)"
      )
  })

  ## fig7-8 ----------------------------------------------------------

  output$fig7 <- renderTmap({
    tm_shape(
      rasters_hillshade(),
      name = "Hillshade"
    ) +
      tm_raster(
        pal = gray.colors(
          n = 10,
          start = 0,
          end = 1
        ),
        style = "cont",
        alpha = 0.9,
        legend.show = FALSE
      ) +
      tm_shape(
        rasters_elevation(),
        name = "Elevation"
      ) +
      tm_raster(
        title = "Elevation (m)",
        style = "cont",
        palette = rev(met.brewer(
          "Hiroshige",
          n = 100
        )),
        alpha = 0.6,
        midpoint = NA
      ) +
      tm_shape(
        name = "Height above ground",
        hwm_sfc() %>%
          mutate(
            sqrt_elev_m = sqrt(
              elev_ft * 0.3048
            )
          )
      ) +
      tm_dots(
        title = "Height above ground (ft)",
        col = "height_above_gnd",
        palette = met.brewer(
          "OKeeffe2",
          n = 6
        ),
        border.alpha = 0,
        style = "pretty",
        size = "sqrt_elev_m",
        popup.vars = c(
          "ID" = "hwm_id",
          "State" = "stateName",
          "County" = "countyName",
          "Heigth above ground (ft)" = "height_above_gnd",
          "Elevation (ft)" = "elev_ft",
          "Type" = "hwm_environment"
        ),
        alpha = 0.3
      ) +
      tm_layout(
        title =
          "Height above ground vs elevation"
      )
  })

  output$fig8 <- renderPlot({
    hwm_sfc() %>%
      mutate(complete = "All") %>%
      ggplot(aes(
        x = elev_ft,
        y = height_above_gnd,
        color = hwm_environment
      )) +
      geom_point(
        alpha = 0.5,
        size = 2
      ) +
      geom_smooth(
        se = FALSE,
        method = lm,
        size = 1.5
      ) +
      geom_smooth(
        se = FALSE,
        method = lm,
        size = 1.5,
        aes(color = complete)
      ) +
      scale_color_manual(
        values =
          met.brewer(
            "Austria",
            n = 3
          )
      ) +
      theme_pulp_fiction() +
      labs(
        title = glue("Heights vs elevations of high-water marks"),
        subtitle = glue(
          "The green line indicates the regression line <br>",
          "of all data points"
        ),
        caption = "Data source: USGS",
        x = "Elevation (ft)",
        y = "Height above ground (ft)",
        color = "High-water mark type"
      )
  })


  ## fig9-10 ----------------------------------------------------------

  output$fig9 <- renderTmap({
    tm_shape(
      rasters_precip(),
      name = "Precipitation"
    ) +
      tm_raster(
        title = "Normal precipitation (%)",
        style = "pretty",
        alpha = 0.8,
        palette = "Blues",
        legend.show = TRUE
      ) +
      tm_shape(
        hwm_mean_duration_5km(),
        name = "High-water marks"
      ) +
      tm_dots(
        title = "Heigh above ground (ft)",
        size = 0.05,
        border.alpha = 0,
        col = "height_above_gnd",
        palette = met.brewer(
          "OKeeffe2",
          n = 6
        ),
        style = "pretty",
        popup.vars = c(
          "ID" = "hwm_id",
          "State" = "stateName",
          "County" = "countyName",
          "Heigth above ground (ft)" = "height_above_gnd",
          "Elevation (ft)" = "elev_ft",
          "Type" = "hwm_environment"
        )
      ) +
      tm_layout(
        title =
          glue("High-water mark height vs relative precipitation")
      )
  })

  output$fig10 <- renderPlot({
    hwm_sfc() %>%
      mutate(
        precip = hwm_sfc() %>%
          terra::vect() %>%
          terra::extract(
            rasters_precip() %>%
              terra::classify(
                cbind(NA, 0)
              ),
            .
          ) %>%
          pull(percent_of_normal_precip),
        precip = precip / 100
      ) %>%
      ggplot(aes(
        x = precip,
        y = height_above_gnd
      )) +
      geom_point(
        alpha = 0.6,
        color = met.brewer(
          "Austria",
          n = 1
        )
      ) +
      geom_smooth(method = lm) +
      scale_x_continuous(labels = scales::percent) +
      theme_pulp_fiction() +
      labs(
        title = glue("Heights vs elevations of high-water marks"),
        caption = "Data source: USGS",
        x = "Normal precipitation (%) ",
        y = "Height above ground (ft)"
      )
  })
}

shinyApp(ui, server)
