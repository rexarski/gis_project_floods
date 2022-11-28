
# setup ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(sf)
library(tmap)
library(tidyverse)
library(gt)


# load data and source script -----------------------------------------

tmap_mode('view')

rasters <- terra::rast('data/processed/rasters.tif')
hwm <- read_csv('data/processed/hwm.csv')

rasters$perm_water <-
  rasters$perm_water %>%
  terra::classify(
    cbind(0, NA))

rasters$flooded <-
  rasters$flooded %>%
  terra::classify(
    cbind(0, NA))

rasters$duration <-
  rasters$duration %>%
  terra::classify(
    cbind(0, NA))

source('script/custom-ggplot-theme-pulp-fiction.R')

tmap_options(
  basemaps = c('CartoDB.DarkMatter',
               'Stamen.Toner'))


# ui ------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = 'Project Floods'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('High-water mark', tabName = 'high-water-mark', icon = icon('ruler')),
      menuItem('Duration', tabName = 'interactive-map-1', icon = icon('clock')),
      menuItem('Population density', tabName = 'interactive-map-2', icon = icon('user')),
      menuItem('Elevation', tabName = 'interactive-map-3', icon = icon('mountain')),
      menuItem('Precipitation', tabName = 'interactive-map-4', icon = icon('tint')),
      menuItem('Summary', tabName = 'summary', icon = icon('smile'))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css')
    ),
    tabItems(
      
      
      ## 1st tab ------------------------------------------------------------
      
      tabItem(tabName = 'high-water-mark',
              tags$br(),
              fluidRow(
                column(
                  width = 3,
                  includeMarkdown('text/tab1.md')
                ),
                column(
                  width = 9,
                  box(
                    tmapOutput('fig1'),
                    width = '100%'),
                  box(
                    gt_output('fig2'),
                    width = '100%')
                ))
      ),
      
      ## 2nd tab ------------------------------------------------------------
      
      ### 2-1 map -----------------------------------------------------------
      
      
      tabItem(tabName = 'interactive-map-1',
              tags$br(),
              h2('High-water mark vs duration'),
              fluidRow(
                column(
                  width = 3,
                  includeMarkdown('text/tab2-map-1.md')
                ),
                column(
                  width = 9,
                  box(tmapOutput('fig3'), width = '100%'),
                  box(plotOutput('fig4'), width = '100%')
                )
              )
      ),
      
      
      ### 2-2 map -----------------------------------------------------------
      
      tabItem(tabName = 'interactive-map-2',
              tags$br(),
              h2('High-water mark vs population density'),
              fluidRow(
                column(
                  width = 3,
                  includeMarkdown('text/tab2-map-2.md')
                ),
                column(
                  width = 9,
                  box(tmapOutput('fig5'), width = '100%'),
                  box(plotOutput('fig6'), width = '100%')
                )
              )
      ),
      
      
      ### 2-3map -----------------------------------------------------------
      
      tabItem(tabName = 'interactive-map-3',
              tags$br(),
              h2('Height vs elevation'),
              fluidRow(
                column(
                  width = 3,
                  includeMarkdown('text/tab2-map-3.md')
                ),
                column(
                  width = 9,
                  box(tmapOutput('fig7'), width = '100%'),
                  box(plotOutput('fig8'), width = '100%')
                )
              )
      ),
      
      
      ### 2-4 map ----------------------------------------------------------
      
      tabItem(tabName = 'interactive-map-4',
              tags$br(),
              h2('Height vs precipitation'),
              fluidRow(
                column(
                  width = 3,
                  includeMarkdown('text/tab2-map-4.md')
                ),
                column(
                  width = 9,
                  box(tmapOutput('fig9'), width = '100%'),
                  box(plotOutput('fig10'), width = '100%')
                )
              )
      ),
      
      
      ## 3rd tab -----------------------------------------------------------
      
      tabItem(tabName = 'summary',
              tags$br(),
              h2('Summary'),
              fluidRow(
                column(
                  width = 12,
                  includeMarkdown('text/tab3.md')
                )
              )
      )
    )
  )
)


# server --------------------------------------------------------------

server <- function(input, output) {
  
  
  ## reactive -----------------------------------------------------------
  
  hwm_reactive <- reactive({
    hwm %>%
      st_as_sf(
        coords = c('longitude', 'latitude'),
        crs = 4326)
  })
  
  hwm_mean_duration_5km <- reactive({
    hwm_reactive() %>%
      mutate(
        mean_duration_5km =
          hwm %>%
          st_as_sf(
            coords = c('longitude', 'latitude'),
            crs = 4326) %>%
          st_buffer(5000) %>%
          terra::vect() %>%
          terra::extract(
            rasters$duration %>%
              terra::classify(
                cbind(NA, 0)),
            .,
            mean,
            na.rm = TRUE) %>%
          pull() %>%
          round(2))
  })
  
  
  ## fig1-2 ----------------------------------------------------------
  
  output$fig1 <- renderTmap({
    tm_shape(
      rasters$flooded,
      name = 'Flooded area') +
      tm_raster(
        style = 'cat',
        alpha = 1,
        palette = 'Blues',
        legend.show = FALSE) +
      tm_shape(
        hwm %>%
          st_as_sf(
            coords = c('longitude',
                       'latitude'),
            crs = 4326),
        name = 'High-water marks') +
      tm_dots(
        title = 'High-water mark type',
        col = 'hwm_environment',
        size = 0.05,
        border.alpha = 0,
        palette = met.brewer('Hokusai2',
                             n = 2),
        style = 'cat',
        popup.vars = c(
          'ID' = 'hwm_id',
          'State' = 'stateName',
          'County' = 'countyName',
          'Heigth above ground (ft)' = 'height_above_gnd',
          'Elevation (ft)' = 'elev_ft',
          'Type' = 'hwm_environment')) +
      tm_layout(
        title =
          'High-water mark locations<br>after Hurricane Florence (2018)')
  })
  
  output$fig2 <- render_gt({
    hwm %>%
      slice_max(n = 15, elev_ft) %>%
      gt() %>%
      cols_label(
        hwm_id = 'ID',
        stateName = 'State',
        countyName = 'County',
        height_above_gnd = 'Height above ground (ft)',
        elev_ft = 'Elevation (ft)',
        hwm_environment = 'Environment',
        latitude = 'Latitude',
        longitude = 'Longitude') %>%
      tab_header(
        title = md('**Top 15 High-water marks after Hurricane Florence (2018)**'),
        subtitle = md('in North Carolina and South Carolina')) %>%
      tab_footnote(
        footnote = md(
          'Data source: **[USGS](https://stn.wim.usgs.gov/FEV/#2018Florence)**. [Download](data/processed/hwm.csv)'),
        placement = 'right') %>%
      tab_style(
        locations = cells_column_labels(
          columns = everything()),
        style = list(
          cell_borders(
            sides = 'bottom',
            weight = px(3)),
          cell_text(
            weight = 'bold',
            size = 24))) %>%
      data_color(
        columns = elev_ft,
        colors = scales::col_numeric(
          c('#FEF0D9',
            '#990000'),
          domain = c(
            min(hwm$elev_ft),
            max(hwm$elev_ft)),
          alpha = 0.75)) %>%
      data_color(
        columns = height_above_gnd,
        colors = scales::col_numeric(
          c('#2fb7c4',
            '#1b4789'),
          domain = c(
            min(hwm$height_above_gnd),
            max(hwm$height_above_gnd)),
          alpha = 0.75)) %>%
      opt_all_caps() %>%
      opt_table_font(
        font = list(
          google_font('Chivo'),
          default_fonts())) %>%
      # cols_width(
      #   c(height_above_gnd) ~ px(100),
      #   c(elev_ft) ~ px(80)) %>% 
      tab_options(
        column_labels.border.top.width = px(3),
        column_labels.border.top.color = 'transparent',
        table.border.top.color = 'transparent',
        table.border.bottom.color = 'transparent',
        data_row.padding = px(3),
        source_notes.font.size = 12,
        heading.align = 'left')
  })
  
  
  ## fig3-4 ----------------------------------------------------------
  
  output$fig3 <- renderTmap({
    tm_shape(
      rasters$duration,
      name = 'Flooded duration') +
      tm_raster(
        title = 'Flooded duration (days)',
        style = 'pretty',
        alpha = 0.8,
        palette = 'Reds',
        legend.show = TRUE) +
      tm_shape(
        hwm_mean_duration_5km(),
        name = 'High-water marks') +
      tm_dots(
        title = 'Avg flooded duration<br>within 5km (days)',
        size = 0.05,
        border.alpha = 0,
        col = 'mean_duration_5km',
        pal = 'plasma',
        style = 'cont',
        popup.vars = c(
          'ID' = 'hwm_id',
          'State' = 'stateName',
          'County' = 'countyName',
          'Heigth above ground (ft)' = 'height_above_gnd',
          'Elevation (ft)' = 'elev_ft',
          'Type' = 'hwm_environment',
          'Average flooded duration (days) within 5km radius' =
            'mean_duration_5km')) +
      tm_layout(
        title =
          glue('High-water mark vs flooded durations<br>',
               'after Hurricane Florence (2018)'))
  })
  
  output$fig4 <- renderPlot({
    hwm_mean_duration_5km() %>%
      ggplot(aes(x = mean_duration_5km,
                 fill = hwm_environment,
                 y = sqrt(..count..))) +
      geom_histogram(
        bins = 20,
        color = '#e9ecef',
        alpha = 0.8,
        position = 'identity') +
      scale_fill_manual(values =
                          met.brewer('Austria',
                                     n = 2)) +
      scale_x_continuous(breaks = 0:5) +
      scale_y_sqrt() +
      theme_pulp_fiction() +
      labs(
        title = 'How flooded it is around a high-water mark?',
        subtitle = glue('y-axis is scaled by taking square root <br>',
                        'as the distribution is inflated with zeros'),
        caption = 'Data source: USGS',
        x = 'Avg flooded duration within 5km (days)',
        y = 'Sqrt of high-water mark count',
        fill = 'High-water mark type')
  })
  
  
  ## fig5-6 ----------------------------------------------------------
  
  
  ## fig7-8 ----------------------------------------------------------
  
  
  ## fig9-10 ----------------------------------------------------------
  
  
}


shinyApp(ui, server)