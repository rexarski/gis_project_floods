library(sf)
library(lubridate)
library(tmap)
library(tidyverse)
library(gt)

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
               'Stamen.Toner',
               'Esri.WorldImagery',
               'Esri.WorldTopoMap'))

# interactive-map: trial ----------------------------------------------

tm_shape(rasters$flooded) +
  tm_raster(
    style = 'cat',
    alpha = 1,
    palette = 'Blues'
  ) +
  tm_shape(rasters$perm_water) +
  tm_raster(
    # style = 'cat',
    # alpha = 1,
    palette = 'Greens') +
  tm_shape(rasters$pop_density) +
  tm_raster(
    palette = 'Reds',
    alpha = 0.5,) +
  tm_shape(
    hwm %>%
      st_as_sf(
        coords = c('longitude',
                   'latitude'),
        crs = 4326)) +
  tm_dots(
    col = 'hwm_environment',
    # palette = c('Spectral'),
    style = 'cat',
    # size = 'height_above_gnd',
    popup.vars = TRUE,
    alpha = 0.5,
    clustering = TRUE)

# interactive-map: high-water marks -----------------------------------

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
    palette = c('aquamarine',
                'darkolivegreen3'),
    style = 'cat',
    popup.vars = c(
      'ID' = 'hwm_id',
      'State' = 'stateName',
      'County' = 'countyName',
      'Heigth above ground (ft)' = 'height_above_gnd',
      'Elevation (ft)' = 'elev_ft',
      'Type' = 'hwm_environment')) +
  tm_minimap() +
  tm_layout(
    title = 
      'High-water mark locations<br>after Hurricane Florence (2018)')


# interactive table: high-water marks ---------------------------------

hwm %>%
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
    title = md('**High-water marks after Hurricane Florence (2018)**'),
    subtitle = md('in North Carolina and South Carolina')) %>%
  tab_footnote(
    footnote = md(
      'Data source: **[USGS](https://stn.wim.usgs.gov/FEV/#2018Florence)**'),
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

# interactive-map: hwm vs duration ------------------------------------

hwm_mean_duration_5km <-
  hwm %>%
  st_as_sf(
    coords = c('longitude', 'latitude'),
    crs = 4326) %>%
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
    hwm_mean_duration_5km,
    name = 'High-water marks') +
  tm_dots(
    title = 'Avg flooded duration<br>within 5km (days)',
    size = 0.05,
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
  tm_minimap() +
  tm_layout(
    title = 
      'High-water mark vs flooded durations<br>after Hurricane Florence (2018)')


hwm_mean_duration_5km %>%
  ggplot(aes(x = mean_duration_5km,
             fill = hwm_environment)) +
  geom_histogram(
    bins = 50,
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
    y = 'High-water mark count',
    fill = 'High-water mark type')


# interactive-map: hwm vs pop-density ---------------------------------

# density rescale


# interactive-map: hwm vs perm-water ----------------------------------


# interactive-map: height_above_gnd vs elev_ft ------------------------

# add topo


# interactive-map: height_above_gnd vs precip abnormality -------------



