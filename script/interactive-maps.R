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

# 01 [tmap] hwm  -------------------------------------------------------

fig1 <-
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



# 02 [gt] hwm  ---------------------------------------------------------

fig2 <-
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
      'Data source: **[USGS](https://stn.wim.usgs.gov/FEV/#2018Florence). [Download](data/processed/hwm.csv).**'),
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
      c('#fde7b9',
        '#f8b425'),
      domain = c(
        min(hwm$height_above_gnd),
        max(hwm$height_above_gnd)),
      alpha = 0.75)) %>%
  data_color(
    columns = hwm_environment,
    colors = scales::col_factor(
      c('#6fafb7',
        '#2b70a4'),
      domain = c(
        'Coastal',
        'Riverine')),
    alpha = 0.75) %>%
  opt_all_caps() %>%
  opt_table_font(
    font = list(
      google_font('Chivo'),
      default_fonts())) %>%
  tab_options(
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = 'transparent',
    table.border.top.color = 'transparent',
    table.border.bottom.color = 'transparent',
    data_row.padding = px(3),
    source_notes.font.size = 12,
    heading.align = 'left')


# 03 [tmap] hwm vs duration --------------------------------------------

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

fig3 <-
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


# 04 [ggplot2] hwm vs duration ----------------------------------------

fig4 <- 
  hwm_mean_duration_5km %>%
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


# 05 [tmap] hwm vs pop-density ----------------------------------------

# Randomly sample 500 points within the area of interest
# and find its pop_density 

set.seed(2022)

samples <- 
  rasters$pop_density %>% 
  terra::spatSample(
    size = 1000,
    method= 'regular', 
    as.point = TRUE) %>% 
  st_as_sf() %>%
  drop_na()

samples <-
  samples %>%
  mutate(pop_density = round(pop_density),
         dist_to_hwm_km = samples %>%
           st_distance(
             # unionized hwm
             hwm %>%
               st_as_sf(
                 coords = c('longitude', 'latitude'),
                 crs = 4326) %>%
               st_union(),
           ) %>%
           units::set_units('km') %>%
           as.double())

fig5 <- 
  tm_shape(rasters$pop_density,
           name = 'Population density') +
  tm_raster(
    title = 'Population density (ppl/km^2)',
    style = 'kmeans',
    alpha = 0.6,
    palette = 'Greens') +
  tm_shape(
    hwm %>%
      st_as_sf(
        coords = c('longitude',
                   'latitude'),
        crs = 4326),
    name = 'High-water mark') +
  tm_dots(
    border.alpha = 0,
    col = '#f8b425',
    size = 0.05,
    popup.vars = c(
      'ID' = 'hwm_id',
      'State' = 'stateName',
      'County' = 'countyName',
      'Heigth above ground (ft)' = 'height_above_gnd',
      'Elevation (ft)' = 'elev_ft',
      'Type' = 'hwm_environment'),
    alpha = 0.5) +
  tm_layout(
    title = glue(
      'High-water mark vs population density<br>',
      'after Hurricane Florence (2018)'))


# 06 [ggplot2] hwm vs pop-density -------------------------------------

fig6 <-
  samples %>%
  as_tibble() %>%
  ggplot(aes(x = dist_to_hwm_km,
             y = sqrt(pop_density))) +
  geom_point(
    alpha = 0.5,
    size = 2,
    color = '#cd2327') +
  geom_smooth(
    method = lm,
    size = 2) +
  theme_pulp_fiction() +
  labs(
    title = 'More high-water marks in urban areas than rural areas?',
    subtitle = glue('Population density of randomly sampled 1,000 ',
                    'geospatial points vs<br>distances to',
                    'their closest high-water marks'),
    caption = 'Data source: USGS',
    x = 'Distance to closest high-water mark (km)',
    y = 'Sqrt of population denstiy (ppl/km^2)')

# 07 [tmap] height_above_gnd vs elev_ft -------------------------------

fig7 <-
  tm_shape(
    rasters$hillshade,
    name = 'Hillshade') +
  tm_raster(
    pal = gray.colors(
      n = 10, 
      start = 0, 
      end = 1),
    style = 'cont',
    alpha = 0.9,
    legend.show = FALSE) +
  tm_shape(
    rasters$elevation,
    name = 'Elevation') +
  tm_raster(
    title = 'Elevation (m)',
    style = 'cont',
    palette = rev(met.brewer(
      'Hiroshige',
      n=100)),
    alpha = 0.6,
    midpoint = NA) +
  tm_shape(
    name = 'Height above ground',
    hwm %>%
      st_as_sf(
        coords = c('longitude',
                   'latitude'),
        crs = 4326) %>%
      mutate(
        sqrt_elev_m = sqrt(
          elev_ft * 0.3048))) +
  tm_dots(
    title = 'Height above ground (ft)',
    col = 'height_above_gnd',
    palette = met.brewer(
      'OKeeffe2',
      n = 6),
    border.alpha = 0,
    style = 'pretty',
    size = 'sqrt_elev_m',
    popup.vars = c(
      'ID' = 'hwm_id',
      'State' = 'stateName',
      'County' = 'countyName',
      'Heigth above ground (ft)' = 'height_above_gnd',
      'Elevation (ft)' = 'elev_ft',
      'Type' = 'hwm_environment'),
    alpha = 0.3) +
  tm_layout(
    title = 
      'Height above ground vs elevation')

# 08 [ggplot2] height_above_gnd vs elev_ft ----------------------------

fig8 <-
  hwm %>%
  mutate(complete = 'All') %>%
  ggplot(aes(
    x = elev_ft,
    y = height_above_gnd,
    color = hwm_environment)) +
  geom_point(
    alpha = 0.5,
    size = 2) +
  geom_smooth(
    se = FALSE,
    method = lm,
    size = 1.5) +
  geom_smooth(
    se = FALSE,
    method = lm,
    size = 1.5,
    aes(color = complete)) +
  scale_color_manual(values = 
                       met.brewer(
                         'Austria',
                         n = 3)) +
  theme_pulp_fiction() +
  labs(
    title = glue('Heights vs elevations of high-water marks'),
    subtitle = glue('The green line indicates the regression line <br>',
                    'of all data points'),
    caption = 'Data source: USGS',
    x = 'Elevation (ft)',
    y = 'Height above ground (ft)',
    color = 'High-water mark type')


# 09 [tmap] height_above_gnd vs precip abnormality --------------------

fig9 <- 
  tm_shape(
    rasters$percent_of_normal_precip,
    name = 'Precipitation') +
  tm_raster(
    title = 'Normal precipitation (%)',
    style = 'pretty',
    alpha = 0.8,
    palette = 'Blues',
    legend.show = TRUE) +
  tm_shape(
    hwm_mean_duration_5km,
    name = 'High-water marks') +
  tm_dots(
    title = 'Heigh above ground (ft)',
    size = 0.05,
    border.alpha = 0,
    col = 'height_above_gnd',
    palette = met.brewer(
      'OKeeffe2',
      n = 6),
    style = 'pretty',
    popup.vars = c(
      'ID' = 'hwm_id',
      'State' = 'stateName',
      'County' = 'countyName',
      'Heigth above ground (ft)' = 'height_above_gnd',
      'Elevation (ft)' = 'elev_ft',
      'Type' = 'hwm_environment')) +
  tm_layout(
    title =
      glue('High-water mark height vs relative precipitation'))


# 10 [ggplot2] height_above_gnd vs precip abnormality -----------------

fig10 <-
  hwm %>%
  mutate(
    precip = hwm %>%
      st_as_sf(
        coords = c('longitude', 'latitude'),
        crs = 4326) %>%
      terra::vect() %>%
      terra::extract(
        rasters$percent_of_normal_precip %>%
          terra::classify(
            cbind(NA, 0)),
        .) %>%
      pull(percent_of_normal_precip),
    precip = precip / 100) %>%
  ggplot(aes(
    x = precip,
    y = height_above_gnd)) +
  geom_point(alpha = 0.6,
             color = met.brewer(
               'Austria',
               n = 1)) +
  geom_smooth(method = lm) +
  scale_x_continuous(labels = scales::percent) +
  theme_pulp_fiction() +
  labs(
    title = glue('Heights vs elevations of high-water marks'),
    caption = 'Data source: USGS',
    x = 'Normal precipitation (%) ',
    y = 'Height above ground (ft)')
