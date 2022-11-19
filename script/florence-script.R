# Note that some data might not be in this repository due to
# the file size limit of GitHub. However, all processed data are stored
# under the data/processed/ directory.

library(sf)
library(lubridate)
library(tmap)
library(tidyverse)

# tmap_mode('view')
tmap_mode('plot')

# decide to steer the wheel a little bit to focus on a flood event
# that is closer to present and more accessible in terms of data
# availability


# Data ----------------------------------------------------------------


# Global Flood Database

# URL: https://global-flood-database.cloudtostreet.ai/#interactive-map

floods <- 
  terra::rast('data/raw/florence.tif')

# flooded area boundary including both NC and SC, used as a 'mask'
# over areas below.
# TODO: how to generate the actual 'outline' of this area?

boundary <- 
  floods$flooded %>%
  terra::boundaries(
    inner = FALSE) %>%
  terra::classify(
    cbind(0, 1))

#############################################

# layerCor: by reclassify all 0 to NA so that they layers share 
# the exact same pixels

floods %>%
  terra::classify(
    cbind(NA, 0)) %>%
  terra::layerCor('pearson')

#############################################

# reclassify two categorical layers (jrc_perm_water and flooded)
# remove the '0' pixels

# floods$jrc_perm_water <-
#   floods$jrc_perm_water %>%
#   terra::classify(
#     cbind(0, NA))
# 
# floods$flooded <-
#   floods$flooded %>%
#   terra::classify(
#     cbind(0, NA))

# Sneak peak of areas

tm_style('classic') +
  tm_shape(floods$duration) +
  tm_raster(
    style ='cont',
    palette = 'Greens')

floods$jrc_perm_water %>%
  tm_shape() +
  tm_raster(
    palette = 'Blues',
    style = 'cat')

floods$clear_views %>%
  tm_shape() +
  tm_raster()

floods$clear_perc %>%
  tm_shape() +
  tm_raster()

floods$flooded %>%
  tm_shape() +
  tm_raster(
    style = 'cat',
    palette = 'Reds')

floods %>%
  terra::writeRaster(
    'data/processed/floods.tif',
    overwrite = TRUE)

# High-water mark

# URL: https://stn.wim.usgs.gov/FEV/#2018Florence

hwm <- read_csv('data/raw/highwatermark.csv') %>%
  select(latitude,
         longitude,
         stateName,
         height_above_gnd,
         elev_ft) %>%
  filter(stateName %in%
           c('NC',
             'SC')) %>%
  drop_na()

hwm %>% write_csv('data/processed/hwm.csv')

tm_shape(boundary) +
  tm_raster() +
  tm_shape(
    hwm %>%
      st_as_sf(
        coords = c('longitude',
                   'latitude'),
        crs = 4326)) +
  tm_dots(
    col = 'elev_ft',
    palette = 'Spectral',
    style = 'cont',
    size = 'height_above_gnd')

# Precipitation

# URL: https://water.weather.gov/precip/download.php

# Although precip has the most coarse resolution, we are more interested
# in the floods raster which convey more information. We are going to
# resample precip to finer resolution.

precip <- terra::rast('data/raw/precip.tif') %>%
  terra::project('epsg:4326') %>%
  terra::resample(floods) %>%
  terra::mask(boundary)

# Band 1 - Observation - Last 24 hours of QPE spanning 12Z to 12Z in inches

precip$precip_1 %>%
  tm_shape() +
  tm_raster()

# Band 2 - PRISM normals - PRISM normals in inches
# (see "Normal Precipitation" section on the About page)

precip$precip_2 %>%
  tm_shape() +
  tm_raster()

# Band 3 - Departure from normal - The departure from normal in inches

precip$precip_3 %>%
  tm_shape() +
  tm_raster(
    midpoint = NA)

# Band 4 - Percent of normal - The percent of normal

precip$precip_4 %>%
  tm_shape() +
  tm_raster()

precip %>%
  terra::writeRaster(
    'data/processed/precip.tif',
    overwrite = TRUE)



# Digital Elevation Model (DEM)

# URL: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/elevation/copernicus-dem/elevation

dem <- 
  terra::rast('data/raw/10_DEM_y30x-90/10_DEM_y30x-90.tif') %>%
  terra::merge(
    terra::rast('data/raw/10_DEM_y30x-80/10_DEM_y30x-80.tif')) %>%
  terra::resample(floods) %>%
  terra::mask(boundary)

dem %>%
  terra::writeRaster(
    'data/processed/dem.tif',
    overwrite = TRUE)

hillshade <- 
  terra::shade(
    slope <-
      terra::terrain(
        dem,
        v = 'slope',
        unit = 'radians'),
    terra::terrain(
      dem,
      v = 'aspect',
      unit = 'radians'),
    angle = 45,
    direction = 135)

hillshade %>%
  terra::writeRaster(
    'data/processed/hillshade.tif',
    overwrite = TRUE)

hillshade %>%
  tm_shape('Hillshade') +
  tm_raster(
    palette = 
      gray.colors(
        n = 7, 
        start = 0, 
        end = 1),
    style = 'cont',
    alpha = 0.9,
    legend.show = FALSE) +
  dem %>% 
  tm_shape(name = 'Elevation') +
  tm_raster(
    title = 'Elevation (m)',
    palette = terrain.colors(n = 10),
    style = 'cont')

