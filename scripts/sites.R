# generate compact sites list

library(tidyverse)

data.table::fread("sites.csv", encoding = "UTF-8") %>%
  dplyr::distinct(SITE, LAT, LONG) %>%
  sf::st_as_sf(coords = c("LONG", "LAT"), 
               remove = F, 
               crs = 4326, 
               na.fail = F) %>%
  sf::st_write(dsn = "sites.geojson", layer = "sites")