# 

library(tidyverse)
library(sf)
library(concaveman)

sites <- data.table::fread("sites.csv", encoding = "UTF-8") %>%
  sf::st_as_sf(
    coords = c("LONG", "LAT"),
    crs = 4326, 
    remove = F
  )

# Frequency of sites per pottery group
pottery.sites.freq <- as.data.frame(
  stats::aggregate(
    SITE ~ POTTERY, 
    data = sites,
    FUN = length)) 

# Area per pottery group (Convex hull)
# see https://github.com/joelgombin/concaveman

id <- dplyr::filter(pottery.sites.freq, SITE > 2)
conc.hull.lst <- list()
for(i in 1:nrow(id)){
  sites.f <- dplyr::filter(sites, POTTERY == id[i,1])
  conc.hull <- concaveman::concaveman(sites.f)
  conc.hull$POTTERY = id[i, "POTTERY"]
  conc.hull.lst[[i]] <- conc.hull
}
pottery.area <- do.call(rbind, conc.hull.lst) %>%
  sf::st_make_valid()

ggplot() +
  geom_sf(data = pottery.area)

pottery.area %>%
  sf::st_write(
    dsn = "pottery_distribution.geojson",
    layer = "polygons", 
    delete_dsn = TRUE)
