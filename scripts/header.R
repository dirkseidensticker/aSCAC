# SETUP ----
library(dplyr)
library(elevatr)
library(ggplot2)
library(ggthemes)
library(ggsn)
library(gridExtra)
library(ggrepel)
library(oxcAAR)
quickSetupOxcal()
library(cowplot)
library(dplyr)
library(osmdata)
library(osmextract)
library(osmplotr)
library(raster)
library(reshape2)
library(rnaturalearth)
library(tidyr)
library(sf)
library(svglite)
library(geojsonsf)
library(xlsx)

# rcarbon ####
library(rcarbon)
library(parallel)
ncores = (detectCores() / 2)

# radiocarbon database
c14 <- data.table::fread(
    "https://raw.githubusercontent.com/dirkseidensticker/aDRAC/master/aDRAC.csv", 
    encoding = "UTF-8") %>%
  dplyr::mutate(C14AGE = as.numeric(C14AGE), 
                C14STD = as.numeric(C14STD)) %>%
  sf::st_as_sf(
    coords = c("LONG", "LAT"),
    crs = 4326,
    na.fail = F
  )

# Spatial data ----
f <- geojsonsf::geojson_sf("GIS/regions.geojson")


pottery <- read.csv(
  "https://raw.githubusercontent.com/dirkseidensticker/aSCAC/master/potterygroups.csv",
  encoding = "UTF-8")
# styleschrono$POSc <- as.character(styleschrono$POS)
pottery$FROM <- as.numeric(pottery$FROM)
pottery$TO <- as.numeric(pottery$TO)
