#load necessary packages into R:
library(raster)
library(terra)
library(sf)
library(tidyverse)
library(viridis)

#Load Polygon of fire: used to crop every landsat raster except
#the initial overlay
setwd("Z:\\cjachowicz\\data\\creek_FIRE_DATA\\California_Historic_Fire_Perimeters_-7474393541221033101")
polygon_fire <- st_read("ca_fire_perimeters.shp")


#Load landsat data: pre-fire, post-fire, and long-term
prefire_folder <- list.files("Z:\\cjachowicz\\data\\creek_FIRE_DATA\\prefire_folder", full.names = T)
postfire_folder <- list.files("Z:\\cjachowicz\\data\\creek_FIRE_DATA\\postfire_folder", full.names = T)
longterm_folder <- list.files("Z:\\cjachowicz\\data\\creek_FIRE_DATA\\longterm_folder", full.names = T)
#see what they look like
prefire_folder  #3 4 5 7 10 (bands in folder) for 2020 08 27
postfire_folder #3 4 5 7 10 for 2021 08 30
longterm_folder #3 4 5 7 10 for 2025 08 17









