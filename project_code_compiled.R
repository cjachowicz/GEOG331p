#load necessary packages into R:
library(raster)
library(terra)
library(sf)
library(tidyverse)
library(viridis)

#DATA LOADING

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


#FUNCTION: CROPPING

#function to crop every raster by the Creek Fire polygon, for data analysis
crop_to_polygon <- function(landsat_file, polygon_fire) {
  polygon_vect <- vect(st_transform(polygon_fire, st_crs(landsat_file)))
  masked_landsat <- crop(mask(landsat_file, polygon_vect), polygon_vect)
}


#PART 1: Calculate dNBR using pre and post bands 5 and 7

#Load NIR and SWIR2 bands
#PREFIRE BANDS FOR EQUATION
prefire_nir <- rast(prefire_folder[3])   #band 5
prefire_swir2 <- rast(prefire_folder[4]) #band 7
#POSTFIRE BANDS FOR EQUATION
postfire_nir <- rast(postfire_folder[3])   #b5
postfire_swir2 <- rast(postfire_folder[4]) #b7

#crop to correct size:
prefire_nir <- crop_to_polygon(prefire_nir,polygon_fire)
prefire_swir2 <- crop_to_polygon(prefire_swir2, polygon_fire)
postfire_nir <- crop_to_polygon(postfire_nir, polygon_fire)
postfire_swir2 <- crop_to_polygon(postfire_swir2, polygon_fire)

#Calculation:
# NBR formula: (NIR - SWIR2) / (NIR + SWIR2)
calc_nbr <- function(nir, swir2) {
  nbr <- (nir - swir2) / (nir + swir2)
  return(nbr)
}
# Calculate NBR for both periods
nbr_prefire <- calc_nbr(prefire_nir, prefire_swir2)
nbr_postfire <- calc_nbr(postfire_nir, postfire_swir2)
# Match CRS
crs(nbr_postfire) <- crs(nbr_prefire)
# Resample postfire to perfect match prefire
postfire_resampled <- resample(nbr_postfire, nbr_prefire, method = "bilinear")

# Compute dNBR
dnbr <- nbr_prefire - postfire_resampled
########COMPUTATION DONE############
####################################

#PART 2: Calculate Surface Land Temperature using band 10 pre post long

#assign bands 10 for each variable
preTemp <- rast(prefire_folder[5])   #band 10
postTemp <- rast(postfire_folder[5]) #band 10
longTemp <- rast(longterm_folder[5]) #band 10

#crop to correct size:
preTemp <- crop_to_polygon(preTemp,polygon_fire)
postTemp <- crop_to_polygon(postTemp, polygon_fire)
longTemp <- crop_to_polygon(longTemp, polygon_fire)

#use given constants to convert to degrees Celsius:
# Convert DN (Digital Numbers) to brightness temperature in Kelvin
# Landsat 8/9 Band 10 conversion constants
ML <- 0.0003342  # Multiplicative rescaling factor
AL <- 0.1        # Additive rescaling factor
K1 <- 774.8853   # K1 constant for Band 10
K2 <- 1321.0789  # K2 constant for Band 10

# Function to convert in Celsius
dn_to_celsius <- function(dn, ML, AL, K1, K2) {
  #  radiance
  radiance <- ML * dn + AL
  # brightness temperature (Kelvin)
  temp_k <- K2 / log((K1 / radiance) + 1)
  #  Celsius
  temp_c <- temp_k - 273.15
  return(temp_c)
}
# Convert all bands to Celsius
preTemp <- dn_to_celsius(preTemp, ML, AL, K1, K2)
postTemp <- dn_to_celsius(postTemp, ML, AL, K1, K2)
longTemp <- dn_to_celsius(longTemp, ML, AL, K1, K2)

# Calculate temperature difference (after - before)
temp_dif_immediate <- postTemp - preTemp
temp_dif_long <- longTemp - preTemp
########COMPUTATION DONE############
####################################








