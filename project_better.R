# Install/load packages
install.packages(c("raster", "sf", "tidyverse", "viridis", "terra"))

library(raster)
library(terra)
library(sf)
library(tidyverse)
library(viridis)

# ===== LOAD LANDSAT DATA =====
# After downloading, you'll have a folder with .TIF files
# Example file names end in: _SR_B5.TIF (NIR) and _SR_B7.TIF (SWIR2)
#set paths to each file:
path_to_prefire <- "Z:\\cjachowicz\\data\\creek_FIRE_DATA\\prefire_folder"
path_to_postfire <- "Z:\\cjachowicz\\data\\creek_FIRE_DATA\\postfire_folder"

#folder contains: bands 5 + 7 for surface reflectance and surface temperature pre fire
prefire_folder <- list.files("Z:\\cjachowicz\\data\\creek_FIRE_DATA\\prefire_folder", full.names = T)
#folder with same for post fire
postfire_folder <- list.files("Z:\\cjachowicz\\data\\creek_FIRE_DATA\\postfire_folder", full.names = T)

#PREFIRE BANDS FOR EQUATION
prefire_nir <- rast(prefire_folder[3]) #band 5
prefire_swir2 <- rast(prefire_folder[4]) #band 7
#POSTFIRE BANDS FOR EQUATION
postfire_nir <- rast(postfire_folder[2]) #01/18/2021
postfire_swir2 <- rast(postfire_folder[3]) #01/18/2021



#+++++++++++++++++++++++ polygon++++++++++++++++++++
#---------------------------------------------------
#poly_folder <- list.files("Z:\\cjachowicz\\data\\creek_FIRE_DATA\\California_Historic_Fire_Perimeters_-7474393541221033101")
#print(poly_folder)
#polygon_fire <- st_read(poly_folder[4])
#print(polygon_fire)
#referne shape if above doesnt work
#fire_poly <- st_read("ca_fire_perimeters.shp")

#shp_files <- list.files(pattern = "\\.shp$")
#polygon_fire <- st_read(shp_files[1])


#direct approach: WORKED FINE __________________ ___________________ ___________________
setwd("Z:\\cjachowicz\\data\\creek_FIRE_DATA\\California_Historic_Fire_Perimeters_-7474393541221033101")
polygon_fire <- st_read("ca_fire_perimeters.shp")
print(polygon_fire)


#+++++++++++++++++++++++ polygon++++++++++++++++++++
#---------------------------------------------------



# ===== CALCULATE NBR =====
# NBR formula: (NIR - SWIR2) / (NIR + SWIR2)

calc_nbr <- function(nir, swir2) {
  nbr <- (nir - swir2) / (nir + swir2)
  return(nbr)
}

# Calculate NBR for both periods
nbr_prefire <- calc_nbr(prefire_nir, prefire_swir2) #08/28/2020
nbr_postfire <- calc_nbr(postfire_nir, postfire_swir2) #01/18/2021

# Free memory from raw bands
rm(prefire_nir, prefire_swir2, postfire_nir, postfire_swir2)
gc()

#IMAGES NOT SAME DIMENSIONS: NEED TO RESAMPLE POSTFIRE  Error: [-] extents do not match
nbr_postfire_resampled <- resample(nbr_postfire, nbr_prefire, method = "bilinear")
dnbr <- nbr_prefire - nbr_postfire_resampled


# Calculate dNBR (differenced NBR - shows burn severity)
dnbr <- nbr_prefire - nbr_postfire





library(raster)

# Match CRS
crs(nbr_postfire) <- crs(nbr_prefire)

# Resample postfire to prefect match prefire
postfire_resampled <- resample(nbr_postfire, nbr_prefire, method = "bilinear")

# Compute dNBR
dnbr <- nbr_prefire - postfire_resampled


# ===== CREATE VISUALIZATIONS WITH GGPLOT2 =====
# Aggregate by factor of 25 (reduces pixels from e.g., 10000x10000 to 400x400)
# This reduces memory by 625x (25^2)

#changed 25 -> 15
nbr_prefire <- aggregate(nbr_prefire, fact = 15, fun = mean)
nbr_postfire <- aggregate(nbr_postfire, fact = 15, fun = mean)



nbr_pre_df <- crop_custom(nbr_pre_df)
r_final <- crop_custom(ENTER_NAME_HERE)



# 1. PRE-FIRE NBR MAP
# Convert to dataframe only when needed, then plot and remove
nbr_pre_df <- as.data.frame(nbr_prefire, xy = TRUE)
names(nbr_pre_df)[3] <- "NBR"

ggplot(nbr_pre_df, aes(x = x, y = y, fill = NBR)) +
  geom_raster() +
  scale_fill_viridis(option = "viridis", na.value = "transparent") +
  coord_equal() +
  labs(title = "Pre-fire NBR - Creek Fire Area",
       subtitle = "August 2020",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

rm(nbr_pre_df)
gc()

# 2. POST-FIRE NBR MAP
nbr_post_df <- as.data.frame(nbr_postfire, xy = TRUE)
names(nbr_post_df)[3] <- "NBR"

ggplot(nbr_post_df, aes(x = x, y = y, fill = NBR)) +
  geom_raster() +
  scale_fill_viridis(option = "viridis", na.value = "transparent") +
  coord_equal() +
  labs(title = "Post-fire NBR - Creek Fire Area",
       subtitle = "October 2020",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

rm(nbr_post_df)
gc()

# Free NBR rasters if no longer needed
rm(nbr_prefire, nbr_postfire)
gc()

# 3. BURN SEVERITY MAP (dNBR)
dnbr_df <- as.data.frame(dnbr, xy = TRUE)
names(dnbr_df)[3] <- "dNBR"

# Define burn severity classes
dnbr_df <- dnbr_df %>%
  mutate(severity = case_when(
    dNBR < 0.1 ~ "Unburned",
    dNBR >= 0.1 & dNBR < 0.27 ~ "Low",
    dNBR >= 0.27 & dNBR < 0.44 ~ "Moderate-Low",
    dNBR >= 0.44 & dNBR < 0.66 ~ "Moderate-High",
    dNBR >= 0.66 ~ "High",
    TRUE ~ NA_character_
  ))

ggplot(dnbr_df, aes(x = x, y = y, fill = severity)) +
  geom_raster() +
  scale_fill_manual(values = c("Unburned" = "#2c7bb6",
                               "Low" = "#abd9e9",
                               "Moderate-Low" = "#ffffbf",
                               "Moderate-High" = "#fdae61",
                               "High" = "#d7191c"),
                    na.value = "transparent") +
  coord_equal() +
  labs(title = "Burn Severity - Creek Fire",
       subtitle = "dNBR (Pre-fire minus Post-fire)",
       fill = "Burn Severity",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

rm(dnbr_df, dnbr)
gc()




crop_custom <- function(r) {
  e <- ext(r)
  xr <- e$xmax - e$xmin
  yr <- e$ymax - e$ymin
  
  # 10% crop
  r1 <- crop(r, ext(
    e$xmin + 0.16 * xr,
    e$xmax - 0.16 * xr,
    e$ymin + 0.16 * yr,
    e$ymax - 0.16 * yr
  ))
  
  e <- ext(r1)
  xr <- e$xmax - e$xmin
  yr <- e$ymax - e$ymin
  
  # Additional 25% crop (top & right)
  r2 <- crop(r1, ext(
    e$xmin,
    e$xmax - 0.30 * xr,
    e$ymin,
    e$ymax - 0.30 * yr
  ))
  
  r2
}

# Usage
r_final <- crop_custom(ENTER_NAME_HERE)
plot(r_final, 
     main = "Pre-Fire NDVI (August 28 2020)",
     col = terrain.colors(100),
     axes = TRUE)
#conclude plot usage





























