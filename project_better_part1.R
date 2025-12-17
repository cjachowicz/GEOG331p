library(raster)
library(terra)
library(sf)
library(tidyverse)
library(viridis)

# ===== LOAD LANDSAT DATA =====
# After downloading, you'll have a folder with .TIF files
# Example file names end in: _SR_B5.TIF (NIR) and _SR_B7.TIF (SWIR2)



#LOAD POLYGON OF FIRE MAPPING HERE
#direct approach: WORKED FINE __________________ ___________________ ___________________
setwd("Z:\\cjachowicz\\data\\creek_FIRE_DATA\\California_Historic_Fire_Perimeters_-7474393541221033101")
polygon_fire <- st_read("ca_fire_perimeters.shp")
print(polygon_fire)


#+++++++++++++++++++++++ polygon++++++++++++++++++++
#---------------------------------------------------





#folder contains: bands 5 + 7 for surface reflectance and surface temperature pre fire
prefire_folder <- list.files("Z:\\cjachowicz\\data\\creek_FIRE_DATA\\prefire_folder", full.names = T)
#folder with same for post fire
postfire_folder <- list.files("Z:\\cjachowicz\\data\\creek_FIRE_DATA\\postfire_folder", full.names = T)

# Load NIR and SWIR2 bands
#PREFIRE BANDS FOR EQUATION
prefire_nir <- rast(prefire_folder[3]) #band 5
prefire_swir2 <- rast(prefire_folder[4]) #band 7
#POSTFIRE BANDS FOR EQUATION
postfire_nir <- rast(postfire_folder[2])
postfire_swir2 <- rast(postfire_folder[3])



############ NEW STUFF

#polygon has same crs as raster file
st_transform(polygon_fire, crs(prefire_nir))
#mask it
polygon_vect <- vect(polygon_fire)

# Apply mask
masked_landsat <- mask(prefire_nir, polygon_vect)
plot(masked_landsat)



raster_df <- as.data.frame(masked_landsat, xy = TRUE, na.rm = TRUE)
ggplot(raster_df, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c() +
  coord_equal() +
  theme_minimal() +
  labs(title = "Masked Landsat Image", fill = "Reflectance")







#### END NEW stuff ###


#crop
prefire_nir <- crop_custom(prefire_nir)
prefire_swir2 <- crop_custom(prefire_swir2)
postfire_nir <- crop_custom(postfire_nir)
postfire_swir2 <- crop_custom(postfire_swir2)

#changed 25 -> 15
prefire_nir <- aggregate(prefire_nir, fact = 15, fun = mean)
prefire_swir2 <- aggregate(prefire_swir2, fact = 15, fun = mean)
postfire_nir <- aggregate(postfire_nir, fact = 15, fun = mean)
postfire_swir2 <- aggregate(postfire_swir2, fact = 15, fun = mean)

# ===== CALCULATE NBR =====
# NBR formula: (NIR - SWIR2) / (NIR + SWIR2)

calc_nbr <- function(nir, swir2) {
  nbr <- (nir - swir2) / (nir + swir2)
  return(nbr)
}

# Calculate NBR for both periods
nbr_prefire <- calc_nbr(prefire_nir, prefire_swir2)
nbr_postfire <- calc_nbr(postfire_nir, postfire_swir2)

#avoid the error
# Match CRS
crs(nbr_postfire) <- crs(nbr_prefire)

# Resample postfire to prefect match prefire
postfire_resampled <- resample(nbr_postfire, nbr_prefire, method = "bilinear")

# Compute dNBR
dnbr <- nbr_prefire - postfire_resampled

#dnbr <- nbr_prefire - nbr_postfire
#/\ didnt work: extents dont match

dnbr <- crop_custom(dnbr)
nbr_pre_df <- crop_custom(nbr_pre_df)
nbr_post_df <- crop_custom(nbr_post_df)



# ===== CREATE VISUALIZATIONS WITH GGPLOT2 =====

# Convert rasters to dataframes for ggplot
nbr_pre_df <- as.data.frame(nbr_prefire, xy = TRUE)
nbr_post_df <- as.data.frame(nbr_postfire, xy = TRUE)
dnbr_df <- as.data.frame(dnbr, xy = TRUE)

# Rename columns for clarity
names(nbr_pre_df)[3] <- "NBR"
names(nbr_post_df)[3] <- "NBR"
names(dnbr_df)[3] <- "dNBR"

# 1. PRE-FIRE NBR MAP
ggplot(nbr_pre_df, aes(x = x, y = y, fill = NBR)) +
  geom_raster() +
  scale_fill_viridis(option = "viridis", na.value = "transparent") +
  coord_equal() +
  labs(title = "Pre-fire NBR - Creek Fire Area",
       subtitle = "August 2020",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# 2. POST-FIRE NBR MAP
ggplot(nbr_post_df, aes(x = x, y = y, fill = NBR)) +
  geom_raster() +
  scale_fill_viridis(option = "viridis", na.value = "transparent") +
  coord_equal() +
  labs(title = "Post-fire NBR - Creek Fire Area",
       subtitle = "October 2020",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# 3. BURN SEVERITY MAP (dNBR)
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











# 000. PRE-FIRE NBR MAP
nbr_prefire <- as.data.frame(nbr_prefire, xy = TRUE)

ggplot(nbr_prefire, aes(x = x, y = y, fill = NBR)) +
  geom_raster() +
  scale_fill_viridis(option = "viridis", na.value = "transparent") +
  coord_equal() +
  labs(title = "Pre-fire NBR - Creek Fire Area",
       subtitle = "August 2020",
       x = "Longitude", y = "Latitude") +
  theme_minimal()




















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




