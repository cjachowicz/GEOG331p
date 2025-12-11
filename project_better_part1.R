library(raster)
library(terra)
library(sf)
library(tidyverse)
library(viridis)

# ===== LOAD LANDSAT DATA =====
# After downloading, you'll have a folder with .TIF files
# Example file names end in: _SR_B5.TIF (NIR) and _SR_B7.TIF (SWIR2)

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


# ===== CALCULATE NBR =====
# NBR formula: (NIR - SWIR2) / (NIR + SWIR2)

calc_nbr <- function(nir, swir2) {
  nbr <- (nir - swir2) / (nir + swir2)
  return(nbr)
}

# Calculate NBR for both periods
nbr_prefire <- calc_nbr(prefire_nir, prefire_swir2)
nbr_postfire <- calc_nbr(postfire_nir, postfire_swir2)

# Calculate dNBR (differenced NBR - shows burn severity)
dnbr <- nbr_prefire - nbr_postfire

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