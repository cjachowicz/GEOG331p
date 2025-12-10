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
prefire_nir <- rast(prefire_folder[1])
prefire_swir2 <- rast(prefire_folder[2])
#POSTFIRE BANDS FOR EQUATION
postfire_nir <- rast(postfire_folder[1])
postfire_swir2 <- rast(postfire_folder[2])

# ===== CALCULATE NBR =====
# NBR formula: (NIR - SWIR2) / (NIR + SWIR2)

calc_nbr <- function(nir, swir2) {
  nbr <- (nir - swir2) / (nir + swir2)
  return(nbr)
}

# Calculate NBR for both periods
nbr_prefire <- calc_nbr(prefire_nir, prefire_swir2)
nbr_postfire <- calc_nbr(postfire_nir, postfire_swir2)

# Free memory from raw bands
rm(prefire_nir, prefire_swir2, postfire_nir, postfire_swir2)
gc()

#IMAGES NOT SAME DIMENSIONS: NEED TO RESAMPLE POSTFIRE  Error: [-] extents do not match
nbr_postfire_resampled <- resample(nbr_postfire, nbr_prefire, method = "bilinear")
dnbr <- nbr_prefire - nbr_postfire_resampled


# Calculate dNBR (differenced NBR - shows burn severity)
dnbr <- nbr_prefire - nbr_postfire



# ===== CREATE VISUALIZATIONS WITH GGPLOT2 =====
# Aggregate by factor of 25 (reduces pixels from e.g., 10000x10000 to 400x400)
# This reduces memory by 625x (25^2)

#changed 25 -> 15
nbr_prefire <- aggregate(nbr_prefire, fact = 15, fun = mean)
nbr_postfire <- aggregate(nbr_postfire, fact = 15, fun = mean)




# ===== CROP TO FIRE AREA (bottom-left 2/3 of frame) =====
# Get extent of the raster
e <- ext(nbr_prefire)

# Calculate new extent for bottom-left 2/3
new_extent <- ext(
  e[1],                           # xmin (left edge - keep same)
  e[1] + (e[2] - e[1]) * 2/3,    # xmax (2/3 across from left)
  e[3],                           # ymin (bottom edge - keep same)
  e[3] + (e[4] - e[3]) * 2/3     # ymax (2/3 up from bottom)
)

# Crop both rasters to fire area
nbr_prefire <- crop(nbr_prefire, new_extent)
nbr_postfire <- crop(nbr_postfire, new_extent)

# ===== ADDITIONAL 10% TRIM FROM ALL SIDES =====
# Get extent of cropped raster
e2 <- ext(nbr_prefire)

# Calculate dimensions
width <- e2[2] - e2[1]
height <- e2[4] - e2[3]

# Create trimmed extent (remove 10% from each side)
trimmed_extent <- ext(
  e2[1] + width * 0.1,   # xmin (trim 10% from left)
  e2[2] - width * 0.1,   # xmax (trim 10% from right)
  e2[3] + height * 0.1,  # ymin (trim 10% from bottom)
  e2[4] - height * 0.1   # ymax (trim 10% from top)
)

# Apply final trim
nbr_prefire <- crop(nbr_prefire, trimmed_extent)
nbr_postfire <- crop(nbr_postfire, trimmed_extent)







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

# 4. TIME SERIES RECOVERY GRAPH
# (You'll need multiple post-fire dates for this)
# Load recovery images for 2021, 2022, 2023, 2024

# Example for time series
recovery_data <- data.frame(
  date = as.Date(c("2020-08-01", "2020-10-01", 
                   "2021-08-01", "2022-08-01", 
                   "2023-08-01", "2024-08-01")),
  mean_nbr = c(0.45, 0.15, 0.25, 0.32, 0.38, 0.41),
  stage = c("Pre-fire", "Immediate", "1-year", 
            "2-year", "3-year", "4-year")
)

ggplot(recovery_data, aes(x = date, y = mean_nbr)) +
  geom_line(size = 1.2, color = "#2c7bb6") +
  geom_point(size = 3, color = "#d7191c") +
  geom_vline(xintercept = as.Date("2020-09-04"), 
             linetype = "dashed", color = "red") +
  annotate("text", x = as.Date("2020-09-04"), y = 0.5, 
           label = "Fire Start", angle = 90, vjust = -0.5) +
  labs(title = "Vegetation Recovery - Creek Fire",
       subtitle = "Mean NBR over time",
       x = "Date", y = "Mean NBR") +
  theme_minimal()

