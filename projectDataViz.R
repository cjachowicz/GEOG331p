install.packages("viridis")
install.packages(c("terra", "ggplot2"))
library(terra)
library(ggplot2)
library(viridis)

#set paths to each file:
path_to_prefire <- "Z:\\cjachowicz\\data\\creek_FIRE_DATA\\prefire_folder"
path_to_postfire <- "Z:\\cjachowicz\\data\\creek_FIRE_DATA\\postfire_folder"

#folder contains: bands 5 + 7 for surface reflectance and surface temperature pre fire
prefire_folder <- list.files("Z:\\cjachowicz\\data\\creek_FIRE_DATA\\prefire_folder", full.names = T)
#folder with same for post fire
postfire_folder <- list.files("Z:\\cjachowicz\\data\\creek_FIRE_DATA\\postfire_folder", full.names = T)

# read in files 1-2 as a single multi-band raster
lc <- rast(prefire_folder [1:2])

#print variables
prefire_folder 
lc


plot(lc)

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
# CALC NBR PRE AND POST
nbr_prefire <- calc_nbr(prefire_nir, prefire_swir2)
nbr_postfire <- calc_nbr(postfire_nir, postfire_swir2)

# CALC dNBR (difference NBR shows burn severity)
dnbr <- nbr_prefire - nbr_postfire

#IMAGES NOT SAME DIMENSIONS: NEED TO RESAMPLE POSTFIRE Error: [-] extents do not match
nbr_postfire_resampled <- resample(nbr_postfire, nbr_prefire, method = "bilinear")
dnbr <- nbr_prefire - nbr_postfire_resampled

#SCALE TO MAKE DATA WRANGLING EASIER
# Crop + Aggregate + ggplot2
nbr_pre_agg <- aggregate(crop(nbr_prefire, fire_bbox), fact = 5)
nbr_pre_df <- as.data.frame(nbr_pre_agg, xy = TRUE)

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





