#project_better_part3

#Main focus: analyze vegetation patterns over time since fire passed
library(raster)
library(terra)
library(sf)
library(tidyverse)
library(viridis)

prefire_folder <- list.files("Z:\\cjachowicz\\data\\creek_FIRE_DATA\\prefire_folder", full.names = T)
#folder with same for post fire
postfire_folder <- list.files("Z:\\cjachowicz\\data\\creek_FIRE_DATA\\postfire_folder", full.names = T)

#see order
prefire_folder #3 4 5 7

# Pre-fire scene (before September 2020)
pre_fire_red <- rast(prefire_folder[2])  # Band 4 (Red)
pre_fire_nir <-  rast(prefire_folder[3])  # Band 5 (NIR)

# Post-fire scene (immediately after fire, Fall 2020)
post_fire_red <- rast(postfire_folder[1])
post_fire_nir <- rast(postfire_folder[2])



#################SCALING NOT DONE, CHECK COPY AGAIN FROM FILE 1
#Apply scaling factor to reduce data usage
pre_fire_red <- aggregate(b10_before, fact = 10, fun = "mean")
after_fire_2021 <- aggregate(b10_after_2021, fact = 10, fun = mean)



#bands 4 and 5 later after fire




# Pre-fire NDVI
ndvi_pre <- (pre_fire_nir - pre_fire_red) / (pre_fire_nir + pre_fire_red)

# Post-fire NDVI
ndvi_post <- (post_fire_nir - post_fire_red) / (post_fire_nir + post_fire_red)

# NDVI change from pre-fire to post-fire
ndvi_change_immediate <- ndvi_post - ndvi_pre





par(mfrow = c(2, 2), mar = c(4, 4, 3, 6))

# 1. Pre-fire NDVI
plot(ndvi_pre, 
     main = "Pre-Fire NDVI (August 28 2020)",
     col = terrain.colors(100),
     axes = TRUE)

# 2. Post-fire NDVI
plot(ndvi_post,
     main = "Post-Fire NDVI (January 15 2021)",
     col = terrain.colors(100),
     axes = TRUE)

# 3. NDVI Change (difference)
plot(ndvi_change_immediate,
     main = "NDVI Change (Post - Pre)",
     col = rev(heat.colors(100)),
     axes = TRUE)

# 4. Histogram of NDVI change
hist(ndvi_change_immediate,
     main = "Distribution of NDVI Change",
     xlab = "NDVI Change",
     col = "steelblue",
     breaks = 50)







