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
#LONG TERM FIRE FOLDER
longterm_folder <- list.files("Z:\\cjachowicz\\data\\creek_FIRE_DATA\\longterm_folder", full.names = T)

#see order
prefire_folder #3 4 5 7
longterm_folder #3 4 5 7 10

# Pre-fire scene (before September 2020)
pre_fire_red <- rast(prefire_folder[2])  # Band 4 (Red)
pre_fire_nir <-  rast(prefire_folder[3])  # Band 5 (NIR)

# Post-fire scene (immediately after fire, Fall 2020)
post_fire_red <- rast(postfire_folder[1]) #b4
post_fire_nir <- rast(postfire_folder[2]) #b5

#long_term_red and nir (2025 january)
long_term_red <- rast(longterm_folder[2]) #b4
long_term_nir <- rast(longterm_folder[3]) #b5


############## Flexible Cropping Functions ####################

# Function to crop from top (removes upper portion)
# proportion: fraction to remove from top (e.g., 0.33 removes top third)
crop_from_top <- function(r, proportion = 1/3) {
  e <- ext(r)
  ymin <- e[3]
  ymax <- e[4]
  y_range <- ymax - ymin
  
  new_ymax <- ymax - (proportion * y_range)  # lower the top
  new_ymin <- ymin  # keep bottom
  
  new_extent <- ext(e[1], e[2], new_ymin, new_ymax)
  crop(r, new_extent)
}

# Function to crop from right (removes right portion)
# proportion: fraction to remove from right (e.g., 0.25 removes right quarter)
crop_from_right <- function(r, proportion = 1/4) {
  e <- ext(r)
  xmin <- e[1]
  xmax <- e[2]
  x_range <- xmax - xmin
  
  new_xmax <- xmax - (proportion * x_range)  # move right edge left
  new_xmin <- xmin  # keep left
  
  new_extent <- ext(new_xmin, new_xmax, e[3], e[4])
  crop(r, new_extent)
}



pre_fire_red <- crop_from_top(pre_fire_red, 1/4)
pre_fire_nir <- crop_from_top(pre_fire_nir, 1/4)
pre_fire_red <- crop_from_right(pre_fire_red, 1/4)
pre_fire_nir <- crop_from_right(pre_fire_nir, 1/4)

post_fire_red <- crop_from_right(post_fire_red, 1/4)
post_fire_nir <- crop_from_right(post_fire_nir, 1/4)
post_fire_red  <- crop_from_top(post_fire_red, 1/4)
post_fire_nir  <- crop_from_top(post_fire_nir , 1/4)

long_term_nir  <- crop_from_top(long_term_nir, 1/4)
long_term_red  <- crop_from_top(long_term_red , 1/4)
long_term_nir <- crop_from_right(long_term_nir, 1/4)
long_term_red  <- crop_from_right(long_term_red , 1/4)




fire_data <- list(
  pre_fire_red = pre_fire_red,
  pre_fire_nir = pre_fire_nir,
  post_fire_red = post_fire_red,
  post_fire_nir = post_fire_nir,
  long_term_red = long_term_red,
  long_term_nir = long_term_nir
)

# Apply to all at once
fire_data <- lapply(fire_data, crop_from_right)




############## Crop to bottom two-thirds FIRST ####################
# Function to crop to bottom two-thirds
crop_bottom_two_thirds <- function(r) {
  e <- ext(r)
  ymin <- e[3]  # original ymin
  ymax <- e[4]  # original ymax
  y_range <- ymax - ymin
  new_ymax <- ymax  # keep top
  new_ymin <- ymax - (2/3 * y_range)  # bottom 2/3
  new_extent <- ext(e[1], e[2], new_ymin, new_ymax)
  crop(r, new_extent)
}

# Apply to all rasters
pre_fire_red <- crop_bottom_two_thirds(pre_fire_red)
pre_fire_nir <- crop_bottom_two_thirds(pre_fire_nir)
post_fire_red <- crop_bottom_two_thirds(post_fire_red)
post_fire_nir <- crop_bottom_two_thirds(post_fire_nir)
long_term_red <- crop_bottom_two_thirds(long_term_red)
long_term_nir <- crop_bottom_two_thirds(long_term_nir)









############## Aggregate first (faster) ####################
pre_fire_red <- aggregate(pre_fire_red, fact = 10, fun = mean)
pre_fire_nir <- aggregate(pre_fire_nir, fact = 10, fun = mean)
post_fire_red <- aggregate(post_fire_red, fact = 10, fun = mean)
post_fire_nir <- aggregate(post_fire_nir, fact = 10, fun = mean)
long_term_red <- aggregate(long_term_red, fact = 10, fun = mean)
long_term_nir <- aggregate(long_term_nir, fact = 10, fun = mean)

# Then align everything
pre_fire_nir <- resample(pre_fire_nir, pre_fire_red, method = "bilinear")
post_fire_red <- resample(post_fire_red, pre_fire_red, method = "bilinear")
post_fire_nir <- resample(post_fire_nir, pre_fire_red, method = "bilinear")
long_term_red <- resample(long_term_red, pre_fire_red, method = "bilinear")
long_term_nir <- resample(long_term_nir, pre_fire_red, method = "bilinear")

# Crop to common extent
common_extent <- intersect(ext(pre_fire_red), ext(pre_fire_nir))
pre_fire_red <- crop(pre_fire_red, common_extent)
pre_fire_nir <- crop(pre_fire_nir, common_extent)
post_fire_red <- crop(post_fire_red, common_extent)
post_fire_nir <- crop(post_fire_nir, common_extent)
long_term_red <- crop(long_term_red, common_extent)
long_term_nir <- crop(long_term_nir, common_extent)

# Now calculate NDVI
ndvi_pre <- (pre_fire_nir - pre_fire_red) / (pre_fire_nir + pre_fire_red)

# Post-fire NDVI
ndvi_post <- (post_fire_nir - post_fire_red) / (post_fire_nir + post_fire_red)

# NDVI change from pre-fire to post-fire
ndvi_change_immediate <- ndvi_post - ndvi_pre



par(mfrow = c(1, 1))

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



################MAIN FUNCTION HERE#############
#                TO CROP                #


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
r_final <- crop_custom(ndvi_post)
plot(r_final, 
     main = "Pre-Fire NDVI (August 28 2020)",
     col = terrain.colors(100),
     axes = TRUE)
#conclude plot usage




plot(r_final)
plot(ndvi_post)


plot(r_final, 
     main = "Pre-Fire NDVI (August 28 2020)",
     col = terrain.colors(100),
     axes = TRUE)
plot(ndvi_post, 
     main = "Pre-Fire NDVI (August 28 2020)",
     col = terrain.colors(100),
     axes = TRUE)




























