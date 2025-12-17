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
postfire_folder
longterm_folder #3 4 5 7 10

# Pre-fire scene (before September 2020) 08/28/2020
pre_fire_red <- rast(prefire_folder[2])  # Band 4 (Red)
pre_fire_nir <-  rast(prefire_folder[3])  # Band 5 (NIR)

# Post-fire scene (immediately after fire, Fall 2020) #01/18/2021
post_fire_red <- rast(postfire_folder[5]) #b4
post_fire_nir <- rast(postfire_folder[6]) #b5

#long_term_red and nir (2025 january) #01/13/2025
long_term_red <- rast(longterm_folder[7]) #b4
long_term_nir <- rast(longterm_folder[8]) #b5




#ploygon here
#direct approach: WORKED FINE __________________ ___________________ ___________________
setwd("Z:\\cjachowicz\\data\\creek_FIRE_DATA\\California_Historic_Fire_Perimeters_-7474393541221033101")
polygon_fire <- st_read("ca_fire_perimeters.shp")
print(polygon_fire)
vect(polygon_fire)

#same crs
polygon <- st_transform(polygon_fire, crs(ndvi_pre))
# 4. Mask the raster using the polygon
masked_landsat <- mask(ndvi_pre, polygon)
# 5. Optional: Crop to the polygon extent for a smaller raster
masked_landsat <- crop(masked_landsat, polygon)
# 6. Plot the masked raster
plot(masked_landsat)
plot(ndvi_pre)


polygon_vect <- vect(st_transform(polygon, crs(ndvi_post)))
# Mask and crop in one step
masked_landsat <- crop(mask(ndvi_post, polygon_vect), polygon_vect)
# Plot result
plot(masked_landsat)


# Convert raster to dataframe for ggplot
landsat_df <- as.data.frame(ndvi_long, xy = TRUE)
names(landsat_df)[3] <- "value"
ggplot() +
  geom_raster(data = landsat_df, aes(x = x, y = y, fill = value)) +
  geom_sf(data = polygon, fill = NA, color = "red", size = 1.2) +
  coord_sf() +
  theme_minimal()




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

ndvi_post <- resample(ndvi_post, ndvi_pre, method = "bilinear")
# NDVI change from pre-fire to post-fire
ndvi_change_immediate <- ndvi_post - ndvi_pre

# long-fire NDVI
ndvi_long <- (long_term_nir - long_term_red) / (long_term_nir + long_term_red)

# NDVI change from pre-fire to long
ndvi_change_late <- ndvi_long - ndvi_pre

par(mfrow = c(1, 1))

par(mfrow = c(2, 2), mar = c(4, 4, 3, 6))

ndvi_pre <- crop_custom(ndvi_pre)
ndvi_post <- crop_custom(ndvi_post)
ndvi_change_immediate <- crop_custom(ndvi_change_immediate)
ndvi_long <- crop_custom(ndvi_long)

# 1. Pre-fire NDVI
plot(ndvi_pre, 
     main = "Pre-Fire NDVI (August 28 2020)",
     col = terrain.colors(100),
     axes = TRUE)

# 2. Post-fire NDVI
plot(ndvi_post,
     main = "Post-Fire NDVI (January 13 2021)",
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
# 5. Long-fire NDVI
plot(ndvi_long, 
     main = "Post-Fire NDVI (January 13 2025)",
     col = terrain.colors(100),
     axes = TRUE)
# 6. DIFFERENCE BETWEEN PRE AND LONG
plot(ndvi_change_late, 
     main = "NDVI Change (Post[2025] - Pre)",
     col = rev(heat.colors(100)),
     axes = TRUE)


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




























