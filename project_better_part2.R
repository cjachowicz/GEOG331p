library(raster)
library(terra)
library(sf)
library(tidyverse)
library(viridis)

#create band10 vars
path_to_prefire <- "Z:\\cjachowicz\\data\\creek_FIRE_DATA\\prefire_folder"

#get files into a list
b10_before <- list.files("Z:\\cjachowicz\\data\\creek_FIRE_DATA\\temp_pre_B10_folder", full.names = T)
b10_after_2021  <- list.files("Z:\\cjachowicz\\data\\creek_FIRE_DATA\\temp_postfire_B10\\temp_2021", full.names = T)
#convert list into a raster file
b10_before <- rast(b10_before[1])
b10_after_2021 <- rast(b10_after_2021[1])

#Apply scaling factor to reduce data usage
before_fire <- aggregate(b10_before, fact = 10, fun = "mean")
after_fire_2021 <- aggregate(b10_after_2021, fact = 10, fun = mean)

#IMAGES NOT SAME DIMENSIONS: NEED TO RESAMPLE POSTFIRE  Error: [-] extents do not match
after_fire_2021 <- resample(after_fire_2021, before_fire, method = "bilinear")

# Convert DN (Digital Numbers) to brightness temperature in Kelvin
# Landsat 8/9 Band 10 conversion constants
ML <- 0.0003342  # Multiplicative rescaling factor
AL <- 0.1        # Additive rescaling factor
K1 <- 774.8853   # K1 constant for Band 10
K2 <- 1321.0789  # K2 constant for Band 10

# Function to convert DN to temperature in Celsius
dn_to_celsius <- function(dn, ML, AL, K1, K2) {
  # Convert to radiance
  radiance <- ML * dn + AL
  # Convert to brightness temperature (Kelvin)
  temp_k <- K2 / log((K1 / radiance) + 1)
  # Convert to Celsius
  temp_c <- temp_k - 273.15
  return(temp_c)
}
# Convert both images to Celsius
temp_before <- dn_to_celsius(before_fire, ML, AL, K1, K2)
temp_after <- dn_to_celsius(after_fire_2021, ML, AL, K1, K2)

# Calculate temperature difference (after - before)
temp_diff <- temp_after - temp_before

# Create visualization
par(mfrow = c(2, 2), mar = c(4, 4, 3, 6))

####################
#PLOT DIAGRAMS HERE#
####################

# Plot before fire
plot(temp_before, 
     main = "Temperature Before Fire",
     col = viridis(100),
     axes = FALSE,
     plg = list(title = "°C", title.adj = 0.2))

# Plot after fire
plot(temp_after, 
     main = "Temperature After Fire",
     col = viridis(100),
     axes = FALSE,
     plg = list(title = "°C", title.adj = 0.2))

# Plot temperature difference
plot(temp_diff, 
     main = "Temperature Difference (After - Before)",
     col = hcl.colors(100, "RdBu", rev = TRUE),
     axes = FALSE,
     plg = list(title = "°C", title.adj = 0.2))

# Plot histogram of temperature differences
hist(values(temp_diff), 
     main = "Distribution of Temperature Changes",
     xlab = "Temperature Difference (°C)",
     col = "skyblue",
     breaks = 50)


# Print summary statistics
cat("\n=== Temperature Statistics ===\n")
cat("\nBefore Fire (°C):\n")
print(summary(values(temp_before)))
cat("\nAfter Fire (°C):\n")
print(summary(values(temp_after)))
cat("\nTemperature Difference (°C):\n")
print(summary(values(temp_diff)))


