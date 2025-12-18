#load necessary packages into R:
library(raster)
library(terra)
library(sf)
library(tidyverse)
library(viridis)
library(gridExtra)


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


#PART 3: Calculate NDVI using bands 4 and 5 for pre post long

#Load the rest of bands not used yet
prefire_red <- rast(prefire_folder[2])    # Band 4 (Red)
postfire_red <- rast(postfire_folder[2])  # Band 4 (Red)
longterm_red <- rast(longterm_folder[2])  # Band 4 (Red)
longterm_nir <- rast(longterm_folder[3])  # Band 5 

#crop to correct size:
prefire_red <- crop_to_polygon(prefire_red,polygon_fire)
postfire_red <- crop_to_polygon(postfire_red, polygon_fire)
longterm_red <- crop_to_polygon(longterm_red, polygon_fire)
longterm_nir <- crop_to_polygon(longterm_nir, polygon_fire)

#Calculate NDVI: NDVI = (NIR - Red) / (NIR + Red)
pre_NDVI  <- (prefire_nir - prefire_red) / (prefire_nir + prefire_red)
post_NDVI <- (postfire_nir - postfire_red) / (postfire_nir + postfire_red)
long_NDVI <- (longterm_nir - longterm_red) / (longterm_nir + longterm_red)
########COMPUTATION DONE############
####################################


#PART 4: assign classes to dnbr
#assign classes to dnbr:
burn_class <- dnbr
burn_class[dnbr < 0.1] <- 0
burn_class[dnbr >= 0.1 & dnbr < 0.27] <- 1
burn_class[dnbr >= 0.27 & dnbr < 0.44] <- 2
burn_class[dnbr >= 0.44 & dnbr < 0.66] <- 3
burn_class[dnbr >= 0.66] <- 4


#PART 5: test for normality between nDBR and NDVI post long

# Calculate NDVI changes
ndvi_change_immediate <- post_NDVI - pre_NDVI # Short-term recovery
ndvi_change_long <- long_NDVI - pre_NDVI # Long-term recovery
# Calculate SLT changes
temp_dif_immediate <- postTemp - preTemp
temp_dif_long <- longTemp - preTemp



df_analysis <- data.frame(
  dnbr = values(dnbr),
  ndvi_imm = values(ndvi_change_immediate),
  ndvi_long = values(ndvi_change_long),
  temp_imm = values(temp_dif_immediate),
  temp_long = values(temp_dif_long)
)

# Remove rows with any NA values
df_analysis <- na.omit(df_analysis)

cat("\n = = = DATASET SUMMARY = = = \n")
cat("Number of valid pixels analyzed:", nrow(df_analysis), "\n")
cat("\nVariable ranges:\n")
print(summary(df_analysis))


#PART 6: CREATE CORRELATION TESTS

#Using Spearman's rank correlation:
df_analysis <- data.frame(
  dnbr = as.vector(values(dnbr)),
  ndvi_imm = as.vector(values(ndvi_change_immediate)),
  ndvi_long = as.vector(values(ndvi_change_long)),
  temp_imm = as.vector(values(temp_dif_immediate)),
  temp_long = as.vector(values(temp_dif_long))
)

# Remove rows with any NA values
df_analysis <- na.omit(df_analysis)

cat("\n = = = DATASET SUMMARY = = = \n")
cat("Number of valid pixels analyzed:", nrow(df_analysis), "\n")
cat("\nVariable ranges:\n")
print(summary(df_analysis))

# dNBR vs NDVI change (immediate)
cor_dnbr_ndvi_imm <- cor.test(df_analysis$dnbr, df_analysis$ndvi_imm, method = "spearman")
# dNBR vs NDVI change (long-term)
cor_dnbr_ndvi_long <- cor.test(df_analysis$dnbr, df_analysis$ndvi_long, method = "spearman")
# dNBR vs Temperature difference (immediate)
cor_dnbr_temp_imm <- cor.test(df_analysis$dnbr, df_analysis$temp_imm, method = "spearman")
# dNBR vs Temperature difference (long-term)
cor_dnbr_temp_long <- cor.test(df_analysis$dnbr, df_analysis$temp_long, method = "spearman")
# 
cor_ndvi_temp_imm <- cor.test(df_analysis$ndvi_imm, df_analysis$temp_imm, method = "spearman")
#
cor_ndvi_temp_long <- cor.test(df_analysis$ndvi_long, df_analysis$temp_long, method = "spearman")

# Print correlation summary
print(" = = = CORRELATION SUMMARY: = = = ")

cat("\n--- dNBR vs NDVI Change (Immediate) ---\n")
cat("Spearman's rho:", cor_dnbr_ndvi_imm$estimate, "\n")
cat("p-value:", cor_dnbr_ndvi_imm$p.value, "\n")
cat("\n--- dNBR vs NDVI Change (Long-term) ---\n")
cat("Spearman's rho:", cor_dnbr_ndvi_long$estimate, "\n")
cat("p-value:", cor_dnbr_ndvi_long$p.value, "\n")
cat("\n--- dNBR vs Temperature Difference (Immediate) ---\n")
cat("Spearman's rho:", cor_dnbr_temp_imm$estimate, "\n")
cat("p-value:", cor_dnbr_temp_imm$p.value, "\n")
cat("\n--- dNBR vs Temperature Difference (Long-term) ---\n")
cat("Spearman's rho:", cor_dnbr_temp_long$estimate, "\n")
cat("p-value:", cor_dnbr_temp_long$p.value, "\n")
cat("\n--- NDVI vs Temperature Difference (Immediate) ---\n")
cat("Spearman's rho:", cor_ndvi_temp_imm$estimate, "\n")
cat("p-value:", cor_ndvi_temp_imm$p.value, "\n")
cat("\n--- NDVI vs Temperature Difference (Long-term) ---\n")
cat("Spearman's rho:", cor_ndvi_temp_long$estimate, "\n")
cat("p-value:", cor_ndvi_temp_long$p.value, "\n")

#  correlation matrix - quick access
cor_matrix <- data.frame(
  Variable_Pair = c("dNBR vs NDVI (Immediate)", 
                    "dNBR vs NDVI (Long-term)",
                    "dNBR vs Temperature (Immediate)",
                    "dNBR vs Temperature (Long-term)",
                    "NDVI vs Temperature (Immediate)",
                    "NDVI vs Temperature (Long-term)"),
  Spearmans_rho = c(cor_dnbr_ndvi_imm$estimate,
                    cor_dnbr_ndvi_long$estimate,
                    cor_dnbr_temp_imm$estimate,
                    cor_dnbr_temp_long$estimate,
                    cor_ndvi_temp_imm$estimate,
                    cor_ndvi_temp_long$estimate),
  P_value = c(cor_dnbr_ndvi_imm$p.value,
              cor_dnbr_ndvi_long$p.value,
              cor_dnbr_temp_imm$p.value,
              cor_dnbr_temp_long$p.value,
              cor_ndvi_temp_imm$p.value,
              cor_ndvi_temp_long$p.value)
)

print(" = = = CORRELATION TABLE = = = ")
print(cor_matrix)

########################################

# Subsample data 
set.seed(123)  # reproducibility
n_sample <- min(5000, nrow(df_analysis)) #5k points each
df_sample <- df_analysis[sample(nrow(df_analysis), n_sample), ]

# Compute Spearman correlations on subsample
cor_dnbr_ndvi_imm <- cor.test(df_sample$dnbr, df_sample$ndvi_imm, method = "spearman")
cor_dnbr_ndvi_long <- cor.test(df_sample$dnbr, df_sample$ndvi_long, method = "spearman")
cor_dnbr_temp_imm <- cor.test(df_sample$dnbr, df_sample$temp_imm, method = "spearman")
cor_dnbr_temp_long <- cor.test(df_sample$dnbr, df_sample$temp_long, method = "spearman")
cor_ndvi_temp_imm <- cor.test(df_sample$ndvi_imm, df_sample$temp_imm, method = "spearman")
cor_ndvi_temp_long <- cor.test(df_sample$ndvi_long, df_sample$temp_long, method = "spearman")

# Print correlation summary
cat("\n = = = CORRELATION SUMMARY (Subsampled) = = = \n")

cor_list <- list(
  "dNBR vs NDVI Change (Immediate)" = cor_dnbr_ndvi_imm,
  "dNBR vs NDVI Change (Long-term)" = cor_dnbr_ndvi_long,
  "dNBR vs Temperature Difference (Immediate)" = cor_dnbr_temp_imm,
  "dNBR vs Temperature Difference (Long-term)" = cor_dnbr_temp_long,
  "NDVI vs Temperature Difference (Immediate)" = cor_ndvi_temp_imm,
  "NDVI vs Temperature Difference (Long-term)" = cor_ndvi_temp_long
)

for(name in names(cor_list)){
  cat("\n---", name, "---\n")
  cat("Spearman's rho:", cor_list[[name]]$estimate, "\n")
  cat("p-value:", cor_list[[name]]$p.value, "\n")
}

# Create correlation table for quick reference
cor_matrix <- data.frame(
  Variable_Pair = names(cor_list),
  Spearmans_rho = sapply(cor_list, function(x) x$estimate),
  P_value = sapply(cor_list, function(x) x$p.value)
)

cat("\n = = = CORRELATION TABLE (Subsampled) = = = \n")
print(cor_matrix, row.names = FALSE)

###############################################

#PART 7: REGRESSION ANALYSIS
# M1: Short-term NDVI change vs. fire severity
model1 <- lm(ndvi_imm ~ dnbr, data = df_analysis)
summary_model1 <- summary(model1)
print(summary_model1)
# M2: Long-term NDVI change vs. fire severity
model2 <- lm(ndvi_long ~ dnbr, data = df_analysis)
summary_model2 <- summary(model2)
print(summary_model2)
# M3: Short-term NDVI change vs. Short-term temperature change
model3 <- lm(ndvi_imm ~ temp_imm, data = df_analysis)
summary_model3 <- summary(model3)
print(summary_model3)
# M4: Long-term NDVI change vs. Long-term temperature change
model4 <- lm(ndvi_long ~ temp_long, data = df_analysis)
summary_model4 <- summary(model4)
print(summary_model4)
# M5: Short-term temperature change vs. fire severity
model5 <- lm(temp_imm ~ dnbr, data = df_analysis)
summary_model5 <- summary(model5)
print(summary_model5)
# M6: Long-term temperature change vs. fire severity
model6 <- lm(temp_long ~ dnbr, data = df_analysis)
summary_model6 <- summary(model6)
print(summary_model6)


#print summaries of each:
print(" - - - SUMMARY STATISTICS FOR EACH PAIR: - - - ")
print(summary_model1)
print(summary_model2)
print(summary_model3)
print(summary_model4)
print(summary_model5)
print(summary_model6)


# Subsample data plotting 
set.seed(123) # for reproducibility
n_plot <- min(5000, nrow(df_analysis)) #max 5k points, makes computer run normal speed
df_plot <- df_analysis[sample(nrow(df_analysis), n_plot), ]
#In this case, sampling actual values worked better than abstracting the entire data set. It also used less memory for some reason

# Function for scatterplot Spearman correlation
plot_correlation <- function(x, y, x_label, y_label, cor_test, df_plot) {
  rho <- round(cor_test$estimate, 3)
  pval <- signif(cor_test$p.value, 3)
  ggplot(data = df_plot, aes(x = x, y = y)) +
    geom_point(alpha = 0.5, color = "steelblue3") +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(
      x = x_label,
      y = y_label,
      title = paste0(x_label, " vs ", y_label),
      subtitle = paste0("Spearman's rho = ", rho, ", p = ", pval)
    ) +
    theme_minimal()
}

# Generate plots using subsampled dataset
p1 <- plot_correlation(df_plot$dnbr, df_plot$ndvi_imm, "dNBR", "NDVI Change (Immediate)", cor_dnbr_ndvi_imm, df_plot)
p2 <- plot_correlation(df_plot$dnbr, df_plot$ndvi_long, "dNBR", "NDVI Change (Long-term)", cor_dnbr_ndvi_long, df_plot)
p3 <- plot_correlation(df_plot$dnbr, df_plot$temp_imm, "dNBR", "Temperature Difference (Immediate)", cor_dnbr_temp_imm, df_plot)
p4 <- plot_correlation(df_plot$dnbr, df_plot$temp_long, "dNBR", "Temperature Difference (Long-term)", cor_dnbr_temp_long, df_plot)
p5 <- plot_correlation(df_plot$ndvi_imm, df_plot$temp_imm, "NDVI Change (Immediate)", "Temperature Difference (Immediate)", cor_ndvi_temp_imm, df_plot)
p6 <- plot_correlation(df_plot$ndvi_long, df_plot$temp_long, "NDVI Change (Long-term)", "Temperature Difference (Long-term)", cor_ndvi_temp_long, df_plot)

# Plot regressions at once
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3)



#PART 8: ANOVA ANALYSIS BY BURN SEVERITY CLASS
#chose to stop here

#PART F: ANY OTHER PLOTS WORTH CONSIDERING (most of which were used in the slideshow, but I realize they don't provide much value besides a visual)
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

# Plot before fire
plot(preTemp, 
     main = "Temperature Before Fire",
     col = viridis(100),
     axes = FALSE,
     plg = list(title = "°C", title.adj = 0.2))

# Plot after fire
plot(postTemp, 
     main = "Temperature After Fire",
     col = viridis(100),
     axes = FALSE,
     plg = list(title = "°C", title.adj = 0.2))

# Plot temperature difference
plot(temp_dif_immediate, 
     main = "Temperature Difference (After - Before)",
     col = hcl.colors(100, "RdBu", rev = TRUE),
     axes = FALSE,
     plg = list(title = "°C", title.adj = 0.2))

# Plot histogram of temperature differences Across Creek Fire Mapping
hist(values(temp_dif_immediate), 
     main = "Distribution of Temperature Changes",
     xlab = "Temperature Difference (°C)",
     col = "red4",
     breaks = 50)



