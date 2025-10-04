#create a function. The names of the arguments for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
}
#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")

#evaluate a true statement
assert(2 == 2, "error: unequal values")
#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5,5)
c <- c(1,2)
#test cases:
assert(length(a) == length(b), "error: unequal length") #=TRUE
assert(length(a) == length(c), "error: lists are of unequal length") #=FALSE
assert(length(c) == length(b), "not an arror: error") #=FALSE

#read in the data file, skip the first 3 rows
datW <- read.csv("Z:\\cjachowicz\\data\\bewkes\\bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
print(datW[1,])
#get sensor info from file
sensorInfo <-   read.csv("Z:\\cjachowicz\\data\\bewkes\\bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)
print(sensorInfo)
#get column names from sensorInfo table and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)
print(datW[1,])

#############Question 3#############
#use install.packages to install lubridate
#install.packages(c("lubridate")) #<- line used to install the package lubridate. 
#Uncomment once finished so you don't accidentally redownload it. it is helpful to comment this line after you run this line of code on the computer and the package installs. You really don't want to do this over and over again.
library(lubridate)
#"The following objects are masked from ‘package:base’:" MEANS default R has packages
#with those names, and the base functions were overwritten to avoid same names. you can still call the masked function by specifying which package it is from e.g. base::union() or lubridate::union()

#convert to standardized format
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")
#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calculations
datW[1,]

#See how many values have missing data for each sensor observation
#air temperature
length(which(is.na(datW$air.temperature)))
#wind speed
length(which(is.na(datW$wind.speed)))
#precipitation
length(which(is.na(datW$precipitation)))
#soil temperature
length(which(is.na(datW$soil.moisture)))
#soil moisture
length(which(is.na(datW$soil.temp)))
#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")
#make a plot with filled in points (using pch)
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

#I'm going to make a new column to work with that indicates that I am conducting QAQC because overwriting values should be done cautiously and can lead to confusing issues.
#It can be particularly confusing when you are just learning R. Here I'm using the ifelse function. the first argument is a logical statement to be evaluated as true or false on a vector. the second argument is the value that my air.tempQ1 column will be given if the statement
#is true. The last value is the value that will be given to air.tempQ1 if the statement is false.
#In this case it is just given the air temperature value
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)
#new col <- ifelse(if temperature < 0, replace the value with NA, otherwise keep its current temp)

#check the values at the extreme range of the data (each quartile) and throughout the percentiles
quantile(datW$air.tempQ1)
Ssometimes removing extremes is good, but you have to be cautious. Look at days with really low air temperature
datW[datW$air.tempQ1 < 8,] 
#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]  

#############Question 4#############
#Plot precipitation and lightning strikes on the same plot. Normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked, make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & Lightning",
     type="n")
#plot precipitation points only when there is precipitation, make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        
#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "rosybrown2", pch=19)

#############Question 5#############
assert(length(lightscale) == length(datW$lightning.acvitivy), "You have an unequal length")
#filter out storms in wind and air temperature measurements. filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm. create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

#############Question 6#############
#Remove any suspect data measurements from wind measurements
datW$wind.speedQ1 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$wind.speed))
#Check that all suspect measurements (precipitation >= 2 and some lightning activity) are in fact NA (na).
assert(all(is.na(datW$wind.speedQ1[datW$precipitation  >= 2 & datW$lightning.acvitivy >0])), "wrong")
#Check that all suspect measurements (precipitation > 5) are in fact NA (na).
assert(all(is.na(datW$wind.speedQ1[datW$precipitation > 5])),"not filtered")

#compares totals of NA values of wind speed to NA values of air temp
assert(all(is.na(datW$wind.speedQ1) == is.na(datW$air.tempQ2)), "Not correct buddy")
#Plot all valid wind speed measurements using wind.speedQ1:
plot(datW$DD , datW$wind.speedQ1, xlab = "Day of Year", ylab = "Filtered Wind Speed",
     type="n")
#Add lines to the graph
lines(datW$DD, datW$wind.speed)
#Add individual points to the graph
points(datW$DD, datW$wind.speed, col = "turquoise2")
point_NA <- datW$wind.speedQ1 

#############Question 7#############
par(mfrow = c(2, 1))
#soil moisture plot
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & Soil Moisture",
     type="n",
     ylim = c(0, 5.5))
#Plot every point during precipitation 
#Create index for data with precipitation
precipitation_idx <- datW$precipitation > 0
points(datW$DD[precipitation_idx], datW$precipitation[precipitation_idx],
       col= "green4", pch=19)        
#Plot points with at least some soil moisture. x = Day of moisture, y = Measurement of moisture. Create data with only moisture measurements to index:
moisture_idx <- datW$soil.moisture > 0
points(datW$DD[moisture_idx], datW$soil.moisture[moisture_idx], col = "turquoise3", pch = 15)

#soil temperature plot
plot(datW$DD , datW$air.tempQ2, xlab = "Day of Year", ylab = "Air Temp and Soil Temp in Degrees C",
     type="n")
#plot only when there is air temp
air_temp <- datW$air.tempQ2 > 0
points(datW$DD[air_temp], datW$air.tempQ2[air_temp],
       col= "grey49", pch=15)        
#plot only when there is soil temp
#soil temp index where temp is greater than 0
soil_temp <- datW$soil.temp > 0
points(datW$DD[soil_temp], datW$soil.temp[soil_temp],
       col= "red4", pch=19)

#############Question 8#############
#Calculate each average. Draw from complete data set. Not including NA values.
air_avg <- round(mean(datW$air.tempQ2, na.rm=TRUE),1)
wind_avg <- round(mean(datW$wind.speedQ1, na.rm=TRUE),1)
soil_temp_avg <- round(mean(datW$soil.temp, na.rm=TRUE),1)
soil_moist_avg <- round(mean(datW$soil.moisture, na.rm=TRUE),3)
precipitation_avg <- mean(datW$precipitation, na.rm=TRUE)
#Calculate total for each observation
total_air <- sum(!is.na(datW$air.tempQ2))
total_wind <- sum(!is.na(datW$wind.speedQ1))
total_soil_temp <- sum(!is.na(datW$soil.temp))
total_soil_moist <- sum(!is.na(datW$soil.moisture))
total_precipitation <- sum(!is.na(datW$precipitation))

#create a table with average air temp, wind speed, soil moisture, and soil temp
average_table <- data.frame("Observation Items"       = (c("Air Temperature","Wind Speed","Soil Moisture","Soil Temperature","Precipitation")),
                            "Observation Averages"    = (c(air_avg,wind_avg,soil_temp_avg,soil_moist_avg,precipitation_avg)),
                            "Cumulative Observations" = (c(total_air,total_wind,total_soil_temp,total_soil_moist,total_precipitation)))
#total observations going into the calculations:
total_totals <- sum(total_air,total_wind,total_soil_temp,total_soil_moist,total_precipitation)
print(total_totals)

#############Question 9#############
par(mfrow = c(2, 2))
#The same x axis range for each plot will be the standard day of year range used so far
#Plot for Air Temperature:
plot(datW$DD, datW$air.tempQ2, xlab = "Day of Year", ylab = "Air Temp in Degrees C",
     type="b", col= "red2", pch=15)
#Plot for Precipitation:
plot(datW$DD, datW$precipitation, xlab = "Day of Year", ylab = "Precipitation in mm",
     type="b", col= "orange1", pch=15)
#Plot for Soil Moisture:
plot(datW$DD, datW$soil.temp, xlab = "Day of Year", ylab = "Soil Moisture in (Meters^3 / Meter^3)",
     type="b", col= "yellow2", pch=15)
#Plot for Soil Temperature:
plot(datW$DD, datW$soil.moisture, xlab = "Day of Year", ylab = "Soil Temp in Degrees C",
     type="b", col= "olivedrab1", pch=15)