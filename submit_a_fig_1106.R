library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)

datW <- read.csv("Z:\\cjachowicz\\data\\PROJECT_DATA\\daily_weather.csv.csv") #read in precip data
head(datW) #hourly precipitation is in inches


datN <- read.csv("Z:\\cjachowicz\\data\\Mortality.csv") 
head(datN) 

## 11 / 6 / 25 ##
#####PLOT 1#####
ggplot(datN, aes(x=datN$Species,y=datN$X..Survival)) + 
  geom_point(color="blue") + 
  labs(title = "Survival rate (%) by species",
                            x = "Species observed",
                            y = "Survival rate (%)")

#####PLOT 2######
ggplot(datN, aes(x=Bay,y=X..Survival)) + 
  geom_point(color="tomato4") + 
  labs(title = "Survival rate (%) by bay",
       x = "Bay observed",
       y = "Survival rate (%)")




## 11 / 13 / 25 ##
######PLOT 3#########
#general timeline, ten years
#2015-2025, can be truncated to fit the data later

#shows daily high and low temperature
datW <- datW %>%
  mutate(Date = mdy(Date)) #convert date to manipulable format
datW$doy <- yday(datW$Date)#get day of year
datW$year <- year(datW$Date)#get year 

focus_dataset <- datW %>% #creates subset only between 1/1/2015 and 1/1/2025
  filter(Date >= as.Date("2015-01-01") & 
         Date <= as.Date("2025-01-01"))
#rename variables shorter
max_temp <- "Maximum Temperature degrees (F)"
min_temp <- "Minimum Temperature degrees (F)"
precip <- "Precipitation (inches)"

ggplot(data = focus_dataset, aes(x = datW$year, y = max_temp))


ggplot(focus_dataset, aes(x = Date)) +
  geom_point(aes(y = "Maximum Temperature degrees (F)", color = "Maximum Temp")) +
  geom_point(aes(y = "Minimum Temperature degrees (F)", color = "Minimum Temp")) +
  labs(
    title = "Daily Maximum and Minimum Temperatures (2015–2025)",
    x = "Date",
    y = "Temperature (°F)",
    color = "Legend"
  ) + theme_minimal()



#######PLOT 4########
#builds off plot 3, has snow and precipitation amounts added

testSet <- datW %>% #creates subset only between 1/1/2015 and 1/1/2025
  filter(Date >= as.Date("2015-01-01") & 
           Date <= as.Date("2015-12-31"))

length(testSet$doy)
length(testSet$"Precipitation (inches)")
colnames(testSet)[colnames(testSet) == "precip"]

ggplot(data = datW[datW$year==2016], aes(x = year, y = precip)) + geom_point(x = year, y = precip,color = "blue")

ggplot(testSet, aes(x = doy, y = precip)) +
  geom_point(color = "steelblue") +
  labs(
    title = "Annual Precipitation Over the Years",
    x = "Year",
    y = "Precipitation (mm)"
  ) +
  theme_minimal(base_size = 14)




#THIS IS CORRECT DO THIS
testSet$Precipitation..inches<- as.numeric(testSet$Precipitation..inches)

#replace m, t, s to NA. cnvet these to NA
#convert data to nums not chr

unique(datW$Maximum.Temperature.degrees..F.)
unique(datW$Minimum.Temperature.degrees..F.)

avoid <- c("S","M","T")

data_new <- replace(datW$Maximum.Temperature.degrees..F., avoid, NA)


#Convert every string into a number; any letter becomes NA
datW <- datW %>% 
  mutate(across(where(is.character), ~ suppressWarnings(as.numeric(.x))))

#subset 1/1/2015 to 12/31/2025, 365 observations of 8 variables
newSubSet <- datW %>%
  filter(Date >= as.Date("2015-01-01") & 
           Date <= as.Date("2015-12-31"))


#ggplot
ggplot(datW,aes(x=doy)) +
  geom_point(aes(y = Minimum.Temperature.degrees..F.), color = "cyan2",alpha = .5) +
  geom_point(aes(y = Maximum.Temperature.degrees..F.), color = "hotpink", alpha = .5) +
  labs(
    title = "Daily Min and Max Temperature (degrees F)",
    x = "Doy (2015)",
    y = "Temperature (degrees F)",
  ) +
  theme_minimal()+
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))





















