library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)
#Treatment and Post application monitoring (2019-2022)
#goes from 8/1/2019 to 10/12/2022

#this chart showing weather patterns will be overlayed on
#several datapoints over this time frame; providing a solid
#base to work from

datW <- read.csv("Z:\\cjachowicz\\data\\PROJECT_DATA\\daily_weather.csv.csv") #read in precip data
head(datW) #hourly precipitation is in inches


datN <- read.csv("Z:\\cjachowicz\\data/")
datN <- read.csv("Z:\\cjachowicz\\data\\Mortality.csv") 
head(datN) 


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
#-------
# Convert to Date format
datW$date <- as.Date(datW$date, format = "%m/%d/%Y")

# Filter between 2019 and 2022
subset_data <- data %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2022-12-31"))

# View result
print(subset_data)