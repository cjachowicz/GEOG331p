library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)

datW <- read.csv("Z:\\cjachowicz\\data\\PROJECT_DATA\\daily_weather.csv.csv") #read in precip data
head(datW) #hourly precipitation is in inches


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
