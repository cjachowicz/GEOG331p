library(lubridate)
library(dplyr)
datH <- read.csv("Z:\\cjachowicz\\data\\hw5_data\\stream_flow_data.csv",
                 na.strings = c("Eqp")) #read in streamflow data
head(datH) #hourly precipitation is in mm
datP <- read.csv("Z:\\cjachowicz\\data\\hw5_data\\2049867.csv")                            
head(datP)#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
datesD <- as.Date(datD$date, "%m/%d/%Y")#convert date and time
datD$doy <- yday(datesD)#get day of year
datD$year <- year(datesD)#calculate year
timesD <- hm(datD$time)#define time
datD$month <- month(datesD)#calculate month

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
datP$doy <- yday(dateP)#get day of year
datP$year <- year(dateP)#get year 

#### get decimal formats #####
datD$hour <- hour(timesD ) + (minute(timesD )/60)#convert time from a string to a more usable format with a decimal hour
datD$decDay <- datD$doy + (datD$hour/24)#get full decimal time
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365)) #calculate a decimal year, but account for leap year
datP$hour <- hour(dateP ) + (minute(dateP )/60)#calculate times for datP  
datP$decDay <- datP$doy + (datP$hour/24)#get full decimal time
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365)) #calculate a decimal year, but account for leap year
         
### QUESTION 2 ###
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

### QUESTION 3 ###
datP_sum <- sum(!is.na(datP))
datH_sum <- sum(!is.na(datH))
totalObservations <- datP_sum + datH_sum
datP_sum / totalObservations
datH_sum / totalObservations
pfreq <- table(datP$HPCP)
hfreq <- table(datH$discharge)
nrow(pfreq)
nrow(hfreq)

### QUESTION 4 ###
par(mai=c(1,1,1,1))
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     col="black",
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

### QUESTION 5 ###
par(mai=c(1,1,1,1))
dat2017 <- subset(datD, year == 2017)
average2017 <- aggregate(discharge ~ doy, dat2017, mean, na.rm = TRUE)
colnames(average2017) <- c("doy","daily2017")
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Month of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     col="blue",
     ylim=c(0,120),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
box(which = "plot", lwd = 1, col = "black")  
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, c(15,45,74,105,135,166,196,227,258,288,319,349), #place labels in middle of each month's range
     lab=month.abb) #shortcut that labels every month (12)
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean of total observations","1 standard deviation", "2017 mean of observations"), #append 2017 average to legend items
       lwd=c(2,NA),#lines
       col=c("blue",rgb(0.392, 0.584, 0.929,.2),"tomato2"),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border
lines(average2017$doy, average2017$daily2017, col="tomato2", lwd=2)
### QUESTION 7 ###
allDischarge <- aggregate(datP$HPCP, 
                          by = list(year = datP$year, doy = datP$doy),
                          FUN = length) #dataframe with all discharge data
colnames(allDischarge) <- c("year","doy","observations")
allDischarge$full24 <- allDischarge$observations == 24 #new col indicating bool(Has 24 observations)
datD <- left_join(datD, allDischarge, by = c("year", "doy"))#apply bool to observations in datD

par(mai=c(1,1,1,1))#plot it
plot(datD$decYear,datD$discharge, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     col = rgb(0.8, 0.15, 0.15,0.6),
     ylim=c(0,525),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
     box(which = "plot", lwd = 1, col = "black")
points(datD$decYear[datD$full24],
       datD$discharge[datD$full24],
     pch=5,
     col="turquoise2")
axis(1, at = pretty(datD$decYear), labels = pretty(datD$decYear))
axis(2, at = pretty(datD$discharge), labels = pretty(datD$discharge), las = 2)
legend("topright",
       legend=c("All discharge","Days with 24 hrs precip data"),
       pch= c(NA, 5),
       lwd = c(2, NA),
       col=c(rgb(0.8, 0.15, 0.15,0.6),"turquoise2"),
       bty="n")
### QUESTION 8 ###
#Second hydrograph, January 13.#subsest discharge and precipitation within range of interest
hydroD2 <- datD[datD$doy >= 13 & datD$doy < 14 & datD$year == 2011,]
hydroP2 <- datP[datP$doy >= 13 & datP$doy < 14 & datP$year == 2011,]
min(hydroD2$discharge)

#get minimum and maximum range of discharge to plot. go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl2 <- floor(min(hydroD2$discharge))-1
#ceiling rounds up to the integer
yh2 <- ceiling(max(hydroD2$discharge))+1
#minimum and maximum range of precipitation to plot
pl2 <- 0
pm2 <-  ceiling(max(hydroP2$HPCP))+.5
#scale precipitation to fit on the graph
hydroP2$pscale <- (((yh2-yl2)/(pm2-pl2)) * hydroP2$HPCP) + yl2

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD2$decDay,
     hydroD2$discharge, 
     type="l", 
     ylim=c(yl2,yh2), 
     lwd=2,
     xlab="Day of year 2011", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP2)){
  polygon(c(hydroP2$decDay[i]-0.017,hydroP2$decDay[i]-0.017,
            hydroP2$decDay[i]+0.017,hydroP2$decDay[i]+0.017),
          c(yl2,hydroP2$pscale[i],hydroP2$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}
### QUESTION 9 ###
library(ggplot2)
datD$yearPlot <- as.factor(datD$year)#specify year as a factor
ggplot(data= datD, aes(yearPlot,discharge)) + geom_boxplot()#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + geom_violin() #make a violin plot

#specify season as a factor
datD$season <- as.factor(cut(datD$month,
                             breaks = c(0, 2, 5, 8, 11, 12),
                             labels = c("Winter", "Spring", "Summer", "Fall", "Winter"),
                             include.lowest = TRUE))

ggplot(data= datD[datD$year==2016,], aes(season,discharge)) + 
  geom_violin() +
  labs(x="Season in 2016", y=expression(paste("Discharge ft"^"3 ","sec"^"-1")))#Violin plot, by season, for 2016
ggplot(data= datD[datD$year==2017,], aes(season,discharge)) + 
  geom_violin()+
  labs(x="Season in 2017", y=expression(paste("Discharge ft"^"3 ","sec"^"-1")))#Violin plot, by season, for 2017