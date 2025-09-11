#################################
######  GEOG331 Activity2  ######
#################################

#make a vector of tree heights in meters
heights <- c(30,41,20,22)
#convert to cm
heights_cm <- heights*100
heights_cm

#look at the first tree height
heights[1]

#look at the 2nd and 3rd tree heights
heights[2:3]

#get more info on the matrix function
help(matrix)
#OR do: ?matrix

#set up a matrix with 2 columns and fill in by rows
#first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat

is.matrix(heights)

#set up another matrix, 5 columns and fll by columns
Sec <- matrix(c(9,8,7,6,5,4,3,2,1,0,21,12,31,14,555), ncol=5, byrow = FALSE)
Sec

#set up a matrix that fills in by columns
#first argument is the vector of numbers to fill in the matrix
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol

#subset the matrix to look at row 1, column2
Mat.bycol[1,2]

Mat.bycol[1,1:2]

#look at all values in row 1
Mat.bycol[1,]

#look at all values in column 2
Mat.bycol[,2]

#read in weather station file from your data folder

# Here is my PC file path - note my data folder is on Google Drive, so I can access it from multiple computers
datW <- read.csv("G:\\My Drive\\Documents\\teaching\\GEOG331\\data\\noaa_weather\\2011124.csv",
                 stringsAsFactors = T)

# Here is my Mac file path
#datW <- read.csv("/Volumes/GoogleDrive/My Drive/Documents/teaching/GEOG331/F20/data/noaa_weather/2011124.csv")

