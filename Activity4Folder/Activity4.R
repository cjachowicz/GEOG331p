head(iris)
install.packages(c("tidyverse"))
library(tidyverse)
devtools::install_github("r-lib/conflicted")
#use built in iris dataset and take a look at it 
head(iris)
#load in some tidyverse packages
library(tidyverse)
#mask every conflict with dplyr packages
dplyr::filter()
dplyr::lag()

#####################################
##### Part 1: for loops         #####
#####################################
#Using only data for iris versicolor write a for loop that produces 
#a regression table for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

#assign necessary measurements name for easy future use
iris_sepal_length <- iris$Sepal.Length
iris_sepal_width <- iris$Sepal.Width
iris_petal_length <- iris$Petal.Length
iris_petal_width <- iris$Petal.Width
#create a list of equations to put into the regression command within for loop
forlooplist <- list(
    iris_sepal_length ~ iris_sepal_width,
    iris_petal_length ~ iris_petal_width,
    iris_sepal_length ~ iris_petal_length
)
#list to put regression tables in
reg_list <- list()
#for-loop creating every regression table
for (duet in 1:length(forlooplist)){
  model <- lm(duet, data = iris)
  reg_list[[duet]] <- model
}
#####################################
##### Part 2: data in dplyr     #####
#####################################
#use dplyr to join data of maximum height to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))
#dplyr joins height variables to the leftmost columns of the entire iris dataset
iris_height <- dplyr::left_join(iris, height)

#####################################
##### Part 3: plots in ggplot2  #####
#####################################
#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + theme_classic() + geom_point()

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length
fancy <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species, size = Petal.Length))
fancy + theme_classic() + geom_point() + labs(x = "Sepal length (cm)",y = "Sepal width (cm)", title = "Sepal length by Sepal width for different species")
#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		