# install.packages("devtools")
#devtools::install_github("r-lib/conflicted")

#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
dplyr::filter()
dplyr::lag()
library(tidyverse)
#mask every conflict with dplyr packages
dplyr::filter()
dplyr::lag()
#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

# hint: consider using a list, and also new vectors for regression variables

#irisVersicolor <- data.frame(Species = c("versicolor")
#print(irisVersicolor)

#calculate necessary measurements for easy future use
iris_sepal_length <- iris$Sepal.Length
iris_sepal_width <- iris$Sepal.Width
iris_petal_length <- iris$Petal.Length
iris_petal_width <- iris$Petal.Width

#create a list f equations to put into the regression command within for loop
forlooplist <- list(
    iris_sepal_length ~ iris_sepal_width,
    iris_petal_length ~ iris_petal_width,
    iris_sepal_length ~ iris_petal_length
)
#for-loop creating every regression table
for (duet in forlooplist){
  model <- lm(duet, data = iris)
  print("Model Summary: ")
  print(duet)
  summary(model)
}
#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

virginica <- iris[iris$Species == "virginica",]
setosa <- iris[iris$Species == "setosa",]
versicolor <- iris[iris$Species == "versicolor",]

filter(iris, setosa$Sepal.Length < 100)

#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
new_plot <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width))

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
new_plot + theme_classic() + geom_point()

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length
fancy <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Length))

fancy + labs(x = "Petal length (cm)",
                  y = "Petal width (cm)",
                  title = "Petal length and width by species")
fancy + scale_radius()

fancy + theme_classic() + geom_point()

fancy <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = "red", size = Petal.Length))
fancy + theme_classic() + geom_point() + scale_color_manual(values = c("red" = "turquoise2"))





#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		

