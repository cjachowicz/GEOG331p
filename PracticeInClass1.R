#practice done in class 10/7/2025
#script to see examples of linear regression
#in R using built-in IRIS data

#if you want to find other built-in data to mess around with, look it up online

rm(list = ls())

#subset the virginica species in a new dataframe called "flower"
flower <- iris[iris$Species == "virginica",]

#make a scatter plot to look at sepal length vs petal length
plot(flower$Sepal.Length, flower$Petal.Length, pch = 19,
     xlab = "Sepal Length", ylab = "Petal Length", main = "Iris Virginica")

#graph looks good, seems like we can assume a linear relationship here

#fit a regresion model
fit <- lm(flower$Petal.Length ~ flower$Sepal.Length)
#print(fit) for shorter results
#summary(fit) for more detailed results


#plot the residuals
plot(flower$Sepal.Length, summary(fit)$residuals,pch = 19,
     xlab = "Sepal Length", ylab = "Residuals")
abline(h = 0)

#summary(fit)$residuals in a print function retrieves all te residuals for you

#abline(h=0) creates a horizontal line at h=0=y

#check for normality of distribution
hist(summary(fit)$residuals,col = "turquoise3",
     main = "Residual Distribution", xlab = "Residuals")


#qqnorm or qq line can provide another visual check
qqnorm(summary(fit)$residuals,pch = 19)
qqline(summary(fit)$residuals,pch = 19)

#Use Shapiro wilks test to check normality
shapiro.test(summary(fit)$residuals)

#if p value is less than 0.05, our data set significantly differs from what we would expect
#tells us our data are normally distributed

#pvalue of slope is what we're interested in





