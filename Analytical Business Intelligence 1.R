# Analytics for Business Intelligence

setwd("D:/ABI Assignments")
Auto <- read.csv("D:/ABI Assignments/Auto.csv")
dim(Auto) # Number of observations and Variables

# Opening the file editer and changing the name of variable to cylinders to cyl
fix(Auto) 
names(Auto) # Changed the name
na.omit(Auto) # Used to omit missing values

#There are no missing values
dim(Auto)
attach(Auto)

# Plot cyl vs. mpg
plot(Auto$cyl, Auto$mpg,
     xlab = "cyl",
     ylab = "mpg")

attach(Auto)
plot(cyl, mpg, main="Scatterplot", 
     xlab="cyl ", ylab="mpg ")

pdf (" Figure.pdf ")
plot(cyl,mpg)
dev.off ()

# Producing Histogram for Horse Power Variable

hist(horsepower, breaks=6, col="blue")

pdf (" Figure 1 .pdf ")
hist(horsepower ,col =" blue ")
dev.off ()

summary(mpg)
summary(acceleration)
library(e1071)
skewness(Auto$mpg)
skewness(Auto$acceleration)

#Observation:
#The data samples are skewed.
#If mean and median does not coincide, it is skewed and not symmetric.
#For mpg,  Media =23.00 < Mean =23.52- It is Right Skewed
#For acceleration,  Media=15.00 < Mean=15.56- It is Right Skewed
#The data samples are Positive skewed which indicates that the mean of the data values is larger than the median, and the data distribution is right-skewed.


#Simple (one variable) linear regression

library(MASS)
fix(Boston)
summary(Boston)


fit <- lm(medv~lstat , data = Boston) # Fitting a Simple linear regression model
summary(fit)

#OBSERVATION

#	p-value is 2.2e-16 which is very less than 0.05, so it is highly significant, P-value is less than 0.05, this will reject the null hypothesis. 
#	The T-statistic value must be greater than 2(or less than -2) which indicates the coefficient is significant with >95% coincidence.
# Tvalue is significant for medv = 61.4 
#T value is significant = -24.41


names(Boston)

# Plotting Scatterplot
attach(Boston)
plot(medv, lstat, main="Scatterplot", 
     xlab="lstat ", ylab="medv ")
abline(lm(medv~lstat), col="red") 


# Multiple Linear Regression

fit <- lm(medv~lstat+age, data = Boston)
summary(fit)

#OBSERVATION:
#R square Value:
#R squares measures the variability of the data is captured by the Model.
#R-squares value = 0.5513(55.13%).
#F-value
#F- statistic value is 309, which is larger than 1 and we can reject null hypothesis. Also it indicates that there would be relation between the response and predictors.
