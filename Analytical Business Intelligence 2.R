# Analytics for Business Intelligence

# Regression with Interaction

library(MASS)
data("Boston")
head(Boston)

fix(Boston)
names(Boston)

# Interaction Between lstat and Black
summary(lm(medv~lstat:black, data=Boston))

# Interaction Between lstat and Age
summary(lm(medv~lstat*age, data = Boston))

dim(Boston) #Checking Observations and Variables

#  Multivariate Regression with Interaction

lm.fit =lm(medv~lstat*age, data = Boston)
attach(Boston)
lm.fit= lm(medv~lstat*age, data = Boston)
summary(lm.fit)

# Yes, The interaction term is significant here because the p-value is very small (0.0252) and less than 0.05.
# Yes, the answer is different for confidence probability 5% and 1% because P-value is very small so it rejects null hypothesis. If P-value is greater than 5% we accept null hypothesis.

#Multiple linear Regression without Intercation

lm.fit = lm(medv~lstat+age , data = Boston)
summary(lm.fit)

# P-value is drastically changing if we put interaction effect in the model.
# Without Interaction: - On Individual Basis, the lstat will still reject null hypothesis, but age will have it changed from being rejected without the interaction variable to being accepted with the interaction variable. 
# The Residual Standard Error for Model 1 with Interaction - 6.149 on 502 degrees of freedom
# the Residual standard error for Model 2 without Interaction - 6.173 on 503 degrees of freedom.  As we can see that the Residual standard error for both the models are same, although the model without interaction will perform better but there will not be any significant impact on the model.

#Non linear Transform of Predictors

lm.fit1 = lm(medv~lstat + I(lstat^2))
summary(lm.fit1)


# Single regression of medv against lstat
lm.fit = lm(medv~lstat, data = Boston)
summary(lm.fit)

#Adjusted R-square for linear model is 0.5432 and for Quadratic model is 0.6393.
# In the Quadratic model we see the adjusted R-squared value of 0.6393 is higher than the adjusted R-squared value of 0.5432 in linear model. That means, introduction of a new predictor term lstat^2 improves the model more than would be expected by chance. Also, since the value 0.6393 in quadratic model is closer to 1 and, 0.5432 in linear model is closer to 0, this indicates that the Quadratic model has a better fit

# Using Annova function to compare 2 models
lm.fit = lm(medv~lstat)
anova(lm.fit, lm.fit1)

#Quadratic Fit is superior model than linear fit because F-statstic is 135.2 and the associated p-value is highly significant.

#Classification Logistic Regression

library(ISLR)
names(Smarket)
summary(Smarket)
dim(Smarket)

#Checking the Corelation

cor(Smarket [ ,-9])

#There is a substantial correlation between Volume and Year.
#By plotting the graph we can see that the volume is increasing over time.


library(ggplot2)
plot(Smarket$Year, Smarket$Volume)++abline(lm(Volume~Year, data=Smarket))

library(ISLR)
names(Smarket)
summary(Smarket)
dim(Smarket)
data("Smarket")
attach(Smarket)
plot(Volume)
head(Smarket)

#Logistic regression Model

glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume , data = Smarket , family = binomial)
summary(glm.fit)

# The negative coefficient for this predictor suggests that if the market had a positive return yesterday, then it is less likely to go up today.
# The smallest p-value here is associated with Lag1.
# At a value of 0.15, the p-value is still relatively large, and so there is no clear evidence of a real association between Lag1and Direction.


#Coefficient

coef(glm.fit)
summary(glm.fit)$coef
#The results for the coefficients for the model are consistent with the previous step as the values of the coefficients are same.

glm.probs = predict(glm.fit , type = "response")
glm.probs[1:10]

contrasts(Direction) # Indicating Dummy Variables

glm.pred=rep("Down",1250)
glm.pred[glm.probs > 0.5]= "Up"

fix(Smarket)

table(glm.pred , Direction)

(507+145)/1250


mean(glm.pred==Direction)

train =(Year<2005)
Smarket.2005= Smarket [!train ,]
dim(Smarket.2005)

Direction.2005= Direction[!train]
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,
            data=Smarket, family =binomial,subset =train)

glm.probs=predict(glm.fit, Smarket.2005,type="response")
glm.pred=rep ("Down",252)

glm.pred[glm.probs >0.5]="Up"

table(glm.pred, Direction.2005) 

mean(glm.pred==Direction.2005)
mean(glm.pred!= Direction.2005)