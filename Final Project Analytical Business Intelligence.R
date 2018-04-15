#Final Project: Analytical Business Intelligence 
# Submitted By: Jayendra Bhardwaj
# RUID: 181006372

#Support Vector Classifier

set.seed(6372)
#generating Random Numbers and using command Matrix to generate two sets of data
x <- matrix(rnorm(20*2), ncol=2) 
y <- c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
plot(x, col=(3-y))


#SVM Classification Plot
library(e1071)
dat <- data.frame(x=x, y=as.factor(y))
svm.fit<- svm(y ~., data=dat, kernel = 'linear', cost=0.1, scale=FALSE)
plot(svm.fit, dat)

summary(svm.fit) #Identites of Support Vector

svm.fit$index

#The summary lets us know there were 18 support vectors which are {1  2  3  4  5  6  8  9 10 11 12 13 14 15 16 17 18 19}, 
#9 in the first class and 9 in the second

# Increase number of cost parameter to 10
dat <- data.frame(x=x, y=as.factor(y))
svm.fit1 <- svm(y ~., data=dat, kernel='linear', cost=10, scale=FALSE)
plot(svm.fit1, dat)

summary(svm.fit1)
svm.fit1$index

#The summary lets us know there were 18 support vectors which are {1  2  3  4  9 11 16 17 19}, 
#5 in the first class and 4 in the second

#Comparing SVM with Linear Kernel
set.seed(6372)

tune.out <- tune(svm, y ~., data=dat, kernel='linear',
                 ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)

#The best cost is 1 for the output.
# best performance: 0.25

# Getting the best Model

bestmod = tune.out$best.model
summary(bestmod)
#Here we see that cost= 1 results in the lowest cross-validation error rate. 

#Generating the Test data

xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest [ ytest ==1 ,]= xtest [ ytest ==1 ,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))
yhat <- predict(tune.out$best.model, testdat)
#install.packages("caret")
library(caret)
confusionMatrix(yhat, testdat$y)

#consider a situation in which the two classes are linearly separable
x[y==1 ,]= x[y==1 ,]+01
plot(x, col =(y+5) /2, pch =19)

# Fiting the support vector classifier and plotting the hyperplane
dat=data.frame(x=x,y=as.factor (y))
svmfit =svm(y~ ., data=dat , kernel ="linear", cost =1e5)
summary (svmfit)
plot(svmfit , dat)


#Generating the data with a non-linear class boundary
set.seed (6372)
x=matrix (rnorm (200*2) , ncol =2)
x[1:100 ,]=x[1:100 ,]+2
x[101:150 ,]= x[101:150 ,] -2
y=c(rep (1 ,150) ,rep (2 ,50) )
dat=data.frame(x=x,y=as.factor (y))
plot(x, col=y)

#Fiting the training data
train=sample (200 ,100)
svmfit =svm(y~., data=dat [train, ], kernel ="radial", gamma =1,cost =1)
plot(svmfit , dat[train ,])

set.seed (6372)
tune.out=tune(svm , y~., data=dat[train ,], kernel ="radial", ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000), gamma=c(0.5,1,2,3,4) ))
summary(tune.out)
#Therefore, the best choice of parameters involves cost=1 and gamma=0.5

#Percentage of Misclassified objects
yhat <- predict(tune.out$best.model, dat[-train,])
confusionMatrix(yhat, dat[-train, 'y'])
#The optimal values of cost is 1 and gamma=0.5. Percentage of misclassified objects is 10 percent.


#Decision Trees for Classification
library (tree)
library (ISLR)
attach (Carseats)
View(Carseats)
High=ifelse (Sales <=8," No"," Yes ")
Carseats =data.frame(Carseats ,High)
tree.carseats =tree(High~.-Sales ,Carseats )
summary (tree.carseats )
#Plotting the tree carseats
plot(tree.carseats )

tree.carseats

#Evaluating the performance of Classification
set.seed (6372)
train=sample (1: nrow(Carseats), 200)
Carseats.test=Carseats [-train ,]
High.test=High[-train ]
tree.carseats =tree(High~.-Sales ,Carseats ,subset =train )
tree.pred=predict (tree.carseats ,Carseats.test ,type ="class")
table(tree.pred ,High.test)

#Optimal number of leaves
set.seed (6388)
cv.carseats =cv.tree(tree.carseats ,FUN=prune.misclass )
names(cv.carseats)
cv.carseats

#Plotting the error rate
par(mfrow =c(1,2))
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
plot(cv.carseats$k ,cv.carseats$dev ,type="b")

#Applyting the prune.misclass

prune.carseats =prune.misclass(tree.carseats,best =9)
plot(prune.carseats)
text(prune.carseats,pretty=0)

