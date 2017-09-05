library(forecast)
library(datasets)
library(MASS)
library(ISLR)
library(devtools)

data(Boston)
Boston<- data.frame(Boston)
str(Boston)
summary(Boston)

attach(Boston)
lm.fit=lm(medv~lstat)
lm.fit
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval="confidence")
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval="prediction")

plot(lstat,medv)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")

plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)

par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

####################################
#Multiple Linear Regression 3.6.3
####################################

lm.fit=lm(medv~lstat+age)
summary(lm.fit)

#regress medv on all predictors
lm.fit<- lm(medv~.,data=Boston)
summary(lm.fit)

library(car)
#compute variable inflation factor
vif(lm.fit)
#most appear to have low/moderate VIF
#this model uses all vars except age
lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)
#same can be done using update function
lm.fit1<-update(lm.fit,~.-age)
summary(lm.fit1)

#Interaction Terms 3.6.4
summary(lm(medv~lstat*age,data=Boston))
mod.1<-lm(medv~lstat*age,data=Boston)

####################################
#Non Linear Transformations of Predictors 3.6.5
####################################

lm.fit2<- lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

lm.fit<- lm(medv~lstat)
anova(lm.fit,lm.fit2)
#here, model 1 represents a linear submodel containing
#only one predictor, while model 2 corresponds to the larger
#quadratic model that has two predictors, lstat and lstat^2


par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit5<- lm(medv~poly(lstat,5))
summary(lm.fit5)
#^ This suggests that including additional polynomial terms
#up to the 5th order leads to an improvement in the model

summary(lm(medv~log(rm),data=Boston))

####################################
#Qualitative Predictors 3.6.6
####################################

data(Carseats)
Carseats<- data.frame(Carseats)
str(Carseats)
summary(Carseats)
names(Carseats)

lm.fit<- lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)

#Shows dummy coding values
attach(Carseats)
contrasts(ShelveLoc)
contrasts=list(ShelveLoc=contr.treatment(c("Bad","Good","Medium",base=3)))
?contrasts

lm.fit<- lm(Sales~.+Income:Advertising+Price:Age,contrasts = list(ShelveLoc=contr.treatment(c("Bad", "Good", "Medium"), base = 3)),data=Carseats)
summary(lm.fit)
####################################
#Writing Functions 3.6.7
####################################

LoadLibraries=function(){
+ library(ISLR)
+ library(MASS)
+ print("The libraries have been loaded.")}

LoadLibraries

####################################
#Week 2 Lab Excercises 4/8/17
####################################







