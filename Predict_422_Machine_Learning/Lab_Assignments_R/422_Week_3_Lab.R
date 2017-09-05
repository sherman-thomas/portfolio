library(forecast)
library(datasets)
library(MASS)
library(ISLR)
library(devtools)
library(boot)

#THE VALIDATION SET APPROACH
set.seed(3)
train=sample(392,196)
data(Auto)
attach(Auto)

lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
#The estimated test MSE for the linear regression
#fit is 26.14

#we can use the poly() function to estimate the test
#error for the polynomial and cubic regressions
lm.fit2<- lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3<- lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
#these error rates are 19.82 and 19.78 respectively.


#if we choose a different training set instead,
#then we will obtain somewhat different error 
#rates on the validaiton set.
set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2<- lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3<- lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
#Using this split, we find that the valdiation set error
#rates for the linear, quadratic, and cubic terms are
#23.30,18.90 and 19.26 respectively.

################################################
#THE LOOCV APPROACH (Leave-One-Out Cross-Validation)

glm.fit<- glm(mpg~horsepower,data=Auto)
coef(glm.fit)

lm.fit<- lm(mpg~horsepower,data=Auto)
coef(lm.fit)

#library(boot)
glm.fit<- glm(mpg~horsepower,data=Auto)
cv.err<- cv.glm(Auto,glm.fit)
cv.err$delta
#this gives us the cross-validation results
#they happen to be identical (up to 2 decimals)
#in this instance. 

cv.error<-rep(0,6)
for (i in 1:6){
  glm.fit<-glm(mpg~poly(horsepower,i),data=Auto)
cv.error[i]<-cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

################################################
#k-FOLD CROSS-VALIDATION

set.seed(17)
cv.error.10<-rep(0,10)
for (i in 1:10){
  glm.fit<-glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]<-cv.glm(Auto,glm.fit,K=5)$delta[1]
}
cv.error.10

################################################
#boostrap 
data("Portfolio")

alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/var(X)+var(Y)-2*cov(X,Y))
}

alpha.fn(Portfolio,1:100)

set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace = T))

boot(Portfolio,alpha.fn,R=1000)


################################################
#ESTIMATING ACCURACY OF LINEAR REGRESSION MODEL

boot.fn=function(data,index)
return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto,1:392)

set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))

#this gives 1,000 estimates for the intercept and slope terms
boot(Auto,boot.fn,1000)
#this result indicates that the boostrap estimate for the 
#Standard Error (SE) for the intercept (Beta0) is 0.86
#and .0074 for the slope (Beta1).

summary(lm(mpg~horsepower,data=Auto))$coef
#SE estimates are 
#0.717 for intercept
#0.0064 for the slope.

#using bootstrap to fit quadratic model
boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(2)
boot(Auto,boot.fn,1000)

summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef


################################################
#video lec10 r C validation
plot(mpg~horsepower,data=Auto)
##LOOCV
glm.fit=glm(mpg~horsepower,data=Auto)
cv.glm(Auto,glm.fit)$delta

#simple function to use formula(5.2)
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}
loocv(glm.fit)

cv.error=rep(0,5)
plot(mpg~horsepower,data=Auto)
degree=1:5
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d),data=Auto)
  cv.error[d]=loocv(glm.fit)
}
plot(degree,cv.error,type='b')

## 10-fold CV
cv.error10=rep(0,5)
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d),data=Auto)
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type='b',col='red')

##bootstrap
alpha=function(x,y){
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}
alpha(Portfolio$X,Portfolio$Y)

#what is the standard error of alpha?
alpha.fn=function(data,index){
  with(data[index,],alpha(X,Y))
}
alpha.fn(Portfolio,1:10)

set.seed(2)
alpha.fn(Portfolio,sample(1:1000,100,replace = TRUE))
#this will compute 1,000 bootstraps

boot.out=boot(Portfolio,alpha.fn,R=1000)
boot.out
#SE=0.08
plot(boot.out)

############################
#ASSIGNMENT
attach(Auto)
set.seed(3)
train=sample(392,196)
