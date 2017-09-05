library(ISLR)
attach(Wage)

#POLYNOMIAL AND STEP FUNCTIONS
fit=lm(wage~poly(age,4),data=Wage)
coef(summary(fit))
#The function above returns a matrix whose columns are a basis of orthogonal
#polynomials, which essentially means that each column is a linear
#combination of the variables age, age^2, age^3 and age^4.

fit2=lm(wage~poly(age,4,raw=T),data=Wage)
coef(summary(fit2))
#Here we can also use poly() to obtain age, age^2, age^3 and age^4
#directly, if we prefer. We can do this by using the raw=TRUE argument to
#the poly() function. Later we see that this does not affect the model in a
#meaningful way—though the choice of basis clearly affects the coefficient
#estimates, it does not affect the fitted values obtained.

fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(fit2a)
#This simply creates the polynomial basis functions on the fly, taking care
#to protect terms like age^2 via the wrapper function I() 
#(the ^ symbol has wrapper a special meaning in formulas).

fit2b=lm(wage~cbind(age,age^2,age^3,age ^4),data=Wage)
#this does the same as the above but more compactly,
#using the cbind funciton for building the model matrix
#from a collection of vectors. cbind serves as the wrapper

agelims =range(age)
age.grid=seq(from=agelims [1],to=agelims [2])
preds=predict (fit ,newdata =list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit +2*preds$se.fit ,preds$fit -2*preds$se.fit)
#This grid of values for age at which we want predictions, and
#then call the generic predict() function, specifying that we want standard
#errors as well.

par(mfrow=c(1,2),mar=c(4.5,4.5,1,1) ,oma=c(0,0,4,0))
plot(age ,wage ,xlim=agelims ,cex =.5,col=" darkgrey ")
title(" Degree -4 Polynomial ",outer=T)
lines(age.grid ,preds$fit ,lwd=2,col="blue")
matlines (age.grid ,se.bands ,lwd=1, col=" blue",lty=3)

preds2=predict(fit2,newdata =list(age=age.grid),se=TRUE)
max(abs(preds$fit -preds2$fit))

fit.1=lm(wage~age ,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)

coef(summary(fit.5))

fit=glm(I(wage >250)~poly(age,4),data=Wage, family=binomial)
#this fits the polynomial logistic regression model.
#response becomes binary (if wages is >250, yhat=1)

preds=predict(fit,newdata=list(age=age.grid),se=T)
pfit=exp(preds$fit )/(1+exp(preds$fit ))
se.bands.logit = cbind(preds$fit + 2*preds$se.fit,preds$fit - 2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

preds=predict(fit,newdata =list(age=age.grid),type=" response ",se=T)
#if we do type=response, it directly gives probabilities
#however, corresponding confidence intervals would result in
#negative probabilities

plot(age ,I(wage >250),xlim=agelims ,type="n",ylim=c(0,.2))
points(jitter(age), I((wage >250)/5),cex=.5,pch ="|",col="darkgrey ")
lines(age.grid,pfit,lwd=2, col ="blue")
matlines(age.grid,se.bands,lwd=1, col=" blue",lty=3)

table(cut(age,4))
#in order to fit a step function, this picks 4 cutpoints

################################################################
#SPLINES
library(splines)
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred=predict(fit,newdata =list(age=age.grid),se=T)
plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit + 2*pred$se,lty="dashed")
lines(age.grid,pred$fit - 2*pred$se,lty="dashed")
#The bs() function generates the entire matrix of basis
#functions for splines with a specified set of knots

dim(bs(age,knots=c(25,40,60)))
dim(bs(age,df=6))
attr(bs(age,df=6),"knots")
#by default, the bs() function uses a degree of 3, but 
#here we've split this into 6 different degrees/cubic functions.

fit2=lm(wage~ns(age,df=4),data=Wage)
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid,pred2$fit,col="red",lwd=2)

#in order to fit a smoothing spline, we use
#smooth.spline() in the following code:
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Spline")
fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline (age,wage,cv=TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend ("topright",legend=c("16 DF","6.8 DF"),
        col=c("red","blue"),lty=1,lwd=2, cex =.8)

#This shows what's needed to perform local regression
#and uses the loess() function
plot(age ,wage ,xlim=agelims ,cex =.5,col=" darkgrey ")
title("Local Regression ")
fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)
lines(age.grid,predict (fit,data.frame(age=age.grid)),
        col="red",lwd=2)
lines(age.grid ,predict (fit2,data.frame(age=age.grid)),
      col="blue",lwd=2)
legend ("topright ",legend=c("Span=0.2","Span=0.5"),
        col=c("red","blue"),lty=1,lwd=2,cex=.8)
#this shows a local linear regression using spans of 
#0.2 and 0.5: IE each neighborhood consists of 20% or 50%
#of the observations. The larger the span, the smoother
#the fit. 
#locfit lib can also be used to fit local reg models.


###############################################################
#GAM'S
#below, we predict wage using natural spline functions of year and age,
#treating education as a qualitative predictor. Since this is just a 
#big linear regression model, we can just us the lm() function.
gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)

library(foreach)
library(gam)
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)

par(mfrow=c(1,3))
plot(gam.m3, se=TRUE,col =" blue")
plot.gam(gam1, se=TRUE, col="red") 
#plot.gam can be used on gam1, even though it's a linear model

gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")
#these results show that a GAM with a linear function 
#of year is better than a GAM that doesn't include year.
#However, there is no evidence that a non-linear function of 
#year is needed (p-value of .3485). These ANOVA results indicate
#that M2 would be the preferred model.

summary(gam.m3)
#the p values for year and age corresond to a null hypothesis
#of a linear relationship verses the alternative, non-linear relationship.
#these results reinfornce that a linear function is 
#adequate for this term. However, there is clear evidence 
#that a lon-linear terms is required for age.

#make predictions on the training set
preds=predict(gam.m2,newdata=Wage)
#then, we can use the local regression fits as building blocks
#in a GAM, using the lo() function.
gam.lo=gam(wage~s(year,df=4)+lo(age,span =0.7)+education,data=Wage)
plot.gam(gam.lo, se=TRUE, col=" green")

gam.lo.i=gam(wage~lo(year,age,span=0.5)+education,data=Wage)
library(akima)
plot(gam.lo.i)

#again, in order to fit the logistic regression GAM, we need to
#use the I() function in constructing the binary response, 
#using family=binomial.
gam.lr=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green ")

table(education,I(wage>250))
#since this shows that there are no high earners in the <hs catergory,
#we can fit the log reg GAM by using all but this category.
gam.lr.s=gam(I(wage >250)~year+s(age,df=5)+education,family=binomial,data=Wage,
             subset=(education!="1.< HS Grad"))
plot(gam.lr.s,se=T,col="green")

################################################################
#
################################################################
#
################################################################
#
