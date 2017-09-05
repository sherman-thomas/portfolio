setwd("C:/Users/herma/Desktop")
library(e1071)
library(ISLR)

#We begin by generating the observations which belong to two classes,
#and checking whether or not the classes are linearly separable.
set.seed(1)
x=matrix(rnorm(20*2),ncol=2)
y=c(rep(-1,10),rep(1,10))
x[y==1,]=x[y==1,]+1
plot(x,col=(3-y))

#They are not. Next, we fit a support vector classifier.
#Note - in order for the svm function to perform classification
#we must encode the resonse as a factor variable. We now
#create a data frame with the response coded as a factor.
dat=data.frame(x=x,y=as.factor(y))
svmfit=svm(y~.,data=dat,kernel="linear",cost=10,scale=FALSE)
#the argument scale=FALSE tells the svm function not to scale
#each feature to have mean zero or SD one; depending on the application
#we might want to use scale=TRUE

plot(svmfit,dat)
#NOTE - the second feature is plotted on the x-axis and second is 
#plotted on the y (in contrast to the normal plot function arg)
#The decision boundary betwen the two classes in linear, though
#due to the way in which the plotting function is implemented
#in this library the decision boundary looks somewhat jagged
#in the plot. In this case, we see only one observation is 
#misclassified. 

svmfit$index
summary(svmfit)
#this tells us that a linear kernel was used with cost=10, 
#and there were seven support vectors, four in one class and 
#three in the other.

#What happens if we instead use a smaller value of the cost parameter?
svmfit=svm(y~.,data=dat,kernel='linear',cost=0.1,scale=FALSE)
plot(svmfit,dat)
svmfit$index
summary(svmfit)
#now that a smaller value of cost is used, we obtain a larger number
#of support vectors because the margin is now wider.

#the e1071 lib also includes the tune() function, to perform CV.
#by default, tune() performs ten-fold CV on a set of models of interest.
#The following command indicates that we want to compare SVMs
#with a linear kernel, using a range of values of the cost parameter.
set.seed(1)
tune.out=tune(svm,y~.,data=dat,kernel="linear",
              ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
#we can see that cost=0.1 results in the lowest CV error rate.
#the tune funciton stores the best model obtained, which can 
#be accessed as follows.
bestmod=tune.out$best.model
summary(bestmod)

#the predict function can be used to predict the class label on a 
#set of test observations, at any given value of the cost param.
#we begin by generating a test data set.
xtest=matrix(rnorm(20*2),ncol=2)
ytest=sample(c(-1,1),20,rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,]+1
testdat=data.frame(x=xtest,y=as.factor(ytest))
ypred=predict(bestmod,testdat)
table(predict=ypred,truth=testdat$y)
#Thus, with this value of cost, 19 of the test obs are correctly
#classified. What if we used cost=0.01?
svmfit=svm(y~.,data=dat,kernel='linear',cost=0.01,scale=FALSE)
ypred=predict(svmfit,testdat)
table(predict=ypred,truth=testdat$y)
#in this case, one additional obs is misclassified.

#Now we consider a situation where the two classes are linearly separable
#First, we further separate the two classes in our simulated data set
#so that they are linearly separable.
x[y==1,]=x[y==1,]+0.5
plot(x,col=(y+5)/2,pch=19)

dat=data.frame(x=x,y=as.factor(y))
svmfit=svm(y~.,data=dat,kernel='linear',cost=1e5)
summary(svmfit)
plot(svmfit,dat)
#No training errors were made and only 3 support vectors were used.
#However, we can see from the figure that the margin is very narrow
#because the obs that are not support vectors, indicated as circles,
#are very close to the decision boundary. It seems likely this model
#would perform poorly on the testdata. 

#We'll try a smaller value for cost.
svmfit=svm(y~.,data=dat,kernel='linear',cost=1)
summary(svmfit)
plot(svmfit,dat)
#Although we misclassify one training observation, we obtain a much 
#wider margin and make use of seven support vectors. This model 
#would probably peform better than the model with cost=1e5.

#9.6.2 SUPPORT VECTOR MACHINES

set.seed(1)
x=matrix(rnorm (200*2) , ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150 ,]=x[101:150,]-2
y=c(rep(1,150) ,rep(2,50))
dat=data.frame(x=x,y=as.factor(y))

plot(x,col=y)


train=sample(200,100)
svmfit=svm(y~., data=dat[train,], kernel ="radial", gamma=1,
           cost=1)
plot(svmfit, dat[train,])
summary(svmfit)
#We can see from the figure that there are a fair number of training
#erros in this sVM fit. If we increase cost, we can reduce 
#the number of training errors, however this might create a more
#irregular decision boundary that seems to be at risk of 
#overfitting the data.

svmfit=svm(y~., data=dat[train,], kernel ="radial", gamma=1,
           cost=1e5)
plot(svmfit, dat[train,])
summary(svmfit)

#We can perform CV using tune() to select the best choice of gamma 
#and cost.
set.seed(1)
tune.out=tune(svm,y~., data=dat[train,], kernel ="radial",
                ranges=list(cost=c(0.1,1,10,100,1000),
                            gamma=c(0.5,1,2,3,4)))
summary(tune.out)
#This tells us the best choice of parameters involves 
#cost=1 and gamma=2. Notice when we view the test set predictions
#for this model, we subset the dataframe dat using -train as
#an index set.
table(true=dat[-train,"y"],pred=predict(tune.out$best.model,
      newdata=dat[-train,]))
#This shows us that 10% of the observations are misclassified.

#9.6.3 ROC CURVES
library(ROCR)
rocplot=function(pred,truth, ...){
  predob=prediction(pred,truth)
  perf=performance(predob,"tpr","fpr")
  plot(perf,...)}

svmfit.opt=svm(y~., data=dat[train,], kernel ="radial",gamma=2,
               cost=1, decision.values =T)
fitted=attributes(predict(svmfit.opt,dat[train,],
                          decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rocplot(fitted,dat[train,"y"],main="Training Data")
#SVM appears to be producing accurate predictions, but we can increase
#gamma to produce a more flexible fit and generate further improvements.
svmfit.flex=svm(y~., data=dat[train,], kernel ="radial",gamma=50,
                cost=1, decision.values =T)
fitted=attributes(predict(svmfit.flex ,dat[train,],
                            decision.values=T))$decision.values
rocplot(fitted,dat[train,"y"],add=T,col="red ")
#However, these ROC curves are all on training data. We're really
#more interested in the level of accuracy with the test data.
#When we compute the ROC curves on the test data, the model with 
#gamma=2 appears to provide the most accurate results. 

fitted=attributes(predict(svmfit.opt,dat[-train,],
                            decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"], main="Test Data")
fitted=attributes(predict(svmfit.flex,dat[- train,],
                            decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red")


#9.6.4 SVM WITH MULTIPLE CLASSES
#Example exploring multi-class classification. This uses a third class.
set.seed(1)
x=rbind(x, matrix(rnorm (50*2), ncol=2))
y=c(y,rep(0,50))
x[y==0,2]= x[y==0,2]+2
dat=data.frame(x=x,y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))

#we know fit SVM to the data:
svmfit=svm(y~., data=dat,kernel='radial',cost=10,gamma=1)
plot(svmfit,dat)

#******************************************
#we can peform support vector regression by setting the function 
#argument to numerical rather than factor - FYI
#******************************************


#9.6.5 Application to Gene Expression Data
library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)

table(Khan$ytrain)
table(Khan$ytest)

dat=data.frame(x=Khan$xtrain,y=as.factor(Khan$ytrain))
out=svm(y~.,data=dat,kernel='linear',cost=10)
summary(out)
table(out$fitted,dat$y)
#we see there are no training errors. This is not surprsing because
#the large number of variables relative to the number of obs
#implies that it is easy to find hyperplanes that fully separate 
#classes. Again we're more interest in what our accuracy looks like 
#with the test data set.
dat.te=data.frame(x=Khan$xtest,y=as.factor(Khan$ytest))
pred.te=predict(out,newdata=dat.te)
table(pred.te,dat.te$y)
#This shows that using cost=10 yields two test set errors.



