library(ISLR)
library(tree)
attach(Carseats)
hist(Sales)
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)

tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)


set.seed(2)
train=sample(1:nrow(Carseats),200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train) 
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+57)/200

set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
cv.carseats

plot(cv.carseats)

par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")

#apply prune misclass to obtain 9 tree node
prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)

#use predict function to see how well tree performs on test set
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(94+60)/200
#shows 77% of observations were correctly classified

#if we increase value of 'best' we get larget pruned tree
#with lower classification accuracy
prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carsets)
test(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+62)/200

#8.3.2 Fitting Regression Trees
library(MASS)
attach(Boston)
set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston,pretty=0)

#now we use cv.tree function to see if pruning
#will improve performance
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')
#this shows the most complex tree would be selected
#through cross-validation

#shows what happens when we reduce tree to best 5
prune.boston=prune.tree(tree.boston,best=5)
plot(tree.boston)
text(tree.boston,pretty=0)

#we'll keep original cv tree results to make predictions on 
#the test set
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)
#this shows the test set MSE is 25.05, meaning the sqaure root
#MSE is around 5.005 indicating that this model leads to test
#predictions that are within around $5,005 of the true median
#home value.

#8.3.3 Bagging and Random Forests
library(randomForest)
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston
#mtry=13 indicates that all 13 predictors sould be considered for each
#tree split



#how well does this perform on the test set?
yhat.bag=predict(bag.boston,newdata = Boston[-train,])
plot(yhat.bag,boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
#test MSE here is 13.47 - which is almost half of what we had before

#we could grow the the number of trees grown using the ntree argument
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag=predict(bag.boston,newdata = Boston[-train,])
mean((yhat.bag-boston.test)^2)

bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=500)
yhat.bag=predict(bag.boston,newdata = Boston[-train,])
mean((yhat.bag-boston.test)^2)


set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf=predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
#reduces test MSE to 11.31
#importance function gives the importance of each variable used

importance(rf.boston)
varImpPlot(rf.boston)

#addl example
set.seed(101)
train=sample(1:nrow(Boston),300)
rf.boston=randomForest(medv~.,data=Boston,subset=train)
rf.boston
#these are OOB residuals/debiased estimates of predition error

oob.err=double(13)
test.err=double(13)
for(mtry in 1:13){
  fit=randomForest(medv~.,data=Boston,subset=train,mtry=mtry,ntree=400)
  oob.err[mtry]=fit$mse[400]
  pred=predict(fit,Boston[-train,])
  test.err[mtry]=with(Boston[-train,],mean((medv-pred)^2))
  cat(mtry," ")
}

matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue",type="b",ylab="Mean Squared Error"))
legend("topright",legend=c("OOB","Test"),pch=19,col=c("red","blue"))
abline(h=min(test.err),col="red")


#8.3.4 Boosting
library(gbm)
set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution='gaussian',n.trees=5000,interaction.depth=3,shrinkage=0.01)
summary(boost.boston)
#this shows that lstat and rm are by the most important variables

par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")


yhat.boost=predict(boost.boston,newdata=Boston[-train ,],n.trees=5000) 
mean((yhat.boost-boston.test)^2)
#gives similar MSE as earlier results

boost.boston=gbm(medv~.,data=Boston[train,],distribution='gaussian',
                 n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
#this changes alpha value to 0.2
yhat.boost=predict(boost.boston,newdata =Boston[-train ,],n.trees=5000) 
mean((yhat.boost-boston.test)^2)
#gives slightly lower MSE


#addl ex
n.trees=seq(from=100,to=10000,by=100)
predmat=predict(boost.boston,newdata=Boston[-train,],n.trees=n.trees)
dim(predmat)
berr=with(Boston[-train,],apply( (predmat-medv)^2,2,mean))
plot(n.trees,berr,pch=19,ylab="Mean Squared Error",xlab="# Trees",main="Boosting Test Error")
abline(h=min(test.err),col="red")




