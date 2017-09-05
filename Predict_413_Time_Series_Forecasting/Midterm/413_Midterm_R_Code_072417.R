setwd("C:/Users/herma/Desktop/413/Midterm")
library(MASS)
library(lattice)
library(ggplot2)
library(corrplot)
library(rmarkdown)
library(rticles)
library(caret)
library(randomForest)
library(rpart)
library(RColorBrewer)
library(tidyverse)
library(PerformanceAnalytics)
library(usdm)
library(class)
library(bestglm)
library(splines)
library(tree)
library(leaps)
library(survival)
library(car)
library(ResourceSelection)

mydata <- read.csv('train.csv') 

mydata$ID<- mydata$X
mydata$recency<-mydata$Months.since.Last.Donation
mydata$frequency<- mydata$Number.of.Donations
mydata$volume<- mydata$Total.Volume.Donated..c.c..
mydata$time<- mydata$Months.since.First.Donation
mydata$donr<- mydata$Made.Donation.in.March.2007

mydata<-mydata[,7:12] 
head(mydata)
str(mydata)
summary(mydata)

############
#EDA
hist(donr)
table(donr)
#23.96% actually donate

kdepairs(mydata[,-c(1)])
pairs(mydata[,-c(1)])

par(mfrow=c(2,2))

hist(recency) #zero-inflated
hist(frequency) #zero-inflated
hist(volume)
hist(time)

boxplot(recency,main="Recency")
boxplot(frequency,main="Frequency")
boxplot(volume,main="Volume")
boxplot(time,main="Time")


cor.test(mydata$recency,mydata$donr)
cor.test(mydata$frequency,mydata$donr)
cor.test(mydata$time,mydata$donr)
cor.test(mydata$rate,mydata$donr)
cor.test(mydata$ratio,mydata$donr)

vif(mydata)

###############
# Data Preparation
mydata$rate<- mydata$frequency/mydata$time
mydata$elapsed<- mydata$time-mydata$recency
mydata$ratio<- mydata$recency/mydata$frequency
#mydata$failure<- mydata$time-mydata$frequency
#mydata$hazard<- mydata$failure/mydata$time
#mydata$log_time<- log(mydata$time)

pairs(mydata[,-c(1,4,6)])

#########
#PREP 2

mydata$recency[mydata$recency<=2] <- 5
mydata$recency[mydata$recency >2 & mydata$recency <=7] <- 4
mydata$recency[mydata$recency > 7 & mydata$recency <=10] <- 3
mydata$recency[mydata$recency > 10 & mydata$recency <=14] <- 2
mydata$recency[mydata$recency >14] <-1
  
mydata$frequency[mydata$frequency <=1] <- 1
mydata$frequency[mydata$frequency >1 & mydata$frequency<=2] <- 2
mydata$frequency[mydata$frequency > 2 & mydata$frequency<=3] <- 3
mydata$frequency[mydata$frequency >3 & mydata$frequency<=4] <- 4
mydata$frequency[mydata$frequency >=5] <- 5

#####
cor.test(mydata$rate,mydata$donr)
cor.test(mydata$ratio,donr)
cor.test(recency,donr)
cor.test(recency,time)
cor.test(frequency,recency)
cor.test(mydata$elapse,mydata$donr)



###################################################
set.seed(1)
test=sample(576,176)
train=mydata[-test,]
data.train<- train[,-c(1,4)]
x.train<- data.train[,-c(4)]
c.train<- data.train[,4] #donr
n.train.c<- length(c.train) #400
y.train <- data.train[c.train==1,4] #observations with donr=1
n.train.y <- length(y.train) #101
#25.25% response rate

# Validation Set
set.seed(1)
train=sample(576,400)
test=mydata[-train,]
data.valid <-  test[,-c(1,4)]
x.valid <- data.valid[,-c(4)]
c.valid <- data.valid[,4] # donr
n.valid.c <- length(c.valid) # 176
y.valid <- data.valid[c.valid==1,4] # observations with donr=1
n.valid.y <- length(y.valid) # 48
#27.27% response rate

#x.train.mean <- apply(x.train, 2, mean)
#x.train.sd <- apply(x.train, 2, sd)
#x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
#apply(x.train.std, 2, mean) # check zero mean
#apply(x.train.std, 2, sd) # check unit sd
#data.train.std.c <- data.frame(x.train.std, donr=c.train)

#x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
#data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr

#x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
#data.test.std <- data.frame(x.test.std)

#####################################
par(mfrow=c(1,1))
#Set Function for Model Evaluation
LogLoss <- function(actual, predicted, eps=0.00001) {
  predicted <- pmin(pmax(predicted, eps), 1-eps)
  -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
}
#####################################
# Classifcaiton Model Development
# 1) Logistic Models
model.log.1 <- glm(donr ~ recency+frequency*time+ratio,data.train,family=binomial("logit"))
summary(model.log.1)

yhat=model.log.1$fitted.values
LogLoss(c.train,yhat)
#0.4926084

model.log.2 <- glm(donr ~ recency+frequency*time+ratio,data.valid,family=binomial("logit"))
summary(model.log.2)

yhat=model.log.2$fitted.values
LogLoss(c.valid,yhat)
#0.5044102


########################################
#BAGGED MODEL

tree.data.train = data.train
tree.data.train$donr = as.numeric(tree.data.train$donr)
tree.data.valid = data.valid
tree.data.valid$donr = as.numeric(tree.data.valid$donr)
# Fit tree and test.
tree.donr = tree(donr ~., tree.data.train)
plot(tree.donr)
text(tree.donr, pretty=0)
tree.pred = predict(tree.donr, tree.data.valid)
table(tree.pred, c.valid)
treeError <- mean(tree.pred != c.valid)
treeError

set.seed(1) # For reproducible results
bag.donr = randomForest(donr ~ ., data=tree.data.train, mtry=5, 
                        importance=TRUE)
bag.pred = predict(bag.donr, newdata=tree.data.valid)
LogLoss(bag.pred,c.valid)
#2.212502

########
#3) Tree===BAGGING
tree.data.train = data.train
#tree.data.train$donr = as.factor(tree.data.train$donr)
tree.data.valid = data.valid
#tree.data.valid$donr = as.factor(tree.data.valid$donr)
# Fit tree and test.
tree.donr = tree(donr ~., tree.data.train)
plot(tree.donr)
text(tree.donr, pretty=0)
tree.pred = predict(tree.donr, tree.data.valid)
table(tree.pred, c.valid)
LogLoss(c.valid,tree.pred)
#0.4750621

# 3) RF/Bagging
set.seed(1) # For reproducible results
bag.donr = randomForest(donr ~ ., data=data.train, mtry=5, 
                        importance=TRUE, type="probability")
plot(bag.donr)
bag.pred = predict(bag.donr, newdata=data.train)
LogLoss(c.train,bag.pred)
#0.2456265

bag.pred = predict(bag.donr, newdata=data.valid)
table(bag.pred, c.valid)
LogLoss(c.valid,bag.pred)
#0.249752

#4) Boosting
library(gbm)
set.seed(1)
boost.donr = gbm(donr ~ ., data = data.train, distribution = "bernoulli",
                 n.trees = 5000, interaction.depth = 4)
summary(boost.donr)

boost.probs = predict.gbm(boost.donr, newdata = data.train,
                          n.trees = 5000, type = "response")
LogLoss(c.train,boost.probs)
#0.3905953

boost.donr = gbm(donr ~ ., data = data.valid, distribution = "bernoulli",
                 n.trees = 5000, interaction.depth = 4)
summary(boost.donr)

boost.probs = predict.gbm(boost.donr, newdata = data.valid,
                          n.trees = 5000, type = "response")

table(boost.probs, c.valid)
LogLoss(c.valid,boost.probs)
#0.3331708



#######
# Test Set
data.test <- read.csv('test.csv')
data.test$recency<-data.test$Months.since.Last.Donation
data.test$frequency<- data.test$Number.of.Donations
data.test$volume<- data.test$Total.Volume.Donated..c.c..
data.test$time<- data.test$Months.since.First.Donation
data.test$rate<- data.test$Number.of.Donations/data.test$Months.since.First.Donation
data.test$ratio<- data.test$Months.since.Last.Donation/data.test$Months.since.First.Donation
data.test$elapsed<- data.test$Months.since.Last.Donation-data.test$Months.since.First.Donation
n.test <- dim(data.test)[1] # 200

boost.donr = gbm(donr ~ ., data = data.train, distribution = "bernoulli",
                 n.trees = 5000, interaction.depth = 4)
boost.pred = predict.gbm(boost.donr, data.test,
                          n.trees = 5000, type = "response")

Made.Donation.in.March.2007<- boost.pred

sub<- cbind(data.test$X,Made.Donation.in.March.2007)

write.csv(sub,file="sherman_413_nw.csv",row.names = FALSE)
#.5005



bag.donr = randomForest(donr ~ ., data=data.train, mtry=5, 
                        importance=TRUE, type="probability")

bag.pred = predict(bag.donr, newdata=data.test)
Made.Donation.in.March.2007<- bag.pred

sub2<-cbind(data.test$X,Made.Donation.in.March.2007)
write.csv(sub2,file="sherman_413_nw2.csv",row.names = FALSE)
