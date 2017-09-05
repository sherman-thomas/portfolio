setwd("C:/Users/herma/Desktop/422")
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


charity <- read.csv('charity.csv') # load the "charity.csv" file

#####################################################
#set up classification set
deletevars<- names(data.train) %in% c("ID","damt","part")
data.train.class<- data.train[!deletevars]
table(data.train.class$donr)
names(data.train.class)

#####################################################
#set up prediction set
deletevars.pred<- names(data.train)  %in% c("ID","donr","part")
data.train.pred<- data.train[!deletevars.pred]
names(data.train.pred)

#####################################################
#Regional Breakdown 
sum(charity$reg1=='1')
sum(charity$reg2=='1')
sum(charity$reg3=='1')
sum(charity$reg4=='1')

sum(1605,2555,1071,1117,1661)


PopulationByRegion <- c(1605, 2555, 1071, 1117, 1661)
RegionName <- c("Region 1", "Region 2", "Region 3", "Region 4", "Region 5")
barplot(PopulationByRegion, names=RegionName, main="Population By Region")
###################################################
# EDA
hist(donr)
table(donr)
# 0 = 3008 Non-Doners
# 1 = 2994 Doners
# There are 2007 'NA' results in the test set.
# The resonse rate in the test set is 10%****************

table(data.train$donr)
table(data.valid$donr)
# Weighted sampling has been used so the training and validation sets have
#approx equal numbers of donors and non-donors.

hist(damt) #zero inflated distribution
#3008 records with zero value = 37.5% of sample
#leaves remaining 62.4% 

table(home)
# Homeowners - 0.8665252 (6,940)
# Non - 0.1334748 (1,069)

table(genf)
# F - 4848, 60.5
# M - 3161, 39.5%

table(chld)
hist(chld)
#29.9% of population does not have child
#Remaining 70% appears normally distributed

hist(hinc)#mostly normal dist
boxplot(hinc)

hist(wrat) #skewed left distr. will want to transform
boxplot(wrat) #especially given low lying outliers


hist(avhv)
boxplot(avhv) #outliers
qqnorm(avhv)
qqline(avhv)
#skewed right and possesses outlier values

hist(incm)
boxplot(incm) #outliers
qqnorm(incm)
qqline(incm)
#skewed right

hist(inca)
boxplot(inca) #outliers 
qqnorm(inca) #needs trans
qqline(inca)
  
  
hist(plow) #needs transformation
boxplot(plow) #outliers present
qqnorm(plow)
qqline(plow)

hist(npro) #may need to normalize but mostly normal dist
boxplot(npro) #outliers present 
qqnorm(npro)
qqline(npro)

hist(tgif) #needs trans
boxplot(tgif) #many outliers
qqnorm(tgif)
qqline(tgif)

hist(lgif) #needs trans. skewed.
boxplot(lgif)
qqnorm(lgif)
qqline(lgif)


hist(rgif) # needs trans. skewed
boxplot(rgif)
qqnorm(rgif)
qqline(rgif)

hist(tdon) #needs to be trimmed. mostly normal dist but outliers at high end.
boxplot(tdon)
qqnorm(tdon)
qqline(tdon)

hist(tlag) #skewed right with high value outliers. Trim vs Trans
boxplot(tlag)
qqnorm(tlag)
qqline(tlag)

hist(agif) # trim vs trans
boxplot(agif)
qqnorm(agif)
qqline(agif)


###################################################
# Tranformations 
#charity$avhv<- log(charity$avhv)
#charity$incm<- log(charity$incm)
#charity$inca<- log(charity$inca)
#charity$plow<- log(charity$plow)
#charity$npro<- log(charity$npro)
#charity$tgif<- log(charity$tgif)
#charity$lgif<- log(charity$lgif)
#charity$rgif<- log(charity$rgif)
#charity$tdon<- log(charity$tdon)
#charity$tlag<- log(charity$tlag)
#charity$agif<- log(charity$agif)
###################################################
#Outliers
#Many of these variables need to be trimmed in order to adjust
#distriubtions due to the presence of outliers.
#In order to to handle this, we'll calculate the Interquartile Range
#and then identify where the high-end and low-end cut-off points will be
#by multiplying the IQR by 1.5 and adding/subtracting from the Q3/Q1 points.

charity$avhv[charity$avhv > 343] <- 343
charity$incm[charity$incm > 94.5] <- 94.5
charity$inca[charity$inca > 110] <- 110
charity$plow[charity$plow > 46.5] <- 46.5
charity$npro[charity$npro > 151] <- 151
charity$tgif[charity$tgif > 248] <- 248
charity$lgif[charity$lgif > 47.5] <- 47.5
charity$rgif[charity$rgif > 39.5] <- 39.5
charity$tdon[charity$tdon > 32.5] <- 32.5
charity$tlag[charity$tlag > 11.5] <- 11.5
charity$agif[charity$agif > 26.545] <- 26.545
###################################################

#change this but use to figure out where to omit
wealth.vars <- data.frame(cbind(charity$hinc, charity$wrat, charity$avhv,
                                charity$incm, charity$inca, charity$plow))
colnames(wealth.vars) <- c("hinc", "wrat", "avhv", "incm", "inca", "plow")
cor(wealth.vars)
WealthCors <- chart.Correlation(wealth.vars)


interaction.vars <- data.frame(cbind(charity$npro, charity$tgif,
                                     charity$lgif, charity$rgif, 
                                     charity$tdon, charity$tlag,
                                     charity$agif))
colnames(interaction.vars) <- c("npro", "tgif", "lgif", "rgif", 
                                "tdon", "tlag", "agif")
cor(interaction.vars)
IntCors <- chart.Correlation(interaction.vars)



# VIF
vif(charity)

###################################################
#Set up train/validation/test sets
charity.t<- charity

data.train <- charity.t[charity$part=="train",]
x.train <- data.train[,2:21]
c.train <- data.train[,22] # donr
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,23] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995

data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,2:21]
c.valid <- data.valid[,22] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,23] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999

data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,2:21]

x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)
###################################################
# Classifcaiton Model Development
# 1) Logistic Models
model.log.1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                     avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                   data.train.std.c,family=binomial("logit"))
summary(model.log.1)
#outlier adjusted set AIC = 2198
model.log.2 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + I(hinc^2) + wrat + I(wrat^2) + incm + npro + tgif + tdon + tlag, 
                   data.train.std.c,family=binomial(link="logit"))
summary(model.log.2)
# log with only sig AIC = 2151
#******
model.log.2 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home*chld + I(hinc^2) + wrat + I(wrat^2) + incm + npro + tgif + tdon + tlag, 
                   data.train.std.c,family=binomial(link="logit"))
summary(model.log.2)
# AIC = 2139 (Outlier Set)
# Max.Prof = 11674.5 |
# Valid.set = 1205.9 | Max.Prof = 11679
#******
model.log.3 <- glm(donr ~., data.train.std.c,family=binomial(link="logit"))
summary(model.log.3)
###
#2)  LDA
###
model.lda1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c) # include additional terms on the fly using I()
model.lda1
# Note: strictly speaking, LDA should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model


model.lda2 <- lda(donr ~ reg1 + reg2 + home*chld + I(hinc^2) + wrat + incm + npro + tgif + tdon + tlag, 
                  data.train.std.c)
model.lda2

###

post.valid.lda1 <- predict(model.lda2, data.valid.std.c)$posterior[,2] # n.valid.c post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1, decreasing=T)]-2)
plot(profit.lda1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.lda1)) # report number of mailings and maximum profit
# 1329.0 11624.5

cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda1, c.valid) 
###

###
# 3) Tree
tree.data.train = data.train.std.c
tree.data.train$donr = as.factor(tree.data.train$donr)
tree.data.valid = data.valid.std.c
tree.data.valid$donr = as.factor(tree.data.valid$donr)
# Fit tree and test.
tree.Donr = tree(donr ~., tree.data.train)
plot(tree.Donr)
text(tree.Donr, pretty=0)
tree.pred = predict(tree.Donr, tree.data.valid, type = "class")
table(tree.pred, c.valid)
(783 + 929)/2018 # Percent correctly classified
(236 + 70)/2018 # Percent incorrectly classified
# Error 0.1516353
treeError <- mean(tree.pred != c.valid)
treeError
#Profit= 11140

# Cross-validation to prune classification tree.
cv.tree.Donr <- cv.tree(tree.Donr, FUN=prune.misclass)
plot(cv.tree.Donr$size, cv.tree.Donr$dev, type = "b")
plot(cv.tree.Donr$k, cv.tree.Donr$dev, type = "b")
# Most significant reductions by five, with lower at 9 and lowest at 15.
prune.tree = prune.misclass(tree.Donr, best=5)
tree.pred.2 = predict(prune.tree, tree.data.valid, type="class")
table(tree.pred.2, c.valid)
# Error 0.1838454
treeError2 <- mean(tree.pred.2 != c.valid)
treeError2

# Error 0.154113
prune.tree = prune.misclass(tree.Donr, best=9)
tree.pred.3 = predict(prune.tree, tree.data.valid, type="class")
table(tree.pred.3, c.valid)
treeError3 <- mean(tree.pred.3 != c.valid)
treeError3

prune.tree = prune.misclass(tree.Donr, best=15)
tree.pred.4 = predict(prune.tree, tree.data.valid, type="class")
table(tree.pred.4, c.valid)
#Donors = 236+929
#MaxP = (14.5*929)-(2*1165) = 11140.5
# Error 0.1516353
treeError4 <- mean(tree.pred.4 != c.valid)
treeError4
###
# 4) RF/Bagging
set.seed(1) # For reproducible results
bag.Donr = randomForest(donr ~ ., data=tree.data.train, mtry=20, 
                        importance=TRUE, type="classification")
importance(bag.Donr)

bag.pred = predict(bag.Donr, newdata=tree.data.valid)
# Correct predictions 901/999 donors.
table(bag.pred, c.valid)
# Don = 128+901
# MaxP = (14.5*901)-(2*1029) = 11006.5 
# Error 0.1119921
bagError = mean(bag.pred != c.valid)
bagError

# 5) Boosting
library(gbm)
set.seed(1)
boost.Donr = gbm(donr ~ ., data = data.train.std.c, distribution = "bernoulli",
                 n.trees = 5000, interaction.depth = 4)
set.seed(1)
boost.probs = predict.gbm(boost.Donr, newdata = data.valid.std.c,
                          n.trees = 5000, type = "response")
boost.pred = rep("0", 2018)
boost.pred[boost.probs > .5] = "1"
table(boost.pred , c.valid)
boostError <- mean(boost.pred != c.valid)
boostError
#0.1070367
#Donors Pred = 142+925 
#Max Prof = (14.5*925)-(2*1067)=11278.5
###
library(e1071)
set.seed(1)
# Use cross-validation to select the best parameters for the SVC.
tune.out = tune(svm, donr ~., data = tree.data.train, kernel = "linear",
                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10)))
summary(tune.out)
# Best cost parameter is 1.
set.seed(1)
svm.fit1 = svm(donr ~., data = tree.data.train, kernel = "linear",
               cost = 1, scale = FALSE)
# Make predictions based on best model.
bestmod = tune.out$best.model
summary(bestmod)
set.seed(1)
svm.pred = predict(bestmod, tree.data.valid)
table(svm.pred, c.valid)
# Donors Pred = 191+862
# Max Prof = (14.5 * 862) - (2 * 1053)
# Error 0.1625372
svmError <- mean(svm.pred != c.valid)
svmError
#############
# LAST STEP IN CLASSIFICATION STEP
# Oversampling adjustment for calculating number of mailings for test set

n.mail.valid <- which.max(profit.log1)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .5 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set

cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)

###################################################
# Prediction Model Development
# 1) LReg
reg.mod1 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)

pred.valid.ls1 <- predict(reg.mod1, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls1)^2) # mean prediction error
# 1.711
sd((y.valid - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error
# 0.1648803

#2) Subset Reg
regfit.full<- regsubsets(damt~.,data=data.train.std.y)
reg.summary = summary(regfit.full)
names(reg.summary)
reg.summary$rsq
reg.summary$adjr2
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

















































