setwd("C:/Users/herma/Desktop/413/Final")
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
library(party)
library(mlbench)
library(imputeTS)
library(hts)
library(h2o)
library(mice)
library(VIM)

test_Data_Features<- read.csv('dengue_features_test.csv')
train<- read.csv('dengue_features_train.csv') #Features of training set
labels<-read.csv('dengue_labels_train.csv') #Number of dengue cases for each row in training set
sub<-  read.csv('submission_format.csv')


train$total_cases<- labels$total_cases
str(train)
summary(train)

train$week_start_date <- as.Date(train$week_start_date)

num_NA <- sapply(train, function(x) sum(is.na(x)))
sum(num_NA)
#548 NA values
#NA value percentage by column
NA_percentage <- function(x){sum(is.na(x))/length(x)*100}
percent_data<- apply(train,2,NA_percentage)



#aggr_plot <- aggr(train, numbers=TRUE, sortVars=TRUE, 
 #                 labels=names(train), cex.axis=.7, gap=3, 
  #                ylab=c("Histogram of missing data","Pattern"))


train<-na.interpolation(train)
test<-test_Data_Features
test[is.na(test)==TRUE] <- 0
test1<-na.interpolation(test)

Num_NA1 <- sapply(train, function(x) sum(is.na(x)))
sum(Num_NA1)

sj=train[1:936,]
sum(is.na(sj))
iq=train[937:1456,]
sum(is.na(iq))

mycor=cor(train[,5:25]) 
corrplot(mycor, method="circle")

mycor=cor(iq[,5:25]) 
corrplot(mycor, method="circle") 

mycor=cor(sj[,5:25]) 
corrplot(mycor, method="circle") 

kdepairs(sj[,5:25])
kdepairs(iq[,5:25])

sj.ts=ts(sj$total_cases, frequency=52, start=c(1990,04,30)) 
iq.ts=ts(iq$total_cases, frequency=52, start=c(2000,07,01)) 

par(mfrow=c(1,2))
plot(sj.ts)
plot(iq.ts)

par(mfrow=c(1,1))
acf(sj.ts)
pacf(sj.ts)

tsdisplay(sj.ts)
tsdisplay(diff(sj.ts,1))
t<-decompose(sj.ts,type="additive")

acf(iq.ts)
pacf(iq.ts)
tsdisplay(diff(iq.ts,1))
r<-decompose(iq.ts,type="additive")

#create Test Data Sets
test<- test_Data_Features
test$week_start_date <- as.Date(test$week_start_date)

sj.test=test[1:260,]
iq.test=test[261:416,]

#######
#NNAR
fit_nnetar_sj <- nnetar(sj.ts,repeats=25, size=12, decay=0.1,linout=TRUE)
plot(forecast(fit_nnetar_sj,h=260))
a=forecast(fit_nnetar_sj,h=260)


fit_nnetar_iq <- nnetar(iq.ts,repeats=25, size=18, decay=0.1,linout=TRUE)
plot(forecast(fit_nnetar_iq,h=156))
b=forecast(fit_nnetar_sj,h=156)

nnetar_sj_sol <- data.frame(test[1:260,-4], total_cases = round(a$mean))

nnetar_iq_sol <- data.frame(test[261:416,-4], total_cases =round(b$mean))

nnetar_solution <- bind_rows(nnetar_sj_sol,nnetar_iq_sol)
submit_1<- data.frame(city = test$city, year = test$year, weekofyear = test$weekofyear, total_cases = round(nnetar_solution$total_cases))
write.csv(submit_1,file="nnetar_model_sth_nw.csv")
#MAE_Score = 40.19

######################################
#Arima Models
fit <- Arima(sj.ts, order=c(0,1,2), seasonal=c(0,1,2))
fit_Arima=forecast(fit, h = 260)
plot(forecast(fit))
tsdisplay(residuals(fit))
Box.test(residuals(fit), lag=151, fitdf=52, type="Ljung")

sj_arima_predict<- data.frame(test[1:260,-4], total_cases = round(fit_Arima$mean))

#Seasonal Arima***
fit_iq <- Arima(iq.ts, order=c(0,1,2), seasonal=c(0,2,1))
fit_Arima_iq=forecast(fit_iq, h = 156)
plot(forecast(fit_Arima_iq))
tsdisplay(residuals(fit_Arima_iq))
Box.test(residuals(fit_Arima_iq), lag=151, fitdf=52, type="Ljung")

iq_arima_predict<- data.frame(test[261:416,-4], total_cases = round(fit_Arima_iq$mean))

arima_solution<- bind_rows(sj_arima_predict,iq_arima_predict)
submit_2<- data.frame(city = test$city, year = test$year, weekofyear = test$weekofyear, total_cases = round(arima_solution$total_cases))
write.csv(submit_2,file="arima_model_sth_nw.csv")
#MAE_Score = 33.91

######################################
#Random Forest
sj_rf_model <- randomForest(total_cases ~., data = sj)
print(sj_rf_model)
sj_rf_prediction<- predict(object=sj_rf_model, sj.test)

iq_rf_model <- randomForest(total_cases ~. , data = iq)
print(iq_rf_model)
iq_rf_prediction<- predict(object=iq_rf_model, iq.test)

rf_sj_sol <- data.frame(test[1:260,-4], total_cases = round(sj_rf_prediction))
rf_iq_sol <- data.frame(test[261:416,-4], total_cases =round(iq_rf_prediction))

rf_solution <- bind_rows(rf_sj_sol,rf_iq_sol)
submit_3<- data.frame(city = test$city, year = test$year, weekofyear = test$weekofyear, total_cases = round(rf_solution$total_cases))
write.csv(submit_3,file="rf_model_sth_nw.csv")
#MAE_Score = 28.14




