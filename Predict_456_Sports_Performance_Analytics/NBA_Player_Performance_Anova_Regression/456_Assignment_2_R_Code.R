setwd("/Users/sth/Desktop")
library(ggplot2)
library(rvest)
library(stringr)
library(tidyr)
library(lubridate)

bball<- read.csv('BasketballDatasetAnalysis.csv', header=TRUE,stringsAsFactors = FALSE)
head(bball)
summary(bball)
str(bball)


NBA<- bball[c(1,2,3,4,5,6,7,11,12,13,14,15,16)] 
str(NBA)
summary(NBA)

NBA$TEAM<- c("OKC", "MIA", "MIN", "NOP", "SAC", "LAC", "NYK", "GSW", "DAL", "HOU", 
             "CHA", "DET", "POR", "PHO", "SAS", "IND", "CLE", "TOR", "MEM", "CHI",
             "DEN", "ATL", "WAS", "LAL","UTA", "BRK", "ORL", "TOT", "BOS", "MIL", "PHI")

TEAM<-NBA$Team

NBA$Position<- c("SF", "PF", "C", "PG", "SG","SG-PG","SF-PF","PF-SF","SG-SF")
POS<-NBA$Position #Character variable
PLAYER<- NBA$Player #player based
                
#Numeric Variables
MINS <- as.numeric(NBA$Minutes) #per year AGE<-as.numeric(NBA$Age)
AST<- NBA$AST #team based
STL<- NBA$STL #player based
BLK<- NBA$BLK
GAMES<- as.numeric(NBA$Games) #player based REB<- NBA$TRB #not norm dist
TOV<- NBA$TOV
PER<- NBA$PER
USG<- as.numeric(NBA$USG)
NUM.NBA<- NBA[c(3,5,6,7,8,9,10,11,12,13)] 
str(NUM.NBA)
pairs(NUM.NBA)

hist(GAMES)
REB=rnorm(REB) 
BLK=rnorm(BLK) 
AST=rnorm(AST) 
MINS=rnorm(MINS) 
GAMES=rnorm(GAMES)

par(mfrow=c(3,2)) 
hist(AGE) 
hist(rnorm(MINS)) 
hist(rnorm(GAMES)) 
hist(PER) 
hist(rnorm(USG)) 
hist(STL) 
hist(rnorm(AST)) 
hist(rnorm(BLK)) 
hist(rnorm(REB)) 
hist(rnorm(TOV))

par(mfrow=c(1,1)) 
boxplot(AST~Position, data=bball, 
        xlab="Basketball Player Position", ylab="Assists") 
boxplot(STL~Position, data=bball, 
        xlab="Basketball Player Position", ylab="Steals")
boxplot(TOV~Position, data=bball,
        xlab="Basketball Player Position", ylab="Turnovers") 
boxplot(BLK~Position, data=bball, 
        xlab="Basketball Player Position", ylab="Blocks") 
boxplot(REB~Position, data=bball, 
        xlab="Basketball Player Position", ylab="Rebounds")
boxplot(USG~Position, data=bball, 
        xlab="Basketball Player Position", ylab="USG")
boxplot(PER~Position, data=bball, 
        xlab="Basketball Player Position", ylab="PER")
boxplot(AGE~Position, data=bball, 
        xlab="Basketball Player Position", ylab="Age")
boxplot(MINS~Position, data=bball,
        xlab="Basketball Player Position", ylab="Minutes")
boxplot(GAMES~Position, data=bball, 
        xlab="Basketball Player Position", ylab="Games")

#aov Model testing for Age differences
MINS_Model<-aov(Minutes~Position + Age, data=bball) 
summary(MINS_Model)
GAMES_Model<-aov(Games~Position + Age, data=bball) 
summary(GAMES_Model)
STL_Model<-aov(STL~Position + Age, data=bball) 
summary(STL_Model)
AST_Model<-aov(AST~Position + Age, data=bball) 
summary(AST_Model)
REB_Model<-aov(TRB~Position + Age, data=bball) 
summary(REB_Model)
BLK_Model<-aov(BLK~Position + Age, data=bball) 
summary(BLK_Model)
TOV_Model<-aov(TOV~Position + Age, data=bball) 
summary(TOV_Model)
PER_Model<-aov(PER~Position + Age, data=bball) 
summary(PER_Model)
USG_Model<-aov(USG~Position + Age, data=bball) 
summary(USG_Model)

#aov Models controlling for age
MINS_Model_Control<- aov(MINS~Position, data=bball) 
summary(MINS_Model_Control)
GAMES_Model_Control<- aov(GAMES~Position, data=bball) 
summary(GAMES_Model_Control)
STL_Model_Control<- aov(STL~Position, data=bball) 
summary(STL_Model_Control)
AST_Model_Control<- aov(AST~Position, data=bball) 
summary(AST_Model_Control)
REB_Model_Control<- aov(REB~Position, data=bball) 
summary(REB_Model_Control)
BLK_Model_Control<- aov(BLK~Position, data=bball) 
summary(BLK_Model_Control)
TOV_Model_Control<- aov(TOV~Position, data=bball) 
summary(TOV_Model_Control)
PER_Model_Control<- aov(PER~Position, data=bball) 
summary(PER_Model_Control)
USG_Model_Control<- aov(USG~Position, data=bball) 
summary(USG_Model_Control)

            