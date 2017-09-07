
library(stringr) 
library(tidyr) 
library(ggplot2) 
library(knitr) 
library(lattice) 
library(lubridate) 
library(plyr) 
library(dplyr) 
library(lpSolve) 
library(tidyverse) 
library(car) 

#Import 2016 NBA Draft Data 
nba.draft.2016 <- read.csv('nba.draft.2016.csv',stringsAsFactors = FALSE) 
data.frame(nba.draft.2016) 
summary(nba.draft.2016) 

#Add Variable to Indicate Player Position 
player_position <- read.csv('pos.1617.csv',stringsAsFactors = FALSE) 
data.frame(player_position) 
summary(player_position) 

#Add Player Salary Data 
data.frame(NBA.PAYROLL.1617) 
summary(NBA.PAYROLL.1617) 

#Merge and Prep Data 
nba_draft_player_eval <- merge(nba.draft.2016,player_position,by.x="Player",by.y="Player") 
summary(nba_draft_player_eval) 

nba_draft_player_eval_pay <- merge(nba_draft_player_eval,NBA.PAYROLL.1617,by.x="Player",by.y="Player") 
nba_draft_player_eval_pay<-nba_draft_player_eval_pay[-c(1),] #remove row for dupe player 
nba_draft_player_eval_pay<-nba_draft_player_eval_pay[-c(22,24,25)] #remove columns 
summary(nba_draft_player_eval_pay) 
str(nba_draft_player_eval_pay) 

#Create Variable Consideration Set 
Player<- nba_draft_player_eval_pay$Player 
Games<- as.numeric(nba_draft_player_eval_pay$Games) 
Minutes<- as.numeric(nba_draft_player_eval_pay$Minutes) 
Points<- as.numeric(nba_draft_player_eval_pay$PTS) 
FG.<- as.numeric(nba_draft_player_eval_pay$FG.) 
X3P.<- as.numeric(nba_draft_player_eval_pay$X3P.) 
FT.<- as.numeric(nba_draft_player_eval_pay$FT.) 
WS.48<- as.numeric(nba_draft_player_eval_pay$WS.48) 
BPM<- as.numeric(nba_draft_player_eval_pay$BPM) 
VORP<- as.numeric(nba_draft_player_eval_pay$VORP) 
COST<- as.numeric(nba_draft_player_eval_pay$Salary2016_17) 
Pos<- nba_draft_player_eval_pay$Pos 
AST<- as.numeric(nba_draft_player_eval_pay$AST) 

#Create new data frame for analysis and remove NA values 
nba_work<- data.frame(cbind(Player, Pos, Games, Minutes, Points, AST, FG., X3P., FT., WS.48, BPM, VORP, COST)) 
summary(nba_work) 

#PART 2: 
#Import 2016 NBA Draft Data
nba_work <- read.csv('nba.work.csv',stringsAsFactors = FALSE) 
data.frame(nba_work) 

FG.<- recode(nba_work$FG.,"NA=0.0") 
FG.<- recode(nba_work$FG.,"1.000=0.0") 
FT.<- recode(nba_work$FT.,"NA=0.0") 
FT.<- recode(nba_work$FT.,"1.000=0.0") 
X3P.<- recode(nba_work$X3P.,"NA=0.0") 
X3P.<- recode(nba_work$X3P.,"1.000=0.0") 
             
#Create Variable Consideration Set 
Player<- nba_work$Player 
Games<- as.numeric(nba_work$Games) 
Minutes<- as.numeric(nba_work$Minutes) 
Points<- as.numeric(nba_work$Points) 
FG.<- as.numeric(nba_work$FG.) 
X3P.<- as.numeric(nba_work$X3P.) 
FT.<- as.numeric(nba_work$FT.) 
WS.48<- as.numeric(nba_work$WS.48) 
BPM<- as.numeric(nba_work$BPM) 
VORP<- as.numeric(nba_work$VORP) 
COST<- as.numeric(nba_work$COST) 
Pos<- nba_work$Pos 
AST<- as.numeric(nba_work$AST) 
             
MPG<- Minutes/Games 
PPG<- Points/Games 
APG<- AST/Games 
           
nba_work<- cbind(nba_work,MPG,PPG,APG) 
summary(nba_work) 
             
#EDA 
par(mfrow=c(2,2)) 
hist(Games) 
hist(Minutes) 
hist(Points) 
hist(COST) 
             
par(mfrow=c(3,3)) 
plot(COST~MPG) 
plot(COST~PPG) 
plot(COST~APG) 
plot(COST~FG.) 
plot(COST~X3P.) 
plot(COST~FT.) 
plot(COST~WS.48) 
plot(COST~BPM) 
plot(COST~VORP)
             
             
par(mfrow=c(1,1)) 
             
rook_pts <- ggplot(data=nba_work) + 
  geom_boxplot(mapping=aes(x=Pos,y=Points,colour=Pos))+ 
  ggtitle("NBA Rookie Total PTS by Position")+ 
  theme(plot.title=element_text(hjust=0.5,size=15)) 
rook_pts 
             
rook_games <- ggplot(data=nba_work) + 
  geom_boxplot(mapping=aes(x=Pos,y=PPG,colour=Pos))+ 
  ggtitle("NBA Rookie PPG by Position")+ 
  theme(plot.title=element_text(hjust=0.5,size=15)) 
rook_games 
             
rook_mins <- ggplot(data=nba_work) + 
  geom_boxplot(mapping=aes(x=Pos,y=MPG,colour=Pos))+ 
  ggtitle("NBA Rookie MPG by Position")+ 
  theme(plot.title=element_text(hjust=0.5,size=15)) 
rook_mins 
             
             
rook_fg_pct <- ggplot(data=nba_work) + 
  geom_boxplot(mapping=aes(x=Pos,y=FG.,colour=Pos))+ 
  ggtitle("NBA Rookie FG Percentage by Position")+ 
  theme(plot.title=element_text(hjust=0.5,size=15)) 
rook_fg_pct 
             
rook_x3Pt <- ggplot(data=nba_work) + 
  geom_boxplot(mapping=aes(x=Pos,y=X3P.,colour=Pos))+ 
  ggtitle("NBA Rookie 3-Point Percentage by Position")+ 
  theme(plot.title=element_text(hjust=0.5,size=15)) 
rook_x3Pt 
             
rook_FT <- ggplot(data=nba_work) + 
  geom_boxplot(mapping=aes(x=Pos,y=FT.,colour=Pos))+ 
  ggtitle("NBA Rookie Free-Throw Percentage by Position")+ 
  theme(plot.title=element_text(hjust=0.5,size=15)) 
rook_FT 
             
rook_AST <- ggplot(data=nba_work) + 
  geom_boxplot(mapping=aes(x=Pos,y=AST,colour=Pos))+ 
  ggtitle("NBA Rookie Total AST by Position")+ 
  theme(plot.title=element_text(hjust=0.5,size=15)) 
rook_AST 
             
rook_cost <- ggplot(data=nba_work) + 
  geom_boxplot(mapping=aes(x=Pos,y=COST,colour=Pos))+ 
  ggtitle("NBA Rookie Total AST by Position")+ 
  theme(plot.title=element_text(hjust=0.5,size=15)) 
rook_cost 
             
             
             
#PLOTS BY INDIVIDUAL PLAYERS 
############################# 
rook_ppg <- ggplot(data=nba_work) + 
  geom_point(mapping=aes(x=Player,y=PPG,colour=Pos),size=4)+ 
  ggtitle("NBA Rookie PPG 2016-17")+ 
  theme(plot.title=element_text(hjust=0.5,size=10)) 
rook_ppg + theme(axis.text.x=element_text(angle=90,hjust=1,size=10),axis.title.x=element_blank()) 
             
             
rook_ws_48 <- ggplot(data=nba_work) + 
  geom_point(mapping=aes(x=Player,y=WS.48,colour=Pos),size=4) + 
  ggtitle("NBA Rookie Win-Share per 48 Min 2016-17") + 
  theme(plot.title=element_text(hjust=0.5,size=10)) 
rook_ws_48 + theme(axis.text.x=element_text(angle=90,hjust=1,size=10),axis.title.x=element_blank()) 
             
             
rook_mpg_player<- ggplot(data=nba_work) + 
  geom_point(mapping=aes(x=Player,y=MPG,colour=Pos),size=4) + 
  ggtitle("NBA Rookie MPG 2016-17") + 
  theme(plot.title=element_text(hjust=0.5,size=10)) 
rook_mpg_player + theme(axis.text.x=element_text(angle=90,hjust=1,size=10),axis.title.x=element_blank()) 
             
rook_fg_pct_player <- ggplot(data=nba_work) + 
  geom_point(mapping=aes(x=Player,y=FG.,colour=Pos),size=4) + 
  ggtitle("NBA Rookie FG Percentage 2016-17")+ 
  theme(plot.title=element_text(hjust=0.5,size=10)) 
rook_fg_pct_player + theme(axis.text.x=element_text(angle=90,hjust=1,size=10),axis.title.x=element_blank()) 
             
rook_x3Pt_player<- ggplot(data=nba_work) + 
  geom_point(mapping=aes(x=Player,y=X3P.,colour=Pos),size=4) + 
  ggtitle("NBA Rookie 3-Point Percenatge 2016-17")+ 
  theme(plot.title=element_text(hjust=0.5,size=10)) 
rook_x3Pt_player + theme(axis.text.x=element_text(angle=90,hjust=1,size=10),axis.title.x=element_blank()) 
             
rook_FT_player<- ggplot(data=nba_work) + 
  geom_point(mapping=aes(x=Player,y=FT.,colour=Pos),size=4) + 
  ggtitle("NBA Rookie Free-Throw Percenatge 2016-17")+ 
  theme(plot.title=element_text(hjust=0.5,size=10)) 
rook_FT_player + theme(axis.text.x=element_text(angle=90,hjust=1,size=10),axis.title.x=element_blank()) 
             
#C<- cbind(subset(nba_work,Pos=="C")) 
#PF<- cbind(subset(nba_work,Pos=="PF")) 
#PG<- cbind(subset(nba_work,Pos=="PG")) 
#SF<- cbind(subset(nba_work,Pos=="SF")) 
#SG<- cbind(subset(nba_work,Pos=="SG")) 
 
#G<- cbind(subset(nba_work,Pos=="PG"| Pos=="SG")) 
#W<-cbind(subset(nba_work,Pos=="PF"| Pos=="SF")) 
             
# sort data by the PositionID and retain selected variables 
nba_work <- nba_work[sort.list(nba_work$Pos),] 
table(nba_work$X,nba_work$Pos) 
             
table(nba_work) 
             
nba_work$Centers<- c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) 
nba_work$Guard<- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,1,1,1,1,1,1,1,1) 
nba_work$Forward<-c(0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0) 
             
             
             
constraint_matrix <- as.matrix(rbind(nba_work$Centers, nba_work$Centers, t(rep(1, length = 40)), 
             nba_work$COST)) 
             
constraint_matrix <- as.matrix(rbind(nba_work$Forward, nba_work$Guard,nba_work$Centers, t(rep(1, length = 40)), 
             nba_work$COST)) 
             
dimnames(constraint_matrix) <- 
  list(c("OneCenterMax", 
         "OneCenterMin", 
         "NBAForward", 
         "FivePlayerMax", 
         "SalaryMax"), 
          nba_work$Pos) 
             
# solve the knapsack problem 
knapsack_object <- 
  lp(const.mat = constraint_matrix, 
     objective = nba_work$PPG, 
     direction = "max", 
     const.rhs = c(2, 2, 1, 5, 10), 
     const.dir = c("<=", "<=","<=", "<=", "<="), 
     int.vec = 1:40, all.bin = TRUE) 
             
#############################
#show the solution 
cat("\n\nBest Set of Draft Picks\n") 
View(nba_work[as.logical(knapsack_object$solution),]) 
             
# show the points per period maximum 
cat("\n\nMaximum Points per Period:", knapsack_object$objval, "\n") 
             
OPTImUM<- data.frame(nba_work[as.logical(knapsack_object$solution),]) 
             
#"Ivica Zubac" "C" "5.88888888888889" "1.034956" 
#"Isaiah Whitehead" "PG" "7.28301886792453" "1.074145" 
#"Brandon Ingram" "SF" "8.23809523809524" "5.28168" 
#"Caris LeVert" "SF" "7.11111111111111" "1.56228" 
#"Malcolm Brogdon" "SG" "9.9344262295082""0.925" 
             
#Salary Cost = $9.87M 
#Maximum Points per Game: 38.45554 
             
             
             