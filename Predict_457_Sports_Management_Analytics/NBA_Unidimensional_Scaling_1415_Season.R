setwd("/Users/sth/Desktop/R")
library(stringr)
library(tidyr)
library(ggplot2)
library(knitr)
library(lattice)
library(lubridate)
library(plyr)
library(dplyr)
library(lpSolve)
library(mtcars)
library(tidyverse)
library(methods)
library(data.table)
library(leaps)
library(reshape2)
library(grid)


NBA.PREDICTIONS<- read.csv('NBA_PREDICT_TEAM_12.16.csv', header=TRUE,stringsAsFactors = FALSE)
summary(NBA.PREDICTIONS)
#per.poss.13<- subset(NBA.PREDICTIONS,YEAR=="2012.13")
#per.poss.14<- subset(NBA.PREDICTIONS,YEAR=="2013.14")
#per.poss.15<- subset(NBA.PREDICTIONS,YEAR=="2014.15")
#per.poss.16<- subset(NBA.PREDICTIONS,YEAR=="2015.16")
###per.poss.17<- subset(NBA.PREDICTIONS,YEAR=="2016.17")

NBA.RESULTS<- read.csv("NBA_Game_by_Game_1316.csv",header=TRUE,stringsAsFactors = FALSE)
summary(NBA.RESULTS)
#SEASON.1415<- read.csv("SEASON.1415.csv",stringsAsFactors = TRUE)
#SEASON.1516<- read.csv("SEASON.1516.csv",stringsAsFactors = TRUE)
#SEASON.1415$YEAR<- c(2014.15)
#SEASON.1516$YEAR<- c(2015.16)
#NBA.RESULTS$YEAR<-as.Date(NBA.RESULTS$YEAR,"%Y")
#WINNER<- cbind(subset(SEASON.1415,SEASON.1415$WINS=="1"))
#LOSER<- cbind(subset(SEASON.1415,SEASON.1415$WINS=="0"))
#HOME.TEAM<- cbind(subset(SEASON.1415,SEASON.1415$HOME=="1"))
#AWAY.TEAM<- cbind(subset(SEASON.1415,SEASON.1415$HOME=="0"))
#PA.HOME<- cbind(HOME.TEAM$PTS-HOME.TEAM$PLUS.MINUS)
#PA.AWAY<-cbind(AWAY.TEAM$PTS-AWAY.TEAM$PLUS.MINUS)
#HOME<- cbind(HOME.TEAM,PA.HOME)
#AWAY<- cbind(AWAY.TEAM,PA.AWAY)
SEASON.1415<- read.csv("SEASON.1415.csv",stringsAsFactors = TRUE)
SEASON.1415$YEAR<- c(2014.15)
SEASON.1415$OPP.PTS<- cbind(SEASON.1415$PTS-SEASON.1415$PLUS.MINUS)
P1<- SEASON.1415$FGA-SEASON.1415$OREB
P2<-SEASON.1415$TOV
P3<-SEASON.1415$FTA*.44
P4<- P1+P2+P3
SEASON.1415$PTS.POSS<- cbind(SEASON.1415$PTS/P4)

record_number<- seq(nrow(SEASON.1415))
nba_scores<- cbind(data.frame(record_number),SEASON.1415)

team_info <- read.csv("nba_team_names_abbreviations.csv",stringsAsFactors = FALSE)
list_of_team_names <- team_info$abbreviation

nba_scores$home_conference <- rep("", length = nrow(nba_scores))
nba_scores$home_division <- rep("", length = nrow(nba_scores))
nba_scores$home_team_code <- rep("", length = nrow(nba_scores))

for (i in seq(along = list_of_team_names)) {
  this_team_info <- 
    team_info[(team_info$abbreviation == list_of_team_names[i]),]
  indices_for_team <- 
    which (nba_scores$TEAM == list_of_team_names[i])
  for (j in seq(along = indices_for_team)) {
    nba_scores$home_conference[indices_for_team[j]] <-
      this_team_info$conference[1]
    nba_scores$home_division[indices_for_team[j]] <-
      this_team_info$division[1]
    nba_scores$home_team_code[indices_for_team[j]] <-
      this_team_info$abbreviation[1]
  }
}  

nba_scores$away_conference <- rep("", length = nrow(nba_scores))
nba_scores$away_division <- rep("", length = nrow(nba_scores))
nba_scores$away_team_code <- rep("", length = nrow(nba_scores))
for (i in seq(along = list_of_team_names)) {
  this_team_info <- 
    team_info[(team_info$abbreviation == list_of_team_names[i]),]
  indices_for_team <- 
    which (nba_scores$OPP == list_of_team_names[i])
  for (j in seq(along = indices_for_team)) {
    nba_scores$away_conference[indices_for_team[j]] <-
      this_team_info$conference[1]
    nba_scores$away_division[indices_for_team[j]] <-
      this_team_info$division[1]
    nba_scores$away_team_code[indices_for_team[j]] <-
      this_team_info$abbreviation[1]
  }
}    

nba_scores$win_visitor_home <- rep("Home", length = nrow(nba_scores))
for (i in seq(along = nba_scores$record_number)) 
  if (nba_scores$OPP.PTS[i] > nba_scores$PTS[i]) 
    nba_scores$win_visitor_home[i] <- "Visitor" 

nba_scores$win_team_code <- rep("", length = nrow(nba_scores))
nba_scores$lose_team_code <- rep("", length = nrow(nba_scores))
for (i in seq(along = nba_scores$record_number)) {
  if (nba_scores$OPP.PTS[i] > nba_scores$PTS[i]) { 
    nba_scores$win_team_code[i] <- nba_scores$away_team_code[i]
    nba_scores$lose_team_code[i] <- nba_scores$home_team_code[i]
  }
  
  if (nba_scores$OPP.PTS[i] < nba_scores$PTS[i]) {
    nba_scores$win_team_code[i] <- nba_scores$home_team_code[i] 
    nba_scores$lose_team_code[i] <- nba_scores$away_team_code[i]
  } 
}

nba_scores_home<- cbind(subset(nba_scores,nba_scores$HOME=='1'))
with(nba_scores_home, table(win_visitor_home))


ordered_team_codes <- sort(team_info$abbreviation)
# total number of wins
wins_mat <- matrix(0, nrow = 30, ncol = 30, 
                   dimnames = list(ordered_team_codes, ordered_team_codes))
prop_mat <- wins_mat  # proportion wins

# build matrices for entire set of 30 teams
for (i in seq(along = nba_scores_home$record_number)) {
  # tally the number of times team k beats team j
  wins_mat[nba_scores_home$lose_team_code[i], nba_scores_home$win_team_code[i]] <- 
    wins_mat[nba_scores_home$lose_team_code[i], nba_scores_home$win_team_code[i]] + 1
}


for(j in 1:length(ordered_team_codes)) { # begin outer for-loop
  for(k in 1:length(ordered_team_codes)) { # begin inner for-loop
    if (j == k) prop_mat[j,k] <- NA   # set diagonal entries missing
    if (j > k) {  # begin outer if-block
      between_team_games <- 
        wins_mat[ordered_team_codes[j],ordered_team_codes[k]] +
        wins_mat[ordered_team_codes[k],ordered_team_codes[j]]
      
      # if teams never play others within the season
      # we set the entry in the cell 
      # to be the mean of the team
      if (between_team_games == 0) { # begin first inner if-block
        prop_mat[j,k] <- sum(wins_mat[j,]) / 
          (sum(wins_mat[,j]) + sum(wins_mat[j,]))
        prop_mat[k,j] <- sum(wins_mat[k,]) / 
          (sum(wins_mat[,k]) + sum(wins_mat[k,]))
      } # end first inner if-block  
      
      # when teams play other teams at least once 
      # we compute the proportion of times they beat one another
      if (between_team_games > 0) { # begin second inner if-block   
        prop_mat[j,k] <- 
          wins_mat[ordered_team_codes[j],ordered_team_codes[k]]/
          between_team_games
        prop_mat[k,j] <- 
          wins_mat[ordered_team_codes[k],ordered_team_codes[j]]/
          between_team_games
      }  # end second inner if-block
    } # end outer if-block
  } # end inner for-loop
} # end outer for-loop

for (j in 1:length(ordered_team_codes))
  print(sum(wins_mat[,j]) + sum(wins_mat[j,]))

for(i in seq(along = ordered_team_codes))
  cat("\n", ordered_team_codes[i], ":", mean(prop_mat[,i], na.rm = TRUE))


pc_mat <- prop_mat
for(j in 1:length(ordered_team_codes)) 
  for(k in 1:length(ordered_team_codes)) 
    if (j == k) pc_mat[j,k] <- 0.50   # set diagonal entries missing

nobjects <- length(ordered_team_codes)

mean_pc_mat <- numeric(nobjects)
object_quantile <- numeric(nobjects)
for(j in 1:nobjects)
{
  mean_pc_mat[j] <- mean(pc_mat[,j])
  object_quantile[j] <- qnorm(mean_pc_mat[j])
} 

z.score.converter <- function(z) {round(((100*z) + 500),digits=0)}

scale_score <- numeric(nobjects)
for(j in 1:nobjects)
  scale_score[j] <- z.score.converter(object_quantile[j]) 

sorted_team_info <- team_info[sort.list(team_info$abbreviation),]
sorted_team_info$scale_score <- scale_score

sorted_team_info$name_with_score <- rep("", length = nrow(sorted_team_info))
for(i in seq(along = ordered_team_codes))
  sorted_team_info$name_with_score[i] <- 
  paste(sorted_team_info$team_name[i], 
        " (", sorted_team_info$scale_score[i], ")", sep = "") 

scaling_frame <- 
  sorted_team_info[,c("abbreviation", "team_name", 
                      "name_with_score", "scale_score")]
names(scaling_frame) <- 
  c("object.name", "long.object.name",
    "long.object.name.with.score", "scale_score")

ordered_scaling_frame.1415 <- 
  scaling_frame[sort.list(scale_score, decreasing=TRUE),] 
