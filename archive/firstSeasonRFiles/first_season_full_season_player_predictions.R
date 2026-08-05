library("ggplot2")
library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library("readr")
library("dplyr")
library("modelr")
library("leaps")
library("ggrepel")

#Years
Years_Dataframe <- read_csv("~/R Stuff/FantasyFootball 2.0/Years_Dataframe.csv")
Past_Year <- Years_Dataframe$Past_Year[1]
This_Year <- Years_Dataframe$This_Year[1]

###read files
Receiving_player_percents <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Receiving_Percents_", This_Year, ".csv", sep = "")))
Rushing_player_percents <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Rushing_Percents_", This_Year, ".csv", sep = "")))
player_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Player_Ratings_", This_Year, ".csv", sep = ""))) %>% 
  clean_names() %>% 
  rename("Position" = "pos")
off_team_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Offensive_Ratings_", This_Year, ".csv", sep = "")))

####Predict Player PR (percent rating)
#Receiving
player_receiving_pr <- Receiving_player_percents %>% 
  rename("Team_s" = "Team") %>% 
  left_join(off_team_ratings, by = c("Team_s")) %>% mutate(
    tgt_pr = adj_tgt_per*Off_Pas_Att_Rat,
    rec_pr = adj_rec_per*Off_Cmp_Rat,
    rec_yds_pr = adj_rec_yds_per*Off_Pas_Yds_Rat,
    rec_tds_pr = adj_rec_tds_per*Off_Pas_Tds_Rat
  ) %>% 
  select(player, Position, Team, Team_s, tgt_pr:rec_tds_pr)

#Rushing
player_rushing_pr <- Rushing_player_percents %>% 
  rename("Team_s" = "Team") %>% 
  left_join(off_team_ratings, by = c("Team_s")) %>% mutate(
    rus_att_pr = adj_rus_att_per*Off_Rus_Att_Rat,
    rus_yds_pr = adj_rus_yds_per*Off_Rus_Yds_Rat,
    rus_tds_pr = adj_rus_tds_per*Off_Rus_Tds_Rat
  ) %>% 
  select(player, Position, Team, Team_s, rus_att_pr:rus_tds_pr)

#Passing
player_passing_pr <- off_team_ratings %>% 
  select(QB, Team, Team_s, Off_Cmp_Rat:Off_Int_Rat) %>% 
  mutate(Position = "QB")
colnames(player_passing_pr) <- c("player", "Team","Team_s", "cmp_pr", "pas_att_pr", "pas_yds_pr", "pas_tds_pr", "int_pr", "Position")

#Combine
player_pr <- full_join(player_passing_pr, player_rushing_pr, by = c("player", "Position", "Team", "Team_s")) %>% 
  full_join(player_receiving_pr, by = c("player", "Position", "Team", "Team_s"))

#NAs to 0
player_pr[is.na(player_pr)] <- 0

####Combine with player ratings
player_predictions <- full_join(player_pr, player_ratings, by = c("player", "Position")) %>% 
  select(player, Position, player_games, player_gs, Team, Team_s, cmp_pr:pas_tds_pr, rus_att_pr:rec_tds_pr, int_pr, pl_cmp:pl_rec_tds, pl_int:pl_fl, fl_per_tou)

#Rookies
rookie_predictions <- player_predictions %>% 
  filter(is.na(pl_cmp))

c <- 19
while(c < 31){
  rookie_predictions[, c] <- rookie_predictions[, c - 12]
  c <- c + 1
}

veteran_predictions <- player_predictions %>% 
  filter(!is.na(pl_cmp))

player_predictions <- rbind(rookie_predictions, veteran_predictions)

####Combining PR and Ratings
#Coefs, all but int tested
cmp_pr_coef <- 0.2 #need to make sure ratings make sense
pas_att_pr_coef <- 0.4 #need to make sure ratings make sense
pas_yds_pr_coef <- 0.7 #need to make sure ratings make sense
pas_tds_pr_coef <- 0.8 #need to make sure ratings make sense
rus_att_pr_coef <- 0.4
rus_yds_pr_coef <- 0.4
rus_tds_pr_coef <- 0.7
tgt_pr_coef <- 0.2
rec_pr_coef <- 0.2
rec_yds_pr_coef <- 0.2
rec_tds_pr_coef <- 0.6
int_pr_coef <- 0.5

#combine
combine_predictions <- function(df, col){
  #percent rating coefficient
  pr_coef <- get(paste(col, "_pr_coef", sep = ""))
  
  #rcombine
  df[, paste(col, "_pred", sep = "")] <- ((df[, paste(col, "_pr", sep = "")]*pr_coef) + (df[, paste("pl_", col, sep = "")]*(1 - pr_coef)))
  
  return(df)
}

####
player_predictions <- combine_predictions(player_predictions, "pas_att")
player_predictions <- combine_predictions(player_predictions, "cmp")
player_predictions <- combine_predictions(player_predictions, "pas_yds")
player_predictions <- combine_predictions(player_predictions, "pas_tds")
player_predictions <- combine_predictions(player_predictions, "rus_att")
player_predictions <- combine_predictions(player_predictions, "rus_yds")
player_predictions <- combine_predictions(player_predictions, "rus_tds")
player_predictions <- combine_predictions(player_predictions, "tgt")
player_predictions <- combine_predictions(player_predictions, "rec")
player_predictions <- combine_predictions(player_predictions, "rec_yds")
player_predictions <- combine_predictions(player_predictions, "rec_tds")
player_predictions <- combine_predictions(player_predictions, "int")

#Select cols
player_predictions <- player_predictions %>% 
  select(player, Position, Team, Team_s, pas_att_pred:rec_tds_pred, int_pred, pl_fmb:fl_per_tou)

####Turnovers
##QBs
QBs <- player_predictions %>% 
  filter(Position == "QB") %>% 
  filter(!is.na(pas_att_pred))

vet_QBs <- QBs %>% 
  filter(!is.na(pl_fl))

rookie_fl <- mean(vet_QBs$pl_fl)*1.2

QBs <- QBs %>% 
  mutate(pl_fl = ifelse(is.na(pl_fl), rookie_fl, pl_fl))

#min and max
min_pl_fl <- 0.07 #for players with 0 fmbs in low game counts
max_pl_fl <- 0.45 #for players with fmbs in low game counts

QBs <- QBs %>% 
  mutate(pl_fl = ifelse(pl_fl > min_pl_fl, pl_fl, min_pl_fl),
         pl_fl = ifelse(pl_fl < max_pl_fl, pl_fl, max_pl_fl))

#regress
mean_pl_fl <- mean(QBs$pl_fl)

QBs <- QBs %>% 
  mutate(fl_pred = (pl_fl - mean_pl_fl)*0.7 + mean_pl_fl)

#QB fumbles
QB_fumbles <- QBs %>% 
  select(player:Team_s, fl_pred) %>% 
  mutate(fl_per_tou = 0) %>% 
  select(player:Team_s, fl_per_tou, fl_pred) %>% 
  rename("QB_fl" = "fl_pred")

#select
QBs <- QBs %>% 
  select(player:int_pred, fl_pred)

##Non QBs
flex <- player_predictions %>% 
  filter(Position != "QB")

vets <- flex %>% 
  filter(!is.na(fl_per_tou))

mean_fl_per_tou <- mean(vets$fl_per_tou)
rep_fl_per_tou <- mean_fl_per_tou*1.4 #will be regressed later
max_fl_per_tou <- 0.035 #regressed later

#Bring down high vals
flex <- flex %>% 
  mutate(fl_per_tou = ifelse(fl_per_tou > max_fl_per_tou, max_fl_per_tou, fl_per_tou))

#NAs
flex <- flex %>% 
  mutate(fl_per_tou = ifelse(is.na(fl_per_tou), rep_fl_per_tou, fl_per_tou))

#regress
flex <- flex %>% 
  mutate(fl_per_tou = (fl_per_tou - mean_fl_per_tou)*0.5 + mean_fl_per_tou)

#predict touches and fl
flex <- flex %>% 
  mutate(touches_pred = rus_att_pred + rec_pred,
         fl_pred = touches_pred*fl_per_tou)

#flex fumbles
flex_fumbles <- flex %>% 
  select(player:Team_s, fl_per_tou) %>% 
  mutate(QB_fl = 0)

#select
flex <- flex %>% 
  select(player:int_pred, fl_pred)

##Recombine
player_predictions <- rbind(QBs, flex)

#fumbles
player_fumbles <- rbind(QB_fumbles, flex_fumbles)

colnames(player_fumbles) <- c("player", "Position", "Team", "Team_s", "py_fl_per_tou", "py_qb_fl")

#Full Season
player_predictions[5:17] <- player_predictions[5:17]*17

#Fix negative predictions
player_predictions[player_predictions < 0] <- 0

#Add up fantasy points
player_predictions <- player_predictions %>% 
  mutate(fpts_pred = pas_yds_pred*0.04 + pas_tds_pred*4 + rus_yds_pred*0.1 + rus_tds_pred*6 + rec_yds_pred*0.1 + rec_tds_pred*6 + 0.5*rec_pred - 1*int_pred - 2*fl_pred)

#Filter out players with no team or NA preds
player_predictions <- player_predictions %>% 
  filter(!is.na(Team)) %>% 
  filter(!is.na(fpts_pred))

#write_csv
write_csv(player_predictions, eval(paste("~/R Stuff/FantasyFootball 2.0/old_fullSeasonPredictions/", This_Year, "/Full_Season_Predictions_", This_Year, ".csv", sep = "")))
write_csv(player_fumbles, eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Fumbles_Ratings_", This_Year, ".csv", sep = "")))





