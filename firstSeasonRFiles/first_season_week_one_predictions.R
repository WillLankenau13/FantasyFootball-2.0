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

#Week
upcoming_week <- 1

###Years
Past_Year <- Years_Dataframe$Past_Year[1]
This_Year <- Years_Dataframe$This_Year[1]

###read files
Receiving_player_percents <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Receiving_Percents_", This_Year, ".csv", sep = "")))
Rushing_player_percents <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Rushing_Percents_", This_Year, ".csv", sep = "")))
player_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Player_Ratings_", This_Year, ".csv", sep = ""))) %>% 
  clean_names() %>% 
  rename("Position" = "pos")
player_fumbles <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Fumbles_Ratings_", This_Year, ".csv", sep = ""))) %>% 
  select(!Team)
off_team_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Offensive_Ratings_", This_Year, ".csv", sep = ""))) %>% 
  rename("Long_Team" = "Team")
def_team_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Defensive_Ratings_", This_Year, ".csv", sep = "")))
teams <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/teams.csv", sep = "")))

#player_names_func
Receiving_player_percents <- player_names_func(Receiving_player_percents)
Rushing_player_percents <- player_names_func(Rushing_player_percents)
player_ratings <- player_names_func(player_ratings)
player_fumbles <- player_names_func(player_fumbles)
off_team_ratings <- player_names_func(off_team_ratings)
def_team_ratings <- player_names_func(def_team_ratings)

#inactives list
inactive_designations <- c("O", "SUSP", "PUP", "IR")

#yahoo
yahoo <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/Yahoo/", This_Year, "/Yahoo_Week_", upcoming_week, ".csv", sep = ""))) %>% 
  select(ID:Starting) %>% 
  mutate(player = paste(`First Name`, `Last Name`)) %>% 
  filter(!is.na(ID)) %>% 
  filter(!(`Injury Status` %in% inactive_designations))

yahoo <- player_names_func(yahoo)

yahoo <- yahoo %>% 
  left_join(teams, by = c("Team" = "Yahoo")) %>% 
  select(ID:Position, Short_Name, Opponent:player) %>% 
  rename("Team" = "Short_Name") %>% 
  left_join(teams, by = c("Opponent" = "Yahoo")) %>% 
  select(ID:Position, Team, Short_Name, Game:player) %>% 
  rename("Opponent" = "Short_Name") 

#combine player percents
combined_player_percents <- full_join(Receiving_player_percents, Rushing_player_percents, by = c("player", "Team", "Position")) %>% 
  select(player:player_games.x, Pick.x, adj_tgt_per:adj_rec_tds_per, player_games.y, Pick.y, adj_rus_att_per:adj_rus_tds_per) %>% 
  mutate(Pick = ifelse(is.na(Pick.x), Pick.y, Pick.x),
         player_games = ifelse(is.na(player_games.x), player_games.y, player_games.x)) %>% 
  select(player:Team, player_games, Pick, adj_tgt_per:adj_rec_tds_per, adj_rus_att_per:adj_rus_tds_per)

combined_player_percents[, 6:12][is.na(combined_player_percents[, 6:12])] <- 0
combined_player_percents[, 4][is.na(combined_player_percents[, 4])] <- 0

###update ratings for starting QB
#starting qbs
not_starting <- c("Deshaun Watson", "Kenny Pickett")

off_team_ratings[off_team_ratings == "Deshaun Watson"] <- "Jacoby Brissett"
off_team_ratings[off_team_ratings == "Kenny Pickett"] <- "Mitchell Trubisky"

#select cols
off_team_ratings <- off_team_ratings %>% 
  select(Long_Team, Team_s, QB, Off_Rus_Att_Rat:Off_Int_Rat)

#offensive ratings for yahoo players
off_ratings <- left_join(off_team_ratings, yahoo, by = c("QB" = "player"))

#QB ratings
QB_ratings <- player_ratings %>% 
  filter(Position == "QB")

#starting QB df
starting_qbs <- off_ratings %>% 
  select(Team_s, QB)

yahoo <- yahoo %>% 
  full_join(starting_qbs, by = c("Team" = "Team_s")) %>% 
  mutate(startingQB = ifelse(player == QB, 1, 0)) %>% 
  filter(Position != "QB" | startingQB == 1)

#injury status
injury_status <- yahoo %>% 
  select(player, Position, `Injury Status`)
colnames(injury_status) = c("player", "pos", "injury_status")

#starting QBs ratings
starting_qb_ratings <- left_join(starting_qbs, player_ratings, by = c("QB" = "player")) %>% 
  select(Team_s:player_gs, pl_cmp:pl_pas_tds, pl_int)

colnames(starting_qb_ratings) <- c("Team_s", "QB", "team", "Pos", "QB_Games", "player_gs", "QB_Cmp", "QB_Att", "QB_Yds", "QB_TDs", "QB_Int")

n = length(starting_qb_ratings$QB_Cmp)
Cmp_rep = 0.9/3*(sort(starting_qb_ratings$QB_Cmp)[2] + sort(starting_qb_ratings$QB_Cmp)[3] + sort(starting_qb_ratings$QB_Cmp)[4])
Att_rep = 0.9/3*(sort(starting_qb_ratings$QB_Att)[2] + sort(starting_qb_ratings$QB_Att)[3] + sort(starting_qb_ratings$QB_Att)[4])
Yds_rep = 0.9/3*(sort(starting_qb_ratings$QB_Yds)[2] + sort(starting_qb_ratings$QB_Yds)[3] + sort(starting_qb_ratings$QB_Yds)[4])
Tds_rep = 0.9/3*(sort(starting_qb_ratings$QB_TDs)[2] + sort(starting_qb_ratings$QB_TDs)[3] + sort(starting_qb_ratings$QB_TDs)[4])
Int_rep = 1.1/3*(sort(starting_qb_ratings$QB_Int)[n-1] + sort(starting_qb_ratings$QB_Int)[n-2] + sort(starting_qb_ratings$QB_Int)[n-3])

#Replace with replacement level if necessary
starting_qb_ratings <- starting_qb_ratings %>% 
  mutate(QB_Cmp = ifelse(is.na(QB_Cmp) | (QB_Games < 10 & QB_Cmp < Cmp_rep), Cmp_rep, QB_Cmp),
         QB_Att = ifelse(is.na(QB_Att) | (QB_Games < 10 & QB_Att < Att_rep), Att_rep, QB_Att),
         QB_Yds = ifelse(is.na(QB_Yds) | (QB_Games < 10 & QB_Yds < Yds_rep), Yds_rep, QB_Yds),
         QB_TDs = ifelse(is.na(QB_TDs) | (QB_Games < 10 & QB_TDs < Tds_rep), Tds_rep, QB_TDs),
         QB_Int = ifelse(is.na(QB_Int) | (QB_Games < 10 & QB_Int < Int_rep), Int_rep, QB_Int))


QB_adj_off_ratings <- left_join(off_ratings, starting_qb_ratings, by = c("QB", "Team_s")) %>% 
  mutate(Off_Cmp_Rat = Off_Cmp_Rat*0.2 + QB_Cmp*0.8,
         Off_Pas_Att_Rat = Off_Pas_Att_Rat*0.2 + QB_Att*0.8,
         Off_Pas_Yds_Rat = Off_Pas_Yds_Rat*0.2 + QB_Yds*0.8,
         Off_Pas_Tds_Rat = Off_Pas_Tds_Rat*0.2 + QB_TDs*0.8,
         Off_Int_Rat = Off_Int_Rat*0.2 + QB_Int*0.8) %>% 
  select(Long_Team:Starting)


###update ratings for active rushers
adjusted_rushing <- left_join(yahoo, Rushing_player_percents, by = c("player", "Team", "Position")) %>% 
  select(player, Position, Team, Opponent, player_games, `Injury Status`, adj_rus_att_per:adj_rus_tds_per) %>% 
  rename("past_year_games_played" = "player_games")
  
adjusted_rushing$adj_rus_att_per[is.na(adjusted_rushing$adj_rus_att_per)] <- 0
adjusted_rushing$adj_rus_yds_per[is.na(adjusted_rushing$adj_rus_yds_per)] <- 0
adjusted_rushing$adj_rus_tds_per[is.na(adjusted_rushing$adj_rus_tds_per)] <- 0

#Get percent by team
adjusted_rushing_by_team <- adjusted_rushing %>% 
  group_by(Team) %>% 
  summarise(tot_rus_att_per = sum(adj_rus_att_per),
            tot_rus_yds_per = sum(adj_rus_yds_per),
            tot_rus_tds_per = sum(adj_rus_tds_per))

#Adjust individuals
adjusted_rushing <- adjusted_rushing %>% 
  full_join(adjusted_rushing_by_team, by = c("Team")) %>% 
  mutate(adj_rus_att_per = adj_rus_att_per/tot_rus_att_per,
         adj_rus_yds_per = adj_rus_yds_per/tot_rus_yds_per,
         adj_rus_tds_per = adj_rus_tds_per/tot_rus_tds_per) %>% 
  select(player:`Injury Status`, adj_rus_att_per:adj_rus_tds_per)


###update ratings for active receivers
adjusted_receiving <- left_join(yahoo, Receiving_player_percents, by = c("player", "Team", "Position")) %>% 
  select(player, Position, Team, Opponent, player_games, `Injury Status`, adj_tgt_per:adj_rec_tds_per) %>% 
  rename("past_year_games_played" = "player_games")

adjusted_receiving$adj_tgt_per[is.na(adjusted_receiving$adj_tgt_per)] <- 0
adjusted_receiving$adj_rec_per[is.na(adjusted_receiving$adj_rec_per)] <- 0
adjusted_receiving$adj_rec_yds_per[is.na(adjusted_receiving$adj_rec_yds_per)] <- 0
adjusted_receiving$adj_rec_tds_per[is.na(adjusted_receiving$adj_rec_tds_per)] <- 0

#Sum percents by team
adjusted_receiving_by_team <- adjusted_receiving %>% 
  group_by(Team) %>% 
  summarise(tot_tgt_per = sum(adj_tgt_per),
            tot_rec_per = sum(adj_rec_per),
            tot_rec_yds_per = sum(adj_rec_yds_per),
            tot_rec_tds_per = sum(adj_rec_tds_per))

#Adjust invididual percents
adjusted_receiving <- adjusted_receiving %>% 
  full_join(adjusted_receiving_by_team, by = c("Team")) %>% 
  mutate(adj_tgt_per = adj_tgt_per/tot_tgt_per,
         adj_rec_per = adj_rec_per/tot_rec_per,
         adj_rec_yds_per = adj_rec_yds_per/tot_rec_yds_per,
         adj_rec_tds_per = adj_rec_tds_per/tot_rec_tds_per) %>% 
  select(player:`Injury Status`, adj_tgt_per:adj_rec_tds_per)


###Adjusted Combined
adjusted_combined <- full_join(adjusted_receiving, adjusted_rushing, by = c("player", "Team", "Opponent", "Position", "Injury Status")) %>% 
  mutate(past_year_games_played = ifelse(is.na(past_year_games_played.x), past_year_games_played.y, past_year_games_played.x)) %>% 
  select(!past_year_games_played.x) %>% 
  select(!past_year_games_played.y) %>% 
  filter(Position != "DEF") %>% 
  mutate(games_played = 0,
         touches = 0,
         fmb = 0)

###update offensive ratings
#coefs for regression to healthy team, not tested at all, just vibes
#Numbers are high because tot_rus_att_per is always less than 1 for the first season week 1
rus_att <- 0.6
rus_yds <- 0.6
rus_tds <- 0.5
pas_att <- 0.9
cmp <- 0.9
pas_yds <- 0.8
pas_tds <- 0.8

adjusted_off_ratings <- QB_adj_off_ratings %>% 
  full_join(adjusted_rushing_by_team, by = c("Team_s" = "Team")) %>% 
  full_join(adjusted_receiving_by_team, by = c("Team_s" = "Team")) %>% 
  transmute(Team = Team_s,
            QB = QB,
            adj_rus_att_rat = Off_Rus_Att_Rat*((1-tot_rus_att_per)*rus_att + tot_rus_att_per),
            adj_rus_yds_rat = Off_Rus_Yds_Rat*((1-tot_rus_yds_per)*rus_yds + tot_rus_yds_per),
            adj_rus_tds_rat = Off_Rus_Tds_Rat*((1-tot_rus_tds_per)*rus_tds + tot_rus_tds_per),
            adj_pas_att_rat = Off_Pas_Att_Rat*((1-tot_tgt_per)*pas_att + tot_tgt_per),
            adj_cmp_rat = Off_Cmp_Rat*((1-tot_rec_per)*cmp + tot_rec_per),
            adj_pas_yds_rat = Off_Pas_Yds_Rat*((1-tot_rec_yds_per)*pas_yds + tot_rec_yds_per),
            adj_pas_tds_rat = Off_Pas_Tds_Rat*((1-tot_rec_tds_per)*pas_tds + tot_rec_tds_per),
            adj_int_rat = Off_Int_Rat)


###Get matchups
matchups <- yahoo %>% 
  filter(startingQB == 1) %>% 
  select(Team, Opponent) %>% 
  distinct()

team_predictions <- matchups %>% 
  left_join(adjusted_off_ratings, by = c("Team")) %>% 
  left_join(def_team_ratings, by = c("Opponent" = "Short_Name")) %>% 
  clean_names

#combining coefficients
cmp_off_coef <- 0.7
pas_att_off_coef <- 0.7
pas_yds_off_coef <- 0.7
pas_tds_off_coef <- 0.7
int_off_coef <- 0.8

rus_att_off_coef <- 0.6
rus_yds_off_coef <- 0.5
rus_tds_off_coef <- 0.5


#combine offense and defense
combine_predictions <- function(df, col){
  #percent rating coefficient
  off_coef <- get(paste(col, "_off_coef", sep = ""))
  
  #rcombine
  df[, paste("team_", col, "_pred", sep = "")] <- ((df[, paste("adj_", col, "_rat", sep = "")]*off_coef) + (df[, paste("def_", col, sep = "")]*(1 - off_coef)))
  
  return(df)
}

team_predictions <- combine_predictions(team_predictions, "pas_att")
team_predictions <- combine_predictions(team_predictions, "cmp")
team_predictions <- combine_predictions(team_predictions, "pas_yds")
team_predictions <- combine_predictions(team_predictions, "pas_tds")
team_predictions <- combine_predictions(team_predictions, "rus_att")
team_predictions <- combine_predictions(team_predictions, "rus_yds")
team_predictions <- combine_predictions(team_predictions, "rus_tds")
team_predictions <- combine_predictions(team_predictions, "int")

#select cols
team_predictions <- team_predictions %>% 
  select(team, team_pas_att_pred:team_int_pred)


###player predictions for player percents
player_predictions <- adjusted_combined %>% 
  full_join(team_predictions, by = c("Team" = "team")) 

player_predictions <- player_predictions %>%
  mutate(is_QB = ifelse(Position == "QB", 1, 0)) %>% 
  mutate(pas_att_pred = team_pas_att_pred*is_QB,
         cmp_pred = team_cmp_pred*is_QB,
         pas_yds_pred = team_pas_yds_pred*is_QB,
         pas_tds_pred = team_pas_tds_pred*is_QB,
         int_pred = team_int_pred*is_QB,
         tgt_pred = team_pas_att_pred*adj_tgt_per,
         rec_pred = team_cmp_pred*adj_rec_per,
         rec_yds_pred = team_pas_yds_pred*adj_rec_yds_per,
         rec_tds_pred = team_pas_tds_pred*adj_rec_tds_per,
         rus_att_pred = team_rus_att_pred*adj_rus_att_per,
         rus_yds_pred = team_rus_yds_pred*adj_rus_yds_per,
         rus_tds_pred = team_rus_tds_pred*adj_rus_tds_per) %>% 
  select(player:Opponent, pas_att_pred:rus_tds_pred)

###Fumbles
player_predictions <- player_predictions %>% 
  left_join(player_fumbles, by = c("player", "Team" = "Team_s", "Position")) 

player_predictions[is.na(player_predictions)] <- 0

player_predictions <- player_predictions %>% 
  mutate(fl_pred = ifelse(Position == "QB", py_qb_fl, py_fl_per_tou*(rus_att_pred + rec_pred)))

#combine with adjusted combined
adjusted_combined <- adjusted_combined %>% 
  left_join(player_fumbles, by = c("player", "Team" = "Team_s", "Position"))
adjusted_combined[, 6:15][is.na(adjusted_combined[, 6:15])] <- 0

#combine with combined player percents
combined_player_percents <- combined_player_percents %>% 
  left_join(player_fumbles, by = c("player", "Team" = "Team_s", "Position"))
combined_player_percents[, 6:14][is.na(combined_player_percents[, 6:14])] <- 0

#remove fumbles from QB ratings
QB_ratings <- QB_ratings %>% 
  select(player:pl_rus_tds, pl_int)

#####fix up QB ratings####
QB_ratings <- QB_ratings %>% 
  mutate(games_played = 0) %>% 
  rename("py_games_played" = "player_games") %>% 
  select(player:py_games_played, games_played, pl_pas_att, pl_cmp, pl_pas_yds, pl_pas_tds, pl_int)
colnames(QB_ratings) <- c("player", "team", "pos", "py_games_played", "games_played", "pas_att_rat", "cmp_rat", "pas_yds_rat", "pas_tds_rat", "int_rat")

n = length(QB_ratings$pas_att_rat)
pas_att_rep = 20
cmp_rep = 9
pas_yds_rep = 150
pas_tds_rep = 0.6
int_rep = 1

####Clean up team ratings####
off_team_ratings <- off_team_ratings %>% 
  select(Team_s:Off_Int_Rat) %>% 
  rename("team" = "Team_s") %>% 
  clean_names() %>% 
  select(!qb)

off_team_ratings <- off_team_ratings %>% 
  select(team, off_rus_att_rat, off_rus_yds_rat, off_rus_tds_rat, off_pas_att_rat, off_cmp_rat, off_pas_yds_rat, off_pas_tds_rat, off_int_rat)


def_team_ratings <- def_team_ratings %>% 
  rename("team" = "Short_Name") %>% 
  clean_names() %>% 
  select(!games)

colnames(def_team_ratings)[c(2, 3, 4, 5, 6, 7, 8, 9, 10)] <- paste(colnames(def_team_ratings)[c(2, 3, 4, 5, 6, 7, 8, 9, 10)], 'rat', sep = '_')

def_team_ratings <- def_team_ratings %>% 
  select(team, def_rus_att_rat, def_rus_yds_rat, def_rus_tds_rat, def_pas_att_rat, def_cmp_rat, def_pas_yds_rat, def_pas_tds_rat, def_int_rat)


####Clean up player percents####
combined_player_percents <- combined_player_percents %>% 
  rename("pos" = "Position") %>% 
  rename("team" = "Team") %>% 
  rename("py_games_played" = "player_games")  %>% 
  mutate(games_played = 0,
         touches = 0,
         fmb = 0) %>% 
  select(player, pos, team, games_played, py_games_played, touches, fmb, adj_rus_att_per, adj_rus_yds_per, adj_rus_tds_per, adj_tgt_per, adj_rec_per, adj_rec_yds_per, adj_rec_tds_per, py_qb_fl, py_fl_per_tou)

####Clean up adjusted combined####
adjusted_combined <- adjusted_combined %>% 
  rename("py_games_played" = "past_year_games_played") %>% 
  mutate(games_played = 0,
         touches = 0,
         fmb = 0) %>% 
  select(!`Injury Status`) %>%
  select(!Opponent) %>% 
  rename("pos" = "Position") %>% 
  rename("team" = "Team") %>% 
  select(player, pos, team, games_played, py_games_played, touches, fmb, adj_rus_att_per, adj_rus_yds_per, adj_rus_tds_per, adj_tgt_per, adj_rec_per, adj_rec_yds_per, adj_rec_tds_per, py_qb_fl, py_fl_per_tou)
  
##Injury status
adjusted_combined <- adjusted_combined %>%
  left_join(injury_status, by = c("player", "pos"))

#Replace with replacement level if necessary
QB_ratings <- QB_ratings %>% 
  mutate(pas_att_rat = ifelse(is.na(pas_att_rat) | (py_games_played < 5 & pas_att_rat < pas_att_rep), pas_att_rep, pas_att_rat),
         cmp_rat = ifelse(is.na(cmp_rat) | (py_games_played < 5 & cmp_rat < cmp_rep), cmp_rep, cmp_rat),
         pas_yds_rat = ifelse(is.na(pas_yds_rat) | (py_games_played < 5 & pas_yds_rat < pas_yds_rep), pas_yds_rep, pas_yds_rat),
         pas_tds_rat = ifelse(is.na(pas_tds_rat) | (py_games_played < 5 & pas_tds_rat < pas_tds_rep), pas_tds_rep, pas_tds_rat),
         int_rat = ifelse(is.na(int_rat) | (py_games_played < 5 & int_rat < int_rep), int_rep, int_rat)) %>% 
  select(!pos)

#####fantasy points####
player_predictions <- player_predictions %>% 
  mutate(fpts_pred = pas_yds_pred*0.04 + pas_tds_pred*4 + rus_yds_pred*0.1 + rus_tds_pred*6 + rec_yds_pred*0.1 + rec_tds_pred*6 + 0.5*rec_pred - 1*int_pred - 2*fl_pred)

#write csv
write_csv(player_predictions, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyPredictions/", This_Year, "/Week_", upcoming_week, "_Player_Predictions.csv", sep = "")))
write_csv(team_predictions, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyPredictions/", This_Year, "/Week_", upcoming_week, "_Team_predictions.csv", sep = "")))

write_csv(combined_player_percents, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", upcoming_week, "/Player_Percents.csv", sep = "")))
write_csv(off_team_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", upcoming_week, "/Off_Team_Ratings.csv", sep = "")))
write_csv(def_team_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", upcoming_week, "/Def_Team_Ratings.csv", sep = "")))
write_csv(QB_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", upcoming_week, "/QB_Ratings.csv", sep = "")))
write_csv(player_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", upcoming_week, "/Player_Ratings.csv", sep = "")))

write_csv(adjusted_combined, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyAdjusted/", This_Year, "/Week_", upcoming_week, "/Player_Percents_Adjusted.csv", sep = "")))






