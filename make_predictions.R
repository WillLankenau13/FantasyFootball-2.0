# library("ggplot2")
# library("tidyverse")
# library("lubridate")
# library("incidence")
# library("stringr")
# library("janitor")
# library("readr")
# library("dplyr")
# library("modelr")
# library("leaps")
# library("ggrepel")

#Week
# upcoming_week <- 2

#Year
This_Year <- Years_Dataframe$This_Year[1]

#inactives list
inactive_designations <- c("O", "SUSP", "PUP", "IR")

#import files
player_percents <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", upcoming_week, "/Player_Percents.csv", sep = "")))
QB_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", upcoming_week, "/QB_ratings.csv", sep = "")))
off_team_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", upcoming_week, "/Off_Team_ratings.csv", sep = "")))
def_team_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", upcoming_week, "/Def_Team_ratings.csv", sep = "")))
starting_qbs <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/startingQBs/", This_Year, "/Week_", upcoming_week, "_Starting_QBs.csv", sep = "")))
teams <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/teams.csv", sep = "")))



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

#injury status
injury_status <- yahoo %>% 
  select(player, Position, `Injury Status`)
colnames(injury_status) = c("player", "pos", "injury_status")


####QB Adjustment####
starting_qbs <- player_names_func(starting_qbs)

QB_ratings_no_team <- QB_ratings %>% 
  select(!team)
starting_qb_ratings <- starting_qbs %>% 
  left_join(QB_ratings_no_team, by = c("QB" = "player"))

yahoo <- yahoo %>% 
  full_join(starting_qbs, by = c("Team" = "team")) %>% 
  mutate(startingQB = ifelse(player == QB, 1, 0)) %>% 
  filter(Position != "QB" | startingQB == 1)

##Replacement
pas_att_rep = 20
cmp_rep = 9
pas_yds_rep = 150
pas_tds_rep = 0.6
int_rep = 1

starting_qb_ratings <- starting_qb_ratings %>% 
  mutate(pas_att_rat = ifelse(is.na(pas_att_rat), pas_att_rep, pas_att_rat),
         cmp_rat = ifelse(is.na(cmp_rat), cmp_rep, cmp_rat),
         pas_yds_rat = ifelse(is.na(pas_yds_rat), pas_yds_rep, pas_yds_rat),
         pas_tds_rat = ifelse(is.na(pas_tds_rat), pas_tds_rep, pas_tds_rat),
         int_rat = ifelse(is.na(int_rat), int_rep, int_rat),
         games_played = ifelse(is.na(games_played), 0, games_played),
         py_games_played = ifelse(is.na(py_games_played),0, py_games_played))

QB_adj_off_team_ratings <- left_join(off_team_ratings, starting_qb_ratings, by = c("team")) %>%
  mutate(off_cmp_rat = off_cmp_rat*0.2 + cmp_rat*0.8,
         off_pas_att_rat = off_pas_att_rat*0.2 + pas_att_rat*0.8,
         off_pas_yds_rat = off_pas_yds_rat*0.2 + pas_yds_rat*0.8,
         off_pas_tds_rat = off_pas_tds_rat*0.2 + pas_tds_rat*0.8,
         off_int_rat = off_int_rat*0.2 + int_rat*0.8) %>%
  select(team:off_int_rat)

####update ratings for active rushers and receivers####
adjusted <- left_join(yahoo, player_percents, by = c("player", "Position" = "pos")) %>% 
  clean_names() %>% 
  select(player, position, team, opponent, injury_status, games_played:py_fl_per_tou, touches, fmb) %>% 
  rename("pos" = "position")

adjusted[, 6:18][is.na(adjusted[, 6:18])] <- 0

#Get percent by team
adjusted_by_team <- adjusted %>% 
  group_by(team) %>% 
  summarise(tot_rus_att_per = sum(adj_rus_att_per),
            tot_rus_yds_per = sum(adj_rus_yds_per),
            tot_rus_tds_per = sum(adj_rus_tds_per),
            tot_tgt_per = sum(adj_tgt_per),
            tot_rec_per = sum(adj_rec_per),
            tot_rec_yds_per = sum(adj_rec_yds_per),
            tot_rec_tds_per = sum(adj_rec_tds_per))

#Adjust individuals
adjusted <- adjusted %>% 
  full_join(adjusted_by_team, by = c("team")) %>% 
  mutate(adj_rus_att_per = adj_rus_att_per/tot_rus_att_per,
         adj_rus_yds_per = adj_rus_yds_per/tot_rus_yds_per,
         adj_rus_tds_per = adj_rus_tds_per/tot_rus_tds_per,
         adj_tgt_per = adj_tgt_per/tot_tgt_per,
         adj_rec_per = adj_rec_per/tot_rec_per,
         adj_rec_yds_per = adj_rec_yds_per/tot_rec_yds_per,
         adj_rec_tds_per = adj_rec_tds_per/tot_rec_tds_per) %>% 
  select(player:injury_status, games_played:fmb, adj_rus_att_per:adj_rec_tds_per, py_qb_fl, py_fl_per_tou)


####update offensive ratings####
#coefs, not tested much
#new3
# rus_att <- 0.3
# rus_yds <- 0.3
# rus_tds <- 0.25
# pas_att <- 0.2
# cmp <- 0.2
# pas_yds <- 0.15
# pas_tds <- 0.15

#new2
# rus_att <- 0.5
# rus_yds <- 0.5
# rus_tds <- 0.4
# pas_att <- 0.2
# cmp <- 0.2
# pas_yds <- 0.3
# pas_tds <- 0.3

rus_att <- 0.3
rus_yds <- 0.3
rus_tds <- 0.1
pas_att <- 0.1
cmp <- 0.1
pas_yds <- 0.2
pas_tds <- 0.2

#No adjust for players
# adjusted_off_team_ratings <- QB_adj_off_team_ratings %>%
#   full_join(adjusted_by_team, by = c("team")) %>%
#   transmute(team = team,
#             adj_rus_att_rat = off_rus_att_rat,
#             adj_rus_yds_rat = off_rus_yds_rat,
#             adj_rus_tds_rat = off_rus_tds_rat,
#             adj_pas_att_rat = off_pas_att_rat,
#             adj_cmp_rat = off_cmp_rat,
#             adj_pas_yds_rat = off_pas_yds_rat,
#             adj_pas_tds_rat = off_pas_tds_rat,
#             adj_int_rat = off_int_rat)

#new
adjusted_off_team_ratings <- QB_adj_off_team_ratings %>%
  full_join(adjusted_by_team, by = c("team")) %>%
  transmute(team = team,
            adj_rus_att_rat = off_rus_att_rat*((tot_rus_att_per-1)*rus_att + 1),
            adj_rus_yds_rat = off_rus_yds_rat*((tot_rus_yds_per-1)*rus_yds + 1),
            adj_rus_tds_rat = off_rus_tds_rat*((tot_rus_tds_per-1)*rus_tds + 1),
            adj_pas_att_rat = off_pas_att_rat*((tot_tgt_per-1)*pas_att + 1),
            adj_cmp_rat = off_cmp_rat*((tot_rec_per-1)*cmp + 1),
            adj_pas_yds_rat = off_pas_yds_rat*((tot_rec_yds_per-1)*pas_yds + 1),
            adj_pas_tds_rat = off_pas_tds_rat*((tot_rec_tds_per-1)*pas_tds + 1),
            adj_int_rat = off_int_rat)

#old
# adjusted_off_team_ratings <- QB_adj_off_team_ratings %>%
#   full_join(adjusted_by_team, by = c("team")) %>%
#   transmute(team = team,
#             adj_rus_att_rat = off_rus_att_rat*((1-tot_rus_att_per)*rus_att + tot_rus_att_per),
#             adj_rus_yds_rat = off_rus_yds_rat*((1-tot_rus_yds_per)*rus_yds + tot_rus_yds_per),
#             adj_rus_tds_rat = off_rus_tds_rat*((1-tot_rus_tds_per)*rus_tds + tot_rus_tds_per),
#             adj_pas_att_rat = off_pas_att_rat*((1-tot_tgt_per)*pas_att + tot_tgt_per),
#             adj_cmp_rat = off_cmp_rat*((1-tot_rec_per)*cmp + tot_rec_per),
#             adj_pas_yds_rat = off_pas_yds_rat*((1-tot_rec_yds_per)*pas_yds + tot_rec_yds_per),
#             adj_pas_tds_rat = off_pas_tds_rat*((1-tot_rec_tds_per)*pas_tds + tot_rec_tds_per),
#             adj_int_rat = off_int_rat)

####Get matchups####
matchups <- yahoo %>% 
  filter(startingQB == 1) %>% 
  select(Team, Opponent) %>% 
  distinct()

team_predictions <- matchups %>% 
  left_join(adjusted_off_team_ratings, by = c("Team" = "team")) %>% 
  left_join(def_team_ratings, by = c("Opponent" = "team")) %>% 
  clean_names()


####Tean Predictions####
#combining coefficients
cmp_off_coef <- 0.7
pas_att_off_coef <- 0.7
pas_yds_off_coef <- 0.65
pas_tds_off_coef <- 0.6
int_off_coef <- 0.8

rus_att_off_coef <- 0.6
rus_yds_off_coef <- 0.5
rus_tds_off_coef <- 0.5


#combine offense and defense
combine_predictions <- function(df, col){
  #percent rating coefficient
  off_coef <- get(paste(col, "_off_coef", sep = ""))
  
  #rcombine
  df[, paste("team_", col, "_pred", sep = "")] <- ((df[, paste("adj_", col, "_rat", sep = "")]*off_coef) + (df[, paste("def_", col, "_rat", sep = "")]*(1 - off_coef)))
  
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

####player predictions####
player_predictions <- adjusted %>% 
  full_join(team_predictions, by = c("team")) 

player_predictions <- player_predictions %>%
  mutate(is_QB = ifelse(pos == "QB", 1, 0)) %>% 
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
  select(player:opponent, pas_att_pred:rus_tds_pred)

####Fumbles####
rep_fl_per_tou <- 0.007
rep_py_qb_fl <- 0.21

fumbles <- player_percents %>%
  select(player, pos, team, games_played:fmb, py_qb_fl, py_fl_per_tou) %>% 
  mutate(touches = ifelse(touches == 0 & fmb > 0, fmb, touches),
    adj_fl_per_tou = (games_played/(py_games_played + games_played))*(fmb/touches) + (py_games_played/(py_games_played + games_played))*(py_fl_per_tou),
    adj_fl_per_tou = ifelse(adj_fl_per_tou > 0.1, 0.1, adj_fl_per_tou),
    adj_fl_per_tou = ifelse(is.na(adj_fl_per_tou), rep_fl_per_tou, adj_fl_per_tou),
    adj_qb_fl = (3*games_played/(py_games_played + 3*games_played))*(fmb/games_played) + (py_games_played/(py_games_played + 3*games_played))*(py_qb_fl),
    adj_qb_fl = ifelse((py_games_played < 5 & games_played < 5 & adj_qb_fl > 0.45), 0.45, adj_qb_fl)) %>% 
  select(player, pos, team, games_played:py_fl_per_tou, adj_fl_per_tou, adj_qb_fl)

player_predictions <- player_predictions %>%
  left_join(fumbles, by = c("player", "pos", "team")) %>%
  mutate(fl_pred = ifelse(pos == "QB", adj_qb_fl, adj_fl_per_tou*(rec_pred + rus_att_pred))) %>%
select(player:rus_tds_pred, py_fl_per_tou, py_qb_fl, fl_pred)

####FPTS Predictions####
player_predictions[, 5:19][is.na(player_predictions[, 5:19])] <- 0

player_predictions <- player_predictions %>% 
  mutate(fpts_pred = pas_yds_pred*0.04 + pas_tds_pred*4 + rus_yds_pred*0.1 + rus_tds_pred*6 + rec_yds_pred*0.1 + rec_tds_pred*6 + 0.5*rec_pred - 1*int_pred - 2*fl_pred)

####Clean Adjusted####
adjusted <- adjusted %>% 
  select(player:team, games_played:adj_rec_tds_per, py_qb_fl, py_fl_per_tou)

####Injury status####
adjusted <- adjusted %>%
  left_join(injury_status, by = c("player", "pos"))

####Write csv####
write_csv(player_predictions, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyPredictions/", This_Year, "/Week_", upcoming_week, "_Player_Predictions.csv", sep = "")))
write_csv(team_predictions, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyPredictions/", This_Year, "/Week_", upcoming_week, "_Team_predictions.csv", sep = "")))

write_csv(adjusted, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyAdjusted/", This_Year, "/Week_", upcoming_week, "/Player_Percents_Adjusted.csv", sep = "")))

