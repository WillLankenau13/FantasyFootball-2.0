

#download files
player_percents <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Player_Percents.csv", sep = "")))
qb_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/QB_Ratings.csv", sep = "")))
off_team_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Off_Team_Ratings.csv", sep = "")))
def_team_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Def_Team_Ratings.csv", sep = "")))
starting_qbs <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/startingQBs/Starting_QBs_", This_Year, ".csv", sep = "")))


mean(off_team_ratings$off_pas_yds_rat)
mean(def_team_ratings$def_pas_yds_rat)


starting_qb_ratings <- starting_qbs %>% 
  select(!team) %>% 
  left_join(qb_ratings, by = c("player"))

qb_adj <- 0.7

QB_adj_off_team_ratings <- left_join(off_team_ratings, starting_qb_ratings, by = c("team")) %>%
  mutate(off_cmp_rat = off_cmp_rat*(1-qb_adj) + cmp_rat*qb_adj,
         off_pas_att_rat = off_pas_att_rat*(1-qb_adj) + pas_att_rat*qb_adj,
         off_pas_yds_rat = off_pas_yds_rat*(1-qb_adj) + pas_yds_rat*qb_adj,
         off_pas_tds_rat = off_pas_tds_rat*(1-qb_adj) + pas_tds_rat*qb_adj,
         off_int_rat = off_int_rat*(1-qb_adj) + int_rat*qb_adj) %>%
  select(team:off_int_rat)


#average defense
team_predictions <- QB_adj_off_team_ratings %>% 
  mutate(def_rus_att_rat = mean(def_team_ratings$def_rus_att_rat),
         def_rus_yds_rat = mean(def_team_ratings$def_rus_yds_rat),
         def_rus_tds_rat = mean(def_team_ratings$def_rus_tds_rat),
         def_pas_att_rat = mean(def_team_ratings$def_pas_att_rat),
         def_cmp_rat = mean(def_team_ratings$def_cmp_rat),
         def_pas_yds_rat = mean(def_team_ratings$def_pas_yds_rat),
         def_pas_tds_rat = mean(def_team_ratings$def_pas_tds_rat),
         def_int_rat = mean(def_team_ratings$def_int_rat))


####Tean Predictions####
#combining coefficients
#well tested
cmp_off_coef <- 0.7
pas_att_off_coef <- 0.7
pas_yds_off_coef <- 0.8
pas_tds_off_coef <- 0.8
int_off_coef <- 0.7
rus_att_off_coef <- 0.6
rus_yds_off_coef <- 0.5
rus_tds_off_coef <- 0.5


#combine offense and defense
combine_predictions <- function(df, col){
  #percent rating coefficient
  off_coef <- get(paste(col, "_off_coef", sep = ""))
  
  #rcombine
  df[, paste("team_", col, "_pred", sep = "")] <- ((df[, paste("off_", col, "_rat", sep = "")]*off_coef) + (df[, paste("def_", col, "_rat", sep = "")]*(1 - off_coef)))
  
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


####Player Predictions####
player_predictions <- player_percents %>% 
  full_join(team_predictions, by = c("team")) %>% 
  filter(pos != "QB" | player %in% starting_qbs$player)

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
  select(player:team, pas_att_pred:rus_tds_pred)



####totals
season_player_predictions <- player_predictions
season_player_predictions[, 4:15] <- season_player_predictions[, 4:15]*17

#fpts
season_player_predictions <- season_player_predictions %>% 
  mutate(fpts_pred = pas_yds_pred*0.04 + pas_tds_pred*4 + rus_yds_pred*0.1 + rus_tds_pred*6 + rec_yds_pred*0.1 + rec_tds_pred*6 + 0.5*rec_pred - 1*int_pred)

  


write_csv(season_player_predictions, eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonPredictions/", This_Year, "/Player_Predictions.csv", sep = "")))


