


# #Week
# upcoming_week <- 1

#Year
This_Year <- This_Year_d

#inactives list
inactive_designations <- c("O", "SUSP", "PUP", "IR", "NFI", "D", "PI")
#PI is projected inactive

#import files
player_percents <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyRatings/", This_Year, "/Week_", upcoming_week, "/Player_Percents.csv", sep = "")))
QB_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyRatings/", This_Year, "/Week_", upcoming_week, "/QB_ratings.csv", sep = "")))
off_team_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyRatings/", This_Year, "/Week_", upcoming_week, "/Off_Team_Ratings.csv", sep = "")))
def_team_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyRatings/", This_Year, "/Week_", upcoming_week, "/Def_Team_Ratings.csv", sep = "")))
starting_qbs <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/startingQBs/", This_Year, "/Week_", upcoming_week, "_Starting_QBs.csv", sep = "")))
teams <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/utils/teams.csv", sep = "")))
# active_players <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/activePlayers/", This_Year, "/Week_", upcoming_week, "_Active_Players.csv", sep = ""))) %>%
#   mutate(active = 1)

# active_players <- player_names_func(active_players)

#yahoo
yahoo <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/Yahoo/", This_Year, "/Yahoo_Week_", upcoming_week, ".csv", sep = ""))) %>% 
  select(ID:Starting) %>% 
  mutate(player = paste(`First Name`, `Last Name`)) %>% 
  filter(!is.na(ID)) %>% 
  filter(!(`Injury Status` %in% inactive_designations)) %>% 
  rename("pos" = "Position")

yahoo <- player_names_func(yahoo)

yahoo <- yahoo %>% 
  left_join(teams, by = c("Team" = "Yahoo")) %>% 
  select(ID:pos, Short_Name, Opponent:player) %>% 
  rename("Team" = "Short_Name") %>% 
  left_join(teams, by = c("Opponent" = "Yahoo")) %>% 
  select(ID:pos, Team, Short_Name, Game:player) %>% 
  rename("Opponent" = "Short_Name") %>% 
  filter(pos != "DEF")

#active_players
# yahoo <- yahoo %>%
#   full_join(active_players, by = c("player", "Team" = "team", "pos")) %>%
#   filter(Position == "DEF" | active == 1 | `Injury Status` == "Q") %>%
#   select(!active)

#injury status
injury_status <- yahoo %>% 
  select(player, pos, `Injury Status`)
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
  filter(pos != "QB" | startingQB == 1)

##Replacement
pas_att_rep = 30
cmp_rep = 18
pas_yds_rep = 210
pas_tds_rep = 1
int_rep = 1
sc_att_rep = 1.4
sc_yds_rep = 12
sc_tds_rep = 0.05

starting_qb_ratings <- starting_qb_ratings %>% 
  mutate(pas_att_rat = ifelse(is.na(pas_att_rat), pas_att_rep, pas_att_rat),
         cmp_rat = ifelse(is.na(cmp_rat), cmp_rep, cmp_rat),
         pas_yds_rat = ifelse(is.na(pas_yds_rat), pas_yds_rep, pas_yds_rat),
         pas_tds_rat = ifelse(is.na(pas_tds_rat), pas_tds_rep, pas_tds_rat),
         int_rat = ifelse(is.na(int_rat), int_rep, int_rat),
         sc_att_rat = ifelse(is.na(sc_att_rat), sc_att_rep, sc_att_rat),
         sc_yds_rat = ifelse(is.na(sc_yds_rat), sc_yds_rep, sc_yds_rat),
         sc_tds_rat = ifelse(is.na(sc_tds_rat), sc_tds_rep, sc_tds_rat),
         games_played = ifelse(is.na(games_played), 0, games_played),
         py_games_played = ifelse(is.na(py_games_played),0, py_games_played))

#tested
qb_adj <- 0.7

QB_adj_off_team_ratings <- left_join(off_team_ratings, starting_qb_ratings, by = c("team")) %>%
  mutate(off_cmp_rat = off_cmp_rat*(1-qb_adj) + cmp_rat*qb_adj,
         off_pas_att_rat = off_pas_att_rat*(1-qb_adj) + pas_att_rat*qb_adj,
         off_pas_yds_rat = off_pas_yds_rat*(1-qb_adj) + pas_yds_rat*qb_adj,
         off_pas_tds_rat = off_pas_tds_rat*(1-qb_adj) + pas_tds_rat*qb_adj,
         off_int_rat = off_int_rat*(1-qb_adj) + int_rat*qb_adj) %>%
  select(team:off_int_rat, sc_att_rat:sc_tds_rat, off_rus_att_rat:off_rus_tds_rat)

####update ratings for active rushers and receivers####
adjusted <- left_join(yahoo, player_percents, by = c("player", "pos")) %>% 
  clean_names() %>% 
  select(player, pos, team, opponent, injury_status, py_games_played:rec_tds_per)

adjusted[, 6:14][is.na(adjusted[, 6:14])] <- 0

#filter out qb
#qbs unaffected by injuries and inactives
qb <- adjusted %>% 
  filter(pos == "QB")
qb_vals <- qb
colnames(qb)[8:14] <- paste0("adj_", colnames(qb)[8:14])
colnames(qb_vals)[8:14] <- paste0("qb_", colnames(qb_vals)[8:14])
qb_vals <- qb_vals %>% 
  select(player, team, qb_rus_att_per:qb_rus_tds_per) %>% 
  full_join(starting_qbs, by = c("player" = "QB", "team")) %>% 
  select(!player)
qb_vals[is.na(qb_vals[])] <- 0

adjusted <- adjusted %>% 
  filter(pos != "QB")

#Get percent by team
adjusted_by_team <- adjusted %>% 
  group_by(team) %>% 
  summarise(tot_rus_att_per = sum(rus_att_per),
            tot_rus_yds_per = sum(rus_yds_per),
            tot_rus_tds_per = sum(rus_tds_per),
            tot_tgt_per = sum(tgt_per),
            tot_rec_per = sum(rec_per),
            tot_rec_yds_per = sum(rec_yds_per),
            tot_rec_tds_per = sum(rec_tds_per))

#Adjust individuals
adjusted <- adjusted %>% 
  full_join(adjusted_by_team, by = c("team")) %>% 
  left_join(qb_vals, by = "team") %>% 
  mutate(adj_rus_att_per = (1-qb_rus_att_per)*rus_att_per/tot_rus_att_per,
         adj_rus_yds_per = (1-qb_rus_yds_per)*rus_yds_per/tot_rus_yds_per,
         adj_rus_tds_per = (1-qb_rus_tds_per)*rus_tds_per/tot_rus_tds_per,
         adj_tgt_per = tgt_per/tot_tgt_per,
         adj_rec_per = rec_per/tot_rec_per,
         adj_rec_yds_per = rec_yds_per/tot_rec_yds_per,
         adj_rec_tds_per = rec_tds_per/tot_rec_tds_per) %>% 
  select(player:injury_status, py_games_played, games_played, adj_rus_att_per:adj_rec_tds_per)


#bring back qbs
adjusted_by_team <- adjusted_by_team %>% 
  left_join(qb_vals, by = "team") %>% 
  mutate(tot_rus_att_per = tot_rus_att_per + qb_rus_att_per,
          tot_rus_yds_per = tot_rus_yds_per + qb_rus_yds_per,
          tot_rus_tds_per = tot_rus_tds_per + qb_rus_tds_per) %>% 
  select(team:tot_rec_tds_per)

adjusted <- rbind(adjusted, qb)

####update offensive ratings####
rus_att_upd_coef <- 0
rus_yds_upd_coef <- 0
rus_tds_upd_coef <- 0
pas_att_upd_coef <- 0.1
cmp_upd_coef <- 0.4
pas_yds_upd_coef <- 0.1
pas_tds_upd_coef <- 0

# rus_att <- 0.3
# rus_yds <- 0.3
# rus_tds <- 0.1
# pas_att <- 0.1
# cmp <- 0.1
# pas_yds <- 0.2
# pas_tds <- 0.2

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
            adj_cmp_rat = off_cmp_rat*((tot_rec_per-1)*cmp_upd_coef + 1),
            adj_pas_yds_rat = off_pas_yds_rat*((tot_rec_yds_per-1)*pas_yds_upd_coef + 1),
            adj_pas_tds_rat = off_pas_tds_rat*((tot_rec_tds_per-1)*pas_tds_upd_coef + 1),
            adj_int_rat = off_int_rat,
            adj_sc_att_rat = sc_att_rat,
            adj_sc_yds_rat = sc_yds_rat,
            adj_sc_tds_rat = sc_tds_rat,
            adj_rus_att_rat = off_rus_att_rat*((tot_rus_att_per-1)*rus_att_upd_coef + 1),
            adj_rus_yds_rat = off_rus_yds_rat*((tot_rus_yds_per-1)*rus_yds_upd_coef + 1),
            adj_rus_tds_rat = off_rus_tds_rat*((tot_rus_tds_per-1)*rus_tds_upd_coef + 1),
            adj_pas_att_rat = off_pas_att_rat*((tot_tgt_per-1)*pas_att_upd_coef + 1),
            )

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
  df[, paste("team_", col, "_pred", sep = "")] <- ((df[, paste("adj_", col, "_rat", sep = "")]*off_coef) + (df[, paste("def_", col, "_rat", sep = "")]*(1 - off_coef)))
  
  return(df)
}

team_predictions <- combine_predictions(team_predictions, "pas_att")
team_predictions <- combine_predictions(team_predictions, "cmp")
team_predictions <- combine_predictions(team_predictions, "pas_yds")
team_predictions <- combine_predictions(team_predictions, "pas_tds")
team_predictions <- combine_predictions(team_predictions, "int")
team_predictions <- combine_predictions(team_predictions, "rus_att")
team_predictions <- combine_predictions(team_predictions, "rus_yds")
team_predictions <- combine_predictions(team_predictions, "rus_tds")

#Note that scramble yards are only a factor of predicted scrambles. This is intentional and gives a good estimate
sc_td_coef =  0.2

team_predictions <- team_predictions %>% 
  mutate(team_sc_att_pred = adj_sc_att_rat,
         team_sc_yds_pred = team_sc_att_pred*7.52,
         team_sc_tds_pred = team_sc_att_pred*0.03*(1-sc_td_coef) + sc_td_coef*adj_sc_tds_rat)


#select cols
team_predictions <- team_predictions %>% 
  select(team, team_pas_att_pred:team_int_pred, team_sc_att_pred:team_sc_tds_pred, team_rus_att_pred:team_rus_tds_pred)

####Player Predictions####
player_predictions <- adjusted %>% 
  full_join(team_predictions, by = c("team")) 

player_predictions <- player_predictions %>%
  mutate(is_QB = ifelse(pos == "QB", 1, 0)) %>% 
  mutate(pas_att_pred = team_pas_att_pred*is_QB,
         cmp_pred = team_cmp_pred*is_QB,
         pas_yds_pred = team_pas_yds_pred*is_QB,
         pas_tds_pred = team_pas_tds_pred*is_QB,
         int_pred = team_int_pred*is_QB,
         sc_att_pred = team_sc_att_pred*is_QB,
         sc_yds_pred = team_sc_yds_pred*is_QB,
         sc_tds_pred = team_sc_tds_pred*is_QB,
         tgt_pred = team_pas_att_pred*adj_tgt_per,
         rec_pred = team_cmp_pred*adj_rec_per,
         rec_yds_pred = team_pas_yds_pred*adj_rec_yds_per,
         rec_tds_pred = team_pas_tds_pred*adj_rec_tds_per,
         rus_att_pred = team_rus_att_pred*adj_rus_att_per,
         rus_yds_pred = team_rus_yds_pred*adj_rus_yds_per,
         rus_tds_pred = team_rus_tds_pred*adj_rus_tds_per) %>% 
  select(player:opponent, pas_att_pred:rus_tds_pred)

#fumbles
player_predictions <- player_predictions %>% 
  mutate(fl_pred = ifelse(pos == "QB", pas_att_pred*0.005+sc_att_pred*0.033, (rus_att_pred+rec_pred)*0.005))


####FPTS Predictions####
player_predictions[, 5:20][is.na(player_predictions[, 5:20])] <- 0

player_predictions <- player_predictions %>% 
  mutate(fpts_pred = pas_yds_pred*0.04 + pas_tds_pred*4 + sc_yds_pred*0.1 + sc_tds_pred*6 + rus_yds_pred*0.1 + rus_tds_pred*6 + rec_yds_pred*0.1 + rec_tds_pred*6 + 0.5*rec_pred - 1*int_pred - 2*fl_pred)

####Clean Adjusted####
adjusted <- adjusted %>% 
  select(player:team, py_games_played:adj_rec_tds_per)

####Injury status####
adjusted <- adjusted %>%
  left_join(injury_status, by = c("player", "pos"))

####Write csv####
write_csv(player_predictions, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyPredictions/", This_Year, "/Week_", upcoming_week, "_Player_Predictions.csv", sep = "")))
write_csv(team_predictions, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyPredictions/", This_Year, "/Week_", upcoming_week, "_Team_Predictions.csv", sep = "")))

write_csv(adjusted, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyAdjusted/", This_Year, "/Week_", upcoming_week, "/Player_Percents_Adjusted.csv", sep = "")))


####For Website####
fw_player_predictions <- player_predictions %>% 
  mutate(std_pred = pas_yds_pred*0.04 + pas_tds_pred*4 + sc_yds_pred*0.1 + sc_tds_pred*6 + rus_yds_pred*0.1 + rus_tds_pred*6 + rec_yds_pred*0.1 + rec_tds_pred*6 + 0*rec_pred - 1*int_pred - 2*fl_pred,
         half_pred = pas_yds_pred*0.04 + pas_tds_pred*4 + sc_yds_pred*0.1 + sc_tds_pred*6 + rus_yds_pred*0.1 + rus_tds_pred*6 + rec_yds_pred*0.1 + rec_tds_pred*6 + 0.5*rec_pred - 1*int_pred - 2*fl_pred,
         ppr_pred = pas_yds_pred*0.04 + pas_tds_pred*4 + sc_yds_pred*0.1 + sc_tds_pred*6 + rus_yds_pred*0.1 + rus_tds_pred*6 + rec_yds_pred*0.1 + rec_tds_pred*6 + 1*rec_pred - 1*int_pred - 2*fl_pred) %>% 
  select(!fpts_pred) %>% 
  arrange(desc(half_pred))

#write_json(fw_player_predictions, eval(paste("~/FantasyWebsite/public/data/backfill/model_1.0/", This_Year, "/Week_", upcoming_week, "_Player_Predictions.json", sep = "")), pretty = TRUE)


