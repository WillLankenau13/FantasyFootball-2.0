


#Week
# past_week <- 18
# upcoming_week <- 19

#Year
This_Year <- This_Year_d

#read files
#predictions
past_week_player_predictions <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyPredictions/", This_Year, "/Week_", past_week, "_Player_Predictions.csv", sep = "")))
past_week_team_predictions <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyPredictions/", This_Year, "/Week_", past_week, "_Team_Predictions.csv", sep = "")))

#ratings
past_week_combined_player_percents_rat <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyRatings/", This_Year, "/Week_", past_week, "/Player_Percents.csv", sep = "")))
past_week_off_team_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyRatings/", This_Year, "/Week_", past_week, "/Off_Team_Ratings.csv", sep = "")))
past_week_def_team_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyRatings/", This_Year, "/Week_", past_week, "/Def_Team_Ratings.csv", sep = "")))
past_week_QB_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyRatings/", This_Year, "/Week_", past_week, "/QB_Ratings.csv", sep = "")))

#prediction of player percents
past_week_adjusted_combined_player_percents_rat <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyAdjusted/", This_Year, "/Week_", past_week, "/Player_Percents_Adjusted.csv", sep = "")))

#player stats
past_week_player_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyStats/", This_Year, "/byWeek/Week_", past_week, "_Stats.csv", sep = "")))


####weekly player percents####

#Team Stats
#There is no serious problem with summing of player stats to get team snaps. It does not include special teams (good), while team data does. However, sometimes players playing offensive snaps whose position is not an offensive position (lb/rb hybrid) are not counted.
past_week_team_stats <- past_week_player_stats %>% 
  group_by(team) %>% 
  summarize(team_pas_att = sum(pas_att),
            team_cmp = sum(cmp),
            team_pas_yds = sum(pas_yds),
            team_pas_tds = sum(pas_tds),
            team_int = sum(int),
            team_rus_att = sum(rus_att),
            team_rus_yds = sum(rus_yds),
            team_rus_tds = sum(rus_tds),
            team_tgt = sum(tgt),
            team_rec = sum(rec),
            team_rec_yds = sum(rec_yds),
            team_rec_tds = sum(rec_tds))

#combine with player stats
past_week_player_percents <- past_week_player_stats %>% 
  left_join(past_week_team_stats, by = c("team"))

#get player percents
player_percents_func <- function(df, col){
  #rcombine
  df[, paste(col, "_per", sep = "")] <- ((df[, paste(col, sep = "")])/(df[, paste("team_", col, sep = "")]))
  df[, paste(col, "_per", sep = "")][is.na(df[, paste(col, "_per", sep = "")])] <- 0
  return(df)
}

past_week_player_percents <- player_percents_func(past_week_player_percents, "rus_att")
past_week_player_percents <- player_percents_func(past_week_player_percents, "rus_yds")
past_week_player_percents <- player_percents_func(past_week_player_percents, "rus_tds")
past_week_player_percents <- player_percents_func(past_week_player_percents, "tgt")
past_week_player_percents <- player_percents_func(past_week_player_percents, "rec")
past_week_player_percents <- player_percents_func(past_week_player_percents, "rec_yds")
past_week_player_percents <- player_percents_func(past_week_player_percents, "rec_tds")

#select cols
past_week_player_percents <- past_week_player_percents %>% 
  select(player:opp, rus_att_per:rec_tds_per, snap_per, st_snaps)

t_past_week_adjusted_combined_player_percents_rat <- past_week_adjusted_combined_player_percents_rat

player_percents <- past_week_player_percents %>% 
  full_join(t_past_week_adjusted_combined_player_percents_rat, by = c("player", "pos")) %>% 
  mutate(team = ifelse(is.na(team.x), team.y, team.x)) %>% 
  select(player, pos, week, team, opp, rus_att_per:st_snaps, py_games_played, games_played, adj_rus_att_per:adj_rec_tds_per, injury_status)


#filter out players who may have been injured
player_percents <- player_percents %>% 
  filter(!(is.na(snap_per) & injury_status %in% c("Q", "D")))

#NAs to 0
player_percents[, 6:23][is.na(player_percents[, 6:23])] <- 0

#for players who actually played
player_percents <- player_percents %>% 
  mutate(played = ifelse(snap_per > 0, 1, 0))

#increase games played (only for players with offensive snaps)
player_percents <- player_percents %>%
  mutate(games_played = games_played + played)

#volatility
#tested
rus_vol_a <- 0.05
rus_vol_b <- 0.5
rus_vol_c <- 0.5
rus_vol_d <- 17

rec_vol_a <- 0.05
rec_vol_b <- 0.3
rec_vol_c <- 1
rec_vol_d <- 50

player_percents <- player_percents %>%
mutate(rus_vol = rus_vol_a + (rus_vol_b/(games_played + rus_vol_c + (1-played)))*(1 + (17 - py_games_played)/rus_vol_d),
       rec_vol = rec_vol_a + (rec_vol_b/(games_played + rec_vol_c + (1-played)))*(1 + (17 - py_games_played)/rec_vol_d))

# player_percents <- player_percents %>%
#   mutate(rus_vol = 0.05 + (0.5/(games_played + 0.5))*(1 + (17 - py_games_played)/17),
#          rec_vol = 0.05 + (0.3/(games_played + 1))*(1 + (17 - py_games_played)/50))


#get changes
player_percents_changes <- player_percents %>% 
  mutate(d_rus_att_per = (rus_att_per - adj_rus_att_per)*rus_vol,
         d_rus_yds_per = (rus_yds_per - adj_rus_yds_per)*rus_vol,
         d_rus_tds_per = (rus_tds_per - adj_rus_tds_per)*rus_vol,
         d_tgt_per = (tgt_per - adj_tgt_per)*rec_vol,
         d_rec_per = (rec_per - adj_rec_per)*rec_vol,
         d_rec_yds_per = (rec_yds_per - adj_rec_yds_per)*rec_vol,
         d_rec_tds_per = (rec_tds_per - adj_rec_tds_per)*rec_vol) %>% 
  select(player, pos, team, games_played, py_games_played, played, d_rus_att_per:d_rec_tds_per)

#update player percents
t_past_week_combined_player_percents_rat <- past_week_combined_player_percents_rat %>% 
  select(!team:games_played) 
updated_player_percents <- left_join(player_percents_changes, t_past_week_combined_player_percents_rat, by = c("player", "pos"))


updated_player_percents[updated_player_percents$player %in% duplicated(updated_player_percents$player), ]
updated_player_percents$player[duplicated(updated_player_percents$player)]

dups <- updated_player_percents$player[duplicated(updated_player_percents$player)]

if (length(dups) > 0) {
  stop("Duplicated players found Year ", paste(This_Year),  "Week ", paste(upcoming_week), ": ", paste(unique(dups), collapse = ", "))
}


#NAs rep
updated_player_percents[, 4:20][is.na(updated_player_percents)[, 4:20]] <- 0

updated_player_percents <- updated_player_percents %>% 
  mutate(upd_rus_att_per = rus_att_per + d_rus_att_per,
         upd_rus_yds_per = rus_yds_per + d_rus_yds_per,
         upd_rus_tds_per = rus_tds_per + d_rus_tds_per,
         upd_tgt_per = tgt_per + d_tgt_per,
         upd_rec_per = rec_per + d_rec_per,
         upd_rec_yds_per = rec_yds_per + d_rec_yds_per,
         upd_rec_tds_per = rec_tds_per + d_rec_tds_per) %>% 
  mutate(upd_rus_att_per = ifelse(played == 0 & upd_rus_att_per < 0.05 & rus_att_per >= 0.05, 0.05, upd_rus_att_per),
         upd_rus_yds_per = ifelse(played == 0 & upd_rus_yds_per < 0.05 & rus_yds_per >= 0.05, 0.05, upd_rus_yds_per),
         upd_rus_tds_per = ifelse(played == 0 & upd_rus_tds_per < 0.05 & rus_tds_per >= 0.05, 0.05, upd_rus_tds_per),
         upd_tgt_per = ifelse(played == 0 & upd_tgt_per < 0.05 & tgt_per >= 0.05, 0.05, upd_tgt_per),
         upd_rec_per = ifelse(played == 0 & upd_rec_per < 0.03 & rec_per >= 0.03, 0.03, upd_rec_per),
         upd_rec_yds_per = ifelse(played == 0 & upd_rec_yds_per < 0.03 & rec_yds_per >= 0.03, 0.03, upd_rec_yds_per),
         upd_rec_tds_per = ifelse(played == 0 & upd_rec_tds_per < 0.03 & rec_tds_per >= 0.03, 0.03, upd_rec_tds_per))

#fix high variance percents
#tested
upd_val_1_1 <- 0.9
upd_val_2_1 <- 0.5
upd_val_2_2 <- 0.2
upd_val_3 <- 0.7
upd_val_4_1 <- 0.8
upd_val_4_2 <- 0
upd_val_5_1 <- 0.4
upd_val_5_2 <- 0.2
upd_val_5_3 <- 0

updated_player_percents <- updated_player_percents %>%
  mutate(upd_rus_yds_per = upd_val_1_1*upd_rus_yds_per + (1-upd_val_1_1)*upd_rus_att_per,
         upd_rus_tds_per = upd_val_2_1*upd_rus_tds_per + upd_val_2_2*upd_rus_yds_per + (1-upd_val_2_1-upd_val_2_2)*upd_rus_att_per,
         upd_rec_per = upd_val_3*upd_rec_per + (1-upd_val_3)*upd_tgt_per,
         upd_rec_yds_per = upd_val_4_1*upd_rec_yds_per + upd_val_4_2*upd_rec_per + (1-upd_val_4_1-upd_val_4_2)*upd_tgt_per,
         upd_rec_tds_per = upd_val_5_1*upd_rec_tds_per + upd_val_5_2*upd_rec_yds_per + upd_val_5_3*upd_rec_per + (1 - upd_val_5_1 - upd_val_5_2 - upd_val_5_3)*upd_tgt_per)

#Normalize player percents
player_percents_by_team <- updated_player_percents %>%
  group_by(team) %>%
  summarise(old_tot_rus_att_per = sum(rus_att_per),
            old_tot_rus_yds_per = sum(rus_yds_per),
            old_tot_rus_tds_per = sum(rus_tds_per),
            old_tot_tgt_per = sum(tgt_per),
            old_tot_rec_per = sum(rec_per),
            old_tot_rec_yds_per = sum(rec_yds_per),
            old_tot_rec_tds_per = sum(rec_tds_per),
            new_tot_rus_att_per = sum(upd_rus_att_per),
            new_tot_rus_yds_per = sum(upd_rus_yds_per),
            new_tot_rus_tds_per = sum(upd_rus_tds_per),
            new_tot_tgt_per = sum(upd_tgt_per),
            new_tot_rec_per = sum(upd_rec_per),
            new_tot_rec_yds_per = sum(upd_rec_yds_per),
            new_tot_rec_tds_per = sum(upd_rec_tds_per))

updated_player_percents <- updated_player_percents %>%
  left_join(player_percents_by_team, by = c("team")) %>%
  mutate(upd_rus_att_per = upd_rus_att_per*(old_tot_rus_att_per/new_tot_rus_att_per),
         upd_rus_yds_per = upd_rus_yds_per*(old_tot_rus_yds_per/new_tot_rus_yds_per),
         upd_rus_tds_per = upd_rus_tds_per*(old_tot_rus_tds_per/new_tot_rus_tds_per),
         upd_tgt_per = upd_tgt_per*(old_tot_tgt_per/new_tot_tgt_per),
         upd_rec_per = upd_rec_per*(old_tot_rec_per/new_tot_rec_per),
         upd_rec_yds_per = upd_rec_yds_per*(old_tot_rec_yds_per/new_tot_rec_yds_per),
         upd_rec_tds_per = upd_rec_tds_per*(old_tot_rec_tds_per/new_tot_rec_tds_per))

#Regress to sum to 1
# reg_coef <- 2
# 
# updated_player_percents_by_team <- updated_player_percents %>%
#   group_by(team) %>%
#   summarise(tot_rus_att_per = sum(upd_rus_att_per),
#             tot_rus_yds_per = sum(upd_rus_yds_per),
#             tot_rus_tds_per = sum(upd_rus_tds_per),
#             tot_tgt_per = sum(upd_tgt_per),
#             tot_rec_per = sum(upd_rec_per),
#             tot_rec_yds_per = sum(upd_rec_yds_per),
#             tot_rec_tds_per = sum(upd_rec_tds_per)) %>%
#   mutate(reg_rus_att_per = (((1 - tot_rus_att_per)/reg_coef) + tot_rus_att_per)/(tot_rus_att_per),
#          reg_rus_yds_per = (((1 - tot_rus_yds_per)/reg_coef) + tot_rus_yds_per)/(tot_rus_yds_per),
#          reg_rus_tds_per = (((1 - tot_rus_tds_per)/reg_coef) + tot_rus_tds_per)/(tot_rus_tds_per),
#          reg_tgt_per = (((1 - tot_tgt_per)/reg_coef) + tot_tgt_per)/(tot_tgt_per),
#          reg_rec_per = (((1 - tot_rec_per)/reg_coef) + tot_rec_per)/(tot_rec_per),
#          reg_rec_yds_per = (((1 - tot_rec_yds_per)/reg_coef) + tot_rec_yds_per)/(tot_rec_yds_per),
#          reg_rec_tds_per = (((1 - tot_rec_tds_per)/reg_coef) + tot_rec_tds_per)/(tot_rec_tds_per))
# 
# updated_player_percents <- updated_player_percents %>%
#   left_join(updated_player_percents_by_team, by = c("team")) %>%
#   mutate(upd_rus_att_per = upd_rus_att_per*reg_rus_att_per,
#          upd_rus_yds_per = upd_rus_yds_per*reg_rus_yds_per,
#          upd_rus_tds_per = upd_rus_tds_per*reg_rus_tds_per,
#          upd_tgt_per = upd_tgt_per*reg_tgt_per,
#          upd_rec_per = upd_rec_per*reg_rec_per,
#          upd_rec_yds_per = upd_rec_yds_per*reg_rec_yds_per,
#          upd_rec_tds_per = upd_rec_tds_per*reg_rec_tds_per)

#select
updated_player_percents <- updated_player_percents %>% 
  select(player, pos, team, py_games_played, games_played, upd_rus_att_per:upd_rec_tds_per)

#fix negative updated percents
updated_player_percents[updated_player_percents < 0] <- 0

#clean up
colnames(updated_player_percents) <- colnames(past_week_combined_player_percents_rat)

#combine with full player percents rating
not_active_player_percents <- past_week_combined_player_percents_rat %>% 
  filter(!(player %in% updated_player_percents$player))

full_updated_player_percents <- rbind(not_active_player_percents, updated_player_percents)

#fix negative ratings
full_updated_player_percents[full_updated_player_percents < 0] <- 0

####QB Ratings####
#join
QB_ratings_dif <- full_join(past_week_team_predictions, past_week_player_stats, by = c("team"))

#filter QBs
QB_ratings_dif <- QB_ratings_dif %>% 
  filter(pos == "QB")

#select cols
QB_ratings_dif <- QB_ratings_dif %>% 
  select(player, team, opp, team_pas_att_pred:team_sc_tds_pred, pas_att:sc_tds, snap_per)

#if QB played less than 20% of snaps, set to 0, snap multiplier
QB_ratings_dif <- QB_ratings_dif %>% 
  mutate(snap_per = ifelse(snap_per < 0.2, 0, snap_per),
         snap_mul = ifelse(snap_per == 0, 0, 1/snap_per))

#get difference from predicted and actual
QB_ratings_dif <- QB_ratings_dif %>% 
  mutate(d_pas_att = pas_att*snap_mul - team_pas_att_pred,
         d_cmp = cmp*snap_mul - team_cmp_pred,
         d_pas_yds = pas_yds*snap_mul - team_pas_yds_pred,
         d_pas_tds = pas_tds*snap_mul - team_pas_tds_pred,
         d_int = int*snap_mul - team_int_pred,
         d_sc_att = sc_att*snap_mul - team_sc_att_pred,
         d_sc_yds = sc_yds*snap_mul - team_sc_yds_pred,
         d_sc_tds = sc_tds*snap_mul - team_sc_tds_pred)

#update QB ratings
updated_QB_ratings <- full_join(past_week_QB_ratings, QB_ratings_dif, by = "player")

#update team and select
updated_QB_ratings <- updated_QB_ratings %>% 
  mutate(team = team.y) %>% 
  select(player, team, py_games_played, games_played, pas_att_rat:sc_tds_rat, d_pas_att:d_sc_tds, snap_per)

#new players
pas_att_rep = 20
cmp_rep = 9
pas_yds_rep = 150
pas_tds_rep = 0.6
int_rep = 1
sc_att_rep <- 1.4 #not tested, based on scramble testing optimal percent of average attempts and league average
sc_yds_rep <- 12 #not tested
sc_tds_rep <- 0.05 #not tested

#Replace with replacement level if necessary
updated_QB_ratings <- updated_QB_ratings %>% 
  mutate(pas_att_rat = ifelse(is.na(pas_att_rat), pas_att_rep, pas_att_rat),
         cmp_rat = ifelse(is.na(cmp_rat), cmp_rep, cmp_rat),
         pas_yds_rat = ifelse(is.na(pas_yds_rat), pas_yds_rep, pas_yds_rat),
         pas_tds_rat = ifelse(is.na(pas_tds_rat), pas_tds_rep, pas_tds_rat),
         int_rat = ifelse(is.na(int_rat), int_rep, int_rat),
         sc_att_rat = ifelse(is.na(sc_att_rat), sc_att_rep, sc_att_rat),
         sc_yds_rat = ifelse(is.na(sc_yds_rat), sc_yds_rep, sc_yds_rat),
         sc_tds_rat = ifelse(is.na(sc_tds_rat), sc_tds_rep, sc_tds_rat))

#zeroes
updated_QB_ratings$py_games_played[is.na(updated_QB_ratings$py_games_played)] <- 0
updated_QB_ratings$games_played[is.na(updated_QB_ratings$games_played)] <- 0

##dnp
dnp_QB_ratings <- updated_QB_ratings %>%
  filter(is.na(snap_per) | snap_per == 0) 

#players who played
updated_QB_ratings <- updated_QB_ratings %>%
  filter(!is.na(snap_per) & snap_per > 0)

#increment games played
updated_QB_ratings <- updated_QB_ratings %>%
  mutate(games_played = games_played + 1)

#volatility
#tested
#low
qb_vol_a <- 0.1
qb_vol_b <- 0.07
qb_vol_c <- 34

updated_QB_ratings <- updated_QB_ratings %>%
  mutate(vol = (qb_vol_a + (qb_vol_b/games_played)*(1 + (17 - py_games_played)/qb_vol_c))*snap_per/1)

#high
# updated_QB_ratings <- updated_QB_ratings %>%
#   mutate(vol = (0.3 + (0.1/games_played)*(1 + (17 - py_games_played)/34))*snap_per/1)

#old
# updated_QB_ratings <- updated_QB_ratings %>%
#   mutate(vol = (0.2 + (0.1/games_played)*(1 + (17 - py_games_played)/34))*snap_per/1)

#update ratings
updated_QB_ratings <- updated_QB_ratings %>% 
  mutate(upd_pas_att_rat = pas_att_rat + d_pas_att*vol,
         upd_cmp_rat = cmp_rat + d_cmp*vol,
         upd_pas_yds_rat = pas_yds_rat + d_pas_yds*vol,
         upd_pas_tds_rat = pas_tds_rat + d_pas_tds*(vol/2),
         upd_int_rat = int_rat + d_int*(vol/2),
         upd_sc_att_rat = sc_att_rat + d_sc_att*(vol/2),
         upd_sc_yds_rat = sc_yds_rat + d_sc_yds*(vol/2),
         upd_sc_tds_rat = sc_tds_rat + d_sc_tds*(vol/2)) %>% 
  select(player, team, py_games_played, games_played, upd_pas_att_rat:upd_sc_tds_rat)

#recombine with dnp
dnp_QB_ratings <- dnp_QB_ratings %>% 
  mutate(upd_pas_att_rat = pas_att_rat,
         upd_cmp_rat = cmp_rat,
         upd_pas_yds_rat = pas_yds_rat,
         upd_pas_tds_rat = pas_tds_rat,
         upd_int_rat = int_rat,
         upd_sc_att_rat = sc_att_rat,
         upd_sc_yds_rat = sc_yds_rat,
         upd_sc_tds_rat = sc_tds_rat) %>% 
  select(player, team, py_games_played, games_played, upd_pas_att_rat:upd_sc_tds_rat) 

updated_QB_ratings <- rbind(updated_QB_ratings, dnp_QB_ratings)

#clean up
colnames(updated_QB_ratings) <- colnames(past_week_QB_ratings)

#fix negative ratings
updated_QB_ratings[updated_QB_ratings < 0] <- 0

####Off Team Ratings####
#get team stats
past_week_team_stats <- past_week_player_stats %>% 
  group_by(team) %>% 
  summarise(across(pas_att:rec_tds, sum),
                           .groups = 'drop') %>% 
  select(team, pas_att:pas_tds, int, rus_att:rus_tds)

#combine predicted and actual
off_team_dif <- full_join(past_week_team_stats, past_week_team_predictions, by = c("team")) %>% 
  filter(!is.na(pas_att))

#get difference
off_team_dif <- off_team_dif %>% 
  transmute(team = team,
            d_pas_att = pas_att - team_pas_att_pred,
            d_cmp = cmp - team_cmp_pred,
            d_pas_yds = pas_yds - team_pas_yds_pred,
            d_pas_tds = pas_tds - team_pas_tds_pred,
            d_int = int - team_int_pred,
            d_rus_att = rus_att - team_rus_att_pred,
            d_rus_yds = rus_yds - team_rus_yds_pred,
            d_rus_tds = rus_tds - team_rus_tds_pred)

#combine with ratings
updated_off_team_ratings <- left_join(off_team_dif, past_week_off_team_ratings, by = c("team"))

#volatility
#tested
#new
off_vol_a <- 0.8
off_vol_b <- 0.2

updated_off_team_ratings <- updated_off_team_ratings %>% 
  mutate(vol = off_vol_a + (off_vol_b/past_week))

#old
# updated_off_team_ratings <- updated_off_team_ratings %>% 
#   mutate(vol = 0.05 + (0.3/past_week))

#update ratings
updated_off_team_ratings <- updated_off_team_ratings %>% 
  mutate(upd_pas_att_rat = off_pas_att_rat + d_pas_att*vol,
         upd_cmp_rat = off_cmp_rat + d_cmp*vol,
         upd_pas_yds_rat = off_pas_yds_rat + d_pas_yds*vol,
         upd_pas_tds_rat = off_pas_tds_rat + d_pas_tds*(vol/2),
         upd_int_rat = off_int_rat + d_int*(vol/2),
         upd_rus_att_rat = off_rus_att_rat + d_rus_att*vol,
         upd_rus_yds_rat = off_rus_yds_rat + d_rus_yds*vol,
         upd_rus_tds_rat = off_rus_tds_rat + d_rus_tds*(vol/2))

#update ratings for regressing to 1 in player percents
#coefs from make predictions
rus_att <- 0
rus_yds <- 0
rus_tds <- 0
pas_att <- 0.1
cmp <- 0.4
pas_yds <- 0.1
pas_tds <- 0

# team_adjustment <- updated_player_percents_by_team %>%
#   mutate(rus_att_adj = ((tot_rus_att_per - 1)*rus_att + 1)/(((tot_rus_att_per - 1)/reg_coef)*rus_att + 1),
#          rus_yds_adj = ((tot_rus_yds_per - 1)*rus_yds + 1)/(((tot_rus_yds_per - 1)/reg_coef)*rus_yds + 1),
#          rus_tds_adj = ((tot_rus_tds_per - 1)*rus_tds + 1)/(((tot_rus_tds_per - 1)/reg_coef)*rus_tds + 1),
#          pas_att_adj = ((tot_tgt_per - 1)*pas_att + 1)/(((tot_tgt_per - 1)/reg_coef)*pas_att + 1),
#          pas_cmp_adj = ((tot_rec_per - 1)*cmp + 1)/(((tot_rec_per - 1)/reg_coef)*cmp + 1),
#          pas_yds_adj = ((tot_rec_yds_per - 1)*pas_yds + 1)/(((tot_rec_yds_per - 1)/reg_coef)*pas_yds + 1),
#          pas_tds_adj = ((tot_rec_tds_per - 1)*pas_tds + 1)/(((tot_rec_tds_per - 1)/reg_coef)*pas_tds + 1))
# 
# updated_off_team_ratings <- updated_off_team_ratings %>%
#   left_join(team_adjustment, by = c("team")) %>%
#   mutate(upd_rus_att_rat = upd_rus_att_rat*rus_att_adj,
#          upd_rus_yds_rat = upd_rus_yds_rat*rus_yds_adj,
#          upd_rus_tds_rat = upd_rus_tds_rat*rus_tds_adj,
#          upd_pas_att_rat = upd_pas_att_rat*pas_att_adj,
#          upd_cmp_rat = upd_cmp_rat*pas_cmp_adj,
#          upd_pas_yds_rat = upd_pas_yds_rat*pas_yds_adj,
#          upd_pas_tds_rat = upd_pas_tds_rat*pas_tds_adj)

#clean up
updated_off_team_ratings <- updated_off_team_ratings %>% 
  select(team, upd_pas_att_rat:upd_rus_tds_rat)

colnames(updated_off_team_ratings) <- colnames(past_week_off_team_ratings)

#teams on bye
not_active_off_team_ratings <- past_week_off_team_ratings %>% 
  filter(!(team %in% updated_off_team_ratings$team))

full_updated_off_team_ratings <- rbind(not_active_off_team_ratings, updated_off_team_ratings)


####Def Team Ratings####
#get team stats
past_week_team_stats <- past_week_player_stats %>% 
  group_by(team, opp) %>% 
  summarise(across(pas_att:rec_tds, sum),
            .groups = 'drop') %>% 
  select(team, opp, pas_att:pas_tds, int, rus_att:rus_tds)

#combine predicted and actual
def_team_dif <- full_join(past_week_team_stats, past_week_team_predictions, by = c("team")) %>% 
  select(!team) %>% 
  rename("team" = "opp") %>% 
  filter(!is.na(team))

#get difference
def_team_dif <- def_team_dif %>% 
  transmute(team = team,
            d_pas_att = pas_att - team_pas_att_pred,
            d_cmp = cmp - team_cmp_pred,
            d_pas_yds = pas_yds - team_pas_yds_pred,
            d_pas_tds = pas_tds - team_pas_tds_pred,
            d_int = int - team_int_pred,
            d_rus_att = rus_att - team_rus_att_pred,
            d_rus_yds = rus_yds - team_rus_yds_pred,
            d_rus_tds = rus_tds - team_rus_tds_pred)

#combine with ratings
updated_def_team_ratings <- left_join(def_team_dif, past_week_def_team_ratings, by = c("team"))

#volatility
#tested
def_vol_a <- 0.3
def_vol_b <- 0.2

updated_def_team_ratings <- updated_def_team_ratings %>% 
  mutate(vol = def_vol_a + (def_vol_b/past_week))

#update ratings
updated_def_team_ratings <- updated_def_team_ratings %>% 
  mutate(upd_pas_att_rat = def_pas_att_rat + d_pas_att*vol,
         upd_cmp_rat = def_cmp_rat + d_cmp*vol,
         upd_pas_yds_rat = def_pas_yds_rat + d_pas_yds*vol,
         upd_pas_tds_rat = def_pas_tds_rat + d_pas_tds*(vol/2),
         upd_int_rat = def_int_rat + d_int*(vol/2),
         upd_rus_att_rat = def_rus_att_rat + d_rus_att*vol,
         upd_rus_yds_rat = def_rus_yds_rat + d_rus_yds*vol,
         upd_rus_tds_rat = def_rus_tds_rat + d_rus_tds*(vol/2))

#clean up
updated_def_team_ratings <- updated_def_team_ratings %>% 
  select(team, upd_pas_att_rat:upd_rus_tds_rat)

colnames(updated_def_team_ratings) <- colnames(past_week_def_team_ratings)

#teams on bye
not_active_def_team_ratings <- past_week_def_team_ratings %>% 
  filter(!(team %in% updated_def_team_ratings$team))

full_updated_def_team_ratings <- rbind(not_active_def_team_ratings, updated_def_team_ratings)


####Write Csv####
write_csv(full_updated_player_percents, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyRatings/", This_Year, "/Week_", upcoming_week, "/Player_Percents.csv", sep = "")))
write_csv(updated_QB_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyRatings/", This_Year, "/Week_", upcoming_week, "/QB_Ratings.csv", sep = "")))
write_csv(full_updated_off_team_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyRatings/", This_Year, "/Week_", upcoming_week, "/Off_Team_Ratings.csv", sep = "")))
write_csv(full_updated_def_team_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyRatings/", This_Year, "/Week_", upcoming_week, "/Def_Team_Ratings.csv", sep = "")))

