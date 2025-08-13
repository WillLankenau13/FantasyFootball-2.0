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
# past_week <- 11
# upcoming_week <- 12

#Year
This_Year <- Years_Dataframe$This_Year[1]

#read files
#predictions
past_week_player_predictions <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyPredictions/", This_Year, "/Week_", past_week, "_Player_Predictions.csv", sep = "")))
past_week_team_predictions <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyPredictions/", This_Year, "/Week_", past_week, "_Team_Predictions.csv", sep = "")))

#ratings
past_week_combined_player_percents_rat <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", past_week, "/Player_Percents.csv", sep = "")))
past_week_off_team_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", past_week, "/Off_Team_Ratings.csv", sep = "")))
past_week_def_team_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", past_week, "/Def_Team_Ratings.csv", sep = "")))
past_week_QB_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", past_week, "/QB_Ratings.csv", sep = "")))

#prediction of player percents
past_week_adjusted_combined_player_percents_rat <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyAdjusted/", This_Year, "/Week_", past_week, "/Player_Percents_Adjusted.csv", sep = "")))

#player stats
d_past_week_player_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/", This_Year, "/byWeek/Week_", past_week, "_Stats.csv", sep = ""))) %>% 
  clean_names()
d_past_week_st_snaps <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/", This_Year, "/byWeek/Week_", past_week, "_ST_Snaps.csv", sep = ""))) %>% 
  clean_names()

past_week_player_stats <- d_past_week_player_stats %>% 
  select(player, pos, g_number, week, team, opp, att_19, cmp_18, yds_22, td_23, int, att_37, yds_38, td_40, tgt_43, rec, yds_45, td_47, fmb_62, x2pm, off_percent_68)
past_week_st_snaps <- d_past_week_st_snaps %>% 
  select(player, pos, g_number, week, team, opp, st_snp_17)

colnames(past_week_player_stats) <- c("player", "pos", "game_number", "week", "team", "opp", "pas_att", "cmp", "pas_yds", "pas_tds", "int", "rus_att", "rus_yds", "rus_tds", "tgt", "rec", "rec_yds", "rec_tds", "fmb_game", "two_point", "snap_per")
colnames(past_week_st_snaps) <- c("player", "pos", "game_number", "week", "team", "opp", "st_snaps")

#player names func
past_week_player_stats <- player_names_func(past_week_player_stats)
past_week_st_snaps <- player_names_func(past_week_st_snaps)

#add special teams
past_week_player_stats <- past_week_player_stats %>% 
  full_join(past_week_st_snaps, by = c("player", "pos", "game_number", "week", "team", "opp"))

#NA vals to 0
past_week_player_stats[is.na(past_week_player_stats)] <- 0

####Hamlin Game####
if(This_Year == 2022 & past_week == 17){
  past_week_player_stats <- past_week_player_stats %>% 
    filter(team != "BUF") %>% 
    filter(team != "CIN")
}

####weekly player percents####
#get touches
past_week_player_stats <- past_week_player_stats %>% 
  mutate(touches_game = rus_att + rec)

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
  select(player:opp, rus_att_per:rec_tds_per, touches_game, fmb_game:snap_per)

t_past_week_adjusted_combined_player_percents_rat <- past_week_adjusted_combined_player_percents_rat

player_percents <- past_week_player_percents %>% 
  full_join(t_past_week_adjusted_combined_player_percents_rat, by = c("player")) %>% 
  mutate(team = ifelse(is.na(team.x), team.y, team.x),
       pos = ifelse(is.na(pos.x), pos.y, pos.x)) %>% 
  select(player, pos, game_number, week, team, opp, rus_att_per:snap_per, games_played:py_fl_per_tou)

player_percents <- player_percents %>% 
  filter(!is.na(snap_per))


#NAs to 0
player_percents[, 7:30][is.na(player_percents[, 7:30])] <- 0


#increase games played, touches, and fumbles (only for players with offensive snaps)
player_percents <- player_percents %>%
  mutate(games_played = games_played + 1,
         touches = touches + touches_game,
         fmb = fmb + fmb_game)

#volatility
#tested
#low
# player_percents <- player_percents %>%
#   mutate(vol = 0.03 + (0.3/games_played)*(1 + (17 - py_games_played)/34))

#high
# player_percents <- player_percents %>%
#   mutate(vol = 0.1 + (0.5/games_played)*(1 + (17 - py_games_played)/34))

#old
player_percents <- player_percents %>%
  mutate(vol = 0.05 + (0.5/games_played)*(1 + (17 - py_games_played)/34))

#get changes
player_percents_changes <- player_percents %>% 
  mutate(d_rus_att_per = (rus_att_per - adj_rus_att_per)*vol,
         d_rus_yds_per = (rus_yds_per - adj_rus_yds_per)*vol,
         d_rus_tds_per = (rus_tds_per - adj_rus_tds_per)*vol,
         d_tgt_per = (tgt_per - adj_tgt_per)*vol,
         d_rec_per = (rec_per - adj_rec_per)*vol,
         d_rec_yds_per = (rec_yds_per - adj_rec_yds_per)*vol,
         d_rec_tds_per = (rec_tds_per - adj_rec_tds_per)*vol,
         this_year_touches = touches) %>% 
  select(player, pos, team, games_played, py_games_played, touches, fmb, d_rus_att_per:d_rec_tds_per)

#update player percents
t_past_week_combined_player_percents_rat <- past_week_combined_player_percents_rat %>% 
  select(!pos:py_games_played) %>% 
  select(!touches:fmb)
updated_player_percents <- left_join(player_percents_changes, t_past_week_combined_player_percents_rat, by = c("player"))


#NAs and fumbles rep
rep_py_fl_per_tou <- 0.007
rep_py_qb_fl <- 0.21

updated_player_percents$py_fl_per_tou[is.na(updated_player_percents$py_fl_per_tou)] <- rep_py_fl_per_tou
updated_player_percents$py_qb_fl[is.na(updated_player_percents$py_qb_fl)] <- rep_py_qb_fl
updated_player_percents[, 4:21][is.na(updated_player_percents)[, 4:21]] <- 0

updated_player_percents <- updated_player_percents %>% 
  mutate(upd_rus_att_per = adj_rus_att_per + d_rus_att_per,
         upd_rus_yds_per = adj_rus_yds_per + d_rus_yds_per,
         upd_rus_tds_per = adj_rus_tds_per + d_rus_tds_per,
         upd_tgt_per = adj_tgt_per + d_tgt_per,
         upd_rec_per = adj_rec_per + d_rec_per,
         upd_rec_yds_per = adj_rec_yds_per + d_rec_yds_per,
         upd_rec_tds_per = adj_rec_tds_per + d_rec_tds_per) 

#fix high variance percents
#tested
updated_player_percents <- updated_player_percents %>%
  mutate(upd_rus_yds_per = 0.9*upd_rus_yds_per + 0.1*upd_rus_att_per,
         upd_rus_tds_per = 0.5*upd_rus_tds_per + 0.2*upd_rus_yds_per + 0.3*upd_rus_att_per,
         upd_rec_per = 0.7*upd_rec_per + 0.3*upd_tgt_per,
         upd_rec_yds_per = 0.8*upd_rec_yds_per + upd_rec_per*0 + upd_tgt_per*0.2,
         upd_rec_tds_per = 0.4*upd_rec_tds_per + 0.2*upd_rec_yds_per + 0*upd_rec_per + 0.4*upd_tgt_per)

#Normalize player percents
player_percents_by_team <- updated_player_percents %>%
  group_by(team) %>%
  summarise(old_tot_rus_att_per = sum(adj_rus_att_per),
            old_tot_rus_yds_per = sum(adj_rus_yds_per),
            old_tot_rus_tds_per = sum(adj_rus_tds_per),
            old_tot_tgt_per = sum(adj_tgt_per),
            old_tot_rec_per = sum(adj_rec_per),
            old_tot_rec_yds_per = sum(adj_rec_yds_per),
            old_tot_rec_tds_per = sum(adj_rec_tds_per),
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
reg_coef <- 2

updated_player_percents_by_team <- updated_player_percents %>%
  group_by(team) %>%
  summarise(tot_rus_att_per = sum(upd_rus_att_per),
            tot_rus_yds_per = sum(upd_rus_yds_per),
            tot_rus_tds_per = sum(upd_rus_tds_per),
            tot_tgt_per = sum(upd_tgt_per),
            tot_rec_per = sum(upd_rec_per),
            tot_rec_yds_per = sum(upd_rec_yds_per),
            tot_rec_tds_per = sum(upd_rec_tds_per)) %>%
  mutate(reg_rus_att_per = (((1 - tot_rus_att_per)/reg_coef) + tot_rus_att_per)/(tot_rus_att_per),
         reg_rus_yds_per = (((1 - tot_rus_yds_per)/reg_coef) + tot_rus_yds_per)/(tot_rus_yds_per),
         reg_rus_tds_per = (((1 - tot_rus_tds_per)/reg_coef) + tot_rus_tds_per)/(tot_rus_tds_per),
         reg_tgt_per = (((1 - tot_tgt_per)/reg_coef) + tot_tgt_per)/(tot_tgt_per),
         reg_rec_per = (((1 - tot_rec_per)/reg_coef) + tot_rec_per)/(tot_rec_per),
         reg_rec_yds_per = (((1 - tot_rec_yds_per)/reg_coef) + tot_rec_yds_per)/(tot_rec_yds_per),
         reg_rec_tds_per = (((1 - tot_rec_tds_per)/reg_coef) + tot_rec_tds_per)/(tot_rec_tds_per))

updated_player_percents <- updated_player_percents %>%
  left_join(updated_player_percents_by_team, by = c("team")) %>%
  mutate(upd_rus_att_per = upd_rus_att_per*reg_rus_att_per,
         upd_rus_yds_per = upd_rus_yds_per*reg_rus_yds_per,
         upd_rus_tds_per = upd_rus_tds_per*reg_rus_tds_per,
         upd_tgt_per = upd_tgt_per*reg_tgt_per,
         upd_rec_per = upd_rec_per*reg_rec_per,
         upd_rec_yds_per = upd_rec_yds_per*reg_rec_yds_per,
         upd_rec_tds_per = upd_rec_tds_per*reg_rec_tds_per)

#select
updated_player_percents <- updated_player_percents %>% 
  select(player, pos, team, games_played, py_games_played, touches, fmb, upd_rus_att_per:upd_rec_tds_per, py_qb_fl, py_fl_per_tou)

#fix negative updated percents
updated_player_percents[updated_player_percents < 0] <- 0

#clean up
colnames(updated_player_percents) <- colnames(past_week_combined_player_percents_rat)

#combine with full player percents rating
not_actice_player_percents <- past_week_combined_player_percents_rat %>% 
  filter(!(player %in% updated_player_percents$player))

full_updated_player_percents <- rbind(not_actice_player_percents, updated_player_percents)


####QB Ratings####
#join
QB_ratings_dif <- full_join(past_week_team_predictions, past_week_player_stats, by = c("team"))

#filter QBs
QB_ratings_dif <- QB_ratings_dif %>% 
  filter(pos == "QB")

#select cols
QB_ratings_dif <- QB_ratings_dif %>% 
  select(player, team, opp, team_pas_att_pred:team_pas_tds_pred, team_int_pred, pas_att:int, snap_per)

#if QB played less than 10% of snaps, set to 0, snap multiplier
QB_ratings_dif <- QB_ratings_dif %>% 
  mutate(snap_per = ifelse(snap_per < 20, 0, snap_per),
         snap_mul = ifelse(snap_per == 0, 0, 100/snap_per))

#get difference from predicted and actual
QB_ratings_dif <- QB_ratings_dif %>% 
  mutate(d_pas_att = pas_att*snap_mul - team_pas_att_pred,
         d_cmp = cmp*snap_mul - team_cmp_pred,
         d_pas_yds = pas_yds*snap_mul - team_pas_yds_pred,
         d_pas_tds = pas_tds*snap_mul - team_pas_tds_pred,
         d_int = int*snap_mul - team_pas_tds_pred)

#update QB ratings
updated_QB_ratings <- full_join(past_week_QB_ratings, QB_ratings_dif, by = "player")

##dnp
dnp_QB_ratings <- updated_QB_ratings %>%
  filter(is.na(snap_per) | snap_per == 0) %>% 
  filter(!is.na(team.x))

#players who played
updated_QB_ratings <- updated_QB_ratings %>%
  filter(!is.na(snap_per) & snap_per > 0)

#update team and select
updated_QB_ratings <- updated_QB_ratings %>% 
  mutate(team = team.y) %>% 
  select(player, team, py_games_played, games_played, pas_att_rat:int_rat, d_pas_att:d_int, snap_per)

#new players
pas_att_rep = 20
cmp_rep = 9
pas_yds_rep = 150
pas_tds_rep = 0.6
int_rep = 1

#Replace with replacement level if necessary
updated_QB_ratings <- updated_QB_ratings %>% 
  mutate(pas_att_rat = ifelse(is.na(pas_att_rat), pas_att_rep, pas_att_rat),
         cmp_rat = ifelse(is.na(cmp_rat), cmp_rep, cmp_rat),
         pas_yds_rat = ifelse(is.na(pas_yds_rat), pas_yds_rep, pas_yds_rat),
         pas_tds_rat = ifelse(is.na(pas_tds_rat), pas_tds_rep, pas_tds_rat),
         int_rat = ifelse(is.na(int_rat), int_rep, int_rat))

#zeroes
updated_QB_ratings[is.na(updated_QB_ratings)] <- 0

#increment games played
updated_QB_ratings <- updated_QB_ratings %>%
  mutate(games_played = games_played + 1)

#volatility
#tested
#low
updated_QB_ratings <- updated_QB_ratings %>%
  mutate(vol = (0.1 + (0.07/games_played)*(1 + (17 - py_games_played)/34))*snap_per/100)

#high
# updated_QB_ratings <- updated_QB_ratings %>%
#   mutate(vol = (0.3 + (0.1/games_played)*(1 + (17 - py_games_played)/34))*snap_per/100)

#old
# updated_QB_ratings <- updated_QB_ratings %>%
#   mutate(vol = (0.2 + (0.1/games_played)*(1 + (17 - py_games_played)/34))*snap_per/100)

#update ratings
updated_QB_ratings <- updated_QB_ratings %>% 
  mutate(upd_pas_att_rat = pas_att_rat + d_pas_att*vol,
         upd_cmp_rat = cmp_rat + d_cmp*vol,
         upd_pas_yds_rat = pas_yds_rat + d_pas_yds*vol,
         upd_pas_tds_rat = pas_tds_rat + d_pas_tds*(vol/2),
         upd_int_rat = int_rat + d_int*(vol/2)) %>% 
  select(player, team, py_games_played, games_played, upd_pas_att_rat:upd_int_rat)

#recombine with dnp
dnp_QB_ratings <- dnp_QB_ratings %>% 
  mutate(upd_pas_att_rat = pas_att_rat,
         upd_cmp_rat = cmp_rat,
         upd_pas_yds_rat = pas_yds_rat,
         upd_pas_tds_rat = pas_tds_rat,
         upd_int_rat = int_rat) %>% 
  mutate(team = team.x) %>% 
  select(player, team, py_games_played, games_played, upd_pas_att_rat:upd_int_rat) 

updated_QB_ratings <- rbind(updated_QB_ratings, dnp_QB_ratings)

#clean up
colnames(updated_QB_ratings) <- colnames(past_week_QB_ratings)

####Off Team Ratings####
#get team stats
past_week_team_stats <- past_week_player_stats %>% 
  group_by(team) %>% 
  summarise(across(pas_att:rec_tds, sum),
                           .groups = 'drop') %>% 
  select(team, pas_att:pas_tds, rus_att:rus_tds, int)

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
            d_rus_att = rus_att - team_rus_att_pred,
            d_rus_yds = rus_yds - team_rus_yds_pred,
            d_rus_tds = rus_tds - team_rus_tds_pred,
            d_int = int - team_int_pred)

#combine with ratings
updated_off_team_ratings <- left_join(off_team_dif, past_week_off_team_ratings, by = c("team"))

#volatility
#tested
#new
updated_off_team_ratings <- updated_off_team_ratings %>% 
  mutate(vol = 0.08 + (0.2/past_week))

#old
# updated_off_team_ratings <- updated_off_team_ratings %>% 
#   mutate(vol = 0.05 + (0.3/past_week))

#update ratings
updated_off_team_ratings <- updated_off_team_ratings %>% 
  mutate(upd_rus_att_rat = off_rus_att_rat + d_rus_att*vol,
         upd_rus_yds_rat = off_rus_yds_rat + d_rus_yds*vol,
         upd_rus_tds_rat = off_rus_tds_rat + d_rus_tds*(vol/2),
         upd_pas_att_rat = off_pas_att_rat + d_pas_att*vol,
         upd_cmp_rat = off_cmp_rat + d_cmp*vol,
         upd_pas_yds_rat = off_pas_yds_rat + d_pas_yds*vol,
         upd_pas_tds_rat = off_pas_tds_rat + d_pas_tds*(vol/2),
         upd_int_rat = off_int_rat + d_int*(vol/2))

#update ratings for regressing to 1 in player percents
#coefs from make predictions
rus_att <- 0
rus_yds <- 0
rus_tds <- 0
pas_att <- 0.1
cmp <- 0.4
pas_yds <- 0.1
pas_tds <- 0

team_adjustment <- updated_player_percents_by_team %>%
  mutate(rus_att_adj = ((tot_rus_att_per - 1)*rus_att + 1)/(((tot_rus_att_per - 1)/reg_coef)*rus_att + 1),
         rus_yds_adj = ((tot_rus_yds_per - 1)*rus_yds + 1)/(((tot_rus_yds_per - 1)/reg_coef)*rus_yds + 1),
         rus_tds_adj = ((tot_rus_tds_per - 1)*rus_tds + 1)/(((tot_rus_tds_per - 1)/reg_coef)*rus_tds + 1),
         pas_att_adj = ((tot_tgt_per - 1)*pas_att + 1)/(((tot_tgt_per - 1)/reg_coef)*pas_att + 1),
         pas_cmp_adj = ((tot_rec_per - 1)*cmp + 1)/(((tot_rec_per - 1)/reg_coef)*cmp + 1),
         pas_yds_adj = ((tot_rec_yds_per - 1)*pas_yds + 1)/(((tot_rec_yds_per - 1)/reg_coef)*pas_yds + 1),
         pas_tds_adj = ((tot_rec_tds_per - 1)*pas_tds + 1)/(((tot_rec_tds_per - 1)/reg_coef)*pas_tds + 1))

updated_off_team_ratings <- updated_off_team_ratings %>%
  left_join(team_adjustment, by = c("team")) %>%
  mutate(upd_rus_att_rat = upd_rus_att_rat*rus_att_adj,
         upd_rus_yds_rat = upd_rus_yds_rat*rus_yds_adj,
         upd_rus_tds_rat = upd_rus_tds_rat*rus_tds_adj,
         upd_pas_att_rat = upd_pas_att_rat*pas_att_adj,
         upd_cmp_rat = upd_cmp_rat*pas_cmp_adj,
         upd_pas_yds_rat = upd_pas_yds_rat*pas_yds_adj,
         upd_pas_tds_rat = upd_pas_tds_rat*pas_tds_adj)

#clean up
updated_off_team_ratings <- updated_off_team_ratings %>% 
  select(team, upd_rus_att_rat:upd_int_rat)

colnames(updated_off_team_ratings) <- colnames(past_week_off_team_ratings)

#teams on bye
not_actice_off_team_ratings <- past_week_off_team_ratings %>% 
  filter(!(team %in% updated_off_team_ratings$team))

full_updated_off_team_ratings <- rbind(not_actice_off_team_ratings, updated_off_team_ratings)


####Def Team Ratings####
#get team stats
past_week_team_stats <- past_week_player_stats %>% 
  group_by(team, opp) %>% 
  summarise(across(pas_att:rec_tds, sum),
            .groups = 'drop') %>% 
  select(team, opp, pas_att:pas_tds, rus_att:rus_tds, int)

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
            d_rus_att = rus_att - team_rus_att_pred,
            d_rus_yds = rus_yds - team_rus_yds_pred,
            d_rus_tds = rus_tds - team_rus_tds_pred,
            d_int = int - team_int_pred)

#combine with ratings
updated_def_team_ratings <- left_join(def_team_dif, past_week_def_team_ratings, by = c("team"))

#volatility
#tested
updated_def_team_ratings <- updated_def_team_ratings %>% 
  mutate(vol = 0.03 + (0.2/past_week))

#update ratings
updated_def_team_ratings <- updated_def_team_ratings %>% 
  mutate(upd_rus_att_rat = def_rus_att_rat + d_rus_att*vol,
         upd_rus_yds_rat = def_rus_yds_rat + d_rus_yds*vol,
         upd_rus_tds_rat = def_rus_tds_rat + d_rus_tds*(vol/2),
         upd_pas_att_rat = def_pas_att_rat + d_pas_att*vol,
         upd_cmp_rat = def_cmp_rat + d_cmp*vol,
         upd_pas_yds_rat = def_pas_yds_rat + d_pas_yds*vol,
         upd_pas_tds_rat = def_pas_tds_rat + d_pas_tds*(vol/2),
         upd_int_rat = def_int_rat + d_int*(vol/2))

#clean up
updated_def_team_ratings <- updated_def_team_ratings %>% 
  select(team, upd_rus_att_rat:upd_int_rat)

colnames(updated_def_team_ratings) <- colnames(past_week_def_team_ratings)

#teams on bye
not_actice_def_team_ratings <- past_week_def_team_ratings %>% 
  filter(!(team %in% updated_def_team_ratings$team))

full_updated_def_team_ratings <- rbind(not_actice_def_team_ratings, updated_def_team_ratings)


####Write Csv####
write_csv(full_updated_player_percents, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", upcoming_week, "/Player_Percents.csv", sep = "")))
write_csv(updated_QB_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", upcoming_week, "/QB_Ratings.csv", sep = "")))
write_csv(full_updated_off_team_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", upcoming_week, "/Off_Team_Ratings.csv", sep = "")))
write_csv(full_updated_def_team_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", upcoming_week, "/Def_Team_Ratings.csv", sep = "")))


