library(nflfastR)

df_list <- list()

c <- 1

while(c < 13){
  


# #Week
past_week <- c

#Year
This_Year <- This_Year_d

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
past_week_st_snaps <- d_past_week_st_snaps
names(past_week_st_snaps)[names(past_week_st_snaps) == "st_snp_17"] <- "st_snp"
past_week_st_snaps <- past_week_st_snaps %>% 
  select(player, pos, g_number, week, team, opp, st_snp)

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



roster <- nflfastR::fast_scraper_roster(2025) %>% 
  select(!week)


snaps <- load_snap_counts(2025) 
snaps <- snaps %>% 
  rename("opp" = "opponent") %>% 
  rename("pos" = "position")
snaps <- player_names_func(snaps)


df <- load_pbp(2025)
df[df == "Mi.Wilson"] <- "M.Wilson"
  

data <- df %>% 
  select(!posteam_timeouts_remaining:fourth_down_failed, punt_inside_twenty:kickoff_fair_catch, lateral_receiver_player_id:pass_defense_2_player_name, sack_player_id:return_yards)

t <- df %>% 
  filter(play_type != "no_play") %>% 
  filter(receiver_player_name == "C.Brown") %>% 
  filter(week == 12)

s <- df %>% 
  filter(rusher == "B.Hall") %>% 
  filter(week == 6)

ari1 <- data %>% 
  filter(posteam == "ARI") %>% 
  filter(week == 1) %>% 
  filter(play_type != "no_play") %>%
  filter(qb_scramble == 0) %>% 
  filter(pass == 1)

weekly_passing_stats <- df %>% 
  filter(play_type != "no_play") %>%
  filter(qb_scramble == 0) %>% 
  filter(sack == 0) %>% 
  filter(play_type_nfl != "UNSPECIFIED") %>%
  filter(is.na(two_point_conv_result)) %>% 
  group_by(posteam, defteam, week, passer_player_name, passer_player_id) %>% 
  summarize(pas_att = sum(pass) + sum(qb_spike),
            cmp = sum(complete_pass),
            pas_yds = sum(receiving_yards, na.rm = TRUE) + sum(lateral_receiving_yards, na.rm = TRUE),
            pas_tds = sum(pass_touchdown)) %>% 
  filter(!is.na(passer_player_name)) %>% 
  rename("player" = "passer_player_name") %>% 
  rename("player_id" = "passer_player_id")

weekly_receiving_stats <- df %>% 
  filter(play_type != "no_play") %>%
  filter(qb_scramble == 0) %>% 
  filter(sack == 0) %>% 
  filter(play_type_nfl != "UNSPECIFIED") %>% 
  filter(is.na(two_point_conv_result)) %>% 
  group_by(posteam, defteam, week, receiver_player_name, receiver_player_id) %>% 
  summarize(tgt = sum(pass),
            rec = sum(complete_pass),
            rec_yds = sum(receiving_yards, na.rm = TRUE)) %>% 
  filter(!is.na(receiver_player_name)) %>% 
  rename("player" = "receiver_player_name") %>% 
  rename("player_id" = "receiver_player_id")

weekly_lateral_receiving_stats <- df %>% 
  filter(play_type != "no_play") %>%
  filter(qb_scramble == 0) %>% 
  filter(sack == 0) %>% 
  filter(play_type_nfl != "UNSPECIFIED") %>% 
  filter(is.na(two_point_conv_result)) %>% 
  group_by(posteam, defteam, week, lateral_receiver_player_name, lateral_receiver_player_id) %>% 
  summarize(lat_rec_yds = sum(lateral_receiving_yards, na.rm = TRUE)) %>% 
  filter(!is.na(lateral_receiver_player_name)) %>% 
  rename("player" = "lateral_receiver_player_name") %>% 
  rename("player_id" = "lateral_receiver_player_id")

weekly_rushing_stats <- df %>% 
  filter(play_type != "no_play") %>% 
  filter(play_type_nfl != "UNSPECIFIED") %>% 
  filter(qb_scramble == 0) %>% 
  filter(is.na(two_point_conv_result)) %>% 
  group_by(posteam, defteam, week, rusher_player_name, rusher_player_id) %>% 
  summarize(rus_att = sum(rush),
            rus_yds = sum(rushing_yards, na.rm = TRUE)) %>% 
  filter(!is.na(rusher_player_name)) %>%
  rename("player" = "rusher_player_name") %>% 
  rename("player_id" = "rusher_player_id")

weekly_lateral_rushing_stats <- df %>% 
  filter(play_type != "no_play") %>% 
  filter(play_type_nfl != "UNSPECIFIED") %>% 
  filter(qb_scramble == 0) %>% 
  filter(is.na(two_point_conv_result)) %>% 
  group_by(posteam, defteam, week, lateral_rusher_player_name, lateral_rusher_player_id) %>% 
  summarize(lat_rus_yds = sum(lateral_rushing_yards, na.rm = TRUE)) %>% 
  filter(!is.na(lateral_rusher_player_name)) %>%
  rename("player" = "lateral_rusher_player_name") %>% 
  rename("player_id" = "lateral_rusher_player_id")

weekly_td_stats <- df %>% 
  filter(play_type != "no_play") %>% 
  filter(play_type_nfl != "UNSPECIFIED") %>% 
  filter(qb_scramble == 0) %>% 
  filter(is.na(two_point_conv_result)) %>% 
  group_by(posteam, defteam, week, td_player_name, td_player_id) %>% 
  summarize(rus_tds = sum(rush_touchdown, na.rm = TRUE),
            rec_tds = sum(pass_touchdown, na.rm = TRUE)) %>% 
  filter(!is.na(td_player_name)) %>%
  rename("player" = "td_player_name") %>% 
  rename("player_id" = "td_player_id")

dfs <- list(weekly_passing_stats, weekly_rushing_stats, weekly_lateral_rushing_stats, weekly_receiving_stats, weekly_lateral_receiving_stats, weekly_td_stats)

weekly_stats <- reduce(dfs, full_join, by = c("player", "player_id", "posteam", "defteam", "week")) 
weekly_stats[is.na(weekly_stats)] <- 0

weekly_stats <- weekly_stats %>% 
  mutate(rus_yds = rus_yds + lat_rus_yds,
         rec_yds = rec_yds + lat_rec_yds) %>% 
  ungroup() %>% 
  left_join(roster, by = c("player_id" = "gsis_id")) %>% 
  select(full_name, position, week, posteam, defteam, pas_att, cmp, pas_yds, pas_tds, rus_att, rus_yds, rus_tds, tgt, rec, rec_yds, rec_tds)
colnames(weekly_stats) <- c("player", "pos", "week", "team", "opp", "pas_att", "cmp", "pas_yds", "pas_tds", "rus_att", "rus_yds", "rus_tds", "tgt", "rec", "rec_yds", "rec_tds")

weekly_stats <- player_names_func(weekly_stats)

weekly_stats <- weekly_stats %>% 
  full_join(snaps, by = c("player", "pos", "team", "opp", "week")) %>% 
  filter(pos %in% c("QB", "RB", "WR", "TE")) %>% 
  select(player, pos, week, team, opp, pas_att, cmp, pas_yds, pas_tds, rus_att, rus_yds, rus_tds, tgt, rec, rec_yds, rec_tds, offense_snaps, offense_pct, st_snaps, st_pct)

colnames(weekly_stats) <- c("player", "pos", "week", "team", "opp", "pas_att", "cmp", "pas_yds", "pas_tds", "rus_att", "rus_yds", "rus_tds", "tgt", "rec", "rec_yds", "rec_tds", "off_snps", "off_snp_per", "st_snaps", "st_snp_per")
weekly_stats[is.na(weekly_stats)] <- 0

checking <- weekly_stats %>% 
  filter(week == past_week) %>% 
  full_join(past_week_player_stats, by = c("player", "pos", "week", "team", "opp")) %>% 
  mutate(off_per.x = off_snp_per*100,
         off_per.y = round(snap_per)) %>% 
  filter(pas_att.x != pas_att.y | cmp.x != cmp.y | pas_yds.x != pas_yds.y | pas_tds.x != pas_tds.y | tgt.x != tgt.y | rec.x != rec.y | rec_yds.x != rec_yds.y | rec_tds.x != rec_tds.y | abs(off_per.x - off_per.y) > 1 | st_snaps.x != st_snaps.y)
  
checking2 <- weekly_stats %>% 
  filter(week == past_week) %>% 
  full_join(past_week_player_stats, by = c("player", "pos", "week", "team", "opp")) %>% 
  mutate(off_per.x = off_snp_per*100,
         off_per.y = round(snap_per)) %>% 
  filter(pos != "QB") %>% 
  filter(rus_att.x != rus_att.y | rus_yds.x != rus_yds.y | rus_tds.x != rus_tds.y)

temp <- rbind(checking, checking2)

df_list[[c]] <- temp

c <- c+1
}

combined <- do.call(rbind, df_list)

