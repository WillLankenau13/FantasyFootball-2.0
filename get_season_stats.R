
#Year
This_Year <- 2021

#df_list
df_list <- list()
c <- 1

while(c < 19){
  temp <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/", This_Year, "/byWeek/Week_", c, "_Stats.csv", sep = "")))
  
  df_list[[c]] <- temp
  c <- c+1
}

#combine data
data <- do.call(rbind, df_list)

#sum offensive stats
off_col_list <- c("pas_att", "cmp", "pas_yds", "pas_tds", "int", "sc_att", "sc_yds", "sc_tds", "rus_att", "rus_yds", "rus_tds", "tgt", "rec", "rec_yds", "rec_tds")
team_offense <- data %>% 
  group_by(team) %>% 
  summarize(across(all_of(off_col_list), ~ sum(.x, na.rm = TRUE)),
            games = n_distinct(week))

#sum defensive stats
def_col_list <- c("pas_att", "cmp", "pas_yds", "pas_tds", "int", "rus_att", "rus_yds", "rus_tds", "tgt", "rec", "rec_yds", "rec_tds")
team_defense <- data %>% 
  group_by(opp) %>% 
  summarize(across(all_of(def_col_list), ~ sum(.x, na.rm = TRUE)),
            games = n_distinct(week))

#sum individual stats
ind_col_list <- c("pas_att", "cmp", "pas_yds", "pas_tds", "int", "sc_att", "sc_yds", "sc_tds", "rus_att", "rus_yds", "rus_tds", "tgt", "rec", "rec_yds", "rec_tds", "fmb_l", "touches")
player_season_stats <- data %>% 
  mutate(gs = ifelse(snap_per > 0.6 & pos == "QB", 1, 0),
         game_played = snap_per > 0 | st_snaps > 0,
         fmb_l = fmb_l_g,
         touches = touches_g) %>% 
  group_by(player, pos) %>% 
  summarize(team = if (n_distinct(team) > 1) {"2TM"} else {team[which.max(week)]},
            recent_team = team[which.max(week)],
            across(all_of(ind_col_list), ~ sum(.x, na.rm = TRUE)),
            games = sum(game_played),
            tot_snap_per = sum(snap_per))


#write csv
write_csv(team_offense, eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonStats/", This_Year, "/team_offensive_stats.csv", sep = "")))
write_csv(team_defense, eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonStats/", This_Year, "/team_defensive_stats.csv", sep = "")))
write_csv(player_season_stats, eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonStats/", This_Year, "/player_stats.csv", sep = "")))

