


#year
Past_Year <- 2021
First_Year <- 2022

#read_csv
py_team_offense <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonStats/", Past_Year, "/team_offensive_stats.csv", sep = "")))
py_team_defense <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonStats/", Past_Year, "/team_defensive_stats.csv", sep = "")))
py_player_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonStats/", Past_Year, "/player_stats.csv", sep = "")))


####Offensive Team Ratings####
off_rat_function <- function(df, cols){
  #rcombine
  for(col in cols){
    df[, paste("off_", col, "_rat", sep = "")] <- df[, paste(col, sep = "")] / df[, "games"]
  }
  
  return(df)
}

col_list <- c("pas_att", "cmp", "pas_yds", "pas_tds", "int", "rus_att", "rus_yds", "rus_tds")
off_team_ratings <- off_rat_function(py_team_offense, col_list) %>% 
  select(team, off_pas_att_rat:off_rus_tds_rat)


####Defensive Team Ratings####
def_rat_function <- function(df, cols){
  #rcombine
  for(col in cols){
    df[, paste("def_", col, "_rat", sep = "")] <- df[, paste(col, sep = "")] / df[, "games"]
  }
  
  return(df)
}

col_list <- c("pas_att", "cmp", "pas_yds", "pas_tds", "int", "rus_att", "rus_yds", "rus_tds")
def_team_ratings <- def_rat_function(py_team_defense, col_list) %>% 
  select(opp, def_pas_att_rat:def_rus_tds_rat) %>% 
  rename("team" = "opp")


####QB Ratings####
qb_rat_function <- function(df, cols){
  #rcombine
  for(col in cols){
    df[, paste(col, "_rat", sep = "")] <- df[, paste(col, sep = "")] / df[, "tot_snap_per"]
  }
  
  return(df)
}

qb_stats <- py_player_stats %>% 
  filter(pos == "QB") %>% 
  filter(tot_snap_per > 1.5)

col_list <- c("pas_att", "cmp", "pas_yds", "pas_tds", "int", "sc_att", "sc_yds", "sc_tds")
qb_ratings <- qb_rat_function(qb_stats, col_list) %>% 
  mutate(py_games_played = NA,
         games_played = games) %>% 
  select(player, team, py_games_played, games_played, pas_att_rat:sc_tds_rat) 


####Player Percents####
#average for 2TM players
col_list <- c("pas_att", "cmp", "pas_yds", "pas_tds", "int", "sc_att", "sc_yds", "sc_tds", "rus_att", "rus_yds", "rus_tds", "tgt", "rec", "rec_yds", "rec_tds")
avg_py_team_offense <- py_team_offense %>% 
  summarize(team = "2TM",
            across(all_of(col_list), ~ mean(.x, na.rm = TRUE)),
            games = 17)

py_team_offense_w_avg <- rbind(py_team_offense, avg_py_team_offense)

player_percents <- py_player_stats %>% 
  left_join(py_team_offense_w_avg, by = c("team"), suffix = c("", "_team"))

pp_function <- function(df, cols){
  #rcombine
  for(col in cols){
    df[, paste(col, "_per", sep = "")] <- (df[, paste(col, sep = "")]/df[, "games"])/ (df[, paste(col, "_team", sep = "")]/df[, "games_team"])
  }
  
  return(df)
}

col_list <- c("rus_att", "rus_yds", "rus_tds", "tgt", "rec", "rec_yds", "rec_tds")
player_percents <- pp_function(player_percents, col_list) %>% 
  mutate(py_games_played = NA,
         games_played = games) %>% 
  select(player, pos, team, py_games_played, games_played, rus_att_per:rec_tds_per)

#adjustments
player_percents <- player_percents %>% 
  mutate(rus_yds_per = 0.95*rus_yds_per + 0.05*rus_att_per,
          rus_tds_per = 0.75*rus_tds_per + 0.1*rus_yds_per + 0.15*rus_att_per,
          rec_per = 0.85*rec_per + 0.15*tgt_per,
          rec_yds_per = 0.9*rec_yds_per + rec_per*0 + tgt_per*0.1,
          rec_tds_per = 0.7*rec_tds_per + 0.1*rec_yds_per + 0*rec_per + 0.2*tgt_per)




####Write Csv####
write_csv(player_percents, (eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", Past_Year, "/Week_19/Player_Percents.csv", sep = ""))))
write_csv(qb_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", Past_Year, "/Week_19/QB_Ratings.csv", sep = "")))
write_csv(off_team_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", Past_Year, "/Week_19/Off_Team_Ratings.csv", sep = "")))
write_csv(def_team_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", Past_Year, "/Week_19/Def_Team_Ratings.csv", sep = "")))



