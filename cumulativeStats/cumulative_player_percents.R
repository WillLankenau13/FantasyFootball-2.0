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

#read files
d_cumulative_off_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/cumulativeStats/cumulative_off_stats_", This_Year, ".csv", sep = "")))
d_cumulative_player_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/cumulativeStats/cumulative_player_stats_", This_Year, ".csv", sep = "")))

#rename games
cumulative_off_stats <- d_cumulative_off_stats %>% 
  rename("team_games" = "games")
cumulative_player_stats <- d_cumulative_player_stats %>% 
  rename("player_games" = "games")



#combine
cumulative_player_percents <- left_join(cumulative_player_stats, cumulative_off_stats, by = c("Team", "week")) %>% 
  select(Player, Team, Pos, week, player_games, team_games, cum_rus_att:cum_rec_tds, cum_fmb, cum_off_rus_att:cum_off_rus_tds, cum_off_cmp:cum_off_pas_tds, cum_off_fmb)

#player percents
cumulative_player_percents <- cumulative_player_percents %>% 
  mutate(rus_att_per = (cum_rus_att/player_games)/(cum_off_rus_att/team_games),
         rus_yds_per = (cum_rus_yds/player_games)/(cum_off_rus_yds/team_games),
         rus_tds_per = (cum_rus_tds/player_games)/(cum_off_rus_tds/team_games),
         tgt_per = (cum_tgt/player_games)/(cum_off_pas_att/team_games),
         rec_per = (cum_rec/player_games)/(cum_off_cmp/team_games),
         rec_yds_per = (cum_rec_yds/player_games)/(cum_off_pas_yds/team_games),
         rec_tds_per = (cum_rec_tds/player_games)/(cum_off_pas_tds/team_games),
         fmb_per = (cum_fmb/player_games)/(cum_off_fmb/team_games))

#write
write_csv(cumulative_player_percents, eval(paste("~/R Stuff/FantasyFootball 2.0/cumulativeStats/cumulative_player_percents_2022.csv", sep = "")))

