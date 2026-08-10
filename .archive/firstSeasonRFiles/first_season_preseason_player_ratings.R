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
d_py_player <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonStats/", Past_Year, "/Fantasy_", Past_Year, ".csv", sep = ""))) %>% 
  clean_names()

d_py_player <- player_names_func(d_py_player)

#select
player_ratings <- d_py_player %>% 
  select(player, tm, fant_pos, g, gs, cmp, att_9, yds_10, td_11, att_13, yds_14, td_16, tgt, rec, yds_19, td_21, int, fmb, fl)

#rename
colnames(player_ratings) <- c("player", "team", "pos", "player_games", "player_gs", "pl_cmp", "pl_pas_att", "pl_pas_yds", "pl_pas_tds", "pl_rus_att", "pl_rus_yds", "pl_rus_tds", "pl_tgt", "pl_rec", "pl_rec_yds", "pl_rec_tds", "pl_int", "pl_fmb", "pl_fl")

#fumbles
fl_per <- sum(player_ratings$pl_fl)/sum(player_ratings$pl_fmb)

player_ratings <- player_ratings %>% 
  mutate(pl_fl = 0.7*pl_fmb*fl_per + 0.3*pl_fl,
         touches = pl_rus_att + pl_rec + 1,
         fl_per_tou = pl_fl/touches,
         pl_int = ifelse(pos == "QB", pl_int, 0))

#QB Games
player_ratings <- player_ratings %>% 
  mutate(player_games = ifelse(pos == "QB", player_gs*0.8 + player_games*0.2, player_games))

#by game
player_ratings[6:20] <- player_ratings[6:20]/player_ratings$player_games

#write_csv
write_csv(player_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Player_Ratings_", This_Year, ".csv", sep = "")))






