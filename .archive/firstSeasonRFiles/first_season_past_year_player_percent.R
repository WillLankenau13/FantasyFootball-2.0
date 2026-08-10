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
d_py_off <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonStats/", Past_Year, "/Team_Offense_", Past_Year, ".csv", sep = ""))) %>% 
  clean_names()
teams <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/teams.csv", sep = "")))

d_py_player <- player_names_func(d_py_player)
d_py_off <- player_names_func(d_py_off)

#select
py_player <- d_py_player %>% 
  select(player, tm, fant_pos, g, gs, att_13, yds_14, td_16, tgt, rec, yds_19, td_21)

py_off <- d_py_off %>% 
  select(tm, g, cmp, att_12, yds_13, td_14, att_18, yds_19, td_20)

#rename
colnames(py_player) <- c("player", "team", "pos", "player_games", "player_gs", "pl_rus_att", "pl_rus_yds", "pl_rus_tds", "pl_tgt", "pl_rec", "pl_rec_yds", "pl_rec_tds")
colnames(py_off) <- c("l_team", "team_games", "off_cmp", "off_pas_att", "off_pas_yds", "off_pas_tds", "off_rus_att", "off_rus_yds", "off_rus_tds")

#averages
py_off <- py_off %>% 
  adorn_totals(name = "2Teams")

Data1 <- subset(py_off, l_team != "2Teams")
Data2 <- subset(py_off, l_team == "2Teams")

Data2[2:9] <- Data2[2:9] / 32

py_off <- rbind(Data1, Data2)

#join
py_player_percents <- left_join(py_player, teams, by = c("team" = "Sportsref")) %>% 
  left_join(py_off, by = c("Long_Name" = "l_team"))

#percents
percents <- function(df, new_col, pl_col, t_col){
  df[, paste(new_col)] <- (df[, paste(pl_col)]/df[, "player_games"])/(df[, paste(t_col)]/17)
  return(df)
}

py_player_percents <- percents(py_player_percents, "rus_att_per", "pl_rus_att", "off_rus_att")
py_player_percents <- percents(py_player_percents, "rus_yds_per", "pl_rus_yds", "off_rus_yds")
py_player_percents <- percents(py_player_percents, "rus_tds_per", "pl_rus_tds", "off_rus_tds")
py_player_percents <- percents(py_player_percents, "tgt_per", "pl_tgt", "off_pas_att")
py_player_percents <- percents(py_player_percents, "rec_per", "pl_rec", "off_cmp")
py_player_percents <- percents(py_player_percents, "rec_yds_per", "pl_rec_yds", "off_pas_yds")
py_player_percents <- percents(py_player_percents, "rec_tds_per", "pl_rec_tds", "off_pas_tds")


#write
write_csv(py_player_percents, eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonStats/", Past_Year, "/Player_percents_", Past_Year, ".csv", sep = "")))



