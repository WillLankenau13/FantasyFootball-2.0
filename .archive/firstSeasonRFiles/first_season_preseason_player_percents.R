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

###read files
d_Player_percents_past_year <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonStats/", Past_Year, "/Player_percents_", Past_Year, ".csv", sep = "")))
draft <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/draft/draft_", This_Year, ".csv", sep = ""))) %>% 
  rename("player" = "Player")
starting_QBs <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/startingQBs/Starting_QBs_", This_Year, ".csv", sep = "")))
teams <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/teams.csv", sep = "")))

#Yahoo Week 1
Yahoo_Week_1 <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/Yahoo/", This_Year, "/Yahoo_Week_1.csv", sep = ""))) %>% 
  select(ID:Starting) %>% 
  mutate(player = paste(`First Name`, `Last Name`)) %>% 
  filter(!is.na(ID))

Yahoo_Week_1 <- player_names_func(Yahoo_Week_1)
d_Player_percents_past_year <- player_names_func(d_Player_percents_past_year)
draft <- player_names_func(draft)

#Yahoo Teams
Yahoo_Week_1 <- Yahoo_Week_1 %>% 
  left_join(teams, by = c("Team" = "Yahoo")) %>% 
  select(ID:Position, Short_Name, Opponent:player) %>% 
  rename("Team" = "Short_Name")

#### Rushing ####
#Only starting QBs rushing
yes_starting_QBs <- starting_QBs %>% 
  mutate(starting = "yes")

Yahoo_Rushing <- Yahoo_Week_1 %>% 
  full_join(yes_starting_QBs, by = c("player" = "full_name", "Team")) %>% 
  filter(Position != "QB" | !is.na(starting))

#Join
Rushing_player_percents <- Yahoo_Rushing %>% 
  left_join(d_Player_percents_past_year, by = c("player")) %>% 
  left_join(draft, by = c("player")) %>% 
  select(player, Position, Team, player_games, pl_rus_att, pl_rus_yds, pl_rus_tds, rus_att_per, rus_yds_per, rus_tds_per, Pick) %>% 
  filter(Position == "RB" | !is.na(pl_rus_att) | (Position == "QB" & !is.na(Pick))) %>% 
  filter(!(is.na(Pick) & is.na(pl_rus_att))) %>% 
  filter(!(pl_rus_att == 0 & is.na(Pick)))

#Draftees player percents
NonQB_player_percents <- Rushing_player_percents %>% 
  filter(Position != "QB") %>% 
  mutate(rus_att_per = ifelse(is.na(rus_att_per), 0.76*exp(-0.00675*Pick), rus_att_per),
         rus_yds_per = ifelse(is.na(rus_yds_per), 0.76*exp(-0.00675*Pick), rus_yds_per),
         rus_tds_per = ifelse(is.na(rus_tds_per), 0.76*exp(-0.00675*Pick), rus_tds_per))

QB_player_percents <- Rushing_player_percents %>% 
  filter(Position == "QB") %>% 
  mutate(rus_att_per = ifelse(is.na(rus_att_per), 0.1, rus_att_per),
         rus_yds_per = ifelse(is.na(rus_yds_per), 0.1, rus_yds_per),
         rus_tds_per = ifelse(is.na(rus_tds_per), 0.1, rus_tds_per))

Rushing_player_percents <- rbind(QB_player_percents, NonQB_player_percents)

#Adjust Low Game Count
Rushing_player_percents <- Rushing_player_percents %>% 
  mutate(rus_tds_per = ifelse(is.na(player_games), rus_tds_per, ifelse(rus_tds_per/player_games > 0.3, 0.3*player_games, rus_tds_per)))

#Adjust TDs
Rushing_player_percents <- Rushing_player_percents %>% 
  mutate(rus_tds_per = 0.5*rus_tds_per + 0.4*rus_att_per + 0.1*rus_yds_per)

#Get percent by team
Rushing_per_by_team <- Rushing_player_percents %>% 
  group_by(Team) %>% 
  summarise(tot_rus_att_per = sum(rus_att_per),
            tot_rus_yds_per = sum(rus_yds_per),
            tot_rus_tds_per = sum(rus_tds_per))

#Adjust individuals
Rushing_player_percents <- Rushing_player_percents %>% 
  full_join(Rushing_per_by_team, by = c("Team")) %>% 
  mutate(adj_rus_att_per = rus_att_per/tot_rus_att_per,
         adj_rus_yds_per = rus_yds_per/tot_rus_yds_per,
         adj_rus_tds_per = rus_tds_per/tot_rus_tds_per)


#### Receiving ####
#Receiving_Past_Year
Receiving_player_percents <- Yahoo_Week_1 %>% 
  left_join(d_Player_percents_past_year, by = c("player")) %>% 
  left_join(draft, by = c("player")) %>% 
  select(player, Position, Team, player_games, pl_tgt, pl_rec, pl_rec_yds, pl_rec_tds, tgt_per, rec_per, rec_yds_per, rec_tds_per, Pick) %>% 
  filter(Position != "QB") %>% 
  filter(!is.na(pl_tgt) | !is.na(Pick)) %>% 
  filter(!(pl_tgt == 0 & is.na(Pick)))

#draftees
Receiving_player_percents <- Receiving_player_percents %>% 
  mutate(rep = ifelse(Position == "WR", 0.29*exp(-0.0106*Pick), ifelse(Position == "RB", 0.13*exp(-0.0045*Pick), ifelse(Position == "TE", 0.26*exp(-0.0114*Pick), NA)))) %>% 
  mutate(tgt_per = ifelse(is.na(tgt_per), rep, tgt_per),
         rec_per = ifelse(is.na(rec_per), rep, rec_per),
         rec_yds_per = ifelse(is.na(rec_yds_per), rep, rec_yds_per),
         rec_tds_per = ifelse(is.na(rec_tds_per), rep, rec_tds_per))

#Adjust Low Game Count
Receiving_player_percents <- Receiving_player_percents %>% 
  mutate(rec_tds_per = ifelse(is.na(player_games), rec_tds_per, ifelse(rec_tds_per/player_games > 0.3, 0.3*player_games, rec_tds_per)))

#Adjust TDs
Receiving_player_percents <- Receiving_player_percents %>% 
  mutate(rec_tds_per = 0.5*rec_tds_per + 0.25*tgt_per + 0.15*rec_per + 0.1*rec_yds_per)

#Sum percents by team
Receiving_per_by_team <- Receiving_player_percents %>% 
  group_by(Team) %>% 
  summarise(tot_tgt_per = sum(tgt_per),
            tot_rec_per = sum(rec_per),
            tot_rec_yds_per = sum(rec_yds_per),
            tot_rec_tds_per = sum(rec_tds_per))

#Adjust invididual percents
Receiving_player_percents <- Receiving_player_percents %>% 
  full_join(Receiving_per_by_team, by = c("Team")) %>% 
  mutate(adj_tgt_per = tgt_per/tot_tgt_per,
         adj_rec_per = rec_per/tot_rec_per,
         adj_rec_yds_per = rec_yds_per/tot_rec_yds_per,
         adj_rec_tds_per = rec_tds_per/tot_rec_tds_per)


#Write csv
write_csv(Receiving_player_percents, eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Receiving_Percents_", This_Year, ".csv", sep = "")))
write_csv(Rushing_player_percents, eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Rushing_Percents_", This_Year, ".csv", sep = "")))







