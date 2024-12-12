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
d_Off_stats_past_year <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonStats/", Past_Year, "/Team_Offense_", Past_Year, ".csv", sep = "")))
d_def_stats_past_year <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonStats/", Past_Year, "/Team_Defense_", Past_Year, ".csv", sep = "")))
d_fantasy_past_year <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonStats/", Past_Year, "/Fantasy_", Past_Year, ".csv", sep = ""))) %>% 
  rename("player" = "Player")
d_Yahoo_Week_1 <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/Yahoo/", This_Year, "/Yahoo_Week_1.csv", sep = "")))
d_Player_percents_past_year <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonStats/", Past_Year, "/Player_percents_", Past_Year, ".csv", sep = "")))
starting_QBs <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/startingQBs/Starting_QBs_", This_Year, ".csv", sep = "")))
teams <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/teams.csv", sep = "")))

#Yahoo Week 1
Yahoo_Week_1 <- d_Yahoo_Week_1 %>% 
  select(ID:Starting) %>% 
  mutate(player = paste(`First Name`, `Last Name`)) %>% 
  filter(!is.na(ID))

#Names
Yahoo_Week_1 <- player_names_func(Yahoo_Week_1)
d_fantasy_past_year <- player_names_func(d_fantasy_past_year)
d_Player_percents_past_year <- player_names_func(d_Player_percents_past_year)
d_Off_stats_past_year <- player_names_func(d_Off_stats_past_year)
d_def_stats_past_year <- player_names_func(d_def_stats_past_year)

#Yahoo Teams
Yahoo_Week_1 <- Yahoo_Week_1 %>% 
  left_join(teams, by = c("Team" = "Yahoo")) %>% 
  select(ID:Position, Short_Name, Opponent:player) %>% 
  rename("Team" = "Short_Name")

#Max Percents
QB_mp_yds <- 0.6
QB_mp_tds <- 0.5
QB_mp_cmp <- 0.6
QB_mp_att <- 0.5
Rus_mp <- 0.4
Rec_mp <- 0.2

#Regression to mean
# rm_pas_yds <- 0.6
# rm_pas_tds <- 0.3
# rm_cmp <- 0.5 #
# rm_att <- 0.5 #
# rm_rus_yds <- 0.6 #
# rm_rus_tds <- 0.3 #
# rm_rus_att <- 0.5 #

###Off_stats_past_year
Off_stats_past_year <- d_Off_stats_past_year %>% 
  select(Tm, G, `Att...18`, `Yds...19`, `TD...20`, Cmp, `Att...12`, `Yds...13`, `TD...14`, Int)

Off_stats_past_year <- Off_stats_past_year %>% 
  rename("Team" = "Tm") %>% 
  rename("Games" = "G") %>% 
  rename("Off_Rus_Att" = "Att...18") %>% 
  rename("Off_Rus_Yds" = "Yds...19") %>% 
  rename("Off_Rus_Tds" = "TD...20") %>% 
  rename("Off_Cmp" = "Cmp") %>% 
  rename("Off_Pas_Att" = "Att...12") %>% 
  rename("Off_Pas_Yds" = "Yds...13") %>% 
  rename("Off_Pas_Tds" = "TD...14") %>% 
  rename("Off_Int" = "Int")

###def_stats_past_year
def_stats_past_year <- d_def_stats_past_year %>% 
  select(Tm, G, `Att...18`, `Yds...19`, `TD...20`, Cmp, `Att...12`, `Yds...13`, `TD...14`, Int)

def_stats_past_year <- def_stats_past_year %>% 
  rename("Team" = "Tm") %>% 
  rename("Games" = "G") %>% 
  rename("Def_Rus_Att" = "Att...18") %>% 
  rename("Def_Rus_Yds" = "Yds...19") %>% 
  rename("Def_Rus_Tds" = "TD...20") %>% 
  rename("Def_Cmp" = "Cmp") %>% 
  rename("Def_Pas_Att" = "Att...12") %>% 
  rename("Def_Pas_Yds" = "Yds...13") %>% 
  rename("Def_Pas_Tds" = "TD...14") %>% 
  rename("Def_Int" = "Int")

###QB_past_year
QB_past_year <- d_fantasy_past_year %>% 
  filter(FantPos == "QB") %>% 
  select(player, Tm, G, GS, Cmp, `Att...9`, `Yds...10`, `TD...11`, Int) %>% 
  mutate(QB_Games = GS + (G - GS)/2) %>% 
  rename("QB_Cmp" = "Cmp",
         "QB_Att" = "Att...9",
         "QB_Yds" = "Yds...10",
         "QB_TDs" = "TD...11",
         "QB_Int" = "Int") %>% 
  select(player:Tm, QB_Games, QB_Cmp:QB_Int)

#Full Year Stats
QB_past_year <- QB_past_year %>% 
  mutate(QB_Cmp = QB_Cmp*17/QB_Games,
         QB_Att = QB_Att*17/QB_Games,
         QB_Yds = QB_Yds*17/QB_Games,
         QB_TDs = QB_TDs*17/QB_Games)

QB_past_year <- QB_past_year %>% 
  right_join(starting_QBs, by = c("player" = "full_name")) %>% 
  select(!Tm)

#NA Players
Main_QBs <- QB_past_year %>% 
  filter(QB_Games >= 10)

#Replacement Level
n = length(Main_QBs$QB_Cmp)
Cmp_rep = 0.9/3*(sort(Main_QBs$QB_Cmp)[2] + sort(Main_QBs$QB_Cmp)[3] + sort(Main_QBs$QB_Cmp)[4])
Att_rep = 0.9/3*(sort(Main_QBs$QB_Att)[2] + sort(Main_QBs$QB_Att)[3] + sort(Main_QBs$QB_Att)[4])
Yds_rep = 0.9/3*(sort(Main_QBs$QB_Yds)[2] + sort(Main_QBs$QB_Yds)[3] + sort(Main_QBs$QB_Yds)[4])
Tds_rep = 0.9/3*(sort(Main_QBs$QB_TDs)[2] + sort(Main_QBs$QB_TDs)[3] + sort(Main_QBs$QB_TDs)[4])
Int_rep = 1.1/3*(sort(Main_QBs$QB_Int)[n-1] + sort(Main_QBs$QB_Int)[n-2] + sort(Main_QBs$QB_Int)[n-3])

#Replace with replacement level if necessary
QB_past_year <- QB_past_year %>% 
  mutate(QB_Cmp = ifelse(is.na(QB_Cmp) | (QB_Games < 10 & QB_Cmp < Cmp_rep), Cmp_rep, QB_Cmp),
         QB_Att = ifelse(is.na(QB_Att) | (QB_Games < 10 & QB_Att < Att_rep), Att_rep, QB_Att),
         QB_Yds = ifelse(is.na(QB_Yds) | (QB_Games < 10 & QB_Yds < Yds_rep), Yds_rep, QB_Yds),
         QB_TDs = ifelse(is.na(QB_TDs) | (QB_Games < 10 & QB_TDs < Tds_rep), Tds_rep, QB_TDs),
         QB_Int = ifelse(is.na(QB_Int) | (QB_Games < 10 & QB_Int < Int_rep), Int_rep, QB_Int))


###Rushing_past_year
yes_starting_QBs <- starting_QBs %>% 
  mutate(starting = "yes")

#Only starting QBs rushing
Yahoo_Rushing <- Yahoo_Week_1 %>% 
  full_join(yes_starting_QBs, by = c("Team")) %>% 
  filter(Position != "QB" | !is.na(starting))

#Join
Rushing_past_year <- Yahoo_Rushing %>% 
  left_join(d_Player_percents_past_year, by = c("player")) %>% 
  filter(!is.na(rus_yds_per)) %>% 
  select(player, Team, player_games, pl_rus_att, pl_rus_yds, pl_rus_tds, rus_att_per, rus_yds_per, rus_tds_per)

#Get percent by team
Rushing_per_by_team <- Rushing_past_year %>% 
  group_by(Team) %>% 
  summarise(tot_rus_att_per = sum(rus_att_per),
            tot_rus_yds_per = sum(rus_yds_per),
            tot_rus_tds_per = sum(rus_tds_per))

#Adjust individuals
Rushing_past_year <- Rushing_past_year %>% 
  full_join(Rushing_per_by_team, by = c("Team")) %>% 
  mutate(adj_pl_rus_att = pl_rus_att/tot_rus_att_per,
         adj_pl_rus_yds = pl_rus_yds/tot_rus_yds_per,
         adj_pl_rus_tds = pl_rus_tds/tot_rus_tds_per)

#Get new team stats
Rushing_by_team <- Rushing_past_year %>% 
  group_by(Team) %>% 
  summarise(tot_rus_att = sum(adj_pl_rus_att),
            tot_rus_yds = sum(adj_pl_rus_yds),
            tot_rus_tds = sum(adj_pl_rus_tds),
            tot_rus_att_per = sum(rus_att_per),
            tot_rus_yds_per = sum(rus_yds_per),
            tot_rus_tds_per = sum(rus_tds_per)) %>% 
  mutate(rus_att_ratio = ifelse(tot_rus_att_per > 1, Rus_mp, tot_rus_att_per*Rus_mp),
         rus_yds_ratio = ifelse(tot_rus_yds_per > 1, Rus_mp, tot_rus_yds_per*Rus_mp),
         rus_tds_ratio = ifelse(tot_rus_tds_per > 1, Rus_mp, tot_rus_tds_per*Rus_mp))

Rushing_by_team[Rushing_by_team == "NaN"] <- 0

###Receiving_Past_Year
Receiving_Past_Year <- Yahoo_Week_1 %>% 
  left_join(d_Player_percents_past_year, by = c("player")) %>% 
  filter(!is.na(rec_yds_per)) %>% 
  select(player, Team, player_games, pl_tgt, pl_rec, pl_rec_yds, pl_rec_tds, tgt_per, rec_per, rec_yds_per, rec_tds_per)

#Sum percents by team
Receiving_per_by_team <- Receiving_Past_Year %>% 
  group_by(Team) %>% 
  summarise(tot_tgt_per = sum(tgt_per),
            tot_rec_per = sum(rec_per),
            tot_rec_yds_per = sum(rec_yds_per),
            tot_rec_tds_per = sum(rec_tds_per))

#Adjust invididual percents
Receiving_Past_Year <- Receiving_Past_Year %>% 
  full_join(Receiving_per_by_team, by = c("Team")) %>% 
  mutate(adj_pl_tgt = pl_tgt/tot_tgt_per,
         adj_pl_rec = pl_rec/tot_rec_per,
         adj_pl_rec_yds = pl_rec_yds/tot_rec_yds_per,
         adj_pl_rec_tds = pl_rec_tds/tot_rec_tds_per)

#New team stats
Receiving_by_team <- Receiving_Past_Year %>% 
  group_by(Team) %>% 
  summarise(tot_tgt = sum(adj_pl_tgt),
            tot_rec = sum(adj_pl_rec),
            tot_rec_yds = sum(adj_pl_rec_yds),
            tot_rec_tds = sum(adj_pl_rec_tds),
            tot_tgt_per = sum(tgt_per),
            tot_rec_per = sum(rec_per),
            tot_rec_yds_per = sum(rec_yds_per),
            tot_rec_tds_per = sum(rec_tds_per)) %>% 
  mutate(tgt_ratio = ifelse(tot_tgt_per > 1, Rec_mp, tot_tgt_per*Rec_mp),
         rec_ratio = ifelse(tot_rec_per > 1, Rec_mp, tot_rec_per*Rec_mp),
         rec_yds_ratio = ifelse(tot_rec_yds_per > 1, Rec_mp, tot_rec_yds_per*Rec_mp),
         rec_tds_ratio = ifelse(tot_rec_tds_per > 1, Rec_mp, tot_rec_tds_per*Rec_mp))

Receiving_by_team[Receiving_by_team == "NaN"] <- 0

###Preseason Ratings
##offensive ratings
small_teams <- teams %>% 
  select(Long_Name, Short_Name) %>% 
  distinct(Long_Name, .keep_all = TRUE)

off_ratings <- Off_stats_past_year %>% 
  left_join(small_teams, by = c("Team" = "Long_Name")) %>% 
  left_join(QB_past_year, by = c("Short_Name" = "Team")) %>% 
  filter(!is.na(player)) %>% 
  left_join(Rushing_by_team, by = c("Short_Name" = "Team")) %>% 
  left_join(Receiving_by_team, by = c("Short_Name" = "Team"))


off_ratings <- off_ratings %>%
  transmute(Team = Team,
            Team_s = Short_Name,
            Games = Games,
            QB_Games = QB_Games,
            QB = player,
            Off_Rus_Att_Rat = Off_Rus_Att*(1-rus_att_ratio) + tot_rus_att*rus_att_ratio,
            Off_Rus_Yds_Rat = Off_Rus_Yds*(1-rus_yds_ratio) + tot_rus_yds*rus_yds_ratio,
            Off_Rus_Tds_Rat = Off_Rus_Tds*(1-rus_tds_ratio) + tot_rus_tds*rus_tds_ratio,
            Off_Cmp_Rat = QB_mp_cmp*QB_Cmp + Off_Cmp*(1-rec_ratio-QB_mp_cmp) + tot_rec*rec_ratio,
            Off_Pas_Att_Rat = QB_mp_att*QB_Att + Off_Pas_Att*(1-tgt_ratio-QB_mp_att) + tot_tgt*tgt_ratio,
            Off_Pas_Yds_Rat = QB_mp_yds*QB_Yds + Off_Pas_Yds*(1-rec_yds_ratio-QB_mp_yds) + tot_rec_yds*rec_yds_ratio,
            Off_Pas_Tds_Rat = QB_mp_tds*QB_TDs + Off_Pas_Tds*(1-rec_tds_ratio-QB_mp_tds) + tot_rec_tds*rec_tds_ratio,
            Off_Int_Rat = QB_Int)

#per game
off_ratings[6:13] <- off_ratings[6:13]/off_ratings$Games

##defensive ratings
def_ratings <- def_stats_past_year

#Team Name
small_teams <- teams %>% 
  select("Short_Name", "Long_Name") %>% 
  distinct()

def_ratings <- def_ratings %>% 
  left_join(small_teams, by = c("Team" = "Long_Name")) %>% 
  select(Short_Name, Games, Def_Rus_Att:Def_Int)

#per game
def_ratings[3:10] <- def_ratings[3:10]/def_ratings$Games

#regression coefficients
#tested on earlier data before percents were made better
Off_Rus_Att_Rat_reg <- 0.5
Off_Rus_Yds_Rat_reg <- 0.5
Off_Rus_Tds_Rat_reg <- 0.15
Off_Cmp_Rat_reg <- 0.7
Off_Pas_Att_Rat_reg <- 0.8
Off_Pas_Yds_Rat_reg <- 0.6
Off_Pas_Tds_Rat_reg <- 0.4

#
Def_Rus_Att_reg <- 0.5
Def_Rus_Yds_reg <- 0.7
Def_Rus_Tds_reg <- 0.15
Def_Cmp_reg <- 0.1
Def_Pas_Att_reg <- 0.2
Def_Pas_Yds_reg <- 0.1
Def_Pas_Tds_reg <- 0.1

#regress to mean
regress <- function(df, col){
  #get mean value
  t <- data.frame(df[, paste(col)])
  mean_val <- mean(t[ , 1])
  
  #regression coefficient
  reg_coef <- get(paste(col, "_reg", sep = ""))
  
  #regress
  df[, paste(col)] <- (df[, paste(col)] - mean_val)*reg_coef + mean_val
  
  #return
  return(df)
}

off_ratings <- regress(off_ratings, "Off_Rus_Att_Rat")
off_ratings <- regress(off_ratings, "Off_Rus_Yds_Rat")
off_ratings <- regress(off_ratings, "Off_Rus_Tds_Rat")
off_ratings <- regress(off_ratings, "Off_Cmp_Rat")
off_ratings <- regress(off_ratings, "Off_Pas_Att_Rat")
off_ratings <- regress(off_ratings, "Off_Pas_Yds_Rat")
off_ratings <- regress(off_ratings, "Off_Pas_Tds_Rat")

def_ratings <- regress(def_ratings, "Def_Rus_Att")
def_ratings <- regress(def_ratings, "Def_Rus_Yds")
def_ratings <- regress(def_ratings, "Def_Rus_Tds")
def_ratings <- regress(def_ratings, "Def_Cmp")
def_ratings <- regress(def_ratings, "Def_Pas_Att")
def_ratings <- regress(def_ratings, "Def_Pas_Yds")
def_ratings <- regress(def_ratings, "Def_Pas_Tds")

#Write csv
write_csv(off_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Offensive_Ratings_", This_Year, ".csv", sep = "")))
write_csv(def_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Defensive_Ratings_", This_Year, ".csv", sep = "")))


