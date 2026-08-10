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

player_predictions <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonPredictions/", This_Year, "/Full_Season_Predictions_", This_Year, ".csv", sep = "")))

fantasy_pros_QB <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyProsFullSeasonPredictions/", This_Year, "/FantasyProsQB.csv", sep = ""))) %>% 
  clean_names()
fantasy_pros_RB <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyProsFullSeasonPredictions/", This_Year, "/FantasyProsRB.csv", sep = ""))) %>% 
  clean_names()
fantasy_pros_WR <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyProsFullSeasonPredictions/", This_Year, "/FantasyProsWR.csv", sep = ""))) %>% 
  clean_names()
fantasy_pros_TE <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyProsFullSeasonPredictions/", This_Year, "/FantasyProsTE.csv", sep = ""))) %>% 
  clean_names()

fantasy_pros_QB <- player_names_func(fantasy_pros_QB)
fantasy_pros_RB <- player_names_func(fantasy_pros_RB)
fantasy_pros_WR <- player_names_func(fantasy_pros_WR)
fantasy_pros_TE <- player_names_func(fantasy_pros_TE)

####fantasyPros
fantasy_pros_TE <- fantasy_pros_TE %>% 
  mutate(rus_att_f = 0,
         rus_yds_f = 0,
         rus_tds_f = 0)

colnames(fantasy_pros_QB) <- c("player", "team", "pas_att_f", "cmp_f", "pas_yds_f", "pas_tds_f", "int_f", "rus_att_f", "rus_yds_f", "rus_tds_f", "fl_f", "fpts_f")

colnames(fantasy_pros_RB) <- c("player", "team", "rus_att_f", "rus_yds_f", "rus_tds_f", "rec_f", "rec_yds_f", "rec_tds_f", "fl_f", "fpts_f")
colnames(fantasy_pros_WR) <- c("player", "team", "rec_f", "rec_yds_f", "rec_tds_f", "rus_att_f", "rus_yds_f", "rus_tds_f", "fl_f", "fpts_f")
colnames(fantasy_pros_TE) <- c("player", "team", "rec_f", "rec_yds_f", "rec_tds_f", "fl_f", "fpts_f", "rus_att_f", "rus_yds_f", "rus_tds_f")

fantasy_pros_RB <- fantasy_pros_RB %>% 
  select(player, team, rus_att_f, rus_yds_f, rus_tds_f, rec_f, rec_yds_f, rec_tds_f, fl_f, fpts_f)
fantasy_pros_WR <- fantasy_pros_WR %>% 
  select(player, team, rus_att_f, rus_yds_f, rus_tds_f, rec_f, rec_yds_f, rec_tds_f, fl_f, fpts_f)
fantasy_pros_TE <- fantasy_pros_TE %>% 
  select(player, team, rus_att_f, rus_yds_f, rus_tds_f, rec_f, rec_yds_f, rec_tds_f, fl_f, fpts_f)

fantasy_pros_flex <- rbind(fantasy_pros_RB, fantasy_pros_TE, fantasy_pros_WR)

#half ppr
fantasy_pros_flex <- fantasy_pros_flex %>% 
  mutate(fpts_f = rus_yds_f*0.1 + rus_tds_f*6 + rec_yds_f*0.1 + rec_tds_f*6 + 0.5*rec_f - 2*fl_f)

#fantasy pros total
fantasy_pros_flex <- fantasy_pros_flex %>% 
  mutate(int_f = 0,
         pas_att_f = 0,
         cmp_f = 0,
         pas_yds_f = 0,
         pas_tds_f = 0) %>% 
  select(player, team, pas_att_f, cmp_f, pas_yds_f, pas_tds_f, rus_att_f, rus_yds_f, rus_tds_f, rec_f, rec_yds_f, rec_tds_f, fl_f, int_f, fpts_f)

fantasy_pros_QB <- fantasy_pros_QB %>% 
  mutate(rec_f = 0,
         rec_yds_f = 0, 
         rec_tds_f = 0) %>% 
  select(player, team, pas_att_f, cmp_f, pas_yds_f, pas_tds_f, rus_att_f, rus_yds_f, rus_tds_f, rec_f, rec_yds_f, rec_tds_f, fl_f, int_f, fpts_f)

fantasy_pros <- rbind(fantasy_pros_flex, fantasy_pros_QB)

#combine with fantasyPros
f_combined <- full_join(player_predictions, fantasy_pros, by = c("player", "Team_s" = "team")) %>% 
  filter(!is.na(fpts_pred)) %>% 
  filter(!is.na(fpts_f))

n_combined <- f_combined

####Combining fpros and my preds
cmp_m_coef <- 0
pas_att_m_coef <- 0
pas_yds_m_coef <- 0.7
pas_tds_m_coef <- 0.7 
rus_att_m_coef <- 0.5
rus_yds_m_coef <- 0.5
rus_tds_m_coef <- 0.3
rec_m_coef <- 0.5
rec_yds_m_coef <- 0.5
rec_tds_m_coef <- 0.3
int_m_coef <- 0.5
fl_m_coef <- 0.5

#combine
combine_predictions <- function(df, col){
  #percent rating coefficient
  m_coef <- get(paste(col, "_m_coef", sep = ""))
  
  #rcombine
  df[, paste(col, "_com", sep = "")] <- ((df[, paste(col, "_pred", sep = "")]*m_coef) + (df[, paste(col, "_f", sep = "")]*(1 - m_coef)))
  
  return(df)
}

####
f_combined <- combine_predictions(f_combined, "pas_att")
f_combined <- combine_predictions(f_combined, "cmp")
f_combined <- combine_predictions(f_combined, "pas_yds")
f_combined <- combine_predictions(f_combined, "pas_tds")
f_combined <- combine_predictions(f_combined, "rus_att")
f_combined <- combine_predictions(f_combined, "rus_yds")
f_combined <- combine_predictions(f_combined, "rus_tds")
f_combined <- combine_predictions(f_combined, "rec")
f_combined <- combine_predictions(f_combined, "rec_yds")
f_combined <- combine_predictions(f_combined, "rec_tds")
f_combined <- combine_predictions(f_combined, "int")
f_combined <- combine_predictions(f_combined, "fl")

#combine predictions
f_combined <- f_combined %>% 
  select(player, Position, Team, Team_s, pas_att_com:fl_com) %>% 
  mutate(fpts_com = pas_yds_com*0.04 + pas_tds_com*4 + rus_yds_com*0.1 + rus_tds_com*6 + rec_yds_com*0.1 + rec_tds_com*6 + 0.5*rec_com - 2*fl_com - 1*int_com)

write_csv(f_combined, eval(paste("~/R Stuff/FantasyFootball 2.0/combinedPredictions/", This_Year, "/combined_predictions_", This_Year, ".csv", sep = "")))





