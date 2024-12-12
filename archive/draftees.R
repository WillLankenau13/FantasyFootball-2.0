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

#I'm not sure this file is useful anywhere

Past_Year <- 2021
This_Year <- 2022

#Read Files
draft <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/draft/draft_", Past_Year, ".csv", sep = "")))
Yahoo_Week_1 <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/Yahoo/", This_Year, "/Yahoo_Week_1.csv", sep = "")))
d_Player_percents_past_year <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonStats/", Past_Year, "/Player_percents_", Past_Year, ".csv", sep = "")))

#Names
player_names_func <- function(df){
  
  df$player <- str_replace_all(df$player, "[^[:alnum:]]", " ")
  df$player <- str_replace_all(df$player, "\\s+", " ")
  df$player <- str_replace_all(df$player, " III", " ")
  df$player <- str_replace_all(df$player, " II", " ")
  df$player <- str_replace_all(df$player, " Jr", " ")
  df$player <- trimws(df$player)
  
  df[df == "DJ Moore"] <- "D J Moore"
  df[df == "DK Metcalf"] <- "D K Metcalf"
  df[df == "Eli Mitchell"] <- "Elijah Mitchell"
  df[df == "Gabe Davis"] <- "Gabriel Davis"
  df[df == "Mitch Trubisky"] <- "Mitchell Trubisky"
  
  return(df)
}

d_Player_percents_past_year <- player_names_func(d_Player_percents_past_year)
Yahoo_Week_1 <- player_names_func(Yahoo_Week_1)

#Do stuff
draftees <- Yahoo_Week_1 %>% 
  left_join(d_Player_percents_past_year, by = c("full_name" = "player")) %>% 
  left_join(draft, by = c("full_name" = "Player")) %>% 
  select(full_name, Position, Team, player_games, pl_tgt, pl_rec, pl_rec_yds, pl_rec_tds, tgt_per, rec_per, rec_yds_per, rec_tds_per, Pick) %>% 
  filter(Position != "QB") %>% 
  filter(!is.na(Pick))
