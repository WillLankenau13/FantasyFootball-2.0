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

#whoever had more pas attempts week one
#Change Rodgers for injury

#read_csv
week_one_passing <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/WeeklyStats/week_one_", This_Year, "_passing.csv", sep = ""))) %>% 
  rename("player" = "Player")
teams <- read_csv("~/R Stuff/FantasyFootball 2.0/teams.csv")

#Names
player_names_func <- function(df){
  
  df$player <- str_replace_all(df$player, "[^[:alnum:]]", " ")
  df$player <- str_replace_all(df$player, "\\s+", " ")
  df$player <- str_replace_all(df$player, " III", " ")
  df$player <- str_replace_all(df$player, " II", " ")
  df$player <- str_replace_all(df$player, " Jr", " ")
  df$player <- trimws(df$player)
  
  df$player[df$player == "DJ Moore"] <- "D J Moore"
  df$player[df$player == "DK Metcalf"] <- "D K Metcalf"
  df$player[df$player == "Eli Mitchell"] <- "Elijah Mitchell"
  df$player[df$player == "Gabe Davis"] <- "Gabriel Davis"
  df$player[df$player == "Mitch Trubisky"] <- "Mitchell Trubisky"
  
  return(df)
}

week_one_passing <- player_names_func(week_one_passing)

#group by team
team <- week_one_passing %>% 
  group_by(Team) %>% 
  summarize(max = max(Att...15))

#keep starters
week_one_passing <- week_one_passing %>% 
  full_join(team, by = c("Team")) %>% 
  mutate(keep = ifelse(max == Att...15, 1, 0)) %>% 
  filter(keep == 1)

#select rows
starting_QBs <- week_one_passing %>% 
  select(player, Team)

#fix team names
starting_QBs <- starting_QBs %>% 
  left_join(teams, by = c("Team" = "Sportsref")) %>% 
  select(player, Short_Name)

colnames(starting_QBs) <- c("full_name", "Team")

starting_QBs$full_name[starting_QBs$full_name == "Zach Wilson"] <- "Aaron Rodgers"

#write_csv
write_csv(starting_QBs, eval(paste("~/R Stuff/FantasyFootball 2.0/startingQBs/Starting_QBs_", This_Year, ".csv", sep = "")))






