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



###Years
#Set Years
Past_Year_d <- 2021
This_Year_d <- 2022

Years_Dataframe <- data.frame(Past_Year = Past_Year_d,
                              This_Year = This_Year_d)

#Create DF
write_csv(Years_Dataframe, "~/R Stuff/FantasyFootball 2.0/Years_Dataframe.csv")

###Names
player_names_func <- function(df){
  
  if("player" %in% colnames(df)){
  df$player <- str_replace_all(df$player, "[^[:alnum:]]", " ")
  df$player <- str_replace_all(df$player, "\\s+", " ")
  df$player <- str_replace_all(df$player, " III", " ")
  df$player <- str_replace_all(df$player, " II", " ")
  df$player <- str_replace_all(df$player, " Jr", " ")
  df$player <- trimws(df$player)
  
  }
  
  if("pos" %in% colnames(df)){
    df <- df %>% 
      mutate(pos = ifelse(player == "Taysom Hill", "TE", pos),
             pos = ifelse(pos == "FB", "RB", pos))
  }
  if("position" %in% colnames(df)){
    df <- df %>% 
      mutate(position = ifelse(player == "Taysom Hill", "TE", position),
             position = ifelse(position == "FB", "RB", position))
  }
  if("Pos" %in% colnames(df)){
    df <- df %>% 
      mutate(Pos = ifelse(player == "Taysom Hill", "TE", Pos),
             Pos = ifelse(Pos == "FB", "RB", Pos))
  }
  if("Position" %in% colnames(df)){
    df <- df %>% 
      mutate(Position = ifelse(player == "Taysom Hill", "TE", Position),
             Position = ifelse(Position == "FB", "RB", Position))
  }
  
  if("team" %in% colnames(df)){
    df$team[df$team == "GNB"] <- "GB"
    df$team[df$team == "JAX"] <- "JAC"
    df$team[df$team == "KAN"] <- "KC"
    df$team[df$team == "LVR"] <- "LV"
    df$team[df$team == "NWE"] <- "NE"
    df$team[df$team == "NOR"] <- "NO"
    df$team[df$team == "SFO"] <- "SF"
    df$team[df$team == "TAM"] <- "TB"
  }
  
  if("opp" %in% colnames(df)){
    df$opp[df$opp == "GNB"] <- "GB"
    df$opp[df$opp == "JAX"] <- "JAC"
    df$opp[df$opp == "KAN"] <- "KC"
    df$opp[df$opp == "LVR"] <- "LV"
    df$opp[df$opp == "NWE"] <- "NE"
    df$opp[df$opp == "NOR"] <- "NO"
    df$opp[df$opp == "SFO"] <- "SF"
    df$opp[df$opp == "TAM"] <- "TB"
  }
  
  df[df == "DJ Moore"] <- "D J Moore"
  df[df == "DJ Chark"] <- "D J Chark"
  df[df == "DK Metcalf"] <- "D K Metcalf"
  df[df == "PJ Walker"] <- "P J Walker"
  df[df == "Eli Mitchell"] <- "Elijah Mitchell"
  df[df == "Gabe Davis"] <- "Gabriel Davis"
  df[df == "Mitch Trubisky"] <- "Mitchell Trubisky"
  df[df == "Josh Palmer"] <- "Joshua Palmer"
  df[df == "Robbie Chosen"] <- "Robbie Anderson"
  df[df == "Michael Pittman Jr"] <- "Michael Pittman"
  df[df == "Ken Walker"] <- "Kenneth Walker"
  df[df == "Chigoziem Okonkwo"] <- "Chig Okonkwo"
  df[df == "Washington Football Team"] <- "Washington Commanders"
  
  return(df)
}

