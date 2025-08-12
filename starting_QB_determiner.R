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

#week
which_week <- 18

#Years
Years_Dataframe <- read_csv("~/R Stuff/FantasyFootball 2.0/Years_Dataframe.csv")
Past_Year <- Years_Dataframe$Past_Year[1]
This_Year <- Years_Dataframe$This_Year[1]

#files
passing_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/", This_Year, "/weekly_passing_stats.csv", sep = ""))) %>%
  clean_names()
fpros_qb <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Online_Models/Week_", which_week, "/FantasyPros_2022_Week_", which_week, "_QB_Rankings.csv", sep = ""))) %>% 
  clean_names %>% 
  rename("player" = "player_name")

#
data <- player_names_func(fpros_qb)


# data <- data %>% 
#   filter(player != "Jeff Driskel") %>% 
#   filter(player != "Taylor Heinicke")

###
data <- slice_max(data, proj_fpts, by = team)

starting_qbs <- data %>% 
  select(team, player)

colnames(starting_qbs) <- c("team", "QB")

write_csv(starting_qbs, eval(paste("~/R Stuff/FantasyFootball 2.0/startingQBs/", This_Year, "/Week_", which_week, "_Starting_QBs.csv", sep = "")))









