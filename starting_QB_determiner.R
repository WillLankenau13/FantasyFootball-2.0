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
This_Year <- 2022

#files
fpros_qb <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyPros/", This_Year, "/FantasyPros_", This_Year, "_Week_", which_week, "_QB_Rankings.csv", sep = ""))) %>% 
  clean_names %>% 
  rename("player" = "player_name")

#
data <- player_names_func(fpros_qb)

second_top_per_team <- data %>%
  group_by(team) %>%
  slice_max(order_by = proj_fpts, n = 2, with_ties = FALSE) %>%  # get top 2
  arrange(team, desc(proj_fpts)) %>%  # highest first
  slice(2) %>%  # take the second row = second-highest
  select(team, player, proj_fpts) %>%
  ungroup()

max(second_top_per_team$proj_fpts)


# data <- data %>%
#   filter(player != "Anthony Richardson")

###
top <- slice_max(data, proj_fpts, by = team)

starting_qbs <- top %>% 
  select(team, player)

colnames(starting_qbs) <- c("team", "QB")

write_csv(starting_qbs, eval(paste("~/R Stuff/FantasyFootball 2.0/startingQBs/", This_Year, "/Week_", which_week, "_Starting_QBs.csv", sep = "")))


max(second_top_per_team$proj_fpts)






