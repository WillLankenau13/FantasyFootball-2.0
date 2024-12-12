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


cumulative_off_stats <- read_csv("~/R Stuff/FantasyFootball 2.0/cumulativeStats/cumulative_off_stats_2022.csv")
cumulative_def_stats <- read_csv("~/R Stuff/FantasyFootball 2.0/cumulativeStats/cumulative_def_stats_2022.csv")
weekly_off_stats <- read_csv("~/R Stuff/FantasyFootball 2.0/weeklyStats/2022/Offensive_2022.csv")

#select weekly stats
weekly_stats <- weekly_off_stats %>% 
  select(Team, Opp, Week, Att...38) %>% 
  rename("week" = "Week")

#cumulative off stats
cumulative_off_stats <- cumulative_off_stats %>% 
  mutate(off_rus_att = cum_off_rus_att/games,
         week = week + 1) %>%  
  select(Team, week, off_rus_att) 

#cumulative def stats
cumulative_def_stats <- cumulative_def_stats %>% 
  mutate(def_rus_att = cum_def_rus_att/games,
         week = week + 1) %>% 
  select(Team, week, def_rus_att)


weekly_stats <- weekly_stats %>% 
  left_join(cumulative_off_stats, by = c("Team", "week")) %>% 
  left_join(cumulative_def_stats, by = c("Opp" = "Team", "week")) %>% 
  filter(week > 5)

testing <- weekly_stats %>% 
  rename("actual" = "Att...38")

r_mean <- mean(testing$actual)


coef <- 0.6

testing <- testing %>%
  mutate(pred = coef*off_rus_att + (1 - coef)*def_rus_att,
         resid = pred - actual, 
         sq_resid = resid^2,
         var = (actual - r_mean)^2) 

r_squared = 1 - sum(testing$sq_resid)/sum(testing$var)
r_squared


pas_yds_off_coef <- 0.7
cmp_off_coef <- 0.7
pas_tds_off_coef <- 0.7
rus_yds_off_coef <- 0.5
rus_att_off_coef <- 0.6


