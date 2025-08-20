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
d_off_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/Offensive_", This_Year, ".csv", sep = "")))

#select columns
off_stats <- d_off_stats %>% 
  select(Team, Opp, `...13`, Result, Week, `Att...38`, `Yds...39`, `TD...41`, `Cmp...16`, `Att...17`, `Yds...20`, `TD...21`, Int, `TO`)

#rename columns
off_stats <- off_stats %>% 
  rename("HA" = "...13")%>% 
  rename("Off_Rus_Att" = "Att...38") %>% 
  rename("Off_Rus_Yds" = "Yds...39") %>% 
  rename("Off_Rus_Tds" = "TD...41") %>% 
  rename("Off_Cmp" = "Cmp...16") %>% 
  rename("Off_Pas_Att" = "Att...17") %>% 
  rename("Off_Pas_Yds" = "Yds...20") %>% 
  rename("Off_Pas_Tds" = "TD...21") %>% 
  rename("Off_Int" = "Int") %>% 
  rename("Off_TO" = "TO")

#fumbles
off_stats <- off_stats %>% 
  mutate(Off_Fmb = Off_TO - Off_Int)

c <- 1

temp <- off_stats %>% 
  filter(Week <= c)

#week 1 cumulative stats
cumulative_off_stats <- temp %>% 
  group_by(Team) %>% 
  summarize(week = c,
            games = n(),
            cum_off_cmp = sum(Off_Cmp),
            cum_off_pas_att = sum(Off_Pas_Att),
            cum_off_pas_yds = sum(Off_Pas_Yds),
            cum_off_pas_tds = sum(Off_Pas_Tds),
            cum_off_int = sum(Off_Int),
            cum_off_fmb = sum(Off_Fmb),
            cum_off_rus_att = sum(Off_Rus_Att),
            cum_off_rus_yds = sum(Off_Rus_Yds),
            cum_off_rus_tds = sum(Off_Rus_Tds))

c <- c + 1

#cumulative stats
while(c <= 18){
  temp <- off_stats %>% 
    filter(Week <= c)
  
  temp_group <- temp %>% 
    group_by(Team) %>% 
    summarize(week = c,
              games = n(),
              cum_off_cmp = sum(Off_Cmp),
              cum_off_pas_att = sum(Off_Pas_Att),
              cum_off_pas_yds = sum(Off_Pas_Yds),
              cum_off_pas_tds = sum(Off_Pas_Tds),
              cum_off_int = sum(Off_Int),
              cum_off_fmb = sum(Off_Fmb),
              cum_off_rus_att = sum(Off_Rus_Att),
              cum_off_rus_yds = sum(Off_Rus_Yds),
              cum_off_rus_tds = sum(Off_Rus_Tds))
  
  cumulative_off_stats <- rbind(temp_group, cumulative_off_stats)
  
  c <- c + 1
}

#write csv
write_csv(cumulative_off_stats, eval(paste("~/R Stuff/FantasyFootball 2.0/cumulativeStats/cumulative_off_stats_", This_Year, ".csv", sep = "")))

