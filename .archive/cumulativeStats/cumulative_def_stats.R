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
d_def_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/Defensive_", This_Year, ".csv", sep = "")))

#select columns
def_stats <- d_def_stats %>% 
  select(Team, Opp, `...12`, Result, Week, `Att...15`, `Yds...16`, `TD...18`, `Cmp...19`, `Att...20`, `Yds...22`, `TD...23`, Int, `TO...29`)

#rename columns
def_stats <- def_stats %>% 
  rename("HA" = "...12")%>% 
  rename("Def_Rus_Att" = "Att...15") %>% 
  rename("Def_Rus_Yds" = "Yds...16") %>% 
  rename("Def_Rus_Tds" = "TD...18") %>% 
  rename("Def_Cmp" = "Cmp...19") %>% 
  rename("Def_Pas_Att" = "Att...20") %>% 
  rename("Def_Pas_Yds" = "Yds...22") %>% 
  rename("Def_Pas_Tds" = "TD...23") %>% 
  rename("Def_Int" = "Int") %>% 
  rename("Def_TO" = "TO...29")

#fumbles
def_stats <- def_stats %>% 
  mutate(Def_Fmb = Def_TO - Def_Int)

c <- 1

temp <- def_stats %>% 
  filter(Week <= c)

#week 1 cumulative stats
cumulative_def_stats <- temp %>% 
  group_by(Team) %>% 
  summarize(week = c,
            games = n(),
            cum_def_cmp = sum(Def_Cmp),
            cum_def_pas_att = sum(Def_Pas_Att),
            cum_def_pas_yds = sum(Def_Pas_Yds),
            cum_def_pas_tds = sum(Def_Pas_Tds),
            cum_def_int = sum(Def_Int),
            cum_def_fmb = sum(Def_Fmb),
            cum_def_rus_att = sum(Def_Rus_Att),
            cum_def_rus_yds = sum(Def_Rus_Yds),
            cum_def_rus_tds = sum(Def_Rus_Tds))

c <- c + 1

#cumulative stats
while(c <= 18){
  temp <- def_stats %>% 
    filter(Week <= c)
  
  temp_group <- temp %>% 
    group_by(Team) %>% 
    summarize(week = c,
              games = n(),
              cum_def_cmp = sum(Def_Cmp),
              cum_def_pas_att = sum(Def_Pas_Att),
              cum_def_pas_yds = sum(Def_Pas_Yds),
              cum_def_pas_tds = sum(Def_Pas_Tds),
              cum_def_int = sum(Def_Int),
              cum_def_fmb = sum(Def_Fmb),
              cum_def_rus_att = sum(Def_Rus_Att),
              cum_def_rus_yds = sum(Def_Rus_Yds),
              cum_def_rus_tds = sum(Def_Rus_Tds))
  
  cumulative_def_stats <- rbind(temp_group, cumulative_def_stats)
  
  c <- c + 1
}

#write csv
write_csv(cumulative_def_stats, eval(paste("~/R Stuff/FantasyFootball 2.0/cumulativeStats/cumulative_def_stats_", This_Year, ".csv")))

