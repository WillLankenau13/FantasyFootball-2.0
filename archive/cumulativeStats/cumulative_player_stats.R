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
d_qb_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/", This_Year, "/Passing_Weekly_Stats_", This_Year, ".csv", sep = "")))
d_non_qb_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/", This_Year, "/RushingReceiving_Weekly_Stats_", This_Year, ".csv", sep = "")))
d_blanks <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/", This_Year, "/Blanks_", This_Year, ".csv", sep = "")))


#fix draft
draft <- d_non_qb_stats %>% 
  select(Player, Draft) %>% 
  arrange(Draft) %>% 
  distinct(Player, .keep_all = TRUE)

blanks <- left_join(d_blanks, draft, by = c("Player"))

#rbind
non_qb_stats <- rbind(d_non_qb_stats, blanks)

#Select columns
qb_stats <- d_qb_stats %>% 
  select(Player, Week, Team:Result, Cmp:`Att...15`, `Yds...18`, TD:Int, Pos., Draft)

non_qb_stats <- non_qb_stats %>% 
  select(Player, Week, Team:Result, `Fmb...22`, `Att...27`, `Yds...28`, `TD...30`, `Tgt...32`:`Yds...34`, `TD...36`, Pos., Draft)

#Name Columns
qb_stats <- qb_stats %>% 
  rename("HA" = "...11" ) %>% 
  rename("Pas_Att" = "Att...15") %>% 
  rename("Pas_Yds" = "Yds...18") %>% 
  rename("Pas_Tds" = "TD") %>% 
  rename("Pos" = "Pos.") %>% 
  select(!Result)

non_qb_stats <- non_qb_stats %>% 
  rename("HA" = "...14") %>% 
  rename("Fmb" = "Fmb...22") %>% 
  rename("Rus_Att" = "Att...27") %>% 
  rename("Rus_Yds" = "Yds...28") %>% 
  rename("Rus_Tds" = "TD...30") %>% 
  rename("Tgt" = "Tgt...32") %>% 
  rename("Rec_Yds" = "Yds...34") %>% 
  rename("Rec_Tds" = "TD...36") %>% 
  rename("Pos" = "Pos.") %>% 
  select(Player, Week, Team, HA, Opp, Pos, Draft, Fmb:Rec_Tds)
  
#Join Data
player_stats <- full_join(qb_stats, non_qb_stats, by = c("Player", "Week", "Team","HA", "Opp", "Pos", "Draft"))

# #Remove Draft
# player_stats <- player_stats %>% 
#   select(!Draft)

#Home Away
player_stats <- player_stats %>% 
  mutate(HA = ifelse(is.na(HA), "Home", "Away"))

#Set NAs to 0
player_stats[, 6:9][is.na(player_stats[, 6:9])] <- 0
player_stats[, 13:20][is.na(player_stats[, 13:20])] <- 0

c <- 1

temp <- player_stats %>% 
  filter(Week <= c)

#week 1 cumulative stats
cumulative_player_stats <- temp %>% 
  group_by(Player, Team) %>% 
  summarize(week = c,
            games = n(),
            cum_cmp = sum(Cmp),
            cum_pas_att = sum(Pas_Att),
            cum_pas_yds = sum(Pas_Yds),
            cum_pas_tds = sum(Pas_Tds),
            cum_int = sum(Int),
            cum_fmb = sum(Fmb),
            cum_rus_att = sum(Rus_Att),
            cum_rus_yds = sum(Rus_Yds),
            cum_rus_tds = sum(Rus_Tds),
            cum_tgt = sum(Tgt),
            cum_rec = sum(Rec),
            cum_rec_yds = sum(Rec_Yds),
            cum_rec_tds = sum(Rec_Tds))

c <- c + 1

#cumulative stats
while(c <= 18){
  temp <- player_stats %>% 
    filter(Week <= c)
  
  temp_group <- temp %>% 
    group_by(Player, Team) %>% 
    summarize(week = c,
              games = n(),
              cum_cmp = sum(Cmp),
              cum_pas_att = sum(Pas_Att),
              cum_pas_yds = sum(Pas_Yds),
              cum_pas_tds = sum(Pas_Tds),
              cum_int = sum(Int),
              cum_fmb = sum(Fmb),
              cum_rus_att = sum(Rus_Att),
              cum_rus_yds = sum(Rus_Yds),
              cum_rus_tds = sum(Rus_Tds),
              cum_tgt = sum(Tgt),
              cum_rec = sum(Rec),
              cum_rec_yds = sum(Rec_Yds),
              cum_rec_tds = sum(Rec_Tds))
  
  cumulative_player_stats <- rbind(temp_group, cumulative_player_stats)
  
  c <- c + 1
}

#Player Details
player_details <- player_stats %>% 
  select(Player, Pos) %>% 
  distinct()

cumulative_player_stats <- cumulative_player_stats %>% 
  left_join(player_details, by = ("Player"))


#write csv
write_csv(cumulative_player_stats, eval(paste("~/R Stuff/FantasyFootball 2.0/cumulativeStats/cumulative_player_stats_", This_Year, ".csv", sep = "")))
write_csv(player_stats, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/weekly_player_stats_", This_Year, ".csv", sep = "")))








