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


#read files
combined_preds <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/combinedPredictions/", This_Year, "/combined_predictions_", This_Year, ".csv", sep = "")))
fantasy_pros_DST <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyProsFullSeasonPredictions/", This_Year, "/FantasyProsDST.csv", sep = ""))) %>% 
  clean_names() %>% 
  filter(!is.na(fpts)) %>% 
  mutate(pos = "DST",
         pos_rank = row_number()) %>% 
  select(player, team, pos, fpts, pos_rank)
fantasy_pros_K <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyProsFullSeasonPredictions/", This_Year, "/FantasyProsK.csv", sep = ""))) %>% 
  clean_names() %>% 
  filter(!is.na(fpts)) %>% 
  mutate(pos = "K",
         pos_rank = row_number()) %>% 
  select(player, team, pos, fpts, pos_rank)


#select cols
combined_preds <- combined_preds %>% 
  select(player, Team, Position, fpts_com)
colnames(combined_preds) <- c("player", "team", "pos", "fpts")

#pos rank
QB <- combined_preds %>% 
  filter(pos == "QB") %>% 
  arrange(desc(fpts)) %>% 
  mutate(pos_rank = row_number())
RB <- combined_preds %>% 
  filter(pos == "RB") %>% 
  arrange(desc(fpts)) %>% 
  mutate(pos_rank = row_number())
WR <- combined_preds %>% 
  filter(pos == "WR") %>% 
  arrange(desc(fpts)) %>% 
  mutate(pos_rank = row_number())
TE <- combined_preds %>% 
  filter(pos == "TE") %>% 
  arrange(desc(fpts)) %>% 
  mutate(pos_rank = row_number())
  
combined <- rbind(QB, RB, WR, TE, fantasy_pros_DST, fantasy_pros_K)


#fpts
replacement_levels <- data.frame(pos = c("QB", "RB", "WR", "TE", "DST", "K"),
                                 RL = c(200, 80, 100, 80, 120, 90))

combined <- full_join(combined, replacement_levels, by = c("pos")) %>% 
  select(player, team, pos, fpts, pos_rank, RL)

#write csv
write_csv(combined, eval(paste("~/R Stuff/FantasyFootball 2.0/salaryCap/csv", This_Year, ".csv", sep = "")))









