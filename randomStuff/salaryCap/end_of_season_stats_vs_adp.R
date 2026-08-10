

qb_data <- read_csv("~/R Stuff/FantasyFootball 2.0/SalaryCap/FantasyPros_Fantasy_Football_Statistics_QB.csv") %>% 
  clean_names() %>% 
  rename("l8_rank" = "rank") %>% 
  select(player, fpts_g, l8_rank)
rb_data <- read_csv("~/R Stuff/FantasyFootball 2.0/SalaryCap/FantasyPros_Fantasy_Football_Statistics_RB.csv") %>% 
  clean_names() %>% 
  rename("l8_rank" = "rank") %>% 
  select(player, fpts_g, l8_rank)
wr_data <- read_csv("~/R Stuff/FantasyFootball 2.0/SalaryCap/FantasyPros_Fantasy_Football_Statistics_WR.csv") %>% 
  clean_names() %>% 
  rename("l8_rank" = "rank") %>% 
  select(player, fpts_g, l8_rank)
te_data <- read_csv("~/R Stuff/FantasyFootball 2.0/SalaryCap/FantasyPros_Fantasy_Football_Statistics_TE.csv") %>% 
  clean_names() %>% 
  rename("l8_rank" = "rank") %>% 
  select(player, fpts_g, l8_rank)

adp <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/adp/FantasyPros_", This_Year, "_Overall_ADP_Rankings.csv", sep = ""))) %>% 
  clean_names() %>% 
  rename("pos_rank" = "pos") %>% 
  select(player, rank, avg, pos_rank)



last_stats <- rbind(qb_data, rb_data, wr_data, te_data)
last_stats$player <- gsub("\\s*\\([^)]*\\)", "", last_stats$player)
last_stats <- player_names_func(last_stats)


adp <- player_names_func(adp)


data <- adp %>% 
  left_join(last_stats, by = c("player"))

