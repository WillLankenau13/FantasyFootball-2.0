
year <- 2023

df_list <- list()

c <- 1

while(c < 19){
  
  week <- c
  
  past_week_player_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/", year, "/byWeek/Week_", week, "_Stats.csv", sep = ""))) %>%
    clean_names() %>%
    select(!date)
  past_week_player_stats <- player_names_func(past_week_player_stats)
  
  
  yahoo <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/Yahoo/", year, "/Yahoo_Week_", week, ".csv", sep = ""))) %>% 
    select(ID:Starting) %>% 
    mutate(player = paste(`First Name`, `Last Name`)) %>% 
    filter(!is.na(ID))
  
  yahoo <- player_names_func(yahoo)
  
  yahoo <- yahoo %>% 
    left_join(teams, by = c("Team" = "Yahoo")) %>% 
    select(ID:Position, Short_Name, Opponent:player) %>% 
    rename("Team" = "Short_Name") %>% 
    left_join(teams, by = c("Opponent" = "Yahoo")) %>% 
    select(ID:Position, Team, Short_Name, Game:player) %>% 
    rename("Opponent" = "Short_Name") 
  
  temp <- full_join(yahoo, past_week_player_stats, by = c("player")) %>% 
    mutate(week = c)
  
  df_list[[c]] <- temp
  
  c <- c+1
}

combined <- do.call(rbind, df_list)

combined <- combined %>% 
  filter(!is.na(`Injury Status`)) %>% 
  filter(!is.na(rk)) %>% 
  filter(`Injury Status` != "Q") %>% 
  filter(`Injury Status` != "D") %>% 
  select(player, week, team, `Injury Status`)


