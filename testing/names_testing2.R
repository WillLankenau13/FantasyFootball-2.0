

players_2023 <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonStats/2023/Fantasy_2023.csv", sep = ""))) %>% 
  clean_names()

players_2023 <- player_names_func(players_2023)

###testing 
t <- Yahoo_Week_1 %>% 
  full_join(d_Player_percents_past_year, by = c("player")) %>% 
  left_join(draft, by = c("player")) %>% 
  left_join(players_2023, by = c("player")) %>% 
  filter(is.na(Salary) | (is.na(player_games) & is.na(Rnd))) %>% 
  filter(Position != "DEF" | is.na(Position)) %>% 
  filter(Position != "QB" | is.na(Position)) %>% 
  select(player, Position, Salary, team, Rnd, tm:fant_pt)

tt <- t %>% 
  filter(player == "C J Stroud")