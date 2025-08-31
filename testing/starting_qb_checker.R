year <- 2022
a <- 2

df_list <- list()

while(a < 19){
  active_players <-  read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/activePlayers/", year, "/Week_", a, "_Active_Players.csv", sep = "")))
  starting_qbs <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/startingQBs/", year, "/Week_", a, "_Starting_QBs.csv", sep = "")))

  q <- starting_qbs %>% 
    left_join(active_players, by = c("QB" = "player")) %>% 
    filter(is.na(pos)) %>% 
    mutate(week = a) %>% 
    left_join(active_players, by = c("team.x" = "team")) %>% 
    filter(pos.y == "QB")
  
  df_list[[a]] <- q
  
  a <- a+1
}

j <- do.call(rbind, df_list)

k <- 9
active_players <-  read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/activePlayers/", year, "/Week_", k, "_Active_Players.csv", sep = ""))) %>% 
  filter(pos == "QB")



