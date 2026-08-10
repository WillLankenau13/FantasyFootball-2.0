
year <- 2024
a <- 1

while(a < 19){
  #player stats
  d_past_week_player_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/", year, "/byWeek/Week_", a, "_Stats.csv", sep = ""))) %>% 
    clean_names()
  d_past_week_st_snaps <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/", year, "/byWeek/Week_", a, "_ST_Snaps.csv", sep = ""))) %>% 
    clean_names()
  
  past_week_player_stats <- d_past_week_player_stats %>% 
    select(player, pos, team, off_snp)
  past_week_st_snaps <- d_past_week_st_snaps
  names(past_week_st_snaps)[names(past_week_st_snaps) == "st_snp_17"] <- "st_snp"
  past_week_st_snaps <- past_week_st_snaps %>% 
    select(player, pos, team, st_snp)
  
  #player names func
  past_week_player_stats <- player_names_func(past_week_player_stats)
  past_week_st_snaps <- player_names_func(past_week_st_snaps)
  
  #add special teams
  past_week_player_snaps <- past_week_player_stats %>% 
    full_join(past_week_st_snaps, by = c("player", "pos", "team"))
  
  #NA vals to 0
  past_week_player_snaps[is.na(past_week_player_snaps)] <- 0
  
  #only with snaps
  active_players <- past_week_player_snaps %>% 
    filter(off_snp > 0 | st_snp > 0) %>% 
    select(player, team, pos)
  
  write_csv(active_players, eval(paste("~/R Stuff/FantasyFootball 2.0/activePlayers/", year, "/Week_", a, "_Active_Players.csv", sep = "")))
  
  
  a <- a+1
}





