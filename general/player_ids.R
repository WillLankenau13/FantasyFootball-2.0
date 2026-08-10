
teams <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/teams.csv", sep = "")))
year <- 2014
roster_list <- list()
yahoo_list <- list()

while(year <= 2021){
  temp <- fast_scraper_roster(year) %>% 
    rename("player" = "full_name",
           "pos" = "position") %>% 
    select(player, pos, team, gsis_id, yahoo_id, season)
  
  roster_list[[year-2013]] <- temp
  
  year <- year+1
  
}

week <- 1

while(year <= 2025){
  temp <- fast_scraper_roster(year) %>% 
    rename("player" = "full_name",
           "pos" = "position") %>% 
    select(player, pos, team, gsis_id, yahoo_id, season)
  
  roster_list[[year-2013]] <- temp
  
  while((week <= 18 & year <= 2024) | (week <= 14 & year == 2025)){
    #yahoo
    yahoo <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/Yahoo/", year, "/Yahoo_Week_", week, ".csv", sep = ""))) %>% 
      select(ID:Starting) %>% 
      mutate(player = paste(`First Name`, `Last Name`)) %>% 
      filter(!is.na(ID)) %>% 
      rename("pos" = "Position")
    
    yahoo <- player_names_func(yahoo)
    
    yahoo <- yahoo %>% 
      left_join(teams, by = c("Team" = "Yahoo")) %>% 
      select(ID:pos, Short_Name, Opponent:player) %>% 
      rename("team" = "Short_Name") %>% 
      left_join(teams, by = c("Opponent" = "Yahoo")) %>% 
      select(ID:pos, team, Short_Name, Game:player) %>% 
      rename("Opponent" = "Short_Name") %>% 
      filter(pos != "DEF") %>% 
      mutate(season = year)
    
    yahoo_list[[(year-2022)*18 + week]] <- yahoo
    week <- week+1
  }
  
  week <- 1
  year <- year+1
}

ids <- do.call(rbind, roster_list) 

ids <- ids %>% 
  filter(pos %in% c("QB", "RB", "WR", "TE"))

ids <- ids[!duplicated(ids), ]

ids <- player_names_func(ids)


yahoo <- do.call(rbind, yahoo_list)

yahoo <- yahoo %>% 
  mutate(yahoo_id = sub(".*\\.(\\d+)\\$.*", "\\1", ID)) %>% 
  select(yahoo_id, player, pos, team, season) %>% 
  distinct()


players <- inner_join(ids, yahoo, by = c("yahoo_id", "season")) %>% 
  rename("player" = "player.y") %>% 
  select(player, gsis_id, yahoo_id) %>% 
  distinct()

l_ids <- ids %>% 
  filter(!(gsis_id %in% players$gsis_id))
l_yahoo <- yahoo %>% 
  filter(!(yahoo_id %in% players$yahoo_id))


players2 <- inner_join(l_ids, l_yahoo, by = c("player", "team", "pos", "season")) %>% 
  rename("yahoo_id" = "yahoo_id.y") %>% 
  select(player, gsis_id, yahoo_id) %>% 
  distinct()

l_ids <- l_ids %>% 
  filter(!(gsis_id %in% players2$gsis_id))
l_yahoo <- l_yahoo %>% 
  filter(!(yahoo_id %in% players2$yahoo_id))


players3 <- inner_join(l_ids, l_yahoo, by = c("player", "team", "season")) %>% 
  rename("yahoo_id" = "yahoo_id.y") %>% 
  select(player, gsis_id, yahoo_id) %>% 
  distinct()

l_ids <- l_ids %>% 
  filter(!(gsis_id %in% players3$gsis_id))
l_yahoo <- l_yahoo %>% 
  filter(!(yahoo_id %in% players3$yahoo_id))


players4 <- inner_join(l_ids, l_yahoo, by = c("player", "pos", "season")) %>% 
  rename("yahoo_id" = "yahoo_id.y") %>% 
  select(player, gsis_id, yahoo_id) %>% 
  distinct()

l_ids <- l_ids %>% 
  filter(!(gsis_id %in% players4$gsis_id))
l_yahoo <- l_yahoo %>% 
  filter(!(yahoo_id %in% players4$yahoo_id))


player_ids <- rbind(players, players2, players3, players4)



#write_csv
write_csv(player_ids, eval(paste("~/R Stuff/FantasyFootball 2.0/player_ids.csv", sep = "")))





