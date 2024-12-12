


weekly_player_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/weekly_player_stats_2022.csv", sep = ""))) %>% 
  rename("player" = "Player")

week_1 <- weekly_player_stats %>% 
  filter(Week == 1)

#Names
player_names_func <- function(df){
  
  df$player <- str_replace_all(df$player, "[^[:alnum:]]", " ")
  df$player <- str_replace_all(df$player, "\\s+", " ")
  df$player <- str_replace_all(df$player, " II", " ")
  df$player <- str_replace_all(df$player, " III", " ")
  df$player <- str_replace_all(df$player, " Jr", " ")
  df$player <- trimws(df$player)
  
  df[df == "DJ Moore"] <- "D J Moore"
  df[df == "DK Metcalf"] <- "D K Metcalf"
  df[df == "Eli Mitchell"] <- "Elijah Mitchell"
  df[df == "Gabe Davis"] <- "Gabriel Davis"
  df[df == "Mitch Trubisky"] <- "Mitchell Trubisky"
  
  return(df)
}


Yahoo_Week_1 <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/Yahoo/", This_Year, "/Week_1_Yahoo.csv", sep = ""))) %>% 
  select(ID:Starting) %>% 
  mutate(player = paste(`First Name`, `Last Name`)) %>% 
  filter(!is.na(ID))

Yahoo_Week_1 <- player_names_func(Yahoo_Week_1)
week_1 <- player_names_func(week_1)


test <- full_join(Yahoo_Week_1, week_1, by = c("player")) %>% 
  filter(is.na(ID) | is.na(Week))


# profootball - yahoo
# D J Moore - DJ Moore
# D K Metcalf - DK Metcalf
# Elijah Mitchell - Eli Mitchell
# Gabriel Davis - Gabe Davis
# Mitchell Trusbisky - Mitch Trubisky





