library("tidyverse")


#Year and Week
upcoming_week <- 4
This_Year <- This_Year_d

#Read Fantasy Pros Data
FP_QB <- read_csv((eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyPros/", This_Year, "/FantasyPros_", This_Year, "_Week_", upcoming_week, "_QB_Rankings.csv", sep = "")))) %>% 
  clean_names() %>% 
  select(player_name:opp, proj_fpts) %>% 
  mutate(pos = "QB")
FP_RB <- read_csv((eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyPros/", This_Year, "/FantasyPros_", This_Year, "_Week_", upcoming_week, "_RB_Rankings.csv", sep = "")))) %>% 
  clean_names() %>% 
  select(player_name:opp, proj_fpts) %>% 
  mutate(pos = "RB")
FP_WR <- read_csv((eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyPros/", This_Year, "/FantasyPros_", This_Year, "_Week_", upcoming_week, "_WR_Rankings.csv", sep = "")))) %>% 
  clean_names() %>% 
  select(player_name:opp, proj_fpts) %>% 
  mutate(pos = "WR")
FP_TE <- read_csv((eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyPros/", This_Year, "/FantasyPros_", This_Year, "_Week_", upcoming_week, "_TE_Rankings.csv", sep = "")))) %>% 
  clean_names() %>% 
  select(player_name:opp, proj_fpts) %>% 
  mutate(pos = "TE")
FP_DST <- read_csv((eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyPros/", This_Year, "/FantasyPros_", This_Year, "_Week_", upcoming_week, "_DST_Rankings.csv", sep = "")))) %>% 
  clean_names() %>% 
  select(player_name:opp, proj_fpts) %>% 
  mutate(pos = "DST")

#My Projections
player_predictions <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyPredictions/", This_Year, "/Week_", upcoming_week, "_Player_Predictions.csv", sep = "")))

#Read Yahoo Data
Yahoo <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/Yahoo/", This_Year, "/Yahoo_Week_", upcoming_week, ".csv", sep = ""))) %>% 
  select(ID:Starting) %>% 
  mutate(player = paste(`First Name`, `Last Name`))

###Player Names Function
#Tidy the Yahoo Data
Yahoo <- Yahoo[complete.cases(Yahoo[,c("Last Name")]),]

Yahoo <- player_names_func(Yahoo) #player names func


#Tidy Fantasy Pros data
FP <- rbind(FP_QB, FP_RB, FP_WR, FP_TE, FP_DST)

FP <- FP %>% 
  rename("player" = "player_name") %>% 
  clean_names()

FP$opp <- gsub("\\bat\\b", "", FP$opp)      # remove "at"
FP$opp <- gsub("\\bvs\\.\\b", "", FP$opp)  # remove "vs."
FP$opp <- trimws(FP$opp)   

FP <- player_names_func(FP) #player names func


#Combine with my projections
combined_player_predictions <- full_join(player_predictions, FP, by = c("player", "team")) %>% 
  select(player, pos.x, pos.y, team, opponent, opp, fpts_pred, proj_fpts) %>% 
  mutate(pos = coalesce(pos.x, pos.y),
         opp = coalesce(opponent, opp)) %>% 
  select(player, pos, team, opp, fpts_pred, proj_fpts) %>% 
  filter(team != "FA")

combined_player_predictions[is.na(combined_player_predictions)] <- 0

combined_player_predictions <- combined_player_predictions %>% 
  mutate(dif = fpts_pred - proj_fpts,
         fpts = ifelse(pos == "DST", proj_fpts, 0.7*proj_fpts + 0.3*fpts_pred))

t <- combined_player_predictions %>% 
  filter(pos != "DST") %>% 
  group_by(pos) %>%
  mutate(my_rank = row_number(desc(fpts_pred)),
         fpros_rank = row_number(desc(proj_fpts))) %>%
  ungroup()

#Join yahoo and fantasy pros data
combined <- full_join(combined_player_predictions, Yahoo, by = c("player"))


#Tidy the combined data
 combined <- combined %>% 
  select(player, pos, team, opp, Salary, fpts) %>% 
  mutate(ppd = fpts/Salary) %>% 
  filter(!is.na(ppd))

#round points per dollar to 4 decimals
combined$ppd <- round(combined$ppd, 4)

#rename col names
colnames(combined) <- c("Player", "Pos", "Team", "Opp", "Salary", "FPTS", "ppd")


#Points Above Replacement
PAR <- function(m_sal, position){ 
  
  if(position == "Flex"){
    df <- combined %>% 
      filter(Pos == "RB" | Pos == "WR")
  } else {
    df <- combined %>% 
      filter(Pos == position)
  }

  rep_player <- df %>% 
    filter(Salary == m_sal)
  
  rep_pts <- max(rep_player$FPTS)
  
  df <- df %>% 
    mutate(min_sal = m_sal,
           rep_points = rep_pts)
  
  return(df)
}

#get points above replacement
n_combined <- rbind(PAR(20, "QB"),
                  PAR(10, "Flex"),
                  PAR(10, "TE"),
                  PAR(10, "DST"))

#Points above replacement per dollar
n_combined <- n_combined %>% 
  mutate(PAR_PD = ifelse(Salary > min_sal, (FPTS - rep_points)/(Salary - min_sal), 0))


#Write
write_csv(n_combined, (eval(paste("~/R Stuff/FantasyFootball 2.0/combinedWeeklyPredictions/", This_Year, "/Week_", upcoming_week, ".csv", sep = ""))))



