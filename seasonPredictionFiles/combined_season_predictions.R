


my_season_player_predictions <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonPredictions/", This_Year, "/Player_Predictions.csv", sep = "")))
online_season_player_predictions_QB <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyPros/fantasyProsFullSeasonPredictions/", This_Year, "/FantasyPros_Fantasy_Football_Projections_QB.csv", sep = ""))) %>% 
  clean_names()
online_season_player_predictions_RB <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyPros/fantasyProsFullSeasonPredictions/", This_Year, "/FantasyPros_Fantasy_Football_Projections_RB.csv", sep = ""))) %>% 
  clean_names()
online_season_player_predictions_WR <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyPros/fantasyProsFullSeasonPredictions/", This_Year, "/FantasyPros_Fantasy_Football_Projections_WR.csv", sep = ""))) %>% 
  clean_names()
online_season_player_predictions_TE <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyPros/fantasyProsFullSeasonPredictions/", This_Year, "/FantasyPros_Fantasy_Football_Projections_TE.csv", sep = ""))) %>% 
  clean_names()

adp <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/adp/FantasyPros_", This_Year, "_Overall_ADP_Rankings.csv", sep = ""))) %>% 
  clean_names() %>% 
  rename("pos_rank" = "pos")
adp <- player_names_func(adp)


#colnames
colnames(online_season_player_predictions_QB) <- c("player", "team", "pas_att_fpros", "cmp_fpros", "pas_yds_fpros", "pas_tds_fpros", "int_fpros", "rus_att_fpros", "rus_yds_fpros", "rus_tds_fpros", "fl_fpros", "fpts_fpros")
colnames(online_season_player_predictions_RB) <- c("player", "team", "rus_att_fpros", "rus_yds_fpros", "rus_tds_fpros", "rec_fpros", "rec_yds_fpros", "rec_tds_fpros", "fl_fpros", "fpts_fpros")
colnames(online_season_player_predictions_WR) <- c("player", "team", "rec_fpros", "rec_yds_fpros", "rec_tds_fpros", "rus_att_fpros", "rus_yds_fpros", "rus_tds_fpros", "fl_fpros", "fpts_fpros")
colnames(online_season_player_predictions_TE) <- c("player", "team", "rec_fpros", "rec_yds_fpros", "rec_tds_fpros", "fl_fpros", "fpts_fpros")

#bind
online_season_player_predictions <- bind_rows(online_season_player_predictions_QB, online_season_player_predictions_RB, online_season_player_predictions_WR, online_season_player_predictions_TE)

#player_names_func
online_season_player_predictions <- player_names_func(online_season_player_predictions)

online_season_player_predictions[is.na(online_season_player_predictions)] <- 0


#join
combined_player_predictions <- full_join(my_season_player_predictions, online_season_player_predictions, by = c("player","team"))

combine_o_and_my_predictions <- function(df, cols){
  for (col in cols) {
    
    # combine
    df[, paste0("com_", col, "_pred")] <- (df[, paste0(col, "_pred")] + df[, paste0(col, "_fpros")]*5)/6
  }
  return(df)
}

# define the columns once
cols <- c("pas_att", "cmp", "pas_yds", "pas_tds", "int",
          "rus_att", "rus_yds", "rus_tds", "rec", "rec_yds", "rec_tds")

combined_player_predictions <- combine_o_and_my_predictions(combined_player_predictions, cols)

#combined fpts pred
combined_player_predictions <- combined_player_predictions %>% 
  mutate(fpts_fpros = pas_yds_fpros*0.04 + pas_tds_fpros*4 + rus_yds_fpros*0.1 + rus_tds_fpros*6 + rec_yds_fpros*0.1 + rec_tds_fpros*6 + 0.5*rec_fpros - 1*int_fpros,
    com_fpts_pred = com_pas_yds_pred*0.04 + com_pas_tds_pred*4 + com_rus_yds_pred*0.1 + com_rus_tds_pred*6 + com_rec_yds_pred*0.1 + com_rec_tds_pred*6 + 0.5*com_rec_pred - 1*com_int_pred)


#join with adp
combined_player_predictions <- combined_player_predictions %>% 
  full_join(adp, by = c("player", "team")) 

combined_player_predictions$adp_pos <- gsub("[^A-Za-z]", "", combined_player_predictions$pos_rank)

combined_player_predictions <- combined_player_predictions %>% 
  filter(adp_pos != "DST") %>% 
  filter(adp_pos != "K")

flex <- combined_player_predictions %>% 
  filter(pos != "QB")

#for draft
for_draft <- combined_player_predictions %>% 
  select(player, pos, team, fpts_pred, fpts_fpros, com_fpts_pred, rank, bye, pos_rank) %>% 
  arrange(rank) %>%
  mutate(pos_rank = as.numeric(gsub("[^0-9]", "", pos_rank)))

#write csv
write_csv(combined_player_predictions, eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonPredictions/Combined/", This_Year, "/Combined_Player_Predictions.csv", sep = "")))
write_csv(for_draft, eval(paste("~/R Stuff/FantasyFootball 2.0/salaryCap/for_draft_", This_Year, ".csv", sep = "")))




