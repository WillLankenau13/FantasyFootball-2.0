



#Year and Week
upcoming_week <- 6
This_Year <- This_Year_d

betting_values <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/combinedWeeklyPredictions/", This_Year, "/Week_", upcoming_week, "_With_Betting_Odds.csv", sep = ""))) %>% 
  clean_names()
player_predictions <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyPredictions/", This_Year, "/Week_", upcoming_week, "_Player_Predictions.csv", sep = "")))


data <- betting_values %>% 
  left_join(player_predictions, by = c("player", "pos", "team", "opp" = "opponent")) %>% 
  select(player:opp, pas_yds:pred_fpts, pas_att_pred:rus_tds_pred, fpts_pred) %>% 
  rename("com_fpts" = "pred_fpts",
         "my_fpts" = "fpts_pred")

QB <- data %>% 
  filter(pos == "QB") %>% 
  mutate(dif_pas_yds = pas_yds_pred - pas_yds,
         dif_pas_tds = pas_tds_pred - pas_td,
         dif_rus_yds = rus_yds_pred - yds,
         dif_rus_tds = rus_tds_pred - pred_t_ds) %>% 
  select(player:opp, com_fpts, my_fpts, odds_fpts , pas_yds, pas_tds, yds, t_ds, pred_t_ds, pas_yds_pred, pas_tds_pred, rus_yds_pred, rus_tds_pred, dif_pas_yds:dif_rus_tds)

RB <- data %>% 
  filter(pos == "RB") %>% 
  mutate(dif_tot_yds = rus_yds_pred + rec_yds_pred - yds,
         dif_rec = rec_pred - rec,
         dif_tds = rus_tds_pred + rec_tds_pred - pred_t_ds,
         tot_td_pred = rus_tds_pred + rec_tds_pred) %>% 
  select(player:opp, com_fpts, my_fpts, odds_fpts,yds, t_ds, pred_t_ds, rus_yds_pred, rus_tds_pred, rec_pred, rec_yds_pred, rec_tds_pred, tot_td_pred, dif_tot_yds:dif_tds)

WR <- data %>% 
  filter(pos == "WR") %>% 
  mutate(dif_rec_yds = rec_yds_pred - yds,
         dif_rec = rec_pred - rec,
         dif_tds = rec_tds_pred - pred_t_ds) %>% 
  select(player:opp, com_fpts, my_fpts, odds_fpts,yds, t_ds, pred_t_ds, rec_pred, rec_yds_pred, rec_tds_pred, dif_rec_yds:dif_tds)

TE <- data %>% 
  filter(pos == "TE") %>% 
  mutate(dif_rec_yds = rec_yds_pred - yds,
         dif_rec = rec_pred - rec,
         dif_tds = rec_tds_pred - pred_t_ds) %>% 
  select(player:opp, com_fpts, my_fpts, odds_fpts,yds, t_ds, pred_t_ds, rec_pred, rec_yds_pred, rec_tds_pred, dif_rec_yds:dif_tds)

