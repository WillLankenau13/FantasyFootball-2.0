week <- 3
year <- 2025

my_pred <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyPredictions/", year, "/Week_", week, "_Player_Predictions.csv", sep = "")))
QB_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", year, "/Week_", week, "/QB_Ratings.csv", sep = "")))
fpros_qb <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyPros/", year, "/FantasyPros_", year, "_Week_", week, "_QB_Rankings.csv", sep = "")))
fpros_rb <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyPros/", year, "/FantasyPros_", year, "_Week_", week, "_RB_Rankings.csv", sep = "")))
fpros_wr <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyPros/", year, "/FantasyPros_", year, "_Week_", week, "_WR_Rankings.csv", sep = "")))
fpros_te <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyPros/", year, "/FantasyPros_", year, "_Week_", week, "_TE_Rankings.csv", sep = "")))
def_team_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", year, "/Week_", week, "/Def_Team_ratings.csv", sep = "")))
player_percents <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", year, "/Week_", week, "/Player_Percents.csv", sep = "")))

fpros_pred <- rbind(fpros_qb, fpros_rb, fpros_wr, fpros_te)

fpros_pred <- fpros_pred %>% 
  select(`PLAYER NAME`, `TEAM`, `OPP`, `PROJ. FPTS`)

colnames(fpros_pred) <- c("player", "team", "opp", "fpros_pred")

fpros_pred <- player_names_func(fpros_pred) 

fpros_pred <- fpros_pred %>% 
  filter(fpros_pred != "-")

#combine
temp <- full_join(my_pred, fpros_pred, by = c("player")) %>% 
  select(player, pos, team.x, opponent, fpts_pred, fpros_pred, pas_att_pred:fl_pred)

#difference
temp$fpros_pred <- as.numeric(temp$fpros_pred)

temp <- temp %>% 
  mutate(difference = fpts_pred - fpros_pred)

###Real Data
d_past_week_player_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/", year, "/byWeek/Week_", week, "_Stats.csv", sep = ""))) %>%
  clean_names() %>%
  select(!date)

player_stats <- player_names_func(d_past_week_player_stats)

player_stats <- player_stats %>%
  mutate(fpts = yds_22*0.04 + td_23*4 + yds_38*0.1 + td_40*6 + yds_45*0.1 + td_47*6 + 0.5*rec - 1*int - 2*fmb_7) %>%
  select(player, yds_22, td_23, yds_38, td_40, yds_45, td_47, rec, int, fmb_7, fpts, off_percent_8)
colnames(player_stats) <- c("player", "pas_yds", "pas_tds", "rus_yds", "rus_tds", "rec_yds", "rec_tds", "rec", "int", "fmb", "fpts", "snap_per")

#ratings
temp <- temp %>% 
  full_join(QB_ratings, by = "player")

#player percents
temp <- temp %>% 
  full_join(player_percents, by = "player")

temp <- temp %>%
  full_join(player_stats, by = c("player"))

s <- temp %>% 
  select(player, pos.y, team.x, fpts_pred, fpts, fpros_pred) %>% 
  filter(is.na(fpts_pred))


#
data <- temp %>%
  filter(!is.na(fpts)) %>%
  filter(!is.na(fpts_pred)) %>%
  filter(!is.na(fpros_pred)) %>% 
  filter(fpros_pred > 5) %>% 
  mutate(my_resid = (fpts - fpts_pred),
         fpros_resid = (fpts - fpros_pred),
         my_r_sq = my_resid^2,
         fpros_r_sq = fpros_resid^2,
         abs_my_resid = abs(my_resid),
         abs_fpros_resid = abs(fpros_resid)) %>% 
  mutate(pos = pos.x) 

median(data$abs_fpros_resid)
median(data$abs_my_resid)

mean(data$abs_fpros_resid)
mean(data$abs_my_resid)

median(data$fpros_resid)
median(data$my_resid)

sum(data$fpros_r_sq)/nrow(data) 
sum(data$my_r_sq)/nrow(data)

qb <- data %>%
  filter(pos == "QB") 

sum(qb$fpros_r_sq)/nrow(qb)
sum(qb$my_r_sq)/nrow(qb) 

rb <- data %>%
  filter(pos == "RB")

sum(rb$fpros_r_sq)/nrow(rb)
sum(rb$my_r_sq)/nrow(rb) 

wr <- data %>%
  filter(pos == "WR")

sum(wr$fpros_r_sq)/nrow(wr)
sum(wr$my_r_sq)/nrow(wr) 

te <- data %>%
  filter(pos == "TE")

sum(te$fpros_r_sq)/nrow(te)
sum(te$my_r_sq)/nrow(te) 

