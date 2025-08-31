

df_list <- list()
year <- 2022


c <- 3

while(c < 19){
  
  week <- c
  
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
  
  #defensive
  temp <- temp %>% 
    left_join(def_team_ratings, by = c("opponent" = "team"))
  
  temp <- temp %>%
    full_join(player_stats, by = c("player")) %>%
    filter(!is.na(fpts)) %>%
    filter(!is.na(fpts_pred)) %>%
    filter(!is.na(fpros_pred)) %>% 
    mutate(week = c)
  
  df_list[[c]] <- temp
  
  c <- c+1
}

combined <- do.call(rbind, df_list)

data <- combined %>% 
  filter(fpts > 0) %>%
  filter(fpros_pred > 7) %>% 
  mutate(my_resid = (fpts - fpts_pred),
         fpros_resid = (fpts - fpros_pred),
         my_r_sq = my_resid^2,
         fpros_r_sq = fpros_resid^2) %>%
  filter(!is.na(fpts)) %>%
  filter(!is.na(fpts_pred)) %>%
  filter(!is.na(fpros_pred)) %>% 
  mutate(pos = pos.x)

median(data$fpros_resid)
median(data$my_resid)

sum(data$my_r_sq)
sum(data$fpros_r_sq)

# sum(data$pas_yds)
# sum(data$rec_yds)
# sum(data$pas_yds_pred)
# sum(data$rec_yds_pred)
# median(wr$rec_yds_resid)

qb <- data %>%
  filter(pos == "QB") 

qb <- qb %>% 
  mutate(pas_yds_resid = pas_yds - pas_yds_pred,
         pas_tds_resid = pas_tds - pas_tds_pred,
         rus_yds_resid = rus_yds - rus_yds_pred,
         rus_tds_resid = rus_tds - rus_tds_pred,
         int_resid = int - int_pred)
median(qb$pas_yds_resid)*0.04
median(qb$pas_tds_resid)*4
median(qb$rus_yds_resid)*0.1
median(qb$rus_tds_resid)*6
median(qb$int_resid)*(-1)

median(qb$pas_yds_resid)
mean(qb$pas_yds_resid)
#0.7 off pas yds -12.35
#1 off pasyds -14.57

qb2 <- qb %>% 
  filter(week == 12)
median(qb2$pas_yds_resid)

ind <- qb %>% 
  filter(player == "Aaron Rodgers") %>% 
  select(player, week, opponent, pas_yds_rat, def_pas_yds_rat, pas_yds_pred, pas_yds, pas_yds_resid)

qb3 <- qb %>% 
  select(player, week, opponent, pas_yds_resid, snap_per)


wr <- data %>% 
  filter(pos == "WR") %>% 
  mutate(rec_yds_resid = rec_yds - rec_yds_pred)
median(wr$rec_yds_resid)
mean(wr$rec_yds_resid)

by_qb <- qb %>% 
  group_by(player) %>% 
  summarize(difference = sum(difference),
            yards_resid = sum(pas_yds_resid))

wr_team <- wr %>% 
  group_by(team.x) %>% 
  summarize(resid = sum(my_resid))

median(qb$pas_yds_resid)*0.04 + median(qb$pas_tds_resid)*4 + median(qb$rus_yds_resid)*0.1 + median(qb$rus_tds_resid)*
  median(qb$int_resid)

ggplot(qb, aes(x = fpts, y = fpts_pred)) +
  geom_point() + 
  geom_abline(slope = 1) +
  xlim(5, 30) +
  ylim(5, 30)

mod <- lm(pas_yds ~ pas_yds_pred, qb)
summary(mod)


# qb <- data %>%
#   filter(pos == "QB")
# 
# rb <- data %>%
#   filter(pos == "RB")
# 
# wr <- data %>%
#   filter(pos == "WR")
# 
# median(qb$pas_yds_resid)
# median(rb$rus_yds_pred)
# median(wr$rec_yds_pred)


qb <- data %>%
  filter(pos == "QB") 

sum(qb$fpros_r_sq)/nrow(qb)
sum(qb$my_r_sq)/nrow(qb) 
#56 old
#52 normal
#50 no adj
#51.12 new
#51.7 new2
#50.9 new 3
#50.78 new norm
#50.20 new norm reg
#50.24 fixed 0 snaps

#45.08 old (wrong data file probably)
#48.87 zeroes for inactives adjustment
#49.00 no regress in update ratings
#49.74 high vol
#48.21 low vol
#49.02 old vol
#48.65 No high variance fix (low vol)

rb <- data %>%
  filter(pos == "RB")

sum(rb$fpros_r_sq)/nrow(rb)
sum(rb$my_r_sq)/nrow(rb) 
#60 old
#61 normal
#59 no adj
#59.35 new
#59.37 new2
#59.33 new3
#59.95 new norm
#59.21 new norm reg
#58.76 fixed 0 snaps

#58.42 old
#58.51 zeroes
#58.63 no regress
#58.05 high vol
#59.44 low vol
#58.41 old vol
#61.31 no high variance fix (old vol)

wr <- data %>%
  filter(pos == "WR")

sum(wr$fpros_r_sq)/nrow(wr)
sum(wr$my_r_sq)/nrow(wr) 
#48 old
#53 normal
#50 no adj
#49.11 new
#48.83 new2
#49.16 new3
#50.80 new norm
#50.00 new norm reg
#48.72 fixed 0 snaps

#47.80 old
#47.96 zeroes
#48.11 no regress
#48.11 high vol
#47.61 low vol
#47.80 old vol
#48.55 no high variance fix (old vol)

te <- data %>%
  filter(pos == "TE")

sum(te$fpros_r_sq)/nrow(te)
sum(te$my_r_sq)/nrow(te) 
#38 old
#38 normal
#38 no adj
#38.09 new
#37.98 new2
#38.11 new3
#37.94 new norm
#38.10 new norm reg
#38.22 fixed 0 snaps

#38.78 old
#38.39 zeroes
#38.54 no regress
#38.42 high vol
#39.77 low vol
#38.78 old vol
#40.62 no high variance fix (old vol)


#medians
median(qb$my_resid) #significant overprediction
median(rb$my_resid) #above 7 overprediction, overall slight overprediction
median(wr$my_resid) #significant underprediction, overall slight underprediction
median(te$my_resid) #significant overprediction, overall slight underprediction

qb <- qb %>% 
  filter(!is.na(pas_att_rat))

mod <- lm(fpros_resid ~ week + pos, data)
summary(mod)

mod2 <- lm(int ~ 0 + int_rat + def_int_rat, qb)
summary(mod2)


ind2 <- combined %>% 
  filter(player == "Michael Carter") %>% 
  select(player, team.x, opponent, week, rus_yds, rus_yds_pred, adj_rus_yds_per)



