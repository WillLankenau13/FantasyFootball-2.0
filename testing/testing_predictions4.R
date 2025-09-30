
year <- 2025

Past_Year_d <- year-1
This_Year_d <- year

a <- 1

while(a < 19){
  upcoming_week <- a
  source("make_predictions.R")

  past_week <- a
  upcoming_week <- a+1
  source("update_ratings.R")

  a <- a+1
}


df_list <- list()



c <- 2

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
  filter(!is.na(fpts)) %>%
  filter(!is.na(fpts_pred)) %>%
  filter(!is.na(fpros_pred)) %>% 
  filter(fpts > 0) %>% 
  mutate(my_resid = (fpts - fpts_pred),
         fpros_resid = (fpts - fpros_pred),
         my_r_sq = my_resid^2,
         fpros_r_sq = fpros_resid^2,
         abs_my_resid = abs(my_resid),
         abs_fpros_resid = abs(fpros_resid)) %>% 
  mutate(pos = pos.x) 

median(data$abs_my_resid)
median(data$abs_fpros_resid)

mean(data$abs_my_resid)
mean(data$abs_fpros_resid)

mean_d <- mean(data$fpts)

data <- data %>% 
  mutate(sst = (fpts - mean_d)^2)

median(data$fpros_resid)
median(data$my_resid)
#2022: -0.44 vs -1.6
#2023: -0.12 vs -1.4
#2024: 0.04 vs -1
 
1 - sum(data$fpros_r_sq)/sum(data$sst)
1 - sum(data$my_r_sq)/sum(data$sst)

sum(data$fpros_r_sq)/nrow(data) 
sum(data$my_r_sq)/nrow(data)
#2022: 50.05 vs 47.62
#2023: 51.11 vs 48.37
#2024: 53.27 vs 49.48

#healthy scratch players fix
#35.90

qb <- data %>%
  filter(pos == "QB") 

sum(qb$fpros_r_sq)/nrow(qb)
sum(qb$my_r_sq)/nrow(qb) 
#2022: 49.18 vs 46.09
#2023: 52.54 vs 49.28
#2024: 61.73 vs 58.13

#61.45

rb <- data %>%
  filter(pos == "RB")

sum(rb$fpros_r_sq)/nrow(rb)
sum(rb$my_r_sq)/nrow(rb) 
#2022: 58.38 vs 56.27
#2023: 50.67 vs 48.66
#2024: 50.47 vs 47.78

#35.01

wr <- data %>%
  filter(pos == "WR")

sum(wr$fpros_r_sq)/nrow(wr)
sum(wr$my_r_sq)/nrow(wr) 
#2022: 46.64 vs 43.95
#2023: 53.07 vs 50.29
#2024: 53.61 vs 48.64

#36.91

te <- data %>%
  filter(pos == "TE")

sum(te$fpros_r_sq)/nrow(te)
sum(te$my_r_sq)/nrow(te) 
#2022: 39.33 vs 38.86
#2023: 41.57 vs 38.10
#2024: 39.79 vs 37.13

#20.38

ind2 <- combined %>% 
  filter(player == "George Kittle") %>% 
  select(player, team.x, opponent, week, rec_yds, rec_yds_pred, adj_rec_yds_per)

t <- data %>% 
  select(player, pos, team.x, week, fpts, fpts_pred, fpros_pred, my_resid, fpros_resid) %>% 
  mutate(dif = abs(my_resid) - abs(fpros_resid)) %>% 
  filter(pos == "QB")



d <- qb %>% 
  mutate(rus_yds_resid = rus_yds - rus_yds_pred) %>% 
           select(player, pos, team.x, week, fpts, fpts_pred, fpros_pred, my_resid, fpros_resid, rus_yds, rus_yds_pred, rus_yds_resid)
  

r <- data %>% 
  select(player, pos, team.x, week, fpts, fpts_pred, fpros_pred) %>% 
  filter(fpros_pred > 0 | fpts_pred > 0)

mean_fpts <- mean(r$fpts)

a <- 0.3

r <- r %>% 
  mutate(com_fpts = a*fpts_pred + (1-a)*fpros_pred,
         ssr = (fpts - fpts_pred)^2,
         sst = (fpts - mean_fpts)^2,
         ssc = (fpts - com_fpts)^2,
         ssrfp = (fpts - fpros_pred)^2)

sum(r$ssr)
sum(r$sst)

1 - sum(r$ssr)/sum(r$sst)
1 - sum(r$ssrfp)/sum(r$sst)
1 - sum(r$ssc)/sum(r$sst)

summary(lm(fpts ~ fpts_pred, r))$r.squared

mod <- lm(fpts ~ fpts_pred, r)
summary(mod)

mod2 <- lm(fpts ~ fpros_pred, r)
summary(mod2)

mod3 <- lm(fpts ~ 0 + fpts_pred + fpros_pred, r)
summary(mod3)
