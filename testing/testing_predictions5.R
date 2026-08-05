
year <- 2024

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



c <- 1

while(c < 19){
  
  week <- c
  
  old_pred <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/preScramblePredictions/", year, "/Week_", week, "_Player_Predictions.csv", sep = "")))
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
  
  #combine with old predictions
  old_pred <- old_pred %>% 
    select(player, pos, team, pas_att_pred:rus_tds_pred, fpts_pred)
  
  names(old_pred) <- sub("_pred$", "_pred_old", names(old_pred))
  
  temp <- temp %>% 
    full_join(old_pred, by = c("player", "pos", "team.x" = "team"))
  
  #difference
  temp$fpros_pred <- as.numeric(temp$fpros_pred)
  
  temp <- temp %>% 
    mutate(difference = fpts_pred - fpros_pred)
  
  ###Real Data
  d_past_week_player_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/", year, "/byWeek/Week_", week, "_Stats.csv", sep = ""))) %>%
    clean_names()
  
  player_stats <- player_names_func(d_past_week_player_stats)
  
  player_stats <- player_stats %>%
    mutate(fpts = pas_yds*0.04 + pas_tds*4 + sc_yds*0.1 + sc_tds*6 + rus_yds*0.1 + rus_tds*6 + rec_yds*0.1 + rec_tds*6 + 0.5*rec - 1*int - 2*fmb_l)
  
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

combined2 <- do.call(rbind, df_list)

t <- combined2 %>% 
  filter(pos == "QB")

rsq_1 <- 1 - sum((t$rus_att - t$rus_att_pred)^2) / sum((t$rus_att - mean(t$rus_att))^2)
rsq_2 <- 1 - sum((t$rus_yds - t$rus_yds_pred)^2) / sum((t$rus_yds - mean(t$rus_yds))^2)
rsq_3 <- 1 - sum((t$rus_tds - t$rus_tds_pred)^2) / sum((t$rus_tds - mean(t$rus_tds))^2)

data <- combined2 %>%
  filter(!is.na(fpts)) %>%
  filter(!is.na(fpts_pred)) %>%
  filter(!is.na(fpros_pred)) %>% 
  filter(!is.na(fpts_pred_old)) %>% 
  filter(fpts > 0) %>% 
  mutate(my_resid = (fpts - fpts_pred),
         fpros_resid = (fpts - fpros_pred),
         old_resid = (fpts - fpts_pred_old),
         my_r_sq = my_resid^2,
         fpros_r_sq = fpros_resid^2,
         old_r_sq = old_resid^2,
         abs_my_resid = abs(my_resid),
         abs_fpros_resid = abs(fpros_resid),
         abs_old_resid = abs(old_resid)) %>% 
  mutate(pos = pos.x) %>% 
  clean_names()

ind <- data %>% 
  filter(player == "Jalen Hurts") %>% 
  select(player, week, opponent, pas_yds_rat, def_pas_yds_rat, pas_yds_pred, pas_yds, rus_yds_pred, rus_yds)

s <- data %>% 
  select(player, pos, team, fpts, fpts_pred, fpts_pred_old, fpros_pred)


median(data$abs_old_resid)
median(data$abs_my_resid)
median(data$abs_fpros_resid)

mean(data$abs_old_resid)
mean(data$abs_my_resid)
mean(data$abs_fpros_resid)


mean_d <- mean(data$fpts)

data <- data %>% 
  mutate(sst = (fpts - mean_d)^2)

median(data$old_resid)
median(data$my_resid)
median(data$fpros_resid)

1 - sum(data$old_r_sq)/sum(data$sst)
1 - sum(data$my_r_sq)/sum(data$sst)
1 - sum(data$fpros_r_sq)/sum(data$sst)


sum(data$old_r_sq)/nrow(data)
sum(data$my_r_sq)/nrow(data)
sum(data$fpros_r_sq)/nrow(data) 


qb <- data %>%
  filter(pos == "QB") 

sum(qb$old_r_sq)/nrow(qb) 
sum(qb$my_r_sq)/nrow(qb) 
sum(qb$fpros_r_sq)/nrow(qb)



rb <- data %>%
  filter(pos == "RB")

sum(rb$old_r_sq)/nrow(rb) 
sum(rb$my_r_sq)/nrow(rb) 
sum(rb$fpros_r_sq)/nrow(rb)



wr <- data %>%
  filter(pos == "WR")

sum(wr$old_r_sq)/nrow(wr) 
sum(wr$my_r_sq)/nrow(wr) 
sum(wr$fpros_r_sq)/nrow(wr)



te <- data %>%
  filter(pos == "TE")

sum(te$old_r_sq)/nrow(te) 
sum(te$my_r_sq)/nrow(te) 
sum(te$fpros_r_sq)/nrow(te)



ind2 <- combined %>% 
  filter(player == "George Kittle") %>% 
  select(player, team.x, opponent, week, rec_yds, rec_yds_pred, adj_rec_yds_per)

t <- data %>% 
  select(player, pos, team, week, fpts, fpts_pred, fpros_pred, my_resid, fpros_resid) %>% 
  mutate(dif = abs(my_resid) - abs(fpros_resid)) %>% 
  filter(pos == "QB")



d <- qb %>% 
  mutate(rus_yds_resid = rus_yds - rus_yds_pred) %>% 
  select(player, pos, team, week, fpts, fpts_pred, fpros_pred, my_resid, fpros_resid, rus_yds, rus_yds_pred, rus_yds_resid)


r <- data %>% 
  select(player, pos, team, week, fpts, fpts_pred, fpros_pred) %>% 
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
