



#Create Dataframe
v1 <- c(0, 0.1, 0.2, 0.3, 0.4)
v2 <- c(0)

results_df <- expand.grid(Value1 = v1, Value2 = v2)

results_df <- results_df %>% 
  mutate(pas_yds_median_resid = NA,
         pas_att_avg_r_sq = NA,
         cmp_avg_r_sq = NA,
         pas_yds_avg_r_sq = NA,
         pas_tds_avg_r_sq = NA,
         rus_yds_median_resid = NA,
         rus_att_avg_r_sq = NA,
         rus_yds_avg_r_sq = NA,
         rus_tds_avg_r_sq = NA)

a <- 1

while(a <= nrow(results_df)){
  
  val1 <- results_df$Value1[a]
  val2 <- results_df$Value2[a]

#Create Predictions
c <- 1

past_week <- c
upcoming_week <- c+1
source("update_ratings.R")

c <- 2

while(c < 19){
  upcoming_week <- c
  source("make_predictions.R")
  
  past_week <- c
  upcoming_week <- c+1
  source("update_ratings.R")
  
  c <- c+1
}

c <- 3
week <- c

my_pred <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyPredictions/", This_Year, "/Week_", week, "_Player_Predictions.csv", sep = "")))
team_pred <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyPredictions/", This_Year, "/Week_", week, "_Team_Predictions.csv", sep = "")))
QB_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", week, "/QB_Ratings.csv", sep = "")))
def_team_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", week, "/Def_Team_ratings.csv", sep = "")))



###Real Data
d_past_week_player_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/", This_Year, "/byWeek/Week_", week, "_Stats.csv", sep = ""))) %>%
  clean_names() %>%
  select(!date)

player_stats <- player_names_func(d_past_week_player_stats)

player_stats <- player_stats %>%
  mutate(fpts = yds_22*0.04 + td_23*4 + yds_38*0.1 + td_40*6 + yds_45*0.1 + td_47*6 + 0.5*rec - 1*int - 2*fmb_7) %>%
  select(player, team, yds_22, td_23, att_37, yds_38, td_40, yds_45, td_47, rec, tgt_5, int, fmb_7, fpts, off_percent_8)
colnames(player_stats) <- c("player", "team", "pas_yds", "pas_tds", "rus_att", "rus_yds", "rus_tds", "rec_yds", "rec_tds", "rec", "tgt", "int", "fmb", "fpts", "snap_per")

past_week_team_stats <- player_stats %>% 
  group_by(team) %>% 
  summarise(across(pas_yds:fmb, sum),
            .groups = 'drop') %>% 
  select(team, pas_yds:fmb)

#ratings
temp <- team_pred %>% 
  full_join(past_week_team_stats, by = "team")

combined <- temp %>% 
  mutate(week = week)

c <- c+1

while(c < 19){
  week <- c
  
  my_pred <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyPredictions/", This_Year, "/Week_", week, "_Player_Predictions.csv", sep = "")))
  team_pred <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyPredictions/", This_Year, "/Week_", week, "_Team_Predictions.csv", sep = "")))
  QB_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", week, "/QB_Ratings.csv", sep = "")))
  def_team_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", week, "/Def_Team_ratings.csv", sep = "")))
  
  ###Real Data
  d_past_week_player_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/", This_Year, "/byWeek/Week_", week, "_Stats.csv", sep = ""))) %>%
    clean_names() %>%
    select(!date)
  
  player_stats <- player_names_func(d_past_week_player_stats)
  
  player_stats <- player_stats %>%
    mutate(fpts = yds_22*0.04 + td_23*4 + yds_38*0.1 + td_40*6 + yds_45*0.1 + td_47*6 + 0.5*rec - 1*int - 2*fmb_7) %>%
    select(player, team, yds_22, td_23, att_37, yds_38, td_40, yds_45, td_47, rec, tgt_5, int, fmb_7, fpts, off_percent_8)
  colnames(player_stats) <- c("player", "team", "pas_yds", "pas_tds", "rus_att", "rus_yds", "rus_tds", "rec_yds", "rec_tds", "rec", "tgt", "int", "fmb", "fpts", "snap_per")
  
  past_week_team_stats <- player_stats %>% 
    group_by(team) %>% 
    summarise(across(pas_yds:fmb, sum),
              .groups = 'drop') %>% 
    select(team, pas_yds:fmb)
  
  #ratings
  temp <- team_pred %>% 
    full_join(past_week_team_stats, by = "team") %>% 
    mutate(week = week)
  
  combined <- rbind(combined, temp)
  
  c <- c+1
}

data <- combined %>% 
  mutate(pas_yds_resid = rec_yds - team_pas_yds_pred,
         rus_yds_resid = rus_yds - team_rus_yds_pred,
         pas_att_r_sq = (tgt - team_pas_att_pred)^2,
         cmp_r_sq = (cmp - team_cmp_pred)^2,
         pas_yds_r_sq = (rec_yds - team_pas_yds_pred)^2,
         pas_tds_r_sq = (rec_tds - team_pas_tds_pred)^2,
         rus_att_r_sq = (rus_att - team_rus_att_pred)^2,
         rus_yds_r_sq = (rus_yds - team_rus_yds_pred)^2,
         rus_tds_r_sq = (rus_tds - team_rus_tds_pred)^2) %>% 
  filter(!is.na(pas_yds_resid)) %>% 
  filter(week > 7)

results_df$pas_yds_median_resid[a] <- median(data$pas_yds_resid)
results_df$rus_yds_median_resid[a] <- median(data$rus_yds_resid)


results_df$pas_att_avg_r_sq[a] <- sum(data$pas_att_r_sq)/nrow(data) 
results_df$cmp_avg_r_sq[a] <- sum(data$cmp_r_sq)/nrow(data) 
results_df$pas_yds_avg_r_sq[a] <- sum(data$pas_yds_r_sq)/nrow(data) 
results_df$pas_tds_avg_r_sq[a] <- sum(data$pas_tds_r_sq)/nrow(data) 
results_df$rus_att_avg_r_sq[a] <- sum(data$rus_att_r_sq)/nrow(data) 
results_df$rus_yds_avg_r_sq[a] <- sum(data$rus_yds_r_sq)/nrow(data) 
results_df$rus_tds_avg_r_sq[a] <- sum(data$rus_tds_r_sq)/nrow(data) 

a <- a+1

}

t <- results_df %>% 
  select(Value1, pas_yds_median_resid:pas_tds_avg_r_sq)




# testing <- data %>% 
#   select(team, week, team_rus_att_pred:team_rus_tds_pred, rus_att:rus_tds) %>% 
#   mutate(rus_att_resid = rus_att - team_rus_att_pred,
#          rus_yds_resid = rus_yds - team_rus_yds_pred,
#          rus_tds_resid = rus_tds - team_rus_tds_pred,
#          rus_att_r_sq = rus_att_resid^2,
#          rus_yds_r_sq = rus_yds_resid^2,
#          rus_tds_r_sq = rus_tds_resid^2)
# 
# ggplot(testing, aes(x = team_rus_yds_pred, y = rus_yds_resid)) +
#   geom_point() 
# 
# 
# 
# mod <- lm(rus_yds ~ 0 + team_rus_yds_pred, testing)
# summary(mod)






