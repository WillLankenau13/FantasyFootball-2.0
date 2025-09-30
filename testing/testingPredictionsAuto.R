
year <- 2024


#Create Dataframe
vl1 <- c(17, 34, 50)
vl2 <- c(0.3, 0.4, 0.5, 0.6)
vl3 <- c(0, 0.5, 1)

results_df <- expand.grid(Value1 = vl1, Value2 = vl2, Value3 = vl3)

results_df <- results_df %>% 
  mutate(median_resid = NA,
         sum_r_sq = NA,
         pas_att_avg_r_sq = NA,
         cmp_avg_r_sq = NA,
         pas_yds_avg_r_sq = NA,
         pas_tds_avg_r_sq = NA,
         int_avg_r_sq = NA,
         rus_att_avg_r_sq = NA,
         rus_yds_avg_r_sq = NA,
         rus_tds_avg_r_sq = NA,
         tgt_avg_r_sq = NA,
         rec_avg_r_sq = NA,
         rec_yds_avg_r_sq = NA,
         rec_tds_avg_r_sq = NA)

a <- 1

while(a <= nrow(results_df)){
  
  val1 <- results_df$Value1[a]
  val2 <- results_df$Value2[a]
  val3 <- results_df$Value3[a]

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

c <- 2
week <- c

my_pred <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyPredictions/", year, "/Week_", week, "_Player_Predictions.csv", sep = "")))

###Real Data
d_past_week_player_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/", year, "/byWeek/Week_", week, "_Stats.csv", sep = ""))) %>%
  clean_names() %>%
  select(!date)

player_stats <- player_names_func(d_past_week_player_stats)

player_stats <- player_stats %>%
  mutate(fpts = yds_22*0.04 + td_23*4 + yds_38*0.1 + td_40*6 + yds_45*0.1 + td_47*6 + 0.5*rec - 1*int - 2*fmb_7) %>%
  select(player, att_19, cmp_18, yds_22, td_23, att_37, yds_38, td_40, tgt_43, rec, yds_45, td_47, int, fmb_7, fpts, off_percent_8)
colnames(player_stats) <- c("player", "pas_att", "cmp", "pas_yds", "pas_tds", "rus_att", "rus_yds", "rus_tds", "tgt", "rec", "rec_yds", "rec_tds", "int", "fmb", "fpts", "snap_per")

temp <- my_pred %>%
  full_join(player_stats, by = c("player")) %>%
  filter(!is.na(fpts))

combined <- temp %>% 
  mutate(week = week)

c <- c+1

while(c < 19){
  week <- c
  
  my_pred <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyPredictions/", year, "/Week_", week, "_Player_Predictions.csv", sep = "")))

  ###Real Data
  d_past_week_player_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/", year, "/byWeek/Week_", week, "_Stats.csv", sep = ""))) %>%
    clean_names() %>%
    select(!date)
  
  player_stats <- player_names_func(d_past_week_player_stats)
  
  player_stats <- player_stats %>%
    mutate(fpts = yds_22*0.04 + td_23*4 + yds_38*0.1 + td_40*6 + yds_45*0.1 + td_47*6 + 0.5*rec - 1*int - 2*fmb_7) %>%
    select(player, att_19, cmp_18, yds_22, td_23, att_37, yds_38, td_40, tgt_43, rec, yds_45, td_47, int, fmb_7, fpts, off_percent_8)
  colnames(player_stats) <- c("player", "pas_att", "cmp", "pas_yds", "pas_tds", "rus_att", "rus_yds", "rus_tds", "tgt", "rec", "rec_yds", "rec_tds", "int", "fmb", "fpts", "snap_per")
  
  temp <- my_pred %>%
    full_join(player_stats, by = c("player")) %>%
    filter(!is.na(fpts)) %>% 
    mutate(week = week)
  
  combined <- rbind(combined, temp)
  
  c <- c+1
}

data <- combined %>% 
  filter(fpts > 0) %>%
  filter(fpts_pred > 6) %>% 
  mutate(my_resid = (fpts - fpts_pred),
         my_r_sq = my_resid^2) %>%
  filter(!is.na(fpts)) %>%
  filter(!is.na(fpts_pred))


results_df$median_resid[a] <- median(data$my_resid)
results_df$sum_r_sq[a] <- sum(data$my_r_sq)


m_r_sq <- function(df, col){
  #rcombine
  df[, paste(col, "_r_sq", sep = "")] <- (df[, paste(col, sep = "")] - df[, paste(col, "_pred", sep = "")])^2
  
  return(df)
}

data <- m_r_sq(data, "pas_att")
data <- m_r_sq(data, "cmp")
data <- m_r_sq(data, "pas_yds")
data <- m_r_sq(data, "pas_tds")
data <- m_r_sq(data, "rus_att")
data <- m_r_sq(data, "rus_yds")
data <- m_r_sq(data, "rus_tds")
data <- m_r_sq(data, "int")
data <- m_r_sq(data, "tgt")
data <- m_r_sq(data, "rec")
data <- m_r_sq(data, "rec_yds")
data <- m_r_sq(data, "rec_tds")

results_df$pas_att_avg_r_sq[a] <- sum(data$pas_att_r_sq)/nrow(data) 
results_df$cmp_avg_r_sq[a] <- sum(data$cmp_r_sq)/nrow(data) 
results_df$pas_yds_avg_r_sq[a] <- sum(data$pas_yds_r_sq)/nrow(data) 
results_df$pas_tds_avg_r_sq[a] <- sum(data$pas_tds_r_sq)/nrow(data) 
results_df$rus_att_avg_r_sq[a] <- sum(data$rus_att_r_sq)/nrow(data) 
results_df$rus_yds_avg_r_sq[a] <- sum(data$rus_yds_r_sq)/nrow(data) 
results_df$rus_tds_avg_r_sq[a] <- sum(data$rus_tds_r_sq)/nrow(data) 
results_df$int_avg_r_sq[a] <- sum(data$int_r_sq)/nrow(data) 
results_df$tgt_avg_r_sq[a] <- sum(data$tgt_r_sq)/nrow(data) 
results_df$rec_avg_r_sq[a] <- sum(data$rec_r_sq)/nrow(data) 
results_df$rec_yds_avg_r_sq[a] <- sum(data$rec_yds_r_sq)/nrow(data) 
results_df$rec_tds_avg_r_sq[a] <- sum(data$rec_tds_r_sq)/nrow(data) 

a <- a+1

}



t <- results_df %>% 
  select(Value1, Value2, Value3, rus_att_avg_r_sq:rus_tds_avg_r_sq, tgt_avg_r_sq:rec_tds_avg_r_sq)

