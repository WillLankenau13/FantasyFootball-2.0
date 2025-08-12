



#download
d_cum_off_stats <- read_csv("~/R Stuff/FantasyFootball 2.0/cumulativeStats/cumulative_off_stats_2022.csv") %>% 
  clean_names()
d_cum_def_stats <- read_csv("~/R Stuff/FantasyFootball 2.0/cumulativeStats/cumulative_def_stats_2022.csv") %>%
  clean_names()

weekly_stats <- read_csv("~/R Stuff/FantasyFootball 2.0/weeklyStats/2022/Offensive_2022.csv") %>% 
  clean_names()


#tidy
weekly_stats <- weekly_stats %>% 
  select(team, opp, week, att_17, cmp_16, yds_20, td_21, int, att_38, yds_39, td_41)
colnames(weekly_stats) <- c("team", "opp", "week", "pas_att", "cmp", "pas_yds", "pas_tds", "int", "rus_att", "rus_yds", "rus_tds")

cum_off_stats <- d_cum_off_stats %>% 
  mutate(week = week+1)
cum_off_stats[4:12] <- cum_off_stats[4:12]/cum_off_stats$games

cum_def_stats <- d_cum_def_stats %>% 
  mutate(week = week+1)
cum_def_stats[4:12] <- cum_def_stats[4:12]/cum_def_stats$games

#join
joined <- weekly_stats %>% 
  left_join(cum_off_stats, by = c("team", "week")) %>% 
  left_join(cum_def_stats, by = c("opp" = "team", "week")) %>% 
  filter(week > 12) %>% 
  filter(week != 18)

#results df
values <- seq(0, 1, by = 0.01)

results_df <- expand.grid(Value = values)

results_df <- results_df %>% 
  mutate(pas_att_avg_r_sq = NA,
         cmp_avg_r_sq = NA,
         pas_yds_avg_r_sq = NA,
         pas_tds_avg_r_sq = NA,
         int_avg_r_sq = NA,
         rus_att_avg_r_sq = NA,
         rus_yds_avg_r_sq = NA,
         rus_tds_avg_r_sq = NA)
a <- 1

while(a <= nrow(results_df)){

v <- results_df$Value[a]
  
#predictions
m_predictions <- function(df, col){
  #rcombine
  df[, paste(col, "_pred", sep = "")] <- ((df[, paste("cum_off_", col, sep = "")]*v) + (df[, paste("cum_def_", col, sep = "")]*(1 - v)))
  
  return(df)
}

joined <- m_predictions(joined, "pas_att")
joined <- m_predictions(joined, "cmp")
joined <- m_predictions(joined, "pas_yds")
joined <- m_predictions(joined, "pas_tds")
joined <- m_predictions(joined, "rus_att")
joined <- m_predictions(joined, "rus_yds")
joined <- m_predictions(joined, "rus_tds")
joined <- m_predictions(joined, "int")


m_r_sq <- function(df, col){
  #rcombine
  df[, paste(col, "_r_sq", sep = "")] <- (df[, paste(col, sep = "")] - df[, paste(col, "_pred", sep = "")])^2
  
  return(df)
}

joined <- m_r_sq(joined, "pas_att")
joined <- m_r_sq(joined, "cmp")
joined <- m_r_sq(joined, "pas_yds")
joined <- m_r_sq(joined, "pas_tds")
joined <- m_r_sq(joined, "rus_att")
joined <- m_r_sq(joined, "rus_yds")
joined <- m_r_sq(joined, "rus_tds")
joined <- m_r_sq(joined, "int")

results_df$pas_att_avg_r_sq[a] <- sum(joined$pas_att_r_sq)/nrow(joined) 
results_df$cmp_avg_r_sq[a] <- sum(joined$cmp_r_sq)/nrow(joined) 
results_df$pas_yds_avg_r_sq[a] <- sum(joined$pas_yds_r_sq)/nrow(joined) 
results_df$pas_tds_avg_r_sq[a] <- sum(joined$pas_tds_r_sq)/nrow(joined) 
results_df$rus_att_avg_r_sq[a] <- sum(joined$rus_att_r_sq)/nrow(joined) 
results_df$rus_yds_avg_r_sq[a] <- sum(joined$rus_yds_r_sq)/nrow(joined) 
results_df$rus_tds_avg_r_sq[a] <- sum(joined$rus_tds_r_sq)/nrow(joined) 
results_df$int_avg_r_sq[a] <- sum(joined$int_r_sq)/nrow(joined) 

a <- a+1

}







