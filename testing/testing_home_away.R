



#download
d_cum_off_stats <- read_csv("~/R Stuff/FantasyFootball 2.0/cumulativeStats/cumulative_off_stats_2022.csv") %>% 
  clean_names()
d_cum_def_stats <- read_csv("~/R Stuff/FantasyFootball 2.0/cumulativeStats/cumulative_def_stats_2022.csv") %>%
  clean_names()

weekly_stats <- read_csv("~/R Stuff/FantasyFootball 2.0/weeklyStats/2022/Offensive_2022.csv") %>% 
  clean_names()


#tidy
weekly_stats <- weekly_stats %>%
  mutate(home = ifelse(is.na(x13), 1, 0)) %>% 
  select(team, opp, home, week, att_17, cmp_16, yds_20, td_21, int, att_38, yds_39, td_41)
colnames(weekly_stats) <- c("team", "opp", "home", "week", "pas_att", "cmp", "pas_yds", "pas_tds", "int", "rus_att", "rus_yds", "rus_tds")

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
  filter(week > 5) %>% 
  filter(week != 18)

#results df
values1 <- seq(1, 1.1, by = 0.01)
values2 <- seq(0.9, 1, by = 0.01)
values3 <- seq(0.9, 1, by = 0.01)
values4 <- seq(1, 1.1, by = 0.01)

results_df <- expand.grid(Value1 = values1, Value2 = values2, Value3 = values3, Value4 = values4)

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
  
v1 <- results_df$Value1[a] #off home
v2 <- results_df$Value2[a] #off away
v3 <- results_df$Value3[a] #def home
v4 <- results_df$Value4[a] #def away
  
data <- joined %>% 
  mutate(off_adj = ifelse(home == 1, v1, v2),
         def_adj = ifelse(home == 1, v3, v4))
  
data[14:22] <- data[14:22]*data$off_adj
data[24:32] <- data[24:32]*data$def_adj
  
cmp_off_coef <- 0.7
pas_att_off_coef <- 0.7
pas_yds_off_coef <- 0.8
pas_tds_off_coef <- 0.8
int_off_coef <- 0.7
rus_att_off_coef <- 0.6
rus_yds_off_coef <- 0.5
rus_tds_off_coef <- 0.5

#predictions
m_predictions <- function(df, col){
  #percent rating coefficient
  off_coef <- get(paste(col, "_off_coef", sep = ""))
  
  #rcombine
  df[, paste(col, "_pred", sep = "")] <- ((df[, paste("cum_off_", col, sep = "")]*(off_coef)) + (df[, paste("cum_def_", col, sep = "")]*(1 - off_coef)))
  
  return(df)
}

data <- m_predictions(data, "pas_att")
data <- m_predictions(data, "cmp")
data <- m_predictions(data, "pas_yds")
data <- m_predictions(data, "pas_tds")
data <- m_predictions(data, "rus_att")
data <- m_predictions(data, "rus_yds")
data <- m_predictions(data, "rus_tds")
data <- m_predictions(data, "int")


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

results_df$pas_att_avg_r_sq[a] <- sum(data$pas_att_r_sq)/nrow(data) 
results_df$cmp_avg_r_sq[a] <- sum(data$cmp_r_sq)/nrow(data) 
results_df$pas_yds_avg_r_sq[a] <- sum(data$pas_yds_r_sq)/nrow(data) 
results_df$pas_tds_avg_r_sq[a] <- sum(data$pas_tds_r_sq)/nrow(data) 
results_df$rus_att_avg_r_sq[a] <- sum(data$rus_att_r_sq)/nrow(data) 
results_df$rus_yds_avg_r_sq[a] <- sum(data$rus_yds_r_sq)/nrow(data) 
results_df$rus_tds_avg_r_sq[a] <- sum(data$rus_tds_r_sq)/nrow(data) 
results_df$int_avg_r_sq[a] <- sum(data$int_r_sq)/nrow(data) 

a <- a+1

print(a)

}


t <- results_df 

adj_f <- function(df, col, n){
  #rcombine
  df[, paste("adj_", col, "_r_sq", sep = "")] <- sqrt(df[, paste(col, "_avg_r_sq", sep = "")])/n
  
  return(df)
}

t <- adj_f(t, "pas_att", (1/0))
t <- adj_f(t, "cmp", (1/0))
t <- adj_f(t, "pas_yds", 10)
t <- adj_f(t, "pas_tds", (1/6))
t <- adj_f(t, "rus_att", (1/0))
t <- adj_f(t, "rus_yds", 10)
t <- adj_f(t, "rus_tds", (1/6))

t$new_sum <- rowSums(t[ , 13:19], na.rm = TRUE)
t$yds_sum <- rowSums(t[ , c(7, 11)], na.rm = TRUE)


mod <- lm(pas_yds ~ 0 + cum_off_pas_yds + cum_def_pas_yds + cum_off_rus_yds + cum_def_rus_yds, joined)
summary(mod)

mod <- lm(pas_tds ~ 0 + cum_off_pas_tds + cum_def_pas_tds + home, joined)
summary(mod)

mod <- lm(rus_tds ~ 0 + cum_off_rus_tds + cum_def_rus_tds + home, joined)
summary(mod)


