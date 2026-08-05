year <- 2014

df_list <- list()
pbp_list <- list()
c <- 1
a <- 0

while(year < 2025){
  pbp_list[[year-2013]] <- load_pbp(year)
  
  while(c < 17){
    temp <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/", year, "/byWeek/Week_", c, "_Stats.csv", sep = ""))) %>% 
      mutate(year = year)
    
    df_list[[c + a*16]] <- temp
    c <- c+1
  }
  c <- 1
  year <- year + 1
  a <- a+1
  
}


combined <- do.call(rbind, df_list)
pbp <- do.call(rbind, pbp_list) %>% 
  mutate(fmb = ifelse(is.na(fumbled_1_player_name), 0, 1),
         fmb_l = ifelse(fmb == 0, 0, ifelse(fumbled_1_team == fumble_recovery_1_team | is.na(fumble_recovery_1_team), 0, 1))) 

combined <- combined %>% 
  mutate(fmb_rat = ifelse(pos == "QB", fmb/(pas_att+sc_att), fmb/touches_g),
         fmb_l_rat = ifelse(pos == "QB", fmb_l/(pas_att+sc_att), fmb_l/touches_g)) %>% 
  filter(snap_per > 0.9) %>% 
  filter(pos == "QB")

t <- combined %>% 
  filter(pos != "QB")


s <- pbp %>% 
  filter(complete_pass == 1) %>%
  filter(passer != fumbled_1_player_name | is.na(fumbled_1_player_name) | is.na(passer))

r <- pbp %>% 
  filter(rush == 1) %>%
  filter(passer != fumbled_1_player_name | is.na(fumbled_1_player_name) | is.na(passer))
  
sum(s$fmb_l)/sum(s$fmb)
sum(s$fmb)/nrow(s)
sum(s$fmb_l)/nrow(s)

sum(r$fmb_l)/sum(r$fmb)
sum(r$fmb)/nrow(r)
sum(r$fmb_l)/nrow(r)

# q <- pbp %>% 
#   filter(pass == 1) %>% 
#   mutate(qb_fmb_l = ifelse(passer != fumbled_1_player_name & (fumbled_1_team == fumble_recovery_1_team | is.na(fumble_recovery_1_team)), 0, 1))
#   
# sum(q$qb_fmb_l)/nrow()
# sum(t$fmb)/sum(t$touches_g)
# sum(t$fmb_l)/sum(t$touches_g)


sum(t$fmb_l)/sum(t$fmb)
sum(t$fmb)/sum(t$touches_g)
sum(t$fmb_l)/sum(t$touches_g)

d <- 10
df_list2 <- list()

while(d < 17){
  temp <- combined %>% 
    filter(week < d) %>% 
    group_by(player, pos, team, year) %>% 
    summarize(avg_fmb_rat = mean(fmb_rat),
              avg_fmb_l_rat = mean(fmb_l_rat),
              avg_touches = mean(touches_g),
              avg_rus_att = mean(rus_att),
              avg_rec = mean(rec),
              avg_sc_att = mean(sc_att),
              avg_pas_att = mean(pas_att),
              avg_sks = mean(sks),
              avg_dbs = mean(dbs),
              sk_ratio = avg_sks/avg_dbs,
              games = n()
    )
  
  def_temp <- combined %>% 
    filter(week < d) %>% 
    group_by(opp, year) %>% 
    summarize(def_games = n_distinct(week),
              def_fmb_rat = sum(fmb)/def_games,
              def_fmb_l_rat = sum(fmb_l)/def_games,
              def_avg_sks = mean(sks),
              def_avg_dbs = mean(dbs),
              def_sk_ratio = def_avg_sks/def_avg_dbs)
  
  temp2 <- combined %>% 
    filter(week == d) %>% 
    select(player, pos, week, year, team, opp, pas_att, sc_att, rus_att, rec, touches_g, fmb, fmb_l)
  
  temp3 <- inner_join(temp, temp2, by = c("player", "pos", "team", "year")) %>% 
    left_join(def_temp, by = c("opp","year"))
  
  df_list2[[d-9]] <- temp3
  d <- d+1
}

data <- do.call(rbind, df_list2)

data <- data %>% 
  filter(games > 6) %>% 
  filter(pos == "QB") %>% 
  filter(!is.na(avg_fmb_rat))

p_list <- seq(0, 1, by = 0.01)
q_list <- seq(0, 1, by = 0.01)

results_df <- expand.grid(p = p_list, q = q_list)

results_df <- results_df %>% 
  mutate(rsq_a = NA,
         rsq_a0 = NA,
         rsq_a1 = NA,
         rsq_a2 = NA,
         rsq_a3 = NA,
         rsq_a4 = NA)

a <- 1

while(a <= nrow(results_df)){
  p <- results_df$p[a]
  q <- results_df$q[a]
  
  data <- data %>% 
    mutate(pred_fmb_l = avg_fmb_l_rat*(avg_pas_att+avg_sc_att),
           pred_fmb_l_0 = avg_fmb_rat*(avg_pas_att+avg_sc_att)*0.55,
           pred_fmb_l_1 = avg_pas_att*0.1*p + avg_sc_att*0.1*q,
           pred_fmb_l_2 = avg_dbs*0.1*p + avg_sc_att*0.1*q,
           pred_fmb_l_3 = 0.23*p + avg_pas_att*0.005 + avg_sc_att*0.001*q,
           pred_fmb_l_4 = 0.23*p + (avg_pas_att*0.005 + avg_sc_att*0.028)*(1-p))
           
           rsq_a <- 1 - sum((data$fmb_l - data$pred_fmb_l)^2) / sum((data$fmb_l - mean(data$fmb_l))^2)
           rsq_a0 <- 1 - sum((data$fmb_l - data$pred_fmb_l_0)^2) / sum((data$fmb_l - mean(data$fmb_l))^2)
           rsq_a1 <- 1 - sum((data$fmb_l - data$pred_fmb_l_1)^2) / sum((data$fmb_l - mean(data$fmb_l))^2)
           rsq_a2 <- 1 - sum((data$fmb_l - data$pred_fmb_l_2)^2) / sum((data$fmb_l - mean(data$fmb_l))^2)
           rsq_a3 <- 1 - sum((data$fmb_l - data$pred_fmb_l_3)^2) / sum((data$fmb_l - mean(data$fmb_l))^2)
           rsq_a4 <- 1 - sum((data$fmb_l - data$pred_fmb_l_4)^2) / sum((data$fmb_l - mean(data$fmb_l))^2)
           
           results_df$rsq_a[a] <- rsq_a
           results_df$rsq_a0[a] <- rsq_a0
           results_df$rsq_a1[a] <- rsq_a1
           results_df$rsq_a2[a] <- rsq_a2
           results_df$rsq_a3[a] <- rsq_a3
           results_df$rsq_a4[a] <- rsq_a4
           
           a <- a+1
}
  

mod <- lm(fmb_l ~ avg_sc_att + avg_pas_att + sk_ratio*def_sk_ratio, data)
summary(mod)

mod2 <- lm(fmb_l ~ avg_sc_att, data)
summary(mod2)

mod3 <- lm(sc_att ~ avg_sc_att + def_avg_sc_att, data)
summary(mod3)

mod4 <- lm(sc_yds ~ 0 + pred_sc_yds, data)
summary(mod4)

mean(data$fmb_l)

ggplot(data, aes(pred_sc_yds, sc_yds), position = "jitter") +
  geom_jitter() +
  xlim(0, 80) +
  ylim(0, 80)


data <- data %>% 
  mutate(pred_fmb_l_non_qb = avg_touches*0.005,
         pred_fmb_l_qb = avg_dbs*0.005 + avg_sc_att*0.028,
         pred_fmb_l_qb = avg_pas_att*0.005 + avg_sc_att*0.033)

