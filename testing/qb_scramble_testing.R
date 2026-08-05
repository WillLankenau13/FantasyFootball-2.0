year <- 2014

df_list <- list()
c <- 1
a <- 0

while(year < 2025){

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


combined <- combined %>% 
  mutate(db = pas_att + sc_att,
         sc_per = sc_att/db,
         sc_yds_p_att = ifelse(is.na(sc_yds/sc_att), 0, sc_yds/sc_att)) %>% 
  filter(pos == "QB") %>% 
  filter(pas_att != 0) %>% 
  filter(snap_per > 0.9)

sum(combined$sc_tds)/sum(combined$sc_att)
sum(combined$sc_yds)/sum(combined$sc_att)
sum(combined$sc_att)/sum(combined$sks)
mean(combined$sc_att)
mean(combined$sc_tds)

d <- 10
df_list2 <- list()

while(d < 17){
  temp <- combined %>% 
    filter(week < d) %>% 
    group_by(player, team, year) %>% 
    summarize(avg_pas_att = mean(pas_att),
              avg_sc_att = mean(sc_att),
              #avg_db = mean(dbs),
              #avg_sk = mean(sks),
              avg_sc_yds = mean(sc_yds),
              avg_sc_tds = mean(sc_tds),
              avg_sc_per = mean(sc_per),
              avg_sc_yds_p_att = mean(sc_yds_p_att[sc_yds_p_att != 0]),
              #avg_db = mean(db),
              #sc_ratio = avg_sc_att/(avg_sc_att + avg_sk),
              games = n()
    )
  
  def_temp <- combined %>% 
    filter(week < d) %>% 
    group_by(opp, year) %>% 
    summarize(def_avg_pas_att = mean(pas_att),
              def_avg_sc_att = mean(sc_att),
              #def_avg_db = mean(dbs),
              #def_avg_sk = mean(sks),
              def_avg_sc_yds = mean(sc_yds),
              def_avg_sc_tds = mean(sc_tds),
              def_avg_sc_per = mean(sc_per),
              #def_avg_sc_yds_p_att = mean(sc_yds_p_att[sc_yds_p_att != 0]),
              #def_avg_db = mean(db),
              #def_pres_per = (def_avg_sk+def_avg_sc_att)/def_avg_db)
    )
              
  temp2 <- combined %>% 
    filter(week == d) %>% 
    select(player, week, year, team, opp, pas_att, sc_att, sc_yds, sc_tds, sc_per)
  
  temp3 <- inner_join(temp, temp2, by = c("player", "team", "year")) %>% 
    left_join(def_temp, by = c("opp","year"))
  
  df_list2[[d-9]] <- temp3
  d <- d+1
}

data <- do.call(rbind, df_list2)

data <- data %>% 
  #mutate(avg_sc_yds_p_att = ifelse(is.na(avg_sc_yds_p_att), 0, avg_sc_yds_p_att),
         #sc_ratio = ifelse(is.na(sc_ratio), 0, sc_ratio)) %>% 
  filter(games > 6)

p_list <- seq(0, 1, by = 0.01)
q_list <- seq(0, 0, by = 1)

results_df <- expand.grid(p = p_list, q = q_list)

results_df <- results_df %>% 
  mutate(rsq_a = NA,
         rsq_a0 = NA,
         rsq_a1 = NA,
         rsq_a2 = NA,
         rsq_a3 = NA,
         rsq_a4 = NA,
         rsq_b = NA,
         rsq_b0 = NA,
         rsq_b1 = NA,
         rsq_b2 = NA,
         rsq_b3 = NA,
         rsq_b4 = NA,
         rsq_b5 = NA,
         rsq_b6 = NA,
         rsq_c = NA,
         rsq_c0 = NA,
         rsq_c1 = NA,
         rsq_c2 = NA)

a <- 1

while(a <= nrow(results_df)){
  p <- results_df$p[a]
  q <- results_df$q[a]
  
  data <- data %>% 
    mutate(pred_sc_att = avg_sc_att,
           pred_sc_att_0 = avg_sc_att*p + def_avg_sc_att*(1-p),
           pred_sc_att_1 = avg_sc_att*0.78 + 1.62*(0.22),
           pred_sc_att_2 = 0,#avg_db*avg_sc_per*p + def_avg_db*def_avg_sc_per*(1-p),
           pred_sc_att_3 = 0, #(avg_db*p + def_avg_db*(1-p))*(avg_sc_per*p + def_avg_sc_per*(1-p)),
           pred_sc_att_4 = 0)#avg_sc_att*p + def_avg_sc_att*(1-p)*2/5 + def_avg_sk*(1-p)*3/5)
  
  rsq_a <- 1 - sum((data$sc_att - data$pred_sc_att)^2) / sum((data$sc_att - mean(data$sc_att))^2)
  rsq_a0 <- 1 - sum((data$sc_att - data$pred_sc_att_0)^2) / sum((data$sc_att - mean(data$sc_att))^2)
  rsq_a1 <- 1 - sum((data$sc_att - data$pred_sc_att_1)^2) / sum((data$sc_att - mean(data$sc_att))^2)
  rsq_a2 <- 1 - sum((data$sc_att - data$pred_sc_att_2)^2) / sum((data$sc_att - mean(data$sc_att))^2)
  rsq_a3 <- 1 - sum((data$sc_att - data$pred_sc_att_3)^2) / sum((data$sc_att - mean(data$sc_att))^2)
  rsq_a4 <- 1 - sum((data$sc_att - data$pred_sc_att_4)^2) / sum((data$sc_att - mean(data$sc_att))^2)
  
    
  # data <- data %>% 
  #   mutate(pred_sc_yds = avg_sc_yds,
  #          pred_sc_yds_0 = avg_sc_yds*p + def_avg_sc_yds*(1-p),
  #          pred_sc_yds_1 = avg_sc_att*avg_sc_yds_p_att,
  #          pred_sc_yds_2 = avg_db*avg_sc_per*avg_sc_yds_p_att,
  #          pred_sc_yds_3 = pred_sc_att_0*avg_sc_yds_p_att,
  #          pred_sc_yds_4 = pred_sc_att_0*(avg_sc_yds_p_att*p + def_avg_sc_yds_p_att*(1-p)),
  #          pred_sc_yds_5 = pred_sc_att_0*7.37,
  #          pred_sc_yds_6 = pred_sc_att_1*(avg_sc_yds_p_att*p + q*(1-p))
  #          )
  #   
  # rsq_b <- 1 - sum((data$sc_yds - data$pred_sc_yds)^2) / sum((data$sc_yds - mean(data$sc_yds))^2)
  # rsq_b0 <- 1 - sum((data$sc_yds - data$pred_sc_yds_0)^2) / sum((data$sc_yds - mean(data$sc_yds))^2)
  # rsq_b1 <- 1 - sum((data$sc_yds - data$pred_sc_yds_1)^2) / sum((data$sc_yds - mean(data$sc_yds))^2)
  # rsq_b2 <- 1 - sum((data$sc_yds - data$pred_sc_yds_2)^2) / sum((data$sc_yds - mean(data$sc_yds))^2)
  # rsq_b3 <- 1 - sum((data$sc_yds - data$pred_sc_yds_3)^2) / sum((data$sc_yds - mean(data$sc_yds))^2)
  # rsq_b4 <- 1 - sum((data$sc_yds - data$pred_sc_yds_4)^2) / sum((data$sc_yds - mean(data$sc_yds))^2)
  # rsq_b5 <- 1 - sum((data$sc_yds - data$pred_sc_yds_5)^2) / sum((data$sc_yds - mean(data$sc_yds))^2)
  # rsq_b6 <- 1 - sum((data$sc_yds - data$pred_sc_yds_6)^2) / sum((data$sc_yds - mean(data$sc_yds))^2)
  # 
  # data <- data %>% 
  #   mutate(pred_sc_tds = pred_sc_att_0*0.03,
  #          pred_sc_tds_0 = pred_sc_att_0*0.03*p + avg_sc_tds*(1-p),
  #          pred_sc_tds_1 = pred_sc_att_0*0.03*p + 0.05*(1-p),
  #          pred_sc_tds_2 = pred_sc_att_0*0.03*p + 0.05*(q) + avg_sc_tds*(1-p-q))
  # 
  # rsq_c <- 1 - sum((data$sc_tds - data$pred_sc_tds)^2) / sum((data$sc_tds - mean(data$sc_tds))^2)
  # rsq_c0 <- 1 - sum((data$sc_tds - data$pred_sc_tds_0)^2) / sum((data$sc_tds - mean(data$sc_tds))^2)
  # rsq_c1 <- 1 - sum((data$sc_tds - data$pred_sc_tds_1)^2) / sum((data$sc_tds - mean(data$sc_tds))^2)
  # rsq_c2 <- 1 - sum((data$sc_tds - data$pred_sc_tds_2)^2) / sum((data$sc_tds - mean(data$sc_tds))^2)
  # 
  # 
  results_df$rsq_a[a] <- rsq_a
  results_df$rsq_a0[a] <- rsq_a0
  results_df$rsq_a1[a] <- rsq_a1
  results_df$rsq_a2[a] <- rsq_a2
  results_df$rsq_a3[a] <- rsq_a3
  results_df$rsq_a4[a] <- rsq_a4
  
  # results_df$rsq_b[a] <- rsq_b
  # results_df$rsq_b0[a] <- rsq_b0
  # results_df$rsq_b1[a] <- rsq_b1
  # results_df$rsq_b2[a] <- rsq_b2
  # results_df$rsq_b3[a] <- rsq_b3
  # results_df$rsq_b4[a] <- rsq_b4
  # results_df$rsq_b5[a] <- rsq_b5
  # results_df$rsq_b6[a] <- rsq_b6
  # 
  # results_df$rsq_c[a] <- rsq_c
  # results_df$rsq_c0[a] <- rsq_c0
  # results_df$rsq_c1[a] <- rsq_c1
  # results_df$rsq_c2[a] <- rsq_c2
  
  a <- a+1
}

t <- results_df %>% 
  select(c(1, 2, 17,18, 19,20))

rsq_c1 <- 1 - sum((data$sc_tds - data$sc_tds_avg)^2) / sum((data$sc_tds - mean(data$sc_tds))^2)
rsq_c2 <- 1 - sum((data$sc_tds - data$pred_sc_tds)^2) / sum((data$sc_tds - mean(data$sc_tds))^2)
rsq_c3 <- 1 - sum((data$sc_tds - data$pred_sc_tds_2)^2) / sum((data$sc_tds - mean(data$sc_tds))^2)


mod <- lm(sc_att ~ 0 + avg_sc_att + def_avg_sc_att, data)
summary(mod)

mod2 <- lm(sc_att ~ avg_sc_att, data)
summary(mod2)

mod3 <- lm(sc_att ~ avg_sc_att + def_avg_sc_att, data)
summary(mod3)

mod4 <- lm(sc_yds ~ 0 + pred_sc_yds, data)
summary(mod4)


ggplot(data, aes(pred_sc_yds, sc_yds), position = "jitter") +
  geom_jitter() +
  xlim(0, 80) +
  ylim(0, 80)


data <- data %>% 
  mutate(pred_sc_att_f = avg_sc_att,
         pred_sc_yds_f = pred_sc_att_f*7.52,
         pred_sc_tds_f = pred_sc_att_f*0.03*0.8 + 0.2*avg_sc_tds)

