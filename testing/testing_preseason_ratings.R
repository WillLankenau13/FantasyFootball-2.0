########


off_2022 <- read_csv("~/R Stuff/FantasyFootball 2.0/team_offense_2022.csv") %>% 
  mutate(Tm = ifelse(Tm == "Washington Commanders", "Washington Football Team", Tm))

off_2021 <- read_csv("~/R Stuff/FantasyFootball 2.0/fullSeasonStats/2021/Team_Offense_2021.csv")

#per game
off_2022[5:25] <- off_2022[5:25]/off_2022$G
off_2021[5:25] <- off_2021[5:25]/off_2021$G

colnames(off_2022) <- paste(colnames(off_2022), "2022", sep = "_")
colnames(off_2021) <- paste(colnames(off_2021), "2021", sep = "_")

off_ratings

offense <- full_join(off_ratings, off_2022, by = c("Team" = "Tm_2022")) %>% 
  full_join(off_2021, by = c("Team" = "Tm_2021"))


ggplot(offense, aes(total_tds_2022, Off_Pas_Yds_Rat + Off_Rus_Yds_Rat), position = "jitter") +
  geom_jitter() +
  geom_abline(slope = 1, intercept = 0)

ggplot(offense, aes(TD...20_2022, pred_rus_tds), position = "jitter") +
  geom_jitter() +
  geom_abline(slope = 1, intercept = 0)

ggplot(offense, aes(TD...14_2022, Off_Pas_Tds_Rat), position = "jitter") +
  geom_jitter() +
  geom_abline(slope = 1, intercept = 0)


offense <- offense %>% 
  mutate(total_tds_2022 = TD...20_2022 + TD...14_2022)

model <- lm(TD...14_2022 ~ Off_Pas_Tds_Rat, offense)
summary(model)

offense <- offense %>% 
  add_predictions(model) %>% 
  rename("pred_pas_tds" = "pred")

model2 <- lm(total_tds_2022 ~ Off_Pas_Yds_Rat + Off_Rus_Yds_Rat, offense)
summary(model2)

offense <- offense %>% 
  add_predictions(model2) %>% 
  rename("pred_tot_tds" = "pred") %>% 
  mutate(pred_rus_tds = pred_tot_tds - pred_pas_tds)


model3 <- lm(TD...20_2022 ~ pred_rus_tds, offense)
summary(model3)


summary(model)
