


team_offense_2022 <- read_csv("~/R Stuff/FantasyFootball 2.0/fullSeasonStats/2022/Team_Offense_2022.csv")
team_offense_2022[team_offense_2022 == "Washington Commanders"] <- "Washington Football Team"

team_offense_2022[4:25] <- team_offense_2022[4:25]/17

offense <- full_join(off_ratings, team_offense_2022, by = c("Team" = "Tm"))

team_defense_2022 <- read_csv("~/R Stuff/FantasyFootball 2.0/fullSeasonStats/2022/Team_Defense_2022.csv")
team_defense_2022[team_defense_2022 == "Washington Commanders"] <- "Washington Football Team"

team_defense_2022[4:25] <- team_defense_2022[4:25]/team_defense_2022$G

defense <- full_join(def_ratings, team_defense_2022, by = c("Team" = "Tm"))

x <- 0.5

# r_mean <- mean(defense$Def_Pas_Tds)
# 
# x <- 0.1
# 
# defense <- defense %>% 
#   mutate(new = (Def_Pas_Tds - r_mean)*x + r_mean,
#          resid = TD...14 - new,
#          sq_resid = resid^2,
#          var = (TD...14 - r_mean)^2)
# 
# r_squared = 1 - sum(defense$sq_resid)/sum(defense$var)
# r_squared

#regress to mean
r_mean <- mean(offense$Off_Pas_Yds_Rat)

offense <- offense %>% 
  mutate(n_Off_Pas_Yds_Rat = (Off_Pas_Yds_Rat - r_mean)*x + r_mean,
         resid = Yds...13 - n_Off_Pas_Yds_Rat,
         sq_resid = resid^2,
         var = (Yds...13 - r_mean)^2)

r_squared = 1 - sum(offense$sq_resid)/sum(offense$var)
r_squared

model <- lm(Yds...13 ~ 0 + n_Off_Pas_Yds_Rat, offense)
summary(model)

ggplot(offense, aes(Yds...13, Off_Pas_Yds_Rat), position = "jitter") +
  geom_jitter() +
  geom_abline(slope = 1, intercept = 0)

ggplot(offense, aes(Yds...13, n_Off_Pas_Yds_Rat), position = "jitter") +
  geom_jitter() +
  geom_abline(slope = 1, intercept = 0)

t <- offense %>% 
  select(Team, Off_Pas_Yds_Rat, n_Off_Pas_Yds_Rat)
