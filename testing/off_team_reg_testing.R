d_off_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/off_team_stats.csv", sep = ""))) %>% 
  clean_names()

off_stats <- d_off_stats %>% 
  select(team, season, g, att_15, cmp_14, yds_18, td_19, att_32, yds_33, td_35)

colnames(off_stats) <- c("team", "season", "games", "pas_att", "cmp", "pas_yds", "pas_tds", "rus_att", "rus_yds", "rus_tds")

off_stats[, 4:10] <- off_stats[, 4:10]/off_stats$games

off_stats <- off_stats %>% 
  mutate(past_season = season - 1)

off_stats$team[off_stats$team == "STL"] <- "LAR"
off_stats$team[off_stats$team == "OAK"] <- "LV"
off_stats$team[off_stats$team == "SDG"] <- "LAC"

combined <- left_join(off_stats, off_stats, by = c("team", "past_season" = "season"), suffix = c("", "_prev"))

mod <- lm(rus_tds ~ rus_tds_prev, combined)
summary(mod)

mean(combined$pas_att)

mod$coefficients[1]
mod$coefficients[1]/(1 - mod$coefficients[2])

current <- combined %>% 
  filter(season == 2024)

mean(current$pas_yds)
