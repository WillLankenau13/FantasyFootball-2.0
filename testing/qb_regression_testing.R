
d_qb_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/qb_seasons.csv", sep = ""))) %>% 
  clean_names()

qb_stats <- d_qb_stats %>% 
  select(player, team, gs, season, att, cmp_11, yds_15, td, int, off_percent_41)

colnames(qb_stats) <- c("player", "team", "games_started", "season", "pas_att", "cmp", "pas_yds", "pas_tds", "int", "snp_per")

qb_stats[, 5:9] <- qb_stats[, 5:9]/qb_stats$games_started

qb_stats <- qb_stats %>% 
  filter(games_started > 5) %>% 
  mutate(past_season = season - 1)

combined <- left_join(qb_stats, qb_stats, by = c("player", "past_season" = "season"), suffix = c("", "_prev"))

combined <- combined %>% 
  filter(!is.na(team_prev))

mod <- lm(int ~ int_prev, combined)
summary(mod)

mean(combined$pas_tds)

mod$coefficients[1]
mod$coefficients[1]/(1 - mod$coefficients[2])

current <- combined %>% 
  filter(season == 2024)

mean(current$pas_yds)
