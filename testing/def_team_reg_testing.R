
#file is a little messed up; has some offensive values
d_def_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/def_team_stats.csv", sep = ""))) %>% 
  clean_names()

def_stats <- d_def_stats %>% 
  select(team, season, g, att_34, cmp_33, yds_36, td_37, att_42, yds_43, td_45)

colnames(def_stats) <- c("team", "season", "games", "pas_att", "cmp", "pas_yds", "pas_tds", "rus_att", "rus_yds", "rus_tds")

def_stats[, 4:10] <- def_stats[, 4:10]/def_stats$games

def_stats <- def_stats %>% 
  mutate(past_season = season - 1)

def_stats$team[def_stats$team == "STL"] <- "LAR"
def_stats$team[def_stats$team == "OAK"] <- "LV"
def_stats$team[def_stats$team == "SDG"] <- "LAC"

combined <- left_join(def_stats, def_stats, by = c("team", "past_season" = "season"), suffix = c("", "_prev"))

mod <- lm(pas_tds ~ pas_tds_prev, combined)
summary(mod)

mean(combined$pas_att)

mod$coefficients[1]
mod$coefficients[1]/(1 - mod$coefficients[2])

current <- combined %>% 
  filter(season == 2024)

mean(current$pas_yds)
