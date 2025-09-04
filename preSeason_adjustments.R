

#Years
# Years_Dataframe <- read_csv("~/R Stuff/FantasyFootball 2.0/Years_Dataframe.csv")
Past_Year <- This_Year_d-1
This_Year <- This_Year_d

###read files
d_player_percents_past_year <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", Past_Year, "/Week_19/Player_Percents.csv", sep = "")))
draft <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/draft/draft_", This_Year, ".csv", sep = ""))) %>% 
  clean_names() %>% 
  rename("team" = "tm") %>% 
  select(player, team, pos, pick)
starting_QBs <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/startingQBs/Starting_QBs_", This_Year, ".csv", sep = "")))
qb_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", Past_Year, "/Week_19/QB_Ratings.csv", sep = "")))
d_off_team_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", Past_Year, "/Week_19/Off_Team_Ratings.csv", sep = "")))
d_def_team_ratings <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", Past_Year, "/Week_19/Def_Team_Ratings.csv", sep = "")))

#Yahoo Week 1
Yahoo_Week_1 <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/Yahoo/", This_Year, "/Yahoo_Week_1.csv", sep = ""))) %>% 
  select(ID:Starting) %>% 
  mutate(player = paste(`First Name`, `Last Name`)) %>% 
  filter(!is.na(ID)) %>% 
  rename("team" = "Team") %>% 
  rename("opp" = "Opponent")

#player names func
Yahoo_Week_1 <- player_names_func(Yahoo_Week_1)
d_player_percents_past_year <- player_names_func(d_player_percents_past_year)
draft <- player_names_func(draft)


####Player Percents####
#Draftees player percents
#tested a while ago
draftees <- draft %>% 
  filter(pos %in% c("QB", "RB", "WR", "TE")) %>% 
  mutate(rus_rep = ifelse(pos == "QB", 0.1, ifelse(pos == "RB", 0.76*exp(-0.00675*pick), 0)),
         rec_rep = ifelse(pos == "WR", 0.29*exp(-0.0106*pick), ifelse(pos == "RB", 0.13*exp(-0.0045*pick), ifelse(pos == "TE", 0.26*exp(-0.0114*pick), 0)))) %>% 
  mutate(rus_att_per = rus_rep,
         rus_yds_per = rus_rep,
         rus_tds_per = rus_rep,
         tgt_per = rec_rep,
         rec_per = rec_rep,
         rec_yds_per = rec_rep,
         rec_tds_per = rec_rep) %>% 
  mutate(py_games_played = 0,
         games_played = 0,
         touches = 0,
         fmb = 0) %>% 
  mutate(py_qb_fl = 0.21,
         py_fl_per_tou = 0.007) %>% 
  select(!rus_rep:rec_rep) %>% 
  select(!pick)

#Past Year Player Percents
player_percents_past_year <- d_player_percents_past_year 

#Fumbles
avg_qb_fl <- player_percents_past_year %>% 
  filter(pos == "QB") %>% 
  summarize(
    avg_fl = sum(fmb, na.rm = TRUE) / sum(games_played, na.rm = TRUE)
  ) %>% 
  pull(avg_fl)

avg_fl_per_tou <- player_percents_past_year %>% 
  filter(pos != "QB") %>% 
  summarize(
    avg_fl = sum(fmb, na.rm = TRUE) / sum(touches, na.rm = TRUE)
  ) %>% 
    pull(avg_fl)

player_percents_past_year <- player_percents_past_year %>% 
  mutate(py_qb_fl = ifelse(pos == "QB", (fmb/games_played)*(games_played/22) + avg_qb_fl*(22-games_played)/22, 0),
         py_fl_per_tou = ifelse(pos != "QB", (fmb/600) + avg_fl_per_tou*(600-touches)/600, 0))

#New Season
player_percents_past_year <- player_percents_past_year %>% 
  mutate(py_games_played = games_played,
         games_played = 0,
         fmb = 0,
         touches = 0)

#rename it to match past year player percents
names(player_percents_past_year) <- sub("^adj_", "", names(player_percents_past_year))

#rbind
draftees <- draftees[, names(player_percents_past_year)]

player_percents_past_year <- rbind(player_percents_past_year, draftees)

#Combine with yahoo
player_percents <- player_percents_past_year %>% 
  select(!team)

player_percents <- full_join(player_percents, Yahoo_Week_1, by = c("player")) %>% 
  select(player, pos, team, games_played:py_fl_per_tou) %>% 
  filter(!is.na(pos))

#get only top players
#6 WR
#4 RB
#3 TE
#we will consider only players with high percents already
#players with percents outside the top will be adjusted the same rate as the top players

#NA team players
na_team_player_percents <- player_percents %>% 
  filter(is.na(team))

#value for comparison
player_percents <- player_percents %>% 
  mutate(per_value = ifelse(pos == "WR", rec_yds_per, ifelse(pos == "RB", rus_yds_per, ifelse(pos == "TE", rec_yds_per, 0))))

#get only top players
top_player_percents <- player_percents %>%
  filter(!is.na(team)) %>% 
  group_by(team, pos) %>%
  mutate(rank = row_number(desc(per_value))) %>%
  filter(
    (pos == "WR" & rank <= 6) |
      (pos == "RB" & rank <= 4) |
      (pos == "TE" & rank <= 3) |
      !(pos %in% c("WR", "RB", "TE"))   # keep all other positions
  ) %>%
  ungroup() %>%
  select(-rank) %>% # drop helper column if not needed
  filter(player %in% starting_QBs$player | pos != "QB") #starting qbs only 


#by team
top_player_percents_by_team <- top_player_percents %>% 
  group_by(team) %>% 
  summarise(across(rus_att_per:rec_tds_per, sum,
                   .names = "tot_{.col}"),
            .groups = 'drop') 

#adjust values
top_player_percents <- top_player_percents %>% 
  left_join(top_player_percents_by_team, by = "team")

adj <- function(df, cols){
  #rcombine
  for(col in cols){
    df[, paste("adj_", col, "_per", sep = "")] <- df[, paste(col, "_per", sep = "")] / df[, paste("tot_", col, "_per", sep = "")]
  }
  
  return(df)
}

cols <- c("rus_att", "rus_yds", "rus_tds",
          "tgt", "rec", "rec_yds", "rec_tds")

adj_top_player_percents <- adj(top_player_percents, cols)

#non-top player percents
non_top_player_percents <- player_percents %>% 
  filter(!is.na(team)) %>% 
  filter(!(player %in% adj_top_player_percents$player)) %>% 
  left_join(top_player_percents_by_team, by = "team")

adj_non_top_player_percents <- adj(non_top_player_percents, cols)

#rbind and select
adj_player_percents <- rbind(adj_top_player_percents, adj_non_top_player_percents)
adj_player_percents <- adj_player_percents %>% 
  select(player:fmb, adj_rus_att_per:adj_rec_tds_per, py_qb_fl, py_fl_per_tou)

#rejoing NAs
colnames(na_team_player_percents) <- colnames(adj_player_percents)
adj_player_percents <- rbind(adj_player_percents, na_team_player_percents)



####QBs####

###regress ratings
#find averages
enough_games <- qb_ratings %>% 
  filter(games_played >= 6)

avg_pas_att <- mean(enough_games$pas_att_rat)
avg_cmp <- mean(enough_games$cmp_rat)
avg_pas_yds <- mean(enough_games$pas_yds_rat)
avg_pas_tds <- mean(enough_games$pas_tds_rat)
avg_int <- mean(enough_games$int_rat)

pas_att_val <- 0.65
cmp_val <- 0.68
pas_yds_val <- 0.65
pas_tds_val <- 0.4
int_val <- 0.26

#regress
reg_qb_ratings <- qb_ratings %>% 
  mutate(
    pas_att_rat = pas_att_val * pas_att_rat + (1 - pas_att_val) * avg_pas_att,
    cmp_rat     = cmp_val     * cmp_rat     + (1 - cmp_val)     * avg_cmp,
    pas_yds_rat = pas_yds_val * pas_yds_rat + (1 - pas_yds_val) * avg_pas_yds,
    pas_tds_rat = pas_tds_val * pas_tds_rat + (1 - pas_tds_val) * avg_pas_tds,
    int_rat     = int_val     * int_rat     + (1 - int_val)     * avg_int
  )

###draftees
#well tested
qb_draftees <- draft %>% 
  filter(pos == "QB") %>% 
  mutate(py_games_played = 0,
         games_played = 0,
         pas_att_rat = 35 - 0.9*log(pick),
         cmp_rat = 19 + 3.1*(1/sqrt(pick)),
         pas_yds_rat = 245 - 8.5*log(pick),
         pas_tds_rat = 1.25,
         int_rat = 0.95) %>% 
  select(!c(pick, pos))

###join
qb_ratings <- rbind(reg_qb_ratings, qb_draftees)

###update teams and fix games
qb_ratings <- qb_ratings %>% 
  select(!team) %>% 
  left_join(Yahoo_Week_1, by = "player") %>% 
  select(player, team, py_games_played:int_rat) %>% 
  mutate(py_games_played = games_played,
         games_played = 0)


####Off Team Ratings####
off_team_ratings <- d_off_team_ratings

# 1. Compute league averages in one step
off_team_avgs <- off_team_ratings %>% 
  summarize(across(ends_with("_rat"), \(x) mean(x, na.rm = TRUE)))


# 2. Store shrinkage weights in a named vector
weights <- c(
  off_rus_att_rat = 0.4,
  off_rus_yds_rat = 0.38,
  off_rus_tds_rat = 0.32,
  off_pas_att_rat = 0.43,
  off_cmp_rat     = 0.54,
  off_pas_yds_rat = 0.55,
  off_pas_tds_rat = 0.44
)

# 3. Apply shrinkage formula dynamically
off_team_ratings <- off_team_ratings %>%
  mutate(across(names(weights), 
                \(x) weights[cur_column()] * x +
                  (1 - weights[cur_column()]) * off_team_avgs[[cur_column()]]))


####Def Team Ratings####
def_team_ratings <- d_def_team_ratings

# 1. Compute league averages in one step
def_team_avgs <- def_team_ratings %>% 
  summarize(across(ends_with("_rat"), \(x) mean(x, na.rm = TRUE)))


# 2. Store shrinkage weights in a named vector
weights <- c(
  def_rus_att_rat = 0.37,
  def_rus_yds_rat = 0.33,
  def_rus_tds_rat = 0.23,
  def_pas_att_rat = 0.26,
  def_cmp_rat     = 0.41,
  def_pas_yds_rat = 0.37,
  def_pas_tds_rat = 0.21
)

# 3. Apply shrinkage formula dynamically
def_team_ratings <- def_team_ratings %>%
  mutate(across(names(weights), 
                \(x) weights[cur_column()] * x +
                  (1 - weights[cur_column()]) * def_team_avgs[[cur_column()]]))





####Write Csv####
write_csv(adj_player_percents, eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Player_Percents.csv", sep = "")))
write_csv(qb_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/QB_Ratings.csv", sep = "")))
write_csv(off_team_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Off_Team_Ratings.csv", sep = "")))
write_csv(def_team_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/Def_Team_Ratings.csv", sep = "")))

write_csv(adj_player_percents, eval(paste("~/R Stuff/FantasyFootball 2.0/WeeklyRatings/", This_Year, "/Week_1/Player_Percents.csv", sep = "")))
write_csv(qb_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/WeeklyRatings/", This_Year, "/Week_1/QB_Ratings.csv", sep = "")))
write_csv(off_team_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/WeeklyRatings/", This_Year, "/Week_1/Off_Team_Ratings.csv", sep = "")))
write_csv(def_team_ratings, eval(paste("~/R Stuff/FantasyFootball 2.0/WeeklyRatings/", This_Year, "/Week_1/Def_Team_Ratings.csv", sep = "")))



