
#####Create Lists#####
create_2d_list <- function(){
  df_list <- vector("list", 5)
  names(df_list) <- 2021:2025
  
  for (year in names(df_list)) {
    df_list[[year]] <- vector("list", 19)
    names(df_list[[year]]) <- paste0("Week_", 1:18)
  }
  
  return(df_list)
}

create_1d_list <- function(){
  df_list <- vector("list", 5)
  names(df_list) <- 2021:2025
  return(df_list)
}

yahoo_2d <- create_2d_list()
player_stats_2d <- create_2d_list()
starting_qbs_2d <- create_2d_list()

draft_1d <- create_1d_list()
starting_qbs_1d <- create_1d_list()

depth_chart_2025 <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/depthCharts/2025/Week_1_Depth_Chart.csv", sep = "")))
teams <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/utils/teams.csv", sep = "")))

#####Initialize#####
###Yearly Constants
for (year in c("2022", "2023", "2024", "2025")){
  draft_1d[[year]] <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/draft/draft_", year, ".csv", sep = ""))) %>% 
    clean_names() %>% 
    rename("team" = "tm") %>% 
    select(player, team, pos, pick)
  starting_qbs_1d[[year]] <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/startingQBs/Starting_QBs_", year, ".csv", sep = "")))
}

###Weekly Constants
for (year in c("2022", "2023", "2024", "2025")){
  for (week in 1:18){
    yahoo_2d[[year]][[paste0("Week_", week)]] <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/Yahoo/", year, "/Yahoo_Week_", week, ".csv", sep = "")))
    
    player_stats_2d[[year]][[paste0("Week_", week)]] <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyStats/", year, "/byWeek/Week_", week, "_Stats.csv", sep = "")))

    starting_qbs_2d[[year]][[paste0("Week_", week)]] <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/startingQBs/", year, "/Week_", week, "_Starting_QBs.csv", sep = "")))
  }
}

###2021 (hardcoded year)
week19_player_percents_2021 <- read_csv("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyRatings/2021/Week_19/Player_Percents.csv")
week19_qb_ratings_2021 <- read_csv("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyRatings/2021/Week_19/QB_Ratings.csv")
week19_off_team_ratings_2021 <- read_csv("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyRatings/2021/Week_19/Off_Team_Ratings.csv")
week19_def_team_ratings_2021 <- read_csv("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyRatings/2021/Week_19/Def_Team_Ratings.csv")



#####Stuff#####
# This_Year <- 2022
# past_week <- 1

#get_weekly_loss(This_Year, past_week)
#weekly_loss_2d[["2022"]][[paste0("Week_", past_week)]]

# preseason_adjustments(2022, initial_params)
# make_predictions(2022, 1, initial_params)
# update_ratings(2022, 1, initial_params)


#####Preseason Adjustments#####
preseason_adjustments <- function(This_Year, params, player_percents_2d, qb_ratings_2d, off_team_ratings_2d, def_team_ratings_2d){
  #Past Year
  Past_Year <- This_Year-1
  
  This_Year_char <- as.character(This_Year)
  Past_Year_char <- as.character(Past_Year)
  
  
  ###read files
  d_player_percents_past_year <- player_percents_2d[[Past_Year_char]][["Week_19"]]
  draft <- draft_1d[[This_Year_char]]
  starting_QBs <-starting_qbs_1d[[This_Year_char]]
  qb_ratings <- qb_ratings_2d[[Past_Year_char]][["Week_19"]]
  d_off_team_ratings <- off_team_ratings_2d[[Past_Year_char]][["Week_19"]]
  d_def_team_ratings <- def_team_ratings_2d[[Past_Year_char]][["Week_19"]]
  
  #depth chart
  if(This_Year >= 2025){
    d_depth_chart <- depth_chart_2025
    
    depth_chart <- d_depth_chart %>%
      select(2)
    colnames(depth_chart) <- c("player")
    
    depth_chart <- depth_chart %>%
      filter(!is.na(player)) %>%
      filter(!(player %in% starting_QBs$team))
    
    depth_chart <- player_names_func(depth_chart)
  }
  
  #Yahoo Week 1
  Yahoo_Week_1 <- yahoo_2d[[This_Year_char]][["Week_1"]] %>% 
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
           games_played = 0) %>% 
    select(!rus_rep:rec_rep) %>% 
    select(!pick)
  
  #Past Year Player Percents
  player_percents_past_year <- d_player_percents_past_year 
  
  #New Season
  player_percents_past_year <- player_percents_past_year %>% 
    mutate(py_games_played = games_played,
           games_played = 0)
  
  # #rename it to match past year player percents
  # names(player_percents_past_year) <- sub("^adj_", "", names(player_percents_past_year))
  
  #rbind
  draftees <- draftees[, names(player_percents_past_year)]
  
  player_percents_past_year <- rbind(player_percents_past_year, draftees)
  
  #Combine with yahoo
  player_percents <- player_percents_past_year %>% 
    select(!team)
  
  player_percents <- full_join(player_percents, Yahoo_Week_1, by = c("player", "pos" = "Position")) %>% 
    select(player, pos, team, py_games_played:rec_tds_per) %>% 
    filter(!is.na(rus_att_per))
  
  #NA team players
  na_team_player_percents <- player_percents %>% 
    filter(is.na(team))
  
  if(This_Year >= 2025){
    dc_player_percents <- player_percents %>% 
      filter(!is.na(team)) %>% 
      filter(player %in% starting_QBs$player | pos != "QB") %>% 
      filter(player %in% depth_chart$player)
    
    
    #by team
    dc_player_percents_by_team <- dc_player_percents %>% 
      group_by(team) %>% 
      summarise(across(rus_att_per:rec_tds_per, sum,
                       .names = "tot_{.col}"),
                .groups = 'drop') 
    
    #adjust values
    dc_player_percents <- dc_player_percents %>% 
      left_join(dc_player_percents_by_team, by = "team")
    
    adj <- function(df, cols){
      #rcombine
      for(col in cols){
        df[, paste("adj_", col, "_per", sep = "")] <- df[, paste(col, "_per", sep = "")] / df[, paste("tot_", col, "_per", sep = "")]
      }
      
      return(df)
    }
    
    cols <- c("rus_att", "rus_yds", "rus_tds",
              "tgt", "rec", "rec_yds", "rec_tds")
    
    adj_dc_player_percents <- adj(dc_player_percents, cols)
    
    #non-top player percents
    non_dc_player_percents <- player_percents %>% 
      filter(!is.na(team)) %>% 
      filter(!(player %in% adj_dc_player_percents$player)) %>% 
      left_join(dc_player_percents_by_team, by = "team")
    
    adj_non_dc_player_percents <- adj(non_dc_player_percents, cols)
    
    adj_non_dc_player_percents <- adj_non_dc_player_percents %>% 
      mutate(adj_rus_att_per = pmin(adj_rus_att_per, 0.05),
             adj_rus_yds_per = pmin(adj_rus_yds_per, 0.05),
             adj_rus_tds_per = pmin(adj_rus_tds_per, 0.05),
             adj_tgt_per = pmin(adj_tgt_per, 0.03),
             adj_rec_per = pmin(adj_rec_per, 0.03),
             adj_rec_yds_per = pmin(adj_rec_yds_per, 0.03),
             adj_rec_tds_per = pmin(adj_rec_tds_per, 0.03))
    
    #rbind
    adj_player_percents <- rbind(adj_dc_player_percents, adj_non_dc_player_percents)
  } else {
    #get only top players
    #6 WR
    #4 RB
    #3 TE
    #we will consider only players with high percents already
    #players with percents outside the top will be adjusted the same rate as the top players
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
      select(-rank) %>% # drop helper column
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
    
    adj_non_top_player_percents <- adj_non_top_player_percents %>% 
      mutate(adj_rus_att_per = pmin(adj_rus_att_per, 0.05),
             adj_rus_yds_per = pmin(adj_rus_yds_per, 0.05),
             adj_rus_tds_per = pmin(adj_rus_tds_per, 0.05),
             adj_tgt_per = pmin(adj_tgt_per, 0.03),
             adj_rec_per = pmin(adj_rec_per, 0.03),
             adj_rec_yds_per = pmin(adj_rec_yds_per, 0.03),
             adj_rec_tds_per = pmin(adj_rec_tds_per, 0.03))
    
    #rbind
    adj_player_percents <- rbind(adj_top_player_percents, adj_non_top_player_percents)
  }
  
  
  #select
  adj_player_percents <- adj_player_percents %>% 
    select(player:games_played, adj_rus_att_per:adj_rec_tds_per)
  
  #rejoing NAs
  colnames(na_team_player_percents) <- colnames(adj_player_percents)
  adj_player_percents <- rbind(adj_player_percents, na_team_player_percents)
  
  #rename columns
  names(adj_player_percents) <- sub("^adj_", "", names(adj_player_percents))
  
  ####QBs####
  
  ###regress ratings
  #find averages
  enough_games <- qb_ratings %>% 
    filter(games_played >= 6) #filter is only to find averages
  
  avg_pas_att <- mean(enough_games$pas_att_rat)
  avg_cmp <- mean(enough_games$cmp_rat)
  avg_pas_yds <- mean(enough_games$pas_yds_rat)
  avg_pas_tds <- mean(enough_games$pas_tds_rat)
  avg_int <- mean(enough_games$int_rat)
  avg_sc_att <- mean(enough_games$sc_att_rat)
  avg_sc_yds <- mean(enough_games$sc_yds_rat)
  avg_sc_tds <- mean(enough_games$sc_tds_rat)
  
  pas_att_val <- 0.65
  cmp_val <- 0.68
  pas_yds_val <- 0.65
  pas_tds_val <- 0.4
  int_val <- 0.26
  sc_att_val <- 0.75 #not tested, based on scramble testing optimal percent of average attempts and league average
  sc_yds_val <- 0.75 #not tested
  sc_tds_val <- 0.75 #not tested
  
  #regress
  reg_qb_ratings <- qb_ratings %>% 
    mutate(
      pas_att_rat = pas_att_val * pas_att_rat + (1 - pas_att_val) * avg_pas_att,
      cmp_rat     = cmp_val     * cmp_rat     + (1 - cmp_val)     * avg_cmp,
      pas_yds_rat = pas_yds_val * pas_yds_rat + (1 - pas_yds_val) * avg_pas_yds,
      pas_tds_rat = pas_tds_val * pas_tds_rat + (1 - pas_tds_val) * avg_pas_tds,
      int_rat     = int_val     * int_rat     + (1 - int_val)     * avg_int,
      sc_att_rat  = sc_att_val  * sc_att_rat  + (1 - sc_att_val)  * avg_sc_att,
      sc_yds_rat  = sc_yds_val  * sc_yds_rat  + (1 - sc_yds_val)  * avg_sc_yds,
      sc_tds_rat  = sc_tds_val  * sc_tds_rat  + (1 - sc_tds_val)  * avg_sc_tds
    )
  
  ###draftees
  #well tested except scramble
  qb_draftees <- draft %>% 
    filter(pos == "QB") %>% 
    mutate(py_games_played = 0,
           games_played = 0,
           pas_att_rat = 35 - 0.9*log(pick),
           cmp_rat = 19 + 3.1*(1/sqrt(pick)),
           pas_yds_rat = 245 - 8.5*log(pick),
           pas_tds_rat = 1.25,
           int_rat = 0.95,
           sc_att_rat = 1.4,
           sc_yds_rat = 12,
           sc_tds_rat = 0.05) %>% 
    select(!c(pick, pos))
  
  ###join
  qb_ratings <- rbind(reg_qb_ratings, qb_draftees)
  
  #qb yahoo
  yahoo_qb <- Yahoo_Week_1 %>% 
    filter(Position == "QB")
  
  ###update teams and fix games
  qb_ratings <- qb_ratings %>% 
    select(!team) %>% 
    left_join(yahoo_qb, by = "player") %>% 
    select(player, team, py_games_played:sc_tds_rat) %>% 
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
  
  ####Return####
  return(list(
    player_percents = adj_player_percents,
    qb_ratings = qb_ratings,
    off_team_ratings = off_team_ratings,
    def_team_ratings = def_team_ratings
  ))
}

#####Make Predictions#####
make_predictions <- function(This_Year, upcoming_week, params, player_percents_2d, qb_ratings_2d, off_team_ratings_2d, def_team_ratings_2d){
  This_Year_char <- as.character(This_Year)
  
  #inactives list
  inactive_designations <- c("O", "SUSP", "PUP", "IR", "NFI", "D", "PI")
  #PI is projected inactive
  
  #import files
  player_percents <- player_percents_2d[[This_Year_char]][[paste0("Week_", upcoming_week)]]
  QB_ratings <- qb_ratings_2d[[This_Year_char]][[paste0("Week_", upcoming_week)]]
  off_team_ratings <- off_team_ratings_2d[[This_Year_char]][[paste0("Week_", upcoming_week)]]
  def_team_ratings <- def_team_ratings_2d[[This_Year_char]][[paste0("Week_", upcoming_week)]]
  starting_qbs <- starting_qbs_2d[[This_Year_char]][[paste0("Week_", upcoming_week)]]
  # active_players <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/activePlayers/", This_Year, "/Week_", upcoming_week, "_Active_Players.csv", sep = ""))) %>%
  #   mutate(active = 1)
  
  # active_players <- player_names_func(active_players)
  
  #yahoo
  yahoo <- yahoo_2d[[This_Year_char]][[paste0("Week_", upcoming_week)]] %>% 
    select(ID:Starting) %>% 
    mutate(player = paste(`First Name`, `Last Name`)) %>% 
    filter(!is.na(ID)) %>% 
    filter(!(`Injury Status` %in% inactive_designations)) %>% 
    rename("pos" = "Position")
  
  yahoo <- player_names_func(yahoo)
  
  yahoo <- yahoo %>% 
    left_join(teams, by = c("Team" = "Yahoo")) %>% 
    select(ID:pos, Short_Name, Opponent:player) %>% 
    rename("Team" = "Short_Name") %>% 
    left_join(teams, by = c("Opponent" = "Yahoo")) %>% 
    select(ID:pos, Team, Short_Name, Game:player) %>% 
    rename("Opponent" = "Short_Name") %>% 
    filter(pos != "DEF")
  
  #active_players
  # yahoo <- yahoo %>%
  #   full_join(active_players, by = c("player", "Team" = "team", "pos")) %>%
  #   filter(Position == "DEF" | active == 1 | `Injury Status` == "Q") %>%
  #   select(!active)
  
  #injury status
  injury_status <- yahoo %>% 
    select(player, pos, `Injury Status`)
  colnames(injury_status) = c("player", "pos", "injury_status")
  
  
  ####QB Adjustment####
  starting_qbs <- player_names_func(starting_qbs)
  
  QB_ratings_no_team <- QB_ratings %>% 
    select(!team)
  starting_qb_ratings <- starting_qbs %>% 
    left_join(QB_ratings_no_team, by = c("QB" = "player"))
  
  yahoo <- yahoo %>% 
    full_join(starting_qbs, by = c("Team" = "team")) %>% 
    mutate(startingQB = ifelse(player == QB, 1, 0)) %>% 
    filter(pos != "QB" | startingQB == 1)
  
  ##Replacement
  pas_att_rep = 30
  cmp_rep = 18
  pas_yds_rep = 210
  pas_tds_rep = 1
  int_rep = 1
  sc_att_rep = 1.4
  sc_yds_rep = 12
  sc_tds_rep = 0.05
  
  starting_qb_ratings <- starting_qb_ratings %>% 
    mutate(pas_att_rat = ifelse(is.na(pas_att_rat), pas_att_rep, pas_att_rat),
           cmp_rat = ifelse(is.na(cmp_rat), cmp_rep, cmp_rat),
           pas_yds_rat = ifelse(is.na(pas_yds_rat), pas_yds_rep, pas_yds_rat),
           pas_tds_rat = ifelse(is.na(pas_tds_rat), pas_tds_rep, pas_tds_rat),
           int_rat = ifelse(is.na(int_rat), int_rep, int_rat),
           sc_att_rat = ifelse(is.na(sc_att_rat), sc_att_rep, sc_att_rat),
           sc_yds_rat = ifelse(is.na(sc_yds_rat), sc_yds_rep, sc_yds_rat),
           sc_tds_rat = ifelse(is.na(sc_tds_rat), sc_tds_rep, sc_tds_rat),
           games_played = ifelse(is.na(games_played), 0, games_played),
           py_games_played = ifelse(is.na(py_games_played),0, py_games_played))
  
  #tested
  qb_adj <- params["qb_adj"]
  #qb_adj <- 0.7
  
  QB_adj_off_team_ratings <- left_join(off_team_ratings, starting_qb_ratings, by = c("team")) %>%
    mutate(off_cmp_rat = off_cmp_rat*(1-qb_adj) + cmp_rat*qb_adj,
           off_pas_att_rat = off_pas_att_rat*(1-qb_adj) + pas_att_rat*qb_adj,
           off_pas_yds_rat = off_pas_yds_rat*(1-qb_adj) + pas_yds_rat*qb_adj,
           off_pas_tds_rat = off_pas_tds_rat*(1-qb_adj) + pas_tds_rat*qb_adj,
           off_int_rat = off_int_rat*(1-qb_adj) + int_rat*qb_adj) %>%
    select(team:off_int_rat, sc_att_rat:sc_tds_rat, off_rus_att_rat:off_rus_tds_rat)
  
  ####update ratings for active rushers and receivers####
  adjusted <- left_join(yahoo, player_percents, by = c("player", "pos")) %>% 
    clean_names() %>% 
    select(player, pos, team, opponent, injury_status, py_games_played:rec_tds_per)
  
  adjusted[, 6:14][is.na(adjusted[, 6:14])] <- 0
  
  #filter out qb
  #qbs unaffected by injuries and inactives
  qb <- adjusted %>% 
    filter(pos == "QB")
  qb_vals <- qb
  colnames(qb)[8:14] <- paste0("adj_", colnames(qb)[8:14])
  colnames(qb_vals)[8:14] <- paste0("qb_", colnames(qb_vals)[8:14])
  qb_vals <- qb_vals %>% 
    select(player, team, qb_rus_att_per:qb_rus_tds_per) %>% 
    full_join(starting_qbs, by = c("player" = "QB", "team")) %>% 
    select(!player)
  qb_vals[is.na(qb_vals[])] <- 0
  
  adjusted <- adjusted %>% 
    filter(pos != "QB")
  
  #Get percent by team
  adjusted_by_team <- adjusted %>% 
    group_by(team) %>% 
    summarise(tot_rus_att_per = sum(rus_att_per),
              tot_rus_yds_per = sum(rus_yds_per),
              tot_rus_tds_per = sum(rus_tds_per),
              tot_tgt_per = sum(tgt_per),
              tot_rec_per = sum(rec_per),
              tot_rec_yds_per = sum(rec_yds_per),
              tot_rec_tds_per = sum(rec_tds_per))
  
  #Adjust individuals
  adjusted <- adjusted %>% 
    full_join(adjusted_by_team, by = c("team")) %>% 
    left_join(qb_vals, by = "team") %>% 
    mutate(adj_rus_att_per = (1-qb_rus_att_per)*rus_att_per/tot_rus_att_per,
           adj_rus_yds_per = (1-qb_rus_yds_per)*rus_yds_per/tot_rus_yds_per,
           adj_rus_tds_per = (1-qb_rus_tds_per)*rus_tds_per/tot_rus_tds_per,
           adj_tgt_per = tgt_per/tot_tgt_per,
           adj_rec_per = rec_per/tot_rec_per,
           adj_rec_yds_per = rec_yds_per/tot_rec_yds_per,
           adj_rec_tds_per = rec_tds_per/tot_rec_tds_per) %>% 
    select(player:injury_status, py_games_played, games_played, adj_rus_att_per:adj_rec_tds_per)
  
  
  #bring back qbs
  adjusted_by_team <- adjusted_by_team %>% 
    left_join(qb_vals, by = "team") %>% 
    mutate(tot_rus_att_per = tot_rus_att_per + qb_rus_att_per,
           tot_rus_yds_per = tot_rus_yds_per + qb_rus_yds_per,
           tot_rus_tds_per = tot_rus_tds_per + qb_rus_tds_per) %>% 
    select(team:tot_rec_tds_per)
  
  adjusted <- rbind(adjusted, qb)
  
  ####update offensive ratings####
  # rus_att_upd_coef <- params["rus_att_upd_coef"]
  # rus_yds_upd_coef <- params["rus_yds_upd_coef"]
  # rus_tds_upd_coef <- params["rus_tds_upd_coef"]
  # pas_att_upd_coef <- params["pas_att_upd_coef"]
  # cmp_upd_coef <- params["cmp_upd_coef"]
  # pas_yds_upd_coef <- params["pas_yds_upd_coef"]
  # pas_tds_upd_coef <- params["pas_tds_upd_coef"]
  
  rus_att_upd_coef <-  0
  rus_yds_upd_coef <-  0
  rus_tds_upd_coef <-  0
  pas_att_upd_coef <-  0.1
  cmp_upd_coef <-  0.4
  pas_yds_upd_coef <-  0.1
  pas_tds_upd_coef <-  0
  
  # rus_att <- 0.3
  # rus_yds <- 0.3
  # rus_tds <- 0.1
  # pas_att <- 0.1
  # cmp <- 0.1
  # pas_yds <- 0.2
  # pas_tds <- 0.2
  
  #No adjust for players
  # adjusted_off_team_ratings <- QB_adj_off_team_ratings %>%
  #   full_join(adjusted_by_team, by = c("team")) %>%
  #   transmute(team = team,
  #             adj_rus_att_rat = off_rus_att_rat,
  #             adj_rus_yds_rat = off_rus_yds_rat,
  #             adj_rus_tds_rat = off_rus_tds_rat,
  #             adj_pas_att_rat = off_pas_att_rat,
  #             adj_cmp_rat = off_cmp_rat,
  #             adj_pas_yds_rat = off_pas_yds_rat,
  #             adj_pas_tds_rat = off_pas_tds_rat,
  #             adj_int_rat = off_int_rat)
  
  #new
  adjusted_off_team_ratings <- QB_adj_off_team_ratings %>%
    full_join(adjusted_by_team, by = c("team")) %>%
    transmute(team = team,
              adj_cmp_rat = off_cmp_rat*((tot_rec_per-1)*cmp_upd_coef + 1),
              adj_pas_yds_rat = off_pas_yds_rat*((tot_rec_yds_per-1)*pas_yds_upd_coef + 1),
              adj_pas_tds_rat = off_pas_tds_rat*((tot_rec_tds_per-1)*pas_tds_upd_coef + 1),
              adj_int_rat = off_int_rat,
              adj_sc_att_rat = sc_att_rat,
              adj_sc_yds_rat = sc_yds_rat,
              adj_sc_tds_rat = sc_tds_rat,
              adj_rus_att_rat = off_rus_att_rat*((tot_rus_att_per-1)*rus_att_upd_coef + 1),
              adj_rus_yds_rat = off_rus_yds_rat*((tot_rus_yds_per-1)*rus_yds_upd_coef + 1),
              adj_rus_tds_rat = off_rus_tds_rat*((tot_rus_tds_per-1)*rus_tds_upd_coef + 1),
              adj_pas_att_rat = off_pas_att_rat*((tot_tgt_per-1)*pas_att_upd_coef + 1),
    )
  
  #old
  # adjusted_off_team_ratings <- QB_adj_off_team_ratings %>%
  #   full_join(adjusted_by_team, by = c("team")) %>%
  #   transmute(team = team,
  #             adj_rus_att_rat = off_rus_att_rat*((1-tot_rus_att_per)*rus_att + tot_rus_att_per),
  #             adj_rus_yds_rat = off_rus_yds_rat*((1-tot_rus_yds_per)*rus_yds + tot_rus_yds_per),
  #             adj_rus_tds_rat = off_rus_tds_rat*((1-tot_rus_tds_per)*rus_tds + tot_rus_tds_per),
  #             adj_pas_att_rat = off_pas_att_rat*((1-tot_tgt_per)*pas_att + tot_tgt_per),
  #             adj_cmp_rat = off_cmp_rat*((1-tot_rec_per)*cmp + tot_rec_per),
  #             adj_pas_yds_rat = off_pas_yds_rat*((1-tot_rec_yds_per)*pas_yds + tot_rec_yds_per),
  #             adj_pas_tds_rat = off_pas_tds_rat*((1-tot_rec_tds_per)*pas_tds + tot_rec_tds_per),
  #             adj_int_rat = off_int_rat)
  
  ####Get matchups####
  matchups <- yahoo %>% 
    filter(startingQB == 1) %>% 
    select(Team, Opponent) %>% 
    distinct()
  
  team_predictions <- matchups %>% 
    left_join(adjusted_off_team_ratings, by = c("Team" = "team")) %>% 
    left_join(def_team_ratings, by = c("Opponent" = "team")) %>% 
    clean_names()
  
  
  ####Tean Predictions####
  #combining coefficients
  #well tested
  # cmp_off_coef <- params["cmp_off_coef"]
  # pas_att_off_coef <- params["pas_att_off_coef"]
  # pas_yds_off_coef <- params["pas_yds_off_coef"]
  # pas_tds_off_coef <- params["pas_tds_off_coef"]
  # int_off_coef <- params["int_off_coef"]
  # rus_att_off_coef <- params["rus_att_off_coef"]
  # rus_yds_off_coef <- params["rus_yds_off_coef"]
  # rus_tds_off_coef <- params["rus_tds_off_coef"]
  
  cmp_off_coef <- 0.7
  pas_att_off_coef <- 0.7
  pas_yds_off_coef <- 0.8
  pas_tds_off_coef <- 0.8
  int_off_coef <- 0.7
  rus_att_off_coef <- 0.6
  rus_yds_off_coef <- 0.5
  rus_tds_off_coef <- 0.5
  
  
  #combine offense and defense
  combine_predictions <- function(df, col){
    #percent rating coefficient
    off_coef <- get(paste(col, "_off_coef", sep = ""))
    
    #rcombine
    df[, paste("team_", col, "_pred", sep = "")] <- ((df[, paste("adj_", col, "_rat", sep = "")]*off_coef) + (df[, paste("def_", col, "_rat", sep = "")]*(1 - off_coef)))
    
    return(df)
  }
  
  team_predictions <- combine_predictions(team_predictions, "pas_att")
  team_predictions <- combine_predictions(team_predictions, "cmp")
  team_predictions <- combine_predictions(team_predictions, "pas_yds")
  team_predictions <- combine_predictions(team_predictions, "pas_tds")
  team_predictions <- combine_predictions(team_predictions, "int")
  team_predictions <- combine_predictions(team_predictions, "rus_att")
  team_predictions <- combine_predictions(team_predictions, "rus_yds")
  team_predictions <- combine_predictions(team_predictions, "rus_tds")
  
  #Note that scramble yards are only a factor of predicted scrambles. This is intentional and gives a good estimate
  sc_td_coef <- params["sc_td_coef"]
  sc_td_coef <- 0.2
  
  team_predictions <- team_predictions %>% 
    mutate(team_sc_att_pred = adj_sc_att_rat,
           team_sc_yds_pred = team_sc_att_pred*7.52,
           team_sc_tds_pred = team_sc_att_pred*0.03*(1-sc_td_coef) + sc_td_coef*adj_sc_tds_rat)
  
  
  #select cols
  team_predictions <- team_predictions %>% 
    select(team, team_pas_att_pred:team_int_pred, team_sc_att_pred:team_sc_tds_pred, team_rus_att_pred:team_rus_tds_pred)
  
  ####Player Predictions####
  player_predictions <- adjusted %>% 
    full_join(team_predictions, by = c("team")) 
  
  player_predictions <- player_predictions %>%
    mutate(is_QB = ifelse(pos == "QB", 1, 0)) %>% 
    mutate(pas_att_pred = team_pas_att_pred*is_QB,
           cmp_pred = team_cmp_pred*is_QB,
           pas_yds_pred = team_pas_yds_pred*is_QB,
           pas_tds_pred = team_pas_tds_pred*is_QB,
           int_pred = team_int_pred*is_QB,
           sc_att_pred = team_sc_att_pred*is_QB,
           sc_yds_pred = team_sc_yds_pred*is_QB,
           sc_tds_pred = team_sc_tds_pred*is_QB,
           tgt_pred = team_pas_att_pred*adj_tgt_per,
           rec_pred = team_cmp_pred*adj_rec_per,
           rec_yds_pred = team_pas_yds_pred*adj_rec_yds_per,
           rec_tds_pred = team_pas_tds_pred*adj_rec_tds_per,
           rus_att_pred = team_rus_att_pred*adj_rus_att_per,
           rus_yds_pred = team_rus_yds_pred*adj_rus_yds_per,
           rus_tds_pred = team_rus_tds_pred*adj_rus_tds_per) %>% 
    select(player:opponent, pas_att_pred:rus_tds_pred)
  
  #fumbles
  player_predictions <- player_predictions %>% 
    mutate(fl_pred = ifelse(pos == "QB", pas_att_pred*0.005+sc_att_pred*0.033, (rus_att_pred+rec_pred)*0.005))
  
  
  ####FPTS Predictions####
  player_predictions[, 5:20][is.na(player_predictions[, 5:20])] <- 0
  
  player_predictions <- player_predictions %>% 
    mutate(fpts_pred = pas_yds_pred*0.04 + pas_tds_pred*4 + sc_yds_pred*0.1 + sc_tds_pred*6 + rus_yds_pred*0.1 + rus_tds_pred*6 + rec_yds_pred*0.1 + rec_tds_pred*6 + 0.5*rec_pred - 1*int_pred - 2*fl_pred)
  
  ####Clean Adjusted####
  adjusted <- adjusted %>% 
    select(player:team, py_games_played:adj_rec_tds_per)
  
  ####Injury status####
  adjusted <- adjusted %>%
    left_join(injury_status, by = c("player", "pos"))
  
  ####Return####
  return(list(
    player_predictions = player_predictions,
    team_predictions = team_predictions,
    player_percents_adjusted = adjusted
  ))
}

#####Update Ratings#####
update_ratings <- function(This_Year, past_week, params, player_percents_2d, qb_ratings_2d, off_team_ratings_2d, def_team_ratings_2d, player_predictions_2d, team_predictions_2d, player_percents_adjusted_2d){
  #Year
  This_Year_char <- as.character(This_Year)
  
  #week
  upcoming_week <- past_week+1
  
  #read files
  #predictions
  past_week_player_predictions <- player_predictions_2d[[This_Year_char]][[paste0("Week_", past_week)]]
  past_week_team_predictions <- team_predictions_2d[[This_Year_char]][[paste0("Week_", past_week)]]
  
  #ratings
  past_week_combined_player_percents_rat <- player_percents_2d[[This_Year_char]][[paste0("Week_", past_week)]]
  past_week_off_team_ratings <- off_team_ratings_2d[[This_Year_char]][[paste0("Week_", past_week)]]
  past_week_def_team_ratings <- def_team_ratings_2d[[This_Year_char]][[paste0("Week_", past_week)]]
  past_week_QB_ratings <- qb_ratings_2d[[This_Year_char]][[paste0("Week_", past_week)]]
  
  #prediction of player percents
  past_week_adjusted_combined_player_percents_rat <- player_percents_adjusted_2d[[This_Year_char]][[paste0("Week_", past_week)]]
  
  #player stats
  past_week_player_stats <- player_stats_2d[[This_Year_char]][[paste0("Week_", past_week)]]
  
  
  ####weekly player percents####
  
  #Team Stats
  #There is no serious problem with summing of player stats to get team snaps. It does not include special teams (good), while team data does. However, sometimes players playing offensive snaps whose position is not an offensive position (lb/rb hybrid) are not counted.
  past_week_team_stats <- past_week_player_stats %>% 
    group_by(team) %>% 
    summarize(team_pas_att = sum(pas_att),
              team_cmp = sum(cmp),
              team_pas_yds = sum(pas_yds),
              team_pas_tds = sum(pas_tds),
              team_int = sum(int),
              team_rus_att = sum(rus_att),
              team_rus_yds = sum(rus_yds),
              team_rus_tds = sum(rus_tds),
              team_tgt = sum(tgt),
              team_rec = sum(rec),
              team_rec_yds = sum(rec_yds),
              team_rec_tds = sum(rec_tds))
  
  #combine with player stats
  past_week_player_percents <- past_week_player_stats %>% 
    left_join(past_week_team_stats, by = c("team"))
  
  #get player percents
  player_percents_func <- function(df, col){
    #rcombine
    df[, paste(col, "_per", sep = "")] <- ((df[, paste(col, sep = "")])/(df[, paste("team_", col, sep = "")]))
    df[, paste(col, "_per", sep = "")][is.na(df[, paste(col, "_per", sep = "")])] <- 0
    return(df)
  }
  
  past_week_player_percents <- player_percents_func(past_week_player_percents, "rus_att")
  past_week_player_percents <- player_percents_func(past_week_player_percents, "rus_yds")
  past_week_player_percents <- player_percents_func(past_week_player_percents, "rus_tds")
  past_week_player_percents <- player_percents_func(past_week_player_percents, "tgt")
  past_week_player_percents <- player_percents_func(past_week_player_percents, "rec")
  past_week_player_percents <- player_percents_func(past_week_player_percents, "rec_yds")
  past_week_player_percents <- player_percents_func(past_week_player_percents, "rec_tds")
  
  #select cols
  past_week_player_percents <- past_week_player_percents %>% 
    select(player:opp, rus_att_per:rec_tds_per, snap_per, st_snaps)
  
  t_past_week_adjusted_combined_player_percents_rat <- past_week_adjusted_combined_player_percents_rat
  
  player_percents <- past_week_player_percents %>% 
    full_join(t_past_week_adjusted_combined_player_percents_rat, by = c("player", "pos")) %>% 
    mutate(team = ifelse(is.na(team.x), team.y, team.x)) %>% 
    select(player, pos, week, team, opp, rus_att_per:st_snaps, py_games_played, games_played, adj_rus_att_per:adj_rec_tds_per, injury_status)
  
  
  #filter out players who may have been injured
  player_percents <- player_percents %>% 
    filter(!(is.na(snap_per) & injury_status %in% c("Q", "D")))
  
  #NAs to 0
  player_percents[, 6:23][is.na(player_percents[, 6:23])] <- 0
  
  #for players who actually played
  player_percents <- player_percents %>% 
    mutate(played = ifelse(snap_per > 0, 1, 0))
  
  #increase games played (only for players with offensive snaps)
  player_percents <- player_percents %>%
    mutate(games_played = games_played + played)
  
  #volatility
  #tested
  rus_vol_a <- params["rus_vol_a"]
  rus_vol_b <- params["rus_vol_b"]
  rus_vol_c <- params["rus_vol_c"]
  rus_vol_d <- params["rus_vol_d"]
  # 
  # rec_vol_a <- params["rec_vol_a"]
  # rec_vol_b <- params["rec_vol_b"]
  # rec_vol_c <- params["rec_vol_c"]
  # rec_vol_d <- params["rec_vol_d"]
  
  # rus_vol_a <- 0.05
  # rus_vol_b <- 0.5
  # rus_vol_c <- 0.5
  # rus_vol_d <- 17
  
  rec_vol_a <- 0.05
  rec_vol_b <- 0.3
  rec_vol_c <- 1
  rec_vol_d <- 50
  
  player_percents <- player_percents %>%
    mutate(rus_vol = rus_vol_a + (rus_vol_b/(games_played + rus_vol_c + (1-played)))*(1 + (17 - py_games_played)/rus_vol_d),
           rec_vol = rec_vol_a + (rec_vol_b/(games_played + rec_vol_c + (1-played)))*(1 + (17 - py_games_played)/rec_vol_d))
  
  # player_percents <- player_percents %>%
  #   mutate(rus_vol = 0.05 + (0.5/(games_played + 0.5))*(1 + (17 - py_games_played)/17),
  #          rec_vol = 0.05 + (0.3/(games_played + 1))*(1 + (17 - py_games_played)/50))
  
  
  #get changes
  player_percents_changes <- player_percents %>% 
    mutate(d_rus_att_per = (rus_att_per - adj_rus_att_per)*rus_vol,
           d_rus_yds_per = (rus_yds_per - adj_rus_yds_per)*rus_vol,
           d_rus_tds_per = (rus_tds_per - adj_rus_tds_per)*rus_vol,
           d_tgt_per = (tgt_per - adj_tgt_per)*rec_vol,
           d_rec_per = (rec_per - adj_rec_per)*rec_vol,
           d_rec_yds_per = (rec_yds_per - adj_rec_yds_per)*rec_vol,
           d_rec_tds_per = (rec_tds_per - adj_rec_tds_per)*rec_vol) %>% 
    select(player, pos, team, games_played, py_games_played, played, d_rus_att_per:d_rec_tds_per)
  
  #update player percents
  t_past_week_combined_player_percents_rat <- past_week_combined_player_percents_rat %>% 
    select(!team:games_played) 
  updated_player_percents <- left_join(player_percents_changes, t_past_week_combined_player_percents_rat, by = c("player", "pos"))
  
  
  updated_player_percents[updated_player_percents$player %in% duplicated(updated_player_percents$player), ]
  updated_player_percents$player[duplicated(updated_player_percents$player)]
  
  dups <- updated_player_percents$player[duplicated(updated_player_percents$player)]
  
  if (length(dups) > 0) {
    stop("Duplicated players found Year ", paste(This_Year),  "Week ", paste(upcoming_week), ": ", paste(unique(dups), collapse = ", "))
  }
  
  
  #NAs rep
  updated_player_percents[, 4:20][is.na(updated_player_percents)[, 4:20]] <- 0
  
  updated_player_percents <- updated_player_percents %>% 
    mutate(upd_rus_att_per = rus_att_per + d_rus_att_per,
           upd_rus_yds_per = rus_yds_per + d_rus_yds_per,
           upd_rus_tds_per = rus_tds_per + d_rus_tds_per,
           upd_tgt_per = tgt_per + d_tgt_per,
           upd_rec_per = rec_per + d_rec_per,
           upd_rec_yds_per = rec_yds_per + d_rec_yds_per,
           upd_rec_tds_per = rec_tds_per + d_rec_tds_per) %>% 
    mutate(upd_rus_att_per = ifelse(played == 0 & upd_rus_att_per < 0.05 & rus_att_per >= 0.05, 0.05, upd_rus_att_per),
           upd_rus_yds_per = ifelse(played == 0 & upd_rus_yds_per < 0.05 & rus_yds_per >= 0.05, 0.05, upd_rus_yds_per),
           upd_rus_tds_per = ifelse(played == 0 & upd_rus_tds_per < 0.05 & rus_tds_per >= 0.05, 0.05, upd_rus_tds_per),
           upd_tgt_per = ifelse(played == 0 & upd_tgt_per < 0.05 & tgt_per >= 0.05, 0.05, upd_tgt_per),
           upd_rec_per = ifelse(played == 0 & upd_rec_per < 0.03 & rec_per >= 0.03, 0.03, upd_rec_per),
           upd_rec_yds_per = ifelse(played == 0 & upd_rec_yds_per < 0.03 & rec_yds_per >= 0.03, 0.03, upd_rec_yds_per),
           upd_rec_tds_per = ifelse(played == 0 & upd_rec_tds_per < 0.03 & rec_tds_per >= 0.03, 0.03, upd_rec_tds_per))
  
  #fix high variance percents
  #tested
  # upd_val_1_1 <- params["upd_val_1_1"]
  # upd_val_2_1 <- params["upd_val_2_1"]
  # upd_val_2_2 <- params["upd_val_2_2"]
  # upd_val_3 <- params["upd_val_3"]
  # upd_val_4_1 <- params["upd_val_4_1"]
  # upd_val_4_2 <- params["upd_val_4_2"]
  # upd_val_5_1 <- params["upd_val_5_1"]
  # upd_val_5_2 <- params["upd_val_5_2"]
  # upd_val_5_3 <- params["upd_val_5_3"]
  
  upd_val_1_1 <- 0.9
  upd_val_2_1 <- 0.5
  upd_val_2_2 <- 0.2
  upd_val_3 <- 0.7
  upd_val_4_1 <- 0.8
  upd_val_4_2 <- 0
  upd_val_5_1 <- 0.4
  upd_val_5_2 <- 0.2
  upd_val_5_3 <- 0
  
  updated_player_percents <- updated_player_percents %>%
    mutate(upd_rus_yds_per = upd_val_1_1*upd_rus_yds_per + (1-upd_val_1_1)*upd_rus_att_per,
           upd_rus_tds_per = upd_val_2_1*upd_rus_tds_per + upd_val_2_2*upd_rus_yds_per + (1-upd_val_2_1-upd_val_2_2)*upd_rus_att_per,
           upd_rec_per = upd_val_3*upd_rec_per + (1-upd_val_3)*upd_tgt_per,
           upd_rec_yds_per = upd_val_4_1*upd_rec_yds_per + upd_val_4_2*upd_rec_per + (1-upd_val_4_1-upd_val_4_2)*upd_tgt_per,
           upd_rec_tds_per = upd_val_5_1*upd_rec_tds_per + upd_val_5_2*upd_rec_yds_per + upd_val_5_3*upd_rec_per + (1 - upd_val_5_1 - upd_val_5_2 - upd_val_5_3)*upd_tgt_per)
  
  #Normalize player percents
  player_percents_by_team <- updated_player_percents %>%
    group_by(team) %>%
    summarise(old_tot_rus_att_per = sum(rus_att_per),
              old_tot_rus_yds_per = sum(rus_yds_per),
              old_tot_rus_tds_per = sum(rus_tds_per),
              old_tot_tgt_per = sum(tgt_per),
              old_tot_rec_per = sum(rec_per),
              old_tot_rec_yds_per = sum(rec_yds_per),
              old_tot_rec_tds_per = sum(rec_tds_per),
              new_tot_rus_att_per = sum(upd_rus_att_per),
              new_tot_rus_yds_per = sum(upd_rus_yds_per),
              new_tot_rus_tds_per = sum(upd_rus_tds_per),
              new_tot_tgt_per = sum(upd_tgt_per),
              new_tot_rec_per = sum(upd_rec_per),
              new_tot_rec_yds_per = sum(upd_rec_yds_per),
              new_tot_rec_tds_per = sum(upd_rec_tds_per))
  
  updated_player_percents <- updated_player_percents %>%
    left_join(player_percents_by_team, by = c("team")) %>%
    mutate(upd_rus_att_per = upd_rus_att_per*(old_tot_rus_att_per/new_tot_rus_att_per),
           upd_rus_yds_per = upd_rus_yds_per*(old_tot_rus_yds_per/new_tot_rus_yds_per),
           upd_rus_tds_per = upd_rus_tds_per*(old_tot_rus_tds_per/new_tot_rus_tds_per),
           upd_tgt_per = upd_tgt_per*(old_tot_tgt_per/new_tot_tgt_per),
           upd_rec_per = upd_rec_per*(old_tot_rec_per/new_tot_rec_per),
           upd_rec_yds_per = upd_rec_yds_per*(old_tot_rec_yds_per/new_tot_rec_yds_per),
           upd_rec_tds_per = upd_rec_tds_per*(old_tot_rec_tds_per/new_tot_rec_tds_per))
  
  #Regress to sum to 1
  # reg_coef <- 2
  # 
  # updated_player_percents_by_team <- updated_player_percents %>%
  #   group_by(team) %>%
  #   summarise(tot_rus_att_per = sum(upd_rus_att_per),
  #             tot_rus_yds_per = sum(upd_rus_yds_per),
  #             tot_rus_tds_per = sum(upd_rus_tds_per),
  #             tot_tgt_per = sum(upd_tgt_per),
  #             tot_rec_per = sum(upd_rec_per),
  #             tot_rec_yds_per = sum(upd_rec_yds_per),
  #             tot_rec_tds_per = sum(upd_rec_tds_per)) %>%
  #   mutate(reg_rus_att_per = (((1 - tot_rus_att_per)/reg_coef) + tot_rus_att_per)/(tot_rus_att_per),
  #          reg_rus_yds_per = (((1 - tot_rus_yds_per)/reg_coef) + tot_rus_yds_per)/(tot_rus_yds_per),
  #          reg_rus_tds_per = (((1 - tot_rus_tds_per)/reg_coef) + tot_rus_tds_per)/(tot_rus_tds_per),
  #          reg_tgt_per = (((1 - tot_tgt_per)/reg_coef) + tot_tgt_per)/(tot_tgt_per),
  #          reg_rec_per = (((1 - tot_rec_per)/reg_coef) + tot_rec_per)/(tot_rec_per),
  #          reg_rec_yds_per = (((1 - tot_rec_yds_per)/reg_coef) + tot_rec_yds_per)/(tot_rec_yds_per),
  #          reg_rec_tds_per = (((1 - tot_rec_tds_per)/reg_coef) + tot_rec_tds_per)/(tot_rec_tds_per))
  # 
  # updated_player_percents <- updated_player_percents %>%
  #   left_join(updated_player_percents_by_team, by = c("team")) %>%
  #   mutate(upd_rus_att_per = upd_rus_att_per*reg_rus_att_per,
  #          upd_rus_yds_per = upd_rus_yds_per*reg_rus_yds_per,
  #          upd_rus_tds_per = upd_rus_tds_per*reg_rus_tds_per,
  #          upd_tgt_per = upd_tgt_per*reg_tgt_per,
  #          upd_rec_per = upd_rec_per*reg_rec_per,
  #          upd_rec_yds_per = upd_rec_yds_per*reg_rec_yds_per,
  #          upd_rec_tds_per = upd_rec_tds_per*reg_rec_tds_per)
  
  #select
  updated_player_percents <- updated_player_percents %>% 
    select(player, pos, team, py_games_played, games_played, upd_rus_att_per:upd_rec_tds_per)
  
  #fix negative updated percents
  updated_player_percents[updated_player_percents < 0] <- 0
  
  #clean up
  colnames(updated_player_percents) <- colnames(past_week_combined_player_percents_rat)
  
  #combine with full player percents rating
  not_active_player_percents <- past_week_combined_player_percents_rat %>% 
    filter(!(player %in% updated_player_percents$player))
  
  full_updated_player_percents <- rbind(not_active_player_percents, updated_player_percents)
  
  #fix negative ratings
  full_updated_player_percents[full_updated_player_percents < 0] <- 0
  
  ####QB Ratings####
  #join
  QB_ratings_dif <- full_join(past_week_team_predictions, past_week_player_stats, by = c("team"))
  
  #filter QBs
  QB_ratings_dif <- QB_ratings_dif %>% 
    filter(pos == "QB")
  
  #select cols
  QB_ratings_dif <- QB_ratings_dif %>% 
    select(player, team, opp, team_pas_att_pred:team_sc_tds_pred, pas_att:sc_tds, snap_per)
  
  #if QB played less than 20% of snaps, set to 0, snap multiplier
  QB_ratings_dif <- QB_ratings_dif %>% 
    mutate(snap_per = ifelse(snap_per < 0.2, 0, snap_per),
           snap_mul = ifelse(snap_per == 0, 0, 1/snap_per))
  
  #get difference from predicted and actual
  QB_ratings_dif <- QB_ratings_dif %>% 
    mutate(d_pas_att = pas_att*snap_mul - team_pas_att_pred,
           d_cmp = cmp*snap_mul - team_cmp_pred,
           d_pas_yds = pas_yds*snap_mul - team_pas_yds_pred,
           d_pas_tds = pas_tds*snap_mul - team_pas_tds_pred,
           d_int = int*snap_mul - team_int_pred,
           d_sc_att = sc_att*snap_mul - team_sc_att_pred,
           d_sc_yds = sc_yds*snap_mul - team_sc_yds_pred,
           d_sc_tds = sc_tds*snap_mul - team_sc_tds_pred)
  
  #update QB ratings
  updated_QB_ratings <- full_join(past_week_QB_ratings, QB_ratings_dif, by = "player")
  
  #update team and select
  updated_QB_ratings <- updated_QB_ratings %>% 
    mutate(team = team.y) %>% 
    select(player, team, py_games_played, games_played, pas_att_rat:sc_tds_rat, d_pas_att:d_sc_tds, snap_per)
  
  #new players
  pas_att_rep = 20
  cmp_rep = 9
  pas_yds_rep = 150
  pas_tds_rep = 0.6
  int_rep = 1
  sc_att_rep <- 1.4 #not tested, based on scramble testing optimal percent of average attempts and league average
  sc_yds_rep <- 12 #not tested
  sc_tds_rep <- 0.05 #not tested
  
  #Replace with replacement level if necessary
  updated_QB_ratings <- updated_QB_ratings %>% 
    mutate(pas_att_rat = ifelse(is.na(pas_att_rat), pas_att_rep, pas_att_rat),
           cmp_rat = ifelse(is.na(cmp_rat), cmp_rep, cmp_rat),
           pas_yds_rat = ifelse(is.na(pas_yds_rat), pas_yds_rep, pas_yds_rat),
           pas_tds_rat = ifelse(is.na(pas_tds_rat), pas_tds_rep, pas_tds_rat),
           int_rat = ifelse(is.na(int_rat), int_rep, int_rat),
           sc_att_rat = ifelse(is.na(sc_att_rat), sc_att_rep, sc_att_rat),
           sc_yds_rat = ifelse(is.na(sc_yds_rat), sc_yds_rep, sc_yds_rat),
           sc_tds_rat = ifelse(is.na(sc_tds_rat), sc_tds_rep, sc_tds_rat))
  
  #zeroes
  updated_QB_ratings$py_games_played[is.na(updated_QB_ratings$py_games_played)] <- 0
  updated_QB_ratings$games_played[is.na(updated_QB_ratings$games_played)] <- 0
  
  ##dnp
  dnp_QB_ratings <- updated_QB_ratings %>%
    filter(is.na(snap_per) | snap_per == 0) 
  
  #players who played
  updated_QB_ratings <- updated_QB_ratings %>%
    filter(!is.na(snap_per) & snap_per > 0)
  
  #increment games played
  updated_QB_ratings <- updated_QB_ratings %>%
    mutate(games_played = games_played + 1)
  
  #volatility
  # qb_vol_a <- params["qb_vol_a"]
  # qb_vol_b <- params["qb_vol_b"]
  # qb_vol_c <- params["qb_vol_c"]
  
  #tested
  #low
  qb_vol_a = 0.1
  qb_vol_b = 0.07
  qb_vol_c = 34

  updated_QB_ratings <- updated_QB_ratings %>%
    mutate(vol = (qb_vol_a + (qb_vol_b/games_played)*(1 + (17 - py_games_played)/qb_vol_c))*snap_per/1)
  
  #high
  # updated_QB_ratings <- updated_QB_ratings %>%
  #   mutate(vol = (0.3 + (0.1/games_played)*(1 + (17 - py_games_played)/34))*snap_per/1)
  
  #old
  # updated_QB_ratings <- updated_QB_ratings %>%
  #   mutate(vol = (0.2 + (0.1/games_played)*(1 + (17 - py_games_played)/34))*snap_per/1)
  
  #update ratings
  updated_QB_ratings <- updated_QB_ratings %>% 
    mutate(upd_pas_att_rat = pas_att_rat + d_pas_att*vol,
           upd_cmp_rat = cmp_rat + d_cmp*vol,
           upd_pas_yds_rat = pas_yds_rat + d_pas_yds*vol,
           upd_pas_tds_rat = pas_tds_rat + d_pas_tds*(vol/2),
           upd_int_rat = int_rat + d_int*(vol/2),
           upd_sc_att_rat = sc_att_rat + d_sc_att*(vol/2),
           upd_sc_yds_rat = sc_yds_rat + d_sc_yds*(vol/2),
           upd_sc_tds_rat = sc_tds_rat + d_sc_tds*(vol/2)) %>% 
    select(player, team, py_games_played, games_played, upd_pas_att_rat:upd_sc_tds_rat)
  
  #recombine with dnp
  dnp_QB_ratings <- dnp_QB_ratings %>% 
    mutate(upd_pas_att_rat = pas_att_rat,
           upd_cmp_rat = cmp_rat,
           upd_pas_yds_rat = pas_yds_rat,
           upd_pas_tds_rat = pas_tds_rat,
           upd_int_rat = int_rat,
           upd_sc_att_rat = sc_att_rat,
           upd_sc_yds_rat = sc_yds_rat,
           upd_sc_tds_rat = sc_tds_rat) %>% 
    select(player, team, py_games_played, games_played, upd_pas_att_rat:upd_sc_tds_rat) 
  
  updated_QB_ratings <- rbind(updated_QB_ratings, dnp_QB_ratings)
  
  #clean up
  colnames(updated_QB_ratings) <- colnames(past_week_QB_ratings)
  
  #fix negative ratings
  updated_QB_ratings[updated_QB_ratings < 0] <- 0
  
  ####Off Team Ratings####
  #get team stats
  past_week_team_stats <- past_week_player_stats %>% 
    group_by(team) %>% 
    summarise(across(pas_att:rec_tds, sum),
              .groups = 'drop') %>% 
    select(team, pas_att:pas_tds, int, rus_att:rus_tds)
  
  #combine predicted and actual
  off_team_dif <- full_join(past_week_team_stats, past_week_team_predictions, by = c("team")) %>% 
    filter(!is.na(pas_att))
  
  #get difference
  off_team_dif <- off_team_dif %>% 
    transmute(team = team,
              d_pas_att = pas_att - team_pas_att_pred,
              d_cmp = cmp - team_cmp_pred,
              d_pas_yds = pas_yds - team_pas_yds_pred,
              d_pas_tds = pas_tds - team_pas_tds_pred,
              d_int = int - team_int_pred,
              d_rus_att = rus_att - team_rus_att_pred,
              d_rus_yds = rus_yds - team_rus_yds_pred,
              d_rus_tds = rus_tds - team_rus_tds_pred)
  
  #combine with ratings
  updated_off_team_ratings <- left_join(off_team_dif, past_week_off_team_ratings, by = c("team"))
  
  #volatility
  off_vol_a <- 0.8
  off_vol_b <- 0.2
  
  #tested
  #new
  # off_vol_a <- params["off_vol_a"]
  # off_vol_b <- params["off_vol_b"]
  
  updated_off_team_ratings <- updated_off_team_ratings %>% 
    mutate(vol = off_vol_a + (off_vol_b/past_week))
  
  #old
  # updated_off_team_ratings <- updated_off_team_ratings %>% 
  #   mutate(vol = 0.05 + (0.3/past_week))
  
  #update ratings
  updated_off_team_ratings <- updated_off_team_ratings %>% 
    mutate(upd_pas_att_rat = off_pas_att_rat + d_pas_att*vol,
           upd_cmp_rat = off_cmp_rat + d_cmp*vol,
           upd_pas_yds_rat = off_pas_yds_rat + d_pas_yds*vol,
           upd_pas_tds_rat = off_pas_tds_rat + d_pas_tds*(vol/2),
           upd_int_rat = off_int_rat + d_int*(vol/2),
           upd_rus_att_rat = off_rus_att_rat + d_rus_att*vol,
           upd_rus_yds_rat = off_rus_yds_rat + d_rus_yds*vol,
           upd_rus_tds_rat = off_rus_tds_rat + d_rus_tds*(vol/2))
  
  #update ratings for regressing to 1 in player percents
  #coefs from make predictions
  # rus_att <- 0
  # rus_yds <- 0
  # rus_tds <- 0
  # pas_att <- 0.1
  # cmp <- 0.4
  # pas_yds <- 0.1
  # pas_tds <- 0
  
  # team_adjustment <- updated_player_percents_by_team %>%
  #   mutate(rus_att_adj = ((tot_rus_att_per - 1)*rus_att + 1)/(((tot_rus_att_per - 1)/reg_coef)*rus_att + 1),
  #          rus_yds_adj = ((tot_rus_yds_per - 1)*rus_yds + 1)/(((tot_rus_yds_per - 1)/reg_coef)*rus_yds + 1),
  #          rus_tds_adj = ((tot_rus_tds_per - 1)*rus_tds + 1)/(((tot_rus_tds_per - 1)/reg_coef)*rus_tds + 1),
  #          pas_att_adj = ((tot_tgt_per - 1)*pas_att + 1)/(((tot_tgt_per - 1)/reg_coef)*pas_att + 1),
  #          pas_cmp_adj = ((tot_rec_per - 1)*cmp + 1)/(((tot_rec_per - 1)/reg_coef)*cmp + 1),
  #          pas_yds_adj = ((tot_rec_yds_per - 1)*pas_yds + 1)/(((tot_rec_yds_per - 1)/reg_coef)*pas_yds + 1),
  #          pas_tds_adj = ((tot_rec_tds_per - 1)*pas_tds + 1)/(((tot_rec_tds_per - 1)/reg_coef)*pas_tds + 1))
  # 
  # updated_off_team_ratings <- updated_off_team_ratings %>%
  #   left_join(team_adjustment, by = c("team")) %>%
  #   mutate(upd_rus_att_rat = upd_rus_att_rat*rus_att_adj,
  #          upd_rus_yds_rat = upd_rus_yds_rat*rus_yds_adj,
  #          upd_rus_tds_rat = upd_rus_tds_rat*rus_tds_adj,
  #          upd_pas_att_rat = upd_pas_att_rat*pas_att_adj,
  #          upd_cmp_rat = upd_cmp_rat*pas_cmp_adj,
  #          upd_pas_yds_rat = upd_pas_yds_rat*pas_yds_adj,
  #          upd_pas_tds_rat = upd_pas_tds_rat*pas_tds_adj)
  
  #clean up
  updated_off_team_ratings <- updated_off_team_ratings %>% 
    select(team, upd_pas_att_rat:upd_rus_tds_rat)
  
  colnames(updated_off_team_ratings) <- colnames(past_week_off_team_ratings)
  
  #teams on bye
  not_active_off_team_ratings <- past_week_off_team_ratings %>% 
    filter(!(team %in% updated_off_team_ratings$team))
  
  full_updated_off_team_ratings <- rbind(not_active_off_team_ratings, updated_off_team_ratings)
  
  
  ####Def Team Ratings####
  #get team stats
  past_week_team_stats <- past_week_player_stats %>% 
    group_by(team, opp) %>% 
    summarise(across(pas_att:rec_tds, sum),
              .groups = 'drop') %>% 
    select(team, opp, pas_att:pas_tds, int, rus_att:rus_tds)
  
  #combine predicted and actual
  def_team_dif <- full_join(past_week_team_stats, past_week_team_predictions, by = c("team")) %>% 
    select(!team) %>% 
    rename("team" = "opp") %>% 
    filter(!is.na(team))
  
  #get difference
  def_team_dif <- def_team_dif %>% 
    transmute(team = team,
              d_pas_att = pas_att - team_pas_att_pred,
              d_cmp = cmp - team_cmp_pred,
              d_pas_yds = pas_yds - team_pas_yds_pred,
              d_pas_tds = pas_tds - team_pas_tds_pred,
              d_int = int - team_int_pred,
              d_rus_att = rus_att - team_rus_att_pred,
              d_rus_yds = rus_yds - team_rus_yds_pred,
              d_rus_tds = rus_tds - team_rus_tds_pred)
  
  #combine with ratings
  updated_def_team_ratings <- left_join(def_team_dif, past_week_def_team_ratings, by = c("team"))
  
  #volatility
  #tested
  # def_vol_a <- params["def_vol_a"]
  # def_vol_b <- params["def_vol_b"]
  
  def_vol_a <- 0.3
  def_vol_b <- 0.2
  
  updated_def_team_ratings <- updated_def_team_ratings %>% 
    mutate(vol = def_vol_a + (def_vol_b/past_week))
  
  #update ratings
  updated_def_team_ratings <- updated_def_team_ratings %>% 
    mutate(upd_pas_att_rat = def_pas_att_rat + d_pas_att*vol,
           upd_cmp_rat = def_cmp_rat + d_cmp*vol,
           upd_pas_yds_rat = def_pas_yds_rat + d_pas_yds*vol,
           upd_pas_tds_rat = def_pas_tds_rat + d_pas_tds*(vol/2),
           upd_int_rat = def_int_rat + d_int*(vol/2),
           upd_rus_att_rat = def_rus_att_rat + d_rus_att*vol,
           upd_rus_yds_rat = def_rus_yds_rat + d_rus_yds*vol,
           upd_rus_tds_rat = def_rus_tds_rat + d_rus_tds*(vol/2))
  
  #clean up
  updated_def_team_ratings <- updated_def_team_ratings %>% 
    select(team, upd_pas_att_rat:upd_rus_tds_rat)
  
  colnames(updated_def_team_ratings) <- colnames(past_week_def_team_ratings)
  
  #teams on bye
  not_active_def_team_ratings <- past_week_def_team_ratings %>% 
    filter(!(team %in% updated_def_team_ratings$team))
  
  full_updated_def_team_ratings <- rbind(not_active_def_team_ratings, updated_def_team_ratings)
  
  
  ####Return####
  return(list(
    player_percents = full_updated_player_percents,
    qb_ratings = updated_QB_ratings,
    off_team_ratings = full_updated_off_team_ratings,
    def_team_ratings = full_updated_def_team_ratings
  ))
}


#####Get Loss#####
get_weekly_loss <- function(This_Year, past_week, player_predictions_2d){
  #Year
  This_Year_char <- as.character(This_Year)
  
  #get data
  past_week_player_predictions <- player_predictions_2d[[This_Year_char]][[paste0("Week_", past_week)]]
  past_week_player_stats <- player_stats_2d[[This_Year_char]][[paste0("Week_", past_week)]] %>%
    clean_names() 
  
  #calculate fpts
  player_stats <- player_names_func(past_week_player_stats)
  
  player_stats <- player_stats  %>% 
    mutate(fpts = pas_yds*0.04 + pas_tds*4 + sc_yds*0.1 + sc_tds*6 + rus_yds*0.1 + rus_tds*6 + rec_yds*0.1 + rec_tds*6 + 0.5*rec - 1*int - 2*fmb_l)
  
  
  ###Simple Placeholder Loss
  combined <- full_join(past_week_player_predictions, player_stats, by = c("player", "pos", "team", "opponent" = "opp")) %>% 
    select(player, pos, team, opponent, fpts_pred, fpts)
  
  combined$fpts_pred[is.na(combined$fpts_pred)] <- 0
  combined$fpts[is.na(combined$fpts)] <- 0
  
  combined <- combined %>% 
    mutate(resid = fpts - fpts_pred,
           resid_sq = resid^2)
  
  loss <- sum(combined$resid_sq)
  
  return(loss)
  
}



