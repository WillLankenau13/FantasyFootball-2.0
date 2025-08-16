

past_week_team_stats2 <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/", This_Year, "/Offensive_2022.csv", sep = ""))) %>% 
  clean_names() %>% 
  select(!date) %>% 
  select(team, opp, week, att_17, cmp_16, yds_20, td_21, att_38, yds_39, td_41, int, yds_27)

past_week_team_stats2 <- player_names_func(past_week_team_stats2)

colnames(past_week_team_stats2) <- c("team", "opp", "week", "pas_att", "cmp", "pas_yds", "pas_tds", "rus_att", "rus_yds", "rus_tds", "int", "sck_yds")

past_week_team_stats2 <- past_week_team_stats2 %>% 
  mutate(pas_yds = pas_yds + sck_yds)

c <- 18
df_list <- list()

while(c < 19){
  d_past_week_player_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/", This_Year, "/byWeek/Week_", c, "_Stats.csv", sep = ""))) %>% 
    clean_names()
  past_week_player_stats <- d_past_week_player_stats %>% 
    select(player, pos, g_number, week, team, opp, att_19, cmp_18, yds_22, td_23, int, att_37, yds_38, td_40, tgt_43, rec, yds_45, td_47, fmb_62, x2pm, off_percent_68)
  colnames(past_week_player_stats) <- c("player", "pos", "game_number", "week", "team", "opp", "pas_att", "cmp", "pas_yds", "pas_tds", "int", "rus_att", "rus_yds", "rus_tds", "tgt", "rec", "rec_yds", "rec_tds", "fmb_game", "two_point", "snap_per")
  past_week_player_stats <- player_names_func(past_week_player_stats)
  past_week_player_stats[is.na(past_week_player_stats)] <- 0
  
  past_week_team_stats <- past_week_player_stats %>% 
    group_by(team, opp, week) %>% 
    summarise(across(pas_att:rec_tds, sum),
              .groups = 'drop') %>% 
    select(team, opp, week, pas_att:pas_tds, rus_att:rus_tds, int)
  
  df_list[[c]] <- past_week_team_stats
 
  c <- c+1 
}

past_week_team_stats <- do.call(rbind, df_list)

t <- left_join(past_week_team_stats, past_week_team_stats2, by = c("team", "opp", "week"))


dif_func <- function(df, col){
  #rcombine
  df[, paste(col, "_dif", sep = "")] <- df[, paste(col, ".x", sep = "")] - df[, paste(col, ".y", sep = "")]
  
  return(df)
}

cols <- c("pas_att", "cmp", "pas_yds", "pas_tds", 
          "rus_att", "rus_yds", "rus_tds", 
          "int")

for (c in cols) {
  t <- dif_func(t, c)
}


cols <- c("pas_att_dif", "cmp_dif", "pas_yds_dif", "pas_tds_dif", 
          "rus_att_dif", "rus_yds_dif", "rus_tds_dif", 
          "int_dif")

filtered <- t[rowSums(t[cols] != 0) > 0, ]



s <- filtered %>% 
  filter(rus_att_dif != 0) %>% 
  select(team, opp, week, rus_att_dif, rus_yds_dif, rus_tds_dif, rus_att.x, rus_att.y, rus_yds.x, rus_yds.y, rus_tds.x, rus_tds.y)


