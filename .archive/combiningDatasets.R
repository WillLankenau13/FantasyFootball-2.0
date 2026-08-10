##Combine two datasets
datasets_func <- function(df){
  df <- df %>% 
    select(!x3:x8)
  colnames(df) <- paste(sep = '_', colnames(df), as.character(unlist(df[1,])))
  df <- df[-1, ]
  names(df) <- gsub("[[:digit:]]", "", names(df) )
  df <- df %>% 
    select(!passing__Sk:`passing__Sk%`) %>% 
    select(x_Player, `x_Pos.`, `x_G#`, x_Week, x_Team, x_Opp, passing__Att, passing__Cmp, passing__Yds, passing__TD, passing__Int, rushing__Att, rushing__Yds, rushing__TD, receiving__Tgt, receiving__Rec, receiving__Yds, receiving__TD, fumbles__Fmb, x_PM, `snap_counts__Off%`)
  colnames(df) <- c("player", "pos", "game_number", "week", "team", "opp", "pas_att", "cmp", "pas_yds", "pas_tds", "int", "rus_att", "rus_yds", "rus_tds", "tgt", "rec", "rec_yds", "rec_tds", "fmb_game", "two_point", "snap_per")
  return(df)
}

past_week_player_stats1 <- datasets_func(d_past_week_player_stats1)
past_week_player_stats2 <- datasets_func(d_past_week_player_stats2)


##convert to numerics
sapply(data_frame, class)

#defining the vector of columns to convert
vec <- c(3, 4, 7:21)

#apply the conversion on columns
past_week_player_stats[ , vec] <- apply(past_week_player_stats[ , vec,drop=F], 2,           
                                        function(x) as.numeric(as.character(x)))


