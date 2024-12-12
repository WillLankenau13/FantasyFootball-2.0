




player_predictions <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonPredictions/", This_Year, "/Full_Season_Predictions_", This_Year, ".csv", sep = "")))
d_this_season_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonStats/", This_Year, "/Fantasy_", This_Year, ".csv", sep = ""))) %>% 
  clean_names()
fantasy_pros_QB <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyProsFullSeasonPredictions/", This_Year, "/FantasyProsQB.csv", sep = ""))) %>% 
  clean_names()
fantasy_pros_RB <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyProsFullSeasonPredictions/", This_Year, "/FantasyProsRB.csv", sep = ""))) %>% 
  clean_names()
fantasy_pros_WR <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyProsFullSeasonPredictions/", This_Year, "/FantasyProsWR.csv", sep = ""))) %>% 
  clean_names()
fantasy_pros_TE <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyProsFullSeasonPredictions/", This_Year, "/FantasyProsTE.csv", sep = ""))) %>% 
  clean_names()

d_this_season_stats <- player_names_func(d_this_season_stats)
fantasy_pros_QB <- player_names_func(fantasy_pros_QB)
fantasy_pros_RB <- player_names_func(fantasy_pros_RB)
fantasy_pros_WR <- player_names_func(fantasy_pros_WR)
fantasy_pros_TE <- player_names_func(fantasy_pros_TE)

####fantasyPros
fantasy_pros_TE <- fantasy_pros_TE %>% 
  mutate(rus_att_f = 0,
         rus_yds_f = 0,
         rus_tds_f = 0)

colnames(fantasy_pros_QB) <- c("player", "team", "pas_att_f", "cmp_f", "pas_yds_f", "pas_tds_f", "int_f", "rus_att_f", "rus_yds_f", "rus_tds_f", "fl_f", "fpts_f")

colnames(fantasy_pros_RB) <- c("player", "team", "rus_att_f", "rus_yds_f", "rus_tds_f", "rec_f", "rec_yds_f", "rec_tds_f", "fl_f", "fpts_f")
colnames(fantasy_pros_WR) <- c("player", "team", "rec_f", "rec_yds_f", "rec_tds_f", "rus_att_f", "rus_yds_f", "rus_tds_f", "fl_f", "fpts_f")
colnames(fantasy_pros_TE) <- c("player", "team", "rec_f", "rec_yds_f", "rec_tds_f", "fl_f", "fpts_f", "rus_att_f", "rus_yds_f", "rus_tds_f")

fantasy_pros_RB <- fantasy_pros_RB %>% 
  select(player, team, rus_att_f, rus_yds_f, rus_tds_f, rec_f, rec_yds_f, rec_tds_f, fl_f, fpts_f)
fantasy_pros_WR <- fantasy_pros_WR %>% 
  select(player, team, rus_att_f, rus_yds_f, rus_tds_f, rec_f, rec_yds_f, rec_tds_f, fl_f, fpts_f)
fantasy_pros_TE <- fantasy_pros_TE %>% 
  select(player, team, rus_att_f, rus_yds_f, rus_tds_f, rec_f, rec_yds_f, rec_tds_f, fl_f, fpts_f)

fantasy_pros_flex <- rbind(fantasy_pros_RB, fantasy_pros_TE, fantasy_pros_WR)
  
#half ppr
fantasy_pros_flex <- fantasy_pros_flex %>% 
  mutate(fpts_f = rus_yds_f*0.1 + rus_tds_f*6 + rec_yds_f*0.1 + rec_tds_f*6 + 0.5*rec_f - 2*fl_f)


####full season stats
#select
this_season_stats <- d_this_season_stats %>% 
  select(player, tm, fant_pos, g, gs, cmp, att_9, yds_10, td_11, int, att_13, yds_14, td_16, tgt, rec, yds_19, td_21, fmb, fl)

#rename
colnames(this_season_stats) <- c("player", "team", "pos", "player_games", "player_gs", "cmp", "pas_att", "pas_yds", "pas_tds", "int", "rus_att", "rus_yds", "rus_tds", "tgt", "rec", "rec_yds", "rec_tds", "fmb", "fl")

#set na vals to 0
this_season_stats[is.na(this_season_stats)] <- 0

#Add up fantasy points
this_season_stats <- this_season_stats %>% 
  mutate(fpts = pas_yds*0.04 + pas_tds*4 + rus_yds*0.1 + rus_tds*6 + rec_yds*0.1 + rec_tds*6 + 0.5*rec - 1*int - 2*fl)

#Per game
player_predictions[5:18] <- player_predictions[5:18]/17
this_season_stats[6:20] <- this_season_stats[6:20]/this_season_stats$player_games
fantasy_pros_QB[3:12] <- fantasy_pros_QB[3:12]/17
fantasy_pros_flex[3:10] <- fantasy_pros_flex[3:10]/17

#combine with predictions
t_combined <- full_join(player_predictions, this_season_stats, by = c("player"))

#remove players
not_counting <- c("Aaron Rodgers", "Joshua Dobbs", "Anthony Richardson")

#combine with fantasyPros
f_combined <- full_join(t_combined, fantasy_pros_flex, by = c("player", "Team_s" = "team")) %>% 
  filter(!is.na(fpts_pred)) %>% 
  filter(!(player %in% not_counting)) %>% 
  filter(!is.na(fpts_f)) %>% 
  filter(!is.na(team))

qb_combined <- full_join(t_combined, fantasy_pros_QB, by = c("player", "Team_s" = "team")) %>% 
  filter(!is.na(fpts_pred)) %>% 
  filter(!(player %in% not_counting)) %>% 
  filter(!is.na(fpts_f)) %>% 
  filter(!is.na(team))

comp <- f_combined %>% 
  select(player, Team_s, fpts_pred, fpts_f, fpts) %>% 
  mutate(my_dif = fpts_pred - fpts,
         f_dif = fpts_f - fpts)

comp <- comp %>% 
  mutate_if(is.numeric, ~round(., 0))

#R squareds
top <- function(df, posn, n){
  temp <- df %>% 
    filter(Position == posn) %>% 
    arrange(desc(fpts_f))
  
  temp <- head(temp, n)
  
  return(temp)
}
top_combined <- rbind(top(f_combined, "QB", 32), top(f_combined, "WR", 80), top(f_combined, "RB", 80), top(f_combined, "TE", 50))


col <- "int"
{
testing <- qb_combined %>% 
  transmute(player = player,
            Team = Team,
            my = get(paste(col, "_pred", sep = "")),
            fpros = get(paste(col, "_f", sep = "")),
            actual = get(paste(col)))

r_mean <- mean(testing$actual)

testing <- testing %>%
  mutate(my_resid = my - actual, 
         my_sq_resid = my_resid^2,
         f_resid = fpros - actual, 
         f_sq_resid = f_resid^2,
         var = (actual - r_mean)^2) 

my_r_squared = 1 - sum(testing$my_sq_resid)/sum(testing$var)
f_r_squared = 1 - sum(testing$f_sq_resid)/sum(testing$var)
  }

my_r_squared
f_r_squared

#players I did bad on
testing <- testing %>% 
  mutate(dif = abs(my_resid) - abs(f_resid))

#Combinations
col <- "rec_yds"
my_coef = 0.5
{
  testing <- top_combined %>% 
    transmute(player = player,
              Team = Team,
              my = get(paste(col, "_pred", sep = "")),
              fpros = get(paste(col, "_f", sep = "")),
              actual = get(paste(col)),
              pred = my*my_coef + fpros*(1-my_coef))
  
  r_mean <- mean(testing$actual)
  
  testing <- testing %>%
    mutate(resid = pred - actual, 
           sq_resid = resid^2,
           var = (actual - r_mean)^2) 
  
  r_squared = 1 - sum(testing$sq_resid)/sum(testing$var)
}

r_squared




