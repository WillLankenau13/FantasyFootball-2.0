#get error
top <- function(df, posn, n){
  temp <- df %>% 
    filter(Position == posn) %>% 
    arrange(desc(FPTS))
  
  temp <- head(temp, n)
  
  return(temp)
}

test_coef <- 0.7
player_predictions <- combine_predictions(player_predictions, "rec_tds", test_coef) #

combined <- full_join(player_predictions, this_season_stats, by = c("player"))

top_combined <- rbind(top(combined, "QB", 40), top(combined, "WR", 80), top(combined, "RB", 80), top(combined, "TE", 50))
top_combined <- top_combined %>%
  filter(!is.na(Team)) %>%
  select(!cmp:pas_tds)

r_mean <- mean(top_combined$rec_tds) #

top_combined <- top_combined %>%
  mutate(resid = rec_tds - rec_tds_pred, #
         sq_resid = resid^2,
         var = (rec_tds - r_mean)^2) #

r_squared = 1 - sum(top_combined$sq_resid)/sum(top_combined$var)
r_squared




