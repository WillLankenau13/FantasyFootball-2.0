t <- player_predictions %>% 
  filter(pos == "WR" | pos == "TE") %>% 
  select(player, team, pos, tgt_pred:rec_tds_pred)

s <- player_predictions %>% 
  filter(pos == "RB") %>% 
  select(player, team, pos, rus_att_pred:rus_tds_pred)
s[] <- lapply(s, function(x) format(x, scientific = FALSE))
s <- s %>% 
  mutate(rus_att_pred = as.numeric(rus_att_pred),
         rus_yds_pred = as.numeric(rus_yds_pred),
         rus_tds_pred = as.numeric(rus_tds_pred))
s[] <- lapply(s, function(x) if (is.numeric(x)) round(x, 2) else x)

j <- player_predictions %>% 
  filter(pos == "QB") %>% 
  select(player, team, pos, pas_att_pred:int_pred)


m <- player_predictions %>% 
  filter(pos == "QB") %>% 
  select(player, team, pos, sc_att_pred:sc_yds_pred, rus_att_pred:rus_tds_pred) %>% 
  mutate(tot_yds = sc_yds_pred + rus_yds_pred)
