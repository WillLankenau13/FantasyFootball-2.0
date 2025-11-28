

#Week
past_week <- 1
  
#Year
This_Year <- This_Year_d

#raw data
roster <- fast_scraper_roster(2025) %>% 
    select(!c(week, team))
snaps <- load_snap_counts(2025) %>% 
  filter(week == past_week)
pbp_data <- load_pbp(2025) %>% 
  filter(week == past_week)


#initial tidying
snaps <- snaps %>% 
  rename("opp" = "opponent",
         "pos" = "position",
         "snap_per" = "offense_pct")
snaps <- player_names_func(snaps)
pbp_data[pbp_data == "Mi.Wilson"] <- "M.Wilson"

#filter out plays
pbp_data <- pbp_data %>% 
  filter(play_type != "no_play") %>%
  filter(sack == 0) %>% 
  filter(play_type_nfl != "UNSPECIFIED") %>%
  filter(is.na(two_point_conv_result))

#weekly passing stats
#pas att does not include spikes
weekly_passing_stats <- pbp_data %>% 
  filter(qb_scramble == 0) %>% 
  group_by(posteam, defteam, week, passer_player_name, passer_player_id) %>% 
  summarize(pas_att = sum(pass),
            cmp = sum(complete_pass),
            pas_yds = sum(receiving_yards, na.rm = TRUE) + sum(lateral_receiving_yards, na.rm = TRUE),
            pas_tds = sum(pass_touchdown),
            int = sum(interception)) %>% 
  filter(!is.na(passer_player_name)) %>% 
  rename("player" = "passer_player_name") %>% 
  rename("player_id" = "passer_player_id")

weekly_receiving_stats <- pbp_data %>% 
  filter(qb_scramble == 0) %>% 
  group_by(posteam, defteam, week, receiver_player_name, receiver_player_id) %>% 
  summarize(tgt = sum(pass),
            rec = sum(complete_pass),
            rec_yds = sum(receiving_yards, na.rm = TRUE)) %>% 
  filter(!is.na(receiver_player_name)) %>% 
  rename("player" = "receiver_player_name") %>% 
  rename("player_id" = "receiver_player_id")

weekly_lateral_receiving_stats <- pbp_data %>% 
  filter(qb_scramble == 0) %>% 
  group_by(posteam, defteam, week, lateral_receiver_player_name, lateral_receiver_player_id) %>% 
  summarize(lat_rec_yds = sum(lateral_receiving_yards, na.rm = TRUE)) %>% 
  filter(!is.na(lateral_receiver_player_name)) %>% 
  rename("player" = "lateral_receiver_player_name") %>% 
  rename("player_id" = "lateral_receiver_player_id")

weekly_rushing_stats <- pbp_data %>% 
  filter(qb_scramble == 0) %>% 
  group_by(posteam, defteam, week, rusher_player_name, rusher_player_id) %>% 
  summarize(rus_att = sum(rush),
            rus_yds = sum(rushing_yards, na.rm = TRUE)) %>% 
  filter(!is.na(rusher_player_name)) %>%
  rename("player" = "rusher_player_name") %>% 
  rename("player_id" = "rusher_player_id")

weekly_lateral_rushing_stats <- pbp_data %>% 
  group_by(posteam, defteam, week, lateral_rusher_player_name, lateral_rusher_player_id) %>% 
  summarize(lat_rus_yds = sum(lateral_rushing_yards, na.rm = TRUE)) %>% 
  filter(!is.na(lateral_rusher_player_name)) %>%
  rename("player" = "lateral_rusher_player_name") %>% 
  rename("player_id" = "lateral_rusher_player_id")

weekly_td_stats <- pbp_data %>% 
  group_by(posteam, defteam, week, td_player_name, td_player_id) %>% 
  summarize(rus_tds = sum(rush_touchdown, na.rm = TRUE),
            rec_tds = sum(pass_touchdown, na.rm = TRUE)) %>% 
  filter(!is.na(td_player_name)) %>%
  rename("player" = "td_player_name") %>% 
  rename("player_id" = "td_player_id")

weekly_scramble_stats <- pbp_data %>% 
  filter(qb_scramble == 1) %>% 
  group_by(posteam, defteam, week, rusher_player_name, rusher_player_id) %>% 
  summarize(sc_att = sum(qb_scramble, na.rm = TRUE),
            sc_yds = sum(rushing_yards, na.rm = TRUE),
            sc_tds  = sum(passer == td_player_name, na.rm = TRUE)) %>% 
  filter(!is.na(rusher_player_name)) %>%
  rename("player" = "rusher_player_name") %>% 
  rename("player_id" = "rusher_player_id")

weekly_fumble_stats <- pbp_data %>% 
  filter(pass == 1| rush == 1) %>% 
  group_by(posteam, defteam, week, fumbled_1_player_name, fumbled_1_player_id) %>% 
  summarize(fmb = n(),
            fmb_l = sum(fumbled_1_team != fumble_recovery_1_team, na.rm = TRUE)) %>% 
  filter(!is.na(fumbled_1_player_name)) %>%
  rename("player" = "fumbled_1_player_name") %>% 
  rename("player_id" = "fumbled_1_player_id")

#list for each stat
dfs_list <- list(weekly_passing_stats, weekly_rushing_stats, weekly_lateral_rushing_stats, weekly_receiving_stats, weekly_lateral_receiving_stats, weekly_td_stats, weekly_scramble_stats, weekly_fumble_stats)

#combine stats
weekly_stats <- reduce(dfs_list, full_join, by = c("player", "player_id", "posteam", "defteam", "week")) 

#set NAs to 0
weekly_stats[is.na(weekly_stats)] <- 0

#combine lateral yards and join full name
weekly_stats <- weekly_stats %>% 
  mutate(rus_yds = rus_yds + lat_rus_yds,
         rec_yds = rec_yds + lat_rec_yds) %>% 
  ungroup() %>% 
  left_join(roster, by = c("player_id" = "gsis_id")) %>% 
  select(!player) %>% 
  rename("player" = "full_name",
         "pos" = "position",
         "team" = "posteam",
         "opp" = "defteam") %>% 
  select(player, pos, week, team, opp, pas_att, cmp, pas_yds, pas_tds, int, sc_att, sc_yds, sc_tds, rus_att, rus_yds, rus_tds, tgt, rec, rec_yds, rec_tds, fmb, fmb_l)

#player names func
weekly_stats <- player_names_func(weekly_stats)

#join snaps
weekly_stats <- weekly_stats %>% 
  full_join(snaps, by = c("player", "pos", "team", "opp", "week")) %>% 
  filter(pos %in% c("QB", "RB", "WR", "TE")) %>% 
  select(player, pos, week, team, opp, pas_att, cmp, pas_yds, pas_tds, int, sc_att, sc_yds, sc_tds, rus_att, rus_yds, rus_tds, tgt, rec, rec_yds, rec_tds, snap_per, st_snaps, fmb, fmb_l)

#NAs to 0
weekly_stats[is.na(weekly_stats)] <- 0

#touches
weekly_stats <- weekly_stats %>% 
  mutate(touches = rus_att + rec)

