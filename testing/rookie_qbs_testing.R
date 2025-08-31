

d_qb_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/rookie_qb_stats.csv", sep = ""))) %>% 
  clean_names()

qb_stats <- d_qb_stats %>% 
  select(player, pick, draft_year, g_number, week, team, opp, att, cmp_20, yds_24, td, int, off_percent_40)

colnames(qb_stats) <- c("player", "pick", "draft_year", "g_number", "week", "team", "opp", "pas_att", "cmp", "pas_yds", "pas_tds", "int", "snp_per")

qb_stats <- qb_stats %>% 
  mutate(oo_sq_pick = 1/sqrt(pick)) %>% 
  filter(week < 6)


mod <- lm(pas_att ~ log(pick), qb_stats)
summary(mod)

mod2 <- lm(pas_att ~ oo_sq_pick, qb_stats)
summary(mod2)

mod3 <- lm(pas_yds ~ g_number, qb_stats)
summary(mod3)

qb_stats$resid <- resid(mod)
qb_stats$resid2 <- resid(mod2)

ggplot(qb_stats, aes(x = log(pick), y = resid)) +
  geom_point()

ggplot(qb_stats, aes(x = log(pick), y = resid2)) +
  geom_point()


residuals <- qb_stats %>% 
  group_by(pick) %>% 
  summarize(avg_resid = mean(resid),
            avg_resid2 = mean(resid2),
            count= n())
 


