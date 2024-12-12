

d_past_week_st_snaps <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/", This_Year, "/Special_Teams_Snaps.csv", sep = ""))) %>% 
  clean_names()

a <- 1
while(a < 19){
  temp <- d_past_week_st_snaps %>% 
    filter(week == a)
  
  write_csv(temp, eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyStats/", This_Year, "/byWeek/Week_", a, "_ST_Snaps.csv", sep = "")))

  a <- a+1
}
