year <- 2021


while(year < 2025){
  Past_Year_d <- year-1
  This_Year_d <- year
  
  c <- 1
  
  while(c < 19){
    past_week <- c
    source("get_weekly_stats.R")
    
    c <- c+1
  }
  
  year <- year+1
}


Past_Year_d <- year-1
This_Year_d <- year
c <- 1

while(c < 14){
  past_week <- c
  source("get_weekly_stats.R")
  
  c <- c+1
}
