
year <- 2022

This_Year_d <- year

c <- 1

past_week <- c
upcoming_week <- c+1
source("update_ratings.R")

c <- 2

while(c < 19){
  upcoming_week <- c
  source("make_predictions.R")
  
  past_week <- c
  upcoming_week <- c+1
  source("update_ratings.R")
  
  c <- c+1
}

year <- year+1

while(year < 2025){
  Past_Year_d <- year-1
  This_Year_d <- year
  
  source("preSeason_adjustments.R")
  
  c <- 1
  
  while(c < 19){
    upcoming_week <- c
    source("make_predictions.R")
    
    past_week <- c
    upcoming_week <- c+1
    source("update_ratings.R")
    
    c <- c+1
  }
  
  year <- year+1
}

Past_Year_d <- year-1
This_Year_d <- year

source("preSeason_adjustments.R")
