

source("first_season_setup.R")

year <- 2022

Past_Year_d <- year-1
This_Year_d <- year

source("pipeline/preSeason_adjustments.R")

c <- 1

while(c < 19){
  upcoming_week <- c
  source("pipeline/make_predictions.R")
  
  past_week <- c
  upcoming_week <- c+1
  source("pipeline/update_ratings.R")
  
  c <- c+1
}

year <- year+1

while(year < 2026){
  Past_Year_d <- year-1
  This_Year_d <- year
  
  source("pipeline/preSeason_adjustments.R")
  
  c <- 1
  
  while(c < 19){
    upcoming_week <- c
    source("pipeline/make_predictions.R")
    
    past_week <- c
    upcoming_week <- c+1
    source("pipeline/update_ratings.R")
    
    c <- c+1
  }
  
  year <- year+1
}

# Past_Year_d <- year-1
# This_Year_d <- year
# 
# source("preSeason_adjustments.R")
# 
# c <- 1
# 
# while(c < 17){
#   upcoming_week <- c
#   source("make_predictions.R")
#   
#   past_week <- c
#   upcoming_week <- c+1
#   source("update_ratings.R")
#   
#   c <- c+1
# }
