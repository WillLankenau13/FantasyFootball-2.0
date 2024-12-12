
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