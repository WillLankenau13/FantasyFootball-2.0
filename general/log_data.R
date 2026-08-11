
year <- 2025

model_verson <- "1.0"


c <- 1
while(c < 19){
  week <- c
  
  player_data <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyPredictions/", year, "/Week_", week, "_Player_Predictions.csv", sep = "")))
  
  team_data <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyPredictions/", year, "/Week_", week, "_Team_Predictions.csv", sep = "")))
  
  
  
  write_csv(player_data, eval(paste("~/R Stuff/FantasyFootball 2.0/predictionsLibrary/backlog/model_v", model_verson, "/", year, "/Week_", week, "_", year, "_Player_Predictions_v", model_verson, ".csv", sep = "")))
  write_csv(team_data, eval(paste("~/R Stuff/FantasyFootball 2.0/predictionsLibrary/backlog/model_v", model_verson, "/", year, "/Week_", week, "_", year, "_Team_Predictions_v", model_verson, ".csv", sep = "")))
  
  c <- c+1
}
