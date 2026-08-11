
This_Year <- This_Year_d

#preseason/ratings/This_Year
folder <- eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/ratings/", This_Year, "/", sep = ""))
if (!dir.exists(folder)) {dir.create(folder)}

#fullSeasonPredictions/This_Year
# folder <- eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonPredictions/", This_Year, "/", sep = ""))
# if (!dir.exists(folder)) {dir.create(folder)}

#weeklyRatings/This_Year
folder <- eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyRatings/", This_Year, "/", sep = ""))
if (!dir.exists(folder)) {dir.create(folder)}

#weeks
c <- 1
while(c < 20){
  folder <- eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyRatings/", This_Year, "/Week_", c, sep = ""))
  if (!dir.exists(folder)) {dir.create(folder)}
  c <- c + 1
}

#weeklyAdjusted/This_Year
folder <- eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyAdjusted/", This_Year, "/", sep = ""))
if (!dir.exists(folder)) {dir.create(folder)}

#weeks
c <- 1
while(c < 19){
  folder <- eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyAdjusted/", This_Year, "/Week_", c, sep = ""))
  if (!dir.exists(folder)) {dir.create(folder)}
  c <- c + 1
}


#fantasypros/This_Year
folder <- eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyPros/", This_Year, "/", sep = ""))
if (!dir.exists(folder)) {dir.create(folder)}

#fantasypros/fantasyProsFullSeasonPredictions/This_Year
folder <- eval(paste("~/R Stuff/FantasyFootball 2.0/fantasyPros/fantasyProsFullSeasonPredictions/", This_Year, "/", sep = ""))
if (!dir.exists(folder)) {dir.create(folder)}

#weeklyData/startingQBs/This_Year
folder <- eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/startingQBs/", This_Year, "/", sep = ""))
if (!dir.exists(folder)) {dir.create(folder)}

#weeklyData/weeklyPredictions/This_Year
folder <- eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyPredictions/", This_Year, "/", sep = ""))
if (!dir.exists(folder)) {dir.create(folder)}

#weeklyData/weeklyStats/This_Year/byWeek
folder <- eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyStats/", This_Year, "/", sep = ""))
if (!dir.exists(folder)) {dir.create(folder)}
folder <- eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/weeklyStats/", This_Year, "/byWeek/", sep = ""))
if (!dir.exists(folder)) {dir.create(folder)}

#weeklyData/Yahoo/This_Year
folder <- eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyData/Yahoo/", This_Year, "/", sep = ""))
if (!dir.exists(folder)) {dir.create(folder)}

