


#preseason/This_Year
folder <- eval(paste("~/R Stuff/FantasyFootball 2.0/preseason/", This_Year, "/", sep = ""))
if (!dir.exists(folder)) {dir.create(folder)}

#fullSeasonPredictions/This_Year
# folder <- eval(paste("~/R Stuff/FantasyFootball 2.0/fullSeasonPredictions/", This_Year, "/", sep = ""))
# if (!dir.exists(folder)) {dir.create(folder)}

#weeklyRatings/This_Year
folder <- eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/", sep = ""))
if (!dir.exists(folder)) {dir.create(folder)}

#weeks
c <- 1
while(c < 20){
  folder <- eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyRatings/", This_Year, "/Week_", c, sep = ""))
  if (!dir.exists(folder)) {dir.create(folder)}
  c <- c + 1
}

#weeklyAdjusted/This_Year
folder <- eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyAdjusted/", This_Year, "/", sep = ""))
if (!dir.exists(folder)) {dir.create(folder)}

#weeks
c <- 1
while(c < 19){
  folder <- eval(paste("~/R Stuff/FantasyFootball 2.0/weeklyAdjusted/", This_Year, "/Week_", c, sep = ""))
  if (!dir.exists(folder)) {dir.create(folder)}
  c <- c + 1
}

