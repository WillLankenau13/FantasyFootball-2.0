library("tidyverse")
library("lpSolve")

#Year and Week
upcoming_week <- 4
This_Year <- This_Year_d

#Download combined data
optim <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/combinedWeeklyPredictions/", This_Year, "/Week_", upcoming_week, ".csv", sep = "")))

#Data with betting over/unders
optim_wo <- read_csv(eval(paste("~/R Stuff/FantasyFootball 2.0/combinedWeeklyPredictions/", This_Year, "/Week_", upcoming_week, "_With_Betting_Odds.csv", sep = "")))

optim_wo <- optim_wo %>%
  mutate(FPTS = Avg) %>%
  select(Player:PAR_PD)

colnames(optim_wo) <- colnames(optim)

#combine data with and without betting odds
optim <- optim %>%
  filter(!(Player %in% optim_wo$Player))

optim <- rbind(optim, optim_wo)


#filter out teams that players cannot be picked from
filter_teams <- c("ARI", "SEA", "CIN", "DEN", "GB", "DAL", "NYJ", "MIA", "PIT", "MIN")

optim <- optim %>% 
  filter(!(Team %in% filter_teams))

#Players I do not want to have in my fantasy lineup
not_picking <- c()

optim <- optim %>% 
  filter(!(Player %in% not_picking))

#Players I want in my fantasy lineup
players_picking <- c()


#set up for optimization
optim <- optim %>% 
  arrange(Salary, desc(FPTS)) %>% 
  mutate(ones = 1,
         zeroes = 0,
         picking = ifelse(Player %in% players_picking, 1, 0))

#By position
QB <- optim %>% 
  filter(Pos == "QB")

RB <- optim %>% 
  filter(Pos == "RB")

WR <- optim %>% 
  filter(Pos == "WR")

TE <- optim %>% 
  filter(Pos == "TE")

DST <- optim %>% 
  filter(Pos == "DST")

#Combine (need for proper ordering of lists)
Players_o <- rbind(QB, RB, WR, TE, DST)


#Optimization

#Decision Matrix
Objective.in <- c(Players_o$FPTS)

#Constraint Matrix
Const.mat <- matrix(c(Players_o$Salary,
                      Players_o$ones,
                      Players_o$picking,
                      QB$ones, RB$zeroes, WR$zeroes, TE$zeroes, DST$zeroes,
                      QB$zeroes, RB$ones, WR$zeroes, TE$zeroes, DST$zeroes,
                      QB$zeroes, RB$zeroes, WR$ones, TE$zeroes, DST$zeroes,
                      QB$zeroes, RB$zeroes, WR$zeroes, TE$ones, DST$zeroes,
                      QB$zeroes, RB$zeroes, WR$zeroes, TE$zeroes, DST$ones,
                      QB$zeroes, RB$ones, WR$zeroes, TE$zeroes, DST$zeroes,
                      QB$zeroes, RB$zeroes, WR$ones, TE$zeroes, DST$zeroes,
                      QB$zeroes, RB$zeroes, WR$zeroes, TE$ones, DST$zeroes
                      ), nrow = 11, byrow = TRUE)

#Define Constraints
Salary_con <- 200
Player_con <- 9
Picking_con <- length(players_picking)
QB_con <- 1
RB_con <- 3
WR_con <- 4
TE_con <- 2
DST_con <- 1
min_RB_con <- 2
min_WR_con <- 3
min_TE_con <- 1

#Constraint Rhs
Const.rhs <- c(Salary_con, Player_con, Picking_con, QB_con, RB_con, WR_con, TE_con, DST_con, min_RB_con, min_WR_con, min_TE_con)

#Constraint Directions
Const.dir<-c("<=", "=", "=", "=", "<=", "<=", "<=", "=", ">=", ">=", ">=")

#Optimize
Optimum<-lp(direction = "max", Objective.in, Const.mat, Const.dir, Const.rhs, all.bin = TRUE)

#matrices
fpts_matrix <- matrix(c(Players_o$FPTS))
salary_matrix <- matrix(c(Players_o$Salary))

solution_matrix <- Optimum[["solution"]]

#Print Expected Points
sum(fpts_matrix*solution_matrix)

#Print Salary
sum(salary_matrix*solution_matrix)

#Print Team
Players_o["Selection"] <- solution_matrix

#get lineup
team <- Players_o %>% 
  filter(Selection == 1) %>% 
  select(Player, Pos, Team, Opp, Salary, FPTS, ppd, PAR_PD)


#Print linup and projected points
team
sum(fpts_matrix*solution_matrix)














