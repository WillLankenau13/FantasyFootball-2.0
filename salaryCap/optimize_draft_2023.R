library("ggplot2")
library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library("readr")
library("dplyr")
library("modelr")
library("leaps")
library("ggrepel")
library("lpSolve")

data <- read_csv("~/R Stuff/FantasyFootball 2.0/DraftData2023.csv")

optim <- data %>% 
  select(Player, Team...2, Pos, FPTS, `Pos Rank`, `PAR Value`, `Inf Value`, Cost, `Estimated Cost`)

#Remove players
not_picking <- c("Travis Kelce")

optim <- optim %>% 
  filter(!(Player %in% not_picking))

#Ones and Zeroes
optim <- optim %>% 
  mutate(ones = 1,
         zeroes = 0) %>% 
  arrange(Cost, desc(FPTS))

#Position
qb <- optim %>% 
  filter(Pos == "QB")
rb <- optim %>% 
  filter(Pos == "RB")
wr <- optim %>% 
  filter(Pos == "WR")
te <- optim %>% 
  filter(Pos == "TE")

players <- rbind(qb, rb, wr, te)

#Decision Matrix
Objective.in <- c(players$FPTS)

#Constraint Matrix
Const.mat <- matrix(c(players$Cost,
                      players$ones,
                      qb$ones, rb$zeroes, wr$zeroes, te$zeroes,
                      qb$zeroes, rb$ones, wr$zeroes, te$zeroes,
                      qb$zeroes, rb$zeroes, wr$ones, te$zeroes,
                      qb$zeroes, rb$zeroes, wr$zeroes, te$ones,
                      qb$ones, rb$zeroes, wr$zeroes, te$zeroes,
                      qb$zeroes, rb$ones, wr$zeroes, te$zeroes,
                      qb$zeroes, rb$zeroes, wr$ones, te$zeroes,
                      qb$zeroes, rb$zeroes, wr$zeroes, te$ones
), nrow = 10, byrow = TRUE)

#Define Constraints
Salary_con <- 190
Player_con <- 8
QB_con <- 2
RB_con <- 3
WR_con <- 5
TE_con <- 3
min_QB_con <- 1
min_RB_con <- 1
min_WR_con <- 3
min_TE_con <- 1


#Constraint Rhs
Const.rhs <- c(Salary_con, Player_con, QB_con, RB_con, WR_con, TE_con, min_QB_con, min_RB_con, min_WR_con, min_TE_con)

#Constraint Directions
Const.dir<-c("<=", "=", "<=", "<=", "<=", "<=", ">=", ">=", ">=", ">=")

#Optimize
Optimum <- lp(direction = "max", Objective.in, Const.mat, Const.dir, Const.rhs, all.bin = TRUE)

#matrices
pred_matrix <- matrix(c(players$FPTS))
cost_matrix <- matrix(c(players$Cost))

solution_matrix <- Optimum[["solution"]]

#Print Expected Points
sum(pred_matrix*solution_matrix)

#Print Cost
sum(cost_matrix*solution_matrix)

#Print Team
players["Selection"] <- solution_matrix

team <- players %>% 
  filter(Selection == 1) 

sum(team$`Estimated Cost`)

team
