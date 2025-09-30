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

data <- read_csv("~/R Stuff/FantasyFootball 2.0/SalaryCap/Draft_Data_2024.csv") %>% 
  clean_names()

optim <- data %>% 
  select(player, team_2, pos, fpts, pos_rank, par_value, inf_value, cost, estimated_cost)

#Remove players
not_picking <- c()

optim <- optim %>% 
  filter(!(player %in% not_picking))

#Ones and Zeroes
optim <- optim %>% 
  mutate(ones = 1,
         zeroes = 0) %>% 
  arrange(cost, desc(fpts))

#position
qb <- optim %>% 
  filter(pos == "QB")
rb <- optim %>% 
  filter(pos == "RB")
wr <- optim %>% 
  filter(pos == "WR")
te <- optim %>% 
  filter(pos == "TE")

players <- rbind(qb, rb, wr, te)

#Decision Matrix
Objective.in <- c(players$fpts)

#Constraint Matrix
Const.mat <- matrix(c(players$cost,
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
pred_matrix <- matrix(c(players$fpts))
cost_matrix <- matrix(c(players$cost))

solution_matrix <- Optimum[["solution"]]

#Print Expected Points
sum(pred_matrix*solution_matrix)

#Print cost
sum(cost_matrix*solution_matrix)

#Print Team
players["Selection"] <- solution_matrix

team <- players %>% 
  filter(Selection == 1) 

sum(team$estimated_cost)

team
