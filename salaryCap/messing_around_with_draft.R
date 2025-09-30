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

data <- read_csv("~/R Stuff/FantasyFootball 2.0/SalaryCap/pre_draft_2025.csv") %>% 
  clean_names() %>% 
  filter(!is.na(fpts)) %>%
  mutate(estimated_cost = as.numeric(estimated_cost),
         par_value = as.numeric(par_value))

optim <- data %>% 
  select(player, team, pos, fpts, pos_rank, par_value, cost, estimated_cost) %>% 
  filter(!is.na(estimated_cost)) %>% 
  mutate(top_guy = ifelse((pos == "QB" & pos_rank <= 4)  | (pos != "QB" & fpts > 270), 1, 0)) %>% 
  mutate(base_fpts = fpts)

a <- 1
df_list <- list()
top_list <- list()

while(a < 30000){
  k <- (floor(a/10000) + 1)/10
  
  optim <- optim %>%
    mutate(rand_cost = rnorm(n(), mean = (3*estimated_cost + 0)/3, sd = k * estimated_cost),
           rand_cost = pmax(rand_cost, 1),
           fpts = rnorm(n(), mean = base_fpts, sd = 3))
           #,
           #rand_cost = ifelse(top_guy == 1 & (rand_cost < estimated_cost), estimated_cost, rand_cost))
  
  #Remove players
  not_picking <- c()
  
  optim <- optim %>% 
    filter(!(player %in% not_picking))
  
  #Ones and Zeroes
  optim <- optim %>% 
    mutate(ones = 1,
           zeroes = 0) %>% 
    arrange(rand_cost, desc(fpts))
  
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
  Const.mat <- matrix(c(players$rand_cost,
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
  cost_matrix <- matrix(c(players$rand_cost))
  
  solution_matrix <- Optimum[["solution"]]
  
  #Print Expected Points
  sum(pred_matrix*solution_matrix)
  
  #Print cost
  sum(cost_matrix*solution_matrix)
  
  #Print Team
  players["Selection"] <- solution_matrix
  
  team <- players %>% 
    filter(Selection == 1) %>% 
    select(player, team, pos, top_guy, fpts, pos_rank, estimated_cost, par_value, rand_cost) %>% 
    mutate(overpay = rand_cost - par_value) 
  
  df_list[[a]] <- team
  top_list[[a]] <- sum(team$top_guy)
  
  
  a <- a+1
}

overall <- do.call(rbind, df_list)
top_df <- data.frame(top_count = unlist(top_list))


by_player <- overall %>% 
  group_by(player) %>% 
  summarize(count = n(),
            overpay_mean = mean(overpay),
            overpay_3rd = quantile(overpay, 0.75, na.rm = TRUE)) %>% 
  left_join(optim, by = c("player")) %>% 
  select(player, team, pos, fpts, pos_rank, par_value, estimated_cost, count, overpay_mean, overpay_3rd)


n <- top_df %>% 
  group_by(top_count) %>% 
  summarize(count = n())

t <- by_player %>% 
  select(player, overpay_mean, overpay_3rd)


write_csv(by_player, eval(paste("~/R Stuff/FantasyFootball 2.0/salaryCap/overpay_values_", This_Year, ".csv", sep = "")))




