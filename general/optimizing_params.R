

#simulations function
run_model_simulations <- function(params){
  
  ###create 2d lists
  weekly_loss_2d <- create_2d_list()
  
  player_predictions_2d <- create_2d_list()
  team_predictions_2d <- create_2d_list()
  
  player_percents_adjusted_2d <- create_2d_list()
  player_percents_2d <- create_2d_list()
  off_team_ratings_2d <- create_2d_list()
  def_team_ratings_2d <- create_2d_list()
  qb_ratings_2d <- create_2d_list()
  
  ###2021
  player_percents_2d[["2021"]][["Week_19"]] <- week19_player_percents_2021
  qb_ratings_2d[["2021"]][["Week_19"]] <- week19_qb_ratings_2021
  off_team_ratings_2d[["2021"]][["Week_19"]] <- week19_off_team_ratings_2021
  def_team_ratings_2d[["2021"]][["Week_19"]] <- week19_def_team_ratings_2021
  
  #source("general/first_season_setup.R")
  
  year <- 2022
  
  ###preseason adjustments
  preseason <- preseason_adjustments(year, params, player_percents_2d, qb_ratings_2d, off_team_ratings_2d, def_team_ratings_2d)
  player_percents_2d[[as.character(year)]][[paste0("Week_", 1)]] <- preseason$player_percents
  qb_ratings_2d[[as.character(year)]][[paste0("Week_", 1)]] <- preseason$qb_ratings
  off_team_ratings_2d[[as.character(year)]][[paste0("Week_", 1)]] <- preseason$off_team_ratings
  def_team_ratings_2d[[as.character(year)]][[paste0("Week_", 1)]] <- preseason$def_team_ratings
  
  c <- 1
  
  while(c < 19){
    week <- c
    
    ###make predictions
    preds <- make_predictions(year, week, params, player_percents_2d, qb_ratings_2d, off_team_ratings_2d, def_team_ratings_2d)
    player_predictions_2d[[as.character(year)]][[paste0("Week_", week)]] <- preds$player_predictions
    team_predictions_2d[[as.character(year)]][[paste0("Week_", week)]] <- preds$team_predictions
    player_percents_adjusted_2d[[as.character(year)]][[paste0("Week_", week)]] <- preds$player_percents_adjusted
    
    ###update ratings
    updates <- update_ratings(year, week, params, player_percents_2d, qb_ratings_2d, off_team_ratings_2d, def_team_ratings_2d, player_predictions_2d, team_predictions_2d, player_percents_adjusted_2d)
    player_percents_2d[[as.character(year)]][[paste0("Week_", week+1)]] <- updates$player_percents
    qb_ratings_2d[[as.character(year)]][[paste0("Week_", week+1)]] <- updates$qb_ratings
    off_team_ratings_2d[[as.character(year)]][[paste0("Week_", week+1)]] <- updates$off_team_ratings
    def_team_ratings_2d[[as.character(year)]][[paste0("Week_", week+1)]] <- updates$def_team_ratings
    
    c <- c+1
  }
  
  year <- year+1
  
  while(year < 2026){
    ###preseason adjustments
    preseason <- preseason_adjustments(year, params, player_percents_2d, qb_ratings_2d, off_team_ratings_2d, def_team_ratings_2d)
    player_percents_2d[[as.character(year)]][[paste0("Week_", 1)]] <- preseason$player_percents
    qb_ratings_2d[[as.character(year)]][[paste0("Week_", 1)]] <- preseason$qb_ratings
    off_team_ratings_2d[[as.character(year)]][[paste0("Week_", 1)]] <- preseason$off_team_ratings
    def_team_ratings_2d[[as.character(year)]][[paste0("Week_", 1)]] <- preseason$def_team_ratings
    
    c <- 1
    
    while(c < 19){
      week <- c
      
      #make predictions
      preds <- make_predictions(year, week, params, player_percents_2d, qb_ratings_2d, off_team_ratings_2d, def_team_ratings_2d)
      player_predictions_2d[[as.character(year)]][[paste0("Week_", week)]] <- preds$player_predictions
      team_predictions_2d[[as.character(year)]][[paste0("Week_", week)]] <- preds$team_predictions
      player_percents_adjusted_2d[[as.character(year)]][[paste0("Week_", week)]] <- preds$player_percents_adjusted
      
      #get weekly loss
      weekly_loss_2d[[as.character(year)]][[paste0("Week_", week)]] <- get_weekly_loss(year, week, player_predictions_2d)
      
      #update ratings
      updates <- update_ratings(year, week, params, player_percents_2d, qb_ratings_2d, off_team_ratings_2d, def_team_ratings_2d, player_predictions_2d, team_predictions_2d, player_percents_adjusted_2d)
      player_percents_2d[[as.character(year)]][[paste0("Week_", week+1)]] <- updates$player_percents
      qb_ratings_2d[[as.character(year)]][[paste0("Week_", week+1)]] <- updates$qb_ratings
      off_team_ratings_2d[[as.character(year)]][[paste0("Week_", week+1)]] <- updates$off_team_ratings
      def_team_ratings_2d[[as.character(year)]][[paste0("Week_", week+1)]] <- updates$def_team_ratings
      
      c <- c+1
    }
    
    year <- year+1
  }
  
  return(weekly_loss_2d)
}

#total loss function
get_total_loss <- function(params){
  weekly_loss_2d <- run_model_simulations(params)
  
  print(params)
  
  loss <- sum(unlist(weekly_loss_2d), na.rm = TRUE)
  
  return(loss)
}



total_loss <- get_total_loss(initial_params)
total_loss

weekly_loss_2d

#new_total_loss <- total_loss
#high_qb_adj_total_loss <- total_loss

print(low_qb_adj_total_loss)
print(high_qb_adj_total_loss)
print(new_total_loss)

result <- optim(
  par = initial_params,
  fn = get_total_loss,
  method = "L-BFGS-B",
  lower = lower_bounds,
  upper = upper_bounds,
  control = list(maxit = 5,trace = 1)
)

test_result <- parLapply(cl, list(initial_params, initial_params), get_total_loss)
print(test_result)

p2 <- initial_params
p2["qb_adj"] <- p2["qb_adj"] + 0.01  # or whichever of your 5 test params
test_result2 <- parLapply(cl, list(initial_params, p2), get_total_loss)
print(test_result2)

####Parallelizing
cl <- makeCluster(20)
clusterExport(cl, varlist = c(
  #functions
  "get_total_loss",
  "run_model_simulations",
  "make_predictions",
  "preseason_adjustments",
  "update_ratings",
  "get_weekly_loss",
  "create_2d_list",
  "player_names_func",
  
  #data
  "get_total_loss", "run_model_simulations", "yahoo_2d", "player_stats_2d", "starting_qbs_2d",
  "draft_1d", "starting_qbs_1d", "depth_chart_2025", "teams",   "week19_player_percents_2021", "week19_qb_ratings_2021",
  "week19_off_team_ratings_2021", "week19_def_team_ratings_2021"
))

clusterEvalQ(cl, {
  library(dplyr)
  library(janitor)
  library(readr)
  library(stringr)
  library(tidyr) 
})

log_file <- "~/R Stuff/FantasyFootball 2.0/optim_log.csv"
if (!file.exists(log_file)) {
  write.csv(data.frame(t(c(names(initial_params), "loss"))), 
            log_file, row.names = FALSE, col.names = FALSE)
}

logged_base_fn <- function(params) {
  loss <- get_total_loss(params)
  row <- c(params, loss = loss)
  write.table(t(row), log_file, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
  loss
}

clusterExport(cl, "logged_base_fn")
clusterExport(cl, "log_file")

parallel_gradient <- function(params, fn, eps_frac = 0.01, cluster) {
  n <- length(params)
  eps <- pmax(abs(params) * eps_frac, 1e-6)
  
  param_list <- list()
  for (i in seq_len(n)) {
    p_plus <- params; p_plus[i] <- min(p_plus[i] + eps[i], upper_bounds[i])
    param_list[[paste0(i, "_plus")]] <- p_plus
    
    p_minus <- params; p_minus[i] <- max(p_minus[i] - eps[i], lower_bounds[i])
    param_list[[paste0(i, "_minus")]] <- p_minus
  }
  results <- parLapply(cluster, param_list, fn)
  
  for (nm in names(param_list)) {
    row <- c(param_list[[nm]], loss = results[[nm]])
    write.table(t(row), log_file, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
  }
  
  grad <- numeric(n)
  for (i in seq_len(n)) {
    grad[i] <- (results[[paste0(i, "_plus")]] - results[[paste0(i, "_minus")]]) / (2 * eps[i])
  }
  names(grad) <- names(params)
  grad
}

result <- optim(
  par = initial_params,
  fn = logged_base_fn,
  gr = function(p) parallel_gradient(p, get_total_loss, cluster = cl),
  method = "L-BFGS-B",
  lower = lower_bounds,
  upper = upper_bounds,
  control = list(maxit = 5, trace = 1)
)

stopCluster(cl)

####Params####
initial_params <- c(
  rus_vol_a = 0.05,
  rus_vol_b = 0.5,
  rus_vol_c = 0.5,
  rus_vol_d = 17,
  # 
  # rec_vol_a = 0.05,
  # rec_vol_b = 0.3,
  # rec_vol_c = 1,
  # rec_vol_d = 50,
  # 
  # upd_val_1_1 = 0.9,
  # upd_val_2_1 = 0.5,
  # upd_val_2_2 = 0.2,
  # upd_val_3 = 0.7,
  # upd_val_4_1 = 0.8,
  # upd_val_4_2 = 0,
  # upd_val_5_1 = 0.4,
  # upd_val_5_2 = 0.2,
  # upd_val_5_3 = 0,
  # 
  # qb_vol_a = 0.1,
  # qb_vol_b = 0.07,
  # qb_vol_c = 34,
  # 
  # off_vol_a = 0.8,
  # off_vol_b = 0.2,
  # 
  # def_vol_a = 0.3,
  # def_vol_b = 0.2,
  
  qb_adj = 0.7#,
  
  # rus_att_upd_coef = 0,
  # rus_yds_upd_coef = 0,
  # rus_tds_upd_coef = 0,
  # pas_att_upd_coef = 0.1,
  # cmp_upd_coef = 0.4,
  # pas_yds_upd_coef = 0.1,
  # pas_tds_upd_coef = 0,
  # 
  # cmp_off_coef = 0.7,
  # pas_att_off_coef = 0.7,
  # pas_yds_off_coef = 0.8,
  # pas_tds_off_coef = 0.8,
  # int_off_coef = 0.7,
  # rus_att_off_coef = 0.6,
  # rus_yds_off_coef = 0.5,
  # rus_tds_off_coef = 0.5,
  # 
  # sc_td_coef = 0.2
)

param_scales <- c(
  rus_vol_a = 0.1,
  rus_vol_b = 1,
  rus_vol_c = 1,
  rus_vol_d = 30,
  qb_adj = 1
)

lower_bounds = c(
  rus_vol_a = 0,
  rus_vol_b = 0,
  rus_vol_c = 0,
  rus_vol_d = 0,
  # 
  # rec_vol_a = 0,
  # rec_vol_b = 0,
  # rec_vol_c = 0,
  # rec_vol_d = 0,
  # 
  # upd_val_1_1 = 0,
  # upd_val_2_1 = 0,
  # upd_val_2_2 = 0,
  # upd_val_3 = 0,
  # upd_val_4_1 = 0,
  # upd_val_4_2 = 0,
  # upd_val_5_1 = 0,
  # upd_val_5_2 = 0,
  # upd_val_5_3 = 0,
  # 
  # qb_vol_a = 0,
  # qb_vol_b = 0,
  # qb_vol_c = 0,
  # 
  # off_vol_a = 0,
  # off_vol_b = 0,
  # 
  # def_vol_a = 0,
  # def_vol_b = 0,
  
  qb_adj = 0#,
  
  # rus_att_upd_coef = 0,
  # rus_yds_upd_coef = 0,
  # rus_tds_upd_coef = 0,
  # pas_att_upd_coef = 0,
  # cmp_upd_coef = 0,
  # pas_yds_upd_coef = 0,
  # pas_tds_upd_coef = 0,
  # 
  # cmp_off_coef = 0,
  # pas_att_off_coef = 0,
  # pas_yds_off_coef = 0,
  # pas_tds_off_coef = 0,
  # int_off_coef = 0,
  # rus_att_off_coef = 0,
  # rus_yds_off_coef = 0,
  # rus_tds_off_coef = 0,
  # 
  # sc_td_coef = 0
)

upper_bounds = c(
  rus_vol_a = 1,
  rus_vol_b = 1,
  rus_vol_c = 5,
  rus_vol_d = 100,
  # 
  # rec_vol_a = 1,
  # rec_vol_b = 1,
  # rec_vol_c = 5,
  # rec_vol_d = 100,
  # 
  # upd_val_1_1 = 1,
  # upd_val_2_1 = 1,
  # upd_val_2_2 = 1,
  # upd_val_3 = 1,
  # upd_val_4_1 = 1,
  # upd_val_4_2 = 1,
  # upd_val_5_1 = 1,
  # upd_val_5_2 = 1,
  # upd_val_5_3 = 1,
  # 
  # qb_vol_a = 1,
  # qb_vol_b = 1,
  # qb_vol_c = 100,
  # 
  # off_vol_a = 1,
  # off_vol_b = 1,
  # 
  # def_vol_a = 1,
  # def_vol_b = 1,
  
  qb_adj = 1#,
  
  # rus_att_upd_coef = 1,
  # rus_yds_upd_coef = 1,
  # rus_tds_upd_coef = 1,
  # pas_att_upd_coef = 1,
  # cmp_upd_coef = 1,
  # pas_yds_upd_coef = 1,
  # pas_tds_upd_coef = 1,
  # 
  # cmp_off_coef = 1,
  # pas_att_off_coef = 1,
  # pas_yds_off_coef = 1,
  # pas_tds_off_coef = 1,
  # int_off_coef = 1,
  # rus_att_off_coef = 1,
  # rus_yds_off_coef = 1,
  # rus_tds_off_coef = 1,
  # 
  # sc_td_coef = 1
)

