
run_model_simulations <- function(parameters){
  
  rus_vol_a <- par["rus_vol_a"]
  rus_vol_b <- par["rus_vol_b"]
  rus_vol_c <- par["rus_vol_c"]
  rus_vol_d <- par["rus_vol_d"]
  rec_vol_a <- par["rec_vol_a"]
  rec_vol_b <- par["rec_vol_b"]
  rec_vol_c <- par["rec_vol_c"]
  rec_vol_d <- par["rec_vol_d"]
  
  upd_val_1_1 <- par["upd_val_1_1"]
  upd_val_2_1 <- par["upd_val_2_1"]
  upd_val_2_2 <- par["upd_val_2_2"]
  upd_val_3 <- par["upd_val_3"]
  upd_val_4_1 <- par["upd_val_4_1"]
  upd_val_4_2 <- par["upd_val_4_2"]
  upd_val_5_1 <- par["upd_val_5_1"]
  upd_val_5_2 <- par["upd_val_5_2"]
  upd_val_5_3 <- par["upd_val_5_3"]
  
  qb_vol_a <- par["qb_vol_a"]
  qb_vol_b <- par["qb_vol_b"]
  qb_vol_c <- par["qb_vol_c"]
  
  off_vol_a <- par["off_vol_a"]
  off_vol_b <- par["off_vol_b"]
  
  def_vol_a <- par["def_vol_a"]
  def_vol_b <- par["def_vol_b"]
  
  qb_adj <- par["qb_adj"]
  
  rus_att_upd_coef <- par["rus_att_upd_coef"]
  rus_yds_upd_coef <- par["rus_yds_upd_coef"]
  rus_tds_upd_coef <- par["rus_tds_upd_coef"]
  pas_att_upd_coef <- par["pas_att_upd_coef"]
  cmp_upd_coef <- par["cmp_upd_coef"]
  pas_yds_upd_coef <- par["pas_yds_upd_coef"]
  pas_tds_upd_coef <- par["pas_tds_upd_coef"]
  
  cmp_off_coef <- par["cmp_off_coef"]
  pas_att_off_coef <- par["pas_att_off_coef"]
  pas_yds_off_coef <- par["pas_yds_off_coef"]
  pas_tds_off_coef <- par["pas_tds_off_coef"]
  int_off_coef <- par["int_off_coef"]
  rus_att_off_coef <- par["rus_att_off_coef"]
  rus_yds_off_coef <- par["rus_yds_off_coef"]
  rus_tds_off_coef <- par["rus_tds_off_coef"]
  
  sc_td_coef =  par["sc_td_coef"]
  
  #source("general/first_season_setup.R")
  
  year <- 2022
  
  Past_Year_d <- year-1
  This_Year_d <- year
  
  preseason_adjustments(year)
  
  c <- 1
  
  while(c < 19){
    week <- c
    make_predictions(year, week)
    
    update_ratings(year, week)
    
    c <- c+1
  }
  
  year <- year+1
  
  while(year < 2026){
    preseason_adjustments(year)
    
    c <- 1
    
    while(c < 19){
      week <- c
      make_predictions(year, week)
      
      get_weekly_loss(year, week)
      
      update_ratings(year, week)
      
      c <- c+1
    }
    
    year <- year+1
  }
  
  return
}

get_total_loss <- function(){
  run_model_simulations()
  
  loss <- sum(unlist(weekly_loss_2d), na.rm = TRUE)
  
  return(loss)
}

weekly_loss_2d[["2024"]][["Week_2"]]

total_loss <- get_total_loss()

run_model_simulations()

result <- optim(
  par = par,
  fn = loss_function,
  method = "L-BFGS-B",
  lower = c(0, 0, 0, ...),
  upper = c(1, 1, 1, ...)
)

par <- c(
  rus_vol_a <- 0.05,
  rus_vol_b <- 0.5,
  rus_vol_c <- 0.5,
  rus_vol_d <- 17,
  
  rec_vol_a <- 0.05,
  rec_vol_b <- 0.3,
  rec_vol_c <- 1,
  rec_vol_d <- 50,
  
  upd_val_1_1 <- 0.9,
  upd_val_2_1 <- 0.5,
  upd_val_2_2 <- 0.2,
  upd_val_3 <- 0.7,
  upd_val_4_1 <- 0.8,
  upd_val_4_2 <- 0,
  upd_val_5_1 <- 0.4,
  upd_val_5_2 <- 0.2,
  upd_val_5_3 <- 0,
  
  qb_vol_a <- 0.1,
  qb_vol_b <- 0.07,
  qb_vol_c <- 34,
  
  off_vol_a <- 0.8,
  off_vol_b <- 0.2,
  
  def_vol_a <- 0.3,
  def_vol_b <- 0.2,
  
  qb_adj <- 0.7,
  
  rus_att_upd_coef <- 0,
  rus_yds_upd_coef <- 0,
  rus_tds_upd_coef <- 0,
  pas_att_upd_coef <- 0.1,
  cmp_upd_coef <- 0.4,
  pas_yds_upd_coef <- 0.1,
  pas_tds_upd_coef <- 0,
  
  cmp_off_coef <- 0.7,
  pas_att_off_coef <- 0.7,
  pas_yds_off_coef <- 0.8,
  pas_tds_off_coef <- 0.8,
  int_off_coef <- 0.7,
  rus_att_off_coef <- 0.6,
  rus_yds_off_coef <- 0.5,
  rus_tds_off_coef <- 0.5,
  
  sc_td_coef =  0.2
)

