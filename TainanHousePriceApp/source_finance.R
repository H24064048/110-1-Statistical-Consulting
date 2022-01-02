rate_func <- function(x, r){
  return((1+r)^(x-1))
}

vec_Sn_r <- function(len, rate){  
  #rate is in 0.xx
  return(rate_func((1:len), rate))
}


simulate_finance <- function(start_capital = 1000000, annual_cap_return = 5.0, annual_cap_dev = 3,
                             annual_inflation = 2.5,
                             annual_netincome = 500000, annual_income_growth = 1, 
                             monthly_expense = 10000, expense_growth = 1,
                             n_obs = 20,
                             monthly_rent = 10000, rent_annual_incre = 1, target_house_price = 8000000, left_house_price = 7000000,
                             handover_year = 3, loan_int_rate = 1.8){
  
  ann_cap_grow <- annual_cap_return/100
  ann_cap_dev  <- annual_cap_dev/100
  ann_infla      <- annual_inflation/100
  ann_inc_grow   <- annual_income_growth/100
  ann_rent_inc   <- rent_annual_incre/100
  ann_exp_grow   <- expense_growth/100
  ann_loan_rate  <- loan_int_rate/100
  yearly_expense <- monthly_expense*12
  yearly_rent    <- monthly_rent*12
  n_pay          <- n_obs - handover_year
  first_payment  <- target_house_price - left_house_price
  
  # Initial capital
  # First payment
  
  # Investment return
  dev_v <- qnorm(runif(n_obs,0,1))
  year_roi_v <- 1 + ann_cap_grow - dev_v*ann_cap_dev
  
  # Income-Expense
  year_income_v  <- annual_netincome*vec_Sn_r(n_obs, ann_inc_grow)  
  year_expense_v <- yearly_expense*vec_Sn_r(n_obs, ann_exp_grow)
  
  # Plan Rent House
  year_rent_v <- yearly_rent*vec_Sn_r(n_obs, ann_rent_inc)
  
  # Plan Buy House
  buy_rent <- yearly_rent*vec_Sn_r(handover_year, ann_rent_inc)
  year_laon <- (((1+ann_loan_rate)^n_pay)*ann_loan_rate)/(((1+ann_loan_rate)^n_pay)-1)*left_house_price
  year_laon_v <- rep(year_laon, n_pay)
  buy_expense_v <- c(buy_rent, year_laon_v)
  
  # Total
  capital_rent <- start_capital
  yearly_cap_rent <- rep(NA, n_obs)
  capital_buy <- start_capital - first_payment
  yearly_cap_buy <- rep(NA, n_obs)
  for(i in (1:n_obs)){
    rent_yearly_start <- capital_rent*year_roi_v[i]
    yearly_cap_rent[i] <- rent_yearly_start + year_income_v[i] - year_expense_v[i] - year_rent_v[i]
    buy_yearly_start  <- capital_buy*year_roi_v[i]
    yearly_cap_buy[i]  <- buy_yearly_start + year_income_v[i] - year_expense_v[i] - buy_expense_v[i]
    capital_rent <- yearly_cap_rent[i]
    capital_buy  <- yearly_cap_buy[i]
  }
  return(list(rent=yearly_cap_rent, buy=yearly_cap_buy))
}
