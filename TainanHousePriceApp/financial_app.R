library(shiny)
library(ggplot2)
library(patchwork)
library(reshape2)
library(dplyr)

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
simulate_finance()

#n_sim <- 50
#sim1 <- data.frame(t(replicate(n_sim,simulate_finance())))
#sim1$ind <- 1:n_sim

#cap_rent <- melt(sim1$rent, value.name = 'Capital_Rent')
#cap_rent$ind <- rep(1:20,n_sim)
#cap_buy  <- melt(sim1$buy, value.name = 'Capital_Buy')
#cap_buy$ind <- rep(1:20,n_sim)

# p1 <- ggplot(data= cap_rent, aes(x=ind, y=Capital_Rent/1000000, group=L1, color=L1))+
#   scale_color_gradient2(midpoint=median(1:n_sim),low="steelblue4", high="firebrick4", mid='bisque3', space ="Lab", guide = FALSE)+
#   geom_line()+
#   labs(x= 'Year',y= 'Capital (Millions)',title= '')
# 
# p2 <- ggplot(data= subset(cap_rent, ind==20), aes(x=Capital_Rent/1000000))+
#   geom_density()+
#   labs(x = 'Final Capital (M)',
#        y = 'Density',
#        title = '')
# 
# p1 / (p2+p2)

#(((1+0.02)^12)*0.02)/(((1+0.02)^12)-1)*10000 #loan and interest average pay 12 perior (1y) 24 year rate
#https://ebank.taipeifubon.com.tw/B2C/cfhqu/cfhqu018/CFHQU018_Home.faces

#here he smartly uses paste0 to name variable to call in the server,
#and since two column are the same, the function can be called once.

# Define UI for application that plots random distributions
ui <-  fluidPage(
          
          fluidRow(
            column(3, tags$h3("Financial Status")),
            column(6, tags$h3("Housing Plan Simulation")),
            column(3, tags$h3("Buy One!"))
          ),
          fluidRow(
            column(3,
                   sliderInput("n_obs", "Years to come (To clear debt)", min = 0, max = 40, value = 20),
                   sliderInput("start_capital", "Initial capital", min = 400000, max = 5000000, value = 1000000, step = 100000, pre = "$", sep = ","),
                   sliderInput("annual_cap_return", "Median investment return (in %)", min = 0.0, max = 30.0, value = 5.0, step = 0.5),
                   sliderInput("annual_cap_dev", "Investment fluctuation rate(in %)", min = 0, max = 30.0, value = 3, step = 1),
                   sliderInput("annual_netincome", "Annual net income", min = 300000, max = 2000000, value = 500000, step = 50000, pre = "$", sep = ","),
                   sliderInput("annual_income_growth", "Annual Income growth (in %)", min = 0, max = 20, value = 1, step = 0.5),
                   sliderInput("monthly_expense", "Monthly expenditure", min = 1000, max = 100000, value = 10000, step = 1000, pre = "$", sep = ","),
                   sliderInput("expense_growth", "Expenditure growth (in %)", min = 0, max = 20, value = 1, step = 0.5)
                   ),
            column(6,
                   plotOutput("FinSimPlot", height = "600px")),
            column(3,
                   sliderInput("annual_inflation", "Annual inflation (in %)", min = 0, max = 10, value = 2.5, step = 0.1),
                   sliderInput("monthly_rent", "Rent (Monthly)", min = 4000, max = 30000, value = 10000, step = 1000, pre = "$", sep = ","),
                   sliderInput("rent_annual_incre", "Rent Increase (Monthly)  (%)", min = 0, max = 15, value= 0, step=0.1),
                   sliderInput("target_house_price", "Buy house price", min = 3000000, max = 15000000, value = 8000000, step = 500000, pre = "$", sep = ","),
                   sliderInput("left_house_price", "Remaining house payment (Loan)", min = 3000000, max = 15000000, value = 8000000, step = 500000, pre = "$", sep = ","),
                   sliderInput("handover_year", "Handover year", min = 1, max = 10, value = 3, step = 1),
                   sliderInput("loan_int_rate", "Loan interest (%)", min = 0, max = 16, value = 1.8, step=0.1),
                   actionButton("Sim_butt", "Start simulation", icon("random"))
                   )
                   
          )
)

server <- function(input, output) {
  
  output$FinSimPlot <- renderPlot({
    n_sim = 30
    sim1 <- data.frame(t(replicate(n_sim,simulate_finance(input$start_capital, input$annual_cap_return, input$annual_cap_dev,
                                                         input$annual_inflation, input$annual_netincome, input$annual_income_growth, 
                                                         input$monthly_expense, input$expense_growth, input$n_obs, input$monthly_rent, 
                                                         input$rent_annual_incre, input$target_house_price, input$left_house_price,
                                                         input$handover_year, input$loan_int_rate))))
    sim1$ind <- 1:n_sim
    cap_rent <- melt(sim1$rent, value.name = 'Capital_Rent')
    cap_rent$ind <- rep((1:input$n_obs),n_sim)
    cap_buy  <- melt(sim1$buy, value.name = 'Capital_Buy')
    cap_buy$ind <- rep((1:input$n_obs),n_sim)
    
    p1 <- ggplot(data= cap_rent, aes(x=ind, y=Capital_Rent/1000000, group=L1, color=L1))+
      scale_color_gradient2(midpoint=median(1:n_sim),low="steelblue4", high="firebrick4", mid='bisque3', space ="Lab", guide = "none")+
      geom_line()+
      labs(x= 'Year',y= 'Capital (Millions)',title= '')
    
    p2 <- ggplot(data= subset(cap_rent, ind==(input$n_obs)), aes(x=Capital_Rent/1000000))+
      geom_density()+
      labs(x = 'Final Capital (M)',
           y = 'Density',
           title = '')
    
    p1 / (p2+p2)
  })
  
  
}

shinyApp(ui = ui, server=server)