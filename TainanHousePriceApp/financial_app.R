library(shiny)

rate_func <- function(x, r){
  return((1+r)^(x-1))
}

vec_Sn_r <- function(len, rate){  
  #rate is in 0.xx
  return(rate_func((1:len), rate))
}

sum(10000*12*vec_Sn_r(20, 0.01))


simulate_finance <- function(start_capital = 1000000, annual_cap_return = 5.0, annual_cap_dev = 3,
                         annual_inflation = 2.5,
                         annual_netincome = 500000, annual_income_growth = 1, monthly_expense = 0,
                         n_obs = 20, n_sim = 200,
                         monthly_rent = 10000, rent_annual_incre = 0, target_house_price = 8000000,
                         loan_int_rate = 1.8){
  
  ann_cap_return <- annual_cap_return/100
  ann_infla      <- annual_inflation/100
  ann_inc_grow   <- annual_income_growth/100
  ann_rent_inc   <- rent_annual_incre/100
  yearly_expense <- monthly_expense*12
  ann_cap_add    <- annual_netincome - yearly_expense
  yearly_rent    <- monthly_rent*12
  
  
  # Plan Rent House
  cum_rent <- sum(yearly_rent*vec_Sn_r(n_obs, ann_rent_inc))
  # Plan Buy House
  buy_rent <- sum(yearly_rent*vec_Sn_r(handover_year, ann_rent_inc))
  cum_loan_int <- sum(left_house_price*vec_Sn_r((n_obs-handover_year), loan_int_rate))  #wrong
}


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
                   sliderInput("n_obs", "Years to come", min = 0, max = 40, value = 20),
                   sliderInput("start_capital", "Initial capital", min = 400000, max = 5000000, value = 1000000, step = 100000, pre = "$", sep = ","),
                   sliderInput("annual_cap_return", "Median investment return (in %)", min = 0.0, max = 30.0, value = 5.0, step = 0.5),
                   sliderInput("annual_cap_dev", "Investment fluctuation rate(in %)", min = 0, max = 30.0, value = 3, step = 1),
                   sliderInput("annual_inflation", "Annual inflation (in %)", min = 0, max = 20, value = 2.5, step = 0.1),
                   sliderInput("annual_netincome", "Annual net income", min = 300000, max = 2000000, value = 500000, step = 50000, pre = "$", sep = ","),
                   sliderInput("annual_income_growth", "Annual Income growth (in %)", min = 0, max = 20, value = 1, step = 0.5),
                   sliderInput("monthly_expense", "Monthly expenditure", min = 1000, max = 100000, value = 10000, step = 1000, pre = "$", sep = ",")
                   ),
            column(6,
                   plotOutput("a_distPlot", height = "600px")),
            column(3,
                   sliderInput("monthly_rent", "Rent (Monthly)", min = 4000, max = 30000, value = 10000, step = 1000, pre = "$", sep = ","),
                   sliderInput("rent_annual_incre", "Rent Increase (Monthly)  (%)", min = 0, max = 15, value= 0, step=0.1),
                   sliderInput("target_house_price", "Buy house price", min = 3000000, max = 15000000, value = 8000000, step = 500000, pre = "$", sep = ","),
                   sliderInput("left_house_price", "Remaining house payment", min = 3000000, max = 15000000, value = 8000000, step = 500000, pre = "$", sep = ","),
                   sliderInput("handover_year", "Handover year", min = 0, max = 10, value = 3, step = 1),
                   sliderInput("loan_int_rate", "Loan interest (%)", min = 0, max = 16, value = 1.8, step=0.1),
                   actionButton("Sim_butt", "Start simulation", icon("random"))
                   #sliderInput(),
                   #sliderInput(),
                   #sliderInput(),
                   #sliderInput(),
                   )
                   
          )
)

server <- function(input, output) {
    
  
}

shinyApp(ui = ui, server=server)