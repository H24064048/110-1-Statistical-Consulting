library(shiny)
library(DT)
library(dplyr)
library(shinythemes)

library(ggplot2)
library(ggridges)
library(scales)

source("source_plot.R", encoding="utf-8")

#setwd(choose.dir())
new_house <- read.csv('A_Tainan.csv')
rent_house <- read.csv('C_Tainan.csv')
rent_house2 <- rent_house %>% filter(總額元 <= 50000)
# Define UI for application that draws a histogram
ui <- navbarPage(
    
    # Application title
    title="內政部實價登陸資料應用",
    theme = shinytheme("sandstone"),
    fluid = TRUE,
    
    # Sidebar with a slider input for number of bins 
    tabPanel("資料視察",
             sidebarLayout(
                 sidebarPanel(
                     titlePanel("Title Panel here is 中文"),
                     
                     sliderInput("bins",
                                 "Number of bins:",
                                 min = 1,
                                 max = 50,
                                 value = 30),
                     
                     fluidRow(column(3,
                                     # Select which Gender(s) to plot
                                     checkboxGroupInput(inputId = "GenderFinder",
                                                        label = "Select Gender(s):",
                                                        choices = c("Male" = "M", "Female" = "F"),
                                                        selected = "M"),
                                     # Select which Division(s) to plot
                                     checkboxGroupInput(inputId = "DivisionFinder",
                                                        label = "Select Division(s):",
                                                        choices = c("DI", "DII", "DIII"),
                                                        selected = "DI")
                     ),
                     column(6, offset = 2,
                            # Select which Region(s) to plot
                            checkboxGroupInput(inputId = "RegionFinder",
                                               label = "行政區",
                                               choices = c("安平  區" = "安平區", "東區" = "東區", "西區" = "MidWest", "中西區", "West", "南區" = "南區", "安南區", "永康區", "新市區"),
                                               selected = "NewEngland")
                     )),
                     sliderInput("transactiondate", 
                                 "交易日期",
                                 min = 1050101, 
                                 max = 1081231, 
                                 value = c(1060101, 1061231))
                 ),
                 
                 mainPanel(
                     # Show a plot of the generated distribution
                     plotOutput("distPlot"),
                     # Main Table
                     DT::dataTableOutput("mytable")
                 )
             ),
    ),
    tabPanel("個人化推薦",
             fluidRow(
                 wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 720px;",
                           plotOutput("Rent_density", height = 680))
             ) 
    ),
    tabPanel("關於")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$mytable = DT::renderDataTable({
        new_house[,c("鄉鎮市區", "交易年月日", "總價元", "建物型態", "建物現況格局.廳", "建物現況格局.衛", "建物現況格局.隔間")] %>%
            filter(鄉鎮市區 == input$'RegionFinder')%>%
            filter(交易年月日 > input$'transactiondate'[1] & 交易年月日 < input$'transactiondate'[2])
    })
    
    output$Rent_density <- renderPlot({ 
        rentdist_fig(rent_house2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
