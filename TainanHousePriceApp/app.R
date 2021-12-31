library(shiny)
library(DT)
library(dplyr)
library(shinythemes)

library(ggplot2)
library(ggridges)
library(scales)

source("source_plot.R", encoding="utf-8")

#setwd(choose.dir())
new_house <- read.csv('B2016Q1_2021Q3.csv')
rent_house <- read.csv('C2016Q1_2021Q3.csv')
rent_house2 <- rent_house %>% filter(總額元 <= 50000)

ui <- navbarPage(
    
    # Application title
    title="內政部實價登陸資料應用",
    theme = shinytheme("sandstone"),
    fluid = TRUE,
    
    # Tab 1 
    tabPanel("資料視察",
        sidebarLayout(
            sidebarPanel(
                titlePanel("Title Panel here is 中文"),
                sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
                fluidRow(
                    column(3,
                        # Select which Gender(s) to plot
                        checkboxGroupInput(inputId = "GenderFinder",
                                            label = "Select Gender(s):",
                                            choices = c("Male" = "M", "Female" = "F"),
                                            selected = "M"),
                        # Select which Division(s) to plot
                        checkboxGroupInput(inputId = "BuildingMaterial",
                                            label = "主要建材:",
                                            choices = c("鋼骨混凝土造", "鋼骨鋼筋混凝土造", "鋼筋混凝土造", "鋼筋混凝土構造"),
                                            selected = c("鋼骨混凝土造", "鋼骨鋼筋混凝土造", "鋼筋混凝土造", "鋼筋混凝土構造"))
                    ),
                    column(6, offset = 2,
                        # Select which Region(s) to plot
                        checkboxGroupInput(inputId = "RegionFinder",
                                            label = "行政區",
                                            choices = c("安平區", "新市區", "東區", "西區", "中西區", "南區", "安南區", "永康區"),
                                            selected = "安平區")
                    )
                ),
                sliderInput("transactionyear", "交易年分", min = 103, max = 110, value = c(106, 110))
            ),
                 
            mainPanel(
                plotOutput("distPlot"),
                # Main Table
                DT::dataTableOutput("mytable")
            )
        ),
    ),
    
    # Tab 2
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
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$mytable = DT::renderDT({ DT::datatable(
        new_house[,c("鄉鎮市區", "交易年月日", "總價元", "建物型態", "建物現況格局.房", "建物現況格局.廳", "建物現況格局.衛", "建物型態", "主要建材")] %>%
            filter(主要建材 %in% input$'BuildingMaterial') %>%
            filter(鄉鎮市區 %in% input$'RegionFinder') %>%
            filter((交易年月日 > (input$'transactionyear'[1] *10000)) & (交易年月日 < (((input$'transactionyear'[2]) + 1)*10000))),
        colnames = c("鄉鎮市區", "交易年月日", "總價元", "建物型態", "房", "廳", "衛", "建物型態", "主要建材"), 
        escape = FALSE
    )})
    
    output$Rent_density <- renderPlot({ 
        rentdist_fig(rent_house2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
