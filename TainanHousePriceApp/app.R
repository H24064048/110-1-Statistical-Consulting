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
    tabPanel("預售屋",
        sidebarLayout(
            sidebarPanel(
                #titlePanel("預售屋現況"),
                
                fluidRow(
                    column(4,
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
                    column(4, offset = 2,
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
                DT::dataTableOutput("mytable"),
                width = 8
            )
        ),
    ),
    
    # Tab 2
    tabPanel("租屋",
        sidebarLayout(
            sidebarPanel(
                sliderInput("binn", "Number of bins:", min = 1, max = 50, value = 30),
                fluidRow(
                    column(4,
                        checkboxGroupInput(inputId = "RegionFinder2",
                                            label = "行政區",
                                            # choices = c("新營區","鹽水區","白河區","柳營區","後壁區","東山區","麻豆區","下營區","六甲區","官田區","大內區","佳里區","學甲區","西港區","七股區","將軍區","北門區","新化區","新市區",
                                            #             "善化區","安定山上區","玉井區","楠西區","南化區","左鎮區","仁德區","歸仁區","關廟區","龍崎區","永康區","東區區","南區區","中西區","北區","安南區","安平區"),
                                            choices = c("東區", "北區", "南區", "中西區", "安平區" , "安南區", "新市區", "仁德區", "永康區", "善化區", "安定區"),
                                            selected = c("安平區", "新市區", "東區", "西區", "中西區", "南區", "安南區"))
                    ),
                    column(4,
                        selectInput(inputId =  "RentBedRoom",
                                            label = "最少房數",
                                            choices = c('1'=1,'2'=2,'3'=3, '4'=4, '5'=5)),
                        selectInput(inputId =  "RentLiveRoom",
                                    label = "最少廳數",
                                    choices = c('1'=1,'2'=2,'3'=3)),
                        selectInput(inputId =  "RentBathRoom",
                                    label = "最少衛數",
                                    choices = c('1'=1,'2'=2,'3'=3))
                    )
                )
            ),
            mainPanel(
                #plotOutput("Rent_density", height = 680)
                wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 720px;",
                            plotOutput("Rent_density", height = 680))
            )
        )
    ),
    
    # Tab 3
    tabPanel("關於")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        hist(new_house %>%
                 filter(主要建材 %in% input$'BuildingMaterial') %>%
                 filter(鄉鎮市區 %in% input$'RegionFinder') %>%
                 filter((交易年月日 > (input$'transactionyear'[1] *10000)) & (交易年月日 < (((input$'transactionyear'[2]) + 1)*10000))) %>%
                 select(總價元)/1000000,
            col = 'darkgray', border = 'white', xlab='總價(百萬元)')
    })
    

    output$mytable = DT::renderDT({ DT::datatable(
        new_house[,c("鄉鎮市區", "交易年月日", "總價元", "建物移轉總面積平方公尺", "建物型態", "建物現況格局.房", "建物現況格局.廳", "建物現況格局.衛", "主要建材")] %>%
            filter(主要建材 %in% input$'BuildingMaterial') %>%
            filter(鄉鎮市區 %in% input$'RegionFinder') %>%
            filter((交易年月日 > (input$'transactionyear'[1] *10000)) & (交易年月日 < (((input$'transactionyear'[2]) + 1)*10000))),
        colnames = c("鄉鎮市區", "交易年月日", "總價元", "平方公尺", "建物型態", "房", "廳", "衛", "主要建材"), 
        escape = FALSE
    )})
    
    output$Rent_density <- renderPlot({ 
        rentdist_fig(rent_house2 %>%
                        filter(鄉鎮市區 %in% input$'RegionFinder2') %>%
                        filter(建物現況格局.房 >= input$'RentBedRoom') %>%
                        filter(建物現況格局.廳 >= input$'RentLiveRoom') %>%
                        filter(建物現況格局.衛 >= input$'RentBathRoom'))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
