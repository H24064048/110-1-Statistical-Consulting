library(shiny)
library(DT)
library(dplyr)
library(shinythemes)

library(ggplot2)
library(ggridges)
library(scales)

library(patchwork)
library(reshape2)


source("source_plot.R", encoding="utf-8")
source("source_finance.R")

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
                    column(3,
                        checkboxGroupInput(inputId = "RegionFinder",
                                            label = "行政區",
                                            choices = c("安平區", "新市區", "東區", "西區", "中西區", "南區", "安南區", "永康區"),
                                            selected = "安平區") 
                    ),
                    column(4, #offset = 1,
                        # checkboxGroupInput(inputId = "GenderFinder",
                        #                     label = "Select Gender(s):",
                        #                     choices = c("Male" = "M", "Female" = "F"),
                        #                     selected = "M"),
                        checkboxGroupInput(inputId = "BuildingMaterial",
                                            label = "主要建材:",
                                            choices = c("鋼骨混凝土造", "鋼骨鋼筋混凝土造", "鋼筋混凝土造", "鋼筋混凝土構造"),
                                            selected = c("鋼骨混凝土造", "鋼骨鋼筋混凝土造", "鋼筋混凝土造", "鋼筋混凝土構造"))
                    ),
                    column(3,
                        selectInput(inputId =  "BuyBedRoom",
                                    label = "最少房數",
                                    choices = c('1'=1,'2'=2,'3'=3, '4'=4, '5'=5)),
                        selectInput(inputId =  "BuyLiveRoom",
                                    label = "最少廳數",
                                    choices = c('1'=1,'2'=2,'3'=3)),
                        selectInput(inputId =  "BuyBathRoom",
                                    label = "最少衛數",
                                    choices = c('1'=1,'2'=2,'3'=3))
                    )
                ),
                sliderInput("transactionyear", "交易年分", min = 103, max = 110, value = c(106, 110)),
                sliderInput("buyhousesize", "移轉總面積(坪)", min = 10, max = 100, value = c(20, 30), step=5)
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
                # sliderInput("binn", "Number of bins:", min = 1, max = 50, value = 30),
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
                    ),
                    column(4,
                           selectInput(inputId =  "Organization",
                                       label = "管理組織",
                                       multiple = TRUE,
                                       choices = c("有","無"),
                                       selected = c("有","無")),
                           selectInput(inputId =  "HouseApplication",
                                       label = "附傢俱",
                                       multiple = TRUE,
                                       choices = c("有","無"),
                                       selected = c("有","無")),
                           selectInput(inputId =  "BuildingType",
                                       label = "建物型態",
                                       multiple = TRUE,
                                       choices = c('公寓'='公寓(5樓含以下無電梯)', '住宅大樓'= '住宅大樓(11層含以上有電梯)', '套房'='套房(1房1廳1衛)', '華廈'='華廈(10層含以下有電梯)'),
                                       selected = c('公寓(5樓含以下無電梯)','住宅大樓(11層含以上有電梯)','華廈(10層含以下有電梯)')
                                       )
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
    tabPanel("個人財務",
        fluidPage(
            fluidRow(
                column(2, tags$h3("Financial Status")),
                column(8, tags$h3("Housing Plan Simulation")),
                column(2, tags$h3("Buy or Rent"))
            ),
            fluidRow(
                column(2,
                    sliderInput("n_obs", "Years to come (To clear debt)", min = 0, max = 40, value = 20),
                    sliderInput("start_capital", "Initial capital", min = 400000, max = 5000000, value = 1000000, step = 100000, pre = "$", sep = ","),
                    sliderInput("annual_cap_return", "Median investment return (in %)", min = 0.0, max = 30.0, value = 5.0, step = 0.5),
                    sliderInput("annual_cap_dev", "Investment fluctuation rate(in %)", min = 0, max = 30.0, value = 3, step = 1),
                    sliderInput("annual_netincome", "Annual net income", min = 300000, max = 2000000, value = 500000, step = 50000, pre = "$", sep = ","),
                    sliderInput("annual_income_growth", "Annual Income growth (in %)", min = 0, max = 20, value = 1, step = 0.5),
                    sliderInput("monthly_expense", "Monthly expenditure", min = 1000, max = 100000, value = 10000, step = 1000, pre = "$", sep = ","),
                    sliderInput("expense_growth", "Expenditure growth (in %)", min = 0, max = 20, value = 1, step = 0.5)
                ),
                column(8,
                    plotOutput("FinSimPlot", height = "600px")),
                column(2,
                    sliderInput("annual_inflation", "Annual inflation (in %)", min = 0, max = 10, value = 2.5, step = 0.1),
                    sliderInput("monthly_rent", "Rent (Monthly)", min = 4000, max = 30000, value = 10000, step = 1000, pre = "$", sep = ","),
                    sliderInput("rent_annual_incre", "Rent Increase (Monthly)  (%)", min = 0, max = 15, value= 0, step=0.1),
                    sliderInput("target_house_price", "Buy house price", min = 3000000, max = 15000000, value = 8000000, step = 500000, pre = "$", sep = ","),
                    sliderInput("left_house_price", "Remaining house payment (Loan)", min = 3000000, max = 15000000, value = 8000000, step = 500000, pre = "$", sep = ","),
                    sliderInput("handover_year", "Handover year", min = 1, max = 10, value = 3, step = 1),
                    sliderInput("loan_int_rate", "Loan interest (%)", min = 0, max = 16, value = 1.8, step=0.1)#,
                    #actionButton("Sim_butt", "Start simulation", icon("random"))
                )
            )
        )                 
    ),
    
    # Tab 4
    tabPanel("關於")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Tab 1
    output$distPlot <- renderPlot({
        ggplot(data=new_house %>%
                   filter(主要建材 %in% input$'BuildingMaterial') %>%
                   filter(鄉鎮市區 %in% input$'RegionFinder') %>%
                   filter(建物現況格局.房 >= input$'BuyBedRoom') %>%
                   filter(建物現況格局.廳 >= input$'BuyLiveRoom') %>%
                   filter(建物現況格局.衛 >= input$'BuyBathRoom') %>%
                   filter((建物移轉總面積平方公尺 > (input$'buyhousesize'[1] *3.305785)) & (建物移轉總面積平方公尺 < ((input$'buyhousesize'[2])*3.305785))) %>%
                   filter((交易年月日 > (input$'transactionyear'[1] *10000)) & (交易年月日 < (((input$'transactionyear'[2]) + 1)*10000))),
                aes(x = 總價元/1000000)) + 
            geom_histogram(bins = 30) +
            labs(x = '總價(百萬元)',
                 y = '成交數',
                 title = '房價分布圖')
    })
    

    output$mytable = DT::renderDT({ DT::datatable(
        new_house[,c("鄉鎮市區", "交易年月日", "總價元", "建物移轉總面積平方公尺", "建物型態", "建物現況格局.房", "建物現況格局.廳", "建物現況格局.衛", "主要建材")] %>%
            filter(主要建材 %in% input$'BuildingMaterial') %>%
            filter(鄉鎮市區 %in% input$'RegionFinder') %>%
            filter(建物現況格局.房 >= input$'BuyBedRoom') %>%
            filter(建物現況格局.廳 >= input$'BuyLiveRoom') %>%
            filter(建物現況格局.衛 >= input$'BuyBathRoom') %>%
            filter((建物移轉總面積平方公尺 > (input$'buyhousesize'[1] *3.305785)) & (建物移轉總面積平方公尺 < ((input$'buyhousesize'[2])*3.305785))) %>%
            filter((交易年月日 > (input$'transactionyear'[1] *10000)) & (交易年月日 < (((input$'transactionyear'[2]) + 1)*10000))),
        colnames = c("鄉鎮市區", "交易年月日", "總價元", "平方公尺", "建物型態", "房", "廳", "衛", "主要建材"), 
        escape = FALSE
    )})
    
    # Tab 2
    output$Rent_density <- renderPlot({ 
        rentdist_fig(rent_house %>%
                        filter(鄉鎮市區 %in% input$'RegionFinder2') %>%
                        filter(建物現況格局.房 >= input$'RentBedRoom') %>%
                        filter(建物現況格局.廳 >= input$'RentLiveRoom') %>%
                        filter(建物現況格局.衛 >= input$'RentBathRoom') %>%
                        filter(有無管理組織 %in% input$'Organization') %>%
                        filter(有無附傢俱 %in% input$'HouseApplication') %>%
                        filter(建物型態 %in% input$'BuildingType'))
                        
    })
    
    # Tab 3
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
        
        p1 <- ggplot(data= cap_buy, aes(x=ind, y=Capital_Buy/1000000, group=L1, color=L1))+
            scale_color_gradient2(midpoint=median(1:n_sim),low="steelblue4", high="firebrick4", mid='bisque3', space ="Lab", guide = "none")+
            geom_line()+
            labs(x= 'Year',y= 'Capital (Millions)',title= 'Accumulated assets (Buying)')+
            theme(plot.title = element_text(hjust = 0.5, color = "navyblue", size = 15, face = "bold"))
        
        p2 <- ggplot(data= cap_rent, aes(x=ind, y=Capital_Rent/1000000, group=L1, color=L1))+
            scale_color_gradient2(midpoint=median(1:n_sim),low="steelblue4", high="firebrick4", mid='bisque3', space ="Lab", guide = "none")+
            geom_line()+
            labs(x= 'Year',y= 'Capital (Millions)',title= 'Accumulated assets (Renting)')+
            theme(plot.title = element_text(hjust = 0.5, color = "navyblue", size = 15, face = "bold"))
        
        p3 <- ggplot(data= subset(cap_rent, ind==(input$n_obs)), aes(x=Capital_Rent/1000000))+
            geom_density()+
            labs(x = 'Final Capital (M)',
                 y = 'Density',
                 title = 'Final Asset (Renting)')+
            theme(plot.title = element_text(hjust = 0.5, color = "navyblue", size = 15, face = "bold"))
        
        p4 <- ggplot(data= subset(cap_buy, ind==(input$n_obs)), aes(x=Capital_Buy/1000000))+
            geom_density()+
            labs(x = 'Final Capital (M)',
                 y = 'Density',
                 title = 'Final Asset (Buying)')+
            theme(plot.title = element_text(hjust = 0.5, color = "navyblue", size = 15, face = "bold"))
        
        p5 <- ggplot(data= cap_buy %>%
                         group_by(ind) %>% 
                         summarise(mean(Capital_Buy >0))
                     ,aes(x=ind, y=`mean(Capital_Buy > 0)`))+
            geom_line()+
            scale_y_continuous(limits = c(0, 1))+
            labs(x = 'Year',
                 y = 'Percentage',
                 title = 'Chance of not Broke')+
            theme(plot.title = element_text(hjust = 0.5, color = "navyblue", size = 15, face = "bold"))

        p1 / p2 / (p3+p4+p5)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
