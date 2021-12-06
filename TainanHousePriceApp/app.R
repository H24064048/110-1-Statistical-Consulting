#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins 
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
                                      label = "區域",
                                      choices = c("安平區" = "NewEngland", "東區" = "MidAtlantic", "西區" = "MidWest", "安南區", "West", "南區" = "SouthWest", "沙崙區", "Alaska", "Hawaii"),
                                      selected = "NewEngland")
            ))
            
        ),


        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           # Main Table
           DT::dataTableOutput("mytable")
        )
    )
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
        faithful
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
