library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
sales <- readr::read_csv("data/sales_data_sample.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Sales Dashboard"),
    sidebarLayout(
        sidebarPanel(
            tags$p(HTML("To view Total Sales by Territory, select territory and minimum total sales<br><br>")),
    selectInput("territory", "1. Select Territory", choices = unique(sales$TERRITORY),selected = "EMEA"),
    sliderInput(
        inputId = "TotalSales",
        label = "2. Select Minimum Total Sales" ,
        min=50000,max=200000,value=50000,step=10000)
    ),
    mainPanel(
        tags$p(HTML("<b>Data Table</b>")),
    DT::dataTableOutput("selected"),
    tags$p(HTML("<br><b>Plot<b><br>")),
    plotOutput(outputId = "plot")
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    selected <- reactive({
        if (input$territory == "NA") {
            #subset(sales, is.na(TERRITORY))
            sales %>%
                mutate(YEAR_ID=as.integer(YEAR_ID)) %>%
                select(TERRITORY,PRODUCTLINE,YEAR_ID,SALES) %>%
                filter(is.na(TERRITORY)) %>%
                group_by(TERRITORY,PRODUCTLINE,YEAR_ID) %>%
                    summarise(TOTAL_SALES=sum(SALES)) %>%
                    filter(TOTAL_SALES > input$TotalSales)
                
        } else {
            #subset(sales, TERRITORY == input$territory)
            sales %>%
                mutate(YEAR_ID=as.integer(YEAR_ID)) %>%
                select(TERRITORY,PRODUCTLINE,YEAR_ID,SALES) %>%
            filter(TERRITORY==input$territory) %>%
                group_by(TERRITORY,PRODUCTLINE,YEAR_ID) %>%
                summarise(TOTAL_SALES=sum(SALES)) %>%
                filter(TOTAL_SALES > input$TotalSales)
        }
    })
    output$selected <- DT::renderDataTable(head(selected(), 100))
    
    output$plot<- renderPlot({
    ggplot(data=selected(),
                     aes(x=YEAR_ID, y=TOTAL_SALES,fill=PRODUCTLINE)) +
        geom_bar(stat="identity") +
            labs(x="Year",y="Total Sales")
            
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
