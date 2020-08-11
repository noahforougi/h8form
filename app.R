library(shiny)
library(plotly)
library(ggthemes)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("H.8 Form Data"),

    sidebarPanel(
        selectInput('type','Asset or Liability Type', levels(as.factor(dat$`Type`))),
        selected = list(levels(as.factor(dat$`Type`)))[5]),
    
    mainPanel(
        plotlyOutput('plot')
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    y <- reactive({input$type})
    
    output$plot <- renderPlotly(
        plot1 <- ggplotly(
            dat %>% 
                filter(Type == paste(y())) %>%
                ggplot(aes(x = date, y = value, color = `Bank Type`)) +
                geom_line()  + 
                xlab("Date") + 
                ylab("Dollar Value") + 
                theme_minimal()
                )  
        )
}

# Run the application 
shinyApp(ui = ui, server = server)
