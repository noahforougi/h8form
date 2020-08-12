# Load packages and read in data ---------------------------------------------------------------------------
library(shiny)
library(plotly)
library(here)

h8form_c <- read_csv(file = here("proc", "h8form_c.csv"))

# UI -------------------------------------------------------------------------------------------------------

ui <- dashboardPage(
    
    dashboardHeader(title = "H.8. Form Data"),
    
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "introduction", icon = icon("chart-line")),
            menuItem("Trends", tabName = "trends", icon = icon("chart-line")),
            menuItem("Statistics", tabName = "statistics", icon = icon("ruler-horizontal"))
        )
    ),
    
    
    dashboardBody(
        tabItems(
            # Introduction to Dashboard Tab
            tabItem(
                tabName = "introduction",
                
                h2("Introduction to the H.8. Form Dashboard"),
                p("Every week, the Federal Reserve releases H.8. Form data, generally at 4:15 pm EST on Friday. This data consists
              of aggregated balance sheet data across 5 different types of banks - domestically chartered commercial banks; 
              large domestically chartered commercial banks; small domestically chartered commercial banks; and foreign-related 
              institutions in the United States. In this dashboard, you can visualize and measure differences across all 
              statistics in this form for each of these types of statistics."),
                
                
                fluidPage(
                    box(
                        title = "Visualize Current H.8. Form Data",
                        selectInput('type','Choose Asset or Liability Type', levels(as.factor(h8form_c$`Type`))), 
                        background = "light-blue", 
                        width = 12
                        
                    )),
                
                fluidPage(
                    box(
                        title = "Historical Series of Selected Variable", width = 12, status = "primary",
                        color = "aqua", 
                        plotlyOutput("plot1")
                    ))
                
                    
            ),
            
            # Trends/Visualizations Tab
            tabItem(
                tabName = "trends",
                h2("Trends and Projections"),
                p("In this part of the dashboard, you will be able to use long term historical data to decompose historical data, as well as make some forecasts."),
                p("Decomposition: The Hodrick–Prescott filter (also known as Hodrick–Prescott decomposition) is a mathematical tool used in macroeconomics, 
                  especially in real business cycle theory, to remove the cyclical component of a time series from raw data. It is used to obtain a smoothed-curve 
                  representation of a time series, one that is more sensitive to long-term than to short-term fluctuations."),
                p("Forecasts"),
                
                
                fluidPage(
                    box(
                        title = "Decomposition",
                        background = "light-blue", 
                        width = 12,
                        status = "primary"
                        
                        
                    )),
                
                fluidPage(
                    box(
                        title = "Forecast", 
                        background = "aqua",
                        width = 12, 
                        status = "primary"
                        )
                    )
            ),
            
            # Final tab content
            tabItem(
                tabName = "statistics",
                h2("Statistics tab content"),
                p("In this part of the dashboard, there will be the opportunity to run some basic statistical tests to understand
                      differences in these balance sheet data.")
            ))))

server <- function(input, output) {
    
    variable_choice <- reactive(input$type)
    
    output$plot1 <- renderPlotly(
        ggplotly(ggplot(data = h8form_c %>% 
                   filter(Type == variable_choice()), 
               aes(x = date, y = value, color = `Bank Type`)) + 
            geom_line()  + 
            xlab("Date") + 
            ylab("Dollars (Billions)") + 
            theme_minimal() 
                   ))
       

   
}

shinyApp(ui, server)