# Load packages and read in data ---------------------------------------------------------------------------
library(shiny)
library(plotly)
library(here)
library(mFilter)
library(lubridate)
library(forecast)
library(tidyverse)
library(data.table)
library(shinydashboard)

h8form_c <- read_csv(file = here("proc", "h8form_c.csv"))
h8form_h <- read_csv(file = here("proc", "h8form_h.csv"))

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
                h2("Time Series Analysis"),
                p("In this part of the dashboard, you will be able to use long term historical data to decompose historical data, as well as make some forecasts."),
                h4("What is a Hodrick Prescott Filter?"),
                p("Decomposition: The Hodrick–Prescott filter (also known as Hodrick–Prescott decomposition) is a mathematical tool used in macroeconomics, 
                  especially in real business cycle theory, to remove the cyclical component of a time series from raw data. It is used to obtain a smoothed-curve 
                  representation of a time series, one that is more sensitive to long-term than to short-term fluctuations."),
                
                p("More mathematical explanation will be inserted here."),

                
                fluidPage(
                    box(
                        title = "Hodrick Prescott Filtered Time Series Decomposition", width = 12, status = "primary",
                        color = "aqua", 
                        selectInput('bank_choice',
                                    'Choose type of commercial bank', 
                                    levels(as.factor(h8form_h$id)), 
                                    selected = "all commercial banks"),
                        selectInput('asset_liability_choice',
                                    'Choose Asset or Liability Type',
                                    levels(as.factor(h8form_h$variable)),
                                    selected = "Bank credit"),
                        selectInput('sa_nsa_choice',
                                    'Choose Seasonally Adjusted or Non Seasonally Adjusted Values', 
                                    levels(as.factor(h8form_h$adjusted)), 
                                    selected = "not seasonally adjusted"),
                        selectInput('growth_choice',
                                    'Choose Level or Annula Growth Rate', 
                                    levels(as.factor(h8form_h$growth)), 
                                    selected = "level"),
                        sliderInput("param_choice", 
                                    "Frequency Paramter", 
                                    min = 0, max = 1000, 
                                    value = 144),
                        plotlyOutput("plot2")
        

                    )
                    ),
                
                h4("What is Forecasting?"),
                p("Making predictions about the future is called extrapolation in the classical statistical handling of time series data. More modern fields focus on the topic and refer to it as time series forecasting. Forecasting involves taking models fit on historical data and using them to predict future observations."),
                
                p("More mathematical explanation will be inserted here."),
                fluidPage(
                    box(
                        title = "Forecast", 
                        width = 12, 
                        status = "primary"
                        )
                    )
            ),
            
            
            # Final Tab Content
            tabItem(
                tabName = "statistics",
                h2("Statistical Tests"),
                p("In this part of the dashboard, there will be the opportunity to run some basic statistical tests, like a difference in means test, to understand
                      difference in these balance sheet data between banks.")
                ))))


server <- function(input, output) {
    
    # Introduction tab plots
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
    
    
    
    # Trends tab plots
    output$plot2 <- renderPlotly({
      dat <- h8form_h %>%
        filter(id == input$bank_choice &
                 variable == input$asset_liability_choice & 
                 adjusted == input$sa_nsa_choice &
                 growth == input$growth_choice) %>%
        mutate(date = myd(paste(date, "01"))) 
      filtered <- dat %>%
        select(value) %>%
        ts(., frequency = 12) %>% 
        hpfilter(., freq = input$param_choice)
      
      ready <- data.frame(
        dat, 
        trend = as.vector(filtered$trend), 
        cycle = as.vector(filtered$cycle)
      )
      
      
      ggplotly(ready %>%
                 ggplot(., aes(x = date)) +
                 geom_line(aes(y = value, color = "red")) + 
                 geom_line(aes(y = trend, color = "blue")) + 
                 ylab("Dollar Value (Billions)") + 
                 xlab("Date") +
                 ggtitle(paste("Hodrick Prescott Filtered Decomposition of", input$asset_liability_choice), 
                         subtitle = paste("Lambda Choice:" ,input$param_choice))
      )
      }
      )
    }

shinyApp(ui, server)


