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
                h2("Trends and Projections"),
                p("In this part of the dashboard, you will be able to use long term historical data to decompose historical data, as well as make some forecasts."),
                p("Decomposition: The Hodrick–Prescott filter (also known as Hodrick–Prescott decomposition) is a mathematical tool used in macroeconomics, 
                  especially in real business cycle theory, to remove the cyclical component of a time series from raw data. It is used to obtain a smoothed-curve 
                  representation of a time series, one that is more sensitive to long-term than to short-term fluctuations."),
                p("Forecasts"),
                
                
                fluidPage(
                    box(
                        title = "Hodtrick Prescott Filtered Time Series", width = 12, status = "primary",
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
                        dateInput(inputId = "date", 
                                  label = "Select start date", 
                                  value = ymd(20000101), 
                                  min = ymd(19800101),
                                  max = ymd(20190101)
                                  ),
                        plotlyOutput("plot2")
        

                    )
                    ),
                
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
    bank_choice <- reactive(input$bank_choice)
    asset_liability_choice <- reactive(input$asset_liability_choice)
    sa_nsa_choice <- reactive(input$sa_nsa_choice)
    growth_choice <- reactive(input$growth_choice)
    param_choice <- reactive(input$param_choice)
    date <- reactive(input$date)
    
    dat <-  reactive({
        h8form_h %>%
            filter(id == input$bank_choice &
                   variable == asset_liability_choice() & 
                   adjusted == sa_nsa_choice() &
                   growth == growth_choice()) %>%
        mutate(date = myd(paste(date, "01")))
    })
    
    filtered <- reactive({
        dat() %>%
            select(value) %>%
            ts(., frequency = 12) %>% 
            hpfilter(., freq =input$param_choice)[c("trend", "cycle")]
    })
    
    
    ready <- reactive({
        tibble(
        date_observation = dat$date,
        variable = dat$variable, 
        id = dat$id, 
        adjusted = dat$adjusted, 
        growth = dat$growth,
        value = dat$value,
        trend = filtered$trend, 
        cycle = filtered$cycle
        )
    })
    


    output$plot2 <- renderPlot(
        

        ggplot(
           data = ready(),
           aes(x = date_observation)) + 
                geom_line(aes(y = value123, color = "red")) + 
                geom_line(aes(y = trend123, color = "blue")))
    
      }

shinyApp(ui, server)


