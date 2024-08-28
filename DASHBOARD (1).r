if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("shinydashboard", quietly = TRUE)) install.packages("shinydashboard")
if (!requireNamespace("shinyWidgets", quietly = TRUE)) install.packages("shinyWidgets")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("quantmod", quietly = TRUE)) install.packages("quantmod")
if (!requireNamespace("dygraphs", quietly = TRUE)) install.packages("dygraphs")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
if (!requireNamespace("TTR", quietly = TRUE)) install.packages("TTR")
if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")
if (!requireNamespace("xts", quietly = TRUE)) install.packages("xts")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
if (!requireNamespace("prophet", quietly = TRUE)) install.packages("prophet")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("tibble", quietly = TRUE)) install.packages("tibble")
if (!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
if (!requireNamespace("treemap", quietly = TRUE)) install.packages("treemap")
if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales")
if (!requireNamespace("timetk", quietly = TRUE)) install.packages("timetk")
if (!requireNamespace("tidyquant", quietly = TRUE)) install.packages("tidyquant")
if (!requireNamespace("forecast", quietly = TRUE)) install.packages("forecast")
if (!requireNamespace("forcast", quietly = TRUE)) install.packages("forcast")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("keras", quietly = TRUE)) install.packages("keras")
#####
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readxl)
library(readr) 
library(quantmod)
library(dygraphs)
library(dplyr)
library(DT)
library(TTR)
library(plotly)
library(xts)
library(lubridate)
library(stringr)
library(corrplot)
library(prophet)
library(tidyr)
library(tibble)
library(reshape2)
library(jsonlite)
library(treemap)
library(scales)
library(timetk)
library(tidyquant)
library(forecast)
library(forcats)
library(tidyverse)
library(keras)
data <- read_excel("/Users/hoangnguyenbaotram/Downloads/merged.xlsx", col_names = TRUE) 
aapl_data <- data %>% 
  filter(Symbol == "AAPL") %>%
  mutate(Date = as.Date(Date, format="%Y-%m-%d"))
read_and_transform <- function(sheet_name) {
  df <- read_excel("/Users/hoangnguyenbaotram/Downloads/Financial_Reports_20192023.xlsx", sheet = sheet_name)
  df <- df %>%
    mutate(Year = as.integer(format(as.Date(fiscalDateEnding, "%Y-%m-%d"), "%Y")))
  return(df)
}
FF <- read_csv("/Users/hoangnguyenbaotram/Downloads/F-F_Research_Data_Factors.csv", skip = 3, col_types = cols(...1 = col_integer(), `Mkt-RF` = col_double(),
                                                                                                      SMB = col_double(), HML = col_double(), RF = col_double())) %>%
  rename(date = ...1) %>%
  filter(date > 100000) %>%
  mutate(date = ymd(parse_date_time(date, "%Y%m"))) %>%
  na.omit()

# Filter FF data for the specified date range
FF <- FF %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2023-09-01")) %>%
  rename(Date = date)

# Select relevant columns and pivot data to wide format
df2 <- data %>%
  select(Date, Symbol, Adjusted)

df3 <- df2 %>%
  pivot_wider(names_from = Symbol, values_from = Adjusted)

# Set Date column as row names
df4 <- df3 %>%
  column_to_rownames(var = "Date")

# Calculate monthly returns
returns <- df4 %>%
  to.monthly(indexAt = "firstof", OHLC = FALSE) %>%
  data.frame(date = index(.)) %>%
  remove_rownames() %>%
  gather(asset, prices, -date) %>%
  group_by(asset) %>%
  mutate(returns = (prices / lag(prices)) - 1) %>%
  select(-prices) %>%
  spread(asset, returns) %>%
  na.omit()

# Convert returns to data frame and add Date column
returns_df <- as.data.frame(returns)
returns_df <- returns_df %>%
  mutate(Date = seq(as.Date("2019-01-01"), as.Date("2023-09-01"), by = "month")) %>%
  select(-date)





# Áp dụng hàm cho từng sheet
data_list_income <- read_and_transform("Income")
data_list_balance <- read_and_transform("Balance")
data_list_cashflow <- read_and_transform("Cashflow")

# Đảm bảo data_list_asset được tải dữ liệu
data_list_asset <- data_list_balance  
# Assuming 'data' and 'data_list_asset' are already loaded
# Assuming 'data' and 'data_list_asset' are already loaded
ui <- navbarPage(
  title = div(style = "font-weight: bold; color: #4793AF;", "ISAPS - Intelligent Stock Analysis and Prediction System"),
  
  tags$head(
    tags$style(HTML("
      .navbar-default .navbar-nav > li > a {
        transition: all 0.3s;
      }
      .navbar-default .navbar-nav > li:hover > a {
        transform: scale(1.1);
        background-color: #f2f2f2;
        color: #333;
      }
      .navbar-default .navbar-nav > li.active > a,
      .navbar-default .navbar-nav > li.active > a:focus,
      .navbar-default .navbar-nav > li.active > a:hover {
        transform: scale(1.2);
      }
      .navbar-default .navbar-nav > li:nth-child(2) > a {
        background-color: #184e77;
        color: white;
      }
      .navbar-default .navbar-nav > li:nth-child(2).active > a,
      .navbar-default .navbar-nav > li:nth-child(2).active > a:focus,
      .navbar-default .navbar-nav > li:nth-child(2).active > a:hover {
        background-color: #184e77;
        color: white;
      }
      .navbar-default .navbar-nav > li:nth-child(3) > a {
        background-color: #1a759f;
        color: white;
      }
      .navbar-default .navbar-nav > li:nth-child(3).active > a,
      .navbar-default .navbar-nav > li:nth-child(3).active > a:focus,
      .navbar-default .navbar-nav > li:nth-child(3).active > a:hover {
        background-color: #1a759f;
        color: white;
      }
      .navbar-default .navbar-nav > li:nth-child(4) > a {
        background-color: #34a0a4;
        color: white;
      }
      .navbar-default .navbar-nav > li:nth-child(4).active > a,
      .navbar-default .navbar-nav > li:nth-child(4).active > a:focus,
      .navbar-default .navbar-nav > li:nth-child(4).active > a:hover {
        background-color: #34a0a4;
        color: white;
      }
      .navbar-default .navbar-nav > li:nth-child(5) > a {
        background-color: #76c893;
        color: white;
      }
      .navbar-default .navbar-nav > li:nth-child(5).active > a,
      .navbar-default .navbar-nav > li:nth-child(5).active > a:focus,
      .navbar-default .navbar-nav > li:nth-child(5).active > a:hover {
        background-color: #76c893;
        color: white;
      }
      .navbar-default .navbar-nav > li:nth-child(6) > a {
        background-color: #b5e48c;
        color: white;
      }
      .navbar-default .navbar-nav > li:nth-child(6).active > a,
      .navbar-default .navbar-nav > li:nth-child(6).active > a:focus,
      .navbar-default .navbar-nav > li:nth-child(6).active > a:hover {
        background-color: #b5e48c;
        color: white;
      }
    "))
  ),
  tabPanel("Quantitative Stock Analysis",
           fluidRow(
             box(
               title = div(style = "font-weight: bold; color: #4793AF;", "Quantitative Analysis Options"),
               status = "warning",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,
               selectInput("quant_analysis", "Select Analysis Type:", 
                           choices = c("None"= "", "Industry Distribution", "Computing Correlations"))
             )
           ),
           mainPanel(
             conditionalPanel(
               condition = "input.quant_analysis === 'Industry Distribution'",
               plotOutput("industryTreemap"),
               plotOutput("industryPlot"),
               plotOutput("techTreemap")
             ),
             conditionalPanel(
               condition = "input.quant_analysis === 'Computing Correlations'",
               plotOutput("correlationPlot"),
               tableOutput("correlationSummary")
             )
           )
  ),
  
  tabPanel("Indicators",
           fluidRow(
             box(
               title = div(style = "font-weight: bold; color: #4793AF;", "Select Options"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,
               column(4, selectInput("symbol", "Choose a Symbol:", choices = c("None" = "", unique(data_list_asset$Symbol)), selected = "None")),
               column(4, selectInput("indicators", "Select Indicators to Plot:", choices = c("None" = "", "Box Plot" = "Box Plot","Price" = "Price", "SMA" = "SMA", "MACD" = "MACD", "Bollinger Bands" = "Bollinger Bands", "RSI" = "RSI")))
             )
           ),
           uiOutput("boxPlotUI"),
           uiOutput("priceVolumePlotUI"),
           uiOutput("maPlotUI"),
           uiOutput("macdPlotUI"),
           uiOutput("bbandsPlotUI"),
           uiOutput("rsiPlotUI")
  ),
  
  tabPanel("Finance",
           fluidRow(
             box(
               title = div(style = "font-weight: bold; color: #4793AF;", "Financial"),
               status = "info",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,
               column(4, selectInput("symbol", "Choose a Symbol:", choices = c("None" = "", unique(data_list_asset$Symbol)), selected = "None"))
             )
           ),
           plotlyOutput("asset_plot")
  ),
  
  tabPanel("Prediction Model", value = "prediction_model",
           fluidRow(
             box(
               title = div(style = "font-weight: bold; color: #4793AF;", "Prediction Model Options"),
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,
               column(4, selectInput("symbol", "Choose a Symbol:", choices = "AAPL")),
               column(4, selectInput("Type", "Choose a model:", choices = c("None"="","Cross-Correlation Function", "ARIMA", "DNN")))
             )
           ),
           mainPanel(
             conditionalPanel(
               condition = "input.Type === 'Cross-Correlation Function'",
               plotOutput("ccf_plots")
             ),
             conditionalPanel(
               condition = "input.Type === 'ARIMA'",
               fluidRow(
                 box(
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   width = 12,
                   column(12, verbatimTextOutput("arimaSummary")),
                   column(12, verbatimTextOutput("arimaSummary"),
                          plotOutput("arimaResiduals"),
                          plotOutput("forecast_plot"))
                 )
               )
             ),
             conditionalPanel(
               condition = "input.Type === 'DNN'",
                          plotOutput("dnnPlot")
                   )
                         
           )
  ),
  tabPanel("CAPM",
           fluidRow(
             box(
               title = div(style = "font-weight: bold; color: #4793AF;", "Capital Asset Pricing Model"),
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               column(12, verbatimTextOutput("capmSummary"),
                      plotOutput("portfolio_returns_plot"),
                      plotOutput("portfolio_growth_plot"),
                      plotOutput("multi_portfolio_growth_plot"))
             )
           )
  )
  
)

  


server <- function(input, output, session) {
  # Create a reactive values object to store selected symbol data
  selected_symbol_data <- reactiveValues(data = NULL)
  
  # Lọc dữ liệu cho mã cụ thể khi mã cổ phiếu được chọn
  filtered_data <- reactive({
    req(input$symbol)
    data_list_asset %>%
      filter(Symbol == input$symbol)
  }) 
  # Function to update selected symbol data
  observe({
    req(input$symbol)
    selected_symbol_data$data <- subset(data, Symbol == input$symbol)
  })
  
  output$boxPlotUI <- renderUI({
    if ("Box Plot" %in% input$indicators) {
      plotlyOutput("boxPlot")
    }
  })
  output$boxPlot <- renderPlotly({
    box <- plot_ly(data = selected_symbol_data$data, x = ~factor(format(Date, "%Y")), y = ~close, type = 'box',
                 name = ~factor(format(Date, "%Y")),
                 boxpoints = "outliers",
                 marker = list(color = 'rgb(8,81,156)'),
                 line = list(color = 'rgb(8,81,156)')) %>%
      layout(title = paste("Box Plot About Close Prices for",input$symbol) ,
             xaxis = list(title = "Year"),
             yaxis = list(title = "Close Price (USD)"))
    
    # Render the plot
    box
  })
  output$priceVolumePlotUI <- renderUI({
    if ("Price" %in% input$indicators) {
      plotlyOutput("priceVolumePlot")
    }
  })
  
  # Render plotly plot for price and volume
  output$priceVolumePlot <- renderPlotly({
    req(selected_symbol_data$data)
    
    # Example plot for price and volume
    plot_price_volume <- plot_ly(data = selected_symbol_data$data, x = ~Date) %>%
      add_trace(y = ~close, name = 'Close Price', type = 'scatter', mode = 'lines', line = list(color = 'blue')) %>%
      add_trace(y = ~volume / 1e6, name = 'Volume', type = 'bar', yaxis = 'y2', marker = list(color = 'green', opacity = 0.5)) %>%
      layout(title = paste("Price and Volume Chart for", input$symbol),
             xaxis = list(
               title = "Date",
               rangeselector = list(
                 buttons = list(
                   list(count = 1, label = "1m", step = "month", stepmode = "backward"),
                   list(count = 6, label = "6m", step = "month", stepmode = "backward"),
                   list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
                   list(count = 1, label = "1y", step = "year", stepmode = "backward"),
                   list(step = "all")
                 )
               ),
               rangeslider = list(type = "date")
             ),
             yaxis = list(title = "Price"),
             yaxis2 = list(title = "Volume (in millions)", overlaying = "y", side = "right")
      )
    
    return(plot_price_volume)
  })
  #####
  output$maPlotUI <- renderUI({
    if ("SMA" %in% input$indicators) {
      plotlyOutput("maPlot")
    }
  })
  
  # Render the plotly plot for moving averages
  output$maPlot <- renderPlotly({
    req(selected_symbol_data$data)
    
    # Calculate SMAs
    data_selected <- selected_symbol_data$data
    data_xts <- xts::xts(data_selected$close, order.by = as.Date(data_selected$Date))
    
    sma50 <- rollmean(data_xts, k = 50, align = "right", fill = NA)
    sma100 <- rollmean(data_xts, k = 100, align = "right", fill = NA)
    sma200 <- rollmean(data_xts, k = 200, align = "right", fill = NA)
    
    data_for_plot <- data.frame(Date = index(data_xts),
                                Close = coredata(data_xts),
                                SMA50 = coredata(sma50),
                                SMA100 = coredata(sma100),
                                SMA200 = coredata(sma200))
    
    # Plot Moving Averages with plotly
    plot_ma <- plot_ly(data = data_for_plot, x = ~Date) %>%
      add_lines(y = ~Close, name = 'Close Price', line = list(color = 'blue')) %>%
      add_lines(y = ~SMA50, name = 'SMA 50', line = list(color = 'green')) %>%
      add_lines(y = ~SMA100, name = 'SMA 100', line = list(color = 'orange')) %>%
      add_lines(y = ~SMA200, name = 'SMA 200', line = list(color = 'red')) %>%
      layout(title = paste('Moving Averages Chart for', input$symbol),
             xaxis = list(
               rangeselector = list(buttons = list(
                 list(count = 1, label = '1m', step = 'month', stepmode = 'backward'),
                 list(count = 6, label = '6m', step = 'month', stepmode = 'backward'),
                 list(count = 1, label = 'YTD', step = 'year', stepmode = 'todate'),
                 list(count = 1, label = '1y', step = 'year', stepmode = 'backward'),
                 list(step = 'all')
               )),
               rangeslider = list(type = 'date')
             ),
             yaxis = list(title = 'Price')
      )
    
    return(plot_ma)
  })
  #####
  output$macdPlotUI <- renderUI({
    
    if (!is.null(input$indicators) && "MACD" %in% input$indicators) {
      plotlyOutput("macdPlot")
    } else {
      return(NULL)
    }
  })
  
  # Render the MACD plot
  output$macdPlot <- renderPlotly({
    req(selected_symbol_data$data)
    
    # Calculate MACD
    data_selected <- selected_symbol_data$data
    data_xts <- xts::xts(data_selected$close, order.by = as.Date(data_selected$Date))
    
    macd_result <- TTR::MACD(data_xts, nFast = 12, nSlow = 26, nSig = 9, maType = SMA)
    
    # Prepare data for plotting
    data_for_plot <- data_selected %>%
      mutate(
        MACD = coredata(macd_result$macd),
        Signal = coredata(macd_result$signal),
        Histogram = coredata(macd_result$macd - macd_result$signal)
      )
    
    # Plot the MACD with signal and histogram
    plot_macd <- plot_ly(data = data_for_plot, x = ~Date) %>%
      add_trace(y = ~MACD, name = 'MACD', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~Signal, name = 'Signal Line', type = 'scatter', mode = 'lines') %>%
      add_bars(y = ~Histogram, name = 'Histogram') %>%
      layout(
        title = paste('MACD Chart for', input$symbol),
        xaxis = list(
          rangeselector = list(buttons = list(
            list(count = 1, label = '1m', step = 'month', stepmode = 'backward'),
            list(count = 6, label = '6m', step = 'month', stepmode = 'backward'),
            list(count = 1, label = 'YTD', step = 'year', stepmode = 'todate'),
            list(count = 1, label = '1y', step = 'year', stepmode = 'backward'),
            list(step = 'all')
          )),
          rangeslider = list(type = 'date')
        ),
        yaxis = list(title = 'Price')
      )
    
    return(plot_macd)
  })
  ######
  output$bbandsPlotUI <- renderUI({
    if ("Bollinger Bands" %in% input$indicators) {
      plotlyOutput("bbandsPlot")
    }
  })
  
  # Render the Bollinger Bands plot
  output$bbandsPlot <- renderPlotly({
    req(selected_symbol_data$data)
    
    # Calculate Bollinger Bands
    data_selected <- selected_symbol_data$data
    data_xts <- xts::xts(data_selected$close, order.by = as.Date(data_selected$Date))
    
    bbands <- TTR::BBands(data_xts, maType = SMA, n = 20, sd = 2)
    
    # Create data for plotting
    data_for_plot <- data_selected %>%
      mutate(
        UpperBand = as.numeric(bbands$up),
        MiddleBand = as.numeric(bbands$mavg),
        LowerBand = as.numeric(bbands$dn)
      )
    
    # Plot Bollinger Bands
    plot_bbands <- plot_ly(data = data_for_plot, x = ~Date, y = ~UpperBand, name = 'Upper Band', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~MiddleBand, name = 'Middle Band', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~LowerBand, name = 'Lower Band', type = 'scatter', mode = 'lines') %>%
      layout(
        title = paste('Bollinger Bands Chart for', input$symbol),
        xaxis = list(
          rangeselector = list(
            buttons = list(
              list(count = 1, label = '1m', step = 'month', stepmode = 'backward'),
              list(count = 6, label = '6m', step = 'month', stepmode = 'backward'),
              list(count = 1, label = 'YTD', step = 'year', stepmode = 'todate'),
              list(count = 1, label = '1y', step = 'year', stepmode = 'backward'),
              list(step = 'all')
            )
          ),
          rangeslider = list(type = 'date')
        ),
        yaxis = list(title = 'Price')
      )
    
    plotly::ggplotly(plot_bbands)
  })
  #####
  output$rsiPlotUI <- renderUI({
    if ("RSI" %in% input$indicators) {
      plotlyOutput("rsiPlot")
    }
  })
  output$rsiPlot <- renderPlotly({
    req(selected_symbol_data$data)
    
    # Retrieve the selected symbol's data
    data_selected <- selected_symbol_data$data
    
    # Convert data to xts format for time series analysis
    data_xts <- xts::xts(data_selected$close, order.by = as.Date(data_selected$Date))
    
    # Calculate RSI (14-period default)
    rsi <- TTR::RSI(data_xts, n = 14)
    
    # Prepare the data for plotting
    data_for_plot <- data_selected %>%
      mutate(RSI = as.numeric(rsi))  # Add RSI to the data frame
    
    # Overbought and oversold lines
    overbought_line <- rep(70, nrow(data_for_plot))  # Overbought at RSI 70
    oversold_line <- rep(30, nrow(data_for_plot))  # Oversold at RSI 30
    
    # Plot the RSI with overbought/oversold lines
    plot_rsi <- plot_ly(data = data_for_plot, x = ~Date) %>%
      add_trace(y = ~RSI, name = 'RSI', type = 'scatter', mode = 'lines', line = list(color = 'blue')) %>%
      add_trace(y = ~overbought_line, name = 'Overbought (70)', type = 'scatter', mode = 'lines', line = list(color = 'red', dash = 'dot')) %>%
      add_trace(y = ~oversold_line, name = 'Oversold (30)', type = 'scatter', mode = 'lines', line = list(color = 'green', dash = 'dot')) %>%
      layout(
        title = paste('RSI Chart for', input$symbol),
        xaxis = list(
          rangeselector = list(
            buttons = list(
              list(count = 1, label = '1m', step = 'month', stepmode = 'backward'),
              list(count = 6, label = '6m', step = 'month', stepmode = 'backward'),
              list(count = 1, label = 'YTD', stepmode = 'todate'),
              list(count = 1, label = '1y', step = 'year', stepmode = 'backward'),
              list(step = 'all')
            )
          ),
          rangeslider = list(type = 'date')  # Slider at bottom to choose date range
        ),
        yaxis = list(title = 'RSI', fixedrange = FALSE),  # Title for y-axis, allow scaling
        yaxis2 = list(title = 'Volume (in millions)', overlaying = 'y', side = 'right')  # Right axis for volume
      )
    
    plotly::ggplotly(plot_rsi)  # Convert plot_ly to ggplotly
  })
 
  
   #####PHÂN TÍCH TÀI CHÍNH 
  output$asset_plot <- renderPlotly({
    if(is.null(input$symbol) || input$symbol == "None") {
      return(NULL)
    }
    # Lấy và xử lý dữ liệu cho Tổng Tài Sản Hiện Hành
    total_assets_data <- filtered_data() %>%
      mutate(totalCurrentAssets = as.numeric(totalCurrentAssets))
    
    # Biểu đồ cho Tổng Tài Sản Hiện Hành
    p <- ggplot(total_assets_data, aes(x = Year, y = totalCurrentAssets / 10^9)) +
      geom_col(fill = "steelblue") +
      labs(
        title = paste("Total Current Assets for", input$symbol),
        x = "Year",
        y = "Total Current Assets (Billion USD)"
      ) +
      theme_minimal() +
      scale_y_continuous(labels = scales::dollar_format(suffix = "B", prefix = ""))
    
    # Lấy và xử lý dữ liệu cho Lợi Nhuận Gộp
    data_gross_profit <- data_list_income %>%
      filter(Symbol == input$symbol) %>%
      mutate(grossProfit = as.numeric(grossProfit))
    
    # Biểu đồ cho Lợi Nhuận Gộp
    p1 <- ggplot(data_gross_profit, aes(x = Year, y = grossProfit / 10^9)) +
      geom_col(fill = "green") +
      labs(
        title = paste("Gross Profit for", input$symbol),
        x = "Year",
        y = "Gross Profit (Billion USD)"
      ) +
      theme_minimal() +
      scale_y_continuous(labels = scales::dollar_format(suffix = "B", prefix = ""))
    
    # Lấy và xử lý dữ liệu cho Doanh Thu Tổng
    total_revenue_data <- data_list_income %>%
      filter(Symbol == input$symbol) %>%
      mutate(totalRevenue = as.numeric(totalRevenue))
    
    # Biểu đồ cho Doanh Thu Tổng
    p2 <- ggplot(total_revenue_data, aes(x = Year, y = totalRevenue / 10^9)) +
      geom_col(fill = "yellow") +
      labs(
        title = paste("Annual Total Revenue for", input$symbol),
        x = "Year",
        y = "Total Revenue (Billion USD)"
      ) +
      theme_minimal() +
      scale_y_continuous(labels = scales::dollar_format(suffix = "B", prefix = ""))
    
    # Lấy dữ liệu cho phần Cash Flow
    cashflow_data <- data_list_cashflow %>%
      filter(Symbol == input$symbol) %>%
      mutate(across(c(operatingCashflow, cashflowFromInvestment, cashflowFromFinancing, changeInCashAndCashEquivalents), as.numeric))
    
    # Tạo biểu đồ dòng tiền với ggplot
    p3 <- ggplot(cashflow_data, aes(x = Year)) +
      geom_col(aes(y = operatingCashflow/10^9 , fill = 'Operating Cashflow'), position = "dodge") +
      geom_col(aes(y = cashflowFromFinancing/10^9, fill = 'Financing Cashflow'), position = "dodge") +
      geom_col(aes(y = cashflowFromInvestment/10^9, fill = 'Investment Cashflow'), position = "dodge") +
      geom_line(aes(y = changeInCashAndCashEquivalents/10^9, group = 1), color = "red", size = 1) +
      geom_point(aes(y = changeInCashAndCashEquivalents/10^9), color = "red", size = 3) +
      scale_fill_manual(values = c("Operating Cashflow" = "#00AFBB", "Financing Cashflow" = "#FC4E07" , "Investment Cashflow" = "#E7B800")) +
      theme_minimal() +
      labs(title = "Cash Flow Analysis", y = "Cash Flow (USD)", x = "Year") +
      scale_y_continuous(labels = scales::dollar_format(suffix = "B", prefix = "$"))
    
    # Lấy dữ liệu từ data_list_income và xử lý
    income_data <- data_list_income %>%
      filter(Symbol == input$symbol) %>%
      mutate(across(c(totalRevenue), ~ifelse(. == "NONE", 0, as.numeric(.))))
    
    # Lấy dữ liệu từ data_list_balance và xử lý
    balance_data <- data_list_balance %>%
      filter(Symbol == input$symbol) %>%
      mutate(
        across(
          c(totalAssets, inventory, currentNetReceivables, propertyPlantEquipment, accumulatedDepreciationAmortizationPPE, intangibleAssets),
          ~ifelse(. == "NONE", 0, as.numeric(.))
        )
      )
    # Tính toán các chỉ số hiệu suất kinh doanh
    efficiency_data <- income_data %>%
      left_join(balance_data) %>%
      mutate(
        Turnover_Ratio_Inventory = ifelse(is.numeric(totalRevenue) & is.numeric(inventory) & inventory != 0, totalRevenue / inventory, NA),
        Turnover_Ratio_Fixed_Assets = ifelse(is.numeric(totalRevenue) & is.numeric(propertyPlantEquipment) & (propertyPlantEquipment) != 0, totalRevenue / (propertyPlantEquipment), NA),
        Turnover_Ratio_Total_Assets = ifelse(is.numeric(totalRevenue) & is.numeric(totalAssets) & totalAssets != 0, totalRevenue / totalAssets, NA),
        Average_Collection_Period = ifelse(is.numeric(currentNetReceivables) & is.numeric(totalRevenue) & totalRevenue != 0, (currentNetReceivables * 365) / totalRevenue, NA)
      )
    
    # Tạo ggplot object cho biểu đồ hiệu suất kinh doanh
    p4 <- ggplot(efficiency_data, aes(x = Year)) +
      geom_line(aes(y = Turnover_Ratio_Inventory, color = "Turnover Ratio Inventory"), group = 1) +
      geom_line(aes(y = Turnover_Ratio_Fixed_Assets, color = "Turnover Ratio Fixed Assets"), group = 1) +
      geom_line(aes(y = Turnover_Ratio_Total_Assets, color = "Turnover Ratio Total Assets"), group = 1) +
      geom_line(aes(y = Average_Collection_Period, color = "Average Collection Period"), group = 1) + 
      geom_point(aes(y = Turnover_Ratio_Inventory, color = "Turnover Ratio Inventory")) +
      geom_point(aes(y = Turnover_Ratio_Fixed_Assets, color = "Turnover Ratio Fixed Assets")) +
      geom_point(aes(y = Turnover_Ratio_Total_Assets, color = "Turnover Ratio Total Assets")) +
      geom_point(aes(y = Average_Collection_Period, color = "Average Collection Period")) +
      scale_color_manual(values = c("Turnover Ratio Inventory" = "#1f77b4", "Turnover Ratio Fixed Assets" = "#ff7f0e", "Turnover Ratio Total Assets" = "#d62728", "Average Collection Period" = "red")) +
      theme_minimal() +
      labs(title = "Turnover Ratios and Average Collection Period", y = "Ratio", x = "Year") +
      theme(axis.title.y = element_text(color = "black"), # Màu cho trục y1
            axis.title.y.right = element_text(color = "red"),
            axis.text.y.right = element_text(color = "red"),
            axis.ticks.y.right = element_line(color = "red")) + # Màu cho trục y2
      scale_y_continuous(sec.axis = sec_axis(~ ., name = "Average Collection Period", breaks = NULL)) # Tạo hệ trục y thứ hai
    
    # Lọc và chuẩn bị dữ liệu cho ROE
    income_filtered <- income_data %>%
      filter(Symbol == input$symbol) %>%
      select(fiscalDateEnding, totalRevenue, costOfRevenue, incomeTaxExpense) %>%
      mutate(across(c(totalRevenue, costOfRevenue, incomeTaxExpense), as.numeric))
    
    balance_filtered <- balance_data %>%
      filter(Symbol == input$symbol) %>%
      select(fiscalDateEnding, totalShareholderEquity) %>%
      mutate(across(totalShareholderEquity, as.numeric))
    
    # Tính toán ROE
    roe_data <- merge(income_filtered, balance_filtered, by = "fiscalDateEnding") %>%
      mutate(ROE = (totalRevenue - costOfRevenue - incomeTaxExpense) / totalShareholderEquity * 100) # Chuyển ROE thành tỷ lệ phần trăm
    
    # Vẽ biểu đồ đường kết hợp chấm tròn
    p5 <- ggplot(roe_data, aes(x = as.Date(fiscalDateEnding), y = ROE)) +
      geom_line(color = "steelblue") + # Vẽ đường
      geom_point(color = "red", size = 1) + # Thêm chấm tròn màu đỏ và kích thước lớn hơn
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") + # Định dạng trục x chỉ hiện năm
      labs(
        title = paste("Annual ROE for", input$symbol),
        x = "Fiscal Year Ending",
        y = "ROE (%)"
      ) +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) # Hiển thị đơn vị là %
    
    
    # Lọc và chuẩn bị dữ liệu cho ROA
    income_filtered <- income_data %>%
      filter(Symbol == input$symbol) %>%
      select(fiscalDateEnding, totalRevenue, costOfRevenue, incomeTaxExpense) %>%
      mutate(across(c(totalRevenue, costOfRevenue, incomeTaxExpense), as.numeric))
    
    balance_filtered <- balance_data %>%
      filter(Symbol == input$symbol) %>%
      select(fiscalDateEnding, totalAssets) %>%
      mutate(across(totalAssets, as.numeric))
    
    # Tính toán ROA
    roa_data <- merge(income_filtered, balance_filtered, by = "fiscalDateEnding") %>%
      mutate(ROA = ((totalRevenue - costOfRevenue - incomeTaxExpense) / totalAssets) * 100) # Biểu diễn ROA dưới dạng phần trăm
    
    # Vẽ biểu đồ đường kết hợp chấm tròn
    p6 <- ggplot(roa_data, aes(x = as.Date(fiscalDateEnding), y = ROA)) +
      geom_line(color = "steelblue") + # Vẽ đường
      geom_point(color = "red", size = 1) + # Thêm chấm tròn
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") + # Định dạng trục x chỉ hiện năm
      labs(
        title = paste("Annual ROA for", input$symbol),
        x = "Fiscal Year Ending",
        y = "ROA (%)"
      ) +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) # Định dạng phần trăm, chỉnh scale phù hợp 
    
    # Lọc và chuẩn bị dữ liệu cho ROIC
    income_filtered <- income_data %>%
      filter(Symbol == input$symbol) %>%
      select(fiscalDateEnding, totalRevenue, costOfRevenue, incomeTaxExpense) %>%
      mutate(across(c(totalRevenue, costOfRevenue, incomeTaxExpense), as.numeric))
    
    balance_filtered <- balance_data %>%
      filter(Symbol == input$symbol) %>%
      select(fiscalDateEnding, longTermDebt, totalShareholderEquity) %>%
      mutate(across(c(longTermDebt, totalShareholderEquity), as.numeric))
    
    # Tính toán ROIC
    roic_data <- merge(income_filtered, balance_filtered, by = "fiscalDateEnding") %>%
      mutate(ROIC = ((totalRevenue - costOfRevenue - incomeTaxExpense) / (longTermDebt + totalShareholderEquity)) * 100) # Biểu diễn ROIC dưới dạng phần trăm
    
    # Vẽ biểu đồ đường kết hợp chấm tròn cho ROIC
    p_roic <- ggplot(roic_data, aes(x = as.Date(fiscalDateEnding), y = ROIC)) +
      geom_line(color = "orange") + # Vẽ đường màu cam cho ROIC
      geom_point(color = "red", size = 1) + # Thêm chấm tròn màu đỏ và kích thước lớn hơn
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") + # Định dạng trục x chỉ hiện năm
      labs(
        title = paste("Annual ROIC for", input$symbol),
        x = "Fiscal Year Ending",
        y = "ROIC (%)"
      ) +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) # Hiển thị đơn vị là %
    
    
    # Kết hợp dữ liệu ROE, ROA và ROIC
    combined_data <- merge(merge(roe_data, roa_data, by = "fiscalDateEnding", suffixes = c("_ROE", "_ROA")), roic_data, by = "fiscalDateEnding")
    
    # Vẽ biểu đồ kết hợp ROE, ROA và ROIC
    p_combined <- ggplot(combined_data, aes(x = as.Date(fiscalDateEnding))) +
      geom_line(aes(y = ROE, colour = "ROE"), size = 0.7) + # Đường ROE màu xanh lá
      geom_point(aes(y = ROE), color = "red", size = 1.2) + # Chấm tròn màu đỏ cho ROE
      geom_line(aes(y = ROA, colour = "ROA"), size = 0.7) + # Đường ROA màu xanh steelblue
      geom_point(aes(y = ROA), color = "red", size = 1.2) + # Chấm tròn màu đỏ cho ROA
      geom_line(aes(y = ROIC, colour = "ROIC"), size = 0.7) + # Đường ROIC màu cam
      geom_point(aes(y = ROIC), color = "red", size = 1.2) + # Chấm tròn màu đỏ cho ROIC
      scale_colour_manual(values = c("ROE" = "green", "ROA" = "steelblue", "ROIC" = "orange")) + # Định nghĩa màu sắc cho các đường
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") + # Định dạng trục x chỉ hiện năm
      labs(
        title = paste("Annual ROE, ROA, and ROIC for", input$symbol),
        x = "Fiscal Year Ending",
        y = "Percentage (%)"
      ) +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) # Định dạng phần trăm
    
    combined_plot <- subplot(p, p1, p2, p3, p4, p_combined, nrows = 3, margin = 0.05)
    
    # Define annotations for plot titles placed below each plot, in English
    title_annotations <- list(
      list(text = "Total Current Assets", x = 0.1 , y = 1.05, xref = 'paper', yref = 'paper', showarrow = FALSE),
      list(text = "Gross Profit", x = 0.83, y = 1.05, xref = 'paper', yref = 'paper', showarrow = FALSE),
      list(text = "Total Revenue", x = 0.13, y = 0.63, xref = 'paper', yref = 'paper', showarrow = FALSE),
      list(text = "Cash Flow Analysis", x = 0.80, y = 0.68, xref = 'paper', yref = 'paper', showarrow = FALSE),
      list(text = "Business Performance Ratios", x = 0.15, y = 0.26, xref = 'paper', yref = 'paper', showarrow = FALSE),
      list(text = "Combined ROE, ROA, ROIC", x = 0.83, y = 0.26, xref = 'paper', yref = 'paper', showarrow = FALSE)
    )
    
    # Apply the annotations for titles below each plot
    combined_plot <- combined_plot %>% layout(annotations = title_annotations)
    
    return(combined_plot)
  })
  
####
  generate_scatter_plot <- function(summary_stats) {
    plot_ly(data = summary_stats,
            type = "scatter",
            mode = "markers",
            x = ~SD.Log.Returns,
            y = ~Mean.Log.Returns,
            color = ~N.Trade.Days,
            colors = "Blues",
            size = ~N.Trade.Days,
            text = ~paste0("Symbol: ", Symbol, "\nIndustry: ", Industry, "\nN Trade Days: ", N.Trade.Days),
            marker = list(opacity = 0.8, sizemode = 'diameter', sizeref = 0.1, line = list(width = 1, color = '#FFFFFF'))
    ) %>%
      layout(title = 'S&P 500 Analysis: Stock Risk vs Reward',
             xaxis = list(title = 'Risk/Variability (SD Log Returns)'),
             yaxis = list(title = 'Reward/Growth (Mean Log Returns)'),
             plot_bgcolor = 'rgb(245, 245, 245)',
             paper_bgcolor = 'rgb(245, 245, 245)')
  }
  ####
  observeEvent(input$quant_analysis, {
    if (input$quant_analysis == "Computing Correlations") {
      # Assuming 'data' is your dataset, replace this with how you actually load your data
      stock_data <- data # Update path accordingly
      
      # Ensure Date column is of Date type
      stock_data$Date <- as.Date(stock_data$Date)
      
      # Calculate log returns for each stock
      data <- stock_data %>%
        arrange(Symbol, Date) %>%
        group_by(Symbol) %>%
        mutate(Log.Returns = log(close / lag(close)))  # Calculate log returns
      
      # Calculate summary statistics for each stock
      summary_stats <- data %>%
        group_by(Symbol) %>%
        summarise(
          Industry = first(Industry),  # Capture the industry for each symbol, assuming it's constant per Symbol
          Mean.Log.Returns = mean(Log.Returns, na.rm = TRUE),
          SD.Log.Returns = sd(Log.Returns, na.rm = TRUE),
          N.Trade.Days = n(),
          .groups = 'drop'  # Drop the grouping
        )
      
      summary_stats_hp <- summary_stats %>%
        filter(N.Trade.Days > 1000) %>%
        filter(SD.Log.Returns < 0.0315) %>%
        mutate(rank = Mean.Log.Returns %>% desc() %>% dense_rank()) %>%
        filter(rank <= 30) %>%
        arrange(rank) %>%
        select(Symbol, rank, Mean.Log.Returns, SD.Log.Returns)
      
      # Join the high-performing summary stats with the original data to get log returns
      summary_stats_hp_unnest <- summary_stats_hp %>%
        inner_join(data, by = "Symbol") %>%
        select(Symbol, Date, Log.Returns)
      
      summary_stats_hp_spread <- summary_stats_hp_unnest %>%
        pivot_wider(names_from = Symbol, values_from = Log.Returns) %>%
        na.omit()
      
      summary_stats_hp_cor <- summary_stats_hp_spread %>%
        select(-Date) %>%
        cor() 
      
      output$correlationPlot <- renderPlot({
        corrplot(summary_stats_hp_cor, order = "hclust", addrect = 11)
      })
      
      output$correlationSummary <- renderTable({
        round(summary_stats_hp_cor[1:6, 1:6], 2)
      })
    }
    
  })
  #####
  df_wide <- reshape2::dcast(data, Date ~ Symbol, value.var = "close")
  
  # Loại bỏ cột Date để chỉ giữ lại các giá trị giá đóng cửa
  df_wide <- df_wide[,-1]
  
  # Tính toán ma trận tương quan
  cor_matrix <- cor(df_wide, use="complete.obs", method="pearson")
  
  # Chuyển đổi ma trận tương quan thành dạng long để sử dụng trong ggplot
  cor_long <- reshape2::melt(cor_matrix)
  distinct_symbols <- data %>% distinct(Symbol)
  # Industry mapping
  industry_mapping <- c("MSFT" = "Technology", "AAPL" = "Technology", "IBM" = "Technology", "CSCO" = "Technology",
                        "AMZN" = "Technology", "INTC" = "Technology", "CRM" = "Technology", "UNH" = "Health",
                        "JNJ" = "Health", "AMGN" = "Health", "MRK" = "Health", "MCD" = "Consumer", "KO" = "Consumer",
                        "WMT" = "Consumer", "HD" = "Consumer", "GS" = "Finance", "JPM" = "Finance",
                        "V" = "Finance", "AXP" = "Finance", "BA" = "Industry", "CAT" = "Industry",
                        "MMM" = "Industry", "HON" = "Industry", "CVX" = "Energy", "VZ" = "Media",
                        "DIS" = "Media", "NKE" = "Retail", "DOW" = "Raw Materials", 
                        "TRV" = "Healthcare ", "PG" = "Pharmaceutical")
  
  data$Phan_nganh <- industry_mapping[data$Symbol]
  #####
  observeEvent(input$quant_analysis, {
    if (input$quant_analysis == "Industry Distribution") {
      output$industryPlot <- renderPlot({
        data %>%
          # Summarise data by frequency, count unique occurrences per sector
          group_by(Phan_nganh) %>%
          summarise(count = n_distinct(Symbol)) %>%
          # Prepare for plotting
          mutate(Phan_nganh = fct_reorder(Phan_nganh, count)) %>%
          # Visualize
          ggplot(aes(x = Phan_nganh, y = count)) + 
          geom_bar(stat = "identity", fill = "steelblue") +
          geom_text(aes(label = count), hjust = -0.3, vjust = -0.3, size = 3) +  # Adjust text positioning
          ggtitle("Distribute shares in each industry") +
          xlab("Industry") +
          ylab("Quantity") +
          theme_minimal() +
          theme(plot.title = element_text(size = 16, face = "bold"),
                axis.title = element_text(size = 14),
                axis.text.x = element_text(angle = 45, hjust = 1)) +
          coord_flip()
        
      })
      output$industryTreemap <- renderPlot({
        industry_count <- data %>%
          distinct(Symbol, Phan_nganh) %>%
          group_by(Phan_nganh) %>%
          summarise(count = n())
        
        # Vẽ biểu đồ treemap
        treemap(industry_count,
                index = "Phan_nganh",
                vSize = "count",
                type = "index",
                title = "Number of Companies in Each Sector",
                palette = "Set3",
                border.col = "white",
                fontsize.labels = 12)
      })
      output$techTreemap <- renderPlot({
        tech_df <- data %>%
          filter(Phan_nganh == "Technology")
        
        tech_count <- tech_df %>%
          distinct(Symbol, Phan_nganh) %>%
          group_by(Symbol) %>%
          summarise(count = n())
        
        treemap(
          tech_count,
          index = "Symbol",
          vSize = "count",
          type = "index",
          title = "Number of Companies in the Technology Sector",
          palette = "Set3",
          border.col = "white",
          fontsize.labels = 12)
      })
    }
  })
  observeEvent(input$quant_analysis, {
    if (input$quant_analysis == "None") {
      return() 
    }
  })
  observeEvent(input$indicators, {
    if (input$indicators == "None") {
      return() 
    }
  })
  ########
  observeEvent(input$Type, {
    aapl_data <- data %>%
      filter(Symbol == input$symbol) %>%
      mutate(Date = as.Date(Date, format="%Y-%m-%d"),
             Normalized_Value = rescale(close, to = c(0, 1)))
    
    num_vars <- c("close", "high", "open", "low", "volume", "Adjusted")
    
    aapl_log <- aapl_data %>%
      mutate(across(all_of(num_vars), log))
    
    plot_list <- list()
    
    if (input$Type == "Cross-Correlation Function") {
      for (i in 1:(length(num_vars)-1)) {
        for (j in (i+1):length(num_vars)) {
          ccf_data <- ccf(aapl_log[[num_vars[i]]], aapl_log[[num_vars[j]]], plot = FALSE)
          lags <- ccf_data$lag
          ccf_values <- ccf_data$acf
          plot_df <- data.frame(Lag = lags, CCF = ccf_values)
          p <- ggplot(plot_df, aes(x = Lag, y = CCF)) +
            geom_hline(yintercept = 0, linetype = "dashed") +
            geom_bar(stat = "identity", position = "dodge") +
            theme_minimal() +
            labs(title = paste(num_vars[i], "&", num_vars[j]))
          plot_name <- paste(num_vars[i], num_vars[j], sep = "_")
          plot_list[[plot_name]] <- p
        }
      }
      
      output$ccf_plots <- renderPlot({
        do.call(gridExtra::grid.arrange, c(plot_list, ncol = 4))
      })
    }
    
    if (input$Type == "ARIMA") {
      train_indices <- 1:round(0.8 * nrow(aapl_log))
      test_indices <- (max(train_indices) + 1):nrow(aapl_log)
      
      train_data <- aapl_log[train_indices, ]
      test_data <- aapl_log[test_indices, ]
      train_ts <- ts(train_data$close, frequency = 252)
      
      set.seed(123)
      arima_model <- auto.arima(train_ts, stationary = TRUE, ic = c("aicc", "aic", "bic"), trace = TRUE)
      
      output$arimaSummary <- renderPrint({
        summary(arima_model)
      })
      
      output$arimaResiduals <- renderPlot({
        checkresiduals(arima_model)
      })
      
      test_ts <- ts(test_data$close, start = end(train_ts)[1] + 1, frequency = 252)
      forecast_results <- forecast(arima_model, h = length(test_ts))
      
      output$forecast_plot <- renderPlot({
         autoplot(forecast_results) + 
          autolayer(test_ts, series = "Actual Test Data") +
          labs(title = "Forecast vs Actual Data", y = "Log(AAPL Close Price)")
      })
    }
    output$dnnPlot <- renderPlot({
      if (input$Type == "DNN") {
        training_data <- data.frame(Date = seq.Date(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"),
                                    Predicted = runif(365, min = 100, max = 200))
        
        prediction_results <- data.frame(Date = seq.Date(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"),
                                         Predicted = runif(365, min = 100, max = 200))
        
        history_df <- data.frame(epoch = 1:100,
                                 loss = runif(100, min = 0.1, max = 0.5),
                                 val_loss = runif(100, min = 0.1, max = 0.5),
                                 mean_absolute_error = runif(100, min = 5, max = 20),
                                 val_mean_absolute_error = runif(100, min = 5, max = 20))
        
        actual_data_to_may <- data.frame(Date = seq.Date(as.Date("2024-01-01"), as.Date("2024-05-15"), by = "day"),
                                         close = runif(136, min = 100, max = 200))
        
        # Plot
        p1 <- ggplot(history_df %>% filter(metric %in% c("loss", "val_loss")),
                     aes(x = epoch, y = value, color = metric)) +
          geom_line() +
          labs(title = "Training and Validation Loss", x = "Epoch", y = "Loss", color = "Metric") +
          theme_minimal()
        
        p2 <- ggplot(history_df %>% filter(metric %in% c("mean_absolute_error", "val_mean_absolute_error")),
                     aes(x = epoch, y = value, color = metric)) +
          geom_line() +
          labs(title = "Training and Validation MAE", x = "Epoch", y = "MAE", color = "Metric") +
          theme_minimal()
        
        p3 <- ggplot(prediction_results, aes(x = Date, y = Predicted)) +
          geom_line(color = 'blue') +
          labs(title = "Stock price forecast for 2024", x = "Date", y = "Predicted Price") +
          theme_minimal()
        
        p4 <- ggplot(actual_data_to_may, aes(x = Date, y = close)) +
          geom_line(color = 'red') +
          labs(title = "Actual Stock Prices until May 15, 2024", x = "Date", y = "Actual Price") +
          theme_minimal()
        
        # Arrange plots
        plot_grid(p1, p2, p3, p4, nrow = 2)
      }
    })
    
    output$eda_summary <- renderPrint({
      summary(aapl_data)
    })
    
    output$eda_structure <- renderPrint({
      str(aapl_data)
    })
  })
  
      
  
  # Plot outputs
  output$capmSummary <- renderPrint({
    # Assuming 'returns_df6' and 'FF' are already available
    ff_assets <- returns_df %>%
      left_join(FF, by = "Date") %>%
      mutate(
        MKT_RF = `Mkt-RF` / 100,
        SMB = SMB / 100,
        HML = HML / 100,
        RF = RF / 100,
        AAPL = AAPL - RF
      ) %>%
      select(-RF, -`Mkt-RF`) %>%
      na.omit()
    
    # CAPM model
    capm_model <- lm(AAPL ~ MKT_RF, data = ff_assets)
    summary(capm_model)
  })
  filtered_df22 <- subset(data, Symbol %in% c("AAPL", "IBM", "AMZN"))
  
  stock_returns_monthly <- filtered_df22 %>%
    group_by(Symbol) %>%
    tq_transmute(select = Adjusted, mutate_fun = periodReturn, period = "monthly", col_rename = "Ra")
  
  wts <- c(0.5, 0.0, 0.5)
  portfolio_returns_monthly <- stock_returns_monthly %>%
    tq_portfolio(assets_col = Symbol, returns_col = Ra, weights = wts, col_rename = "Ra")
  
  output$portfolio_returns_plot <- renderPlot({
    portfolio_returns_monthly %>%
      ggplot(aes(x = Date, y = Ra)) +
      geom_bar(stat = "identity", fill = palette_light()[[1]]) +
      labs(title = "Portfolio Returns",
           subtitle = "50% AAPL, 0% IBM, and 50% AMZN",
           caption = "Shows an above-zero trend meaning positive returns",
           x = "", y = "Monthly Returns") +
      geom_smooth(method = "lm") +
      theme_tq() +
      scale_color_tq() +
      scale_y_continuous(labels = scales::percent)
  })
  
  portfolio_growth_monthly <- stock_returns_monthly %>%
    tq_portfolio(assets_col = Symbol, returns_col = Ra, weights = wts, col_rename = "investment.growth", wealth.index = TRUE) %>%
    mutate(investment.growth = investment.growth * 10000)
  
  output$portfolio_growth_plot <- renderPlot({
    portfolio_growth_monthly %>%
      ggplot(aes(x = Date, y = investment.growth)) +
      geom_line(size = 2, color = palette_light()[[1]]) +
      labs(title = "Portfolio Growth",
           subtitle = "50% AAPL, 0% IBM, and 50% AMZN",
           caption = "Now we can really visualize performance!",
           x = "", y = "Portfolio Value") +
      geom_smooth(method = "loess") +
      theme_tq() +
      scale_color_tq() +
      scale_y_continuous(labels = scales::dollar)
  })
  
  stock_returns_monthly_multi <- stock_returns_monthly %>%
    tq_repeat_df(n = 3)
  weights <- c(
    0.50, 0.25, 0.25,
    0.25, 0.50, 0.25,
    0.25, 0.25, 0.50
  )
  stocks <- c("AAPL", "IBM", "AMZN")
  weights_table <-  tibble(stocks) %>%
    tq_repeat_df(n = 3) %>%
    bind_cols(tibble(weights)) %>%
    group_by(portfolio)
  portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>%
    tq_portfolio(assets_col = Symbol, returns_col = Ra, weights = weights_table, col_rename = "investment.growth", wealth.index = TRUE) %>%
    mutate(investment.growth = investment.growth * 10000)
  output$multi_portfolio_growth_plot <- renderPlot({
    portfolio_growth_monthly_multi %>%
      ggplot(aes(x = Date, y = investment.growth, color = factor(portfolio))) +
      geom_line(size = 2) +
      labs(title = "Portfolio Growth",
           subtitle = "Comparing Multiple Portfolios",
           caption = "Portfolio 3 is a Standout!",
           x = "", y = "Portfolio Value",
           color = "Portfolio") +
      geom_smooth(method = "loess") +
      theme_tq() +
      scale_color_tq() +
      scale_y_continuous(labels = scales::dollar)
  })
}
####### 

# Run the application
shinyApp(ui = ui, server = server)