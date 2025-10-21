# Enhanced Trading Scan Analysis Dashboard
# Load required libraries
library(shiny)
library(bslib)
library(dplyr)
library(data.table)
library(DT)
library(ggplot2)
library(plotly)
library(shinycssloaders)
library(shinyWidgets)
library(tidyquant)

# Configuration
CONFIG <- list(
  DATE_FORMAT = "%Y-%m-%d",
  DEFAULT_LOOKBACK_DAYS = 90,
  COLOR_PALETTE = list(
    primary = "#007bff",
    success = "#28a745",
    danger = "#dc3545",
    warning = "#ffc107",
    info = "#17a2b8"
  )
)

# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# Data loading function
create_sample_data <- function() {
  sample_data <- fread("https://raw.githubusercontent.com/rahul-c1/scan/refs/heads/main/weekly_scans.csv")
  sample_data$date <- as.Date(sample_data$date)
  setnames(sample_data, "scan", "scan_symbol")
  setnames(sample_data, "date", "scan_date")
  return(sample_data)
}

# CSV Trading Log Reader
csv_trading_log <- function() {
  tryCatch({
    if (class(try(read.csv("https://raw.githubusercontent.com/rahul-c1/scan/refs/heads/main/trading_log.csv", nrows = 1), silent = TRUE)) == "data.frame") {
      csv_data <- fread("https://raw.githubusercontent.com/rahul-c1/scan/refs/heads/main/trading_log.csv")
      
      if (nrow(csv_data) > 0) {
        csv_data[, trade_date := as.Date(trade_date)]
        csv_data[, trade_time := as.POSIXct(trade_time)]
        csv_data[, created_at := as.POSIXct(created_at)]
        csv_data[, updated_at := as.POSIXct(updated_at)]
        return(csv_data[order(-trade_date, -trade_time)])
      }
    }
    return(data.table())
  }, error = function(e) {
    return(data.table())
  })
}

# UI Definition
ui <- page_navbar(
  title = "Trading Scan Analysis",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#007bff",
    base_font = font_google("Roboto"),
    heading_font = font_google("Roboto Slab")
  ),
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      .navbar { box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      .well { background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px; }
      .value-box { border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); padding: 15px; margin-bottom: 20px; }
      .metric-value { font-size: 32px; font-weight: bold; margin: 0; }
      .metric-label { font-size: 14px; color: #6c757d; margin: 5px 0 0 0; }
      h4 { color: #2c3e50; font-weight: 600; margin-bottom: 15px; }
      .tab-content { padding: 20px; }
      .action-button { margin-right: 5px; margin-bottom: 5px; }
      .info-icon { color: #17a2b8; cursor: help; }
    "))
  ),
  
  # Dashboard Tab
  nav_panel("📊 Dashboard",
            fluidRow(
              column(3,
                     div(class = "value-box", style = "background-color: #e3f2fd;",
                         uiOutput("total_symbols_metric")
                     )
              ),
              column(3,
                     div(class = "value-box", style = "background-color: #f3e5f5;",
                         uiOutput("active_scans_metric")
                     )
              ),
              column(3,
                     div(class = "value-box", style = "background-color: #e8f5e9;",
                         uiOutput("hot_symbols_metric")
                     )
              ),
              column(3,
                     div(class = "value-box", style = "background-color: #fff3e0;",
                         uiOutput("scan_types_metric")
                     )
              )
            ),
            
            fluidRow(
              column(6,
                     card(
                       card_header("📈 Scan Activity Trend (30 Days)"),
                       card_body(
                         plotlyOutput("dashboard_trend", height = "300px") %>% withSpinner()
                       )
                     )
              ),
              column(6,
                     card(
                       card_header("🔥 Top Movers This Week"),
                       card_body(
                         DT::dataTableOutput("dashboard_top_movers") %>% withSpinner()
                       )
                     )
              )
            ),
            
            fluidRow(
              column(6,
                     card(
                       card_header("📊 Scan Type Distribution"),
                       card_body(
                         plotlyOutput("scan_distribution", height = "300px") %>% withSpinner()
                       )
                     )
              ),
              column(6,
                     card(
                       card_header("⚡ Recent Activity Summary"),
                       card_body(
                         uiOutput("recent_activity_summary")
                       )
                     )
              )
            )
  ),
  
  # Scan Frequency Tab
  nav_panel("Scan Frequency",
            fluidRow(
              column(4,
                     card(
                       card_header("Filters"),
                       card_body(
                         dateRangeInput("freq_date_range", "Date Range:",
                                        start = Sys.Date() - 90,
                                        end = Sys.Date(),
                                        max = Sys.Date()
                         ),
                         div(style = "margin-bottom: 10px;",
                             actionButton("freq_last_week", "Last Week", class = "btn-sm btn-outline-primary action-button"),
                             actionButton("freq_last_month", "Last Month", class = "btn-sm btn-outline-primary action-button"),
                             actionButton("freq_last_quarter", "Last Quarter", class = "btn-sm btn-outline-primary action-button")
                         ),
                         selectInput("freq_scan_type", "Scan Type:",
                                     choices = NULL,
                                     multiple = TRUE
                         ),
                         numericInput("freq_min_appearances", "Min Appearances:",
                                      value = 1, min = 1
                         ),
                         actionButton("freq_update", "Update Analysis", 
                                      class = "btn-primary", 
                                      style = "width: 100%;"
                         )
                       )
                     )
              ),
              column(8,
                     card(
                       card_header("Top 20 Symbols by Frequency"),
                       card_body(
                         plotlyOutput("freq_plot", height = "400px") %>% withSpinner()
                       )
                     ),
                     card(
                       card_header("Detailed Results"),
                       card_body(
                         DT::dataTableOutput("freq_table") %>% withSpinner()
                       )
                     )
              )
            )
  ),
  
  # Scan Patterns Tab
  nav_panel("Scan Patterns",
            fluidRow(
              column(3,
                     card(
                       card_header("Pattern Analysis"),
                       card_body(
                         dateRangeInput("pattern_date_range", "Date Range:",
                                        start = Sys.Date() - 180,
                                        end = Sys.Date()
                         ),
                         selectInput("pattern_view", "View Type:",
                                     choices = list(
                                       "Heatmap" = "heatmap",
                                       "Time Series" = "timeseries",
                                       "Weekly Breakdown" = "weekly"
                                     )
                         ),
                         actionButton("pattern_update", "Update", 
                                      class = "btn-primary",
                                      style = "width: 100%;"
                         )
                       )
                     )
              ),
              column(9,
                     card(
                       card_header("Pattern Visualization"),
                       card_body(
                         plotlyOutput("pattern_plot", height = "500px") %>% withSpinner()
                       )
                     ),
                     card(
                       card_header("Recent Scan Summary"),
                       card_body(
                         DT::dataTableOutput("pattern_summary") %>% withSpinner()
                       )
                     )
              )
            )
  ),
  
  # Momentum Tracker Tab
  nav_panel("Momentum Tracker",
            fluidRow(
              column(4,
                     card(
                       card_header("Momentum Settings"),
                       card_body(
                         sliderInput("momentum_weeks", "Analysis Period (weeks):",
                                     value = 2, min = 2, max = 12, step = 1
                         ),
                         selectInput("momentum_type", "Momentum Type:",
                                     choices = list(
                                       "Heating Up" = "Heating Up",
                                       "Cooling Down" = "Cooling Down",
                                       "Both" = "both"
                                     )
                         ),
                         actionButton("momentum_update", "Analyze Momentum", 
                                      class = "btn-primary",
                                      style = "width: 100%;"
                         )
                       )
                     ),
                     card(
                       card_header("🔥 Hot Symbols Alert"),
                       card_body(
                         verbatimTextOutput("hot_symbols")
                       )
                     )
              ),
              column(8,
                     card(
                       card_header("Momentum Visualization"),
                       card_body(
                         plotlyOutput("momentum_plot", height = "400px") %>% withSpinner()
                       )
                     ),
                     card(
                       card_header("Detailed Momentum Analysis"),
                       card_body(
                         DT::dataTableOutput("momentum_table") %>% withSpinner()
                       )
                     )
              )
            )
  ),
  
  # Portfolio Screen Tab
  nav_panel("Portfolio Screen",
            fluidRow(
              column(4,
                     card(
                       card_header("Portfolio Input"),
                       card_body(
                         textAreaInput("portfolio_symbols", "Enter Symbols (one per line):",
                                       value = "AGX\nBE\nPLTR", height = "150px"
                         ),
                         dateRangeInput("portfolio_date_range", "Analysis Period:",
                                        start = Sys.Date() - 30,
                                        end = Sys.Date()
                         ),
                         actionButton("portfolio_analyze", "Analyze Portfolio", 
                                      class = "btn-success",
                                      style = "width: 100%;"
                         )
                       )
                     ),
                     card(
                       card_header("Portfolio Metrics"),
                       card_body(
                         verbatimTextOutput("portfolio_metrics")
                       )
                     )
              ),
              column(8,
                     card(
                       card_header("Portfolio Scan Activity"),
                       card_body(
                         plotlyOutput("portfolio_plot", height = "400px") %>% withSpinner()
                       )
                     ),
                     card(
                       card_header("Weekly Comparison"),
                       card_body(
                         DT::dataTableOutput("portfolio_table3") %>% withSpinner()
                       )
                     )
              )
            )
  ),
  
  # Signal Strength Tab
  nav_panel("Signal Strength",
            fluidRow(
              column(4,
                     card(
                       card_header("Signal Weights Configuration"),
                       card_body(
                         p("Assign weights to each scan type:"),
                         uiOutput("weight_inputs"),
                         numericInput("signal_min_score", "Min Signal Score:",
                                      value = 0.25, min = 0, max = 1, step = 0.05
                         ),
                         actionButton("signal_calculate", "Calculate Signals", 
                                      class = "btn-primary",
                                      style = "width: 100%;"
                         )
                       )
                     )
              ),
              column(8,
                     card(
                       card_header("Signal Strength Visualization"),
                       card_body(
                         plotlyOutput("signal_plot", height = "400px") %>% withSpinner()
                       )
                     ),
                     card(
                       card_header("Top Signal Strength Symbols"),
                       card_body(
                         DT::dataTableOutput("signal_table") %>% withSpinner()
                       )
                     )
              )
            )
  ),
  
  # Alert System Tab
  nav_panel("Alert System",
            fluidRow(
              column(4,
                     card(
                       card_header("Alert Configuration"),
                       card_body(
                         selectInput("alert_symbols", "Watch Symbols:",
                                     choices = NULL, multiple = TRUE
                         ),
                         selectInput("alert_scan_types", "Watch Scan Types:",
                                     choices = NULL, multiple = TRUE
                         ),
                         numericInput("alert_frequency_threshold", "Frequency Threshold:",
                                      value = 3, min = 1
                         ),
                         checkboxInput("alert_new_symbols", "Alert on New Symbols", TRUE),
                         actionButton("alert_setup", "Setup Alerts", 
                                      class = "btn-warning",
                                      style = "width: 100%;"
                         )
                       )
                     ),
                     card(
                       card_header("Active Alerts Summary"),
                       card_body(
                         verbatimTextOutput("active_alerts")
                       )
                     )
              ),
              column(8,
                     card(
                       card_header("Recent Alert Activity"),
                       card_body(
                         DT::dataTableOutput("alert_table") %>% withSpinner()
                       )
                     ),
                     card(
                       card_header("Activity Timeline"),
                       card_body(
                         plotlyOutput("alert_timeline", height = "300px") %>% withSpinner()
                       )
                     )
              )
            )
  ),
  
  # Scan Effectiveness Tab
  nav_panel("Scan Effectiveness",
            fluidRow(
              column(4,
                     card(
                       card_header("⚙️ Analysis Settings"),
                       card_body(
                         dateRangeInput("effectiveness_date_range", "Analysis Period:",
                                        start = Sys.Date() - 60,
                                        end = Sys.Date(),
                                        max = Sys.Date()
                         ),
                         selectInput("effectiveness_scan_filter", "Filter by Scan Types:",
                                     choices = NULL, multiple = TRUE
                         ),
                         numericInput("effectiveness_min_appearances", "Min Scan Appearances:",
                                      value = 2, min = 1, max = 10
                         ),
                         numericInput("effectiveness_lookforward_days", "Price Lookforward Days:",
                                      value = 7, min = 1, max = 30,
                                      step = 1
                         ),
                         actionButton("effectiveness_analyze", "Analyze Effectiveness", 
                                      class = "btn-primary",
                                      style = "width: 100%;"
                         ),
                         hr(),
                         helpText("Note: Price data fetching may take 30-60 seconds for large symbol lists."),
                         br(),
                         downloadButton("effectiveness_download", "Download Results", 
                                        class = "btn-info",
                                        style = "width: 100%;"
                         )
                       )
                     ),
                     card(
                       card_header("📊 Quick Stats"),
                       card_body(
                         verbatimTextOutput("effectiveness_quick_stats")
                       )
                     )
              ),
              column(8,
                     navset_card_tab(
                       nav_panel("📈 Performance Overview",
                                 fluidRow(
                                   column(6,
                                          card(
                                            card_header("Scan Type Win Rate"),
                                            card_body(
                                              plotlyOutput("scan_winrate_chart", height = "350px") %>% withSpinner()
                                            )
                                          )
                                   ),
                                   column(6,
                                          card(
                                            card_header("Average Price Performance"),
                                            card_body(
                                              plotlyOutput("scan_avgreturn_chart", height = "350px") %>% withSpinner()
                                            )
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          card(
                                            card_header("Detailed Performance Metrics"),
                                            card_body(
                                              DT::dataTableOutput("effectiveness_summary_table") %>% withSpinner()
                                            )
                                          )
                                   )
                                 )
                       ),
                       nav_panel("🎯 Symbol Performance",
                                 fluidRow(
                                   column(12,
                                          card(
                                            card_header("Top Performing Symbols"),
                                            card_body(
                                              plotlyOutput("top_symbols_chart", height = "400px") %>% withSpinner()
                                            )
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          card(
                                            card_header("Detailed Symbol Results"),
                                            card_body(
                                              DT::dataTableOutput("symbol_performance_table") %>% withSpinner()
                                            )
                                          )
                                   )
                                 )
                       ),
                       nav_panel("📊 Scan Comparison",
                                 fluidRow(
                                   column(6,
                                          card(
                                            card_header("Max Gain Distribution"),
                                            card_body(
                                              plotlyOutput("max_gain_distribution", height = "350px") %>% withSpinner()
                                            )
                                          )
                                   ),
                                   column(6,
                                          card(
                                            card_header("Return vs Risk"),
                                            card_body(
                                              plotlyOutput("return_risk_scatter", height = "350px") %>% withSpinner()
                                            )
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          card(
                                            card_header("Scan Type Correlation Matrix"),
                                            card_body(
                                              plotlyOutput("scan_correlation_heatmap", height = "400px") %>% withSpinner()
                                            )
                                          )
                                   )
                                 )
                       ),
                       nav_panel("💡 Insights",
                                 fluidRow(
                                   column(12,
                                          card(
                                            card_header("🎯 Scan Effectiveness Summary"),
                                            card_body(
                                              verbatimTextOutput("effectiveness_insights")
                                            )
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(6,
                                          card(
                                            card_header("Best Scan by Metric"),
                                            card_body(
                                              uiOutput("best_scan_metrics")
                                            )
                                          )
                                   ),
                                   column(6,
                                          card(
                                            card_header("Improvement Opportunities"),
                                            card_body(
                                              uiOutput("improvement_opportunities")
                                            )
                                          )
                                   )
                                 )
                       )
                     )
              )
            )
  ),
  
  # Bulk Scanner Tab
  nav_panel("Bulk Scanner",
            fluidRow(
              column(4,
                     card(
                       card_header("Bulk Symbol Analysis"),
                       card_body(
                         textAreaInput("bulk_symbols", "Enter Symbols:",
                                       value = "", height = "150px",
                                       placeholder = "One per line, comma or space separated"
                         ),
                         fileInput("bulk_upload", "Or Upload CSV/TXT:",
                                   accept = c(".csv", ".txt")
                         ),
                         dateRangeInput("bulk_date_range", "Analysis Period:",
                                        start = Sys.Date() - 60,
                                        end = Sys.Date()
                         ),
                         selectInput("bulk_scan_filter", "Filter by Scan Types:",
                                     choices = NULL, multiple = TRUE
                         ),
                         numericInput("bulk_min_scans", "Minimum Scans Required:",
                                      value = 1, min = 0
                         ),
                         actionButton("bulk_analyze", "Run Analysis", 
                                      class = "btn-success",
                                      style = "width: 100%;"
                         ),
                         br(), br(),
                         downloadButton("bulk_download", "Download Results", 
                                        class = "btn-info",
                                        style = "width: 100%;"
                         )
                       )
                     )
              ),
              column(8,
                     navset_card_tab(
                       nav_panel("Results",
                                 DT::dataTableOutput("bulk_results_table") %>% withSpinner()
                       ),
                       nav_panel("Matrix",
                                 DT::dataTableOutput("bulk_matrix_table") %>% withSpinner()
                       ),
                       nav_panel("Statistics",
                                 fluidRow(
                                   column(6,
                                          plotlyOutput("bulk_coverage_plot", height = "300px") %>% withSpinner()
                                   ),
                                   column(6,
                                          plotlyOutput("bulk_distribution_plot", height = "300px") %>% withSpinner()
                                   )
                                 ),
                                 verbatimTextOutput("bulk_summary_stats")
                       )
                     )
              )
            )
  ),
  
  # Trading Log Tab
  nav_panel("Trading Log",
            fluidRow(
              column(4,
                     card(
                       card_header("Quick Filters"),
                       card_body(
                         selectInput("quick_symbol_filter", "Filter by Symbol:", 
                                     choices = NULL, multiple = TRUE
                         ),
                         selectInput("quick_status_filter", "Filter by Status:",
                                     choices = list("All" = "all", "Open" = "open", "Closed" = "closed")
                         ),
                         dateRangeInput("quick_date_filter", "Date Range:",
                                        start = Sys.Date() - 30, end = Sys.Date()
                         ),
                         downloadButton("download_trades", "Download Log", 
                                        class = "btn-success",
                                        style = "width: 100%;"
                         )
                       )
                     )
              ),
              column(8,
                     navset_card_tab(
                       nav_panel("Trading Log",
                                 DT::dataTableOutput("trading_log_table") %>% withSpinner()
                       ),
                       nav_panel("Performance",
                                 fluidRow(
                                   column(6,
                                          verbatimTextOutput("pnl_summary")
                                   ),
                                   column(6,
                                          verbatimTextOutput("trade_stats")
                                   )
                                 ),
                                 fluidRow(
                                   column(6,
                                          plotlyOutput("pnl_chart", height = "300px") %>% withSpinner()
                                   ),
                                   column(6,
                                          plotlyOutput("win_loss_chart", height = "300px") %>% withSpinner()
                                   )
                                 )
                       ),
                       nav_panel("Strategy",
                                 fluidRow(
                                   column(6,
                                          plotlyOutput("entry_criteria_chart", height = "300px") %>% withSpinner()
                                   ),
                                   column(6,
                                          plotlyOutput("scan_source_chart", height = "300px") %>% withSpinner()
                                   )
                                 )
                       )
                     )
              )
            )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Load scan data
  scan_data <- reactive({
    create_sample_data()
  })
  
  # Update input choices
  observe({
    data <- scan_data()
    scan_types <- unique(data$scan_symbol)
    symbols <- unique(data$symbol)
    
    updateSelectInput(session, "freq_scan_type", choices = scan_types)
    updateSelectInput(session, "alert_symbols", choices = symbols)
    updateSelectInput(session, "alert_scan_types", choices = scan_types)
    updateSelectInput(session, "bulk_scan_filter", choices = scan_types)
    updateSelectInput(session, "effectiveness_scan_filter", choices = scan_types)
  })
  
  # Quick date buttons
  observeEvent(input$freq_last_week, {
    updateDateRangeInput(session, "freq_date_range",
                         start = Sys.Date() - 7, end = Sys.Date()
    )
  })
  
  observeEvent(input$freq_last_month, {
    updateDateRangeInput(session, "freq_date_range",
                         start = Sys.Date() - 30, end = Sys.Date()
    )
  })
  
  observeEvent(input$freq_last_quarter, {
    updateDateRangeInput(session, "freq_date_range",
                         start = Sys.Date() - 90, end = Sys.Date()
    )
  })
  
  # Dashboard Metrics
  output$total_symbols_metric <- renderUI({
    data <- scan_data()
    last_date <- max(data$scan_date)
    count <- length(unique(data[scan_date == last_date]$symbol))
    
    tagList(
      h2(count, class = "metric-value", style = "color: #1976d2;"),
      p("Active Symbols Today", class = "metric-label")
    )
  })
  
  output$active_scans_metric <- renderUI({
    data <- scan_data()
    last_date <- max(data$scan_date)
    count <- nrow(data[scan_date == last_date])
    
    tagList(
      h2(count, class = "metric-value", style = "color: #7b1fa2;"),
      p("Total Scans Today", class = "metric-label")
    )
  })
  
  output$hot_symbols_metric <- renderUI({
    data <- scan_data()
    dates <- sort(unique(data$scan_date), decreasing = TRUE)[1:2]
    
    if (length(dates) >= 2) {
      recent <- data[scan_date == dates[1], .N, by = symbol]
      prior <- data[scan_date == dates[2], .N, by = symbol]
      
      merged <- merge(recent, prior, by = "symbol", all.x = TRUE)
      merged[is.na(N.y), N.y := 0]
      hot_count <- sum(merged$N.x > merged$N.y * 1.5)
    } else {
      hot_count <- 0
    }
    
    tagList(
      h2(hot_count, class = "metric-value", style = "color: #388e3c;"),
      p("Heating Up Symbols", class = "metric-label")
    )
  })
  
  output$scan_types_metric <- renderUI({
    data <- scan_data()
    count <- length(unique(data$scan_symbol))
    
    tagList(
      h2(count, class = "metric-value", style = "color: #f57c00;"),
      p("Scan Types Active", class = "metric-label")
    )
  })
  
  # Dashboard Trend
  output$dashboard_trend <- renderPlotly({
    data <- scan_data()
    last_30_days <- data[scan_date >= (Sys.Date() - 30)]
    
    daily_counts <- last_30_days[, .N, by = scan_date][order(scan_date)]
    
    plot_ly(daily_counts, x = ~scan_date, y = ~N, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#007bff', width = 3),
            marker = list(size = 8, color = '#007bff'),
            hovertemplate = paste('<b>Date:</b> %{x}<br>',
                                  '<b>Scans:</b> %{y}<br>',
                                  '<extra></extra>')
    ) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Number of Scans"),
        hovermode = "x unified"
      )
  })
  
  # Dashboard Top Movers
  output$dashboard_top_movers <- renderDT({
    data <- scan_data()
    dates <- sort(unique(data$scan_date), decreasing = TRUE)[1:2]
    
    if (length(dates) >= 2) {
      current <- data[scan_date == dates[1], .N, by = symbol]
      prior <- data[scan_date == dates[2], .N, by = symbol]
      
      merged <- merge(current, prior, by = "symbol", all = TRUE)
      merged[is.na(N.x), N.x := 0]
      merged[is.na(N.y), N.y := 0]
      merged[, change := N.x - N.y]
      
      top_movers <- head(merged[order(-change)], 10)
      setnames(top_movers, c("Symbol", "Current", "Prior", "Change"))
      
      datatable(top_movers,
                options = list(
                  pageLength = 10,
                  dom = 't',
                  ordering = FALSE
                ),
                rownames = FALSE
      ) %>%
        formatStyle('Change',
                    color = styleInterval(0, c('red', 'green')),
                    fontWeight = 'bold'
        )
    }
  })
  
  # Scan Distribution
  output$scan_distribution <- renderPlotly({
    data <- scan_data()
    last_date <- max(data$scan_date)
    scan_counts <- data[scan_date == last_date, .N, by = scan_symbol]
    
    plot_ly(scan_counts, labels = ~scan_symbol, values = ~N, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            hovertemplate = paste('<b>%{label}</b><br>',
                                  'Count: %{value}<br>',
                                  'Percent: %{percent}<br>',
                                  '<extra></extra>'),
            marker = list(line = list(color = '#fff', width = 2))
    ) %>%
      layout(showlegend = TRUE)
  })
  
  # Recent Activity Summary
  output$recent_activity_summary <- renderUI({
    data <- scan_data()
    last_24h <- data[scan_date >= (Sys.Date() - 1)]
    last_week <- data[scan_date >= (Sys.Date() - 7)]
    
    tagList(
      tags$div(style = "padding: 10px;",
               tags$h5("📊 Activity Overview"),
               tags$hr(),
               tags$p(tags$strong("Last 24h Scans: "), nrow(last_24h)),
               tags$p(tags$strong("Last Week Scans: "), nrow(last_week)),
               tags$p(tags$strong("Active Symbols: "), length(unique(last_week$symbol))),
               tags$p(tags$strong("Scan Types: "), length(unique(last_week$scan_symbol))),
               tags$hr(),
               tags$p(style = "color: #6c757d; font-size: 12px;",
                      paste("Last updated:", format(Sys.time(), "%Y-%m-%d %H:%M"))
               )
      )
    )
  })
  
  # Scan Frequency Analysis
  freq_data <- eventReactive(input$freq_update, {
    req(input$freq_date_range)
    
    data <- scan_data()
    
    data <- data[scan_date >= input$freq_date_range[1] & 
                   scan_date <= input$freq_date_range[2]]
    
    if (!is.null(input$freq_scan_type) && length(input$freq_scan_type) > 0) {
      data <- data[scan_symbol %in% input$freq_scan_type]
    }
    
    unique_dates <- sort(unique(data$scan_date), decreasing = TRUE)
    current_date <- unique_dates[1]
    prior_date <- if(length(unique_dates) >= 2) unique_dates[2] else current_date
    
    freq_summary <- data[, .(
      prior_date_count = sum(scan_date == prior_date),
      current_date_count = sum(scan_date == current_date),
      scan_types = length(unique(scan_symbol[scan_date == current_date])),
      first_seen = min(scan_date),
      last_seen = max(scan_date)
    ), by = symbol][current_date_count >= input$freq_min_appearances][order(-current_date_count)]
    
    return(freq_summary)
  })
  
  output$freq_plot <- renderPlotly({
    data <- freq_data()
    
    if (nrow(data) > 0) {
      top_20 <- head(data, 20)
      top_20[, change := current_date_count - prior_date_count]
      
      plot_ly(top_20, x = ~reorder(symbol, current_date_count), y = ~current_date_count,
              type = 'bar',
              marker = list(
                color = ~change,
                colorscale = list(c(0, 'red'), c(0.5, 'yellow'), c(1, 'green')),
                line = list(color = 'rgb(8,48,107)', width = 1.5)
              ),
              text = ~paste0(symbol, "<br>",
                             "Current: ", current_date_count, "<br>",
                             "Prior: ", prior_date_count, "<br>",
                             "Change: ", change),
              hoverinfo = 'text'
      ) %>%
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Appearances"),
          showlegend = FALSE
        ) %>%
        config(displayModeBar = FALSE)
    }
  })
  
  output$freq_table <- renderDT({
    data <- freq_data()
    
    datatable(data,
              rownames = FALSE,
              class = 'cell-border stripe hover',
              extensions = 'Buttons',
              options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                pageLength = 20,
                scrollX = TRUE,
                order = list(list(1, 'desc'))
              )
    ) %>%
      formatStyle('current_date_count',
                  background = styleColorBar(range(data$current_date_count), '#90caf9'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      )
  })
  
  # Pattern Analysis
  pattern_data <- eventReactive(input$pattern_update, {
    data <- scan_data()
    
    if (!is.null(input$pattern_date_range)) {
      data <- data[scan_date >= input$pattern_date_range[1] & 
                     scan_date <= input$pattern_date_range[2]]
    }
    
    data[, week := format(as.Date(scan_date), "%Y-%W")]
    pattern_summary <- data[, .N, by = .(week, scan_symbol)]
    
    return(list(data = data, summary = pattern_summary))
  })
  
  output$pattern_plot <- renderPlotly({
    pattern_result <- pattern_data()
    data <- pattern_result$summary
    
    if (nrow(data) > 0) {
      if (input$pattern_view == "heatmap") {
        pivot_data <- dcast(data, week ~ scan_symbol, value.var = "N", fill = 0)
        mat <- as.matrix(pivot_data[, -1])
        
        plot_ly(
          x = colnames(mat),
          y = pivot_data$week,
          z = mat,
          type = "heatmap",
          colorscale = list(c(0, "white"), c(0.5, "yellow"), c(1, "red")),
          hovertemplate = paste('<b>Week:</b> %{y}<br>',
                                '<b>Scan:</b> %{x}<br>',
                                '<b>Count:</b> %{z}<br>',
                                '<extra></extra>')
        ) %>%
          layout(
            xaxis = list(title = "Scan Type"),
            yaxis = list(title = "Week")
          )
      } else if (input$pattern_view == "timeseries") {
        raw_data <- pattern_result$data
        plot_data <- raw_data[, .(symbol_count = .N), by = .(scan_symbol, scan_date)]
        
        plot_ly(plot_data, x = ~scan_date, y = ~symbol_count, color = ~scan_symbol,
                type = 'scatter', mode = 'lines+markers',
                line = list(width = 2),
                marker = list(size = 6)
        ) %>%
          layout(
            xaxis = list(title = "Date"),
            yaxis = list(title = "Number of Symbols"),
            hovermode = "x unified"
          )
      } else {
        raw_data <- pattern_result$data
        last_5_dates <- sort(unique(raw_data$scan_date), decreasing = TRUE)[1:5]
        date_counts <- raw_data[scan_date %in% last_5_dates, .N, by = scan_date][order(scan_date)]
        
        plot_ly(date_counts, x = ~scan_date, y = ~N, type = 'bar',
                marker = list(color = '#007bff'),
                text = ~N, textposition = 'outside'
        ) %>%
          layout(
            xaxis = list(title = "Date"),
            yaxis = list(title = "Total Symbols")
          )
      }
    }
  })
  
  output$pattern_summary <- renderDT({
    raw_data <- pattern_data()$data
    recent_dates <- sort(unique(raw_data$scan_date), decreasing = TRUE)[1:4]
    
    result <- raw_data[scan_date %in% recent_dates, .N, by = .(scan_symbol, scan_date)]
    wide_result <- dcast(result, scan_symbol ~ scan_date, value.var = "N", fill = 0)
    
    datatable(wide_result,
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE
    )
  })
  
  # Momentum Tracker
  momentum_data <- eventReactive(input$momentum_update, {
    data <- scan_data()
    weeks_back <- input$momentum_weeks
    
    sorted_dates <- sort(unique(data$scan_date), decreasing = TRUE)
    last_2_dates <- sorted_dates[c(1, min(weeks_back, length(sorted_dates)))]
    
    if (length(last_2_dates) != 2) {
      last_2_dates <- sorted_dates[c(1, 2)]
    }
    
    most_recent_date <- last_2_dates[1]
    second_recent_date <- last_2_dates[2]
    
    recent_freq <- data[scan_date == most_recent_date, .N, by = symbol]
    historical_freq <- data[scan_date == second_recent_date, .N, by = symbol]
    
    momentum <- merge(recent_freq, historical_freq, by = "symbol", all = TRUE)
    momentum[is.na(N.x), N.x := 0]
    momentum[is.na(N.y), N.y := 0]
    
    momentum[, `:=`(
      momentum_score = round(ifelse(N.y > 0, (N.x / N.y) * 100, N.x * 100), 2),
      freq_change = N.x - N.y,
      freq_change_pct = round(ifelse(N.y > 0, ((N.x - N.y) / N.y) * 100, 
                                     ifelse(N.x > 0, 100, 0)), 2)
    )]
    
    momentum[, momentum_type := case_when(
      momentum_score >= 200 & freq_change >= 2 ~ "Heating Up",
      momentum_score >= 120 & freq_change > 0 ~ "Heating Up",
      momentum_score >= 150 & N.y <= 1 ~ "Heating Up",
      momentum_score <= 50 & freq_change <= -2 ~ "Cooling Down",
      momentum_score <= 80 & freq_change < 0 ~ "Cooling Down",
      momentum_score <= 25 & N.y >= 3 ~ "Cooling Down",
      TRUE ~ "Neutral"
    )]
    
    momentum[, momentum_strength := case_when(
      momentum_type == "Heating Up" & momentum_score >= 300 ~ "Very Strong",
      momentum_type == "Heating Up" & momentum_score >= 200 ~ "Strong", 
      momentum_type == "Heating Up" & momentum_score >= 120 ~ "Moderate",
      momentum_type == "Cooling Down" & momentum_score <= 25 ~ "Very Strong",
      momentum_type == "Cooling Down" & momentum_score <= 50 ~ "Strong",
      momentum_type == "Cooling Down" & momentum_score <= 80 ~ "Moderate",
      TRUE ~ "Weak"
    )]
    
    setnames(momentum, c("N.x", "N.y"), c("recent_freq", "historical_freq"))
    
    return(momentum[order(-momentum_score)])
  })
  
  output$momentum_plot <- renderPlotly({
    data <- momentum_data()
    
    if (nrow(data) > 0) {
      filtered_data <- if (input$momentum_type != "both") {
        if (input$momentum_type == "Heating Up") {
          data[momentum_type == input$momentum_type][order(-freq_change)]
        } else {
          data[momentum_type == input$momentum_type][order(freq_change)]
        }
      } else {
        data
      }
      
      top_20 <- head(filtered_data, 20)
      
      plot_ly(top_20, 
              x = ~reorder(symbol, momentum_score), 
              y = ~momentum_score,
              type = 'bar',
              marker = list(
                color = ~momentum_score,
                colorscale = list(c(0, 'red'), c(0.5, 'yellow'), c(1, 'green')),
                line = list(color = 'rgb(8,48,107)', width = 1.5)
              ),
              text = ~paste0(symbol, "<br>",
                             "Score: ", momentum_score, "%<br>",
                             "Change: ", freq_change, "<br>",
                             momentum_strength),
              hoverinfo = 'text'
      ) %>%
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Momentum Score (%)"),
          shapes = list(
            type = 'line',
            x0 = 0, x1 = 1, xref = 'paper',
            y0 = 100, y1 = 100,
            line = list(dash = 'dash', color = 'black', width = 2)
          )
        )
    }
  })
  
  output$momentum_table <- renderDT({
    data <- momentum_data()
    
    if (input$momentum_type == "Heating Up") {
      data <- data[momentum_type == input$momentum_type][order(-freq_change)]
    } else if (input$momentum_type == "Cooling Down") {
      data <- data[momentum_type == input$momentum_type][order(freq_change)]
    }
    
    datatable(data,
              options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE
    ) %>%
      formatStyle('momentum_score',
                  background = styleColorBar(range(data$momentum_score), '#90caf9'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      )
  })
  
  output$hot_symbols <- renderText({
    data <- momentum_data()
    
    if (input$momentum_type == "Heating Up") {
      hot <- head(data[momentum_type == "Heating Up"][order(-momentum_score)], 10)
      if (nrow(hot) > 0) {
        paste("🔥 Hot Symbols:\n", paste(hot$symbol, collapse = ", "))
      } else {
        "No heating up symbols"
      }
    } else if (input$momentum_type == "Cooling Down") {
      cool <- head(data[momentum_type == "Cooling Down"][order(freq_change)], 10)
      if (nrow(cool) > 0) {
        paste("❄️ Cool Symbols:\n", paste(cool$symbol, collapse = ", "))
      } else {
        "No cooling down symbols"
      }
    } else {
      hot <- head(data[momentum_type == "Heating Up"][order(-momentum_score)], 5)
      paste("🔥 Top Movers:\n", paste(hot$symbol, collapse = ", "))
    }
  })
  
  # Portfolio Analysis
  portfolio_data <- eventReactive(input$portfolio_analyze, {
    data <- scan_data()
    symbols <- trimws(unlist(strsplit(input$portfolio_symbols, "\n")))
    symbols <- symbols[symbols != ""]
    
    if (!is.null(input$portfolio_date_range)) {
      data <- data[scan_date >= input$portfolio_date_range[1] & 
                     scan_date <= input$portfolio_date_range[2]]
    }
    
    portfolio_scans <- data[symbol %in% symbols]
    return(list(data = portfolio_scans, symbols = symbols))
  })
  
  output$portfolio_plot <- renderPlotly({
    port_data <- portfolio_data()$data
    
    if (nrow(port_data) > 0) {
      last_2_dates <- sort(unique(port_data$scan_date), decreasing = TRUE)[1:2]
      
      if (length(last_2_dates) >= 2) {
        current_date <- last_2_dates[1]
        last_date <- last_2_dates[2]
        
        current_counts <- port_data[scan_date == current_date, .N, by = symbol]
        last_counts <- port_data[scan_date == last_date, .N, by = symbol]
        
        setnames(current_counts, "N", "current_count")
        setnames(last_counts, "N", "last_count")
        
        all_symbols <- merge(current_counts, last_counts, by = "symbol", all = TRUE)
        all_symbols[is.na(current_count), current_count := 0]
        all_symbols[is.na(last_count), last_count := 0]
        
        plot_ly(all_symbols, x = ~symbol, y = ~last_count, type = 'bar', 
                name = format(as.Date(last_date), "%m-%d"),
                marker = list(color = '#90caf9')) %>%
          add_trace(y = ~current_count, 
                    name = format(as.Date(current_date), "%m-%d"),
                    marker = list(color = '#1976d2')) %>%
          layout(
            barmode = 'group',
            xaxis = list(title = "Symbol"),
            yaxis = list(title = "Number of Scans"),
            hovermode = "x unified"
          )
      }
    }
  })
  
  output$portfolio_table3 <- renderDT({
    port_data <- portfolio_data()$data
    
    if (nrow(port_data) > 0) {
      weekly_dates <- sort(unique(port_data$scan_date), decreasing = TRUE)[1:2]
      
      if (length(weekly_dates) >= 2) {
        this_week_date <- weekly_dates[1]
        last_week_date <- weekly_dates[2]
        
        this_week_scans <- port_data[scan_date == this_week_date, 
                                     .(scan_types = paste(unique(scan_symbol), collapse = ", "),
                                       scan_count = .N), 
                                     by = symbol]
        setnames(this_week_scans, c("scan_types", "scan_count"), 
                 c("this_week_scans", "this_week_count"))
        
        last_week_scans <- port_data[scan_date == last_week_date, 
                                     .(scan_types = paste(unique(scan_symbol), collapse = ", "),
                                       scan_count = .N), 
                                     by = symbol]
        setnames(last_week_scans, c("scan_types", "scan_count"), 
                 c("last_week_scans", "last_week_count"))
        
        weekly_comparison <- merge(this_week_scans, last_week_scans, by = "symbol", all = TRUE)
        weekly_comparison[is.na(this_week_scans), this_week_scans := "None"]
        weekly_comparison[is.na(last_week_scans), last_week_scans := "None"]
        weekly_comparison[is.na(this_week_count), this_week_count := 0]
        weekly_comparison[is.na(last_week_count), last_week_count := 0]
        
        weekly_comparison[, `:=`(
          change = this_week_count - last_week_count,
          status = case_when(
            this_week_count > last_week_count ~ "📈 Increased",
            this_week_count < last_week_count ~ "📉 Decreased",
            this_week_count == last_week_count & this_week_count > 0 ~ "➡️ Same",
            this_week_count == 0 & last_week_count == 0 ~ "❌ No Activity",
            this_week_count > 0 & last_week_count == 0 ~ "🆕 New Entry",
            this_week_count == 0 & last_week_count > 0 ~ "🚫 Dropped Out"
          )
        )]
        
        datatable(weekly_comparison,
                  options = list(pageLength = 15, scrollX = TRUE),
                  rownames = FALSE
        ) %>%
          formatStyle('status',
                      backgroundColor = styleEqual(
                        c('📈 Increased', '📉 Decreased', '🆕 New Entry', '🚫 Dropped Out', '➡️ Same', '❌ No Activity'),
                        c('#d4edda', '#f8d7da', '#cce5ff', '#ffcccc', '#f0f0f0', '#e6e6e6')
                      )
          ) %>%
          formatStyle('change',
                      color = styleInterval(c(-0.1, 0.1), c('red', 'black', 'green')),
                      fontWeight = 'bold'
          )
      }
    }
  })
  
  output$portfolio_metrics <- renderText({
    port_data <- portfolio_data()
    symbols <- port_data$symbols
    scans <- port_data$data
    
    coverage <- length(unique(scans$symbol)) / length(symbols) * 100
    avg_scans <- if (length(symbols) > 0) nrow(scans) / length(symbols) else 0
    
    paste0(
      "📊 Portfolio Coverage: ", round(coverage, 1), "%\n",
      "📈 Average Scans per Symbol: ", round(avg_scans, 1), "\n",
      "🔢 Total Portfolio Scans: ", nrow(scans), "\n",
      "📅 Date Range: ", 
      if (nrow(scans) > 0) paste(min(scans$scan_date), "to", max(scans$scan_date)) else "N/A"
    )
  })
  
  # Signal Strength
  output$weight_inputs <- renderUI({
    data <- scan_data()
    scan_types <- unique(data$scan_symbol)
    
    tagList(
      lapply(scan_types, function(scan_type) {
        div(style = "margin-bottom: 15px;",
            fluidRow(
              column(8,
                     tags$label(scan_type, style = "font-weight: 500;")
              ),
              column(4,
                     numericInput(
                       paste0("weight_", scan_type),
                       label = NULL,
                       value = 0.50,
                       min = 0,
                       max = 1,
                       step = 0.05,
                       width = "100%"
                     )
              )
            )
        )
      })
    )
  })
  
  signal_data <- eventReactive(input$signal_calculate, {
    data <- scan_data()
    scan_types <- unique(data$scan_symbol)
    max_date <- max(data$scan_date)
    recent_dates <- sort(unique(data$scan_date), decreasing = TRUE)[1:min(5, length(unique(data$scan_date)))]
    
    weights <- sapply(scan_types, function(st) {
      weight_input <- paste0("weight_", st)
      if (!is.null(input[[weight_input]])) input[[weight_input]] else 0.50
    })
    
    sophisticated_scores <- data[scan_date %in% recent_dates, {
      current_day <- .SD[scan_date == max_date]
      
      if (nrow(current_day) > 0) {
        base_weighted_score <- sum(weights[current_day$scan_symbol])
        diversity_count <- length(unique(current_day$scan_symbol))
        diversity_bonus <- diversity_count * 0.15 * base_weighted_score
        
        scan_frequencies <- table(current_day$scan_symbol)
        frequency_multiplier <- 1 + sum(pmax(0, scan_frequencies - 1) * 0.1)
        
        all_dates <- sort(unique(.SD$scan_date))
        if (length(all_dates) >= 2) {
          recent_activity <- sum(.SD$scan_date >= all_dates[max(1, length(all_dates)-2)])
          historical_activity <- sum(.SD$scan_date < all_dates[max(1, length(all_dates)-2)])
          momentum_factor <- ifelse(historical_activity > 0, 
                                    1 + (recent_activity - historical_activity) / historical_activity * 0.2,
                                    1 + recent_activity * 0.1)
        } else {
          momentum_factor <- 1
        }
        
        unique_recent_dates <- length(unique(.SD$scan_date))
        consistency_multiplier <- 1 + (unique_recent_dates - 1) * 0.05
        
        sophisticated_score <- (
          (base_weighted_score + diversity_bonus) * 
            frequency_multiplier * 
            momentum_factor * 
            consistency_multiplier
        )
        
        list(
          sophisticated_score = as.numeric(round(sophisticated_score, 2)),
          base_weighted_score = as.numeric(round(base_weighted_score, 2)),
          diversity_score = as.numeric(round(diversity_bonus, 2)),
          frequency_mult = as.numeric(round(frequency_multiplier, 3)),
          momentum_factor = as.numeric(round(momentum_factor, 3)),
          consistency_mult = as.numeric(round(consistency_multiplier, 3)),
          total_scans_today = as.integer(nrow(current_day)),
          unique_scan_types = as.integer(diversity_count),
          confidence_level = as.character(case_when(
            sophisticated_score >= base_weighted_score * 1.5 ~ "Very High",
            sophisticated_score >= base_weighted_score * 1.2 ~ "High",
            sophisticated_score >= base_weighted_score * 0.8 ~ "Medium",
            TRUE ~ "Low"
          ))
        )
      } else {
        list(
          sophisticated_score = 0.0,
          base_weighted_score = 0.0,
          diversity_score = 0.0,
          frequency_mult = 1.0,
          momentum_factor = 1.0,
          consistency_mult = 1.0,
          total_scans_today = 0L,
          unique_scan_types = 0L,
          confidence_level = "None"
        )
      }
    }, by = symbol][order(-sophisticated_score)]
    
    return(sophisticated_scores)
  })
  
  output$signal_plot <- renderPlotly({
    data <- signal_data()
    
    if (nrow(data) > 0) {
      top_20 <- head(data, 20)
      
      plot_ly(top_20, 
              x = ~reorder(symbol, base_weighted_score), 
              y = ~base_weighted_score,
              type = 'bar',
              marker = list(color = '#7b1fa2'),
              text = ~paste0(symbol, "<br>",
                             "Score: ", base_weighted_score, "<br>",
                             "Confidence: ", confidence_level),
              hoverinfo = 'text'
      ) %>%
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Weighted Score")
        )
    }
  })
  
  output$signal_table <- renderDT({
    data <- signal_data()
    strong_signals <- data[base_weighted_score >= input$signal_min_score]
    
    datatable(strong_signals,
              options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE
    ) %>%
      formatStyle('confidence_level',
                  backgroundColor = styleEqual(
                    c('Very High', 'High', 'Medium', 'Low'),
                    c('#c8e6c9', '#fff9c4', '#ffccbc', '#ffebee')
                  )
      )
  })
  
  # Alert System
  alert_data <- eventReactive(input$alert_setup, {
    data <- scan_data()
    alerts <- data.table()
    
    recent_data <- data[scan_date >= (Sys.Date() - 5)]
    
    if (!is.null(input$alert_symbols) && length(input$alert_symbols) > 0) {
      symbol_freq <- recent_data[symbol %in% input$alert_symbols, .N, by = symbol]
      freq_alerts <- symbol_freq[N >= input$alert_frequency_threshold]
      
      if (nrow(freq_alerts) > 0) {
        freq_alert_data <- data.table(
          timestamp = Sys.time(),
          symbol = freq_alerts$symbol,
          alert_type = "Frequency Threshold",
          details = paste("Appeared", freq_alerts$N, "times this week"),
          scan_count = freq_alerts$N,
          priority = ifelse(freq_alerts$N >= input$alert_frequency_threshold * 2, "High", "Medium")
        )
        alerts <- rbind(alerts, freq_alert_data)
      }
    }
    
    if (!is.null(input$alert_scan_types) && length(input$alert_scan_types) > 0) {
      last_week <- recent_data[scan_symbol %in% input$alert_scan_types]
      previous_data <- data[scan_date < (Sys.Date() - 7) & scan_date >= (Sys.Date() - 30)]
      previous_symbols <- unique(previous_data[scan_symbol %in% input$alert_scan_types]$symbol)
      
      new_symbols <- setdiff(unique(last_week$symbol), previous_symbols)
      
      if (length(new_symbols) > 0) {
        new_symbol_alerts <- data.table(
          timestamp = Sys.time(),
          symbol = new_symbols,
          alert_type = "New Symbol",
          details = paste("First appearance in", paste(input$alert_scan_types, collapse = "/")),
          scan_count = sapply(new_symbols, function(s) sum(last_week$symbol == s)),
          priority = "High"
        )
        alerts <- rbind(alerts, new_symbol_alerts)
      }
    }
    
    if (input$alert_new_symbols) {
      normal_period <- data[scan_date >= (Sys.Date() - 30) & scan_date < (Sys.Date() - 7)]
      recent_period <- data[scan_date >= (Sys.Date() - 7)]
      
      if (nrow(normal_period) > 0 && nrow(recent_period) > 0) {
        normal_freq <- normal_period[, .(avg_weekly = .N / 3), by = symbol]
        recent_freq <- recent_period[, .N, by = symbol]
        
        activity_comparison <- merge(recent_freq, normal_freq, by = "symbol", all.x = TRUE)
        activity_comparison[is.na(avg_weekly), avg_weekly := 0.5]
        activity_comparison[, activity_ratio := N / avg_weekly]
        
        unusual_activity <- activity_comparison[activity_ratio >= 3 & N >= 2]
        
        if (nrow(unusual_activity) > 0) {
          activity_alerts <- data.table(
            timestamp = Sys.time(),
            symbol = unusual_activity$symbol,
            alert_type = "Unusual Activity",
            details = paste("Activity", round(unusual_activity$activity_ratio, 1), "x normal"),
            scan_count = unusual_activity$N,
            priority = ifelse(unusual_activity$activity_ratio >= 5, "High", "Medium")
          )
          alerts <- rbind(alerts, activity_alerts)
        }
      }
    }
    
    if (nrow(alerts) > 0) {
      alerts[, priority_order := ifelse(priority == "High", 1, 2)]
      alerts <- alerts[order(priority_order, -scan_count)]
    }
    
    return(alerts)
  })
  
  output$alert_table <- renderDT({
    alerts <- alert_data()
    
    if (nrow(alerts) > 0) {
      display_alerts <- alerts[, .(
        Timestamp = format(timestamp, "%Y-%m-%d %H:%M"),
        Symbol = symbol,
        Alert_Type = alert_type,
        Details = details,
        Scan_Count = scan_count,
        Priority = priority
      )]
      
      datatable(display_alerts,
                options = list(pageLength = 15, order = list(list(5, 'desc'))),
                rownames = FALSE
      ) %>%
        formatStyle('Priority',
                    backgroundColor = styleEqual(c('High', 'Medium'), 
                                                 c('#ffcccc', '#ffffcc'))
        )
    } else {
      data <- scan_data()
      recent_activity <- data[scan_date >= (Sys.Date() - 7)][order(-scan_date)]
      
      if (nrow(recent_activity) > 0) {
        recent_display <- recent_activity[, .(
          Date = scan_date,
          Symbol = symbol,
          Scan_Type = scan_symbol,
          Status = "Recent Activity"
        )]
        
        datatable(head(recent_display, 50),
                  options = list(pageLength = 15),
                  caption = "Recent Scan Activity (No Active Alerts)"
        )
      }
    }
  })
  
  output$alert_timeline <- renderPlotly({
    data <- scan_data()
    timeline <- data[scan_date >= (Sys.Date() - 30), .N, by = scan_date][order(scan_date)]
    
    if (nrow(timeline) > 0) {
      plot_ly(timeline, x = ~scan_date, y = ~N, type = 'scatter', mode = 'lines+markers',
              line = list(color = '#007bff', width = 2),
              marker = list(size = 8, color = '#007bff')
      ) %>%
        layout(
          title = "Daily Scan Activity (Last 30 Days)",
          xaxis = list(title = "Date"),
          yaxis = list(title = "Number of Scans")
        )
    }
  })
  
  output$active_alerts <- renderText({
    alerts <- alert_data()
    data <- scan_data()
    last_24h <- data[scan_date >= (Sys.Date() - 1)]
    last_week <- data[scan_date >= (Sys.Date() - 7)]
    
    if (nrow(alerts) > 0) {
      high_priority <- sum(alerts$priority == "High")
      medium_priority <- sum(alerts$priority == "Medium")
      
      alert_summary <- paste0(
        "🚨 ACTIVE ALERTS:\n",
        "High Priority: ", high_priority, "\n",
        "Medium Priority: ", medium_priority, "\n",
        "Total Alerts: ", nrow(alerts), "\n\n"
      )
    } else {
      alert_summary <- "✅ No Active Alerts\n\n"
    }
    
    recent_summary <- paste0(
      "📊 RECENT ACTIVITY:\n",
      "Last 24h Scans: ", nrow(last_24h), "\n",
      "Last Week Scans: ", nrow(last_week), "\n",
      "Active Symbols: ", length(unique(last_week$symbol)), "\n",
      "Scan Types: ", length(unique(last_week$scan_symbol))
    )
    
    paste(alert_summary, recent_summary)
  })
  
  # ============================================================================
  # SCAN EFFECTIVENESS WITH PRICE PERFORMANCE
  # ============================================================================
  
  # Helper function to safely get stock prices
  get_stock_prices <- function(symbol, start_date, end_date) {
    tryCatch({
      prices <- tq_get(symbol, 
                       get = "stock.prices",
                       from = as.Date(start_date)+1,
                       to = end_date)
      if (nrow(prices) > 0) {
        return(prices)
      } else {
        return(NULL)
      }
    }, error = function(e) {
      return(NULL)
    })
  }
  
  # Main effectiveness analysis with price data
  effectiveness_analysis <- eventReactive(input$effectiveness_analyze, {
    withProgress(message = 'Analyzing scan effectiveness...', value = 0, {
      
      incProgress(0.1, detail = "Loading scan data")
      
      data <- scan_data()
      
      # Filter by date range
      if (!is.null(input$effectiveness_date_range)) {
        data <- data[scan_date >= input$effectiveness_date_range[1] & 
                       scan_date <= input$effectiveness_date_range[2]]
      }
      
      # Filter by scan types
      if (!is.null(input$effectiveness_scan_filter) && 
          length(input$effectiveness_scan_filter) > 0) {
        data <- data[scan_symbol %in% input$effectiveness_scan_filter]
      }
      
      # Get symbol-level data with first/last scan dates
      incProgress(0.2, detail = "Calculating scan metrics")
      
      symbol_scans <- data[, .(
        scan_types = paste(unique(scan_symbol), collapse = ", "),
        scan_count = .N,
        first_scan_date = min(scan_date),
        last_scan_date = max(scan_date),
        days_in_scans = as.numeric(max(scan_date) - min(scan_date)) + 1
      ), by = symbol]
      
      # Filter by minimum appearances
      symbol_scans <- symbol_scans[scan_count >= input$effectiveness_min_appearances]
      
      if (nrow(symbol_scans) == 0) {
        showNotification("No symbols meet the minimum appearance criteria", 
                         type = "warning")
        return(NULL)
      }
      
      incProgress(0.3, detail = paste("Fetching prices for", nrow(symbol_scans), "symbols"))
      
      # Fetch price data for each symbol
      price_results <- list()
      total_symbols <- nrow(symbol_scans)
      
      for (i in 1:total_symbols) {
        symbol <- symbol_scans$symbol[i]
        first_date <- symbol_scans$first_scan_date[i]
        last_date <- symbol_scans$last_scan_date[i]
        
        # Add lookforward period
        end_date <- min(last_date + input$effectiveness_lookforward_days, Sys.Date())
        
        # Update progress
        if (i %% 5 == 0) {
          incProgress(0.5 / total_symbols, 
                      detail = paste("Fetching prices:", i, "/", total_symbols))
        }
        
        prices <- get_stock_prices(symbol, first_date, end_date)
        
        if (!is.null(prices) && nrow(prices) > 0) {
          # Get price at first scan
          first_price <- prices$close[prices$date == first_date]
          if (length(first_price) == 0) {
            first_price <- prices$close[1]
          }
          
          # Get price at last scan (or lookforward end)
          last_price_date <- min(last_date + input$effectiveness_lookforward_days, 
                                 max(prices$date))
          last_price <- prices$close[prices$date == last_price_date]
          if (length(last_price) == 0) {
            last_price <- tail(prices$close, 1)
          }
          
          # Calculate metrics
          if (length(first_price) > 0 && length(last_price) > 0 && first_price > 0) {
            pct_change <- ((last_price - first_price) / first_price) * 100
            
            # Find highest price in the period
            max_price <- max(prices$close, na.rm = TRUE)
            max_pct_gain <- ((max_price - first_price) / first_price) * 100
            
            # Find lowest price for drawdown
            min_price <- min(prices$close, na.rm = TRUE)
            max_drawdown <- ((min_price - first_price) / first_price) * 100
            
            price_results[[symbol]] <- list(
              symbol = symbol,
              first_price = first_price,
              last_price = last_price,
              max_price = max_price,
              min_price = min_price,
              pct_change = pct_change,
              max_pct_gain = max_pct_gain,
              max_drawdown = max_drawdown,
              price_data_available = TRUE
            )
          }
        }
      }
      
      incProgress(0.8, detail = "Combining results")
      
      # Convert to data table
      if (length(price_results) > 0) {
        price_dt <- rbindlist(lapply(price_results, as.data.table))
        
        # Merge with scan data
        results <- merge(symbol_scans, price_dt, by = "symbol", all.x = TRUE)
        
        # Add scan-level aggregations
        scan_performance <- data[symbol %in% results$symbol, 
                                 .(symbols = .N), 
                                 by = scan_symbol]
        
        # For each scan type, calculate performance metrics
        scan_metrics <- data[symbol %in% results$symbol, {
          scan_symbols <- unique(symbol)
          perf_data <- results[symbol %in% scan_symbols & price_data_available == TRUE]
          
          if (nrow(perf_data) > 0) {
            list(
              total_symbols = length(scan_symbols),
              symbols_with_price_data = nrow(perf_data),
              avg_return = mean(perf_data$pct_change, na.rm = TRUE),
              median_return = median(perf_data$pct_change, na.rm = TRUE),
              avg_max_gain = mean(perf_data$max_pct_gain, na.rm = TRUE),
              win_rate = sum(perf_data$pct_change > 0, na.rm = TRUE) / nrow(perf_data) * 100,
              avg_winner = mean(perf_data$pct_change[perf_data$pct_change > 0], na.rm = TRUE),
              avg_loser = mean(perf_data$pct_change[perf_data$pct_change < 0], na.rm = TRUE),
              best_performer = perf_data$symbol[which.max(perf_data$pct_change)],
              best_return = max(perf_data$pct_change, na.rm = TRUE),
              worst_performer = perf_data$symbol[which.min(perf_data$pct_change)],
              worst_return = min(perf_data$pct_change, na.rm = TRUE),
              sharpe_ratio = ifelse(sd(perf_data$pct_change, na.rm = TRUE) > 0,
                                    mean(perf_data$pct_change, na.rm = TRUE) / 
                                      sd(perf_data$pct_change, na.rm = TRUE), 0)
            )
          } else {
            list(
              total_symbols = length(scan_symbols),
              symbols_with_price_data = 0,
              avg_return = NA,
              median_return = NA,
              avg_max_gain = NA,
              win_rate = NA,
              avg_winner = NA,
              avg_loser = NA,
              best_performer = NA,
              best_return = NA,
              worst_performer = NA,
              worst_return = NA,
              sharpe_ratio = NA
            )
          }
        }, by = scan_symbol]
        
        incProgress(1, detail = "Complete!")
        
        return(list(
          symbol_results = results,
          scan_metrics = scan_metrics,
          analysis_params = list(
            date_range = input$effectiveness_date_range,
            lookforward_days = input$effectiveness_lookforward_days,
            min_appearances = input$effectiveness_min_appearances
          )
        ))
      } else {
        showNotification("Could not fetch price data for any symbols", 
                         type = "error")
        return(NULL)
      }
    })
  })
  
  # Quick Stats
  output$effectiveness_quick_stats <- renderText({
    results <- effectiveness_analysis()
    
    if (is.null(results)) {
      return("Click 'Analyze Effectiveness' to begin.\n\nNote: Analysis requires symbols with price data available.")
    }
    
    symbol_results <- results$symbol_results
    scan_metrics <- results$scan_metrics
    
    total_symbols <- nrow(symbol_results)
    with_price_data <- sum(symbol_results$price_data_available == TRUE, na.rm = TRUE)
    
    avg_return_all <- mean(symbol_results$pct_change, na.rm = TRUE)
    winners <- sum(symbol_results$pct_change > 0, na.rm = TRUE)
    win_rate <- winners / with_price_data * 100
    
    paste0(
      "📊 ANALYSIS SUMMARY:\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
      "Total Symbols: ", total_symbols, "\n",
      "With Price Data: ", with_price_data, "\n",
      "Coverage: ", round(with_price_data/total_symbols*100, 1), "%\n\n",
      
      "💰 PERFORMANCE:\n",
      "Avg Return: ", sprintf("%.2f%%", avg_return_all), "\n",
      "Win Rate: ", sprintf("%.1f%%", win_rate), "\n",
      "Winners: ", winners, " | Losers: ", with_price_data - winners, "\n\n",
      
      "🎯 BEST SCAN:\n",
      scan_metrics$scan_symbol[which.max(scan_metrics$avg_return)], "\n",
      "Avg Return: ", sprintf("%.2f%%", 
                              max(scan_metrics$avg_return, na.rm = TRUE))
    )
  })
  
  # Scan Win Rate Chart
  output$scan_winrate_chart <- renderPlotly({
    results <- effectiveness_analysis()
    if (is.null(results)) return(NULL)
    
    scan_metrics <- results$scan_metrics
    scan_metrics <- scan_metrics[!is.na(win_rate)]
    
    plot_ly(scan_metrics, x = ~reorder(scan_symbol, win_rate), y = ~win_rate,
            type = 'bar',
            marker = list(
              color = ~win_rate,
              colorscale = list(c(0, 'red'), c(0.5, 'yellow'), c(1, 'green')),
              line = list(color = 'rgb(8,48,107)', width = 1.5)
            ),
            text = ~paste0(scan_symbol, "<br>",
                           "Win Rate: ", round(win_rate, 1), "%<br>",
                           "Symbols: ", symbols_with_price_data),
            hoverinfo = 'text'
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Win Rate (%)", range = c(0, 100)),
        showlegend = FALSE
      )
  })
  
  # Average Return Chart
  output$scan_avgreturn_chart <- renderPlotly({
    results <- effectiveness_analysis()
    if (is.null(results)) return(NULL)
    
    scan_metrics <- results$scan_metrics
    scan_metrics <- scan_metrics[!is.na(avg_return)]
    
    plot_ly(scan_metrics, x = ~reorder(scan_symbol, avg_return), y = ~avg_return,
            type = 'bar',
            marker = list(
              color = ~avg_return,
              colorscale = list(c(0, 'red'), c(0.5, 'yellow'), c(1, 'green')),
              line = list(color = 'rgb(8,48,107)', width = 1.5)
            ),
            text = ~paste0(scan_symbol, "<br>",
                           "Avg Return: ", sprintf("%.2f%%", avg_return), "<br>",
                           "Max Gain: ", sprintf("%.2f%%", avg_max_gain)),
            hoverinfo = 'text'
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Average Return (%)"),
        showlegend = FALSE
      )
  })
  
  # Performance Summary Table
  output$effectiveness_summary_table <- renderDT({
    results <- effectiveness_analysis()
    if (is.null(results)) return(NULL)
    
    scan_metrics <- results$scan_metrics
    scan_metrics <- scan_metrics[!is.na(avg_return)]
    
    display_table <- scan_metrics[, .(
      Scan_Type = scan_symbol,
      Symbols = symbols_with_price_data,
      Avg_Return = sprintf("%.2f%%", avg_return),
      Median_Return = sprintf("%.2f%%", median_return),
      Win_Rate = sprintf("%.1f%%", win_rate),
      Avg_Winner = sprintf("%.2f%%", avg_winner),
      Avg_Loser = sprintf("%.2f%%", avg_loser),
      Avg_Max_Gain = sprintf("%.2f%%", avg_max_gain),
      Best_Symbol = best_performer,
      Best_Return = sprintf("%.2f%%", best_return),
      Sharpe = sprintf("%.2f", sharpe_ratio)
    )][order(-Avg_Return)]
    
    datatable(display_table,
              rownames = FALSE,
              extensions = 'Buttons',
              options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                pageLength = 15,
                scrollX = TRUE
              )
    ) %>%
      formatStyle('Win_Rate',
                  background = styleColorBar(c(0, 100), '#90caf9'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      )
  })
  
  # Top Symbols Chart
  output$top_symbols_chart <- renderPlotly({
    results <- effectiveness_analysis()
    if (is.null(results)) return(NULL)
    
    symbol_results <- results$symbol_results
    symbol_results <- symbol_results[price_data_available == TRUE]
    
    # Top 20 by return
    top_20 <- head(symbol_results[order(-pct_change)], 20)
    
    plot_ly(top_20, x = ~reorder(symbol, pct_change), y = ~pct_change,
            type = 'bar',
            marker = list(
              color = ~pct_change,
              colorscale = list(c(0, 'red'), c(0.5, 'yellow'), c(1, 'green'))
            ),
            text = ~paste0(symbol, "<br>",
                           "Return: ", sprintf("%.2f%%", pct_change), "<br>",
                           "Max Gain: ", sprintf("%.2f%%", max_pct_gain), "<br>",
                           "Scans: ", scan_types),
            hoverinfo = 'text'
    ) %>%
      layout(
        title = "Top 20 Performing Symbols",
        xaxis = list(title = ""),
        yaxis = list(title = "Return (%)"),
        showlegend = FALSE
      )
  })
  
  # Symbol Performance Table
  output$symbol_performance_table <- renderDT({
    results <- effectiveness_analysis()
    if (is.null(results)) return(NULL)
    
    symbol_results <- results$symbol_results
    symbol_results <- symbol_results[price_data_available == TRUE]
    
    display_table <- symbol_results[, .(
      Symbol = symbol,
      Scan_Types = scan_types,
      Scan_Count = scan_count,
      First_Price = sprintf("$%.2f", first_price),
      Last_Price = sprintf("$%.2f", last_price),
      Return = sprintf("%.2f%%", pct_change),
      Max_Gain = sprintf("%.2f%%", max_pct_gain),
      Max_Drawdown = sprintf("%.2f%%", max_drawdown),
      First_Seen = as.character(first_scan_date),
      Last_Seen = as.character(last_scan_date),
      Days_Active = days_in_scans
    )][order(-Return)]
    
    datatable(display_table,
              rownames = FALSE,
              extensions = 'Buttons',
              options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                pageLength = 25,
                scrollX = TRUE
              )
    ) %>%
      formatStyle('Return',
                  color = styleInterval(0, c('red', 'green')),
                  fontWeight = 'bold'
      ) %>%
      formatStyle('Max_Gain',
                  background = styleColorBar(c(0, max(symbol_results$max_pct_gain, na.rm = TRUE)), 
                                             '#90caf9'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      )
  })
  
  # Max Gain Distribution
  output$max_gain_distribution <- renderPlotly({
    results <- effectiveness_analysis()
    if (is.null(results)) return(NULL)
    
    symbol_results <- results$symbol_results
    symbol_results <- symbol_results[price_data_available == TRUE]
    
    plot_ly(symbol_results, x = ~max_pct_gain, type = 'histogram',
            marker = list(color = '#1976d2'),
            nbinsx = 30
    ) %>%
      layout(
        title = "Distribution of Maximum Gains",
        xaxis = list(title = "Max Gain (%)"),
        yaxis = list(title = "Number of Symbols")
      )
  })
  
  # Return vs Risk Scatter
  output$return_risk_scatter <- renderPlotly({
    results <- effectiveness_analysis()
    if (is.null(results)) return(NULL)
    
    scan_metrics <- results$scan_metrics
    scan_metrics <- scan_metrics[!is.na(avg_return) & !is.na(sharpe_ratio)]
    
    plot_ly(scan_metrics, 
            x = ~avg_return, 
            y = ~win_rate,
            type = 'scatter',
            mode = 'markers+text',
            text = ~scan_symbol,
            textposition = 'top center',
            marker = list(
              size = ~symbols_with_price_data * 2,
              color = ~avg_max_gain,
              colorscale = 'Viridis',
              showscale = TRUE,
              colorbar = list(title = "Avg Max Gain (%)")
            )
    ) %>%
      layout(
        title = "Return vs Win Rate (Bubble Size = Symbol Count)",
        xaxis = list(title = "Average Return (%)"),
        yaxis = list(title = "Win Rate (%)")
      )
  })
  
  # Scan Correlation Heatmap
  output$scan_correlation_heatmap <- renderPlotly({
    results <- effectiveness_analysis()
    if (is.null(results)) return(NULL)
    
    data <- scan_data()
    symbol_results <- results$symbol_results
    
    # Filter to symbols in results
    data <- data[symbol %in% symbol_results$symbol]
    
    # Create symbol-scan matrix
    scan_matrix <- dcast(data[, .N, by = .(symbol, scan_symbol)], 
                         symbol ~ scan_symbol, value.var = "N", fill = 0)
    
    # Convert to binary (appeared or not)
    scan_cols <- names(scan_matrix)[names(scan_matrix) != "symbol"]
    for (col in scan_cols) {
      scan_matrix[[col]] <- ifelse(scan_matrix[[col]] > 0, 1, 0)
    }
    
    # Calculate correlation
    cor_matrix <- cor(scan_matrix[, ..scan_cols], use = "pairwise.complete.obs")
    
    plot_ly(
      x = colnames(cor_matrix),
      y = rownames(cor_matrix),
      z = cor_matrix,
      type = "heatmap",
      colorscale = list(c(0, "white"), c(0.5, "yellow"), c(1, "red")),
      hovertemplate = paste('<b>Scan 1:</b> %{y}<br>',
                            '<b>Scan 2:</b> %{x}<br>',
                            '<b>Correlation:</b> %{z:.2f}<br>',
                            '<extra></extra>')
    ) %>%
      layout(
        title = "Scan Type Symbol Overlap Correlation",
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "")
      )
  })
  
  # Effectiveness Insights
  output$effectiveness_insights <- renderText({
    results <- effectiveness_analysis()
    if (is.null(results)) {
      return("Run analysis to see insights.")
    }
    
    scan_metrics <- results$scan_metrics
    symbol_results <- results$symbol_results[price_data_available == TRUE]
    params <- results$analysis_params
    
    best_scan <- scan_metrics[which.max(avg_return)]
    worst_scan <- scan_metrics[which.min(avg_return)]
    most_consistent <- scan_metrics[which.max(win_rate)]
    best_sharpe <- scan_metrics[which.max(sharpe_ratio)]
    
    best_symbol <- symbol_results[which.max(pct_change)]
    worst_symbol <- symbol_results[which.min(pct_change)]
    
    avg_return <- mean(symbol_results$pct_change, na.rm = TRUE)
    median_return <- median(symbol_results$pct_change, na.rm = TRUE)
    avg_max_gain <- mean(symbol_results$max_pct_gain, na.rm = TRUE)
    
    paste0(
      "🎯 SCAN EFFECTIVENESS ANALYSIS\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n",
      
      "📅 ANALYSIS PARAMETERS:\n",
      "   Date Range: ", format(params$date_range[1], "%Y-%m-%d"), " to ", 
      format(params$date_range[2], "%Y-%m-%d"), "\n",
      "   Lookforward Period: ", params$lookforward_days, " days\n",
      "   Min Appearances: ", params$min_appearances, "\n",
      "   Symbols Analyzed: ", nrow(symbol_results), "\n\n",
      
      "🏆 BEST PERFORMING SCAN:\n",
      "   ", best_scan$scan_symbol, "\n",
      "   • Average Return: ", sprintf("%.2f%%", best_scan$avg_return), "\n",
      "   • Win Rate: ", sprintf("%.1f%%", best_scan$win_rate), "\n",
      "   • Avg Max Gain: ", sprintf("%.2f%%", best_scan$avg_max_gain), "\n",
      "   • Sharpe Ratio: ", sprintf("%.2f", best_scan$sharpe_ratio), "\n",
      "   • Best Symbol: ", best_scan$best_performer, 
      " (", sprintf("%.2f%%", best_scan$best_return), ")\n\n",
      
      "📉 NEEDS IMPROVEMENT:\n",
      "   ", worst_scan$scan_symbol, "\n",
      "   • Average Return: ", sprintf("%.2f%%", worst_scan$avg_return), "\n",
      "   • Win Rate: ", sprintf("%.1f%%", worst_scan$win_rate), "\n\n",
      
      "🎲 MOST CONSISTENT:\n",
      "   ", most_consistent$scan_symbol, " - ", 
      sprintf("%.1f%%", most_consistent$win_rate), " win rate\n\n",
      
      "📊 OVERALL STATISTICS:\n",
      "   • Average Return: ", sprintf("%.2f%%", avg_return), "\n",
      "   • Median Return: ", sprintf("%.2f%%", median_return), "\n",
      "   • Avg Max Gain: ", sprintf("%.2f%%", avg_max_gain), "\n",
      "   • Positive Returns: ", sum(symbol_results$pct_change > 0), "/", 
      nrow(symbol_results), "\n\n",
      
      "⭐ TOP PERFORMER:\n",
      "   ", best_symbol$symbol, " - ", sprintf("%.2f%%", best_symbol$pct_change), "\n",
      "   • Max Gain: ", sprintf("%.2f%%", best_symbol$max_pct_gain), "\n",
      "   • Scan Types: ", best_symbol$scan_types, "\n",
      "   • First Seen: ", format(best_symbol$first_scan_date, "%Y-%m-%d"), "\n\n",
      
      "💡 KEY INSIGHTS:\n",
      if (avg_return > 5) {
        "   ✓ Strong overall performance - scans identifying quality symbols\n"
      } else if (avg_return > 0) {
        "   • Positive returns but room for improvement\n"
      } else {
        "   ✗ Negative average return - review scan criteria\n"
      },
      
      if (best_scan$win_rate > 60) {
        paste0("   ✓ ", best_scan$scan_symbol, " shows excellent consistency\n")
      } else "",
      
      if (avg_max_gain > avg_return * 2) {
        "   • High max gains suggest good entry timing opportunities\n"
      } else "",
      
      if (best_sharpe$sharpe_ratio > 1) {
        paste0("   ✓ ", best_sharpe$scan_symbol, " has best risk-adjusted returns\n")
      } else ""
    )
  })
  
  # Best Scan Metrics
  output$best_scan_metrics <- renderUI({
    results <- effectiveness_analysis()
    if (is.null(results)) return(NULL)
    
    scan_metrics <- results$scan_metrics
    
    best_return <- scan_metrics[which.max(avg_return)]
    best_winrate <- scan_metrics[which.max(win_rate)]
    best_sharpe <- scan_metrics[which.max(sharpe_ratio)]
    
    tagList(
      tags$div(style = "padding: 10px;",
               tags$h5("💰 Best Average Return"),
               tags$p(tags$strong(best_return$scan_symbol)),
               tags$p(sprintf("%.2f%%", best_return$avg_return)),
               tags$hr(),
               
               tags$h5("🎯 Best Win Rate"),
               tags$p(tags$strong(best_winrate$scan_symbol)),
               tags$p(sprintf("%.1f%%", best_winrate$win_rate)),
               tags$hr(),
               
               tags$h5("📊 Best Risk-Adjusted"),
               tags$p(tags$strong(best_sharpe$scan_symbol)),
               tags$p(paste("Sharpe:", sprintf("%.2f", best_sharpe$sharpe_ratio)))
      )
    )
  })
  
  # Improvement Opportunities
  output$improvement_opportunities <- renderUI({
    results <- effectiveness_analysis()
    if (is.null(results)) return(NULL)
    
    scan_metrics <- results$scan_metrics
    
    # Find scans that need improvement
    low_performers <- scan_metrics[avg_return < 0 | win_rate < 40]
    low_performers <- low_performers[order(avg_return)]
    
    if (nrow(low_performers) > 0) {
      improvement_list <- lapply(1:min(3, nrow(low_performers)), function(i) {
        scan <- low_performers[i]
        tags$div(
          tags$p(tags$strong(scan$scan_symbol)),
          tags$ul(
            tags$li(paste("Return:", sprintf("%.2f%%", scan$avg_return))),
            tags$li(paste("Win Rate:", sprintf("%.1f%%", scan$win_rate)))
          ),
          if (i < min(3, nrow(low_performers))) tags$hr() else NULL
        )
      })
      
      tagList(
        tags$div(style = "padding: 10px;",
                 tags$h5("⚠️ Needs Improvement"),
                 improvement_list,
                 tags$p(style = "margin-top: 15px; color: #6c757d; font-size: 12px;",
                        "Consider adjusting scan criteria or combining with other scans."
                 )
        )
      )
    } else {
      tagList(
        tags$div(style = "padding: 10px;",
                 tags$h5("✅ All Scans Performing Well"),
                 tags$p("No significant issues detected.")
        )
      )
    }
  })
  
  # Download Effectiveness Results
  output$effectiveness_download <- downloadHandler(
    filename = function() {
      paste("scan_effectiveness_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      results <- effectiveness_analysis()
      if (!is.null(results)) {
        write.csv(results$symbol_results, file, row.names = FALSE)
      }
    }
  )
  
  # ============================================================================
  # BULK SCANNER
  # ============================================================================
  
  # Bulk Scanner
  uploaded_symbols <- reactive({
    if (!is.null(input$bulk_upload)) {
      file_ext <- tools::file_ext(input$bulk_upload$datapath)
      
      if (file_ext == "csv") {
        uploaded_data <- fread(input$bulk_upload$datapath, header = FALSE)
        symbols <- unique(trimws(unlist(uploaded_data)))
      } else if (file_ext == "txt") {
        symbols <- trimws(readLines(input$bulk_upload$datapath))
      } else {
        symbols <- character(0)
      }
      
      return(symbols[symbols != ""])
    }
    return(NULL)
  })
  
  observe({
    data <- scan_data()
    scan_types <- unique(data$scan_symbol)
    updateSelectInput(session, "bulk_scan_filter", choices = scan_types)
  })
  
  bulk_data <- eventReactive(input$bulk_analyze, {
    withProgress(message = 'Analyzing symbols...', value = 0, {
      incProgress(0.2, detail = "Loading data")
      
      data <- scan_data()
      
      text_symbols <- if (input$bulk_symbols != "") {
        unique(trimws(unlist(strsplit(input$bulk_symbols, "[,\n\t ]+"))))
      } else {
        character(0)
      }
      
      upload_symbols <- uploaded_symbols()
      all_symbols <- unique(c(text_symbols, upload_symbols))
      all_symbols <- all_symbols[all_symbols != ""]
      
      if (length(all_symbols) == 0) {
        return(NULL)
      }
      
      incProgress(0.4, detail = "Filtering data")
      
      if (!is.null(input$bulk_date_range)) {
        data <- data[scan_date >= input$bulk_date_range[1] & 
                       scan_date <= input$bulk_date_range[2]]
      }
      
      if (!is.null(input$bulk_scan_filter) && length(input$bulk_scan_filter) > 0) {
        data <- data[scan_symbol %in% input$bulk_scan_filter]
      }
      
      incProgress(0.6, detail = "Processing symbols")
      
      symbol_results <- data.table(symbol = all_symbols)
      
      scan_summary <- data[symbol %in% all_symbols, .(
        total_scans = .N,
        scan_types = paste(unique(scan_symbol), collapse = " | "),
        scan_type_count = length(unique(scan_symbol)),
        first_seen = min(scan_date),
        last_seen = max(scan_date),
        days_active = as.numeric(max(scan_date) - min(scan_date)) + 1,
        avg_scans_per_week = round(.N / (as.numeric(max(scan_date) - min(scan_date) + 1) / 7), 2)
      ), by = symbol]
      
      results <- merge(symbol_results, scan_summary, by = "symbol", all.x = TRUE)
      
      results[is.na(total_scans), `:=`(
        total_scans = 0,
        scan_types = "Not Found",
        scan_type_count = 0,
        first_seen = as.Date(NA),
        last_seen = as.Date(NA),
        days_active = 0,
        avg_scans_per_week = 0
      )]
      
      filtered_results <- results[total_scans >= input$bulk_min_scans]
      
      incProgress(0.8, detail = "Creating matrix")
      
      matrix_data <- data[symbol %in% all_symbols, .N, by = .(symbol, scan_symbol)]
      scan_matrix <- dcast(matrix_data, symbol ~ scan_symbol, value.var = "N", fill = 0)
      
      missing_symbols <- setdiff(all_symbols, scan_matrix$symbol)
      if (length(missing_symbols) > 0) {
        scan_types <- unique(data$scan_symbol)
        missing_matrix <- data.table(symbol = missing_symbols)
        for (st in scan_types) {
          missing_matrix[[st]] <- 0
        }
        scan_matrix <- rbind(scan_matrix, missing_matrix, fill = TRUE)
      }
      
      incProgress(1, detail = "Complete!")
      
      return(list(
        results = filtered_results,
        all_results = results,
        matrix = scan_matrix,
        symbols_requested = all_symbols,
        symbols_found = sum(results$total_scans > 0),
        total_scans = sum(results$total_scans, na.rm = TRUE)
      ))
    })
  })
  
  output$bulk_results_table <- renderDT({
    bulk_result <- bulk_data()
    
    if (is.null(bulk_result)) {
      return(datatable(data.frame(Message = "Please enter symbols and click 'Run Analysis'")))
    }
    
    results_table <- bulk_result$results
    
    if (nrow(results_table) > 0) {
      display_table <- results_table[, .(
        Symbol = symbol,
        Total_Scans = total_scans,
        Scan_Types_Found = scan_types,
        Unique_Scan_Types = scan_type_count,
        First_Seen = ifelse(is.na(first_seen), "Never", as.character(first_seen)),
        Last_Seen = ifelse(is.na(last_seen), "Never", as.character(last_seen)),
        Days_Active = days_active,
        Scans_Per_Week = avg_scans_per_week
      )][order(-Total_Scans)]
      
      datatable(display_table,
                rownames = FALSE,
                extensions = 'Buttons',
                options = list(
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel'),
                  pageLength = 25,
                  scrollX = TRUE
                )
      ) %>%
        formatStyle('Total_Scans',
                    background = styleColorBar(range(display_table$Total_Scans), '#90caf9'),
                    backgroundSize = '100% 90%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
        )
    }
  })
  
  output$bulk_matrix_table <- renderDT({
    bulk_result <- bulk_data()
    
    if (is.null(bulk_result) || is.null(bulk_result$matrix)) {
      return(datatable(data.frame(Message = "No matrix data available")))
    }
    
    matrix_table <- bulk_result$matrix
    scan_cols <- names(matrix_table)[names(matrix_table) != "symbol"]
    matrix_table[, Total := rowSums(.SD), .SDcols = scan_cols]
    matrix_table <- matrix_table[order(-Total)]
    
    datatable(matrix_table,
              options = list(pageLength = 20, scrollX = TRUE),
              rownames = FALSE
    )
  })
  
  output$bulk_coverage_plot <- renderPlotly({
    bulk_result <- bulk_data()
    
    if (is.null(bulk_result)) {
      return(NULL)
    }
    
    found_count <- bulk_result$symbols_found
    total_count <- length(bulk_result$symbols_requested)
    not_found <- total_count - found_count
    
    plot_ly(
      labels = c("Found", "Not Found"),
      values = c(found_count, not_found),
      type = 'pie',
      marker = list(colors = c('#66cc66', '#ff6666'))
    ) %>%
      layout(title = paste("Symbol Coverage:", round(found_count/total_count*100, 1), "%"))
  })
  
  output$bulk_distribution_plot <- renderPlotly({
    bulk_result <- bulk_data()
    
    if (is.null(bulk_result)) {
      return(NULL)
    }
    
    results <- bulk_result$all_results[total_scans > 0]
    
    if (nrow(results) > 0) {
      plot_ly(results, x = ~total_scans, type = 'histogram',
              marker = list(color = '#007bff')
      ) %>%
        layout(
          title = "Distribution of Scan Frequencies",
          xaxis = list(title = "Number of Scans"),
          yaxis = list(title = "Number of Symbols")
        )
    }
  })
  
  output$bulk_summary_stats <- renderText({
    bulk_result <- bulk_data()
    
    if (is.null(bulk_result)) {
      return("No analysis performed yet.\nPlease enter symbols and click 'Run Analysis'.")
    }
    
    total_requested <- length(bulk_result$symbols_requested)
    total_found <- bulk_result$symbols_found
    total_scans <- bulk_result$total_scans
    results <- bulk_result$all_results[total_scans > 0]
    
    stats_text <- paste0(
      "📊 BULK ANALYSIS SUMMARY:\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
      "Total Symbols Analyzed: ", total_requested, "\n",
      "Symbols Found in Scans: ", total_found, "\n",
      "Coverage Rate: ", round(total_found/total_requested*100, 1), "%\n",
      "Total Scan Instances: ", total_scans, "\n\n"
    )
    
    if (nrow(results) > 0) {
      stats_text <- paste0(stats_text,
                           "📈 ACTIVITY BREAKDOWN:\n",
                           "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
                           "Average Scans per Symbol: ", round(mean(results$total_scans), 1), "\n",
                           "Median Scans per Symbol: ", median(results$total_scans), "\n",
                           "Most Active Symbol: ", results$symbol[which.max(results$total_scans)], 
                           " (", max(results$total_scans), " scans)\n",
                           "Symbols with 5+ scans: ", sum(results$total_scans >= 5), "\n",
                           "Symbols with 10+ scans: ", sum(results$total_scans >= 10)
      )
    }
    
    return(stats_text)
  })
  
  output$bulk_download <- downloadHandler(
    filename = function() {
      paste("bulk_scan_analysis_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      bulk_result <- bulk_data()
      if (!is.null(bulk_result)) {
        write.csv(bulk_result$all_results, file, row.names = FALSE)
      }
    }
  )
  
  # ============================================================================
  # TRADING LOG
  # ============================================================================
  
  # Trading Log
  filtered_trading_log <- reactive({
    log_data <- csv_trading_log()
    
    if (nrow(log_data) == 0) return(log_data)
    
    if (!is.null(input$quick_symbol_filter) && length(input$quick_symbol_filter) > 0) {
      log_data <- log_data[symbol %in% input$quick_symbol_filter]
    }
    
    if (input$quick_status_filter != "all") {
      log_data <- log_data[status == input$quick_status_filter]
    }
    
    if (!is.null(input$quick_date_filter)) {
      log_data <- log_data[trade_date >= input$quick_date_filter[1] & 
                             trade_date <= input$quick_date_filter[2]]
    }
    
    return(log_data[order(-trade_date, -trade_time)])
  })
  
  observe({
    csv_log <- csv_trading_log()
    if (nrow(csv_log) > 0) {
      log_symbols <- sort(unique(csv_log$symbol))
      updateSelectInput(session, "quick_symbol_filter", choices = log_symbols)
    }
  })
  
  output$trading_log_table <- renderDT({
    log_data <- filtered_trading_log()
    
    if (nrow(log_data) > 0) {
      display_data <- log_data[, .(
        ID = trade_id,
        Symbol = symbol,
        Action = toupper(action),
        Date = as.character(trade_date),
        Time = format(trade_time, "%H:%M"),
        Entry_Price = sprintf("$%.2f", entry_price),
        Exit_Price = ifelse(is.na(exit_price), "Open", sprintf("$%.2f", exit_price)),
        Quantity = quantity,
        Status = toupper(status),
        PnL = ifelse(status == "open", "Open", sprintf("$%.2f", pnl)),
        PnL_Percent = ifelse(status == "open", "Open", sprintf("%.1f%%", pnl_percent))
      )]
      
      datatable(display_data,
                options = list(pageLength = 15, scrollX = TRUE, order = list(list(0, 'desc'))),
                rownames = FALSE
      ) %>%
        formatStyle('PnL',
                    backgroundColor = styleInterval(0, c('#ffcccc', '#ccffcc'))
        ) %>%
        formatStyle('Status',
                    backgroundColor = styleEqual(c('OPEN', 'CLOSED'), c('#ffffcc', '#ccccff'))
        )
    } else {
      datatable(data.frame(Message = "No trades found in CSV file"))
    }
  })
  
  output$pnl_summary <- renderText({
    csv_log <- csv_trading_log()
    
    if (nrow(csv_log) == 0) {
      return("No CSV trading log found")
    }
    
    closed_trades <- csv_log[status == "closed" & !is.na(pnl)]
    
    if (nrow(closed_trades) == 0) {
      return("No closed trades in CSV for P&L analysis")
    }
    
    total_pnl <- sum(closed_trades$pnl, na.rm = TRUE)
    avg_pnl <- mean(closed_trades$pnl, na.rm = TRUE)
    win_trades <- sum(closed_trades$pnl > 0, na.rm = TRUE)
    loss_trades <- sum(closed_trades$pnl < 0, na.rm = TRUE)
    win_rate <- win_trades / nrow(closed_trades) * 100
    
    avg_win <- if(win_trades > 0) mean(closed_trades[pnl > 0]$pnl) else 0
    avg_loss <- if(loss_trades > 0) mean(closed_trades[pnl < 0]$pnl) else 0
    
    paste0(
      "💰 P&L SUMMARY:\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
      "Total P&L: $", sprintf("%.2f", total_pnl), "\n",
      "Average P&L: $", sprintf("%.2f", avg_pnl), "\n",
      "Total Trades: ", nrow(closed_trades), "\n",
      "Winning Trades: ", win_trades, "\n",
      "Losing Trades: ", loss_trades, "\n",
      "Win Rate: ", sprintf("%.1f%%", win_rate), "\n",
      "Average Win: $", sprintf("%.2f", avg_win), "\n",
      "Average Loss: $", sprintf("%.2f", avg_loss)
    )
  })
  
  output$trade_stats <- renderText({
    csv_log <- csv_trading_log()
    
    if (nrow(csv_log) == 0) {
      return("No CSV trading log found")
    }
    
    open_trades <- sum(csv_log$status == "open")
    total_value <- sum(csv_log$entry_price * csv_log$quantity, na.rm = TRUE)
    unique_symbols <- length(unique(csv_log$symbol))
    
    most_traded <- names(sort(table(csv_log$symbol), decreasing = TRUE))[1]
    
    paste0(
      "📊 TRADE STATISTICS:\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
      "Total Trades: ", nrow(csv_log), "\n",
      "Open Positions: ", open_trades, "\n",
      "Closed Positions: ", nrow(csv_log) - open_trades, "\n",
      "Unique Symbols: ", unique_symbols, "\n",
      "Total Trade Value: $", sprintf("%.2f", total_value), "\n",
      "Most Traded: ", most_traded
    )
  })
  
  output$pnl_chart <- renderPlotly({
    csv_log <- csv_trading_log()
    closed_trades <- csv_log[status == "closed" & !is.na(pnl)][order(trade_date)]
    
    if (nrow(closed_trades) > 0) {
      closed_trades[, cumulative_pnl := cumsum(pnl)]
      
      plot_ly(closed_trades, x = 1:nrow(closed_trades), y = ~cumulative_pnl,
              type = 'scatter', mode = 'lines+markers',
              line = list(color = '#007bff', width = 2),
              marker = list(size = 8, color = '#007bff')
      ) %>%
        add_trace(y = 0, type = 'scatter', mode = 'lines',
                  line = list(color = 'red', dash = 'dash', width = 1),
                  showlegend = FALSE
        ) %>%
        layout(
          title = "Cumulative P&L",
          xaxis = list(title = "Trade Number"),
          yaxis = list(title = "Cumulative P&L ($)")
        )
    }
  })
  
  output$win_loss_chart <- renderPlotly({
    csv_log <- csv_trading_log()
    closed_trades <- csv_log[status == "closed" & !is.na(pnl)]
    
    if (nrow(closed_trades) > 0) {
      wins <- sum(closed_trades$pnl > 0)
      losses <- sum(closed_trades$pnl < 0)
      
      plot_ly(
        labels = c("Wins", "Losses"),
        values = c(wins, losses),
        type = 'pie',
        marker = list(colors = c('#66cc66', '#ff6666'))
      ) %>%
        layout(title = paste("Win Rate:", round(wins/(wins+losses)*100, 1), "%"))
    }
  })
  
  output$entry_criteria_chart <- renderPlotly({
    csv_log <- csv_trading_log()
    
    if (nrow(csv_log) > 0) {
      criteria_counts <- csv_log[, .N, by = entry_criteria]
      
      plot_ly(criteria_counts, x = ~entry_criteria, y = ~N, type = 'bar',
              marker = list(color = '#007bff')
      ) %>%
        layout(
          title = "Entry Criteria Usage",
          xaxis = list(title = "Entry Criteria"),
          yaxis = list(title = "Number of Trades")
        )
    }
  })
  
  output$scan_source_chart <- renderPlotly({
    csv_log <- csv_trading_log()
    
    if (nrow(csv_log) > 0) {
      all_sources <- unlist(strsplit(csv_log$scan_source, "\\|"))
      all_sources <- all_sources[all_sources != ""]
      
      if (length(all_sources) > 0) {
        source_counts <- data.table(source = all_sources)[, .N, by = source]
        
        plot_ly(source_counts, x = ~source, y = ~N, type = 'bar',
                marker = list(color = '#28a745')
        ) %>%
          layout(
            title = "Scan Source Usage",
            xaxis = list(title = "Scan Source"),
            yaxis = list(title = "Number of Trades")
          )
      }
    }
  })
  
  output$download_trades <- downloadHandler(
    filename = function() {
      paste("trading_log_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      log_data <- filtered_trading_log()
      if (nrow(log_data) > 0) {
        write.csv(log_data, file, row.names = FALSE)
      }
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
