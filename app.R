library(shiny)
library(dplyr)
library(data.table)
library(DT)
# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# Sample data structure (replace with your actual data loading)
# Assuming your data has columns: scan_date, scan_symbol, symbol
# create_sample_data <- function() {
#   # This function creates sample data - replace with your actual data loading
#   dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "week")
#   scan_types <- c("CarusoInsights", "roc_mdt25")
#   symbols_pool <- c("AGX", "AMSC", "CAMT", "CENX", "CS", "HNRG", "EL", "ATRO", "CYBN", "PRAX")
#   
#   sample_data <- data.table(
#     scan_date = sample(dates, 1000, replace = TRUE),
#     scan_symbol = sample(scan_types, 1000, replace = TRUE),
#     symbol = sample(symbols_pool, 1000, replace = TRUE)
#   )
#   
#   return(sample_data)
# }

create_sample_data <- function() {
sample_data <- fread("https://raw.githubusercontent.com/rahul-c1/scan/refs/heads/main/weekly_scans.csv")
sample_data$date <- as.Date(sample_data$date)
setnames(sample_data,"scan","scan_symbol")
setnames(sample_data,"date","scan_date")
}
# UI
ui <- navbarPage(
  title = "Trading Scan Analysis Dashboard",
  theme = "bootstrap.min.css",
  
  # Tab 1: Scan Frequency Analyzer
  tabPanel("Scan Frequency",
           fluidRow(
             column(4,
                    wellPanel(
                      h4("Filters"),
                      dateRangeInput("freq_date_range", "Date Range:",
                                     start = Sys.Date() - 90,
                                     end = Sys.Date()),
                      selectInput("freq_scan_type", "Scan Type:",
                                  choices = NULL,
                                  multiple = TRUE),
                      numericInput("freq_min_appearances", "Min Appearances:",
                                   value = 1, min = 1),
                      actionButton("freq_update", "Update Analysis", class = "btn-primary")
                    )
             ),
             column(8,
                    plotOutput("freq_plot"),
                    DT::dataTableOutput("freq_table")
             )
           )
  ),
  
  # Tab 2: Scan Pattern Dashboard
  tabPanel("Scan Patterns",
           fluidRow(
             column(3,
                    wellPanel(
                      h4("Pattern Analysis"),
                      dateRangeInput("pattern_date_range", "Date Range:",
                                     start = Sys.Date() - 180,
                                     end = Sys.Date()),
                      selectInput("pattern_view", "View Type:",
                                  choices = list("Heatmap" = "heatmap",
                                                 "Time Series" = "timeseries",
                                                 "Weekly Breakdown" = "weekly")),
                      actionButton("pattern_update", "Update", class = "btn-primary")
                    )
             ),
             column(9,
                    plotOutput("pattern_plot", height = "500px"),
                    DT::dataTableOutput("pattern_summary")
             )
           )
  ),
  
  # Tab 3: Symbol Momentum Tracker
  tabPanel("Momentum Tracker",
           fluidRow(
             column(4,
                    wellPanel(
                      h4("Momentum Settings"),
                      numericInput("momentum_weeks", "Analysis Period (weeks):",
                                   value = 2, min = 2, max = 12),
                      numericInput("momentum_threshold", "Momentum Threshold:",
                                   value = 100, min = 0, max = 1000),
                      selectInput("momentum_type", "Momentum Type:",
                                  choices = list("Heating Up" = "Heating Up",
                                                 "Cooling Down" = "Cooling Down",
                                                 "Both" = "both")),
                      actionButton("momentum_update", "Analyze Momentum", class = "btn-primary")
                    ),
                    wellPanel(
                      h4("Hot Symbols Alert"),
                      verbatimTextOutput("hot_symbols")
                    )
             ),
             column(8,
                    plotOutput("momentum_plot"),
                    DT::dataTableOutput("momentum_table")
             )
           )
  ),
  
  # Tab 4: Scan Correlation Matrix
  # tabPanel("Correlations",
  #          fluidRow(
  #            column(3,
  #                   wellPanel(
  #                     h4("Correlation Analysis"),
  #                     dateRangeInput("corr_date_range", "Date Range:",
  #                                    start = Sys.Date() - 90,
  #                                    end = Sys.Date()),
  #                     numericInput("corr_min_freq", "Min Symbol Frequency:",
  #                                  value = 5, min = 1),
  #                     actionButton("corr_update", "Calculate", class = "btn-primary")
  #                   )
  #            ),
  #            column(9,
  #                   plotOutput("correlation_heatmap", height = "400px"),
  #                   h4("Scan Co-occurrence Analysis"),
  #                   DT::dataTableOutput("correlation_table")
  #            )
  #          )
  # ),
  
  # Tab 5: Portfolio Screening
  tabPanel("Portfolio Screen",
           fluidRow(
             column(4,
                    wellPanel(
                      h4("Portfolio Input"),
                      textAreaInput("portfolio_symbols", "Enter Symbols (one per line):",
                                    value = "AGX\nBE\nPLTR", height = "150px"),
                      dateRangeInput("portfolio_date_range", "Analysis Period:",
                                     start = Sys.Date() - 30,
                                     end = Sys.Date()),
                      actionButton("portfolio_analyze", "Analyze Portfolio", class = "btn-primary")
                    ),
                    wellPanel(
                      h4("Portfolio Metrics"),
                      verbatimTextOutput("portfolio_metrics")
                    )
             ),
             column(8,
                    plotOutput("portfolio_plot"),
                    h4("Portfolio Scan Activity"),
                    DT::dataTableOutput("portfolio_table3")
                    #DT::dataTableOutput("portfolio_table2")
                    #DT::dataTableOutput("portfolio_table")
                    #plotOutput("portfolio_table3")
             )
           )
  ),
  
  # Tab 6: Signal Strength Calculator
  tabPanel("Signal Strength",
           fluidRow(
             column(4,
                    wellPanel(
                      h4("Signal Weights"),
                      p("Assign weights to each scan type:"),
                      uiOutput("weight_inputs"),
                      br(),
                      numericInput("signal_min_score", "Min Signal Score:",
                                   value = .25, min = 0, max = 1),
                      actionButton("signal_calculate", "Calculate Signals", class = "btn-primary")
                    )
             ),
             column(8,
                    plotOutput("signal_plot"),
                    h4("Top Signal Strength Symbols"),
                    DT::dataTableOutput("signal_table")
             )
           )
  ),
  
  # # Tab 7: Historical Performance
  # tabPanel("Historical Analysis",
  #          fluidRow(
  #            column(4,
  #                   wellPanel(
  #                     h4("Backtest Settings"),
  #                     dateRangeInput("hist_date_range", "Analysis Period:",
  #                                    start = Sys.Date() - 180,
  #                                    end = Sys.Date()),
  #                     selectInput("hist_scan_type", "Focus Scan Type:",
  #                                 choices = NULL),
  #                     numericInput("hist_lookback", "Lookback Days:",
  #                                  value = 7, min = 1, max = 30),
  #                     actionButton("hist_analyze", "Run Analysis", class = "btn-primary")
  #                   )
  #            ),
  #            column(8,
  #                   plotOutput("historical_plot"),
  #                   h4("Scan Effectiveness Metrics"),
  #                   DT::dataTableOutput("historical_table")
  #            )
  #          )
  # ),
  
  # Tab 8: Alert System
  tabPanel("Alert System",
           fluidRow(
             column(4,
                    wellPanel(
                      h4("Alert Settings"),
                      selectInput("alert_symbols", "Watch Symbols:",
                                  choices = NULL, multiple = TRUE),
                      selectInput("alert_scan_types", "Watch Scan Types:",
                                  choices = NULL, multiple = TRUE),
                      numericInput("alert_frequency_threshold", "Frequency Threshold:",
                                   value = 3, min = 1),
                      checkboxInput("alert_new_symbols", "Alert on New Symbols", TRUE),
                      actionButton("alert_setup", "Setup Alerts", class = "btn-primary")
                    ),
                    wellPanel(
                      h4("Active Alerts"),
                      verbatimTextOutput("active_alerts")
                    )
             ),
             column(8,
                    h4("Recent Alert Activity"),
                    DT::dataTableOutput("alert_table"),
                    br(),
                    plotOutput("alert_timeline")
             )
           )
  ),
  # Tab 10: Bulk Symbol Scanner
  tabPanel("Bulk Scanner",
           fluidRow(
             column(4,
                    wellPanel(
                      h4("Bulk Symbol Analysis"),
                      textAreaInput("bulk_symbols", "Enter Symbols (one per line, comma or space separated):",
                                    value = "", height = "200px"),
                      fileInput("bulk_upload", "Or Upload CSV/TXT File:",
                                accept = c(".csv", ".txt")),
                      dateRangeInput("bulk_date_range", "Analysis Period:",
                                     start = Sys.Date() - 60,
                                     end = Sys.Date()),
                      selectInput("bulk_scan_filter", "Filter by Scan Types:",
                                  choices = NULL, multiple = TRUE),
                      numericInput("bulk_min_scans", "Minimum Scans Required:",
                                   value = 1, min = 0),
                      actionButton("bulk_analyze", "Run Bulk Analysis", class = "btn-success"),
                      br(), br(),
                      downloadButton("bulk_download", "Download Results", class = "btn-info")
                    )
             ),
             column(8,
                    tabsetPanel(
                      tabPanel("Results Table",
                               h4("Bulk Scan Results"),
                               DT::dataTableOutput("bulk_results_table")
                      ),
                      tabPanel("Scan Matrix",
                               h4("Symbol vs Scan Type Matrix"),
                               DT::dataTableOutput("bulk_matrix_table")
                      ),
                      tabPanel("Summary Stats",
                               fluidRow(
                                 column(6,
                                        h4("Coverage Analysis"),
                                        plotOutput("bulk_coverage_plot", height = "300px")
                                 ),
                                 column(6,
                                        h4("Scan Distribution"),
                                        plotOutput("bulk_distribution_plot", height = "300px")
                                 )
                               ),
                               h4("Bulk Analysis Summary"),
                               verbatimTextOutput("bulk_summary_stats")
                      )
                    )
             )
           )
  ),
  # Tab 11: Trading Log
  tabPanel("Trading Log",
           fluidRow(
             column(4,
                    wellPanel(
                      h4("Add New Trade"),
                      selectizeInput("trade_symbol", "Symbol:",
                                     choices = NULL, options = list(create = TRUE)),
                      radioButtons("trade_action", "Action:",
                                   choices = list("Buy" = "buy", "Sell" = "sell"),
                                   selected = "buy", inline = TRUE),
                      dateInput("trade_date", "Trade Date:", value = Sys.Date()),
                      textInput("trade_time", "Trade Time (HH:MM):", value = format(Sys.time(), "%H:%M")),
 #                     timeInput("trade_time", "Trade Time:", value = Sys.time()),
                      numericInput("trade_price", "Entry Price ($):", value = 0, min = 0, step = 0.01),
                      numericInput("trade_quantity", "Quantity:", value = 100, min = 1),
                      numericInput("trade_target", "Target Price ($):", value = 0, min = 0, step = 0.01),
                      numericInput("trade_stop", "Stop Loss ($):", value = 0, min = 0, step = 0.01),
                      selectInput("trade_entry_criteria", "Entry Criteria:",
                                  choices = list(
                                    "OpenEqualsLow" = "OpenEqualsLow",
                                    "4percent" = "4percent", 
                                    "21dayBreakout" = "21dayBreakout",
                                    "VolumeSpike" = "VolumeSpike",
                                    "GapUp" = "GapUp",
                                    "BreakoutPattern" = "BreakoutPattern",
                                    "ScanAlert" = "ScanAlert",
                                    "TechnicalSetup" = "TechnicalSetup",
                                    "NewsEvent" = "NewsEvent",
                                    "Other" = "Other"
                                  )),
                      selectInput("trade_scan_source", "Scan Source:",
                                  choices = NULL, multiple = TRUE),
                      textAreaInput("trade_notes", "Trade Notes:", height = "100px"),
                      br(),
                      actionButton("add_trade", "Add Trade", class = "btn-primary"),
                      actionButton("update_trade", "Update Selected", class = "btn-warning"),
                      actionButton("delete_trade", "Delete Selected", class = "btn-danger")
                    ),
 wellPanel(
   h4("Close Selected Trade"),
   numericInput("exit_price", "Exit Price ($):", value = 0, min = 0, step = 0.01),
   dateInput("exit_date", "Exit Date:", value = Sys.Date()),
   textInput("exit_time", "Exit Time (HH:MM):", value = format(Sys.time(), "%H:%M")),
   textAreaInput("exit_notes", "Exit Notes:", height = "60px"),
   br(),
   actionButton("close_trade", "Close Selected Trade", class = "btn-warning"),
   br(), br(),
   verbatimTextOutput("selected_trade_info")
 ),
                    wellPanel(
                      h4("Quick Actions"),
                      selectInput("quick_symbol_filter", "Filter by Symbol:", 
                                  choices = NULL, multiple = TRUE),
                      selectInput("quick_status_filter", "Filter by Status:",
                                  choices = list("All" = "all", "Open" = "open", "Closed" = "closed")),
                      dateRangeInput("quick_date_filter", "Date Range:",
                                     start = Sys.Date() - 30, end = Sys.Date()),
                      downloadButton("download_trades", "Download Trading Log", class = "btn-success"),
                      # Add this button in the Quick Actions wellPanel:
                      actionButton("bulk_close_trades", "View Open Trades", class = "btn-info")
                    )
 
             ),
             column(8,
                    tabsetPanel(
                      tabPanel("Trading Log",
                               DT::dataTableOutput("trading_log_table")
                      ),
                      tabPanel("Performance Dashboard",
                               fluidRow(
                                 column(6,
                                        h4("P&L Summary"),
                                        verbatimTextOutput("pnl_summary")
                                 ),
                                 column(6,
                                        h4("Trade Statistics"),
                                        verbatimTextOutput("trade_stats")
                                 )
                               ),
                               fluidRow(
                                 column(6,
                                        plotOutput("pnl_chart", height = "300px")
                                 ),
                                 column(6,
                                        plotOutput("win_loss_chart", height = "300px")
                                 )
                               )
                      ),
                      tabPanel("Strategy Analysis",
                               fluidRow(
                                 column(6,
                                        h4("Entry Criteria Performance"),
                                        plotOutput("entry_criteria_chart", height = "300px")
                                 ),
                                 column(6,
                                        h4("Scan Source Performance"),
                                        plotOutput("scan_source_chart", height = "300px")
                                 )
                               ),
                               h4("Strategy Performance Table"),
                               DT::dataTableOutput("strategy_performance_table")
                      ),
                      tabPanel("Risk Analysis",
                               fluidRow(
                                 column(6,
                                        h4("Position Sizing Distribution"),
                                        plotOutput("position_size_chart", height = "300px")
                                 ),
                                 column(6,
                                        h4("Risk-Reward Analysis"),
                                        plotOutput("risk_reward_chart", height = "300px")
                                 )
                               ),
                               h4("Risk Metrics"),
                               DT::dataTableOutput("risk_metrics_table")
                      )
                    )
             )
           )
  )
)

# Server
server <- function(input, output, session) {
  
  
  
  # Add this reactive function to read the CSV file
  csv_trading_log <- reactive({
    tryCatch({
      if (class(read.csv2("https://raw.githubusercontent.com/rahul-c1/scan/refs/heads/main/trading_log.csv"))=="data.frame") {
        csv_data <- fread("https://raw.githubusercontent.com/rahul-c1/scan/refs/heads/main/trading_log.csv")
        
        # Ensure proper data types
        if (nrow(csv_data) > 0) {
          csv_data[, trade_date := as.Date(trade_date)]
          csv_data[, trade_time := as.POSIXct(trade_time)]
          csv_data[, created_at := as.POSIXct(created_at)]
          csv_data[, updated_at := as.POSIXct(updated_at)]
          
          # Ensure all required columns exist
          required_cols <- c("trade_id", "symbol", "action", "trade_date", "trade_time", 
                             "entry_price", "exit_price", "quantity", "target_price", 
                             "stop_loss", "entry_criteria", "scan_source", "notes", 
                             "status", "pnl", "pnl_percent", "created_at", "updated_at")
          
          missing_cols <- setdiff(required_cols, names(csv_data))
          if (length(missing_cols) > 0) {
            for (col in missing_cols) {
              if (col %in% c("pnl", "pnl_percent", "entry_price", "exit_price", "target_price", "stop_loss")) {
                csv_data[[col]] <- as.numeric(NA)
              } else if (col %in% c("trade_date")) {
                csv_data[[col]] <- as.Date(NA)
              } else if (col %in% c("trade_time", "created_at", "updated_at")) {
                csv_data[[col]] <- as.POSIXct(NA)
              } else if (col == "trade_id") {
                csv_data[[col]] <- as.integer(NA)
              } else if (col == "quantity") {
                csv_data[[col]] <- as.integer(0)
              } else {
                csv_data[[col]] <- as.character("")
              }
            }
          }
          
          return(csv_data[order(-trade_date, -trade_time)])
        }
      }
      # Return empty data.table with correct structure if file doesn't exist
      return(data.table(
        trade_id = integer(),
        symbol = character(),
        action = character(),
        trade_date = as.Date(character()),
        trade_time = as.POSIXct(character()),
        entry_price = numeric(),
        exit_price = numeric(),
        quantity = integer(),
        target_price = numeric(),
        stop_loss = numeric(),
        entry_criteria = character(),
        scan_source = character(),
        notes = character(),
        status = character(),
        pnl = numeric(),
        pnl_percent = numeric(),
        created_at = as.POSIXct(character()),
        updated_at = as.POSIXct(character())
      ))
    }, error = function(e) {
      # Return empty data.table on error
      return(data.table(
        trade_id = integer(),
        symbol = character(),
        action = character(),
        trade_date = as.Date(character()),
        trade_time = as.POSIXct(character()),
        entry_price = numeric(),
        exit_price = numeric(),
        quantity = integer(),
        target_price = numeric(),
        stop_loss = numeric(),
        entry_criteria = character(),
        scan_source = character(),
        notes = character(),
        status = character(),
        pnl = numeric(),
        pnl_percent = numeric(),
        created_at = as.POSIXct(character()),
        updated_at = as.POSIXct(character())
      ))
    })
  })
  
  # Load and process data
  scan_data <- reactive({
    # Replace this with your actual data loading
    create_sample_data()
  })
  
  # Update choice inputs based on data
  observe({
    data <- scan_data()
    scan_types <- unique(data$scan_symbol)
    symbols <- unique(data$symbol)
    
    updateSelectInput(session, "freq_scan_type", choices = scan_types)
    updateSelectInput(session, "hist_scan_type", choices = scan_types)
    updateSelectInput(session, "alert_symbols", choices = symbols)
    updateSelectInput(session, "alert_scan_types", choices = scan_types)
  })
  
  # Tab 1: Scan Frequency Analysis
  freq_data <- eventReactive(input$freq_update, {
    data <- scan_data()
    
    # Filter data
    if (!is.null(input$freq_date_range)) {
      data <- data[scan_date >= input$freq_date_range[1] & 
                     scan_date <= input$freq_date_range[2]]
    }
    
    if (!is.null(input$freq_scan_type) && length(input$freq_scan_type) > 0) {
      data <- data[scan_symbol %in% input$freq_scan_type]
    }
    
    
    unique_dates <- sort(unique(data$scan_date), decreasing = TRUE)
    current_date <- unique_dates[1]
    prior_date <- unique_dates[2]
    
    print(prior_date,current_date)
    
    freq_summary <- data[, .(
      prior_date_count = sum(scan_date == prior_date),
      current_date_count = sum(scan_date == current_date),
      #appearances = .N,
      #scan_types = length(unique(scan_symbol)),
      scan_types = length(unique(scan_symbol[scan_date == current_date])),
      #min_scan_date = min(scan_date),
      #max_scan_date = max(scan_date),
      first_seen = min(scan_date),
      last_seen = max(scan_date)
    ), by = symbol][current_date_count >= input$freq_min_appearances][order(-(current_date_count-prior_date_count))]
    
    return(freq_summary)
    
    # Calculate frequency
    # freq_summary <- data[, .(
    #   appearances = .N,
    #   scan_types = length(unique(scan_symbol)),
    #   first_seen = min(scan_date),
    #   last_seen = max(scan_date)
    # ), by = symbol][appearances >= input$freq_min_appearances][order(-appearances)]
    # 
    # return(freq_summary)
  })
  
  output$freq_plot <- renderPlot({
    data <- freq_data()
    if (nrow(data) > 0) {
      top_20 <- head(data, 20)
      barplot(top_20$current_date_count, names.arg = top_20$symbol,
              las = 2, main = "Top 20 Symbols by Scan Frequency",
              col = "steelblue", ylab = "Number of Appearances")
    }
  })
  
  output$freq_table <- DT::renderDataTable({
    DT::datatable(freq_data(), options = list(pageLength = 15))
  })
  
  # Tab 2: Scan Pattern Analysis
  pattern_data <- eventReactive(input$pattern_update, {
    data <- scan_data()
    
    if (!is.null(input$pattern_date_range)) {
      data <- data[scan_date >= input$pattern_date_range[1] & 
                     scan_date <= input$pattern_date_range[2]]
    }
    
    # Create weekly summary
    data[, week := format(as.Date(scan_date), "%Y-%W")]
    #data[, week :=paste0(strftime(as.Date(scan_date), format = "%Y"),"-",strftime(as.Date(scan_date), format = "%V"))]
    pattern_summary <- data[, .N, by = .(week, scan_symbol)]
    
    return(pattern_summary)
  })
  
  output$pattern_plot <- renderPlot({
    data <- pattern_data()
    if (nrow(data) > 0) {
      if (input$pattern_view == "heatmap") {
        # Simple heatmap using base R
        pivot_data <- dcast(data, week ~ scan_symbol, value.var = "N", fill = 0)
        mat <- as.matrix(pivot_data[, -1])
        rownames(mat) <- pivot_data$week
        heatmap(mat, main = "Scan Activity Heatmap", scale = "column")
      } else if (input$pattern_view == "timeseries") {
        
        data <- scan_data()
        
        if (!is.null(input$pattern_date_range)) {
          data <- data[scan_date >= input$pattern_date_range[1] & 
                         scan_date <= input$pattern_date_range[2]]
        }
        # Time series plot
        # weekly_totals <- data[, sum(N), by = week]
        # plot(1:nrow(weekly_totals), weekly_totals$V1, type = "l",
        #      main = "Weekly Scan Activity", xlab = "Week", ylab = "Total Scans")
        # First aggregate the data to get counts by scan_symbol and date
        # Using the correct column names from your data
        plot_data <- data[, .(symbol_count = .N), by = .(scan_symbol, scan_date)]
        
        # Convert scan_date to Date format
        plot_data$scan_date <- as.Date(plot_data$scan_date)
        
        # Get unique scan symbols and dates for plotting
        scan_symbols <- unique(plot_data$scan_symbol)
        dates <- sort(unique(plot_data$scan_date))
        
        # Set up colors for different scan symbols
        colors <- rainbow(length(scan_symbols))
        names(colors) <- scan_symbols
        
        # Create the base plot
        plot(range(dates), range(plot_data$symbol_count), 
             type = "n",
             xlab = "Date", 
             ylab = "Number of Symbols",
             main = "Time Series of Symbol Counts by Scan Type",
             xaxt = "n")
        
        # Add x-axis with formatted dates
        axis.Date(1, at = dates, format = "%m-%d", las = 2)
        
        # Plot lines for each scan_symbol
        for(i in 1:length(scan_symbols)) {
          scan_data <- plot_data[scan_symbol == scan_symbols[i]]
          lines(scan_data$scan_date, scan_data$symbol_count, 
                col = colors[i], lwd = 2, type = "o", pch = 16)
        }
        
        # Add legend
        legend("topright", 
               legend = scan_symbols,
               col = colors,
               lwd = 2,
               pch = 16,
               bty = "n")
        
        
      }
      else{
        data <- scan_data()
        
        if (!is.null(input$pattern_date_range)) {
          data <- data[scan_date >= input$pattern_date_range[1] & 
                         scan_date <= input$pattern_date_range[2]]
        }
        # Get the last 5 unique dates
        last_5_dates <- sort(unique(data$scan_date), decreasing = TRUE)[1:5]
        
        # Count total symbols for each of the last 5 dates
        date_counts <- data[scan_date %in% last_5_dates, .(total_symbols = .N), by = scan_date]
        
        # Sort by date for proper ordering
        date_counts <- date_counts[order(scan_date)]
        
        # Create bar plot
        barplot(date_counts$total_symbols,
                names.arg = format(as.Date(date_counts$scan_date), "%m-%d"),
                main = "Total Symbol Counts by Last 5 Dates",
                xlab = "Date",
                ylab = "Total Number of Symbols",
                col = "steelblue",
                border = "white",
                las = 2,
                ylim = c(0, max(date_counts$total_symbols) * 1.1))
        
        # Add value labels on top of bars
        text(x = 1:nrow(date_counts), 
             y = date_counts$total_symbols + max(date_counts$total_symbols) * 0.02,
             labels = date_counts$total_symbols,
             pos = 3,
             cex = 0.8)
        
        # Add grid for better readability
        grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")
      }
    }
  })
  
  output$pattern_summary <- DT::renderDataTable({
    data <- scan_data()
    if (!is.null(input$pattern_date_range)) {
      data <- data[scan_date >= input$pattern_date_range[1] & 
                     scan_date <= input$pattern_date_range[2]]
    }
    # summary_table <- data[, .(total_scans = .N), by = .(scan_date,scan_symbol)]
    # DT::datatable(summary_table, options = list(pageLength = 10))
    
    recent_dates <- sort(unique(data$scan_date), decreasing = TRUE)[1:4]
    
    # Create a summary table with counts by scan_symbol for each of the 4 dates
    result <- data[scan_date %in% recent_dates, 
                   .(count = .N), 
                   by = .(scan_symbol, scan_date)]
    
    # Reshape to wide format with dates as columns
    library(data.table)
    wide_result <- dcast(result, scan_symbol ~ scan_date, value.var = "count", fill = 0)
    
    # Reorder columns by date (most recent first)
    date_cols <- as.character(sort(recent_dates, decreasing = FALSE))
    wide_result <- wide_result[, c("scan_symbol", date_cols), with = FALSE]
    
    # Add a total column
    #wide_result[, total := rowSums(.SD), .SDcols = date_cols]
    
    # Display the result
    DT::datatable(wide_result, options = list(pageLength = 10))
  })
  
  # Tab 3: Momentum Tracker
  momentum_data <- eventReactive(input$momentum_update, {
    data <- scan_data()
    weeks_back <- input$momentum_weeks
    # 
    # # Calculate recent vs historical frequency
    # recent_date <- Sys.Date() - (weeks_back * 7)
    
    # recent_freq <- data[scan_date >= recent_date, .N, by = symbol]
    # historical_freq <- data[scan_date < recent_date, .N, by = symbol]
    
    sorted_dates <- sort(unique(data$scan_date), decreasing = TRUE)
    
    # Determine which dates to use based on weeks_back parameter
    if(exists("weeks_back") && !is.null(weeks_back)) {
      last_2_dates <- sorted_dates[c(1, weeks_back)]
    } else {
      last_2_dates <- sorted_dates[c(1, 2)]
    }
    
    # Check if we got 2 dates, if not fall back to c(1,2)
    if(length(last_2_dates) != 2) {
      last_2_dates <- sorted_dates[c(1, 2)]
    }
    
    
    #last_2_dates <- sort(unique(data$scan_date), decreasing = TRUE)[c(1,weeks_back)]
    most_recent_date <- last_2_dates[1]
    second_recent_date <- last_2_dates[2]
    
    # Get frequency for most recent date
    recent_freq <- data[scan_date == most_recent_date, .N, by = symbol]
    
    # Get frequency for the comparison date
    historical_freq <- data[scan_date == second_recent_date, .N, by = symbol]
    
    momentum <- merge(recent_freq, historical_freq, by = "symbol", all = TRUE)
    momentum[is.na(N.x), N.x := 0]
    momentum[is.na(N.y), N.y := 0]
    
    #momentum[, momentum_score := round(ifelse(N.y > 0, (N.x / N.y) * 100, N.x * 100),2)]
    #momentum[, momentum_type := ifelse(momentum_score > 100, "Heating Up", "Cooling Down")]
    
    # Combined momentum score logic
    momentum[, `:=`(
      # Base momentum score (your original logic)
      momentum_score = round(ifelse(N.y > 0, (N.x / N.y) * 100, N.x * 100), 2),
      
      # Calculate additional metrics for sophisticated scoring
      freq_change = N.x - N.y,
      freq_change_pct = round(ifelse(N.y > 0, ((N.x - N.y) / N.y) * 100, 
                                     ifelse(N.x > 0, 100, 0)), 2),
      activity_ratio = round(ifelse(N.y > 0, N.x / N.y, 
                                    ifelse(N.x > 0, Inf, 0)), 2)
    )]
    
    # Sophisticated momentum classification with multiple criteria
    momentum[, momentum_type := case_when(
      # Strong heating up: high momentum score AND significant increase
      momentum_score >= 200 & freq_change >= 2 ~ "Heating Up",
      
      # Moderate heating up: above baseline with positive change
      momentum_score >= 120 & freq_change > 0 ~ "Heating Up",
      
      # New emergence: high recent activity with little/no historical
      momentum_score >= 150 & N.y <= 1 ~ "Heating Up",
      
      # Strong cooling down: low momentum score AND significant decrease
      momentum_score <= 50 & freq_change <= -2 ~ "Cooling Down",
      
      # Moderate cooling down: below baseline with negative change
      momentum_score <= 80 & freq_change < 0 ~ "Cooling Down",
      
      # Disappearing: little recent activity with high historical
      momentum_score <= 25 & N.y >= 3 ~ "Cooling Down",
      
      # Default: neutral/stable
      TRUE ~ "Neutral"
    )]
    
    # Add momentum strength for further analysis
    momentum[, momentum_strength := case_when(
      momentum_type == "Heating Up" & momentum_score >= 300 ~ "Very Strong",
      momentum_type == "Heating Up" & momentum_score >= 200 ~ "Strong", 
      momentum_type == "Heating Up" & momentum_score >= 120 ~ "Moderate",
      momentum_type == "Cooling Down" & momentum_score <= 25 ~ "Very Strong",
      momentum_type == "Cooling Down" & momentum_score <= 50 ~ "Strong",
      momentum_type == "Cooling Down" & momentum_score <= 80 ~ "Moderate",
      TRUE ~ "Weak"
    )]
    
    # Sort by momentum type and strength
    momentum <- momentum[order(momentum_type, -momentum_score)]
    
    setnames(momentum, c("N.x", "N.y"), c("recent_freq", "historical_freq"))
    
    return(momentum[order(-momentum_score)])
  })
  
  output$momentum_plot <- renderPlot({
    data <- momentum_data()
    if (input$momentum_type=="Heating Up" | input$momentum_type == "Cooling Down")
    
    if (input$momentum_type=="Heating Up")
    {
      data <- data[momentum_type==input$momentum_type,][order(momentum_type, -freq_change)]
    }
    
    else if (input$momentum_type == "Cooling Down")
    {
      data <- data[momentum_type==input$momentum_type,][order(momentum_type, freq_change)]
    }
    
    if (nrow(data) > 0) {
      top_20 <- head(data, 20)
      colors <- ifelse(top_20$momentum_score > 100, "maroon", "lightblue")
      barplot(top_20$momentum_score, names.arg = top_20$symbol,
              las = 2, main = "Symbol Momentum Score", col = colors,
              ylab = "Momentum Score (%)")
      abline(h = 100, lty = 2, col = "black")
    }
  })
  
  output$momentum_table <- DT::renderDataTable({
    data <- momentum_data()
    if (input$momentum_type=="Heating Up")
    {
      data <- data[momentum_type==input$momentum_type,][order(momentum_type, -freq_change)]
    }
    
    else if (input$momentum_type == "Cooling Down")
    {
      data <- data[momentum_type==input$momentum_type,][order(momentum_type, freq_change)]
    }
    
    DT::datatable(data, options = list(pageLength = 15))
  })
  
  output$hot_symbols <- renderText({
    data <- momentum_data()
    if (input$momentum_type=="Heating Up" | input$momentum_type == "Cooling Down")
    {
      data <- data[momentum_type==input$momentum_type,]
    }
    
    if (input$momentum_type=="Heating Up" )
    {
      hot <- data[order(momentum_type, -momentum_score)] #[momentum_score > input$momentum_threshold]
    if (nrow(hot) > 0) {
      paste("Hot Symbols:", paste(head(hot$symbol, 10), collapse = ", "))
    } 
    }
    else if (input$momentum_type=="Cooling Down" )
    {
      cool <- data[order(freq_change)]
      if (nrow(cool) > 0) {
        paste("Cool Symbols:", paste(head(cool$symbol, 10), collapse = ", "))
      } 
    }
    
    else {
      "No symbols above threshold"
    }
  })
  
  # Tab 4: Correlation Analysis
  corr_data <- eventReactive(input$corr_update, {
    data <- scan_data()
    
    if (!is.null(input$corr_date_range)) {
      data <- data[scan_date >= input$corr_date_range[1] & 
                     scan_date <= input$corr_date_range[2]]
    }
    
    # Filter symbols with minimum frequency
    symbol_freq <- data[, .N, by = symbol][N >= input$corr_min_freq]
    data <- data[symbol %in% symbol_freq$symbol]
    
    # Create scan type co-occurrence matrix
    symbol_scans <- data[, .(scan_types = list(unique(scan_symbol))), by = symbol]
    
    return(symbol_scans)
  })
  
  output$correlation_heatmap <- renderPlot({
    # Placeholder for correlation heatmap
    plot(1:10, 1:10, main = "Scan Type Correlation Matrix", 
         xlab = "Scan Type", ylab = "Scan Type")
    text(5, 5, "Correlation analysis\nwould be displayed here", cex = 1.5)
  })
  
  output$correlation_table <- DT::renderDataTable({
    data.frame(
      Scan_Type_1 = c("CarusoInsights", "roc_mdt25"),
      Scan_Type_2 = c("roc_mdt25", "CarusoInsights"),
      Correlation = c(0.75, 0.75),
      P_Value = c(0.001, 0.001)
    )
  })
  
  # Tab 5: Portfolio Screening
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
  
  output$portfolio_plot <- renderPlot({
    port_data <- portfolio_data()$data
    if (nrow(port_data) > 0) {
      # symbol_counts <- port_data[, .N, by = symbol]
      # barplot(symbol_counts$N, names.arg = symbol_counts$symbol,
      #         main = "Portfolio Scan Activity", col = "darkgreen",
      #         ylab = "Number of Scans")
      
      # Get the last 2 unique dates
      last_2_dates <- sort(unique(port_data$scan_date), decreasing = TRUE)[1:2]
      current_date <- last_2_dates[1]
      last_date <- last_2_dates[2]
      
      # Get counts for both dates by symbol
      current_counts <- port_data[scan_date == current_date, .N, by = symbol]
      last_counts <- port_data[scan_date == last_date, .N, by = symbol]
      
      # Merge the counts to ensure we have all symbols
      setnames(current_counts, "N", "current_count")
      setnames(last_counts, "N", "last_count")
      
      # Full outer join to get all symbols from both dates
      all_symbols <- merge(current_counts, last_counts, by = "symbol", all = TRUE)
      
      # Replace NA values with 0
      all_symbols[is.na(current_count), current_count := 0]
      all_symbols[is.na(last_count), last_count := 0]
      
      # Create matrix for grouped bar plot
      count_matrix <- as.matrix(t(all_symbols[, .(last_count, current_count)]))
      colnames(count_matrix) <- all_symbols$symbol
      
      # Create grouped bar plot
      barplot(count_matrix,
              main = "Portfolio Scan Activity - Last vs Current Date",
              ylab = "Number of Scans",
              col = c("lightblue", "darkgreen"),
              beside = TRUE,
              legend.text = c(paste("Last Date:", format(as.Date(last_date), "%m-%d")),
                              paste("Current Date:", format(as.Date(current_date), "%m-%d"))),
              args.legend = list(x = "topright"),
              las = 2,
              cex.names = 0.8)
      
      # Add grid for better readability
      grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")
    }
  })
  
  
  output$portfolio_plot2 <- renderPlot({
    port_data <- portfolio_data()$data
    if (nrow(port_data) > 0) {
      # ===== CREATIVE IDEA 5: PORTFOLIO RADAR CHART =====
      # Create radar chart data for top symbols
      library(fmsb)
      top_symbols <- port_data[, .N, by = symbol][order(-N)][1:min(6, .N)]$symbol
      
      radar_data <- port_data[symbol %in% top_symbols, {
        dates <- sort(unique(scan_date))
        list(
          Volume = .N,
          Diversity = length(unique(scan_symbol)),
          Recency = as.numeric(max(scan_date) - min(port_data$scan_date)) + 1,
          Consistency = length(unique(scan_date)),
          Momentum = ifelse(length(dates) >= 2, 
                            sum(scan_date == dates[length(dates)]), 1)
        )
      }, by = symbol]
      
      # Normalize for radar chart (0-100 scale)
      radar_matrix <- as.data.frame(radar_data[, -1])
      radar_matrix[] <- lapply(radar_matrix, function(x) (x/max(x, na.rm=TRUE))*100)
      rownames(radar_matrix) <- radar_data$symbol
      
      # Add max and min rows for radar chart
      radar_final <- rbind(rep(100, ncol(radar_matrix)), 
                           rep(0, ncol(radar_matrix)), 
                           radar_matrix)
      
      radarchart(radar_final, 
                 title = "Portfolio Multi-Dimensional Analysis")
    }
  })
  
  # output$portfolio_plot2 <- renderPlot({
  #   port_data <- portfolio_data()$data
  #   if (nrow(port_data) > 0) {
  #     # ===== CREATIVE IDEA 6: PORTFOLIO TIMELINE =====
  #     timeline_data <- port_data[, {
  #       list(
  #         event_type = "scan",
  #         event_detail = paste0(length(unique(scan_symbol)), " scan types"),
  #         scan_count = .N
  #       )
  #     }, by = .(symbol, scan_date)]
  #     
  #     # Simple timeline visualization
  #     plot(as.Date(timeline_data$scan_date), 
  #          as.numeric(as.factor(timeline_data$symbol)),
  #          pch = 19, 
  #          cex = timeline_data$scan_count/3,
  #          col = rainbow(length(unique(timeline_data$symbol)))[as.numeric(as.factor(timeline_data$symbol))],
  #          main = "Portfolio Scan Timeline",
  #          xlab = "Date", ylab = "Symbols",
  #          yaxt = "n")
  #     axis(2, at = 1:length(unique(timeline_data$symbol)), 
  #          labels = unique(timeline_data$symbol), las = 2)
  #   }
  # })
  
  # output$portfolio_plot2 <- renderPlot({
  #   port_data <- portfolio_data()$data
  #   if (nrow(port_data) > 0) {
  #     # Get counts for all dates by symbol
  #     symbol_date_counts <- port_data[, .N, by = .(symbol, scan_date)]
  #     
  #     # Get all unique symbols and dates
  #     all_symbols <- unique(symbol_date_counts$symbol)
  #     all_dates <- sort(unique(symbol_date_counts$scan_date))
  #     
  #     # Create a complete grid of all symbol-date combinations
  #     complete_grid <- CJ(symbol = all_symbols, scan_date = all_dates)
  #     
  #     # Merge with actual counts and fill missing with 0
  #     symbol_counts_complete <- merge(complete_grid, symbol_date_counts, 
  #                                     by = c("symbol", "scan_date"), all.x = TRUE)
  #     symbol_counts_complete[is.na(N), N := 0]
  #     
  #     # Reshape to wide format for plotting
  #     library(data.table)
  #     count_matrix_wide <- dcast(symbol_counts_complete, symbol ~ scan_date, value.var = "N")
  #     
  #     # Convert to matrix (excluding symbol column)
  #     symbol_names <- count_matrix_wide$symbol
  #     count_matrix <- as.matrix(count_matrix_wide[, -1])
  #     rownames(count_matrix) <- symbol_names
  #     
  #     # Transpose for barplot (dates as groups, symbols as series)
  #     count_matrix_t <- t(count_matrix)
  #     
  #     # Create colors for each symbol
  #     colors <- rainbow(length(all_symbols))
  #     
  #     # Create grouped bar plot
  #     barplot(count_matrix_t,
  #             main = "Portfolio Scan Activity - All Dates",
  #             ylab = "Number of Scans",
  #             xlab = "Symbols",
  #             col = colors,
  #             beside = TRUE,
  #             legend.text = format(as.Date(all_dates), "%m-%d"),
  #             args.legend = list(x = "topright", cex = 0.7),
  #             las = 2,
  #             cex.names = 0.8)
  #     
  #     # Add grid for better readability
  #     grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")
  #   }
  # })
  # 
  
  # output$portfolio_table <- DT::renderDataTable({
  #   port_data <- portfolio_data()$data
  #   if (nrow(port_data) > 0) {
  #     summary_table <- port_data[, .(
  #       total_scans = .N,
  #       scan_types = length(unique(scan_symbol)),
  #       latest_scan = max(scan_date)
  #     ), by = symbol]
  #     DT::datatable(summary_table, options = list(pageLength = 10))
  #   }
  # })
 
  output$portfolio_table3 <- DT::renderDataTable({
    port_data <- portfolio_data()$data
    if (nrow(port_data) > 0) {
      # Get the last 2 unique dates (representing this week vs last week)
      weekly_dates <- sort(unique(port_data$scan_date), decreasing = TRUE)[1:2]
      this_week_date <- weekly_dates[1]
      last_week_date <- weekly_dates[2]
      
      # Get scan details for this week and last week
      this_week_scans <- port_data[scan_date == this_week_date, 
                                   .(scan_types = paste(unique(scan_symbol), collapse = ", "),
                                     scan_count = .N), 
                                   by = symbol]
      setnames(this_week_scans, c("scan_types", "scan_count"), c("this_week_scans", "this_week_count"))
      
      last_week_scans <- port_data[scan_date == last_week_date, 
                                   .(scan_types = paste(unique(scan_symbol), collapse = ", "),
                                     scan_count = .N), 
                                   by = symbol]
      setnames(last_week_scans, c("scan_types", "scan_count"), c("last_week_scans", "last_week_count"))
      
      # Merge the data to show side-by-side comparison
      weekly_comparison <- merge(this_week_scans, last_week_scans, by = "symbol", all = TRUE)
      
      # Replace NA values with appropriate defaults
      weekly_comparison[is.na(this_week_scans), this_week_scans := "None"]
      weekly_comparison[is.na(last_week_scans), last_week_scans := "None"]
      weekly_comparison[is.na(this_week_count), this_week_count := 0]
      weekly_comparison[is.na(last_week_count), last_week_count := 0]
      
      # Add comparison metrics
      weekly_comparison[, `:=`(
        change = this_week_count - last_week_count,
        status = case_when(
          this_week_count > last_week_count ~ "📈 Increased",
          this_week_count < last_week_count ~ "📉 Decreased", 
          this_week_count == last_week_count & this_week_count > 0 ~ "➡️ Same",
          this_week_count == 0 & last_week_count == 0 ~ "❌ No Activity",
          this_week_count > 0 & last_week_count == 0 ~ "🆕 New Entry",
          this_week_count == 0 & last_week_count > 0 ~ "🚫 Dropped Out"
        ),
        new_scans = "",
        dropped_scans = ""
      )]
      
      # Identify new and dropped scan types
      for(i in 1:nrow(weekly_comparison)) {
        this_week_types <- if(weekly_comparison$this_week_scans[i] != "None") {
          trimws(strsplit(weekly_comparison$this_week_scans[i], ",")[[1]])
        } else { character(0) }
        
        last_week_types <- if(weekly_comparison$last_week_scans[i] != "None") {
          trimws(strsplit(weekly_comparison$last_week_scans[i], ",")[[1]])
        } else { character(0) }
        
        new_types <- setdiff(this_week_types, last_week_types)
        dropped_types <- setdiff(last_week_types, this_week_types)
        
        weekly_comparison$new_scans[i] <- if(length(new_types) > 0) paste(new_types, collapse = ", ") else ""
        weekly_comparison$dropped_scans[i] <- if(length(dropped_types) > 0) paste(dropped_types, collapse = ", ") else ""
      }
      
      # Reorder columns for better presentation
      setcolorder(weekly_comparison, c("symbol", "status", "change", 
                                       "this_week_count", "this_week_scans",
                                       "last_week_count", "last_week_scans", 
                                       "new_scans", "dropped_scans"))
      
      # Create enhanced datatable
      library(DT)
      DT::datatable(weekly_comparison, 
                    options = list(
                      pageLength = 15, 
                      dom = 'Bfrtip',
                      scrollX = TRUE,
                      columnDefs = list(
                        list(width = '100px', targets = c(0, 1, 2, 3, 5)),
                        list(width = '200px', targets = c(4, 6, 7, 8))
                      )
                    ),
                    caption = paste0("📊 Weekly Scan Comparison: ", 
                                     format(as.Date(this_week_date), "%m/%d"), 
                                     " vs ", 
                                     format(as.Date(last_week_date), "%m/%d")))  %>%
        DT::formatStyle('status',
                        backgroundColor = DT::styleEqual(
                          c('📈 Increased', '📉 Decreased', '🆕 New Entry', '🚫 Dropped Out', '➡️ Same', '❌ No Activity'), 
                          c('#d4edda', '#f8d7da', '#cce5ff', '#ffcccc', '#f0f0f0', '#e6e6e6')
                        )) %>%
        DT::formatStyle('change',
                        color = DT::styleInterval(c(-0.1, 0.1), c('red', 'black', 'green')),
                        fontWeight = 'bold') %>%
        DT::formatStyle('new_scans',
                        backgroundColor = DT::styleEqual('', 'white', '#e8f5e8')) %>%
        DT::formatStyle('dropped_scans', 
                        backgroundColor = DT::styleEqual('', 'white', '#ffe8e8'))
      
      # # Optional: Summary statistics
      # cat("\n=== WEEKLY SCAN SUMMARY ===\n")
      # cat("This Week Date:", format(as.Date(this_week_date), "%Y-%m-%d"), "\n")
      # cat("Last Week Date:", format(as.Date(last_week_date), "%Y-%m-%d"), "\n")
      # cat("Total Symbols This Week:", nrow(weekly_comparison[this_week_count > 0]), "\n")
      # cat("Total Symbols Last Week:", nrow(weekly_comparison[last_week_count > 0]), "\n")
      # cat("New Entries:", nrow(weekly_comparison[status == "🆕 New Entry"]), "\n")
      # cat("Dropped Out:", nrow(weekly_comparison[status == "🚫 Dropped Out"]), "\n")
      # cat("Increased Activity:", nrow(weekly_comparison[status == "📈 Increased"]), "\n")
      # cat("Decreased Activity:", nrow(weekly_comparison[status == "📉 Decreased"]), "\n")
    }
  })
  
  output$portfolio_table <- DT::renderDataTable({
    port_data <- portfolio_data()$data
    if (nrow(port_data) > 0) {
      # ===== CREATIVE IDEA 1: PORTFOLIO HEATMAP =====
      # Show scan intensity over time with color coding
      heatmap_data <- port_data[, .N, by = .(symbol, scan_date)]
      heatmap_matrix <- dcast(heatmap_data, symbol ~ scan_date, value.var = "N", fill = 0)
      heatmap_matrix_only <- as.matrix(heatmap_matrix[, -1])
      rownames(heatmap_matrix_only) <- heatmap_matrix$symbol
      
      # Create heatmap
      heatmap(heatmap_matrix_only, 
              main = "Portfolio Scan Intensity Heatmap",
              xlab = "Dates", ylab = "Symbols",
              col = colorRampPalette(c("white", "yellow", "red"))(50))
      
      # ===== CREATIVE IDEA 2: PORTFOLIO MOMENTUM DASHBOARD =====
      momentum_analysis <- port_data[, {
        dates <- sort(unique(scan_date))
        if(length(dates) >= 2) {
          recent_count <- sum(scan_date == dates[length(dates)])
          prev_count <- sum(scan_date == dates[length(dates)-1])
          momentum_score <- ifelse(prev_count > 0, (recent_count/prev_count)*100, recent_count*100)
          trend <- ifelse(momentum_score > 120, "🔥 Hot", 
                          ifelse(momentum_score < 80, "❄️ Cooling", "➡️ Stable"))
        } else {
          momentum_score <- 100
          trend <- "➡️ New"
        }
        
        list(
          total_scans = .N,
          unique_scan_types = length(unique(scan_symbol)),
          momentum_score = round(momentum_score, 1),
          trend = trend,
          scan_frequency = round(.N / length(unique(scan_date)), 1),
          latest_scan = max(scan_date),
          scan_consistency = round(length(unique(scan_date)) / max(length(unique(port_data$scan_date))), 2)
        )
      }, by = symbol]
      
      DT::datatable(momentum_analysis, 
                    options = list(pageLength = 15, dom = 'Bfrtip'),
                    caption = "🚀 Portfolio Momentum Dashboard") %>%
        DT::formatStyle('trend',
                        backgroundColor = DT::styleEqual(c('🔥 Hot', '❄️ Cooling', '➡️ Stable'), 
                                                         c('#ffcccc', '#ccddff', '#f0f0f0')))
      
    }
  })
  
  
  output$portfolio_table2 <- DT::renderDataTable({
    port_data <- portfolio_data()$data
    if (nrow(port_data) > 0) {
      pattern_analysis <- port_data[, {
        scan_dates <- sort(unique(scan_date))
        scan_types <- unique(scan_symbol)
        
        # Calculate scan patterns
        avg_gap <- if(length(scan_dates) > 1) {
          mean(diff(as.Date(scan_dates)))
        } else { NA }
        
        # Scan diversity score
        diversity_score <- length(scan_types) / length(unique(port_data$scan_symbol)) * 100
        
        # Recent activity (last 7 days equivalent)
        recent_dates <- tail(sort(unique(port_data$scan_date)), 3)
        recent_activity <- sum(scan_date %in% recent_dates)
        
        list(
          total_activity = .N,
          scan_diversity = paste0(round(diversity_score, 1), "%"),
          avg_scan_gap = round(avg_gap, 1),
          recent_activity = recent_activity,
          scan_pattern = case_when(
            avg_gap <= 1 ~ "⚡ High Frequency",
            avg_gap <= 3 ~ "🔄 Regular",
            avg_gap <= 7 ~ "📅 Weekly",
            TRUE ~ "🐌 Sporadic"
          ),
          first_seen = min(scan_date),
          latest_scan = max(scan_date)
        )
      }, by = symbol]      
    }
  })
  

  output$portfolio_metrics <- renderText({
    port_data <- portfolio_data()
    symbols <- port_data$symbols
    scans <- port_data$data
    
    coverage <- length(unique(scans$symbol)) / length(symbols) * 100
    avg_scans <- nrow(scans) / length(symbols)
    
    paste0("Portfolio Coverage: ", round(coverage, 1), "%\n",
           "Average Scans per Symbol: ", round(avg_scans, 1), "\n",
           "Total Portfolio Scans: ", nrow(scans))
  })
  
  # Tab 6: Signal Strength Calculator
  output$weight_inputs <- renderUI({
    data <- scan_data()
    scan_types <- unique(data$scan_symbol)
    
    weight_inputs <- lapply(scan_types, function(scan_type) {
      numericInput(paste0("weight_", scan_type), 
                   paste("Weight for", scan_type, ":"),
                   value = 0.05, min = 0, max = 1, step = 0.05)
    })
    
    do.call(tagList, weight_inputs)
  })
  
  signal_data <- eventReactive(input$signal_calculate, {
    data <- scan_data()
    scan_types <- unique(data$scan_symbol)
    
    # Get weights from inputs
    weights <- sapply(scan_types, function(st) {
      weight_input <- paste0("weight_", st)
      if (!is.null(input[[weight_input]])) {
        input[[weight_input]]
      } else {
        0.05
      }
    })
    
    # Calculate weighted scores
    # symbol_scores <- data[scan_date==max(scan_date), .(
    #   weighted_score = sum(weights[scan_symbol]),
    #   total_scans = .N
    # ), by = symbol][order(-weighted_score)]
    
    # Get the last few dates for temporal analysis
    recent_dates <- sort(unique(data$scan_date), decreasing = TRUE)[1:min(5, length(unique(data$scan_date)))]
    max_date <- max(data$scan_date)
    
    # Sophisticated multi-factor scoring
    sophisticated_scores <- data[scan_date %in% recent_dates, {
      # Current day analysis
      current_day <- .SD[scan_date == max_date]
      
      if(nrow(current_day) > 0) {
        # 1. BASE WEIGHTED SCORE (your original logic)
        base_weighted_score <- sum(weights[current_day$scan_symbol])
        
        # 2. SCAN DIVERSITY BONUS (more scan types = higher confidence)
        diversity_count <- length(unique(current_day$scan_symbol))
        diversity_bonus <- diversity_count * 0.15 * base_weighted_score
        
        # 3. FREQUENCY MULTIPLIER (multiple occurrences of same scan type)
        scan_frequencies <- table(current_day$scan_symbol)
        frequency_multiplier <- 1 + sum(pmax(0, scan_frequencies - 1) * 0.1)
        
        # 4. MOMENTUM SCORE (comparing recent activity)
        all_dates <- sort(unique(.SD$scan_date))
        if(length(all_dates) >= 2) {
          recent_activity <- sum(.SD$scan_date >= all_dates[max(1, length(all_dates)-2)])
          historical_activity <- sum(.SD$scan_date < all_dates[max(1, length(all_dates)-2)])
          momentum_factor <- ifelse(historical_activity > 0, 
                                    1 + (recent_activity - historical_activity) / historical_activity * 0.2,
                                    1 + recent_activity * 0.1)
        } else {
          momentum_factor <- 1
        }
        
        # 5. CONSISTENCY SCORE (appears across multiple recent dates)
        unique_recent_dates <- length(unique(.SD$scan_date))
        consistency_multiplier <- 1 + (unique_recent_dates - 1) * 0.05
        
        # 6. RECENCY DECAY (weight recent dates more heavily)
        recency_weights <- .SD[, {
          days_ago <- as.numeric(max_date - scan_date)
          decay_factor <- exp(-days_ago * 0.1)  # exponential decay
          sum(weights[scan_symbol] * decay_factor)
        }]
        
        # 7. VOLATILITY ADJUSTMENT (penalize erratic patterns)
        daily_counts <- .SD[, .N, by = scan_date]
        volatility_penalty <- ifelse(nrow(daily_counts) > 1, 
                                     1 - (sd(daily_counts$N) / mean(daily_counts$N)) * 0.1,
                                     1)
        volatility_penalty <- pmax(0.7, volatility_penalty)  # cap penalty
        
        # 8. PREMIUM SCAN TYPE BONUS (weight high-value scan types more)
        premium_scans <- c("Breakout", "Volume", "Momentum")  # adjust based on your scan types
        premium_bonus <- sum(current_day$scan_symbol %in% premium_scans) * 0.1 * base_weighted_score
        
        # FINAL SOPHISTICATED SCORE CALCULATION
        sophisticated_score <- (
          (base_weighted_score + diversity_bonus + premium_bonus) * 
            frequency_multiplier * 
            momentum_factor * 
            consistency_multiplier * 
            volatility_penalty
        ) + recency_weights * 0.3
        
        list(
          # Final scores
          sophisticated_score = as.numeric(round(sophisticated_score, 2)),
          base_weighted_score = as.numeric(round(base_weighted_score, 2)),
          
          # Component breakdowns
          diversity_score = as.numeric(round(diversity_bonus, 2)),
          frequency_mult = as.numeric(round(frequency_multiplier, 3)),
          momentum_factor = as.numeric(round(momentum_factor, 3)),
          consistency_mult = as.numeric(round(consistency_multiplier, 3)),
          volatility_adj = as.numeric(round(volatility_penalty, 3)),
          recency_component = as.numeric(round(recency_weights, 2)),
          
          # Additional metrics
          total_scans_today = as.integer(nrow(current_day)),
          unique_scan_types = as.integer(diversity_count),
          days_active = as.integer(unique_recent_dates),
          total_recent_scans = as.integer(.N),
          
          # Confidence level
          confidence_level = as.character(case_when(
            sophisticated_score >= base_weighted_score * 1.5 ~ "Very High",
            sophisticated_score >= base_weighted_score * 1.2 ~ "High",
            sophisticated_score >= base_weighted_score * 0.8 ~ "Medium",
            TRUE ~ "Low"
          ))
        )
      } else {
        # No activity on current date
        list(
          sophisticated_score = 0.0,
          base_weighted_score = 0.0,
          diversity_score = 0.0,
          frequency_mult = 1.0,
          momentum_factor = 1.0,
          consistency_mult = 1.0,
          volatility_adj = 1.0,
          recency_component = 0.0,
          total_scans_today = 0L,
          unique_scan_types = 0L,
          days_active = as.integer(length(unique(.SD$scan_date))),
          total_recent_scans = as.integer(.N),
          confidence_level = "None"
        )
      }
    }, by = symbol][order(-sophisticated_score)]
    
    
    
    # Alternative: Machine Learning-based scoring (if you want to get really advanced)
    ml_based_scores <- data[scan_date %in% recent_dates, {
      current_day <- .SD[scan_date == max_date]
      
      if(nrow(current_day) > 0) {
        # Feature engineering
        features <- list(
          base_weight = sum(weights[current_day$scan_symbol]),
          scan_diversity = length(unique(current_day$scan_symbol)),
          scan_frequency = nrow(current_day),
          momentum_3day = sum(.SD$scan_date >= (max_date - 2)),
          consistency = length(unique(.SD$scan_date)),
          max_single_weight = max(weights[current_day$scan_symbol]),
          weight_variance = var(weights[current_day$scan_symbol])
        )
        
        # Simple ensemble scoring (you could replace with actual ML model)
        ensemble_score <- (
          features$base_weight * 0.4 +
            features$scan_diversity^1.2 * 5 +
            log1p(features$scan_frequency) * 10 +
            features$momentum_3day * 3 +
            features$consistency * 2 +
            features$max_single_weight * 0.3
        ) * (1 - pmin(0.3, features$weight_variance / 100))
        
        list(
          ml_score = as.numeric(round(ensemble_score, 2)),
          base_weight = as.numeric(features$base_weight),
          scan_diversity = as.integer(features$scan_diversity),
          scan_frequency = as.integer(features$scan_frequency),
          momentum_3day = as.integer(features$momentum_3day),
          consistency = as.integer(features$consistency)
        )
      } else {
        list(ml_score = 0.0, base_weight = 0.0, scan_diversity = 0L, 
             scan_frequency = 0L, momentum_3day = 0L, consistency = 0L)
      }
    }, by = symbol][order(-ml_score)]
    
    # Display results
    print("=== SOPHISTICATED SCORING RESULTS ===")
    print(head(sophisticated_scores[, .(symbol, sophisticated_score, base_weighted_score, 
                                        confidence_level, unique_scan_types, momentum_factor)], 10))
    
    print("\n=== ML-BASED SCORING RESULTS ===")
    print(head(ml_based_scores, 10))
    #return(symbol_scores)
    return(sophisticated_scores)
  })
  
  output$signal_plot <- renderPlot({
    data <- signal_data()
    if (nrow(data) > 0) {
      top_20 <- head(data, 20)
      barplot(top_20$base_weighted_score, names.arg = top_20$symbol,
              las = 2, main = "Signal Strength Scores", col = "purple",
              ylab = "Weighted Score")
    }
  })
  
  output$signal_table <- DT::renderDataTable({
    data <- signal_data()
    strong_signals <- data[base_weighted_score >= input$signal_min_score]
    DT::datatable(strong_signals, options = list(pageLength = 15))
  })
  
  # Tab 7: Historical Analysis
  output$historical_plot <- renderPlot({
    plot(1:10, rnorm(10), main = "Historical Scan Performance Analysis",
         xlab = "Time", ylab = "Performance Metric")
    text(5, 0, "Historical performance\nanalysis would be\ndisplayed here", cex = 1.2)
  })
  
  output$historical_table <- DT::renderDataTable({
    data.frame(
      Scan_Type = c("CarusoInsights", "roc_mdt25"),
      Success_Rate = c("75%", "68%"),
      Avg_Days_to_Move = c(3.2, 4.1),
      Best_Performance = c("15%", "12%")
    )
  })
  
  # Alert data processing
  alert_data <- eventReactive(input$alert_setup, {
    data <- scan_data()
    alerts <- data.table()
    
    # Recent data (last 7 days for alerts)
    recent_data <- data[scan_date >= (Sys.Date() - 5)]
    
    # Alert 1: Frequency threshold alerts
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
    
    # Alert 2: New symbols appearing in watched scan types
    if (!is.null(input$alert_scan_types) && length(input$alert_scan_types) > 0) {
      # Get symbols from last week vs previous weeks
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
    
    # Alert 3: Unusual activity (symbols with 3x normal frequency)
    if (input$alert_new_symbols) {
      # Calculate normal frequency (last 30 days average)
      normal_period <- data[scan_date >= (Sys.Date() - 30) & scan_date < (Sys.Date() - 7)]
      recent_period <- data[scan_date >= (Sys.Date() - 7)]
      
      if (nrow(normal_period) > 0 && nrow(recent_period) > 0) {
        normal_freq <- normal_period[, .(avg_weekly = .N / 3), by = symbol]  # 3 weeks average
        recent_freq <- recent_period[, .N, by = symbol]
        
        activity_comparison <- merge(recent_freq, normal_freq, by = "symbol", all.x = TRUE)
        activity_comparison[is.na(avg_weekly), avg_weekly := 0.5]  # New symbols get low baseline
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
    
    # Sort alerts by priority and scan count
    if (nrow(alerts) > 0) {
      alerts[, priority_order := ifelse(priority == "High", 1, 2)]
      alerts <- alerts[order(priority_order, -scan_count)]
    }
    
    return(alerts)
  })
  
  # Alert summary for recent activity
  recent_alert_summary <- reactive({
    data <- scan_data()
    
    # Last 24 hours activity
    last_24h <- data[scan_date >= (Sys.Date() - 1)]
    
    # Last week activity  
    last_week <- data[scan_date >= (Sys.Date() - 7)]
    
    # Create timeline data
    timeline_data <- data[scan_date >= (Sys.Date() - 30)]
    daily_counts <- timeline_data[, .N, by = scan_date][order(scan_date)]
    
    list(
      last_24h = last_24h,
      last_week = last_week,
      timeline = daily_counts
    )
  })
  
  output$alert_table <- DT::renderDataTable({
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
      
      DT::datatable(display_alerts, 
                    options = list(pageLength = 15, order = list(list(5, 'desc'))),
                    rownames = FALSE) %>%
        DT::formatStyle('Priority',
                        backgroundColor = DT::styleEqual(c('High', 'Medium'), 
                                                         c('#ffcccc', '#ffffcc')))
    } else {
      # Show recent activity when no alerts
      data <- scan_data()
      recent_activity <- data[scan_date >= (Sys.Date() - 7)][order(-scan_date)]
      
      if (nrow(recent_activity) > 0) {
        recent_display <- recent_activity[, .(
          Date = scan_date,
          Symbol = symbol,
          Scan_Type = scan_symbol,
          Status = "Recent Activity"
        )]
        
        DT::datatable(head(recent_display, 50), 
                      options = list(pageLength = 15),
                      caption = "Recent Scan Activity (No Active Alerts)")
      } else {
        DT::datatable(data.frame(Message = "No recent scan activity found"))
      }
    }
  })
  
  output$alert_timeline <- renderPlot({
    summary_data <- recent_alert_summary()
    timeline <- summary_data$timeline
    
    if (nrow(timeline) > 0) {
      plot(timeline$scan_date, timeline$N, type = "l", 
           main = "Daily Scan Activity (Last 30 Days)",
           xlab = "Date", ylab = "Number of Scans",
           col = "blue", lwd = 2)
      
      # Add points for recent activity
      points(timeline$scan_date, timeline$N, pch = 16, col = "blue")
      
      # Highlight last 7 days
      recent_dates <- timeline[scan_date >= (Sys.Date() - 7)]
      if (nrow(recent_dates) > 0) {
        points(recent_dates$scan_date, recent_dates$N, pch = 16, col = "red", cex = 1.2)
      }
      
      # Add trend line
      if (nrow(timeline) > 1) {
        trend_line <- lm(N ~ as.numeric(scan_date), data = timeline)
        abline(trend_line, col = "gray", lty = 2)
      }
      
      legend("topright", legend = c("Daily Activity", "Last 7 Days", "Trend"), 
             col = c("blue", "red", "gray"), pch = c(16, 16, NA), lty = c(1, NA, 2))
    } else {
      plot(1, 1, type = "n", main = "No Timeline Data Available",
           xlab = "Date", ylab = "Activity")
      text(1, 1, "No scan data found for timeline", cex = 1.2)
    }
  })
  
  output$active_alerts <- renderText({
    alerts <- alert_data()
    summary_data <- recent_alert_summary()
    
    # Count different types of alerts
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
    
    # Add recent activity summary
    recent_summary <- paste0(
      "📊 RECENT ACTIVITY:\n",
      "Last 24h Scans: ", nrow(summary_data$last_24h), "\n",
      "Last Week Scans: ", nrow(summary_data$last_week), "\n",
      "Active Symbols: ", length(unique(summary_data$last_week$symbol)), "\n",
      "Scan Types: ", length(unique(summary_data$last_week$scan_symbol))
    )
    
    paste(alert_summary, recent_summary)
  })
  
  
  # Tab 10: Bulk Symbol Scanner Analysis
  
  # Parse uploaded file
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
  
  # Update scan filter choices
  observe({
    data <- scan_data()
    scan_types <- unique(data$scan_symbol)
    updateSelectInput(session, "bulk_scan_filter", choices = scan_types)
  })
  
  # Bulk analysis data processing
  bulk_data <- eventReactive(input$bulk_analyze, {
    data <- scan_data()
    
    # Get symbols from text input or file upload
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
    
    # Filter by date range
    if (!is.null(input$bulk_date_range)) {
      data <- data[scan_date >= input$bulk_date_range[1] & 
                     scan_date <= input$bulk_date_range[2]]
    }
    
    # Filter by scan types if selected
    if (!is.null(input$bulk_scan_filter) && length(input$bulk_scan_filter) > 0) {
      data <- data[scan_symbol %in% input$bulk_scan_filter]
    }
    
    # Create comprehensive results table
    symbol_results <- data.table(symbol = all_symbols)
    
    # Get scan data for each symbol
    scan_summary <- data[symbol %in% all_symbols, .(
      total_scans = .N,
      scan_types = paste(unique(scan_symbol), collapse = " | "),
      scan_type_count = length(unique(scan_symbol)),
      first_seen = min(scan_date),
      last_seen = max(scan_date),
      days_active = as.numeric(max(scan_date) - min(scan_date)) + 1,
      avg_scans_per_week = round(.N / (as.numeric(max(scan_date) - min(scan_date) + 1) / 7), 2)
    ), by = symbol]
    
    # Merge with all requested symbols
    results <- merge(symbol_results, scan_summary, by = "symbol", all.x = TRUE)
    
    # Fill missing values
    results[is.na(total_scans), `:=`(
      total_scans = 0,
      scan_types = "Not Found",
      scan_type_count = 0,
      first_seen = as.Date(NA),
      last_seen = as.Date(NA),
      days_active = 0,
      avg_scans_per_week = 0
    )]
    
    # Apply minimum scans filter
    filtered_results <- results[total_scans >= input$bulk_min_scans]
    
    # Create scan type matrix
    matrix_data <- data[symbol %in% all_symbols, .N, by = .(symbol, scan_symbol)]
    scan_matrix <- dcast(matrix_data, symbol ~ scan_symbol, value.var = "N", fill = 0)
    
    # Add symbols not found in any scans
    missing_symbols <- setdiff(all_symbols, scan_matrix$symbol)
    if (length(missing_symbols) > 0) {
      scan_types <- unique(data$scan_symbol)
      missing_matrix <- data.table(symbol = missing_symbols)
      for (st in scan_types) {
        missing_matrix[[st]] <- 0
      }
      scan_matrix <- rbind(scan_matrix, missing_matrix, fill = TRUE)
    }
    
    return(list(
      results = filtered_results,
      all_results = results,
      matrix = scan_matrix,
      symbols_requested = all_symbols,
      symbols_found = sum(results$total_scans > 0),
      total_scans = sum(results$total_scans, na.rm = TRUE)
    ))
  })
  
  output$bulk_results_table <- DT::renderDataTable({
    bulk_result <- bulk_data()
    
    if (is.null(bulk_result)) {
      return(DT::datatable(data.frame(Message = "Please enter symbols and click 'Run Bulk Analysis'")))
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
      
      DT::datatable(display_table,
                    options = list(pageLength = 25, scrollX = TRUE, dom = 'Bfrtip',
                                   buttons = c('copy', 'csv', 'excel')),
                    rownames = FALSE,
                    extensions = 'Buttons') %>%
        DT::formatStyle('Total_Scans',
                        backgroundColor = DT::styleInterval(c(1, 3, 7, 15), 
                                                            c('#ffffff', '#e6ffe6', '#ccffcc', '#99ff99', '#66ff66'))) %>%
        DT::formatStyle('Scan_Types_Found',
                        backgroundColor = DT::styleEqual('Not Found', '#ffeeee'))
    } else {
      DT::datatable(data.frame(Message = "No symbols meet the minimum scan criteria"))
    }
  })
  
  output$bulk_matrix_table <- DT::renderDataTable({
    bulk_result <- bulk_data()
    
    if (is.null(bulk_result) || is.null(bulk_result$matrix)) {
      return(DT::datatable(data.frame(Message = "No matrix data available")))
    }
    
    matrix_table <- bulk_result$matrix
    
    # Add total column
    scan_cols <- names(matrix_table)[names(matrix_table) != "symbol"]
    matrix_table[, Total := rowSums(.SD), .SDcols = scan_cols]
    
    # Order by total scans
    matrix_table <- matrix_table[order(-Total)]
    
    DT::datatable(matrix_table,
                  options = list(pageLength = 20, scrollX = TRUE),
                  rownames = FALSE,
                  caption = "Symbol vs Scan Type Matrix (0 = Not Found, Numbers = Scan Count)") %>%
      DT::formatStyle(scan_cols,
                      backgroundColor = DT::styleInterval(c(0.5, 2, 5, 10), 
                                                          c('#ffffff', '#ffffcc', '#ffccaa', '#ff9999', '#ff6666')))
  })
  
  output$bulk_coverage_plot <- renderPlot({
    bulk_result <- bulk_data()
    
    if (is.null(bulk_result)) {
      plot(1, 1, type = "n", main = "No Data", xlab = "", ylab = "")
      return()
    }
    
    found_count <- bulk_result$symbols_found
    total_count <- length(bulk_result$symbols_requested)
    not_found <- total_count - found_count
    
    # Pie chart of coverage
    pie_data <- c(found_count, not_found)
    pie_labels <- c(paste("Found (", found_count, ")", sep = ""),
                    paste("Not Found (", not_found, ")", sep = ""))
    pie_colors <- c("#66cc66", "#ff6666")
    
    pie(pie_data, labels = pie_labels, col = pie_colors,
        main = paste("Symbol Coverage:", round(found_count/total_count*100, 1), "%"))
  })
  
  output$bulk_distribution_plot <- renderPlot({
    bulk_result <- bulk_data()
    
    if (is.null(bulk_result)) {
      plot(1, 1, type = "n", main = "No Data", xlab = "", ylab = "")
      return()
    }
    
    results <- bulk_result$all_results[total_scans > 0]
    
    if (nrow(results) > 0) {
      # Histogram of scan frequencies
      hist(results$total_scans, 
           breaks = max(10, min(30, length(unique(results$total_scans)))),
           main = "Distribution of Scan Frequencies",
           xlab = "Number of Scans", ylab = "Number of Symbols",
           col = "lightblue", border = "darkblue")
      
      # Add mean line
      abline(v = mean(results$total_scans), col = "red", lwd = 2, lty = 2)
      text(mean(results$total_scans), max(table(results$total_scans)) * 0.8,
           paste("Mean:", round(mean(results$total_scans), 1)), pos = 4, col = "red")
    }
  })
  
  output$bulk_summary_stats <- renderText({
    bulk_result <- bulk_data()
    
    if (is.null(bulk_result)) {
      return("No analysis performed yet.\nPlease enter symbols and click 'Run Bulk Analysis'.")
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
      "Total Scan Instances: ", total_scans, "\n\n",
      
      "📈 ACTIVITY BREAKDOWN:\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
    )
    
    if (nrow(results) > 0) {
      stats_text <- paste0(stats_text,
                           "Average Scans per Symbol: ", round(mean(results$total_scans), 1), "\n",
                           "Median Scans per Symbol: ", median(results$total_scans), "\n",
                           "Most Active Symbol: ", results$symbol[which.max(results$total_scans)], 
                           " (", max(results$total_scans), " scans)\n",
                           "Symbols with 1 scan: ", sum(results$total_scans == 1), "\n",
                           "Symbols with 5+ scans: ", sum(results$total_scans >= 5), "\n",
                           "Symbols with 10+ scans: ", sum(results$total_scans >= 10), "\n\n",
                           
                           "📅 TEMPORAL ANALYSIS:\n",
                           "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
                           "Average Days Active: ", round(mean(results$days_active, na.rm = TRUE), 1), "\n",
                           "Average Scans/Week: ", round(mean(results$avg_scans_per_week, na.rm = TRUE), 2)
      )
    } else {
      stats_text <- paste0(stats_text, "No symbols found with scan activity in the selected period.")
    }
    
    return(stats_text)
  })
  
  # Download handler for bulk results
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
  
  # Tab 11: Trading Log System
  
  # Initialize trading log data storage
  trading_log <- reactiveVal(data.table(
    trade_id = integer(),
    symbol = character(),
    action = character(),
    trade_date = as.Date(character()),
    trade_time = as.POSIXct(character()),
    entry_price = numeric(),
    exit_price = numeric(),
    quantity = integer(),
    target_price = numeric(),
    stop_loss = numeric(),
    entry_criteria = character(),
    scan_source = character(),
    notes = character(),
    status = character(),
    pnl = numeric(),
    pnl_percent = numeric(),
    created_at = as.POSIXct(character()),
    updated_at = as.POSIXct(character())
  ))
  
  # Update symbol choices from scan data
  observe({
    data <- scan_data()
    symbols <- sort(unique(data$symbol))
    scan_types <- unique(data$scan_symbol)
    
    updateSelectizeInput(session, "trade_symbol", choices = symbols)
    updateSelectInput(session, "trade_scan_source", choices = scan_types)
    
    # Update filter choices
    current_log <- trading_log()
    if (nrow(current_log) > 0) {
      log_symbols <- sort(unique(current_log$symbol))
      updateSelectInput(session, "quick_symbol_filter", choices = log_symbols)
    }
  })
  
  # Add new trade
  observeEvent(input$add_trade, {
    if (input$trade_symbol != "" && input$trade_price > 0 && input$trade_quantity > 0) {
      current_log <- trading_log()
      
      new_trade <- data.table(
        trade_id = ifelse(nrow(current_log) == 0, 1, max(current_log$trade_id) + 1),
        symbol = input$trade_symbol,
        action = input$trade_action,
        trade_date = input$trade_date,
        trade_time = as.POSIXct(paste(input$trade_date, paste0(input$trade_time, ":00"))),
        #trade_time = as.POSIXct(paste(input$trade_date, format(input$trade_time, "%H:%M:%S"))),
        entry_price = input$trade_price,
        exit_price = NA_real_,
        quantity = input$trade_quantity,
        target_price = ifelse(input$trade_target > 0, input$trade_target, NA_real_),
        stop_loss = ifelse(input$trade_stop > 0, input$trade_stop, NA_real_),
        entry_criteria = input$trade_entry_criteria,
        scan_source = ifelse(is.null(input$trade_scan_source), "", paste(input$trade_scan_source, collapse = "|")),
        notes = input$trade_notes,
        status = "open",
        pnl = 0,
        pnl_percent = 0,
        created_at = Sys.time(),
        updated_at = Sys.time()
      )
      
      updated_log <- rbind(current_log, new_trade)
      trading_log(updated_log)
      
      # Clear form
      updateSelectizeInput(session, "trade_symbol", selected = "")
      updateNumericInput(session, "trade_price", value = 0)
      updateNumericInput(session, "trade_quantity", value = 100)
      updateNumericInput(session, "trade_target", value = 0)
      updateNumericInput(session, "trade_stop", value = 0)
      updateTextAreaInput(session, "trade_notes", value = "")
      updateTextInput(session, "trade_time", value = format(Sys.time(), "%H:%M"))
      
      
      showNotification("Trade added successfully!", type = "message")
    } else {
      showNotification("Please fill in all required fields (Symbol, Price, Quantity)", type = "error")
    }
  })
  
  # Get selected trade for editing
  selected_trade <- reactive({
    if (!is.null(input$trading_log_table_rows_selected)) {
      current_log <- filtered_trading_log()
      if (length(input$trading_log_table_rows_selected) > 0) {
        return(current_log[input$trading_log_table_rows_selected[1]])
      }
    }
    return(NULL)
  })
  
  # Update trade
  observeEvent(input$update_trade, {
    selected <- selected_trade()
    if (!is.null(selected)) {
      current_log <- trading_log()
      trade_id <- selected$trade_id
      
      # Update the selected trade
      current_log[trade_id == trade_id, `:=`(
        symbol = input$trade_symbol,
        action = input$trade_action,
        trade_date = input$trade_date,
        trade_time = as.POSIXct(paste(input$trade_date, paste0(input$trade_time, ":00"))),
        #trade_time = as.POSIXct(paste(input$trade_date, format(input$trade_time, "%H:%M:%S"))),
        entry_price = input$trade_price,
        quantity = input$trade_quantity,
        target_price = ifelse(input$trade_target > 0, input$trade_target, NA_real_),
        stop_loss = ifelse(input$trade_stop > 0, input$trade_stop, NA_real_),
        entry_criteria = input$trade_entry_criteria,
        scan_source = ifelse(is.null(input$trade_scan_source), "", paste(input$trade_scan_source, collapse = "|")),
        notes = input$trade_notes,
        updated_at = Sys.time()
      )]
      
      trading_log(current_log)
      showNotification("Trade updated successfully!", type = "message")
    } else {
      showNotification("Please select a trade to update", type = "warning")
    }
  })
  
  # Delete trade
  observeEvent(input$delete_trade, {
    selected <- selected_trade()
    if (!is.null(selected)) {
      current_log <- trading_log()
      updated_log <- current_log[trade_id != selected$trade_id]
      trading_log(updated_log)
      showNotification("Trade deleted successfully!", type = "message")
    } else {
      showNotification("Please select a trade to delete", type = "warning")
    }
  })
  
  # Filter trading log
  filtered_trading_log <- reactive({
    log_data <- trading_log()
    
    if (nrow(log_data) == 0) return(log_data)
    
    # Apply filters
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
  
  # Trading log table
  # output$trading_log_table <- DT::renderDataTable({
  #   log_data <- filtered_trading_log()
  #   
  #   if (nrow(log_data) > 0) {
  #     display_data <- log_data[, .(
  #       ID = trade_id,
  #       Symbol = symbol,
  #       Action = toupper(action),
  #       Date = as.character(trade_date),
  #       Time = format(trade_time, "%H:%M"),
  #       Entry_Price = sprintf("$%.2f", entry_price),
  #       Exit_Price = ifelse(is.na(exit_price), "Open", sprintf("$%.2f", exit_price)),
  #       Quantity = quantity,
  #       Target = ifelse(is.na(target_price), "N/A", sprintf("$%.2f", target_price)),
  #       Stop_Loss = ifelse(is.na(stop_loss), "N/A", sprintf("$%.2f", stop_loss)),
  #       Entry_Criteria = entry_criteria,
  #       Scan_Source = scan_source,
  #       Status = toupper(status),
  #       PnL = ifelse(status == "open", "Open", sprintf("$%.2f", pnl)),
  #       PnL_Percent = ifelse(status == "open", "Open", sprintf("%.1f%%", pnl_percent)),
  #       Notes = substr(notes, 1, 50)
  #     )]
  #     
  #     DT::datatable(display_data,
  #                   options = list(pageLength = 15, scrollX = TRUE,
  #                                  order = list(list(0, 'desc'))),
  #                   selection = 'single',
  #                   rownames = FALSE) %>%
  #       DT::formatStyle('PnL',
  #                       backgroundColor = DT::styleInterval(0, c('#ffcccc', '#ccffcc'))) %>%
  #       DT::formatStyle('Status',
  #                       backgroundColor = DT::styleEqual(c('OPEN', 'CLOSED'), 
  #                                                        c('#ffffcc', '#ccccff')))
  #   } else {
  #     DT::datatable(data.frame(Message = "No trades recorded yet"))
  #   }
  # })
  # Update the trading log table output to show both manual entries and CSV data
  output$trading_log_table <- DT::renderDataTable({
    manual_log <- filtered_trading_log()
    csv_log <- csv_trading_log()
    
    # Combine manual entries and CSV data
    if (nrow(manual_log) > 0 && nrow(csv_log) > 0) {
      combined_log <- rbind(manual_log, csv_log)
      combined_log <- combined_log[order(-trade_date, -trade_time)]
      # Remove duplicates based on trade_id if they exist
      combined_log <- combined_log[!duplicated(trade_id)]
    } else if (nrow(csv_log) > 0) {
      combined_log <- csv_log
    } else if (nrow(manual_log) > 0) {
      combined_log <- manual_log
    } else {
      combined_log <- data.table()
    }
    
    if (nrow(combined_log) > 0) {
      display_data <- combined_log[, .(
        ID = trade_id,
        Symbol = symbol,
        Action = toupper(action),
        Date = as.character(trade_date),
        Time = format(trade_time, "%H:%M"),
        Entry_Price = sprintf("$%.2f", entry_price),
        Exit_Price = ifelse(is.na(exit_price), "Open", sprintf("$%.2f", exit_price)),
        Quantity = quantity,
        Target = ifelse(is.na(target_price), "N/A", sprintf("$%.2f", target_price)),
        Stop_Loss = ifelse(is.na(stop_loss), "N/A", sprintf("$%.2f", stop_loss)),
        Entry_Criteria = entry_criteria,
        Scan_Source = scan_source,
        Status = toupper(status),
        PnL = ifelse(status == "open", "Open", sprintf("$%.2f", pnl)),
        PnL_Percent = ifelse(status == "open", "Open", sprintf("%.1f%%", pnl_percent)),
        Notes = substr(notes, 1, 50),
        Source = ifelse(trade_id %in% manual_log$trade_id, "Manual", "CSV")
      )]
      
      DT::datatable(display_data,
                    options = list(pageLength = 15, scrollX = TRUE,
                                   order = list(list(0, 'desc'))),
                    selection = 'single',
                    rownames = FALSE,
                    caption = "Trading Log (Manual Entries + CSV Data)") %>%
        DT::formatStyle('PnL',
                        backgroundColor = DT::styleInterval(0, c('#ffcccc', '#ccffcc'))) %>%
        DT::formatStyle('Status',
                        backgroundColor = DT::styleEqual(c('OPEN', 'CLOSED'), 
                                                         c('#ffffcc', '#ccccff'))) %>%
        DT::formatStyle('Source',
                        backgroundColor = DT::styleEqual(c('Manual', 'CSV'), 
                                                         c('#e6f3ff', '#f0f8e6')))
    } else {
      DT::datatable(data.frame(Message = "No trades found in manual entries or CSV file"))
    }
  })
  
  
  # # P&L Summary
  # output$pnl_summary <- renderText({
  #   log_data <- filtered_trading_log()
  #   
  #   if (nrow(log_data) == 0) {
  #     return("No trades to analyze")
  #   }
  #   
  #   closed_trades <- log_data[status == "closed" & !is.na(pnl)]
  #   
  #   if (nrow(closed_trades) == 0) {
  #     return("No closed trades for P&L analysis")
  #   }
  #   
  #   total_pnl <- sum(closed_trades$pnl, na.rm = TRUE)
  #   avg_pnl <- mean(closed_trades$pnl, na.rm = TRUE)
  #   win_trades <- sum(closed_trades$pnl > 0, na.rm = TRUE)
  #   loss_trades <- sum(closed_trades$pnl < 0, na.rm = TRUE)
  #   win_rate <- win_trades / nrow(closed_trades) * 100
  #   
  #   paste0(
  #     "💰 P&L SUMMARY:\n",
  #     "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
  #     "Total P&L: $", sprintf("%.2f", total_pnl), "\n",
  #     "Average P&L per Trade: $", sprintf("%.2f", avg_pnl), "\n",
  #     "Total Trades: ", nrow(closed_trades), "\n",
  #     "Winning Trades: ", win_trades, "\n",
  #     "Losing Trades: ", loss_trades, "\n",
  #     "Win Rate: ", sprintf("%.1f%%", win_rate), "\n",
  #     
  #     if (win_trades > 0) {
  #       paste0("Average Win: $", sprintf("%.2f", mean(closed_trades[pnl > 0]$pnl)), "\n")
  #     } else "",
  #     
  #     if (loss_trades > 0) {
  #       paste0("Average Loss: $", sprintf("%.2f", mean(closed_trades[pnl < 0]$pnl)), "\n")
  #     } else ""
  #   )
  # })
  
  # Update P&L Summary to use CSV data
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
      "💰 P&L SUMMARY (CSV Data):\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
      "Total P&L: $", sprintf("%.2f", total_pnl), "\n",
      "Average P&L per Trade: $", sprintf("%.2f", avg_pnl), "\n",
      "Total Closed Trades: ", nrow(closed_trades), "\n",
      "Winning Trades: ", win_trades, "\n",
      "Losing Trades: ", loss_trades, "\n",
      "Win Rate: ", sprintf("%.1f%%", win_rate), "\n",
      "Average Win: $", sprintf("%.2f", avg_win), "\n",
      "Average Loss: $", sprintf("%.2f", avg_loss), "\n",
      "Profit Factor: ", sprintf("%.2f", ifelse(avg_loss != 0, abs(avg_win * win_trades / (avg_loss * loss_trades)), 0))
    )
  })
  
  
  
  # # Trade Statistics
  # output$trade_stats <- renderText({
  #   log_data <- filtered_trading_log()
  #   
  #   if (nrow(log_data) == 0) {
  #     return("No trades to analyze")
  #   }
  #   
  #   open_trades <- sum(log_data$status == "open")
  #   total_value <- sum(log_data$entry_price * log_data$quantity, na.rm = TRUE)
  #   
  #   paste0(
  #     "📊 TRADE STATISTICS:\n",
  #     "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
  #     "Total Trades: ", nrow(log_data), "\n",
  #     "Open Positions: ", open_trades, "\n",
  #     "Closed Positions: ", nrow(log_data) - open_trades, "\n",
  #     "Total Trade Value: $", sprintf("%.2f", total_value), "\n",
  #     "Average Position Size: $", sprintf("%.2f", total_value / nrow(log_data)), "\n",
  #     "Most Traded Symbol: ", names(sort(table(log_data$symbol), decreasing = TRUE))[1], "\n",
  #     "Most Used Entry Criteria: ", names(sort(table(log_data$entry_criteria), decreasing = TRUE))[1]
  #   )
  # })
  
  
  # Update Trade Statistics to use CSV data
  output$trade_stats <- renderText({
    csv_log <- csv_trading_log()
    
    if (nrow(csv_log) == 0) {
      return("No CSV trading log found")
    }
    
    open_trades <- sum(csv_log$status == "open")
    total_value <- sum(csv_log$entry_price * csv_log$quantity, na.rm = TRUE)
    unique_symbols <- length(unique(csv_log$symbol))
    
    # Get most frequent values
    most_traded <- names(sort(table(csv_log$symbol), decreasing = TRUE))[1]
    most_used_criteria <- names(sort(table(csv_log$entry_criteria), decreasing = TRUE))[1]
    
    paste0(
      "📊 TRADE STATISTICS (CSV Data):\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
      "Total Trades: ", nrow(csv_log), "\n",
      "Open Positions: ", open_trades, "\n",
      "Closed Positions: ", nrow(csv_log) - open_trades, "\n",
      "Unique Symbols: ", unique_symbols, "\n",
      "Total Trade Value: $", sprintf("%.2f", total_value), "\n",
      "Average Position Size: $", sprintf("%.2f", total_value / nrow(csv_log)), "\n",
      "Most Traded Symbol: ", most_traded, "\n",
      "Most Used Entry Criteria: ", most_used_criteria
    )
  })
  
  # Download handler
  output$download_trades <- downloadHandler(
    filename = function() {
      paste("trading_log_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      log_data <- filtered_trading_log()
      if (nrow(log_data) > 0) {
        write.csv(log_data, file, row.names = FALSE)
      } else {
        write.csv(data.frame(Message = "No trades to export"), file, row.names = FALSE)
      }
    }
  )
  
  # Performance Charts
  # output$pnl_chart <- renderPlot({
  #   log_data <- filtered_trading_log()
  #   closed_trades <- log_data[status == "closed" & !is.na(pnl)][order(trade_date)]
  #   
  #   if (nrow(closed_trades) > 0) {
  #     closed_trades[, cumulative_pnl := cumsum(pnl)]
  #     
  #     plot(1:nrow(closed_trades), closed_trades$cumulative_pnl, type = "l",
  #          main = "Cumulative P&L", xlab = "Trade Number", ylab = "Cumulative P&L ($)",
  #          col = "blue", lwd = 2)
  #     abline(h = 0, col = "red", lty = 2)
  #     points(1:nrow(closed_trades), closed_trades$cumulative_pnl, pch = 16, col = "blue")
  #   } else {
  #     plot(1, 1, type = "n", main = "No Closed Trades", xlab = "", ylab = "")
  #     text(1, 1, "No closed trades\nfor P&L analysis", cex = 1.2)
  #   }
  # })
  
  #Update P&L Chart to use CSV data
  output$pnl_chart <- renderPlot({
    csv_log <- csv_trading_log()
    closed_trades <- csv_log[status == "closed" & !is.na(pnl)][order(trade_date)]
    
    if (nrow(closed_trades) > 0) {
      closed_trades[, cumulative_pnl := cumsum(pnl)]
      
      plot(1:nrow(closed_trades), closed_trades$cumulative_pnl, type = "l",
           main = "Cumulative P&L (CSV Data)", xlab = "Trade Number", ylab = "Cumulative P&L ($)",
           col = "blue", lwd = 2)
      abline(h = 0, col = "red", lty = 2)
      points(1:nrow(closed_trades), closed_trades$cumulative_pnl, pch = 16, col = "blue")
      
      # Add trend line
      if (nrow(closed_trades) > 2) {
        trend <- lm(cumulative_pnl ~ I(1:nrow(closed_trades)), data = closed_trades)
        abline(trend, col = "green", lty = 2)
      }
    } else {
      plot(1, 1, type = "n", main = "No Closed Trades in CSV", xlab = "", ylab = "")
      text(1, 1, "No closed trades\nfound in CSV file", cex = 1.2)
    }
  })
  
  # output$win_loss_chart <- renderPlot({
  #   log_data <- filtered_trading_log()
  #   closed_trades <- log_data[status == "closed" & !is.na(pnl)]
  #   
  #   if (nrow(closed_trades) > 0) {
  #     wins <- sum(closed_trades$pnl > 0)
  #     losses <- sum(closed_trades$pnl < 0)
  #     
  #     pie_data <- c(wins, losses)
  #     pie_labels <- c(paste("Wins (", wins, ")", sep = ""),
  #                     paste("Losses (", losses, ")", sep = ""))
  #     pie_colors <- c("#66cc66", "#ff6666")
  #     
  #     pie(pie_data, labels = pie_labels, col = pie_colors,
  #         main = paste("Win/Loss Ratio:", round(wins/(wins+losses)*100, 1), "% Wins"))
  #   } else {
  #     plot(1, 1, type = "n", main = "No Closed Trades", xlab = "", ylab = "")
  #   }
  # })
  # 
  # output$entry_criteria_chart <- renderPlot({
  #   log_data <- filtered_trading_log()
  #   
  #   if (nrow(log_data) > 0) {
  #     criteria_counts <- table(log_data$entry_criteria)
  #     barplot(criteria_counts, main = "Entry Criteria Usage",
  #             las = 2, col = "lightblue", ylab = "Number of Trades")
  #   }
  # })
  output$win_loss_chart <- renderPlot({
    csv_log <- csv_trading_log()
    closed_trades <- csv_log[status == "closed" & !is.na(pnl)]
    
    if (nrow(closed_trades) > 0) {
      wins <- sum(closed_trades$pnl > 0)
      losses <- sum(closed_trades$pnl < 0)
      breakeven <- sum(closed_trades$pnl == 0)
      
      if (wins + losses + breakeven > 0) {
        pie_data <- c(wins, losses, breakeven)
        pie_labels <- c(paste("Wins (", wins, ")", sep = ""),
                        paste("Losses (", losses, ")", sep = ""),
                        paste("Breakeven (", breakeven, ")", sep = ""))
        pie_colors <- c("#66cc66", "#ff6666", "#ffff66")
        
        # Remove zero values
        non_zero <- pie_data > 0
        pie_data <- pie_data[non_zero]
        pie_labels <- pie_labels[non_zero]
        pie_colors <- pie_colors[non_zero]
        
        pie(pie_data, labels = pie_labels, col = pie_colors,
            main = paste("Win/Loss Ratio:", round(wins/(wins+losses)*100, 1), "% Wins"))
      }
    } else {
      plot(1, 1, type = "n", main = "No Closed Trades in CSV", xlab = "", ylab = "")
      text(1, 1, "No closed trades\nfound in CSV file", cex = 1.2)
    }
  })
  
  # Update Entry Criteria Chart to use CSV data
  output$entry_criteria_chart <- renderPlot({
    csv_log <- csv_trading_log()
    
    if (nrow(csv_log) > 0) {
      criteria_counts <- table(csv_log$entry_criteria)
      barplot(criteria_counts, main = "Entry Criteria Usage (CSV Data)",
              las = 2, col = "lightblue", ylab = "Number of Trades")
    } else {
      plot(1, 1, type = "n", main = "No CSV Data", xlab = "", ylab = "")
      text(1, 1, "No trading log CSV\nfile found", cex = 1.2)
    }
  })
  
  
  # output$scan_source_chart <- renderPlot({
  #   log_data <- filtered_trading_log()
  #   
  #   if (nrow(log_data) > 0) {
  #     # Parse scan sources (handle multiple sources separated by |)
  #     all_sources <- unlist(strsplit(log_data$scan_source, "\\|"))
  #     all_sources <- all_sources[all_sources != ""]
  #     
  #     if (length(all_sources) > 0) {
  #       source_counts <- table(all_sources)
  #       barplot(source_counts, main = "Scan Source Usage",
  #               las = 2, col = "lightgreen", ylab = "Number of Trades")
  #     }
  #   }
  # })
  
  # Update Scan Source Chart to use CSV data  
  output$scan_source_chart <- renderPlot({
    csv_log <- csv_trading_log()
    
    if (nrow(csv_log) > 0) {
      # Parse scan sources (handle multiple sources separated by |)
      all_sources <- unlist(strsplit(csv_log$scan_source, "\\|"))
      all_sources <- all_sources[all_sources != ""]
      
      if (length(all_sources) > 0) {
        source_counts <- table(all_sources)
        barplot(source_counts, main = "Scan Source Usage (CSV Data)",
                las = 2, col = "lightgreen", ylab = "Number of Trades")
      }
    } else {
      plot(1, 1, type = "n", main = "No CSV Data", xlab = "", ylab = "")
      text(1, 1, "No trading log CSV\nfile found", cex = 1.2)
    }
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
