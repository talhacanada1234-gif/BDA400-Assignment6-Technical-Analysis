# ================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 6: Technical Analysis using R, Visualization Phase
# Student: talha ali
# ================================================================
#
# Portfolio Visualization Dashboard using R Shiny
# Data source: Yahoo Finance via quantmod
#
# Required packages:
# install.packages(c("shiny", "ggplot2", "quantmod", "dplyr",
#                    "tidyr", "zoo", "scales"))
#
# Run:
# shiny::runApp("app.R")
#
# ================================================================

library(shiny)
library(ggplot2)
library(quantmod)
library(dplyr)
library(tidyr)
library(zoo)
library(scales)

# -----------------------------
# 1. Helper functions
# -----------------------------

fetch_stock_data <- function(symbol, start_date, end_date) {
  tryCatch({
    x <- getSymbols(
      Symbols = symbol,
      src = "yahoo",
      from = as.Date(start_date),
      to = as.Date(end_date) + 1,
      auto.assign = FALSE,
      warnings = FALSE
    )

    if (NROW(x) == 0) stop("No data returned for the selected symbol/date range.")

    # Use adjusted close when available for analysis.
    df <- data.frame(
      Date = as.Date(index(x)),
      Open = as.numeric(Op(x)),
      High = as.numeric(Hi(x)),
      Low = as.numeric(Lo(x)),
      Close = as.numeric(Cl(x)),
      Adjusted = as.numeric(Ad(x)),
      Volume = as.numeric(Vo(x))
    )

    df <- df %>% filter(!is.na(Adjusted))
    if (nrow(df) < 5) stop("Not enough observations for analysis.")

    df
  }, error = function(e) {
    stop(
      paste0(
        "Unable to download data for ", symbol, ". ",
        "Check the ticker, dates, and internet connection. Details: ",
        conditionMessage(e)
      )
    )
  })
}

aggregate_period <- function(df, timeframe) {
  if (timeframe == "Daily") return(df)

  if (timeframe == "Weekly") {
    df <- df %>%
      mutate(Period = as.Date(cut(Date, breaks = "week", start.on.monday = TRUE)))
  } else {
    df <- df %>%
      mutate(Period = as.Date(cut(Date, breaks = "month")))
  }

  df %>%
    group_by(Period) %>%
    summarise(
      Date = max(Date),
      Open = first(Open),
      High = max(High, na.rm = TRUE),
      Low = min(Low, na.rm = TRUE),
      Close = last(Close),
      Adjusted = last(Adjusted),
      Volume = sum(Volume, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    select(Date, Open, High, Low, Close, Adjusted, Volume)
}

add_indicators <- function(df, short_n, long_n, rsi_n, macd_fast, macd_slow, macd_signal) {
  df <- df %>%
    arrange(Date) %>%
    mutate(
      MA_Short = zoo::rollmean(Adjusted, k = short_n, fill = NA, align = "right"),
      MA_Long = zoo::rollmean(Adjusted, k = long_n, fill = NA, align = "right")
    )

  # RSI calculation using quantmod/TTR implementation.
  rsi_obj <- RSI(df$Adjusted, n = rsi_n)
  df$RSI <- as.numeric(rsi_obj)

  macd_obj <- MACD(
    df$Adjusted,
    nFast = macd_fast,
    nSlow = macd_slow,
    nSig = macd_signal,
    maType = "EMA"
  )
  df$MACD <- as.numeric(macd_obj[, "macd"])
  df$MACD_Signal <- as.numeric(macd_obj[, "signal"])
  df$MACD_Hist <- df$MACD - df$MACD_Signal

  # Signal is generated only when an actual crossover occurs.
  previous_short <- dplyr::lag(df$MA_Short)
  previous_long <- dplyr::lag(df$MA_Long)

  df$Signal <- "Hold"
  df$Signal[
    !is.na(df$MA_Short) & !is.na(df$MA_Long) &
      !is.na(previous_short) & !is.na(previous_long) &
      previous_short <= previous_long & df$MA_Short > df$MA_Long
  ] <- "Buy"

  df$Signal[
    !is.na(df$MA_Short) & !is.na(df$MA_Long) &
      !is.na(previous_short) & !is.na(previous_long) &
      previous_short >= previous_long & df$MA_Short < df$MA_Long
  ] <- "Sell"

  df
}

make_price_plot <- function(df, chart_type, show_ma, show_signals) {
  p <- ggplot(df, aes(x = Date))

  if (chart_type == "Line") {
    p <- p + geom_line(aes(y = Adjusted), linewidth = 0.7)
  } else if (chart_type == "Area") {
    p <- p + geom_area(aes(y = Adjusted), alpha = 0.35)
  } else {
    # Lightweight candlestick implementation using ggplot2.
    p <- p +
      geom_linerange(aes(ymin = Low, ymax = High), linewidth = 0.35) +
      geom_linerange(
        aes(
          ymin = pmin(Open, Close),
          ymax = pmax(Open, Close),
          colour = Close >= Open
        ),
        linewidth = 2.8
      ) +
      scale_colour_manual(
        values = c(`TRUE` = "darkgreen", `FALSE` = "firebrick"),
        guide = "none"
      )
  }

  if (show_ma) {
    p <- p +
      geom_line(aes(y = MA_Short, linetype = "Short MA"), linewidth = 0.8, na.rm = TRUE) +
      geom_line(aes(y = MA_Long, linetype = "Long MA"), linewidth = 0.8, na.rm = TRUE) +
      scale_linetype_manual(values = c("Short MA" = "dashed", "Long MA" = "dotted"))
  }

  if (show_signals) {
    buy_df <- df %>% filter(Signal == "Buy")
    sell_df <- df %>% filter(Signal == "Sell")

    if (nrow(buy_df) > 0) {
      p <- p +
        geom_point(data = buy_df, aes(y = Adjusted), shape = 24, size = 3.5,
                   fill = "darkgreen", colour = "black") +
        geom_text(data = buy_df, aes(y = Adjusted, label = "BUY"),
                  vjust = -1.0, fontface = "bold", colour = "darkgreen", size = 3.2)
    }

    if (nrow(sell_df) > 0) {
      p <- p +
        geom_point(data = sell_df, aes(y = Adjusted), shape = 25, size = 3.5,
                   fill = "firebrick", colour = "black") +
        geom_text(data = sell_df, aes(y = Adjusted, label = "SELL"),
                  vjust = 1.6, fontface = "bold", colour = "firebrick", size = 3.2)
    }
  }

  p +
    labs(
      title = "Stock Price and Trading Signals",
      x = NULL,
      y = "Price (Adjusted Close)",
      linetype = "Technical Indicator"
    ) +
    scale_y_continuous(labels = dollar_format(prefix = "$")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}

# -----------------------------
# 2. User Interface
# -----------------------------

ui <- fluidPage(
  titlePanel("BDA400 Portfolio Technical Analysis Dashboard"),

  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Stock Symbol:", value = "AAPL"),
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = Sys.Date() - 365,
        end = Sys.Date()
      ),
      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly"),
        selected = "Daily"
      ),
      selectInput(
        "chart_type",
        "Chart Type:",
        choices = c("Line", "Candlestick", "Area"),
        selected = "Candlestick"
      ),
      numericInput("short_ma", "Short MA Period:", value = 20, min = 2, max = 200),
      numericInput("long_ma", "Long MA Period:", value = 50, min = 3, max = 300),
      numericInput("rsi_n", "RSI Period:", value = 14, min = 2, max = 100),
      numericInput("macd_fast", "MACD Fast:", value = 12, min = 2, max = 100),
      numericInput("macd_slow", "MACD Slow:", value = 26, min = 3, max = 200),
      numericInput("macd_signal", "MACD Signal:", value = 9, min = 2, max = 100),

      checkboxGroupInput(
        "technical_indicators",
        "Technical Indicators:",
        choices = c("Moving Averages", "RSI", "MACD"),
        selected = c("Moving Averages", "RSI", "MACD")
      ),

      checkboxInput("show_signals", "Show BUY / SELL annotations", TRUE),
      actionButton("refresh", "Refresh Data"),
      br(), br(),
      helpText(
        "Trading rule: BUY when the short moving average crosses above ",
        "the long moving average; SELL when it crosses below; otherwise HOLD."
      )
    ),

    mainPanel(
      h4(textOutput("status")),
      plotOutput("stock_chart", height = "550px"),
      conditionalPanel(
        condition = "input.technical_indicators.indexOf('RSI') >= 0",
        plotOutput("rsi_chart", height = "220px")
      ),
      conditionalPanel(
        condition = "input.technical_indicators.indexOf('MACD') >= 0",
        plotOutput("macd_chart", height = "260px")
      ),
      h4("Latest Trading Signals"),
      tableOutput("signal_table"),
      h4("Latest Market Data"),
      tableOutput("latest_data")
    )
  )
)

# -----------------------------
# 3. Server
# -----------------------------

server <- function(input, output, session) {

  raw_data <- eventReactive(input$refresh, {
    validate(
      need(nzchar(trimws(input$symbol)), "Enter a stock ticker symbol."),
      need(input$date_range[1] <= input$date_range[2], "Invalid date range."),
      need(input$short_ma < input$long_ma, "Short MA must be smaller than Long MA.")
    )

    showNotification("Downloading Yahoo Finance data...", type = "message")
    fetch_stock_data(
      toupper(trimws(input$symbol)),
      input$date_range[1],
      input$date_range[2]
    )
  }, ignoreNULL = FALSE)

  analysis_data <- reactive({
    df <- raw_data()
    df <- aggregate_period(df, input$time_frame)

    validate(
      need(
        nrow(df) >= input$long_ma,
        paste0(
          "Not enough observations for a ", input$long_ma,
          "-period moving average. Expand the date range or use a shorter MA."
        )
      )
    )

    add_indicators(
      df,
      input$short_ma,
      input$long_ma,
      input$rsi_n,
      input$macd_fast,
      input$macd_slow,
      input$macd_signal
    )
  })

  output$status <- renderText({
    paste0(
      toupper(input$symbol), " | ",
      format(input$date_range[1], "%Y-%m-%d"), " to ",
      format(input$date_range[2], "%Y-%m-%d"), " | ",
      input$time_frame, " | ", input$chart_type
    )
  })

  output$stock_chart <- renderPlot({
    df <- analysis_data()

    make_price_plot(
      df,
      input$chart_type,
      "Moving Averages" %in% input$technical_indicators,
      input$show_signals
    )
  })

  output$rsi_chart <- renderPlot({
    df <- analysis_data()

    ggplot(df, aes(x = Date, y = RSI)) +
      geom_line(linewidth = 0.8) +
      geom_hline(yintercept = c(30, 70), linetype = "dashed") +
      annotate("text", x = max(df$Date), y = 70, label = "Overbought 70",
               hjust = 1, vjust = -0.4, size = 3) +
      annotate("text", x = max(df$Date), y = 30, label = "Oversold 30",
               hjust = 1, vjust = 1.4, size = 3) +
      scale_y_continuous(limits = c(0, 100)) +
      labs(title = "Relative Strength Index (RSI)", x = NULL, y = "RSI") +
      theme_minimal(base_size = 11)
  })

  output$macd_chart <- renderPlot({
    df <- analysis_data()

    ggplot(df, aes(x = Date)) +
      geom_col(aes(y = MACD_Hist), alpha = 0.45) +
      geom_line(aes(y = MACD), linewidth = 0.8) +
      geom_line(aes(y = MACD_Signal), linewidth = 0.8, linetype = "dashed") +
      geom_hline(yintercept = 0, linetype = "dotted") +
      labs(
        title = "MACD",
        x = NULL,
        y = "MACD",
        caption = "Solid = MACD | Dashed = Signal | Bars = Histogram"
      ) +
      theme_minimal(base_size = 11)
  })

  output$signal_table <- renderTable({
    df <- analysis_data()

    df %>%
      filter(Signal != "Hold") %>%
      select(Date, Adjusted, MA_Short, MA_Long, Signal) %>%
      arrange(desc(Date)) %>%
      head(10) %>%
      mutate(across(where(is.numeric), ~ round(.x, 2)))
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$latest_data <- renderTable({
    df <- analysis_data()

    df %>%
      select(Date, Open, High, Low, Close, Adjusted, Volume) %>%
      arrange(desc(Date)) %>%
      head(10) %>%
      mutate(
        across(c(Open, High, Low, Close, Adjusted), ~ round(.x, 2)),
        Volume = comma(round(Volume, 0))
      )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
}

# -----------------------------
# 4. Launch application
# -----------------------------
shinyApp(ui = ui, server = server)
