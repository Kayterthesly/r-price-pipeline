# =============================================================================
# dashboard/app.R
# Pipeline: r-price-pipeline
# Stage: 5 — Interactive Dashboard (Shiny + Plotly)
# Usage: shiny::runApp(here::here("dashboard"))
#        OR click "Run App" button in RStudio when this file is open
# =============================================================================

library(shiny)
library(plotly)
library(bslib)
library(dplyr)
library(DBI)
library(duckdb)
library(logger)
library(here)

# Source pipeline functions — dashboard calls them directly (no API dependency)
source(here::here("r_scripts", "03_modeling.R"))

# =============================================================================
# UI DEFINITION
# =============================================================================
ui <- page_sidebar(
  title = "r-price-pipeline | Forecast Dashboard",
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Inter")),
  
  # ── Sidebar ────────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 280,
    
    h5("📈 Forecast Settings"),
    hr(),
    
    textInput(
      inputId = "symbol",
      label   = "Ticker Symbol",
      value   = "BTC-USD",
      placeholder = "e.g. BTC-USD, AAPL"
    ),
    
    sliderInput(
      inputId = "horizon",
      label   = "Forecast Horizon (days)",
      min     = 5,
      max     = 90,
      value   = 30,
      step    = 5
    ),
    
    sliderInput(
      inputId = "history_days",
      label   = "History to Display (days)",
      min     = 30,
      max     = 365,
      value   = 180,
      step    = 30
    ),
    
    hr(),
    actionButton(
      inputId = "run",
      label   = "▶  Run Forecast",
      class   = "btn-primary w-100"
    ),
    hr(),
    
    # Environment info
    h6("Environment"),
    verbatimTextOutput("env_info", placeholder = TRUE)
  ),
  
  # ── Main Panel ─────────────────────────────────────────────────────────────
  navset_card_tab(
    
    # Tab 1: Price chart
    nav_panel(
      title = "📊 Price & Forecast",
      plotlyOutput("price_chart", height = "500px"),
      br(),
      uiOutput("forecast_status")
    ),
    
    # Tab 2: Model metadata
    nav_panel(
      title = "🔬 Model Info",
      br(),
      tableOutput("model_meta_table"),
      br(),
      h6("Forecast Table (first 10 rows)"),
      tableOutput("forecast_table")
    ),
    
    # Tab 3: About
    nav_panel(
      title = "ℹ About",
      br(),
      p("This dashboard runs the full r-price-pipeline:"),
      tags$ol(
        tags$li("Reads price data from DuckDB (raw_prices)"),
        tags$li("Computes lag-safe technical features (feature_prices)"),
        tags$li("Fits auto.arima on log returns (80/20 temporal split)"),
        tags$li("Converts log return forecasts back to price levels"),
        tags$li("Displays historical prices + forecast cone")
      ),
      hr(),
      p(strong("Data source:"), Sys.getenv("ENV_MODE", unset = "synthetic")),
      p(strong("Author:"), "Kingsley Akenu (@Kayterthesly / KAIZEN 改善)"),
      p(strong("Pipeline commit:"), "See model metadata tab after running forecast")
    )
  )
)

# =============================================================================
# SERVER LOGIC
# =============================================================================
server <- function(input, output, session) {
  
  # Reactive store for forecast results
  forecast_data <- reactiveVal(NULL)
  error_message <- reactiveVal(NULL)
  
  # ── Environment info ────────────────────────────────────────────────────────
  output$env_info <- renderText({
    paste0(
      "Mode: ", Sys.getenv("ENV_MODE", "synthetic"), "\n",
      "R: ",    paste(R.version$major, R.version$minor, sep = "."), "\n",
      "DB: ",   basename(Sys.getenv("DB_PATH", "pipeline_store.duckdb"))
    )
  })
  
  
  
  # ── Run forecast on button click ────────────────────────────────────────────
  observeEvent(input$run, {
    
    # Reset state
    forecast_data(NULL)
    error_message(NULL)
    
    # Validate symbol input
    sym <- trimws(input$symbol)
    if (nchar(sym) == 0) {
      error_message("Please enter a ticker symbol (e.g. BTC-USD)")
      return()
    }
    
    # Run with progress indicator
    withProgress(message = paste("Forecasting", sym, "..."), value = 0, {
      
      incProgress(0.2, detail = "Loading feature data...")
      
      result <- tryCatch({
        incProgress(0.3, detail = "Fitting ARIMA model...")
        compute_asset_forecasts(
          target_symbol    = sym,
          forecast_horizon = as.integer(input$horizon)
        )
      }, error = function(e) {
        error_message(paste("Error:", conditionMessage(e)))
        NULL
      })
      
      incProgress(0.4, detail = "Rendering chart...")
      forecast_data(result)
    })
  })
  
  # ── Price + Forecast chart ──────────────────────────────────────────────────
  output$price_chart <- renderPlotly({
    
    result <- forecast_data()
    
    # Load historical prices from DuckDB
    con <- tryCatch(get_db_connection(), error = function(e) NULL)
    if (is.null(con)) {
      return(plot_ly() |>
               layout(title = "Cannot connect to database — run the pipeline first"))
    }
    on.exit(dbDisconnect(con, shutdown = TRUE))
    
    sym          <- trimws(input$symbol)
    history_days <- input$history_days
    cutoff_date  <- Sys.Date() - history_days
    
    historical <- tryCatch({
      tbl(con, "raw_prices") |>
        dplyr::filter(symbol == !!sym, date >= !!cutoff_date) |>
        dplyr::arrange(date) |>
        dplyr::collect()
    }, error = function(e) NULL)
    
    # Base chart
    fig <- plot_ly()
    
    # Historical price line
    if (!is.null(historical) && nrow(historical) > 0) {
      fig <- fig |>
        add_lines(
          data = historical,
          x    = ~date,
          y    = ~adjusted,
          name = "Historical Price",
          line = list(color = "#2196F3", width = 1.5)
        )
    }
    
    # Forecast layers (only if forecast was run)
    if (!is.null(result)) {
      fc <- result$forecast_df
      
      # CI ribbon
      fig <- fig |>
        add_ribbons(
          data       = fc,
          x          = ~forecast_date,
          ymin       = ~price_lower,
          ymax       = ~price_upper,
          name       = "95% CI",
          fillcolor  = "rgba(255,152,0,0.15)",
          line       = list(color = "transparent"),
          hoverinfo  = "skip"
        )
      
      # Forecast point estimate
      fig <- fig |>
        add_lines(
          data = fc,
          x    = ~forecast_date,
          y    = ~price_hat,
          name = "Forecast",
          line = list(color = "#FF9800", width = 2, dash = "dash")
        )
    }
    
    # Layout
    title_text <- if (is.null(result)) {
      paste(sym, "— Historical Prices (click Run Forecast)")
    } else {
      paste(sym, "— Historical +", input$horizon, "Day Forecast")
    }
    
    # Cap y-axis at 1.5x historical max so CI explosion doesn't distort scale
    y_max <- if (!is.null(historical) && nrow(historical) > 0) {
      max(historical$adjusted, na.rm = TRUE) * 1.5
    } else NULL
    
    fig |> layout(
      title      = list(text = title_text, font = list(size = 14)),
      xaxis      = list(title = "Date", showgrid = FALSE),
      yaxis      = list(title = "Price (USD)", showgrid = TRUE,
                        gridcolor = "#f0f0f0",
                        range = if (!is.null(y_max)) list(0, y_max) else NULL),
      hovermode  = "x unified",
      legend     = list(orientation = "h", y = -0.15),
      plot_bgcolor  = "#ffffff",
      paper_bgcolor = "#ffffff"
    )
  })
  
  # ── Forecast status message ─────────────────────────────────────────────────
  output$forecast_status <- renderUI({
    err <- error_message()
    res <- forecast_data()
    
    if (!is.null(err)) {
      div(class = "alert alert-danger", err)
    } else if (!is.null(res)) {
      div(
        class = "alert alert-success",
        paste0(
          "✅ Forecast complete | Model: ", res$model_meta$model_type,
          " | RMSE: ", round(res$model_meta$rmse_test, 5),
          " | Trace: ", substr(res$model_version, 1, 30), "..."
        )
      )
    } else {
      div(class = "alert alert-info",
          "Set your parameters and click ▶ Run Forecast to begin.")
    }
  })
  
  # ── Model metadata table ────────────────────────────────────────────────────
  output$model_meta_table <- renderTable({
    result <- forecast_data()
    req(result)
    
    meta <- result$model_meta
    data.frame(
      Field = c("Model Version", "Symbol", "Model Type",
                "Train Rows", "Test Rows", "Forecast Horizon",
                "Test RMSE", "Last Price", "Last Date",
                "Data Hash", "Created At"),
      Value = c(meta$model_version, meta$symbol, meta$model_type,
                meta$n_train, meta$n_test, meta$forecast_horizon,
                round(meta$rmse_test, 8), meta$last_price,
                meta$last_date, meta$data_hash, meta$created_at)
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ── Forecast data table ─────────────────────────────────────────────────────
  output$forecast_table <- renderTable({
    result <- forecast_data()
    req(result)
    
    result$forecast_df |>
      head(10) |>
      dplyr::mutate(
        forecast_date  = format(forecast_date, "%Y-%m-%d"),
        price_hat      = round(price_hat, 2),
        price_lower    = round(price_lower, 2),
        price_upper    = round(price_upper, 2)
      ) |>
      dplyr::select(forecast_date, price_hat, price_lower, price_upper)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# =============================================================================
# LAUNCH
# =============================================================================
shinyApp(ui = ui, server = server)