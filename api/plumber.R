# =============================================================================
# api/plumber.R
# Pipeline: r-price-pipeline
# Stage: 4 — REST API (Plumber)
# Purpose: expose forecast pipeline as HTTP endpoints
# Launch with: source(here::here("api", "run_api.R"))
# DO NOT add pr_run() here — keep definitions separate from launcher
# =============================================================================

library(here)

# Source the full modeling pipeline
source(here::here("r_scripts", "03_modeling.R"))  # loads 00_utils, forecast, etc.

#* @apiTitle r-price-pipeline Forecast API
#* @apiDescription ARIMA/ETS price forecasting for stocks and crypto
#* @apiVersion 1.0.0

# -----------------------------------------------------------------------------
# GET /health
# Liveness check — confirms API is running and shows environment state.
# No parameters required.
# -----------------------------------------------------------------------------
#* Health check
#* @tag utility
#* @get /health
function() {
  list(
    status    = "ok",
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    env_mode  = Sys.getenv("ENV_MODE", unset = "synthetic"),
    r_version = paste(R.version$major, R.version$minor, sep = ".")
  )
}

# -----------------------------------------------------------------------------
# POST /predict/price
# Main forecast endpoint. Accepts JSON body with symbol and horizon.
#
# Request body (JSON):
#   { "symbol": "BTC-USD", "horizon": 30 }
#
# Response (JSON):
#   { symbol, forecast, model_meta, trace_id, data_source, error (if any) }
# -----------------------------------------------------------------------------
#* Generate price forecast
#* @tag forecast
#* @post /predict/price
function(req, res) {
  
  # Parse request body
  body <- tryCatch(
    jsonlite::fromJSON(req$postBody),
    error = function(e) NULL
  )
  
  # Validate body exists
  if (is.null(body)) {
    res$status <- 400
    return(list(
      error     = "Invalid or missing JSON body",
      example   = '{"symbol": "BTC-USD", "horizon": 30}',
      trace_id  = uuid::UUIDgenerate()
    ))
  }
  
  # Extract and validate symbol
  symbol <- body$symbol
  if (is.null(symbol) || nchar(trimws(symbol)) == 0) {
    res$status <- 400
    return(list(
      error    = "Missing or empty 'symbol' field",
      trace_id = uuid::UUIDgenerate()
    ))
  }
  
  # Extract and validate horizon (default 30, clamp 1–365)
  horizon <- if (is.null(body$horizon)) 30L else as.integer(body$horizon)
  if (is.na(horizon) || horizon < 1 || horizon > 365) {
    res$status <- 400
    return(list(
      error    = "'horizon' must be an integer between 1 and 365",
      trace_id = uuid::UUIDgenerate()
    ))
  }
  
  # Unique trace ID for this request — used for log correlation
  trace_id <- uuid::UUIDgenerate()
  log_info("API request received | trace_id={trace_id} | symbol={symbol} | horizon={horizon}")
  
  # Run forecast with full error handling
  forecast_result <- tryCatch({
    compute_asset_forecasts(
      target_symbol    = symbol,
      forecast_horizon = horizon
    )
  }, error = function(e) {
    log_error("Forecast failed | trace_id={trace_id} | error={conditionMessage(e)}")
    NULL
  })
  
  # Return error response if model failed
  if (is.null(forecast_result)) {
    res$status <- 500
    return(list(
      error    = paste("Forecast failed for symbol:", symbol,
                       "— run fetch_and_store_history() and generate_pipeline_features() first"),
      trace_id = trace_id
    ))
  }
  
  log_info("API response sent | trace_id={trace_id} | symbol={symbol}")
  
  # Return successful response
  list(
    symbol      = symbol,
    horizon     = horizon,
    forecast    = forecast_result$forecast_df,
    model_meta  = forecast_result$model_meta,
    trace_id    = trace_id,
    data_source = Sys.getenv("ENV_MODE", unset = "synthetic")
  )
}