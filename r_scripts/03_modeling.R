# =============================================================================
# 03_modeling.R
# Pipeline: r-price-pipeline
# Stage: 3 — Modeling & Forecasting
# Purpose: fit ARIMA/ETS on log returns → forecast → convert back to prices
# Usage: source this file, then call compute_asset_forecasts("BTC-USD")
# =============================================================================

source(here::here("r_scripts", "00_utils.R"))

library(forecast)
library(digest)
library(jsonlite)
library(dplyr)
library(logger)
library(here)

# -----------------------------------------------------------------------------
# ensure_model_registry_table()
# Creates audit table in DuckDB if it doesn't exist.
# Records every model run for traceability.
# -----------------------------------------------------------------------------
ensure_model_registry_table <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS model_registry (
      model_version  VARCHAR PRIMARY KEY,
      symbol         VARCHAR NOT NULL,
      model_type     VARCHAR NOT NULL,
      n_train        INTEGER,
      n_test         INTEGER,
      forecast_horizon INTEGER,
      data_hash      VARCHAR,
      rmse_test      DOUBLE,
      created_at     TIMESTAMP
    );
  ")
  log_info("model_registry table verified/created")
}

# -----------------------------------------------------------------------------
# compute_asset_forecasts()
# Reads feature_prices, splits train/test, fits ARIMA (ETS fallback),
# evaluates on test set, forecasts horizon steps ahead, converts to prices.
#
# Args:
#   target_symbol    : ticker string e.g. "BTC-USD"
#   forecast_horizon : days ahead to forecast (default 30)
#   train_ratio      : proportion of data for training (default 0.8)
# -----------------------------------------------------------------------------
compute_asset_forecasts <- function(target_symbol,
                                    forecast_horizon = 30,
                                    train_ratio = 0.8) {
  
  # Input validation
  if (!is.character(target_symbol) || nchar(trimws(target_symbol)) == 0) {
    stop("[MODEL ABORT] target_symbol must be a non-empty string", call. = FALSE)
  }
  if (forecast_horizon < 1 || forecast_horizon > 365) {
    stop("[MODEL ABORT] forecast_horizon must be between 1 and 365", call. = FALSE)
  }
  
  log_info("Starting forecast | symbol={target_symbol} | horizon={forecast_horizon}")
  
  con <- get_db_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  ensure_model_registry_table(con)
  
  # ── STEP 1: Load feature data, drop NA warm-up rows ─────────────────────────
  df_raw <- tbl(con, "feature_prices") %>%
    dplyr::filter(symbol == !!target_symbol) %>%
    dplyr::arrange(date) %>%
    dplyr::collect()
  
  if (nrow(df_raw) == 0) {
    stop("[MODEL ABORT] No feature data for '", target_symbol,
         "'. Run generate_pipeline_features() first.", call. = FALSE)
  }
  
  # Drop rows where any key feature is NA (warm-up period)
  # SMA-200 has 200 NAs — this determines our minimum drop
  df <- df_raw %>%
    dplyr::filter(
      !is.na(log_return),
      !is.na(sma_50),
      !is.na(sma_200),
      !is.na(vol_30)
    )
  
  log_info("Rows after dropping NA warm-up | symbol={target_symbol} | rows={nrow(df)}")
  
  if (nrow(df) < 100) {
    stop("[MODEL ABORT] Insufficient data after NA removal: ", nrow(df),
         " rows. Need at least 100.", call. = FALSE)
  }
  
  # ── STEP 2: Temporal train/test split (never random for time series) ─────────
  n_total <- nrow(df)
  n_train  <- floor(n_total * train_ratio)
  n_test   <- n_total - n_train
  
  df_train <- df[1:n_train, ]
  df_test  <- df[(n_train + 1):n_total, ]
  
  log_info("Train/test split | train={n_train} | test={n_test}")
  
  # ── STEP 3: Data lineage hash (reproducibility audit) ────────────────────────
  set.seed(101)
  data_hash <- digest::digest(df_train, algo = "md5")
  log_info("Data lineage hash | {data_hash}")
  
  # ── STEP 4: Fit model on training log returns ─────────────────────────────────
  train_ts <- ts(df_train$log_return, frequency = 1)
  
  model_type <- "ARIMA"
  fitted_model <- tryCatch({
    log_info("Fitting auto.arima on training set...")
    auto.arima(train_ts, stepwise = TRUE, approximation = TRUE)
  }, error = function(e) {
    log_warn("ARIMA failed: {conditionMessage(e)} — switching to ETS")
    model_type <<- "ETS"
    ets(train_ts)
  })
  
  log_info("Model fitted | type={model_type}")
  
  # ── STEP 5: Evaluate on test set (RMSE) ──────────────────────────────────────
  test_forecast <- forecast(fitted_model, h = n_test)
  test_actual   <- df_test$log_return
  test_pred     <- as.numeric(test_forecast$mean)
  
  rmse_test <- sqrt(mean((test_actual - test_pred)^2, na.rm = TRUE))
  log_info("Test RMSE (log returns) | {round(rmse_test, 6)}")
  
  # ── STEP 6: Refit on ALL data, then forecast horizon steps ahead ──────────────
  full_ts <- ts(df$log_return, frequency = 1)
  
  full_model <- tryCatch({
    auto.arima(full_ts, stepwise = TRUE, approximation = TRUE)
  }, error = function(e) {
    log_warn("Full refit ARIMA failed — using ETS")
    ets(full_ts)
  })
  
  future_forecast  <- forecast(full_model, h = forecast_horizon)
  forecast_returns <- as.numeric(future_forecast$mean)
  forecast_lower   <- as.numeric(future_forecast$lower[, 2])  # 95% CI lower
  forecast_upper   <- as.numeric(future_forecast$upper[, 2])  # 95% CI upper
  
  # ── STEP 7: Convert log return forecasts back to price levels ─────────────────
  last_price <- tail(df$adjusted, 1)
  last_date  <- tail(df$date, 1)
  
  price_forecast <- last_price * exp(cumsum(forecast_returns))
  price_lower    <- last_price * exp(cumsum(forecast_lower))
  price_upper    <- last_price * exp(cumsum(forecast_upper))
  
  forecast_dates <- seq(last_date + 1, by = "day", length.out = forecast_horizon)
  
  forecast_df <- data.frame(
    symbol         = target_symbol,
    forecast_date  = forecast_dates,
    log_return_hat = forecast_returns,
    price_hat      = price_forecast,
    price_lower    = price_lower,
    price_upper    = price_upper
  )
  
  log_info("Forecasts converted to prices | last_price={round(last_price, 2)}")
  
  # ── STEP 8: Save model artifact and metadata ──────────────────────────────────
  # UUID suffix: collision-proof even when tests call this function
  # multiple times per second. uuid package already installed (Stage 4).
  model_version <- paste0(
    tolower(model_type), "_",
    gsub("[^a-zA-Z0-9]", "", target_symbol), "_",
    format(Sys.time(), "%Y%m%d"),
    "_",
    substr(uuid::UUIDgenerate(), 1, 8)
  )
  
  # Save model RDS
  model_path <- here("models", paste0(model_version, ".rds"))
  saveRDS(full_model, file = model_path)
  
  # Save JSON metadata
  meta <- list(
    model_version    = model_version,
    symbol           = target_symbol,
    model_type       = model_type,
    n_train          = n_train,
    n_test           = n_test,
    forecast_horizon = forecast_horizon,
    data_hash        = data_hash,
    rmse_test        = round(rmse_test, 8),
    last_price       = round(last_price, 4),
    last_date        = format(last_date),
    created_at       = format(Sys.time())
  )
  
  meta_path <- here("models", paste0("metadata_", model_version, ".json"))
  jsonlite::write_json(meta, path = meta_path, auto_unbox = TRUE, pretty = TRUE)
  
  # Write to model registry in DuckDB
  registry_row <- data.frame(
    model_version    = model_version,
    symbol           = target_symbol,
    model_type       = model_type,
    n_train          = n_train,
    n_test           = n_test,
    forecast_horizon = as.integer(forecast_horizon),
    data_hash        = data_hash,
    rmse_test        = rmse_test,
    created_at       = Sys.time()
  )
  DBI::dbWriteTable(con, "model_registry", registry_row, append = TRUE)
  
  log_info("Model saved | version={model_version} | RMSE={round(rmse_test,6)}")
  log_info("Forecast complete | symbol={target_symbol} | horizon={forecast_horizon}")
  
  list(
    forecast_df   = forecast_df,
    model_meta    = meta,
    model_version = model_version,
    rmse_test     = rmse_test
  )
}


# -----------------------------------------------------------------------------
# inspect_model_registry()
# Shows all model runs recorded in DuckDB for audit.
# -----------------------------------------------------------------------------
inspect_model_registry <- function() {
  con <- get_db_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  result <- DBI::dbGetQuery(con,
                            "SELECT model_version, symbol, model_type, n_train, n_test,
            forecast_horizon, rmse_test, created_at
     FROM model_registry
     ORDER BY created_at DESC")
  
  print(result)
  invisible(result)
}


# -----------------------------------------------------------------------------
# print_forecast_summary()
# Human-readable summary of a forecast result.
# -----------------------------------------------------------------------------
print_forecast_summary <- function(forecast_result) {
  meta <- forecast_result$model_meta
  df   <- forecast_result$forecast_df
  
  cat("\n=== Forecast Summary ===\n")
  cat("Symbol        :", meta$symbol, "\n")
  cat("Model type    :", meta$model_type, "\n")
  cat("Model version :", meta$model_version, "\n")
  cat("Train rows    :", meta$n_train, "\n")
  cat("Test rows     :", meta$n_test, "\n")
  cat("Test RMSE     :", round(meta$rmse_test, 6), "(log return scale)\n")
  cat("Last price    :", meta$last_price, "\n")
  cat("Last date     :", meta$last_date, "\n")
  cat("Horizon       :", meta$forecast_horizon, "days\n")
  cat("Data hash     :", meta$data_hash, "\n\n")
  
  cat("Price forecast (first 10 days):\n")
  print(head(df[, c("forecast_date", "price_hat",
                    "price_lower", "price_upper")], 10))
  
  cat("\nPrice forecast (last 5 days):\n")
  print(tail(df[, c("forecast_date", "price_hat",
                    "price_lower", "price_upper")], 5))
}