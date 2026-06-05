# =============================================================================
# 01_ingestion.R
# Pipeline: r-price-pipeline
# Stage: 1 — Data Ingestion
# Purpose: Fetch price data (synthetic or Yahoo Finance) → store in DuckDB
# Usage: source this file, then call fetch_and_store_history("BTC-USD")
# =============================================================================

source(here::here("r_scripts", "00_utils.R"))  # get_db_connection()

library(dplyr)
library(dbplyr)
library(tidyquant)
library(logger)
library(here)

# -----------------------------------------------------------------------------
# ensure_raw_prices_table()
# Creates the raw_prices table in DuckDB if it doesn't exist yet.
# Safe to call multiple times — CREATE TABLE IF NOT EXISTS is idempotent.
# -----------------------------------------------------------------------------
ensure_raw_prices_table <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS raw_prices (
      symbol   VARCHAR NOT NULL,
      date     DATE    NOT NULL,
      open     DOUBLE,
      high     DOUBLE,
      low      DOUBLE,
      close    DOUBLE,
      adjusted DOUBLE,
      volume   DOUBLE,
      PRIMARY KEY (symbol, date)
    );
  ")
  log_info("raw_prices table verified/created")
}

# -----------------------------------------------------------------------------
# upsert_prices()
# Writes a data frame into raw_prices using DuckDB's upsert pattern.
# Safe to call multiple times — existing rows are updated, new rows inserted.
# -----------------------------------------------------------------------------
upsert_prices <- function(con, prices_df) {
  
  # Write to a temporary staging table
  dbWriteTable(con, "tmp_prices", prices_df, overwrite = TRUE)
  
  # Upsert: insert new rows, update existing ones
  dbExecute(con, "
    INSERT INTO raw_prices
    SELECT * FROM tmp_prices
    ON CONFLICT (symbol, date) DO UPDATE SET
      open     = excluded.open,
      high     = excluded.high,
      low      = excluded.low,
      close    = excluded.close,
      adjusted = excluded.adjusted,
      volume   = excluded.volume;
  ")
  
  # Clean up staging table
  dbExecute(con, "DROP TABLE IF EXISTS tmp_prices;")
  
  log_info("Upsert complete | rows={nrow(prices_df)}")
}

# -----------------------------------------------------------------------------
# fetch_and_store_history()
# Main ingestion function. Pulls data based on ENV_MODE and stores in DuckDB.
#
# Args:
#   symbol     : ticker string e.g. "BTC-USD", "AAPL", "DANGCEM.LG"
#   years_back : how many years of history to fetch (default 10)
#   overwrite  : if TRUE, deletes existing rows for symbol before inserting
# -----------------------------------------------------------------------------
fetch_and_store_history <- function(symbol, years_back = 10, overwrite = FALSE) {
  
  # Input validation
  if (!is.character(symbol) || nchar(trimws(symbol)) == 0) {
    stop("[INGEST ABORT] symbol must be a non-empty string", call. = FALSE)
  }
  if (!is.numeric(years_back) || years_back <= 0) {
    stop("[INGEST ABORT] years_back must be a positive number", call. = FALSE)
  }
  
  mode <- Sys.getenv("ENV_MODE", unset = "synthetic")
  log_info("Starting ingestion | symbol={symbol} | mode={mode} | years_back={years_back}")
  
  con <- get_db_connection()
  on.exit(dbDisconnect(con, shutdown = TRUE))  # always close, even on error
  
  ensure_raw_prices_table(con)
  
  # Optionally wipe existing data for this symbol
  if (overwrite) {
    deleted <- dbExecute(con,
                         "DELETE FROM raw_prices WHERE symbol = ?",
                         params = list(symbol))
    log_info("Overwrite: deleted {deleted} existing rows for {symbol}")
  }
  
  # ── SYNTHETIC MODE ──────────────────────────────────────────────────────────
  if (mode == "synthetic") {
    
    total_days  <- as.integer(years_back * 252)  # trading days, not calendar
    all_dates   <- seq(Sys.Date() - total_days + 1, Sys.Date(), by = "day")
    
    # Filter to weekdays only (Mon–Fri) — stock markets closed on weekends
    trading_dates <- all_dates[!weekdays(all_dates) %in% c("Saturday", "Sunday")]
    n             <- length(trading_dates)
    
    set.seed(42)  # deterministic synthetic data — same seed = same data
    base_price <- 100
    
    synthetic_df <- data.frame(
      symbol   = rep(symbol, n),
      date     = trading_dates,
      open     = runif(n, base_price * 0.98, base_price * 1.02),
      high     = runif(n, base_price * 1.01, base_price * 1.05),
      low      = runif(n, base_price * 0.93, base_price * 0.99),
      close    = base_price + cumsum(rnorm(n, mean = 0.05, sd = 1)),
      adjusted = base_price + cumsum(rnorm(n, mean = 0.05, sd = 1)),
      volume   = sample(100000:10000000, n, replace = TRUE)
    )
    
    upsert_prices(con, synthetic_df)
    log_info("Synthetic ingestion complete | symbol={symbol} | rows={n}")
    
    # ── LIVE YAHOO MODE ─────────────────────────────────────────────────────────
  } else if (mode %in% c("live_yahoo", "production")) {
    
    start_date <- Sys.Date() - as.integer(years_back * 365)
    end_date   <- Sys.Date()
    
    log_info("Fetching from Yahoo Finance | from={start_date} | to={end_date}")
    
    prices_raw <- tryCatch({
      tq_get(
        symbol,
        from = as.character(start_date),
        to   = as.character(end_date),
        get  = "stock.prices"
      )
    }, error = function(e) {
      stop("[INGEST ABORT] Yahoo Finance fetch failed for '", symbol, "': ",
           conditionMessage(e), call. = FALSE)
    })
    
    if (is.null(prices_raw) || nrow(prices_raw) == 0) {
      stop("[INGEST ABORT] No data returned for symbol '", symbol,
           "'. Check the ticker and try again.", call. = FALSE)
    }
    
    # Standardize columns — !!symbol forces the function parameter (not column)
    prices_clean <- prices_raw %>%
      transmute(
        symbol   = !!symbol,
        date     = as.Date(date),
        open     = as.double(open),
        high     = as.double(high),
        low      = as.double(low),
        close    = as.double(close),
        adjusted = as.double(adjusted),
        volume   = as.double(volume)
      )
    
    upsert_prices(con, prices_clean)
    log_info("Live ingestion complete | symbol={symbol} | rows={nrow(prices_clean)}")
  }
}

# -----------------------------------------------------------------------------
# inspect_raw_prices()
# Quick audit query — shows what's in the DB after ingestion.
# -----------------------------------------------------------------------------
inspect_raw_prices <- function(symbol = NULL) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con, shutdown = TRUE))
  
  query <- if (!is.null(symbol)) {
    paste0("SELECT symbol, MIN(date) AS earliest, MAX(date) AS latest,
            COUNT(*) AS total_rows
            FROM raw_prices WHERE symbol = '", symbol, "'
            GROUP BY symbol")
  } else {
    "SELECT symbol, MIN(date) AS earliest, MAX(date) AS latest,
     COUNT(*) AS total_rows FROM raw_prices GROUP BY symbol"
  }
  
  result <- dbGetQuery(con, query)
  print(result)
  invisible(result)
}