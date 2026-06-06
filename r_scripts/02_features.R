# =============================================================================
# 02_features.R
# Pipeline: r-price-pipeline
# Stage: 2 — Feature Engineering
# Purpose: compute lag-safe technical features from raw prices → feature_prices
# Usage: source this file, then call generate_pipeline_features("BTC-USD")
# =============================================================================

source(here::here("r_scripts", "00_utils.R"))  # get_db_connection()

library(dplyr)
library(TTR)
library(logger)
library(here)

# -----------------------------------------------------------------------------
# generate_pipeline_features()
# Reads raw_prices for a symbol, computes features, writes to feature_prices.
#
# LEAKAGE POLICY:
#   - log_return  : NOT lagged — backward-looking by definition (safe)
#   - sma_50      : lagged 1 — uses today's price, so shift to yesterday
#   - sma_200     : lagged 1 — same reason
#   - vol_30      : lagged 1 — rolling SD of returns, shift to yesterday
#   - rsi_14      : lagged 1 — momentum signal, shift to yesterday
#   - rolling_max : lagged 1 — uses today's price in cummax
#   - drawdown    : computed from lagged adjusted + lagged rolling_max
#
# Args:
#   target_symbol : ticker string e.g. "BTC-USD", "DANGCEM.LG"
# -----------------------------------------------------------------------------
generate_pipeline_features <- function(target_symbol) {
  
  # Input validation
  if (!is.character(target_symbol) || nchar(trimws(target_symbol)) == 0) {
    stop("[FEATURES ABORT] target_symbol must be a non-empty string", call. = FALSE)
  }
  
  log_info("Starting feature engineering | symbol={target_symbol}")
  
  con <- get_db_connection()
  on.exit(dbDisconnect(con, shutdown = TRUE))
  
  # ── STEP 1: Pull raw prices from DuckDB ─────────────────────────────────────
  raw_df <- tbl(con, "raw_prices") %>%
    dplyr::filter(symbol == !!target_symbol) %>%
    dplyr::arrange(date) %>%
    dplyr::collect()
  
  if (nrow(raw_df) == 0) {
    stop("[FEATURES ABORT] No data found for symbol '", target_symbol,
         "'. Run fetch_and_store_history() first.", call. = FALSE)
  }
  
  log_info("Raw data loaded | symbol={target_symbol} | rows={nrow(raw_df)}")
  
  # ── STEP 2: Compute log return (no lag needed — purely backward-looking) ─────
  df_step1 <- raw_df %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      log_return = log(adjusted) - log(dplyr::lag(adjusted, 1))
    )
  
  # ── STEP 3: Compute rolling features (pre-lag — using actual aligned values) ─
  # These are computed correctly first, then lagged below.
  # Split into separate mutate() calls to avoid referencing half-built columns.
  df_step2 <- df_step1 %>%
    dplyr::mutate(
      sma_50_raw      = TTR::SMA(adjusted, n = 50),
      sma_200_raw     = TTR::SMA(adjusted, n = 200),
      vol_30_raw      = TTR::runSD(log_return, n = 30) * sqrt(252),
      rsi_14_raw      = TTR::RSI(adjusted, n = 14),
      rolling_max_raw = cummax(adjusted)
    )
  
  # ── STEP 4: Lag all rolling features by 1 day (leakage prevention) ───────────
  df_features <- df_step2 %>%
    dplyr::mutate(
      sma_50      = dplyr::lag(sma_50_raw,      1),
      sma_200     = dplyr::lag(sma_200_raw,     1),
      vol_30      = dplyr::lag(vol_30_raw,       1),
      rsi_14      = dplyr::lag(rsi_14_raw,       1),
      rolling_max = dplyr::lag(rolling_max_raw, 1),
      
      # Drawdown: uses lagged adjusted price + lagged rolling max — fully leak-free
      drawdown    = (dplyr::lag(adjusted, 1) / dplyr::lag(rolling_max_raw, 1)) - 1
    ) %>%
    # Drop intermediate _raw columns — not needed in feature table
    dplyr::select(-ends_with("_raw"))
  
  log_info("Features computed | symbol={target_symbol} | columns={ncol(df_features)}")
  
  # ── STEP 5: Write feature table to DuckDB ────────────────────────────────────
  # Symbol-aware write: preserve other symbols, replace only target_symbol
  # This mirrors the raw_prices upsert pattern — multi-symbol, persistent
  if (!DBI::dbExistsTable(con, "feature_prices")) {
    # First run ever: create table from data frame schema
    DBI::dbWriteTable(con, "feature_prices", df_features, overwrite = FALSE)
    log_info("feature_prices table created | rows={nrow(df_features)}")
  } else {
    # Subsequent runs: delete this symbol's rows, append fresh ones
    deleted <- DBI::dbExecute(con,
                              "DELETE FROM feature_prices WHERE symbol = ?",
                              params = list(target_symbol))
    log_info("Deleted {deleted} existing rows for {target_symbol}")
    DBI::dbWriteTable(con, "feature_prices", df_features, append = TRUE)
    log_info("feature_prices rows appended | rows={nrow(df_features)}")
  }
  
  log_info("Feature engineering complete | symbol={target_symbol}")
  invisible(df_features)
}


# -----------------------------------------------------------------------------
# inspect_feature_prices()
# Audit the feature table — shows column stats and NA counts.
# NAs are expected at the start of each rolling window (not bugs).
# -----------------------------------------------------------------------------
inspect_feature_prices <- function(target_symbol) {
  
  con <- get_db_connection()
  on.exit(dbDisconnect(con, shutdown = TRUE))
  
  df <- tbl(con, "feature_prices") %>%
    dplyr::filter(symbol == !!target_symbol) %>%
    dplyr::collect()
  
  if (nrow(df) == 0) {
    message("No feature data found for '", target_symbol, "'")
    return(invisible(NULL))
  }
  
  cat("\n=== Feature Table Inspection ===\n")
  cat("Symbol  :", target_symbol, "\n")
  cat("Rows    :", nrow(df), "\n")
  cat("Columns :", ncol(df), "\n")
  cat("Date range:", format(min(df$date)), "→", format(max(df$date)), "\n\n")
  
  # NA count per feature column
  feature_cols <- c("log_return", "sma_50", "sma_200",
                    "vol_30", "rsi_14", "rolling_max", "drawdown")
  cat("NA counts per feature (expected — rolling window warm-up):\n")
  na_counts <- sapply(feature_cols, function(col) sum(is.na(df[[col]])))
  print(data.frame(feature = names(na_counts), na_count = na_counts,
                   row.names = NULL))
  
  cat("\nFirst 5 rows with features:\n")
  print(head(df[, c("symbol", "date", "adjusted", "log_return",
                    "sma_50", "vol_30", "drawdown")], 5))
  
  cat("\nLast 5 rows with features:\n")
  print(tail(df[, c("symbol", "date", "adjusted", "log_return",
                    "sma_50", "vol_30", "drawdown")], 5))
  
  invisible(df)
}


# -----------------------------------------------------------------------------
# check_leakage()
# Basic leakage test: verifies that each lagged feature on day T
# matches the un-lagged value on day T-1.
# A mismatch means the lag was not applied correctly.
# -----------------------------------------------------------------------------
check_leakage <- function(target_symbol) {
  
  con <- get_db_connection()
  on.exit(dbDisconnect(con, shutdown = TRUE))
  
  df <- tbl(con, "feature_prices") %>%
    dplyr::filter(symbol == !!target_symbol) %>%
    dplyr::arrange(date) %>%
    dplyr::collect()
  
  cat("\n=== Leakage Check ===\n")
  cat("Symbol:", target_symbol, "\n\n")
  
  # For each lagged feature: value on row T must equal raw on row T-1
  # We check sma_50 as the representative test
  df_check <- df %>%
    dplyr::mutate(
      sma_50_recomputed = TTR::SMA(adjusted, n = 50),
      # sma_50 in feature table should equal yesterday's sma_50_recomputed
      sma_50_expected   = dplyr::lag(sma_50_recomputed, 1),
      leakage_flag      = abs(sma_50 - sma_50_expected) > 1e-10
    ) %>%
    dplyr::filter(!is.na(leakage_flag))
  
  n_flags <- sum(df_check$leakage_flag, na.rm = TRUE)
  
  if (n_flags == 0) {
    cat("✅ PASSED — sma_50 lag verified. No leakage detected.\n")
  } else {
    cat("❌ FAILED —", n_flags, "rows with leakage detected in sma_50.\n")
    cat("Review generate_pipeline_features() lag logic.\n")
  }
  
  cat("Rows checked:", nrow(df_check), "\n")
  invisible(df_check)
}