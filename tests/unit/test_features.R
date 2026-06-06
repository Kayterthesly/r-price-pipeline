# =============================================================================
# tests/unit/test_features.R
# Pipeline: r-price-pipeline
# Stage: 6 — Unit Tests: Feature Engineering
# Run: testthat::test_file(here::here("tests/unit/test_features.R"))
# =============================================================================

library(testthat)
library(here)
library(dplyr)

source(here::here("r_scripts", "00_utils.R"))
source(here::here("r_scripts", "01_ingestion.R"))
source(here::here("r_scripts", "02_features.R"))

# ── Helper: ensure synthetic BTC-USD data exists ─────────────────────────────
setup_test_data <- function(symbol = "TEST-UNIT") {
  fetch_and_store_history(symbol, years_back = 2, overwrite = TRUE)
  generate_pipeline_features(symbol)
}

# ── Test suite ────────────────────────────────────────────────────────────────
test_that("feature table has correct columns", {
  setup_test_data()
  con <- get_db_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  df <- tbl(con, "feature_prices") |>
    dplyr::filter(symbol == "TEST-UNIT") |>
    dplyr::collect()
  
  expected_cols <- c("symbol", "date", "adjusted", "log_return",
                     "sma_50", "sma_200", "vol_30", "rsi_14",
                     "rolling_max", "drawdown")
  expect_true(all(expected_cols %in% names(df)),
              info = paste("Missing columns:", paste(setdiff(expected_cols, names(df)), collapse = ", ")))
})

test_that("log_return has exactly 1 NA (first row only)", {
  con <- get_db_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  df <- tbl(con, "feature_prices") |>
    dplyr::filter(symbol == "TEST-UNIT") |>
    dplyr::collect()
  
  n_na <- sum(is.na(df$log_return))
  expect_equal(n_na, 1L,
               info = paste("Expected 1 NA in log_return, got", n_na))
})

test_that("sma_50 has fewer NAs than sma_200", {
  con <- get_db_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  df <- tbl(con, "feature_prices") |>
    dplyr::filter(symbol == "TEST-UNIT") |>
    dplyr::collect()
  
  expect_lt(sum(is.na(df$sma_50)), sum(is.na(df$sma_200)),
            label = "sma_50 should have fewer NAs than sma_200")
})

test_that("leakage check passes for TEST-UNIT", {
  # Re-run check_leakage and capture output
  con <- get_db_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  df <- tbl(con, "feature_prices") |>
    dplyr::filter(symbol == "TEST-UNIT") |>
    dplyr::arrange(date) |>
    dplyr::collect()
  
  df_check <- df |>
    dplyr::mutate(
      sma_50_recomputed = TTR::SMA(adjusted, n = 50),
      sma_50_expected   = dplyr::lag(sma_50_recomputed, 1),
      leakage_flag      = abs(sma_50 - sma_50_expected) > 1e-10
    ) |>
    dplyr::filter(!is.na(leakage_flag))
  
  n_flags <- sum(df_check$leakage_flag, na.rm = TRUE)
  expect_equal(n_flags, 0L,
               info = paste("Leakage detected in", n_flags, "rows"))
})

test_that("drawdown is always <= 0", {
  con <- get_db_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  df <- tbl(con, "feature_prices") |>
    dplyr::filter(symbol == "TEST-UNIT") |>
    dplyr::collect() |>
    dplyr::filter(!is.na(drawdown))
  
  expect_true(all(df$drawdown <= 0),
              info = "Drawdown must always be <= 0 (price below or at rolling max)")
})

test_that("feature rows match raw_prices rows", {
  con <- get_db_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  n_raw <- tbl(con, "raw_prices") |>
    dplyr::filter(symbol == "TEST-UNIT") |>
    dplyr::collect() |> nrow()
  
  n_feat <- tbl(con, "feature_prices") |>
    dplyr::filter(symbol == "TEST-UNIT") |>
    dplyr::collect() |> nrow()
  
  expect_equal(n_raw, n_feat,
               info = "feature_prices must have same row count as raw_prices")
})