# =============================================================================
# tests/unit/test_modeling.R
# Pipeline: r-price-pipeline
# Stage: 6 — Unit Tests: Modeling
# Run: testthat::test_file(here::here("tests/unit/test_modeling.R"))
# =============================================================================

library(testthat)
library(here)

source(here::here("r_scripts", "00_utils.R"))
source(here::here("r_scripts", "03_modeling.R"))

test_that("compute_asset_forecasts rejects empty symbol", {
  expect_error(
    compute_asset_forecasts("", forecast_horizon = 10),
    regexp = "non-empty string"
  )
})

test_that("compute_asset_forecasts rejects horizon > 365", {
  expect_error(
    compute_asset_forecasts("TEST-UNIT", forecast_horizon = 400),
    regexp = "between 1 and 365"
  )
})

test_that("compute_asset_forecasts rejects horizon < 1", {
  expect_error(
    compute_asset_forecasts("TEST-UNIT", forecast_horizon = 0),
    regexp = "between 1 and 365"
  )
})

test_that("forecast result has required fields", {
  result <- compute_asset_forecasts("TEST-UNIT", forecast_horizon = 10)
  
  expect_true(is.list(result))
  expect_true("forecast_df"   %in% names(result))
  expect_true("model_meta"    %in% names(result))
  expect_true("model_version" %in% names(result))
  expect_true("rmse_test"     %in% names(result))
})

test_that("forecast_df has correct number of rows", {
  horizon <- 10L
  result  <- compute_asset_forecasts("TEST-UNIT", forecast_horizon = horizon)
  expect_equal(nrow(result$forecast_df), horizon)
})

test_that("model_meta contains required keys", {
  result <- compute_asset_forecasts("TEST-UNIT", forecast_horizon = 10)
  meta   <- result$model_meta
  
  required_keys <- c("model_version", "symbol", "model_type",
                     "n_train", "n_test", "rmse_test",
                     "data_hash", "last_price", "last_date")
  expect_true(all(required_keys %in% names(meta)),
              info = paste("Missing keys:", paste(setdiff(required_keys, names(meta)), collapse = ", ")))
})

test_that("RMSE is a positive finite number", {
  result <- compute_asset_forecasts("TEST-UNIT", forecast_horizon = 10)
  expect_true(is.finite(result$rmse_test))
  expect_gt(result$rmse_test, 0)
})

test_that("model run is recorded in model_registry", {
  result <- compute_asset_forecasts("TEST-UNIT", forecast_horizon = 10)
  
  con <- get_db_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  registry <- DBI::dbGetQuery(con,
                              paste0("SELECT * FROM model_registry WHERE model_version = '",
                                     result$model_version, "'"))
  
  expect_equal(nrow(registry), 1L,
               info = "Model run must be recorded in model_registry table")
})