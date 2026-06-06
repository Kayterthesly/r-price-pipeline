# =============================================================================
# tests/data_tests.R
# Pipeline: r-price-pipeline
# Stage: 6 — Data Tests: DuckDB schema and integrity validation
# Run: Rscript tests/data_tests.R
# Used in CI to validate DB state before model training
# =============================================================================

library(here)
library(DBI)

source(here::here("r_scripts", "00_utils.R"))

cat("\n=== Running Data Tests ===\n\n")
passed <- 0L
failed <- 0L

run_test <- function(name, expr) {
  result <- tryCatch(expr, error = function(e) FALSE)
  if (isTRUE(result)) {
    cat("✅ PASS:", name, "\n")
    passed <<- passed + 1L
  } else {
    cat("❌ FAIL:", name, "\n")
    failed <<- failed + 1L
  }
}

con <- get_db_connection()
on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

# ── Table existence ───────────────────────────────────────────────────────────
run_test("raw_prices table exists", {
  DBI::dbExistsTable(con, "raw_prices")
})

run_test("feature_prices table exists", {
  DBI::dbExistsTable(con, "feature_prices")
})

run_test("model_registry table exists", {
  DBI::dbExistsTable(con, "model_registry")
})

# ── Schema validation ─────────────────────────────────────────────────────────
run_test("raw_prices has required columns", {
  cols <- DBI::dbListFields(con, "raw_prices")
  all(c("symbol", "date", "open", "high", "low",
        "close", "adjusted", "volume") %in% cols)
})

run_test("feature_prices has required columns", {
  cols <- DBI::dbListFields(con, "feature_prices")
  all(c("symbol", "date", "adjusted", "log_return",
        "sma_50", "sma_200", "vol_30", "drawdown") %in% cols)
})

# ── Row counts ────────────────────────────────────────────────────────────────
run_test("raw_prices has data (> 100 rows)", {
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM raw_prices")$n
  n > 100
})

run_test("feature_prices has data (> 100 rows)", {
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM feature_prices")$n
  n > 100
})

# ── Data integrity ────────────────────────────────────────────────────────────
run_test("No NULL symbols in raw_prices", {
  n <- DBI::dbGetQuery(con,
                       "SELECT COUNT(*) AS n FROM raw_prices WHERE symbol IS NULL")$n
  n == 0
})

run_test("No NULL dates in raw_prices", {
  n <- DBI::dbGetQuery(con,
                       "SELECT COUNT(*) AS n FROM raw_prices WHERE date IS NULL")$n
  n == 0
})

run_test("No negative prices in raw_prices", {
  n <- DBI::dbGetQuery(con,
                       "SELECT COUNT(*) AS n FROM raw_prices WHERE adjusted < 0")$n
  n == 0
})

run_test("feature_prices row count matches raw_prices", {
  n_raw  <- DBI::dbGetQuery(con,
                            "SELECT COUNT(*) AS n FROM raw_prices WHERE symbol = 'BTC-USD'")$n
  n_feat <- DBI::dbGetQuery(con,
                            "SELECT COUNT(*) AS n FROM feature_prices WHERE symbol = 'BTC-USD'")$n
  n_raw == n_feat
})

run_test("model_registry has at least one entry", {
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM model_registry")$n
  n > 0
})

# ── Summary ───────────────────────────────────────────────────────────────────
cat("\n=== Data Test Summary ===\n")
cat("Passed:", passed, "\n")
cat("Failed:", failed, "\n")

if (failed > 0) {
  cat("\n❌ DATA TESTS FAILED — do not proceed to model training\n")
  quit(status = 1)
} else {
  cat("\n✅ All data tests passed\n")
  quit(status = 0)
}