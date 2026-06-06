# r-price-pipeline

Stock & Crypto Price Analysis and Prediction Pipeline  
**Author:** Kingsley Akenu (@Kayterthesly / KAIZEN 改善)  
**Stack:** R 4.5.2 · DuckDB · tidyquant · ARIMA · Prophet · Plumber · Shiny

---

## Pipeline Log

### Stage 0 — Workspace Setup
**Date:** 2026-06-03 | **Commit:** 5dcfa2d | **Status:** Complete
- renv initialized (bare), packages locked: here, logger, purrr
- `.Renviron` configured: ENV_MODE, DB_PATH, LOG_LEVEL
- Directory scaffold created, Git initialized

### Stage 1 — Data Ingestion
**Date:** 2026-06-05 | **Commit:** ecd680e | **Status:** Complete
- DuckDB store: `data/pipeline_store.duckdb`
- Table: `raw_prices` — PRIMARY KEY (symbol, date)
- Synthetic test: 1,800 rows | Live Yahoo test: 2,008 rows total
- Upsert confirmed: 523 synthetic rows replaced by real market data

### Stage 2 — Feature Engineering
**Date:** 2026-06-05 | **Commit:** 0177c87 | **Status:** Complete
- `feature_prices` table: 2,008 rows, 15 columns
- Features: log_return, sma_50, sma_200, vol_30, rsi_14, rolling_max, drawdown
- All rolling features lagged by 1 — zero leakage confirmed
- Leakage check: PASSED — 1,958 rows verified

### Stage 3 — Modeling & Forecasting
**Date:** 2026-06-06 | **Commit:** 9c0e939 | **Status:** ✅ Complete

- Model: ARIMA (auto-selected) on log returns, ETS fallback
- Train: 1,446 rows | Test: 362 rows | RMSE: 0.022626
- 30-day forecast: flat point estimate (white noise), expanding CI cone
- Data hash: 987a1a9a896623cb177c6ae4377d881f
- Artifacts: `models/arima_BTCUSD_20260606031326.rds` + `.json`
- `model_registry` table initialized in DuckDB

### Stage 4 — REST API (Plumber)
**Date:** 2026-06-06 | **Commit:** b570b1c | **Status:** ✅ Complete

- `GET /health` — liveness check, returns env_mode and r_version
- `POST /predict/price` — full ARIMA forecast pipeline over HTTP
- trace_id propagation verified end-to-end (httr ↔ background job logs)
- Input validation + tryCatch error handling on all endpoints