# r-price-pipeline

Stock & Crypto Price Analysis and Prediction Pipeline
**Author:** Kingsley Akenu (@Kayterthesly / KAIZEN 改善)
**Stack:** R 4.5.2 · DuckDB · tidyquant · ARIMA · Prophet · Plumber · Shiny

---

## Pipeline Log

### Stage 0 — Workspace Setup
**Date:** 2026-06-03 | **Commit:** 5dcfa2d | **Status:** ✅ Complete
- renv initialized (bare), packages locked: here, logger, purrr
- `.Renviron` configured: ENV_MODE, DB_PATH, LOG_LEVEL
- Directory scaffold created, Git initialized

### Stage 1 — Data Ingestion
**Date:** 2026-06-05 | **Commit:** ecd680e | **Status:** ✅ Complete
- DuckDB store: `data/pipeline_store.duckdb`
- Table: `raw_prices` — PRIMARY KEY (symbol, date)
- Synthetic test: 1,800 rows | Live Yahoo test: 2,008 rows total
- Upsert confirmed: 523 synthetic rows replaced by real market data