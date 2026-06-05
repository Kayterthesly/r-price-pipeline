# =============================================================================
# 00_utils.R
# Pipeline: r-price-pipeline
# Purpose: Shared utilities — DB connection, common helpers
# Sourced by: all stage scripts (01, 02, 03...)
# NEVER run this file directly — always source() it from another script
# =============================================================================

library(DBI)
library(duckdb)
library(logger)
library(here)

# -----------------------------------------------------------------------------
# get_db_connection()
# Returns an active DuckDB (or production) connection based on ENV_MODE.
# Caller is responsible for closing: on.exit(dbDisconnect(con, shutdown=TRUE))
# -----------------------------------------------------------------------------
get_db_connection <- function() {
  
  mode    <- Sys.getenv("ENV_MODE",  unset = "synthetic")
  db_file <- here(Sys.getenv("DB_PATH", unset = "data/pipeline_store.duckdb"))
  
  log_info("Opening DB connection | mode={mode} | path={db_file}")
  
  if (mode %in% c("synthetic", "live_yahoo")) {
    
    # Ensure data/ directory exists before DuckDB creates the file
    if (!dir.exists(dirname(db_file))) {
      dir.create(dirname(db_file), recursive = TRUE)
    }
    
    con <- dbConnect(duckdb(), dbdir = db_file, read_only = FALSE)
    
  } else if (mode == "production") {
    
    # Production warehouse — credentials come from .Renviron only
    # Add PROD_DB_HOST, PROD_DB_NAME, PROD_DB_PORT, PROD_DB_USER, PROD_DB_PASS
    # to .Renviron when production mode is activated (Stage 6+)
    required_prod_vars <- c(
      "PROD_DB_HOST", "PROD_DB_NAME", "PROD_DB_PORT",
      "PROD_DB_USER", "PROD_DB_PASS"
    )
    missing <- required_prod_vars[nchar(Sys.getenv(required_prod_vars)) == 0]
    if (length(missing) > 0) {
      stop("[DB ABORT] Missing production .Renviron vars: ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
    
    con <- DBI::dbConnect(
      RPostgres::Postgres(),
      host     = Sys.getenv("PROD_DB_HOST"),
      dbname   = Sys.getenv("PROD_DB_NAME"),
      port     = as.integer(Sys.getenv("PROD_DB_PORT")),
      user     = Sys.getenv("PROD_DB_USER"),
      password = Sys.getenv("PROD_DB_PASS")
    )
    
  } else {
    stop("[DB ABORT] Unknown ENV_MODE: '", mode, "'. ",
         "Set ENV_MODE to synthetic, live_yahoo, or production in .Renviron",
         call. = FALSE)
  }
  
  con
}