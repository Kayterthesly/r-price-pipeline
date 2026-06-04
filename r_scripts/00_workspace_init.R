# =============================================================================
# 00_workspace_init.R
# Pipeline: r-price-pipeline
# Stage: 0 — Workspace Setup
# Author: Kingsley (@Kayterthesly / KAIZEN 改善)
# =============================================================================

library(here)
library(logger)
library(purrr)

# -------------------------------------------------------------------
# GUARD: verify renv lockfile exists before anything runs
# -------------------------------------------------------------------
if (!file.exists(here("renv.lock"))) {
  stop(
    "[PIPELINE ABORT] renv.lock not found. ",
    "Run renv::init(bare=TRUE) then renv::snapshot() in console first.",
    call. = FALSE
  )
}

# -------------------------------------------------------------------
# LOGGER: map .Renviron LOG_LEVEL string to logger constant safely
# -------------------------------------------------------------------
log_level_str <- Sys.getenv("LOG_LEVEL", unset = "INFO")

log_level_map <- list(
  TRACE = logger::TRACE,
  DEBUG = logger::DEBUG,
  INFO  = logger::INFO,
  WARN  = logger::WARN,
  ERROR = logger::ERROR,
  FATAL = logger::FATAL
)

if (!log_level_str %in% names(log_level_map)) {
  warning(paste0("Unknown LOG_LEVEL '", log_level_str, "', defaulting to INFO"))
  log_level_str <- "INFO"
}

log_threshold(log_level_map[[log_level_str]])
log_info("Logger initialized | level={log_level_str}")

# -------------------------------------------------------------------
# ENV VALIDATION: fail loudly if .Renviron vars are missing
# -------------------------------------------------------------------
required_env_vars <- c("ENV_MODE", "DB_PATH", "LOG_LEVEL")
missing_vars <- required_env_vars[nchar(Sys.getenv(required_env_vars)) == 0]

if (length(missing_vars) > 0) {
  stop(
    "[PIPELINE ABORT] Missing .Renviron variables: ",
    paste(missing_vars, collapse = ", "),
    call. = FALSE
  )
}

log_info("Environment mode: {Sys.getenv('ENV_MODE')}")
log_info("Database path: {here(Sys.getenv('DB_PATH'))}")

# -------------------------------------------------------------------
# DIRECTORY SCAFFOLD: create all pipeline folders
# -------------------------------------------------------------------
pipeline_dirs <- c(
  here("data"),
  here("r_scripts"),
  here("api"),
  here("models"),
  here("notes"),
  here("tests", "unit"),
  here("dashboard"),
  here("infra")
)

purrr::walk(pipeline_dirs, function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    log_info("Created directory: {dir_path}")
  } else {
    log_info("Directory exists (skipped): {dir_path}")
  }
})

log_info("=================================================")
log_info("Stage 0 PASSED. Proceed to Stage 1: Ingestion.")
log_info("=================================================")