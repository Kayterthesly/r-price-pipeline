# =============================================================================
# infra/Dockerfile
# Pipeline: r-price-pipeline
# Stage: 6 — Containerized Plumber API
# Build: docker build -f infra/Dockerfile -t r-price-pipeline .
# Run:   docker run -p 8000:8000 r-price-pipeline
# =============================================================================

FROM rocker/r-ver:4.5.0

# System dependencies for R packages
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy lockfile first — Docker cache layer: only re-install if lockfile changes
COPY renv.lock renv.lock
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json
COPY renv/.gitignore renv/.gitignore
COPY .Rprofile .Rprofile

# Install renv and restore packages
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"
RUN R -e "renv::restore(clean = TRUE, prompt = FALSE)"

# Copy application code (after package install for better caching)
COPY r_scripts/ r_scripts/
COPY api/ api/

# Create required directories
RUN mkdir -p data models notes

# Environment — override with docker run -e ENV_MODE=live_yahoo
ENV ENV_MODE=synthetic
ENV DB_PATH=data/pipeline_store.duckdb
ENV LOG_LEVEL=INFO

EXPOSE 8000

# Health check: ping /health every 30 seconds
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
  CMD curl -f http://localhost:8000/health || exit 1

ENTRYPOINT ["R", "-e", "source('api/run_api.R')"]