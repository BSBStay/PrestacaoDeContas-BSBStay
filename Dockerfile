
# ============================================================
# Dockerfile — BSBStay Shiny App
# Otimizado para Render.com (Docker runtime)
# ============================================================

FROM rocker/r-ver:4.3.3

ENV DEBIAN_FRONTEND=noninteractive

# ── Dependências do sistema ───────────────────────────────────
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    libxt-dev \
    libsqlite3-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    zlib1g-dev \
    libuv1-dev \
    pandoc \
    make \
    g++ \
    curl \
    && rm -rf /var/lib/apt/lists/*

# ── Pacotes R ─────────────────────────────────────────────────
RUN R -q -e "install.packages(c( \
    'shiny','dplyr','tidyr','lubridate','readxl','janitor', \
    'plotly','DT','DBI','RSQLite','shinycssloaders','stringr', \
    'htmlwidgets','bslib','digest','later','htmltools','openxlsx' \
  ), repos='https://cloud.r-project.org', Ncpus=parallel::detectCores())"

# ── Diretório de trabalho ─────────────────────────────────────
WORKDIR /opt/render/project/src

# ── Copia o código-fonte ──────────────────────────────────────
COPY . .

# ── Diretórios locais ─────────────────────────────────────────
RUN mkdir -p data/raw

RUN mkdir -p /data/bsbstay && chmod 777 /data/bsbstay \
 && mkdir -p /tmp/bsbstay_cache && chmod 777 /tmp/bsbstay_cache

# ── Variáveis de ambiente ─────────────────────────────────────
ENV APP_ROOT=/opt/render/project/src \
    APP_CACHE_DIR=/data/bsbstay \
    APP_MODE=public \
    MAX_CACHE_AGE_H=6 \
    PORT=3838

# ── Healthcheck ───────────────────────────────────────────────
HEALTHCHECK --interval=30s --timeout=10s --start-period=180s --retries=3 \
  CMD curl -sf http://localhost:${PORT} || exit 1

EXPOSE 3838

CMD ["R", "-q", "-f", "/opt/render/project/src/run.R"]

