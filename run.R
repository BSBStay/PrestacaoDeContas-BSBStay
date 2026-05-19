# run.R — Ponto de entrada único para Docker / Render.com
#
# Responsabilidades EXCLUSIVAS deste arquivo:
#   1. Configurar shiny.host e shiny.port (UMA VEZ)
#   2. Apontar APP_CACHE_DIR para /tmp (gravável no Render)
#   3. Pré-aquecer o cache SQLite (UMA VEZ, antes do primeiro usuário)
#   4. Iniciar o app

# ── 1. Host / Port ────────────────────────────────────────────
options(
  shiny.host            = "0.0.0.0",
  shiny.port            = as.integer(Sys.getenv("PORT", "3838")),
  shiny.maxRequestSize  = 50 * 1024^2,
  shiny.sanitize.errors = FALSE
)

# ── 2. Paths ──────────────────────────────────────────────────
# /opt/render/project/src é read-only após o build.
# Dados mutáveis (SQLite, xlsx cache) devem ir para /tmp.
app_root  <- normalizePath(Sys.getenv("APP_ROOT", "."), winslash = "/", mustWork = FALSE)
cache_dir <- Sys.getenv("APP_CACHE_DIR", "/tmp/bsbstay_cache")
raw_dir   <- Sys.getenv("APP_RAW_DIR",   file.path(app_root, "data", "raw"))

dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(raw_dir,   recursive = TRUE, showWarnings = FALSE)

Sys.setenv(APP_CACHE_DIR = cache_dir)
Sys.setenv(APP_RAW_DIR   = raw_dir)
Sys.setenv(APP_ROOT      = app_root)

message(sprintf("[run.R] APP_ROOT      = %s", app_root))
message(sprintf("[run.R] APP_CACHE_DIR = %s", cache_dir))

# ── 3. Pacotes mínimos ────────────────────────────────────────
suppressPackageStartupMessages({ library(shiny); library(later) })

# ── 4. gdrive_public.R — carrega UMA VEZ no ambiente global ──
# Todos os módulos filhos verificam exists("carregar_dados_app")
# antes de fazer source novamente, evitando cargas duplas.
source(file.path(app_root, "R", "gdrive_public.R"), local = FALSE)

# ── 5. Pré-aquecimento em 2 etapas ──────────────────────────
#
# Etapa A (síncrona, antes de abrir a porta):
#   Tenta carregar APENAS do SQLite local (forcar_dl=FALSE, sem network).
#   Se o SQLite existir, é rápido (~1-2s) e a porta abre no prazo do Render.
#   Se não existir ainda (fresh deploy), APP_DATA_GLOBAL fica vazio.
#
# Etapa B (assíncrona, após a porta abrir):
#   Baixa o xlsx do Drive e reprocessa o ETL.
#   Atualiza APP_DATA_GLOBAL. O polling de 3s no app_public detecta e popula rv$app_data.

message("[run.R] Etapa A — carregando do cache local (sem network)...")
APP_DATA_GLOBAL <<- tryCatch(
  carregar_dados_app(
    file_id    = DRIVE_FILE_ID,
    folder_id  = DRIVE_FOLDER_ID,
    forcar_dl  = FALSE,   # NÃO baixa o Drive agora
    forcar_etl = FALSE    # usa SQLite se existir
  ),
  error = function(e) {
    message("[run.R] Cache local indisponível: ", e$message)
    structure(list(), erro_msg = e$message)
  }
)
message(sprintf("[run.R] Etapa A concluída: %d proprietário(s) em cache.", length(APP_DATA_GLOBAL)))

# ── 6. Inicia o app (porta abre aqui) ────────────────────────
app <- source(file.path(app_root, "app.R"), local = new.env())$value

# ── 7. Etapa B — atualização do Drive em background ──────────
# Executa APÓS o print(app) iniciar o servidor Shiny.
# later::later garante que roda dentro do event loop do Shiny,
# atualizando APP_DATA_GLOBAL sem bloquear nenhuma sessão ativa.
later::later(function() {
  message("[run.R] Etapa B — baixando dados atualizados do Drive...")
  novo <- tryCatch(
    carregar_dados_app(
      file_id    = DRIVE_FILE_ID,
      folder_id  = DRIVE_FOLDER_ID,
      forcar_dl  = TRUE,    # força download do Drive
      forcar_etl = TRUE     # reprocessa ETL
    ),
    error = function(e) {
      message("[run.R] Etapa B falhou: ", e$message)
      NULL
    }
  )
  if (!is.null(novo) && length(novo) > 0) {
    APP_DATA_GLOBAL <<- novo
    message(sprintf("[run.R] Etapa B concluída: %d proprietário(s) atualizados.", length(APP_DATA_GLOBAL)))
  }
}, delay = 1)

print(app)