# ============================================================
# gdrive_public.R  —  BSBStay Shiny integração Google Drive
# v3.1 — Correções Render:
#   - parse_date_safe em todas as datas críticas
#   - correção de ambiguidade no mutate() de fact_manutencao
#   - correção de ambiguidade no mutate() de fact_despesas
#   - loops com chaves explícitas para evitar erro de parse
# ==============================================================

# ── Ambiente / paths ──────────────────────────────────────────
APP_ROOT <- get0(
  "APP_ROOT",
  ifnotfound = normalizePath(Sys.getenv("APP_ROOT", "."), winslash = "/", mustWork = FALSE)
)

DATA_DIR <- normalizePath(
  Sys.getenv("APP_DATA_DIR", file.path(APP_ROOT, "data")),
  winslash = "/",
  mustWork = FALSE
)

CACHE_DIR <- normalizePath(
  Sys.getenv("APP_CACHE_DIR", file.path(DATA_DIR, "cache")),
  winslash = "/",
  mustWork = FALSE
)

RAW_DIR <- normalizePath(
  Sys.getenv("APP_RAW_DIR", file.path(DATA_DIR, "raw")),
  winslash = "/",
  mustWork = FALSE
)

dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)

# ── Constantes ────────────────────────────────────────────────
DRIVE_FOLDER_ID <- Sys.getenv("DRIVE_FOLDER_ID", unset = "1753AZxwmyyWYS2oYQPLeMHIz5gM8bscb")
DRIVE_FILE_ID   <- Sys.getenv("DRIVE_FILE_ID",   unset = "1fnereY6JOrAbSl1yw_o_U94Fb0KTuHGJU85GEUrCBiU")

CACHE_XLSX      <- file.path(CACHE_DIR, "db_master_drive.xlsx")
SQLITE_PATH     <- file.path(CACHE_DIR, "bsbstay.sqlite")
CACHE_META_KEY  <- "last_drive_sync"

# Reduzido de 6h para 2h: o cache de 6h fazia o painel servir dados
# desatualizados por até 6h após uma correção na planilha fonte (ex:
# alteração de comissão), mesmo com o ETL já tendo rodado corretamente
# no Drive. 2h reduz a janela de inconsistência sem sobrecarregar o
# Drive com downloads excessivos. Pode ser ajustado via env var.
MAX_CACHE_AGE_H <- suppressWarnings(as.numeric(Sys.getenv("MAX_CACHE_AGE_H", "2")))
if (is.na(MAX_CACHE_AGE_H) || MAX_CACHE_AGE_H <= 0) MAX_CACHE_AGE_H <- 2

# ── Pacotes ───────────────────────────────────────────────────
.ensure_pkgs <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) {
    stop(
      "Pacotes ausentes no ambiente do container: ",
      paste(miss, collapse = ", "),
      ". Refaça o build da imagem Docker."
    )
  }
  invisible(TRUE)
}

.ensure_pkgs(c("readxl", "DBI", "RSQLite", "dplyr", "lubridate", "tidyr", "janitor"))

# ── Utilitários ───────────────────────────────────────────────
# Compartilhados por app.R, app_public.R e app_master.R (este arquivo é
# sourceado por todos antes de qualquer uso) — NÃO redefinir nos apps.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

# Formata moeda BRL — vetorizada, segura dentro de dplyr::transmute/mutate
brl <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(is.na(v), "R$ —",
         paste0("R$ ", formatC(v, format = "f", digits = 2,
                               big.mark = ".", decimal.mark = ",")))
}

# Cota-parte do proprietário no resultado. Em imóvel de dono único é
# igual ao resultado integral. O fallback para `resultado_liq` protege
# contra objetos de cache gerados antes da coluna existir.
cota_col <- function(df) {
  if ("resultado_cota" %in% names(df)) df$resultado_cota else df$resultado_liq
}

parse_date_safe <- function(x) {
  if (is.null(x) || all(is.na(x))) return(as.Date(NA))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct", "POSIXt"))) return(as.Date(x))
  
  if (is.numeric(x)) {
    return(suppressWarnings(as.Date(as.numeric(x), origin = "1899-12-30")))
  }
  
  if (is.character(x)) {
    num_try   <- suppressWarnings(as.numeric(x))
    is_serial <- !is.na(num_try) & num_try > 40000 & num_try < 60000
    out       <- as.Date(rep(NA, length(x)))
    
    if (any(is_serial, na.rm = TRUE)) {
      out[is_serial] <- as.Date(num_try[is_serial], origin = "1899-12-30")
    }
    
    if (any(!is_serial & !is.na(x), na.rm = TRUE)) {
      out[!is_serial & !is.na(x)] <- suppressWarnings(as.Date(x[!is_serial & !is.na(x)]))
    }
    
    return(out)
  }
  
  suppressWarnings(as.Date(as.character(x)))
}

normalizar_cpf_cnpj <- function(x) {
  trimws(as.character(x))
}

# ── Formatação de mês independente de locale ───────────────────
# Usa vetores PT-BR hardcoded — funciona igual em qualquer SO/container
.MESES_FULL <- c("janeiro","fevereiro","março","abril","maio","junho",
                 "julho","agosto","setembro","outubro","novembro","dezembro")
.MESES_ABBR <- c("jan","fev","mar","abr","mai","jun",
                 "jul","ago","set","out","nov","dez")

fmt_mes_pt <- function(x, abreviado = FALSE) {
  # Aceita Date, "YYYY-MM", "YYYY-MM-DD" ou qualquer coisa que contenha YYYY-MM
  d <- suppressWarnings(as.Date(paste0(substr(as.character(x), 1, 7), "-01")))
  ifelse(
    is.na(d),
    as.character(x),
    paste0(
      tools::toTitleCase(if (abreviado) .MESES_ABBR[as.integer(format(d, "%m"))]
                         else           .MESES_FULL[as.integer(format(d, "%m"))]),
      "/", format(d, "%Y")
    )
  )
}

# ── URLs de download ──────────────────────────────────────────
urls_para_file_id <- function(file_id) {
  c(
    paste0("https://docs.google.com/spreadsheets/d/", file_id, "/export?format=xlsx"),
    paste0("https://docs.google.com/spreadsheets/d/", file_id, "/export?format=xlsx&id=", file_id),
    paste0("https://drive.google.com/uc?export=download&id=", file_id, "&confirm=t"),
    paste0("https://drive.google.com/uc?export=download&id=", file_id),
    paste0("https://drive.usercontent.google.com/download?id=", file_id, "&export=download&confirm=t")
  )
}

# ── Download binário base R com retry ─────────────────────────
baixar_url_base <- function(urls, destino, timeout_s = 120) {
  metodos <- unique(c("libcurl", "auto", "curl", if (.Platform$OS.type == "windows") "wininet"))

  old_to <- getOption("timeout")
  options(timeout = timeout_s)
  on.exit(options(timeout = old_to), add = TRUE)

  for (url in urls) {
    for (met in metodos) {
      ok <- tryCatch({
        tmp <- tempfile(fileext = ".xlsx")
        on.exit(unlink(tmp), add = TRUE)

        st <- utils::download.file(url, tmp, mode = "wb", quiet = TRUE, method = met)
        if (st != 0) return(NULL)

        sig    <- readBin(tmp, raw(), n = 4)
        is_zip  <- identical(sig, as.raw(c(0x50, 0x4B, 0x03, 0x04)))
        is_ole2 <- identical(sig, as.raw(c(0xD0, 0xCF, 0x11, 0xE0)))
        if (!is_zip && !is_ole2) return(NULL)

        dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
        file.copy(tmp, destino, overwrite = TRUE)
        TRUE
      }, error = function(e) NULL)

      if (isTRUE(ok)) return(TRUE)
    }
  }

  FALSE
}

# ── Download principal ─────────────────────────────────────────
baixar_db_master_publico <- function(
    file_id  = DRIVE_FILE_ID,
    destino  = CACHE_XLSX,
    forcar   = FALSE,
    timeout_s = 120
) {
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  
  if (!forcar && file.exists(destino)) {
    idade_h <- as.numeric(difftime(Sys.time(), file.mtime(destino), units = "hours"))
    if (idade_h < MAX_CACHE_AGE_H) {
      return(list(ok = TRUE, path = destino, source = "cache"))
    }
  }
  
  fid <- trimws(file_id %||% "")
  if (!nzchar(fid)) {
    raw_dir <- RAW_DIR
    candidatos <- if (dir.exists(raw_dir)) {
      list.files(raw_dir, pattern = "\\.(xlsx|xls)$", full.names = TRUE)
    } else {
      character(0)
    }
    
    if (length(candidatos) > 0) {
      file.copy(candidatos[1], destino, overwrite = TRUE)
      return(list(ok = TRUE, path = destino, source = "local_raw"))
    }
    
    return(list(ok = FALSE, path = NULL, source = "erro", msg = "DRIVE_FILE_ID nao configurado"))
  }
  
  ok <- baixar_url_base(urls_para_file_id(fid), destino, timeout_s)
  if (ok) {
    return(list(ok = TRUE, path = destino, source = "drive"))
  }
  
  raw_dir <- RAW_DIR
  candidatos <- if (dir.exists(raw_dir)) {
    list.files(raw_dir, pattern = "\\.(xlsx|xls)$", full.names = TRUE)
  } else {
    character(0)
  }
  
  if (length(candidatos) > 0) {
    file.copy(candidatos[1], destino, overwrite = TRUE)
    return(list(ok = TRUE, path = destino, source = "local_raw"))
  }
  
  if (file.exists(destino)) {
    return(list(ok = TRUE, path = destino, source = "cache_old"))
  }
  
  list(ok = FALSE, path = NULL, source = "erro", msg = "Nao foi possivel baixar. Coloque o xlsx em data/raw/")
}

# ── SQLite helpers ─────────────────────────────────────────────
sqlite_connect <- function(path = SQLITE_PATH) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  DBI::dbConnect(RSQLite::SQLite(), path)
}

sqlite_get_meta <- function(key, con = NULL) {
  own <- is.null(con)
  if (own) con <- sqlite_connect()
  on.exit(if (own) DBI::dbDisconnect(con), add = TRUE)
  
  if (!DBI::dbExistsTable(con, "meta")) return(NA_character_)
  res <- DBI::dbGetQuery(con, "SELECT value FROM meta WHERE key=?", params = list(key))
  if (nrow(res) == 0) NA_character_ else res$value[[1]]
}

sqlite_set_meta <- function(key, value, con = NULL) {
  own <- is.null(con)
  if (own) con <- sqlite_connect()
  on.exit(if (own) DBI::dbDisconnect(con), add = TRUE)
  
  if (!DBI::dbExistsTable(con, "meta")) {
    DBI::dbExecute(con, "CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT)")
  }
  
  DBI::dbExecute(
    con,
    "INSERT OR REPLACE INTO meta(key,value) VALUES(?,?)",
    params = list(key, as.character(value))
  )
  
  invisible(TRUE)
}

sqlite_write_table <- function(df, table_name, con = NULL) {
  own <- is.null(con)
  if (own) con <- sqlite_connect()
  on.exit(if (own) DBI::dbDisconnect(con), add = TRUE)
  
  DBI::dbWriteTable(con, table_name, df, overwrite = TRUE)
  invisible(TRUE)
}

sqlite_read_table <- function(table_name, con = NULL) {
  own <- is.null(con)
  if (own) con <- sqlite_connect()
  on.exit(if (own) DBI::dbDisconnect(con), add = TRUE)
  
  if (!DBI::dbExistsTable(con, table_name)) return(NULL)
  DBI::dbReadTable(con, table_name)
}

sqlite_tables_exist <- function(tables, con = NULL) {
  own <- is.null(con)
  if (own) con <- sqlite_connect()
  on.exit(if (own) DBI::dbDisconnect(con), add = TRUE)
  
  all(vapply(tables, DBI::dbExistsTable, logical(1), conn = con))
}

# ── Gerencial: tabela de ajustes no SQLite ───────────────────
# Camada de sobreposição: não toca no Drive, persiste no disco.

ger_ensure_table <- function(con = NULL) {
  own <- is.null(con); if (own) con <- sqlite_connect()
  on.exit(if (own) DBI::dbDisconnect(con), add = TRUE)
  if (!DBI::dbExistsTable(con, "ger_ajustes")) {
    DBI::dbExecute(con, "
      CREATE TABLE ger_ajustes (
        mes             TEXT NOT NULL,
        apto            TEXT NOT NULL,
        cpf_cnpj        TEXT NOT NULL,
        taxa_adm        REAL DEFAULT 0,
        manutencao      REAL DEFAULT 0,
        reposicao       REAL DEFAULT 0,
        despesas        REAL DEFAULT 0,
        receita_ajuste  REAL DEFAULT NULL,
        rec_original    REAL DEFAULT NULL,
        nota            TEXT DEFAULT \'\',
        publicado       INTEGER DEFAULT 0,
        ts              TEXT,
        PRIMARY KEY (mes, apto)
      )
    ")
    message("[Ger] Tabela ger_ajustes criada no SQLite.")
  } else {
    # Migração: adiciona colunas novas se tabela já existia sem elas
    cols <- DBI::dbListFields(con, "ger_ajustes")
    if (!"receita_ajuste" %in% cols)
      DBI::dbExecute(con, "ALTER TABLE ger_ajustes ADD COLUMN receita_ajuste REAL DEFAULT NULL")
    if (!"rec_original" %in% cols)
      DBI::dbExecute(con, "ALTER TABLE ger_ajustes ADD COLUMN rec_original REAL DEFAULT NULL")
  }
  invisible(TRUE)
}

# Grava ou atualiza um ajuste (rascunho ou publicado)
ger_save_ajuste <- function(mes, apto, cpf_cnpj, taxa_adm, manutencao,
                            reposicao, despesas, nota = "",
                            receita_ajuste = NULL, rec_original = NULL,
                            publicado = FALSE, con = NULL) {
  own <- is.null(con); if (own) con <- sqlite_connect()
  on.exit(if (own) DBI::dbDisconnect(con), add = TRUE)
  ger_ensure_table(con)
  ts  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  pub <- as.integer(isTRUE(publicado))
  # receita_ajuste: NULL = não houve ajuste de receita (mantém original)
  rec_aj  <- if (is.null(receita_ajuste) || is.na(receita_ajuste)) NA_real_
  else as.numeric(receita_ajuste)
  rec_ori <- if (is.null(rec_original)   || is.na(rec_original))   NA_real_
  else as.numeric(rec_original)
  DBI::dbExecute(con,
                 "INSERT OR REPLACE INTO ger_ajustes
       (mes, apto, cpf_cnpj, taxa_adm, manutencao, reposicao, despesas,
        receita_ajuste, rec_original, nota, publicado, ts)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
                 params = list(as.character(mes), as.character(apto),
                               gsub("[^0-9]","",as.character(cpf_cnpj)),
                               as.numeric(taxa_adm), as.numeric(manutencao),
                               as.numeric(reposicao), as.numeric(despesas),
                               rec_aj, rec_ori,
                               as.character(nota), pub, ts)
  )
  invisible(TRUE)
}

# Lê todos os ajustes (rascunhos + publicados)
ger_load_ajustes <- function(con = NULL) {
  own <- is.null(con); if (own) con <- sqlite_connect()
  on.exit(if (own) DBI::dbDisconnect(con), add = TRUE)
  ger_ensure_table(con)
  tryCatch(
    DBI::dbReadTable(con, "ger_ajustes"),
    error = function(e) data.frame()
  )
}

# Remove um ajuste (restaurar para original)
ger_delete_ajuste <- function(mes, apto, con = NULL) {
  own <- is.null(con); if (own) con <- sqlite_connect()
  on.exit(if (own) DBI::dbDisconnect(con), add = TRUE)
  ger_ensure_table(con)
  DBI::dbExecute(con,
                 "DELETE FROM ger_ajustes WHERE mes = ? AND apto = ?",
                 params = list(as.character(mes), as.character(apto))
  )
  invisible(TRUE)
}

# ── Leitura e normalização do xlsx ────────────────────────────
ler_e_processar_db_master <- function(path_xlsx) {
  stopifnot(file.exists(path_xlsx))
  
  sheets_ok <- readxl::excel_sheets(path_xlsx)
  
  obrig <- c(
    "dim_proprietario",
    "dim_imovel",
    "fact_reservas",
    "fact_manutencao",
    "fact_reposicao",
    "fact_repasse",
    "fact_despesas",
    "agg_prestacao_contas"
  )
  
  falt <- setdiff(obrig, sheets_ok)
  if (length(falt)) {
    stop("Abas faltantes no xlsx: ", paste(falt, collapse = ", "))
  }
  
  out <- setNames(lapply(obrig, function(sh) {
    df <- readxl::read_excel(path_xlsx, sheet = sh, guess_max = 5000)
    janitor::clean_names(df)
  }), obrig)
  
  # ── dim_proprietario ──
  out$dim_proprietario <- out$dim_proprietario |>
    dplyr::mutate(
      owner_id          = format(as.numeric(owner_id), scientific = FALSE, trim = TRUE),
      cpf_cnpj          = normalizar_cpf_cnpj(cpf_cnpj),
      nome_proprietario = as.character(nome_proprietario)
    ) |>
    dplyr::filter(!is.na(owner_id), !is.na(cpf_cnpj), nzchar(cpf_cnpj))
  
  # ── dim_imovel ──
  out$dim_imovel <- out$dim_imovel |>
    dplyr::mutate(
      property_id    = as.character(property_id),
      owner_id       = format(as.numeric(owner_id), scientific = FALSE, trim = TRUE),
      nome_canonico  = as.character(nome_canonico),
      empreendimento = as.character(empreendimento),
      unidade        = ifelse(is.na(unidade), NA_character_, gsub("\\.0$", "", as.character(unidade)))
    ) |>
    dplyr::filter(!is.na(property_id), nzchar(property_id))
  
  # ── agg_prestacao_contas ──
  out$agg_prestacao_contas <- out$agg_prestacao_contas |>
    dplyr::mutate(
      competencia       = format(parse_date_safe(competencia), "%Y-%m"),
      cpf_cnpj          = normalizar_cpf_cnpj(cpf_cnpj),
      nome_proprietario = as.character(nome_proprietario),
      owner_id          = format(as.numeric(owner_id), scientific = FALSE, trim = TRUE),
      property_id       = as.character(property_id),
      nome_canonico     = as.character(nome_canonico),
      empreendimento    = as.character(empreendimento),
      unidade           = ifelse(is.na(unidade), NA_character_, gsub("\\.0$", "", as.character(unidade))),
      dplyr::across(
        c(
          noites_no_mes, dias_no_mes, taxa_ocupacao, reservas,
          receita_liquida, diaria_media, comissao_pct, tx_adm,
          manutencao_total, reposicao_total, despesas_total,
          custos_total, resultado, itens_reposicao, qtd_itens
        ),
        ~ suppressWarnings(as.numeric(.x))
      )
    ) |>
    dplyr::filter(!is.na(cpf_cnpj), nzchar(cpf_cnpj), !is.na(competencia))
  
  # ── fact_reservas ──
  out$fact_reservas <- out$fact_reservas |>
    dplyr::mutate(
      competencia         = format(parse_date_safe(competencia), "%Y-%m"),
      property_id         = as.character(property_id),
      checkin             = as.character(parse_date_safe(checkin)),
      checkout            = as.character(parse_date_safe(checkout)),
      noites_total        = suppressWarnings(as.numeric(noites_total)),
      noites_no_mes       = suppressWarnings(as.numeric(noites_no_mes)),
      diaria_liquida      = suppressWarnings(as.numeric(diaria_liquida)),
      receita_liquida_mes = suppressWarnings(as.numeric(receita_liquida_mes))
    ) |>
    dplyr::filter(!is.na(property_id), !is.na(checkin), !is.na(checkout))
  
  # ── fact_manutencao ──
  manut_data_src <- if ("data" %in% names(out$fact_manutencao)) {
    out$fact_manutencao[["data"]]
  } else {
    out$fact_manutencao[["competencia"]]
  }
  
  out$fact_manutencao <- out$fact_manutencao |>
    dplyr::mutate(
      competencia     = format(parse_date_safe(competencia), "%Y-%m"),
      property_id     = as.character(property_id),
      valor_total     = suppressWarnings(as.numeric(valor_total)),
      data            = as.character(parse_date_safe(manut_data_src)),
      os_id           = if ("os_id" %in% names(out$fact_manutencao)) as.character(os_id) else NA_character_,
      produto_servico = if ("produto_servico" %in% names(out$fact_manutencao)) as.character(produto_servico) else NA_character_
    )
  
  # ── fact_reposicao ──
  out$fact_reposicao <- out$fact_reposicao |>
    dplyr::mutate(
      competencia             = format(parse_date_safe(competencia), "%Y-%m"),
      property_id             = as.character(property_id),
      quantidade              = suppressWarnings(as.numeric(quantidade)),
      valor_unitario_ou_total = suppressWarnings(as.numeric(valor_unitario_ou_total))
    )
  
  # ── fact_repasse ──
  out$fact_repasse <- out$fact_repasse |>
    dplyr::mutate(
      competencia = format(parse_date_safe(competencia), "%Y-%m"),
      property_id = as.character(property_id),
      valor       = suppressWarnings(as.numeric(valor)),
      comissao    = suppressWarnings(as.numeric(comissao))
    )
  
  # ── fact_despesas ──
  desp_data_src <- if ("data" %in% names(out$fact_despesas)) {
    out$fact_despesas[["data"]]
  } else {
    out$fact_despesas[["competencia"]]
  }
  
  out$fact_despesas <- out$fact_despesas |>
    dplyr::mutate(
      competencia = format(parse_date_safe(competencia), "%Y-%m"),
      property_id = as.character(property_id),
      valor       = suppressWarnings(as.numeric(valor)),
      data        = as.character(parse_date_safe(desp_data_src))
    )
  
  out
}

# ── Pipeline principal ─────────────────────────────────────────
carregar_dados_app <- function(
    file_id    = DRIVE_FILE_ID,
    folder_id  = DRIVE_FOLDER_ID,
    forcar_dl  = FALSE,
    forcar_etl = FALSE
) {
  con <- sqlite_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  tabs_ok   <- sqlite_tables_exist(c("agg_prestacao_contas", "dim_imovel", "meta"), con)
  last_sync <- sqlite_get_meta(CACHE_META_KEY, con)
  
  age_h <- if (!is.na(last_sync)) {
    as.numeric(difftime(Sys.time(), as.POSIXct(last_sync), units = "hours"))
  } else {
    Inf
  }
  
  if (!forcar_dl && !forcar_etl && tabs_ok && age_h < MAX_CACHE_AGE_H) {
    message(sprintf("[Cache] SQLite fresco (%.1fh). Carregando.", age_h))
    return(montar_objeto_app_sqlite(con))
  }
  
  dl <- baixar_db_master_publico(file_id = file_id, forcar = forcar_dl)
  if (!dl$ok) stop(dl$msg)
  
  message("[ETL] Processando planilha...")
  db <- ler_e_processar_db_master(dl$path)
  
  for (nm in names(db)) {
    if (is.data.frame(db[[nm]])) {
      sqlite_write_table(db[[nm]], nm, con)
    }
  }
  
  sqlite_set_meta(CACHE_META_KEY, format(Sys.time()), con)
  
  message("[ETL] Concluido.")
  montar_objeto_app_sqlite(con)
}

# ── Monta objeto app a partir do SQLite ───────────────────────
montar_objeto_app_sqlite <- function(con) {
  message("[Montar] Lendo tabelas do SQLite...")
  
  agg        <- sqlite_read_table("agg_prestacao_contas", con)
  dim_prop   <- sqlite_read_table("dim_proprietario", con)
  dim_imovel <- sqlite_read_table("dim_imovel", con)
  reservas   <- sqlite_read_table("fact_reservas", con)
  manutencao <- sqlite_read_table("fact_manutencao", con)
  reposicao  <- sqlite_read_table("fact_reposicao", con)
  despesas   <- sqlite_read_table("fact_despesas", con)
  
  if (is.null(agg) || nrow(agg) == 0) {
    stop("Tabela agg_prestacao_contas vazia. Apague o SQLite e reinicie.")
  }
  
  # ── 1. Normaliza agg ──────────────────────────────────────────
  agg <- tryCatch({
    agg |>
      dplyr::mutate(
        cpf_cnpj      = normalizar_cpf_cnpj(cpf_cnpj),
        competencia   = as.character(competencia),
        mes           = suppressWarnings(as.Date(paste0(substr(competencia, 1, 7), "-01"))),
        mes_label     = fmt_mes_pt(mes, abreviado = TRUE),
        imovel        = as.character(nome_canonico),
        receita_bruta = dplyr::coalesce(as.numeric(receita_liquida), 0),
        taxa_adm      = dplyr::coalesce(as.numeric(tx_adm), 0),
        outros_custos     = dplyr::coalesce(as.numeric(custos_total), 0),
        manutencao_total  = dplyr::coalesce(as.numeric(manutencao_total), 0),
        reposicao_total   = dplyr::coalesce(as.numeric(reposicao_total), 0),
        despesas_total    = dplyr::coalesce(as.numeric(despesas_total), 0),
        devolucao_limpeza = if ("devolucao_limpeza" %in% names(agg))
          dplyr::coalesce(as.numeric(devolucao_limpeza), 0) else 0,
        resultado_liq     = dplyr::coalesce(as.numeric(resultado), 0),
        ocupacao      = round(dplyr::coalesce(as.numeric(taxa_ocupacao), 0) * 100),
        diaria_media  = dplyr::coalesce(as.numeric(diaria_media), 0),
        n_diarias     = dplyr::coalesce(suppressWarnings(as.integer(as.numeric(noites_no_mes))), 0L)
      ) |>
      dplyr::filter(!is.na(cpf_cnpj), nzchar(cpf_cnpj), !is.na(mes))
  }, error = function(e) {
    stop("Erro ao normalizar agg: ", e$message)
  })
  
  # ── 2. Portfolio ──────────────────────────────────────────────
  portfolio <- tryCatch({
    if (!is.null(dim_imovel) && nrow(dim_imovel) > 0 &&
        !is.null(dim_prop) && nrow(dim_prop) > 0) {
      prop_cpf <- dim_prop |>
        dplyr::transmute(
          owner_id = as.character(owner_id),
          cpf_cnpj = normalizar_cpf_cnpj(cpf_cnpj)
        ) |>
        dplyr::filter(!is.na(owner_id), !is.na(cpf_cnpj))
      
      dim_imovel |>
        dplyr::mutate(owner_id = as.character(owner_id)) |>
        dplyr::left_join(prop_cpf, by = "owner_id") |>
        dplyr::filter(!is.na(cpf_cnpj)) |>
        dplyr::transmute(
          cpf_cnpj,
          owner_id,
          property_id,
          id          = as.character(nome_canonico),
          nome        = as.character(nome_canonico),
          bairro      = as.character(dplyr::coalesce(empreendimento, nome_canonico)),
          tipo        = as.character(dplyr::coalesce(unidade, empreendimento, nome_canonico)),
          plataformas = "Airbnb / Booking / Direta"
        )
    } else {
      data.frame()
    }
  }, error = function(e) {
    message("AVISO portfolio: ", e$message)
    data.frame()
  })
  
  # ── Nº de donos por imóvel ───────────────────────────────────
  # Base do rateio do resultado (regra da gestão, jul/2026): receita,
  # taxa e custos são espelhados INTEGRALMENTE para cada co-proprietário,
  # mas o RESULTADO LÍQUIDO é rateado pelo número de donos — cada um
  # recebe sua cota-parte. Com 2 donos = 50% para cada; o Vision 302
  # tem 3 donos, então o rateio é pelo nº real e não uma metade fixa
  # (só assim a soma das cotas reconstitui o resultado do imóvel).
  donos_por_imovel <- if (nrow(portfolio) > 0) {
    portfolio |>
      dplyr::distinct(nome, cpf_cnpj) |>
      dplyr::count(nome, name = "n_donos") |>
      dplyr::rename(imovel = nome)
  } else {
    data.frame(imovel = character(), n_donos = integer())
  }

  # ── Mapa property_id → dono(s) do imóvel ─────────────────────
  # Fatos gravados sob o property_id de apenas um dono são expandidos
  # para TODOS os donos do mesmo imóvel — o join abaixo produz 1 linha
  # por (property_id, dono).
  pid_map <- if (nrow(portfolio) > 0) {
    pid_nome <- portfolio |>
      dplyr::select(property_id, imovel_nome = nome) |>
      dplyr::distinct(property_id, .keep_all = TRUE)
    owners_imovel <- portfolio |>
      dplyr::select(imovel_nome = nome, cpf_cnpj) |>
      dplyr::distinct()
    pid_nome |> dplyr::left_join(owners_imovel, by = "imovel_nome")
  } else {
    data.frame(
      property_id = character(),
      cpf_cnpj = character(),
      imovel_nome = character()
    )
  }

  # Dedupe pós-expansão: FACTs novas já chegam replicadas por co-dono
  # (1 linha por property_id, chave terminando em "|<property_id>").
  # Após o join acima, essas réplicas gerariam o mesmo item 2× para o
  # mesmo dono. Remove pela chave-base (sem o sufixo do pid): itens
  # iguais para o mesmo cpf_cnpj contam uma única vez.
  .dedupe_multi <- function(df, key_col) {
    if (nrow(df) == 0 || !key_col %in% names(df) ||
        !all(c("cpf_cnpj", "property_id") %in% names(df))) return(df)
    k   <- as.character(df[[key_col]])
    suf <- paste0("|", as.character(df$property_id))
    rep_pid <- !is.na(k) & endsWith(k, suf)
    df$.base_key <- ifelse(rep_pid, substr(k, 1L, nchar(k) - nchar(suf)), k)
    df |>
      dplyr::distinct(cpf_cnpj, .base_key, .keep_all = TRUE) |>
      dplyr::select(-".base_key")
  }
  
  # ── 3. Calendario ─────────────────────────────────────────────
  calendario <- tryCatch({
    if (!is.null(reservas) && nrow(reservas) > 0 &&
        all(c("checkin", "checkout", "property_id") %in% names(reservas))) {
      
      res_clean <- reservas |>
        dplyr::mutate(
          checkin  = parse_date_safe(checkin),
          checkout = parse_date_safe(checkout),
          valor    = dplyr::coalesce(as.numeric(diaria_liquida), 0)
        ) |>
        dplyr::filter(!is.na(checkin), !is.na(checkout), checkout > checkin) |>
        dplyr::left_join(pid_map, by = "property_id") |>
        .dedupe_multi("reserva_key")

      if (nrow(res_clean) == 0) return(data.frame())

      # ── Recorte por competência ───────────────────────────────
      # Reservas que cruzam a virada do mês são gravadas em DUAS
      # competências, cada uma com sua fatia de noites (a soma dos
      # noites_no_mes = noites_total). Expandir checkin→checkout
      # inteiro em AMBAS duplicaria os dias compartilhados no
      # calendário. Recortamos cada linha aos limites do mês da sua
      # própria competência — validado contra as 13.970 reservas da
      # base: o recorte reproduz noites_no_mes com zero divergência.
      ini_r <- res_clean$checkin
      fim_r <- res_clean$checkout            # exclusivo (dia do checkout não ocupa)
      if ("competencia" %in% names(res_clean)) {
        m_ini <- suppressWarnings(
          as.Date(paste0(substr(as.character(res_clean$competencia), 1, 7), "-01")))
        # +32 dias sempre cai no mês seguinte (mês mais longo = 31 dias);
        # normalizar para o dia 1º dá o limite superior exclusivo.
        m_fim <- as.Date(format(m_ini + 32L, "%Y-%m-01"))
        tem_c <- !is.na(m_ini)
        ini_r[tem_c] <- pmax(ini_r[tem_c], m_ini[tem_c])
        fim_r[tem_c] <- pmin(fim_r[tem_c], m_fim[tem_c])
      }

      # Vetorizado: expande todas as reservas de uma vez sem lapply row-by-row
      n_nights <- as.integer(fim_r - ini_r)
      valid    <- !is.na(n_nights) & n_nights > 0
      if (!any(valid)) return(data.frame())

      res_v    <- res_clean[valid, ]
      n_v      <- n_nights[valid]
      starts   <- as.integer(ini_r[valid])
      all_int  <- unlist(Map(seq.int, starts, starts + n_v - 1L), use.names = FALSE)
      idx      <- rep(seq_len(nrow(res_v)), n_v)

      cal_df <- data.frame(
        cpf_cnpj      = res_v$cpf_cnpj[idx]      %||% NA_character_,
        property_id   = res_v$property_id[idx],
        apto_original = res_v$imovel_nome[idx]    %||% NA_character_,
        data          = structure(all_int, class = "Date"),
        valor         = res_v$valor[idx],
        ocupado       = TRUE,
        stringsAsFactors = FALSE
      )

      # Blindagem: sobreposições reais na MESMA competência (erro na
      # planilha fonte — duplicata de lançamento ou datas cruzadas)
      # ainda gerariam o mesmo dia 2×. O calendário nunca deve renderizar
      # dia repetido; o conflito é reportado à gestão para correção na origem.
      cal_df |> dplyr::distinct(cpf_cnpj, property_id, data, .keep_all = TRUE)
    } else {
      data.frame()
    }
  }, error = function(e) {
    message("AVISO calendario: ", e$message)
    data.frame()
  })
  
  # ── 4. Reservas nível reserva ────────────────────────────────
  reservas_clean <- tryCatch({
    if (!is.null(reservas) && nrow(reservas) > 0) {
      cols_res <- names(reservas)
      reservas |>
        dplyr::mutate(
          checkin        = parse_date_safe(checkin),
          checkout       = parse_date_safe(checkout),
          diaria_liquida = dplyr::coalesce(as.numeric(diaria_liquida), 0),
          noites_total   = dplyr::coalesce(as.numeric(noites_total), 0),
          receita_total  = dplyr::coalesce(as.numeric(receita_liquida_mes), 0),
          # Campos numéricos de detalhamento de reserva (ETL v5.1+).
          # Tratamento seguro: se a coluna ainda não existir (meses
          # processados antes desta versão do ETL), cria como NA em
          # vez de quebrar a leitura.
          adultos              = if ("adultos" %in% cols_res)
            dplyr::coalesce(as.numeric(adultos), 0) else NA_real_,
          criancas             = if ("criancas" %in% cols_res)
            dplyr::coalesce(as.numeric(criancas), 0) else NA_real_,
          valor_total_reserva  = if ("valor_total_reserva" %in% cols_res)
            dplyr::coalesce(as.numeric(valor_total_reserva), 0) else NA_real_,
          taxa_limpeza         = if ("taxa_limpeza" %in% cols_res)
            dplyr::coalesce(as.numeric(taxa_limpeza), 0) else NA_real_,
          comissao_canal       = if ("comissao_canal" %in% cols_res)
            dplyr::coalesce(as.numeric(comissao_canal), 0) else NA_real_,
          hospede              = if ("hospede" %in% cols_res)
            as.character(hospede) else NA_character_
        ) |>
        dplyr::filter(!is.na(checkin), !is.na(checkout), checkout > checkin) |>
        dplyr::left_join(pid_map, by = "property_id") |>
        dplyr::filter(!is.na(cpf_cnpj)) |>
        .dedupe_multi("reserva_key")
    } else {
      data.frame()
    }
  }, error = function(e) {
    message("AVISO reservas_clean: ", e$message)
    data.frame()
  })
  
  # ── 5. Manutenção ────────────────────────────────────────────
  manutencao_clean <- tryCatch({
    if (!is.null(manutencao) && nrow(manutencao) > 0) {
      manutencao |>
        dplyr::mutate(
          property_id     = as.character(property_id),
          valor_total     = dplyr::coalesce(as.numeric(valor_total), 0),
          competencia     = as.character(competencia),
          os_id           = if ("os_id" %in% names(manutencao)) as.character(os_id) else NA_character_,
          produto_servico = if ("produto_servico" %in% names(manutencao)) as.character(produto_servico) else NA_character_
        ) |>
        dplyr::left_join(pid_map, by = "property_id") |>
        dplyr::filter(!is.na(cpf_cnpj)) |>
        .dedupe_multi("manut_key")
    } else {
      data.frame()
    }
  }, error = function(e) {
    message("AVISO manutencao: ", e$message)
    data.frame()
  })
  
  # ── 6. Reposição ─────────────────────────────────────────────
  reposicao_clean <- tryCatch({
    if (!is.null(reposicao) && nrow(reposicao) > 0) {
      reposicao |>
        dplyr::mutate(
          property_id             = as.character(property_id),
          quantidade              = dplyr::coalesce(as.numeric(quantidade), 0),
          valor_unitario_ou_total = dplyr::coalesce(as.numeric(valor_unitario_ou_total), 0),
          competencia             = as.character(competencia)
        ) |>
        dplyr::left_join(pid_map, by = "property_id") |>
        dplyr::filter(!is.na(cpf_cnpj)) |>
        .dedupe_multi("rep_key")
    } else {
      data.frame()
    }
  }, error = function(e) {
    message("AVISO reposicao: ", e$message)
    data.frame()
  })
  
  # ── 7. Despesas ──────────────────────────────────────────────
  despesas_clean <- tryCatch({
    if (!is.null(despesas) && nrow(despesas) > 0) {
      df <- despesas |>
        dplyr::mutate(
          property_id = as.character(property_id),
          valor       = dplyr::coalesce(as.numeric(valor), 0),
          competencia = as.character(competencia)
        ) |>
        dplyr::left_join(pid_map, by = "property_id") |>
        dplyr::filter(!is.na(cpf_cnpj)) |>
        .dedupe_multi("desp_key")

      if (!"categoria" %in% names(df)) {
        if ("tipo" %in% names(df)) {
          df <- dplyr::rename(df, categoria = tipo)
        } else {
          df <- dplyr::mutate(df, categoria = "Outras")
        }
      }
      
      df <- dplyr::mutate(
        df,
        categoria = dplyr::coalesce(as.character(categoria), "Outras")
      )
      
      df
    } else {
      data.frame()
    }
  }, error = function(e) {
    message("AVISO despesas: ", e$message)
    data.frame()
  })
  
  # ── 8. Owners ────────────────────────────────────────────────
  owners <- agg |>
    dplyr::distinct(cpf_cnpj, nome_proprietario) |>
    dplyr::filter(!is.na(cpf_cnpj), nzchar(cpf_cnpj))
  
  if (nrow(portfolio) > 0) {
    n_im <- portfolio |>
      dplyr::count(cpf_cnpj, name = "n_imoveis")
    
    owners <- dplyr::left_join(owners, n_im, by = "cpf_cnpj")
  }
  
  owners <- owners |>
    dplyr::mutate(
      n_imoveis = dplyr::coalesce(suppressWarnings(as.integer(as.numeric(n_imoveis))), 1L),
      perfil = dplyr::case_when(
        n_imoveis >= 4 ~ "Expansao Acelerada",
        n_imoveis == 3 ~ "Carteira Diversificada",
        TRUE ~ "Portfolio Concentrado"
      )
    )
  
  # ── 9. Lista final por cpf_cnpj ──────────────────────────────
  # Carrega ajustes gerenciais uma única vez antes do loop
  ger_ajustes_cache <- tryCatch(ger_load_ajustes(con), error = function(e) data.frame())
  
  obj <- lapply(owners$cpf_cnpj, function(cpf) {
    orow <- owners |>
      dplyr::filter(cpf_cnpj == cpf) |>
      dplyr::slice(1)
    
    port <- if (nrow(portfolio) > 0) portfolio |> dplyr::filter(cpf_cnpj == cpf) else data.frame()
    # Aplicar ajustes gerenciais publicados sobre os dados base
    recs_base <- agg |> dplyr::filter(cpf_cnpj == cpf)
    recs <- tryCatch({
      if (exists("ger_ajustes_cache") && !is.null(ger_ajustes_cache) && nrow(ger_ajustes_cache) > 0) {
        aj <- ger_ajustes_cache |>
          dplyr::filter(publicado == 1L) |>
          dplyr::mutate(
            mes_chr  = substr(as.character(mes), 1, 7),
            apto_chr = as.character(apto)
          )
        if (nrow(aj) > 0) {
          recs_base |>
            dplyr::mutate(
              .mes_chr  = substr(as.character(competencia), 1, 7),
              .apto_chr = as.character(imovel)
            ) |>
            dplyr::left_join(
              aj |> dplyr::select(mes_chr, apto_chr,
                                  ger_taxa = taxa_adm,
                                  ger_man  = manutencao,
                                  ger_rep  = reposicao,
                                  ger_des  = despesas,
                                  ger_rec  = receita_ajuste),
              by = c(".mes_chr" = "mes_chr", ".apto_chr" = "apto_chr")
            ) |>
            dplyr::mutate(
              # Receita: usa ajuste se preenchido, senão mantém original
              receita_bruta    = dplyr::if_else(!is.na(ger_rec), ger_rec, receita_bruta),
              taxa_adm         = dplyr::if_else(!is.na(ger_taxa), ger_taxa, taxa_adm),
              manutencao_total = dplyr::if_else(!is.na(ger_man),  ger_man,  manutencao_total),
              reposicao_total  = dplyr::if_else(!is.na(ger_rep),  ger_rep,  reposicao_total),
              despesas_total   = dplyr::if_else(!is.na(ger_des),  ger_des,  despesas_total),
              outros_custos    = dplyr::if_else(!is.na(ger_man),
                                                dplyr::coalesce(ger_man,0) +
                                                  dplyr::coalesce(ger_rep,0) +
                                                  dplyr::coalesce(ger_des,0),
                                                outros_custos),
              resultado_liq    = receita_bruta - taxa_adm - outros_custos
            ) |>
            dplyr::select(-dplyr::starts_with("."), -dplyr::starts_with("ger_"))
        } else { recs_base }
      } else { recs_base }
    }, error = function(e) {
      message("[Ger] AVISO ao aplicar ajustes: ", e$message)
      recs_base
    })

    # ── Rateio do resultado entre co-proprietários ──────────────
    # `resultado_liq` permanece sendo o resultado INTEGRAL do imóvel —
    # é o que o painel admin consolida (totais, rankings, Insights).
    # `resultado_cota` é a parte deste proprietário, exibida na
    # prestação de contas individual. Imóvel de dono único: as duas
    # colunas são iguais (n_donos = 1), então nada muda para eles.
    # Aplicado DEPOIS dos ajustes gerenciais, para que a cota reflita
    # qualquer revisão publicada pelo admin.
    recs <- tryCatch({
      if (nrow(recs) > 0) {
        recs |>
          dplyr::left_join(donos_por_imovel, by = "imovel") |>
          dplyr::mutate(
            n_donos        = dplyr::coalesce(as.integer(n_donos), 1L),
            n_donos        = dplyr::if_else(n_donos < 1L, 1L, n_donos),
            resultado_cota = resultado_liq / n_donos
          )
      } else recs
    }, error = function(e) {
      message("[Rateio] AVISO: ", e$message)
      if (nrow(recs) > 0) dplyr::mutate(recs, n_donos = 1L, resultado_cota = resultado_liq) else recs
    })

    cal  <- if (!is.null(calendario) && nrow(calendario) > 0) calendario |> dplyr::filter(cpf_cnpj == cpf) else data.frame()
    resv <- if (nrow(reservas_clean) > 0) reservas_clean |> dplyr::filter(cpf_cnpj == cpf) else data.frame()
    man  <- if (nrow(manutencao_clean) > 0) manutencao_clean |> dplyr::filter(cpf_cnpj == cpf) else data.frame()
    rep  <- if (nrow(reposicao_clean) > 0) reposicao_clean |> dplyr::filter(cpf_cnpj == cpf) else data.frame()
    des  <- if (nrow(despesas_clean) > 0) despesas_clean |> dplyr::filter(cpf_cnpj == cpf) else data.frame()
    
    cfg <- if (nrow(port) > 0) {
      lapply(seq_len(nrow(port)), function(i) {
        as.list(port[i, c("id", "nome", "bairro", "tipo", "plataformas")])
      })
    } else {
      list()
    }
    
    list(
      proprietario = orow$nome_proprietario[[1]],
      email        = NA_character_,
      perfil       = orow$perfil[[1]],
      cnpj         = cpf,
      imoveis_ids  = if (nrow(port) > 0) port$id else character(0),
      imoveis_cfg  = cfg,
      receitas     = recs,
      calendario   = cal,
      reservas     = resv,
      manutencao   = man,
      reposicao    = rep,
      despesas     = des
    )
  })
  
  message(sprintf("[App] %d proprietario(s) carregado(s).", length(obj)))
  stats::setNames(obj, owners$cpf_cnpj)
}

# ── Status ─────────────────────────────────────────────────────
status_cache <- function() {
  con <- sqlite_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  last    <- sqlite_get_meta(CACHE_META_KEY, con)
  tabelas <- tryCatch(DBI::dbListTables(con), error = function(e) character(0))
  
  cache_age_h <- if (!is.na(last)) {
    round(as.numeric(difftime(Sys.time(), as.POSIXct(last), units = "hours")), 1)
  } else {
    NA_real_
  }
  
  list(
    last_sync   = last,
    cache_age_h = cache_age_h,
    xlsx_cache  = file.exists(CACHE_XLSX),
    sqlite_path = SQLITE_PATH,
    tabelas     = tabelas
  )
}

# ══════════════════════════════════════════════════════════════
# AUTH — Gerenciamento de senhas dos proprietários
# Armazenadas no SQLite local com hash SHA-256 (digest)
# ══════════════════════════════════════════════════════════════

# Garante que a tabela de senhas existe no SQLite
auth_ensure_table <- function(con = NULL) {
  own <- is.null(con)
  if (own) con <- sqlite_connect()
  on.exit(if (own) DBI::dbDisconnect(con), add = TRUE)
  
  if (!DBI::dbExistsTable(con, "auth_senhas")) {
    DBI::dbExecute(con, "
      CREATE TABLE auth_senhas (
        cpf_cnpj     TEXT PRIMARY KEY,
        senha_hash   TEXT NOT NULL,
        criado_em    TEXT NOT NULL,
        alterado_em  TEXT NOT NULL
      )
    ")
    message("[Auth] Tabela auth_senhas criada.")
  }
  invisible(TRUE)
}

# ── Hashing de senhas ─────────────────────────────────────────
# Formato atual (v2): "pbkdf2v2$<iterações>$<salt>$<hash>"
#   - salt aleatório por usuário (inviabiliza rainbow tables)
#   - SHA-256 iterado (encarece força bruta offline ~10.000×)
# Formato legado: hash SHA-256 puro (64 hex) com salt = cpf.
# A migração é transparente: auth_check_senha valida no formato legado
# e regrava no v2 no primeiro login bem-sucedido.
AUTH_HASH_ITER <- 10000L

.auth_require_digest <- function() {
  if (!requireNamespace("digest", quietly = TRUE))
    stop("Pacote 'digest' necessário para hashing de senhas.")
}

.auth_salt_novo <- function() {
  .auth_require_digest()
  # Entropia de múltiplas fontes do processo; suficiente para unicidade do salt
  digest::digest(paste(Sys.time(), Sys.getpid(), stats::runif(4), sep = "|"),
                 algo = "sha256", serialize = FALSE)
}

.auth_hash_v2 <- function(senha, salt, iter = AUTH_HASH_ITER) {
  .auth_require_digest()
  h <- paste0(salt, "||", trimws(senha))
  for (i in seq_len(iter)) h <- digest::digest(h, algo = "sha256", serialize = FALSE)
  paste0("pbkdf2v2$", iter, "$", salt, "$", h)
}

# Hash legado (SHA-256 único, salt = cpf) — mantido SÓ para validar e
# migrar senhas criadas antes do v2. Não usar em novos cadastros.
auth_hash <- function(senha, cpf_cnpj) {
  .auth_require_digest()
  digest::digest(paste0(trimws(cpf_cnpj), "||", trimws(senha)),
                 algo = "sha256", serialize = FALSE)
}

# Verifica se proprietário já tem senha cadastrada
auth_tem_senha <- function(cpf_cnpj, con = NULL) {
  own <- is.null(con)
  if (own) con <- sqlite_connect()
  on.exit(if (own) DBI::dbDisconnect(con), add = TRUE)
  
  auth_ensure_table(con)
  cpf_norm <- gsub("[^0-9]", "", as.character(cpf_cnpj))
  
  res <- tryCatch(
    DBI::dbGetQuery(con,
                    "SELECT COUNT(*) AS n FROM auth_senhas WHERE cpf_cnpj = ?",
                    params = list(cpf_norm)),
    error = function(e) data.frame(n = 0)
  )
  isTRUE(res$n[1] > 0)
}

# Cadastra ou atualiza senha do proprietário (UPSERT atômico — sem race condition)
auth_set_senha <- function(cpf_cnpj, senha, con = NULL) {
  own <- is.null(con)
  if (own) con <- sqlite_connect()
  on.exit(if (own) DBI::dbDisconnect(con), add = TRUE)

  auth_ensure_table(con)
  cpf_norm <- gsub("[^0-9]", "", as.character(cpf_cnpj))
  hash     <- .auth_hash_v2(senha, .auth_salt_novo())
  agora    <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  DBI::dbExecute(con,
    "INSERT INTO auth_senhas (cpf_cnpj, senha_hash, criado_em, alterado_em)
     VALUES (?, ?, ?, ?)
     ON CONFLICT(cpf_cnpj) DO UPDATE SET senha_hash = excluded.senha_hash,
                                         alterado_em = excluded.alterado_em",
    params = list(cpf_norm, hash, agora, agora))
  invisible(TRUE)
}

# Valida senha informada contra o hash armazenado
auth_check_senha <- function(cpf_cnpj, senha, con = NULL) {
  own <- is.null(con)
  if (own) con <- sqlite_connect()
  on.exit(if (own) DBI::dbDisconnect(con), add = TRUE)
  
  auth_ensure_table(con)
  cpf_norm <- gsub("[^0-9]", "", as.character(cpf_cnpj))
  
  res <- tryCatch(
    DBI::dbGetQuery(con,
                    "SELECT senha_hash FROM auth_senhas WHERE cpf_cnpj = ?",
                    params = list(cpf_norm)),
    error = function(e) data.frame(senha_hash = character(0))
  )
  if (nrow(res) == 0) return(FALSE)
  stored <- res$senha_hash[1]

  if (startsWith(stored, "pbkdf2v2$")) {
    # Formato v2: recomputa com o salt e iterações armazenados
    partes <- strsplit(stored, "$", fixed = TRUE)[[1]]
    if (length(partes) != 4) return(FALSE)
    iter <- suppressWarnings(as.integer(partes[2]))
    if (is.na(iter) || iter < 1L) return(FALSE)
    return(identical(stored, .auth_hash_v2(senha, partes[3], iter)))
  }

  # Formato legado (SHA-256 único): valida e migra para v2 no sucesso
  ok_legado <- identical(stored, auth_hash(senha, cpf_norm))
  if (ok_legado) {
    tryCatch(auth_set_senha(cpf_norm, senha, con),
             error = function(e) message("[Auth] AVISO migração v2: ", e$message))
  }
  ok_legado
}