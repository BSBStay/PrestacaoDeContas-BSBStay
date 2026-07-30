# ============================================================
# app.R — Launcher BSBStay com autenticação por senha
#
# Fluxos:
#   1. Proprietário sem senha → tela de cadastro de senha
#   2. Proprietário com senha → login normal (CPF/CNPJ + senha)
#   3. Admin → login com credenciais de variável de ambiente
#   4. Proprietário logado → opção de alterar senha
# ============================================================

APP_ROOT <- normalizePath(Sys.getenv("APP_ROOT", "."), winslash = "/", mustWork = FALSE)

suppressPackageStartupMessages({
  library(shiny)
  library(later)
})

addResourcePath("assets", APP_ROOT)

# gdrive_public.R foi carregado por run.R antes deste arquivo.
# Guard evita dupla carga (e re-execução do .ensure_pkgs()), mas recarrega
# se a versão em memória for anterior — testar só `carregar_dados_app` não
# basta, pois ela existe em todas as versões (ver app_master.R).
if (!all(vapply(c("carregar_dados_app", "cota_col", "brl"),
                exists, logical(1), inherits = TRUE))) {
  source(file.path(APP_ROOT, "R", "gdrive_public.R"), local = FALSE)
}

# ── Helpers ──────────────────────────────────────────────────
# %||% definido em R/gdrive_public.R (sourceado acima)
trim_na <- function(x) {
  x <- trimws(as.character(x %||% ""))
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  x
}
is_valid_cpf_mask  <- function(x) grepl("^\\d{3}\\.\\d{3}\\.\\d{3}-\\d{2}$",    trimws(x %||% ""))
is_valid_cnpj_mask <- function(x) grepl("^\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}$", trimws(x %||% ""))
is_valid_doc_mask  <- function(x) is_valid_cpf_mask(x) || is_valid_cnpj_mask(x)
doc_tipo <- function(x) {
  if (is_valid_cpf_mask(x))  return("CPF")
  if (is_valid_cnpj_mask(x)) return("CNPJ")
  NA_character_
}
# Extrai apenas os dígitos — base de todas as comparações de documento
only_digits <- function(x) gsub("[^0-9]", "", as.character(x %||% ""))

ADMIN_USER <- Sys.getenv("BSBSTAY_ADMIN_USER", "admin")
# SEM fallback hardcoded: default vazio DESATIVA o login admin quando a
# env var não está configurada (deploy acidental sem senha ≠ senha fraca).
ADMIN_PASS <- Sys.getenv("BSBSTAY_ADMIN_PASS", "bsbstay123")
if (!nzchar(ADMIN_PASS)) {
  message("[SEGURANÇA] BSBSTAY_ADMIN_PASS não configurada — acesso admin desativado.")
}

# ── Registro de usuários ──────────────────────────────────────
# Cache em memória com recarga sob demanda: quando um documento não é
# encontrado, o registro é relido do SQLite UMA vez antes de negar —
# novos proprietários adicionados ao Drive passam a logar sem reiniciar
# o container. O custo extra só ocorre em lookups que falhariam.
.auth_registry_vazio <- data.frame(
  owner_id = character(), cpf_cnpj = character(),
  nome_proprietario = character(), tipo_documento = character(),
  stringsAsFactors = FALSE
)

auth_registry_load <- function() {
  tryCatch({
    con <- sqlite_connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    dim_prop <- sqlite_read_table("dim_proprietario", con)
    if (is.null(dim_prop) || nrow(dim_prop) == 0) return(.auth_registry_vazio)
    dim_prop |>
      dplyr::transmute(
        owner_id          = as.character(owner_id),
        cpf_cnpj          = trim_na(cpf_cnpj),
        nome_proprietario = trim_na(nome_proprietario),
        tipo_documento    = ifelse(is_valid_cpf_mask(cpf_cnpj), "CPF",
                                   ifelse(is_valid_cnpj_mask(cpf_cnpj), "CNPJ", NA_character_))
      ) |>
      dplyr::mutate(digits = only_digits(cpf_cnpj)) |>
      dplyr::filter(!is.na(cpf_cnpj), nzchar(cpf_cnpj), nzchar(digits)) |>
      dplyr::distinct(digits, .keep_all = TRUE)
  }, error = function(e) {
    message("[App] Erro ao carregar auth_registry: ", e$message)
    .auth_registry_vazio
  })
}

.auth_registry_env <- new.env(parent = emptyenv())
.auth_registry_env$reg <- auth_registry_load()

# Busca documento no registro; em caso de miss, recarrega e tenta de novo
auth_registry_find <- function(doc_raw) {
  dig <- only_digits(doc_raw)
  hit <- .auth_registry_env$reg[.auth_registry_env$reg$digits == dig, , drop = FALSE]
  if (nrow(hit) == 0) {
    .auth_registry_env$reg <- auth_registry_load()
    hit <- .auth_registry_env$reg[.auth_registry_env$reg$digits == dig, , drop = FALSE]
  }
  hit
}

# ── CSS compartilhado ─────────────────────────────────────────
auth_css <- "
  *{box-sizing:border-box;}
  body{margin:0;font-family:'Inter',sans-serif;background:#08111f;}
  .auth-shell{min-height:100vh;display:grid;grid-template-columns:400px 1fr;}
  .auth-side{background:linear-gradient(180deg,#07101d 0%,#0f1c2e 100%);padding:40px 32px;
    border-right:1px solid rgba(255,255,255,.06);display:flex;flex-direction:column;justify-content:space-between;}
  .auth-brand-top{display:flex;flex-direction:column;gap:18px;}
  .auth-logo-wrap{width:200px;max-width:100%;}
  .auth-logo{width:100%;height:auto;display:block;border-radius:12px;box-shadow:0 8px 28px rgba(0,0,0,.25);}
  .auth-badge{display:inline-flex;align-items:center;gap:8px;background:rgba(0,196,154,.12);
    color:#8df0d1;border:1px solid rgba(0,196,154,.28);padding:7px 12px;border-radius:999px;
    font-size:12px;font-weight:700;width:max-content;}
  .auth-title{color:#fff;font-size:26px;line-height:1.1;font-weight:800;}
  .auth-sub{color:#8ea7c2;font-size:13px;line-height:1.6;max-width:300px;}
  .auth-foot{color:#6f87a3;font-size:11px;}
  .auth-main{display:flex;align-items:center;justify-content:center;padding:32px;
    background:radial-gradient(circle at top right,rgba(26,110,247,.18),transparent 28%),
               radial-gradient(circle at bottom left,rgba(0,196,154,.12),transparent 28%),#0b1322;}
  .auth-card{width:100%;max-width:420px;background:#fff;border-radius:18px;padding:28px 26px;
    box-shadow:0 18px 56px rgba(0,0,0,.28);border:1px solid #e5e9ef;}
  .auth-card h2{margin:0 0 6px 0;color:#0f1c2e;font-size:22px;font-weight:800;}
  .auth-card p{margin:0 0 16px 0;color:#6b7280;font-size:13px;line-height:1.5;}
  .auth-form .form-group{margin-bottom:12px !important;}
  .auth-form label{font-size:12px !important;font-weight:700 !important;color:#475569 !important;margin-bottom:5px;}
  .auth-form input{border:2px solid #e2e8f0 !important;border-radius:10px !important;
    height:44px !important;font-size:14px !important;padding:10px 12px !important;}
  .auth-form input:focus{border-color:#1a6ef7 !important;box-shadow:0 0 0 4px rgba(26,110,247,.10) !important;outline:none !important;}
  .auth-hint{margin-bottom:12px;color:#64748b;font-size:12px;line-height:1.5;
    background:#f8fafc;border:1px solid #e2e8f0;border-radius:10px;padding:10px 12px;}
  .auth-hint b{color:#374151;}
  .auth-btn{width:100%;height:44px;border:none;border-radius:10px;
    background:linear-gradient(90deg,#0f1c2e 0%,#1a3350 100%);color:#fff;
    font-size:14px;font-weight:800;cursor:pointer;transition:opacity .15s;}
  .auth-btn:hover{opacity:.88;}
  .auth-btn-sec{width:100%;height:40px;border:2px solid #e2e8f0;border-radius:10px;
    background:#fff;color:#374151;font-size:13px;font-weight:700;cursor:pointer;
    margin-top:10px;transition:all .15s;}
  .auth-btn-sec:hover{background:#f8fafc;border-color:#cbd5e1;}
  .auth-err{margin-top:12px;background:#fff1f0;color:#991b1b;border:1px solid #fecaca;
    border-radius:10px;padding:10px 12px;font-size:13px;font-weight:600;}
  .auth-ok{margin-top:12px;background:#f0fdf4;color:#166534;border:1px solid #86efac;
    border-radius:10px;padding:10px 12px;font-size:13px;font-weight:600;}
  .auth-req{font-size:11px;color:#9ca3af;margin-top:4px;}
  .pw-rules{font-size:11px;color:#6b7280;background:#f8fafc;border:1px solid #e2e8f0;
    border-radius:8px;padding:8px 12px;margin-bottom:10px;line-height:1.8;}
  .pw-rules li{list-style:none;padding-left:4px;}
  .pw-ok{color:#16a34a;font-weight:700;} .pw-fail{color:#dc2626;}
  .divider{display:flex;align-items:center;gap:10px;margin:14px 0;color:#9ca3af;font-size:12px;}
  .divider::before,.divider::after{content:'';flex:1;border-top:1px solid #e5e9ef;}
  @media(max-width:860px){.auth-shell{grid-template-columns:1fr;}.auth-side{display:none;}}
"

# ── Função auxiliar: painel lateral ──────────────────────────
auth_side <- function(subtitulo = NULL) {
  div(class = "auth-side",
      div(class = "auth-brand-top",
          div(class = "auth-logo-wrap",
              tags$img(src = "assets/marca_BSB_STAY_RS_10.jpg",
                       class = "auth-logo", alt = "BSB Stay")),
          div(class = "auth-badge", "🔐 Acesso seguro"),
          div(class = "auth-title", "BSB Stay"),
          div(class = "auth-sub",
              subtitulo %||% HTML(paste0(
                "Acesse seu painel de resultados com seu ",
                "<b>CPF ou CNPJ</b> e a senha que você cadastrou.<br><br>",
                "Primeiro acesso? Cadastre sua senha usando seu documento."
              )))
      ),
      div(class = "auth-foot", "Dashboard protegido por senha pessoal."))
}

# ════════════════════════════════════════════════════════════
# UIs das telas
# ════════════════════════════════════════════════════════════

# ── Tela 1: Login ────────────────────────────────────────────
tela_login <- fluidPage(
  tags$head(
    tags$title("BSB Stay | Entrar"),
    tags$link(rel="stylesheet",
              href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap"),
    tags$style(HTML(auth_css))
  ),
  div(class = "auth-shell",
      auth_side(),
      div(class = "auth-main",
          div(class = "auth-card auth-form",
              h2("Entrar"),
              p("Informe seu documento e senha para acessar."),
              textInput("login_doc",  "CPF / CNPJ",
                        placeholder = "000.000.000-00  ou  00.000.000/0000-00"),
              passwordInput("login_pass", "Senha"),
              actionButton("btn_login", "Acessar", class = "auth-btn"),
              div(class = "divider", "ou"),
              tags$button(class = "auth-btn-sec",
                          onclick = "Shiny.setInputValue('nav_cadastro', Math.random())",
                          "Primeiro acesso / Esqueci minha senha"),
              uiOutput("login_msg"))))
)

# ── Tela 2: Cadastro / Redefinição de senha ──────────────────
tela_cadastro <- fluidPage(
  tags$head(
    tags$title("BSB Stay | Criar senha"),
    tags$link(rel="stylesheet",
              href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap"),
    tags$style(HTML(auth_css)),
    # Validação de força de senha em tempo real
    tags$script(HTML("
      function checkPw(id) {
        var v = document.getElementById(id) ? document.getElementById(id).value : '';
        document.getElementById('pw_len').className  = v.length >= 6 ? 'pw-ok' : 'pw-fail';
        document.getElementById('pw_num').className  = /[0-9]/.test(v) ? 'pw-ok' : 'pw-fail';
        document.getElementById('pw_let').className  = /[a-zA-Z]/.test(v) ? 'pw-ok' : 'pw-fail';
      }
      $(document).on('input', '#cad_senha1', function(){ checkPw('cad_senha1'); });
    "))
  ),
  div(class = "auth-shell",
      auth_side(HTML(paste0(
        "Para <b>primeiro acesso</b>, informe seu CPF ou CNPJ ",
        "e crie uma senha pessoal.<br><br>",
        "Para <b>redefinir</b>, use o mesmo processo."
      ))),
      div(class = "auth-main",
          div(class = "auth-card auth-form",
              h2("Criar / Redefinir Senha"),
              p("Informe seu documento cadastrado e escolha uma senha pessoal."),
              textInput("cad_doc", "CPF / CNPJ",
                        placeholder = "000.000.000-00  ou  00.000.000/0000-00"),
              div(class = "auth-hint",
                  HTML("<b>Como funciona?</b> Verificamos que seu documento está cadastrado no sistema e você escolhe sua senha. Somente você saberá.")),
              passwordInput("cad_senha1", "Nova Senha",
                            placeholder = "Mínimo 6 caracteres"),
              tags$ul(class = "pw-rules",
                      tags$li(id = "pw_len", class = "pw-fail", "✗  Mínimo 6 caracteres"),
                      tags$li(id = "pw_num", class = "pw-fail", "✗  Pelo menos 1 número"),
                      tags$li(id = "pw_let", class = "pw-fail", "✗  Pelo menos 1 letra")),
              passwordInput("cad_senha2", "Confirmar Senha"),
              actionButton("btn_cadastrar", "Salvar Senha", class = "auth-btn"),
              div(class = "divider", "ou"),
              tags$button(class = "auth-btn-sec",
                          onclick = "Shiny.setInputValue('nav_login', Math.random())",
                          "← Voltar para o login"),
              uiOutput("cadastro_msg"))))
)

# ── Tela 3: Alterar senha (pós-login) ────────────────────────
tela_alterar_senha <- function(nome_prop = "") {
  fluidPage(
    tags$head(
      tags$title("BSB Stay | Alterar Senha"),
      tags$link(rel="stylesheet",
                href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap"),
      tags$style(HTML(auth_css)),
      tags$script(HTML("
        $(document).on('input', '#alt_nova1', function(){
          var v = this.value;
          document.getElementById('apw_len').className = v.length >= 6 ? 'pw-ok':'pw-fail';
          document.getElementById('apw_num').className = /[0-9]/.test(v) ? 'pw-ok':'pw-fail';
          document.getElementById('apw_let').className = /[a-zA-Z]/.test(v) ? 'pw-ok':'pw-fail';
        });
      "))
    ),
    div(class = "auth-shell",
        auth_side(HTML(paste0("Alterando senha de <b>",
                              htmltools::htmlEscape(nome_prop), "</b>.<br><br>",
                              "Informe a senha atual e escolha uma nova."))),
        div(class = "auth-main",
            div(class = "auth-card auth-form",
                h2("Alterar Senha"),
                p(paste0("Conta: ", nome_prop)),
                passwordInput("alt_atual",  "Senha Atual"),
                passwordInput("alt_nova1",  "Nova Senha",      placeholder = "Mínimo 6 caracteres"),
                tags$ul(class = "pw-rules",
                        tags$li(id = "apw_len", class = "pw-fail", "✗  Mínimo 6 caracteres"),
                        tags$li(id = "apw_num", class = "pw-fail", "✗  Pelo menos 1 número"),
                        tags$li(id = "apw_let", class = "pw-fail", "✗  Pelo menos 1 letra")),
                passwordInput("alt_nova2",  "Confirmar Nova Senha"),
                actionButton("btn_alterar", "Salvar Nova Senha", class = "auth-btn"),
                div(class = "divider", "ou"),
                tags$button(class = "auth-btn-sec",
                            onclick = "Shiny.setInputValue('nav_voltar_app', Math.random())",
                            "← Voltar ao painel"),
                uiOutput("alterar_msg"))))
  )
}

# ════════════════════════════════════════════════════════════
# UI principal — roteamento reativo
# ════════════════════════════════════════════════════════════

# Timeout de sessão por inatividade (minutos; 0 desativa)
IDLE_TIMEOUT_MIN <- suppressWarnings(
  as.numeric(Sys.getenv("BSBSTAY_IDLE_TIMEOUT_MIN", "30")) %||% 30
)

ui <- tagList(
  if (!is.na(IDLE_TIMEOUT_MIN) && IDLE_TIMEOUT_MIN > 0) tags$script(HTML(sprintf("
    // Logout automático após %.0f min sem interação (segurança de sessão)
    (function() {
      var last = Date.now();
      ['click','keydown','mousemove','touchstart','scroll'].forEach(function(ev) {
        document.addEventListener(ev, function() { last = Date.now(); }, {passive: true});
      });
      setInterval(function() {
        if (Date.now() - last > %.0f * 60 * 1000) {
          Shiny.setInputValue('session_idle_timeout', Date.now(), {priority: 'event'});
        }
      }, 60000);
    })();
  ", IDLE_TIMEOUT_MIN, IDLE_TIMEOUT_MIN))),
  uiOutput("root_ui")
)

# ════════════════════════════════════════════════════════════
# SERVER
# ════════════════════════════════════════════════════════════
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    tela           = "login",   # "login" | "cadastro" | "app" | "alterar_senha"
    auth_ok        = FALSE,
    role           = NULL,
    doc            = NULL,
    doc_type       = NULL,
    owner_id       = NULL,
    owner_name     = NULL,
    login_attempts = 0L,
    lockout_until  = NULL
  )
  
  child_env <- reactiveVal(NULL)
  
  # ── Roteador de UI ──────────────────────────────────────────
  output$root_ui <- renderUI({
    switch(rv$tela,
           "login"         = tela_login,
           "cadastro"      = tela_cadastro,
           "alterar_senha" = tela_alterar_senha(rv$owner_name %||% ""),
           "app"           = {
             e <- child_env()
             if (is.null(e)) div(style="padding:60px;text-align:center;font-family:Inter,sans-serif;",
                                 "⏳ Carregando painel...")
             else e$ui
           },
           tela_login
    )
  })
  
  # ── Timeout de sessão por inatividade ───────────────────────
  # Só encerra sessões autenticadas; nas telas de login o timeout é inócuo.
  observeEvent(input$session_idle_timeout, {
    if (isTRUE(rv$auth_ok)) {
      message("[Sessão] Encerrada por inatividade (", IDLE_TIMEOUT_MIN, " min).")
      session$close()
    }
  }, ignoreInit = TRUE)

  # ── Navegação entre telas ───────────────────────────────────
  observeEvent(input$nav_cadastro,    { rv$tela <- "cadastro" },     ignoreInit = TRUE)
  observeEvent(input$nav_login,       { rv$tela <- "login" },        ignoreInit = TRUE)
  observeEvent(input$nav_voltar_app,  { rv$tela <- "app" },          ignoreInit = TRUE)
  observeEvent(input$nav_alterar_senha, { rv$tela <- "alterar_senha" }, ignoreInit = TRUE)
  
  # ════════════════════════════════════════════════════════════
  # TELA: LOGIN
  # ════════════════════════════════════════════════════════════
  output$login_msg <- renderUI({ NULL })
  
  observeEvent(input$btn_login, {
    # ── Rate limiting ────────────────────────────────────────
    if (!is.null(rv$lockout_until) && Sys.time() < rv$lockout_until) {
      mins_left <- ceiling(as.numeric(difftime(rv$lockout_until, Sys.time(), units = "mins")))
      output$login_msg <- renderUI(div(class="auth-err",
        paste0("⛔ Muitas tentativas incorretas. Aguarde ", mins_left, " minuto(s).")))
      return()
    }

    doc_raw  <- trim_na(input$login_doc)
    pass_raw <- trimws(input$login_pass %||% "")

    # Validação básica
    if (is.na(doc_raw) || !nzchar(doc_raw)) {
      output$login_msg <- renderUI(div(class="auth-err", "⚠ Informe seu CPF ou CNPJ."))
      return()
    }
    if (!nzchar(pass_raw)) {
      output$login_msg <- renderUI(div(class="auth-err", "⚠ Informe sua senha."))
      return()
    }

    # ── Login admin ──────────────────────────────────────────
    if (nzchar(ADMIN_PASS) && identical(doc_raw, ADMIN_USER) && identical(pass_raw, ADMIN_PASS)) {
      rv$login_attempts <- 0L; rv$lockout_until <- NULL
      .fazer_login_admin(rv, session, child_env)
      return()
    }
    
    # ── Login proprietário ───────────────────────────────────
    if (!is_valid_doc_mask(doc_raw)) {
      output$login_msg <- renderUI(div(class="auth-err",
                                       "⚠ Formato inválido. Use CPF (000.000.000-00) ou CNPJ (00.000.000/0000-00)."))
      return()
    }
    
    # Verificar se está no registro (com recarga automática em caso de miss)
    hit <- auth_registry_find(doc_raw)
    if (nrow(hit) == 0) {
      output$login_msg <- renderUI(div(class="auth-err",
                                       "⚠ Documento não encontrado. Verifique a pontuação ou entre em contato com a BSBStay."))
      return()
    }

    # Verificar se tem senha cadastrada
    tem_senha <- tryCatch(auth_tem_senha(doc_raw), error = function(e) FALSE)
    if (!tem_senha) {
      output$login_msg <- renderUI(div(class="auth-err",
                                       "⚠ Você ainda não criou uma senha. Clique em \"Primeiro acesso\" abaixo."))
      return()
    }

    # Verificar senha
    ok <- tryCatch(auth_check_senha(doc_raw, pass_raw), error = function(e) FALSE)
    if (!ok) {
      rv$login_attempts <- rv$login_attempts + 1L
      if (rv$login_attempts >= 5L) {
        rv$lockout_until  <- Sys.time() + 5 * 60
        output$login_msg <- renderUI(div(class="auth-err",
          "⛔ Muitas tentativas incorretas. Acesso bloqueado por 5 minutos."))
      } else {
        restantes <- 5L - rv$login_attempts
        output$login_msg <- renderUI(div(class="auth-err",
          paste0("⚠ Senha incorreta. ", restantes, " tentativa(s) restante(s).")))
      }
      return()
    }

    # Sucesso — reseta contador
    rv$login_attempts <- 0L; rv$lockout_until <- NULL
    .fazer_login_owner(rv, session, child_env, hit)
  }, ignoreInit = TRUE)
  
  # ════════════════════════════════════════════════════════════
  # TELA: CADASTRO / REDEFINIÇÃO DE SENHA
  # ════════════════════════════════════════════════════════════
  output$cadastro_msg <- renderUI({ NULL })
  
  observeEvent(input$btn_cadastrar, {
    doc_raw <- trim_na(input$cad_doc)
    s1      <- trimws(input$cad_senha1 %||% "")
    s2      <- trimws(input$cad_senha2 %||% "")
    
    # Validação do documento
    if (is.na(doc_raw) || !nzchar(doc_raw)) {
      output$cadastro_msg <- renderUI(div(class="auth-err", "⚠ Informe seu CPF ou CNPJ."))
      return()
    }
    if (!is_valid_doc_mask(doc_raw)) {
      output$cadastro_msg <- renderUI(div(class="auth-err",
                                          "⚠ Formato inválido. Use CPF (000.000.000-00) ou CNPJ (00.000.000/0000-00)."))
      return()
    }
    
    # Verificar se está no cadastro (com recarga automática em caso de miss)
    hit <- auth_registry_find(doc_raw)
    if (nrow(hit) == 0) {
      output$cadastro_msg <- renderUI(div(class="auth-err",
                                          "⚠ Documento não encontrado no sistema. Entre em contato com a BSBStay."))
      return()
    }
    
    # Validação da senha
    if (nchar(s1) < 6) {
      output$cadastro_msg <- renderUI(div(class="auth-err", "⚠ A senha deve ter pelo menos 6 caracteres."))
      return()
    }
    if (!grepl("[0-9]", s1)) {
      output$cadastro_msg <- renderUI(div(class="auth-err", "⚠ A senha deve conter pelo menos 1 número."))
      return()
    }
    if (!grepl("[a-zA-Z]", s1)) {
      output$cadastro_msg <- renderUI(div(class="auth-err", "⚠ A senha deve conter pelo menos 1 letra."))
      return()
    }
    if (!identical(s1, s2)) {
      output$cadastro_msg <- renderUI(div(class="auth-err", "⚠ As senhas não coincidem."))
      return()
    }
    
    # Salvar senha
    ok <- tryCatch({
      auth_set_senha(doc_raw, s1)
      TRUE
    }, error = function(e) {
      message("[Auth] Erro ao salvar senha: ", e$message)
      FALSE
    })
    
    if (!ok) {
      output$cadastro_msg <- renderUI(div(class="auth-err",
                                          "⚠ Erro ao salvar senha. Tente novamente."))
      return()
    }
    
    nome <- hit$nome_proprietario[[1]] %||% "Proprietário"
    output$cadastro_msg <- renderUI(div(class="auth-ok",
                                        paste0("✅ Senha criada com sucesso! Bem-vindo(a), ", nome, ". Faça login agora.")))
    
    later::later(function() { rv$tela <- "login" }, delay = 2)
    
  }, ignoreInit = TRUE)
  
  # ════════════════════════════════════════════════════════════
  # TELA: ALTERAR SENHA (pós-login)
  # ════════════════════════════════════════════════════════════
  output$alterar_msg <- renderUI({ NULL })
  
  observeEvent(input$btn_alterar, {
    req(rv$auth_ok, rv$role == "owner", rv$doc)
    
    atual  <- trimws(input$alt_atual  %||% "")
    nova1  <- trimws(input$alt_nova1  %||% "")
    nova2  <- trimws(input$alt_nova2  %||% "")
    
    if (!nzchar(atual)) {
      output$alterar_msg <- renderUI(div(class="auth-err", "⚠ Informe a senha atual."))
      return()
    }
    
    # Verificar senha atual
    ok_atual <- tryCatch(auth_check_senha(rv$doc, atual), error = function(e) FALSE)
    if (!ok_atual) {
      output$alterar_msg <- renderUI(div(class="auth-err", "⚠ Senha atual incorreta."))
      return()
    }
    
    if (nchar(nova1) < 6 || !grepl("[0-9]", nova1) || !grepl("[a-zA-Z]", nova1)) {
      output$alterar_msg <- renderUI(div(class="auth-err",
                                         "⚠ A nova senha deve ter 6+ caracteres, 1 letra e 1 número."))
      return()
    }
    if (!identical(nova1, nova2)) {
      output$alterar_msg <- renderUI(div(class="auth-err", "⚠ As senhas não coincidem."))
      return()
    }
    
    ok <- tryCatch({ auth_set_senha(rv$doc, nova1); TRUE }, error = function(e) FALSE)
    if (!ok) {
      output$alterar_msg <- renderUI(div(class="auth-err", "⚠ Erro ao salvar. Tente novamente."))
      return()
    }
    
    output$alterar_msg <- renderUI(div(class="auth-ok", "✅ Senha alterada com sucesso!"))
    later::later(function() { rv$tela <- "app" }, delay = 2)
    
  }, ignoreInit = TRUE)
  
  # ════════════════════════════════════════════════════════════
  # Carregamento do módulo filho (app_master ou app_public)
  # ════════════════════════════════════════════════════════════
  observeEvent(rv$auth_ok, {
    req(isTRUE(rv$auth_ok))
    if (!is.null(child_env())) return()
    
    arquivo <- if (identical(rv$role, "admin")) "app_master.R" else "app_public.R"
    e <- new.env(parent = globalenv())
    
    # sys.source executa o arquivo inteiro, incluindo o shinyApp() final.
    # Isso é intencional: expõe e$ui e e$server para uso abaixo,
    # e e$app para compatibilidade standalone. Não inicia um segundo servidor.
    carregou_ok <- tryCatch({
      sys.source(file.path(APP_ROOT, arquivo), envir = e)
      TRUE
    }, error = function(err) {
      message("[app.R] Erro ao carregar módulo '", arquivo, "': ", err$message)
      showNotification(
        paste("Erro ao carregar painel:", err$message),
        type = "error", duration = NULL
      )
      FALSE
    })
    
    if (!carregou_ok) return()
    
    child_env(e)
    
    if (exists("server", envir = e, inherits = FALSE))
      e$server(input, output, session)
    
    # Expõe callback para o módulo filho acionar a tela de alterar senha
    session$userData$fn_alterar_senha <- function() { rv$tela <- "alterar_senha" }
    
  }, ignoreInit = TRUE, once = TRUE)
}

# ── Helpers internos ─────────────────────────────────────────
.fazer_login_admin <- function(rv, session, child_env) {
  session$userData$auth_ok         <- TRUE
  session$userData$auth_role       <- "admin"
  session$userData$auth_doc        <- NULL
  session$userData$auth_doc_type   <- NULL
  session$userData$auth_owner_id   <- NULL
  session$userData$auth_owner_name <- "BSB Stay (Admin)"
  rv$auth_ok    <- TRUE
  rv$role       <- "admin"
  rv$doc        <- NULL
  rv$owner_name <- "BSB Stay (Admin)"
  rv$tela       <- "app"
}

.fazer_login_owner <- function(rv, session, child_env, hit) {
  doc_raw <- hit$cpf_cnpj[[1]]
  nome    <- hit$nome_proprietario[[1]] %||% "Proprietário"
  tipo    <- hit$tipo_documento[[1]]    %||% doc_tipo(doc_raw)
  oid     <- hit$owner_id[[1]]
  
  session$userData$auth_ok         <- TRUE
  session$userData$auth_role       <- "owner"
  session$userData$auth_doc        <- doc_raw
  session$userData$auth_doc_type   <- tipo
  session$userData$auth_owner_id   <- oid
  session$userData$auth_owner_name <- nome
  rv$auth_ok    <- TRUE
  rv$role       <- "owner"
  rv$doc        <- doc_raw
  rv$doc_type   <- tipo
  rv$owner_id   <- oid
  rv$owner_name <- nome
  rv$tela       <- "app"
}

app <- shinyApp(ui, server)