# ============================================================
# BSB.STAY — Extrato do Proprietário
# app.R — v3.0: Análise Completa de Despesas, Custos,
#               Ordens de Serviço e Diária entre Check-ins
#
# Novidades v3.0 (sobre v2.0):
#   - Seção "Operacional": Despesas | Custos por Apt. | OS
#   - Seção "Análise da Diária": valor entre check-ins + KPIs
#   - Todas as seções seguem os filtros de mês e imóvel
# ============================================================

# ── Bootstrap Render/Docker ───────────────────────────────────
options(
  shiny.host = "0.0.0.0",
  shiny.port = as.integer(Sys.getenv("PORT", "3838"))
)

APP_ROOT <- normalizePath(Sys.getenv("APP_ROOT", "."), winslash = "/", mustWork = FALSE)
dir.create(file.path(APP_ROOT, "data", "cache"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(APP_ROOT, "data", "raw"), recursive = TRUE, showWarnings = FALSE)

# ============================================================
# app_public.R — Extrato do Proprietário autenticado por sessão
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(lubridate)
  library(plotly)
  library(DT)
  library(htmltools)
})

APP_ROOT <- normalizePath(Sys.getenv("APP_ROOT", "."), winslash = "/", mustWork = FALSE)

if (!exists("carregar_dados_app")) {
  source(file.path(APP_ROOT, "R", "gdrive_public.R"), local = FALSE)
}

# Usa APP_DATA_GLOBAL pré-aquecido pelo run.R (não bloqueia a porta).
# Fallback: tenta carregar localmente se o global ainda não estiver pronto.
# APP_DATA_GLOBAL é populado pelo run.R (Etapa A = SQLite local, Etapa B = Drive).
# Nunca fazemos download bloqueante aqui — o login ficaria travado.
APP_DATA <- if (exists("APP_DATA_GLOBAL") && length(APP_DATA_GLOBAL) > 0) {
  APP_DATA_GLOBAL
} else {
  structure(list(), erro_msg = "Dados ainda carregando, aguarde...")
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

TOKEN_TODOS <- "__todos__"

# fmt_mes_pt, MESES_PT_FULL/ABBR definidos em R/gdrive_public.R

fmt_currency <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  if (length(v) == 0) return("R$ —")
  ifelse(
    is.na(v),
    "R$ —",
    paste0("R$ ", formatC(v, format = "f", digits = 2, big.mark = ".", decimal.mark = ","))
  )
}

# brl vetorizada — segura dentro de dplyr::transmute
brl <- function(x) {
  sapply(x, function(v) {
    v <- suppressWarnings(as.numeric(v))
    if (is.na(v)) return("R$ \u2014")
    paste0("R$ ", formatC(v, format = "f", digits = 2, big.mark = ".", decimal.mark = ","))
  }, USE.NAMES = FALSE)
}


safe_num <- function(x, default = 0) {
  y <- suppressWarnings(as.numeric(x))
  y[is.na(y)] <- default
  y
}

safe_date_month <- function(x) {
  if (inherits(x, "Date")) return(as.Date(format(x, "%Y-%m-01")))
  suppressWarnings(as.Date(paste0(substr(as.character(x), 1, 7), "-01")))
}

kcard <- function(lbl, val, delta = "", dn = FALSE, vg = FALSE, icon = "", extra_class = "") {
  div(
    class = paste("kcard", extra_class),
    div(class = "klbl", if (nzchar(icon)) paste(icon, lbl) else lbl),
    div(class = if (vg) "kval g" else "kval", val),
    div(class = if (dn) "kdelta dn" else "kdelta up", delta)
  )
}

frow <- function(lbl, val, neg = FALSE) {
  div(
    class = "fr",
    span(class = "fl", lbl),
    span(class = if (neg) "fv r" else "fv", val)
  )
}

kcard_sm <- function(lbl, val, cor = "blue") {
  cor_map <- c(blue="#1a6ef7", green="#00b388", red="#e03e3e", orange="#d97706", purple="#7c3aed", teal="#0891b2")
  clr <- cor_map[[cor]] %||% "#1a6ef7"
  div(class = "kcard-sm",
      div(class = "ksm-lbl", lbl),
      div(class = "ksm-val", style = paste0("color:", clr), val))
}

ui <- fluidPage(
  tags$head(
    tags$title("BSB.STAY — Extrato do Proprietário"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap"),
    tags$script(HTML("
      // ── TABS OPERACIONAL: gerenciadas 100% no cliente ────────────
      // setOpTab() troca a aba ativa via JS puro, sem re-renderizar o body.
      // Apenas o painel_operacional (renderUI proprio) e atualizado pelo server.
      function setOpTab(aba) {
        // Atualiza classe active nos botoes
        document.querySelectorAll('.op-tab').forEach(function(b) {
          b.classList.remove('active');
        });
        var btn = document.getElementById('optab-' + aba);
        if (btn) btn.classList.add('active');
        // Preserva scroll antes de enviar ao server
        var sy = window.scrollY;
        Shiny.setInputValue('btn_aba_op', aba, {priority: 'event'});
        // Restaura scroll apos o server atualizar o painel_operacional
        var restore = function(e) {
          if (e.detail && e.detail.name === 'painel_operacional') {
            document.removeEventListener('shiny:value', restore);
            requestAnimationFrame(function() {
              requestAnimationFrame(function() {
                window.scrollTo({ top: sy, behavior: 'instant' });
              });
            });
          }
        };
        document.addEventListener('shiny:value', restore);
        // Safety: remover listener apos 3s se nao disparar
        setTimeout(function() {
          document.removeEventListener('shiny:value', restore);
        }, 3000);
      }

      // ── SCROLL GERAL: restaura posicao apos renderUIs que nao sejam ──
      // painel_operacional (que ja tem tratamento proprio acima).
      // Cobre casos como filter_bar, sync_bar, sec_detalhamento_mes etc.
      var _savedScrollY = null;
      var _scrollSaveTime = 0;

      document.addEventListener('mousedown', function(e) {
        var el = e.target.closest('button, [onclick]');
        // Nao capturar cliques nas op-tabs (ja tratadas pelo setOpTab)
        if (el && !e.target.closest('.op-tab') && window.scrollY > 80) {
          _savedScrollY = window.scrollY;
          _scrollSaveTime = Date.now();
        }
      });

      document.addEventListener('shiny:value', function(e) {
        if (e.detail && e.detail.name === 'painel_operacional') return;
        if (_savedScrollY !== null && (Date.now() - _scrollSaveTime) < 2000) {
          var saved = _savedScrollY;
          _savedScrollY = null;
          requestAnimationFrame(function() {
            requestAnimationFrame(function() {
              window.scrollTo({ top: saved, behavior: 'instant' });
            });
          });
        }
      });
    ")),
    tags$style(HTML("
/* ══ RESET ══════════════════════════════════════════════════ */
*{box-sizing:border-box;margin:0;padding:0;}
body{font-family:'Inter',sans-serif;background:#f0f2f5;color:#1e2d3d;font-size:14px;}
a{color:inherit;text-decoration:none;}

/* ══ HEADER ═════════════════════════════════════════════════ */
.hdr{background:#0f1c2e;padding:14px 32px;display:flex;align-items:center;justify-content:space-between;position:sticky;top:0;z-index:100;box-shadow:0 2px 8px rgba(0,0,0,.25);}
.hdr-left{display:flex;align-items:center;gap:14px;}
.hdr-logo-img{height:52px;width:auto;display:block;border-radius:10px;box-shadow:0 4px 14px rgba(0,0,0,.25);background:#fff;}
.hdr-title{color:#fff;font-size:17px;font-weight:700;}
.hdr-sub{color:#5a7a96;font-size:11px;margin-top:2px;}
.hdr-prop{color:#7a9ab5;font-size:12px;text-align:right;line-height:1.5;}
.hdr-prop b{color:#e2f0ff;}
.btn-dl{background:rgba(0,179,136,.15);border:1px solid rgba(0,179,136,.35);color:#34d99e;
  border-radius:8px;padding:6px 14px;font-size:11px;font-weight:700;cursor:pointer;
  font-family:'Inter',sans-serif;white-space:nowrap;transition:all .15s;text-decoration:none;
  display:inline-flex;align-items:center;gap:6px;line-height:1;}
.btn-dl:hover{background:rgba(0,179,136,.25);border-color:rgba(0,179,136,.6);color:#fff;}
.btn-dl svg{flex-shrink:0;}
.btn-dl-ready{background:rgba(0,179,136,.25);border-color:rgba(0,179,136,.5);font-size:10px;}
.hdr-badge{background:#1a3350;color:#5ab4ff;border-radius:20px;padding:3px 10px;font-size:10px;font-weight:700;letter-spacing:.6px;display:inline-block;margin-top:4px;}

/* ══ SYNC BAR ════════════════════════════════════════════════ */
.sync-bar{background:#f8fafc;border-bottom:1px solid #e2e8f0;padding:6px 32px;display:flex;align-items:center;gap:10px;font-size:11px;color:#6b7280;}
.sync-dot{width:7px;height:7px;border-radius:50%;background:#00b388;flex-shrink:0;}
.sync-dot.old{background:#d97706;} .sync-dot.err{background:#e03e3e;}
.sync-btn{background:none;border:1px solid #d1d5db;border-radius:6px;padding:3px 10px;font-size:11px;color:#374151;cursor:pointer;font-family:inherit;transition:all .15s;}
.sync-btn:hover{background:#f3f4f6;}

/* ══ CNPJ BAR ════════════════════════════════════════════════ */
.cnpj-bar{background:#fff;padding:18px 32px;border-bottom:1px solid #e2e8f0;}
.cnpj-bar-inner{max-width:680px;margin:0 auto;}
.cnpj-bar-title{font-weight:800;color:#0f1c2e;font-size:15px;margin-bottom:4px;}
.cnpj-bar-sub{font-size:12px;color:#6b7280;margin-bottom:14px;}
.cnpj-input-wrap{display:flex;gap:10px;align-items:flex-end;}
.cnpj-input-wrap .form-group{flex:1;margin-bottom:0!important;}
.cnpj-input-wrap input{border:2px solid #e2e8f0!important;border-radius:8px!important;padding:10px 14px!important;font-size:14px!important;font-family:'Inter',sans-serif!important;color:#0f1c2e!important;transition:border .15s!important;}
.cnpj-input-wrap input:focus{border-color:#1a6ef7!important;outline:none!important;box-shadow:0 0 0 3px rgba(26,110,247,.12)!important;}
.cnpj-btn{background:#0f1c2e;color:#fff;border:none;border-radius:8px;padding:10px 20px;font-size:13px;font-weight:700;cursor:pointer;font-family:'Inter',sans-serif;white-space:nowrap;}
.cnpj-btn:hover{background:#1a3350;}
.cnpj-erro{background:#fff1f0;border:1px solid #fca5a5;color:#991b1b;padding:10px 14px;border-radius:8px;font-size:13px;margin-top:10px;}
.cnpj-ok{background:#f0fdf4;border:1px solid #86efac;color:#166534;padding:10px 14px;border-radius:8px;font-size:13px;margin-top:10px;}
.cnpj-hint{font-size:11px;color:#9ca3af;margin-top:8px;}

/* ══ FILTER BAR ══════════════════════════════════════════════ */
.fbar{background:#fff;padding:10px 32px;border-bottom:2px solid #e8edf3;display:flex;gap:14px;align-items:center;flex-wrap:wrap;}
.fbar-lbl{font-size:11px;color:#6b7280;font-weight:700;letter-spacing:.6px;}

/* ══ CONTENT ═════════════════════════════════════════════════ */
.content{padding:20px 32px 56px;max-width:1340px;margin:0 auto;}
.sec{font-size:10px;font-weight:800;color:#6b7280;letter-spacing:1.5px;text-transform:uppercase;margin:26px 0 10px;padding-bottom:6px;border-bottom:2px solid #e5e9ef;}

/* ══ KPI CARDS ══════════════════════════════════════════════ */
/* ── KPIs principais: todos os cards com largura igual (simetria) ── */
.kgrid{display:grid;grid-template-columns:repeat(7,1fr);gap:12px;margin-bottom:6px;align-items:stretch;}
@media(max-width:1300px){.kgrid{grid-template-columns:repeat(4,1fr);}}
@media(max-width:700px){.kgrid{grid-template-columns:repeat(2,1fr);}}
.kcard{background:#fff;border-radius:12px;padding:18px 20px;border:1px solid #e5e9ef;
  box-shadow:0 1px 4px rgba(0,0,0,.04);transition:box-shadow .15s;height:100%;}
.kcard:hover{box-shadow:0 3px 12px rgba(0,0,0,.08);}
/* hero: card de destaque — mesma largura mas visual diferenciado */
.kcard.hero{background:linear-gradient(145deg,#0a1e36 0%,#0f2d4a 100%);border:none;
  box-shadow:0 8px 32px rgba(0,30,60,.28);}
.kcard.hero .klbl{color:rgba(255,255,255,.55);letter-spacing:1.2px;}
.kcard.hero .kval{color:#ffffff;font-size:22px;}
.kcard.hero .kval.g{color:#34d99e;}
.kcard.hero .kdelta{color:rgba(255,255,255,.45);}
.kcard.hero .kdelta.up{color:#34d99e;}
.klbl{font-size:10px;font-weight:700;color:#6b7280;letter-spacing:.9px;text-transform:uppercase;margin-bottom:7px;}
.kval{font-size:20px;font-weight:800;color:#0f1c2e;line-height:1.1;}
.kval.g{color:#00b388;}
.kdelta{font-size:11px;margin-top:6px;font-weight:600;}
.kdelta.up{color:#00b388;} .kdelta.dn{color:#e03e3e;}

/* KPI small — para diária e métricas operacionais */
.kgrid-sm{display:grid;grid-template-columns:repeat(6,1fr);gap:10px;margin-bottom:14px;}
@media(max-width:1100px){.kgrid-sm{grid-template-columns:repeat(3,1fr);}}
@media(max-width:600px){.kgrid-sm{grid-template-columns:repeat(2,1fr);}}
.kgrid-sm.kgrid-sm-2{grid-template-columns:repeat(2,1fr);}
.kgrid-sm.kgrid-sm-3{grid-template-columns:repeat(3,1fr);}
.kgrid-sm.kgrid-sm-4{grid-template-columns:repeat(4,1fr);}
@media(max-width:700px){.kgrid-sm.kgrid-sm-3,.kgrid-sm.kgrid-sm-4{grid-template-columns:repeat(2,1fr);}}
.kcard-sm{background:#fff;border-radius:10px;padding:16px 20px;border:1px solid #e5e9ef;
  box-shadow:0 1px 3px rgba(0,0,0,.04);height:100%;}
.ksm-lbl{font-size:9px;font-weight:700;color:#9aa5b4;letter-spacing:.9px;text-transform:uppercase;margin-bottom:5px;}
.ksm-val{font-size:24px;font-weight:800;line-height:1.1;}

/* ══ CARDS ══════════════════════════════════════════════════ */
.card{background:#fff;border-radius:12px;padding:20px;border:1px solid #e5e9ef;box-shadow:0 1px 5px rgba(0,0,0,.04);}
.cgrid{display:grid;grid-template-columns:1fr 1fr;gap:14px;margin-bottom:6px;}
.cgrid-3{display:grid;grid-template-columns:1fr 1fr 1fr;gap:14px;margin-bottom:6px;}
@media(max-width:900px){.cgrid{grid-template-columns:1fr;}.cgrid-3{grid-template-columns:1fr 1fr;}}
@media(max-width:600px){.cgrid-3{grid-template-columns:1fr;}}
.card-hdr{display:flex;justify-content:space-between;align-items:center;margin-bottom:14px;}
.card-ttl{font-size:13px;font-weight:700;color:#1e2d3d;}
.badge{background:#f0f4f8;color:#6b7280;font-size:10px;padding:3px 9px;border-radius:12px;font-weight:700;}
.badge-blue{background:#eff6ff;color:#2563eb;}
.badge-green{background:#f0fdf4;color:#16a34a;}
.badge-orange{background:#fff7ed;color:#d97706;}
.badge-red{background:#fff1f0;color:#e03e3e;}
.badge-purple{background:#faf5ff;color:#7c3aed;}

/* ══ IMÓVEL CARDS ═══════════════════════════════════════════ */
.imovel-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(220px,1fr));gap:12px;margin-bottom:6px;}
.icard{background:#fff;border-radius:10px;padding:14px 16px;border:1px solid #e5e9ef;box-shadow:0 1px 3px rgba(0,0,0,.04);transition:box-shadow .15s;}
.icard:hover{box-shadow:0 4px 14px rgba(0,0,0,.08);}
.icard-nome{font-size:12px;font-weight:700;color:#0f1c2e;margin-bottom:2px;}
.icard-end{font-size:11px;color:#6b7280;margin-bottom:7px;}
.icard-tipo{font-size:10px;background:#f0f4f8;color:#374151;padding:2px 8px;border-radius:10px;display:inline-block;font-weight:600;margin-bottom:8px;}
.icard-plat{display:flex;gap:4px;flex-wrap:wrap;}
.plat-tag{font-size:9px;font-weight:700;padding:2px 7px;border-radius:8px;text-transform:uppercase;letter-spacing:.4px;}
.p-air{background:#fff1f0;color:#e03e3e;} .p-bk{background:#e8f4ff;color:#1a6ef7;}
.p-dc{background:#fff7e6;color:#d97706;} .p-vr{background:#f0fdf4;color:#16a34a;}
.p-ex{background:#faf5ff;color:#7c3aed;} .p-out{background:#f3f4f6;color:#6b7280;}

/* ══ CALENDÁRIO ═════════════════════════════════════════════ */
.cg{display:grid;grid-template-columns:repeat(7,1fr);gap:4px;text-align:center;}
.ch{font-size:9px;font-weight:800;color:#9aa5b4;padding:6px 0;text-transform:uppercase;}
.cd{background:#d1fae5;border-radius:8px;padding:6px 2px;border:1px solid #a7f3d0;line-height:1.5;}
.cd.v{background:#f7f9fb;border-color:#e2e8f0;}
.cd.e{background:transparent;border:none;}
.cd-n{font-size:11px;font-weight:800;color:#0f1c2e;}
.cd-v{font-size:9px;color:#059669;font-weight:700;}
.cd.v .cd-n{color:#9aa5b4;} .cd.v .cd-v{color:#c8d4de;}

/* ══ FINANCEIRO ═════════════════════════════════════════════ */
.fr{display:flex;justify-content:space-between;padding:9px 0;border-bottom:1px solid #f3f6f9;font-size:13px;}
.fr:last-child{border:none;}
.fl{color:#374151;} .fv{font-weight:700;} .fv.r{color:#e03e3e;} .fv.g{color:#00b388;font-size:15px;}
.ftotal{display:flex;justify-content:space-between;padding:12px 0 4px;font-weight:800;font-size:14px;border-top:2px solid #e5e9ef;margin-top:4px;}

/* ══ RANKING ════════════════════════════════════════════════ */
.ri{display:flex;align-items:center;gap:10px;padding:8px 0;border-bottom:1px solid #f3f6f9;}
.ri:last-child{border:none;}
.rn{width:22px;height:22px;background:#f0f4f8;border-radius:50%;display:flex;align-items:center;justify-content:center;font-size:10px;font-weight:800;color:#6b7280;flex-shrink:0;}
.rname{font-size:12px;font-weight:700;min-width:80px;flex:1;}
.rbw{flex:2;} .rb{height:9px;border-radius:3px;transition:width .4s ease;}
.b1{background:#0052cc;} .b2{background:#2684ff;} .b3{background:#79b8ff;} .b4{background:#bcd6f8;}
.rval{font-size:12px;font-weight:700;white-space:nowrap;}

/* ══ ACUMULADO ══════════════════════════════════════════════ */
.acg{display:grid;grid-template-columns:1fr 1fr;gap:14px;margin-bottom:8px;}
.al{font-size:9px;font-weight:800;color:#6b7280;letter-spacing:.9px;text-transform:uppercase;margin-bottom:3px;}
.av{font-size:22px;font-weight:800;color:#0f1c2e;} .av.g{color:#00b388;}
.ad{font-size:11px;color:#00b388;font-weight:700;}

/* ══ ANÁLISE DE DIÁRIA — intervalo entre check-ins ══════════ */
.diaria-row{display:flex;align-items:center;padding:10px 12px;border-radius:8px;background:#fafbfc;border:1px solid #e5e9ef;margin-bottom:6px;gap:12px;transition:background .1s;}
.diaria-row:hover{background:#f0f7ff;}
.diaria-datas{font-size:11px;color:#6b7280;min-width:140px;}
.diaria-datas b{color:#0f1c2e;font-size:12px;}
.diaria-noites{font-size:11px;color:#6b7280;min-width:60px;}
.diaria-val{font-size:16px;font-weight:800;color:#1a6ef7;min-width:90px;text-align:right;}
.diaria-bar-wrap{flex:1;}
.diaria-bar{height:8px;border-radius:4px;background:#1a6ef7;opacity:.7;}
.diaria-badge{font-size:9px;font-weight:700;padding:2px 8px;border-radius:10px;}
.db-alto{background:#fef3c7;color:#d97706;}
.db-medio{background:#eff6ff;color:#2563eb;}
.db-baixo{background:#f3f4f6;color:#6b7280;}

/* ══ TABELAS OPERACIONAIS ═══════════════════════════════════ */
.tab-wrap{overflow-x:auto;}
.cat-pill{display:inline-block;padding:2px 9px;border-radius:10px;font-size:10px;font-weight:700;}
.cat-energia{background:#fef9c3;color:#854d0e;}
.cat-agua{background:#dbeafe;color:#1d4ed8;}
.cat-condominio{background:#f3f4f6;color:#374151;}
.cat-internet{background:#fdf4ff;color:#7c3aed;}
.cat-limpeza{background:#d1fae5;color:#065f46;}
.cat-manutencao{background:#fff7ed;color:#c2410c;}
.cat-reposicao{background:#fce7f3;color:#9d174d;}
.cat-outros{background:#f3f4f6;color:#6b7280;}

/* status OS */
.os-status{display:inline-block;padding:2px 8px;border-radius:8px;font-size:10px;font-weight:700;}
.os-concluida{background:#d1fae5;color:#065f46;}
.os-pendente{background:#fef3c7;color:#854d0e;}
.os-em-andamento{background:#dbeafe;color:#1d4ed8;}

/* ══ EMPTY / ERROR ══════════════════════════════════════════ */
.empty-state{text-align:center;padding:80px 20px;color:#6b7280;}
.empty-state h3{font-size:20px;color:#374151;margin-bottom:8px;}
.empty-state p{font-size:14px;max-width:440px;margin:0 auto;}
.erro-dados{background:#fff7ed;border:1px solid #fdba74;color:#9a3412;padding:14px 16px;border-radius:10px;margin-bottom:16px;font-size:13px;}
.sem-dados{color:#9aa5b4;font-size:13px;padding:20px 0;text-align:center;}

/* ══ TABS (seções operacionais) ════════════════════════════ */
.op-tabs{display:flex;gap:4px;margin-bottom:16px;background:#f3f4f6;border-radius:10px;padding:4px;}
.op-tab{flex:1;text-align:center;padding:8px 12px;border-radius:7px;font-size:12px;font-weight:700;color:#6b7280;cursor:pointer;border:none;background:none;font-family:'Inter',sans-serif;transition:all .15s;}
.op-tab.active{background:#fff;color:#0f1c2e;box-shadow:0 1px 4px rgba(0,0,0,.1);}
.op-tab:hover:not(.active){background:rgba(255,255,255,.5);}

/* ══ DT OVERRIDES ════════════════════════════════════════════ */
.dataTables_wrapper .dataTables_filter input{border:1px solid #e2e8f0;border-radius:6px;padding:4px 10px;font-size:12px;}
.dataTables_wrapper .dataTables_info,.dataTables_wrapper .dataTables_paginate{font-size:12px;color:#6b7280;}
table.dataTable thead th{font-size:11px;font-weight:700;color:#6b7280;letter-spacing:.5px;text-transform:uppercase;}
table.dataTable tbody td{font-size:12px;}

/* ══ SHINY OVERRIDES ════════════════════════════════════════ */
.form-group{margin-bottom:0!important;}
label{font-size:11px!important;font-weight:700!important;color:#6b7280!important;}
.shiny-spinner-output-container{min-height:60px;}

/* ══ RECV / DET WRAPS ════════════════════════════════════════ */
.recv-wrap,.det-wrap{background:#fff;border-radius:12px;padding:20px;border:1px solid #e5e9ef;box-shadow:0 1px 5px rgba(0,0,0,.04);margin-bottom:14px;}
.recv-hdr,.det-hdr{display:flex;justify-content:space-between;align-items:center;margin-bottom:16px;}
.recv-ttl,.det-ttl{font-size:14px;font-weight:700;color:#1e2d3d;}
    "))
  ),
  
  # ── Header ──────────────────────────────────────────────────
  div(class = "hdr",
      div(class = "hdr-left",
          tags$img(
            src   = "assets/marca_BSB_STAY_RS_10.jpg",
            alt   = "BSB Stay",
            class = "hdr-logo-img"
          ),
          div(div(class = "hdr-title", "Extrato do Proprietário"),
              div(class = "hdr-sub",   "Painel de acompanhamento de resultados"))
      ),
      uiOutput("hdr_prop"),
      # Botão gera xlsx + link aparece ao lado
      div(style="display:flex;align-items:center;gap:6px;",
          uiOutput("dl_link_wrap"),
          tags$button(
            id      = "btn_gerar_dl",
            type    = "button",
            class   = "btn-dl",
            onclick = "Shiny.setInputValue('btn_gerar_dl', Math.random(), {priority:'event'}); return false;",
            tags$svg(xmlns="http://www.w3.org/2000/svg", width="13", height="13",
                     viewBox="0 0 24 24", fill="none", stroke="currentColor",
                     `stroke-width`="2.5", `stroke-linecap`="round", `stroke-linejoin`="round",
                     tags$polyline(points="8 17 12 21 16 17"),
                     tags$line(x1="12", y1="12", x2="12", y2="21"),
                     tags$path(d="M20.88 18.09A5 5 0 0 0 18 9h-1.26A8 8 0 1 0 3 16.29")),
            "Baixar Planilha"
          )
      )
  ),
  uiOutput("sync_bar"),
  
  uiOutput("filter_bar"),
  
  div(class = "content",
      uiOutput("alerta_erro"),
      uiOutput("body")
  )
)

# ═══════════════════════════════════════════════════════════════
# SERVER
# ═══════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    # APP_DATA_GLOBAL já está pronto (Etapa A do run.R carregou do SQLite).
    # Se ainda estiver vazio (fresh deploy sem SQLite), o polling de 3s
    # detecta a conclusão da Etapa B e atualiza rv$app_data automaticamente.
    app_data    = if (exists("APP_DATA_GLOBAL") && length(APP_DATA_GLOBAL) > 0) {
      APP_DATA_GLOBAL
    } else if (exists("APP_DATA") && length(APP_DATA) > 0) {
      APP_DATA
    } else {
      structure(list(), erro_msg = "Dados ainda carregando, aguarde...")
    },
    syncing     = FALSE,
    sync_status = "ok",
    last_sync   = {
      st <- tryCatch(status_cache(), error = function(e) list(last_sync = NA))
      st$last_sync
    },
    op_aba = "despesas"
  )
  
  observeEvent(input$btn_aba_op, { rv$op_aba <- input$btn_aba_op }, ignoreInit = TRUE)
  
  # ── Reactives base ──────────────────────────────────────────
  dados <- reactive({
    req(identical(session$userData$auth_role, "owner"))
    doc   <- as.character(session$userData$auth_doc)
    chaves <- gsub("[^0-9]", "", names(rv$app_data))
    idx    <- which(chaves == gsub("[^0-9]", "", doc))
    req(length(idx) > 0)
    rv$app_data[[idx[1]]]
  })
  
  meses_disponiveis <- reactive({
    d <- dados(); req(d)
    if (is.null(d$receitas) || nrow(d$receitas) == 0) return(character(0))
    sort(unique(d$receitas$competencia[!is.na(d$receitas$competencia)]), decreasing = TRUE)
  })
  
  rec_fil <- reactive({
    d <- dados(); req(d)
    df <- d$receitas
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- df |> dplyr::filter(imovel == input$imovel)
    df
  })
  
  rm <- reactive({
    req(input$mes_sel)
    df_mes <- rec_fil() |> dplyr::filter(competencia == input$mes_sel)
    n_im <- dplyr::n_distinct(df_mes$imovel)
    out  <- df_mes |>
      dplyr::summarise(
        receita_bruta = sum(receita_bruta, na.rm = TRUE),
        taxa_adm      = sum(taxa_adm,      na.rm = TRUE),
        outros_custos = sum(outros_custos, na.rm = TRUE),
        resultado_liq = sum(resultado_liq, na.rm = TRUE),
        ocupacao      = mean(ocupacao,     na.rm = TRUE),
        diaria_media  = mean(diaria_media, na.rm = TRUE),
        n_diarias     = sum(n_diarias,     na.rm = TRUE),
        .groups = "drop"
      )
    out$n_imoveis <- n_im
    # RevPAR correto = Receita Total / Nº de dias do mês
    dias_mes <- tryCatch(
      as.integer(lubridate::days_in_month(as.Date(paste0(input$mes_sel, "-01")))),
      error = function(e) NA_integer_
    )
    out$dias_mes <- dias_mes
    out$revpar   <- if (!is.na(dias_mes) && dias_mes > 0) out$receita_bruta / dias_mes else NA_real_
    out
  })
  
  # Filtra reservas por mês e imóvel
  reservas_fil <- reactive({
    d <- dados(); req(d, input$mes_sel)
    if (is.null(d$reservas) || nrow(d$reservas) == 0) return(data.frame())
    df <- d$reservas |> dplyr::filter(competencia == input$mes_sel)
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- df |> dplyr::filter(imovel_nome == input$imovel)
    df |>
      dplyr::arrange(checkin)
  })

  # Filtra manutenção
  manutencao_fil <- reactive({
    d <- dados(); req(d, input$mes_sel)
    if (is.null(d$manutencao) || nrow(d$manutencao) == 0) return(data.frame())
    df <- d$manutencao |> dplyr::filter(competencia == input$mes_sel)
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- df |> dplyr::filter(imovel_nome == input$imovel)
    df
  })
  
  # Filtra despesas
  despesas_fil <- reactive({
    d <- dados(); req(d, input$mes_sel)
    if (is.null(d$despesas) || nrow(d$despesas) == 0) return(data.frame())
    df <- d$despesas |> dplyr::filter(competencia == input$mes_sel)
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- df |> dplyr::filter(imovel_nome == input$imovel)
    df
  })
  
  # Filtra reposição
  reposicao_fil <- reactive({
    d <- dados(); req(d, input$mes_sel)
    if (is.null(d$reposicao) || nrow(d$reposicao) == 0) return(data.frame())
    df <- d$reposicao |> dplyr::filter(competencia == input$mes_sel)
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- df |> dplyr::filter(imovel_nome == input$imovel)
    df
  })
  
  # ── Sync ──────────────────────────────────────────────────────
  
  # ── Sync ──────────────────────────────────────────────────────
  output$sync_bar <- renderUI({
    dot_class <- paste("sync-dot", rv$sync_status)
    msg_txt <- if (rv$syncing) "\u23f3 Sincronizando..." else if (!is.na(rv$last_sync))
      paste0("\u2713 \u00daltima sincroniza\u00e7\u00e3o: ", rv$last_sync) else "\u26a0 Dados n\u00e3o sincronizados"
    div(class = "sync-bar",
        div(class = dot_class), span(msg_txt),
        tags$button(type = "button", class = "sync-btn",
                    onclick = "Shiny.setInputValue(\'btn_sync\', Math.random()); return false;",
                    if (rv$syncing) "\u23f3 Aguarde..." else "\u21bb Atualizar dados"))
  })
  
  # Polling: sincroniza rv$app_data com APP_DATA_GLOBAL
  # Detecta mudanças via ts_refresh (gravado pelo auto_refresh_ em run.R),
  # cobrindo tanto a Etapa B inicial quanto os refreshes periódicos automáticos.
  observe({
    invalidateLater(2000, session)
    if (exists("APP_DATA_GLOBAL") && is.list(APP_DATA_GLOBAL) && length(APP_DATA_GLOBAL) > 0) {
      ts_global <- APP_DATA_GLOBAL$ts_refresh %||% 0
      ts_local  <- if (is.list(rv$app_data)) rv$app_data$ts_refresh %||% -1 else -1
      if (length(rv$app_data) == 0 || !identical(ts_global, ts_local)) {
        rv$app_data  <- APP_DATA_GLOBAL
        rv$last_sync <- format(APP_DATA_GLOBAL$ts_refresh %||% Sys.time(), "%d/%m/%Y %H:%M")
      }
    }
  })
  
  observeEvent(input$btn_sync, {
    rv$syncing <- TRUE
    tryCatch({
      nd <- carregar_dados_app(folder_id = DRIVE_FOLDER_ID, forcar_dl = TRUE, forcar_etl = TRUE)
      rv$app_data  <- nd; rv$last_sync <- format(Sys.time(), "%d/%m/%Y %H:%M")
      rv$sync_status <- "ok"
      showNotification("✓ Dados atualizados!", type = "message", duration = 4)
    }, error = function(e) {
      rv$sync_status <- "err"
      showNotification(paste("⚠ Falha:", e$message), type = "error", duration = 6)
    })
    rv$syncing <- FALSE
  }, ignoreInit = TRUE)
  
  # ── Header prop ───────────────────────────────────────────────
  
  # ── Header ────────────────────────────────────────────────────
  output$hdr_prop <- renderUI({
    d <- dados(); req(d)
    nome <- session$userData$auth_owner_name %||% d$proprietario %||% "Proprietário"
    tipo <- session$userData$auth_doc_type   %||% "CPF/CNPJ"
    div(style = "display:flex;align-items:center;gap:10px;",
        div(class = "hdr-prop", tags$b(nome), br(), div(class = "hdr-badge", tipo)),
        tags$button(
          type = "button",
          style = paste0("background:rgba(255,255,255,.08);border:1px solid rgba(255,255,255,.18);",
                         "color:#c9dff2;border-radius:8px;padding:6px 12px;font-size:11px;",
                         "font-weight:700;cursor:pointer;font-family:'Inter',sans-serif;",
                         "white-space:nowrap;transition:background .15s;"),
          onclick = "Shiny.setInputValue('nav_alterar_senha', Math.random()); return false;",
          "Alterar Senha")
    )
  })
  
  # ── Filter bar ────────────────────────────────────────────────
  output$filter_bar <- renderUI({
    d <- dados(); req(d, length(meses_disponiveis()) > 0)
    meses <- meses_disponiveis()
    meses_lbl <- setNames(meses, {
      datas <- suppressWarnings(as.Date(paste0(meses, "-01")))
      ifelse(is.na(datas), meses, fmt_mes_pt(datas))
    })
    imoveis <- c("Todos os im\u00f3veis" = "all", setNames(d$imoveis_ids, d$imoveis_ids))
    div(class = "fbar",
        div(class = "fbar-lbl", "M\u00cAS:"),
        selectInput("mes_sel", NULL, choices = meses_lbl, selected = meses[1], width = "180px"),
        div(class = "fbar-lbl", "IM\u00d3VEL:"),
        selectInput("imovel",  NULL, choices = imoveis,   selected = "all",    width = "260px"))
  })
  
  output$alerta_erro <- renderUI({
    msg <- attr(rv$app_data, "erro_msg")
    if (!is.null(msg)) div(class = "erro-dados", tags$b("\u26a0 Aten\u00e7\u00e3o: "), msg)
  })
  
  output$body <- renderUI({
    d <- dados()
    if (is.null(d)) {
      return(div(class = "empty-state",
                 h3("⏳ Carregando dados..."),
                 p("Aguarde enquanto os dados são preparados.")))
    }
    req(input$mes_sel)
    m <- rm()
    mes_label_sel <- {
      dt <- suppressWarnings(as.Date(paste0(input$mes_sel, "-01")))
      if (!is.na(dt)) fmt_mes_pt(dt) else input$mes_sel
    }
    mes_badge_sm <- {
      dt <- suppressWarnings(as.Date(paste0(input$mes_sel, "-01")))
      if (!is.na(dt)) fmt_mes_pt(dt, abreviado = TRUE) else input$mes_sel
    }
    
    tagList(
      # ════════════════════════════════════════
      # 1. RESULTADOS DO MÊS — KPIs principais
      # ════════════════════════════════════════
      div(class = "sec", "RESULTADOS DO MÊS"),
      div(class = "kgrid",
          # Resultado Líquido — card principal em destaque
          kcard("Resultado Líquido", brl(m$resultado_liq),
                paste0("receita: ", brl(m$receita_bruta)),
                vg = TRUE, extra_class = "hero"),
          kcard("Receita Bruta",  brl(m$receita_bruta),  "receita do período"),
          kcard("RevPAR",
                brl(if (!is.na(m$revpar)) m$revpar else 0),
                paste0(if (!is.na(m$dias_mes)) m$dias_mes else "—", " dias no mês"), icon = "📈"),
          # PUB-6: % dinâmica da taxa adm
          kcard("Taxa Adm.", brl(m$taxa_adm), {
            if (!is.na(m$receita_bruta) && m$receita_bruta > 0)
              paste0(round(m$taxa_adm / m$receita_bruta * 100), "% da receita")
            else "— da receita"
          }, dn = TRUE),
          # PUB-4 / PUB-5: Outros Custos sem "fixa + variável", clicável para descer ao Operacional
          tags$div(
            onclick = "document.getElementById('sec_operacional').scrollIntoView({behavior:'smooth'}); return false;",
            style = "cursor:pointer;",
            kcard("Outros Custos", brl(m$outros_custos), "", dn = TRUE)
          ),
          kcard("Ocupação", paste0(round(m$ocupacao), "%"),
                paste0("Diária média: ", brl(m$diaria_media))),
          # PUB-3: Noites Reservadas
          kcard("Noites Reservadas", as.character(m$n_diarias), "noites no período")
      ),
      
      # ════════════════════════════════════════
      # ════════════════════════════════════════
      # 2. EVOLUÇÃO 12 MESES + ACUMULADO (PUB-7/PUB-8/PUB-10)
      # Gráfico de evolução no lugar do antigo gráfico diária/dia
      # ════════════════════════════════════════
      div(class = "cgrid",
          div(class = "card",
              div(class = "card-hdr",
                  div(class = "card-ttl", "Evolução 12 Meses"),
                  span(class = "badge", "Receita + Resultado")),
              shinycssloaders::withSpinner(plotlyOutput("g_evolucao", height = "220px"), type = 4, color = "#00c49a")
          ),
          div(class = "card",
              div(class = "card-hdr",
                  div(class = "card-ttl", "Acumulado dos Últimos 12 Meses"),
                  span(class = "badge", "Base consolidada")),
              uiOutput("acumulado"))
      ),
      
      # ════════════════════════════════════════
      # 3. DETALHAMENTO DO MÊS — KPIs diária + calendário + reservas
      # ════════════════════════════════════════
      uiOutput("sec_detalhamento_mes"),
      
      # Resultado + Custos (Ranking movido para Visão Geral da Carteira)
      div(class = "card",
          div(class = "card-hdr",
              div(class = "card-ttl", "Resultado Financeiro"),
              span(class = "badge", "Após deduções")),
          uiOutput("resultado"),
          div(style = "margin-top:16px;",
              div(class = "card-hdr",
                  div(class = "card-ttl", "Custos do Mês"),
                  span(class = "badge", "Discriminado")),
              shinycssloaders::withSpinner(DTOutput("t_custos"), type = 4, color = "#00c49a"))
      ),
      
      # ════════════════════════════════════════
      # 4. OPERACIONAL: Despesas | Custos | OS | Reposição
      # ════════════════════════════════════════
      div(class = "sec", id = "sec_operacional", "OPERACIONAL"),
      
      div(class = "card",
          # SCROLL FIX: abas sem rv$op_aba no output$body.
          # A classe "active" é gerenciada inteiramente via JS (setOpTab),
          # sem re-renderizar o body. O Shiny.setInputValue dispara apenas
          # o painel_operacional (renderUI próprio), não o body inteiro.
          div(class = "op-tabs", id = "op-tabs-wrap",
              tags$button(
                type = "button", id = "optab-despesas",
                class = "op-tab active",
                onclick = "setOpTab('despesas')",
                "💰 Despesas"
              ),
              tags$button(
                type = "button", id = "optab-custos",
                class = "op-tab",
                onclick = "setOpTab('custos')",
                "🏠 Contas do Apartamento"
              ),
              tags$button(
                type = "button", id = "optab-os",
                class = "op-tab",
                onclick = "setOpTab('os')",
                "🔧 Manutenções"
              ),
              tags$button(
                type = "button", id = "optab-reposicao",
                class = "op-tab",
                onclick = "setOpTab('reposicao')",
                "📦 Reposição"
              )
          ),
          uiOutput("painel_operacional")
      ),
      
      # ════════════════════════════════════════
      # 5. VISÃO GERAL DA CARTEIRA + PORTFÓLIO (PUB-2)
      # ════════════════════════════════════════
      div(class = "sec", "VISÃO GERAL DA CARTEIRA"),
      
      # Ranking + Portfólio lado a lado
      div(class = "cgrid",
          div(class = "card",
              div(class = "card-hdr",
                  div(class = "card-ttl", "Ranking de Imóveis"),
                  span(class = "badge", "Resultado do mês")),
              uiOutput("ranking")),
          div(class = "card",
              div(class = "card-hdr",
                  div(class = "card-ttl", "Portfólio de Imóveis"),
                  span(class = "badge", paste0(length(d$imoveis_cfg %||% list()), " imóveis"))),
              {
                cfg_lst <- d$imoveis_cfg
                if (length(cfg_lst) == 0) p("Nenhum imóvel cadastrado.")
                else div(class = "imovel-grid",
                         lapply(cfg_lst, function(im) {
                           plats <- strsplit(as.character(im$plataformas %||% ""), "/|,| ")[[1]]
                           plats <- trimws(plats[nzchar(trimws(plats))])
                           plat_tags <- lapply(plats, function(p) {
                             cl <- switch(tolower(p),
                                          "airbnb"="plat-tag p-air","booking"=,"bookingcom"="plat-tag p-bk",
                                          "decolar"="plat-tag p-dc","vrbo"="plat-tag p-vr",
                                          "expedia"="plat-tag p-ex","plat-tag p-out")
                             div(class = cl, p)
                           })
                           div(class = "icard",
                               div(class = "icard-nome", as.character(im$nome %||% im$id)),
                               div(class = "icard-end",  as.character(im$bairro %||% "")),
                               div(class = "icard-tipo", as.character(im$tipo %||% "")),
                               div(class = "icard-plat", !!!plat_tags))
                         }))
              })
      ),
      
      
      
      div(class = "sec", "HISTÓRICO DETALHADO"),
      div(class = "card",
          shinycssloaders::withSpinner(DTOutput("t_historico"), type = 4, color = "#00c49a"))
    )
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Gráfico diária por dia (calendário)
  # ═══════════════════════════════════════════════════════════
  # PUB-7: gráfico diária/dia removido
  output$g_diaria_dia <- renderPlotly({ plotly_empty() |> config(displayModeBar=FALSE) })
  
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Calendário de ocupação v2
  # ═══════════════════════════════════════════════════════════
  output$calendario_v2 <- renderUI({
    d <- dados(); req(d, input$mes_sel)
    iid <- if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      input$imovel else if (length(d$imoveis_ids) > 0) d$imoveis_ids[[1]] else return(p("Sem dados."))
    cal <- d$calendario
    if (is.null(cal) || nrow(cal) == 0) return(p(class = "sem-dados", "Sem dados de ocupação."))
    cal <- cal |>
      dplyr::filter(apto_original == iid | property_id == iid,
                    substr(as.character(data), 1, 7) == input$mes_sel) |>
      dplyr::arrange(data)
    if (nrow(cal) == 0) return(p(class = "sem-dados", "Sem dados para o período."))
    mes_inicio <- as.Date(paste0(input$mes_sel, "-01"))
    mes_fim    <- lubridate::ceiling_date(mes_inicio, "month") - 1
    cal_full   <- data.frame(data = seq.Date(mes_inicio, mes_fim, by = "day")) |>
      dplyr::left_join(cal |> dplyr::select(data, valor, ocupado), by = "data") |>
      dplyr::mutate(ocupado = dplyr::coalesce(ocupado, FALSE), valor = dplyr::coalesce(valor, 0))
    hdrs   <- lapply(c("DOM","SEG","TER","QUA","QUI","SEX","SÁB"), function(x) div(class="ch", x))
    prm    <- as.integer(format(mes_inicio, "%w"))
    vazios <- if (prm > 0) lapply(seq_len(prm), function(i) div(class="cd e")) else list()
    dias   <- lapply(seq_len(nrow(cal_full)), function(i) {
      r <- cal_full[i,]
      div(class = if (isTRUE(r$ocupado)) "cd" else "cd v",
          div(class="cd-n", as.integer(format(as.Date(r$data),"%d"))),
          div(class="cd-v", if (r$valor>0) paste0("R$",format(round(r$valor),big.mark=".")) else "—"))
    })
    div(class="cg", !!!c(hdrs, vazios, dias))
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Seção Análise de Receita (condicional — oculta se sem dados)
  # ═══════════════════════════════════════════════════════════
  # PUB-7: seção removida — gráfico diária/dia excluído
  output$sec_analise_receita <- renderUI({ NULL })
  
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Seção Detalhamento do Mês (condicional — oculta se sem dados)
  # ═══════════════════════════════════════════════════════════
  output$sec_detalhamento_mes <- renderUI({
    d <- dados(); req(d, input$mes_sel)
    iid <- if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      input$imovel else if (length(d$imoveis_ids) > 0) d$imoveis_ids[[1]] else return(NULL)
    cal <- d$calendario
    if (is.null(cal) || nrow(cal) == 0) return(NULL)
    cal_fil <- cal |>
      dplyr::filter(apto_original == iid | property_id == iid,
                    substr(as.character(data), 1, 7) == input$mes_sel)
    if (nrow(cal_fil) == 0) return(NULL)
    mes_badge_sm <- {
      dt <- suppressWarnings(as.Date(paste0(input$mes_sel, "-01")))
      if (!is.na(dt)) fmt_mes_pt(dt, abreviado = TRUE) else input$mes_sel
    }
    
    # PUB-11: KPIs de diária calculados das reservas filtradas
    resv_det <- if (!is.null(d$reservas) && nrow(d$reservas) > 0) {
      df_r <- d$reservas |> dplyr::filter(competencia == input$mes_sel)
      if (nzchar(iid) && iid != "all")
        df_r <- df_r |> dplyr::filter(imovel_nome == iid | property_id == iid)
      df_r
    } else data.frame()
    
    kpis_diaria_ui <- if (nrow(resv_det) > 0) {
      d_med <- mean(resv_det$diaria_liquida, na.rm = TRUE)
      d_max_v <- max(resv_det$diaria_liquida, na.rm = TRUE)
      vals_pos <- resv_det$diaria_liquida[!is.na(resv_det$diaria_liquida) & resv_det$diaria_liquida > 0]
      d_min_v <- if (length(vals_pos) > 0) min(vals_pos) else NA_real_
      div(class = "kgrid-sm kgrid-sm-3", style = "margin-bottom:14px;",
          kcard_sm("Diária Média",  brl(d_med),   "blue"),
          kcard_sm("Maior Diária",  brl(d_max_v), "green"),
          kcard_sm("Menor Diária",  brl(d_min_v), "orange"))
    } else NULL
    
    tagList(
      div(class = "sec", "DETALHAMENTO DO MÊS"),
      
      # PUB-11: KPIs diária antes do calendário
      kpis_diaria_ui,
      
      div(class = "det-wrap",
          div(class = "det-hdr",
              div(class = "det-ttl", "Calendário de Ocupação"),
              span(class = "badge badge-green", mes_badge_sm)
          ),
          shinycssloaders::withSpinner(uiOutput("calendario_v2"), type = 4, color = "#00c49a")
      ),
      
      # PUB-12: Relatório de Reservas abaixo do calendário
      div(class = "card", style = "margin-top:14px;",
          div(class = "card-hdr",
              div(class = "card-ttl", "Relatório de Reservas"),
              span(class = "badge badge-blue", mes_badge_sm)),
          div(class = "tab-wrap",
              shinycssloaders::withSpinner(DTOutput("t_relatorio_reservas"), type = 4, color = "#1a6ef7"))
      )
    )
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Relatório de Reservas (PUB-12)
  # ═══════════════════════════════════════════════════════════
  output$t_relatorio_reservas <- renderDT({
    d <- dados(); req(d, input$mes_sel)
    resv <- if (!is.null(d$reservas) && nrow(d$reservas) > 0)
      d$reservas |> dplyr::filter(competencia == input$mes_sel)
    else return(datatable(data.frame(), options = list(dom = "t"), rownames = FALSE))
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      resv <- resv |> dplyr::filter(imovel_nome == input$imovel | property_id == input$imovel)
    resv <- resv |>
      dplyr::arrange(checkin)
    cols_ok <- names(resv)
    df <- resv |> dplyr::transmute(
      `Hóspede`        = if ("hospede"            %in% cols_ok) as.character(hospede)            else "—",
      `Check-in`       = format(as.Date(checkin),  "%d/%m/%Y"),
      `Check-out`      = format(as.Date(checkout), "%d/%m/%Y"),
      `Canal`          = if ("canal"              %in% cols_ok) as.character(canal)              else "—",
      `Noites`         = as.integer(dplyr::coalesce(as.numeric(noites_total), 0)),
      `Adultos`        = if ("adultos"            %in% cols_ok) as.integer(dplyr::coalesce(as.numeric(adultos), 0))  else NA_integer_,
      `Crianças`       = if ("criancas"           %in% cols_ok) as.integer(dplyr::coalesce(as.numeric(criancas), 0)) else NA_integer_,
      `Valor Recebido` = brl(if ("valor_total_reserva" %in% cols_ok) valor_total_reserva else receita_total),
      `Taxa Limpeza`   = brl(if ("taxa_limpeza"        %in% cols_ok) taxa_limpeza        else NA_real_),
      `Comissão Canal` = brl(if ("comissao_canal"      %in% cols_ok) comissao_canal      else NA_real_),
      `Diária Final`   = brl(diaria_liquida)
    )
    datatable(df,
              options = list(dom = "t", paging = FALSE, ordering = TRUE,
                             language = list(emptyTable = "Sem reservas no período")),
              rownames = FALSE, class = "compact stripe")
  }, server = FALSE)

  # ═══════════════════════════════════════════════════════════
  # OUTPUT: KPIs da Diária (MANTIDO internamente, não exibido na seção removida)
  # PUB-14: seção ANÁLISE DA DIÁRIA ENTRE CHECK-INS foi removida do layout
  # Os outputs abaixo são preservados para evitar erros de referência
  # ═══════════════════════════════════════════════════════════
  output$kpis_diaria <- renderUI({ NULL })
  
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Gráfico de diária por reserva (scatter/line)
  # Lógica: cada ponto = 1 reserva; x = checkin; y = diária_liquida
  # Permite ver como a diária varia de reserva para reserva no mês
  # ═══════════════════════════════════════════════════════════
  # PUB-14: seção ANÁLISE DA DIÁRIA ENTRE CHECK-INS removida
  output$g_diaria_reservas <- renderPlotly({ plotly_empty() |> config(displayModeBar=FALSE) })
  
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Lista visual de intervalos entre check-ins
  # Mostra cada reserva como um card horizontal com barra de valor
  # ═══════════════════════════════════════════════════════════
  output$lista_diarias <- renderUI({ NULL })
  
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Painel operacional (tabs: despesas | custos | OS)
  # ═══════════════════════════════════════════════════════════
  output$painel_operacional <- renderUI({
    aba <- rv$op_aba %||% "despesas"
    
    if (aba == "despesas") {
      # ── ABA: Despesas ──────────────────────────────────────
      # Fix: gráfico "Despesas por Apartamento" removido (substituído por pizza full-width)
      # Fix: tabela unificada inclui Manutenção + Reposição
      tagList(
        shinycssloaders::withSpinner(uiOutput("kpis_despesas"), type = 4, color = "#d97706"),
        div(class = "card",
            div(class = "card-hdr",
                div(class = "card-ttl", "Despesas por Categoria"),
                span(class = "badge badge-orange", "Distribuição")),
            shinycssloaders::withSpinner(
              plotlyOutput("g_despesas_cat", height = "260px"), type = 4, color = "#d97706")
        ),
        div(class = "card", style = "margin-top:14px;",
            div(class = "card-hdr",
                div(class = "card-ttl", "Tabela de Despesas"),
                span(class = "badge badge-orange", "Inclui Manutenção e Reposição")),
            div(class = "tab-wrap",
                shinycssloaders::withSpinner(DTOutput("t_despesas"), type = 4, color = "#d97706"))
        )
      )
      
    } else if (aba == "custos") {
      # ── ABA: Contas do Apartamento (ex Custos por Apartamento) ─
      # Fix: renomeado; gráfico de evolução removido; tabela de contas como principal
      tagList(
        shinycssloaders::withSpinner(uiOutput("kpis_custos"), type = 4, color = "#7c3aed"),
        div(class = "card", style = "margin-top:0;",
            div(class = "card-hdr",
                div(class = "card-ttl", "Tabela de Contas"),
                span(class = "badge badge-purple", "Pagamentos do mês")),
            div(class = "tab-wrap",
                shinycssloaders::withSpinner(DTOutput("t_tabela_contas"), type = 4, color = "#7c3aed"))
        ),
        div(class = "card", style = "margin-top:14px;",
            div(class = "card-hdr",
                div(class = "card-ttl", "Contas Detalhadas por Apartamento"),
                span(class = "badge badge-purple", "Mês selecionado")),
            div(class = "tab-wrap",
                shinycssloaders::withSpinner(DTOutput("t_custos_apto"), type = 4, color = "#7c3aed"))
        )
      )
      
    } else if (aba == "reposicao") {
      # ── ABA: Reposição ─────────────────────────────────────
      tagList(
        shinycssloaders::withSpinner(uiOutput("kpis_reposicao"), type = 4, color = "#0891b2"),
        div(class = "card", style = "margin-top:0;",
            div(class = "card-hdr",
                div(class = "card-ttl", "Itens de Reposição"),
                span(class = "badge badge-teal", "Mês selecionado")),
            div(class = "tab-wrap",
                shinycssloaders::withSpinner(DTOutput("t_reposicao"), type = 4, color = "#0891b2"))
        )
      )
    } else {
      # ── ABA: Manutenções (ex Ordens de Serviço) ────────────
      # Fix: renomeado de "Ordens de Serviço" para "Manutenções"
      tagList(
        shinycssloaders::withSpinner(uiOutput("kpis_os"), type = 4, color = "#0052cc"),
        div(class = "card", style = "margin-top:0;",
            div(class = "card-hdr",
                div(class = "card-ttl", "Manutenções"),
                span(class = "badge badge-blue", "Mês selecionado")),
            div(class = "tab-wrap",
                shinycssloaders::withSpinner(DTOutput("t_os"), type = 4, color = "#0052cc"))
        )
      )
    }
  })
  
  
  # ── KPIs Despesas ────────────────────────────────────────────
  output$kpis_despesas <- renderUI({
    des <- despesas_fil()
    rep <- reposicao_fil()
    man <- manutencao_fil()
    total_des <- if (nrow(des) > 0) sum(des$valor,                   na.rm = TRUE) else 0
    total_rep <- if (nrow(rep) > 0) sum(rep$valor_unitario_ou_total, na.rm = TRUE) else 0
    total_man <- if (nrow(man) > 0) sum(man$valor_total,             na.rm = TRUE) else 0
    div(class = "kgrid-sm kgrid-sm-4",
        kcard_sm("Total Contas Imóvel", brl(total_des),                            "orange"),
        kcard_sm("Total Reposição",     brl(total_rep),                            "purple"),
        kcard_sm("Total Manutenção",    brl(total_man),                            "teal"),
        kcard_sm("Total de Despesas",   brl(total_des + total_rep + total_man),    "green")
    )
  })
  
  # ── Pizza inclui Manutenção + Reposição como categorias ─────
  output$g_despesas_cat <- renderPlotly({
    des <- despesas_fil(); man <- manutencao_fil(); rep <- reposicao_fil()
    partes <- list()
    if (nrow(des) > 0 && "categoria" %in% names(des))
      partes[["des"]] <- des |>
      dplyr::group_by(categoria) |>
      dplyr::summarise(total = sum(valor, na.rm = TRUE), .groups = "drop")
    if (nrow(man) > 0)
      partes[["man"]] <- data.frame(categoria="Manutenção",
                                    total=sum(man$valor_total, na.rm=TRUE))
    if (nrow(rep) > 0)
      partes[["rep"]] <- data.frame(categoria="Reposição",
                                    total=sum(rep$valor_unitario_ou_total, na.rm=TRUE))
    validate(need(length(partes) > 0, "Sem dados de despesas."))
    df <- dplyr::bind_rows(partes) |>
      dplyr::group_by(categoria) |>
      dplyr::summarise(total = sum(total, na.rm=TRUE), .groups="drop") |>
      dplyr::arrange(dplyr::desc(total))
    cores <- c("#0052cc","#2684ff","#d97706","#7c3aed","#e03e3e","#00b388","#f59e0b","#9ca3af")
    plot_ly(df, labels=~categoria, values=~total, type="pie",
            marker=list(colors=cores, line=list(color="#fff",width=2)),
            textinfo="percent", textposition="inside",
            hovertemplate="%{label}<br>R$ %{value:,.0f}<extra></extra>") |>
      layout(paper_bgcolor="transparent", plot_bgcolor="transparent",
             showlegend=TRUE,
             legend=list(font=list(size=10), orientation="v", x=1.02, y=0.5),
             margin=list(l=0,r=120,t=10,b=10), autosize=TRUE) |>
      config(displayModeBar=FALSE)
  })
  
  # ── Gráfico despesas por apartamento (barras) ─────────────────
  # g_despesas_apto REMOVIDO (substituído por pizza full-width na aba Despesas)
  
  # ── Tabela despesas unificada (inclui Manutenção + Reposição) ─
  output$t_despesas <- renderDT({
    des <- despesas_fil()
    man <- manutencao_fil()
    rep <- reposicao_fil()
    partes <- list()
    if (nrow(des) > 0) {
      cols_ok <- c("imovel_nome","categoria","descricao","data","competencia","valor")[
        c("imovel_nome","categoria","descricao","data","competencia","valor") %in% names(des)]
      partes[["des"]] <- des |>
        dplyr::select(dplyr::all_of(cols_ok)) |>
        dplyr::rename_with(~ dplyr::recode(.,
                                           imovel_nome="Imóvel", categoria="Categoria", descricao="Descrição",
                                           data="Data", competencia="Competência", valor="Valor (R$)"))
    }
    if (nrow(man) > 0) {
      partes[["man"]] <- data.frame(
        Imóvel       = as.character(man$imovel_nome %||% man$property_id %||% "—"),
        Categoria    = "Manutenção",
        Descrição    = if ("produto_servico" %in% names(man)) as.character(man$produto_servico) else "—",
        Data         = if ("data" %in% names(man)) as.character(man$data) else as.character(man$competencia),
        Competência  = as.character(man$competencia),
        `Valor (R$)` = as.numeric(man$valor_total),
        check.names = FALSE, stringsAsFactors = FALSE)
    }
    if (nrow(rep) > 0) {
      item_col <- if ("item_limpo" %in% names(rep)) rep$item_limpo
      else if ("item_raw" %in% names(rep)) rep$item_raw else "—"
      partes[["rep"]] <- data.frame(
        Imóvel       = as.character(rep$imovel_nome %||% rep$apto_original %||% "—"),
        Categoria    = "Reposição",
        Descrição    = as.character(item_col),
        Data         = as.character(rep$competencia),
        Competência  = as.character(rep$competencia),
        `Valor (R$)` = as.numeric(rep$valor_unitario_ou_total),
        check.names = FALSE, stringsAsFactors = FALSE)
    }
    validate(need(length(partes) > 0, "Sem despesas para o período/imóvel selecionado."))
    df <- dplyr::bind_rows(partes) |>
      dplyr::mutate(`Valor (R$)` = brl(as.numeric(`Valor (R$)`)))
    datatable(df,
              options = list(pageLength = 10, dom = "ftip",
                             language = list(search = "Buscar:", paginate = list(previous="Ant.", `next`="Próx."))),
              rownames = FALSE, class = "compact stripe hover", escape = FALSE)
  }, server = FALSE)
  
  # ── KPIs Contas do Apartamento (simplificado) ────────────────
  output$kpis_custos <- renderUI({
    d <- dados(); req(d, input$mes_sel)
    rec <- rec_fil() |> dplyr::filter(competencia == input$mes_sel)
    if (nrow(rec) == 0) return(p(class="sem-dados","Sem dados."))
    total_custo <- sum(rec$outros_custos, na.rm = TRUE)
    n_apto      <- length(unique(rec$imovel))
    custo_med   <- if (n_apto > 0) round(total_custo / n_apto, 2) else 0
    div(class = "kgrid-sm kgrid-sm-3",
        kcard_sm("Total Contas",     brl(total_custo),       "purple"),
        kcard_sm("Custo/Apto Médio", brl(custo_med),         "blue"),
        kcard_sm("Aptos com Custo",  as.character(n_apto),   "orange")
    )
  })
  
  # ── Tabela de Contas (substitui gráfico Evolução de Custos) ──
  # Proveniente da planilha Pagamentos > AAAA/MM (despesas_fil)
  output$t_tabela_contas <- renderDT({
    des <- despesas_fil()
    validate(need(nrow(des) > 0, "Sem contas registradas para o período/imóvel selecionado."))
    cols_ok <- c("imovel_nome","categoria","descricao","data","competencia","valor")[
      c("imovel_nome","categoria","descricao","data","competencia","valor") %in% names(des)]
    df <- des |>
      dplyr::select(dplyr::all_of(cols_ok)) |>
      dplyr::rename_with(~ dplyr::recode(.,
                                         imovel_nome="Imóvel", categoria="Categoria", descricao="Descrição",
                                         data="Data", competencia="Competência", valor="Valor (R$)")) |>
      dplyr::mutate(dplyr::across(dplyr::any_of("Valor (R$)"), ~ brl(.x)))
    datatable(df,
              options = list(pageLength = 10, dom = "ftip",
                             language = list(search = "Buscar:", paginate = list(previous="Ant.", `next`="Próx."))),
              rownames = FALSE, class = "compact stripe hover", escape = FALSE)
  }, server = FALSE)
  
  # ── Tabela custos por apartamento ────────────────────────────
  output$t_custos_apto <- renderDT({
    d <- dados(); req(d, input$mes_sel)
    rec <- rec_fil() |>
      dplyr::filter(competencia == input$mes_sel) |>
      dplyr::transmute(
        `Imóvel`          = imovel,
        `Receita Bruta`   = brl(receita_bruta),
        `Taxa Adm`        = paste0("- ", brl(taxa_adm)),
        `Manutenção`      = paste0("- ", brl(dplyr::coalesce(as.numeric(manutencao_total), 0))),
        `Reposição`       = paste0("- ", brl(dplyr::coalesce(as.numeric(reposicao_total), 0))),
        `Despesas`        = paste0("- ", brl(dplyr::coalesce(as.numeric(despesas_total), 0))),
        `Outros Custos`   = paste0("- ", brl(outros_custos)),
        `Resultado Líq`   = brl(resultado_liq),
        `% Custo/Receita` = paste0(round(ifelse(receita_bruta > 0, outros_custos/receita_bruta*100, 0)), "%")
      )
    validate(need(nrow(rec) > 0, "Sem dados para o período."))
    datatable(rec, rownames = FALSE, class = "compact stripe hover",
              options = list(dom = "ft", paging = FALSE, ordering = TRUE,
                             language = list(search = "Buscar:"))) |>
      formatStyle("% Custo/Receita",
                  background = styleColorBar(c(0, 100), "#fde68a"),
                  backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  }, server = FALSE)
  
  # ── KPIs Manutenções (simplificado) ──────────────────────────
  output$kpis_os <- renderUI({
    man <- manutencao_fil()
    if (nrow(man) == 0) return(p(class="sem-dados","Sem manutenções no período."))
    total_val <- sum(man$valor_total, na.rm = TRUE)
    n_os      <- nrow(man)
    div(class = "kgrid-sm kgrid-sm-2",
        kcard_sm("Qtde de OS",     as.character(n_os),  "blue"),
        kcard_sm("Custo Total OS", brl(total_val),       "red")
    )
  })
  
  # ── Tabela Ordens de Serviço ──────────────────────────────────
  output$t_os <- renderDT({
    man <- manutencao_fil()
    validate(need(nrow(man) > 0, "Sem ordens de serviço para o período/imóvel selecionado."))
    
    # Colunas simplificadas (removidos: Competência, Tipo Serviço, Descrição, Prestador)
    df <- man |>
      dplyr::transmute(
        `OS ID`           = if ("os_id"          %in% names(man)) as.character(os_id) else "—",
        `Imóvel`          = dplyr::coalesce(as.character(imovel_nome %||% property_id), "—"),
        `Data`            = if ("data" %in% names(man)) as.character(data) else as.character(competencia),
        `Produto/Serviço` = if ("produto_servico" %in% names(man)) as.character(produto_servico) else "—",
        `Status`          = if ("status"          %in% names(man)) as.character(status) else "Concluída",
        `Valor (R$)`      = brl(valor_total)
      )
    
    datatable(df, rownames = FALSE, class = "compact stripe hover", escape = FALSE,
              options = list(pageLength = 10, dom = "ftip",
                             language = list(search="Buscar:",
                                             paginate=list(previous="Ant.",`next`="Próx.")))) |>
      formatStyle("Status",
                  color = styleEqual(
                    c("Concluída","concluída","Pendente","pendente","Em andamento","em andamento"),
                    c("#065f46","#065f46","#854d0e","#854d0e","#1d4ed8","#1d4ed8")
                  ),
                  backgroundColor = styleEqual(
                    c("Concluída","concluída","Pendente","pendente","Em andamento","em andamento"),
                    c("#d1fae5","#d1fae5","#fef3c7","#fef3c7","#dbeafe","#dbeafe")
                  ))
  }, server = FALSE)
  
  # ── KPIs Reposição (simplificado) ────────────────────────────
  output$kpis_reposicao <- renderUI({
    rep <- reposicao_fil()
    if (nrow(rep) == 0) return(p(class = "sem-dados", "Sem itens de reposição para o período."))
    total_val <- sum(suppressWarnings(as.numeric(rep$valor_unitario_ou_total)), na.rm = TRUE)
    n_itens   <- nrow(rep)
    div(class = "kgrid-sm kgrid-sm-2",
        kcard_sm("Total Reposição", brl(total_val),        "teal"),
        kcard_sm("Nº de Itens",     as.character(n_itens), "purple")
    )
  })
  
  # ── Tabela Reposição ──────────────────────────────────────────
  output$t_reposicao <- renderDT({
    rep <- reposicao_fil()
    validate(need(nrow(rep) > 0, "Sem itens de reposição para o período/imóvel selecionado."))
    
    # Resolver coluna de imóvel
    apto_col <- if ("apto_original" %in% names(rep)) rep$apto_original
    else if ("imovel_nome" %in% names(rep)) rep$imovel_nome
    else if ("property_id" %in% names(rep)) rep$property_id
    else rep("—", nrow(rep))
    
    # Resolver coluna de item
    item_col <- if ("item_limpo" %in% names(rep)) rep$item_limpo
    else if ("item_raw" %in% names(rep)) rep$item_raw
    else rep("—", nrow(rep))
    
    # Quantidade e valores
    qtd_col   <- suppressWarnings(as.numeric(if ("quantidade" %in% names(rep)) rep$quantidade else NA))
    val_col   <- suppressWarnings(as.numeric(rep$valor_unitario_ou_total))
    val_total <- ifelse(is.na(qtd_col) | qtd_col <= 0, val_col, qtd_col * val_col)
    max_val   <- max(val_total, na.rm = TRUE)
    
    df <- data.frame(
      `Imóvel`      = as.character(apto_col),
      `Item`        = as.character(item_col),
      `Qtd.`        = ifelse(is.na(qtd_col), "—", as.character(round(qtd_col))),
      `Valor Unit.` = brl(val_col),
      `Valor Total` = brl(val_total),
      check.names      = FALSE,
      stringsAsFactors = FALSE
    )
    
    # Se imóvel filtrado, remover coluna redundante
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- df[, names(df) != "Imóvel", drop = FALSE]
    
    datatable(
      df,
      rownames = FALSE,
      class    = "compact stripe hover",
      options  = list(
        pageLength = 15,
        dom        = "ftip",
        order      = list(list(0, "asc"), list(1, "asc")),
        language   = list(
          search   = "Buscar:",
          info     = "Mostrando _START_ a _END_ de _TOTAL_ itens",
          paginate = list(previous = "Ant.", `next` = "Próx.")
        )
      )
    ) |>
      formatStyle(
        "Valor Total",
        background         = styleColorBar(c(0, if (is.finite(max_val) && max_val > 0) max_val * 1.1 else 1), "#c7f2ed"),
        backgroundSize     = "100% 75%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      )
  }, server = FALSE)
  
  
  # OUTPUT: Resultado financeiro
  # ═══════════════════════════════════════════════════════════
  output$resultado <- renderUI({
    m <- rm()
    dev_limp <- tryCatch({
      df_dev <- rec_fil() |> dplyr::filter(competencia == input$mes_sel)
      if ("devolucao_limpeza" %in% names(df_dev))
        sum(df_dev$devolucao_limpeza, na.rm = TRUE) else 0
    }, error = function(e) 0)
    div(
      frow("Receita Bruta",        brl(m$receita_bruta), FALSE),
      frow("Taxa Administrativa",  paste0("- ", brl(m$taxa_adm)),      TRUE),
      frow("Outros custos fixos",  paste0("- ", brl(m$outros_custos)), TRUE),
      if (dev_limp > 0) frow("Devolução Taxa de Limpeza", paste0("+ ", brl(dev_limp)), FALSE),
      div(class = "ftotal",
          span("RESULTADO LÍQUIDO"),
          span(class = "fv g", brl(m$resultado_liq)))
    )
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Tabela custos (summary do mês — seção financeiro)
  # ═══════════════════════════════════════════════════════════
  output$t_custos <- renderDT({
    d <- dados(); req(d, input$mes_sel)
    rec_m <- d$receitas |> dplyr::filter(competencia == input$mes_sel)
    # PUB-13: filtrar pelo imóvel selecionado antes de montar as linhas
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      rec_m <- rec_m |> dplyr::filter(imovel == input$imovel)
    linhas <- rec_m |>
      dplyr::transmute(`Imóvel` = imovel, Item = "Custos do mês", Valor = brl(outros_custos))
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      linhas <- dplyr::select(linhas, -`Imóvel`)
    datatable(linhas, options=list(dom="t",paging=FALSE,ordering=FALSE),
              rownames=FALSE, class="compact stripe")
  }, server = FALSE)
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Ranking
  # ═══════════════════════════════════════════════════════════
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Ranking de Imóveis — por Resultado do Mês (PUB-OP13)
  # ═══════════════════════════════════════════════════════════
  output$ranking <- renderUI({
    d <- dados(); req(d, input$mes_sel)
    rank_df <- d$receitas |>
      dplyr::filter(competencia == input$mes_sel) |>
      dplyr::group_by(imovel) |>
      dplyr::summarise(resultado = sum(resultado_liq, na.rm=TRUE), .groups="drop") |>
      dplyr::arrange(dplyr::desc(resultado))
    if (nrow(rank_df) == 0) return(p(class="sem-dados","Sem dados."))
    mx  <- max(abs(rank_df$resultado), 1)
    cls <- c("b1","b2","b3","b4")
    items <- lapply(seq_len(nrow(rank_df)), function(i) {
      r <- rank_df[i,]
      div(class="ri",
          div(class="rn", i),
          div(class="rname", r$imovel),
          div(class="rbw", div(class=paste("rb",cls[min(i,4)]),
                               style=paste0("width:",round(abs(r$resultado)/mx*100),"%"))),
          div(class="rval", brl(r$resultado)))
    })
    div(!!!items)
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Acumulado dos Últimos 12 Meses (PUB-10)
  # Sem gráfico — exibe apenas os dois cards de valor
  # ═══════════════════════════════════════════════════════════
  output$acumulado <- renderUI({
    df_base <- rec_fil()
    if (nrow(df_base) == 0) return(p(class="sem-dados","Sem dados."))
    mes_max <- max(df_base$mes, na.rm = TRUE)
    mes_ini <- mes_max %m-% months(11)
    df_12   <- df_base |> dplyr::filter(mes >= mes_ini)
    acum    <- df_12 |> dplyr::summarise(
      rec = sum(receita_bruta, na.rm=TRUE),
      res = sum(resultado_liq, na.rm=TRUE)
    )
    div(class="acg",
        div(div(class="al","RECEITA ACUMULADA"),
            div(class="av", brl(acum$rec)),
            div(class="ad","\u25b2 últimos 12 meses")),
        div(div(class="al","RESULTADO ACUMULADO"),
            div(class="av g", brl(acum$res)),
            div(class="ad","\u25b2 últimos 12 meses"))
    )
  })
  # g_acum stub para evitar erros de referência
  output$g_acum <- renderPlotly({ plotly_empty() |> config(displayModeBar=FALSE) })
  
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Gráfico Diárias por dia (análise temporal)
  # ═══════════════════════════════════════════════════════════
  output$g_diarias <- renderPlotly({
    d <- dados(); req(d, input$mes_sel)
    iid <- if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      input$imovel else if (length(d$imoveis_ids)>0) d$imoveis_ids[[1]] else return(NULL)
    cal <- d$calendario
    if (is.null(cal) || nrow(cal)==0) validate(need(FALSE,"Sem dados."))
    cal <- cal |>
      dplyr::filter(apto_original==iid,format(as.Date(data),"%Y-%m")==input$mes_sel) |>
      dplyr::arrange(data)
    validate(need(nrow(cal)>0,"Sem dados para o período."))
    plot_ly(cal,x=~as.Date(data),y=~valor,type="scatter",mode="lines+markers",
            line=list(color="#1a6ef7",width=2),marker=list(color=ifelse(cal$ocupado,"#1a6ef7","#d1d9e0"),size=6),
            hovertemplate="Dia %{x|%d/%m}<br>R$ %{y:,.0f}<extra></extra>") |>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=F,zeroline=F,tickformat="%d/%m",tickfont=list(size=10),title=""),
             yaxis=list(showgrid=T,gridcolor="#f0f4f8",zeroline=F,tickprefix="R$ ",tickfont=list(size=10),title=""),
             margin=list(l=52,r=10,t=8,b=30),showlegend=FALSE) |>
      config(displayModeBar=FALSE)
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Gráfico Evolução 6 Meses
  # ═══════════════════════════════════════════════════════════
  output$g_evolucao <- renderPlotly({
    df <- rec_fil() |>
      dplyr::group_by(mes, mes_label) |>
      dplyr::summarise(receita=sum(receita_bruta,na.rm=TRUE),resultado=sum(resultado_liq,na.rm=TRUE),.groups="drop") |>
      dplyr::arrange(as.Date(mes)) |>
      tail(12)   # PUB-8: janela de 12 meses
    validate(need(nrow(df)>0,"Sem dados."))
    ordem_labels <- df$mes_label
    n <- nrow(df)
    # PUB-9: resultado em barras, receita em linha
    cores_bar <- c(rep("#c5ede3", max(n-1,0)), "#00b388")[1:n]
    plot_ly(df, x=~mes_label, y=~resultado, type="bar",
            marker=list(color=cores_bar, line=list(color="transparent")),
            name="Resultado Líq.",
            hovertemplate="%{x}<br>Resultado: R$ %{y:,.0f}<extra></extra>") |>
      add_trace(y=~receita, type="scatter", mode="lines+markers",
                line=list(color="#1a6ef7", width=2.5), marker=list(color="#1a6ef7", size=6),
                name="Receita Bruta", yaxis="y2",
                hovertemplate="%{x}<br>Receita: R$ %{y:,.0f}<extra></extra>") |>
      layout(paper_bgcolor="transparent", plot_bgcolor="transparent",
             xaxis=list(showgrid=F, zeroline=F, tickfont=list(size=10), title="",
                        categoryorder="array", categoryarray=ordem_labels),
             yaxis=list(showgrid=T, gridcolor="#f0f4f8", zeroline=F, tickprefix="R$ ", tickfont=list(size=10), title=""),
             yaxis2=list(overlaying="y", side="right", showgrid=F, zeroline=F, tickprefix="R$ ", tickfont=list(size=9), title=""),
             margin=list(l=52,r=52,t=8,b=30),
             legend=list(x=0, y=1.15, orientation="h", font=list(size=10))) |>
      config(displayModeBar=FALSE)
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Tabela histórica
  # ═══════════════════════════════════════════════════════════
  output$t_historico <- renderDT({
    df <- rec_fil() |>
      dplyr::arrange(dplyr::desc(mes)) |>
      dplyr::transmute(
        `Imóvel`        = imovel,
        `Mês`           = mes_label,
        `Receita Bruta` = brl(receita_bruta),
        `Taxa Adm`      = paste0("- ", brl(taxa_adm)),
        `Outros Custos` = paste0("- ", brl(outros_custos)),
        `Resultado Líq` = brl(resultado_liq),
        `Ocupação`      = paste0(round(ocupacao), "%"),
        `Diária Média`  = brl(diaria_media),
        `Nº Diárias`    = n_diarias
      )
    datatable(df,
              options=list(pageLength=12,dom="frtip",
                           language=list(search="Buscar:",info="Mostrando _START_ a _END_ de _TOTAL_ registros",
                                         paginate=list(previous="Anterior",`next`="Próximo"))),
              rownames=FALSE,class="compact stripe hover")
  }, server=FALSE)
  
  
  
  # ═══════════════════════════════════════════════════════════
  # DOWNLOAD — Planilha Excel via /tmp + link direto
  # Estratégia: gera xlsx em /tmp, serve via addResourcePath,
  # exibe link <a href> que o browser baixa sem downloadHandler.
  # Isso resolve o conflito com renderUI / sys.source do app.R.
  # ═══════════════════════════════════════════════════════════
  
  # Gera o xlsx e retorna o path em /tmp
  .gerar_xlsx <- function(d) {
    if (!requireNamespace("openxlsx", quietly = TRUE))
      stop("Pacote openxlsx não instalado.")
    
    nome_prop <- gsub("[^A-Za-z0-9]", "_", d$proprietario %||% "carteira")
    fname     <- paste0("BSBStay_", nome_prop, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    fpath     <- file.path(tempdir(), fname)
    
    recs <- d$receitas
    
    # ── Estilos ──────────────────────────────────────────────
    cor_hdr <- "#0F1C2E"; cor_tx <- "#FFFFFF"; cor_acc <- "#1A6EF7"
    cor_grn <- "#00B388"; cor_red <- "#E03E3E"; cor_str <- "#F8FAFC"
    
    st_hdr <- openxlsx::createStyle(fontName="Arial", fontSize=10,
                                    fontColour=cor_tx, fgFill=cor_hdr, halign="CENTER",
                                    textDecoration="bold", border="Bottom", borderColour=cor_acc)
    st_brl_pos <- openxlsx::createStyle(fontName="Arial", fontSize=10,
                                        fontColour=cor_grn, halign="RIGHT", numFmt='"R$" #,##0.00',
                                        textDecoration="bold")
    st_brl_neg <- openxlsx::createStyle(fontName="Arial", fontSize=10,
                                        fontColour=cor_red, halign="RIGHT", numFmt='"R$" #,##0.00',
                                        textDecoration="bold")
    st_brl <- openxlsx::createStyle(fontName="Arial", fontSize=10,
                                    halign="RIGHT", numFmt='"R$" #,##0.00')
    st_pct <- openxlsx::createStyle(fontName="Arial", fontSize=10,
                                    halign="RIGHT", numFmt="0.0%")
    st_dat <- openxlsx::createStyle(fontName="Arial", fontSize=10,
                                    halign="CENTER", numFmt="DD/MM/YYYY")
    st_str <- openxlsx::createStyle(fgFill=cor_str)
    st_ttl <- openxlsx::createStyle(fontName="Arial", fontSize=14,
                                    fontColour=cor_hdr, textDecoration="bold")
    st_sub <- openxlsx::createStyle(fontName="Arial", fontSize=10,
                                    fontColour="#6B7280")
    
    wb <- openxlsx::createWorkbook()
    openxlsx::modifyBaseFont(wb, fontName="Arial", fontSize=10)
    
    .aba <- function(wb, nm, df, col_brl=c(), col_brl_res=c(), col_pct=c(), col_dat=c(),
                     cor_aba="#5A7A96", titulo=NULL, subtitulo=NULL) {
      # Guard: sem dados
      if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return(invisible())
      
      # Sanitiza nome (Excel: max 31 chars, sem /\[]?*:)
      nm <- substr(gsub("[/\\\\\\[\\]\\?\\*:]", " ", nm), 1, 31)
      
      # Evita nomes duplicados
      existing <- names(wb)
      if (nm %in% existing) {
        for (k in 2:99) {
          nm2 <- substr(paste0(nm, " ", k), 1, 31)
          if (!nm2 %in% existing) { nm <- nm2; break }
        }
      }
      
      openxlsx::addWorksheet(wb, nm, tabColour = cor_aba)
      
      # Título e subtítulo opcionais
      hdr_offset <- 0L
      if (!is.null(titulo) && nzchar(titulo)) {
        openxlsx::writeData(wb, nm, data.frame(x = titulo), startRow = 1, colNames = FALSE)
        openxlsx::addStyle(wb, nm, st_ttl, rows = 1, cols = 1)
        hdr_offset <- 2L
      }
      if (!is.null(subtitulo) && nzchar(subtitulo)) {
        openxlsx::writeData(wb, nm, data.frame(x = subtitulo), startRow = 2, colNames = FALSE)
        openxlsx::addStyle(wb, nm, st_sub, rows = 2, cols = 1)
        hdr_offset <- max(hdr_offset, 2L)
      }
      
      tbl_start  <- 1L + hdr_offset
      data_start <- tbl_start + 1L
      nr <- nrow(df)
      nc <- ncol(df)
      
      openxlsx::writeData(wb, nm, df, startRow = tbl_start, headerStyle = st_hdr)
      tryCatch(openxlsx::freezePane(wb, nm, firstActiveRow = data_start),
               error = function(e) NULL)
      
      # Zebra (pula se menos de 2 linhas)
      if (nr >= 2L) {
        for (r in seq(2L, nr, by = 2L))
          openxlsx::addStyle(wb, nm, st_str,
                             rows = data_start + r - 1L, cols = seq_len(nc),
                             gridExpand = TRUE, stack = TRUE)
      }
      
      # Range de linhas de dados (vazio se nr == 0)
      if (nr < 1L) return(invisible())
      rng <- seq(data_start, data_start + nr - 1L)
      
      for (col in col_brl) {
        ci <- match(col, names(df)); if (is.na(ci)) next
        openxlsx::addStyle(wb, nm, st_brl, rows = rng, cols = ci, stack = TRUE)
      }
      for (col in col_brl_res) {
        ci <- match(col, names(df)); if (is.na(ci)) next
        vals <- suppressWarnings(as.numeric(df[[col]]))
        pos <- which(!is.na(vals) & vals >= 0) + data_start - 1L
        neg <- which(!is.na(vals) & vals <  0) + data_start - 1L
        if (length(pos) > 0) openxlsx::addStyle(wb, nm, st_brl_pos, rows = pos, cols = ci, stack = TRUE)
        if (length(neg) > 0) openxlsx::addStyle(wb, nm, st_brl_neg, rows = neg, cols = ci, stack = TRUE)
      }
      for (col in col_pct) {
        ci <- match(col, names(df)); if (is.na(ci)) next
        openxlsx::addStyle(wb, nm, st_pct, rows = rng, cols = ci, stack = TRUE)
      }
      for (col in col_dat) {
        ci <- match(col, names(df)); if (is.na(ci)) next
        openxlsx::addStyle(wb, nm, st_dat, rows = rng, cols = ci, stack = TRUE)
      }
      
      openxlsx::setColWidths(wb, nm, cols = seq_len(nc), widths = "auto")
      invisible()
    }
    
    
    # ── Aba Resumo ───────────────────────────────────────────
    df_res <- recs |>
      dplyr::mutate(Mes = tryCatch(
        fmt_mes_pt(suppressWarnings(as.Date(paste0(substr(as.character(competencia),1,7),"-01")))),
        error=function(e) as.character(competencia))) |>
      dplyr::group_by(`Mês`=Mes) |>
      dplyr::summarise(
        `Receita Bruta (R$)`     = round(sum(receita_bruta,  na.rm=TRUE),2),
        `Taxa Adm. (R$)`         = round(sum(taxa_adm,       na.rm=TRUE),2),
        `Outros Custos (R$)`     = round(sum(outros_custos,  na.rm=TRUE),2),
        `Resultado Líq. (R$)`    = round(sum(resultado_liq,  na.rm=TRUE),2),
        `Ocupação (%)`           = round(mean(ocupacao,      na.rm=TRUE)/100,4),
        `Diária Média (R$)`      = round(mean(diaria_media,  na.rm=TRUE),2),
        `Noites`                 = sum(n_diarias, na.rm=TRUE),
        .groups="drop") |>
      dplyr::arrange(`Mês`)
    .aba(wb, "Resumo", df_res,
         titulo    = paste0("Resumo — ", d$proprietario %||% "Proprietário"),
         subtitulo = paste0("Gerado em ", format(Sys.Date(),"%d/%m/%Y"), " · BSBStay"),
         col_brl     = c("Receita Bruta (R$)","Taxa Adm. (R$)","Outros Custos (R$)","Diária Média (R$)"),
         col_brl_res = "Resultado Líq. (R$)",
         col_pct     = "Ocupação (%)",
         cor_aba     = cor_acc)
    
    # ── Aba por imóvel ───────────────────────────────────────
    for (im in sort(unique(recs$imovel))) {
      tryCatch({
        df_im <- recs |> dplyr::filter(imovel==im) |>
          dplyr::mutate(Mes=tryCatch(
            fmt_mes_pt(suppressWarnings(as.Date(paste0(substr(as.character(competencia),1,7),"-01")))),
            error=function(e) as.character(competencia))) |>
          dplyr::transmute(
            `Mês`=Mes,
            `Receita Bruta (R$)`  = round(receita_bruta,  2),
            `Taxa Adm. (R$)`      = round(taxa_adm,       2),
            `Manutenção (R$)`     = round(manutencao_total,2),
            `Reposição (R$)`      = round(reposicao_total, 2),
            `Despesas (R$)`       = round(despesas_total,  2),
            `Outros Custos (R$)`  = round(outros_custos,  2),
            `Resultado Líq. (R$)` = round(resultado_liq,  2),
            `Ocupação (%)`        = round(ocupacao/100,   4),
            `Diária Média (R$)`   = round(diaria_media,   2),
            `Noites`              = n_diarias) |>
          dplyr::arrange(`Mês`)
        .aba(wb, im, df_im,
             col_brl=c("Receita Bruta (R$)","Taxa Adm. (R$)","Manutenção (R$)",
                       "Reposição (R$)","Despesas (R$)","Outros Custos (R$)","Diária Média (R$)"),
             col_brl_res="Resultado Líq. (R$)", col_pct="Ocupação (%)")
      }, error = function(e) message("[XLSX] Erro na aba '", im, "': ", e$message))
    }
    
    # ── Aba Reservas ─────────────────────────────────────────
    resv <- tryCatch(d$reservas, error = function(e) NULL)
    if (!is.null(resv) && nrow(resv) > 0) {
      df_rv <- resv |>
        dplyr::transmute(
          `Imóvel`             = as.character(imovel_nome %||% property_id %||% apto_normalizado %||% ""),
          `Check-in`           = tryCatch(as.Date(checkin),  error=function(e) NA),
          `Check-out`          = tryCatch(as.Date(checkout), error=function(e) NA),
          `Noites`             = as.integer(noites_total),
          `Canal`              = as.character(origem_norm %||% ""),
          `Diária (R$)`        = round(as.numeric(diaria_liquida),    2),
          `Receita Total (R$)` = round(as.numeric(receita_liquida_mes %||% receita_total), 2)
        ) |> dplyr::arrange(`Imóvel`, `Check-in`)
      .aba(wb,"Reservas",df_rv,
           col_brl=c("Diária (R$)","Receita Total (R$)"),
           col_dat=c("Check-in","Check-out"), cor_aba="#7C3AED")
    }
    
    # ── Aba Custos ───────────────────────────────────────────
    dfs_cus <- list()
    man <- tryCatch(d$manutencao, error = function(e) NULL)
    rep <- tryCatch(d$reposicao,  error = function(e) NULL)
    des <- tryCatch(d$despesas,   error = function(e) NULL)
    if (!is.null(man) && nrow(man)>0)
      dfs_cus[["man"]] <- man |> dplyr::transmute(
        Tipo="Manutenção", Imóvel=as.character(imovel_nome %||% property_id %||% apto_normalizado %||% ""),
        `Mês`=as.character(competencia),
        Descrição=as.character(produto_servico %||% ""),
        `Valor (R$)`=round(as.numeric(valor_total),2))
    if (!is.null(rep) && nrow(rep)>0)
      dfs_cus[["rep"]] <- rep |> dplyr::transmute(
        Tipo="Reposição", Imóvel=as.character(imovel_nome %||% property_id %||% apto_normalizado %||% ""),
        `Mês`=as.character(competencia),
        Descrição=as.character(item_limpo %||% item_raw %||% ""),
        `Valor (R$)`=round(as.numeric(valor_unitario_ou_total),2))
    if (!is.null(des) && nrow(des)>0)
      dfs_cus[["des"]] <- des |> dplyr::transmute(
        Tipo=as.character(categoria %||% "Despesa"),
        Imóvel=as.character(imovel_nome %||% property_id %||% apto_normalizado %||% ""),
        `Mês`=as.character(competencia),
        Descrição=as.character(descricao %||% ""),
        `Valor (R$)`=round(as.numeric(valor),2))
    if (length(dfs_cus)>0) {
      df_cus <- dplyr::bind_rows(dfs_cus) |> dplyr::arrange(Imóvel, `Mês`, Tipo)
      .aba(wb,"Custos",df_cus, col_brl="Valor (R$)", cor_aba="#D97706")
    }
    
    openxlsx::saveWorkbook(wb, fpath, overwrite=TRUE)
    list(path=fpath, name=fname)
  }
  
  # ── Link de download via addResourcePath + link estático ─────
  #
  # Como funciona:
  #   1. addResourcePath("dl_tmp", tempdir()) — mapeia /tmp para a
  #      URL /dl_tmp/ dentro do processo Shiny. Feito uma vez na sessão.
  #   2. .gerar_xlsx() salva o xlsx em tempdir() e retorna o nome.
  #   3. O link <a href="/dl_tmp/ARQUIVO.xlsx"> serve o arquivo
  #      diretamente — sem downloadHandler, sem conflito de sessão.
  #
  # Esta é a única abordagem que funciona quando o server é carregado
  # via sys.source() após o login (o Shiny não aceita downloadHandler
  # registrado fora da inicialização da sessão).
  
  # Registra o mapeamento uma única vez por sessão
  addResourcePath("dl_tmp", tempdir())
  
  output$dl_link_wrap <- renderUI({ NULL })   # placeholder — aparece após gerar
  
  observeEvent(input$btn_gerar_dl, {
    d <- dados()
    req(d)
    withProgress(message = "Gerando planilha...", value = 0.5, {
      result <- tryCatch(
        .gerar_xlsx(d),
        error = function(e) {
          showNotification(paste("Erro ao gerar planilha:", e$message),
                           type = "error", duration = 6)
          NULL
        }
      )
      if (is.null(result)) return()
      
      # URL servida pelo Shiny via addResourcePath
      url_dl <- paste0("dl_tmp/", result$name)
      
      output$dl_link_wrap <- renderUI({
        tags$a(
          href     = url_dl,
          download = result$name,   # força download no browser (não abre)
          class    = "btn-dl btn-dl-ready",
          tags$svg(xmlns="http://www.w3.org/2000/svg", width="13", height="13",
                   viewBox="0 0 24 24", fill="none", stroke="currentColor",
                   `stroke-width`="2.5", `stroke-linecap`="round", `stroke-linejoin`="round",
                   tags$polyline(points="8 17 12 21 16 17"),
                   tags$line(x1="12", y1="12", x2="12", y2="21"),
                   tags$path(d="M20.88 18.09A5 5 0 0 0 18 9h-1.26A8 8 0 1 0 3 16.29")),
          "Clique para baixar"
        )
      })
      setProgress(1)
    })
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
} # fim server



`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

app <- shinyApp(ui, server)