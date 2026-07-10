# ============================================================
# BSB.STAY — Painel Mestre (Versão Interna) v2.0
# app_master.R
#
# - Sem busca por CNPJ — acesso direto
# - Navegação por Proprietário + Imóvel
# - Visão consolidada da carteira
# - Insights e proposta de valor
# ============================================================

# Pacotes carregados via Dockerfile — sem install.packages em produção
suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(tidyr); library(lubridate)
  library(readxl); library(janitor); library(plotly); library(DT)
  library(DBI); library(RSQLite); library(shinycssloaders); library(stringr)
})

APP_ROOT <- normalizePath(Sys.getenv("APP_ROOT", "."), winslash = "/", mustWork = FALSE)
if (!exists("carregar_dados_app")) {
  source(file.path(APP_ROOT, "R", "gdrive_public.R"), local = FALSE)
}

# ── Helpers ────────────────────────────────────────────────────
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

# Formatação VETORIZADA — seguras dentro de dplyr::transmute/mutate
brl <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(is.na(v), "R$ —",
         paste0("R$ ", formatC(v, format = "f", digits = 2, big.mark = ".", decimal.mark = ",")))
}
brl_compact <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(is.na(v), "R$ —",
         ifelse(abs(v) >= 1e6,
                paste0("R$ ", formatC(v / 1e6, format = "f", digits = 2, decimal.mark = ","), "M"),
                ifelse(abs(v) >= 1e3,
                       paste0("R$ ", formatC(v / 1e3, format = "f", digits = 1, decimal.mark = ","), "k"),
                       paste0("R$ ", formatC(v, format = "f", digits = 2, big.mark = ".", decimal.mark = ",")))))
}
fmt_pct <- function(x, decimals = 1) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(is.na(v), "—",
         paste0(formatC(round(v, decimals), format = "f", digits = decimals), "%"))
}

# Wrappers escalares — para renderUI / kcard / kcard_sm (recebem 1 valor)
s_brl <- function(x) {
  v <- suppressWarnings(as.numeric(x[1]))
  if (is.na(v)) return("R$ —")
  paste0("R$ ", formatC(v, format = "f", digits = 2, big.mark = ".", decimal.mark = ","))
}
s_brl_compact <- function(x) {
  v <- suppressWarnings(as.numeric(x[1]))
  if (is.na(v)) return("R$ —")
  if (abs(v) >= 1e6) paste0("R$ ", formatC(v / 1e6, format = "f", digits = 2, decimal.mark = ","), "M")
  else if (abs(v) >= 1e3) paste0("R$ ", formatC(v / 1e3, format = "f", digits = 1, decimal.mark = ","), "k")
  else paste0("R$ ", formatC(v, format = "f", digits = 2, big.mark = ".", decimal.mark = ","))
}
s_pct <- function(x, decimals = 1) {
  v <- suppressWarnings(as.numeric(x[1]))
  if (is.na(v)) return("—")
  paste0(format(round(v, decimals), nsmall = decimals), "%")
}

# UI components
kcard <- function(lbl, val, sub = "", dn = FALSE, vg = FALSE, icon = "", extra_class = "") {
  div(class = paste("kcard", extra_class),
      div(class = "klbl", if (nzchar(icon)) paste(icon, lbl) else lbl),
      div(class = if (vg) "kval g" else "kval", val),
      div(class = if (dn) "kdelta dn" else "kdelta up", sub)
  )
}
kcard_sm <- function(lbl, val, cor = "blue") {
  clr <- c(blue = "#1a6ef7", green = "#00b388", red = "#e03e3e",
           orange = "#d97706", purple = "#7c3aed", teal = "#0891b2")[[cor]] %||% "#1a6ef7"
  div(class = "kcard-sm",
      div(class = "ksm-lbl", lbl),
      div(class = "ksm-val", style = paste0("color:", clr), val))
}
frow <- function(lbl, val, neg = FALSE) {
  div(class = "fr",
      span(class = "fl", lbl),
      span(class = if (neg) "fv r" else "fv", val))
}
insight_card <- function(ico, titulo, corpo, cor = "blue") {
  bg_clr <- list(
    blue   = c("#eff6ff", "#1d4ed8"),
    green  = c("#f0fdf4", "#15803d"),
    orange = c("#fff7ed", "#c2410c"),
    purple = c("#faf5ff", "#6d28d9"),
    red    = c("#fff1f0", "#b91c1c")
  )[[cor]] %||% c("#eff6ff", "#1d4ed8")
  div(class = "insight-card",
      style = paste0("background:", bg_clr[1], ";border-left:4px solid ", bg_clr[2], ";"),
      div(class = "insight-icon", ico),
      div(class = "insight-body",
          div(class = "insight-title", style = paste0("color:", bg_clr[2]), titulo),
          div(class = "insight-text", corpo)))
}

# ── Carregamento inicial ────────────────────────────────────────
# APP_DATA_GLOBAL populado pelo run.R. Sem download bloqueante aqui.
APP_DATA <- if (exists("APP_DATA_GLOBAL") && length(APP_DATA_GLOBAL) > 0) {
  APP_DATA_GLOBAL
} else {
  structure(list(), erro_msg = "Dados ainda carregando, aguarde...")
}

# Extrai dados flat de todos os proprietários (para aba Carteira)
build_carteira_flat <- function(app_data) {
  if (length(app_data) == 0) return(data.frame())
  res <- lapply(names(app_data), function(cpf) {
    d <- app_data[[cpf]]
    if (is.null(d$receitas) || nrow(d$receitas) == 0) return(NULL)
    d$receitas |> dplyr::mutate(
      cpf_cnpj     = cpf,
      proprietario = d$proprietario %||% cpf,
      n_imoveis    = length(d$imoveis_ids)
    )
  })
  do.call(rbind, Filter(Negate(is.null), res))
}

# ═══════════════════════════════════════════════════════════════
# UI
# ═══════════════════════════════════════════════════════════════
ui <- fluidPage(
  tags$head(
    tags$title("BSB.STAY — Painel Mestre"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggle_fbar', function(msg) {
        var prop = document.getElementById('fbar_prop');
        var cart = document.getElementById('fbar_cart');
        if (prop) prop.style.display = (msg.aba === 'proprietario') ? '' : 'none';
        if (cart) cart.style.display = (msg.aba === 'carteira')     ? '' : 'none';
      });

      // ── TABS OPERACIONAL: gerenciadas 100% no cliente ─────────
      // setOpTab() troca a aba ativa via JS puro, sem re-renderizar o body.
      // Apenas o painel_operacional (renderUI proprio) e atualizado pelo server.
      function setOpTab(aba) {
        document.querySelectorAll('.op-tab').forEach(function(b) { b.classList.remove('active'); });
        var btn = document.getElementById('optab-' + aba);
        if (btn) btn.classList.add('active');
        var sy = window.scrollY;
        Shiny.setInputValue('btn_aba_op', aba, {priority: 'event'});
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
        setTimeout(function() { document.removeEventListener('shiny:value', restore); }, 3000);
      }

      // ── SCROLL GLOBAL: restaura posicao apos outros renderUIs ─
      var _savedScrollY = null;
      var _scrollSaveTime = 0;
      document.addEventListener('mousedown', function(e) {
        var el = e.target.closest('button, [onclick]');
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
.hdr{background:#0a1622;padding:0 32px;display:flex;align-items:stretch;justify-content:space-between;position:sticky;top:0;z-index:100;box-shadow:0 1px 0 rgba(255,255,255,.06),0 4px 20px rgba(0,0,0,.35);}
.hdr-left{display:flex;align-items:center;gap:16px;padding:12px 0;}
.hdr-logo-img{height:46px;width:auto;display:block;border-radius:8px;background:#fff;}
.hdr-title{color:#f0f6ff;font-size:15px;font-weight:700;letter-spacing:-.2px;}
.hdr-sub{color:#4a6580;font-size:10px;margin-top:3px;letter-spacing:.3px;}
.master-badge{background:rgba(90,180,255,.1);color:#5ab4ff;border:1px solid rgba(90,180,255,.2);border-radius:6px;padding:3px 10px;font-size:10px;font-weight:700;letter-spacing:.8px;}
/* Stats no header — texto branco legível sobre fundo escuro */
.hdr-stats{display:flex;align-items:stretch;gap:0;margin-left:auto;}
.hdr-stat{display:flex;flex-direction:column;justify-content:center;align-items:flex-end;padding:0 24px;border-left:1px solid rgba(255,255,255,.07);cursor:default;}
.hdr-stat:last-child{padding-right:0;}
.hdr-stat-val{font-size:18px;font-weight:800;color:#ffffff;line-height:1.1;}
.hdr-stat-lbl{font-size:9px;font-weight:600;color:rgba(255,255,255,.35);letter-spacing:.8px;text-transform:uppercase;margin-top:3px;}

/* ══ SYNC BAR ════════════════════════════════════════════════ */
.sync-bar{background:#0f1c2e;border-bottom:1px solid rgba(255,255,255,.06);padding:0 32px;display:flex;align-items:center;gap:12px;font-size:11px;color:rgba(255,255,255,.4);height:36px;}
.sync-dot{width:6px;height:6px;border-radius:50%;background:#00b388;flex-shrink:0;box-shadow:0 0 6px rgba(0,179,136,.5);}
.sync-dot.old{background:#d97706;box-shadow:0 0 6px rgba(217,119,6,.4);}
.sync-dot.err{background:#e03e3e;box-shadow:0 0 6px rgba(224,62,62,.4);}
.sync-bar span{color:rgba(255,255,255,.5);font-size:11px;}
.sync-btn{background:rgba(255,255,255,.06);border:1px solid rgba(255,255,255,.1);border-radius:6px;padding:4px 12px;font-size:11px;color:rgba(255,255,255,.6);cursor:pointer;font-family:inherit;transition:all .15s;letter-spacing:.2px;}
.sync-btn:hover{background:rgba(255,255,255,.12);color:#fff;border-color:rgba(255,255,255,.2);}

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

/* ══ NAV TABS ════════════════════════════════════════════════ */
.nav-tabs-master{display:flex;gap:2px;padding:0 32px;background:#0f1c2e;border-bottom:1px solid rgba(255,255,255,.06);}
.nav-tab-master{padding:11px 18px;font-size:12px;font-weight:600;color:rgba(255,255,255,.4);cursor:pointer;border:none;background:none;font-family:'Inter',sans-serif;letter-spacing:.1px;border-bottom:2px solid transparent;transition:all .15s;white-space:nowrap;}
.nav-tab-master:hover{color:rgba(255,255,255,.75);}
.nav-tab-master.active{color:#fff;border-bottom-color:#1a6ef7;}

/* ══ FILTER BAR ══════════════════════════════════════════════ */
.fbar{background:#fff;padding:10px 32px;border-bottom:1px solid #eaeff5;display:flex;gap:10px;align-items:center;flex-wrap:wrap;box-shadow:0 1px 3px rgba(0,0,0,.04);}
.fbar-lbl{font-size:10px;color:#9aa5b4;font-weight:700;letter-spacing:.7px;text-transform:uppercase;white-space:nowrap;}
.fbar-sep{width:1px;height:20px;background:#e5e9ef;margin:0 4px;}

/* ══ CONTENT ═════════════════════════════════════════════════ */
.content{padding:20px 32px 56px;max-width:1340px;margin:0 auto;}
.sec{font-size:10px;font-weight:800;color:#6b7280;letter-spacing:1.5px;text-transform:uppercase;margin:26px 0 10px;padding-bottom:6px;border-bottom:2px solid #e5e9ef;}

/* ══ KPI CARDS ══════════════════════════════════════════════ */
.kgrid{display:grid;grid-template-columns:repeat(7,1fr);gap:12px;margin-bottom:6px;align-items:stretch;}
@media(max-width:1300px){.kgrid{grid-template-columns:repeat(4,1fr);}}
@media(max-width:700px){.kgrid{grid-template-columns:repeat(2,1fr);}}
.kcard{background:#fff;border-radius:12px;padding:18px 20px;border:1px solid #e5e9ef;box-shadow:0 1px 4px rgba(0,0,0,.04);transition:box-shadow .15s;height:100%;}
.kcard:hover{box-shadow:0 3px 12px rgba(0,0,0,.08);}
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
.kcard-sm{background:#fff;border-radius:10px;padding:16px 20px;border:1px solid #e5e9ef;box-shadow:0 1px 3px rgba(0,0,0,.04);height:100%;}
.ksm-lbl{font-size:9px;font-weight:700;color:#9aa5b4;letter-spacing:.9px;text-transform:uppercase;margin-bottom:6px;}
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

/* ══ MÓDULO GERENCIAL ════════════════════════════════════════ */
.ger-header{background:linear-gradient(135deg,#0a1e36 0%,#0f2d4a 100%);border-radius:14px;padding:22px 28px;margin-bottom:20px;display:flex;justify-content:space-between;align-items:center;}
.ger-header-title{color:#fff;font-size:17px;font-weight:800;letter-spacing:-.3px;}
.ger-header-sub{color:rgba(255,255,255,.5);font-size:12px;margin-top:3px;}
.ger-stats{display:flex;gap:24px;}
.ger-stat{text-align:right;}
.ger-stat-val{font-size:22px;font-weight:800;color:#34d99e;}
.ger-stat-lbl{font-size:10px;color:rgba(255,255,255,.45);font-weight:700;letter-spacing:.8px;text-transform:uppercase;}
.ger-apto-card{background:#fff;border-radius:12px;border:1px solid #e5e9ef;box-shadow:0 1px 5px rgba(0,0,0,.04);margin-bottom:10px;overflow:hidden;transition:box-shadow .15s;}
.ger-apto-card:hover{box-shadow:0 4px 16px rgba(0,0,0,.08);}
.ger-apto-card.pub{border-left:4px solid #00b388;}
.ger-apto-card.pen{border-left:4px solid #f59e0b;}
.ger-apto-card.rev{border-left:4px solid #3b82f6;}
.ger-apto-hdr{display:flex;align-items:center;padding:14px 18px;gap:14px;cursor:pointer;user-select:none;}
.ger-apto-hdr:hover{background:#fafbfc;}
.ger-apto-nome{font-size:13px;font-weight:700;color:#0f1c2e;flex:1;}
.ger-apto-rec{font-size:13px;font-weight:700;color:#0f1c2e;min-width:110px;text-align:right;}
.ger-apto-res{font-size:12px;font-weight:700;min-width:110px;text-align:right;}
.ger-apto-res.pos{color:#00b388;} .ger-apto-res.neg{color:#e03e3e;}
.ger-status-pill{font-size:10px;font-weight:700;padding:3px 10px;border-radius:10px;white-space:nowrap;}
.pill-pub{background:#d1fae5;color:#065f46;}
.pill-pen{background:#fef3c7;color:#854d0e;}
.pill-rev{background:#dbeafe;color:#1d4ed8;}
.ger-toggle{font-size:11px;color:#9aa5b4;margin-left:8px;transition:transform .2s;}
.ger-body{padding:0 18px 18px;border-top:1px solid #f3f6f9;}
.ger-section{margin-top:16px;}
.ger-sec-title{font-size:10px;font-weight:800;color:#9aa5b4;letter-spacing:1px;text-transform:uppercase;margin-bottom:10px;display:flex;align-items:center;gap:8px;}
.ger-sec-title::after{content:'';flex:1;height:1px;background:#f0f4f8;}
.ger-field-row{display:flex;align-items:center;gap:10px;padding:7px 0;border-bottom:1px solid #fafbfc;}
.ger-field-row:last-child{border:none;}
.ger-field-lbl{font-size:12px;color:#374151;flex:1.5;}
.ger-field-orig{font-size:12px;color:#9aa5b4;flex:1;text-align:right;}
.ger-field-input{flex:1.2;}
.ger-field-input input{border:1.5px solid #e2e8f0;border-radius:7px;padding:5px 10px;font-size:13px;font-weight:600;color:#0f1c2e;width:100%;font-family:'Inter',sans-serif;transition:border .15s;}
.ger-field-input input:focus{border-color:#1a6ef7;outline:none;box-shadow:0 0 0 3px rgba(26,110,247,.1);}
.ger-field-diff{font-size:11px;font-weight:700;min-width:64px;text-align:right;}
.ger-field-diff.pos{color:#00b388;} .ger-field-diff.neg{color:#e03e3e;} .ger-field-diff.neu{color:#9aa5b4;}
.ger-actions{display:flex;gap:10px;margin-top:18px;padding-top:14px;border-top:2px solid #f0f4f8;justify-content:flex-end;}
.ger-btn{border:none;border-radius:8px;padding:9px 18px;font-size:12px;font-weight:700;cursor:pointer;font-family:'Inter',sans-serif;transition:all .15s;}
.ger-btn-reset{background:#f3f4f6;color:#374151;} .ger-btn-reset:hover{background:#e5e7eb;}
.ger-btn-pub{background:linear-gradient(135deg,#0a1e36,#1a6ef7);color:#fff;box-shadow:0 3px 10px rgba(26,110,247,.3);}
.ger-btn-pub:hover{opacity:.92;box-shadow:0 4px 14px rgba(26,110,247,.4);}
.ger-btn-pub:disabled{opacity:.45;cursor:not-allowed;}
.ger-nota{width:100%;border:1.5px solid #e2e8f0;border-radius:7px;padding:8px 10px;font-size:12px;font-family:'Inter',sans-serif;resize:vertical;min-height:60px;color:#374151;}
.ger-nota:focus{border-color:#1a6ef7;outline:none;}
.ger-sumario{background:#f8fafc;border-radius:8px;padding:12px 14px;display:grid;grid-template-columns:1fr 1fr 1fr;gap:8px;margin-top:10px;}
.ger-sum-item{font-size:11px;} .ger-sum-lbl{color:#9aa5b4;font-weight:700;letter-spacing:.5px;text-transform:uppercase;font-size:9px;}
.ger-sum-val{font-size:14px;font-weight:800;color:#0f1c2e;margin-top:1px;}
.ger-sum-val.g{color:#00b388;} .ger-sum-val.r{color:#e03e3e;}
.ger-pub-confirm{background:linear-gradient(135deg,#f0fdf4,#dcfce7);border:1.5px solid #86efac;border-radius:10px;padding:14px 16px;margin-top:12px;display:flex;align-items:center;gap:12px;}
.ger-pub-icon{font-size:22px;}
.ger-pub-msg{font-size:12px;color:#166534;font-weight:600;}
.ger-filter-bar{display:flex;gap:10px;align-items:center;margin-bottom:16px;flex-wrap:wrap;}
.ger-search{flex:1;min-width:200px;border:1.5px solid #e2e8f0;border-radius:8px;padding:7px 12px;font-size:13px;font-family:'Inter',sans-serif;}
.ger-search:focus{border-color:#1a6ef7;outline:none;}
.ger-mes-lbl{font-size:11px;font-weight:700;color:#6b7280;letter-spacing:.6px;white-space:nowrap;}
    "))
  ),
  
  div(class = "hdr",
      div(class = "hdr-left",
          tags$img(
            src   = "assets/marca_BSB_STAY_RS_10.jpg",
            alt   = "BSB Stay",
            class = "hdr-logo-img"
          ),
          div(div(class = "hdr-title", "Painel Mestre"),
              div(class = "hdr-sub",   "Controle e Validação Interno — BSBStay")),
          div(class = "master-badge", "USO INTERNO")
      ),
      uiOutput("hdr_stats")
  ),
  uiOutput("sync_bar"),
  uiOutput("nav_tabs"),
  uiOutput("filter_bar"),
  div(class = "content",
      uiOutput("alerta_erro"),
      uiOutput("body_master"))
)

# ═══════════════════════════════════════════════════════════════
# SERVER
# ═══════════════════════════════════════════════════════════════
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    app_data    = APP_DATA,
    aba         = "proprietario",
    op_aba      = "despesas",
    syncing     = FALSE,
    # Módulo Gerencial — estado de edições e publicações por (mes|apto)
    ger_edits   = list(),      # lista: chave = "mes|apto", valor = lista de ajustes
    ger_pub     = list(),      # lista: chave = "mes|apto", valor = TRUE/timestamp
    sync_status = "ok",
    last_sync   = tryCatch(status_cache(), error = function(e) list(last_sync = NA))$last_sync
  )
  
  observeEvent(input$btn_aba,    { rv$aba    <- input$btn_aba    }, ignoreInit = TRUE)
  observeEvent(input$btn_aba_op, { rv$op_aba <- input$btn_aba_op }, ignoreInit = TRUE)
  
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
  
  # ── Header stats ─────────────────────────────────────────────
  output$hdr_stats <- renderUI({
    d <- rv$app_data; if (length(d) == 0) return(NULL)
    n_prop <- length(d)
    n_imov <- sum(sapply(d, function(x) length(x$imoveis_ids)))
    flat   <- build_carteira_flat(d)
    rec    <- if (nrow(flat) > 0) sum(flat$receita_bruta, na.rm = TRUE) else 0
    div(class = "hdr-stats",
        div(class = "hdr-stat", div(class = "hdr-stat-val", n_prop),
            div(class = "hdr-stat-lbl", "Proprietários")),
        div(class = "hdr-stat", div(class = "hdr-stat-val", n_imov),
            div(class = "hdr-stat-lbl", "Imóveis")),
        div(class = "hdr-stat", div(class = "hdr-stat-val", s_brl_compact(rec)),
            div(class = "hdr-stat-lbl", "Receita Total")))
  })
  
  # ── Sync bar ─────────────────────────────────────────────────
  output$sync_bar <- renderUI({
    dot <- paste("sync-dot", rv$sync_status)
    msg <- if (rv$syncing) "Sincronizando..." else if (!is.na(rv$last_sync))
      paste0("Última sincronização: ", rv$last_sync) else "Dados não sincronizados"
    div(class = "sync-bar",
        div(class = dot),
        span(msg),
        tags$button(type = "button", class  = "sync-btn",
                    onclick = "Shiny.setInputValue('btn_sync', Math.random()); return false;",
                    if (rv$syncing) "Atualizando..." else "Atualizar dados"))
  })
  observeEvent(input$btn_sync, {
    rv$syncing <- TRUE
    tryCatch({
      nd <- carregar_dados_app(
        file_id    = DRIVE_FILE_ID,
        folder_id  = DRIVE_FOLDER_ID,
        forcar_dl  = TRUE,
        forcar_etl = TRUE
      )
      rv$app_data <- nd; rv$last_sync <- format(Sys.time(), "%d/%m/%Y %H:%M"); rv$sync_status <- "ok"
      showNotification("✓ Dados atualizados!", type = "message", duration = 4)
    }, error = function(e) {
      rv$sync_status <- "err"
      showNotification(paste("⚠ Falha:", e$message), type = "error", duration = 6)
    })
    rv$syncing <- FALSE
  }, ignoreInit = TRUE)
  
  # ── Nav tabs ─────────────────────────────────────────────────
  output$nav_tabs <- renderUI({
    abas <- list(
      list(id = "proprietario", lbl = "Por Proprietário"),
      list(id = "carteira",     lbl = "Visão da Carteira"),
      list(id = "gerencial",    lbl = "Revisão Gerencial"),
      list(id = "insights",     lbl = "Insights")
    )
    div(class = "nav-tabs-master",
        lapply(abas, function(a) tags$button(
          type    = "button",
          class   = paste("nav-tab-master", if (rv$aba == a$id) "active" else ""),
          onclick = sprintf("Shiny.setInputValue('btn_aba','%s',{priority:'event'}); return false;", a$id),
          a$lbl)))
  })
  
  # ── Filter bar ───────────────────────────────────────────────
  # Renderizado UMA vez; visibilidade controlada por JS + updateSelectInput.
  output$filter_bar <- renderUI({
    d <- rv$app_data; if (length(d) == 0) return(NULL)
    props      <- names(d)
    prop_nomes <- setNames(props, sapply(props, function(cpf) d[[cpf]]$proprietario %||% cpf))
    flat       <- build_carteira_flat(d)
    meses_cart <- if (nrow(flat) > 0) sort(unique(flat$competencia), decreasing = TRUE) else character(0)
    meses_cart_lbl <- setNames(meses_cart, {
      dt <- suppressWarnings(as.Date(paste0(meses_cart, "-01")))
      ifelse(is.na(dt), meses_cart, fmt_mes_pt(dt))
    })
    tagList(
      div(id = "fbar_prop", class = "fbar",
          style = if (isolate(rv$aba) != "proprietario") "display:none" else "",
          div(class = "fbar-lbl", "PROPRIETÁRIO"),
          selectInput("prop_sel", NULL, choices = prop_nomes, selected = props[1], width = "260px"),
          div(class = "fbar-sep"),
          div(class = "fbar-lbl", "MÊS"),
          selectInput("mes_sel", NULL, choices = character(0), width = "160px"),
          div(class = "fbar-sep"),
          div(class = "fbar-lbl", "IMÓVEL"),
          selectInput("imovel", NULL, choices = c("Todos os imóveis" = "all"), width = "220px")),
      div(id = "fbar_cart", class = "fbar",
          style = if (isolate(rv$aba) != "carteira") "display:none" else "",
          div(class = "fbar-lbl", "MÊS"),
          selectInput("mes_cart", NULL, choices = meses_cart_lbl,
                      selected = meses_cart[1], width = "160px"))
    )
  })
  
  # Mostrar/ocultar fbar via JS sem recriar inputs
  observeEvent(rv$aba, {
    session$sendCustomMessage("toggle_fbar", list(aba = rv$aba))
  }, ignoreInit = TRUE)
  
  # Atualizar mes e imovel quando proprietario muda
  observeEvent(input$prop_sel, {
    req(input$prop_sel)
    d_sel <- rv$app_data[[input$prop_sel]]
    if (is.null(d_sel)) return()
    meses <- if (!is.null(d_sel$receitas) && nrow(d_sel$receitas) > 0)
      sort(unique(d_sel$receitas$competencia), decreasing = TRUE) else character(0)
    meses_lbl <- setNames(meses, {
      dt <- suppressWarnings(as.Date(paste0(meses, "-01")))
      ifelse(is.na(dt), meses, fmt_mes_pt(dt))
    })
    imoveis <- c("Todos os imóveis" = "all", setNames(d_sel$imoveis_ids, d_sel$imoveis_ids))
    updateSelectInput(session, "mes_sel", choices = meses_lbl, selected = meses[1])
    updateSelectInput(session, "imovel",  choices = imoveis,   selected = "all")
  }, ignoreInit = FALSE)
  
  output$alerta_erro <- renderUI({
    msg <- attr(rv$app_data, "erro_msg")
    if (!is.null(msg)) div(class = "erro-dados", tags$b("⚠ Atenção: "), msg)
  })
  
  # ── Body roteamento ──────────────────────────────────────────
  output$body_master <- renderUI({
    if (length(rv$app_data) == 0)
      return(div(class = "empty-state", h3("⏳ Carregando dados..."),
                 p("Aguarde a sincronização com o Google Drive.")))
    switch(rv$aba,
           "proprietario" = uiOutput("body_proprietario"),
           "carteira"     = uiOutput("body_carteira"),
           "gerencial"    = uiOutput("body_gerencial"),
           "insights"     = uiOutput("body_insights"))
  })
  
  # ═══════════════════════════════════════════════════════════
  # REACTIVES — ABA PROPRIETÁRIO
  # ═══════════════════════════════════════════════════════════
  dados <- reactive({
    req(input$prop_sel, rv$aba == "proprietario")
    rv$app_data[[input$prop_sel]]
  })
  
  rec_fil <- reactive({
    d <- dados(); req(d)
    df <- d$receitas
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- dplyr::filter(df, imovel == input$imovel)
    df
  })
  
  rm_mes <- reactive({
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
    dias_mes <- tryCatch(
      as.integer(lubridate::days_in_month(as.Date(paste0(input$mes_sel, "-01")))),
      error = function(e) NA_integer_
    )
    out$dias_mes <- dias_mes
    out$revpar   <- if (!is.na(dias_mes) && dias_mes > 0) out$receita_bruta / dias_mes else NA_real_
    out
  })
  
  reservas_fil <- reactive({
    d <- dados(); req(d, input$mes_sel)
    if (is.null(d$reservas) || nrow(d$reservas) == 0) return(data.frame())
    df <- dplyr::filter(d$reservas, competencia == input$mes_sel)
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- dplyr::filter(df, imovel_nome == input$imovel)
    df |>
      dplyr::arrange(checkin)
  })

  despesas_fil <- reactive({
    d <- dados(); req(d, input$mes_sel)
    if (is.null(d$despesas) || nrow(d$despesas) == 0) return(data.frame())
    df <- dplyr::filter(d$despesas, competencia == input$mes_sel)
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- dplyr::filter(df, imovel_nome == input$imovel)
    df
  })
  
  reposicao_fil <- reactive({
    d <- dados(); req(d, input$mes_sel)
    if (is.null(d$reposicao) || nrow(d$reposicao) == 0) return(data.frame())
    df <- dplyr::filter(d$reposicao, competencia == input$mes_sel)
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- dplyr::filter(df, imovel_nome == input$imovel)
    df
  })
  
  manutencao_fil <- reactive({
    d <- dados(); req(d, input$mes_sel)
    if (is.null(d$manutencao) || nrow(d$manutencao) == 0) return(data.frame())
    df <- dplyr::filter(d$manutencao, competencia == input$mes_sel)
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- dplyr::filter(df, imovel_nome == input$imovel)
    df
  })
  
  # ═══════════════════════════════════════════════════════════
  # ABA 1 — POR PROPRIETÁRIO
  # ═══════════════════════════════════════════════════════════
  output$body_proprietario <- renderUI({
    d <- dados()
    if (is.null(d)) {
      return(div(class = "empty-state",
                 h3("⏳ Carregando dados..."),
                 p("Aguarde enquanto os dados são preparados.")))
    }
    req(input$mes_sel)
    m <- rm_mes()
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
          kcard("Resultado Líquido", brl(m$resultado_liq),
                paste0("receita: ", brl(m$receita_bruta)),
                vg = TRUE, extra_class = "hero"),
          kcard("Receita Bruta",  brl(m$receita_bruta),  "receita do período"),
          kcard("RevPAR",
                brl(if (!is.na(m$revpar)) m$revpar else 0),
                paste0(if (!is.na(m$dias_mes)) m$dias_mes else "—", " dias no mês"), icon = "📈"),
          kcard("Taxa Adm.",      brl(m$taxa_adm),
                paste0(round(if (m$receita_bruta > 0) m$taxa_adm / m$receita_bruta * 100 else 0, 1),
                       "% da receita"), dn = TRUE),
          tags$div(
            onclick = "document.getElementById('sec_operacional').scrollIntoView({behavior:'smooth'}); return false;",
            style = "cursor:pointer;",
            kcard("Outros Custos", brl(m$outros_custos), "", dn = TRUE)
          ),
          kcard("Ocupação", paste0(round(m$ocupacao), "%"),
                paste0("Diária média: ", brl(m$diaria_media))),
          kcard("Noites Reservadas", as.character(m$n_diarias), "noites no período")
      ),
      
      # ════════════════════════════════════════
      # 2. ANÁLISE DE RECEITA
      # ════════════════════════════════════════
      uiOutput("sec_analise_receita"),
      
      # ════════════════════════════════════════
      # 3. DETALHAMENTO DO MÊS — calendário
      # ════════════════════════════════════════
      uiOutput("sec_detalhamento_mes"),
      
      # Resultado + Custos (Ranking movido para Visão Geral)
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
      # 5. ANÁLISE DA DIÁRIA ENTRE CHECK-INS
      # ════════════════════════════════════════
      div(class = "sec", "ANÁLISE DA DIÁRIA ENTRE CHECK-INS"),
      
      # KPIs da diária
      shinycssloaders::withSpinner(uiOutput("kpis_diaria"), type = 4, color = "#1a6ef7"),
      
      # Gráfico + lista de intervalos
      div(class = "cgrid",
          div(class = "card",
              div(class = "card-hdr",
                  div(class = "card-ttl", "Evolução da Diária por Reserva"),
                  span(class = "badge badge-blue", "Nível de reserva")),
              shinycssloaders::withSpinner(
                plotlyOutput("g_diaria_reservas", height = "240px"), type = 4, color = "#1a6ef7")
          ),
          div(class = "card",
              div(class = "card-hdr",
                  div(class = "card-ttl", "Intervalos entre Check-ins"),
                  span(class = "badge badge-blue", mes_badge_sm)),
              uiOutput("lista_diarias"))
      ),
      
      # ════════════════════════════════════════
      # 6. OPERACIONAL: Despesas | Custos | OS
      # ════════════════════════════════════════
      div(class = "sec", id = "sec_operacional", "OPERACIONAL"),
      
      div(class = "card",
          # Tabs de navegação.
          # IMPORTANTE: type="button" evita que o <button> dispare o
          div(class = "op-tabs", id = "op-tabs-wrap",
              tags$button(type="button", id="optab-despesas",   class="op-tab active", onclick="setOpTab('despesas')",   "💰 Despesas"),
              tags$button(type="button", id="optab-custos",     class="op-tab",        onclick="setOpTab('custos')",     "🏠 Contas do Apartamento"),
              tags$button(type="button", id="optab-os",         class="op-tab",        onclick="setOpTab('os')",         "🔧 Manutenções"),
              tags$button(type="button", id="optab-reposicao",  class="op-tab",        onclick="setOpTab('reposicao')",  "📦 Reposição")
          ),
          uiOutput("painel_operacional")
      ),
      
      # ════════════════════════════════════════
      # ════════════════════════════════════════
      # 7. VISÃO GERAL + HISTÓRICO (OP-12: ANÁLISE TEMPORAL removida)
      # ════════════════════════════════════════
      div(class = "sec", "VISÃO GERAL DA CARTEIRA"),
      # Ranking integrado na Visão Geral
      div(class = "card", style = "margin-bottom:14px;",
          div(class = "card-hdr",
              div(class = "card-ttl", "Ranking de Imóveis"),
              span(class = "badge", "Resultado do mês")),
          uiOutput("ranking")),
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
      
      # OP-14: Portfólio de Imóveis incluído na VISÃO GERAL
      div(class = "sec-sub", "PORTFÓLIO DE IMÓVEIS"),
      shinycssloaders::withSpinner(uiOutput("portfolio_cards"), type = 4, color = "#1a6ef7"),
      
      div(class = "sec", "HISTÓRICO DETALHADO"),
      div(class = "card",
          shinycssloaders::withSpinner(DTOutput("t_historico"), type = 4, color = "#00c49a"))
    )
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Gráfico diária por dia (calendário)
  # ═══════════════════════════════════════════════════════════
  output$portfolio_cards <- renderUI({
    d <- dados(); req(d, input$mes_sel)
    if (length(d$imoveis_cfg) == 0) return(p(class="sem-dados","Nenhum imóvel cadastrado."))
    rec_mes <- rec_fil() |> dplyr::filter(competencia == input$mes_sel) |>
      dplyr::select(imovel, receita_bruta, ocupacao, diaria_media)
    div(class = "imovel-grid",
        lapply(d$imoveis_cfg, function(im) {
          nm  <- as.character(im$nome %||% im$id)
          rec <- dplyr::filter(rec_mes, imovel == nm)
          r   <- if (nrow(rec) > 0) rec[1,] else NULL
          div(class = "icard",
              div(class = "icard-nome", nm),
              div(class = "icard-end",  as.character(im$bairro %||% "")),
              div(class = "icard-metrics",
                  if (!is.null(r)) div(class="icard-m g", s_brl_compact(r$receita_bruta)) else NULL,
                  if (!is.null(r)) div(class="icard-m b", paste0(round(r$ocupacao),"% ocup.")) else NULL,
                  if (!is.null(r)) div(class="icard-m",   paste0(s_brl_compact(r$diaria_media),"/noite")) else NULL))
        }))
  })
  
  # ── Análise receita (gráfico diária/dia) ────────────────────
  output$sec_analise_receita <- renderUI({
    d <- dados(); req(d, input$mes_sel)
    iid <- if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      input$imovel else if (length(d$imoveis_ids) > 0) d$imoveis_ids[[1]] else return(NULL)
    cal <- d$calendario
    if (is.null(cal) || nrow(cal) == 0) return(NULL)
    if (nrow(dplyr::filter(cal, (apto_original==iid | property_id==iid),
                           substr(as.character(data),1,7)==input$mes_sel)) == 0) return(NULL)
    mes_lbl <- format(suppressWarnings(as.Date(paste0(input$mes_sel,"-01"))), "%B/%Y")
    tagList(
      div(class="sec","ANÁLISE DE RECEITA"),
      div(class="recv-wrap",
          div(class="recv-hdr", div(class="recv-ttl","Valor da Diária por Dia"),
              span(class="badge badge-blue", mes_lbl)),
          shinycssloaders::withSpinner(plotlyOutput("g_diaria_dia",height="220px"),type=4,color="#1a6ef7")))
  })
  output$g_diaria_dia <- renderPlotly({
    d <- dados(); req(d, input$mes_sel)
    iid <- if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      input$imovel else if (length(d$imoveis_ids) > 0) d$imoveis_ids[[1]] else return(NULL)
    cal <- d$calendario
    if (is.null(cal) || nrow(cal) == 0) validate(need(FALSE, "Sem dados diários."))
    cal <- cal |>
      dplyr::filter(apto_original == iid | property_id == iid,
                    substr(as.character(data), 1, 7) == input$mes_sel) |>
      dplyr::arrange(data)
    validate(need(nrow(cal) > 0, "Sem dados para o período."))
    plot_ly(cal, x = ~as.Date(data), y = ~valor, type = "scatter", mode = "lines+markers",
            line   = list(color = "#1a6ef7", width = 2.5),
            marker = list(color = "#1a6ef7", size = 7),
            hovertemplate = "Dia %{x|%d/%m}<br>R$ %{y:,.0f}<extra></extra>") |>
      layout(paper_bgcolor="transparent", plot_bgcolor="transparent",
             xaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,
                        tickformat="%d/%m",tickfont=list(size=10,color="#6b7280"),title=""),
             yaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,
                        tickprefix="R$ ",tickfont=list(size=10,color="#6b7280"),title=""),
             margin=list(l=55,r=12,t=8,b=32),showlegend=FALSE) |>
      config(displayModeBar = FALSE)
  })
  
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
  output$sec_analise_receita <- renderUI({
    d <- dados(); req(d, input$mes_sel)
    iid <- if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      input$imovel else if (length(d$imoveis_ids) > 0) d$imoveis_ids[[1]] else return(NULL)
    cal <- d$calendario
    if (is.null(cal) || nrow(cal) == 0) return(NULL)
    cal_fil <- cal |>
      dplyr::filter(apto_original == iid | property_id == iid,
                    substr(as.character(data), 1, 7) == input$mes_sel)
    if (nrow(cal_fil) == 0) return(NULL)
    mes_label_sel <- {
      dt <- suppressWarnings(as.Date(paste0(input$mes_sel, "-01")))
      if (!is.na(dt)) fmt_mes_pt(dt) else input$mes_sel
    }
    tagList(
      div(class = "sec", "ANÁLISE DE RECEITA"),
      div(class = "recv-wrap",
          div(class = "recv-hdr",
              div(class = "recv-ttl", "Valor da Diária por Dia"),
              span(class = "badge badge-blue", mes_label_sel)
          ),
          shinycssloaders::withSpinner(plotlyOutput("g_diaria_dia", height = "220px"), type = 4, color = "#1a6ef7")
      )
    )
  })
  
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
    resv_det <- if (!is.null(d$reservas) && nrow(d$reservas) > 0) {
      df_r <- d$reservas |> dplyr::filter(competencia == input$mes_sel)
      if (nzchar(iid) && iid != "all")
        df_r <- df_r |> dplyr::filter(imovel_nome == iid | property_id == iid)
      df_r
    } else data.frame()
    kpis_diaria_ui <- if (nrow(resv_det) > 0) {
      d_med   <- mean(resv_det$diaria_liquida, na.rm = TRUE)
      d_max_v <- max(resv_det$diaria_liquida,  na.rm = TRUE)
      vals_pos <- resv_det$diaria_liquida[!is.na(resv_det$diaria_liquida) & resv_det$diaria_liquida > 0]
      d_min_v  <- if (length(vals_pos) > 0) min(vals_pos) else NA_real_
      div(class = "kgrid-sm kgrid-sm-3", style = "margin-bottom:14px;",
          kcard_sm("Diária Média", brl(d_med),   "blue"),
          kcard_sm("Maior Diária", brl(d_max_v), "green"),
          kcard_sm("Menor Diária", brl(d_min_v), "orange"))
    } else NULL
    tagList(
      div(class = "sec", "DETALHAMENTO DO MÊS"),
      kpis_diaria_ui,
      div(class = "det-wrap",
          div(class = "det-hdr",
              div(class = "det-ttl", "Calendário de Ocupação"),
              span(class = "badge badge-green", mes_badge_sm)
          ),
          shinycssloaders::withSpinner(uiOutput("calendario_v2"), type = 4, color = "#00c49a")
      ),
      div(class = "card", style = "margin-top:14px;",
          div(class = "card-hdr",
              div(class = "card-ttl", "Relatório de Reservas"),
              span(class = "badge badge-blue", mes_badge_sm)),
          div(class = "tab-wrap",
              shinycssloaders::withSpinner(DTOutput("t_relatorio_reservas"), type = 4, color = "#1a6ef7")))
    )
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Relatório de Reservas (adicionado — Fix 5)
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
      `Hóspede`        = if ("hospede"             %in% cols_ok) as.character(hospede)            else "—",
      `Check-in`       = format(as.Date(checkin),  "%d/%m/%Y"),
      `Check-out`      = format(as.Date(checkout), "%d/%m/%Y"),
      `Canal`          = if ("canal"               %in% cols_ok) as.character(canal)              else "—",
      `Noites`         = as.integer(dplyr::coalesce(as.numeric(noites_total), 0)),
      `Adultos`        = if ("adultos"             %in% cols_ok) as.integer(dplyr::coalesce(as.numeric(adultos),  0)) else NA_integer_,
      `Crianças`       = if ("criancas"            %in% cols_ok) as.integer(dplyr::coalesce(as.numeric(criancas), 0)) else NA_integer_,
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
  # OUTPUT: KPIs da Diária entre Check-ins
  # ═══════════════════════════════════════════════════════════
  output$kpis_diaria <- renderUI({
    df <- reservas_fil()
    if (nrow(df) == 0) return(p(class = "sem-dados", "Sem reservas para o período selecionado."))
    
    d_media  <- mean(df$diaria_liquida, na.rm = TRUE)
    d_max    <- max(df$diaria_liquida,  na.rm = TRUE)
    d_min    <- min(df$diaria_liquida[df$diaria_liquida > 0], na.rm = TRUE)
    d_var    <- if (!is.na(d_max) && !is.na(d_min) && d_min > 0)
      paste0("+", round((d_max/d_min - 1)*100), "%") else "—"
    n_int    <- nrow(df)                       # nº de intervalos/reservas
    ticket   <- mean(df$receita_total, na.rm = TRUE)  # ticket médio por reserva
    
    div(class = "kgrid-sm kgrid-sm-3",
        kcard_sm("Diária Média",      brl(d_media), "blue"),
        kcard_sm("Maior Diária",      brl(d_max),   "green"),
        kcard_sm("Menor Diária",      brl(d_min),   "orange")
    )
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Gráfico de diária por reserva (scatter/line)
  # Lógica: cada ponto = 1 reserva; x = checkin; y = diária_liquida
  # Permite ver como a diária varia de reserva para reserva no mês
  # ═══════════════════════════════════════════════════════════
  output$g_diaria_reservas <- renderPlotly({
    df <- reservas_fil()
    validate(need(nrow(df) > 0, "Sem reservas para o período."))
    
    df <- df |>
      dplyr::mutate(
        label = paste0(format(checkin, "%d/%m"), " → ", format(checkout, "%d/%m"),
                       "\n", as.integer(checkout - checkin), " noites",
                       "\nDiária: R$ ", round(diaria_liquida))
      ) |>
      dplyr::arrange(checkin)
    
    d_media <- mean(df$diaria_liquida, na.rm = TRUE)
    
    plot_ly() |>
      add_bars(data = df,
               x = ~checkin, y = ~diaria_liquida,
               marker = list(
                 color = ~diaria_liquida,
                 colorscale = list(c(0,"#bcd6f8"), c(0.5,"#2684ff"), c(1,"#0052cc")),
                 showscale = FALSE,
                 line = list(color = "transparent")
               ),
               text = ~label,
               hovertemplate = "%{text}<extra></extra>",
               name = "Diária"
      ) |>
      add_lines(x = range(df$checkin, na.rm = TRUE),
                y = c(d_media, d_media),
                line = list(color = "#e03e3e", dash = "dash", width = 1.5),
                name = paste0("Média: R$ ", round(d_media)),
                hoverinfo = "skip"
      ) |>
      layout(
        paper_bgcolor = "transparent", plot_bgcolor = "transparent",
        xaxis = list(showgrid=FALSE, zeroline=FALSE, tickformat="%d/%m",
                     tickfont=list(size=10,color="#6b7280"), title=""),
        yaxis = list(showgrid=TRUE, gridcolor="#f3f6f9", zeroline=FALSE,
                     tickprefix="R$ ", tickfont=list(size=10,color="#6b7280"), title=""),
        margin = list(l=55, r=12, t=8, b=32),
        showlegend = TRUE,
        legend = list(x=0, y=1.12, orientation="h", font=list(size=10))
      ) |>
      config(displayModeBar = FALSE)
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Lista visual de intervalos entre check-ins
  # Mostra cada reserva como um card horizontal com barra de valor
  # ═══════════════════════════════════════════════════════════
  output$lista_diarias <- renderUI({
    df <- reservas_fil()
    if (nrow(df) == 0) return(p(class = "sem-dados", "Sem reservas para o período."))
    
    df <- df |> dplyr::arrange(checkin)
    d_max <- max(df$diaria_liquida, na.rm = TRUE)
    # Percentil 75 para classificar como "alto"
    p75 <- quantile(df$diaria_liquida, 0.75, na.rm = TRUE)
    p25 <- quantile(df$diaria_liquida, 0.25, na.rm = TRUE)
    
    items <- lapply(seq_len(min(nrow(df), 15)), function(i) {
      r      <- df[i,]
      noites <- as.integer(r$checkout - r$checkin)
      pct    <- round(r$diaria_liquida / d_max * 100)
      badge_cl <- if (r$diaria_liquida >= p75) "diaria-badge db-alto"
      else if (r$diaria_liquida >= p25) "diaria-badge db-medio"
      else "diaria-badge db-baixo"
      badge_lbl <- if (r$diaria_liquida >= p75) "Alta" else if (r$diaria_liquida >= p25) "Média" else "Baixa"
      
      div(class = "diaria-row",
          div(class = "diaria-datas",
              tags$b(paste0(format(r$checkin, "%d/%m"), " → ", format(r$checkout, "%d/%m"))),
              br(), span(class = "diaria-noites", paste0(noites, " noite", if(noites>1)"s" else ""))
          ),
          div(class = "diaria-bar-wrap",
              div(class = "diaria-bar", style = paste0("width:", pct, "%;"))
          ),
          div(class = "diaria-val", brl(r$diaria_liquida)),
          div(class = badge_cl, badge_lbl)
      )
    })
    
    note <- if (nrow(df) > 15) p(style="font-size:11px;color:#9aa5b4;margin-top:8px;text-align:center;",
                                 paste0("Exibindo 15 de ", nrow(df), " reservas")) else NULL
    tagList(!!!items, note)
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Painel operacional (tabs: despesas | custos | OS)
  # ═══════════════════════════════════════════════════════════
  output$painel_operacional <- renderUI({
    aba <- rv$op_aba %||% "despesas"
    
    if (aba == "despesas") {
      # ── ABA: Despesas ──────────────────────────────────────
      tagList(
        # KPIs de despesas
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
      # ── ABA: Contas do Apartamento (OP-7/OP-9) ─────────────
      tagList(
        shinycssloaders::withSpinner(uiOutput("kpis_custos"), type = 4, color = "#7c3aed"),
        # OP-9: Tabela de Contas substitui o gráfico de evolução
        div(class = "card", style = "margin-top:0;",
            div(class = "card-hdr",
                div(class = "card-ttl", "Tabela de Contas"),
                span(class = "badge badge-purple", "Mês selecionado")),
            div(class = "tab-wrap",
                shinycssloaders::withSpinner(DTOutput("t_contas_apto"), type = 4, color = "#7c3aed"))
        ),
        div(class = "card", style = "margin-top:14px;",
            div(class = "card-hdr",
                div(class = "card-ttl", "Custos Detalhados por Apartamento"),
                span(class = "badge badge-purple", "Mês selecionado")),
            div(class = "tab-wrap",
                shinycssloaders::withSpinner(DTOutput("t_custos_apto"), type = 4, color = "#7c3aed"))
        )
      )
      
    } else if (aba == "reposicao") {
      # ── ABA: Reposição ────────────────────────────────────
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
      # ── ABA: Manutenções (OP-10) ───────────────────────────
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
    des <- despesas_fil(); rep <- reposicao_fil()
    total_des <- if (nrow(des) > 0) sum(des$valor, na.rm = TRUE) else 0
    total_rep <- if (nrow(rep) > 0) sum(rep$valor_unitario_ou_total, na.rm = TRUE) else 0
    n_cat     <- if (nrow(des) > 0 && "categoria" %in% names(des)) length(unique(des$categoria)) else 0
    n_apto    <- if (nrow(des) > 0 && "imovel_nome" %in% names(des)) length(unique(des$imovel_nome)) else 0
    maior_cat <- if (nrow(des) > 0 && "categoria" %in% names(des)) {
      tc <- des |> dplyr::group_by(categoria) |> dplyr::summarise(v=sum(valor,na.rm=TRUE),.groups="drop") |> dplyr::arrange(dplyr::desc(v))
      if (nrow(tc) > 0) tc$categoria[1] else "—"
    } else "—"
    
    # OP-2: calcular total manutenção para os KPIs
    total_man_des <- {
      man_d <- manutencao_fil()
      if (nrow(man_d) > 0) sum(man_d$valor_total, na.rm=TRUE) else 0
    }
    
    div(class = "kgrid-sm",
        kcard_sm("Total Contas Imóvel", brl(total_des),               "orange"),  # OP-3
        kcard_sm("Total Reposição",     brl(total_rep),               "purple"),
        kcard_sm("Total Manutenção",    brl(total_man_des),           "blue"),     # OP-2
        kcard_sm("Maior Categoria",     maior_cat,                     "red"),
        kcard_sm("Total de Despesas",   brl(total_des + total_rep + total_man_des), "green") # OP-4
    )
  })
  
  # ── Gráfico despesas por categoria (pizza) ───────────────────
  output$g_despesas_cat <- renderPlotly({
    des <- despesas_fil()
    man <- manutencao_fil()
    rep <- reposicao_fil()
    
    # OP-5: unificar despesas + manutenção + reposição por categoria
    df_des <- if (nrow(des) > 0 && "categoria" %in% names(des))
      des |> dplyr::transmute(categoria, valor)
    else data.frame(categoria=character(), valor=numeric())
    
    df_man <- if (nrow(man) > 0)
      data.frame(categoria="Manutenção (OS)", valor=man$valor_total)
    else data.frame(categoria=character(), valor=numeric())
    
    df_rep <- if (nrow(rep) > 0)
      data.frame(categoria="Reposição", valor=suppressWarnings(as.numeric(rep$valor_unitario_ou_total)))
    else data.frame(categoria=character(), valor=numeric())
    
    df_all <- dplyr::bind_rows(df_des, df_man, df_rep)
    validate(need(nrow(df_all) > 0, "Sem dados de despesas/custos."))
    df <- df_all |>
      dplyr::group_by(categoria) |>
      dplyr::summarise(total = sum(valor, na.rm=TRUE), .groups="drop") |>
      dplyr::arrange(dplyr::desc(total))
    
    cores <- c("#0052cc","#2684ff","#d97706","#7c3aed","#e03e3e","#00b388","#f59e0b","#9ca3af","#0891b2","#34d399")
    plot_ly(df, labels=~categoria, values=~total, type="pie",
            marker=list(colors=cores, line=list(color="#fff", width=2)),
            textinfo="percent", textposition="inside",
            hovertemplate="%{label}<br>R$ %{value:,.0f}<extra></extra>") |>
      layout(paper_bgcolor="transparent", plot_bgcolor="transparent",
             showlegend=TRUE,
             legend=list(font=list(size=10), orientation="v", x=1.02, y=0.5),
             margin=list(l=0,r=120,t=10,b=10),
             autosize=TRUE) |>
      config(displayModeBar=FALSE)
  })
  
  # ── Gráfico despesas por apartamento (barras) ─────────────────
  output$g_despesas_apto <- renderPlotly({
    des <- despesas_fil()
    validate(need(nrow(des) > 0 && "imovel_nome" %in% names(des), "Sem dados de despesas."))
    df <- des |>
      dplyr::group_by(imovel_nome) |>
      dplyr::summarise(total = sum(valor, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(total)
    plot_ly(df, x = ~total, y = ~imovel_nome, type = "bar", orientation = "h",
            marker = list(color = "#d97706",
                          line = list(color = "transparent")),
            hovertemplate = "%{y}<br>R$ %{x:,.0f}<extra></extra>") |>
      layout(paper_bgcolor="transparent", plot_bgcolor="transparent",
             xaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,
                        tickprefix="R$ ",tickfont=list(size=10),title=""),
             yaxis=list(showgrid=FALSE,zeroline=FALSE,tickfont=list(size=10),title=""),
             margin=list(l=10,r=12,t=8,b=20),showlegend=FALSE) |>
      config(displayModeBar = FALSE)
  })
  
  # ── Tabela despesas ───────────────────────────────────────────
  output$t_despesas <- renderDT({
    des <- despesas_fil()
    man <- manutencao_fil()
    rep <- reposicao_fil()
    
    # OP-6: unificar despesas + manutenção + reposição em uma única tabela
    df_des <- if (nrow(des) > 0) {
      des |> dplyr::transmute(
        `Imóvel`      = dplyr::coalesce(as.character(imovel_nome), "—"),
        `Categoria`   = dplyr::coalesce(as.character(categoria), "Despesa"),
        `Descrição`   = if ("descricao" %in% names(des)) as.character(descricao) else "—",
        `Data`        = if ("data"       %in% names(des)) as.character(data)       else "—",
        `Competência` = as.character(competencia),
        `Valor (R$)`  = brl(valor)
      )
    } else data.frame()
    
    df_man <- if (nrow(man) > 0) {
      man |> dplyr::transmute(
        `Imóvel`      = dplyr::coalesce(as.character(imovel_nome), as.character(property_id), "—"),
        `Categoria`   = "Manutenção (OS)",
        `Descrição`   = if ("produto_servico" %in% names(man)) as.character(produto_servico) else "—",
        `Data`        = if ("data"            %in% names(man)) as.character(data)            else "—",
        `Competência` = as.character(competencia),
        `Valor (R$)`  = brl(valor_total)
      )
    } else data.frame()
    
    df_rep <- if (nrow(rep) > 0) {
      item_col <- if ("item_limpo" %in% names(rep)) rep$item_limpo
      else if ("item_raw" %in% names(rep)) rep$item_raw else rep(NA_character_, nrow(rep))
      rep |> dplyr::transmute(
        `Imóvel`      = dplyr::coalesce(as.character(imovel_nome %||% property_id), "—"),
        `Categoria`   = "Reposição",
        `Descrição`   = dplyr::coalesce(as.character(item_col), "—"),
        `Data`        = "—",
        `Competência` = as.character(competencia),
        `Valor (R$)`  = brl(suppressWarnings(as.numeric(valor_unitario_ou_total)))
      )
    } else data.frame()
    
    df <- dplyr::bind_rows(df_des, df_man, df_rep)
    validate(need(nrow(df) > 0, "Sem despesas/custos para o período/imóvel selecionado."))
    
    datatable(df,
              options = list(pageLength=10, dom="ftip",
                             language=list(search="Buscar:", paginate=list(previous="Ant.", `next`="Próx."))),
              rownames=FALSE, class="compact stripe hover", escape=FALSE)
  }, server=FALSE)
  
  # ── KPIs Custos por Apartamento ──────────────────────────────
  output$kpis_custos <- renderUI({
    d <- dados(); req(d, input$mes_sel)
    rec <- rec_fil() |> dplyr::filter(competencia == input$mes_sel)
    if (nrow(rec) == 0) return(p(class="sem-dados","Sem dados."))
    total_custo <- sum(rec$outros_custos, na.rm = TRUE)
    n_apto      <- length(unique(rec$imovel))
    custo_med   <- if (n_apto > 0) round(total_custo / n_apto, 2) else 0
    div(class = "kgrid-sm kgrid-sm-3",
        kcard_sm("Total Contas",     brl(total_custo),     "purple"),
        kcard_sm("Custo/Apto Médio", brl(custo_med),       "blue"),
        kcard_sm("Aptos com Custo",  as.character(n_apto), "orange")
    )
  })
  
  # ── Gráfico custos por apartamento (barras agrupadas) ────────
  output$g_custos_apto <- renderPlotly({
    d <- dados(); req(d)
    # Usa todos os meses disponíveis para mostrar evolução histórica
    df <- d$receitas |>
      dplyr::group_by(imovel, mes_label, mes) |>
      dplyr::summarise(
        custo = sum(outros_custos, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(mes)
    
    validate(need(nrow(df) > 0, "Sem dados de custos."))
    
    aptos  <- unique(df$imovel)
    cores  <- c("#0052cc","#2684ff","#d97706","#7c3aed","#e03e3e","#00b388","#f59e0b","#9ca3af")
    traces <- lapply(seq_along(aptos), function(i) {
      sub <- df |> dplyr::filter(imovel == aptos[i])
      list(x = sub$mes_label, y = sub$custo, name = aptos[i],
           type = "bar",
           marker = list(color = cores[((i-1)%%length(cores))+1], line=list(color="transparent")),
           hovertemplate = paste0(aptos[i], "<br>%{x}<br>R$ %{y:,.0f}<extra></extra>"))
    })
    
    p <- plot_ly()
    for (tr in traces) p <- do.call(add_trace, c(list(p), tr))
    p |>
      layout(barmode = "group",
             paper_bgcolor="transparent", plot_bgcolor="transparent",
             xaxis=list(showgrid=FALSE,zeroline=FALSE,tickfont=list(size=10),title=""),
             yaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,
                        tickprefix="R$ ",tickfont=list(size=10),title=""),
             margin=list(l=55,r=12,t=8,b=30),
             legend=list(x=0,y=1.15,orientation="h",font=list(size=10))) |>
      config(displayModeBar = FALSE)
  })
  
  # ── Tabela de Contas (OP-9) — substitui gráfico Evolução de Custos ──────────
  output$t_contas_apto <- renderDT({
    des <- despesas_fil()
    if (nrow(des) == 0)
      return(datatable(data.frame(Mensagem="Sem contas para o período/imóvel selecionado."),
                       options=list(dom="t"), rownames=FALSE))
    cols_ok <- names(des)
    df <- des |> dplyr::transmute(
      `Imóvel`      = dplyr::coalesce(as.character(imovel_nome), "—"),
      `Categoria`   = dplyr::coalesce(as.character(categoria),   "—"),
      `Descrição`   = if ("descricao"   %in% cols_ok) as.character(descricao)   else "—",
      `Data`        = if ("data"        %in% cols_ok) as.character(data)        else "—",
      `Competência` = as.character(competencia),
      `Valor (R$)`  = brl(valor)
    )
    datatable(df,
              options=list(pageLength=15, dom="ftip",
                           language=list(search="Buscar:", paginate=list(previous="Ant.", `next`="Próx."))),
              rownames=FALSE, class="compact stripe hover")
  }, server=FALSE)
  
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
  
  # ── KPIs Manutenções ──────────────────────────────────────────
  output$kpis_os <- renderUI({
    man <- manutencao_fil()
    if (nrow(man) == 0) return(p(class="sem-dados","Sem ordens de serviço no período."))
    
    total_val <- sum(man$valor_total, na.rm = TRUE)
    n_os      <- nrow(man)
    n_apto    <- if ("imovel_nome" %in% names(man)) length(unique(man$imovel_nome)) else 0
    val_med   <- mean(man$valor_total, na.rm = TRUE)
    # tipo mais frequente
    tipo_freq <- if ("tipo_servico" %in% names(man)) {
      tc <- sort(table(man$tipo_servico), decreasing = TRUE)
      if (length(tc) > 0) names(tc)[1] else "—"
    } else "—"
    n_pend <- if ("status" %in% names(man)) sum(grepl("pendente", tolower(man$status)), na.rm=TRUE) else 0
    
    div(class = "kgrid-sm kgrid-sm-2",
        kcard_sm("Qtde de OS",     as.character(n_os), "blue"),
        kcard_sm("Custo Total OS", brl(total_val),      "red")
    )
  })
  
  # ── Tabela Manutenções ────────────────────────────────────────
  output$t_os <- renderDT({
    man <- manutencao_fil()
    validate(need(nrow(man) > 0, "Sem ordens de serviço para o período/imóvel selecionado."))
    
    # Monta colunas disponíveis com fallback robusto
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
  
  # ═══════════════════════════════════════════════════════════
  # ── KPIs Reposição ───────────────────────────────────────────
  output$kpis_reposicao <- renderUI({
    rep <- reposicao_fil()
    if (nrow(rep) == 0) return(p(class = "sem-dados", "Sem itens de reposição para o período."))
    
    total_val  <- sum(suppressWarnings(as.numeric(rep$valor_unitario_ou_total)), na.rm = TRUE)
    total_qtd  <- sum(suppressWarnings(as.numeric(rep$quantidade)), na.rm = TRUE)
    n_itens    <- nrow(rep)
    n_aptos    <- if ("apto_original" %in% names(rep)) length(unique(rep$apto_original)) else
      if ("imovel_nome"   %in% names(rep)) length(unique(rep$imovel_nome))   else "—"
    item_freq  <- if ("item_limpo" %in% names(rep)) {
      tc <- sort(table(rep$item_limpo), decreasing = TRUE)
      if (length(tc) > 0) names(tc)[1] else "—"
    } else if ("item_raw" %in% names(rep)) {
      tc <- sort(table(rep$item_raw), decreasing = TRUE)
      if (length(tc) > 0) names(tc)[1] else "—"
    } else "—"
    ticket_med <- if (n_itens > 0) total_val / n_itens else 0
    
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
    m <- rm_mes()
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
  output$ranking <- renderUI({
    d <- dados(); req(d, input$mes_sel)
    # OP-13: rankear por resultado_liq (não por receita_bruta)
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
  # OUTPUT: Acumulado
  # ═══════════════════════════════════════════════════════════
  # PUB-10: Acumulado dos Últimos 12 Meses — sem gráfico
  output$acumulado <- renderUI({
    df_base <- rec_fil()
    if (nrow(df_base) == 0) return(p(class="sem-dados","Sem dados."))
    mes_max <- max(df_base$mes, na.rm=TRUE)
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
  # g_acum stub — referência removida do layout mas mantida para evitar erros
  output$g_acum <- renderPlotly({ plotly_empty() |> config(displayModeBar=FALSE) })
  
  # OP-12: g_diarias stub — ANÁLISE TEMPORAL removida do layout
  output$g_diarias <- renderPlotly({ plotly_empty() |> config(displayModeBar=FALSE) })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Evolução 12 Meses — PUB-9: resultado em barras, receita em linha
  # ═══════════════════════════════════════════════════════════
  output$g_evolucao <- renderPlotly({
    df <- rec_fil() |>
      dplyr::group_by(mes, mes_label) |>
      dplyr::summarise(receita=sum(receita_bruta,na.rm=TRUE),resultado=sum(resultado_liq,na.rm=TRUE),.groups="drop") |>
      dplyr::arrange(as.Date(mes)) |>
      tail(12)
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
  # ABA 2 — VISÃO DA CARTEIRA
  # ═══════════════════════════════════════════════════════════
  output$body_carteira <- renderUI({
    req(input$mes_cart)
    mes_lbl <- { dt <- suppressWarnings(as.Date(paste0(input$mes_cart,"-01")))
    if(!is.na(dt)) fmt_mes_pt(dt) else input$mes_cart }
    tagList(
      div(class="sec","VISÃO CONSOLIDADA DA CARTEIRA",div(class="sec-badge",mes_lbl)),
      shinycssloaders::withSpinner(uiOutput("kpis_carteira"),type=4,color="#1a6ef7"),
      
      div(class="sec","RANKING DE PROPRIETÁRIOS"),
      div(class="cgrid",
          div(class="card",
              div(class="card-hdr",div(class="card-ttl","Receita por Proprietário"),
                  span(class="badge badge-blue","Mês selecionado")),
              uiOutput("ranking_proprietarios")),
          div(class="card",
              div(class="card-hdr",div(class="card-ttl","Ocupação por Proprietário"),
                  span(class="badge badge-green","Taxa média")),
              shinycssloaders::withSpinner(plotlyOutput("g_ocupacao_prop",height="300px"),type=4,color="#00b388"))),
      
      div(class="sec","RANKING DE IMÓVEIS",div(class="sec-badge","Top performers")),
      div(class="cgrid",
          div(class="card",
              div(class="card-hdr",div(class="card-ttl","Top 10 — Maior Receita"),
                  span(class="badge badge-blue","No mês")),
              uiOutput("ranking_imoveis_receita")),
          div(class="card",
              div(class="card-hdr",div(class="card-ttl","Top 10 — Maior Diária Média"),
                  span(class="badge badge-purple","No mês")),
              uiOutput("ranking_imoveis_diaria"))),
      
      div(class="sec","EVOLUÇÃO DA CARTEIRA"),
      div(class="card",
          div(class="card-hdr",div(class="card-ttl","Receita Total por Mês — Todos os Proprietários"),
              span(class="badge","Visão consolidada")),
          shinycssloaders::withSpinner(plotlyOutput("g_evolucao_carteira",height="260px"),type=4,color="#1a6ef7")),
      
      div(class="sec","COMPARATIVO DE OCUPAÇÃO"),
      div(class="cgrid",
          div(class="card",
              div(class="card-hdr",div(class="card-ttl","Distribuição de Ocupação"),
                  span(class="badge badge-teal","Todos os imóveis")),
              shinycssloaders::withSpinner(plotlyOutput("g_dist_ocupacao",height="240px"),type=4,color="#0891b2")),
          div(class="card",
              div(class="card-hdr",div(class="card-ttl","Receita × Resultado"),
                  span(class="badge badge-green","Por imóvel")),
              shinycssloaders::withSpinner(plotlyOutput("g_scatter_rec_res",height="240px"),type=4,color="#00b388"))),
      
      div(class="sec","TABELA COMPLETA DA CARTEIRA"),
      div(class="card",
          shinycssloaders::withSpinner(DTOutput("t_carteira"),type=4,color="#1a6ef7"))
    )
  })
  
  output$kpis_carteira <- renderUI({
    req(input$mes_cart)
    flat <- build_carteira_flat(rv$app_data)
    if(nrow(flat)==0) return(p(class="sem-dados","Sem dados."))
    mes <- dplyr::filter(flat,competencia==input$mes_cart)
    if(nrow(mes)==0) return(p(class="sem-dados","Sem dados para o mês selecionado."))
    div(class="kgrid",
        kcard("Receita Total",   s_brl_compact(sum(mes$receita_bruta,na.rm=TRUE)),
              paste(length(unique(mes$imovel)),"imóveis ativos")),
        kcard("Resultado Total", s_brl_compact(sum(mes$resultado_liq,na.rm=TRUE)),
              "após deduções",vg=TRUE),
        kcard("Ocupação Média",  s_pct(mean(mes$ocupacao,na.rm=TRUE)),  "carteira inteira"),
        kcard("Diária Média",    s_brl(mean(mes$diaria_media,na.rm=TRUE)), "carteira inteira"),
        kcard("Proprietários",   as.character(length(unique(mes$proprietario))),
              paste(length(unique(mes$imovel)),"imóveis ativos")))
  })
  
  output$ranking_proprietarios <- renderUI({
    req(input$mes_cart)
    flat <- build_carteira_flat(rv$app_data)
    df <- flat|>dplyr::filter(competencia==input$mes_cart)|>
      dplyr::group_by(proprietario)|>
      dplyr::summarise(receita=sum(receita_bruta,na.rm=TRUE),n_im=dplyr::first(n_imoveis),.groups="drop")|>
      dplyr::arrange(dplyr::desc(receita))
    if(nrow(df)==0) return(p(class="sem-dados","Sem dados."))
    mx <- max(df$receita,1); cls <- c("b1","b2","b3","b4","b5")
    items <- lapply(seq_len(nrow(df)),function(i){
      r <- df[i,]
      div(class="ri",div(class="rn",i),
          div(div(class="rname",r$proprietario),div(class="rsub",paste(r$n_im,"imóvel(is)"))),
          div(class="rbw",div(class=paste("rb",cls[min(i,5)]),style=paste0("width:",round(r$receita/mx*100),"%;"))),
          div(class="rval",s_brl_compact(r$receita)))
    })
    div(!!!items)
  })
  
  output$g_ocupacao_prop <- renderPlotly({
    req(input$mes_cart)
    flat <- build_carteira_flat(rv$app_data)
    df <- flat|>dplyr::filter(competencia==input$mes_cart)|>
      dplyr::group_by(proprietario)|>dplyr::summarise(ocp=mean(ocupacao,na.rm=TRUE),.groups="drop")|>
      dplyr::arrange(ocp)|>dplyr::mutate(lbl=substr(proprietario,1,22))
    validate(need(nrow(df)>0,"Sem dados."))
    cores <- ifelse(df$ocp>=70,"#00b388",ifelse(df$ocp>=50,"#1a6ef7","#e03e3e"))
    plot_ly(df,x=~ocp,y=~lbl,type="bar",orientation="h",
            marker=list(color=cores,line=list(color="transparent")),
            hovertemplate="%{y}<br>Ocupação: %{x:.1f}%<extra></extra>")|>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,ticksuffix="%",tickfont=list(size=10),title="",range=c(0,105)),
             yaxis=list(showgrid=FALSE,zeroline=FALSE,tickfont=list(size=10),title=""),
             margin=list(l=10,r=12,t=8,b=20),showlegend=FALSE)|>config(displayModeBar=FALSE)
  })
  
  output$ranking_imoveis_receita <- renderUI({
    req(input$mes_cart)
    flat <- build_carteira_flat(rv$app_data)
    df <- flat|>dplyr::filter(competencia==input$mes_cart)|>
      dplyr::arrange(dplyr::desc(receita_bruta))|>dplyr::slice(1:10)
    if(nrow(df)==0) return(p(class="sem-dados","Sem dados."))
    mx <- max(df$receita_bruta,1); cls <- c("b1","b2","b3","b4","b5")
    items <- lapply(seq_len(nrow(df)),function(i){
      r <- df[i,]
      div(class="ri",div(class="rn",i),
          div(div(class="rname",r$imovel),div(class="rsub",r$proprietario)),
          div(class="rbw",div(class=paste("rb",cls[min(i,5)]),style=paste0("width:",round(r$receita_bruta/mx*100),"%;"))),
          div(class="rval",s_brl_compact(r$receita_bruta)))
    })
    div(!!!items)
  })
  
  output$ranking_imoveis_diaria <- renderUI({
    req(input$mes_cart)
    flat <- build_carteira_flat(rv$app_data)
    df <- flat|>dplyr::filter(competencia==input$mes_cart,diaria_media>0)|>
      dplyr::arrange(dplyr::desc(diaria_media))|>dplyr::slice(1:10)
    if(nrow(df)==0) return(p(class="sem-dados","Sem dados."))
    mx <- max(df$diaria_media,1); cls <- c("b1","b2","b3","b4","b5")
    items <- lapply(seq_len(nrow(df)),function(i){
      r <- df[i,]
      div(class="ri",div(class="rn",i),
          div(div(class="rname",r$imovel),div(class="rsub",r$proprietario)),
          div(class="rbw",div(class=paste("rb",cls[min(i,5)]),style=paste0("width:",round(r$diaria_media/mx*100),"%;"))),
          div(class="rval",s_brl(r$diaria_media)))
    })
    div(!!!items)
  })
  
  output$g_evolucao_carteira <- renderPlotly({
    flat <- build_carteira_flat(rv$app_data)
    if(nrow(flat)==0) validate(need(FALSE,"Sem dados."))
    df <- flat|>dplyr::group_by(mes,mes_label)|>
      dplyr::summarise(receita=sum(receita_bruta,na.rm=TRUE),resultado=sum(resultado_liq,na.rm=TRUE),.groups="drop")|>
      dplyr::arrange(as.Date(mes))
    validate(need(nrow(df)>0,"Sem dados."))
    ordem_labels <- df$mes_label
    n <- nrow(df)
    cores <- grDevices::colorRampPalette(c("#c5d8f7","#1a6ef7"))(n)
    plot_ly(df,x=~mes_label,y=~receita,type="bar",
            marker=list(color=cores,line=list(color="transparent")),name="Receita",
            hovertemplate="%{x}<br>R$ %{y:,.0f}<extra></extra>")|>
      add_trace(y=~resultado,type="scatter",mode="lines+markers",
                line=list(color="#00b388",width=2.5),marker=list(color="#00b388",size=7),
                name="Resultado",yaxis="y2",hovertemplate="%{x}<br>R$ %{y:,.0f}<extra></extra>")|>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=FALSE,zeroline=FALSE,tickfont=list(size=10),title="",
                        categoryorder="array",categoryarray=ordem_labels),
             yaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,tickprefix="R$ ",tickfont=list(size=10),title=""),
             yaxis2=list(overlaying="y",side="right",showgrid=FALSE,zeroline=FALSE,tickprefix="R$ ",tickfont=list(size=9),title=""),
             margin=list(l=55,r=55,t=8,b=30),
             legend=list(x=0,y=1.12,orientation="h",font=list(size=10)))|>config(displayModeBar=FALSE)
  })
  
  output$g_dist_ocupacao <- renderPlotly({
    req(input$mes_cart)
    flat <- build_carteira_flat(rv$app_data)
    df <- dplyr::filter(flat,competencia==input$mes_cart)
    validate(need(nrow(df)>0,"Sem dados."))
    plot_ly(df,x=~ocupacao,type="histogram",nbinsx=15,
            marker=list(color="#0891b2",line=list(color="#fff",width=1)),
            hovertemplate="Ocupação: %{x:.0f}%<br>Imóveis: %{y}<extra></extra>")|>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=FALSE,zeroline=FALSE,ticksuffix="%",tickfont=list(size=10),title="Taxa de Ocupação (%)"),
             yaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,tickfont=list(size=10),title="Nº de imóveis"),
             margin=list(l=45,r=12,t=8,b=40),showlegend=FALSE)|>config(displayModeBar=FALSE)
  })
  
  output$g_scatter_rec_res <- renderPlotly({
    req(input$mes_cart)
    flat <- build_carteira_flat(rv$app_data)
    df <- dplyr::filter(flat,competencia==input$mes_cart)
    validate(need(nrow(df)>0,"Sem dados."))
    plot_ly(df,x=~receita_bruta,y=~resultado_liq,type="scatter",mode="markers",
            marker=list(size=10,color=~ocupacao,
                        colorscale=list(c(0,"#bcd6f8"),c(.5,"#1a6ef7"),c(1,"#0052cc")),
                        showscale=TRUE,colorbar=list(title="Ocup.%",len=0.6)),
            text=~imovel,hovertemplate="%{text}<br>Receita: R$ %{x:,.0f}<br>Resultado: R$ %{y:,.0f}<extra></extra>")|>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=TRUE,zerolinecolor="#e5e9ef",tickprefix="R$ ",tickfont=list(size=10),title="Receita Bruta"),
             yaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=TRUE,zerolinecolor="#e5e9ef",tickprefix="R$ ",tickfont=list(size=10),title="Resultado Líq."),
             margin=list(l=55,r=20,t=8,b=45),showlegend=FALSE)|>config(displayModeBar=FALSE)
  })
  
  output$t_carteira <- renderDT({
    req(input$mes_cart)
    flat <- build_carteira_flat(rv$app_data)
    df <- flat|>dplyr::filter(competencia==input$mes_cart)|>
      dplyr::arrange(proprietario,imovel)|>
      dplyr::transmute(
        `Proprietário` = proprietario,
        `Imóvel`       = imovel,
        `Receita`      = brl(receita_bruta),
        `Taxa Adm`     = brl(taxa_adm),
        `Custos`       = brl(outros_custos),
        `Resultado`    = brl(resultado_liq),
        `Ocupação`     = paste0(round(ocupacao),"%"),
        `Diária Média` = brl(diaria_media),
        `Nº Diárias`   = n_diarias)
    datatable(df,options=list(pageLength=20,dom="frtip",
                              language=list(search="Buscar:",info="Mostrando _START_ a _END_ de _TOTAL_",
                                            paginate=list(previous="Anterior",`next`="Próximo"))),
              rownames=FALSE,class="compact stripe hover")
  },server=FALSE)
  
  # ═══════════════════════════════════════════════════════════
  # ABA 3 — INSIGHTS & PROPOSTA DE VALOR
  # ═══════════════════════════════════════════════════════════
  # ═══════════════════════════════════════════════════════════
  # ABA INSIGHTS — Motor analítico multiperíodo
  # Análise: tendência linear, clustering k-means, score de
  # saúde, detecção de anomalias vs. histórico próprio.
  # Base: dez/2025 → abr/2026 (dados disponíveis no SQLite).
  # ═══════════════════════════════════════════════════════════
  
  # ── Funções analíticas (base R, sem pacotes extras) ────────
  
  # Tendência linear: retorna slope normalizado (-1 a +1)
  .trend_slope <- function(y) {
    y <- as.numeric(y[!is.na(y)])
    if (length(y) < 2) return(0)
    x  <- seq_along(y)
    m  <- mean(y); sx <- sd(x); sy <- sd(y)
    if (sx == 0 || sy == 0) return(0)
    r  <- cor(x, y)
    round(r * (sy / m), 3)   # slope relativo à média
  }
  
  # K-means simples (k=3) sobre 2 variáveis normalizadas
  .kmeans_3 <- function(ocp, dar) {
    df <- data.frame(o = as.numeric(ocp), d = as.numeric(dar))
    df <- df[complete.cases(df) & df$o > 0, ]
    if (nrow(df) < 6) return(rep(1L, nrow(df)))
    df$o_n <- (df$o - mean(df$o)) / max(sd(df$o), 0.01)
    df$d_n <- (df$d - mean(df$d)) / max(sd(df$d), 0.01)
    set.seed(42)
    km <- tryCatch(
      kmeans(df[, c("o_n","d_n")], centers = 3, nstart = 10, iter.max = 50),
      error = function(e) NULL
    )
    if (is.null(km)) return(rep(1L, nrow(df)))
    # Reordenar clusters: 1=baixo, 2=médio, 3=alto (por soma ocp+dar)
    scores <- tapply(df$o_n + df$d_n, km$cluster, mean)
    ord    <- order(scores)
    map    <- setNames(seq_along(ord), ord)
    as.integer(map[as.character(km$cluster)])
  }
  
  # Score de saúde 0–100 por imóvel
  .health_score <- function(ocp, dar, ocp_med, dar_med, trend) {
    s_ocp   <- min(100, max(0, (ocp   / max(ocp_med,  1)) * 40))
    s_dar   <- min(40,  max(0, (dar   / max(dar_med,  1)) * 30))
    s_trend <- min(20,  max(0, (trend + 1) * 10))
    round(s_ocp + s_dar + s_trend)
  }
  
  # ── Reactive: análise multiperíodo ─────────────────────────
  ins_analise <- reactive({
    flat <- build_carteira_flat(rv$app_data)
    if (is.null(flat) || nrow(flat) == 0) return(NULL)
    
    flat <- flat |>
      dplyr::mutate(
        mes_chr      = substr(as.character(competencia), 1, 7),
        receita_bruta = as.numeric(receita_bruta),
        ocupacao      = as.numeric(ocupacao),
        diaria_media  = as.numeric(diaria_media),
        resultado_liq = as.numeric(resultado_liq),
        outros_custos = as.numeric(outros_custos)
      )
    
    # ── Estatísticas globais ──────────────────────────────────
    meses_ord  <- sort(unique(flat$mes_chr))
    n_meses    <- length(meses_ord)
    ultimo_mes <- meses_ord[n_meses]
    pen_mes    <- if (n_meses >= 2) meses_ord[n_meses - 1] else ultimo_mes
    
    mes_atual <- flat |> dplyr::filter(mes_chr == ultimo_mes)
    mes_ant   <- flat |> dplyr::filter(mes_chr == pen_mes)
    
    ocp_med   <- mean(mes_atual$ocupacao,     na.rm = TRUE)
    dar_med   <- mean(mes_atual$diaria_media, na.rm = TRUE)
    rec_tot   <- sum(mes_atual$receita_bruta, na.rm = TRUE)
    res_tot   <- sum(mes_atual$resultado_liq, na.rm = TRUE)
    cus_tot   <- sum(mes_atual$outros_custos, na.rm = TRUE)
    margem    <- if (rec_tot > 0) res_tot / rec_tot * 100 else 0
    custo_r   <- if (rec_tot > 0) cus_tot / rec_tot * 100 else 0
    
    rec_ant   <- sum(mes_ant$receita_bruta,   na.rm = TRUE)
    var_rec   <- if (rec_ant > 0) (rec_tot - rec_ant) / rec_ant * 100 else 0
    
    # ── Histórico por imóvel (imóveis com 2+ meses) ──────────
    hist_im <- flat |>
      dplyr::group_by(imovel, proprietario) |>
      dplyr::summarise(
        n_meses      = dplyr::n(),
        ocp_media    = mean(ocupacao,      na.rm = TRUE),
        dar_media    = mean(diaria_media,  na.rm = TRUE),
        rec_media    = mean(receita_bruta, na.rm = TRUE),
        rec_total    = sum(receita_bruta,  na.rm = TRUE),
        trend_rec    = .trend_slope(receita_bruta),
        trend_ocp    = .trend_slope(ocupacao),
        ocp_ultimo   = dplyr::last(ocupacao[order(mes_chr)]),
        dar_ultimo   = dplyr::last(diaria_media[order(mes_chr)]),
        rec_ultimo   = dplyr::last(receita_bruta[order(mes_chr)]),
        .groups      = "drop"
      ) |>
      dplyr::filter(n_meses >= 2)
    
    # ── Clustering k-means (ocupação × diária) ───────────────
    km_labels <- .kmeans_3(hist_im$ocp_media, hist_im$dar_media)
    hist_im$cluster <- km_labels
    # Nomear clusters com base nos centróides
    cl_ocp <- tapply(hist_im$ocp_media, hist_im$cluster, mean)
    cl_dat <- tapply(hist_im$dar_media, hist_im$cluster, mean)
    # Cluster com maior ocp E maior dar = Alta Performance
    cl_score <- cl_ocp + cl_dat
    cl_alto  <- as.integer(names(which.max(cl_score)))
    cl_baixo <- as.integer(names(which.min(cl_score)))
    cl_med   <- setdiff(1:3, c(cl_alto, cl_baixo))
    cluster_nome <- c("1"="Atenção", "2"="Atenção", "3"="Atenção")
    cluster_nome[as.character(cl_alto)]  <- "Alta Performance"
    cluster_nome[as.character(cl_med)]   <- "Potencial"
    cluster_nome[as.character(cl_baixo)] <- "Atenção"
    hist_im$cluster_nome <- cluster_nome[as.character(hist_im$cluster)]
    
    # ── Score de saúde por imóvel ────────────────────────────
    hist_im$score <- mapply(.health_score,
                            hist_im$ocp_media, hist_im$dar_media,
                            ocp_med, dar_med, hist_im$trend_rec)
    
    # ── Detecção de anomalias (queda vs. média própria) ──────
    im_atual <- mes_atual |>
      dplyr::select(imovel, ocupacao, diaria_media, receita_bruta, resultado_liq)
    hist_im <- hist_im |>
      dplyr::left_join(im_atual |> dplyr::rename(
        ocp_atual = ocupacao, dar_atual = diaria_media,
        rec_atual = receita_bruta, res_atual = resultado_liq
      ), by = "imovel") |>
      dplyr::mutate(
        anomalia_rec = dplyr::if_else(
          !is.na(rec_atual) & rec_media > 0,
          (rec_atual - rec_media) / rec_media * 100, NA_real_),
        anomalia_ocp = dplyr::if_else(
          !is.na(ocp_atual) & ocp_media > 0,
          (ocp_atual - ocp_media) / ocp_media * 100, NA_real_)
      )
    
    list(
      flat       = flat,
      mes_atual  = mes_atual,
      hist_im    = hist_im,
      meses_ord  = meses_ord,
      ultimo_mes = ultimo_mes,
      n_meses    = n_meses,
      # KPIs carteira
      ocp_med    = ocp_med,
      dar_med    = dar_med,
      rec_tot    = rec_tot,
      res_tot    = res_tot,
      cus_tot    = cus_tot,
      margem     = margem,
      custo_r    = custo_r,
      var_rec    = var_rec,
      rec_ant    = rec_ant
    )
  })
  
  # ── UI principal da aba Insights ───────────────────────────
  output$body_insights <- renderUI({
    a <- ins_analise()
    if (is.null(a)) return(div(class="empty-state", h3("Sem dados."), p("Aguarde a sincronização.")))
    
    # Label do mês atual
    mes_lbl <- tryCatch(
      fmt_mes_pt(as.Date(paste0(a$ultimo_mes, "-01"))),
      error = function(e) a$ultimo_mes)
    
    tagList(
      # ── KPIs rápidos do mês ───────────────────────────────
      div(style = "display:grid;grid-template-columns:repeat(5,1fr);gap:10px;margin-bottom:20px;",
          .ins_kpi("Receita Total",     brl(a$rec_tot),
                   paste0(ifelse(a$var_rec >= 0, "+", ""), round(a$var_rec, 1), "% vs. mês anterior"),
                   if (a$var_rec >= 0) "green" else "red"),
          .ins_kpi("Resultado Líquido", brl(a$res_tot),
                   paste0("Margem de ", round(a$margem, 1), "%"),
                   if (a$margem >= 60) "green" else if (a$margem >= 40) "blue" else "red"),
          .ins_kpi("Ocupação Média",    paste0(round(a$ocp_med, 1), "%"),
                   paste0(nrow(a$mes_atual), " imóveis no mês"),
                   if (a$ocp_med >= 75) "green" else if (a$ocp_med >= 55) "blue" else "orange"),
          .ins_kpi("Diária Média",      brl(a$dar_med),
                   paste0(a$n_meses, " meses de histórico"),
                   "blue"),
          .ins_kpi("Custo / Receita",   paste0(round(a$custo_r, 1), "%"),
                   if (a$custo_r <= 15) "Eficiência alta" else if (a$custo_r <= 30) "Nível saudável" else "Revisar custos",
                   if (a$custo_r <= 15) "green" else if (a$custo_r <= 30) "blue" else "orange")
      ),
      
      # ── Seção: diagnóstico analítico ─────────────────────
      div(class = "sec", paste0("DIAGNÓSTICO — ", toupper(mes_lbl))),
      shinycssloaders::withSpinner(uiOutput("ins_diagnostico"), type = 4, color = "#1a6ef7"),
      
      # ── Seção: clusters ───────────────────────────────────
      div(class = "sec", "SEGMENTAÇÃO DA CARTEIRA — PERFIS DE PERFORMANCE"),
      shinycssloaders::withSpinner(uiOutput("ins_clusters"),    type = 4, color = "#1a6ef7"),
      
      # ── Seção: alertas ────────────────────────────────────
      div(class = "sec", "ALERTAS E RECOMENDAÇÕES"),
      shinycssloaders::withSpinner(uiOutput("ins_alertas"),     type = 4, color = "#d97706"),
      
      # ── Seção: ranking de saúde ───────────────────────────
      div(class = "sec", "RANKING DE SAÚDE DA CARTEIRA"),
      shinycssloaders::withSpinner(uiOutput("ins_ranking"),     type = 4, color = "#00b388"),
      
      # ── Seção: benchmark imóvel ───────────────────────────
      div(class = "sec", "BENCHMARK — IMÓVEL VS. MÉDIA DA CARTEIRA"),
      div(class = "card",
          div(class = "card-hdr",
              div(class = "card-ttl", "Selecione um imóvel"),
              span(class = "badge badge-blue", "Evolução histórica")),
          selectInput("bench_imovel", NULL,
                      choices  = c("Selecione..." = "",
                                   setNames(
                                     unlist(lapply(rv$app_data, function(d) d$imoveis_ids)),
                                     unlist(lapply(rv$app_data, function(d) d$imoveis_ids)))),
                      width = "280px"),
          shinycssloaders::withSpinner(uiOutput("benchmark_imovel"), type = 4, color = "#1a6ef7")),
      shinycssloaders::withSpinner(uiOutput("sec_benchmark_graficos"), type = 4, color = "#7c3aed")
    )
  })
  
  # ── Helper: KPI card compacto ─────────────────────────────
  .ins_kpi <- function(lbl, val, sub, cor = "blue") {
    bdr <- switch(cor, green="#00b388", red="#e03e3e", orange="#d97706", purple="#7c3aed", "#1a6ef7")
    div(style = paste0("background:#fff;border-radius:10px;padding:14px 16px;",
                       "border:1px solid #e5e9ef;border-top:3px solid ", bdr, ";"),
        div(style = "font-size:9px;font-weight:700;color:#9aa5b4;letter-spacing:.8px;text-transform:uppercase;margin-bottom:6px;", lbl),
        div(style = "font-size:20px;font-weight:800;color:#0f1c2e;", val),
        div(style = paste0("font-size:11px;font-weight:500;color:", bdr, ";margin-top:4px;"), sub)
    )
  }
  
  # ── Diagnóstico analítico ─────────────────────────────────
  output$ins_diagnostico <- renderUI({
    a <- ins_analise(); req(a)
    h  <- a$hist_im
    mes_lbl <- tryCatch(fmt_mes_pt(as.Date(paste0(a$ultimo_mes,"-01"))), error=function(e) a$ultimo_mes)
    
    # Tendência de receita da carteira nos últimos meses
    flat_agg <- a$flat |>
      dplyr::group_by(mes_chr) |>
      dplyr::summarise(rec = sum(receita_bruta, na.rm=TRUE), ocp = mean(ocupacao, na.rm=TRUE), .groups="drop") |>
      dplyr::arrange(mes_chr)
    trend_carteira <- .trend_slope(flat_agg$rec)
    trend_lbl  <- if (trend_carteira > 0.05) "crescimento"
    else if (trend_carteira < -0.05) "queda"
    else "estabilidade"
    
    # Top 3 e bottom 3 por score
    top3  <- h |> dplyr::arrange(dplyr::desc(score)) |> dplyr::slice_head(n=3)
    bot3  <- h |> dplyr::arrange(score) |> dplyr::slice_head(n=3)
    n_alt <- sum(h$cluster_nome == "Alta Performance", na.rm=TRUE)
    n_ate <- sum(h$cluster_nome == "Atenção",           na.rm=TRUE)
    n_pot <- sum(h$cluster_nome == "Potencial",         na.rm=TRUE)
    
    # Imóveis com queda de receita > 20% vs. média própria
    em_queda <- h |> dplyr::filter(!is.na(anomalia_rec), anomalia_rec < -20) |>
      dplyr::arrange(anomalia_rec)
    em_alta  <- h |> dplyr::filter(!is.na(anomalia_rec), anomalia_rec >  25) |>
      dplyr::arrange(dplyr::desc(anomalia_rec))
    
    tagList(
      div(style="display:grid;grid-template-columns:1fr 1fr;gap:14px;",
          # Card tendência
          div(class="card",
              div(class="card-hdr", div(class="card-ttl","Tendência da Carteira"),
                  span(class=paste0("badge badge-",if(trend_carteira>0)"green"else if(trend_carteira<-0.05)"red"else"blue"),
                       toupper(trend_lbl))),
              div(style="padding:14px 18px;",
                  div(style="font-size:13px;color:#374151;line-height:1.6;",
                      paste0("Análise dos ", a$n_meses, " meses disponíveis (dez/2025 a abr/2026) indica ",
                             trend_lbl, " na receita consolidada da carteira. ",
                             "Dos ", nrow(h), " imóveis com histórico completo, ",
                             n_alt, " estão em alta performance, ",
                             n_pot, " têm potencial não explorado e ",
                             n_ate, " requerem atenção imediata.")),
                  div(style="margin-top:12px;display:flex;gap:16px;",
                      lapply(seq_len(nrow(flat_agg)), function(i) {
                        r  <- flat_agg[i,]
                        mx <- max(flat_agg$rec, na.rm=TRUE)
                        ht <- round(r$rec / mx * 48)
                        bdr_c <- if (r$mes_chr == a$ultimo_mes) "#1a6ef7" else "#e2e8f0"
                        mes_s <- tryCatch(
                          substr(fmt_mes_pt(as.Date(paste0(r$mes_chr,"-01")), abreviado=TRUE), 1, 3),
                          error=function(e) r$mes_chr)
                        div(style="display:flex;flex-direction:column;align-items:center;gap:4px;flex:1;",
                            div(style=paste0("width:100%;background:",bdr_c,";border-radius:3px;height:",ht,"px;")),
                            div(style="font-size:9px;color:#9aa5b4;font-weight:600;", mes_s),
                            div(style="font-size:10px;font-weight:700;color:#0f1c2e;",
                                paste0("R$", round(r$rec/1e6, 2), "M"))
                        )
                      })
                  )
              )
          ),
          # Card variação mês a mês
          div(class="card",
              div(class="card-hdr", div(class="card-ttl","Destaques do Mês"),
                  span(class="badge badge-blue", mes_lbl)),
              div(style="padding:14px 18px;",
                  div(style="margin-bottom:10px;",
                      div(style="font-size:9px;font-weight:700;color:#9aa5b4;letter-spacing:.7px;text-transform:uppercase;margin-bottom:6px;","MAIOR CRESCIMENTO VS. MÉDIA PRÓPRIA"),
                      if (nrow(em_alta) > 0) {
                        lapply(seq_len(min(2, nrow(em_alta))), function(i) {
                          r <- em_alta[i,]
                          div(style="display:flex;justify-content:space-between;padding:6px 0;border-bottom:1px solid #f8fafc;font-size:12px;",
                              div(style="color:#0f1c2e;font-weight:600;", r$imovel),
                              div(style="color:#00b388;font-weight:700;",
                                  paste0("+", round(r$anomalia_rec, 0), "% vs. média"))
                          )
                        })
                      } else { div(style="font-size:12px;color:#9aa5b4;","Sem variações expressivas.") }
                  ),
                  div(
                    div(style="font-size:9px;font-weight:700;color:#9aa5b4;letter-spacing:.7px;text-transform:uppercase;margin-bottom:6px;","MAIOR QUEDA VS. MÉDIA PRÓPRIA"),
                    if (nrow(em_queda) > 0) {
                      lapply(seq_len(min(2, nrow(em_queda))), function(i) {
                        r <- em_queda[i,]
                        div(style="display:flex;justify-content:space-between;padding:6px 0;border-bottom:1px solid #f8fafc;font-size:12px;",
                            div(style="color:#0f1c2e;font-weight:600;", r$imovel),
                            div(style="color:#e03e3e;font-weight:700;",
                                paste0(round(r$anomalia_rec, 0), "% vs. média"))
                        )
                      })
                    } else { div(style="font-size:12px;color:#9aa5b4;","Todos dentro da normalidade histórica.") }
                  )
              )
          )
      )
    )
  })
  
  # ── Clusters de performance ───────────────────────────────
  output$ins_clusters <- renderUI({
    a <- ins_analise(); req(a)
    h <- a$hist_im
    
    cluster_info <- list(
      "Alta Performance" = list(
        cor  = "#00b388",
        desc = "Ocupação e diária acima da média da carteira. Imóveis âncora — priorizar retenção dos proprietários.",
        acao = "Apresentar como benchmark para novos proprietários."
      ),
      "Potencial" = list(
        cor  = "#1a6ef7",
        desc = "Diária competitiva mas ocupação abaixo do esperado, ou vice-versa. Ajuste de estratégia pode gerar ganho relevante.",
        acao = "Revisar precificação e distribuição por canal."
      ),
      "Atenção" = list(
        cor  = "#d97706",
        desc = "Ocupação e diária abaixo da média da carteira. Risco de insatisfação do proprietário sem ação corretiva.",
        acao = "Contato proativo com proprietário e revisão de contrato."
      )
    )
    
    div(style = "display:grid;grid-template-columns:repeat(3,1fr);gap:12px;",
        lapply(names(cluster_info), function(nm) {
          info <- cluster_info[[nm]]
          ims  <- h |> dplyr::filter(cluster_nome == nm) |>
            dplyr::arrange(dplyr::desc(score))
          n    <- nrow(ims)
          ocp  <- if (n > 0) round(mean(ims$ocp_media, na.rm=TRUE), 1) else 0
          dar  <- if (n > 0) round(mean(ims$dar_media, na.rm=TRUE), 2) else 0
          rec  <- if (n > 0) sum(ims$rec_total, na.rm=TRUE) else 0
          
          div(class="card",
              div(style=paste0("padding:14px 18px;border-top:3px solid ",info$cor,";"),
                  div(style=paste0("display:flex;justify-content:space-between;align-items:center;margin-bottom:10px;"),
                      div(style=paste0("font-size:13px;font-weight:700;color:",info$cor,";"), nm),
                      div(style=paste0("font-size:22px;font-weight:800;color:#0f1c2e;"), n,
                          div(style="font-size:9px;font-weight:600;color:#9aa5b4;letter-spacing:.6px;text-transform:uppercase;","imóveis"))
                  ),
                  div(style="display:grid;grid-template-columns:1fr 1fr;gap:6px;margin-bottom:12px;",
                      div(style="background:#f8fafc;border-radius:6px;padding:8px;",
                          div(style="font-size:9px;color:#9aa5b4;font-weight:700;letter-spacing:.5px;text-transform:uppercase;","Ocup. Média"),
                          div(style="font-size:16px;font-weight:800;color:#0f1c2e;", paste0(ocp, "%"))),
                      div(style="background:#f8fafc;border-radius:6px;padding:8px;",
                          div(style="font-size:9px;color:#9aa5b4;font-weight:700;letter-spacing:.5px;text-transform:uppercase;","Diária Média"),
                          div(style="font-size:16px;font-weight:800;color:#0f1c2e;", brl(dar)))
                  ),
                  div(style="font-size:11px;color:#6b7280;line-height:1.5;margin-bottom:10px;", info$desc),
                  div(style=paste0("font-size:11px;font-weight:600;color:",info$cor,";padding:8px 10px;",
                                   "background:",info$cor,"18;border-radius:6px;"), info$acao),
                  if (n > 0) {
                    top_ims <- head(ims$imovel, 4)
                    div(style="margin-top:10px;",
                        div(style="font-size:9px;color:#9aa5b4;font-weight:700;letter-spacing:.5px;text-transform:uppercase;margin-bottom:4px;","Destaques"),
                        lapply(top_ims, function(im) {
                          sc <- ims$score[ims$imovel == im][1]
                          div(style="display:flex;justify-content:space-between;font-size:11px;padding:3px 0;border-bottom:1px solid #fafbfc;",
                              div(style="color:#374151;", im),
                              div(style=paste0("font-weight:700;color:",info$cor,";"), paste0(sc, " pts")))
                        })
                    )
                  }
              )
          )
        })
    )
  })
  
  # ── Alertas e recomendações ───────────────────────────────
  output$ins_alertas <- renderUI({
    a <- ins_analise(); req(a)
    h <- a$hist_im
    
    alertas <- list()
    
    # 1. Anomalias de queda vs. histórico próprio
    em_queda <- h |>
      dplyr::filter(!is.na(anomalia_rec), anomalia_rec < -20) |>
      dplyr::arrange(anomalia_rec) |>
      dplyr::slice_head(n = 5)
    for (i in seq_len(nrow(em_queda))) {
      r <- em_queda[i,]
      alertas <- c(alertas, list(
        .ins_alerta("critico",
                    paste0(r$imovel, " — Receita ", round(r$anomalia_rec,0), "% abaixo da sua própria média histórica"),
                    paste0("Proprietário: ", r$proprietario, ". Média histórica: ", brl(r$rec_media),
                           " | Mês atual: ", brl(r$rec_atual %||% 0),
                           ". Verificar disponibilidade no calendário e ação nos canais de distribuição."))
      ))
    }
    
    # 2. Precificação fora do mercado (diária alta, ocupação baixa vs. média própria)
    precif <- h |>
      dplyr::filter(!is.na(ocp_atual), !is.na(dar_atual),
                    dar_atual  > a$dar_med * 1.25,
                    ocp_atual  < a$ocp_med * 0.70) |>
      dplyr::arrange(ocp_atual)
    for (i in seq_len(min(3, nrow(precif)))) {
      r <- precif[i,]
      alertas <- c(alertas, list(
        .ins_alerta("atencao",
                    paste0(r$imovel, " — Diária ", round((r$dar_atual/a$dar_med-1)*100,0),
                           "% acima da média, ocupação ", round(r$ocp_atual,0), "%"),
                    paste0("Possível precificação acima do mercado para o período. ",
                           "Diária atual: ", brl(r$dar_atual), " vs. média da carteira: ", brl(a$dar_med),
                           ". Testar redução de 10–15% para elevar ocupação."))
      ))
    }
    
    # 3. Imóveis em queda de tendência consistente
    queda_trend <- h |>
      dplyr::filter(trend_rec < -0.15, n_meses >= 3) |>
      dplyr::arrange(trend_rec) |>
      dplyr::slice_head(n = 3)
    for (i in seq_len(nrow(queda_trend))) {
      r <- queda_trend[i,]
      alertas <- c(alertas, list(
        .ins_alerta("atencao",
                    paste0(r$imovel, " — Tendência de queda consistente nos últimos ", r$n_meses, " meses"),
                    paste0("Receita média: ", brl(r$rec_media), ". ",
                           "Recomendado: revisão de contrato, verificação de avaliações e ajuste de estratégia de canais."))
      ))
    }
    
    # 4. Oportunidades (alta tendência + abaixo da média de diária)
    oportunidade <- h |>
      dplyr::filter(trend_rec > 0.10, dar_media < a$dar_med * 0.85) |>
      dplyr::arrange(dplyr::desc(trend_rec)) |>
      dplyr::slice_head(n = 3)
    for (i in seq_len(nrow(oportunidade))) {
      r <- oportunidade[i,]
      alertas <- c(alertas, list(
        .ins_alerta("positivo",
                    paste0(r$imovel, " — Crescimento consistente com diária abaixo do potencial"),
                    paste0("Tendência positiva de receita com diária ", brl(r$dar_media),
                           " vs. média da carteira ", brl(a$dar_med),
                           ". Potencial de aumento de ", brl(a$dar_med - r$dar_media),
                           "/noite sem impactar ocupação atual de ", round(r$ocp_media,0), "%."))
      ))
    }
    
    # 5. Top performers consolidados
    tops <- h |>
      dplyr::filter(cluster_nome == "Alta Performance") |>
      dplyr::arrange(dplyr::desc(rec_total)) |>
      dplyr::slice_head(n = 2)
    for (i in seq_len(nrow(tops))) {
      r <- tops[i,]
      alertas <- c(alertas, list(
        .ins_alerta("positivo",
                    paste0(r$imovel, " — Melhor performance acumulada da carteira"),
                    paste0("Receita acumulada: ", brl(r$rec_total), " em ", r$n_meses, " meses. ",
                           "Ocupação média: ", round(r$ocp_media,1), "%. ",
                           "Imóvel referência para benchmark com novos proprietários."))
      ))
    }
    
    if (length(alertas) == 0)
      alertas <- list(.ins_alerta("positivo","Carteira sem alertas críticos",
                                  "Todos os imóveis dentro dos parâmetros esperados para o período."))
    
    div(style="display:flex;flex-direction:column;gap:8px;", !!!alertas)
  })
  
  # ── Helper: card de alerta ────────────────────────────────
  .ins_alerta <- function(tipo, titulo, descricao) {
    cfg <- switch(tipo,
                  critico  = list(bdr="#e03e3e", bg="#fef2f2", lbl_bg="#fee2e2", lbl_cor="#991b1b", lbl="CRÍTICO"),
                  atencao  = list(bdr="#d97706", bg="#fffbeb", lbl_bg="#fef3c7", lbl_cor="#854d0e", lbl="ATENÇÃO"),
                  positivo = list(bdr="#00b388", bg="#f0fdf9", lbl_bg="#d1fae5", lbl_cor="#065f46", lbl="OPORTUNIDADE"),
                  list(bdr="#1a6ef7", bg="#eff6ff", lbl_bg="#dbeafe", lbl_cor="#1e40af", lbl="INFO")
    )
    div(style=paste0("background:",cfg$bg,";border:1px solid ",cfg$bdr,"33;border-left:3px solid ",
                     cfg$bdr,";border-radius:8px;padding:12px 16px;display:flex;gap:14px;align-items:flex-start;"),
        div(style=paste0("flex-shrink:0;padding:3px 8px;border-radius:4px;font-size:9px;font-weight:800;",
                         "letter-spacing:.8px;background:",cfg$lbl_bg,";color:",cfg$lbl_cor,";",
                         "margin-top:1px;white-space:nowrap;"), cfg$lbl),
        div(style="flex:1;",
            div(style="font-size:12px;font-weight:700;color:#0f1c2e;margin-bottom:3px;", titulo),
            div(style="font-size:11px;color:#6b7280;line-height:1.5;", descricao)
        )
    )
  }
  
  # ── Ranking de saúde ─────────────────────────────────────
  output$ins_ranking <- renderUI({
    a <- ins_analise(); req(a)
    h <- a$hist_im |> dplyr::arrange(dplyr::desc(score))
    n <- min(20, nrow(h))
    if (n == 0) return(p(class="sem-dados","Sem dados suficientes para ranking."))
    
    max_s <- max(h$score, na.rm=TRUE)
    
    div(class="card",
        div(class="card-hdr",
            div(class="card-ttl", paste0("Top ", n, " imóveis por score de saúde")),
            span(class="badge badge-blue",
                 paste0(nrow(h), " imóveis analisados"))),
        div(style="padding:6px 18px 14px;",
            # Cabeçalho da tabela
            div(style="display:grid;grid-template-columns:2fr 1fr 1fr 1fr 80px 70px;",
                style="gap:6px;padding:6px 0;border-bottom:2px solid #f0f4f8;margin-bottom:4px;",
                lapply(c("Imóvel","Ocupação","Diária Média","Receita Acum.","Tendência","Score"), function(lbl)
                  div(style="font-size:9px;font-weight:800;color:#9aa5b4;letter-spacing:.6px;text-transform:uppercase;",lbl)
                )
            ),
            lapply(seq_len(n), function(i) {
              r   <- h[i,]
              sc  <- r$score
              bdr <- if (r$cluster_nome=="Alta Performance") "#00b388"
              else if (r$cluster_nome=="Potencial") "#1a6ef7" else "#d97706"
              trend_lbl <- if (r$trend_rec > 0.05) "Crescimento"
              else if (r$trend_rec < -0.05) "Queda"
              else "Estável"
              trend_cor <- if (r$trend_rec > 0.05) "#00b388"
              else if (r$trend_rec < -0.05) "#e03e3e" else "#9aa5b4"
              pct <- round(sc / max(max_s, 1) * 100)
              div(style="display:grid;grid-template-columns:2fr 1fr 1fr 1fr 80px 70px;gap:6px;",
                  style=paste0("padding:8px 0;border-bottom:1px solid #fafbfc;",
                               if(i%%2==0) "background:#fafbfc;" else ""),
                  div(style="display:flex;flex-direction:column;justify-content:center;",
                      div(style="font-size:12px;font-weight:600;color:#0f1c2e;", r$imovel),
                      div(style=paste0("font-size:10px;font-weight:600;color:",bdr,";"), r$cluster_nome)),
                  div(style="font-size:12px;color:#374151;display:flex;align-items:center;",
                      paste0(round(r$ocp_media,0), "%")),
                  div(style="font-size:12px;color:#374151;display:flex;align-items:center;",
                      brl(r$dar_media)),
                  div(style="font-size:12px;color:#374151;display:flex;align-items:center;",
                      brl(r$rec_total)),
                  div(style=paste0("font-size:11px;font-weight:600;color:",trend_cor,";display:flex;align-items:center;"),
                      trend_lbl),
                  div(style="display:flex;align-items:center;gap:6px;",
                      div(style=paste0("flex:1;height:6px;border-radius:3px;background:#f0f4f8;overflow:hidden;"),
                          div(style=paste0("width:",pct,"%;height:100%;background:",bdr,";border-radius:3px;"))),
                      div(style=paste0("font-size:11px;font-weight:800;color:",bdr,";min-width:28px;"), sc)
                  )
              )
            })
        )
    )
  })
  
  output$benchmark_imovel <- renderUI({
    req(input$bench_imovel, nzchar(input$bench_imovel))
    flat <- build_carteira_flat(rv$app_data); if (nrow(flat) == 0) return(NULL)
    im_df <- flat |> dplyr::filter(imovel == input$bench_imovel) |> dplyr::arrange(mes)
    med_df <- flat |> dplyr::group_by(mes, mes_label) |>
      dplyr::summarise(ocp_med = mean(ocupacao, na.rm=TRUE),
                       rec_med = mean(receita_bruta, na.rm=TRUE), .groups="drop") |>
      dplyr::arrange(mes)
    if (nrow(im_df) == 0) return(div(class="sem-dados","Imóvel sem histórico disponível."))
    score_im <- round(.health_score(
      mean(im_df$ocupacao, na.rm=TRUE), mean(im_df$diaria_media, na.rm=TRUE),
      mean(med_df$ocp_med, na.rm=TRUE), mean(flat$diaria_media, na.rm=TRUE), 0))
    div(style="padding:14px 18px;display:grid;grid-template-columns:repeat(4,1fr);gap:12px;",
        .ins_kpi("Receita Acumulada",  brl(sum(im_df$receita_bruta, na.rm=TRUE)), paste0(nrow(im_df)," meses"), "blue"),
        .ins_kpi("Ocupação Média",     paste0(round(mean(im_df$ocupacao,na.rm=TRUE),1),"%"),
                 paste0("Carteira: ", round(mean(med_df$ocp_med,na.rm=TRUE),1),"%"),
                 if(mean(im_df$ocupacao,na.rm=TRUE) >= mean(med_df$ocp_med,na.rm=TRUE))"green" else "orange"),
        .ins_kpi("Diária Média",       brl(mean(im_df$diaria_media,na.rm=TRUE)),
                 paste0("Carteira: ", brl(mean(flat$diaria_media,na.rm=TRUE))), "blue"),
        .ins_kpi("Score de Saúde",     paste0(score_im," / 100"),
                 if(score_im>=70)"Boa performance" else if(score_im>=45)"Performance média" else "Requer atenção",
                 if(score_im>=70)"green" else if(score_im>=45)"blue" else "orange")
    )
  })
  
  output$sec_benchmark_graficos <- renderUI({
    req(input$bench_imovel, nzchar(input$bench_imovel))
    div(class="cgrid",
        div(class="card",
            div(class="card-hdr", div(class="card-ttl","Receita — Imóvel vs. Média"),
                span(class="badge badge-blue","Evolução")),
            shinycssloaders::withSpinner(plotlyOutput("g_bench_receita",  height="220px"), type=4, color="#1a6ef7")),
        div(class="card",
            div(class="card-hdr", div(class="card-ttl","Ocupação — Imóvel vs. Média"),
                span(class="badge badge-green","Evolução")),
            shinycssloaders::withSpinner(plotlyOutput("g_bench_ocupacao", height="220px"), type=4, color="#00b388")))
  })
  
  output$g_bench_receita <- renderPlotly({
    req(input$bench_imovel, nzchar(input$bench_imovel))
    flat <- build_carteira_flat(rv$app_data); if(nrow(flat)==0) validate(need(FALSE,""))
    im_df  <- flat|>dplyr::filter(imovel==input$bench_imovel)|>dplyr::arrange(mes)
    med_df <- flat|>dplyr::group_by(mes,mes_label)|>
      dplyr::summarise(rec_med=mean(receita_bruta,na.rm=TRUE),.groups="drop")|>dplyr::arrange(mes)
    validate(need(nrow(im_df)>0,"Sem dados para este imóvel."))
    plot_ly()|>
      add_lines(data=med_df,x=~mes_label,y=~rec_med,
                line=list(color="#d1d9e0",width=2,dash="dash"),name="Média Carteira",
                hovertemplate="%{x}<br>Média: R$ %{y:,.0f}<extra></extra>")|>
      add_lines(data=im_df,x=~mes_label,y=~receita_bruta,
                line=list(color="#1a6ef7",width=2.5),marker=list(color="#1a6ef7",size=7),
                name=input$bench_imovel,hovertemplate="%{x}<br>R$ %{y:,.0f}<extra></extra>")|>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=FALSE,zeroline=FALSE,tickfont=list(size=10),title=""),
             yaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,tickprefix="R$ ",tickfont=list(size=10),title=""),
             margin=list(l=55,r=12,t=8,b=30),
             legend=list(x=0,y=1.15,orientation="h",font=list(size=10)))|>config(displayModeBar=FALSE)
  })
  
  output$g_bench_ocupacao <- renderPlotly({
    req(input$bench_imovel, nzchar(input$bench_imovel))
    flat <- build_carteira_flat(rv$app_data); if(nrow(flat)==0) validate(need(FALSE,""))
    im_df  <- flat|>dplyr::filter(imovel==input$bench_imovel)|>dplyr::arrange(mes)
    med_df <- flat|>dplyr::group_by(mes,mes_label)|>
      dplyr::summarise(ocp_med=mean(ocupacao,na.rm=TRUE),.groups="drop")|>dplyr::arrange(mes)
    validate(need(nrow(im_df)>0,"Sem dados."))
    plot_ly()|>
      add_lines(data=med_df,x=~mes_label,y=~ocp_med,
                line=list(color="#d1d9e0",width=2,dash="dash"),name="Média Carteira",
                hovertemplate="%{x}<br>Média: %{y:.1f}%<extra></extra>")|>
      add_lines(data=im_df,x=~mes_label,y=~ocupacao,
                line=list(color="#00b388",width=2.5),marker=list(color="#00b388",size=7),
                name=input$bench_imovel,hovertemplate="%{x}<br>%{y:.1f}%<extra></extra>")|>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=FALSE,zeroline=FALSE,tickfont=list(size=10),title=""),
             yaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,ticksuffix="%",tickfont=list(size=10),title=""),
             margin=list(l=45,r=12,t=8,b=30),
             legend=list(x=0,y=1.15,orientation="h",font=list(size=10)))|>config(displayModeBar=FALSE)
  })
  
  
  # ═══════════════════════════════════════════════════════════
  # ABA 3 — REVISÃO GERENCIAL
  # Fluxo: editar → salvar rascunho → publicar → write-back Drive
  # ═══════════════════════════════════════════════════════════
  
  # ── Helpers ────────────────────────────────────────────────
  .ger_key    <- function(mes, apto) paste0(as.character(mes), "|", apto)
  .ger_get    <- function(key, field, default) { e <- rv$ger_edits[[key]]; if (!is.null(e) && !is.null(e[[field]])) e[[field]] else default }
  .ger_is_pub <- function(key) isTRUE(!is.null(rv$ger_pub[[key]]))
  .ger_status <- function(key) { if (.ger_is_pub(key)) "pub" else if (!is.null(rv$ger_edits[[key]]) && length(rv$ger_edits[[key]]) > 0) "rev" else "pen" }
  
  # ── Persistência gerencial no SQLite local ─────────────────
  # Grava na tabela ger_ajustes do bsbstay.sqlite (/data/bsbstay).
  # Não toca na fonte base (Google Drive). Persiste entre sessões.
  .ger_save_sqlite <- function(mes, apto, cpf, edits, publicado = FALSE) {
    tryCatch({
      ger_save_ajuste(
        mes             = mes,
        apto            = apto,
        cpf_cnpj        = cpf,
        taxa_adm        = round(as.numeric(edits$taxa_adm        %||% 0),    2),
        manutencao      = round(as.numeric(edits$manutencao      %||% 0),    2),
        reposicao       = round(as.numeric(edits$reposicao       %||% 0),    2),
        despesas        = round(as.numeric(edits$despesas        %||% 0),    2),
        # receita_ajuste: NULL = sem ajuste; valor = sobrescreve original
        receita_ajuste  = if (is.null(edits$receita_ajuste) || is.na(edits$receita_ajuste))
          NULL
        else round(as.numeric(edits$receita_ajuste), 2),
        rec_original    = if (is.null(edits$rec_original) || is.na(edits$rec_original))
          NULL
        else round(as.numeric(edits$rec_original), 2),
        nota            = as.character(edits$nota %||% ""),
        publicado       = publicado
      )
      TRUE
    }, error = function(e) {
      message("[Ger] Falha ao gravar SQLite: ", e$message)
      FALSE
    })
  }
  
  # ── Reactive: dados do mês ─────────────────────────────────
  # Carrega estado salvo do SQLite quando entra na aba gerencial
  observeEvent(rv$aba, {
    if (rv$aba != "gerencial") return()
    tryCatch({
      aj <- ger_load_ajustes()
      if (is.null(aj) || nrow(aj) == 0) return()
      # Repopula rv$ger_edits e rv$ger_pub com o que estava salvo
      for (i in seq_len(nrow(aj))) {
        key <- paste0(substr(as.character(aj$mes[i]),1,7), "|", aj$apto[i])
        rv$ger_edits[[key]] <- list(
          taxa_adm       = aj$taxa_adm[i],
          manutencao     = aj$manutencao[i],
          reposicao      = aj$reposicao[i],
          despesas       = aj$despesas[i],
          receita_ajuste = if ("receita_ajuste" %in% names(aj) && !is.na(aj$receita_ajuste[i]))
            aj$receita_ajuste[i] else NULL,
          rec_original   = if ("rec_original" %in% names(aj) && !is.na(aj$rec_original[i]))
            aj$rec_original[i] else NULL,
          nota           = aj$nota[i]
        )
        if (isTRUE(aj$publicado[i] == 1L)) {
          rv$ger_pub[[key]] <- if (nzchar(aj$ts[i])) aj$ts[i] else "Publicado"
        }
      }
      message(sprintf("[Ger] %d ajuste(s) carregado(s) do SQLite.", nrow(aj)))
    }, error = function(e) message("[Ger] AVISO ao carregar ajustes: ", e$message))
  }, ignoreInit = FALSE)
  
  ger_dados <- reactive({
    req(rv$aba == "gerencial")
    d <- rv$app_data; if (length(d) == 0) return(NULL)
    mes_sel <- input$ger_mes; req(!is.null(mes_sel), nzchar(mes_sel))
    flat <- build_carteira_flat(d); if (nrow(flat) == 0) return(NULL)
    mes_chr <- substr(as.character(mes_sel), 1, 7)
    flat |>
      dplyr::filter(substr(as.character(competencia), 1, 7) == mes_chr) |>
      dplyr::mutate(
        receita_bruta    = dplyr::coalesce(as.numeric(receita_bruta), 0),
        taxa_adm         = dplyr::coalesce(as.numeric(taxa_adm), 0),
        manutencao_total = dplyr::coalesce(as.numeric(manutencao_total), 0),
        reposicao_total  = dplyr::coalesce(as.numeric(reposicao_total), 0),
        despesas_total   = dplyr::coalesce(as.numeric(despesas_total), 0),
        outros_custos    = dplyr::coalesce(as.numeric(outros_custos), 0),
        resultado_liq    = dplyr::coalesce(as.numeric(resultado_liq), 0)
      ) |>
      dplyr::arrange(proprietario, imovel)
  })
  
  # ── UI: body gerencial ──────────────────────────────────────
  output$body_gerencial <- renderUI({
    d <- rv$app_data
    if (length(d) == 0) return(div(class="empty-state", h3("Carregando..."), p("Aguarde a sincronização.")))
    flat <- build_carteira_flat(d)
    if (nrow(flat) == 0) return(div(class="empty-state", h3("Sem dados.")))
    meses_disp <- sort(unique(substr(as.character(flat$competencia), 1, 7)), decreasing = TRUE)
    meses_lbl  <- setNames(meses_disp, {
      dt <- suppressWarnings(as.Date(paste0(meses_disp, "-01")))
      ifelse(is.na(dt), meses_disp, fmt_mes_pt(dt))
    })
    tagList(
      div(class = "ger-filter-bar",
          div(class = "ger-filter-lbl", "MÊS"),
          selectInput("ger_mes", NULL, choices = meses_lbl, selected = meses_disp[1], width = "170px"),
          div(class = "ger-filter-lbl", "BUSCAR"),
          # Campo de busca sem oninput — usa debounce via JS puro, NÃO dispara renderUI
          tags$input(type = "text", id = "ger_search_input", class = "ger-search",
                     placeholder = "Imóvel ou proprietário..."),
          tags$button("Publicar revisados", id = "ger_pub_todos_btn", type = "button",
                      class = "ger-btn-pub-all",
                      onclick = "Shiny.setInputValue('ger_pub_todos', Math.random(), {priority:'event'}); return false;")
      ),
      uiOutput("ger_kpis"),
      uiOutput("ger_cards")
    )
  })
  
  # ── KPIs ───────────────────────────────────────────────────
  output$ger_kpis <- renderUI({
    df <- ger_dados(); req(df, nrow(df) > 0)
    n  <- nrow(df)
    n_pub <- sum(sapply(seq_len(n), function(i) .ger_is_pub(.ger_key(df$competencia[i], df$imovel[i]))))
    n_rev <- sum(sapply(seq_len(n), function(i) .ger_status(.ger_key(df$competencia[i], df$imovel[i])) == "rev"))
    div(class = "ger-header",
        div(div(class = "ger-header-title", "Revisão Gerencial"),
            div(class = "ger-header-sub",
                paste0(n, " apartamentos · ",
                       fmt_mes_pt(suppressWarnings(as.Date(paste0(df$competencia[1], "-01"))))))),
        div(class = "ger-stats",
            div(class="ger-stat", div(class="ger-stat-val",style="color:#f59e0b;",n-n_pub-n_rev), div(class="ger-stat-lbl","Pendentes")),
            div(class="ger-stat", div(class="ger-stat-val",style="color:#60a5fa;",n_rev),          div(class="ger-stat-lbl","Em Revisão")),
            div(class="ger-stat", div(class="ger-stat-val",n_pub),                                  div(class="ger-stat-lbl","Publicados")),
            div(class="ger-stat", div(class="ger-stat-val",brl(sum(df$receita_bruta))),              div(class="ger-stat-lbl","Receita Total")),
            div(class="ger-stat", div(class="ger-stat-val",
                                      style=if(sum(df$resultado_liq)>=0)"color:#34d99e;" else "color:#f87171;",
                                      brl(sum(df$resultado_liq))),                                                          div(class="ger-stat-lbl","Resultado Líq."))
        )
    )
  })
  
  # ── Cards: renderUI só re-renderiza quando mes muda (não na busca) ─
  # A busca é feita inteiramente via JS — sem re-render do Shiny.
  output$ger_cards <- renderUI({
    df <- ger_dados(); req(df, nrow(df) > 0)
    # Snapshot do estado de edições para evitar re-render desnecessário
    edits_snap <- rv$ger_edits
    pub_snap   <- rv$ger_pub
    
    fmt_diff <- function(orig, edit) {
      d <- round(edit - orig, 2)
      if (abs(d) < 0.01) return(div(class="ger-field-diff neu", "—"))
      div(class=paste("ger-field-diff", if(d<0)"pos" else "neg"),
          if(d>0) paste0("+",brl(d)) else brl(d))
    }
    
    cards <- lapply(seq_len(nrow(df)), function(i) {
      row  <- df[i,]
      mes  <- substr(as.character(row$competencia), 1, 7)
      apto <- row$imovel
      prop <- row$proprietario %||% ""
      key  <- .ger_key(mes, apto)
      st   <- .ger_status(key)
      pub  <- .ger_is_pub(key)
      
      taxa_ed     <- .ger_get(key, "taxa_adm",     row$taxa_adm)
      taxa_pct_ed <- {
        pct_salvo <- .ger_get(key, "taxa_adm_pct", NULL)
        if (!is.null(pct_salvo) && !is.na(pct_salvo)) as.numeric(pct_salvo)
        else if (row$receita_bruta > 0) round(taxa_ed / row$receita_bruta * 100, 2)
        else 0
      }
      man_ed  <- .ger_get(key, "manutencao",     row$manutencao_total)
      rep_ed  <- .ger_get(key, "reposicao",      row$reposicao_total)
      des_ed  <- .ger_get(key, "despesas",       row$despesas_total)
      nota_ed <- .ger_get(key, "nota",           "")
      # Receita ajustada: NULL = usa original
      rec_aj  <- .ger_get(key, "receita_ajuste", NULL)
      rec_ed  <- if (!is.null(rec_aj) && !is.na(rec_aj)) as.numeric(rec_aj)
      else as.numeric(row$receita_bruta)
      res_ed  <- rec_ed - taxa_ed - (man_ed + rep_ed + des_ed)
      
      ks       <- gsub("[^a-zA-Z0-9]","_",key)
      body_id  <- paste0("gbody_", ks)
      pill_lbl <- switch(st, pub="Publicado", rev="Em Revisão", "Pendente")
      pill_cls <- switch(st, pub="pill-pub",  rev="pill-rev",   "pill-pen")
      card_cls <- paste("ger-apto-card", st)
      # data-search para filtro JS
      search_str <- tolower(paste(apto, prop))
      
      div(class = card_cls,
          `data-search` = search_str,
          
          # ── Cabeçalho ─────────────────────────────────
          tags$div(
            class   = "ger-apto-hdr",
            onclick = sprintf("gerToggle('%s')", body_id),
            div(style="flex:1;min-width:0;",
                div(class="ger-apto-nome", apto),
                div(class="ger-apto-prop", prop)),
            div(style="display:flex;gap:20px;align-items:center;flex-shrink:0;",
                div(div(class="ger-col-lbl","Receita"),   div(class="ger-apto-rec", brl(row$receita_bruta))),
                div(div(class="ger-col-lbl","Resultado"),
                    div(class=paste("ger-apto-res",if(res_ed>=0)"pos"else"neg"), brl(res_ed))),
                div(div(class="ger-col-lbl","Status"),
                    div(class=paste("ger-status-pill",pill_cls), pill_lbl)),
                div(id=paste0("chev_",ks), class="ger-chevron", "▼")
            )
          ),
          
          # ── Corpo (colapsado) ──────────────────────────
          div(id=body_id, style="display:none;",
              div(class="ger-body",
                  
                  # Resumo numérico
                  div(class="ger-sumario",
                      div(class="ger-sum-item", div(class="ger-sum-lbl","Receita"),
                          div(class="ger-sum-val", brl(rec_ed),
                              if (!is.null(rec_aj) && !is.na(rec_aj))
                                div(style="font-size:9px;color:#9aa5b4;margin-top:1px;",
                                    paste0("original: ", brl(row$receita_bruta)))
                          )),
                      div(class="ger-sum-item", div(class="ger-sum-lbl","Taxa Adm"),     div(class="ger-sum-val r",  brl(taxa_ed))),
                      div(class="ger-sum-item", div(class="ger-sum-lbl","Manutenção"),   div(class="ger-sum-val r",  brl(man_ed))),
                      div(class="ger-sum-item", div(class="ger-sum-lbl","Reposição"),    div(class="ger-sum-val r",  brl(rep_ed))),
                      div(class="ger-sum-item", div(class="ger-sum-lbl","Despesas"),     div(class="ger-sum-val r",  brl(des_ed))),
                      div(class="ger-sum-item", div(class="ger-sum-lbl","Resultado"),
                          div(class=if(res_ed>=0)"ger-sum-val g"else"ger-sum-val r",
                              id=paste0("ger_res_live_",ks),   # GER-1: target do JS de recálculo em tempo real
                              brl(res_ed)))
                  ),
                  
                  # Campos editáveis
                  div(class="ger-section",
                      div(class="ger-sec-title","Ajustes"),
                      # Receita — campo sensível com indicação de valor original
                      div(style=paste0("display:grid;grid-template-columns:1.8fr 1fr 1.1fr 64px;",
                                       "align-items:center;gap:10px;padding:8px 0;",
                                       "border-bottom:2px solid #f0f4f8;margin-bottom:4px;"),
                          div(style="display:flex;flex-direction:column;",
                              div(class="ger-field-lbl", "Receita Bruta"),
                              div(style="font-size:10px;color:#d97706;font-weight:600;margin-top:2px;",
                                  "Ajuste pontual — não altera a base do Drive")),
                          div(style="display:flex;flex-direction:column;",
                              div(class="ger-field-orig", brl(row$receita_bruta)),
                              div(style="font-size:9px;color:#9aa5b4;","valor original")),
                          div(class="ger-field-input",
                              numericInput(paste0("ger_rec_",ks), NULL,
                                           value = round(rec_ed, 2),
                                           min = 0, step = 0.01, width = "100%")),
                          fmt_diff(row$receita_bruta, rec_ed)
                      ),
                      # Taxa Administrativa — % é o campo principal, R$ é calculado automaticamente
                      div(class="ger-field-row",
                          div(style="display:flex;flex-direction:column;gap:2px;",
                              div(class="ger-field-lbl","Taxa Administrativa"),
                              div(style="font-size:10px;color:#9aa5b4;",
                                  paste0("Original: ", brl(row$taxa_adm), " (",
                                         round(if(row$receita_bruta>0) row$taxa_adm/row$receita_bruta*100 else 0, 1), "%)"))),
                          div(class="ger-field-input", style="display:flex;align-items:center;gap:6px;",
                              div(style="display:flex;flex-direction:column;gap:2px;",
                                  tags$label(style="font-size:9px;color:#6b7280;font-weight:700;","%"),
                                  numericInput(paste0("ger_taxa_pct_",ks), NULL,
                                               value=round(taxa_pct_ed, 2),
                                               min=0, max=100, step=0.01, width="80px")),
                              div(style="color:#9aa5b4;font-size:18px;line-height:1;margin-top:12px;","→"),
                              div(style="display:flex;flex-direction:column;gap:2px;",
                                  tags$label(style="font-size:9px;color:#6b7280;font-weight:700;","R$ calculado"),
                                  numericInput(paste0("ger_taxa_",ks), NULL,
                                               value=round(taxa_ed,2), min=0, step=0.01, width="110px"))),
                          fmt_diff(row$taxa_adm, taxa_ed)),
                      div(class="ger-field-row",
                          div(class="ger-field-lbl","Manutenção"),
                          div(class="ger-field-orig", brl(row$manutencao_total)),
                          div(class="ger-field-input", numericInput(paste0("ger_man_",ks), NULL, value=round(man_ed,2), min=0, step=0.01, width="100%")),
                          fmt_diff(row$manutencao_total, man_ed)),
                      div(class="ger-field-row",
                          div(class="ger-field-lbl","Reposição"),
                          div(class="ger-field-orig", brl(row$reposicao_total)),
                          div(class="ger-field-input", numericInput(paste0("ger_rep_",ks), NULL, value=round(rep_ed,2), min=0, step=0.01, width="100%")),
                          fmt_diff(row$reposicao_total, rep_ed)),
                      div(class="ger-field-row",
                          div(class="ger-field-lbl","Despesas Fixas"),
                          div(class="ger-field-orig", brl(row$despesas_total)),
                          div(class="ger-field-input", numericInput(paste0("ger_des_",ks), NULL, value=round(des_ed,2), min=0, step=0.01, width="100%")),
                          fmt_diff(row$despesas_total, des_ed))
                  ),
                  
                  # Nota interna
                  div(class="ger-section",
                      div(class="ger-sec-title","Nota interna"),
                      tags$textarea(id=paste0("ger_nota_",ks), class="ger-nota",
                                    placeholder="Observação interna (não visível ao proprietário)", nota_ed)
                  ),
                  
                  # Confirmação de publicação
                  if (pub) div(class="ger-pub-confirm",
                               paste0("Publicado em ", rv$ger_pub[[key]], " — dados disponíveis para o proprietário.")),
                  
                  # Botões
                  div(class="ger-actions",
                      tags$button("Restaurar", type = "button",
                                  class="ger-btn ger-btn-secondary",
                                  onclick=sprintf("Shiny.setInputValue('ger_rst_%s', Math.random(), {priority:'event'}); return false;", ks)),
                      tags$button("Salvar rascunho", type = "button",
                                  class="ger-btn ger-btn-draft",
                                  onclick=sprintf("Shiny.setInputValue('ger_save_%s', Math.random(), {priority:'event'}); return false;", ks)),
                      tags$button(if(pub)"Republicar" else "Publicar", type = "button",
                                  class="ger-btn ger-btn-pub",
                                  onclick=sprintf("Shiny.setInputValue('ger_pub_%s', Math.random(), {priority:'event'}); return false;", ks))
                  )
              )
          )
      )
    })
    
    tagList(
      # JS: toggle isolado (não dispara Shiny), busca local sem re-render
      tags$script(HTML("
        function gerToggle(id) {
          var body = document.getElementById(id);
          var ks   = id.replace('gbody_','');
          var chev = document.getElementById('chev_' + ks);
          if (!body) return;
          if (body.style.display === 'none' || body.style.display === '') {
            body.style.display = 'block';
            if (chev) chev.classList.add('open');
          } else {
            body.style.display = 'none';
            if (chev) chev.classList.remove('open');
          }
        }
        // Busca local: filtra cards sem re-render Shiny
        document.addEventListener('input', function(e) {
          if (e.target && e.target.id === 'ger_search_input') {
            var q = e.target.value.toLowerCase();
            document.querySelectorAll('.ger-apto-card').forEach(function(card) {
              var s = (card.getAttribute('data-search') || '').toLowerCase();
              card.style.display = (q === '' || s.indexOf(q) >= 0) ? '' : 'none';
            });
          }
        });
        // GER-1: Recalcula resultado em tempo real ao editar qualquer campo numérico do card
        document.addEventListener('input', function(e) {
          var el = e.target;
          if (!el || !el.id) return;
          var match = el.id.match(/^ger_(rec|taxa|man|rep|des|taxa_pct)_(.+)$/);
          if (!match) return;
          var field = match[1];
          var ks    = match[2];
          function getVal(prefix) {
            var inp = document.getElementById('ger_' + prefix + '_' + ks);
            return inp ? (parseFloat(inp.value) || 0) : 0;
          }
          // Sincronizacao taxa: % e o campo principal, R$ e calculado automaticamente
          if (field === 'taxa_pct') {
            // Admin editou o %: recalcula R$ = receita x pct / 100
            var rec2   = getVal('rec');
            var newTax = Math.round((parseFloat(el.value) || 0) * rec2) / 100;
            // Atualiza o DOM do numericInput R$ visualmente
            var taxEl  = document.getElementById('ger_taxa_' + ks);
            if (taxEl) taxEl.value = newTax.toFixed(2);
            // Comunica o novo valor R$ ao Shiny via setInputValue
            // (mais confiável que dispatchEvent em numericInput bindingsdo Shiny)
            Shiny.setInputValue('ger_taxa_' + ks, newTax, {priority: 'event'});
          } else if (field === 'taxa') {
            // Admin editou o R$ diretamente: atualiza % como referencia visual
            var rec3   = getVal('rec');
            var pct    = rec3 > 0 ? ((parseFloat(el.value) || 0) / rec3 * 100) : 0;
            var pctEl  = document.getElementById('ger_taxa_pct_' + ks);
            if (pctEl) pctEl.value = pct.toFixed(2);
            // Comunica o novo % ao Shiny
            Shiny.setInputValue('ger_taxa_pct_' + ks, pct, {priority: 'event'});
          } else if (field === 'rec') {
            // Receita mudou: mantém o % e recalcula R$ da taxa
            var pctAtual = getVal('taxa_pct');
            var rec4     = parseFloat(el.value) || 0;
            var newTax4  = Math.round(pctAtual * rec4) / 100;
            var taxEl4   = document.getElementById('ger_taxa_' + ks);
            if (taxEl4) taxEl4.value = newTax4.toFixed(2);
            Shiny.setInputValue('ger_taxa_' + ks, newTax4, {priority: 'event'});
          }
          // GER-1: Recalcula resultado líquido no header do card
          var rec  = getVal('rec');
          var taxa = getVal('taxa');
          var man  = getVal('man');
          var rep  = getVal('rep');
          var des  = getVal('des');
          var res  = rec - taxa - man - rep - des;
          var resEl = document.getElementById('ger_res_live_' + ks);
          if (resEl) {
            resEl.textContent = 'R$ ' + res.toLocaleString('pt-BR', {minimumFractionDigits:2, maximumFractionDigits:2});
            resEl.className = res >= 0 ? 'ger-sum-val g' : 'ger-sum-val r';
          }
        });
        // Captura textarea nota
        document.addEventListener('change', function(e) {
          if (e.target && e.target.classList.contains('ger-nota')) {
            Shiny.setInputValue(e.target.id, e.target.value, {priority:'event'});
          }
        });
      ")),
      !!!cards
    )
  })
  
  
  # ── Observers: toggle expand ────────────────────────────────
  # (tratado via JS puro acima — sem observer R necessário)
  
  # ── Observer: Salvar rascunho (dinâmico) ────────────────────
  observe({
    df <- ger_dados(); req(df, nrow(df) > 0)
    lapply(seq_len(nrow(df)), function(i) {
      mes  <- as.character(df$competencia[i])
      apto <- df$imovel[i]
      key  <- .ger_key(mes, apto)
      ks   <- gsub("[^a-zA-Z0-9]", "_", key)
      
      id_save <- paste0("ger_save_",  ks)
      id_rec  <- paste0("ger_rec_",   ks)   # GER-5: definido aqui para estar no escopo de Restaurar e Publicar
      id_taxa <- paste0("ger_taxa_",  ks)
      id_man  <- paste0("ger_man_",   ks)
      id_rep  <- paste0("ger_rep_",   ks)
      id_des  <- paste0("ger_des_",   ks)
      id_nota <- paste0("ger_nota_",  ks)
      id_rst  <- paste0("ger_rst_",   ks)
      id_pub  <- paste0("ger_pub_",   ks)
      
      # Salvar rascunho
      observeEvent(input[[id_save]], {
        taxa_val_s  <- as.numeric(input[[id_taxa]] %||% df$taxa_adm[i])
        id_taxa_pct <- paste0("ger_taxa_pct_", ks)
        taxa_pct_s  <- tryCatch(as.numeric(input[[id_taxa_pct]]),
                                error = function(e) {
                                  if (df$receita_bruta[i] > 0)
                                    round(taxa_val_s / df$receita_bruta[i] * 100, 2)
                                  else 0
                                })
        edits <- list(
          taxa_adm     = taxa_val_s,
          taxa_adm_pct = taxa_pct_s,
          manutencao   = as.numeric(input[[id_man]]  %||% df$manutencao_total[i]),
          reposicao    = as.numeric(input[[id_rep]]  %||% df$reposicao_total[i]),
          despesas     = as.numeric(input[[id_des]]  %||% df$despesas_total[i]),
          nota         = as.character(input[[id_nota]] %||% "")
        )
        rv$ger_edits[[key]] <- edits
        # Grava rascunho no SQLite (publicado = FALSE)
        .ger_save_sqlite(mes, apto, df$cpf_cnpj[i], edits, publicado = FALSE)
        showNotification(paste0("Rascunho salvo — ", apto),
                         type = "message", duration = 3)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      # Restaurar originais
      observeEvent(input[[id_rst]], {
        rv$ger_edits[[key]] <- NULL
        rv$ger_pub[[key]]   <- NULL
        # Remove do SQLite
        tryCatch(ger_delete_ajuste(mes, apto), error = function(e) NULL)
        pct_orig <- if (df$receita_bruta[i] > 0)
          round(df$taxa_adm[i] / df$receita_bruta[i] * 100, 2) else 0
        updateNumericInput(session, id_rec,  value = round(df$receita_bruta[i], 2))
        updateNumericInput(session, id_taxa, value = round(df$taxa_adm[i], 2))
        updateNumericInput(session, paste0("ger_taxa_pct_", ks), value = pct_orig)
        updateNumericInput(session, id_man,  value = round(df$manutencao_total[i], 2))
        updateNumericInput(session, id_rep,  value = round(df$reposicao_total[i], 2))
        updateNumericInput(session, id_des,  value = round(df$despesas_total[i], 2))
        showNotification(paste0("Restaurado — ", apto),
                         type = "warning", duration = 3)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      # Publicar
      observeEvent(input[[id_pub]], {
        # Salva edições antes de publicar (inclui ajuste de receita)
        rec_val_p  <- as.numeric(input[[id_rec]] %||% df$receita_bruta[i])
        rec_aj_p   <- if (abs(rec_val_p - as.numeric(df$receita_bruta[i])) > 0.01)
          rec_val_p else NULL
        taxa_val_p   <- as.numeric(input[[id_taxa]] %||% df$taxa_adm[i])
        id_taxa_pct2 <- paste0("ger_taxa_pct_", ks)
        taxa_pct_p   <- tryCatch(as.numeric(input[[id_taxa_pct2]]),
                                 error = function(e) {
                                   if (df$receita_bruta[i] > 0)
                                     round(taxa_val_p / df$receita_bruta[i] * 100, 2)
                                   else 0
                                 })
        edits <- list(
          taxa_adm       = taxa_val_p,
          taxa_adm_pct   = taxa_pct_p,
          manutencao     = as.numeric(input[[id_man]]  %||% df$manutencao_total[i]),
          reposicao      = as.numeric(input[[id_rep]]  %||% df$reposicao_total[i]),
          despesas       = as.numeric(input[[id_des]]  %||% df$despesas_total[i]),
          receita_ajuste = rec_aj_p,
          rec_original   = as.numeric(df$receita_bruta[i]),
          nota           = as.character(input[[id_nota]] %||% "")
        )
        rv$ger_edits[[key]] <- edits
        ts <- format(Sys.time(), "%d/%m/%Y %H:%M")
        rv$ger_pub[[key]]   <- ts
        
        # Write-back para Google Sheets (aba ger_ajustes no DB_MASTER)
        cpf <- df$cpf_cnpj[i]
        # Grava no SQLite como publicado (publicado = TRUE)
        .ger_save_sqlite(mes, apto, cpf, edits, publicado = TRUE)
        
        # Propaga os valores revisados para rv$app_data do proprietário
        cpf <- df$cpf_cnpj[i]
        if (!is.null(rv$app_data[[cpf]]$receitas)) {
          idx_r <- which(rv$app_data[[cpf]]$receitas$competencia == mes &
                           rv$app_data[[cpf]]$receitas$imovel == apto)
          if (length(idx_r) > 0) {
            # Receita: aplica ajuste se foi modificada
            if (!is.null(edits$receita_ajuste) && !is.na(edits$receita_ajuste))
              rv$app_data[[cpf]]$receitas$receita_bruta[idx_r] <- edits$receita_ajuste
            rv$app_data[[cpf]]$receitas$taxa_adm[idx_r]         <- edits$taxa_adm
            rv$app_data[[cpf]]$receitas$manutencao_total[idx_r] <- edits$manutencao
            rv$app_data[[cpf]]$receitas$reposicao_total[idx_r]  <- edits$reposicao
            rv$app_data[[cpf]]$receitas$despesas_total[idx_r]   <- edits$despesas
            rv$app_data[[cpf]]$receitas$outros_custos[idx_r] <-
              edits$manutencao + edits$reposicao + edits$despesas
            rv$app_data[[cpf]]$receitas$resultado_liq[idx_r] <-
              rv$app_data[[cpf]]$receitas$receita_bruta[idx_r] -
              edits$taxa_adm -
              (edits$manutencao + edits$reposicao + edits$despesas)
          }
        }
        
        showNotification(
          paste0("Publicado — ", apto, " (", df$proprietario[i], ")"),
          type = "message", duration = 5
        )
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    })
  })
  
  # ── Observer: Publicar todos revisados ──────────────────────
  observeEvent(input$ger_pub_todos, {
    df <- ger_dados(); req(df, nrow(df) > 0)
    n_pub <- 0
    for (i in seq_len(nrow(df))) {
      key <- .ger_key(df$competencia[i], df$imovel[i])
      if (.ger_status(key) == "rev" && !.ger_is_pub(key)) {
        ts_all <- format(Sys.time(), "%d/%m/%Y %H:%M")
        rv$ger_pub[[key]] <- ts_all
        # Propaga valores salvos em rascunho e faz write-back
        edits <- rv$ger_edits[[key]]
        if (!is.null(edits)) {
          cpf <- df$cpf_cnpj[i]
          .ger_save_sqlite(as.character(df$competencia[i]), df$imovel[i], cpf, edits, publicado = TRUE)
          cpf <- df$cpf_cnpj[i]
          mes <- as.character(df$competencia[i])
          apto <- df$imovel[i]
          if (!is.null(rv$app_data[[cpf]]$receitas)) {
            idx_r <- which(rv$app_data[[cpf]]$receitas$competencia == mes &
                             rv$app_data[[cpf]]$receitas$imovel == apto)
            if (length(idx_r) > 0) {
              rv$app_data[[cpf]]$receitas$taxa_adm[idx_r]         <- edits$taxa_adm
              rv$app_data[[cpf]]$receitas$manutencao_total[idx_r] <- edits$manutencao
              rv$app_data[[cpf]]$receitas$reposicao_total[idx_r]  <- edits$reposicao
              rv$app_data[[cpf]]$receitas$despesas_total[idx_r]   <- edits$despesas
              rv$app_data[[cpf]]$receitas$outros_custos[idx_r]    <-
                edits$manutencao + edits$reposicao + edits$despesas
              rv$app_data[[cpf]]$receitas$resultado_liq[idx_r]    <-
                rv$app_data[[cpf]]$receitas$receita_bruta[idx_r] -
                edits$taxa_adm -
                (edits$manutencao + edits$reposicao + edits$despesas)
            }
          }
        }
        n_pub <- n_pub + 1
      }
    }
    if (n_pub > 0)
      showNotification(paste0(n_pub, " apartamento(s) publicados."),
                       type = "message", duration = 5)
    else
      showNotification("Nenhum rascunho em revisão para publicar.",
                       type = "warning", duration = 3)
  }, ignoreInit = TRUE)
  
} # fim server

shinyApp(ui, server)