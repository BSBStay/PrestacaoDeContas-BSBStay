/***********************
 * BSB3 / BSBSTAY — DB_MASTER V5.1

 ************************/

const ROOT_FOLDER_ID = "1753AZxwmyyWYS2oYQPLeMHIz5gM8bscb";
const DEFAULT_COMISSAO_PCT = 0.20;

const CFG = {
  SHEETS: {
    CONFIG:       "CONFIG",
    LOG:          "log_ingestao",
    PEND:         "PENDENTES_map_alias",
    MAP_ALIAS:    "map_alias_imovel",
    DIM_IMOVEL:   "dim_imovel",
    DIM_PROP:     "dim_proprietario",
    FACT_RES:     "fact_reservas",
    FACT_MAN:     "fact_manutencao",
    FACT_REP:     "fact_reposicao",
    FACT_DES:     "fact_despesas",
    FACT_REPASSE: "fact_repasse",
    FACT_DEV:     "fact_devolucao_limpeza",
    AGG:          "agg_prestacao_contas",
  },
  DRIVE: {
    SOURCES_FOLDER: "01_Fontes",
    RESERVAS:       ["Reservas"],
    MANUTENCAO:     ["Manutencao", "Manutenção"],
    REPOSICAO:      ["Reposicao", "Reposição", "Resposicao", "Itens de reposicao", "Itens de Reposicao", "Itens de Reposição"],
    PAGAMENTOS:     ["Pagamentos"],
    // Dez/2025: pasta nomeada "Proprietários" (com acento til)
    // Jan/2026+: pasta nomeada "Proprietarios" (sem acento)
    PROPRIETARIOS:  ["Proprietarios", "Proprietários"],
    // Nome exato no Drive (Mai/2026): "Devolução da Taxa de Limpeza"
    // Aliases cobrem variações com/sem acento e com/sem "da"
    DEVOLUCAO:      ["Devolução da Taxa de Limpeza", "Devolucao da Taxa de Limpeza",
                     "Devolução taxa de limpeza",    "Devolucao taxa de limpeza",
                     "Devolução",                    "Devolucao"],
  }
};

// ─────────────────────────────────────────────────────────────────────────────
// UI
// ─────────────────────────────────────────────────────────────────────────────
function onOpen() {
  SpreadsheetApp.getUi()
    .createMenu("BSB3 • Atualização")
    .addItem("▶ Atualizar mês atual",             "runMonthlyUpdateFixedRoot")
    .addItem("⏭ Atualizar e avançar competência", "runMonthlyUpdateAndAdvance")
    .addSeparator()
    .addItem("Recalcular agregados",              "rebuildAggPrestacaoContas")
    .addSeparator()
    .addItem("🗑 Limpar TODAS as FACTs",          "limparTodasFacts")
    .addItem("♻ Reprocessar TODOS os meses",      "reprocessarTodosMeses")
    .addSeparator()
    .addItem("⏰ Ativar atualização automática diária",   "ativarTriggerDiario")
    .addItem("⏹ Desativar atualização automática",        "desativarTriggerDiario")
    .addItem("ℹ Ver status do trigger automático",         "verStatusTrigger")
    .addSeparator()
    .addItem("🔗 Ver link de atualização (Web App)",       "verLinkWebApp")
    .addSeparator()
    .addItem("🔍 Sugerir aliases para pendentes",          "sugerirAliases")
    .addItem("✅ Confirmar aliases sugeridos",              "confirmarAliasesSugeridos")
    .addToUi();
}

function runMonthlyUpdateFixedRoot() {
  const tsInicio = new Date();
  ensureAllSheets_();

  const rawComp    = getCompetenciaAtualRaw_();
  const competencia = parseCompetencia_(rawComp);
  if (!competencia) throw new Error(`CONFIG: competencia_atual inválida: "${rawComp}". Use AAAA-MM.`);

  const log = { status:"OK", msg:"", res:0, man:0, rep:0, des:0, repasse:0, dedup_res:0, dedup_des:0, pendentes:0 };

  try {
    const monthFolder   = getMonthFolderRobust_(ROOT_FOLDER_ID, competencia);
    const sourcesFolder = getChildFolderByName_(monthFolder, CFG.DRIVE.SOURCES_FOLDER);

    // 1) DIMs + alias
    buildDimsAndAliasFromProprietarios_(sourcesFolder, competencia);

    // 1.5) CORREÇÃO APPEND-ONLY: remove linhas da competência atual ANTES
    // de carregar o dedup. Isso garante que exclusões feitas na planilha
    // fonte (ex: OS de manutenção removida) sejam refletidas — o item
    // excluído não será reinserido porque não está mais na fonte, e como
    // o dedup é recarregado DEPOIS desta limpeza, ele não "lembra" da
    // linha antiga e não a recria. Meses anteriores (já encerrados)
    // permanecem intactos, pois o filtro é estritamente por competência.
    // FACT_RES incluída: reservas removidas na fonte desaparecem no painel.
    // Também resolve os 6 aptos multi-dono: chaves antigas (sem propertyId)
    // são apagadas e reinseridas no formato novo (com propertyId) a cada execução.
    removeRowsByCompetencia_(CFG.SHEETS.FACT_RES,     competencia);
    removeRowsByCompetencia_(CFG.SHEETS.FACT_MAN,     competencia);
    removeRowsByCompetencia_(CFG.SHEETS.FACT_REP,     competencia);
    removeRowsByCompetencia_(CFG.SHEETS.FACT_DES,     competencia);
    removeRowsByCompetencia_(CFG.SHEETS.FACT_REPASSE, competencia);
    removeRowsByCompetencia_(CFG.SHEETS.FACT_DEV,     competencia);

    // 2) Estruturas compartilhadas carregadas UMA vez (DEPOIS da limpeza acima)
    const dedupe    = loadExistingKeys_();
    const aliasMap  = buildAliasMap_();
    const canonMap  = buildCanonMap_();
    const pendBatch = [];

    // 3) FACTs
    const r = ingestReservas_(competencia, sourcesFolder, dedupe, aliasMap, canonMap, pendBatch);
    log.res = r.inserted; log.dedup_res = r.duplicated;

    log.man     = ingestManutencao_(competencia, sourcesFolder, dedupe, aliasMap, canonMap, pendBatch);
    log.rep     = ingestReposicao_(competencia, sourcesFolder, dedupe, aliasMap, canonMap, pendBatch);
    log.repasse = ingestRepasse_(competencia, sourcesFolder, dedupe, aliasMap, canonMap, pendBatch);

    const d = ingestDespesas_(competencia, sourcesFolder, dedupe, aliasMap, canonMap, pendBatch);
    log.des = d.inserted; log.dedup_des = d.duplicated;

    // Devolução de Taxa de Limpeza — pasta opcional: se não existir no mês, ignora.
    log.dev = ingestDevolucao_(competencia, sourcesFolder, dedupe, aliasMap, canonMap, pendBatch);

    // 4) Flush pendentes em lote
    if (pendBatch.length) {
      appendRows_(CFG.SHEETS.PEND, pendBatch);
      log.pendentes = pendBatch.length;
    }

    // 5) AGG
    rebuildAggPrestacaoContas(competencia);   // cirúrgico: só recalcula este mês

    log.msg = "Atualização concluída";
    SpreadsheetApp.getUi().alert(
      `✅ ${competencia}\n` +
      `Reservas: ${log.res} (dedup: ${log.dedup_res})\n` +
      `Manutenção: ${log.man} | Reposição: ${log.rep}\n` +
      `Despesas: ${log.des} (dedup: ${log.dedup_des})\n` +
      `Repasse: ${log.repasse} | Pendentes: ${log.pendentes}`
    );
  } catch (err) {
    log.status = "ERRO";
    log.msg    = String(err && err.message ? err.message : err);
    throw err;
  } finally {
    appendLogV5_(tsInicio, new Date(), competencia, log);
  }
}

function runMonthlyUpdateAndAdvance() {
  runMonthlyUpdateFixedRoot();
  advanceCompetenciaAtual_();
}

/**
 * Descobre automaticamente todos os meses disponíveis no Drive
 * (pastas AAAA/AAAA-MM dentro de ROOT_FOLDER_ID) sem lista hardcoded.
 */
function descobrirMesesNoDrive_() {
  const root = DriveApp.getFolderById(ROOT_FOLDER_ID);
  const meses = [];
  const yearIt = root.getFolders();
  while (yearIt.hasNext()) {
    const yearFolder = yearIt.next();
    const yearName = yearFolder.getName().trim();
    if (!/^\d{4}$/.test(yearName)) continue;
    const mesIt = yearFolder.getFolders();
    while (mesIt.hasNext()) {
      const mesFolder = mesIt.next();
      const mesName = mesFolder.getName().trim();
      if (/^\d{4}-\d{2}$/.test(mesName)) meses.push(mesName);
    }
  }
  return meses.sort();
}

function reprocessarTodosMeses() {
  const meses = descobrirMesesNoDrive_();
  if (meses.length === 0) {
    SpreadsheetApp.getUi().alert("Nenhum mês encontrado no Drive.");
    return;
  }
  const resultados = [];

  for (const mes of meses) {
    try {
      setCompetenciaAtual_(mes);
      runMonthlyUpdateFixedRoot();
      resultados.push(`✅ ${mes}: OK`);
    } catch (err) {
      resultados.push(`❌ ${mes}: ${err.message}`);
    }
  }

  SpreadsheetApp.getUi().alert(
    "Reprocessamento completo:\n\n" + resultados.join("\n")
  );
}

// ─────────────────────────────────────────────────────────────────────────────
// Trigger automático diário
// ─────────────────────────────────────────────────────────────────────────────
const TRIGGER_HANDLER = "reprocessarTodosMeses";
const TRIGGER_HORA    = 6; // 06h00 horário do projeto (America/Sao_Paulo)

/**
 * Ativa um trigger diário que reprocessa todos os meses automaticamente.
 * Idempotente: remove triggers antigos antes de criar um novo.
 * A re-execução diária garante que qualquer alteração nas planilhas base
 * (Drive) seja refletida no DB_MASTER sem intervenção manual.
 */
function ativarTriggerDiario() {
  // Remove triggers existentes para evitar duplicatas
  ScriptApp.getProjectTriggers()
    .filter(t => t.getHandlerFunction() === TRIGGER_HANDLER)
    .forEach(t => ScriptApp.deleteTrigger(t));

  ScriptApp.newTrigger(TRIGGER_HANDLER)
    .timeBased()
    .everyDays(1)
    .atHour(TRIGGER_HORA)
    .create();

  SpreadsheetApp.getUi().alert(
    "✅ Atualização automática ativada!\n\n" +
    `Todos os meses serão reprocessados diariamente às ${TRIGGER_HORA}h.\n` +
    "Qualquer alteração nas planilhas do Drive será refletida automaticamente."
  );
}

/**
 * Remove o trigger automático diário.
 */
function desativarTriggerDiario() {
  const triggers = ScriptApp.getProjectTriggers()
    .filter(t => t.getHandlerFunction() === TRIGGER_HANDLER);

  if (triggers.length === 0) {
    SpreadsheetApp.getUi().alert("Nenhum trigger automático ativo no momento.");
    return;
  }
  triggers.forEach(t => ScriptApp.deleteTrigger(t));
  SpreadsheetApp.getUi().alert("⏹ Atualização automática desativada.");
}

/**
 * Mostra o status atual do trigger automático.
 */
function verStatusTrigger() {
  const triggers = ScriptApp.getProjectTriggers()
    .filter(t => t.getHandlerFunction() === TRIGGER_HANDLER);

  if (triggers.length === 0) {
    SpreadsheetApp.getUi().alert(
      "Status: INATIVO\n\nUse 'Ativar atualização automática diária' para habilitar."
    );
  } else {
    SpreadsheetApp.getUi().alert(
      `Status: ATIVO ✅\n\n${triggers.length} trigger(s) configurado(s).\n` +
      `Execução diária às ${TRIGGER_HORA}h — reprocessa todos os meses encontrados no Drive.`
    );
  }
}

function verLinkWebApp() {
  const url = ScriptApp.getService().getUrl();
  if (!url) {
    SpreadsheetApp.getUi().alert(
      "Web App ainda não publicado.\n\n" +
      "Acesse: Apps Script → Implantar → Novo implantação → Web App\n" +
      "Executar como: Eu | Acesso: Qualquer pessoa"
    );
    return;
  }
  SpreadsheetApp.getUi().alert(
    "🔗 Link de atualização automática:\n\n" + url +
    "\n\nSalve este link nos favoritos. Ao clicar, o sistema detecta\n" +
    "automaticamente quais meses foram alterados e reprocessa só eles.\n\n" +
    "Para forçar todos os meses: " + url + "?todos=1\n" +
    "Para um mês específico: " + url + "?mes=2026-06"
  );
}

// ─────────────────────────────────────────────────────────────────────────────
// Web App — endpoint HTTP para reprocessamento sem abrir o Apps Script
// ─────────────────────────────────────────────────────────────────────────────

/**
 * Ponto de entrada HTTP do Web App.
 * Uso:
 *   GET <url>                → detecta meses alterados e reprocessa só eles
 *   GET <url>?mes=2026-06   → força reprocessamento de um mês específico
 *   GET <url>?todos=1       → reprocessa todos os meses (equivale ao menu)
 *
 * Para publicar: Apps Script → Implantar → Novo implantação → Web App
 *   Executar como: Eu (conta do projeto)
 *   Quem tem acesso: Qualquer pessoa (ou "Qualquer pessoa com conta Google" se preferir)
 */
function doGet(e) {
  const params  = e && e.parameter ? e.parameter : {};
  const resultados = [];
  let   titulo  = "";

  try {
    if (params.mes) {
      // Reprocessa mês específico
      const comp = parseCompetencia_(params.mes);
      if (!comp) throw new Error(`Parâmetro mes inválido: "${params.mes}". Use AAAA-MM.`);
      titulo = `Reprocessando ${comp}...`;
      setCompetenciaAtual_(comp);
      runMonthlyUpdateFixedRoot();
      resultados.push({ mes: comp, status: "✅ OK" });

    } else if (params.todos === "1") {
      // Reprocessa todos os meses do Drive
      titulo = "Reprocessando todos os meses...";
      const meses = descobrirMesesNoDrive_();
      for (const mes of meses) {
        try {
          setCompetenciaAtual_(mes);
          runMonthlyUpdateFixedRoot();
          resultados.push({ mes, status: "✅ OK" });
        } catch(err) {
          resultados.push({ mes, status: `❌ ${err.message}` });
        }
      }

    } else {
      // Detecta automaticamente quais meses foram alterados no Drive
      titulo = "Detectando meses alterados...";
      const mesesAlterados = descobrirMesesAlterados_();
      if (mesesAlterados.length === 0) {
        return _htmlResponse("Nenhuma alteração detectada",
          "<p>Nenhum mês foi modificado desde o último processamento.</p>", []);
      }
      for (const mes of mesesAlterados) {
        try {
          setCompetenciaAtual_(mes);
          runMonthlyUpdateFixedRoot();
          resultados.push({ mes, status: "✅ OK" });
        } catch(err) {
          resultados.push({ mes, status: `❌ ${err.message}` });
        }
      }
    }

    // Grava timestamp da última execução no CONFIG
    gravarUltimaExecucao_();

  } catch(err) {
    return _htmlResponse("Erro no reprocessamento", `<p>❌ ${err.message}</p>`, []);
  }

  return _htmlResponse("Reprocessamento concluído", titulo, resultados);
}

/**
 * Descobre quais meses do Drive foram modificados após o último
 * processamento registrado no CONFIG (chave "ultima_execucao_etl").
 * Compara a data de modificação da pasta mensal com o timestamp salvo.
 */
function descobrirMesesAlterados_() {
  const ultimaExec = lerUltimaExecucao_();
  const root = DriveApp.getFolderById(ROOT_FOLDER_ID);
  const alterados = [];

  const yearIt = root.getFolders();
  while (yearIt.hasNext()) {
    const yearFolder = yearIt.next();
    if (!/^\d{4}$/.test(yearFolder.getName().trim())) continue;
    const mesIt = yearFolder.getFolders();
    while (mesIt.hasNext()) {
      const mesFolder = mesIt.next();
      const mesName = mesFolder.getName().trim();
      if (!/^\d{4}-\d{2}$/.test(mesName)) continue;
      // Verifica se qualquer arquivo dentro da pasta foi modificado após ultimaExec
      if (_pastaFoiAlterada_(mesFolder, ultimaExec)) {
        alterados.push(mesName);
      }
    }
  }
  return alterados.sort();
}

function _pastaFoiAlterada_(folder, desde) {
  // Verifica arquivos na pasta e subpastas recursivamente
  const fileIt = folder.getFiles();
  while (fileIt.hasNext()) {
    if (fileIt.next().getLastUpdated() > desde) return true;
  }
  const subIt = folder.getFolders();
  while (subIt.hasNext()) {
    if (_pastaFoiAlterada_(subIt.next(), desde)) return true;
  }
  return false;
}

function lerUltimaExecucao_() {
  const sh = SpreadsheetApp.getActive().getSheetByName(CFG.SHEETS.CONFIG);
  if (!sh) return new Date(0);
  const lr = sh.getLastRow(), lc = sh.getLastColumn();
  if (lr < 2 || lc < 2) return new Date(0);
  const data = sh.getRange(1, 1, lr, lc).getValues();
  const hdrs = data[0].map(x => String(x).trim().toLowerCase());
  const ki = hdrs.indexOf("chave"), vi = hdrs.indexOf("valor");
  if (ki < 0 || vi < 0) return new Date(0);
  for (let r = 1; r < data.length; r++) {
    if (String(data[r][ki]).trim().toLowerCase() === "ultima_execucao_etl") {
      const v = data[r][vi];
      return v instanceof Date ? v : new Date(String(v));
    }
  }
  return new Date(0);
}

function gravarUltimaExecucao_() {
  const sh = SpreadsheetApp.getActive().getSheetByName(CFG.SHEETS.CONFIG);
  if (!sh) return;
  const lr = sh.getLastRow(), lc = sh.getLastColumn();
  if (lr < 2 || lc < 2) return;
  const data = sh.getRange(1, 1, lr, lc).getValues();
  const hdrs = data[0].map(x => String(x).trim().toLowerCase());
  const ki = hdrs.indexOf("chave"), vi = hdrs.indexOf("valor");
  if (ki < 0 || vi < 0) return;
  for (let r = 1; r < data.length; r++) {
    if (String(data[r][ki]).trim().toLowerCase() === "ultima_execucao_etl") {
      sh.getRange(r + 1, vi + 1).setValue(new Date());
      return;
    }
  }
  // Chave não existe ainda: adiciona nova linha
  sh.appendRow(["ultima_execucao_etl", new Date()]);
}

function _htmlResponse(titulo, subtitulo, resultados) {
  const linhas = resultados.map(r =>
    `<tr><td>${r.mes}</td><td>${r.status}</td></tr>`
  ).join("");
  const tabela = resultados.length > 0
    ? `<table border="1" cellpadding="6" style="border-collapse:collapse">
        <tr><th>Mês</th><th>Status</th></tr>${linhas}</table>`
    : "";
  const html = `<!DOCTYPE html><html><head><meta charset="utf-8">
    <title>BSBStay ETL</title>
    <style>body{font-family:sans-serif;padding:24px;max-width:600px}
    h2{color:#1a5276}td,th{padding:6px 12px}
    tr:nth-child(even){background:#f2f2f2}</style></head><body>
    <h2>BSBStay — Atualização de Dados</h2>
    <p><strong>${titulo}</strong></p>${subtitulo}${tabela}
    <p style="color:#888;font-size:12px">Executado em: ${new Date().toLocaleString("pt-BR")}</p>
    </body></html>`;
  return HtmlService.createHtmlOutput(html)
    .setTitle("BSBStay ETL")
    .setXFrameOptionsMode(HtmlService.XFrameOptionsMode.ALLOWALL);
}

/**
 * Limpa TODAS as linhas de dados de TODAS as tabelas FACT e AGG,
 * preservando apenas os cabeçalhos. Executar ANTES de reprocessar
 * os meses individualmente para garantir que não restem linhas
 * órfãs de execuções anteriores (formato de chave antigo, etc.).
 */
function limparTodasFacts() {
  const ui = SpreadsheetApp.getUi();
  const resp = ui.alert(
    "⚠ ATENÇÃO",
    "Isso vai APAGAR todos os dados das tabelas FACT e AGG.\n" +
    "Você precisará reprocessar todos os meses depois.\n\nContinuar?",
    ui.ButtonSet.YES_NO
  );
  if (resp !== ui.Button.YES) return;

  const ss = SpreadsheetApp.getActive();
  const tabelas = [
    CFG.SHEETS.FACT_RES, CFG.SHEETS.FACT_MAN, CFG.SHEETS.FACT_REP,
    CFG.SHEETS.FACT_DES, CFG.SHEETS.FACT_REPASSE, CFG.SHEETS.FACT_DEV,
    CFG.SHEETS.AGG
  ];

  for (const nome of tabelas) {
    const sh = ss.getSheetByName(nome);
    if (!sh) continue;
    const lastRow = sh.getLastRow();
    if (lastRow < 2) continue;
    sh.getRange(2, 1, lastRow - 1, sh.getLastColumn()).clearContent();
  }

  ui.alert("✅ Todas as FACTs e AGG foram limpas.\n\nAgora reprocesse cada mês individualmente.");
}

function setCompetenciaAtual_(competencia) {
  const sh = SpreadsheetApp.getActive().getSheetByName(CFG.SHEETS.CONFIG);
  if (!sh) throw new Error("Aba CONFIG não encontrada.");
  const lr = sh.getLastRow(), lc = sh.getLastColumn();
  if (lr < 2 || lc < 2) return;
  const hdrs = sh.getRange(1, 1, 1, lc).getValues()[0].map(x => String(x).trim().toLowerCase());
  if (hdrs[0] === "chave" && hdrs[1] === "valor") {
    const data = sh.getRange(2, 1, lr - 1, 2).getValues();
    for (let i = 0; i < data.length; i++) {
      if (String(data[i][0]).trim().toLowerCase() === "competencia_atual") {
        sh.getRange(2 + i, 2).setValue(competencia);
        SpreadsheetApp.flush();
        return;
      }
    }
  }
  throw new Error("Chave 'competencia_atual' não encontrada no CONFIG.");
}

// ─────────────────────────────────────────────────────────────────────────────
// CONFIG
// ─────────────────────────────────────────────────────────────────────────────
function getCompetenciaAtualRaw_() {
  const ss = SpreadsheetApp.getActive();
  const sh = ss.getSheetByName(CFG.SHEETS.CONFIG);
  if (!sh) throw new Error("Aba CONFIG não encontrada.");
  const lr = sh.getLastRow(), lc = sh.getLastColumn();
  if (lr < 2 || lc < 1) return "";
  const norm = s => String(s || "").trim().toLowerCase();
  const hdrs = sh.getRange(1,1,1,lc).getValues()[0].map(norm);
  if (hdrs[0] === "chave" && hdrs[1] === "valor") {
    for (const [k,v] of sh.getRange(2,1,lr-1,2).getValues())
      if (norm(k) === "competencia_atual") return v;
    return "";
  }
  const col = hdrs.indexOf("competencia_atual") + 1;
  if (col <= 0) throw new Error("CONFIG: coluna 'competencia_atual' não encontrada.");
  const vals = sh.getRange(2,col,lr-1,1).getValues().flat();
  for (let i = vals.length-1; i >= 0; i--)
    if (vals[i] !== "" && vals[i] != null) return vals[i];
  return "";
}

function parseCompetencia_(val) {
  if (val == null) return "";
  if (val instanceof Date && !isNaN(val))
    return `${val.getFullYear()}-${String(val.getMonth()+1).padStart(2,"0")}`;
  let s = String(val).replace(/\u00A0/g," ").trim().replace(/[._]/g,"-");
  if (!s) return "";
  let m = s.match(/^(\d{4})\s*[-\/]\s*(\d{1,2})$/);
  if (m) return normalizeYM_(m[1],m[2]);
  m = s.match(/^(\d{1,2})\s*[-\/]\s*(\d{4})$/);
  if (m) return normalizeYM_(m[2],m[1]);
  m = s.match(/(\d{4}).*?(\d{1,2})/);
  if (m) return normalizeYM_(m[1],m[2]);
  return "";
}

function normalizeYM_(yyyy, mm) {
  const y = parseInt(yyyy,10), m = parseInt(mm,10);
  return (!y || m<1 || m>12) ? "" : `${y}-${String(m).padStart(2,"0")}`;
}

function nextCompetencia_(c) {
  const cur = parseCompetencia_(c);
  if (!cur) throw new Error(`competencia_atual inválida: "${c}"`);
  let [y,mo] = cur.split("-").map(Number);
  mo++; if (mo===13){mo=1;y++;}
  return `${y}-${String(mo).padStart(2,"0")}`;
}

function advanceCompetenciaAtual_() {
  const sh = SpreadsheetApp.getActive().getSheetByName(CFG.SHEETS.CONFIG);
  if (!sh) return;
  const lr=sh.getLastRow(), lc=sh.getLastColumn();
  if (lr<2||lc<2) return;
  const hdrs = sh.getRange(1,1,1,lc).getValues()[0].map(x=>String(x).trim().toLowerCase());
  if (hdrs[0]==="chave"&&hdrs[1]==="valor") {
    const data = sh.getRange(2,1,lr-1,2).getValues();
    for (let i=0;i<data.length;i++) {
      if (String(data[i][0]).trim().toLowerCase()==="competencia_atual") {
        sh.getRange(2+i,2).setValue(nextCompetencia_(data[i][1]));
        return;
      }
    }
  }
}

// ─────────────────────────────────────────────────────────────────────────────
// DRIVE helpers
// ─────────────────────────────────────────────────────────────────────────────
function normFolderName_(s) {
  return String(s||"").replace(/\u00A0/g," ").trim().toUpperCase().replace(/\s+/g," ");
}

function getMonthFolderRobust_(rootId, competencia) {
  const root  = DriveApp.getFolderById(rootId);
  const year  = competencia.slice(0,4);
  const yNorm = normFolderName_(year);
  let yFolder = null;
  const allY  = root.getFolders();
  while (allY.hasNext()) {
    const f = allY.next();
    if (normFolderName_(f.getName())===yNorm){yFolder=f;break;}
  }
  if (!yFolder) throw new Error(`Pasta do ano não encontrada: ${year}`);
  const target = normFolderName_(competencia);
  let mFolder  = null;
  const allM   = yFolder.getFolders();
  while (allM.hasNext()) {
    const f = allM.next();
    const n = normFolderName_(f.getName());
    if (n===target||n.startsWith(target)){mFolder=f;break;}
  }
  if (!mFolder) throw new Error(`Pasta do mês não encontrada: ${competencia}`);
  return mFolder;
}

function getChildFolderByName_(parent, name) {
  const it = parent.getFoldersByName(name);
  if (!it.hasNext()) throw new Error(`Subpasta não encontrada: ${name}`);
  return it.next();
}

function findFolderByAliases_(parent, aliases) {
  for (const a of aliases) {
    const it = parent.getFoldersByName(a);
    if (it.hasNext()) return it.next();
  }
  return null;
}

function openMostRecentGoogleSheet_(folder) {
  const files = folder.getFilesByType(MimeType.GOOGLE_SHEETS);
  if (!files.hasNext()) throw new Error(
    `Nenhuma planilha Google em: ${folder.getName()} (converta .xlsx para Google Planilhas).`
  );
  // Prioridade: arquivo SEM sufixo numérico "(1)", "(2)" etc.
  // Quando a equipe cria uma cópia acidentalmente (ex: "Manutencao(1).xlsx"),
  // o original sem sufixo é sempre o correto. Só usa o mais recente por data
  // se não existir nenhum arquivo sem sufixo numérico na pasta.
  const reSufixo = /\(\d+\)(\.[a-zA-Z]+)?$/;
  let best = null, bestDate = new Date(0);
  let original = null, originalDate = new Date(0);
  while (files.hasNext()) {
    const f    = files.next();
    const d    = f.getLastUpdated();
    const nome = f.getName().trim();
    const temSufixo = reSufixo.test(nome);
    if (!temSufixo && d > originalDate) { originalDate = d; original = f; }
    if (d > bestDate) { bestDate = d; best = f; }
  }
  const escolhido = original || best;
  return SpreadsheetApp.openById(escolhido.getId());
}

// ─────────────────────────────────────────────────────────────────────────────
// Sheets infra
// ─────────────────────────────────────────────────────────────────────────────
function ensureSheet_(name, headers) {
  const ss = SpreadsheetApp.getActive();
  let sh   = ss.getSheetByName(name);
  if (!sh) sh = ss.insertSheet(name);
  const cur = sh.getLastColumn()>0
    ? sh.getRange(1,1,1,sh.getLastColumn()).getValues()[0]
        .map(x=>String(x||"").trim()).filter(Boolean).join("|")
    : "";
  if (cur !== headers.join("|")) {
    sh.clearContents();
    sh.getRange(1,1,1,headers.length).setValues([headers]).setFontWeight("bold");
    sh.setFrozenRows(1);
  }
  return sh;
}

function writeTableFull_(sheetName, headers, rows) {
  const sh = ensureSheet_(sheetName, headers);
  sh.clearContents();
  sh.getRange(1,1,1,headers.length).setValues([headers]).setFontWeight("bold");
  if (rows.length) sh.getRange(2,1,rows.length,headers.length).setValues(rows);
  sh.setFrozenRows(1);
  const lr = Math.max(1,sh.getLastRow());
  const f  = sh.getFilter(); if (f) f.remove();
  sh.getRange(1,1,lr,headers.length).createFilter();
}

function appendRows_(sheetName, rows) {
  if (!rows.length) return;
  const sh = SpreadsheetApp.getActive().getSheetByName(sheetName);
  sh.getRange(sh.getLastRow()+1,1,rows.length,rows[0].length).setValues(rows);
}

/**
 * CORREÇÃO APPEND-ONLY: remove todas as linhas de uma aba FACT_* que
 * pertencem à competência informada, preservando o cabeçalho e todas as
 * linhas de outras competências (meses já encerrados não são tocados).
 *
 * Necessária porque o pipeline original só fazia appendRows_ (incremental),
 * nunca removendo linhas — itens excluídos na planilha fonte (ex: uma OS
 * de manutenção cancelada) ficavam permanentemente gravados mesmo após a
 * exclusão na origem. Chamar esta função ANTES de loadExistingKeys_()
 * garante que o dedup não "lembre" das linhas removidas, permitindo que
 * o estado da fonte seja refletido fielmente a cada execução.
 *
 * Linhas com competencia vazia/inválida são preservadas por segurança
 * (evita apagar dados por engano caso a coluna esteja corrompida).
 */
function removeRowsByCompetencia_(sheetName, competencia) {
  const sh = SpreadsheetApp.getActive().getSheetByName(sheetName);
  if (!sh) return;
  const lastRow = sh.getLastRow();
  if (lastRow < 2) return;

  const lastCol = sh.getLastColumn();
  const header  = sh.getRange(1, 1, 1, lastCol).getValues()[0].map(String);
  const colComp = header.indexOf("competencia");
  if (colComp < 0) return; // aba sem coluna competencia — não faz nada por segurança

  const data = sh.getRange(2, 1, lastRow - 1, lastCol).getValues();
  const compTarget = String(competencia).trim();

  const kept = data.filter(row => {
    const c = String(row[colComp] || "").trim();
    if (!c) return true; // preserva linhas sem competência (segurança)
    return c !== compTarget;
  });

  if (kept.length === data.length) return; // nada para remover

  sh.getRange(2, 1, lastRow - 1, lastCol).clearContent();
  if (kept.length > 0) {
    sh.getRange(2, 1, kept.length, lastCol).setValues(kept);
  }
}

function readTable_(sheetName) {
  const sh   = SpreadsheetApp.getActive().getSheetByName(sheetName);
  const data = sh.getDataRange().getValues();
  const header = data[0].map(String);
  const idx  = {};
  header.forEach((h,i) => idx[h]=i);
  const rows = data.slice(1).filter(r => r.some(v => v!==""&&v!=null));
  return { header, idx, rows };
}

function ensureAllSheets_() {
  ensureSheet_(CFG.SHEETS.LOG, [
    "ts_inicio","ts_fim","duracao_seg","competencia","status","mensagem",
    "linhas_reservas","linhas_manutencao","linhas_reposicao","linhas_despesas","linhas_repasse",
    "dedup_reservas","dedup_despesas","total_pendentes"
  ]);
  ensureSheet_(CFG.SHEETS.PEND, ["ts","competencia","source","tipo_pendencia","apto_original","apto_normalizado","property_id","observacao"]);
  ensureSheet_(CFG.SHEETS.MAP_ALIAS, ["source","apto_normalizado","property_id"]);
  ensureSheet_(CFG.SHEETS.DIM_PROP, ["owner_id","cpf_cnpj","nome_proprietario","email","telefone"]);
  ensureSheet_(CFG.SHEETS.DIM_IMOVEL, ["property_id","owner_id","nome_canonico","empreendimento","unidade","status","comissao_pct_mes"]);
  ensureSheet_(CFG.SHEETS.FACT_RES, ["competencia","source","reserva_key","apto_original","apto_normalizado","property_id","checkin","checkout","noites_total","noites_no_mes","origem_raw","origem_norm","diaria_liquida","receita_liquida_mes","adultos","criancas","valor_total_reserva","taxa_limpeza","comissao_canal","hospede"]);
  ensureSheet_(CFG.SHEETS.FACT_MAN, ["competencia","source","manut_key","apto_original","apto_normalizado","property_id","os_id","produto_servico","valor_total","adm_sub","obs"]);
  ensureSheet_(CFG.SHEETS.FACT_REP, ["competencia","source","rep_key","apto_original","apto_normalizado","property_id","item_raw","item_limpo","quantidade","valor_unitario_ou_total","adm_sub","obs"]);
  ensureSheet_(CFG.SHEETS.FACT_REPASSE, ["competencia","source","repasse_key","apto_original","apto_normalizado","property_id","proprietario_nome","pix","valor","comissao","pago"]);
  ensureSheet_(CFG.SHEETS.FACT_DEV,     ["competencia","source","dev_key","apto_original","apto_normalizado","property_id","valor_devolvido","obs"]);
  ensureSheet_(CFG.SHEETS.FACT_DES, ["competencia","source","desp_key","apto_original","apto_normalizado","property_id","categoria","descricao","valor","data","obs"]);
  ensureSheet_(CFG.SHEETS.AGG, ["competencia","cpf_cnpj","nome_proprietario","owner_id","property_id","nome_canonico","empreendimento","unidade","noites_no_mes","dias_no_mes","taxa_ocupacao","reservas","receita_liquida","diaria_media","comissao_pct","tx_adm","manutencao_total","reposicao_total","despesas_total","custos_total","resultado","itens_reposicao","qtd_itens"]);
}

// ─────────────────────────────────────────────────────────────────────────────
// Normalização com cache
// ─────────────────────────────────────────────────────────────────────────────
const _normCache = new Map();

/**
 * normalize_: uppercase + remove acentos + remove não-alfanuméricos + colapsa espaços.
 * Usado para chaves de dedup, map_alias e property_id.
 */
function normalize_(s) {
  if (s===null||s===undefined) return "";
  const raw = String(s);
  if (_normCache.has(raw)) return _normCache.get(raw);
  let r = raw.trim().toUpperCase()
    .normalize("NFD").replace(/[\u0300-\u036f]/g,"")
    .replace(/[^A-Z0-9 ]+/g," ")
    .replace(/\s+/g," ").trim();
  _normCache.set(raw, r);
  return r;
}

/**
 * normalizeKey_: versão para indexação de headers — lowercase + remove acentos + trim.
 * Permite comparar "Diária Final" == "diaria final" == "Diária final " etc.
 */
/**
 * Corrige mojibake comum: UTF-8 lido incorretamente como Latin-1/Windows-1252.
 * Ex: "CrianÃ§as" → "Crianças". Detecta o padrão (sequência Ã + caractere
 * de controle/símbolo Latin-1) e tenta reconstruir via round-trip de bytes.
 * Se a string não tiver esse padrão, retorna inalterada — seguro para
 * qualquer header normal.
 */
/**
 * Repara mojibake específico de em dash e en dash lidos como Windows-1252.
 * U+2013 (en dash)  → UTF-8: E2 80 93 → Latin-1: â€" (â€“)
 * U+2014 (em dash)  → UTF-8: E2 80 94 → Latin-1: â€" (â€”)
 * Qualquer sequência â€+char é substituída por espaço, depois dashes reais também.
 * Seguro para nomes de apartamentos — nunca quebra strings sem o padrão.
 */
function fixAptMojibake_(s) {
  return String(s || "").replace(/â€./g, " ").replace(/[–—]/g, " ").trim();
}

function fixMojibake_(s) {
  const str = String(s || "");
  if (!/Ã[\x80-\xBF]/.test(str)) return str; // não tem padrão de mojibake, retorna direto
  try {
    // Reconstrói bytes Latin-1 a partir dos code points e decodifica como UTF-8
    const bytes = [];
    for (let i = 0; i < str.length; i++) {
      const code = str.charCodeAt(i);
      if (code > 255) return str; // tem caractere fora do Latin-1, não é o padrão esperado
      bytes.push(code);
    }
    const decoded = Utilities.newBlob(bytes).getDataAsString("UTF-8");
    return decoded;
  } catch (e) {
    return str; // se falhar, mantém original — nunca quebra o pipeline
  }
}

function normalizeKey_(s) {
  return fixMojibake_(s).trim()
    .normalize("NFD").replace(/[\u0300-\u036f]/g,"")
    .toLowerCase()
    .replace(/\s+/g," ");
}

/**
 * headerIndexCI_ (Case-Insensitive + accent-insensitive)
 * Indexa todos os headers normalizados. Usa normalizeKey_ para garantir que
 * "Diária Final ", "diaria final", "Diária final" mapeiam para a mesma chave.
 */
function headerIndexCI_(row) {
  const idx = {};
  row.forEach((h, i) => {
    const k = normalizeKey_(h);
    if (k) idx[k] = i;
  });
  return idx;
}

/**
 * colCI_: busca uma coluna pelo índice CI com múltiplos aliases normalizados.
 * Retorna o índice da primeira correspondência ou undefined.
 * Uso: const c = colCI_(idx, "Apto", "Apartamento", "APTO")
 */
function colCI_(idx, ...aliases) {
  for (const a of aliases) {
    const k = normalizeKey_(a);
    if (idx[k] !== undefined) return idx[k];
  }
  return undefined;
}

/**
 * findHeaderRow_: varre as primeiras maxRows linhas procurando a linha
 * cujo campo normalizado corresponda a qualquer token de headerTokens.
 * Resolve sheets com linhas vazias no topo (ex: Repasse com 2 linhas de cabeçalho).
 */
function findHeaderRow_(values, headerTokens, maxRows) {
  maxRows = maxRows || 15;
  const tokens = headerTokens.map(normalizeKey_);
  for (let i = 0; i < Math.min(maxRows, values.length); i++) {
    const rowNorm = values[i].map(v => normalizeKey_(String(v||"")));
    if (tokens.some(t => rowNorm.includes(t))) return i;
  }
  return 0;
}

/**
 * openSheetCI_: abre a aba cujo nome normalizado contenha o substring `token`.
 * Fallback: primeira aba da planilha.
 * Resolve "RESERVAS GERAIS" == "Reservas gerais" == "Reservas Gerais - sem subloc".
 */
function openSheetCI_(book, token) {
  const t = normalizeKey_(token);
  for (const sh of book.getSheets()) {
    if (normalizeKey_(sh.getName()).includes(t)) return sh;
  }
  return book.getSheets()[0];
}

/**
 * Casa o nome da aba EXATAMENTE (case/acento-insensitive), sem "includes".
 * Necessário quando duas abas têm nomes parecidos e uma é substring da
 * outra — ex.: "serviços" é substring de "produtos e serviços". Usar
 * openSheetCI_ (que faz .includes()) casaria a aba errada.
 * Retorna null se não encontrar (sem fallback — o chamador decide).
 */
function findSheetExactCI_(book, tokenExact) {
  const t = normalizeKey_(tokenExact);
  for (const sh of book.getSheets()) {
    if (normalizeKey_(sh.getName()) === t) return sh;
  }
  return null;
}

const MESES_PT_ = ["JANEIRO","FEVEREIRO","MARCO","ABRIL","MAIO","JUNHO",
                   "JULHO","AGOSTO","SETEMBRO","OUTUBRO","NOVEMBRO","DEZEMBRO"];

/**
 * Localiza a aba do MÊS DA COMPETÊNCIA pelo nome (não pela posição).
 * Bug corrigido (ago/2026): usar sheets[sheets.length-1] ("a última aba é
 * o mês atual") quebra silenciosamente assim que alguém adiciona QUALQUER
 * aba após a do mês corrente — ex.: uma aba de rascunho/teste. Em jul/2026
 * isso fez o ETL ler uma aba de teste com 73 linhas de outros apartamentos
 * em vez das 583 linhas reais de Julho.
 *
 * Estratégia: casa o nome da aba (normalizado, sem acento/espaço) contra o
 * nome do mês da competência. Se não achar, cai no fallback antigo (última
 * aba) MAS sinaliza uma pendência — o problema fica visível em vez de
 * silencioso.
 */
function findSheetByCompetencia_(sheets, competencia, source, pendBatch) {
  const mesIdx = parseCompetencia_(competencia);
  const mesNome = mesIdx ? MESES_PT_[Number(competencia.split("-")[1]) - 1] : null;
  if (mesNome) {
    for (const sh of sheets) {
      if (normalize_(sh.getName()) === mesNome) return sh;
    }
    // Casamento parcial (ex.: aba "Julho " ou "Julho/26")
    for (const sh of sheets) {
      if (normalize_(sh.getName()).includes(mesNome)) return sh;
    }
  }
  if (pendBatch) {
    queuePend_(pendBatch, competencia, source, "", "",
      `Aba do mês "${mesNome || competencia}" não encontrada entre [${sheets.map(s => s.getName()).join(", ")}]. ` +
      `Usando a última aba como fallback — CONFERIR se é a correta.`,
      "ABA_MES_NAO_ENCONTRADA");
  }
  return sheets[sheets.length - 1];
}

/**
 * parseFormula_: avalia fórmulas simples deixadas como string em células de valor.
 * Ex: "=0.99*7" → 6.93. Suporta + - * /. Retorna 0 em caso de erro.
 */
function parseFormula_(v) {
  if (v === null || v === undefined || v === "") return 0;
  if (typeof v === "number") return v;
  let s = String(v).trim();
  if (s.startsWith("=")) {
    s = s.slice(1).trim();
    try {
      // Avalia apenas expressões numéricas simples (sem funções)
      if (/^[\d\s\.\+\-\*\/\(\)]+$/.test(s)) {
        // eslint-disable-next-line no-eval
        const result = Function('"use strict"; return (' + s + ')')();
        return typeof result === "number" && isFinite(result) ? result : 0;
      }
    } catch(e) { return 0; }
  }
  return parseMoneyCell_(s);
}

function normalizeLoose_(s) {
  return normalize_(String(s||"").replace(/[*#]+/g," "));
}

function onlyDigits_(s) { return String(s||"").replace(/\D+/g,""); }

function sanitizeHospede_(v) {
  if (v == null || v === "") return "";
  // 1. Corrige encoding antes de qualquer outra operação
  let s = fixMojibake_(String(v).trim());
  // 2. Mantém apenas o PRIMEIRO hóspede — corta no primeiro separador de multi-hóspede.
  //    Padrões encontrados nos dados: "Nome CPF: Nome2", "Nome - CPF: Nome2",
  //    "Nome cpf . Nome2", "Nome / Nome2", "Nome // Nome2", "Nome | Nome2"
  s = s.split(/\s+[-–]?\s*\bCPF\b|\/\/|\s+\|\s+|\s+\/(?=[^\s])/i)[0].trim();
  // 3. Remove números de documentos restantes (CPF, RG, passaporte)
  s = s.replace(/\d{3}\.?\d{3}\.?\d{3}[-.]?\d{2}/g, "").trim();
  s = s.replace(/[A-Z]{1,2}\d{6,}/gi, "").trim();
  s = s.replace(/\d{7,}/g, "").trim();
  // 4. Remove a palavra CPF e separadores órfãos nas bordas
  s = s.replace(/\s*\bCPF\b\s*$/i, "").trim();
  s = s.replace(/^[\s\-\/|:;,]+|[\s\-\/|:;,]+$/g, "").trim();
  return s;
}

function normalizeOrigem_(o) {
  const s = String(o||"").toLowerCase();
  if (s.includes("airbnb")) return "Airbnb";
  if (s.includes("booking")) return "BookingCom";
  return "Outros";
}

function headerIndexLoose_(row) {
  const idx={};
  row.forEach((h,i) => { const k=normalize_(h); if(k) idx[k]=i; });
  return idx;
}

function extractEmpUnidade_(apto) {
  const parts = String(apto||"").trim().split(/\s+/);
  return parts.length>=2 && /\d+$/.test(parts[parts.length-1])
    ? { empreendimento: parts.slice(0,-1).join(" "), unidade: parts[parts.length-1] }
    : { empreendimento: String(apto||"").trim(), unidade: "" };
}

function extractQty_(item) {
  const s = String(item||"").trim();
  const m = s.match(/\((\d+)\)\s*$/);
  return { qty: m ? parseInt(m[1],10) : 1, clean: s.replace(/\s*\(\d+\)\s*$/,"").trim() };
}

function round2_(n) { return Math.round((Number(n)||0)*100)/100; }

function parsePercentToDecimal_(v) {
  if (v==null||v==="") return null;
  if (typeof v==="number") return v<=1 ? v : v/100;
  let s = String(v).trim().replace("%","").trim().replace(/\./g,"").replace(",",".");
  const n = Number(s);
  if (isNaN(n)) return null;
  return n<=1 ? n : n/100;
}

function parseMoneyCell_(v) {
  if (v==null||v==="") return 0;
  if (typeof v==="number") return v;
  let s = String(v).trim().replace("R$","").trim();
  if (s==="-"||s==="–") return 0;
  s = s.replace(/\s+/g,"");
  if (s.includes(",")&&s.includes(".")) s=s.replace(/\./g,"").replace(",",".");
  else if (s.includes(",")) s=s.replace(",",".");
  const n = Number(s);
  return isNaN(n)?0:n;
}

function normalizeTipoDespesa_(t) {
  const s = String(t||"").toLowerCase()
    .normalize("NFD").replace(/[\u0300-\u036f]/g,"");
  if (s.includes("cond")) return "Condomínio";
  if (s.includes("energ")) return "Energia";
  if (s.includes("inter")||s.includes("tv")) return "Internet/TV";
  if (s.includes("gas")) return "Gás";
  return "Outros";
}

// ─────────────────────────────────────────────────────────────────────────────
// DIMs + alias
// ─────────────────────────────────────────────────────────────────────────────
function buildDimsAndAliasFromProprietarios_(sourcesFolder, competencia) {
  const f = findFolderByAliases_(sourcesFolder, CFG.DRIVE.PROPRIETARIOS);
  if (!f) throw new Error("Pasta Proprietarios não encontrada em 01_Fontes.");
  const book   = openMostRecentGoogleSheet_(f);
  const sh     = openSheetCI_(book, "propriet");        // "Proprietários" ou qualquer variante
  const values = sh.getDataRange().getValues();

  // Detecta linha de cabeçalho procurando a coluna "Apto"
  const hRow = findHeaderRow_(values, ["apto","apartamento"]);
  const idxL = headerIndexCI_(values[hRow]);

  const cApto  = colCI_(idxL, "Apto", "Apartamento");
  const cProp  = colCI_(idxL, "Proprietário", "Proprietario", "Nome Proprietário", "Nome");
  const cDoc   = colCI_(idxL, "CPF/CNPJ", "CPF CNPJ", "Documento", "CNPJ", "CPF");
  const cEmail = colCI_(idxL, "Email", "E-mail");
  const cTel   = colCI_(idxL, "Telefone", "Celular");
  // "Comissão (%)" era perdido pela versão anterior por causa do acento + parênteses
  const cCom   = colCI_(idxL, "Comissão (%)", "Comissao (%)", "Comissão", "Comissao", "Tx Comissão");

  if (cApto===undefined||cProp===undefined||cDoc===undefined)
    throw new Error("Proprietários: preciso das colunas Apto, Proprietário, CPF/CNPJ.");

  const dimProp=[], dimImovel=[], ownerSeen=new Set(), propSeen=new Set();

  for (let r = hRow+1; r < values.length; r++) {
    const row    = values[r];
    const apto   = row[cApto];
    const docRaw = row[cDoc];
    if (!apto||!docRaw) continue;
    const owner_id = onlyDigits_(docRaw);
    if (!owner_id) continue;

    if (!ownerSeen.has(owner_id)) {
      ownerSeen.add(owner_id);
      dimProp.push([
        owner_id,
        String(docRaw).trim(),
        String(row[cProp]||"").trim(),
        String(cEmail!==undefined?(row[cEmail]||""):""),
        String(cTel!==undefined?(row[cTel]||""):"")
      ]);
    }

    const nomeCanon = String(apto).trim();
    const pid = `${owner_id}__${normalize_(nomeCanon).replace(/\s+/g,"_")}`.slice(0,250);
    if (!propSeen.has(pid)) {
      propSeen.add(pid);
      const pct = cCom!==undefined ? parsePercentToDecimal_(row[cCom]) : null;
      const {empreendimento, unidade} = extractEmpUnidade_(nomeCanon);
      dimImovel.push([pid, owner_id, nomeCanon, empreendimento, unidade, "ATIVO",
        pct===null ? DEFAULT_COMISSAO_PCT : pct]);
    }
  }

  writeTableFull_(CFG.SHEETS.DIM_PROP,
    ["owner_id","cpf_cnpj","nome_proprietario","email","telefone"],
    dimProp.sort((a,b)=>String(a[2]).localeCompare(String(b[2]))));
  writeTableFull_(CFG.SHEETS.DIM_IMOVEL,
    ["property_id","owner_id","nome_canonico","empreendimento","unidade","status","comissao_pct_mes"],
    dimImovel.sort((a,b)=>String(a[2]).localeCompare(String(b[2]))));

  rebuildMapAliasFromDimImovel_();
}

function rebuildMapAliasFromDimImovel_() {
  const sh       = SpreadsheetApp.getActive().getSheetByName(CFG.SHEETS.MAP_ALIAS);
  const existing = sh.getLastRow()>=2
    ? sh.getRange(2,1,sh.getLastRow()-1,3).getValues() : [];

  const dim      = readTable_(CFG.SHEETS.DIM_IMOVEL);
  const autoNorms = new Set(dim.rows.map(r=>normalize_(r[dim.idx["nome_canonico"]])));

  const out=[], seen=new Set();
  const sources = ["reservas","manutencao","reposicao","pagamentos"];

  // Preserva entradas manuais
  for (const r of existing) {
    const src=String(r[0]||"").trim(), aptn=String(r[1]||"").trim(), pid=String(r[2]||"").trim();
    if (!src||!aptn||!pid) continue;
    if (!autoNorms.has(aptn)) {
      const k=`${src}||${aptn}`;
      if (!seen.has(k)){ seen.add(k); out.push([src,aptn,pid]); }
    }
  }

  // Gera entradas automáticas
  for (const r of dim.rows) {
    const pid  = String(r[dim.idx["property_id"]]||"").trim();
    const nome = String(r[dim.idx["nome_canonico"]]||"").trim();
    if (!pid||!nome) continue;
    const aptn = normalize_(nome);
    for (const s of sources) {
      const k=`${s}||${aptn}`;
      if (!seen.has(k)){ seen.add(k); out.push([s,aptn,pid]); }
    }
  }
  writeTableFull_(CFG.SHEETS.MAP_ALIAS,["source","apto_normalizado","property_id"],out);
}

// ─────────────────────────────────────────────────────────────────────────────
// Lookup helpers
// ─────────────────────────────────────────────────────────────────────────────
/**
 * MULTI-DONO: nome de apartamento pode ter mais de um property_id
 * (apartamentos com copropriedade — ex: "Lets 27" tem dois donos).
 * Os mapas agora retornam ARRAYS de pid em vez de um único valor,
 * para que resolve_ devolva TODOS os property_ids correspondentes
 * e a ingestão replique a linha de dados para cada um deles.
 */
function buildAliasMap_() {
  const t=readTable_(CFG.SHEETS.MAP_ALIAS);
  const m=new Map();
  for (const r of t.rows) {
    const src  = String(r[t.idx["source"]]||"").trim();
    const aptn = String(r[t.idx["apto_normalizado"]]||"").trim();
    const pid  = String(r[t.idx["property_id"]]||"").trim();
    if (!src||!aptn||!pid) continue;
    const k = `${src}||${aptn}`;
    if (!m.has(k)) m.set(k, []);
    if (!m.get(k).includes(pid)) m.get(k).push(pid);
  }
  return m;
}

function buildCanonMap_() {
  const t=readTable_(CFG.SHEETS.DIM_IMOVEL);
  const m=new Map();
  for (const r of t.rows) {
    const nome = normalize_(r[t.idx["nome_canonico"]]);
    const pid  = String(r[t.idx["property_id"]]||"").trim();
    if (!nome||!pid) continue;
    if (!m.has(nome)) m.set(nome, []);
    if (!m.get(nome).includes(pid)) m.get(nome).push(pid);
  }
  return m;
}

/**
 * Retorna TODOS os property_ids correspondentes ao apartamento.
 * Para apartamentos com 1 dono, o array tem 1 elemento (comportamento
 * idêntico ao anterior). Para copropriedades, retorna 2+ elementos —
 * a função chamadora deve replicar a linha de dados para cada um.
 */
function resolveAll_(aliasMap, canonMap, source, aptoOriginal) {
  const aptoFixed  = fixAptMojibake_(aptoOriginal);
  const aptoNorm   = normalize_(aptoFixed);
  const aptoLoose  = normalizeLoose_(aptoFixed);
  const pids =
    aliasMap.get(`${source}||${aptoNorm}`)  ||
    canonMap.get(aptoNorm)                   ||
    aliasMap.get(`${source}||${aptoLoose}`) ||
    canonMap.get(aptoLoose)                  ||
    [];
  return { propertyIds: pids, aptoNorm };
}

/**
 * Compatibilidade retroativa: retorna apenas o PRIMEIRO property_id.
 * Mantida para qualquer código legado que ainda espere um único pid.
 * Novas ingestões devem usar resolveAll_ para suportar múltiplos donos.
 */
function resolve_(aliasMap, canonMap, source, aptoOriginal) {
  const { propertyIds, aptoNorm } = resolveAll_(aliasMap, canonMap, source, aptoOriginal);
  return { propertyId: propertyIds.length > 0 ? propertyIds[0] : "", aptoNorm };
}

function queuePend_(batch, competencia, source, aptoOriginal, aptoNorm, obs, tipo) {
  batch.push([new Date(), competencia, source, tipo || "IMOVEL_NAO_CADASTRADO",
    aptoOriginal, aptoNorm, "", obs||""]);
}

// ─────────────────────────────────────────────────────────────────────────────
// Gestão de aliases — Sugestão e confirmação
// ─────────────────────────────────────────────────────────────────────────────

/**
 * Lê o PENDENTES_map_alias, coleta nomes únicos ainda sem alias,
 * pontua cada um contra os nomes canônicos do dim_imovel por sobreposição
 * de tokens e grava sugestões na aba "ALIASES_SUGERIDOS" para revisão manual.
 *
 * Verde  = confiança ALTA  (≥ 60 % de tokens em comum) — pré-preenchido
 * Amarelo = confiança MÉDIA (≥ 30 %)                   — revise
 * Laranja = confiança BAIXA (< 30 %)                   — preencha manualmente
 *
 * Após revisar a aba, execute "✅ Confirmar aliases sugeridos" para aplicar.
 */
function sugerirAliases() {
  const ss     = SpreadsheetApp.getActive();
  const pendSh = ss.getSheetByName(CFG.SHEETS.PEND);
  if (!pendSh || pendSh.getLastRow() < 2) {
    SpreadsheetApp.getUi().alert("Nenhum registro em PENDENTES_map_alias.");
    return;
  }

  const aliasMap = buildAliasMap_();
  const canonMap = buildCanonMap_();
  const values   = pendSh.getDataRange().getValues();
  const pending  = new Map();

  // Colunas: ts(0) competencia(1) source(2) tipo_pendencia(3) apto_original(4) apto_normalizado(5)
  for (let i = 1; i < values.length; i++) {
    const r        = values[i];
    const source   = String(r[2] || "").trim();
    const aptoOrig = String(r[4] || "").trim();
    const aptoNorm = String(r[5] || "").trim();
    if (!source || !aptoNorm) continue;
    const k = `${source}||${aptoNorm}`;
    if (aliasMap.has(k) || canonMap.has(aptoNorm)) continue; // já resolvido
    if (!pending.has(k)) pending.set(k, { source, aptoOrig, aptoNorm });
  }

  if (pending.size === 0) {
    SpreadsheetApp.getUi().alert("✅ Todos os pendentes já possuem alias ou foram resolvidos.");
    return;
  }

  const dim = readTable_(CFG.SHEETS.DIM_IMOVEL);
  const canonicals = dim.rows
    .map(r => ({
      pid:  String(r[dim.idx["property_id"]]  || "").trim(),
      nome: String(r[dim.idx["nome_canonico"]] || "").trim(),
      norm: normalize_(String(r[dim.idx["nome_canonico"]] || ""))
    }))
    .filter(c => c.pid && c.nome);

  function scoreMatch(a, b) {
    const aT = new Set(a.split(" ").filter(t => t.length > 1));
    const bT = new Set(b.split(" ").filter(t => t.length > 1));
    if (aT.size + bT.size === 0) return 0;
    let overlap = 0;
    for (const t of aT) if (bT.has(t)) overlap++;
    return overlap / Math.max(aT.size, bT.size);
  }

  let helperSh = ss.getSheetByName("ALIASES_SUGERIDOS");
  if (!helperSh) helperSh = ss.insertSheet("ALIASES_SUGERIDOS");
  else helperSh.clearContents().clearFormats();

  const headers = [
    "source", "apto_normalizado_pendente", "apto_original",
    "property_id_sugerido", "nome_canonico_sugerido", "confianca",
    "property_id_confirmado ← EDITE AQUI"
  ];
  const out = [headers];

  for (const [, info] of pending) {
    let best = null, bestScore = 0;
    for (const c of canonicals) {
      const s = scoreMatch(info.aptoNorm, c.norm);
      if (s > bestScore) { bestScore = s; best = c; }
    }
    const conf = bestScore >= 0.6 ? "ALTA" : bestScore >= 0.3 ? "MÉDIA" : "BAIXA";
    out.push([
      info.source,
      info.aptoNorm,
      info.aptoOrig,
      best ? best.pid  : "",
      best ? best.nome : "",
      conf,
      best && bestScore >= 0.6 ? best.pid : ""
    ]);
  }

  helperSh.getRange(1, 1, out.length, headers.length).setValues(out);
  helperSh.setFrozenRows(1);
  helperSh.getRange(1, 1, 1, headers.length).setFontWeight("bold");

  for (let i = 1; i < out.length; i++) {
    const conf = out[i][5];
    const bg   = conf === "ALTA" ? "#d9ead3" : conf === "MÉDIA" ? "#fff2cc" : "#fce5cd";
    helperSh.getRange(i + 1, 1, 1, headers.length).setBackground(bg);
  }
  helperSh.autoResizeColumns(1, headers.length);

  ss.setActiveSheet(helperSh);
  SpreadsheetApp.getUi().alert(
    `🔍 ${pending.size} pendentes listados na aba "ALIASES_SUGERIDOS".\n\n` +
    `Preencha a última coluna "property_id_confirmado" para cada linha:\n` +
    `  🟢 VERDE  = alta confiança (pré-preenchido — confira e mantenha)\n` +
    `  🟡 AMARELO = confiança média (revise o property_id sugerido)\n` +
    `  🟠 LARANJA = baixa confiança (preencha manualmente)\n\n` +
    `Deixe "property_id_confirmado" em branco para ignorar o apartamento\n` +
    `(ex.: apartamentos de outros clientes BSBStay que não pertencem ao portfólio).\n\n` +
    `Quando terminar, execute "✅ Confirmar aliases sugeridos" no menu.`
  );
}

/**
 * Lê a aba "ALIASES_SUGERIDOS" (gerada por sugerirAliases),
 * pega todas as linhas onde "property_id_confirmado" foi preenchido
 * e insere os novos aliases em map_alias_imovel.
 * Ignora duplicatas já existentes.
 */
function confirmarAliasesSugeridos() {
  const ss       = SpreadsheetApp.getActive();
  const helperSh = ss.getSheetByName("ALIASES_SUGERIDOS");
  if (!helperSh || helperSh.getLastRow() < 2) {
    SpreadsheetApp.getUi().alert(
      "Aba ALIASES_SUGERIDOS não encontrada.\nExecute '🔍 Sugerir aliases para pendentes' primeiro.");
    return;
  }

  const values  = helperSh.getDataRange().getValues();
  const hdr     = values[0].map(h => String(h).trim().toLowerCase());
  const iSrc    = hdr.findIndex(h => h === "source");
  const iNorm   = hdr.findIndex(h => h.startsWith("apto_normalizado_p"));
  const iPidConf= hdr.findIndex(h => h.startsWith("property_id_confirmado"));

  if (iSrc < 0 || iNorm < 0 || iPidConf < 0) {
    SpreadsheetApp.getUi().alert(
      "Colunas esperadas não encontradas em ALIASES_SUGERIDOS.\n" +
      "Regenere a aba com '🔍 Sugerir aliases para pendentes'.");
    return;
  }

  // Lê aliases já existentes para evitar duplicatas
  const mapSh   = ss.getSheetByName(CFG.SHEETS.MAP_ALIAS);
  const existing = new Set();
  if (mapSh && mapSh.getLastRow() >= 2) {
    const mapVals = mapSh.getRange(2, 1, mapSh.getLastRow() - 1, 3).getValues();
    for (const r of mapVals) {
      existing.add(`${String(r[0]).trim()}||${String(r[1]).trim()}`);
    }
  }

  const toAdd = [];
  let   skipped = 0;

  for (let i = 1; i < values.length; i++) {
    const src     = String(values[i][iSrc]     || "").trim();
    const norm    = String(values[i][iNorm]    || "").trim();
    const pidConf = String(values[i][iPidConf] || "").trim();
    if (!src || !norm || !pidConf) { skipped++; continue; }
    const k = `${src}||${norm}`;
    if (existing.has(k)) { skipped++; continue; }
    toAdd.push([src, norm, pidConf]);
    existing.add(k);
  }

  if (toAdd.length === 0) {
    SpreadsheetApp.getUi().alert(
      `Nenhum alias novo para inserir.\n` +
      `(${skipped} linha(s) ignorada(s): sem property_id preenchido ou alias já existente.)`);
    return;
  }

  if (mapSh) {
    mapSh.getRange(mapSh.getLastRow() + 1, 1, toAdd.length, 3).setValues(toAdd);
  }

  SpreadsheetApp.getUi().alert(
    `✅ ${toAdd.length} alias(es) adicionado(s) a map_alias_imovel.\n` +
    `${skipped} linha(s) ignorada(s) (sem property_id ou já existentes).\n\n` +
    `Execute "♻ Reprocessar TODOS os meses" para recalcular os dados.`
  );
}

// ─────────────────────────────────────────────────────────────────────────────
// Dedupe
// ─────────────────────────────────────────────────────────────────────────────
function loadExistingKeys_() {
  const ss = SpreadsheetApp.getActive();
  function loadSet_(sheetName, keyCol) {
    const sh   = ss.getSheetByName(sheetName);
    const data = sh.getDataRange().getValues();
    if (data.length<2) return new Set();
    const idx  = data[0].map(String).indexOf(keyCol);
    if (idx<0) return new Set();
    const s = new Set();
    for (let r=1;r<data.length;r++){
      const k=String(data[r][idx]||"").trim();
      if(k) s.add(k);
    }
    return s;
  }
  return {
    reservas: loadSet_(CFG.SHEETS.FACT_RES,      "reserva_key"),
    despesas: loadSet_(CFG.SHEETS.FACT_DES,      "desp_key"),
    manut:    loadSet_(CFG.SHEETS.FACT_MAN,      "manut_key"),
    repos:    loadSet_(CFG.SHEETS.FACT_REP,      "rep_key"),
    repasse:  loadSet_(CFG.SHEETS.FACT_REPASSE,  "repasse_key"),
    dev:      loadSet_(CFG.SHEETS.FACT_DEV,      "dev_key"),
  };
}

// ─────────────────────────────────────────────────────────────────────────────
// Datas
// ─────────────────────────────────────────────────────────────────────────────
function monthBounds_(competencia) {
  const c=parseCompetencia_(competencia);
  if (!c) return null;
  const [y,mo]=c.split("-").map(Number);
  const start=new Date(y,mo-1,1), end=new Date(y,mo,1);
  return { start, endExclusive:end, days:Math.round((end-start)/86400000), competencia:c };
}

function toDate_(v) {
  if (!v) return null;
  if (v instanceof Date&&!isNaN(v)) return v;
  const d=new Date(v); return isNaN(d)?null:d;
}

function overlapNights_(checkIn,checkOut,mStart,mEnd) {
  return Math.max(0,Math.round(
    (Math.min(checkOut,mEnd)-Math.max(checkIn,mStart))/86400000
  ));
}

function fmtDate_(d) {
  return Utilities.formatDate(d,Session.getScriptTimeZone(),"yyyy-MM-dd");
}

// ─────────────────────────────────────────────────────────────────────────────
// INGESTÃO — Reservas  ★ CORRIGIDA V5.1
// ─────────────────────────────────────────────────────────────────────────────
/*
 * Problemas resolvidos:
 *  • Nome da aba varia: "RESERVAS GERAIS", "Reservas gerais", "Reservas Gerais - sem subloc"
 *    → openSheetCI_(book, "reserva") resolve qualquer variante
 *  • Coluna diária varia: "Diária Final", "Diária final", "Diária Final " (trailing space)
 *    → headerIndexCI_ + colCI_ com múltiplos aliases resolvem todas as variações
 *  • Lookup de colunas por nome exato falhava silenciosamente
 *    → todos os acessos agora via colCI_()
 */
function ingestReservas_(competencia, sourcesFolder, dedupe, aliasMap, canonMap, pendBatch) {
  const f = findFolderByAliases_(sourcesFolder, CFG.DRIVE.RESERVAS);
  if (!f) throw new Error("Pasta Reservas não encontrada.");

  const book   = openMostRecentGoogleSheet_(f);
  const sh     = openSheetCI_(book, "reserva");          // tolera qualquer capitalização
  const data   = sh.getDataRange().getValues();
  if (data.length < 2) return { inserted: 0, duplicated: 0 };

  const idx    = headerIndexCI_(data[0]);                // CI + accent-insensitive
  const bounds = monthBounds_(competencia);
  if (!bounds) throw new Error(`Competência inválida: ${competencia}`);

  // Coluna da diária com todos os aliases conhecidos
  const cDiaria  = colCI_(idx, "Diária Final", "Diaria Final", "Diária final", "Diaria final");
  const cCheckIn = colCI_(idx, "Check-In", "Check In", "Checkin");
  const cCheckOut= colCI_(idx, "Check-Out", "Check Out", "Checkout");
  const cApto    = colCI_(idx, "Acomodacao", "Acomodação", "Apto", "Apartamento");
  const cOrigem  = colCI_(idx, "Origem");
  const cNoites  = colCI_(idx, "Noites");
  const cHospede = colCI_(idx, "Hóspede", "Hospede", "Nome do Hóspede", "Nome Hospede", "Guest");
  // "Total Pago" não existe na planilha real (schema 2026-05) — o campo correto
  // é "Valor Total da Reserva". Mantido "Total Pago" como alias de fallback
  // para compatibilidade com possíveis schemas antigos/futuros.
  const cTotal     = colCI_(idx, "Valor Total da Reserva", "Total Pago", "Valor Recebido");
  const cAdultos   = colCI_(idx, "N. Adultos", "N Adultos", "Adultos", "Qtd Adultos");
  const cCriancas  = colCI_(idx, "N. Crianças", "N Criancas", "N. CrianÃ§as", "Crianças", "Criancas", "Qtd Criancas");
  const cTaxas     = colCI_(idx, "Total de Taxas", "Taxa de Limpeza", "Taxas");
  const cComissao  = colCI_(idx, "Comissoes OTA", "Comissões OTA", "Comissao OTA", "Comissão Canal", "Comissao Canal");

  const out = [];
  let dup   = 0;

  for (let r = 1; r < data.length; r++) {
    const row      = data[r];
    const checkIn  = toDate_(cCheckIn  !== undefined ? row[cCheckIn]  : null);
    const checkOut = toDate_(cCheckOut !== undefined ? row[cCheckOut] : null);
    if (!checkIn || !checkOut) continue;

    const apt        = cApto    !== undefined ? row[cApto]    : "";
    const origemRaw  = cOrigem  !== undefined ? row[cOrigem]  : "";
    const origemNorm = normalizeOrigem_(origemRaw);
    const noitesTotal = Number(
      (cNoites !== undefined ? row[cNoites] : null) ||
      Math.round((checkOut - checkIn) / 86400000)
    );
    const diariaFinal = Number(cDiaria !== undefined ? (row[cDiaria] ?? 0) : 0);

    if (!noitesTotal || noitesTotal <= 0 || !diariaFinal) continue;

    const noitesMes = overlapNights_(checkIn, checkOut, bounds.start, bounds.endExclusive);
    if (noitesMes <= 0) continue;

    // ARQUITETURA A: fact_reservas tem 1 linha por reserva real (fato do imóvel).
    // Usa resolve_ (primeiro property_id) — a visibilidade para ambos os donos
    // de aptos multi-dono é garantida pelo agg_prestacao_contas, que já tem
    // 1 linha por property_id e é a fonte dos KPIs no dashboard.
    const { propertyId, aptoNorm } = resolve_(aliasMap, canonMap, "reservas", apt);
    if (!propertyId && apt) {
      queuePend_(pendBatch, competencia, "reservas", apt, aptoNorm,
        "Apartamento em reservas não encontrado no dim_imovel.");
    }

    const totalPago      = cTotal     !== undefined ? Number(row[cTotal]     ?? 0) : 0;
    const adultos        = cAdultos   !== undefined ? Number(row[cAdultos]   ?? 0) : 0;
    const criancas       = cCriancas  !== undefined ? Number(row[cCriancas]  ?? 0) : 0;
    const taxaLimpeza    = cTaxas     !== undefined ? Number(row[cTaxas]     ?? 0) : 0;
    const comissaoCanal  = cComissao  !== undefined ? Number(row[cComissao]  ?? 0) : 0;
    const hospede        = sanitizeHospede_(cHospede !== undefined ? row[cHospede] : "");

    const key = `RES|${competencia}|${normalize_(apt)}|${fmtDate_(checkIn)}|${fmtDate_(checkOut)}|${totalPago}`;
    if (dedupe.reservas.has(key)) { dup++; continue; }
    dedupe.reservas.add(key);

    out.push([
      competencia, "reservas", key,
      String(apt || ""), aptoNorm, propertyId || "",
      checkIn, checkOut, noitesTotal, noitesMes,
      String(origemRaw), origemNorm,
      diariaFinal, round2_(diariaFinal * noitesMes),
      adultos, criancas, round2_(totalPago), round2_(taxaLimpeza), round2_(comissaoCanal),
      hospede
    ]);
  }

  // ── Detecção de sobreposição de datas no MESMO apartamento ────────────────
  // Reservas que cruzam a virada do mês são legítimas (cada competência
  // recebe sua fatia via overlapNights_). Já duas reservas do MESMO mês
  // ocupando o mesmo dia indicam erro na planilha fonte: lançamento
  // duplicado ou data de check-in/check-out trocada. Sem esta checagem o
  // problema passa silencioso e infla noites_no_mes (ocupação > 100%).
  const porApto = new Map();
  for (const r of out) {
    const pid = String(r[5] || r[4] || "");
    if (!pid) continue;
    if (!porApto.has(pid)) porApto.set(pid, []);
    porApto.get(pid).push({ apto: r[3], aptoNorm: r[4], ci: r[6], co: r[7] });
  }
  for (const [, lista] of porApto) {
    if (lista.length < 2) continue;
    lista.sort((a, b) => a.ci - b.ci);
    // Máximo corrente de check-out: comparar só com o vizinho imediato
    // perderia o caso de uma reserva longa englobando várias curtas
    // (ex.: 11→21 vs 16→18, 19→20 e 20→22 no mesmo mês).
    let ref = lista[0];
    for (let i = 1; i < lista.length; i++) {
      const cur = lista[i];
      // Intervalos são [check-in, check-out): o dia do check-out não ocupa,
      // então encostar (co == ci) não é conflito.
      if (cur.ci < ref.co) {
        const dias = Math.round((Math.min(cur.co, ref.co) - cur.ci) / 86400000);
        queuePend_(pendBatch, competencia, "reservas", cur.apto, cur.aptoNorm,
          `Sobreposição de ${dias} dia(s): ${fmtDate_(ref.ci)}→${fmtDate_(ref.co)} ` +
          `e ${fmtDate_(cur.ci)}→${fmtDate_(cur.co)}. Verificar duplicata ou data trocada.`,
          "RESERVA_SOBREPOSTA");
      }
      if (cur.co > ref.co) ref = cur;   // avança a referência de maior alcance
    }
  }

  appendRows_(CFG.SHEETS.FACT_RES, out);
  return { inserted: out.length, duplicated: dup };
}

// ─────────────────────────────────────────────────────────────────────────────
// INGESTÃO — Manutenção  ★ CORRIGIDA V5.1
// ─────────────────────────────────────────────────────────────────────────────
/*
 * Problemas resolvidos:
 *  • Schema muda entre meses:
 *    2025-12: Identificador OS | Nome do Produto | Quantidade | Valor Unitário | Apto | ADM/Sublocado | ADM/SUB | OBS
 *    2026-01: Identificador OS | Nome do Produto | Valor Total | Apto | OBS | ADM/SUB
 *    2026-02: Identificador OS | Nome do Produto | Qtd | Val Unitário | Valor Total | Apto | ADM/SUB | Obser
 *    2026-04: igual 2026-02 mas sem ADM/SUB
 *  • "Valor Total" ausente em 2025-12: calcula Quantidade × Valor Unitário como fallback
 *  • "ADM/Sublocado" vs "ADM/SUB" vs ausente: colCI_ com todos os aliases
 *  • Aba "serviços" separada (fix ago/2026): desde jan/2026 a planilha
 *    também recebe lançamentos numa aba "serviços" à parte de "produtos e
 *    serviços" — mesma origem (sistema de field-service), mas exportada
 *    em outra aba, SEM coluna Apto. O ETL só lia "produtos e serviços" e
 *    ignorava silenciosamente a outra: 2.436 linhas (R$ 141.659,70) em 4
 *    dos 6 meses ficaram de fora do sistema. O apto de cada linha de
 *    "serviços" é resolvido via join pelo Identificador OS contra a aba
 *    "atividades" (coluna "Nome do cliente" tem o mesmo formato de
 *    identificador de apto usado no resto da planilha).
 */
function ingestManutencao_(competencia, sourcesFolder, dedupe, aliasMap, canonMap, pendBatch) {
  const f = findFolderByAliases_(sourcesFolder, CFG.DRIVE.MANUTENCAO);
  if (!f) return 0;

  const book = openMostRecentGoogleSheet_(f);
  const out  = [];

  const shProd = openSheetCI_(book, "produtos");   // "produtos e serviços"
  processarLinhasManutencao_(shProd, competencia, dedupe, aliasMap, canonMap, pendBatch, out, null);

  // Aba "serviços" exige nome EXATO (não usar openSheetCI_ com token
  // "servicos": "produtos e serviços" também contém a substring "servicos"
  // e seria casada primeiro, por vir antes na ordem das abas).
  const shServ = findSheetExactCI_(book, "serviços");
  if (shServ) {
    const osParaApto = buildMapaOsParaApto_(book);
    if (osParaApto === null) {
      // Aba "atividades" (fonte do join) nem existe neste mês — não é OS
      // isolada sem match, é a base inteira ausente. Uma pendência só,
      // em vez de uma por linha (poderia chegar a 700+ no mesmo mês).
      const nLinhas = Math.max(0, shServ.getLastRow() - 1);
      if (nLinhas > 0) {
        queuePend_(pendBatch, competencia, "manutencao", "", "",
          `Aba "serviços" tem ${nLinhas} linha(s) mas a aba "atividades" ` +
          `(necessária para resolver o apto de cada linha) não foi encontrada ` +
          `neste mês. Nenhuma linha de "serviços" foi importada — valor não contabilizado.`,
          "SERVICOS_SEM_ATIVIDADES");
      }
    } else {
      processarLinhasManutencao_(shServ, competencia, dedupe, aliasMap, canonMap, pendBatch, out, osParaApto);
    }
  }

  // NOTA: a limpeza da competência atual (correção do bug append-only)
  // já acontece centralizadamente em runMonthlyUpdateFixedRoot, ANTES do
  // dedup ser carregado — ver removeRowsByCompetencia_ ali.
  appendRows_(CFG.SHEETS.FACT_MAN, out);
  return out.length;
}

/**
 * Constrói o mapa Identificador OS -> nome do apto, a partir da aba
 * "atividades" (log bruto de tarefas do sistema de field-service). Usado
 * para resolver o apto das linhas da aba "serviços", que não tem coluna
 * Apto própria.
 */
// Retorna null se a aba "atividades" não existir neste mês (caso distinto
// de "existe mas esta OS específica não está nela" — ver uso em
// ingestManutencao_, que trata os dois casos de forma diferente).
function buildMapaOsParaApto_(book) {
  const sh = findSheetExactCI_(book, "atividades");
  if (!sh) return null;
  const map = new Map();
  const data = sh.getDataRange().getValues();
  if (data.length < 2) return map;
  const idx = headerIndexCI_(data[0]);
  const cOs   = colCI_(idx, "Identificador da OS", "Identificador OS", "OS", "ID OS");
  const cApto = colCI_(idx, "Nome do cliente", "Nome do Cliente");
  if (cOs === undefined || cApto === undefined) return map;
  for (let r = 1; r < data.length; r++) {
    const os = String(data[r][cOs] ?? "").trim();
    const apto = data[r][cApto];
    if (os && apto && !map.has(os)) map.set(os, apto);
  }
  return map;
}

/**
 * Processa uma aba de manutenção (linhas de produto OU de serviço — mesmo
 * schema de valores, coluna de nome do item com rótulo diferente) e
 * empilha em `out`. Se `osParaApto` for passado, a aba não tem coluna
 * Apto própria e o apto é resolvido pelo Identificador OS.
 */
function processarLinhasManutencao_(sh, competencia, dedupe, aliasMap, canonMap, pendBatch, out, osParaApto) {
  const data = sh.getDataRange().getValues();
  if (data.length < 2) return;

  const idx = headerIndexCI_(data[0]);

  const cOsId   = colCI_(idx, "Identificador OS", "Identificador da OS", "OS", "ID OS");
  const cProd   = colCI_(idx, "Nome do Produto", "Nome do Serviço", "Produto", "Serviço", "Produto/Serviço");
  const cApto   = colCI_(idx, "Apto", "Apartamento");
  const cValTot = colCI_(idx, "Valor Total");
  const cValUni = colCI_(idx, "Valor Unitário", "Valor Unitario");
  const cQtd    = colCI_(idx, "Quantidade utilizada", "Quantidade", "Qtd");
  const cAdm    = colCI_(idx, "ADM/SUB", "ADM/Sublocado", "Adm/Sub", "ADM");
  const cObs    = colCI_(idx, "OBS", "Obser", "Observação", "Observacao");

  if (cProd === undefined) return;
  if (cApto === undefined && !osParaApto) return;   // sem Apto e sem join: não há como resolver

  for (let r = 1; r < data.length; r++) {
    const row  = data[r];
    const osId = cOsId !== undefined ? (row[cOsId] ?? "") : "";

    let apt;
    if (cApto !== undefined) {
      apt = row[cApto];
    } else {
      apt = osParaApto.get(String(osId).trim());
      if (!apt) {
        // Sem Apto na linha e sem OS correspondente em "atividades":
        // não há como atribuir o custo a nenhum imóvel. Registra a
        // pendência (visível para revisão manual) e pula a linha.
        if (String(osId).trim()) {
          queuePend_(pendBatch, competencia, "manutencao", "", "",
            `OS "${osId}" na aba de serviços sem correspondência em "atividades" — apto não resolvido.`,
            "OS_SEM_APTO");
        }
        continue;
      }
    }
    if (!apt) continue;

    // MULTI-DONO: replica a linha para cada property_id do apartamento.
    const { propertyIds, aptoNorm } = resolveAll_(aliasMap, canonMap, "manutencao", apt);
    if (propertyIds.length === 0) queuePend_(pendBatch, competencia, "manutencao", apt, aptoNorm,
      "Apartamento em manutenção não encontrado no dim_imovel.");

    const produto = row[cProd] ?? "";
    const obs    = cObs   !== undefined ? (row[cObs]   ?? "") : "";
    const adm    = cAdm   !== undefined ? (row[cAdm]   ?? "") : "";

    // Valor Total: usa diretamente ou calcula Qtd × ValUni como fallback
    let valor = 0;
    if (cValTot !== undefined && row[cValTot] !== "" && row[cValTot] != null) {
      valor = Number(row[cValTot]) || 0;
    } else if (cValUni !== undefined && cQtd !== undefined) {
      valor = (Number(row[cQtd]) || 1) * (Number(row[cValUni]) || 0);
    } else if (cValUni !== undefined) {
      valor = Number(row[cValUni]) || 0;
    }

    const pidsParaGravar = propertyIds.length > 0 ? propertyIds : [""];
    for (const propertyId of pidsParaGravar) {
      const key = `MAN|${competencia}|${normalize_(apt)}|${String(osId)}|${normalize_(produto)}|${valor.toFixed(2)}|${propertyId}`;
      if (dedupe.manut.has(key)) continue;
      dedupe.manut.add(key);

      out.push([competencia, "manutencao", key, String(apt), aptoNorm, propertyId,
        String(osId), String(produto), valor, String(adm), String(obs)]);
    }
  }
}

// ─────────────────────────────────────────────────────────────────────────────
// INGESTÃO — Reposição  ★ CORRIGIDA V5.1
// ─────────────────────────────────────────────────────────────────────────────
/*
 * Problemas resolvidos:
 *  • Nome da aba varia por mês: "Dezembro", "Janeiro ", "Fevereiro", "Abril"
 *    → findSheetByCompetencia_: casa o nome da aba com o mês da competência
 *      (fix ago/2026 — antes usava "última aba", que quebrava se qualquer
 *      aba fosse adicionada depois da do mês corrente; ver comentário na
 *      própria função)
 *  • Schema da coluna de apto varia: "Apto", "Apartamento ", "Apartamento"
 *  • Schema de item varia: "Item/Serviço", "Manutenção/Item de reposição", "Reposição/Enxoval "
 *  • Coluna valor pode ter trailing space: "Valor " (2026-01, 2026-02, 2026-04)
 *  • Valores podem ser fórmulas: "=0.99*7" → parseFormula_()
 */
function ingestReposicao_(competencia, sourcesFolder, dedupe, aliasMap, canonMap, pendBatch) {
  const f = findFolderByAliases_(sourcesFolder, CFG.DRIVE.REPOSICAO);
  if (!f) return 0;

  const book   = openMostRecentGoogleSheet_(f);
  const sheets = book.getSheets();
  if (!sheets.length) return 0;

  const sh   = findSheetByCompetencia_(sheets, competencia, "reposicao", pendBatch);
  const data = sh.getDataRange().getValues();
  if (data.length < 2) return 0;

  // Detecta linha de cabeçalho (pode ter linha de data antes)
  const hRow = findHeaderRow_(data, ["apto","apartamento","item","item/servico","item/servico"]);
  const idx  = headerIndexCI_(data[hRow]);

  // Aliases para todas as variações conhecidas de cada coluna
  const cApto  = colCI_(idx,
    "Apto", "Apartamento", "Apartamento ");
  const cItem  = colCI_(idx,
    "Item/Serviço", "Item/Servico", "Item",
    "Manutenção/Item de reposição", "Manutencao/Item de reposicao",
    "Manutenção/Enxoval/Reposição", "Manutencao/Enxoval/Reposicao",
    "Reposição/Enxoval", "Reposicao/Enxoval");
  const cValor = colCI_(idx, "Valor", "Valor ");
  const cAdm   = colCI_(idx, "Adm/Sub", "ADM/SUB", "ADM",
    "Administração/Sublocado", "Administracao/Sublocado");
  const cObs   = colCI_(idx, "OBS", "Observações", "Observacoes", "Observação", "Observacao", "Lançado");

  if (cApto === undefined || cItem === undefined) return 0;

  const out = [];
  for (let r = hRow + 1; r < data.length; r++) {
    const row    = data[r];
    const apt    = row[cApto];
    const itemRaw = row[cItem];
    if (!apt || !itemRaw) continue;

    const { qty, clean } = extractQty_(itemRaw);
    // MULTI-DONO: replica a linha para cada property_id do apartamento.
    const { propertyIds, aptoNorm } = resolveAll_(aliasMap, canonMap, "reposicao", apt);
    if (propertyIds.length === 0) queuePend_(pendBatch, competencia, "reposicao", apt, aptoNorm,
      "Apartamento em reposição não encontrado no dim_imovel.");

    // parseFormula_ resolve células com "=0.99*7" que o Apps Script não calcula no xlsx
    const valor = round2_(parseFormula_(cValor !== undefined ? row[cValor] : 0));
    const adm   = cAdm !== undefined ? String(row[cAdm] ?? "") : "";
    const obs   = cObs !== undefined ? String(row[cObs] ?? "") : "";

    const pidsParaGravar = propertyIds.length > 0 ? propertyIds : [""];
    for (const propertyId of pidsParaGravar) {
      const key = `REP|${competencia}|${normalize_(apt)}|${normalize_(clean)}|${qty}|${valor.toFixed(2)}|${propertyId}`;
      if (dedupe.repos.has(key)) continue;
      dedupe.repos.add(key);

      out.push([competencia, "reposicao", key, String(apt), aptoNorm, propertyId,
        String(itemRaw), String(clean), qty, valor, adm, obs]);
    }
  }

  // Limpeza da competência atual já ocorre centralmente em
  // runMonthlyUpdateFixedRoot (correção do bug append-only).
  appendRows_(CFG.SHEETS.FACT_REP, out);
  return out.length;
}

// ─────────────────────────────────────────────────────────────────────────────
// INGESTÃO — Repasse  ★ CORRIGIDA V5.1
// ─────────────────────────────────────────────────────────────────────────────
/*
 * Problemas resolvidos:
 *  • Aba "Repasse" começa com 2 linhas vazias antes do cabeçalho
 *    → findHeaderRow_ detecta automaticamente a linha do cabeçalho
 *  • Coluna "Pix " tem trailing space em todos os meses
 *    → headerIndexCI_ + colCI_ eliminam o problema
 */
function ingestRepasse_(competencia, sourcesFolder, dedupe, aliasMap, canonMap, pendBatch) {
  const f = findFolderByAliases_(sourcesFolder, CFG.DRIVE.PAGAMENTOS);
  if (!f) return 0;

  const book = openMostRecentGoogleSheet_(f);
  const sh   = openSheetCI_(book, "repasse");
  if (!sh) return 0;

  const values = sh.getDataRange().getValues();
  if (values.length < 2) return 0;

  // Detecta a linha de cabeçalho (pula as linhas vazias do topo)
  const hRow = findHeaderRow_(values, ["apto","apartamento","proprietario","proprietário"]);
  const idx  = headerIndexCI_(values[hRow]);

  const cApto  = colCI_(idx, "Apto", "Apartamento");
  const cProp  = colCI_(idx, "Proprietário", "Proprietario", "Nome");
  const cPix   = colCI_(idx, "Pix", "Pix ", "Chave Pix");   // "Pix " com espaço é o mais comum
  const cValor = colCI_(idx, "Valor");
  const cCom   = colCI_(idx, "Comissão", "Comissao");
  const cPago  = colCI_(idx, "Pago");

  if (cApto === undefined || cValor === undefined) return 0;

  const out = [];
  for (let r = hRow + 1; r < values.length; r++) {
    const row = values[r];
    const apt = row[cApto];
    if (!apt) continue;

    // REPASSE: diferente das demais fontes, cada linha JÁ indica o
    // proprietário explicitamente (coluna Proprietário). Para apartamentos
    // com múltiplos donos, usa o nome da própria linha para escolher o
    // property_id correto entre os candidatos, em vez de replicar para
    // todos (replicar aqui causaria repasse duplicado para o dono errado).
    const prop  = cProp  !== undefined ? String(row[cProp]  ?? "") : "";
    const { propertyIds, aptoNorm } = resolveAll_(aliasMap, canonMap, "pagamentos", apt);
    let propertyId = propertyIds.length > 0 ? propertyIds[0] : "";
    if (propertyIds.length > 1 && prop) {
      // Cada pid tem o formato "{owner_id}__{NOME_APTO_NORM}". Busca em
      // dim_proprietario o owner_id cujo nome_proprietario combine com o
      // nome da coluna Proprietário desta linha de repasse.
      const propNorm = normalize_(prop);
      const dimPropTbl = readTable_(CFG.SHEETS.DIM_PROP);
      const matched = propertyIds.find(pid => {
        const ownerIdDoPid = pid.split("__")[0];
        const propRow = dimPropTbl.rows.find(r =>
          String(r[dimPropTbl.idx["owner_id"]]||"").trim() === ownerIdDoPid);
        if (!propRow) return false;
        const nomeProp = normalize_(propRow[dimPropTbl.idx["nome_proprietario"]] || "");
        return nomeProp.includes(propNorm) || propNorm.includes(nomeProp);
      });
      if (matched) propertyId = matched;
    }
    const valor = Number(row[cValor] || 0);
    const com   = Number(cCom !== undefined ? (row[cCom] ?? 0) : 0);
    const pix   = cPix   !== undefined ? String(row[cPix]   ?? "") : "";
    const pago  = cPago  !== undefined ? String(row[cPago]  ?? "") : "";

    const key = `RPS|${competencia}|${normalize_(apt)}|${valor.toFixed(2)}|${com.toFixed(2)}|${pix}|${propertyId}`;
    if (dedupe.repasse.has(key)) continue;
    dedupe.repasse.add(key);

    out.push([competencia, "repasse", key, String(apt), aptoNorm, propertyId,
      prop, pix, valor, com, pago]);
  }

  // Limpeza da competência atual já ocorre centralmente em
  // runMonthlyUpdateFixedRoot (correção do bug append-only).
  appendRows_(CFG.SHEETS.FACT_REPASSE, out);
  return out.length;
}

// ─────────────────────────────────────────────────────────────────────────────
// INGESTÃO — Despesas (Consolidado)  ★ CORRIGIDA V5.1
// ─────────────────────────────────────────────────────────────────────────────
/*
 * Problemas resolvidos:
 *  • normalizeTipoDespesa_ agora também remove acentos antes de comparar
 *    → "Condomínio" encontrado mesmo sem acento no dado bruto
 *  • headerIndexCI_ substitui headerIndexLoose_ — mais preciso e consistente
 */
function ingestDespesas_(competencia, sourcesFolder, dedupe, aliasMap, canonMap, pendBatch) {
  const f = findFolderByAliases_(sourcesFolder, CFG.DRIVE.PAGAMENTOS);
  if (!f) return { inserted:0, duplicated:0 };

  const book = openMostRecentGoogleSheet_(f);
  const sh   = openSheetCI_(book, "consolidado");
  if (!sh) throw new Error('Pagamentos: aba "Consolidado" não encontrada.');

  const values = sh.getDataRange().getValues();
  if (!values || values.length < 2) return { inserted:0, duplicated:0 };

  const idx   = headerIndexCI_(values[0]);
  const cApto = colCI_(idx, "Apto", "Apartamento");
  const cProp = colCI_(idx, "Proprietário", "Proprietario");
  const cTipo = colCI_(idx, "Tipo", "Categoria", "Tipo/Categoria");
  const cValor= colCI_(idx, "Valor");

  if (cApto===undefined || cTipo===undefined || cValor===undefined)
    throw new Error('Consolidado: preciso das colunas "Apto", "Tipo" e "Valor".');

  const out = []; let dup = 0;

  for (let r = 1; r < values.length; r++) {
    const row   = values[r];
    const apt   = row[cApto];
    if (!apt) continue;

    const tipoRaw = row[cTipo];
    const tipo    = normalizeTipoDespesa_(tipoRaw);
    const valor   = parseMoneyCell_(row[cValor]);
    if (!valor || valor === 0) continue;

    // MULTI-DONO: despesas fixas do apartamento (condomínio, energia etc.)
    // são do imóvel como um todo, não de um proprietário específico —
    // replica para cada property_id, igual a Manutenção e Reposição.
    const { propertyIds, aptoNorm } = resolveAll_(aliasMap, canonMap, "pagamentos", apt);
    if (propertyIds.length === 0) queuePend_(pendBatch, competencia, "pagamentos", apt, aptoNorm,
      `Apartamento em despesas (${tipo}) não encontrado no dim_imovel.`);

    const propLinha = String(cProp !== undefined ? (row[cProp] || "") : "");
    const pidsParaGravar = propertyIds.length > 0 ? propertyIds : [""];
    for (const propertyId of pidsParaGravar) {
      const key = `DES|${competencia}|${tipo}|${normalize_(apt)}|${round2_(valor).toFixed(2)}|${propertyId}`;
      if (dedupe.despesas.has(key)){ dup++; continue; }
      dedupe.despesas.add(key);

      out.push([competencia, "pagamentos", key, String(apt), aptoNorm, propertyId,
        tipo, String(tipoRaw || tipo), round2_(valor), "", propLinha]);
    }
  }

  // Limpeza da competência atual já ocorre centralmente em
  // runMonthlyUpdateFixedRoot (correção do bug append-only).
  appendRows_(CFG.SHEETS.FACT_DES, out);
  return { inserted: out.length, duplicated: dup };
}

// ─────────────────────────────────────────────────────────────────────────────
// AGG
// ─────────────────────────────────────────────────────────────────────────────
// ─────────────────────────────────────────────────────────────────────────────
// INGESTÃO — Devolução de Taxa de Limpeza
// Pasta opcional (nem todos os meses têm devoluções).
// Cada linha representa um apartamento cujo valor de taxa de limpeza
// NÃO será cobrado do proprietário: o valor devolvido é somado ao resultado
// final no agregado (receita_liquida += valor_devolvido), reduzindo os custos.
// ─────────────────────────────────────────────────────────────────────────────
function ingestDevolucao_(competencia, sourcesFolder, dedupe, aliasMap, canonMap, pendBatch) {
  const f = findFolderByAliases_(sourcesFolder, CFG.DRIVE.DEVOLUCAO);
  if (!f) return 0;   // pasta não existe neste mês — normal, segue sem erro

  const book = openMostRecentGoogleSheet_(f);
  if (!book) return 0;
  const sh   = book.getSheets()[0];
  const data = sh.getDataRange().getValues();
  if (data.length < 2) return 0;

  const idx = headerIndexCI_(data[0]);

  // Colunas esperadas na planilha de devolução.
  // Aceita múltiplos nomes possíveis para robustez.
  const cApto  = colCI_(idx, "Apto", "Apartamento", "Acomodacao", "Acomodação", "Imovel", "Imóvel");
  const cValor = colCI_(idx,
    "Valor a ser devolvido", "Valor a Ser Devolvido",
    "Valor da Devolucao",    "Valor da Devolução",
    "Valor Devolvido",       "Valor",
    "Taxa de Limpeza",       "Total de Taxas",
    "Devolucao",             "Devolução");
  const cObs   = colCI_(idx, "OBS", "Obs", "Observação", "Observacao", "Motivo");

  if (cApto === undefined || cValor === undefined) {
    Logger.log(`AVISO ingestDevolucao_: colunas Apto ou Valor não encontradas. Cabeçalho: ${data[0].join(" | ")}`);
    return 0;
  }

  const out = [];
  for (let r = 1; r < data.length; r++) {
    const row = data[r];
    const apt  = row[cApto];
    if (!apt) continue;

    const valor = round2_(Number(row[cValor] ?? 0));
    if (valor <= 0) continue;

    const obs = cObs !== undefined ? String(row[cObs] ?? "") : "";

    const { propertyIds, aptoNorm } = resolveAll_(aliasMap, canonMap, "devolucao", apt);
    if (propertyIds.length === 0) {
      queuePend_(pendBatch, competencia, "devolucao", apt, aptoNorm,
        "Apartamento em devolução de taxa não encontrado no dim_imovel.");
    }

    const pidsParaGravar = propertyIds.length > 0 ? propertyIds : [""];
    for (const propertyId of pidsParaGravar) {
      const key = `DEV|${competencia}|${normalize_(apt)}|${valor.toFixed(2)}|${propertyId}`;
      if (dedupe.dev.has(key)) continue;
      dedupe.dev.add(key);

      out.push([competencia, "devolucao", key,
        String(apt), aptoNorm, propertyId,
        valor, obs]);
    }
  }

  appendRows_(CFG.SHEETS.FACT_DEV, out);
  return out.length;
}

function rebuildAggPrestacaoContas(competencia) {
  // Sempre lê TODAS as FACTs e sobrescreve o AGG completo (writeTableFull_).
  // Isso garante: sem duplicatas (sem merge kept+out), sem meses perdidos
  // (todas as FACTs são lidas independente do mês passado como argumento).
  // O argumento `competencia` é aceito para compatibilidade com as chamadas
  // em runMonthlyUpdateFixedRoot, mas não filtra a leitura — as FACTs já
  // estão corretas (removeRowsByCompetencia_ + reingestão) antes desta chamada.

  const factRes   = readTable_(CFG.SHEETS.FACT_RES);
  const factMan   = readTable_(CFG.SHEETS.FACT_MAN);
  const factRep   = readTable_(CFG.SHEETS.FACT_REP);
  const factDes   = readTable_(CFG.SHEETS.FACT_DES);
  const factDev   = readTable_(CFG.SHEETS.FACT_DEV);   // Devolução de taxa de limpeza
  const dimImovel = readTable_(CFG.SHEETS.DIM_IMOVEL);
  const dimProp   = readTable_(CFG.SHEETS.DIM_PROP);

  const imovelById = new Map();
  for (const r of dimImovel.rows) {
    const pid = String(r[dimImovel.idx["property_id"]]||"").trim();
    if (!pid) continue;
    imovelById.set(pid,{
      nome:          r[dimImovel.idx["nome_canonico"]],
      empreendimento:r[dimImovel.idx["empreendimento"]],
      unidade:       r[dimImovel.idx["unidade"]],
      ownerId:       String(r[dimImovel.idx["owner_id"]]||"").trim(),
      comissaoPct:   Number(r[dimImovel.idx["comissao_pct_mes"]]||DEFAULT_COMISSAO_PCT)
    });
  }

  const propById = new Map();
  for (const r of dimProp.rows) {
    const oid = String(r[dimProp.idx["owner_id"]]||"").trim();
    if (!oid) continue;
    propById.set(oid,{
      nome: r[dimProp.idx["nome_proprietario"]],
      doc:  String(r[dimProp.idx["cpf_cnpj"]]||"").trim()
    });
  }

  // Mapa nome_canonico → [property_ids] para replicar reservas a co-proprietários.
  // fact_reservas tem 1 linha por reserva real (Arquitetura A), mas o agg precisa
  // de 1 entrada por property_id para que cada dono veja os dados do imóvel.
  const siblingsByPid = new Map();
  const nomeToIds = new Map();
  for (const r of dimImovel.rows) {
    const pid  = String(r[dimImovel.idx["property_id"]]||"").trim();
    const nome = normalize_(r[dimImovel.idx["nome_canonico"]]);
    if (!pid || !nome) continue;
    if (!nomeToIds.has(nome)) nomeToIds.set(nome, []);
    if (!nomeToIds.get(nome).includes(pid)) nomeToIds.get(nome).push(pid);
  }
  for (const [, pids] of nomeToIds) {
    for (const pid of pids) siblingsByPid.set(pid, pids);
  }

  const agg    = new Map();
  const getA_  = (c,p) => {
    const k = `${c}||${p}`;
    if (!agg.has(k)) agg.set(k,{
      competencia:c, property_id:p,
      reservas:0, noites_no_mes:0, receita_liquida:0,
      manutencao_total:0, reposicao_total:0, despesas_total:0,
      itens_reposicao:0, qtd_itens:0,
      devolucao_limpeza:0
    });
    return agg.get(k);
  };

  for (const r of factRes.rows) {
    const c = parseCompetencia_(r[factRes.idx["competencia"]]);
    const p = String(r[factRes.idx["property_id"]]||"").trim();
    if (!c||!p) continue;
    const noites  = Number(r[factRes.idx["noites_no_mes"]]||0);
    const receita = Number(r[factRes.idx["receita_liquida_mes"]]||0);
    // Replica para todos os property_ids do mesmo apartamento (co-proprietários)
    const pids = siblingsByPid.get(p) || [p];
    for (const pid of pids) {
      const a = getA_(c, pid);
      a.reservas++;
      a.noites_no_mes   += noites;
      a.receita_liquida += receita;
    }
  }
  // ── MULTI-DONO: custos espelhados para todos os co-proprietários ──────────
  // Modelo de negócio (validado c/ Adriane em jul/2026): cada dono vê o
  // apartamento INTEIRO — receita, custos e resultado idênticos entre donos.
  //
  // As FACTs de custo podem existir em dois formatos:
  //   a) 1 linha por apartamento (ingestões antigas / alias com pid único)
  //   b) 1 linha por property_id (ingestões novas com resolveAll_)
  // Para agregar 1× por item e espelhar a todos os donos independentemente do
  // formato, deduplicamos pela chave SEM o sufixo |property_id (linhas
  // replicadas por co-propriedade diferem apenas nesse sufixo) e somamos o
  // valor em TODOS os pids irmãos do apartamento.
  const baseKey_ = (rawKey, pid) => {
    const s = String(rawKey||"");
    return s.endsWith(`|${pid}`) ? s.slice(0, -(pid.length+1)) : s;
  };
  const mirrorCost_ = (rows, idx, keyCol, seen, apply) => {
    for (const r of rows) {
      const c = parseCompetencia_(r[idx["competencia"]]);
      const p = String(r[idx["property_id"]]||"").trim();
      if (!c||!p) continue;
      const bk = `${c}|${baseKey_(r[idx[keyCol]], p)}`;
      if (seen.has(bk)) continue;
      seen.add(bk);
      for (const pid of (siblingsByPid.get(p) || [p])) apply(getA_(c, pid), r);
    }
  };

  mirrorCost_(factMan.rows, factMan.idx, "manut_key", new Set(), (a, r) => {
    a.manutencao_total += Number(r[factMan.idx["valor_total"]]||0);
  });
  mirrorCost_(factRep.rows, factRep.idx, "rep_key", new Set(), (a, r) => {
    a.reposicao_total += Number(r[factRep.idx["valor_unitario_ou_total"]]||0);
    a.itens_reposicao++;
    a.qtd_itens += Number(r[factRep.idx["quantidade"]]||0);
  });
  mirrorCost_(factDes.rows, factDes.idx, "desp_key", new Set(), (a, r) => {
    a.despesas_total += Number(r[factDes.idx["valor"]]||0);
  });
  mirrorCost_(factDev.rows, factDev.idx, "dev_key", new Set(), (a, r) => {
    a.devolucao_limpeza += Number(r[factDev.idx["valor_devolvido"]]||0);
  });

  const out  = [];
  const IM0  = { nome:"", empreendimento:"", unidade:"", ownerId:"", comissaoPct:DEFAULT_COMISSAO_PCT };
  const PR0  = { nome:"", doc:"" };

  for (const a of agg.values()) {
    const bounds = monthBounds_(a.competencia);
    if (!bounds) continue;
    const im  = imovelById.get(a.property_id) || IM0;
    const pr  = propById.get(im.ownerId)       || PR0;
    const dias = bounds.days || 0;
    const taxa_ocupacao = dias > 0 ? (a.noites_no_mes / dias) : 0;
    const diaria_media  = a.noites_no_mes > 0 ? (a.receita_liquida / a.noites_no_mes) : 0;
    const custos  = a.manutencao_total + a.reposicao_total + a.despesas_total;
    const tx_adm  = round2_(a.receita_liquida * im.comissaoPct);
    // A devolução de taxa de limpeza reduz os custos efetivos do proprietário:
    // o valor não cobrado é somado ao resultado (positivo para o proprietário).
    const dev_limp = round2_(a.devolucao_limpeza || 0);
    const resultado = round2_(a.receita_liquida - custos - tx_adm + dev_limp);
    out.push([
      bounds.competencia, pr.doc, pr.nome, im.ownerId,
      a.property_id, im.nome, im.empreendimento, im.unidade,
      a.noites_no_mes, dias, taxa_ocupacao,
      a.reservas, a.receita_liquida, diaria_media,
      im.comissaoPct, tx_adm,
      a.manutencao_total, a.reposicao_total, a.despesas_total,
      custos, resultado,
      a.itens_reposicao, a.qtd_itens, dev_limp
    ]);
  }

  out.sort((x,y) =>
    (String(x[0])+String(x[3])+String(x[4])).localeCompare(
     String(y[0])+String(y[3])+String(y[4]))
  );

  const headers = [
    "competencia","cpf_cnpj","nome_proprietario","owner_id","property_id",
    "nome_canonico","empreendimento","unidade","noites_no_mes","dias_no_mes",
    "taxa_ocupacao","reservas","receita_liquida","diaria_media","comissao_pct",
    "tx_adm","manutencao_total","reposicao_total","despesas_total",
    "custos_total","resultado","itens_reposicao","qtd_itens","devolucao_limpeza"
  ];

  // Sempre sobrescreve o AGG completo a partir dos dados acumulados de TODAS as FACTs.
  // As FACTs já estão corretas antes desta chamada (removeRowsByCompetencia_ + reingestão),
  // então out reflete o estado real de todos os meses — sem necessidade de merge com
  // linhas antigas (que era a causa original das duplicatas).
  writeTableFull_(CFG.SHEETS.AGG, headers, out);

  // Formatação numérica
  const sh = SpreadsheetApp.getActive().getSheetByName(CFG.SHEETS.AGG);
  const moneyCols = new Set(["receita_liquida","diaria_media","tx_adm",
    "manutencao_total","reposicao_total","despesas_total","custos_total","resultado"]);
  for (let c = 0; c < headers.length; c++) {
    const col = headers[c];
    if (!out.length) continue;
    const rng = sh.getRange(2, c+1, out.length, 1);
    if (moneyCols.has(col))                           rng.setNumberFormat('"R$" #,##0.00');
    if (col==="taxa_ocupacao"||col==="comissao_pct")  rng.setNumberFormat('0.00%');
  }
}

// ─────────────────────────────────────────────────────────────────────────────
// LOG
// ─────────────────────────────────────────────────────────────────────────────
function appendLogV5_(tsInicio, tsFim, competencia, log) {
  const dur = Math.max(0, Math.round((tsFim - tsInicio) / 1000));
  SpreadsheetApp.getActive().getSheetByName(CFG.SHEETS.LOG).appendRow([
    tsInicio, tsFim, dur, competencia, log.status, log.msg,
    log.res, log.man, log.rep, log.des, log.repasse,
    log.dedup_res, log.dedup_des, log.pendentes
  ]);
}