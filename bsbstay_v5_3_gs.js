/***********************
 * BSB3 / BSBSTAY — DB_MASTER V5.1

 ***********************/

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
    REPOSICAO:      ["Reposicao", "Reposição", "Resposicao"],
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
    .addItem("▶ Atualizar mês atual",          "runMonthlyUpdateFixedRoot")
    .addItem("⏭ Atualizar e avançar competência","runMonthlyUpdateAndAdvance")
    .addSeparator()
    .addItem("Recalcular agregados",            "rebuildAggPrestacaoContas")
    .addSeparator()
    .addItem("🗑 Limpar TODAS as FACTs",        "limparTodasFacts")
    .addItem("♻ Reprocessar TODOS os meses",   "reprocessarTodosMeses")
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

function reprocessarTodosMeses() {
  const meses = ["2025-12", "2026-01", "2026-02", "2026-04", "2026-05"];
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
  let s = String(v).trim();
  // Remove CPF: 000.000.000-00 ou 00000000000
  s = s.replace(/\d{3}\.?\d{3}\.?\d{3}[-.]?\d{2}/g, "").trim();
  // Remove RG: 0.000.000 ou 0000000 (7-10 dígitos seguidos)
  s = s.replace(/\d{1,2}\.?\d{3}\.?\d{3}[-.]?\d{0,2}/g, "").trim();
  // Remove passaporte: sequências alfanuméricas longas (ex: AB123456)
  s = s.replace(/[A-Z]{1,2}\d{6,}/gi, "").trim();
  // Remove sequências de 7+ dígitos soltos restantes
  s = s.replace(/\d{7,}/g, "").trim();
  // Limpa separadores órfãos
  s = s.replace(/^[\s\-\/|:;,]+|[\s\-\/|:;,]+$/g, "").trim();
  return s;
}

function normalizeOrigem_(o) {
  const s = String(o||"").toLowerCase();
  if (s.includes("airbnb")) return "Airbnb";
  if (s.includes("booking")) return "BookingCom";
  return "Outros";
}

function headerIndex_(row) {
  const idx={};
  row.forEach((h,i) => idx[String(h).trim()]=i);
  return idx;
}

function headerIndexLoose_(row) {
  const idx={};
  row.forEach((h,i) => { const k=normalize_(h); if(k) idx[k]=i; });
  return idx;
}

function col_(idxL, ...keys) {
  for (const k of keys) { const kk=normalize_(k); if(idxL[kk]!==undefined) return idxL[kk]; }
  return undefined;
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

function fixEncoding_(s) {
  return String(s||"")
    .replace(/â€"/g,"-").replace(/â€"/g,"-")
    .replace(/â€™/g,"'").replace(/â€œ/g,'"').replace(/â€/g,'"')
    .replace(/Ã£/g,"ã").replace(/Ã©/g,"é").replace(/Ã§/g,"ç");
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
  const aptoNorm  = normalize_(aptoOriginal);
  const aptoLoose = normalizeLoose_(aptoOriginal);
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

function queuePend_(batch, competencia, source, aptoOriginal, aptoNorm, obs) {
  batch.push([new Date(), competencia, source, "IMOVEL_NAO_CADASTRADO",
    aptoOriginal, aptoNorm, "", obs||""]);
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
 */
function ingestManutencao_(competencia, sourcesFolder, dedupe, aliasMap, canonMap, pendBatch) {
  const f = findFolderByAliases_(sourcesFolder, CFG.DRIVE.MANUTENCAO);
  if (!f) return 0;

  const book = openMostRecentGoogleSheet_(f);
  const sh   = openSheetCI_(book, "produtos");           // "produtos e serviços"
  const data = sh.getDataRange().getValues();
  if (data.length < 2) return 0;

  const idx = headerIndexCI_(data[0]);

  const cOsId   = colCI_(idx, "Identificador OS", "Identificador da OS", "OS", "ID OS");
  const cProd   = colCI_(idx, "Nome do Produto", "Produto", "Produto/Serviço");
  const cApto   = colCI_(idx, "Apto", "Apartamento");
  const cValTot = colCI_(idx, "Valor Total");
  const cValUni = colCI_(idx, "Valor Unitário", "Valor Unitario");
  const cQtd    = colCI_(idx, "Quantidade utilizada", "Quantidade", "Qtd");
  const cAdm    = colCI_(idx, "ADM/SUB", "ADM/Sublocado", "Adm/Sub", "ADM");
  const cObs    = colCI_(idx, "OBS", "Obser", "Observação", "Observacao");

  if (cApto === undefined || cProd === undefined) return 0;

  const out = [];
  for (let r = 1; r < data.length; r++) {
    const row = data[r];
    const apt = row[cApto];
    if (!apt) continue;

    // MULTI-DONO: replica a linha para cada property_id do apartamento.
    const { propertyIds, aptoNorm } = resolveAll_(aliasMap, canonMap, "manutencao", apt);
    if (propertyIds.length === 0) queuePend_(pendBatch, competencia, "manutencao", apt, aptoNorm,
      "Apartamento em manutenção não encontrado no dim_imovel.");

    const osId   = cOsId  !== undefined ? (row[cOsId]  ?? "") : "";
    const produto = cProd  !== undefined ? (row[cProd]  ?? "") : "";
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

  // NOTA: a limpeza da competência atual (correção do bug append-only)
  // já acontece centralizadamente em runMonthlyUpdateFixedRoot, ANTES do
  // dedup ser carregado — ver removeRowsByCompetencia_ ali.
  appendRows_(CFG.SHEETS.FACT_MAN, out);
  return out.length;
}

// ─────────────────────────────────────────────────────────────────────────────
// INGESTÃO — Reposição  ★ CORRIGIDA V5.1
// ─────────────────────────────────────────────────────────────────────────────
/*
 * Problemas resolvidos:
 *  • Nome da aba varia por mês: "Dezembro", "Janeiro ", "Fevereiro", "Abril"
 *    → openSheetCI_: usa a aba mais recente (primeira que contiver o mês OU última aba)
 *    → Estratégia: usa a última aba da planilha (que corresponde ao mês atual)
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

  // Usa a última aba (corresponde ao mês mais recente na planilha de reposição)
  const sh   = sheets[sheets.length - 1];
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
  for (const r of factMan.rows) {
    const c = parseCompetencia_(r[factMan.idx["competencia"]]);
    const p = String(r[factMan.idx["property_id"]]||"").trim();
    if (!c||!p) continue;
    getA_(c,p).manutencao_total += Number(r[factMan.idx["valor_total"]]||0);
  }
  for (const r of factRep.rows) {
    const c = parseCompetencia_(r[factRep.idx["competencia"]]);
    const p = String(r[factRep.idx["property_id"]]||"").trim();
    if (!c||!p) continue;
    const a = getA_(c,p);
    a.reposicao_total += Number(r[factRep.idx["valor_unitario_ou_total"]]||0);
    a.itens_reposicao++;
    a.qtd_itens += Number(r[factRep.idx["quantidade"]]||0);
  }
  for (const r of factDes.rows) {
    const c = parseCompetencia_(r[factDes.idx["competencia"]]);
    const p = String(r[factDes.idx["property_id"]]||"").trim();
    if (!c||!p) continue;
    getA_(c,p).despesas_total += Number(r[factDes.idx["valor"]]||0);
  }
  // Devolução de taxa de limpeza: acumula por imóvel/competência
  for (const r of factDev.rows) {
    const c = parseCompetencia_(r[factDev.idx["competencia"]]);
    const p = String(r[factDev.idx["property_id"]]||"").trim();
    if (!c||!p) continue;
    getA_(c,p).devolucao_limpeza += Number(r[factDev.idx["valor_devolvido"]]||0);
  }

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

  // CIRÚRGICO: se competencia foi passada, preserva linhas de outros meses.
  // Caso contrário (chamada manual sem argumento), reconstrói tudo.
  if (competencia) {
    // Normaliza usando parseCompetencia_ para garantir formato YYYY-MM consistente
    const compTarget = parseCompetencia_(competencia);
    const aggSheet   = SpreadsheetApp.getActive().getSheetByName(CFG.SHEETS.AGG);
    const lastRow    = aggSheet.getLastRow();
    if (lastRow >= 2) {
      const lastCol    = aggSheet.getLastColumn();
      const existRows  = aggSheet.getRange(2, 1, lastRow - 1, lastCol).getValues();
      const kept = existRows.filter(r => {
        const c = parseCompetencia_(r[0]);
        return c && c !== compTarget;   // preserva tudo que NÃO é a competência atual
      });
      // Normaliza colunas das linhas preservadas para o schema atual (headers.length).
      // Linhas antigas podem ter menos colunas (ex: antes de adicionar devolucao_limpeza).
      // Preenche colunas faltantes com "" para evitar erro de dimensão no setValues.
      const n = headers.length;
      const keptNorm = kept.map(r => {
        if (r.length === n) return r;
        const row = r.slice(0, n);
        while (row.length < n) row.push("");
        return row;
      });
      aggSheet.getRange(2, 1, lastRow - 1, lastCol).clearContent();
      const allRows = [...keptNorm, ...out];
      allRows.sort((x,y) =>
        (String(x[0])+String(x[3])+String(x[4])).localeCompare(
         String(y[0])+String(y[3])+String(y[4]))
      );
      if (allRows.length > 0) {
        aggSheet.getRange(2, 1, allRows.length, n).setValues(allRows);
      }
    } else {
      // Aba vazia — escreve cabeçalho + novas linhas
      writeTableFull_(CFG.SHEETS.AGG, headers, out);
    }
  } else {
    // Sem competencia: reconstrói tudo (usado em reconstrução manual completa)
    writeTableFull_(CFG.SHEETS.AGG, headers, out);
  }

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