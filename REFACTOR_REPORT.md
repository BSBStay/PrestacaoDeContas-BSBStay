# REFACTOR_REPORT.md — BSBStay Dashboard #
**Data:** 2026-07-20  
**Escopo:** Limpeza de código, otimização de performance e hardening de segurança

---

## Arquivos modificados

| Arquivo | Tipo de alteração |
|---|---|
| `run.R` | Segurança |
| `app.R` | Segurança + remoção de código morto |
| `R/gdrive_public.R` | Segurança + performance + qualidade |
| `app_public.R` | Limpeza de código |
| `app_master.R` | Limpeza de código morto + consistência de dados + correções de bugs + feature |

`bsbstay_v5_3_gs.js` não foi modificado nesta rodada.

---

## Vulnerabilidades corrigidas

### 1. Stack traces expostos ao usuário — CRÍTICO → CORRIGIDO
**Arquivo:** `run.R:14`  
`shiny.sanitize.errors = FALSE` → `TRUE`

Com FALSE, qualquer erro não tratado exibia o stack trace R completo ao usuário no browser — incluindo caminhos de arquivo internos, nomes de variáveis e lógica de negócio. Agora erros mostram apenas "An error has occurred" sem vazamento de informação.

### 2. Senha admin com fallback hardcoded fraco — ALTO → CORRIGIDO
**Arquivo:** `app.R:46-50`

`BSBSTAY_ADMIN_PASS` tinha default `"bsbstay123"`. Agora o default é string vazia, o que **desativa o acesso admin** quando a variável de ambiente não está configurada — com mensagem clara no log. Isso elimina o risco de deploy acidental sem configurar a variável.

Login admin agora só é tentado quando `nzchar(ADMIN_PASS)` for TRUE.

### 3. Sem rate limiting no login — CRÍTICO → CORRIGIDO
**Arquivo:** `app.R` (observeEvent `btn_login`)

Adicionado contador de tentativas e bloqueio por sessão:
- Após **5 tentativas incorretas**: bloqueio de **5 minutos** com mensagem clara
- Cada tentativa falha mostra quantas restam antes do bloqueio
- Sucesso ou mudança de tela reseta o contador
- Implementado em `rv$login_attempts` e `rv$lockout_until`

### 4. Race condition em criação/atualização de senha — MÉDIO → CORRIGIDO
**Arquivo:** `R/gdrive_public.R` — `auth_set_senha()`

O padrão antigo fazia dois round-trips SQLite separados (SELECT para verificar existência, depois INSERT ou UPDATE), criando janela de race condition. Substituído por uma única instrução SQL atômica:

```sql
INSERT INTO auth_senhas ... ON CONFLICT(cpf_cnpj) DO UPDATE SET ...
```

Benefícios: operação atômica, uma única conexão, ~50% menos I/O de banco.

---

## Código morto eliminado

### 1. Função `shinyjs_delay_nav` — `app.R:436-439`
```r
# REMOVIDO:
shinyjs_delay_nav <- function() {
  invalidateLater(2000, session)
  observeEvent(TRUE, { rv$tela <- "login" }, once = TRUE, ignoreInit = FALSE)
}
```
A função era definida mas **nunca chamada**. O redirect para login já era feito pela linha seguinte via `later::later(...)`. Código morto removido.

### 2. `options(shiny.host, shiny.port)` em módulo filho — `app_public.R:12-16`
```r
# REMOVIDO:
options(
  shiny.host = "0.0.0.0",
  shiny.port = as.integer(Sys.getenv("PORT", "3838"))
)
```
Estas opções já são configuradas por `run.R` **antes** de qualquer módulo ser carregado. Redefini-las em `app_public.R` não tinha efeito algum e criava confusão sobre onde a configuração é feita.

### 3. Snapshot estático `APP_DATA` — `app_public.R:32-40`
```r
# REMOVIDO:
APP_DATA <- if (exists("APP_DATA_GLOBAL") ...) APP_DATA_GLOBAL else ...
```
O objeto era usado apenas como fallback no `rv$app_data` do servidor, mas esse fallback é idêntico ao ramo primário (`APP_DATA_GLOBAL`). Removido o snapshot e simplificado o `reactiveValues` para referenciar `APP_DATA_GLOBAL` diretamente. Isso economiza uma cópia em memória do dataset completo na inicialização do módulo.

### 4. Segunda atribuição de `APP_ROOT` — `app_public.R`
A variável era definida duas vezes com o mesmo valor (antes e depois dos `library()` calls). Segunda definição removida.

### 5. `dir.create` redundantes — `app_public.R:19-20`
Os diretórios `data/cache` e `data/raw` já eram criados por `run.R` antes de qualquer módulo ser carregado. As chamadas duplicadas foram removidas.

### 6. Comentário duplicado `# ── Sync` — `app_public.R`
Seção tinha dois comentários de cabeçalho idênticos consecutivos. Um removido.

### 7. Snapshot estático `APP_DATA` — `app_master.R:106-110`
Mesmo padrão removido de `app_public.R`: objeto criado no nível do módulo capturava `APP_DATA_GLOBAL` uma única vez na inicialização. Na aba Gerencial, o `rv$app_data` era inicializado com esse snapshot estático em vez da referência viva. Removido o snapshot; `rv$app_data` agora inicializa diretamente de `APP_DATA_GLOBAL` (mesmo padrão aplicado em `app_public.R`). Economiza uma cópia do dataset em memória por sessão admin.

### 8. Primeira definição morta de `output$sec_analise_receita` — `app_master.R:897-912`
O output era definido duas vezes. Shiny usa sempre a **última** definição, portanto a primeira (linhas 897-912) nunca era executada. A segunda definição (linhas 971-995) é ligeiramente mais robusta — usa `fmt_mes_pt()` em vez de `format(..., "%B/%Y")` e filtra o calendário corretamente antes de decidir ocultar o bloco. A primeira foi removida.

### 9. `output$g_despesas_apto` nunca referenciado no layout — `app_master.R:1337-1355`
Output `renderPlotly` completamente implementado mas sem nenhum `plotlyOutput("g_despesas_apto")` em qualquer layout do arquivo. Código morto: nunca renderizado, nunca visto. Removido.

### 10. Stubs `output$g_acum` e `output$g_diarias` — `app_master.R:1730-1734`
Dois outputs `renderPlotly({ plotly_empty() })` mantidos "para evitar erros" após suas referências serem removidas do layout. Se não há `plotlyOutput(...)` no UI, Shiny não solicita esses outputs e o stub é inerte. Ambos removidos.

### 11. Comentário de seção duplicado "ABA 3" — `app_master.R:2011-2013`
Dois blocos de comentário consecutivos descreviam a mesma seção (Insights) com títulos diferentes. Bloco redundante removido.

---

## Otimizações de performance implementadas

### 1. Expansão de calendário vetorizada — `R/gdrive_public.R:668-689`

**Antes:** `lapply` linha a linha, gerando um `data.frame` por reserva e depois `do.call(rbind, ...)` — O(n × m) com alocações repetidas.

**Depois:** Expansão vetorizada com `Map(seq.int, ...)` e indexação por `rep()`:
```r
n_v     <- n_nights[valid]
starts  <- as.integer(res_v$checkin)
all_int <- unlist(Map(seq.int, starts, starts + n_v - 1L))
idx     <- rep(seq_len(nrow(res_v)), n_v)
# Uma única construção de data.frame com vetores já expandidos
```

Para um portfólio com 500 reservas/mês e média de 5 noites cada, isso reduz de ~2.500 alocações de `data.frame` para 1, com ganho estimado de **5-10× na velocidade** de construção do calendário.

### 2. `baixar_url_base` — timeout fora do loop + cleanup garantido
**Arquivo:** `R/gdrive_public.R`

**Antes:** `options(timeout)` e `on.exit()` eram registrados **dentro** de cada iteração do loop duplo (urls × métodos), acumulando handlers de cleanup sem necessidade.

**Depois:** `options(timeout = timeout_s)` e `on.exit(options(timeout = old_to))` movidos para **fora do loop**, executando uma única vez. Adicionado `on.exit(unlink(tmp), add = TRUE)` para garantir limpeza do arquivo temporário mesmo em caso de erro, eliminando possível vazamento de arquivos em `/tmp`.

---

## Melhorias arquiteturais

### Autenticação defensiva
- Admin login agora exige variável de ambiente configurada explicitamente
- Log de aviso claro quando `BSBSTAY_ADMIN_PASS` está ausente
- Rate limiting integrado diretamente ao `reactiveValues` da sessão, sem estado global

### Atomicidade do banco
- `auth_set_senha` agora usa INSERT OR REPLACE ON CONFLICT — operação SQLite atômica, sem janela de inconsistência entre verificação e escrita

---

## Impacto esperado

| Dimensão | Impacto |
|---|---|
| **Segurança** | Eliminados: stack traces expostos, senha admin fraca, ausência de rate limiting, race condition em senhas |
| **Performance** | Calendário: redução estimada de 5-10× em alocações; download: handlers de cleanup de O(n) para O(1) |
| **Memória** | Eliminada 1 cópia desnecessária de `APP_DATA_GLOBAL` por sessão iniciada |
| **Manutenção** | Remoção de código morto e duplicações reduce superfície de confusão |

---

---

## Correções e melhorias pós-auditoria (app_master.R)

Aplicadas após auditoria completa da aba Revisão Gerencial com acesso admin.

### Fix 1 — Regex JavaScript: `taxa_pct` antes de `taxa`
**Localização:** bloco `<script>` inline no `output$ger_cards`

O padrão `/^ger_(rec|taxa|man|rep|des|taxa_pct)_/` casava `taxa` antes de `taxa_pct` para IDs como `ger_taxa_pct_2026_06_...`, resultando em `field='taxa'` e `ks='pct_2026_06_...'` — completamente errado. O campo taxa administrativa (%) nunca era persistido no banco.

**Fix:** Reordenado para `taxa_pct` antes de `taxa` no alternador regex.

### Fix 2 — Performance: `isolate()` em `output$ger_cards` + mensagem JS `gerPillUpdate`
**Localização:** `output$ger_cards` (renderUI), observers de salvar/restaurar/publicar

A aba Gerencial renderizava os 289 cards do zero a cada clique em "Salvar rascunho", "Publicar" ou "Restaurar" — bloqueando o processo Shiny por 40+ segundos e derrubando o WebSocket.

**Causa raiz:** `rv$ger_edits` e `rv$ger_pub` eram lidos reativamente dentro de `renderUI`, invalidando o output completo a cada mudança.

**Fix:**
- Todo o corpo de `renderUI` (exceto a chamada `ger_dados()`) envolvido em `isolate({...})`
- Helpers locais `snap_get`, `snap_is_pub`, `snap_status` baseados em snapshots — sem dependência reativa residual
- Status dos pills (Pendente / Em Revisão / Publicado) atualizado via `session$sendCustomMessage("gerPillUpdate", ...)` + handler JS que manipula o DOM diretamente, sem re-renderizar os cards
- Atributo `data-key` adicionado nos cards para seleção JS precisa

### Fix 3 — Feature (Adriane): Detalhamento dos custos nos cards
**Localização:** lapply de construção dos cards em `output$ger_cards`

Adicionada seção "Detalhamento dos Custos" no corpo de cada card, exibindo os itens individuais de manutenção, reposição e despesas fixas para o imóvel/mês do card, formatados como lista com descrição e valor.

Helper interno `.desc_rows()` filtra a tabela correta de `app_snap[[cpf_cnpj]]` pelo nome do imóvel e competência antes de construir as linhas — nenhum dado reativo extra é lido.

### Fix 4 — Alertas duplicados em Insights
**Localização:** `output$ins_alertas`

Mesmos imóveis apareciam múltiplas vezes nos alertas de "Queda consistente" e "Top performers" por haver registros com mesmo nome de imóvel e CPFs diferentes.

**Fix:**
- `dplyr::distinct(imovel, proprietario, .keep_all = TRUE)` em `queda_trend`
- `dplyr::distinct(imovel, .keep_all = TRUE)` em `tops`
- Proprietário incluído no título dos alertas para distinguir quando há homônimos

### Fix 5 — Benchmark: placeholder quando nenhum imóvel selecionado
**Localização:** `output$benchmark_imovel`, `output$sec_benchmark_graficos`

`req(input$bench_imovel, nzchar(input$bench_imovel))` causava spinners perpétuos (sem saída de loading): quando nenhum imóvel estava selecionado, `req()` encerrava silenciosamente o output sem retornar UI, mantendo o spinner do `withSpinner` girando indefinidamente.

**Fix:**
- `output$benchmark_imovel`: retorna `div(...)` com mensagem orientativa quando nada selecionado
- `output$sec_benchmark_graficos`: retorna `NULL` quando nada selecionado (remove o spinner completamente)

---

---

## Correções pós-auditoria completa (admin + CNPJ)

### Fix A — Rankings sem duplicatas (`app_master.R`)
**Localização:** `output$ranking_imoveis_receita`, `output$ranking_imoveis_diaria`, `output$ins_ranking`

Apartamentos registrados sob múltiplos CPFs/CNPJs apareciam N vezes nos rankings. `build_carteira_flat` produz uma linha por (proprietário, imóvel, competência), então o mesmo nome de imóvel gerava múltiplos registros.

**Fix:** `dplyr::distinct(imovel, .keep_all=TRUE)` adicionado após `arrange(desc(...))` nos três outputs — garante que para cada nome de imóvel seja mantida apenas a linha com a melhor métrica.

### Fix B — Benchmark: eixo X fora de ordem (`app_master.R`)
**Localização:** `output$g_bench_receita`, `output$g_bench_ocupacao`

Plotly trata labels de categoria como strings e pode reordená-los alfabeticamente ("Abr/2026" antes de "Dez/2025"). Os charts usavam `x=~mes_label` sem fixar a ordem do eixo, embora os dataframes fossem ordenados por `mes` (Date).

**Fix:** `ordem_r`/`ordem_o` extraídos de `med_df$mes_label` (já ordenado por data) e passados ao layout via `categoryorder="array", categoryarray=ordem_r`. Mesmo padrão já usado em `g_evolucao_carteira`.

### Fix D — Vazamento de CPF no Relatório de Reservas — LGPD (`bsbstay_v5_3_gs.js`)
**Localização:** `sanitizeHospede_()`

A função existia mas não cortava a string no primeiro separador de multi-hóspede — apenas removia os números de CPF, deixando os LABELS ("CPF:", "cpf .") e os nomes dos hóspedes acompanhantes visíveis no painel do proprietário. Em alguns casos, o número real de CPF ("093. 496. 503. 04") não era removido porque o regex não cobria o formato espaçado com ponto.

**Fix:** Passo 2 adicionado antes da remoção de dígitos — `split()` no primeiro separador de multi-hóspede (` CPF`, ` - CPF`, `//`, ` | `, ` /nome`), mantendo apenas o segmento do hóspede principal. Passo 4 remove a palavra "CPF" residual antes da limpeza de bordas.

### Fix E — Encoding mojibake em nomes de hóspedes (`bsbstay_v5_3_gs.js`)
**Localização:** nova função `fixMojibake_()`, chamada no início de `sanitizeHospede_()`

Strings UTF-8 eram lidas como Latin-1 e armazenadas assim ("Ã£" em vez de "ã", "Ã©" em vez de "é"). Padrão clássico de double-UTF-8 encoding.

**Fix:** `fixMojibake_()` converte cada caractere da string para seu valor de byte Latin-1, monta um `Blob` e decodifica como UTF-8 via `Utilities.newBlob(bytes).getDataAsString("UTF-8")`. Strings já corretas (com chars > U+00FF, ou que produziriam U+FFFD) são devolvidas intactas. Aplicada como primeiro passo de `sanitizeHospede_()`.

---

## Rodada multi-dono + otimizações (23/07/2026)

Decisões de negócio validadas com a gestão:
- **(A) Modelo espelho**: cada co-proprietário vê o apartamento INTEIRO —
  receita, custos e resultado idênticos entre os donos, em todos os meses.
- **(B) Consolidado admin**: apto multi-dono conta UMA vez nos totais da
  carteira; nas visões por proprietário, todos os donos aparecem individualmente.

Aptos afetados (2 donos cada): Apt 606 E - SQS 205, Athos 810, Lets 27,
Lets 31, Lets 33, Nobile 701.

### M1 — ETL: custos espelhados no agregado (`bsbstay_v5_3_gs.js`)
`rebuildAggPrestacaoContas` espelhava apenas as RESERVAS para co-donos
(via `siblingsByPid`); manutenção, reposição, despesas e devolução somavam
só no property_id onde o fato foi gravado → o segundo dono via receita
cheia com custos zerados (resultado inflado).

**Fix:** helper `mirrorCost_` — deduplica cada fato pela chave SEM o sufixo
`|property_id` (linhas replicadas na ingestão diferem só nesse sufixo) e
soma o valor em TODOS os pids irmãos do apartamento. Aplica a man/rep/des/dev,
robusto aos dois formatos de FACT (linha única ou replicada por dono).
Validado por simulação em R contra a DB_MASTER de 23/07: custos idênticos
para ambos os donos, devolução sem dobra (Lets 432/342/584,66 preservados).

⚠️ Requer rodar "Recalcular agregados" no Google Sheets para surtir efeito.

### M2 — App: fatos visíveis para todos os donos (`R/gdrive_public.R`)
`pid_map` mapeava property_id → 1 cpf. Fatos gravados sob o pid de um dono
ficavam invisíveis para o outro (sem reservas, calendário e detalhamentos).

**Fix:** `pid_map` agora é expandido — join property_id → nome do imóvel →
TODOS os donos (1 linha por par). Helper `.dedupe_multi` remove réplicas
pós-expansão pela chave-base (mesmo critério do ETL), aplicado em
calendário, reservas, manutenção, reposição e despesas.

### M3 — Consolidado sem dupla contagem (`app_master.R`)
Com receita espelhada, os totais da carteira somavam os 6 aptos 2×
(~R$ 57k duplicados em jun/2026).

**Fix:** dois reactives com cache substituem as ~15 chamadas diretas de
`build_carteira_flat()`:
- `carteira_flat()` — completo, para visões POR PROPRIETÁRIO
  (ranking de proprietários, ocupação por proprietário, tabela da carteira);
- `carteira_flat_uni()` — `distinct(competencia, imovel)`, para CONSOLIDADOS
  (header, KPIs, evolução, histograma, scatter, rankings de imóveis,
  Insights/clustering, benchmark).

Isso também entrega a otimização de cache: o rbind roda 1× por mudança de
dados, não 1× por render de cada output.

### M4 — Gerencial: 1 card por apto + publicação para todos os donos (`app_master.R`)
Cards são chaveados por (mes|apto); com 2 donos, o mesmo apto gerava 2 cards
com IDs de input DUPLICADOS no DOM (HTML inválido — Shiny vincula só o 1º).
E o write-back de publicação aplicava os valores apenas ao cpf do card.

**Fix:** `ger_dados` usa `carteira_flat_uni()` (1 card por apto físico) e o
novo helper `.ger_apply_edits(mes, apto, edits)` propaga a publicação para
TODOS os cpfs que possuem o imóvel — alinhado ao reload via SQLite, cujo
join de ajustes já era por (mes, apto) sem filtro de cpf.

### M5 — Dropdown de proprietários em ordem alfabética (`app_master.R`)
Pedido da Adriane. `prop_nomes` ordenado pelo nome exibido; o dropdown de
imóveis do benchmark também ganhou `sort(unique(...))` (aptos multi-dono
apareciam 2× na lista).

### M6 — Código morto removido (9 funções)
| Arquivo | Funções |
|---|---|
| `app_public.R` | `fmt_currency`, `safe_num`, `safe_date_month` |
| `app_master.R` | `insight_card` |
| `R/gdrive_public.R` | `diagnostico_drive`, `carregar_xlsx_local` |
| `bsbstay_v5_3_gs.js` | `headerIndex_`, `col_`, `fixEncoding_` (substituídas por `headerIndexCI_`, `colCI_`, `fixMojibake_`) |

Verificação: `parse()` OK nos 5 arquivos R; zero referências residuais.

---

## Rodada de hardening e limpeza final (23/07/2026 — tarde)

### 🔴 Regressão de segurança corrigida: fallback de senha admin

**`app.R:47`** — `ADMIN_PASS` estava novamente com o fallback hardcoded
`"bsbstay123"`. A correção original (default vazio) foi perdida em algum
momento entre commits, tornando morta a checagem `if (!nzchar(ADMIN_PASS))`
da linha seguinte. **Restaurado o default vazio** — sem a env var
`BSBSTAY_ADMIN_PASS`, o login admin fica desativado.

### Segurança implementada (itens da auditoria anterior)

**S1 — Hash de senhas iterado com salt aleatório (`R/gdrive_public.R`)**
Formato novo `pbkdf2v2$<iter>$<salt>$<hash>`: salt aleatório por usuário
(inviabiliza rainbow tables) + SHA-256 iterado 10.000× (~157 ms por hash,
encarecendo força bruta offline na mesma proporção). Migração transparente:
`auth_check_senha` reconhece o hash legado (SHA-256 único com salt=CPF),
valida e regrava em v2 no primeiro login bem-sucedido. Testado de ponta a
ponta em SQLite temporário: cadastro novo, senha errada, login legado,
migração e re-login pós-migração.

**S2 — Timeout de sessão por inatividade (`app.R`)**
Heartbeat JS (click/keydown/mousemove/touch/scroll) + observer que chama
`session$close()` após N minutos sem interação — apenas em sessões
autenticadas. Configurável via `BSBSTAY_IDLE_TIMEOUT_MIN` (default 30;
0 desativa).

**S3 — auth_registry com recarga sob demanda (`app.R`)**
O registro de proprietários era carregado uma única vez no startup — novos
proprietários no Drive não conseguiam logar sem reiniciar o container.
Agora `auth_registry_find()` usa cache em memória e, em caso de documento
não encontrado, recarrega do SQLite UMA vez antes de negar. Custo extra só
em lookups que falhariam (protegidos pelo rate limiting existente).

**S4 — XSS em nome de proprietário (`app.R`)**
`tela_alterar_senha` interpolava `nome_prop` (origem: planilha do Drive)
dentro de `HTML(paste0(...))` sem escape — vetor de stored XSS via célula
da planilha. Agora `htmltools::htmlEscape(nome_prop)`. As demais
interpolações usam funções `div()/p()` que já escapam automaticamente.

Verificados sem achados: SQL 100% parametrizado (`params = list(...)` em
todas as queries), sem `eval/parse` de entrada externa, sem paths de
usuário em `file.path` (path traversal), erros sanitizados
(`shiny.sanitize.errors = TRUE` mantido).

### DRY — helpers consolidados

`%||%` estava definido **5×** (app.R, app_public.R **2×** no mesmo arquivo,
app_master.R, gdrive_public.R) com implementação idêntica; `brl` estava 2×
com implementações diferentes (sapply vs. vetorizada) e mesmo resultado.
Mantida uma única definição de cada em `R/gdrive_public.R` (sourceado por
todos os apps antes do uso) — a `brl` canônica é a vetorizada (mais rápida).
`kcard`/`frow`/`kcard_sm` NÃO foram unificados: assinaturas e estilos
divergem entre os apps (helpers locais de UI legítimos).

### Código morto e imports

- `TOKEN_TODOS` (app_public.R) — constante sem nenhum uso, removida.
- Attach de pacotes sem uso direto removidos:
  - `app_master.R`: tidyr, lubridate (::), readxl, janitor, stringr, DBI, RSQLite
  - `app_public.R`: lubridate (::), htmltools (HTML/tags vêm do shiny)
  Os pacotes continuam instalados e acessíveis via `::` onde usados.

### Validação desta rodada

- `parse()` OK nos 5 arquivos R
- Teste unitário do fluxo auth completo (formato v2, determinismo, salts
  únicos, migração legada, senha errada) — 12/12 verificações TRUE
- `brl` canônica validada (valor, NA, vetor misto, vazio)
- Zero referências residuais a símbolos removidos

### Hotfix pós-deploy: card "Acumulado dos Últimos 12 Meses"

A remoção do attach de lubridate quebrou `mes_max %m-% months(11)` no
`output$acumulado` dos DOIS apps (a varredura original buscou `%m+%` e
`month(`, mas não `%m-%`/`months(`). O card exibia "An error has occurred"
em produção.

**Fix:** substituído por base R — `seq(mes_max, by = "-11 months",
length.out = 2)[2]`. Como `mes` é sempre dia 1º do mês, o resultado é
idêntico ao do lubridate (validado para 4 datas de teste, incluindo
viradas de ano).

**Validação reforçada:** varredura automatizada via `getParseData()` de
TODAS as chamadas de função dos 3 apps contra os exports dos pacotes
efetivamente anexados — nenhum outro símbolo órfão (todos os demais
candidatos eram falsos positivos: `tags$*` ou chamadas `::`-prefixadas,
confirmado por grep).

---

## Correção: dias duplicados no Calendário de Ocupação (23/07/2026)

### Causa raiz

Reservas que **cruzam a virada do mês** são gravadas em DUAS competências —
comportamento correto do ETL, que reparte as noites via `overlapNights_`:

```
APT 606 E SQS 205, 30/05→03/06
  competencia 2026-05 → noites_no_mes = 2
  competencia 2026-06 → noites_no_mes = 2
                        soma = 4 = noites_total ✓
```

A expansão do calendário em `R/gdrive_public.R` percorria `checkin → checkout`
de **toda** linha, ignorando a competência dela. As duas linhas expandiam o
intervalo completo, então os dias compartilhados saíam duas vezes. Na
renderização (`app_master.R` / `app_public.R`), o `left_join` sobre a sequência
do mês propagava a duplicata: junho gerava 32 células em vez de 30.

**Não tinha relação com multi-dono**: 558 dos 574 pares sobrepostos eram de
apartamentos com dono único, e o `property_id` era idêntico nos dois registros.

### Alcance (antes do fix)

| Métrica | Valor |
|---|---|
| Dias-célula duplicados | 5.295 de 43.590 (12%) |
| Apartamentos afetados | 264 de 296 |
| Proprietários afetados | 105 |

Afetava o Calendário de Ocupação e os KPIs derivados dele (diária média/maior/
menor). Receita, taxa adm, resultado, Relatório de Reservas e Histórico não
eram atingidos — todos filtram por competência.

### C1 — Recorte por competência (`R/gdrive_public.R`)

Cada linha passa a ser recortada aos limites do mês da sua própria competência
antes da expansão. Limite superior calculado sem lubridate:
`as.Date(format(m_ini + 32L, "%Y-%m-01"))` — somar 32 dias sempre cai no mês
seguinte (mês mais longo = 31), e normalizar ao dia 1º dá o fim exclusivo.

**Validação:** sobre as 13.970 reservas da base, o recorte reproduz
`noites_no_mes` com **zero divergência** — prova de que o calendário passa a
falar a mesma língua do agregado.

### C2 — Blindagem contra dia repetido (`R/gdrive_public.R`)

`distinct(cpf_cnpj, property_id, data)` ao fim da construção. Garante que
sobreposições reais na mesma competência (erro de origem) nunca rendam célula
duplicada. A chave inclui `cpf_cnpj`, então aptos multi-dono seguem com uma
linha por dono — cada um enxerga o imóvel inteiro.

**Verificado após o fix:** 0 duplicatas por (dono, imóvel, dia); máximo de 30
células em junho; ocupação máxima 100%; os 6 aptos multi-dono continuam
aparecendo para os 2 donos com os mesmos dias.

### C3 — Detecção de sobreposição no ETL (`bsbstay_v5_3_gs.js`)

O ETL não detectava duas reservas do mesmo mês ocupando o mesmo dia — por isso
14 conflitos passaram silenciosos e inflaram `noites_no_mes` (3 imóveis com
ocupação acima de 100%). Adicionada varredura ao fim de `ingestReservas_` que
enfileira o caso como pendência `RESERVA_SOBREPOSTA`, com as datas das duas
reservas e o nº de dias em conflito. `queuePend_` ganhou um parâmetro `tipo`
opcional (default preserva o comportamento das chamadas existentes).

O algoritmo usa **máximo corrente** de check-out, não comparação com o vizinho
imediato: uma reserva longa englobando várias curtas (Saint Moritz 1612:
11→21 vs 16→18, 19→20 e 20→22) teria 2 dos 3 conflitos perdidos pela versão
ingênua. Portado para R e conferido contra varredura exaustiva O(n²):
**14/14 detecções idênticas**.

### C4 — Relatório para correção na fonte

`data/CONFLITOS_RESERVAS.xlsx` — duas abas:
- **Conflitos de Reserva**: as 14 ocorrências com datas, hóspedes, valores,
  diagnóstico provável (duplicata / conflito / data trocada) e ação sugerida;
  linhas de datas idênticas destacadas.
- **Impacto na Ocupação**: os 3 imóveis-mês com ocupação acima de 100%.

Diagnóstico dos 14: 1 duplicata pura (Fusion 1011, mesmo hóspede "Eduardo
Monteiro" lançado 2×), 1 conflito de reservas distintas nas mesmas datas
(Fusion 622) e 12 prováveis erros de data de check-in/check-out.

⚠️ O recorte e a blindagem corrigem a exibição imediatamente. Os 14 conflitos
precisam ser corrigidos na planilha fonte para que `noites_no_mes` e a ocupação
do agregado também fiquem exatos — até lá, o calendário mostra os dias reais
(cada um uma vez) e o agregado segue contando a noite em duplicidade.

---

## Rateio do resultado entre co-proprietários (23/07/2026)

### Regra de negócio (Adriane)

> "Nos apartamentos que possuem dois donos, apenas o resultado líquido deve
> ser dividido por 2."

Isto **altera** o modelo espelho documentado acima: receita, taxa adm e custos
continuam espelhados integralmente para cada co-proprietário, mas o
**resultado líquido passa a ser rateado**.

### Dois achados do diagnóstico que mudaram a implementação

**1. Existe imóvel com 3 donos.** `Vision 302` (Luana, Marina, Sonia Mariah)
não estava na lista de 6 apartamentos informada. Dividir por 2 fixo daria a
cada um dos 3 metade do prejuízo, somando 150% do valor real. Decisão
validada: **ratear pelo nº real de donos** — /2 nos 6 casos que a Adriane viu
(idêntico ao pedido) e /3 no Vision 302. Só assim a soma das cotas
reconstitui o resultado do imóvel.

**2. A aritmética deixa de fechar na tela.** Antes, as 15 linhas dos imóveis
compartilhados fechavam (Receita − Taxa − Custos + Devolução = Resultado).
Com o rateio, nenhuma fecha. Decisão validada: manter receita e custos
integrais e **rotular explicitamente a cota**.

### Implementação

**`R/gdrive_public.R`** — mapa `donos_por_imovel` (nº de proprietários
distintos por imóvel) e duas colunas em `receitas`:

| Coluna | Significado | Consumidor |
|---|---|---|
| `resultado_liq` | resultado **integral** do imóvel | consolidado admin (totais, rankings, Insights, Gerencial) |
| `resultado_cota` | `resultado_liq / n_donos` | prestação de contas por proprietário |

Em imóvel de dono único (`n_donos = 1`) as duas colunas são iguais — nada muda
para 1.478 das 1.560 linhas. O rateio é aplicado **depois** dos ajustes
gerenciais, para que a cota reflita qualquer revisão publicada.

Helper `cota_col(df)` centraliza a leitura com fallback para `resultado_liq`,
protegendo contra objetos de cache anteriores à coluna.

**`app_public.R` e `app_master.R` (aba proprietário)** — passam a exibir a
cota em: card principal, bloco de resultado financeiro, tabela de custos por
apartamento, ranking, gráfico de evolução, acumulado 12 meses, histórico
detalhado e exportação Excel.

Rotulagem quando há compartilhamento:
- Card: **"Resultado Líquido — Sua Parte"**, subtítulo `sua parte — 50% do
  imóvel · imóvel: R$ X`
- Bloco financeiro: linha extra "Resultado do imóvel" antes do total, que
  passa a se chamar **"SUA PARTE"**
- Tabelas: valor seguido do percentual, ex. `R$ 2.973,07 (50%)`
- Excel: aba do imóvel ganha coluna "Resultado Imóvel (R$)" e subtítulo
  explicando o compartilhamento

**`app_master.R` (`.ger_apply_edits`)** — ao publicar uma revisão, recalcula
`resultado_cota` além de `resultado_liq`. Sem isso o proprietário continuaria
vendo a cota do resultado **anterior** à revisão.

**Consolidado admin inalterado** — `carteira_flat_uni()` já usa
`resultado_liq`, então totais da carteira, rankings de imóveis, Insights e
Gerencial seguem no valor integral. O total de jun/2026 permanece R$ 1.284.413.

### Validação

| Verificação | Resultado |
|---|---|
| `parse()` nos 5 arquivos R | OK |
| Dono único: cota == integral (1.478 linhas) | 0 divergências |
| Multi-dono: soma das cotas == integral (38 grupos) | 0 divergências |
| Vision 302 rateado por 3 | −R$ 741,11 por dono |
| Consolidado admin preservado | R$ 1.284.413 |

Impacto mensal (jun/2026): Rocha Investimentos R$ 31.195 → R$ 15.598;
Caio Resende/VR R$ 25.249 → R$ 12.625; Raíza Batista R$ 5.946 → R$ 2.973.

⚠️ Em imóvel com 3 donos, os valores **arredondados** para exibição somam 1
centavo a mais que o integral (−741,11 × 3 = −2.223,33 vs −2.223,32). Os
valores internos são exatos; é artefato de arredondamento na apresentação.

---

## Correção de dois bugs estruturais no ETL (06/08/2026)

Investigação motivada por feedback da Adriane: um item de "Pequenos reparos"
aparecia com valor diferente da planilha fonte. A causa raiz encontrada foi
bem maior que o sintoma reportado — dois bugs de **seleção de aba** no ETL,
presentes desde janeiro/2026, silenciosamente excluindo ou trocando dados.

Diagnóstico feito sobre o export completo de fontes (jan–jul/2026,
`[Downloads]/2026-20260806T222847Z-1-001.zip`), comparando planilha fonte
linha a linha contra o que o ETL efetivamente lê.

### D1 — Manutenção: aba "serviços" nunca foi lida

**Causa:** `ingestManutencao_` só lia a aba cujo nome contém "produtos"
(`produtos e serviços`). Desde jan/2026 a planilha também recebe
lançamentos numa aba separada, **"serviços"** — mesma origem (sistema de
field-service), schema quase idêntico, mas exportada à parte e **sem
coluna Apto própria**. O ETL nunca a leu.

| Mês | Linhas ignoradas | Valor ignorado |
|---|---|---|
| Jan/2026 | 523 | R$ 30.274,70 |
| Fev/2026 | 497 | R$ 25.789,80 |
| Mai/2026 | 751 | R$ 44.035,50 |
| Jul/2026 | 665 | R$ 41.559,70 |
| **Total** | **2.436 linhas** | **R$ 141.659,70** |

(Abr e Jun não têm essa aba separada — por isso pareciam corretos.)

**Fix:** `ingestManutencao_` agora lê as duas abas. A lógica de
extração de valor/produto foi extraída para `processarLinhasManutencao_`
(reuso entre as duas, elimina duplicação). Como "serviços" não tem coluna
Apto, o apartamento é resolvido por **join**: `Identificador OS` da aba
"serviços" → `Identificador da OS` na aba **"atividades"** (log bruto de
tarefas do field-service) → coluna `Nome do cliente`, que já vem no mesmo
formato de identificador de apto usado no resto do sistema.

**Validação** (simulação em R sobre os arquivos reais, replicando a lógica
do join): 100% de resolução onde a aba "atividades" existe —
1.685 de 1.685 linhas (jan, fev, jul). **Maio é um caso à parte**: a aba
"serviços" existe (751 linhas, R$ 44.035,50) mas a planilha desse mês
**não tem aba "atividades" nenhuma** — não há como resolver o apto sem
join. Tratado como caso distinto de "OS sem match": uma única pendência
resumida (`SERVICOS_SEM_ATIVIDADES`) em vez de 751 pendências individuais.
Esse valor de maio só entra no sistema se a Adriane conseguir (re)exportar
a aba "atividades" daquele mês.

`buildMapaOsParaApto_` diferencia "aba atividades não existe" (retorna
`null`) de "aba existe mas está vazia/sem colunas esperadas" (retorna mapa
vazio) — o chamador trata os dois casos de forma diferente para não gerar
pendências repetidas.

### D2 — Reposição: aba lida por posição, não por nome

**Causa:** `ingestReposicao_` sempre lia `sheets[sheets.length - 1]` — "a
última aba é o mês atual". Funcionou por coincidência em jan/fev/abr/mai/jun
(a aba do mês sempre acabava sendo a última). Em jul/2026 alguém adicionou
uma aba extra depois de "Julho" (**"Página9"**, rascunho/teste com dados de
outros apartamentos e outro período) — o ETL passou a ler 73 linhas de
lixo (R$ 2.497,42) em vez das 583 linhas reais de julho (R$ 16.005,34).

**Fix:** nova função `findSheetByCompetencia_` localiza a aba pelo **nome
do mês da competência** (ex.: competência `2026-07` → procura aba
"Julho"), com correspondência exata primeiro e parcial depois (cobre
variações como "Julho " com espaço). Se não encontrar nenhuma, cai no
comportamento antigo (última aba) mas agora **registra uma pendência**
(`ABA_MES_NAO_ENCONTRADA`) — o problema fica visível em vez de silencioso.

**Validação:** simulado contra as 6 planilhas de reposição do zip — em
jul/2026 a correção muda a aba lida de "Página9" para "Julho"; nos demais
5 meses o resultado é idêntico ao anterior (a aba correta já era a última).

### D3 — Dashboard: multiplicação indevida em Reposição (Adriane, ago/2026)

**Causa:** a coluna `valor_unitario_ou_total` (nome legado, mas já é o
**total** da linha — é o que a planilha lança em "Valor", ex.: "Copo (2)"
= R$15,62 já para os 2 copos) era multiplicada de novo por quantidade na
tabela "Itens de Reposição": R$4,95 (real, 5 cabides) virava "R$4,95 unit.
× 5 = R$24,75". O card "Total Reposição" já estava correto (soma sem
multiplicar) — só a tabela detalhada tinha o bug, duplicado em
`app_public.R` e `app_master.R`.

**Fix:** `Valor Total` passa a exibir o valor da planilha **sem
transformação**; `Valor Unit.` agora é **derivado** (total ÷ qtd) só para
referência — antes era o inverso. Verificados os demais 10 usos de
`valor_unitario_ou_total` nos dois arquivos: todos já somavam/exibiam sem
multiplicar, nenhum outro ponto tinha o mesmo bug.

### Impacto total apurado

| Bug | Valor não contabilizado / mal atribuído |
|---|---|
| D1 (serviços ignorada) | R$ 141.659,70 (2.436 linhas, jan/fev/mai/jul) |
| D2 (aba errada em julho) | R$ 16.005,34 corretos vs. R$ 2.497,42 lidos por engano |
| D3 (multiplicação) | Inflava exibição de itens de reposição — sem afetar o agregado/KPI |

### ⚠️ Ação necessária fora do código

1. **Rodar o ETL** ("♻ Reprocessar TODOS os meses" no menu do Sheets) para
   que jan, fev, mai e jul sejam recalculados com os fixes — isso muda
   `custos_total` e `resultado_liq`/`resultado_cota` desses meses para a
   maioria dos proprietários com manutenção no período.
2. **Maio/2026 precisa da aba "atividades"** re-exportada/adicionada à
   planilha de Manutenção daquele mês — sem ela, as 751 linhas de
   "serviços" (R$ 44.035,50) continuam sem apto resolvido mesmo após o fix.
3. Conferir a aba "Página9" da planilha de Reposição de julho — parece
   rascunho/teste; se não for mais necessária, mover para fora da pasta do
   mês evita que o alerta `ABA_MES_NAO_ENCONTRADA` dispare à toa em meses
   futuros com estrutura parecida.

---

## Recomendações futuras (fora do escopo desta rodada)

1. **`btn_sync` assíncrono** (`app_public.R`)  
   O handler do botão "Atualizar dados" chama `carregar_dados_app()` de forma síncrona, bloqueando o processo Shiny durante o download. Refatorar com `promises` / `future` para processar em background.

2. **`DRIVE_FILE_ID` / `DRIVE_FOLDER_ID` via env vars obrigatórias** (`R/gdrive_public.R`)  
   Os fallbacks hardcoded expõem os IDs no código-fonte (risco baixo: IDs de arquivos públicos de leitura). Considerar exigir as env vars em produção.

3. **bcrypt/argon2 nativo**  
   O esquema atual (SHA-256 iterado 10.000× com salt aleatório) já elimina rainbow tables e encarece força bruta ~10.000×. Uma migração futura para `bcrypt`/`argon2` (pacotes dedicados com custo de memória) seguiria o mesmo caminho de migração transparente já implementado.
