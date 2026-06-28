# BSBStay — Contexto do Projeto para Claude Code

## Visão Geral

**BSBStay** é uma plataforma de gestão de aluguel de curta temporada em Brasília (DF).
O sistema consiste em um dashboard de prestação de contas mensal para proprietários de imóveis,
desenvolvido em R/Shiny com ETL em Google Apps Script.

**Contratante:** BSBStay  
**Desenvolvedor responsável:** Mateus (PMO Strategist / MIRAI Tecnologia)  
**Deploy:** Render.com Starter Plan  
**URL produção:** https://prestacaodecontas-bsbstay.onrender.com  
**Login admin:** usuário `admin`, senha `bsb123`

---

## Stack Técnica

| Camada | Tecnologia |
|---|---|
| ETL | Google Apps Script (`bsbstay_v5_3_gs.js`) |
| Banco de dados | Google Sheets (DB_MASTER) + SQLite local (Render) |
| Dashboard | R/Shiny (`app_public.R`, `app_master.R`, `app.R`, `run.R`) |
| ETL R | `R/gdrive_public.R` — download Drive → SQLite → objetos em memória |
| Deploy | Render.com, Docker (`Dockerfile`, `render.yaml`) |
| Armazenamento | Google Drive (estrutura de pastas por mês) |

---

## Arquivos Principais

```
PrestacaoDeContas-BSBStay/
├── app.R                  — roteador de autenticação (público vs master)
├── app_public.R           — painel do proprietário (cliente final)
├── app_master.R           — painel admin (equipe BSBStay)
├── run.R                  — entrypoint Docker/Render
├── R/
│   └── gdrive_public.R    — ETL R: download xlsx Drive → SQLite → objetos por proprietário
├── etl/
│   └── bsbstay_v5_3_gs.js — Apps Script ETL (versão mais recente)
├── Dockerfile
└── render.yaml
```

---

## Arquitetura do Google Drive

```
BSBSTAY - Dados 1/
├── 2025/
│   └── 2025-12/
│       └── 01_Fontes/
│           ├── Manutencao/
│           ├── Pagamentos/
│           ├── Proprietários/   ← com acento (só Dez/2025)
│           ├── Reposicao/
│           └── Reservas/
├── 2026/
│   ├── 2026-01/ ... 2026-04/   ← mesma estrutura, "Proprietarios" sem acento
│   └── 2026-05/
│       └── 01_Fontes/
│           ├── Manutencao/
│           ├── Pagamentos/
│           ├── Proprietarios/
│           ├── Reposicao/
│           ├── Reservas/
│           └── Devolução da Taxa de Limpeza/   ← nova, só a partir de Mai/2026
└── DB_MASTER/
    └── [DB] BSBStay_VF.xlsx    ← banco principal lido pelo dashboard
```

**Problema conhecido no Drive:**
- Dez/2025, Jan/2026 e Fev/2026 têm arquivos duplicados com sufixo `(1)` em todas as pastas
- ETL corrigido para priorizar arquivo sem sufixo numérico (`openMostRecentGoogleSheet_`)
- Ação manual pendente: deletar arquivos `(1)` do Drive

---

## Estrutura do DB_MASTER (Google Sheets)

| Aba | Descrição |
|---|---|
| `fact_reservas` | Uma linha por reserva por proprietário (ver nota copropriedade) |
| `fact_manutencao` | Ordens de serviço por competência |
| `fact_reposicao` | Itens de reposição por competência |
| `fact_despesas` | Despesas fixas (contas do apartamento) |
| `fact_repasse` | Repasses financeiros por proprietário |
| `fact_devolucao_limpeza` | Devoluções de taxa de limpeza (desde Mai/2026) |
| `agg_prestacao_contas` | Agregado mensal por proprietário — fonte principal do dashboard |
| `dim_imovel` | Cadastro de imóveis com `property_id`, `owner_id`, comissão |
| `dim_proprietario` | Cadastro de proprietários com CPF/CNPJ |
| `map_alias_imovel` | Mapeamento de aliases de nomes de apartamentos |
| `log_ingestao` | Log de execuções do ETL |
| `PENDENTES_map_alias` | Apartamentos não encontrados no dim_imovel |

---

## Fluxo de Dados

```
Planilhas fonte no Drive (Reservas, Manutenção, etc.)
        ↓ Apps Script ETL (bsbstay_v5_3_gs.js)
        ↓ runMonthlyUpdateFixedRoot(competencia)
DB_MASTER no Google Sheets (fact_*, agg_prestacao_contas)
        ↓ gdrive_public.R (download → SQLite → objetos R)
        ↓ carregar_dados_app()
Dashboard Shiny (app_public.R / app_master.R)
        ↓ Ajustes gerenciais (Revisão Gerencial)
        ↓ .ger_save_sqlite() → SQLite local Render
Painel do proprietário (dados Drive + ajustes SQLite)
```

---

## Regras de Negócio Críticas

### Copropriedade (6 apartamentos com 2 donos)

Cada coproprietário vê **100% dos dados do imóvel** — não há rateio.

| Apartamento | Dono 1 | Dono 2 |
|---|---|---|
| Apt 606 E - SQS 103 | Gustavo | Octávio dos Anjos |
| Athos 810 | Raíza | Rocha Investimentos |
| Lets 27 | Rocha Investimentos | VR Consultoria |
| Lets 27 | Rocha Investimentos | VR Consultoria |
| Lets 31 | Rocha Investimentos | VR Consultoria |
| Lets 33 | Rocha Investimentos | VR Consultoria |
| Nobile 701 | Rocha Investimentos | VR Consultoria |

**Problema em aberto (PRIORITÁRIO):** a `fact_reservas` atualmente replica 2 linhas por reserva
(uma por `property_id`), causando duplicatas visíveis no calendário e KPIs do dashboard.
A solução correta (Arquitetura A) é:
- `fact_reservas` deve ter **1 linha por reserva real** (sem replicação por dono)
- A replicação para cada dono deve acontecer **apenas no `agg_prestacao_contas`**
- O `ingestReservas_` deve ser revertido para não usar `resolveAll_` — apenas `resolve_` (primeiro pid)
- O `rebuildAggPrestacaoContas` já recebe os dados corretos do `agg` via `dim_imovel`

### Reprocessamento de competência

Cada execução do ETL para uma competência **reescreve completamente** os dados daquele mês
em todas as tabelas FACT, preservando meses anteriores intactos.
Implementado via `removeRowsByCompetencia_` chamada antes do `loadExistingKeys_`.

### Revisão Gerencial (app_master.R)

Ajustes gerenciais (taxa administrativa, manutenção, receita) são salvos no **SQLite local do Render**,
não na planilha base. A planilha base nunca é modificada pelo dashboard.
Se o ETL reprocessar, os ajustes precisam ser republicados pelo admin.

### LGPD — Campo de hóspede

O nome do hóspede **não é capturado** pelo ETL. Em ~4% dos registros o campo contém
CPF/RG em texto livre sem padrão sanitizável. Apenas campos numéricos são persistidos:
`adultos`, `criancas`, `valor_total_reserva`, `taxa_limpeza`, `comissao_canal`.

### Cache do dashboard

`MAX_CACHE_AGE_H = 2` horas (variável de ambiente `MAX_CACHE_AGE_H`).
O botão "Atualizar dados" força `forcar_dl = TRUE` ignorando o cache.

---

## ETL — Funções Principais (bsbstay_v5_3_gs.js)

| Função | Descrição |
|---|---|
| `runMonthlyUpdateFixedRoot()` | Função principal — processa uma competência |
| `removeRowsByCompetencia_(sheet, comp)` | Remove todas as linhas de uma competência em uma aba FACT |
| `buildAliasMap_()` | Constrói mapa multi-valor alias → lista de property_ids |
| `buildCanonMap_()` | Constrói mapa multi-valor nome canônico → lista de property_ids |
| `resolveAll_(aliasMap, canonMap, source, apto)` | Retorna todos os property_ids de um apartamento |
| `resolve_(...)` | Compatibilidade — retorna apenas o primeiro property_id |
| `ingestReservas_()` | Ingesta planilha de Reservas → fact_reservas |
| `ingestManutencao_()` | Ingesta planilha de Manutenção → fact_manutencao |
| `ingestReposicao_()` | Ingesta planilha de Reposição → fact_reposicao |
| `ingestRepasse_()` | Ingesta planilha de Repasse → fact_repasse |
| `ingestDespesas_()` | Ingesta planilha de Pagamentos → fact_despesas |
| `ingestDevolucao_()` | Ingesta planilha de Devolução de Taxa de Limpeza → fact_devolucao_limpeza |
| `rebuildAggPrestacaoContas(competencia)` | Reconstrói agg_prestacao_contas apenas para a competência informada |
| `openMostRecentGoogleSheet_(folder)` | Abre o xlsx mais recente da pasta, priorizando arquivos sem sufixo numérico |
| `parseCompetencia_(v)` | Normaliza competência para formato YYYY-MM |

---

## gdrive_public.R — Estrutura Principal

| Função/Objeto | Descrição |
|---|---|
| `carregar_dados_app(folder_id, forcar_dl, forcar_etl)` | Função principal — orquestra download e ETL R |
| `baixar_db_master_publico(file_id, forcar)` | Baixa o xlsx do Drive para SQLite local |
| `pid_map` | data.frame: property_id → cpf_cnpj + imovel_nome |
| `reservas_clean` | Reservas normalizadas com left_join(pid_map) |
| `manutencao_clean` | Manutenção normalizada |
| `reposicao_clean` | Reposição normalizada |
| `despesas_clean` | Despesas normalizadas |
| `calendario` | Expansão dia-a-dia das reservas para o calendário de ocupação |

---

## Design System (app_public.R / app_master.R)

**Tipografia:** Inter (Google Fonts)  
**Cores principais:**
- Azul primário: `#1a6ef7` / `#0052cc`
- Verde resultado positivo: `#00b388`
- Vermelho negativo: `#e03e3e`
- Laranja manutenção: `#d97706`
- Roxo reposição: `#7c3aed`

**Componentes CSS customizados:**
- `.kgrid` — grid de KPIs principais (7 colunas iguais `repeat(7,1fr)`)
- `.kcard` / `.kcard.hero` — card de KPI (hero = card de destaque escuro)
- `.kgrid-sm` / `.kgrid-sm-2/3/4` — grid de KPIs secundários
- `.kcard-sm` — card de KPI secundário
- `.op-tab` — botões de aba do Operacional (gerenciados por JS via `setOpTab()`)
- `.ger-*` — componentes da Revisão Gerencial

**JS crítico:**
- `setOpTab(aba)` — troca aba do Operacional sem re-renderizar `output$body`
- `_savedScrollY` — preserva posição do scroll após `renderUI`

---

## Problemas Conhecidos e Pendências

### Crítico
- [ ] **Duplicatas de reservas em aptos multi-dono** — implementar Arquitetura A:
  reverter `ingestReservas_` para não replicar por dono; manter replicação só no `agg`
- [ ] **Comissão alterada não reflete** — investigar se é cache ou problema no ETL
  (Green Park 101H alterada para 15% mas continua mostrando 20%)

### ETL
- [ ] Investigar e ingerir pasta `Devolução da Taxa de Limpeza` (implementado no v5_3,
  aguarda validação com planilha real)
- [ ] 622 linhas sem `property_id` na `fact_manutencao` — aliases faltantes no `map_alias_imovel`
- [ ] Deletar arquivos `(1)` duplicados no Drive (ação manual)
- [ ] Março/2026 ausente no banco — verificar se houve dados nesse mês

### Dashboard
- [ ] `output$portfolio_imoveis` referenciado no app_master mas implementação pendente de verificação
- [ ] Confirmar que `devolucao_limpeza` está sendo lida corretamente pelo `gdrive_public.R`
  e exibida no painel do proprietário

---

## Histórico de Versões do ETL

| Versão | Principais mudanças |
|---|---|
| v3 | Versão base — append-only, sem multi-dono |
| v5.1 | Campos numéricos de reserva (adultos, crianças, etc.), remove hóspede (LGPD) |
| v5.2 | `resolveAll_` multi-dono, `removeRowsByCompetencia_` para FACT_MAN/REP/DES/REPASSE |
| v5.3 | `removeRowsByCompetencia_` em FACT_RES, `rebuildAgg` cirúrgico, chave dedup simplificada, `ingestDevolucao_`, aliases Drive corrigidos, `openMostRecentGoogleSheet_` sem sufixo `(1)` |

---

## Como Iniciar uma Sessão no Claude Code

```
Leia o CONTEXT.md e todos os arquivos do projeto.

Contexto: estamos desenvolvendo o dashboard BSBStay (R/Shiny + Google Apps Script).
O problema prioritário desta sessão é: [DESCREVER AQUI].

Arquivos mais relevantes para começar:
- app_public.R (painel do proprietário)
- app_master.R (painel admin)
- R/gdrive_public.R (ETL R)
- etl/bsbstay_v5_3_gs.js (ETL Apps Script)
```
