# Log de decisões e tratamento de dados

Registro vivo de **o que decidimos, por quê, e as regras permanentes** do projeto.
Objetivo: nunca perder o fio da meada (mesmo trocando de ferramenta/sessão) e
garantir que os tratamentos de base sejam sempre aplicados do mesmo jeito.

> Regra de trabalho: ir **um passo por vez**, explicar antes de fazer e esperar o
> "ok". Nada de empilhar vários passos. Questionar cada decisão metodológica.

---

## Contexto do projeto

- **Objetivo:** prever o **peso** de VALE3 dentro dos fundos da gestora **Itaú**.
- **Orientador:** Prof. Maurício Ferraresi Jr. (FGV EESP).
- **Plano (visão geral):** (1) painel fundo×mês do peso de VALE3; (2) cross-section
  mês a mês do peso contra características de fundo e de ação → o **beta OLS** vira
  um **fator latente**; (3) tornar o fator dinâmico no tempo (θ como random walk,
  filtro de Kalman → modelo de fator dinâmico); previsão multi-horizonte; analisar
  a **matriz de erros** (fundos × características).
- **Bases:** SOMENTE **CONS** (composição consolidada, mensal) e **SH** (série
  histórica, diária). Nada de PTAX/B3/CDA neste projeto.
- **Linguagem:** R (R 4.5.1).

---

## Decisões travadas (escopo)

| Tema | Decisão |
|------|---------|
| Alvo | **Peso** (`Participação_Ativo`), por ser mais simples e *bounded* em [0,1] |
| Ativo | Exatamente `Nome_Ativo == "VALE ON N1 - VALE3"` (posição **direta**) |
| Gestora | 3 nomes na SH: **Itaú Asset Management, Itaú DTVM, Itaú Unibanco** |
| Fundos exclusivos | **Não** remover por enquanto (manter simples) |
| Merge SH×CONS | SH é diária, CONS mensal → casar pelo **dia EXATO** (`SH.DATA == CONS.Data_Competência`) |
| Período (v1) | Apenas **2016** primeiro (rápido, menos custo); generalizar depois |
| Entrega no GitHub | Scripts segmentados (`R/01`, `R/02`, …) **e** `R_full.R` (tudo num arquivo) |
| Fluxo líquido | **Informe Diário CVM** (`CAPTC_DIA − RESG_DIA`), validado com a SH (passo 3b). Quebra a regra "só CONS+SH", mas é a base-mãe da SH e mais confiável |
| Preço/beta da ação | Fonte **externa extremamente confiável** (autorizado); ainda não feito |

---

## Fatos verificados na base (2016)

- **Formatos de data DIFERENTES entre as bases:** SH usa `DD/MM/YYYY` e CONS usa
  `YYYY-MM-DD`. O parser tenta múltiplos formatos → o merge dia-exato casou
  **100%** (0% de fund-months sem match).
- **`Participação_Ativo` é FRAÇÃO**, não porcentagem: vai de 0 a ~0,151 (máx. 15,1%
  da carteira em VALE3 em 2016). Não multiplicar por 100 sem necessidade.
- Valores podem vir em **notação científica** (ex.: `7.18...E-4`); `as.numeric`
  lida bem após remover separador de milhar.
- Gestoras Itaú em 2016: Itaú DTVM (maioria), Itaú Unibanco, Itaú Asset Management;
  **535 fundos** distintos; **388** com VALE3 em algum mês.
- **`SH.APLICAÇÃO` = captação bruta diária** (formato `"R$ x.xxx,xx"`, sempre ≥0);
  a SH **não tem resgate**. Verificado que `SH.APLICAÇÃO == CAPTC_DIA` do Informe
  Diário (fundo 779, jan/2016: R$46.597,54 nos dois). Por isso só a SH **engana**:
  esse fundo teve captação +46k mas resgate 893k → fluxo líquido **−847k**.
- **Informe Diário CVM** (`inf_diario_fi`) é **muito mais limpo** que a SH: sep `;`,
  decimal `.`, sem `R$`; `fread` dá **0 NAs**. Colunas:
  `CNPJ_FUNDO;DT_COMPTC;VL_TOTAL;VL_QUOTA;VL_PATRIM_LIQ;CAPTC_DIA;RESG_DIA;NR_COTST`.
  2016 = `HIST/inf_diario_fi_2016.zip` (12 CSVs mensais). Join por **CNPJ + data**.
  ⚠️ PL do Informe Diário em **reais cheios**; PL da SH em **mil** (fator 1000).

---

## Tratamentos de base (REGRAS PERMANENTES — aplicar sempre)

1. **Normalização de texto de gestora:** remover acentos, maiúsculo, colapsar
   espaços, e casar por núcleos (`ITAU ASSET | ITAU DTVM | ITAU UNIBANCO`). Robusto
   a acentos e sufixos. *(função `normalize_txt`)*

2. **Dedup de linhas EXATAMENTE duplicadas (CONS):** a CVM às vezes repete a mesma
   posição byte-a-byte (ex.: fundo 187623 — MAUSER FIC MM — duas linhas idênticas
   de VALE3 por mês). **Sempre** aplicar `unique()` antes de montar o painel. São
   cópias perfeitas (mesmo peso) → seguro colapsar. Em 2016: 12 duplicatas.

3. **Remover pesos NEGATIVOS:** o peso deve viver em [0,1]. Apareceram resíduos
   contábeis minúsculos (~ −1e-6). **Decisão: remover** essas linhas (não zerar,
   não manter). Em 2016: 8 linhas negativas na CONS (2 em fundos Itaú).
   *Por quê:* não fazem sentido como "peso", são ruído de fechamento, e são
   irrelevantes em volume.

4. **Auditoria de integridade:** após montar o painel, conferir que **não há**
   pares `(cod_fundo, data)` duplicados. Se sobrar duplicado com valores
   *diferentes* (não é cópia exata), **parar e investigar** — não agregar no
   automático.

---

## Histórico (passo a passo)

- **Passo 1** — `R/01_load_bases.R`: carga/sanidade de CONS e SH (amostra), confere
  colunas. Chave de join: `CONS.Código == SH.COD_FUNDO`.
- **Passo 2** — `R/02_build_panel_vale_itau.R`: painel fundo×mês do peso de VALE3
  nos fundos Itaú, **ano 2016**. Aplicou tratamentos 1–4 acima.
  Resultado: **4.062 obs, 388 fundos, 12 meses**, peso ∈ [0; 0,151], 0 duplicados.
  Saída (regenerável, fora do git): `data/processed/painel_vale_itau_2016.csv`.
- **Passo 3** — `R/03_add_fund_flows.R`: característica de **fluxo** (captação,
  resgate, líquido) via Informe Diário CVM, agregada por fundo×mês e juntada ao
  painel por CNPJ+ano+mês. **100% de match** (4.062/4.062), 0 duplicados, 0 NAs.
  Saída: `data/processed/painel_vale_itau_2016_fluxos.csv` (cols novas:
  `captacao`, `resgate`, `fluxo_liq`, `n_dias`).

**Características — situação (lado direito da equação):**
- ✅ **AUM** (`SH.PATRIMONIO_LIQUIDO_(MIL)`), **nº cotistas**
  (`SH.NUMERO_DE_COTISTAS`), **FIC/FI** (de `NOME_FUNDO`/`CLASSIFICACAO_ANBIMA`).
- ✅ **Fluxo** (captação/resgate/líquido) — FEITO no passo 3 (Informe Diário).
- ⬜ **Preço e beta da VALE** — externo, autorizado, ainda não feito.

**Próximos passos (a combinar):** (3b) validar o fluxo via derivação SH (PL+cota)
— exige antes certificar o parsing da `COTA`; (4) adicionar AUM, cotistas, FIC/FI;
(5) preço e beta da VALE de fonte externa; depois generalizar 2017–2021.
