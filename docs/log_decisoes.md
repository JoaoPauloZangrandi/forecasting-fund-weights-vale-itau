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

**Próximo passo (a combinar):** adicionar as **características** (lado direito da
equação) — do fundo (AUM, aplicação−resgate, FIC/FI, nº de cotistas) e da ação
(preço e beta da VALE no último dia do mês anterior). ⚠️ Preço/beta **não estão**
em CONS/SH → decidir com o orientando como obter (possível exceção à regra
"só CONS e SH").
