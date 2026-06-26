# Log de decisões e tratamento de dados

Registro vivo de **o que decidimos, por quê, e as regras permanentes** do projeto.
Objetivo: nunca perder o fio da meada (mesmo trocando de ferramenta/sessão) e
garantir que os tratamentos de base sejam sempre aplicados do mesmo jeito.

> **Handoff:** o arquivo `prompt.md` (raiz do repo) é o **briefing completo** do
> projeto para retomar em qualquer ferramenta (Claude/Codex). Manter `prompt.md`
> **e** este log atualizados a cada passo. As **anotações cruas do Maurício** e a
> evolução das **ideias do orientando** estão em `docs/anotacoes_orientador_e_ideias.md`.

> Regra de trabalho: ir **um passo por vez**, explicar antes de fazer e esperar o
> "ok". Nada de empilhar vários passos. Questionar cada decisão metodológica.

> ⚠️ **Como rodar (gotcha de diretório):** os scripts usam caminhos relativos
> (`data/raw`, `data/processed`), então o working directory precisa ser a raiz do
> repo. Os scripts já fazem `setwd(PROJ_DIR)` sozinhos (default
> `C:/Users/joaoz/forecasting-fund-weights-vale-itau`, override por env `PROJ_DIR`).
> Se rodar manualmente no RStudio fora do projeto, faça antes
> `setwd("C:/Users/joaoz/forecasting-fund-weights-vale-itau")`. O `R_full.R` também
> baixa o Informe Diário sozinho se o zip não estiver em `data/raw`.

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
| Preço/beta da ação | Fonte **Yahoo Finance**, validada contra B3 oficial; feito no passo 5 |
| Fundos com poucos cotistas | Critério simples inicial: remover observações com `n_cotistas <= 3`; manter o painel completo intacto |

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

5. **Validação cruzada SH × Informe Diário (parsing certificado):** a `APLICAÇÃO`
   da SH deve igualar a `CAPTC_DIA` somada do Informe Diário (mesmo CNPJ×mês).
   Verificado em 2016: **99,66%** dos fund-months Itaú batem **ao centavo**
   (0 NAs) → parsing das duas bases está correto. Os ~0,3% que divergem **não são
   erro de parsing nem de agregação** — são diferença de DADO entre as duas séries
   da CVM (detalhe forense no Apêndice A). **Decisão (do orientando):** usar o
   **Informe Diário** como fonte do fluxo (única com resgate); **manter** as
   colunas `captacao_sh` (valor da SH) e `div_captacao` (flag 0/1) no painel — não
   apagar o que a SH traz; **NÃO** rodar teste de robustez com/sem flagados na
   regressão — apenas deixar a discrepância **registrada** para eventual tratamento
   futuro. Em 2016: **17** fund-months divergentes na tabela de fluxo, dos quais
   **16** chegam ao painel final (1 é fund-month sem VALE3 na CONS).

6. **Verificação da agregação de fluxo (recomputo independente):** o fluxo mensal
   é `Σ(CAPTC_DIA − RESG_DIA)` sobre o **mês-calendário da competência**; junta ao
   painel por `(cnpj, ano, mes)`. Provado (2016): identidade
   `fluxo_liq == captacao − resgate` ao epsilon (1e-7); **0 NAs**; competência
   sempre no mês `(ano,mes)` (0 desalinhamentos) e é o último dia útil do mês;
   recomputo por **janela de data explícita** (1º→último dia) bate **exatamente**
   (dif 0) numa amostra de 56 fund-months incl. todos os flagados. Aplicar o mesmo
   teste ao estender para 2017–2021.

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
  `captacao`, `resgate`, `fluxo_liq`, `n_dias`, `captacao_sh`, `div_captacao`).
  Validação cruzada com a SH feita (99,66% ao centavo; 16 fund-months marcados no
  painel). Forense das discrepâncias no **Apêndice A**.

- **Passo 4** — `R/04_add_fund_features.R`: características de fundo da SH, snapshot
  no dia da competência (merge dia-exato por `COD_FUNDO + data`, **0 sem match,
  0 dup, 0 NAs**): `aum` (PL em **R$ cheios** = `PATRIMONIO_LIQUIDO_(MIL)`×1000),
  `n_cotistas`, `is_fic`, `classif_anbima`. Saída:
  `data/processed/painel_vale_itau_2016_features.csv`. FIC: 2.486 / FI: 1.576
  fund-months. AUM de R$680 a R$2,6 bi.
  - **FIC/FI:** a `CLASSIFICACAO_ANBIMA` é de **estratégia** (Multimercados Livre,
    Ações Livre…) e **NÃO** distingue FIC/FI → FIC vem do **NOME** por **token**
    `\bFIC\b`/`\bFICFI\b` (robusto a substring, p/ não pegar "FIC" embutido em
    outra palavra/instituição). Validado em 2016: substring "FIC" (232) = token
    (232), **0 falso positivo**; token sempre seguido de MULTIMERCADO/AÇÕES/RENDA.
  - **`classif_anbima`** guardada como característica categórica extra (estratégia).
  - ⚠️ **Observação p/ depois:** mediana de `n_cotistas` = **2** → há muitos
    **fundos exclusivos** (1–2 cotistas). O orientador quer removê-los em algum
    momento; mantidos por ora (decisão "manter simples"). Candidato a passo futuro.

- **Passo 5** — `R/05_add_stock_features.R`: características da AÇÃO (VALE3),
  fonte **Yahoo Finance** (`VALE3.SA`, `^BVSP`). Colunas: `preco_nominal` (close),
  `preco_ajust` (adjclose), `beta_vale` (cov/var móvel **252 pregões** vs Ibovespa,
  retorno **simples**; VALE pelo adjclose, Ibov pelo close), `data_ref` (a data de
  medição). Tudo medido no **último pregão do mês anterior** à competência (sem
  look-ahead). Saída: **painel FINAL** `data/processed/painel_vale_itau_2016_full.csv`.
  - Dados Yahoo validados: VALE3 e Ibov, 764 pregões 2014–2017, **0 NAs**, datas
    100% alinhadas. Beta verificado por 3 métodos (frollmean = cov/var = lm, dif 0).
  - Resultado coerente: VALE3 nominal R$13,03 (dez/2015) → R$28,06 (nov/2016);
    beta 1,5–1,77 (ação cíclica, alto beta). 0 NAs no painel.
  - **VERIFICAÇÃO FORENSE (parsing/datas):** (a) timestamps crus do Yahoo decodificam
    às 12:00/13:00 UTC (abertura B3 c/ horário de verão) → data atribuída via
    `America/Sao_Paulo` correta: **0 datas em fim de semana**, estritamente
    crescentes, 0 duplicadas, SEM off-by-one; (b) `data_ref` = último pregão do mês
    anterior recalculado independentemente → **igual nos 12 meses**, e o preço casa
    com o `close` da série naquele dia; (c) **CROSS-CHECK CONTRA A B3 OFICIAL**
    (COTAHIST_A2015/2016, PREULT): o `preco_nominal` (Yahoo) = fechamento oficial
    da B3 **EXATAMENTE nos 12 `data_ref`** (máx |dif| = **R$0,00**), 0 datas sem
    match. → preço/datas certificados contra a fonte primária.
  - **LOOK-AHEAD (análise):** `P2` (fim do download) NÃO é look-ahead — o beta usa
    janela móvel "para trás" (252 retornos terminando na data); o buffer nunca
    entra. Provado: beta baixando até 2017 == baixando até 30/11/2016 (dif < 5e-7);
    `preco_nominal` (close) **idêntico** (dif 0). ⚠️ ÚNICA sutileza: o `preco_ajust`
    (adjclose) é retroajustado por proventos FUTUROS → seu *nível* numa data passada
    depende de dividendos posteriores (efeito empírico aqui desprezível). **DECISÃO:
    usar `preco_nominal` como a característica de preço na regressão** (look-ahead
    zero); `preco_ajust` fica só para os retornos/beta (que são limpos).
  - ⚠️ **PENDÊNCIA:** `P2 = 2017-02-01` é margem supérflua p/ 2016 (último uso =
    30/11/2016). Tornar `P2` **dinâmico** (último `data_ref` do painel + pequena
    folga) p/ não conter pregões fora da amostra. Tarefa para a próxima sessão.
  - Yahoo JSON: sep próprio; `query1.finance.yahoo.com/v8/finance/chart/<sym>`;
    precisa de User-Agent no curl; parse com `jsonlite`. Cache em `data/raw/`
    (gitignored); `R/05` e `R_full` baixam sozinhos se faltar.

- **Passo 6** — `R/06_remove_exclusive_funds.R`: remoção simples de fundos com
  poucos cotistas. Decisão do João: por enquanto **não** criar várias variantes;
  usar apenas o critério `n_cotistas > 3`. O painel completo
  `data/processed/painel_vale_itau_2016_full.csv` permanece intacto. Saída:
  `data/processed/painel_vale_itau_2016_filtrado_cotistas_gt3.csv`.
  - Diagnóstico antes do filtro: 4.062 obs fundo-mês, 388 fundos; mediana de
    `n_cotistas` = 2. O corte remove 2.453 obs (60,4%) e deixa 1.609 obs.
    Restam 155 fundos com pelo menos uma observação após o filtro.
  - Interpretação metodológica: o filtro é por **observação fundo-mês**, não por
    exclusão permanente do fundo, porque `n_cotistas` é característica mensal e a
    regressão planejada é cross-section mês a mês. Critérios alternativos
    (`<= 2`, `<= 5`, remover fundo inteiro etc.) ficam para depois, se necessário.

**Características — situação (lado direito da equação): TODAS FEITAS (2016).**
- ✅ **Fundo:** AUM, nº cotistas, FIC/FI (passo 4) + fluxo líquido (passo 3).
- ✅ **Ação:** preço (nominal+ajustado) e beta (passo 5).
- ✅ **Amostra principal simples:** painel filtrado com `n_cotistas > 3` (passo 6).

- **Passo 7** — `R/07_cross_section_regression.R` (PASSO 1 da metodologia do
  Maurício): regressão do peso de VALE3 nas características, amostra filtrada
  (`n_cotistas > 3`). Tratamentos: `log(aum)`, `log(n_cotistas)`, `is_fic` dummy,
  `fluxo_liq/aum`. **Duas versões:**
  - **(A) Cross-section mês a mês** → coeficientes `θ_t` por mês (= o **fator
    latente**) + **matriz de resíduos** fundo×mês. Preço/beta NÃO entram (constantes
    no mês → vão para o Passo 2/dinâmica). Saídas: `reg07_cross_section_coefs_2016.csv`,
    `reg07_cross_section_resid_2016.csv`. **Resultado:** sinais **estáveis nos 12
    meses** — `log(aum)` **−** (fundo maior, menos VALE), `log(cotistas)` **+**,
    `is_fic` **−** (FIC tem menos VALE direto); `fluxo/aum` fraco/instável. R² 0,16–0,27.
    Resíduo médio 0/mês (OLS), sd 0,0118. A estabilidade de θ_t justifica o random
    walk/Kalman do Passo 2.
  - **(B) Pooled** (com preço e beta): `l_aum`, `l_cot`, `is_fic` muito significativos
    (p<2e-16, mesmos sinais); `fluxo/aum` e `preço` não signif.; `beta` +marginal
    (p=0,027). ⚠️ preço/beta têm só **12 valores distintos** → p-valor otimista
    (pouca variação independente); pertencem ao Passo 2.
  - **Insight:** o NÍVEL do peso é estrutural (tamanho/cotistas/FIC); o **fluxo não
    explica o nível** → fluxo provavelmente explica a **variação** do peso (o "prever
    amanhã"). Decisão de tratamento (do João): fluxo = `fluxo_liq/aum`.

**Próximos passos (a combinar):** **(Passo 2) dinâmica de θ_t** (random walk /
Kalman; ligar α_t/θ_t a preço e beta) + previsão multi-horizonte; estudar a **matriz
de erros** (estacionariedade etc.); variantes da regressão (cortes de cotistas,
peso em logit, fluxo defasado p/ explicar Δpeso); generalizar **2017–2021**
(reaplicar regras 5 e 6). Opcional: (3b) validar fluxo via SH PL+cota; P2 dinâmico.

---

## Apêndice A — Forense das discrepâncias SH × Informe Diário (2016)

Investigação completa dos fund-months onde `SH.APLICAÇÃO` (somada no mês) difere de
`CAPTC_DIA` (somada no mês) do Informe Diário, para o mesmo `CNPJ × mês`.

### A.0 — Contexto e conclusão geral
- Cobertura do cruzamento (todos os fundos Itaú, 2016): SH = 5.819 fund-months,
  Informe Diário = 5.824; em ambos = 5.806. (13 só na SH, 18 só no Informe — fundos
  presentes numa base e não na outra.)
- Dos 5.806 em comum: **5.786 batem ao centavo (99,66%)**; **20 divergem**.
- No nosso painel-alvo (VALE3 × Itaú): **16** fund-months com `div_captacao = 1`
  (de 4.062). O 17º divergente da tabela de fluxo não tem VALE3 na CONS, então não
  entra no painel.
- **Parsing e agregação estão certos** (regras 5 e 6). As divergências são
  diferença de DADO entre duas séries da CVM: a SH é série histórica **revisada**;
  o Informe Diário é o **bruto "as-reported"**.

### A.1 — A coluna `div_captacao`
- Tipo `integer`, binária (0/1), **máximo = 1**. NÃO é magnitude.
- No painel: 4.046 zeros, **16 uns**.
- Magnitude da divergência nos 16 (`captacao` do Informe − `captacao_sh` da SH):
  |dif| mediana ≈ **R$ 1,6 mi**, máximo **R$ 13,57 mi**. Todos os 16 têm VALE3
  (peso de 0,018% a 4,16%), ou seja, são observações reais do alvo.

### A.2 — Padrão A: spike isolado de 1 dia
- Exemplo: fundo **61395 = ITAÚ DIVIDENDOS FI AÇÕES** (CNPJ 02.887.290/0001-62),
  jan/2016. Dos 20 dias úteis, **19 são idênticos ao centavo**; toda a diferença
  está em **2016-01-15**: SH = R$ 108.275 vs Informe = R$ 13.677.136.
- Interpretação: uma aplicação grande registrada no bruto e depois corrigida/
  revisada na SH (evento pontual).

### A.3 — Padrão B: razão constante (estrutural, FIC)
- Caso: fundo **318231 = ITAÚ MOMENTO 30 FIC AÇÕES** (CNPJ 16.718.302/0001-30),
  aparece em **8 dos 16** flagados (jan–ago/2016).
- Drill diário do ano todo: 251 dias na SH e 251 no Informe; **0 dias com linha
  duplicada** no Informe (não é bug de duplicação). **123 dos 251 dias divergem.**
- Nos 123 dias divergentes, a razão `SH.APLICAÇÃO / CAPTC_DIA` é **exatamente
  0,7631** em TODOS (desvio-padrão 2,4e-06). Nos 128 dias que batem, 25 são
  zero-zero e os demais são iguais.
- Interpretação: diferença **estrutural** entre as séries para esse FIC (a SH
  guarda ~76,31% do que o Informe reporta como captação, em parte dos dias). A
  causa-raiz (por que 0,7631 e por que só em parte dos dias) **não está documentada
  pela CVM e não foi resolvida** — e não precisa ser, pois a fonte adotada é o
  Informe Diário.

### A.4 — Decisão e o que fica registrado
- **Fonte do fluxo = Informe Diário** (`CAPTC_DIA`, `RESG_DIA`), inclusive nesses 16.
- **Mantemos** `captacao_sh` e `div_captacao` no painel (não apagamos a SH).
- **Não** haverá teste de robustez com/sem flagados na regressão; a discrepância
  fica apenas **registrada** aqui para eventual tratamento futuro.
- Ao estender para 2017–2021: rodar o mesmo cruzamento e drills; esperar mais casos
  dos padrões A e B; manter a mesma política.
