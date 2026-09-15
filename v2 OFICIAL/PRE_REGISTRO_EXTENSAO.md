# Pré-registro — extensão sobre a variância do erro de previsão de peso

**Este documento é commitado ANTES de qualquer regressão ser estimada.** O `git log` é a prova
de anterioridade. Nada abaixo pode ser alterado depois de rodar os scripts 130/131; alteração
posterior é emenda datada, em seção própria no fim.

Amostra: 2016-2021, mesma do TCC. Horizontes h=1 (principal) e h=3 (replicação).
Destino: artigo separado.

---

## 0. O que já foi medido antes deste documento (scripts 125 e 126)

Diagnóstico e decomposição, **sem nenhuma regressão de interesse estimada**:

| Fato | Valor | Fonte |
|---|---|---|
| Controle: R² e ajustado do script 95 reproduzidos | 0,4963 / 0,4047 | `_log_126.txt` |
| Fundos com erro no teste, pós-filtro de poeira | 2.060 | `_log_125.txt` |
| Fundos sem nenhum mês de treino | 608 (29,5%) | `_log_125.txt` |
| Variância de `log(rmse_i)` **dentro** de gestora | **77,4%** | `_log_126.txt` |
| ρ da dispersão do erro por gestora (split ímpar/par) | 0,968 | `_log_126.txt` |
| R²_sinal = R²/ρ | 0,512 | `_log_126.txt` |
| LOO-R² do modelo de 6 regressores (n=40) | **0,089** | `_log_126.txt` |
| LOO-R² do modelo só com concentração | **0,338** | `_log_126.txt` |

Esses números motivam o desenho e **não podem ser usados como confirmação de hipótese**.

## 1. Hipóteses

- **H1.** A dificuldade de prever varia mais **dentro** de gestora do que entre gestoras, e
  características de fundo explicam parte dessa variação intra-casa. *(Já sabemos que 77,4% é
  dentro; o que está em teste é se é explicável.)*
- **H2.** Medidas de **instabilidade** (dispersões, turnover) explicam a dispersão do erro
  acima e além das **médias** das mesmas características.
- **H3.** O mandato (classe Anbima) não acrescenta poder explicativo além da escala da fatia de
  ações. *Hipótese nula declarada de antemão: o desfecho esperado é nulo.*
- **H4.** β_W ≠ β_B (Mundlak): o mecanismo intra-casa difere do mecanismo entre-casas.

## 2. Dependentes

| # | Nome | Definição |
|---|---|---|
| D1 | `l_rmse_peso` | `log(sqrt(mean(erro_oos²)))` por fundo, no teste — métrica peso-do-PL, comparável ao TCC |
| D2 | `l_rmse_sleeve` | idem, com o erro reescalado pela fatia de ações do fundo (`s = peso/Σpeso`) |

**As duas rodam sempre, em paralelo.** Resultado que aparece em D1 e não em D2 é escala de
sleeve, não comportamento.

## 3. Amostras — duas versões, ambas reportadas

O script 125 mostrou que 29,5% dos fundos não têm treino. Em vez de escolher, rodam-se as duas:

| Versão | Definição | n esperado |
|---|---|---|
| **S1 "treino cheio"** | `n_obs≥24`, `n_meses_teste≥6`, `n_meses_treino≥12`; características = médias/sd de todo o treino | ~1.237 |
| **S2 "janela móvel"** | `n_obs≥24`, `n_meses_teste≥6`; características = médias/sd da janela t−12..t−1 relativa à primeira observação de teste do fundo | ~1.804 |

S1 é comparável ao script 95. S2 recupera os fundos jovens ao custo dessa comparabilidade.
**A tabela principal do artigo é S1; S2 é robustez de seleção amostral.**

## 4. Lista FECHADA de regressores

Nenhum regressor fora desta lista entra nas especificações confirmatórias.

### Bloco 0 — base (as 6 do TCC, médias no treino)
1. `beta_fundo` · 2. `l_aum` · 3. `l_cot` · 4. `pct_fic` · 5. `flow_aum` · 6. `hhi_medio`

### Bloco 1 — DISPERSÕES
7. `sd_flow_aum` — **primária do bloco**
8. `sd_log_hhi`
9. `amp_beta` = max−min de `beta_fundo` (não `sd`: janelas rolling de 252 pregões compartilham ~92% dos dados)
10. `sd_d_l_aum` = `sd(diff(l_aum))` — **só robustez**, quase colinear com (7) por identidade

### Bloco 2 — TURNOVER
11. `turn_mediano` — churn dentro da sleeve, deriva passiva descontada
12. `dsleeve_mediano` — entrada/saída líquida ações↔caixa

### Bloco 3 — MANDATO (5 grupos; base = G2)
13. `G1_multimercado` · 14. `G3_indexado` · 15. `G4_estilo` · 16. `G5_nicho` · 17. `G0_sem_classe`

| Grupo | Classes Anbima | n na amostra do plano (script 125) |
|---|---|---|
| G1 Multimercado | Multimercados Livre | 791 |
| G2 Ações amplo (**base**) | Ações Livre | 520 |
| G3 Indexado/benchmark | Índice Ativo, Indexados, ETF | 208 |
| G4 Estilo fundamental | Valor/Crescimento, Dividendos, Sustentabilidade/Governança | 119 |
| G5 Especialista/nicho | Small Caps, Setoriais | 56 |
| G0 Sem classe no treino | — | 110 |

Agrupamento fixado **antes** de olhar qualquer dependente. Nenhum grupo tem n<30, então não há
fusão prevista. Se um grupo cair abaixo de 30 numa subamostra, funde-se com o vizinho conceitual
e **declara-se que a fusão foi por n, não por resultado**.

### Bloco 4 — BREADTH
18. `l_n_ativos` = `log(n_ativos_filtrado_mes)`
19. `forma_hhi` = resíduo de `−log(hhi)` regredido em `l_n_ativos` (concentração **dado** o número de posições)
20. `razao_poeira` = `n_ativos_filtrado / n_ativos_bruto`

### Controles, sempre presentes, nunca interpretados
C1 `soma_peso_medio` · C2 `n_meses_treino` · C3 `fonte_b3_pesada` (fração do peso em tickers sem
ajuste por provento) · C4 `l_n_obs_teste`

## 5. Especificações

- **A1 pooled**: `D ~ Bloco0 + Blocos1-4 + C`
- **A2 Mundlak (PRINCIPAL)**: regressores em desvio da média da gestora `(x_i − x̄_g)` **e** média
  da gestora `x̄_g`, separados. Teste de Wald conjunto β_W = β_B.
- **A3 within-gestora**: `feols(... | gestora_grupo)`. R²_within é o número limpo.

A linha do abstract vem de **A2, amostra S1, dependente D1**, com D2 ao lado.

## 6. Inferência

1. Cluster por `gestora_grupo` (CV1) — reportado, rotulado **referência otimista**.
2. **Wild cluster bootstrap-t com nulo imposto, 9.999 réplicas, pesos de Rademacher** — primária.
3. **Webb weights (6 pontos) + inferência por randomização** (10.000 permutações do rótulo entre as
   40 gestoras) para todo regressor aproximadamente constante dentro do cluster — isto é, os `x̄_g`
   de A2 e as dummies de mandato. Rademacher é sabidamente frágil nesse caso.
4. HC3 não clusterizado — só para mostrar o custo do clustering.

Declarar em toda tabela: para regressores entre-gestora a informação efetiva é **40**, não n.

## 7. Correção de múltiplos testes

- **Primária: 4 testes F de bloco** {dispersões, turnover, mandato, breadth}, com Holm.
- Coeficientes individuais: Holm dentro do bloco e Benjamini-Hochberg na família toda. Reportar
  p bruto, p-Holm e q-BH lado a lado.
- Romano-Wolf stepdown sobre as réplicas do wild bootstrap.

## 8. Hold-out

Split **por fundo**, 50/50, **estratificado por gestora**, semente `20260915`.

- **Desenvolvimento**: exploração livre. Tudo vai para o ledger marcado `pre_registrado=N` se sair
  da lista da Seção 4.
- **Confirmação**: roda **uma única vez**, especificação congelada. É o resultado do artigo.

Justificativa metodológica para o artigo: a regressão com n=40 do TCC **não admite** este
protocolo; a de fundo admite. É razão independente para descer de nível.

## 9. Regras de decisão fixadas de antemão

- **Mandato (H3)**: tabela 2×2 {D1, D2} × {com C1, sem C1}. Sobrevive nas quatro células → mandato.
  Só na célula {D1, sem C1} → aritmética de sleeve. Qualquer outro padrão → inconclusivo, reportado
  como tal.
- **Breadth**: se VIF(`l_n_ativos`, `hhi_medio`) > 10, usar a parametrização `l_n_ativos` +
  `forma_hhi` e **não** interpretar `hhi_medio` isolado.
- **Dispersões**: se `sd_flow_aum` e `sd_d_l_aum` tiverem |r| > 0,8, mantém-se apenas (7); (10) vai
  para robustez. Decisão já tomada aqui, não no momento do resultado.
- **Turnover**: se `cor(turn_mediano, hhi_medio)` > 0,8, o Bloco 2 é declarado redundante com a
  concentração e reportado como tal, sem tentativa de salvamento.

## 10. Critérios de parada

- Se S1 cair abaixo de 600 fundos, abandona-se S1 e S2 vira principal.
- Se o R²_within de A3 for < 0,02 nas duas dependentes, declara-se que a heterogeneidade
  intra-casa é ruído e o artigo passa a ser sobre a decomposição (Blocos C) apenas.
- Se o teste F de um bloco não rejeitar, o bloco é reportado como **nulo declarado**, com o
  intervalo de confiança, e não é reespecificado.

## 11. Ledger

`data/ledger_trials_extensao.csv`, append-only, gravado pelo próprio script. Uma linha por
especificação estimada, incluindo as abandonadas. O artigo reporta o número total.

---

## Emendas

*(nenhuma até aqui)*
