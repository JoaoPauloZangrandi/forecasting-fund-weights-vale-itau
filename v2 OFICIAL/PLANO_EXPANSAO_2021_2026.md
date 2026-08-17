# Plano de expansão da amostra: 2016-2021 → 2016-2026

Este documento existe porque a mudança que vem (dados de posição/preço de
2022 a 2026) é **upstream de tudo**: mais fundamental que qualquer mudança de
característica na Etapa 1 (essas já são cobertas por `PIPELINE.md` +
`00_RUN_ALL_apos_etapa1.R`, que continuam valendo **depois** desta etapa 0).
Aqui o que muda é o próprio range de anos da ingestão de dado bruto — CDA/CONS-SH
da CVM e COTAHIST da B3 — e isso tem efeito cascata em ~60 scripts que hoje têm
`2016`/`2021`/`202001`/`202112` escritos literalmente no código.

Levantamento feito em 15/08/2026 via grep sistemático nas duas árvores de
scripts do repo (`R/` + raiz, e `v2 OFICIAL/scripts/` + `v2 OFICIAL/exploracao_sinais/scripts/`).
Nenhuma linha de código foi alterada ainda — isto é só o plano, pra executar
quando os dados de 2021-2026 estiverem disponíveis.

## Regra geral (reforçada explicitamente pelo João, 15/08/2026): É TUDO, SEM EXCEÇÃO

Expandir o range de anos é a mudança mais upstream possível — mais cedo no
pipeline do que qualquer corte de qualidade ou definição de característica
já feita antes. Pela mesma lógica já registrada em `PIPELINE.md` ("não existe
mudança 'só na Seção 6'"), aqui a regra é ainda mais absoluta: **NENHUM
resultado do TCC e NENHUM candidato da exploração de sinais fica de fora.**
Concretamente, quando os dados novos chegarem:

- **TCC_finalV2.tex inteiro** — não só os números que mudam de valor, mas
  TODAS as seções (Dados, Etapa 1, Etapa 2/3, Seção 5 Resultados, Seção 6
  Aplicação), TODAS as figuras, TODAS as tabelas, recalculadas do zero a
  partir do painel novo. Não existe "essa tabela não deveria mudar" sem
  conferir de verdade — a Tabela 14 (efeito-fixo de gestora, ver histórico
  deste projeto) já ensinou que suposições desse tipo escondem bug real.
- **TODOS os scripts de `exploracao_sinais/` (mais de 90 até 15/08/2026, e
  crescendo), um por um** — não só os que pareceram promissores ou os que
  sobreviveram a robustez. Um resultado nulo com N=24 pode deixar de ser
  nulo com N=72+; um resultado "significativo" com N=24 pode se revelar
  ruído com mais dado. As duas direções importam igualmente, e só dá pra
  saber rodando todos de novo. **Reforçado explicitamente pelo João em
  15/08/2026, segunda vez: "quero rerodar a bateria toda de TUDO que
  testamos, absolutamente tudo" — não é uma leitura minha, é instrução
  literal, releia antes de decidir pular qualquer candidato.**
- **Todo candidato adicional testado nesta sessão** entre agora e a chegada
  dos dados entra nessa mesma obrigação de reteste — a lista "TUDO" cresce
  junto com a exploração, não é uma lista fixa fechada hoje.

## Prazo real: 16/08/2026 até 23:59 (não 17/08 como o edital do desafio Itaú sugeria)

João confirmou em 15/08/2026: os dados 2022-2026 devem chegar amanhã
(16/08), e **o mesmo dia é o prazo final** de uma entrega paralela (Desafio
Quant AI 2026 do Itaú, documentado em `C:\Users\joaoz\Downloads\Itaú
Desafio Quant AI 2026\`, ver memória do projeto). Ou seja, o rerun completo
desta expansão de dados PRECISA estar pronto no mesmo dia em que os dados
chegam — não há folga. Isso muda a prioridade de preparação: qualquer
coisa que reduza tempo de execução no dia (16/08) deve ser feita ANTES,
enquanto os dados ainda não chegaram. Por isso, em 15/08/2026, comecei
dois itens de preparação (ver `config_periodo.R` e o runner mestre de
`exploracao_sinais`, abaixo) — nenhum dos dois precisa do dado novo em mãos
para ser construído, só para ser executado.
- Isso vale mesmo pra scripts marcados como SUPERADO no pipeline atual —
  se algum motivo levar a reativar uma trilha superada no futuro, ela
  também precisa ser rodada com o dado novo antes de qualquer conclusão.

Nenhum atalho de "só recalcular o que parece que vai mudar" — a lição
central de todo o histórico deste projeto (soma_peso, HHI_resto, universo
completo) é que a suposição de "isso não deveria afetar aquilo" já falhou
silenciosamente várias vezes.

## Como usar este documento

Sempre que os dados novos chegarem: seguir as Etapas 0→1→2→3 desta lista, NA
ORDEM, cada uma só depois da anterior estar completa e conferida. A Etapa 1
em diante reaproveita o `PIPELINE.md`/`00_RUN_ALL_apos_etapa1.R` já existentes
— este documento não os substitui, só adiciona o que falta **antes** deles.

---

## Achado estrutural importante (CORRIGIDO em 15/08/2026 após checagem direta): a árvore `R/` NÃO está no caminho crítico atual

Levantamento inicial (mesmo dia) sugeria que a ingestão de FUNDOS ainda
dependia da pasta antiga `R/` (`R/01_load_bases.R` → ... →
`v2 OFICIAL/scripts/13_etapa1_unificada.R`, que lê `painel_predeterminado.csv`).
**Isso é verdade sobre o script 13, mas o script 13 está SUPERADO** — os
motores de Etapa 1 realmente ativos hoje (`96_pipeline_hhi_etapa1.R`,
`99_pipeline_hhi_lag_etapa1.R`, `101_cross_section_universo_completo_hhi.R`)
leem direto de `painel_multiativo_final.csv`/`painel_universo_completo_final.csv`
— confirmado via grep nos 3 arquivos, nenhum toca `painel_predeterminado.csv`.

Rastreando de trás para frente, a cadeia real e completa é:
```
CONS_%d.csv / SH_%d.csv (raw CVM)          COTAHIST_A%d.ZIP (raw B3)
        │                                          │
50_universo_completo_gestora.R            56_precos_cotahist_b3.R
        │                                          │
51_painel_multiativo_universo_completo.R  57_precos_tickers_sucessores.R
        │                                          │
52/53 (features, beta)                             │
        └──────────────┬───────────────────────────┘
                58_merge_final_universo_completo.R
                        │
        100_checagem_soma_peso_universo_completo.R
                        │
              [resync manual] → painel_multiativo_final.csv
                        │
              96 / 99 / 101 (Etapa 1) → resto do grafo do PIPELINE.md
```
**Toda essa cadeia vive dentro de `v2 OFICIAL/scripts/`, autossuficiente.**
A pasta `R/` (e `v2 OFICIAL/scripts/13_etapa1_unificada.R`, que a usa) pode
ser ignorada na expansão de dados — não alimenta o painel final há tempo.
Isso reduz bastante o escopo de arquivos que precisam do range de ano
atualizado: são ~10 arquivos confirmados (50, 51, 52, 53, 56, 57, 58, 60,
100 [não usa range de ano diretamente, só qualidade], 103), não ~20+ como
o levantamento inicial sugeria.

---

## ✅ PREPARAÇÃO JÁ FEITA em 15/08/2026 (antes dos dados novos chegarem)

Como o prazo real ficou apertado (dados chegam 16/08, entrega no mesmo dia
às 23:59 — ver seção de prazo acima), adiantei o que dava pra fazer sem
precisar do dado em mãos:

1. **`v2 OFICIAL/scripts/config_periodo.R` criado** — `ANO_INICIO <- 2016L`,
   `ANO_FIM <- 2021L` (valor atual, preserva o comportamento de hoje).
   **Quando os dados chegarem: editar SÓ o `ANO_FIM` deste arquivo** (ex.
   pra `2026L`) em vez de caçar range hardcoded em vários scripts.
2. **Os 7 scripts confirmados no caminho crítico já fazem `source()` desse
   config e usam `ANO_INICIO:ANO_FIM`** em vez de `2016:2021` hardcoded:
   `50_universo_completo_gestora.R`, `51_painel_multiativo_universo_completo.R`,
   `53_beta_fundo_universo_completo.R`, `56_precos_cotahist_b3.R`,
   `57_precos_tickers_sucessores.R`, `60_retorno_fundo_mensal.R`,
   `103_diagnostico_atricao_universo_completo.R`. Testado: sintaxe válida
   nos 7 (`parse()` sem erro) e `config_periodo.R` roda e preserva
   `ANO_INICIO=2016/ANO_FIM=2021` por padrão — **nenhuma mudança de
   comportamento até alguém editar o `ANO_FIM`**.
   - Os `stopifnot` de tamanho de amostra nos scripts 50/51 foram
     condicionados a `if (ANO_FIM == 2021L)` — não vão mais travar
     sozinhos quando o range mudar; só um aviso no `cat()`.
   - `52_features_universo_completo.R` **não precisou de edição** — já
     deriva o range de meses dinamicamente do painel que lê (script 51),
     se auto-ajusta.
   - **Não confundir com a árvore antiga `R/`** — confirmado que ela NÃO
     está no caminho crítico atual (ver correção acima), não precisa mexer
     nela.
3. **`v2 OFICIAL/exploracao_sinais/RODAR_TUDO.sh` criado** — roda TODOS os
   scripts numerados de `exploracao_sinais/scripts/` (78 no momento, 01 a
   87) em ordem, um comando só, log individual por script em
   `RERUN_2016_2026/`, não trava no primeiro erro (continua e reporta no
   final quais falharam). Testado com 1 script real + 1 quebrado de
   propósito — mecanismo confirmado funcionando (detecta sucesso/erro,
   loga, continua, resume no final). **Isso é literalmente o comando pra
   atender o pedido do João de rerodar "a bateria toda de TUDO" sem
   precisar disparar 78 scripts manualmente sob pressão de prazo.**

Nenhuma dessas mudanças foi rodada de verdade com dado novo ainda (não
temos o dado). O que falta pra executar amanhã: (a) dados 2022-2026 em mãos,
(b) editar `ANO_FIM` em `config_periodo.R`, (c) rodar os 7 scripts do
caminho crítico na ordem do grafo (Etapa 0 abaixo) + resto do
`00_RUN_ALL_apos_etapa1.R`, (d) rodar `RODAR_TUDO.sh`.

## ETAPA 0: expandir a ingestão de dado bruto

Nenhum desses scripts lê de uma configuração central de período — cada
um repete `2016:2021` (ou `2016`/`2021` soltos) literalmente. **Passo 0,
já feito (ver acima)**: `config_periodo.R` + `source()` nos 7 pontos do
caminho crítico confirmado.

### 0.1 Fundos — posições (CONS/SH da CVM) — IGNORAR a pasta `R/`

**Não usar `R/01`-`R/33` nem `R_full.R`** — confirmado (ver "Achado
estrutural" acima) que essa árvore não alimenta o painel final há tempo.
A ingestão de fundos real e ativa é `50_universo_completo_gestora.R` →
`51_painel_multiativo_universo_completo.R` → `52`/`53`, todos em
`v2 OFICIAL/scripts/`, **já preparados** (ver "Preparação já feita" acima)
para rodar com `ANO_INICIO:ANO_FIM` de `config_periodo.R`.

**Pré-requisito ainda pendente (manual, fora do código)**: os arquivos
`cons_2022.csv` a `cons_2026.csv` e `SH_2022.csv` a `SH_2026.csv`
precisam estar em `CVM_DATA_DIR`
(`C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF` por padrão)
antes de rodar o script 50. Isso é o dado que o João vai buscar.

**RESOLVIDO em 15-16/08/2026 — não depende mais da Economatica**:
testamos e validamos que `SH_YYYY.csv`/`cons_YYYY.csv` podem ser
reconstruídos inteiramente a partir de dado público aberto da CVM (sem
acesso à API da Economatica, que o João não tem). Scripts prontos:
`107_reconstroi_SH_cvm.R` e `108_reconstroi_cons_cvm.R` (ambos em
`v2 OFICIAL/scripts/`, cabeçalho de cada um documenta URLs de download e
pré-requisitos). Rodados com sucesso em teste de fumaça contra dez/2021.

Metodologia e teto de precisão validados (4 meses/anos testados
independentemente: dez/2019, dez/2020, dez/2021, jun/2021):

- **COTA / NUMERO_DE_COTISTAS / PATRIMONIO_LIQUIDO / APLICAÇÃO**: vêm do
  `inf_diario_fi` da CVM. 99-99,9% idêntico exato ao SH_2021.csv real.
- **GESTORA / CLASSIFICACAO_ANBIMA**: carry-forward do último ano
  conhecido (2016-2021), casado por CNPJ — 100% confiável onde existe,
  cobre ~80% dos fundos continuados. Fallback via crosswalk empírico
  (`cvm_raw_downloads/crosswalks/*_FINAL.csv`, gerado 15/08/2026) para
  fundos genuinamente novos, ~68-85% preciso. Teto conhecido: ~5-6% dos
  fundos (classes de "private banking" com nome interno tipo "AM3G") não
  têm o gestor real disponível em NENHUMA fonte pública — não afeta o
  sinal HHI, só análises de comparação de gestora do TCC.
- **cons (composição de carteira)**: reconstruído via CDA da CVM
  (BLC_2 + BLC_4) com **look-through recursivo** — resolve a cadeia
  FIC→master→master-do-master usando o PL do `inf_diario` como peso de
  propriedade em cada nível. Sem isso, só ~19% dos fundos batiam (a
  maioria é fundo-de-fundos, cuja exposição a ações só aparece
  indiretamente). Com look-through: entre fundos onde CVM e Economatica
  concordam que há posição (~80-85% do universo), erro mediano por fundo
  de **9%-18%** (varia mês a mês sem padrão tipo "dezembro é sempre
  melhor" — testado e descartado), 53%-82% dos fundos com erro <20%.
  **Este é o teto real** (resíduo de reconciliação proprietária da
  Economatica não capturável em dado público), não um bug — não vale a
  pena perseguir mais precisão aqui. Não compromete o sinal HHI, que
  agrega centenas de fundos por ação (ruído individual se cancela).
  ~15-18% dos fundos por mês ficam sem match dos dois lados, tipicamente
  por lacuna de relato do CDA naquele mês específico.

**Próximo passo real**: baixar os raw files de 2022-2026 (URLs no
cabeçalho dos scripts 107/108, pasta destino `CVM_DATA_DIR/cvm_raw_downloads/`)
e rodar `ANO_RECONSTRUCAO=2022 Rscript 107_reconstroi_SH_cvm.R` seguido de
`108_reconstroi_cons_cvm.R`, revisar a saída em `CVM_DATA_DIR/reconstruido_cvm/`,
e só então mover pra `CVM_DATA_DIR` pra o script 50 consumir. Repetir por ano.

**Atenção que continua valendo** (script 52, que lê Informe Diário de
`data/raw/inf_diario_fi_%d.csv`, não `CVM_DATA_DIR`): confirmar se a CVM
mantém o formato mensal ("DADOS") pra 2022-2026 igual a 2021, ou se mudou
de novo — o script 52 não tem branch condicional de formato, então se o
layout do arquivo mudar, vai falhar ou (pior) ler campo errado
silenciosamente. Conferir a extração de um mês de teste antes de confiar
no resultado.

### 0.2 Preços de ações — B3 (COTAHIST) — JÁ PREPARADO

`v2 OFICIAL/scripts/56_precos_cotahist_b3.R` (download automático da B3) e
`57_precos_tickers_sucessores.R` já fazem `source(config_periodo.R)` e
usam `ANO_INICIO:ANO_FIM` (ver "Preparação já feita" acima) — nada a
editar nesses dois além do `ANO_FIM` central. `58_merge_final_universo_completo.R`
não tem range de ano hardcoded (confirmado, só consolida os outputs de
50-57). `60_retorno_fundo_mensal.R` também já preparado.

### 0.3 Ticker sucessor — TRABALHO MANUAL, não só código

`v2 OFICIAL/scripts/57_precos_tickers_sucessores.R` é **100% ad hoc**, feito
"na mão, um por um" (comentário do próprio cabeçalho, linhas 4-29) para
resolver 23 tickers sem preço na COTAHIST entre 2016-2021: mapa fixo de
6 empresas (linha 36), 4 mapeamentos simples (linha 61), um corte de data
hardcoded (`corte <- 201910L`, linha 69) específico do caso KROT11→COGN3, e
18 tickers permanentemente sem preço por decisão documentada em prosa.

**Isto não vai funcionar sozinho pra 2022-2026.** Qualquer evento societário
depois de dez/2021 (fusão, IPO, troca de ticker, deslistagem — ex.: caso
Americanas, consolidações de varejo/saúde, novas aberturas de capital) vai
aparecer como "sem cotação" da mesma forma que os 23 tickers originais, só
que sem ninguém ter feito a pesquisa manual de sucessor ainda. **Recomendação:
tratar isso como uma tarefa de pesquisa separada, não como parte do rerun de
script** — depois que o script 56 rodar com os anos novos, gerar a lista de
"tickers sem preço" resultante (mesma lógica de diagnóstico que gerou a lista
original de 23) e então pesquisar manualmente, evento por evento, os
sucessores/fusões de 2022-2026 antes de reescrever o script 57 com o mapa
atualizado.

### 0.4 Painel multiativo / universo completo (scripts 25/26/50/51/53/103)

Estes scripts reconstroem o painel a partir de CONS/SH direto (não do
`painel_predeterminado.csv` da árvore `R/`) — mesma lógica do item 0.1, mas
na árvore `v2 OFICIAL/`. Precisam do mesmo tratamento de range de ano:
`17_panel_todas_gestoras.R:51`, `19_beta_fundo_todas_gestoras.R:31`,
`25_panel_multiativo.R:54`, `26_combina_multiativo.R:14`,
`50_universo_completo_gestora.R:33,49`, `51_painel_multiativo_universo_completo.R:40`,
`53_beta_fundo_universo_completo.R:21`, `103_diagnostico_atricao_universo_completo.R:38`.

**Nota**: a maioria desses (17-26) já está marcada como trilha **SUPERADA** no
`PIPELINE.md` (mantida só como referência histórica) — não precisam ser
rerodados de verdade, só o range hardcoded neles fica desatualizado se
alguém abrir esses arquivos sem saber disso. Os que IMPORTAM de verdade pra
esta expansão são `50` (constrói o universo completo — alimenta `100` →
`58`) e `103` (diagnóstico de atrição, citado no `.tex`).

**Asserts que vão quebrar com amostra maior:**
- `v2 OFICIAL/scripts/50_universo_completo_gestora.R:45` —
  `stopifnot(length(todos) == 3254L, length(novos) == 434L)`.
- `v2 OFICIAL/scripts/25_panel_multiativo.R:87`,
  `26_combina_multiativo.R:30`, `51_painel_multiativo_universo_completo.R:74`
  — `stopifnot(... == 96349L)` (mesmo número nos 3, base VALE3 antiga).

Todos esses `stopifnot` são travas de sanidade propositais — o comportamento
esperado é que TRAVEM na primeira tentativa de rodar com dado novo. Isso não
é bug, é o sistema funcionando; só precisa saber que vai acontecer, calcular
o novo valor esperado e atualizar o assert (ou remover, se não fizer mais
sentido como checagem).

---

## ETAPA 1: atualizar o corte superior de amostra antes de rodar o pipeline existente

Depois que a Etapa 0 gerar `painel_universo_completo_final.csv` e
`precos_mensais_final.csv` já cobrindo até 2026, o pipeline documentado em
`PIPELINE.md`/`00_RUN_ALL_apos_etapa1.R` volta a valer — mas **13 scripts**
têm o limite superior do teste hardcoded como `202112L`, que precisa virar
o novo mês final disponível (ex.: `202612L`, ou o que os dados novos
cobrirem):

`06_figuras_extra.R:39`, `15_recompute_downstream.R:66`, `16_figuras_v2.R:88`,
`36_rmse_multihorizonte.R:40`, `42_etapa3_multiativo.R:53`,
`43_rmse_multihorizonte_multiativo.R:41`,
`44_etapa3_multiativo_gestora_multihorizonte.R:44`,
`62_etapa3_universo_completo.R:49`, `81_etapa3_janela_expansiva.R:25`,
`82_etapa3_multiativo_janela_expansiva.R:25`, `83_lambda_por_fundo.R:24`,
`90_pipeline_limpa_soma_peso.R:85` (script de trilha morta, `LIM` já
desatualizado também — ver `PIPELINE.md` "Armadilha #4", baixa prioridade),
`94_auditoria_completa_dados.R:74` (tem os DOIS limites:
`pp[ym < 201601 | ym > 202112]`), `96_pipeline_hhi_etapa1.R:100`,
`99_pipeline_hhi_lag_etapa1.R:134`.

**O corte de TREINO (`CORTE <- 202001L`) não precisa mudar** — continua
fazendo sentido treinar em `<2020-01` e testar em `>=2020-01`, só que agora
o período de teste fica muito mais longo (24 meses → potencialmente 72+
meses). Isso é uma boa notícia grande, ver seção "exploração de sinais"
abaixo.

### 1.1 Filtro de elegibilidade `>= 24 meses` — provavelmente precisa virar proporcional

Hoje `n_meses >= 24` (≈40% dos 60 meses do painel) é usado como filtro de
presença mínima de um ativo pra entrar em médias/cross-section, em 8 lugares:
`101_cross_section_universo_completo_hhi.R:115-116`,
`30_cross_section_multiativo.R:85-86`, `39_panorama_fe_ativo.R:35-36`,
`40_panorama_erro_ativo.R:48-49`, `59_cross_section_universo_completo.R:69-70`,
`97_theta_ape_hhi.R:91-92`, `99_pipeline_hhi_lag_etapa1.R:119`. Com painel de
~120 meses, 24 meses vira só ~20% — decisão a tomar (não só executar):
manter `24` fixo (critério absoluto de "tempo mínimo de histórico", pode
continuar fazendo sentido) OU trocar por proporção (`>= 0.4 * n_meses_total`,
mantém a MESMA seletividade relativa). **Recomendo levar essa escolha
específica ao João antes de decidir** — muda o tamanho da amostra elegível
de forma não trivial, mesmo tipo de decisão que já gerou o "Top-N com filtro
silencioso" registrado como lição em outras partes do projeto.

### 1.2 Lambda hardcoded — armadilha já conhecida, continua valendo

`63_pares_replicantes.R:30`, `66_beta_pares_lideranca.R:23`,
`67_previsao_ajustada_seguidor.R:35` têm `lam <- 0.0690` escrito no código,
copiado manualmente do log do script 61. Com amostra maior, o script 61 vai
estimar um lambda DIFERENTE — os 3 hardcodes vão ficar defasados
silenciosamente se não forem atualizados manualmente. `00_RUN_ALL_apos_etapa1.R`
já tem uma "PARADA OBRIGATÓRIA" pra isso — só confirmar que continua sendo
respeitada com a base nova.

### 1.3 Depois disso, seguir `00_RUN_ALL_apos_etapa1.R` normalmente

Uma vez que a Etapa 0 (dado bruto) e os pontos 1/1.1/1.2 acima estejam
resolvidos, o grafo de dependência documentado em `PIPELINE.md` volta a
valer sem modificação — é exatamente o cenário que aquele documento já foi
desenhado pra cobrir (mudança no início da Etapa 1 → refazer tudo abaixo).

---

## ETAPA 2: `TCC_finalV2.tex` — checklist de revisão manual

Não existe `\input{}` de valor calculado no documento — todo número é texto
estático, 100% manual. Padrões a procurar e corrigir (busca de texto, não
há atalho automatizado):

- **Período em prosa**: ex. linha 58, *"O período coberto vai de janeiro de
  2016 a dezembro de 2021."* — procurar todas as menções de "2021" como fim
  de período.
- **Contagens de meses**: "59 meses", "60 meses", "72 meses", "23 meses",
  "24 meses" — aparecem tanto em prosa quanto em legendas de tabela/figura
  (ex. linha 82, linha 142, linha 243). Todos vão mudar.
- **Contagens de fundos/ativos/observações**: dezenas de números isolados
  (ex. `3.254 fundos`, `536 ativos`, `10.818.395 linhas`, `8.212.294 observações`)
  espalhados pelo texto — precisam ser conferidos um por um contra os CSVs
  regerados, mesma disciplina já usada nas correções de soma_peso de agosto.
- **Split treino/teste em prosa**: linhas 353-354 e 761 descrevem o range
  literal do teste (`jan/2020-dez/2021`) — atualizar pro novo range.
- **Datas de eventos específicos** dentro do período antigo (ex. linha 736,
  871-872) — conferir se continuam corretas ou se merecem contexto adicional
  agora que há mais anos de dado ao redor delas.

**Recomendação de processo**: usar a mesma tabela "Script → onde no TCC" que
já existe no `PIPELINE.md` como checklist — depois de rodar tudo, ir número
por número conferindo cada linha citada contra o log fresco do script
correspondente, exatamente como já foi feito na correção de soma_peso.

---

## ETAPA 3: `exploracao_sinais/` — boa notícia, e o que fazer com ela

### 3.1 Boa notícia: a maioria dos 40 scripts não precisa de edição

Diferente do pipeline oficial, **quase nenhum script de `exploracao_sinais/`
hardcoda um limite SUPERIOR de data** (só `40_covid_event_study.R` usa uma
data específica, `MES_PRE <- 201912L`, que é intencional — marca o mês antes
do choque da COVID, não precisa mudar). Todos os outros 39 usam só
`CORTE <- 202001L` (treino/teste) e depois `ym >= CORTE` sem limite superior
— ou seja, **eles automaticamente vão usar TODO o período de teste
disponível, incluindo os anos novos, sem precisar editar nada**, assim que
o painel de dados subjacente (`precos_mensais_final.csv`,
`painel_multiativo_final.csv`) for atualizado pela Etapa 0/1.

### 3.2 Por que isso importa muito: poder estatístico

A limitação mais citada em quase TODO candidato rejeitado ou marginal nesta
exploração (~490 especificações até aqui) foi tamanho de amostra de teste
pequeno — 24 meses (2020-2021), na maioria dominados pelo próprio choque da
COVID. Com dados até 2026, o período de teste passa de 24 para
potencialmente 72+ meses — um salto de poder estatístico real, não
cosmético. **Recomendação forte: quando os dados novos chegarem, rerodar
TODOS os 40 scripts (mais qualquer coisa nova testada entre agora e lá) na
íntegra**, não só os que pareceram promissores — porque resultados NULOS
com N=24 podem muito bem deixar de ser nulos com N=72+ (efeito real mas
fraco, que precisava de mais dado pra aparecer), e o inverso também: um
resultado "significativo" com N=24 pode se revelar ruído quando testado com
mais observações. Isso é literalmente o pedido do João ("quando eu colocar
mais dados quero que teste com absolutamente tudo que você testou até agora
e mais").

### 3.3 Reconsiderar o limiar de Bonferroni

O contador de especificações testadas (~490 e crescendo) já é usado pra
calibrar o limiar de significância (`0,05/N`). Ao rerodar tudo com mais
dados, o N de testes-já-feitos não reseta (o histórico de tentativas
continua contando pra correção de múltiplos testes) — mas a robustez de
CADA teste individual aumenta (mais meses = erro-padrão menor, séries de
Fama-MacBeth mais longas e mais confiáveis). Registrar explicitamente no
`LOG_CANDIDATOS.md`, quando chegar a hora, se um resultado que passa
Bonferroni com a amostra nova é genuinamente novo ou só uma versão
mais-poderosa de algo já tentado (pra não inflar a contagem de "descobertas"
de forma enganosa).

### 3.4 Candidatos específicos que merecem prioridade no reteste

Pela documentação em `LOG_CANDIDATOS.md`, os candidatos mais sensíveis a
tamanho de amostra pequena (portanto os que mais podem MUDAR de conclusão
com mais dado) são: CIO Peer Momentum (candidatos #15/34-38 — resultado
já é o mais robusto tecnicamente, mas caiu por causa de tradeabilidade, não
de significância; vale conferir se ainda depende de small caps ilíquidas
com uma amostra maior, ou se aparecem versões líquidas também
significativas); a fragilidade condicional a regime de estresse (candidato
#31/32 — só 1-2 episódios de estresse na amostra atual, mais anos trazem
mais episódios de verdade pra testar a hipótese de choque); e qualquer
double-sort com quintil (menos observações por célula é sempre o gargalo,
mais meses ajuda diretamente).

---

## ETAPA 3.5: contexto Desafio Quant AI 2026 (Itaú) + achados da mega-auditoria de 15/08/2026

O TCC e o Desafio Quant AI 2026 da Itaú Asset Management andam juntos (código
de envio da equipe: **AAET**; prazo real confirmado pelo João: 16/08/2026 até
23:59, não 17/08 como o edital sugere). Documentos completos do desafio lidos
e resumidos na memória do projeto — regras-chave: relatório PDF, máx. 5
páginas, 16:9, ~750 palavras, 100% anônimo, precisa de identidade de "robô".

**Decisão fechada (15/08/2026)**: o robô do desafio vai ser baseado no achado
de fragilidade/HHI de posse institucional → volatilidade futura (h=12), NÃO
no CIO Peer Momentum. Motivo: dos três problemas identificados ao longo da
exploração (robustez estatística, iliquidez da perna vendida, e agora
defasagem de divulgação da CVM — ver abaixo), o CIO Peer Momentum falha em
pelo menos dois; a fragilidade/HHI é robusta aos três.

**Achado da mega-auditoria: defasagem de implementação.** Dado publicado da
CVM não fica pronto instantaneamente no início do mês seguinte ao de
referência (confirmado no portal de dados abertos: os 3 meses mais recentes
são retificados diariamente, não "fecham" no dia 1). Decisão: assumir 1 mês
de defasagem de implementação entre o mês de referência do CDA e o mês em
que a posição é de fato acionável (mesma prática padrão da literatura de
holdings 13F nos EUA). Isso é irrelevante pra h=12 (vira h=13, sem diferença
prática) — mais um motivo pra preferir esse sinal ao CIO Peer Momentum
(h=1), que ficaria seriamente comprometido por esse mesmo ajuste.

**Achados da mega-auditoria no script `32_fragilidade_managed_portfolio.R`
(precisam de correção antes do próximo rerun/antes de virar o backtest final
do desafio):**

1. **Descompasso de horizonte (Desenho B, `fragility-tilted`)**: recalcula o
   decil de HHI TODO MÊS e mede só o retorno do mês seguinte — testa a
   hipótese no horizonte (h=1) onde o próprio benchmark HAR-RV (candidato
   #31) já mostrou que o ganho de R² é quase zero. O sinal só tem força
   comprovada em h=12. **Correção**: rebalancear trimestral ou anualmente,
   segurando a posição pelo horizonte de ~12 meses, em vez de reformar o
   tilt todo mês.
2. **Calibração com informação do futuro (Desenho A, market timing)**: a
   normalização do escalonador `w_hhi` usa `mean(1/hhi_agregado_lag)`
   calculada sobre TODO o período de teste — exatamente o problema que
   Cederburg et al. (2020, JFE) documentaram como causa de resultados
   inflados em vol-managed portfolios "testados genuinamente fora da
   amostra" (paper que a própria pesquisa de literatura desta exploração já
   tinha encontrado e citado, sem nunca ter sido corrigido no código).
   **Correção**: calibrar a constante com janela expansiva (só dados até
   aquele mês), não com a média do período de teste inteiro.

Nenhum dos dois invalida o achado estatístico de base (HHI→vol continua
real) — são simplificações que precisam virar correções antes do resultado
virar "backtest" apresentável e defensável pro desafio, que julga
explicitamente "rigor e mitigação de vieses" (15% da nota).

---

## Ordem mestra de execução (checklist único, ATUALIZADO 15/08/2026 pós mega-auditoria)

1. [ ] Dados brutos novos (CONS/SH 2022-2026, `SH_%d.csv`) baixados
   manualmente pro `CVM_DATA_DIR` — pré-requisito de tudo, fora do código.
2. [x] ~~Criar `config_periodo.R` central~~ — **JÁ FEITO** (ver seção
   "Preparação já feita"). Só falta editar `ANO_FIM` nele.
3. [ ] ~~Rodar `R/01` → `R/13`~~ — **NÃO FAZER, árvore `R/` não é o caminho
   crítico** (achado corrigido, ver "Achado estrutural importante" acima).
   Em vez disso: rodar `50_universo_completo_gestora.R` →
   `51_painel_multiativo_universo_completo.R` → `52`/`53` (já preparados).
4. [ ] Confirmar formato do Informe Diário 2022-2026 (script 52 lê de
   `data/raw/inf_diario_fi_%d.csv`) antes de confiar no resultado.
5. [ ] Rodar `56_precos_cotahist_b3.R` (já preparado, self-download automático).
6. [ ] Gerar lista de tickers sem preço pós-2021, pesquisar sucessores
   manualmente, atualizar `57_precos_tickers_sucessores.R` (Etapa 0.3,
   trabalho manual, script já preparado pro range mas o MAPA de sucessores
   precisa de pesquisa nova).
7. [ ] Rodar `58_merge_final_universo_completo.R` → `100_checagem_soma_peso_universo_completo.R`
   (comentário já corrigido) → resync manual → `painel_multiativo_final.csv`.
8. [ ] Rodar `106_benchmark_ibovespa.R` com JSON novo do Yahoo (^BVSP,
   2022-2026) concatenado ao já processado (2014-2021) — script já pronto,
   só falta o dado novo.
9. [ ] Atualizar os 13 arquivos com `202112L` (Etapa 1) pro novo limite
   (lista completa de arquivo:linha na seção ETAPA 1 acima).
10. [ ] Decidir se o filtro de `>= 24 meses` vira proporcional (Etapa 1.1).
11. [ ] Rodar `00_RUN_ALL_apos_etapa1.R` do jeito que já está documentado,
    respeitando a parada do lambda (Etapa 1.2).
12. [ ] Corrigir os `stopifnot` que travarem (valores esperados novos —
    nos scripts 50/51 já ficaram condicionais a `ANO_FIM==2021L`, só vão
    virar aviso, não vão travar).
13. [ ] Revisar `TCC_finalV2.tex` número por número (Etapa 2).
14. [ ] Rodar `bash RODAR_TUDO.sh` dentro de `exploracao_sinais/` — reroda os
    78 scripts numerados na íntegra, log individual por script, resumo no
    final (já testado, funcionando).
15. [ ] **Antes de aceitar o script 32 como backtest final do desafio**:
    aplicar as 2 correções da seção "ETAPA 3.5" acima (rebalanceio
    trimestral/anual casado com h=12; calibração por janela expansiva em
    vez de média do período de teste inteiro).
16. [ ] Atualizar `LOG_CANDIDATOS.md` e este documento com o que mudou de
    conclusão.

## Decisões que precisam do João antes de executar (não são só técnicas)

- Filtro de elegibilidade `>= 24 meses` → manter fixo ou virar proporcional
  (item 1.1).
- Até que mês exatamente os dados novos vão cobrir (define o valor exato
  pra substituir `202112L`).
- Se o esforço manual de pesquisa de tickers sucessores (item 0.3) é
  prioridade imediata ou pode ficar como limitação documentada (perder
  alguns tickers novos "sem preço", do mesmo jeito que já acontece com
  parte dos 2016-2021).
