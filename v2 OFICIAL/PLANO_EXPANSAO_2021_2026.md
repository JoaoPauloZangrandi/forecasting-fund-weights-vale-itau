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
- **TODOS os ~40 scripts de `exploracao_sinais/`, um por um** — não só os
  que pareceram promissores ou os que sobreviveram a robustez. Um resultado
  nulo com N=24 pode deixar de ser nulo com N=72+; um resultado
  "significativo" com N=24 pode se revelar ruído com mais dado. As duas
  direções importam igualmente, e só dá pra saber rodando todos de novo.
- **Todo candidato adicional testado nesta sessão** entre agora e a chegada
  dos dados (candidatos #41 em diante, rodando em background neste momento)
  entra nessa mesma obrigação de reteste — a lista "TUDO" cresce junto com
  a exploração, não é uma lista fixa fechada hoje.
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

## Achado estrutural importante: duas árvores de scripts, não uma

A ingestão de dado bruto de FUNDOS (CONS/SH da CVM) ainda depende da pasta
antiga `R/` (`R/01_load_bases.R` → ... → `R/12_add_features_allyears.R` →
`v2 OFICIAL/scripts/13_etapa1_unificada.R` lê o resultado disso,
`painel_predeterminado.csv`). Só a ingestão de PREÇOS (B3) foi totalmente
refeita dentro de `v2 OFICIAL/scripts/` (54-58). **Isso significa que
expandir o período de fundos exige editar scripts em `R/`, não só em
`v2 OFICIAL/`** — não dá pra tratar `v2 OFICIAL/` como autossuficiente pra essa
tarefa específica.

---

## ETAPA 0 (nova): expandir a ingestão de dado bruto

Nenhum desses scripts hoje lê de uma configuração central de período — cada
um repete `2016:2021` (ou `2016`/`2021` soltos) literalmente. **Passo 0
recomendado, antes de tocar em qualquer coisa**: criar um `config_periodo.R`
com `ANO_INICIO <- 2016L; ANO_FIM <- 2026L` (ou o que os dados novos
permitirem) e `source()` dele nos pontos abaixo, em vez de editar ~30 arquivos
um por um manualmente — reduz drasticamente a chance de esquecer um.

### 0.1 Fundos — posições (CONS/SH da CVM), pasta `R/`

Ordem de execução (sequencial, cada um lê o output do anterior):

1. `R/01_load_bases.R` — linha 19: `YEARS <- 2016:2021`. Lê
   `cons_%d.csv`/`SH_%d.csv` de `CVM_DATA_DIR`
   (`C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF` por padrão).
   **Pré-requisito**: os arquivos `cons_2022.csv` a `cons_2026.csv` (e `SH_`
   equivalentes) precisam estar baixados nesse diretório ANTES de rodar —
   isso é manual, não automatizado por nenhum script aqui.
2. `R/10_build_panel_allyears.R` — linha 27: `for (y in 2016:2021)`; o nome
   do arquivo de saída também tem o range hardcoded
   (`painel_vale_itau_2016_2021.csv`) — atualizar o nome do arquivo também,
   ou os scripts a jusante que leem esse nome literal vão falhar silenciosamente
   (vão continuar lendo o arquivo VELHO se ele não for sobrescrito/renomeado).
3. `R/11_add_flows_allyears.R` — linhas 24-25, loop de fluxo (Informe Diário).
   **Atenção especial**: o comentário nas linhas 10-12 documenta que
   **2016-2020 usa arquivos ANUAIS (formato HIST)** e **2021 usa arquivos
   MENSAIS (formato DADOS)** — são dois formatos de arquivo da CVM
   diferentes. **Antes de rodar**, confirmar se 2022-2026 continuam no
   formato DADOS (mensal, mesmo formato que 2021) ou se a CVM mudou de novo
   — script não tem branch genérico, só o caso "2021" tratado explicitamente
   na estrutura atual do código.
4. `R/12_add_features_allyears.R` — linha 26: `for (y in 2016:2021)`.
5. `R/13_add_stock_allyears.R` — linha 52: filtro `ymk >= 201512 & ymk <= 202111`
   (note o `201512`, um mês ANTES do início nominal — provavelmente pra
   permitir lag de 1 mês na primeira observação; manter essa margem ao
   estender o limite superior). Nome de saída também tem `_2016_2021_full.csv`.
6. `R/15_validate_allyears.R`, `R/20_extensive_margin.R`,
   `R/31_build_panel_multiasset.R` (linhas 24, 124, 141), `R/33_fund_beta.R`
   (linha 27) — mesmo padrão `for (y/yy in 2016:2021)`, sem novidade
   estrutural.
7. `R_full.R` (script raiz "tudo num arquivo só", se ainda for usado como
   referência/backup) — linhas 69, 359, 380, 392, mesmo padrão.

**Asserts que vão QUEBRAR a execução com amostra maior (não silenciosamente
— travam o script, o que é bom, mas precisam de correção manual do valor
esperado):**
- `R/31_build_panel_multiasset.R:111` — `stopifnot(nvale2 == 26123L)`.

### 0.2 Preços de ações — B3 (COTAHIST), pasta `v2 OFICIAL/scripts/`

Fonte atual e oficial é COTAHIST (script 56); os scripts 54/55 (Yahoo
Finance) estão superados, não precisam ser tocados.

1. `v2 OFICIAL/scripts/56_precos_cotahist_b3.R` — linha 28 (download) e
   linhas 49-50 (parsing): `for (y in 2016:2021)`, baixa
   `COTAHIST_A%d.ZIP` de `https://bvmf.bmfbovespa.com.br/InstDados/SerHist/COTAHIST_A%d.ZIP`.
   Esse é o único ponto de todo o pipeline que já é 100% automatizável (não
   depende de download manual prévio) — só estender o range do loop já
   resolve a busca do dado. Comentários de log nas linhas 66/80 também
   mencionam "2016-2021" em texto, atualizar por clareza (não quebra nada
   se ficar desatualizado, mas confunde quem ler o log depois).
2. `v2 OFICIAL/scripts/57_precos_tickers_sucessores.R` — linhas 39-40, mesmo
   range. Ver 0.3 abaixo — este script precisa de trabalho manual adicional,
   não só estender o loop.
3. `v2 OFICIAL/scripts/58_merge_final_universo_completo.R` — consolida
   56+57; conferir se tem algum range hardcoded também (não confirmado no
   levantamento, checar ao editar).
4. `v2 OFICIAL/scripts/60_retorno_fundo_mensal.R` — linha 30:
   `for (y in 2016:2021)`.

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

## Ordem mestra de execução (checklist único)

1. [ ] Dados brutos novos (CONS/SH 2022-2026, `SH_%d.csv`) baixados
   manualmente pro `CVM_DATA_DIR` — pré-requisito de tudo, fora do código.
2. [ ] (Opcional mas recomendado) criar `config_periodo.R` central.
3. [ ] Rodar `R/01` → `R/13` (Etapa 0.1) com range estendido.
4. [ ] Confirmar formato do Informe Diário 2022-2026 (item 0.1.3) antes de
   rodar `R/11`.
5. [ ] Rodar `56_precos_cotahist_b3.R` com range estendido (Etapa 0.2).
6. [ ] Gerar lista de tickers sem preço pós-2021, pesquisar sucessores
   manualmente, atualizar `57_precos_tickers_sucessores.R` (Etapa 0.3).
7. [ ] Rodar `58_merge_final_universo_completo.R`.
8. [ ] Atualizar os 13 arquivos com `202112L` (Etapa 1) pro novo limite.
9. [ ] Decidir (com o João) se o filtro de `24 meses` vira proporcional
   (Etapa 1.1).
10. [ ] Rodar `00_RUN_ALL_apos_etapa1.R` do jeito que já está documentado,
    respeitando a parada do lambda (Etapa 1.2).
11. [ ] Corrigir os `stopifnot` que travarem (valores esperados novos).
12. [ ] Revisar `TCC_finalV2.tex` número por número (Etapa 2).
13. [ ] Rerodar os 40 scripts de `exploracao_sinais/` na íntegra (Etapa 3.2)
    + qualquer candidato novo testado nesse meio-tempo.
14. [ ] Atualizar `LOG_CANDIDATOS.md` e este documento com o que mudou de
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
