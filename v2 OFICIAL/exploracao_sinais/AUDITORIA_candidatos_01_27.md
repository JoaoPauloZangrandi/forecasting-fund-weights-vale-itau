# Auditoria adversarial — scripts 01 a 27 (exploração de sinais de lucro)

Segunda passada de auditoria, focada em achar os MESMOS padrões de erro já
documentados no `LOG_CANDIDATOS.md` (t-test pooled, breaks de quintil
fixos, grupos degenerados, look-ahead, erro de `addm()`) em candidatos que
não foram escrutinados com o mesmo rigor que Comomentum e CIO Peer
Momentum "original" receberam. Metodologia: leitura integral do
`LOG_CANDIDATOS.md` (seções relevantes a scripts 01-27, linhas 1-820) +
leitura linha a linha dos 27 scripts + reconstrução independente em Python
(sem depender do R, que não está instalado neste ambiente) de 3
especificações-chave para checar números do log contra dado bruto.

Legenda de severidade: **CRÍTICO** (muda ou deveria mudar uma conclusão
já registrada) / **IMPORTANTE** (violação real de disciplina
metodológica, impacto prático baixo nos vereditos atuais mas deveria ser
corrigido) / **COSMÉTICO** (bug sem efeito nos resultados, vale registrar
por higiene de código).

---

## F1 — CRÍTICO: CIO Peer Momentum (candidato #15) tem o MESMO artefato de "grupos degenerados" do Comomentum, não divulgado nos scripts 01-27

**Onde:** `11_cio_peer_momentum.R` (função `fama_macbeth`, linhas
115-141), reusada com a mesma lógica em `13_diagnostico_cio_peer_momentum.R`
(linhas 69-95), `14_cio_robustez_periodo.R` (linhas 27-38),
`15_cio_validacao_fatores_custos.R` (linhas 106-127, parte Q),
`16_cio_h2_e_janela_agregada.R` (linhas 29-53), `17_combinacao_cio_efit.R`
(linhas 58-78), `18_window_dressing_calendario.R` (linhas 28-48),
`22_grid_search_cio.R` (linhas 83-115), `23_h3_tentativas_adicionais.R`
(linhas 36-63).

**O que eu chequei:** reconstruí em Python o teste exato de
`11_cio_peer_momentum.R` para h=1 (breaks de quintil calculados no treino
`ym<202001`, aplicados a cada mês do teste) e reproduzi os números do log
byte a byte (spread=4,454pp/mês, t=2,507, p=0,0209 — bate exatamente com
`candidatos_11_fama_macbeth.csv`). Depois contei quantas ações caem em
cada perna (quintil 1 vs 5) MÊS A MÊS, algo que o script 11 nunca imprime
nem salva:

| Achado | Valor |
|---|---|
| Meses de teste (h=1) | 21 |
| Meses com N=1 numa perna | **6** (202002, 202004, 202102, 202106, 202109, 202110) |
| Meses com N≤5 numa perna | **14 de 21 (67%)** |
| N mediano (todas as pernas) | 46 (bate com o log — mas é enganoso, ver abaixo) |

O log de candidato #15 (linhas 296-334 do `LOG_CANDIDATOS.md`) usa
justamente essa mediana ("mediana 46 ações por quintil-mês, não
degenerada") como evidência de que o CIO não sofre do problema do
Comomentum. A mediana está correta, mas **esconde que mais da metade dos
meses tem uma das duas pernas com 1 a 8 ações** — exatamente o padrão que
inflacionou o Comomentum (candidato #14) por retorno idiossincrático de
poucos papéis dominando a "média do quintil".

Testei a sensibilidade do resultado removendo os meses degenerados:

| Amostra | n meses | spread | t | p |
|---|---|---|---|---|
| Todos os 21 meses (script 11, como reportado) | 21 | 4,45pp | 2,51 | **0,021** |
| Removendo só o mês de N mínimo (é o que `13_diagnostico_cio_peer_momentum.R` faz) | 20 | 3,60pp | 2,20 | 0,041 (ainda "significativo") |
| Removendo TODOS os 6 meses com N=1 numa perna | 15 | 4,44pp | 2,07 | **0,058 (deixa de ser significativo a 5%)** |
| Removendo os 14 meses com N≤5 numa perna | 7 | 2,76pp | 1,76 | **0,129 (claramente não significativo)** |

**O diagnóstico existente é insuficiente.** `13_diagnostico_cio_peer_momentum.R`
(linhas 69-78) faz exatamente a checagem certa em espírito — acha o mês
com N mínimo e testa exclusão — mas só remove UM mês (o mínimo), quando
na verdade há 6 meses igualmente degenerados (N=1) e mais 8 meses quase
tão ruins (N=2 a 8). Isso faz o log registrar "Robusto a excluir o mês com
amostra mais fina (spread quase idêntico)" (linha ~306 do log) quando na
verdade a robustez real, testada corretamente, não se sustenta — o
resultado unicamente "sobrevive" porque a exclusão testada foi parcial.

**Isso já foi encontrado e corrigido — só que FORA do intervalo que
auditei.** `36_reconciliacao_metodologia_cio.R` (linha 998+ do log,
datado de 15/08/2026, fora do range 01-27 que me foi designado)
redescobre exatamente este problema de forma independente — "em vários
meses do teste, isso gera quintis degenerados — até 1 única ação na
perna... exatamente o mesmo artefato que causou o falso-positivo do
Comomentum" — e corrige recalculando os breaks A CADA MÊS (não uma vez
só no treino), obtendo "mínimo 53 ações por perna, mediana 57 — nunca
degenera". Ou seja: **dentro do escopo 01-27 que me foi pedido para
auditar, a seção do log dedicada ao candidato #15 (linhas 287-402,
"O MAIS PROMISSOR ATÉ AGORA... não morreu ao primeiro exame rigoroso")
descreve uma robustez que não existe da forma como foi checada** — a
correção real só aparece em scripts fora do meu escopo.

**Causa raiz:** a função `fama_macbeth()` (criada corretamente em
`04_long_short_fama_macbeth.R` para consertar o erro do t-test pooled do
script 03) calcula os breakpoints de quintil **uma única vez**, a partir
da distribuição do sinal no TREINO inteiro, e aplica esse corte FIXO a
todo mês do teste — em vez de recortar a distribuição a cada mês (o
padrão real de Fama-French/Fama-MacBeth). Isso é exatamente o padrão de
erro #2 do escopo desta auditoria ("breaks de quintil fixos calculados no
treino aplicados ao teste, em vez de recalculados a cada mês — mesmo
padrão do erro do CIO original"). A diferença é que aqui ele NÃO foi
percebido a tempo dentro do range 01-27; só foi percebido em script fora
do escopo.

**Solução concreta:** dentro do range 01-27, o candidato #15 (e todos os
resultados de h=1 dependentes dele nos scripts 13-18/22/23) deveria ser
re-testado com breakpoints recalculados a cada mês do teste (a mesma
correção que script 36 já aplica, fora do escopo). Como essa correção já
existe em script 36+, a ação prática é: (a) inserir uma nota explícita na
seção do candidato #15 do `LOG_CANDIDATOS.md` (linhas 287-402) apontando
para a correção em `36_reconciliacao_metodologia_cio.R` e deixando claro
que a conclusão "sobrevive a múltiplas checagens de robustez" descrita
ali foi feita com metodologia depois identificada como falha; (b)
padronizar a função `fama_macbeth()` usada em TODOS os scripts 04-27 para
sempre recalcular breaks por mês, não só no candidato CIO.

---

## F2 — CRÍTICO (sistêmico): a função `fama_macbeth()` reusada em quase todos os scripts 04-27 nunca recalcula quintis por mês, e a maioria não verifica N mínimo por grupo

Consequência direta de F1: como a mesma função (copiada com pequenas
variações) é usada em `04, 05, 06, 07, 08, 09, 11, 12, 13, 14, 15, 16, 17,
18, 19, 20, 21, 22, 23, 24, 25, 26, 27`, **todo resultado baseado em
quintil/decil nesse intervalo carrega o mesmo risco estrutural de F1**,
mesmo quando o resultado final é nulo (um nulo pode esconder um grupo
degenerado que "por sorte" não vira problema — exatamente o alerta do
enunciado desta tarefa). Só 5 dos ~20 scripts que usam essa função (`11,
13, 14, 16, 19`) chegam a IMPRIMIR o N mínimo por quintil-mês, e mesmo
esses não fazem nada com a informação (não excluem, não avisam, não
ajustam o teste).

Dado o volume (>20 scripts, dezenas de especificações), não tive tempo de
reconstruir numericamente TODAS elas. Para calibrar o quanto isso importa
na prática, reconstruí duas amostras adicionais fora do CIO:

- **Demanda agregada revelada, h=6** (`04_long_short_fama_macbeth.R`,
  o "quase-achado" mais citado do log depois do CIO, p=0,057): reconstruí
  em Python e bati os números exatos do log (19 meses, spread=1,88pp,
  t=2,03, p=0,057). **N por quintil-mês: mínimo 7, mediana 58,5 — NÃO é
  degenerado.** Este é um contra-exemplo saudável: o quase-empate aqui é
  legítimo, não um artefato.

- **52-week-high × alta ownership institucional** (`24_anomalias_classicas.R`,
  linha 118-129, o único resultado com p<0,01 da família de anomalias
  clássicas, t=3,13, p=0,005 — não bate Bonferroni mas é o "melhor caso"
  citado no log): reconstruí em Python e bati exatamente (t=3,133,
  p=0,00484, 23 meses). Achei um problema relacionado, mas diferente: a
  variável `prox_52w_high` tem um "ponto de massa" em exatamente 1,0
  (muitas ações no subgrupo de alta ownership estão simultaneamente "na
  máxima de 52 semanas"), o que faz os percentis 80 e 100 do treino
  colidirem — o corte nominal de 5 grupos colapsa silenciosamente para 4
  (via o fallback `if (n_grupos<5) quintil := ifelse(...)` presente em
  quase todo script pós-#09). Resultado: mar/2020 fica com ZERO ações na
  perna "top", e o N mínimo cai a 3 (abr/2020). Não muda o veredito (já
  rejeitado, não bate Bonferroni), mas é o mesmo tipo de cegueira de F1 —
  ninguém olhou o N por mês antes de reportar p=0,005 como "o melhor caso
  da família".

**Solução concreta:** (1) padronizar `fama_macbeth()` para recortar
quintis/decis A CADA MÊS do teste, não uma vez só a partir do treino —
essa é a correção estrutural que resolve F1 e previne o mesmo problema em
qualquer candidato futuro; (2) até isso ser feito, qualquer resultado
"quase-significativo" (p entre 0,03 e 0,10) reportado nos scripts 04-27
deveria ser tratado como não verificado quanto a grupos degenerados,
exceto os dois casos que checei acima (demanda agregada revelada h=6:
limpo; 52wh×alta-ownership: borderline mas já rejeitado de qualquer
forma).

---

## F3 — IMPORTANTE: winsorização (clipping 1%/99%) calculada sobre a amostra INTEIRA (treino+teste), não só o treino

**Onde:** `01_teste_oos_fit_lou.R:76`, `02_herding_demanda_agregada_firesale.R:98`,
`03_lideres_e_long_short.R:103`, `04_long_short_fama_macbeth.R:77`,
`05_breadth_e_latent_demand.R:78,109,130`, `07_subamostra_small_caps.R:91`,
`21_ensemble_ml.R:41`.

Padrão repetido: `q <- quantile(dem$dem_pct, c(0.01,0.99), na.rm=TRUE)`
(ou equivalente para FIT, breadth, latent demand) é calculado sobre o
data.table INTEIRO, antes da separação treino/teste — ou seja, o limite
de winsorização "sabe" sobre outliers do período de teste ao cortar
valores do período de treino (e vice-versa). Isso é uma violação branda
da disciplina "só informação disponível até aquele mês", mesmo que o
alpha/beta do modelo em si continue sendo estimado corretamente só no
treino nesses mesmos scripts.

**Impacto prático:** baixo — todos os candidatos afetados (FIT, demanda
agregada revelada, breadth, latent demand, features do ensemble ML) já
foram rejeitados independentemente disso. Mas é o tipo de vazamento que
poderia inflar um resultado borderline se algum desses sinais voltar a
ser testado no futuro.

**Solução concreta:** trocar `quantile(df$var, ...)` por
`quantile(treino$var, ...)` (calcular os limites de corte só com dado
pré-corte) em cada uma das linhas listadas acima, e aplicar o mesmo
limite (não recalculado) ao teste — mesmo padrão já usado corretamente
para os coeficientes de regressão nos mesmos scripts.

---

## F4 — IMPORTANTE: classificação "small cap" usa tamanho médio da amostra INTEIRA (2016-2021), não só o histórico até cada mês

**Onde:** `07_subamostra_small_caps.R`, linhas 21-26.

```r
tamanho_ativo <- pp[, .(valor_total_medio = sum(valor_mil)), by = .(ativo)]  # soma TODA a amostra
corte_small <- quantile(tamanho_ativo$valor_total_medio, 1/3)
small_caps <- tamanho_ativo[valor_total_medio <= corte_small]$ativo
```

A classificação "é small cap ou não" usa a soma de `valor_mil` ao longo
de TODO o período (treino + teste, 2016-2021) para decidir quais 171 de
513 ativos entram no subgrupo — inclusive quando o teste que segue é
sobre o período de teste (2020+). É viés de look-ahead brando: um ativo
que cresceu MUITO durante o período de teste (e portanto não seria
classificado "small" com dado só até 2019) pode ainda assim entrar no
grupo se sua média histórica total ficar baixa, e vice-versa.

**Impacto prático:** baixo (candidato já rejeitado — todos os p entre
0,22 e 0,82), mas se o candidato #10 for revisitado, a classificação
deveria usar apenas o tamanho médio disponível até o mês de formação do
sinal (ou, no mínimo, só o período de treino).

---

## F5 — COSMÉTICO: variável morta / bug de copiar-colar sem efeito prático

**Onde:** `12_ownership_x_momentum.R`, linha 36.

```r
valor_total <- pp[, .(valor_total_mil = sum(valor_mil), n_fundos = uniqueN(ym)), by = .(ativo, ym)]
```

`n_fundos = uniqueN(ym)` dentro de um agrupamento já particionado por
`ym` é trivialmente sempre 1 (conta valores únicos de `ym` dentro de um
grupo que já tem um único `ym`). Não é o que o nome da variável sugere
(número de fundos) — provavelmente um erro de copiar `uniqueN(cod_fundo)`
de outro trecho do código. A variável nunca é referenciada de novo no
resto do script, então não afeta nenhum resultado — mas é um sinal de
falta de revisão nesse trecho específico.

---

## F6 — COSMÉTICO: tabela de diagnóstico descarta a coluna N antes de imprimir

**Onde:** `14_cio_robustez_periodo.R`, linhas 31-38.

```r
por_mes <- teste[quintil %in% c(1,5), .(retorno_medio = mean(retorno), n=.N), by = .(ym, quintil)]
sm <- dcast(por_mes, ym ~ quintil, value.var = "retorno_medio")   # so' pega retorno_medio, 'n' se perde
...
print(sm)
```

O script calcula `n=.N` por quintil-mês mas o `dcast` seguinte usa
`value.var = "retorno_medio"`, descartando a coluna `n` antes da tabela
"spread mês a mês" ser impressa para inspeção visual. Isso torna
invisível, no próprio diagnóstico que o script pretende fornecer,
exatamente o tipo de degenerescência documentado em F1 — quem olha essa
tabela não vê o N de cada perna, só a média de retorno. Mesma raiz de
problema que F1, script diferente.

---

## Verificações que NÃO encontraram problema (registradas para não parecer que a auditoria só procurou defeito)

- **`t.test()` pooled tratando ativo-mês como observação independente**:
  busquei `t\.test\(` em todo o intervalo 01-27. A ÚNICA ocorrência é
  `03_lideres_e_long_short.R:88`, que já está corretamente documentada no
  log como o erro identificado e corrigido pelo script 04. Nenhuma
  segunda instância escapou.
- **Aritmética de `addm()`**: a função é definida de forma idêntica
  (e matematicamente correta — soma/subtrai meses em formato AAAAMM,
  ano/mês tratados como base-12 corretamente) em todos os 27 scripts.
  Tracei manualmente os usos de maior risco (retorno acumulado t+1 a t+3
  em `16_cio_h2_e_janela_agregada.R` linhas 75-81; defasagem de peso em
  quase todo script 02-09; defasagem de skill em `06` e `08` e `17`) —
  todos na direção correta (sinal olha para o passado, teste de retorno
  olha para o futuro). Nenhuma inversão de sinal encontrada.
- **`08_efit_fluxo_esperado.R` (E[FIT], candidato #11-12)**: metodologia
  limpa — 1º estágio (`flow_aum ~ skill_12m`) treinado só no período de
  treino, aplicado fora da amostra sem vazamento; skill defasado
  corretamente (`ym := addm(ym,1)`). Reconstruí os números do log
  (R²_OOS h=1: 0,47%, h=3: 0,62%, h=6: 0,52%; FIT suavizado h=3: t=2,10,
  p=0,048) via `candidatos_08_linear.csv`/`candidatos_08_fama_macbeth.csv`
  e batem exatamente.
- **`10_diagnostico_comomentum_h6.R`**: diagnóstico já extremamente
  minucioso (conta ações por quintil-mês, testa exclusão do período de
  crash, mede concentração num único mês) — não achei nada para
  acrescentar, é o padrão-ouro de como as outras checagens deveriam ter
  sido feitas.
- **`15_cio_validacao_fatores_custos.R`, parte (P)**: a regressão
  Fama-MacBeth com controles simultâneos (`peer_ret_z + log_tamanho_z +
  beta_z + ret_proprio_z`, 1 coeficiente por mês) usa o regressor de
  forma CONTÍNUA, sem cortar em quintil — por isso não sofre do problema
  de F1/F2. É o teste mais metodologicamente limpo de toda a família CIO
  no intervalo auditado.
- **`21_ensemble_ml.R`**: split treino/validação/teste feito por MÊS (não
  por linha), sem vazamento; único ponto fraco é o mesmo padrão de F3
  (winsorização de uma feature) e o uso do próprio período de teste para
  definir os breakpoints de avaliação do quintil de previsão (menor, dado
  que não usa retorno futuro para isso, só as previsões).

---

## Resumo (≤200 palavras)

Revisei os 27 scripts em profundidade equivalente (li código linha a
linha em todos; reconstruí numericamente em Python — sem R disponível
neste ambiente — 3 especificações-chave para checar o log contra dado
bruto). O achado central: o candidato mais promovido do intervalo, **CIO
Peer Momentum (h=1)**, sofre exatamente o mesmo artefato de "grupos
degenerados" já documentado para Comomentum — confirmei numericamente que
6 dos 21 meses de teste têm 1 única ação numa perna, e que excluir todos
os meses degenerados derruba a significância (p sobe de 0,021 para 0,058
até 0,129, dependendo do corte). O diagnóstico existente no script 13
só remove 1 mês (não os 6), criando falsa confiança. Essa mesma falha foi
depois redescoberta e corrigida em script 36 — mas isso está fora do
range 01-27 que me foi pedido para auditar, então dentro do escopo
designado o problema estava sem sinalização. A causa raiz (função
`fama_macbeth` com breaks fixos do treino, não recalculados por mês) é
compartilhada por ~20 scripts; só verifiquei mais 2 casos numericamente
(um limpo, um borderline) por falta de tempo para os ~15 restantes — isso
deveria ser tratado como pendência, não como "verificado limpo".
Encontrei também vazamento leve de look-ahead em winsorização (7 scripts)
e 2 bugs cosméticos sem efeito prático.
