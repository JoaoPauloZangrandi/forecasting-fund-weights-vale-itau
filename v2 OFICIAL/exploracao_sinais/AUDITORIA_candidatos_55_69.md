# Auditoria adversarial — candidatos #55-69 (rodada de agentes paralelos, 15/08/2026)

Auditoria de segunda camada sobre os scripts `55_construir_sinais_compostos.R` a
`69_diagnostico_efit_x_hhi.R` (`v2 OFICIAL/exploracao_sinais/scripts/`), cobrindo
os 3 "agentes" que produziram esse intervalo: composto multi-sinal (55-59),
centralidade de rede (60-64) e regime de vol × choques (65-69). Não altera
nenhum código — só lê, executa (R 4.5.1 instalado em `C:\Program Files\R\`) e
compara contra `LOG_CANDIDATOS.md`.

**Metodologia da auditoria**: (1) leitura linha a linha dos 15 scripts; (2)
comparação byte-a-byte da construção da matriz CIO (script 60) contra o
script original (`11_cio_peer_momentum.R`); (3) reprodução independente do
teste de confound de tamanho do script 64 via uma regressão Fama-MacBeth
conjunta escrita do zero (não a técnica de resíduo usada no script); (4)
reexecução direta de todos os scripts sem `fwrite` (59, 66, 69) para conferir
os números citados no log, que só existem no console; (5) checagem de todo
CSV em `data/` gerado por esses scripts contra os números citados no log,
número a número; (6) grep sistemático por `t.test(` pooled e por breaks de
quantil fixos do treino aplicados no teste, nos 15 scripts.

**Resultado geral**: a disciplina metodológica das "3 lições inegociáveis"
(Fama-MacBeth de verdade, quintil recortado a cada mês, N mínimo sempre
reportado) foi seguida corretamente em todos os 15 scripts — não encontrei
nenhum `t.test()` pooled nem nenhum break fixo de treino aplicado ao teste
neste intervalo. Toda tabela numérica citada no log (dezenas de valores de
t/p/spread/n_min conferidos) bateu com os CSVs e com reexecução direta,
com uma única exceção de baixa relevância (achado #6 abaixo). O diagnóstico
de confound do candidato de centralidade está CORRETO e foi confirmado de
forma independente. Os problemas reais encontrados são de outra natureza:
normalização calculada fora do período de treino (item 1) e uma
inconsistência de qualidade de dado na variável `hhi_posse` pré-calculada
que o script 55 reutiliza sem checar (item 2).

---

## Achado 1 — IMPORTANTE: winsorização calculada na amostra inteira (treino+teste), não só no treino

**Onde**: `55_construir_sinais_compostos.R:83,93`; `65_regime_vol_x_sinais_retorno.R:68`;
`67_choque_posse_wardlaw_corrigido.R:81`; `68_choque_posse_x_regime_vol.R:46`.

```r
# 55_construir_sinais_compostos.R, linha 83
q <- quantile(br$delta_breadth_pct, c(0.01, 0.99), na.rm = TRUE)   # br cobre treino+teste
br[, delta_breadth_w := pmin(pmax(delta_breadth_pct, q[1]), q[2])]
```

O mesmo padrão se repete para `dem_pct_w` (linha 93 do script 55),
`delta_breadth_w` (script 65, linha 68) e `choque_bruto_w`/`choque_corrigido_w`/
`choque_abs_w` (scripts 67 linha 81, 68 linha 46). Em todos os casos os
limites de 1%/99% usados para capar outliers são calculados sobre a tabela
inteira (`br`, `dem`, `valor_tk`), que inclui tanto `ym<CORTE` quanto
`ym>=CORTE` — ou seja, o valor do cap usado para "limpar" uma observação de
TESTE é influenciado por observações do próprio período de teste (inclusive
por caudas extremas que só existem no teste, como o crash de 2020).

Isso é exatamente o tipo de "estatística de normalização calculada sobre a
amostra inteira em vez de só treino" que a auditoria foi instruída a
procurar. É uma violação real da disciplina "treino</teste>=CORTE" que o
cabeçalho do script 55 explicitamente reivindica cumprir ("Sem look-ahead...
mesma convenção usada no resto da exploração", linhas 26-30) — a reivindicação
não é totalmente precisa.

**Severidade e impacto prático**: o efeito é provavelmente pequeno — a
winsorização só afeta ~2% das observações (as caudas), e apenas 4 sinais
(`delta_breadth_w`, `dem_pct_w`, `choque_bruto_w`, `choque_corrigido_w`,
`choque_abs_w`) entre os ~15 testados neste intervalo de scripts, todos já
classificados como nulos independentemente disso (nenhuma conclusão
"achado real" depende deste detalhe). Mas é sistemático (4 de 15 scripts) e
merece correção, porque não seria caro consertar e a autoconfiança do
cabeçalho ("sem look-ahead") fica formalmente incorreta enquanto isso não
for corrigido.

**Solução concreta**: calcular `q <- quantile(<var>[ym < CORTE], c(0.01,0.99), na.rm=TRUE)`
(ou equivalente, filtrando por `ym < CORTE` antes do `quantile()`) e aplicar
esse MESMO cap (fixo, do treino) tanto ao treino quanto ao teste — análogo
ao que já é feito corretamente para os coeficientes de regressão (EFIT,
scores compostos) em todos os scripts deste intervalo.

---

## Achado 2 — IMPORTANTE: `hhi_posse` pré-calculado (`fit_ativo_mes.csv`) diverge de forma severa da reconstrução feita do zero nos scripts 63/65/67/68/69

**Onde**: `55_construir_sinais_compostos.R:69-71` usa
`fread(file.path(DATA, "fit_ativo_mes.csv"), select = c("ativo","ym","hhi_posse","n_fundos"))`
e filtra só `n_fundos >= 10`, sem qualquer checagem de consistência. Esse
`hhi_posse` alimenta diretamente `56_composto_ranking_percentil.R` (como
`s_hhi`), `57_composto_regressao_multipla_fm.R` e `58_composto_ic_weighted.R`.

Em paralelo, os scripts 63/65/67/68/69 (agentes "rede" e "regime")
RECALCULAM `hhi_posse` do zero a partir de `painel_multiativo_final.csv`,
todos com a MESMA fórmula (`sum((valor_posicao/sum(valor_posicao))^2)`,
`valor_posicao = peso*aum_prev`) — essa parte está correta e consistente
entre si (validado por execução).

**O problema**: comparei os dois `hhi_posse` (pré-calculado de
`fit_ativo_mes.csv` vs. reconstrução fresca) para as 13.523 células
ticker-mês que sobrevivem ao filtro `n_fundos>=10` em ambas as fontes.
Correlação geral é 0,947 (razoável), mas há um número não-trivial de casos
com divergência GIGANTESCA em `n_fundos`, mesmo após o filtro de qualidade:

| ticker | ym | n_fundos (`fit_ativo_mes.csv`) | n_fundos (reconstrução fresca) |
|---|---|---|---|
| VIVT4 | 202011 | 702 | 11 |
| TIMP3 | 202010 | 791 | 5 |
| IGTA3 | 202111 | 800 | 34 |
| NATU3 | 201912 | 595 | 45 |
| ITSA3 | 201705 | 54 | 567 |
| CPFE3 | 201906 | 455 | 958 |

Diferenças de centenas de fundos para o MESMO ticker-mês não são ruído —
indicam um provável bug de agregação por ticker (ou uma fonte de dado
diferente/desatualizada) em `fit_ativo_mes.csv`, um arquivo herdado da
Etapa 1 que o script 55 aceitou sem verificação ("HHI... direto do
`fit_ativo_mes.csv` já calculado", comentário da linha 16-17). Nenhum dos
scripts 55-59 comparou essa fonte contra a reconstrução que os próprios
agentes paralelos (60-69) fizeram do zero na MESMA rodada — cada agente
confiou cegamente na sua própria fonte, sem checagem cruzada entre agentes.

**Severidade e impacto prático**: não muda nenhuma conclusão relatada — o
sinal composto (scripts 56-59) já foi rejeitado de forma robusta
independentemente da qualidade exata de `s_hhi` (o próprio script 59 mostra
que 90%+ do resultado nominal do composto vem de `peer_ret`+`hhi`
combinados, e mesmo esse resultado morre fora de 2020). Mas é um problema de
integridade de dado real, não hipotético, que pode voltar a morder se
`fit_ativo_mes.csv` for reutilizado em outro contexto (por exemplo, se o
achado "HHI prediz volatilidade futura" — fora do escopo desta auditoria,
mas o achado mais citado do documento inteiro — também depender dessa
mesma fonte, vale conferir separadamente se usa `fit_ativo_mes.csv` ou uma
reconstrução independente).

**Solução concreta**: (a) descartar `fit_ativo_mes.csv` como fonte de
`hhi_posse` nos scripts do agente composto e recalcular com a mesma fórmula
usada (e já validada) nos scripts 63/65/67/68/69, para manter as duas
famílias de sinal consistentes entre si; (b) investigar a causa raiz da
divergência em `fit_ativo_mes.csv` (provável bug no agrupamento por
`ativo`/`ticker` da Etapa 1 — por exemplo, dois "ativo" com sufixos
distintos colapsando/não-colapsando no mesmo `ticker` de forma diferente
entre os dois pipelines) antes de usar esse arquivo em qualquer outro
candidato futuro.

---

## Achado 3 — COSMÉTICO: normalização assimétrica ao estimar pesos do composto (treino sem `by=ym`, teste com `by=ym`)

**Onde**: `57_composto_regressao_multipla_fm.R:142` vs `:149`, e `:156` vs `:160`.

```r
# treino (linha 142) -- SEM by=ym: z-score calculado pooled ao longo de todos os meses de treino
for (v in vars8) mtr[, (paste0(v,"_z")) := as.numeric(scale(get(v)))]
...
# teste (linha 149) -- COM by=ym: z-score calculado dentro de cada mês
for (v in vars8) mte[, (paste0(v,"_z")) := as.numeric(scale(get(v))), by = ym]
```

A regressão que estima os PESOS do composto (`fit_tr`) usa z-scores
calculados de forma pooled sobre todo o período de treino, enquanto a
aplicação desses pesos no teste usa z-scores calculados mês a mês
(cross-seccional, a convenção correta para comparação entre ativos no
mesmo mês). Essa assimetria pode distorcer levemente os pesos relativos
estimados entre os 8 sinais (sinais com volatilidade temporal diferente ao
longo do treino recebem pesos levemente diferentes do que receberiam com
z-score mensal). O mesmo padrão se repete no bloco TOP3 (linhas 155-156 vs
160).

**Severidade e impacto prático**: baixo. O teste de significância em si
(Fama-MacBeth com recorte de quintil mês a mês no `score_pesos_treino`) usa
corretamente o z-score mensal do teste — só os PESOS usados para combinar
os 8 sinais em um score único é que vêm de uma estimação levemente
inconsistente. Como o resultado (script 57, scores compostos com pesos do
treino) já foi rejeitado com folga (p=0,34-0,83 em h=1 e h=3; o único caso
"quase" — h=6, p=0,007 — foi diagnosticado a fundo no script 59 e também
rejeitado), essa inconsistência não muda nenhuma conclusão.

**Solução concreta**: adicionar `by = ym` também na linha 142 e na linha
156, para manter a normalização consistente entre a fase de estimação dos
pesos e a fase de aplicação.

---

## Achado 4 — COSMÉTICO: bug silencioso no diagnóstico (6) do script 69 (`n_fundos` inexistente)

**Onde**: `69_diagnostico_efit_x_hhi.R:160-167`.

```r
mt <- merge(alto_t, valor_tot, by = c("ticker","ym"))
cat("Correlacao HHI x log(valor_total) dentro do tercil ALTO-HHI:", ...)   # roda OK
cat("N medio de fundos no tercil ALTO-HHI:", round(mean(m[tercil_hhi==3]$n_fundos, na.rm=TRUE),1), ...)
```

`base` (linha 79 do mesmo script) é construído como
`merge(sinal_efit, crowd[, .(ticker, ym, hhi_posse)], by = c("ticker","ym"))`
— só a coluna `hhi_posse` de `crowd` é levada adiante, `n_fundos` fica de
fora. Como `m` deriva de `base`, a coluna `m$n_fundos` não existe. A
chamada `mean(m[tercil_hhi==3]$n_fundos, na.rm=TRUE)` retorna `NULL` no
lugar de um vetor, e `mean(NULL)` devolve `NA` com o aviso "argumento não é
numérico nem lógico" — silenciosamente, sem interromper o script. Reproduzi
isso rodando o script diretamente:

```
Correlacao HHI x log(valor_total) dentro do tercil ALTO-HHI: -0.364
N medio de fundos no tercil ALTO-HHI: NA | tercil BAIXO-HHI: NA
Mensagens de aviso: ... argumento não é numérico nem lógico: retornando NA
```

**Severidade e impacto prático**: nenhum sobre as conclusões reportadas — o
número quebrado ("N médio de fundos por tercil") nunca é citado no
`LOG_CANDIDATOS.md`; só a correlação HHI×log(valor_total)=-0,364 (que
calculou corretamente) é implicitamente usada para embasar a leitura de que
o tercil de alto HHI não é redutível a um proxy trivial de tamanho.
Classificado como cosmético porque não afeta nenhum resultado citado, mas
revela que essa parte específica do diagnóstico nunca rodou de verdade e
passou despercebida.

**Solução concreta**: incluir `n_fundos` na seleção de colunas do merge que
constrói `base` (linha 79), ou reconstruir `n_fundos` separadamente antes
da parte (6).

---

## Achado 5 — COSMÉTICO: duas construções diferentes de "reversão de curto prazo" coexistem sob o mesmo nome

**Onde**: `55_construir_sinais_compostos.R:44-45` (`reversao := -shift(retorno,1)`,
fiel ao candidato #24 original em `24_anomalias_classicas.R:36`) vs.
`65_regime_vol_x_sinais_retorno.R:120` / `66_diagnostico_reversao_h6.R:55`
(`valor_sinal = -retorno`, SEM defasagem — retorno do próprio mês).

A família "composto" (55-59) usa a reversão com 1 mês de defasagem
adicional (fiel à definição original do candidato #24). A família "regime"
(65-66), no candidato R5, usa uma versão sem essa defasagem — o que o
próprio script 66 já identifica e documenta explicitamente como sendo
"retorno do PRÓPRIO mês" e não reversão clássica, investigando e rejeitando
o resultado por isso mesmo (janela de formação de 1 mês é atípica e o
"sinal" se revelou artefato/momentum de curtíssimo prazo, não reversão).

**Severidade**: nenhuma — já foi corretamente identificado, investigado e
rejeitado pelo próprio agente que o construiu (script 66, ver achado
consolidado no log). Registro aqui só por completude terminológica: se
"reversão" for citada em algum resumo futuro do TCC, há risco de alguém
confundir as duas construções por terem o mesmo nome de variável.

---

## Achado 6 — COSMÉTICO: pequena divergência numérica entre o CSV arquivado do script 65 e a reexecução direta do script 66 para a MESMA especificação

**Onde**: `candidatos_65_regime_vol_x_sinais.csv`, linha
`Reversao curto prazo|ALTO-HHI(mediana),6` reporta `n_min=21, t=-4,285,
p=0,000445`. Reexecutando `66_diagnostico_reversao_h6.R` diretamente (que
recalcula, com o MESMO código, a mesma especificação "ALTO-HHI(mediana)
h=6"), obtive `n_min=22, t=-4,40, p=0,00034` — os números citados no
`LOG_CANDIDATOS.md` (achado R5: "t=-4,40, p=0,00034") batem com a
reexecução do script 66, não com o CSV arquivado do script 65.

Reconstruí as duas cadeias de código lado a lado num script isolado usando
os arquivos de dado ATUAIS (`painel_multiativo_final.csv`,
`precos_mensais_final.csv`) e confirmei que, com os dados de hoje, as duas
construções ("`m` do script 65" e "`m` do script 66") produzem exatamente
a mesma tabela intermediária (2.314 linhas idênticas, mesmo N por mês,
`identical()==TRUE`). Ou seja, o código dos dois scripts é equivalente — a
divergência não é um bug de lógica, é sinal de que o CSV arquivado
(`candidatos_65_regime_vol_x_sinais.csv`) foi gerado numa execução anterior
contra uma versão ligeiramente diferente dos arquivos de dado-fonte do que
a que está no disco agora (o pipeline de dado provavelmente foi re-rodado
em algum momento entre as duas execuções).

**Severidade e impacto prático**: nulo sobre qualquer conclusão — ambos os
números (arquivado e reexecutado) levam à mesma decisão (não significativo
vs. Bonferroni; achado R5 rejeitado como artefato de qualquer forma). Mas é
um sinal de alerta de higiene de reprodutibilidade: os CSVs em `data/` não
são necessariamente regeráveis byte-a-byte sem rodar a cadeia completa do
pipeline de novo a partir dos mesmos dados-fonte no mesmo estado. Vale
como lembrete geral, não like um problema deste candidato específico.

**Solução concreta**: nenhuma ação corretiva necessária no candidato em si;
se o TCC algum dia depender de reproduzir exatamente esses números, rodar
a cadeia completa (`v2 OFICIAL/PIPELINE.md`, Etapa 1 até o fim) antes de
regravar os CSVs de `exploracao_sinais/data/`.

---

## Verificações que CONFIRMARAM a qualidade do trabalho (não são achados de problema — registradas porque a auditoria pediu ceticismo, e ceticismo cortando nos dois sentidos exige documentar o que passou no teste)

### A. Reconstrução da matriz CIO (script 60) é fiel ao script original (11)

Comparei linha a linha `60_centralidade_construcao.R:39-67` contra
`11_cio_peer_momentum.R:35-64`: mesmo filtro (`aum_prev>0 & peso>0`), mesma
matriz `X = peso*aum_prev` em formato fundo×ticker, mesmo `O =
crossprod(X)`, mesma normalização cosseno `CIO = O/sqrt(outer(diag(O),
diag(O)))`, mesmo tratamento de diagonal e de valores não-finitos, mesmo
piso `uniqueN(ticker)>=15 & uniqueN(cod_fundo)>=15`. A única diferença é
que o script 60 não computa `peer_ret` (não precisa — o ângulo é
centralidade, não retorno dos vizinhos), o que é o comportamento esperado
e documentado. **Reconstrução correta, confirmada por leitura direta do
código-fonte** (não apenas pela nota algébrica do próprio script).

### B. Diagnóstico de confound de tamanho (script 64) está correto — reproduzido de forma independente

A instrução da auditoria pediu para não aceitar de cara o diagnóstico "é
proxy de tamanho, não efeito de rede" e checar se o teste de controle foi
uma Fama-MacBeth com os dois regressores simultâneos. O script 64 usa uma
técnica de residualização (`eigen_cent_neutro := residuals(lm(eigen_cent ~
log_tamanho)), by=ym`, seguida do MESMO teste de spread por quintil) — que
é matematicamente equivalente (Frisch-Waugh-Lovell) a um coeficiente
parcial de regressão múltipla, mas não é literalmente uma regressão
conjunta com dois regressores. Escrevi, de forma independente, uma
Fama-MacBeth genuinamente conjunta (`retorno ~ eigen_cent_z + log_tamanho_z`,
uma regressão por mês, erro-padrão da série de coeficientes) e confirmei:

```
h=1:  coef eigen_cent  t=-0.16  p=0.87   |  h=6:  t=-0.46  p=0.65
h=3:  coef eigen_cent  t=-0.06  p=0.95   |  h=12: t=-1.61  p=0.13
```

Em nenhum horizonte o coeficiente PRÓPRIO de `eigen_cent` (controlando
simultaneamente por tamanho) chega perto de significância — mesma
conclusão do script 64. Também confirmei numericamente as correlações
citadas (`deg_avg`×`log_tamanho`=0,667; `eigen_cent`×`log_tamanho`=0,728) e
o resultado de excluir 2020 (t cai para -0,18/-0,29). **O diagnóstico está
correto — não é um teste enviesado "forçando" a conclusão de confound; uma
regressão conjunta de verdade, escrita do zero, chega à mesma conclusão.**

### C. Nenhum `t.test()` pooled, nenhum break fixo de treino aplicado ao teste

Grep sistemático nos 15 scripts não encontrou nenhuma chamada a `t.test()`.
Toda inferência estatística usa a fórmula Fama-MacBeth correta (média da
série temporal de spreads/coeficientes mensais dividida pelo erro-padrão da
própria série, `t = média/(dp/sqrt(n_meses))`). Todo corte de quintil/decil
usa `quantile(...)` dentro de um `by=ym`, recalculado a cada mês do
período de TESTE — nunca um breakpoint fixo do treino. N mínimo por
grupo-mês é reportado em toda tabela de resultado. A checagem (C) do script
64 (rank percentual vs. nível bruto produzindo resultados idênticos) só é
possível se o corte de quantil for de fato recalculado a cada mês — outra
confirmação indireta de que o método está implementado corretamente.

### D. Todos os números citados no log batem com CSVs/reexecução

Conferi numericamente (rodando os scripts ou lendo os CSVs) TODOS os
valores de t/p/spread/n_min citados na seção "Rodada de agentes paralelos"
do log para os candidatos #55-69: scripts 56, 57 (coeficientes e scores),
58, 59 (reexecutado — 5/5 checagens batem, incluindo a composição das
pernas em R$), 61, 62, 63, 64A/C/D (reexecutado — inclusive as correlações
0,667/0,728), 65, 66 (reexecutado — 7/7 partes batem), 67 (reexecutado —
correlações 0,357→0,119 e diagnóstico do crash batem), 68, 69 (reexecutado
— todas as 7 partes batem, exceto o bug cosmético do achado 4). Única
exceção: achado 6 acima (divergência CSV-arquivado vs. reexecução, sem
efeito na conclusão).

---

## Resumo (≤200 palavras)

Auditoria de segunda camada sobre os scripts 55-69 não encontrou nenhum
problema que reverta uma conclusão já publicada no log — a disciplina
Fama-MacBeth/recorte mensal/N-mínimo foi seguida corretamente em todos os
15 scripts, sem nenhum `t.test()` pooled ou break fixo do treino, e todo
número citado no log bateu com reexecução direta. O diagnóstico de que a
centralidade de rede é confound de tamanho (não efeito genuíno) foi
verificado de forma independente com uma regressão Fama-MacBeth conjunta
escrita do zero — confirmado, correto. A reconstrução da matriz CIO no
script 60 é fiel byte-a-byte à lógica original do script 11.

Dois problemas reais, ambos de severidade IMPORTANTE mas sem impacto sobre
nenhuma conclusão relatada (todos os candidatos afetados já eram nulos por
outras vias): (1) a winsorização de 4 sinais (breadth, demanda agregada,
choque de posse bruto/corrigido) usa quantis calculados na amostra
inteira treino+teste, não só treino — viola a disciplina "sem look-ahead"
que o próprio código reivindica; (2) o `hhi_posse` pré-calculado que o
script 55 importa de `fit_ativo_mes.csv` diverge em centenas de fundos, em
dezenas de ticker-meses, da reconstrução feita do zero (e validada) nos
scripts 63/65/67/68/69 — provável bug de agregação por ticker num arquivo
herdado que nunca foi checado contra a fonte primária. Três achados
cosméticos adicionais (bug silencioso de NA no script 69, normalização
assimétrica no script 57, e nomenclatura duplicada de "reversão") completam
a lista.
