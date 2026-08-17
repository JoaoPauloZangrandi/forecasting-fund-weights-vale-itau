# Auditoria adversarial: pipeline de λ e cadeia "pares replicantes" (Seção 6)

Auditoria dos scripts `61` a `72` da pasta `v2 OFICIAL/scripts`, feita rodando
o código de verdade (não só lendo) sempre que uma alegação era verificável
numericamente. R usado: `C:\Program Files\R\R-4.5.1\bin\Rscript.exe`. Todos os
números abaixo foram reproduzidos a partir dos dados atuais do repositório
(pós corte `soma_peso>150%` de 12/08/2026) nesta sessão (15/08/2026).

Not a bene: não reaudito as 6 armadilhas já catalogadas em `PIPELINE.md`
("Armadilhas conhecidas") — parto delas como dadas. Também não reabro a
Etapa 1 (scripts 99/101, fora do escopo pedido); assumo que `peso_pred` e `d`
são gerados com informação disponível em `t` (regressão cross-section por
mês), como aliás é consistente com o resto do pipeline usar isso sem
ressalva.

---

## CRÍTICO 1 — O "sinal do robô" usa informação do líder que só existe em `t+1`, não em `t` (look-ahead estrutural, não só de código)

**Onde:** `v2 OFICIAL/scripts/66_beta_pares_lideranca.R:40-51` e
`v2 OFICIAL/scripts/67_previsao_ajustada_seguidor.R:39-53`; e no próprio
`TCC_finalV2.tex`, equação `eq:sinal` (linhas ~845-849) e `eq:fluxo-retorno`
(linhas ~888-892).

**O problema, com precisão:** Em `ajuste_parcial_universo_completo_h1.csv`
(gerado pelo script 61), cada linha tem `ym` = mês de origem `t`, e as
colunas `dw`, `dw_corrigido` e (depois) `u = dw_corrigido - lam*d` são
**mudanças realizadas entre `t` e `t+1`** — por construção, `dw_corrigido`
usa `peso_fut` (peso em `t+1`) e `r_ativo`/`retorno_fundo` (retornos de `t`
para `t+1`). Ou seja: `u` na linha `ym=t` só é computável depois que `t+1`
aconteceu, mesmo estando rotulado com `ym=t`. Confirmado lendo o CSV
(`61`, linha 80): a linha com `ym=201702` tem `ym_fut=201703` e usa
`r_ativo` de fevereiro→março para gerar `dw_corrigido`.

O script `63` (linha 33, 75-80) sabe disso implicitamente: para decidir quem
é "líder" ele correlaciona `u` da linha `mes_idx=k` (intervalo `[t,t+1]`) com
`u` da linha `mes_idx=k+1` (intervalo `[t+1,t+2]`) — um lag genuíno de um
mês inteiro (`Wl <- W[1:(n-1),]` vs `Wf <- W[2:n,]`, script 63 linhas 75-76).
É essa correlação defasada que decide `corr_a_lidera`/`corr_b_lidera`
(linhas 92-95) e, por extensão, quem é "líder" em `65` (linhas 52-56).

Mas o script `66` (estimação do β) e o `67` (construção do "eco do líder")
**não replicam esse lag**. Eles casam `u_lider` e `u_seguidor`/`d_seguidor`
pela **mesma** coluna `ym`:
- `66`, linha 45: `merge(com_lider, u_seguidor, by = c("seguidor","ativo","ym"), ...)`
- `67`, linha 47: `merge(comb, u_lider, by.x = c("lider","ativo","ym"), by.y = c("cod_fundo","ativo","ym"), ...)`

Isso combina o `u` do líder no intervalo `[t,t+1]` com o `u`/`d` do seguidor
**no mesmíssimo intervalo `[t,t+1]`** — contemporâneo, não defasado. Não é
só uma inconsistência de implementação em relação ao critério de seleção do
`63`: o próprio `TCC_finalV2.tex` documenta a fórmula dessa forma
(`u_{líder,t}` junto com `d_{seguidor,t}` para prever `dw_ajustado`, sem
nenhum `t-1`), então o desenho **pretendido** já tem esse furo — a notação
"`t`" no `u` é enganosa porque `u_{i,n,t}` não é informação de `t`, é
informação de `t+1` batizada com o rótulo `t`.

Consequência prática: o "eco do líder" usado para prever `dw` do seguidor
usa, disfarçado, informação de `t+1` (o quanto o líder efetivamente mexeu
entre `t` e `t+1`) — que na vida real só fica pública quando a CVM divulga
as carteiras de `t+1`, com defasagem. Isso não é implementável como sinal
operável em tempo real (o problema que o "teste decisivo" da Seção
`sec:teste-decisivo` deveria estar testando). Pior: no script `70`, o
`fluxo_pct` (que carrega esse `u_lider` contaminado) é regredido contra
`retorno_fut`, que é **o retorno do mesmo ticker no mesmíssimo intervalo
`ym_fut = addm(ym,1)`** usado para construir `dw_corrigido` lá atrás em `61`
— ou seja, o "preditor" e o "resultado futuro" do teste decisivo às vezes
compartilham, via `r_ativo`, o mesmo dado de retorno realizado no
mesmíssimo mês (`precos_mensais_final.csv`, coluna `retorno`, mesmo
`ticker`/`ym_fut`). Medi a correlação direta entre `fluxo_pct` e
`retorno_fut`: é pequena (`-0,0119`), então a contaminação mecânica parece
não dominar o resultado na prática — mas o desenho do teste está errado em
princípio, não só na magnitude.

**Isso não muda a conclusão de "resultado nulo" para "resultado positivo"**
— pelo contrário: se mesmo com essa vantagem informacional ilegítima o
sinal não prevê retorno, a conclusão de nulidade fica mais, não menos,
confiável. Mas a Seção 6 do TCC não pode alegar (como faz, implicitamente,
ao chamar de "teste decisivo" e "premissa de negociar em cima do sinal")
que testou se o sinal é *operável*: ele nunca testou uma versão do sinal
que só usa informação de fato disponível em `t`.

**Solução concreta:** em `66`, linha 40-41, trocar a chave de junção do
`u_lider` para usar o **mês anterior** ao do seguidor:
```r
u_lider    <- M[, .(ativo, lider = cod_fundo, ym_lider_mais_1 = addm(ym,1L), u_lider = u)]
...
comb_ym <- merge(com_lider, u_seguidor,
                  by.x = c("seguidor","ativo","ym_lider_mais_1"),
                  by.y = c("seguidor","ativo","ym"), allow.cartesian = TRUE)
```
i.e., `u` do líder no intervalo `[t-1,t]` explicando `u`/`d` do seguidor no
intervalo `[t,t+1]` — consistente com o lag genuíno de um mês já usado em
`63` para definir quem é líder. Replicar a mesma correção em `67`
(linha 47) e então re-rodar `68`→`72` com a cadeia corrigida antes de
reportar qualquer número da Tabela `tab:sinal`/`tab:teste-decisivo` como
teste de um sinal "operável".

---

## CRÍTICO 2 — A correção de Benjamini-Hochberg do script 64 assume que o ranking de p-valor dentro do pré-filtro é o ranking verdadeiro entre os 31,9M testes — isso é falso, e afeta a maioria dos pares "significativos"

**Onde:** `v2 OFICIAL/scripts/64_significancia_pares_replicantes.R:35-48`.

**O argumento do comentário (linhas 38-42):** todo par excluído do
pré-filtro tem `|corr|<0,6`, e "correlação mais fraca com n parecido sempre
dá p-valor maior", logo o rank dos pares pré-filtrados dentro de si mesmos
já é o rank verdadeiro entre os `N_TESTES_TOTAL` testes.

**Por que isso é falso:** o p-valor do teste de correlação depende de `r`
**e** de `n` (`t = r*sqrt(n-2)/sqrt(1-r²)`, script 64 linha 30), e `n`
(`n_meses_contemp`) varia de 12 (o mínimo, `MIN_MESES`) até 58 (o span
inteiro dos dados, fev/2017–nov/2021 — confirmei isso: `uniqueN(ym)` no
`ajuste_parcial_universo_completo_h1.csv` é exatamente 58). Um par
**excluído** com `r=0,55` (abaixo do limiar) e `n=58` tem `p≈` muito menor
que um par **incluído** com `r=0,61` e `n=12`. O "n parecido" que o
comentário assume para justificar a monotonicidade **não é verdade** —
`n` varia por um fator de quase 5x entre os pares.

**Verifiquei isso numericamente**, não é hipotético: o menor p-valor que um
par **excluído** poderia ter, no pior caso (r logo abaixo de 0,6, no maior
`n` que existe em todo o dataset, `n=58`) é `p ≈ 6,43×10⁻⁷`. Contei quantos
dos 1.248.772 pares "significativos" (`pares_replicantes_significativos.csv`,
que bate com o número `1.248.772` citado no `TCC_finalV2.tex` linha 820) têm
`pval_contemp` **maior** que esse limite — ou seja, pares para os quais é
matematicamente possível (e, com 87M+ testes de fato realizados, virtualmente
certo) que exista algum par excluído mais significativo que eles, invalidando
a suposição de que seu rank observado é o rank verdadeiro:

```
N vulneráveis = 725.527 de 1.248.772 (58,1%)
```

Ou seja, a correção de múltiplas comparações não é válida para a maioria
absoluta dos pares que o TCC chama de "significativos" — o `p_bh` deles
está subestimado (correção fraca demais), não porque o código tenha um erro
de aritmética, mas porque a premissa do atalho (evitar recomputar os 87M+
testes) não se sustenta quando `n` varia tanto entre pares.

**Solução concreta:** duas opções, em ordem de rigor:
1. **Correta:** modificar `processa_ativo()` em `63` (linha 56-97) para
   também acumular, ANTES do filtro de correlação (linha 84), o vetor de
   `(n_overlap[ut], Cc[ut])` de **todos** os pares que passaram só o filtro
   de cobertura mínima (`n_overlap[ut] >= MIN_MESES`) — não precisa salvar
   `fundo_a`/`fundo_b`/identidade, só os dois números por par (seria ~87M
   linhas de 2 doubles ≈ 1,4GB, viável, bem menor que os 11GB que estourou
   o disco na v1 porque aquela tinha todas as colunas). Com isso, `64`
   calcula o p-valor de verdade para os 87M+ testes e faz BH exato.
2. **Pragmática, sem rerodar 63:** aplicar Bonferroni simples
   (`p_bonf = pmin(1, pval_contemp * N_TESTES_TOTAL)`) só para os pares cujo
   `pval_contemp < 6,43×10⁻⁷` (o limite conservador calculado acima) — esses
   têm rank garantido mesmo no pior caso. Os outros 725 mil pares precisam
   ser reportados como "não confirmados sob a correção atual" até a opção 1
   ser feita.

---

## CRÍTICO 3 — O denominador do BH (`N_TESTES_TOTAL = 31.907.281`, citado no TCC) não tem script que o produza, e o arquivo que o contém é anterior ao corte de 150% que invalidou o resto do pipeline

**Onde:** `v2 OFICIAL/scripts/64_significancia_pares_replicantes.R:27` (lê
`v2 OFICIAL/data/n_testes_total_fase4.csv`).

Rodei `grep -l "n_testes_total" "v2 OFICIAL/scripts"/*.R` — o único script
que **menciona** esse arquivo é o `64`, que só **lê**. Não existe, em
nenhum script de `v2 OFICIAL/scripts`, código que **gera**
`n_testes_total_fase4.csv`. É um número solto, calculado manualmente em
algum momento fora do pipeline reproduzível.

Pior: o timestamp do arquivo é **30/07/2026 23:44**, e
`pares_replicantes_bruto.csv` (saída do script `63`, que depende
diretamente do universo pós-corte 150%) tem timestamp **12/08/2026 23:49** —
ou seja, o `63` foi rerrodado depois da mudança de corte (como manda a
regra do próprio `PIPELINE.md`: "qualquer mudança no início da Etapa 1...
invalida tudo abaixo dela no grafo"), mas o `n_testes_total_fase4.csv` que
o `64` usa como denominador do BH **não foi**. O `TCC_finalV2.tex` (linha
808) cita esse `31.907.281` como se fosse autoritativo, sem que exista
hoje um caminho reprodutível para confirmar que esse número é o correto
para o universo pós-12/08.

Ainda mais confuso: o **comentário** do próprio script 64 (linhas 8 e 13)
diz que o número real de testes é `87.072.544` — um terceiro valor,
diferente tanto do que está no CSV (31,9M) quanto do que seria de se
esperar (o `63` atual produz 1.808.886 pares pré-filtrados, um número que
não bate obviamente com nenhum dos dois). Isso é o mesmo padrão do caso já
catalogado do script 100 (comentário desatualizado vs. código/dado real),
mas aqui é mais grave porque o número errado (potencialmente) não é só um
comentário — é usado de verdade no cálculo do `p_bh` que sustenta a Tabela
`tab:pares` do TCC.

**Solução concreta:** dentro do loop de `processa_ativo()` em `63`
(linha 56), antes do `return(NULL)` da linha 60 e do filtro de correlação
da linha 84-86, contar `nrow(ut)` (o número de pares que passaram só o
filtro de cobertura `n_overlap>=MIN_MESES`, antes do `|corr|>=0,6`) e
acumular essa contagem por ativo num vetor externo ao loop. No fim do
script, `fwrite()` esse total junto com `pares_replicantes_bruto.csv`
(ex.: `v2 OFICIAL/data/n_testes_total_fase4.csv`), **substituindo** o
arquivo órfão atual, para que ele nunca mais fique dessincronizado do
`63` que o alimenta.

---

## IMPORTANTE 4 — Teste decisivo (scripts 70/71) é *pooled*, não Fama-MacBeth; refazendo por mês, a conclusão de "nulo" muda para pelo menos duas das quatro especificações da Tabela `tab:teste-decisivo`

**Onde:** `v2 OFICIAL/scripts/70_teste_fluxo_prediz_retorno.R:50,54,69` e
`v2 OFICIAL/scripts/71_teste_fluxo_retorno_restrito.R:25,29,40`.

Confirmei que o teste, como está no código, trata cada observação
(ativo, mês) como independente em `lm()` e `t.test()` — sem cluster por
`ym`, sem Newey-West, sem Fama-MacBeth. Esse é exatamente o padrão de erro
já documentado em `exploracao_sinais/LOG_CANDIDATOS.md` para outros sinais
deste projeto (regressão pooled trata fundo-ativo-mês como observação
independente).

**Refiz os quatro testes reportados na Tabela `tab:teste-decisivo`
(linha ~910 do `.tex`) como Fama-MacBeth de verdade** (regressão/spread
por mês, depois `t.test()` nos 58 coeficientes/spreads mensais) e comparei
com o `p`-valor pooled que está hoje no `.tex`:

| Especificação | `p` pooled (como está no TCC) | `p` Fama-MacBeth (recalculado) |
|---|---|---|
| Sinal COM eco, regressão, completa | 0,277 | 0,494 (continua nulo) |
| Sinal COM eco, regressão, restrita | 0,737 | 0,688 (continua nulo) |
| Sinal COM eco, quintil Q5-Q1, completa | 0,301 | **0,050** (limítrofe) |
| Sinal COM eco, quintil Q5-Q1, restrita | 0,116 | **0,022** (significativo a 5%) |
| Sinal SEM eco, regressão, completa | **0,0002** | **0,029** (ainda significativo, mas ~150x mais fraco) |
| Sinal SEM eco, regressão, restrita | **0,027** | **0,145** (deixa de ser significativo) |

(Reproduzi os `p`-valores pooled exatamente como no `.tex` antes de
recalcular — bate com a Figura 4 do TCC, "+0,50 p.p. (p=0,301) na
completa, +0,87 p.p. (p=0,116) na restrita" — então a base usada está
correta, a diferença é só o método de agregação.)

Duas coisas para o TCC daqui:
1. A alegação de que o sinal "sem eco" é "estatisticamente detectável nas
   duas amostras (p=0,0002 completa, p=0,027 restrita)" (texto após a
   Tabela `tab:teste-decisivo`) **não sobrevive** no Fama-MacBeth para a
   amostra restrita (`p=0,145`) e fica muito mais fraca na completa. Isso
   não muda a conclusão final (o `R²` já era "irrisório" mesmo pooled), mas
   a frase "estatisticamente detectável" precisa ser qualificada ou
   removida — é um artefato do método de teste, não um achado robusto.
2. O spread Q5-Q1 do sinal "com eco" (o produto final da seção, o que
   importaria para uma estratégia) vira limítrofe/significativo no
   Fama-MacBeth, **na direção "certa"** (Q5 > Q1). Isso é o oposto do
   coeficiente de regressão do mesmo sinal, que continua negativo e nulo —
   uma inconsistência de sinal entre a versão linear e a versão em
   quintis que sugere que esse resultado marginal é frágil/não-linear
   (dirigido por poucos meses ou outliers), não um sinal real perdido. Não
   recomendo reescrever a conclusão do TCC em cima disso, mas recomendo
   **não usar mais o teste pooled** como evidência de "nulo" sem qualificar
   — o método atual tanto infla quanto (nesse caso específico) desinfla
   significância dependendo da especificação, o que por si só mostra que
   ele não é a ferramenta certa.

**Solução concreta:** substituir, em `70` e `71`, os `lm()`/`t.test()`
diretos por uma função `fama_macbeth(dt, formula)` que roda a regressão
(ou o spread Q5-Q1) mês a mês, guarda os 58 coeficientes, e faz
`t.test()` neles — é literalmente o código que usei para gerar a tabela
acima, pode ser colado quase direto. Reportar os `p`-valores
Fama-MacBeth na Tabela `tab:teste-decisivo` (ou lado a lado com o pooled,
sendo explícito sobre qual é o teste "de verdade").

---

## IMPORTANTE 5 — Viés de seleção: pares só entram na análise de "liderança" se tiverem correlação CONTEMPORÂNEA alta; correlação defasada nunca é usada como critério de inclusão

**Onde:** `v2 OFICIAL/scripts/63_pares_replicantes.R:84` —
`keep <- n_overlap[ut] >= MIN_MESES & abs(Cc[ut]) >= LIMIAR_CORR` — o
filtro usa **só** `Cc` (correlação contemporânea, linha 73). `Cl`
(correlação defasada, linha 80) é calculada mas nunca entra no `keep`; ela
só é usada **depois**, como característica descritiva dos pares que já
sobreviveram ao filtro de `Cc`.

**Consequência:** um par onde o fundo B genuinamente copia o fundo A com
defasagem de um mês (correlação defasada alta, `|Cl|` grande) mas cujos
erros **não** coincidem no mesmo mês (correlação contemporânea baixa,
`|Cc|<0,6` — plausível se a defasagem de replicação for real, já que nesse
caso o movimento de A no mês `k` não deveria coincidir com o de B no mesmo
mês `k`, e sim no `k+1`) é **descartado antes de qualquer análise de
liderança**. O "robô caça-replicantes", do jeito que está construído,
detecta majoritariamente comovimento simultâneo (herding/fatores comuns),
e só secundariamente caracteriza direção dentro desse subconjunto já
filtrado — não uma busca imparcial por relações líder-seguidor genuínas.

Isso não infla o resultado (se algo, empobrece — descarta candidatos a
replicante genuíno antes de testá-los), então não compromete a validade do
que foi encontrado, mas compromete a **completude**: a Tabela `tab:pares`
e a frase "existem pares de fundos... cujo comportamento se move em
conjunto" (síntese final do TCC, linha ~970) descreve corretamente o que
foi medido, mas "robô caça-replicantes" como nome sugere mais do que o
filtro de fato captura.

**Solução concreta:** ou (a) trocar o critério de inclusão na linha 84 para
`abs(Cc[ut]) >= LIMIAR_CORR | pmax(abs(Cl[ut]), abs(Cl[ut[,2:1,drop=FALSE]])) >= LIMIAR_CORR`
(mantendo o mesmo limiar, mas também aceitando pares fortes só na versão
defasada), ciente de que isso ~dobra o espaço de testes e portanto o `m` do
BH (reforça a importância do CRÍTICO 3 acima); ou (b), mais barato, deixar
como está mas adicionar uma frase explícita no TCC (Seção `tab:pares`)
avisando que a seleção é sobre correlação contemporânea, e que liderança é
uma caracterização *a posteriori* dentro desse subconjunto, não um
critério de busca.

---

## IMPORTANTE 6 — A cadeia de pares (63/66/67 e tudo a jusante) usa o λ de amostra INTEIRA (Fase 2, script 61), não o λ fora-da-amostra (Fase 3, script 62) que o próprio projeto trata como o correto para a Seção 4/5

**Onde:** `v2 OFICIAL/scripts/63_pares_replicantes.R:30`,
`66_beta_pares_lideranca.R:23`, `67_previsao_ajustada_seguidor.R:35` — todos
usam `lam <- 0.0690`, que é o `lambda COM correcao mecanica` estimado em
**toda** a amostra (fev/2017–nov/2021) pelo script 61 (linha 68-70, sem
corte treino/teste — o próprio cabeçalho do 61 diz isso, linha 4-5).

Comparando com o script 62 (Etapa 3, que faz treino `<2020-01` / teste
`>=2020-01` corretamente): rodei os dois hoje e o λ fora-da-amostra
(h=1, corrigido, treinado só até dez/2019) é **0,0715**, não 0,0690. A
diferença é pequena (~3,5%) mas conceitualmente importa: qualquer `u`
calculado nos scripts 63 em diante usa um λ que "sabe" sobre dados até
nov/2021 mesmo para resíduos de fev/2017 — um look-ahead na escala do
parâmetro, análogo (em espírito, se não em magnitude) ao problema maior do
CRÍTICO 1. É inconsistente que a Seção 4/5 (Etapa 3, script 62) tenha
disciplina OOS rigorosa e a Seção 6 (pares) não, especialmente porque a
Seção 6 é justamente a que tenta virar sinal de estratégia.

**Solução concreta:** recalcular `u` na cadeia 63→72 usando o λ
fora-da-amostra do script 62 (aplicar o λ treinado em `<2020-01` a
**todos** os meses, do jeito que uma estratégia real teria que fazer —
nunca reestimar com dados futuros), em vez do λ de amostra inteira do
script 61. Isso é uma mudança de uma linha em cada um dos três scripts
(`lam <- 0.0715` viria de um `fread()` do resultado do 62 em vez de
hardcoded) — mas dado o CRÍTICO 6 (λ hardcoded, já catalogado no
`PIPELINE.md`), a solução de fundo é a mesma recomendada lá: os scripts
63/66/67 deveriam ler λ de um CSV gerado pelo 62 (ex.:
`etapa3_universo_completo_h1.csv$lambda_corrigido[1]`), não hardcoded.

---

## COSMÉTICO 7 — Comentário do script 63 cita λ=0,0665; o código usa 0,0690; nenhum dos dois bate com o "era 0,0677" do comentário vizinho

**Onde:** `v2 OFICIAL/scripts/63_pares_replicantes.R:7` (cabeçalho, prosa)
vs. linha 30 (código real).

Linha 7: *"...correlacionando a série de erro (u, ajuste parcial COM
correção mecânica, **lambda amostra inteira = 0,0665**, Fase 2) mês a
mês."* Linha 30: `lam <- 0.0690  # atualizado 12/08/2026 apos corte
soma_peso subir de 105% p/ 150% (era 0.0677)`.

Confirmei rodando o script 61 hoje: o valor atual que o script 61
realmente produz é **0,0690** (bate com o código, linha 30) — então o
código está certo e é o comentário do cabeçalho (0,0665) que está errado/
desatualizado, provavelmente um resquício de uma rodada anterior ao ajuste
de 12/08 ou um erro de digitação. Mesmo padrão do caso já catalogado do
script 100 (comentário não bate com código), mas aqui o comentário errado
está na prosa descritiva, não afeta cálculo nenhum — só confunde quem lê o
script pensando que 0,0665 é o valor usado.

**Solução concreta:** trocar `0,0665` por `0,0690` na linha 7 de `63`, ou
— melhor, para não precisar lembrar de atualizar de novo — trocar a frase
para "lambda amostra inteira, ver `lam <-` abaixo, Fase 2" sem cravar o
número na prosa.

---

## COSMÉTICO 8 — Comentário do script 64 cita 87.072.544 testes; o CSV que o script realmente lê hoje tem 31.907.281

**Onde:** `v2 OFICIAL/scripts/64_significancia_pares_replicantes.R:8,13`
(comentário) vs. `v2 OFICIAL/data/n_testes_total_fase4.csv` (conteúdo
atual, lido dinamicamente na linha 27).

Já reportado como parte do CRÍTICO 3 acima (o problema de fundo é que o
CSV não tem script que o gere e pode estar desatualizado); listo aqui
separadamente porque, **mesmo depois** de corrigir o CRÍTICO 3, o
comentário nas linhas 8/13 do script vai continuar desatualizado em
relação ao valor real usado — é puramente descritivo (não afeta o
cálculo, que já lê o CSV dinamicamente), mas confunde.

**Solução concreta:** trocar o número fixo "87.072.544" no comentário por
uma referência dinâmica (ex.: rodar o script uma vez e colar o valor
`cat()`ado, ou simplesmente remover o número específico da prosa e deixar
só "ver `N_TESTES_TOTAL` abaixo, lido de `n_testes_total_fase4.csv`").

---

## O que foi checado e NÃO apresentou problema

- **`addm()`**: a mesma função aparece idêntica em `61`, `62` e `70`
  (`(ym%/%100)*12 + (ym%%100-1) + k`, depois reconvertida). Testei
  manualmente a virada de ano (`addm(202312,1) = 202401`) e bate. Não achei
  nenhum erro de aritmética de data/off-by-one em nenhuma das ocorrências.
- **λ hardcoded em 63/66/67 bate com o que o script 61 produz hoje**:
  rodei o `61` do zero e o `lambda COM correcao mecanica` na amostra comum
  saiu `0,0690`, exatamente o valor hardcoded nos três scripts (essa é a
  armadilha conhecida #3 do `PIPELINE.md` — hoje ela está OK, mas só porque
  ninguém mudou a Etapa 1 desde 12/08; a armadilha continua existindo e
  vale a ressalva do IMPORTANTE 6 acima sobre qual λ deveria ser usado).
- **AUM em `68`**: usa `aum_prev` (mesmo dado predeterminado da Etapa 1),
  não AUM futuro — comentário do próprio script (linhas 8-10) está correto
  e o código confirma.
- **Normalização do fluxo em `70`/`71`** por valor total da posição (não
  R$ bruto) evita o viés óbvio de "ativo grande = fluxo grande" — desenho
  correto.
- **Filtro de posição-poeira** (peso mediano da célula fundo-ativo ≥0,1%,
  script 63 linha 36-42) é uma decisão de limpeza bem documentada e
  motivada por um artefato real já diagnosticado (correlação espúria de
  1,0 em posições ~0); não introduz viés na direção de "fabricar" replicação
  — se algo, é conservador.
- **Remoção de outliers `|beta|>10`** em `66` (linha 57-60) é simétrica
  (não filtra por sinal nem por significância) — não é p-hacking.
- **Fórmula do teste t de correlação** em `64` (linha 30-31) é a fórmula
  padrão correta.

---

## Resumo (≤200 palavras)

O achado mais grave é estrutural, não um bug isolado: o "eco do líder"
(scripts 66/67, e a própria equação `eq:sinal` do TCC) combina o resíduo
`u` do líder e o `d`/`dw` do seguidor no MESMO mês `ym`, quando `u`
representa uma mudança só observável em `t+1` — ou seja, o sinal testado em
`70`/`71` nunca foi genuinamente "informação de `t` prevendo retorno de
`t+1`", é parcialmente contemporâneo com o próprio retorno que tenta
prever. Isso não inverte a conclusão de "nulo" (se algo, reforça — nem com
essa vantagem informacional o sinal funciona), mas invalida a alegação de
ter testado um sinal operável. Segundo achado grave: a correção
Benjamini-Hochberg (script 64) assume implicitamente que `n` (meses de
sobreposição) é parecido entre pares — falso, `n` varia de 12 a 58 — e
verifiquei que isso invalida a premissa de ranking para 58% dos 1.248.772
pares "significativos". Terceiro: o denominador dessa correção
(31.907.281, citado no TCC) não tem script que o produza e é anterior ao
último corte de qualidade de dado. Por fim, refazendo o teste decisivo como
Fama-MacBeth (não pooled) por mês, duas das quatro células da Tabela
`tab:teste-decisivo` mudam de significativo para nulo (ou vice-versa) —
confirma o padrão de erro já visto noutros sinais deste projeto. Nenhum
desses achados muda a conclusão final do TCC ("nulo, sem poder
explicativo"), mas nenhum deles pode ser chamado de "testado até o fim"
sem as correções listadas.
