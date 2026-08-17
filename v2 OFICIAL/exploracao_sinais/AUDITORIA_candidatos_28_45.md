# Auditoria adversarial — candidatos #28-45 (previsão de volatilidade, HHI×outros, CIO revisitado)

Auditoria de `v2 OFICIAL/exploracao_sinais/scripts/28_*.R` a `45_*.R`, **exceto**
`32_fragilidade_managed_portfolio.R` (já auditado separadamente, achados em
`PLANO_EXPANSAO_2021_2026.md`). Não altera nenhum código — só reporta. Todos os
achados abaixo foram verificados rodando os scripts (ou variantes diagnósticas
deles) em R, não apenas por leitura do código.

Foco extra, por pedido explícito: candidatos #28-31 (HHI de posse → volatilidade
futura, h=12), por serem a base da estratégia do Desafio Quant AI.

---

## Achados

### 1. [IMPORTANTE] Script 33 usa a metodologia de quintil já comprovada como defeituosa (breaks fixos do treino) — o número citado no log (p=0,008) evapora com a metodologia corrigida

**Onde:** `33_52wh_x_hhi_double_sort.R`, função `fama_macbeth` linha 45:
```r
breaks <- unique(quantile(treino[[var_x]], seq(0,1,0.2), na.rm = TRUE))
```
Os breaks de quintil são calculados **uma única vez no treino** e aplicados a
todo o período de teste de uma vez (`teste[, quintil := as.integer(cut(...))]`,
sem `by=ym`) — exatamente o "Método A" que os scripts `36_reconciliacao_metodologia_cio.R`
e `37_cio_corrigido_validacao_completa.R` (rodados **depois** do script 33, no
mesmo dia) identificaram como gerador de quintis degenerados para o CIO Peer
Momentum (até 1 ação numa perna nalguns meses). Essa correção — "recorte de
quintil A CADA MÊS" — nunca foi reaplicada ao script 33.

**Verificação empírica (não estava no log):** reproduzi a lógica do script 33
com instrumentação extra reportando o N mínimo por célula (ym, quintil).
Resultado: em TODAS as 6 especificações por tercil (ALTO/BAIXO-HHI × h=1/3/6),
o N mínimo por célula-mês é **2** (às vezes uma perna com 2 ações contra 60+ na
outra, no mesmo mês). Isso é degenerescência real, não hipotética.

Refazendo a mesma pergunta com a metodologia corrigida (recorte de quintil de
`prox_52w_high` A CADA MÊS, dentro de cada tercil de HHI — "Método B"), o
resultado citado no `LOG_CANDIDATOS.md` como "efeito mais forte no terço de
BAIXO HHI, h=1, p=0,008" **desaparece por completo**:

| Especificação | Método A (log atual) | Método B (corrigido) |
|---|---|---|
| BAIXO-HHI, h=1 | t=2,89, p=0,0085 | t=0,58, **p=0,569** |
| ALTO-HHI, h=1 | t=0,74, p=0,468 | t=-0,26, p=0,795 |
| ALTO-HHI, h=6 | t=2,01, p=0,060 | t=2,48, **p=0,032** (inverte quem é mais forte) |

Ou seja: o único número desse script citado como "quase interessante" no log
(p=0,008) é um artefato do método de corte, não um efeito real — e a
conclusão qualitativa também muda parcialmente (com o método corrigido, é o
subgrupo ALTO-HHI em h=6, não BAIXO-HHI em h=1, que fica mais perto de
significativo, ainda sem bater Bonferroni).

**Isso não muda a conclusão final do candidato #33** ("nenhuma versão bate
Bonferroni" continua verdadeiro nos dois métodos) — mas o número específico
citado no log como o resultado mais próximo de interessante está errado e
não deveria ser usado nem como nota lateral.

**Solução concreta:** no `LOG_CANDIDATOS.md`, seção do candidato #33, substituir
a frase "o efeito é mais forte no terço de BAIXO HHI (h=1, p=0,008)" por uma
nota indicando que esse número foi obtido com a metodologia de corte fixo
(sujeita a quintis degenerados) e não sobrevive à correção. Se o candidato #33
for citado em qualquer lugar do relatório do desafio, usar os números do
Método B, não os do script 33 como está.

---

### 2. [IMPORTANTE] Script 28 usa a mesma metodologia de corte fixo para TODOS os 7 sinais — o sinal principal (Crowding/HHI) está confirmado robusto, mas os sinais secundários (CIO, FIT) têm células degeneradas que tornam seus p-valores "quase significativos" não confiáveis

**Onde:** `28_previsao_volatilidade.R`, função `testa_sinal_vol`, linha 98:
```r
breaks <- unique(quantile(treino$x, seq(0,1,0.2), na.rm=TRUE))
```
Mesmo padrão do achado #1: breaks calculados uma vez no treino, aplicados a
todo o teste sem recorte mensal. A função não reporta o N mínimo por célula em
nenhum lugar (só `n_meses`, contagem de meses com dado, não o tamanho de cada
grupo) — por isso essa degenerescência nunca apareceu no log.

**Verificação empírica, sinal por sinal (achado central: a notícia é boa para
Crowding/HHI):**

| Sinal | h | N mínimo por célula-mês | Situação |
|---|---|---|---|
| **Crowding(HHI)** | 6 | **29** (mediana 47) | Saudável — sem degenerescência |
| **Crowding(HHI)** | 12 | **29** (mediana 46,5) | Saudável — sem degenerescência |
| CIO(nível) | 3/6/12 | **1** (várias células com N≤3) | Degenerado |
| CIO(magnitude-abs) | 3/6/12 | idêntico ao CIO(nível), mesma base | Degenerado |
| FIT(nível) | 3/6/12 | **1** (2 células com N≤2 por horizonte) | Degenerado |
| FIT(magnitude-abs) | 3/6/12 | 5 (1-3 células com N<10) | Levemente degenerado |
| Herding(nível) | 3/6/12 | 30-31 | Saudável |
| Herding(magnitude-abs) | 3/6/12 | 11 | Saudável (limítrofe) |

**Consequência prática:** o achado principal do candidato #28 — Crowding(HHI)
prediz volatilidade futura, h=6 (t=8,67) e h=12 (t=14,03) — **está confirmado
livre desse problema específico**. As células são bem balanceadas (mín. 29,
mediana ~47 ações por perna-mês), o que é esperado porque HHI de posse é
calculável para quase todas as ações com fundos suficientes todo mês,
diferente de `peer_ret` (CIO), que depende de uma rede de overlap mais esparsa
e nem sempre bem distribuída.

Mas os números "quase interessantes" das linhas secundárias da mesma tabela —
CIO(magnitude-abs) h=3 p=0,0215, h=6 p=0,0488; FIT(magnitude-abs) h=3 p=0,053,
h=6 p=0,061 — **não deveriam ser citados nem como "padrão parecido mas mais
fraco"** (como o log faz) sem essa ressalva, porque o teste que os produziu
tem células com 1-5 observações nalguns meses. A direção (positiva, consistente
com Crowding) provavelmente está certa, mas a magnitude/significância
reportada não é confiável.

**Solução concreta:**
1. No `LOG_CANDIDATOS.md`, candidato #28: manter a conclusão sobre Crowding(HHI)
   como está (confirmada robusta), mas adicionar uma ressalva de que as linhas
   de CIO/FIT na mesma tabela usam um teste com células degeneradas e não devem
   ser lidas como evidência, nem fraca.
2. Se algum dia quiser reportar CIO/FIT→volatilidade como achado real, refazer
   com recorte de quintil mês a mês (Método B, já usado em 41-45) antes de
   citar qualquer p-valor.
3. Adicionar um print de N mínimo por célula em `testa_sinal_vol` (como já
   existe em `fama_macbeth_recut` dos scripts 41-45) para que esse tipo de
   problema apareça automaticamente em execuções futuras.

---

### 3. [COSMÉTICO/METODOLÓGICO] Script 39: "HHI prediz correlação futura" usa uma janela quase inteiramente sobreposta com o presente, não uma janela genuinamente futura

**Onde:** `39_betting_against_correlation.R`, linhas 87-89:
```r
bc <- merge(crowd[,.(ticker,ym,hhi_posse)], pw[,.(ticker,ym=ymk,corr_12m)], by=c("ticker","ym"))
corr_fut <- pw[, .(ticker, ym_alvo = addm(ymk,-1), corr_futura = corr_12m)]
bc <- merge(bc, corr_fut, by.x=c("ticker","ym"), by.y=c("ticker","ym_alvo"))
```
`corr_12m` é uma correlação móvel de **12 meses terminando no próprio mês
`ymk`** (janela `t-11` a `t`). `corr_futura`, para o mês-sinal `X`, é definida
como `corr_12m` do mês `X+1` — ou seja, a janela `(X-10)` a `(X+1)`. Essa
"correlação futura" **compartilha 11 dos 12 meses** com a informação já
conhecida no mês do sinal (`X`). Não é uma medida de correlação genuinamente
prospectiva — é essencialmente a correlação contemporânea, avançada em só 1
mês de uma janela de 12.

Isso contrasta com a construção cuidadosa e correta de `vol_futura` usada nos
scripts 28-31 (só `t+1` a `t+h`, nunca sobrepondo com o presente) — o padrão de
rigor não foi mantido aqui.

**Impacto:** o resultado citado no log ("HHI mais alto prediz correlação
FUTURA MENOR, t=-5,67, p<0,0001") é real no sentido de que os números batem,
mas a interpretação de "previsão" é enganosa — é majoritariamente uma relação
contemporânea/quase-contemporânea, não uma previsão de 1 mês à frente no
mesmo sentido rigoroso usado no resto da exploração. O próprio log já trata
esse resultado como "achado secundário, não um sinal de lucro" e não o usa
para nenhuma decisão prática, o que limita o dano — mas o rótulo "futura" é
impreciso e poderia enganar um leitor que citasse esse número fora de contexto.

**Solução concreta:** renomear a variável/o resultado para algo como
"correlação quase-contemporânea (avançada em 1 mês de uma janela de 12)" no
log, ou reconstruir `corr_futura` com uma janela genuinamente não sobreposta
(ex.: correlação calculada só sobre os meses `t+1` a `t+12`, no mesmo espírito
de `calc_vol_futura`) antes de usar esse número em qualquer lugar que não seja
uma nota lateral.

---

### 4. [CONFIRMADO, SEM PROBLEMA] Construção de `vol_futura` — idêntica e correta em todos os scripts 28-31

Comparei byte-a-byte a função `calc_vol_futura` nos 4 scripts (28, 29, 30, 31):
são **idênticas**. A lógica (`ym_base = addm(ymk, -k)` para `k` de 1 a `h`,
depois `sd()` dos `h` retornos resultantes) usa exclusivamente retornos de
`t+1` a `t+h` — nunca o retorno do próprio mês `t` do sinal. Verificado também
por raciocínio direto sobre os índices (`ym_base + k = ymk`, então a coluna
`r_k` no `ym_base` corresponde ao retorno em `ym_base+k`). Sem look-ahead, sem
divergência entre cópias da função.

### 5. [CONFIRMADO, SEM PROBLEMA] Construção do HHI — `valor_posicao = peso * aum_prev` é consistente em todos os 9 scripts que a usam, e `n_fundos >= 10` é aplicado uniformemente

`hhi_posse = sum((valor_posicao/sum(valor_posicao))^2)` com
`valor_posicao := peso * aum_prev` aparece **byte-idêntica** em
`28, 29, 30, 31, 33, 34, 39, 40, 41` — todos filtram `n_fundos >= 10` logo em
seguida, sem exceção nem limiar diferente.

Sobre a preocupação específica de "peso e AUM de meses diferentes por
engano": confirmei que `painel_multiativo_final.csv` **não tem** uma coluna
`aum` (contemporânea) — só `aum_prev`. Rastreei a origem em
`v2 OFICIAL/scripts/28_features_multiativo_completo.R:63`: `aum_prev` é
deliberadamente o AUM do fundo no mês **anterior** (`ym_prev`), extraído do
Informe Diário da CVM, como *feature predeterminada* (convenção usada em todo
o pipeline `v2 OFICIAL` para evitar look-ahead). Não é uma mistura acidental
de meses — é a única fonte de AUM disponível no painel, usada de forma
consistente em todos os 9 scripts. Não há bug aqui, mas fica registrado que
o valor em R$ de `valor_posicao` é uma aproximação (peso do mês `t` × AUM do
mês `t-1`), não o valor exato da posição no mês `t` — irrelevante para o
cálculo do HHI (que usa só proporções *dentro* do mesmo grupo ticker-mês, e
todos os fundos do grupo sofrem o mesmo tipo de defasagem), mas vale
documentar explicitamente essa convenção num comentário do código ou no TCC,
para não obrigar o próximo leitor a rastrear isso do zero.

### 6. [CONFIRMADO, SEM PROBLEMA] Significância de #28-31 — Fama-MacBeth genuíno em 30/31, `t.test` em 33-45 sempre aplicado à série mensal já agregada (nunca ativo-mês pooled)

- Script 29 usa regressão pooled (`lm` na amostra inteira) — mas isso é
  **intencional e devidamente rotulado** no próprio script e no log como
  "método errado", servindo de contraste para o script 30 (que refaz com
  Fama-MacBeth correto: regressão cross-seccional mês a mês, erro-padrão da
  série de coeficientes). Não é um bug escondido — é a mesma lição já
  documentada 2x na exploração, aplicada aqui de propósito para fins
  didáticos/de diagnóstico.
- Em todos os `t.test()` encontrados nos scripts 33-45 (`35, 36, 37, 38, 39`),
  o teste é sempre aplicado a uma série **já agregada por mês** (`ret_ls` ou
  `bac`, um valor por `ym`) — nunca a observações ativo-mês tratadas como
  independentes. Matematicamente equivalente ao Fama-MacBeth manual usado em
  outros scripts. Nenhuma recorrência do erro do candidato #1/#3.
- Todas as funções `fama_macbeth`/`fama_macbeth_recut` em 33, 34, 41-45 fazem
  regressão/spread mês a mês com erro-padrão da própria série temporal —
  corretas.

### 7. [CONFIRMADO, SEM PROBLEMA] Números do CSV batem exatamente com o `LOG_CANDIDATOS.md` para os candidatos #28-31

Cruzei manualmente todos os números citados no log contra os CSVs em
`v2 OFICIAL/exploracao_sinais/data/`:

- `candidatos_28_previsao_volatilidade.csv`: Crowding(HHI) h=6 t=8,669
  p=3,3e-08; h=12 t=14,031 p=2,1e-10 — bate com "t=8,67... t=14,03" do log.
- `candidatos_29_diagnostico_crowding_vol.csv` + `_log_29.txt`: correlação
  Pearson 0,1497 / Spearman 0,1648 — bate com "0,15 / 0,16" do log.
- `candidatos_30_fama_macbeth_correto.csv`: h=3 sozinho t=3,38 p=0,0027 →
  controlado t=1,16 p=0,257; h=6 sozinho t=6,01 p<0,001 → controlado t=1,81
  p=0,085; h=12 sozinho t=8,98 p<0,001 → controlado t=3,05 p=0,0076 — bate
  exatamente com a tabela do log.
- `candidatos_31_har_fragilidade.csv`: h=3 6,14%→6,86%→6,88% (ganho
  +0,02pp); h=6 8,29%→8,96%→9,36% (ganho +0,40pp); h=12 0,25%→0,20%→1,42%
  (ganho +1,22pp, ~7×) — bate exatamente com a tabela do log.

Nenhuma discrepância encontrada. Os números que provavelmente vão para o
relatório do desafio estão corretos como reportados.

### 8. [CONFIRMADO, SEM PROBLEMA] Reconciliação do CIO Peer Momentum (scripts 34-38) — metodologia correta, números batem

Script 34 já usa recorte de quintil por mês (`by=ym`) desde o início — não é
afetado pelo bug do Método A. Script 36 documenta corretamente a comparação
Método A vs. B. Script 37 (`t=2,17, p=0,041, N mín. de perna=53`) bate
exatamente com o log. Rodei o script 37 do zero para conferir
(`n_min_perna=53` para h=1, mediana consistente com "mínimo 53, mediana 57"
do log).

---

## Resumo (≤200 palavras)

O achado central da exploração — HHI de posse institucional prevê
volatilidade futura em h=12 (candidatos #28-31) — **passa na auditoria
adversarial sem ressalvas críticas**: a construção de `vol_futura` é idêntica
e correta nos 4 scripts, o HHI usa peso e AUM de forma consistente em 9
scripts, o Fama-MacBeth do script 30 é genuíno (não pooled), e todos os
números conferem exatamente com o `LOG_CANDIDATOS.md`. Verifiquei também que
a tabela de quintis do script 28 (usada para o t-estatístico principal) não
tem células degeneradas para Crowding(HHI) especificamente — a preocupação
que motivou a auditoria mais funda dos scripts 28-31 não se confirmou.

O que a auditoria encontrou de real: dois scripts SECUNDÁRIOS (33 e, em menor
grau, as linhas não-HHI da tabela do 28) reusam uma metodologia de corte de
quintil já comprovada como defeituosa em outro lugar da mesma exploração
(breaks fixos do treino, sem recorte mensal), gerando células com N=1-2. Um
número citado no log (candidato #33, p=0,008) evapora sob a correção — testei
e confirmei. Isso não muda nenhuma conclusão do TCC nem do achado principal,
mas dois números específicos no log deveriam ser corrigidos/marcados como não
confiáveis. Cobertura: li e testei os 16 scripts do escopo linha a linha;
não roda-testei todos os ~85 sub-especificações individualmente, mas
verifiquei a integridade estrutural (construção de sinal, filtros, método de
inferência) de cada um.
