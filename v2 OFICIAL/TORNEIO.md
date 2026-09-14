# Torneio: o fundo que está atrás do benchmark muda a carteira de forma diferente?

Trilha aberta em 11/09/2026, a partir de uma pergunta feita pelo professor depois da
apresentação do TCC:

> *"Existem estudos sobre quando um fundo está abaixo do benchmark no ano e então ele muda
> mais abruptamente a sua posição para tentar bater o benchmark. E se encontrarmos um
> fenômeno de que quem está para trás do benchmark copia quem está acima do benchmark,
> seria um achado muito interessante também."*

Este documento reúne (1) o que a literatura já sabe, (2) o que testamos com a nossa base, e
(3) o que achamos. Os scripts são `110` a `124` em `scripts/`, os logs são
`_log_11*_torneio_*.txt`.

> **Conclusão, 14/09/2026: a trilha fecha com dois resultados nulos.** Não há cópia do líder,
> e o achado inicial de realocação de torneio no 4º trimestre não sobreviveu à checagem de
> robustez (seção 3.5). O que resta de positivo é a decomposição ativa/passiva e um erro de
> calendário que vale para qualquer trabalho que meça negociação a partir da CDA mensal.
> Trilha encerrada; não retomar sem motivo novo.

---

## 1. A literatura

### 1.1 O fenômeno tem nome: hipótese do torneio

O paper fundador é **Brown, Harlow e Starks (1996, *Journal of Finance* 51(1), 85–110)**. A
indústria de fundos é lida como um torneio anual: o gestor é avaliado pelo ranking relativo
no fim do ano e a captação responde de forma convexa ao desempenho. Isso é uma opção — lado
bom ilimitado, lado ruim truncado —, então quem está perdendo no meio do ano tem incentivo a
aumentar risco no segundo semestre. Com 334 fundos americanos de 1976 a 1991, eles acham
exatamente isso.

### 1.2 A crítica que derrubou metade da literatura

**Busse (2001, *JFQA* 36(1), 53–73)** refez o teste com dados diários e o resultado sumiu: a
autocorrelação dos retornos diários enviesa a estimativa de volatilidade feita com poucos
retornos mensais, e o viés é correlacionado com o desempenho do semestre.
**Goriaev, Nijman e Werker (2005, *J. Empirical Finance* 12(1), 127–137)** somam a correlação
transversal entre fundos, que os testes padrão ignoram.

**Por que isso não nos atinge:** as duas críticas atacam testes que medem risco a partir da
*série de retornos da cota*. Nós medimos a carteira. Um teste feito sobre pesos de portfólio é
imune à crítica do Busse por construção — não há estimativa de volatilidade para enviesar.

### 1.3 A virada teórica: não é volatilidade, é desvio do benchmark

**Chen e Pennacchi (2009, *JFQA*)** mostram que, quando a remuneração depende do desempenho
*relativo a um benchmark*, o gestor que está perdendo aumenta a volatilidade do **tracking
error**, e não necessariamente a do retorno. A distinção é tudo: quem está perdendo do
Ibovespa não precisa comprar ativos mais voláteis, precisa se **afastar** do Ibovespa. Testar
torneio com desvio-padrão do retorno é olhar a variável errada.

### 1.4 A virada empírica: medir com carteiras

- **Huang, Sialm e Zhang (2011, *RFS* 24(8), 2575–2616)** — medida de *risk shifting* baseada
  em holdings; quem aumenta risco rende pior depois.
- **Li, Tiwari e Tong (2022, *J. Financial Stability* 63)** — o mais próximo de nós. Fundos
  com desempenho fraco até o fim do 3º trimestre **aumentam o Active Share no 4º trimestre**,
  acompanhado de maior exposição a risco de queda, e os autores concluem que não é informação,
  é estratégia de ranking.
- **Yi e Yan (2024, *IRFA* 96)** — fundos chineses mal posicionados nos três primeiros
  trimestres aumentam o desvio de estilo das dez maiores posições no 4º trimestre.

### 1.5 Quando o sinal inverte

- **Kempf, Ruenzi e Thiele (2009, *JFE* 92(1), 92–108)** — o gestor tem dois incentivos em
  conflito: remuneração (aposta) e emprego (encolhe). Em mercado de alta domina o primeiro; em
  mercado de baixa, o segundo. Nosso período de teste, 2020–2021, tem os dois regimes colados.
- **Taylor (2003, *JEBO* 50, 373–383)** — se o líder antecipa a jogada do perdedor, o
  equilíbrio inverte e é o **líder** quem escolhe a estratégia arriscada com mais frequência.

### 1.6 "O perdedor copia o vencedor?" — duas teorias opostas

| Teoria | Previsão | Referência |
|---|---|---|
| Torneio | o perdedor se **afasta** do rebanho — copiar não faz ninguém ultrapassar ninguém | Chen e Pennacchi (2009); Li, Tiwari e Tong (2022) |
| Carreira | o perdedor se **cola** no rebanho — carteira convencional protege o emprego | Chevalier e Ellison (1999, *QJE* 114(2), 389–432); Jiang e Verardo (2018, *JF* 73(5)) |

Chevalier e Ellison acham que gestores jovens são demitidos se o risco do fundo se desviar da
média do grupo, e que por isso carregam **carteiras mais convencionais** — é uma previsão
sobre composição de carteira, não sobre volatilidade. Jiang e Verardo acham que fundos que
seguem a manada rendem 2%+ a.a. a menos, e que o gap é maior entre gestores com mais
preocupação de carreira.

Como se mede copiar, tecnicamente: **Sias (2004, *RFS* 17(1), 165–206)** decompõe a
persistência da demanda institucional em "seguir os outros" e "seguir a si mesmo". Essa
decomposição é o que separa cópia de inércia — e é a que usamos no teste C.

Que a cópia é tecnicamente viável: **Frank, Poterba, Shackelford e Shoven (2004, *JLE* 47(2))**
e **Verbeek e Wang (2013, *JBF*)** mostram que carteiras que replicam holdings divulgados
entregam desempenho igual ou ligeiramente superior, líquido de taxas.

### 1.7 Literatura brasileira

| Trabalho | Amostra | Achado |
|---|---|---|
| **Januzzi (2018)**, tese CEPEAD/UFMG, cap. 6 | 727 multimercados, 2010–2015, mensal | Torneio **confirmado**: fundos alavancados perdedores ampliam posição em derivativos, elevando risco total, sistemático e de queda, sem contrapartida de retorno ajustado |
| **Marques, Sampaio e Silva (2020)**, *RC&F* | 375 fundos de ações ativos, 31.185 carteiras, 2010–2016 | *Window dressing* concentrado em "gestora pequena, **perdedora contra o Ibovespa**, tracking error alto" — ~2× o rank gap dos de TE baixo |
| **Camargo Neto e Maciel (2024)**, 27º SemeAd/FEA-USP | 2.639 fundos "ações livre", jan/2010–abr/2024 | Sinal **invertido**: relação *positiva* entre excesso de retorno e tracking error — quem se afasta do benchmark é o **vencedor** |
| **Kutchukian (2010)**, dissertação FGV-EAESP | fundos brasileiros | Efeito manada presente; não condiciona ao desempenho relativo |

**O buraco:** ninguém no Brasil testou torneio no nível de **peso de carteira** com o universo
completo da CVM, e a pergunta "o perdedor copia o líder?" não foi respondida com holdings.

---

## 2. O que foi feito

### 2.1 A peça comum (script 110)

Para cada fundo e mês, o desempenho **acumulado no ano civil**, em duas medidas, porque a
literatura usa as duas e elas não são a mesma coisa:

- `excesso_acum` — retorno acumulado da cota menos retorno acumulado do Ibovespa. É o "bater o
  benchmark", que é como a indústria brasileira comunica desempenho de fundo de ações.
- `rank_pct` — percentil do retorno acumulado entre todos os fundos do universo naquele mês. É
  o torneio no sentido literal de BHS: o que importa é a posição na lista.

Exige série completa desde janeiro (senão "acumulado do ano" de um fundo que apareceu em
setembro não é comparável ao de quem está desde janeiro). Resultado: **107.329 fundo-mês,
2.678 fundos, 2017–2021**.

> **2016 fica de fora** porque o primeiro retorno de cada série é NA (precisa da cota de
> dez/2015), o que quebra a exigência de série completa desde janeiro em todo o ano de 2016.

A fração de fundos abaixo do Ibovespa até junho varia muito por ano — 14,5% em 2020, 74,3% em
2019 — e é por isso que **toda regressão leva efeito fixo de ano**. Sem ele, a comparação
"perdedor vs vencedor" mistura anos em que perder era regra com anos em que era exceção.

### 2.2 O confundidor que precisava ser tratado

Um fundo perdedor perde porque as ações dele caíram — e isso **mecanicamente** desloca os
pesos, sem nenhuma decisão do gestor. Todo teste separa as duas coisas, usando a correção já
validada no script 62:

```
peso_mecânico = peso_t × (1 + r_ativo) / (1 + r_fundo)
Δw ATIVA   = peso_{t+1} − peso_mecânico     ← decisão do gestor
Δw PASSIVA = peso_mecânico − peso_t         ← o preço agindo sozinho
```

Essa separação é a vantagem central do nosso desenho: a literatura que só observa retorno não
consegue fazê-la.

---

## 3. Resultados

### 3.1 Teste A — velocidade de ajuste e intensidade de realocação (script 111)

**λ por grupo, 2º semestre, Δw ativa, 3,36 milhões de observações:**

| Grupo (posição até junho) | λ | erro-padrão | meia-vida |
|---|---|---|---|
| Venceu o Ibovespa | **0,0690** | 0,0056 | 9,7 meses |
| Perdeu do Ibovespa | **0,0582** | 0,0048 | 11,6 meses |

Interação `d:perdedor` = **−0,0108** (p = 0,011, EP clusterizado por fundo).
**O fundo que está atrás ajusta mais devagar, não mais rápido.**

**Placebo:** a mesma classificação de junho aplicada ao *primeiro* semestre dá λ = 0,0712 vs
0,0718 — diferença nula, como tem que ser, já que o desempenho de junho não pode ter causado o
comportamento de janeiro a maio. A diferença do 2º semestre é resposta, não traço permanente
da casa.

**Por quartil de ranking** (1 = pior): 0,051 · 0,071 · 0,092 · 0,061. Não é monótono — os dois
extremos ajustam devagar, e quem ajusta mais rápido é o 3º quartil.

**Intensidade de realocação** (análogo do *Risk Adjustment Ratio* de BHS, aplicado a peso:
média |Δw| do 2º semestre ÷ média |Δw| do 1º semestre, dentro do mesmo fundo e ano; 7.089
fundo-anos): log(RAR) ~ perdedor com efeito fixo de ano = **−0,032** (p = 0,029). Versão
contínua, sobre o excesso acumulado = **+0,257** (p = 0,013). **Quem está atrás realoca menos.**

### 3.2 Teste B — desvio em relação ao alvo (script 112)

O erro da Etapa 1, `e = peso − g(x'θ)`, é um *Active Share de características*: distância entre
a carteira do fundo e o que fundos com as **mesmas características** carregavam naquele mês
naquela ação. É um benchmark endógeno e móvel, melhor que o índice fixo usado por Li, Tiwari e
Tong.

**O sinal depende do desenho, e isso é o resultado:**

| Desenho | Direção | Coeficiente |
|---|---|---|
| B1 — semestral (junho → jul-dez) | **vencedor** desvia mais | excesso: +0,340 sobre log-razão RMS (p = 3,6e-12) |
| B2 — Li-Tiwari-Tong (setembro → 4º tri) | **perdedor** desvia mais | perdedor: +0,030 sobre log-razão AS (p = 0,00012) |
| B3 — painel mensal, EF de fundo e mês | perdedor desvia mais | excesso: −0,216 (p < 1e-4) |
| B3b — mesmo, com excesso defasado 1 mês | perdedor desvia mais | −0,187 (p = 0,003) |
| B3b — defasado 3 meses | **nulo** | +0,074 (p = 0,28) |

O B3 decai até zero em 3 meses e o B4 (interação por trimestre) **não** mostra intensificação
rumo ao fim do ano — o que é incompatível com um mecanismo de torneio e compatível com
causalidade reversa via persistência do próprio desvio. O resultado que sobrevive de pé é o
**B2**, que é exatamente o desenho do paper americano.

### 3.3 Teste C — o perdedor copia o líder? (script 113)

Desenho à la Sias (2004): copiar é seguir a **operação** do outro, não ter a mesma posição. O
horse race aninha o próprio modelo do TCC:

```
Δw_{i,n,t} = λ·d_{i,n,t} + γ_L·Δw_líder_{n,t−L} + γ_C·Δw_todos_{n,t−L}
             + γ_P·Δw_perdedor_{n,t−L} + interações com "estar atrás"
```

Líderes e perdedores são decis de desempenho acumulado; todos os agregados são
**leave-one-out** na data de origem, para não capturar a inércia do próprio fundo. A defasagem
de divulgação roda em L = 1 (limite superior do copiável) e L = 3 (limite legal de omissão da
CDA pela ICVM 555).

**Nada. Em nenhuma especificação, em nenhuma defasagem.**

| Termo | L = 1 | L = 3 |
|---|---|---|
| Δw do líder | +0,0030 (p = 0,60) | −0,0084 (p = 0,12) |
| Δw da manada | +0,0153 (p = 0,67) | −0,0031 (p = 0,93) |
| Δw do perdedor (placebo) | −0,0130 (p = 0,34) | +0,0027 (p = 0,82) |
| **Δw líder × estar atrás** | **+0,0181 (p = 0,62)** | **+0,0166 (p = 0,11)** |
| *d* (alvo do próprio fundo) | **+0,0901 (t = 26,2)** | **+0,0883 (t = 37,8)** |

**O null é informativo, não fraco.** A mesma regressão, na mesma amostra de 7,1 milhões de
observações, estima o coeficiente do alvo próprio com t = 26. O desenho detecta sinal de
realocação de peso; ele simplesmente não encontra sinal de seguir o líder.

O descritivo de similaridade (cosseno com a carteira média dos líderes defasada 3 meses) mostra
que líderes se parecem com líderes (0,724) mais do que perdedores (0,670), e que dentro do
mesmo fundo subir no ranking anda junto com ficar mais parecido com os líderes (+0,048,
p < 1e-16). **Isso não é evidência de cópia** — é quase tautológico: quem carrega o que os
líderes carregam rende como eles e sobe no ranking. A direção causal implícita é a oposta da
hipótese do professor.

### 3.4 Síntese, primeira versão: deriva ou aposta? (script 114)

Testes A e B pareciam brigar. A decomposição ativa/passiva resolve, e o resultado muda
conforme a janela.

**No 2º semestre inteiro (jul–dez), 7.089 fundo-anos:**

| Sobre o excesso acumulado até junho | Coeficiente | p |
|---|---|---|
| log-razão da realocação ativa | +0,257 | 0,013 |
| log-razão da deriva passiva | −0,188 | 0,042 |
| fração do movimento que é decisão do gestor | +0,084 | 0,00017 |

Quanto pior o desempenho, menor a parcela do movimento da carteira que é decisão. O perdedor
não está apostando: está parado enquanto o preço mexe a carteira por ele.

**No 4º trimestre isoladamente (classificado em setembro), 7.425 fundo-anos, o quadro parecia
inverter:** perdedor, com efeito fixo de ano, parcela ativa +0,090 log-ponto (p = 7,7e-8),
parcela passiva +0,098 log-ponto (p < 1e-16), fração ativa −0,6 p.p. (não significativa).

Esse foi o único resultado da bateria que sustentava a hipótese do professor, e por isso
passou por uma checagem de robustez dedicada. **Ele não sobreviveu.** A seção 3.5 mostra por
quê; o que segue nesta seção 3.4 fica registrado apenas como histórico do que foi tentado.

### 3.5 A bateria de robustez, e por que o achado do 4º trimestre caiu (scripts 116 a 124)

Quatro problemas foram encontrados, em ordem crescente de gravidade.

**(a) A "inversão" entre classificar em junho e classificar em setembro é aritmética, não
empírica (script 119).** Por construção, o excesso acumulado até setembro é a soma do
acumulado até junho com o do 3º trimestre: no dado, `max |exc_set − (exc_jun + exc_q3)| =
5,3e-16`. Regredir sobre `exc_set` sozinho é, portanto, regredir sobre a soma de duas peças
cujos efeitos têm sinais opostos, e a decomposição fecha exatamente:

| Parcela | Contribuição ao β de `exc_set` |
|---|---|
| vinda de `exc_jun` | +0,380 |
| vinda de `exc_q3` | −0,609 |
| soma, β previsto | −0,229 |
| β observado na regressão | −0,229 |

As duas peças são praticamente ortogonais (corr = −0,009), de modo que separá-las é limpo. Não
havia dois desenhos em conflito; havia um regressor mal construído. A pergunta do script 116,
"qual dos dois desenhos está certo?", estava mal posta.

**(b) A janela do 4º trimestre estava deslocada em um mês (scripts 120 e 121).** A medida `dw`
compara o peso no fim do mês *t* com o peso no fim do mês *t+1*, ou seja, mede a negociação
feita **durante o mês t+1**. Os scripts 114, 116 e 117 agrupam pelo mês *t*, de modo que o que
eles chamam de 4º trimestre (`mes ∈ 10:12`) é a negociação de **novembro, dezembro e janeiro**:
inclui janeiro, posterior ao fim do torneio, e exclui outubro, que é o primeiro mês de reação
à classificação de setembro. Corrigido o calendário e redefinido `exc_q3` como retorno composto
do trimestre menos o Ibovespa no mesmo intervalo, o resultado agregado ainda sobrevive
(`exc_q3` = −1,30, p = 1,2e-06), mas 2017 sai da amostra, porque os dados de peso começam em
fevereiro de 2017.

**(c) Ano a ano o efeito desaparece (script 121).** Com o calendário corrigido:

| ano | coeficiente de `exc_q3` | p |
|---|---|---|
| 2018 | +0,001 | 0,998 |
| 2019 | −0,144 | 0,781 |
| 2020 | −1,181 | 0,057 |
| 2021 | −2,036 | 0,000 |

**(d) No painel trimestral com efeito fixo de fundo, o 4º trimestre não se destaca (scripts 122
a 124).** Este é o desenho decisivo: dependente em nível, `log(realocação ativa do trimestre)`,
sem razão e sem denominador, o que elimina de saída a suspeita de artefato de denominador
levantada no script 116; efeito fixo de fundo, para o nível próprio de atividade da casa;
efeito fixo de ano-trimestre, para o mercado; um coeficiente livre por trimestre; e regressores
padronizados dentro de cada ano-trimestre, para que 2018 e 2020 sejam comparáveis.

| trimestre | desempenho do trimestre anterior | p | desempenho contemporâneo | p |
|---|---|---|---|---|
| 1º | −0,025 | 0,020 | −0,021 | 0,026 |
| 2º | −0,006 | 0,48 | −0,013 | 0,14 |
| 3º | +0,006 | 0,58 | +0,005 | 0,68 |
| 4º | −0,014 | **0,107** | **−0,044** | **4e-08** |

Três leituras, todas contra a hipótese:

1. No 4º trimestre o desempenho anterior não é significativo (p = 0,11), e o teste formal de
   que ele é igual ao do 1º trimestre não rejeita (p = 0,47). A hipótese do torneio exige que o
   4º trimestre se destaque; quem se destaca é o 1º.
2. O que é grande no 4º trimestre é a reação ao desempenho **contemporâneo**, que é o oposto de
   uma decisão tomada em outubro com a informação de setembro.
3. Contraprova: essa mesma reação contemporânea do 4º trimestre aparece, ainda mais forte, na
   **deriva passiva** (−0,89, p = 1e-21), que não é decisão de ninguém. O coeficiente
   contemporâneo do 4º trimestre é mecânica de mercado vazando na medida.

Abrindo por ano e por trimestre (efeito de 1 desvio-padrão, dentro do fundo), o achado inteiro
é de 2021:

| ano | 4º tri, desempenho anterior | p |
|---|---|---|
| 2017 | +0,038 | 0,023 |
| 2018 | −0,007 | 0,60 |
| 2019 | +0,019 | 0,16 |
| 2020 | +0,027 | 0,09 |
| 2021 | −0,164 | 0,000 |

Em quatro dos cinco anos o sinal é positivo ou nulo. E em 2021 o padrão nem é do 4º trimestre:
o 3º tri do mesmo ano dá −0,084 (p < 0,001).

**Nota técnica que vale guardar.** Sem efeito fixo de fundo, os coeficientes por trimestre
explodem e trocam de sinal (+10,4 no 1º tri, −3,1 no 2º, +4,9 no 3º, +2,4 no 4º). A variação
entre fundos nessa medida não é utilizável: qualquer especificação sem efeito fixo de fundo
aqui é ruído.

---

## 4. O que isso quer dizer

A trilha termina com **dois resultados nulos**, ambos bem alimentados.

1. **O perdedor não copia o líder.** O comovimento entre carteiras é todo contemporâneo
   (coeficiente 0,97 sobre o movimento simultâneo dos demais fundos) e o defasado é zero
   (0,0007, p = 0,70), em toda defasagem de divulgação testada. Isso é informação comum, não
   imitação.
2. **Não há realocação de torneio no 4º trimestre.** O que parecia ser torneio era a soma de
   um regressor que mistura duas peças de sinal oposto, uma janela deslocada em um mês e um
   único ano (2021) puxando o agregado.

Duas observações sobrevivem e têm valor próprio:

- **A decomposição ativa/passiva é a contribuição metodológica real.** A literatura
  internacional mede aumento de Active Share sem conseguir separar decisão do gestor de deriva
  de preço, porque não observa carteira com esse detalhe. Nós observamos, e no 2º semestre a
  resposta é deriva. Isso qualifica o achado americano em vez de apenas replicá-lo.
- **O erro de calendário é um alerta transferível.** Quando a variável é a diferença entre
  duas fotografias de fim de mês, ela mede a negociação do mês seguinte, e qualquer janela
  definida sobre o mês da primeira fotografia fica deslocada. Vale para qualquer trabalho que
  use CDA mensal para medir negociação.

## 5. Limitações

- **Amostra 2017–2021**, e o desenho em tempo de negociação com calendário corrigido perde
  2017, ficando com quatro anos úteis. Com quatro anos, um único ano atípico basta para mover
  o agregado, que foi exatamente o que aconteceu.
- **A separação ativa/passiva depende de `r_ativo` e `r_fundo` medidos sem erro.** A reação
  contemporânea que aparece na deriva passiva indica que esse erro existe e não é pequeno.
- **O calendário do torneio foi assumido anual**, com cortes em junho e setembro. Janelas
  alternativas não foram testadas.
- **Não se investigou o que acontece em 2021.** Foi um ano de resgate pesado em fundos de
  ações no Brasil, e a hipótese de que o padrão de 2021 seja venda forçada por resgate, e não
  estratégia de ranking, ficou em aberto por decisão de escopo.

## 6. Arquivos

Todos consomem `erro_e_multiativo.csv` e derivados. **Nenhum altera a Etapa 1 nem qualquer
arquivo do pipeline do TCC**, de modo que a regra do `PIPELINE.md` não é acionada.

| Script | O que faz | Log |
|---|---|---|
| `110_desempenho_relativo_fundo.R` | desempenho acumulado e ranking por fundo-mês | — |
| `111_torneio_teste_a_lambda.R` | λ por grupo, intensidade de realocação e placebo | `_log_111_torneio_a.txt` |
| `112_torneio_teste_b_desvio_alvo.R` | desvio do alvo (Active Share de características) | `_log_112_torneio_b.txt` |
| `113_torneio_teste_c_copia_lider.R` | horse race de cópia do líder e similaridade | `_log_113_torneio_c.txt` |
| `114_torneio_sintese_deriva_vs_aposta.R` | decomposição ativa/passiva e mediação | `_log_114_torneio_sintese.txt` |
| `115_torneio_refino_q4_reversao_manada.R` | perfil trimestral, reversão em janeiro, comovimento | `_log_115_torneio_refino.txt` |
| `116_torneio_diagnostico_q4.R` | cruzamento 2x2 de data de classificação e denominador | `_log_116_diagnostico_q4.txt` |
| `117_torneio_q4_e_fluxo.R` | o 3º trimestre sobrevive ao controle de fluxo? | `_log_117_q4_fluxo.txt` |
| `118_torneio_recencia_vs_fim_de_ano.R` | recência contra posição acumulada | `_log_118_recencia.txt` |
| `119_torneio_inversao_e_aritmetica.R` | prova de que a inversão jun/set é aritmética | — |
| `120_torneio_cache_realocacao_mensal.R` | cacheia realocação ativa e passiva por fundo-mês | — |
| `121_torneio_calendario_corrigido.R` | refaz tudo em tempo de negociação, ano a ano | — |
| `122_torneio_painel_trimestral.R` | painel trimestral em nível, com efeito fixo de fundo | — |
| `123_torneio_placebo_por_trimestre.R` | um coeficiente por trimestre, e a contraprova passiva | — |
| `124_torneio_efeito_padronizado_por_ano.R` | efeito padronizado por ano e por trimestre | — |

**Status: encerrada em 14/09/2026 como resultado nulo.** Não retomar sem motivo novo.
