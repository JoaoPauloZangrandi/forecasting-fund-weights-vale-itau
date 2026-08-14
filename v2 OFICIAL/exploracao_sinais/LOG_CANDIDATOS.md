# Log de candidatos testados (exploração de sinal de lucro)

Documento de trabalho, **não faz parte do TCC**. Registra TODO candidato
testado, inclusive os que falharam — critério: nada aqui vira achado "real"
sem sobreviver a teste fora da amostra (treino <2020-01, teste >=2020-01,
mesma disciplina do resto do TCC).

## Pesquisa de literatura (13/08-14/08/2026)

Achado mais importante: **Wardlaw (2020, Journal of Finance)** mostra que a
construção-padrão de "flow-induced trading"/pressão de fluxo (usada por
Coval-Stafford 2007 e por boa parte da família de papers que inclui Lou
2012) é **mecanicamente correlacionada com o próprio retorno realizado do
período**, porque o denominador (TNA/valor da posição) já embute a
valorização/desvalorização da carteira — quebra a suposta ortogonalidade a
fundamentos. Corrigindo essa contaminação, o efeito de pressão de preço na
literatura vira pequeno e sem reversão. **Isso pode explicar, de forma
honesta e defensável, por que o robô do TCC e o FIT (candidato #1) não
acharam nada** — não necessariamente falta de poder estatístico, pode ser
que o resultado ~0 esteja correto.

Lista completa de candidatos pesquisados (14 no total, com papers, links,
magnitude reportada na literatura original e nível de confiança) está
registrada à parte — ver transcript da pesquisa. Os mais promissores e
viáveis com o dado disponível (holdings mensais BR, sem dado intradiário):

1. **Breadth of ownership (Chen, Hong & Stein 2002, JFE)** — nº de fundos
   com posição positiva no ativo; QUEDA de breadth prevê retorno mais baixo
   (restrição de venda a descoberto). Dado já pronto, fácil de testar.
   Magnitude original: 6,38% bruto / 4,95% ajustado por fatores, spread de
   decil, 12 meses.
2. **Latent demand (Koijen & Yogo 2019, JPE)** — usar o resíduo do modelo
   de peso esperado (Etapa 1) diretamente como preditor agregado, sem
   passar pela conversão em "fluxo do robô" — conceitualmente é quase o que
   já foi construído, só que testado de forma mais direta.
3. **Comovimento via propriedade comum (Anton-Polk 2014 / Lou-Polk
   Comomentum)** — caso empírico mais forte da literatura (identificação
   quase-causal, >9%/ano), mas desenhado pra dado de retorno semanal — dado
   mensal tem poder estatístico bem mais fraco pra essa versão.
4. **Fire sale clássico (Coval-Stafford)** — já testado como candidato #1/C
   acima e rejeitado; risco de contaminação mecânica (Wardlaw) desaconselha
   insistir nessa família sem corrigir a construção primeiro.

## Candidato #1: Flow-Induced Trading (Lou 2012)

**Mecanismo:** FIT_{ativo,mês} = média do fluxo/AUM dos fundos que carregam
o ativo, ponderada pelo tamanho (R$) da posição de cada um — mede pressão
mecânica de compra/venda induzida por captação/resgate dos fundos, não por
skill do gestor.

**Amostra inteira (script 105, sem separar treino/teste):** parecia forte —
t entre -4,6 e -6,1, p<0,00001 em h=0,1,6,12 (só h=3 não significativo).

**Teste fora da amostra (script `exploracao_sinais/scripts/01_teste_oos_fit_lou.R`):**
não sobrevive. β do treino instável entre horizontes (chega a trocar de sinal:
h=0 → -0,021, h=1 → +0,063, h=3 → -0,268). R² fora da amostra **negativo**
em h=1 (-0,39%), h=3 (-2,79%) e h=6 (-0,21%) — pior que prever a média do
treino. Só positivo (e irrisório) em h=0 (0,08%) e h=12 (0,17%). Winsorizar
não resolve.

**Veredito: REJEITADO.** A significância na amostra inteira era artefato de
overfitting/correlação espúria, não sinal real. Reforça (não contradiz) a
conclusão já publicada no TCC de que fluxo previsto não prediz retorno nesta
base. **Possível explicação de fundo:** ver nota Wardlaw (2020) acima — a
métrica pode ter contaminação mecânica com o próprio retorno.

## Candidatos #2-4: Herding (LSV), Demanda agregada revelada, Fire sale extremo

Script `exploracao_sinais/scripts/02_herding_demanda_agregada_firesale.R`,
teste fora da amostra desde o início (não repetiu o erro do candidato #1).

- **(A) Herding (Lakonishok-Shleifer-Vishny 1992):** proporção de fundos
  compradores vs. vendedores de um ativo, relativa à média do mercado
  naquele mês. h=1: R²_OOS=0,004%; h=3: -0,015%; h=6: 0,088%. Todos
  irrisórios.
- **(B) Demanda agregada revelada:** variação % do valor total (R$)
  mantido pelo conjunto de fundos num ativo (diferente do FIT — aqui é o
  que os fundos realmente fizeram, não pressão mecânica de fluxo). h=1:
  R²_OOS=-0,077%; h=3: 0,008%; h=6: 0,176%. Melhor dos 3, ainda irrisório.
- **(C) Fire sale específico (Coval-Stafford 2007):** restrito aos 5% de
  ativo-mês com pressão de venda mecânica mais extrema (não a média, como
  no FIT) — hipótese: efeito concentrado em eventos extremos. R²_OOS
  negativo em 3 dos 4 horizontes (-0,64% em h=1); retorno médio pós-venda
  extrema é positivo e cresce com o horizonte, mas isso parece ser deriva
  de mercado geral, não recuperação específica (R² linear não confirma).

**Veredito: os 3 REJEITADOS.** Mesmo padrão do candidato #1 — parecem
plausíveis na amostra inteira (alguns com p<0,01), mas nenhum produz R²
fora da amostra economicamente relevante.

## Candidato #5: Sinal dos fundos-líder

Script `03_lideres_e_long_short.R`, parte (D). Em vez de olhar o fluxo do
SEGUIDOR (já testado e rejeitado no TCC), testa se o que os 1.754 fundos
identificados como LÍDERES fazem, agregado por ativo-mês, prediz retorno.

R²_OOS: h=1: -0,001%; h=3: 0,007%; h=6: 0,011%. Todos p>0,14 na amostra
inteira. **REJEITADO** — nem os líderes têm sinal detectável.

## ⚠️ Armadilha estatística pega a tempo: long-short por quintil sem correção de dependência cross-sectional

Script `03_lideres_e_long_short.R`, parte (E), testou os 3 candidatos
anteriores como estratégia long-short (comprar quintil top do sinal, vender
quintil bottom, breakpoints do treino aplicados ao teste). Resultado inicial
pareceu **extremamente forte**: FIT em h=0/h=1 deu spread de -6,5 e -8,0 p.p.
com p=10⁻²⁰/10⁻²². Isso teria sido a resposta que procurávamos.

**Mas o teste estava errado.** O `t.test` tratava cada par ativo-mês como
observação independente — só que retornos de ativos diferentes no MESMO mês
são correlacionados entre si (fator de mercado comum), o que infla a
significância artificialmente (o mesmo tipo de erro que a Comomentum de
Lou-Polk, achada na pesquisa de literatura, evita calculando span de tempo
corretamente). Prova de que algo estava errado: o R² da regressão linear
(candidato #1) para o mesmo sinal e horizonte era ~0%, incompatível com
p=10⁻²⁰ num teste supostamente relacionado.

**Corrigido com Fama-MacBeth de verdade** (script `04_long_short_fama_macbeth.R`):
um spread Q5-Q1 POR MÊS (não por ativo-mês), testando se a MÉDIA da série
temporal de ~13-24 spreads mensais é diferente de zero, com o erro-padrão
da própria série temporal. Resultado: **tudo evapora**. FIT h=0: t=-0,02,
p=0,98. FIT h=1: t=1,72, p=0,099 (não significativo a 5%). O único caso
que quase passa de 5% é "demanda agregada revelada" h=6: t=2,03, p=0,057 —
mas com só 19 meses de teste e sendo 1 de ~15 especificações testadas até
agora, isso é exatamente o tipo de "quase-significativo" que se espera
aparecer por acaso mesmo sem sinal nenhum (à taxa de 5%, esperaríamos
~0,75 "hits" em 15 tentativas).

**Veredito: nenhum candidato sobrevive. Lição registrada:** ao testar
estratégia long-short com dado de painel (várias ações no mesmo mês),
usar SEMPRE Fama-MacBeth (ou erro-padrão clusterizado por mês) — nunca
t-test tratando ativo-mês como observações independentes. Isso quase
gerou um falso positivo espetacular que teria sido constrangedor levar
pro João sem essa checagem.

## Candidatos #7-8: Breadth of Ownership e Latent Demand

Script `05_breadth_e_latent_demand.R`. Ambos com regressão OOS E
Fama-MacBeth desde o início.

- **(F) Breadth of ownership (Chen-Hong-Stein 2002):** variação % do
  número de fundos distintos com posição positiva no ativo (margem
  extensiva, diferente de tudo testado antes que pesava por R$). R²_OOS
  negativo em todos os horizontes (h=1 a h=12); Fama-MacBeth: todos
  p>0,46. Nada.
- **(G) Latent demand (Koijen-Yogo 2019), nível e choque:** resíduo do
  modelo de peso esperado (Etapa 1), agregado por ativo-mês e ponderado
  por AUM — usado direto como preditor, sem passar pela conversão em
  "fluxo do robô". Nível: melhor caso p_fm=0,078 (h=3, não significativo
  a 5%). Choque (variação mês a mês do resíduo agregado): melhor caso
  p_fm=0,21. Nada sobrevive.

**Veredito: os 2 REJEITADOS.**

## Síntese depois de 8 candidatos / ~35 especificações testadas

Testados com rigor (fora da amostra, Fama-MacBeth onde aplicável):
FIT/Lou, Herding (LSV), Demanda agregada revelada, Fire sale extremo,
Sinal dos líderes específicos, Breadth of ownership, Latent demand
(nível e choque) — cobrindo a maior parte da literatura relevante e
adaptável ao tipo de dado disponível (holdings mensais BR, sem dado
intradiário). **Nenhum produziu sinal robusto fora da amostra.** O
melhor resultado entre todos foi p≈0,06-0,08 em 1-2 especificações
isoladas — exatamente a taxa de falso-positivo esperada testando ~35
hipóteses a 5%, não evidência de sinal real.

## Candidato #9: Líderes ponderados por skill (Cohen, Coval & Pástor 2005)

Script `06_lideres_ponderados_por_skill.R`. Diferença do candidato #5: em
vez de tratar todo "líder" igual, pondera (H1) ou filtra (H2, só top
quintil) pelo retorno acumulado dos últimos 12 meses do fundo-líder
(informação só até o mês t, sem look-ahead) — testa se o sinal só existe
quando vem de gestores historicamente bons.

H1 (ponderado): melhor caso h=1, R²_OOS=0,10% (p da regressão cheia=0,0002,
mas Fama-MacBeth do mesmo sinal/horizonte: p=0,30 — não significativo).
H2 (só top-quintil skill): melhor caso h=1, Fama-MacBeth p=0,059 (quase,
mas não passa de 5%).

**Veredito: REJEITADO**, com ressalva de que H2 h=1 (p=0,059) é o mais
próximo de significativo até agora — mas, com ~50 especificações já
testadas no total, um p≈0,06 é exatamente a taxa de falso-positivo
esperada por acaso, não evidência.

## Candidato #10: Restrição a small caps

Script `07_subamostra_small_caps.R`. Literatura (Wermers 1999,
Chen-Hong-Stein 2002) documenta efeitos mais fortes em ações menores/menos
líquidas. Testei FIT, demanda agregada revelada e herding restritos ao
terço de ativos com menor valor total mantido pelos fundos (proxy de
tamanho/liquidez, 171 de 513 ativos).

Nenhum melhora: todos os p do Fama-MacBeth entre 0,22 e 0,82. Amostra
menor (menos fundos cobrindo small caps) também reduz poder estatístico,
então isso não é evidência forte contra a hipótese, mas não ajudou.

**Veredito: REJEITADO.**

## Candidato #11: E[FIT] — fluxo esperado em vez de realizado (Lou 2012, construção fiel)

Script `08_efit_fluxo_esperado.R`. A pesquisa apontou que Lou (2012) usa
fluxo **esperado** (previsto por desempenho passado do fundo), não fluxo
realizado como o candidato #1 usou — argumento: só a parte previsível do
fluxo pode gerar retorno futuro sem ser tautológica. Construído: 1º estágio
prevê `flow_aum` de cada fundo a partir do retorno acumulado 12m (só com
dado de treino), reconstrói FIT com o fluxo previsto.

**Resultado mais interessante até agora, mas ainda não é "achado":** R²_OOS
linear positivo em quase todos os horizontes (h=1: 0,47%; h=3: 0,62%; h=6:
0,52% — o único candidato com R² OOS consistentemente positivo). MAS o
Fama-MacBeth do mesmo sinal/horizonte não confirma (p entre 0,21 e 0,95) —
ou seja, o modelo erra um pouco menos em média (R² melhora), mas isso não
vira uma estratégia long-short executável (o spread entre quintis não é
sistematicamente positivo). Interpretação provável: a melhora de ajuste
vem de acertar melhor o NÍVEL médio de retorno no período de teste, não de
discriminar corretamente quem vai subir vs. cair — não é sinal
direcional aproveitável.

## Candidato #12: FIT suavizado (média móvel 3 meses)

Mesmo script, parte (K). Lou explica parte do sucesso do FIT por agregação
cancelar ruído idiossincrático entre fundos; testei se suavizar no TEMPO
(não só entre fundos) ajuda. h=3 Fama-MacBeth: t=2,10, p=0,048 — passa de
5%, mas é 1 resultado marginal entre agora ~80 especificações testadas
(a essa taxa, esperaríamos ~4 "significativos" por puro acaso). Os outros
4 horizontes do mesmo sinal (h=0,1,6,12) não são significativos (p entre
0,49 e 0,81) — um sinal real deveria mostrar padrão mais consistente entre
horizontes vizinhos, não 1 de 5 isolado.

**Veredito dos dois: não confirmados.** E[FIT] é o candidato mais
promissor testado até agora (única melhora de R² OOS consistente), mas
não produz estratégia executável — vale nota como "pista pra investigar
mais" em vez de "achado", se decidirmos continuar.

## Candidato #13: Crowding (proxy sem dado de ADV, Chincarini et al. 2026)

Script `09_crowding_e_comomentum.R`, parte (L). Sem dado de volume/liquidez
diária (não temos), usei HHI de posse entre os fundos como proxy grosseira
de "dificuldade de sair da posição". R²_OOS positivo e consistente (h=1:
0,15%; h=3: 0,13%; h=6: 0,29% — segundo candidato com R² OOS
consistentemente positivo, depois do E[FIT]), mas Fama-MacBeth não
confirma (melhor caso h=1: p=0,085, não passa de 5%).

**Checagem específica do mecanismo de crash** (crowding pré-COVID prediz
queda maior no crash de fev-abr/2020, como a teoria prevê): achei o
**sinal oposto** ao esperado — crowding em jan/2020 correlaciona
POSITIVO com retorno fev-abr/2020 (corr=0,148, p=0,026, n=224) — ativos
mais "crowded" (concentrados) tiveram retorno relativamente MELHOR no
crash, não pior. Contradiz o mecanismo de risco de cauda da literatura
original (pode ser peculiaridade do episódio único, n=1 evento, não dá
pra generalizar de um só crash).

**Veredito: REJEITADO**, com nota de que é (com E[FIT]) um dos únicos 2
candidatos com R² OOS positivo consistente — mas sem long-short executável
e com o teste de mecanismo específico saindo ao contrário do esperado.

## Candidato #14: Comomentum (Lou & Polk) — INVESTIGADO A FUNDO, CONFIRMADO COMO ARTEFATO

Script `09_crowding_e_comomentum.R` parte (M) + diagnóstico dedicado em
`10_diagnostico_comomentum_h6.R`. Este era o candidato com a evidência mais
forte da literatura (~9%/ano, identificação quase-causal) — adaptado pra
dado mensal (a literatura original usa retorno semanal pra estimar a
correlação, limitação já sinalizada pela pesquisa).

**Primeira leitura (parecia promissor):** h=6, Fama-MacBeth t=2,21,
p=0,041 — passa de 5%. Mas a magnitude (spread médio +6,08 p.p./MÊS) é
implausível — anualizado, isso seria bem mais que 100%/ano, incompatível
com qualquer estratégia real conhecida na literatura.

**Investigação dedicada revelou por que:** os "quintis" de comomentum
dentro do decil de maior momentum têm entre **1 e 13 ações por mês** —
com grupos desse tamanho (às vezes 1 ação só!), a "média do quintil" é
dominada por retornos idiossincráticos de ativos individuais, não por um
padrão sistemático. Um único mês (set/2021) contribuiu +43,6 p.p. de
spread sozinho. Excluir os meses do crash da COVID (fev-jun/2020) NÃO
resolve (spread continua ~7,25pp/mês, p=0,05) — confirmando que o
problema é tamanho de amostra insuficiente (poucas ações brasileiras no
top decil de momentum, painel mensal com poucos meses), não um episódio
específico.

**Veredito: REJEITADO, com confiança alta de que é artefato, não sinal
real.** Este é o caso mais claro de "quase caímos numa cilada" da rodada —
registrado com detalhe porque quase foi reportado como achado positivo.
Lição: sempre que um resultado "significativo" tiver magnitude implausível
(muito maior que qualquer estratégia real documentada), investigar
composição da amostra antes de acreditar — não basta olhar o p-valor.

## Candidato #15: CIO Peer Momentum (Ying 2024, JFE) — O MAIS PROMISSOR ATÉ AGORA, mas com ressalvas reais

Scripts `11_cio_peer_momentum.R`, `13_diagnostico_cio_peer_momentum.R`,
`14_cio_robustez_periodo.R`. Mecanismo: ações com donos institucionais em
comum (medido via cosine similarity de posição em R$ entre pares de
ações, ponderado por AUM dos fundos) exibem difusão gradual de informação
— retorno da "vizinhança" de ownership comum hoje prediz retorno da
própria ação no mês seguinte. Frequência mensal nativa (diferente do
Comomentum, que exigia dado semanal).

**Passou em todas as checagens que derrubaram os outros candidatos:**
- Amostra saudável (mediana 46 ações por quintil-mês, não degenerada).
- h=1: R²_OOS=1,55% (maior de toda a busca), Fama-MacBeth t=2,51, p=0,021.
- **Sobrevive controlando pelo retorno PRÓPRIO da ação no mesmo mês**
  (teste decisivo contra a hipótese de comovimento mecânico, ressalva de
  Burt & Hrdlicka 2021): coeficiente do peer_ret quase não muda
  (0,113→0,105), e o retorno próprio perde toda significância quando os
  dois entram juntos (t=0,86, p=0,39) — ou seja, peer_ret carrega
  informação que o retorno próprio da ação NÃO capta.
- Robusto a excluir o mês com amostra mais fina (spread quase idêntico).
- Direção consistente: 71% dos meses (15 de 21) têm spread positivo.

**Ressalvas honestas que impedem chamar isso de "achado" ainda:**
- Só significativo em h=1; some em h=3 (p=0,46), h=6 (p=0,65), h=12
  (p=0,65) — compatível com a teoria (difusão de informação se resolve
  rápido), mas também compatível com sinal frágil.
- Os 2 meses que mais contribuem pro spread (out/2020: +30,6pp; fev/2020:
  +21,6pp) caem os dois em 2020 — o ano do crash/recuperação da COVID,
  de volatilidade muito acima do normal.
- **Excluindo 2020 inteiro (testando só 2021, 12 meses):** spread cai de
  4,27pp/mês pra 2,50pp/mês, e o t cai de 2,38 pra 2,13 — **p=0,057, não
  passa mais de 5%** (embora continue na mesma direção e ordem de
  grandeza).
- Magnitude bruta (2,5-4,3 pp/mês = ~35-70%/ano) é grande demais pra ser
  uma estratégia real sem erosão por custo de transação/impacto de
  mercado/capacidade — mesmo se estatisticamente genuíno, não significa
  que dá pra capturar isso na prática sem fricções.
- Só 21 meses de teste no total — qualquer corte de subamostra
  (2020 vs. 2021) tem pouquíssimo poder estatístico pra distinguir "o
  efeito é mais fraco fora de crise" de "o efeito não existe fora de
  crise, era só ruído de 2020".

**Avaliação honesta:** este é o único candidato, entre ~16 testados e
~100 especificações, que sobrevive a MÚLTIPLAS checagens de robustez
sérias (controle por retorno próprio, exclusão de mês degenerado,
consistência direcional). Não é definitivo — a queda pra p=0,057 sem
2020 é um sinal de alerta real, não só "quase lá". Mas é qualitativamente
diferente de tudo mais testado: não morreu ao primeiro exame rigoroso.

## Candidato #16: Ownership × Momentum (interação condicional, China A-shares 2023)

Script `12_ownership_x_momentum.R`. Hipótese testada: momentum é mais
forte em ações com alta propriedade institucional (evidência de mercado
chinês). **Resultado: o oposto do hipotetizado**, e inconsistente entre
horizontes. Interação negativa e significativa em h=3 (p=0,037 — momentum
mais FRACO com ownership alta), positiva e significativa em h=12
(p=0,022 — sinal invertido). Testando por grupo: o spread de momentum é
maior e mais significativo no terço de BAIXA ownership institucional
(h=1: +4,53pp/mês, p=0,028) do que no de ALTA (h=1: +1,80pp/mês, p=0,18)
— literalmente oposto ao paper chinês.

Interpretação possível (não hipotetizada de início, mas plausível): menos
capital institucional sofisticado em ações de baixa ownership pode
significar menos arbitragem corroendo o efeito momentum ali — consistente
com a literatura geral de "limits to arbitrage" (McLean-Pontiff), só que
não é o mecanismo específico que motivou o teste.

**Veredito: inconclusivo/inconsistente entre horizontes — não confirma a
hipótese original, achado secundário interessante mas não robusto o
suficiente pra reportar como candidato forte.**

## Validação aprofundada do CIO Peer Momentum: controle por fatores + custo de transação

Script `15_cio_validacao_fatores_custos.R`.

**(P) Fama-MacBeth com controles simultâneos** (regressão cross-sectional
mês a mês de retorno(t+1) ~ peer_ret + tamanho + beta + retorno próprio,
coeficientes padronizados, 24 meses de teste): peer_ret **sobrevive**
controlando pelos 3 ao mesmo tempo — média do coeficiente t=2,34, p=0,028.
Tamanho também sai significativo (t=-2,36, menor porte prediz retorno
maior — efeito-tamanho plausível). Beta e retorno próprio NÃO são
significativos nessa regressão conjunta. **Não temos dado de
valor/book-to-market pra controlar** (não existe no painel de holdings) —
limitação registrada.

**(Q) Custo de transação:** spread bruto médio 4,15pp/mês (~63%/ano).
Mesmo com custo agressivo de 150bps round-trip por perna (bem acima do
custo típico institucional no Brasil), sobra 1,15pp/mês líquido
(~15%/ano). Com custo mais realista (30-50bps), sobra 45-52%/ano.

**Avaliação final, sendo direto:** este é o único candidato de 16 que
passou em TODOS os testes de robustez aplicados — controle por retorno
próprio, exclusão de mês degenerado, regressão multivariada com 3
controles simultâneos, custo de transação agressivo. Isso é
qualitativamente diferente de tudo mais testado nesta exploração.

**Mas duas coisas pesam contra tratar isso como pronto pro TCC:**
1. A magnitude (45-63%/ano bruto, 15-52%/ano líquido de custo) é grande
   demais pra ser plausível como edge persistente e escalável — as
   melhores anomalias documentadas na literatura acadêmica raramente
   passam de 15-20%/ano BRUTO. Um resultado desse tamanho, historicamente,
   é sinal de amostra pequena/período atípico mais do que de uma
   descoberta genuína dessa magnitude.
2. Só 21-24 meses de teste (2020-2021), e o resultado enfraquece bastante
   (p=0,057, não passa de 5%) quando restrito só a 2021 — não dá pra
   descartar que é em boa parte um efeito do período de volatilidade
   extrema da COVID, não um padrão estrutural do mercado brasileiro.

**Recomendação honesta:** isto é um achado candidato promissor o
suficiente pra registrar e levar ao orientador como pista de pesquisa
futura — não uma estratégia validada pronta pra reportar como "sinal de
lucro" encontrado. Precisaria de mais anos de dado (fora do escopo do
painel atual, 2016-2021) pra testar se sobrevive fora do período de
crise, e controle por valor/book-to-market (dado indisponível aqui) antes
de qualquer decisão de levar isso a sério como resultado.

## Contagem corrida de especificações testadas: ~115

(pra qualquer achado futuro "significativo" ser levado a sério, precisa
sobreviver considerando essa base de comparação — um único p<0,05 isolado
não basta.)

Verificação adicional: a construção de fluxo usada (`flow_aum`) vem de
captação/resgate diário reportado diretamente pela CVM, não de um
resíduo de variação de patrimônio — **não tem a contaminação mecânica
que Wardlaw (2020) identificou** na literatura padrão de fire
sales/flow-induced trading. Ou seja, os resultados nulos não são
explicados por esse artefato específico; parecem ser resultados nulos
genuínos.
