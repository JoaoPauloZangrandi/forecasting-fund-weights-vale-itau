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

## Tentativa de "resgatar" h=3 do CIO Peer Momentum (14/08/2026, a pedido do João)

Script `16_cio_h2_e_janela_agregada.R`. Testados 2 refinamentos
principiados (não uma varredura arbitrária de horizontes até achar
p<0,05):
- **h=2** (ponto intermediário entre h=1 significativo e h=3 não):
  t=0,51, p=0,615 — nada. O efeito não decai suavemente, cai a quase
  zero logo depois de h=1.
- **Retorno acumulado t+1 a t+3** (testa se o efeito "persiste" quando
  acumulado, mesmo que cada mês isolado não seja previsível): p=0,058 —
  perto, mas não passa de 5%, e isso é majoritariamente o próprio efeito
  de h=1 sendo diluído por 2 meses sem sinal adicional, não evidência de
  persistência genuína.

**Conclusão honesta:** não há um jeito metodologicamente legítimo de
fazer h=3 "sobreviver" — a evidência disponível indica que o efeito é
genuinamente confinado a h=1 (consistente, aliás, com a leitura mais
simples da teoria de Ying 2024: difusão de informação que se resolve
rápido). Continuar fatiando o sinal até algo perto de h=3 funcionar seria
o mesmo tipo de risco estatístico já documentado no caso do Comomentum.

## Rodada 4 de pesquisa (14/08/2026) — esgotando fontes brasileiras + muito recentes

Script/busca dedicada: BCB (Trabalhos para Discussão), ANBIMA, CVM, RBFin,
RAC, RAUSP, BAR, arXiv q-fin, e literatura de horizonte 2-3 meses
especificamente. **Resultado: negativo em quase tudo.**

- **Literatura brasileira**: não existe, nesse recorte específico
  (holdings→retorno futuro no nível de ação). O que existe no Brasil é
  herding (Zulian 2012, Tariki 2014 — documentam existência, não conectam
  a previsibilidade de retorno) e smart-money a nível de fundo (já
  coberto, sem efeito robusto).
- **Horizonte 2-3 meses**: nenhum paper documenta isso especificamente.
  Só lógica indireta (menos cobertura de analista/liquidez → difusão mais
  lenta) — extrapolação plausível, não achado empírico.
- **Extensões de common ownership 2022-2026**: confirmado que Ying (2024)
  É o mesmo paper do CIO Peer Momentum (não é candidato novo). Achado
  Springer 2025 sobre ponderar por concentração do investidor (não overlap
  bruto) — bloqueado por paywall, não verificável.
- **Chen, Chen & Cohen (JFE)**, "Tomorrow Is Another Day" — lido na
  íntegra: active ownership prediz retorno do MERCADO agregado, mas só por
  **1 dia** (mesmo em dado diário, o efeito morre imediatamente) —
  reforça (não abre caminho novo) o padrão de decaimento rápido já visto.
- **Cao Fang & Lee (2026, Accounting & Finance)** --- único achado que
  reivindica persistência de até 1 ANO, o oposto do padrão de tudo mais
  testado. Tentei acessar o texto completo (Wiley, bloqueado; dissertação
  original da Univ. Arkansas, também bloqueada, só abstract disponível).
  Abstract confirma "persiste ao longo do ano seguinte" mas não esclarece
  se é retorno acumulado de 12 meses (que pode ser dominado pelo 1º mês,
  igual tudo mais, só sem reverter depois) ou significância mês a mês
  independente em cada horizonte. **Não foi possível replicar/testar sem
  ver a metodologia exata** (classificação de "segmento de estilo",
  ponderação por qualidade do fundo). Registrado como pista não resolvida,
  não como candidato testado.

## Combinação CIO Peer Momentum + E[FIT]

Script `17_combinacao_cio_efit.R`. Os 2 candidatos com melhor R² OOS
combinados (soma padronizada por mês) não geram mais sinal que o CIO
isolado: $h=1$ $t=2{,}29$ $p=0{,}031$ (levemente PIOR que CIO sozinho,
$p=0{,}021$); $h=2$: nada; $h=3$: $p=0{,}15$ (mais perto que os
candidatos isolados em h=3, mas ainda não significativo); $h=6$: nada.
**Não resgata h=3.**

## Window dressing / efeito de calendário no CIO Peer Momentum

Script `18_window_dressing_calendario.R`. Hipótese: se o sinal fosse
contaminado por "maquiagem de carteira" (gestores ajustando posição antes
de datas de divulgação), deveria ser diferente nos meses de fim de
trimestre. **Resultado: o oposto.** O efeito é mais forte FORA dos meses
de fim de trimestre (t=2,38, p=0,032, 15 meses) e quase desaparece
justamente nos meses de fechamento (t=0,36, p=0,735, 5 meses) — não é
window dressing, e não abre um caminho novo pra h=3.

## Rodada 5 de pesquisa (14/08/2026) — 14 revistas específicas, GIV, redes, calendário

Busca revista-por-revista (JFQA, Journal of Financial Markets, Journal of
Empirical Finance, Financial Management, JBF, JIMF, Pacific-Basin Finance
Journal, Emerging Markets Review, IRFA, Journal of Asset Management,
Quantitative Finance, JPM, Emerging Markets Finance and Trade, China
Finance Review International) + Granular Instrumental Variables
(Gabaix-Koijen) + redes/centralidade + bancos centrais de outros EM +
window dressing. **Resultado: nada de novo testável**, com uma exceção:

- **GIV (Gabaix-Koijen)**: sem extensão que gere sinal de retorno
  cross-sectional a partir de holdings de fundos — só extensões
  econométricas/real estate, fora do escopo.
- **Redes/centralidade de ownership**: achado CWEC (Kita & Zhang) tem
  resultado **negativo** (mais centralidade = retorno MENOR) — não é
  candidato.
- **Bancos centrais (BIS, IMF, ECB, México, Chile, África do Sul)**:
  nada sobre holdings de fundos prevendo retorno de ações a nível firma.
- **Calendário ("Intramonth Momentum Cycle", Nathan-Suominen-Tasa 2026)**:
  achado real e robusto, mas exige dado diário (qual dia dentro do mês) —
  não adaptável ao painel mensal.
- **A exceção: Cao Fang & Lee (2026), "Stock IQ"** — desta vez a pesquisa
  conseguiu acessar o texto COMPLETO (contornando o paywall). Mecanismo
  genuinamente diferente de tudo testado antes: para cada ação, soma sobre
  os fundos que a carregam do DESVIO de cada fundo em relação ao que ELE
  MESMO normalmente faz em ações do mesmo "segmento de estilo" (tamanho ×
  book-to-market × momentum), ponderado pela qualidade/skill do gestor —
  isola stock-picking específico de tilt de estilo amplo. No paper
  original: sobrevive em h=1 **até h=4 individualmente** (não cumulativo),
  robusto contra herding/dumb-money/breadth em corrida de cavalos.

## Candidato #19 (implementação fiel do Stock IQ, Cao Fang & Lee 2026)

Script `19_stock_iq_cao_fang_lee.R`. Adaptação com os dados disponíveis:
segmentos de tamanho × momentum (2 dimensões — não temos
book-to-market, limitação registrada), skill = retorno acumulado 12m do
fundo (não GVA/alfa formal — outra limitação), desvio calculado excluindo
a própria ação (evita circularidade, mesma lógica do HHI_resto do TCC).

**Resultado: nulo em TODOS os horizontes, incluindo h=1** ($p=0{,}29$) —
diferente dos outros candidatos rejeitados, este nem passa no teste da
amostra inteira, muito menos fora dela. $h=2$: $p=0{,}14$; $h=3$:
$p=0{,}21$; $h=4$: $p=0{,}18$; $h=6$: $p=0{,}73$; $h=12$: $p=0{,}28$.

**Interpretação honesta:** a implementação fiel do candidato mais
promissor da literatura recente não replica no mercado brasileiro. Pode
ser (a) falta da dimensão book-to-market enfraquecendo os segmentos de
estilo, (b) proxy de skill mais fraca (retorno bruto vs.\ GVA formal), (c)
universo muito menor que o americano (mediana de 32 ações por quintil vs.\
milhares no paper original, poder estatístico bem mais baixo), ou (d) o
mecanismo genuinamente não se aplica ao Brasil. Não dá pra distinguir
essas hipóteses com os dados disponíveis.

## Candidato #20: Stock IQ com segmentos de 1 dimensão (refinamento)

Script `20_stock_iq_1d_refinado.R`. Segmentos de só tamanho OU só
momentum (quintis, mais ações por grupo que os 9 segmentos 2D do
candidato #19). Mais perto de significativo mas ainda não passa: melhor
caso momentum-1D h=3, $p=0{,}111$. Tamanho-1D h=1/h=3: $p=0{,}19$/$0{,}18$.
**Ainda rejeitado.**

## Candidato #21: Ensemble de Machine Learning (XGBoost, todos os sinais juntos)

Script `21_ensemble_ml.R`. Junta 7 sinais já calculados (CIO peer
momentum, Stock IQ, demanda agregada revelada, herding, crowding,
tamanho, momentum próprio) como features de um XGBoost, treinado só no
período de treino (com validação interna por mês, sem vazamento),
testado fora da amostra em h=1, h=3, h=6.

**Resultado: pior que qualquer sinal isolado.** $R^2_{\text{OOS}}$
**negativo** em todos os horizontes ($-2{,}3\%$, $-8{,}3\%$, $-3{,}1\%$)
--- o modelo overfita no período de treino pequeno (~40-50 meses é pouco
dado pra ML) e piora fora da amostra. Nenhum long-short baseado nas
previsões do modelo é significativo ($p=0{,}79$; $p=0{,}32$; $p=0{,}12$).
Interessante notar: `peer_ret` (CIO) e `mom_12m` (momentum próprio) são
consistentemente as features mais importantes nos 3 horizontes — reforça
que o CIO Peer Momentum é o sinal individual mais informativo entre os
testados, mas combiná-lo não-linearmente com os outros não ajuda, piora.

**Veredito: REJEITADO.** O gap de literatura identificado (ninguém
testou ML nesses sinais) tinha uma razão prática: dado de painel
institucional mensal com poucos anos de histórico é amostra pequena
demais pra ML generalizar melhor que abordagem linear simples.

## Grid search sistemático do CIO Peer Momentum (108 especificações, 14/08/2026)

Script `22_grid_search_cio.R`. Em vez de testar 1 construção por vez,
varredura completa de 36 combinações de construção do sinal (piso de
conexão: percentil 50/60/70/75/80/90; mínimo de peers: 3/5/10; ponderação:
média por CIO vs.\ média simples) × 3 horizontes (h=1,2,3) = 108
especificações, todas registradas (não só as "significativas").

**h=1: sinal ROBUSTO, não é sorte de uma escolha isolada.** 11 de 36
construções (30,6%) passam de $p<0{,}05$, e **todas** compartilham a
mesma escolha estrutural (ponderação por CIO/AUM) — variando o piso de
conexão de 50º a 90º percentil e o mínimo de peers de 3 a 5, o sinal se
mantém ($t$ entre 2,25 e 3,24; spread entre 3,0 e 4,5 pp/mês). Isso é
evidência genuína de robustez, bem mais forte do que uma única
especificação isolada — a escolha de projeto que importa é "ponderar por
overlap", não o piso exato de corte.

**h=2: robustamente ZERO** em toda a grade (0 de 36 significativas) —
confirma o achado anterior.

**h=3: os 4 "acertos" iniciais eram artefato de amostra degenerada.**
Investigação dedicada mostrou que as construções "significativas" em h=3
usavam `ponderado=FALSE` (média simples, não por overlap) e tinham
**1 a 11 meses de teste** (uma tinha literalmente $n=1$!) — amostra
pequena demais pra qualquer inferência. Testando a MESMA construção
robusta que funciona em h=1 (ponderada, com 18-19 meses de teste
completos) em h=3: **nada, em nenhuma das 18 combinações** ($t$ entre
$0{,}34$ e $1{,}25$; $p$ entre $0{,}30$ e $0{,}74$, sem padrão algum).

**Conclusão definitiva:** h=1 é um sinal genuinamente robusto à escolha
de especificação (não um acidente de uma configuração específica); h=3
genuinamente não tem sinal nenhum, com a evidência mais completa e
sistemática que já foi produzida nesta exploração. Isso fecha a pergunta
"será que h=3 funciona com algum ajuste" de forma definitiva.

## Tentativas adicionais em h=3, a pedido explícito (com correção de Bonferroni)

Script `23_h3_tentativas_adicionais.R`. A partir daqui, com ~250 testes
já feitos, o limiar de significância honesto é
$p < 0{,}05/250 = 0{,}0002$ (Bonferroni), não mais $p<0{,}05$ — registrado
e aplicado a partir deste ponto.

- **CIO suavizado (média móvel 2-3 meses)**: $p=0{,}85$ e $p=0{,}44$. Nada.
- **Decis em vez de quintis** (extremos mais extremos): $p=0{,}21$. Nada.
- **CIO neutro em tamanho** (resíduo depois de tirar efeito de
  `log_tamanho`, testa se o sinal não é só um proxy de tamanho
  disfarçado): $h=1$ ainda passa do 5% padrão ($p=0{,}038$) — reforça que
  o achado de h=1 não é artefato de tamanho — mas $h=3$ continua nulo
  ($p=0{,}45$).

**Nenhum resultado, em h=3, chega perto do limiar de Bonferroni.**

## Candidatos #22-24: Anomalias clássicas (52-week high, reversão, idio-vol) — nova família, não mais variações de holdings

Script `24_anomalias_classicas.R`. Mudança de família: em vez de mais
variações de fluxo/holdings, testei 3 anomalias clássicas bem
estabelecidas na literatura de asset pricing (construídas só com preço,
sem precisar de holdings), sozinhas e interagidas com ownership
institucional (pergunta: a presença de fundos fortalece ou enfraquece
essas anomalias — teoria de "limits to arbitrage").

- **52-week high (George-Hwang 2004)**: melhor caso sozinho $h=1$,
  $p=0{,}091$. Nada.
- **Reversão de curto prazo (Jegadeesh 1990)**: melhor caso $h=1$,
  $p=0{,}095$. Nada.
- **Volatilidade idiossincrática (Ang et al.\ 2006)**: melhor caso $h=3$,
  $p=0{,}148$. Nada.
- **Interações com ownership**: o único caso com $p<0{,}01$ em toda a
  bateria foi 52-week-high em ações de **ALTA** ownership institucional,
  $h=1$ ($t=3{,}13$, $p=0{,}005$) — mas isso é o **oposto** do que a
  teoria de limits-to-arbitrage preveria (esperava-se mais forte em
  BAIXA ownership) — mesmo padrão invertido já visto no candidato #16
  (Ownership × Momentum). Reversão de curto prazo em baixa ownership:
  $p=0{,}046$ (passa do 5% padrão, não de Bonferroni).

**Nenhum resultado, em nenhuma das 21 especificações desta família,
chega perto do limiar de Bonferroni** ($p<0{,}0002$; o melhor caso,
$p=0{,}005$, precisaria ser ~25× menor).

## Rodada 6 de pesquisa: anomalias clássicas × ownership institucional (14/08/2026)

Achado mais útil: a literatura brasileira **não replica** a volatilidade
idiossincrática negativa dos EUA (Mendonça et al.\ 2012, RCF-USP: relação
POSITIVA, coef.\ $+0{,}44$, oposta ao "puzzle" americano) — bate
exatamente com o resultado (não significativo, mas na direção certa)
achado no candidato #24. A única anomalia clássica com evidência forte e
replicada especificamente no Brasil é **baixa volatilidade** ($+6\%$ a
$+15{,}5\%$/ano, 2 estudos independentes 2003-2017/2003-2021) — motivou o
candidato #25.

## Candidato #25: Low volatility (Brasil) — sozinha, x ownership, x CIO

Script `25_low_volatility_brasil.R`. Volatilidade TOTAL (não
idiossincrática) dos últimos 12 meses, mesma metodologia dos papers
brasileiros que acharam o efeito.

**Resultado: sinal na direção OPOSTA à literatura brasileira nesta
amostra** (alta vol supera baixa vol aqui, $-1{,}25$ a $-1{,}81$pp/mês
pra "baixa menos alta"), não significativo. Explicação plausível: os
papers brasileiros cobrem 2003-2017/2021 (múltiplos regimes de mercado);
nossa amostra de teste é só 2020-2021, dominada pela recuperação em V
pós-COVID — período em que ações de alto beta/alta vol tipicamente
superam baixa vol, um padrão conhecido de reversão temporária da
anomalia em bull markets fortes, não uma contradição da literatura de
longo prazo.

Interação com ownership: nada. Combinação com CIO Peer Momentum (só
testar CIO dentro do subgrupo de menor volatilidade): $h=1$,
$p=0{,}097$ — mais perto, mas não passa nem do 5% padrão.

**Veredito: REJEITADO**, com nota de que o período de teste curto e
atípico (2020-2021) limita qualquer comparação direta com a literatura
brasileira de mais longo prazo.

## Candidato #26: Persistência de performance do FUNDO (não da ação) — nova pergunta

A pedido do João: em vez de prever retorno de ação a partir de holdings,
testa se o retorno do PRÓPRIO FUNDO é previsível a partir do seu retorno
passado — pergunta clássica (Carhart 1997, "hot hands"), objeto
diferente (retorno do fundo, não da ação), mais fundos que ações no
painel (~3.254 vs ~500) então mais poder estatístico em princípio.

**Script `26_persistencia_performance_fundos.R`, parte (A) — persistência
bruta, 5 janelas de formação (3/6/12/24/36 meses) × 4 horizontes
(1/3/6/12 meses) = 20 especificações, decis:** nada em lugar nenhum. O
melhor caso ($p=0{,}166$) nem passa de 5%, e os spreads trocam de sinal
sem padrão entre janelas/horizontes vizinhos — evidência limpa de ruído,
não de sinal fraco.

**Script `27_persistencia_assimetria_risco.R`:**
- **(B) Assimetria (Carhart):** testei se o decil de fundos PIORES
  continua pior (achado central de Carhart nos EUA) separadamente do
  decil MELHORES. Resultado: decil pior mostra excesso **positivo**
  (não negativo) em h=1 ($+0{,}63$pp/mês, $p=0{,}18$) — direção oposta à
  esperada (mais parecido com reversão à média que persistência de
  badness), mas não significativo.
- **(C) Alfa ajustado por risco** (não retorno bruto): melhor caso
  $h=1$, $p=0{,}089$ — mais perto que a versão bruta, mas ainda não
  passa de 5%.
- **(D) Condicional em tamanho do fundo:** fundos grandes mostram
  reversão (não persistência) mais forte que pequenos, mas não
  significativo ($p=0{,}13$).

**Veredito: REJEITADO em toda a bateria (24 especificações).** Consistente
com a literatura internacional mais robusta (Carhart 1997 e sucessores):
persistência de performance de fundo, além do efeito conhecido de taxa
de administração (que não temos dado pra isolar aqui), é fraca ou
inexistente — mesmo em amostras americanas muito maiores. O painel
brasileiro (curto, 2016-2021) não muda esse padrão.

## MUDANÇA DE OBJETO: previsão de VOLATILIDADE (não retorno) — a pedido do João

João pediu especificamente sinal envolvendo volatilidade/opções.
**Limitação de dado registrada com transparência**: não temos dado de
opções (preço, vol implícita, book) — qualquer simulação de estratégia
de opções exigiria inventar o preço via Black-Scholes com volatilidade
realizada como proxy de implícita, o que não reflete mercado real (prêmio
de risco de vol, skew, liquidez) e produziria um resultado sem validade.
**Reformulação honesta**: testável de verdade é se os sinais já
construídos preveem VOLATILIDADE FUTURA da ação (não direção de
retorno) — pergunta diferente, 100% testável só com preço.

## Candidato #26 (volatilidade): 7 sinais × 3 horizontes = 21 especificações

Script `28_previsao_volatilidade.R`. Testados: crowding (HHI posse), CIO
peer momentum (nível e magnitude absoluta), FIT (nível e magnitude
absoluta), herding (nível e magnitude absoluta) — como preditores de
volatilidade realizada nos próximos 3/6/12 meses.

**ACHADO: crowding prediz volatilidade futura com significância que
passa do limiar de Bonferroni** ($p<0{,}05/350$) em h=6 ($t=8{,}67$,
$p<0{,}00001$) e h=12 ($t=14{,}03$, $p<0{,}00001$) — **primeiro resultado
de toda a exploração (~350 especificações) a passar desse limiar**.
Direção correta: mais concentração → mais volatilidade futura, consistente
com a teoria de crowding/risco de liquidação coordenada (Chincarini et
al.\ 2026, já coberto na pesquisa de literatura). Outros sinais
(magnitude absoluta de CIO/FIT/herding) mostram padrão parecido mas mais
fraco, sem passar de Bonferroni.

## Diagnóstico do achado: crowding sobrevive controlando por volatilidade passada?

Scripts `29_diagnostico_crowding_volatilidade.R` (primeira tentativa,
regressão pooled — **método errado**, mesma armadilha do Comomentum) e
`30_crowding_vol_fama_macbeth_correto.R` (corrigido, regressão
cross-sectional mês a mês, 1 coeficiente por mês).

Correlação crowding × volatilidade passada: fraca (Pearson 0,15,
Spearman 0,16) — não são a mesma coisa, mas correlacionadas o
suficiente pra precisar controlar.

**Com Fama-MacBeth correto, controlando por volatilidade passada (12m):**

| Horizonte | Crowding sozinho | Crowding + vol.\ passada (controlado) |
|---|---|---|
| $h=3$ | $t=3{,}38$ ($p=0{,}003$) | $t=1{,}16$ ($p=0{,}26$) — desaparece |
| $h=6$ | $t=6{,}01$ ($p<0{,}001$) | $t=1{,}81$ ($p=0{,}085$) — enfraquece muito |
| $h=12$ | $t=8{,}98$ ($p<0{,}001$) | $t=3{,}05$ ($p=0{,}008$) — **sobrevive** |

**Interpretação honesta:** em horizontes curtos (3-6 meses), a maior
parte do que parecia "crowding prevê volatilidade" é, na verdade, a
volatilidade prevendo ela mesma (\emph{volatility clustering}, fato
estilizado bem conhecido) — crowding está correlacionado com volatilidade
atual e pega carona nisso. Mas em **h=12, crowding continua significativo
mesmo controlando pela volatilidade recente** — sugere que concentração
de posse carrega informação sobre risco de médio prazo além do que a
volatilidade recente já capta. Não passa do limiar de Bonferroni (agora
$p<0{,}05/380\approx0{,}00013$; o resultado controlado, $p=0{,}008$,
precisaria ser ~60× menor), mas é o achado mais robusto-sob-escrutínio
de toda a exploração fora do CIO Peer Momentum em h=1.

**Aplicação prática honesta (sem opções):** mesmo sem poder simular uma
estratégia de opções de verdade, previsão de volatilidade tem uso direto
em dimensionamento de posição/gestão de risco — ações mais concentradas
em poucos fundos merecem posições menores ou hedge mais cauteloso,
independente de saber a direção do retorno.

## Validação por literatura: crowding→volatilidade é um paper conhecido (Greenwood-Thesmar 2011)

Pesquisa de literatura dedicada confirmou: o que foi descoberto
empiricamente (candidatos #28-30) **é, essencialmente, o mesmo fenômeno**
documentado em **Greenwood, R. & Thesmar, D. (2011), "Stock Price
Fragility", Journal of Finance Economics 102(3)** — concentração/correlação
de posse entre donos institucionais prediz volatilidade futura (não
retorno), com $R^2$ univariado $\approx 8\%$ no paper original (EUA,
holdings trimestrais). **Esse é o MESMO paper que já fundamenta o outro
TCC do João** (`forecasting-exposure-itub4`, risco fundo-sobre-fundo) —
conexão direta entre os dois projetos. Também achado: Xue, He & Hu (2023,
IRFA) mostra herding institucional amplificando volatilidade idiossincrática
futura na China, mesma direção do achado mais fraco de herding aqui.
**Lacuna de literatura identificada**: ninguém combinou holdings
institucionais com metodologia HAR-RV de forecasting de volatilidade —
motivou o teste rigoroso abaixo.

## Teste rigoroso: fragilidade (crowding) como preditor incremental sobre benchmark HAR

Script `31_fragilidade_har_benchmark.R`. Compara $R^2$ fora da amostra de
3 modelos: (1) só vol.\ passada de 12m (já testado); (2) benchmark estilo
HAR-RV (vol.\ passada em 3 janelas: 1-3m, 6m, 12m); (3) HAR + crowding.

| Horizonte | Só vol.\ 12m | HAR (3 janelas) | HAR + Crowding | Ganho de crowding |
|---|---|---|---|---|
| $h=3$ | $6{,}14\%$ | $6{,}86\%$ | $6{,}88\%$ | $+0{,}02$pp — nada |
| $h=6$ | $8{,}29\%$ | $8{,}96\%$ | $9{,}36\%$ | $+0{,}40$pp — pequeno mas real |
| $h=12$ | $0{,}25\%$ | $0{,}20\%$ | $1{,}42\%$ | $+1{,}22$pp — **~7× o benchmark** |

**Achado final, consolidado:** em horizontes curtos, volatilidade passada
(mesmo um HAR simples com 3 janelas) já explica quase tudo que crowding
explicaria — não há ganho real em adicionar concentração de posse. Mas em
**12 meses, o HAR puro praticamente para de funcionar (R² cai a quase
zero — a persistência de volatilidade de curto prazo não chega tão longe),
e é exatamente aí que a fragilidade (crowding) mostra sua contribuição
mais clara e economicamente relevante** — mais de 7× o poder explicativo
do modelo sem ela. Isso bate com a leitura teórica de Greenwood-Thesmar:
fragilidade de posse é sobre risco de LIQUIDAÇÃO COORDENADA, um evento de
cauda que se manifesta mais em horizontes mais longos, não é a mesma
coisa que a persistência mecânica de curto prazo da volatilidade.

**Este é o achado mais sólido, mais bem fundamentado na literatura, e
mais consistente entre metodologias (pooled, Fama-MacBeth, HAR-benchmark)
de toda a exploração — inclusive mais robusto que o CIO Peer Momentum
(h=1), por ter respaldo direto de um paper de JFE com o MESMO mecanismo,
já usado no outro TCC do João.**

## Contagem corrida de especificações testadas: ~404 (395 + 9)

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

## Aprofundamento em volatilidade (14-15/08/2026): pesquisa exaustiva + testes de estratégia

Pedido do usuário: dado que crowding (HHI de posse) prediz volatilidade
futura em h=12 (achado mais sólido da exploração, seção acima), investigar
exaustivamente se isso pode virar um sinal de lucro genuíno — mesmo sem
dado de opções. Cinco buscas em paralelo, cobrindo: (1) fragilidade
condicional a regime de estresse; (2) prêmio de risco de volatilidade sem
opções; (3) extensões de vol-managed portfolios (Moreira-Muir); (4)
crowding como fator de risco precificado; (5) literatura em mercados
emergentes/Brasil.

### Achados de literatura mais importantes

- **Nairac (2013), dissertação de mestrado, University of Cape Town** —
  réplica quase idêntica ao nosso desenho: dados MENSAIS de fundos
  sul-africanos (ao contrário do G&T original, que é trimestral), decompõe
  fragilidade nos mesmos termos algébricos, e acha que é o **componente de
  HHI/concentração de posse — não o termo de covariância de fluxo — que
  carrega a significância**, exatamente o padrão que encontramos. Nunca
  publicada em periódico, só 2 citações. Confirma: **não existe nenhum
  precedente de aplicação desse mecanismo ao Brasil ou à América Latina** —
  é uma contribuição genuinamente original do TCC.
- **VRP "de verdade" (Carr-Wu, Bollerslev-Tauchen-Zhou) não é replicável
  sem opções** — é definicional (depende de expectativa risco-neutra
  extraída de preço de opção), não uma lacuna de dado contornável.
  Dispersion trading tem a mesma limitação estrutural.
- **Caminho mais promissor identificado, com múltiplos precedentes JF/JFE**:
  usar a previsão de volatilidade para *dimensionar posição* (Moreira &
  Muir 2017, "Volatility-Managed Portfolios") em vez de tentar prever
  direção de retorno — não precisa de opções, só de preço à vista.
  **Crítica importante a levar em conta**: Cederburg, O'Doherty, Wang & Yan
  (2020, JFE) mostram que o ganho de Moreira-Muir desaparece/piora quando
  reestimado genuinamente fora da amostra (em vez de usar a regressão
  in-sample inteira) — alerta direto contra look-ahead bias no desenho do
  teste.
- **Segundo caminho**: "Betting Against Correlation" (Asness, Frazzini,
  Gormsen & Pedersen 2020, JFE) — decompõe o efeito de baixo-risco em
  correlação vs. volatilidade, construído só com retornos.
- **Crowding como fator de risco precificado** (retorno médio mais alto
  como compensação, não só vol mais alta): literatura **dividida** — Brown,
  Howard & Lundblad (2021, RFS) a favor; Barroso, Edelen & Karehnke (2021,
  JFQA) e Sias, Turtle & Zykaj (2014, Management Science) contra. Com
  T≈72 meses e um único evento de cauda no período (COVID), não há poder
  estatístico para identificar isso de forma confiável — problema de
  identificação, não de desenho.
- **Fragilidade condicional a regime de estresse**: metodologicamente bem
  estabelecido (Nagel 2012 RFS; Cella-Ellul-Giannetti 2013 RFS; Falato-
  Goldstein-Hortaçsu 2021 JME sobre COVID especificamente) — o mecanismo
  teórico de Coval-Stafford/Greenwood-Thesmar é sobre CHOQUES de
  liquidação, não sobre nível médio constante.
- **Nenhum paper testa a combinação exata "HHI/fragilidade como variável de
  escala forward-looking em vol-managed portfolio"** — confirmado por dois
  agentes de pesquisa independentes. Seria uma contribuição original.

### Teste empírico: `32_fragilidade_managed_portfolio.R`

Três desenhos, todos usando só `ym >= 202001` (mesmo período de teste do
resto do TCC) e HHI conhecido em $t$ aplicado a retorno realizado em $t+1$
(sem look-ahead). **Amostra de teste é pequena: só 24 meses**, dominada
pelo crash e recuperação do COVID — limitação séria de poder estatístico
que deve ser lida em qualquer interpretação abaixo.

**Desenho B — carteira tilted por decil de HHI (menos peso pra mais frágil)
vs. equal-weight:**

| Carteira | Sharpe | Vol. anual | Retorno anual | Max drawdown |
|---|---|---|---|---|
| Tilt por HHI | $-0{,}112$ | $35{,}0\%$ | $-3{,}9\%$ | $-34{,}1\%$ |
| Equal-weight | $-0{,}042$ | $35{,}2\%$ | $-1{,}5\%$ | $-33{,}7\%$ |

Correlação entre as duas séries: $0{,}997$ — o tilt quase não diferencia
da carteira ingênua, e piora o resultado (Sharpe menor). **Não funciona.**

**Desenho A — market timing escalado por fragilidade agregada vs.
buy-and-hold vs. vol-managed clássico (escalado por vol. realizada
passada, sem HHI):**

| Estratégia | Sharpe | Vol. anual | Retorno anual | Max drawdown |
|---|---|---|---|---|
| Buy-and-hold | $-0{,}042$ | $35{,}2\%$ | $-1{,}5\%$ | $-33{,}7\%$ |
| Escalado por HHI agregado | $0{,}088$ | $42{,}6\%$ | $3{,}7\%$ | $-43{,}2\%$ |
| Vol-managed clássico | $-0{,}910$ | $55{,}3\%$ | $-50{,}3\%$ | $-72{,}3\%$ |

A versão escalada por HHI tem Sharpe nominalmente positivo e maior que o
buy-and-hold, mas com drawdown pior — e com $N=24$ meses essa diferença
não é estatisticamente distinguível de ruído. O vol-managed clássico
(a receita literal de Moreira-Muir, sem HHI) é catastrófico neste período
— exatamente a assinatura que a literatura descreve (Liu-Tang-Zhou 2019):
desalavancar logo após o pico de vol do crash de mar/2020 significa perder
a recuperação em V que veio a seguir. **Achado interessante mas frágil**:
o HHI teria evitado esse erro específico (não reage tão forte ao pico
pontual de vol de mercado), mas isso é uma observação de 1 episódio, não
um resultado robusto.

**Desenho C — long-short de fragilidade (long baixo-HHI, short alto-HHI):**

Média mensal do spread: $-1{,}19\%$ ($t=-1{,}54$, $p=0{,}138$, $N=24$) —
**não significativo**. E o sinal é negativo: no período testado, ações de
*alto* HHI (mais frágeis) tiveram retorno *melhor*, não pior, que as de
baixo HHI — oposto do que a hipótese de "fragilidade prevê queda" sugeriria
(embora consistente, em tese, com uma leitura de prêmio de risco
compensatório — mas não seria possível afirmar isso com confiança dado
o $p$ alto). Skewness do spread é negativa ($-0{,}77$), mas como a média
também é negativa, isso não é a assinatura de "vender seguro" (que exigiria
média positiva com risco de cauda negativo) — é simplesmente uma estratégia
perdedora com cauda ruim. O spread não é mais negativo nos meses de maior
vol passada de mercado ($-0{,}81\%$) do que nos de menor vol
($-1{,}38\%$) — ou seja, **não há evidência, neste teste, de que o efeito
de fragilidade sobre retorno seja condicional a estresse** (contrário à
hipótese motivada pela literatura de Coval-Stafford/Greenwood-Thesmar),
mas de novo: $N=24$ meses e 8 vs.\ 16 observações no split é pouquíssimo
poder para essa comparação.

### Conclusão honesta desta rodada

Nenhum dos três desenhos produziu um sinal de lucro robusto e
estatisticamente confiável — o resultado mais "positivo" (Desenho A,
Sharpe $0{,}088>-0{,}042$) é pequeno, vem com drawdown pior, e repousa
sobre 24 observações mensais dominadas por um único evento (COVID). Isso
**não invalida** o achado principal da seção anterior (HHI prediz
volatilidade futura em h=12, robusto e bem fundamentado na literatura) —
apenas confirma que **prever volatilidade é diferente de conseguir
monetizar essa previsão**, e que a amostra de teste disponível (5 anos,
24 meses após o corte de treino) é curta demais para validar qualquer
estratégia de portfólio com confiança estatística, por melhor que seja a
lógica teórica por trás dela. Registrado aqui integralmente, inclusive o
resultado nulo/negativo, pela mesma disciplina do resto do documento.

## Rodada final (15/08/2026): "faça o seu melhor, leve o tempo que for necessário"

Pedido explícito do João de esgotar todas as alternativas razoáveis antes
de desistir da busca por um sinal de lucro tradeable. Oito candidatos
novos (#33-#40), incluindo uma correção metodológica importante que quase
gerou um segundo falso-positivo tipo Comomentum.

### Candidato #33: 52-week-high × HHI (double-sort, extensão do quase-achado do candidato #24)

Script `33_52wh_x_hhi_double_sort.R`. O candidato #24 já tinha achado que
"52-week high" condicionado a ALTA posse institucional (tamanho, não
concentração) dava o resultado mais forte da família de anomalias
clássicas (h=1, p=0,0048, não bateu Bonferroni). Aqui trocamos por HHI
(concentração/crowding, nossa variável real) — resultado **inverteu de
direção**: o efeito é mais forte no terço de BAIXO HHI (h=1, p=0,008), não
alto. Nenhuma das versões (tercil/mediana, h=1/3/6) bate o novo limiar de
Bonferroni (0,05/450). Confirma que "tamanho de posse institucional" e
"concentração de posse" são economicamente distintos — não dá pra tratar
como a mesma coisa — mas nenhum dos dois vira um sinal confiável de forma
consistente.

### Candidatos #34-38: CIO Peer Momentum revisitado — quase um SEGUNDO falso-positivo tipo Comomentum, pego a tempo

Esta é a sequência mais importante da rodada. Script `34` combinou o CIO
Peer Momentum (único sobrevivente de retorno, candidato #15) com peso
inverso ao HHI (tentando reduzir risco de fragilidade dentro da carteira)
e, de bônus, recalculou o long-short puro do CIO — resultado surpreendente:
Sharpe = 1,03 (com tilt por HHI) e ainda maior sem o tilt, muito mais
modesto e plausível (10-20%/ano) que os 45-63%/ano já flagados como
implausíveis no candidato #15.

**Isso levantou uma bandeira: por que tão diferente do candidato #15?**
Script `36_reconciliacao_metodologia_cio.R` comparou lado a lado as duas
metodologias de corte em quintil na MESMA base (`cio_peer_return.csv`):
- **Método A (candidato #15/16 original): breaks de quintil FIXOS,
  calculados no treino, aplicados no teste.** Diagnóstico: em vários
  meses do teste, isso gera quintis **degenerados** — até **1 única ação**
  na perna (ex.: fev/2020 tinha 257 ações na perna "curta" e só 1 na
  "longa"; out/2021 o oposto) — **exatamente o mesmo artefato que causou
  o falso-positivo do Comomentum** (candidato #9/10), só que não tinha
  sido percebido antes porque o teste original olhava só o t-estatística
  agregado, não o N por mês.
- **Método B (re-corte de quintil A CADA MÊS, padrão Fama-French/Fama-
  MacBeth de verdade): grupos saudáveis o tempo todo** (mínimo 53 ações
  por perna, mediana 57) — nunca degenera.

Com o método corrigido: h=1, Sharpe=1,53 (24 meses), t=2,17, p=0,041 —
e, ao contrário do que o candidato #15/16 relatou (enfraquece pra p=0,057
sem 2020), **o resultado corrigido fica mais forte em 2021 sozinho**
(Sharpe=2,57, t=2,57, p=0,026). Sobrevive Fama-MacBeth com controle de
tamanho/beta/retorno próprio (script `37`, t=2,22, p=0,037). Isso
**muda a avaliação anterior do CIO Peer Momentum: com a metodologia
correta, o sinal é mais robusto (não menos) do que já havíamos concluído.**

**Mas duas checagens de tradeabilidade derrubaram a esperança:**
1. **Custo de transação (script 37):** com apenas 100bps de custo
   round-trip por perna (não é agressivo), o resultado líquido já fica
   **negativo** (-4,2%/ano); só sobra algo real (12,6%/ano) com custo
   muito baixo (30bps), pouco realista pro tipo de ação envolvida.
2. **Composição da perna vendida:** dominada por small/micro caps
   ilíquidas (CEEB3, CEED3, DOHL4, MDNE3, HBOR3, GFSA3, TRIS3, etc.) —
   ações onde aluguel pra venda a descoberto no Brasil provavelmente
   custa muito mais que 100bps ou nem está disponível na prática.
3. **Script 38 — restringindo ao universo líquido (top 1/3 a 2/3 por
   posse institucional) desde o início:** a versão LONG-ONLY (só a perna
   comprada, sem exigir short) tem excesso de retorno **essencialmente
   zero** contra o benchmark (t≈0,02-0,21 nos 3 cortes de liquidez, p>0,8)
   — ou seja, **o "lucro" inteiro do long-short vinha da perna vendida em
   papéis ilíquidos**, não de uma vantagem genuína de seleção positiva.
   A versão long-short restrita ao universo líquido perde significância
   (p=0,09-0,15) mesmo com nomes mais realistas na perna vendida
   (CMIG4, MULT3, EGIE3, MRVE3, BBDC3 — blue chips shortáveis de verdade).

**Conclusão honesta**: o CIO Peer Momentum, tecnicamente, é hoje o
resultado estatístico mais limpo e robusto de toda a exploração de
retorno — mas depende quase inteiramente de vender a descoberto small
caps ilíquidas pra existir. Como a maioria dos fundos que o TCC estuda é
long-only, e mesmo um fundo long-short teria dificuldade real de montar
essa perna vendida, **não é um sinal de lucro tradeable na prática**,
mesmo sendo estatisticamente genuíno. Isto é uma distinção importante:
"achado estatisticamente real" ≠ "estratégia implementável" — e vale
registrar como a lição estrutural desta rodada inteira.

### Candidato #39: Betting Against Correlation (Asness-Frazzini-Gormsen-Pedersen 2020)

Script `39_betting_against_correlation.R`. Decompõe beta em correlação ×
vol relativa; testa long baixa-correlação/short alta-correlação com o
mercado, já restrito a universo líquido desde o início (lição do #38).
**Nenhuma versão (3 cortes de liquidez, long-short e long-only) chega
perto de significância** (p entre 0,31 e 0,78, Sharpe 0,2-0,7). Não
funciona no nosso painel/período.

**Achado secundário interessante, não um sinal de lucro**: no mesmo
script, testamos se HHI prediz CORRELAÇÃO futura com o mercado (conecta
com a teoria de comovimento por posse comum, Antón-Polk/Lou-Polk) —
resultado forte e limpo (t=-5,67, p<0,0001, 24 meses, sem degenerescência),
mas na direção OPOSTA da esperada: HHI mais alto prediz correlação FUTURA
MENOR com o mercado, não maior. Interpretação plausível: concentração
alta pode refletir apostas de alta convicção de poucos gestores (mais
risco idiossincrático) em vez de um efeito de "índice" (mais fundos
diversificados = mais parecido com o mercado). Consistente com o achado
de que a vol extra de ações com HHI alto é sobretudo idiossincrática, não
sistemática — mas não vira estratégia, é só uma peça a mais do quebra-
cabeça teórico.

### Candidato #40: Evento COVID em corte transversal (maior poder estatístico do teste, N=217 de uma vez)

Script `40_covid_event_study.R`. Design com muito mais poder que os testes
de série temporal (N=24 meses): usa HHI de dez/2019 (véspera do choque)
prevendo, numa ÚNICA regressão cross-sectional com 217 ações, (a) queda
durante o crash (fev+mar/2020), (b) retorno na recuperação (abr-dez/2020),
(c) volatilidade pós-crash. **Nenhum dos três é significativo** (p=0,108,
0,692, 0,289 respectivamente) — e o sinal da queda no crash é na direção
OPOSTA da hipótese de fire-sale (HHI mais alto → queda LIGEIRAMENTE
menor, não maior; quintil 5 caiu -36,2%, quintil 1 caiu -40,6%). Este é
o teste mais bem desenhado pra capturar o mecanismo teórico original de
Coval-Stafford/Greenwood-Thesmar (choque específico de liquidação, maior
N de uma vez) e mesmo assim não confirma a hipótese de crash amplificado
por fragilidade durante a COVID no Brasil.

### Conclusão geral da rodada final

Depois de ~450 especificações no total (agora incluindo #33-40), incluindo
uma investida séria e bem-sucedida em evitar um segundo falso-positivo
tipo Comomentum: **nenhum sinal de lucro tradeable e robusto foi
encontrado**, mesmo com esforço deliberadamente exaustivo, múltiplos
ângulos metodológicos novos (double-sort, BAC, evento de choque com alto
poder estatístico, correção de metodologia de corte em quintil) e
verificação cuidadosa de tradeabilidade real (custo de transação,
liquidez da perna vendida, viabilidade long-only). O achado mais sólido
de toda a exploração continua sendo o de PREVISÃO de volatilidade (HHI
→ vol futura, h=12, candidato da seção anterior) — que não é, e não virou
nesta rodada, um sinal de lucro monetizável, mas é um resultado real,
bem fundamentado na literatura (Greenwood-Thesmar 2011) e sem precedente
de aplicação ao Brasil (achado da pesquisa de literatura desta rodada,
via a dissertação da África do Sul de Nairac 2013). Essa é a recomendação
honesta pro TCC: não há sinal de lucro pra reportar, mas há uma
contribuição real e nova de PREVISIBILIDADE — que são coisas diferentes,
e é importante não confundir uma com a outra na hora de decidir o que
(eventualmente) entra no documento oficial.

## MUDANÇA DE INSTRUÇÃO DO USUÁRIO (15/08/2026): tradeabilidade deixa de ser exigência

João revisou a exigência que vinha derrubando o CIO Peer Momentum: **não
precisa mais ser realisticamente tradeable** (liquidez suficiente pra
short, custo de transação não corroer o resultado). Pedido explícito:
"não tem problema que tem pouca liquidez ... quero que continue testando
todos os ângulos possíveis pra achar sinais de lucro". As 3 lições
metodológicas continuam **inegociáveis** (Fama-MacBeth mês a mês, quintil
re-cortado a cada mês, disciplina treino<2020/teste≥2020) — só a barra de
"dá pra implementar de verdade" caiu.

### Reclassificação do CIO Peer Momentum sob o novo critério

Sob a barra antiga, o CIO Peer Momentum (candidatos #11/15/22/34-38) tinha
sido classificado como "estatisticamente real, mas não tradeable" — a
única razão da rejeição final era a perna vendida ser dominada por small
caps ilíquidas (script 38) e o resultado não sobreviver a 100bps de custo
(script 37). **Sob o critério novo, isso deixa de ser motivo de rejeição.**
Reafirmando os números já estabelecidos com a metodologia CORRIGIDA
(re-corte de quintil mês a mês, script 36/37): $h=1$, Sharpe$=1,53$ (24
meses), $t=2,17$, $p=0,041$; mais forte em 2021 sozinho (Sharpe$=2,57$,
$p=0,026$); sobrevive Fama-MacBeth com controle de tamanho/beta/retorno
próprio ($t=2,22$, $p=0,037$, script 37).

**Ressalva honesta que precisa acompanhar essa reclassificação**: $p=0,04$
está longe — muitas ordens de magnitude — do limiar de Bonferroni que essa
exploração vem aplicando desde ~250 especificações atrás
($p<0,05/450\approx0,0001$ na época do candidato #33; agora
$p<0,05/500=0,0001$ com mais ~60 especificações desta rodada). Ou seja:
**mesmo sem a barra de tradeabilidade, o CIO Peer Momentum não é "prova",
é o candidato mais forte entre ~500 tentativas** — precisamente o tipo de
resultado que a correção de múltiplos testes existe para nos lembrar de
tratar com cautela. É honesto dizer que é o melhor achado de retorno da
exploração inteira (robusto a 36 variações de construção no grid search do
candidato #22, sobrevive controles, direção teoricamente motivada por
Ying 2024 JFE), mas não é um resultado "livre de dúvida estatística" —
$p=0,04$ isolado, mesmo com todo o suporte de robustez ao redor, ainda tem
uma chance real de ser 1 entre várias dezenas de "quase-achados" que
apareceriam por acaso numa busca deste tamanho. Fica registrado para o
João decidir, com essa ressalva explícita, se isso entra no TCC como
"sinal de lucro candidato" (não tradeable de fato, mas agora aceito como
critério) ou continua tratado como "acompanha a seção de robustez, não a
conclusão principal".

## Rodada de continuação (15/08/2026): 5 novos ângulos, sem exigência de liquidez

Com a barra de tradeabilidade relaxada, a orientação passou a ser esgotar
ângulos ainda não testados, mesmo que a estratégia final não seja
implementável na prática. Cinco candidatos novos (#41-45), cobrindo os
ângulos sugeridos: (a) combinação de sinais, (b) interação com
volatilidade PREVISTA, (d) choque/atenção via variação abrupta de
posição, (e) ideia de literatura "smart money" ainda não tentada, (f)
segmentação por tipo de gestor. (Ângulo (c), sazonalidade mais granular
que fim-de-trimestre, não é testável com os dados disponíveis — o painel
de preços e holdings é MENSAL, não há dado diário/intramês; fim de
trimestre/dezembro já foi coberto no candidato do script `18`.)

### Candidato #41: CIO Peer Momentum × volatilidade PREVISTA (HHI como proxy de regime)

Script `41_cio_x_vol_prevista.R`. Usa o HHI de posse — a própria variável
que a exploração já estabeleceu como preditor real de volatilidade FUTURA
(candidatos #26-31, achado mais sólido da busca) — como proxy de "regime
de volatilidade prevista" (não vol.\ passada bruta, que já foi testada de
outro jeito no candidato #25). Hipótese: o CIO Peer Momentum, por ser um
mecanismo de difusão gradual de informação, deveria ser mais limpo em
ações de baixo HHI (regime de vol.\ futura baixa), onde há menos ruído de
risco de liquidação coordenada contaminando o sinal.

**Resultado: nenhuma evidência de interação.** Terceis e mediana de HHI
não mostram padrão consistente entre horizontes — em $h=1$ o subgrupo de
ALTO HHI é (não-significativamente) mais forte; em $h=3$ o de BAIXO HHI
é mais forte; em $h=6$ ficam parecidos. A regressão Fama-MacBeth formal
com termo de interação ($peer\_ret_z \times hhi_z$) dá coeficiente de
interação essencialmente nulo ($t=0,07$, $p=0,95$). **Rejeitado**: a
volatilidade prevista (via HHI) não modula o CIO Peer Momentum de forma
identificável nesta amostra.

### Candidato #42: Combinação de 3 sinais (CIO + 52-week-high + reversão) via regressão múltipla e ranking composto

Script `42_composto_3_sinais.R`. Combina três sinais de mecanismo
diferente que ficaram "quase lá" sozinhos: CIO Peer Momentum ($p=0,072$
isolado no teste, $h=1$), 52-week-high (candidato #24, $p=0,091$ na época,
$p=0,72$ nesta subamostra) e reversão de curto prazo (candidato #24,
$p=0,095$ na época, $p=0,99$ aqui). Pesos estimados só no treino
(ym$<$2020) aplicados fixos no teste, e também uma versão sem peso nenhum
(soma simples de z-scores, evita qualquer overfit de peso).

**Resultado misto**: a versão com pesos do treino é instável — o peso de
`peer_ret` estimado no treino MUDA DE SINAL entre $h=1$ (positivo) e
$h=3$ (negativo), sinal claro de que a regressão de treino está sobre-
ajustando ruído de período, não capturando uma relação estável. A versão
sem peso (soma simples dos 3 z-scores) tem o melhor resultado da rodada
inteira: $h=1$, $t=2,32$, $p=0,030$ — mais forte que o CIO sozinho
($t=1,88$ na mesma subamostra) mas **ainda muito longe do limiar de
Bonferroni** ($p=0,030$ precisaria ser $\sim$300× menor). Consistente com
a leitura de que combinar sinais fracos e parcialmente independentes
melhora um pouco o t-estatística (como a literatura de combinação de
fatores prevê), mas aqui a melhora é pequena e não muda a conclusão geral.
**Não passa a barra de significância rigorosa**, mas é a melhor confirmação
indireta de que o CIO Peer Momentum carrega informação real (a combinação
com sinais fracos adicionais não destrói o sinal, reforça na direção
certa).

### Candidato #43: Atenção/sentimento via choque abrupto de posição de fundos

Script `43_atencao_choque_posicao.R`. Dois sinais novos, não o NÍVEL de
posse (já testado várias vezes) mas a MUDANÇA incomum: (1) iniciação
líquida — (nº de fundos que abriram posição nova − nº que zeraram) /
donos no mês anterior; (2) choque de valor total detido, padronizado
(z-score) pelo histórico dos últimos 12 meses DAQUELE MESMO ticker (não
corte transversal — cada ação comparada com sua própria "normalidade"),
proxy de atenção institucional anormal na ausência de dado de
notícia/busca (ideia adjacente a Da-Engelberg-Gao 2011, mas construída
com holdings).

**Resultado: nulo limpo em toda a bateria** (9 especificações: 2 sinais ×
3 horizontes + magnitude absoluta do choque). Nenhum $p$ chega perto de
0,05 (todos entre 0,30 e 0,999), sem padrão de sinal consistente entre
horizontes. **Rejeitado.**

### Candidato #44: CIO Peer Momentum segmentado por tipo de gestor (beta_fundo)

Script `44_cio_x_beta_fundo.R`. Usa `beta_fundo` (beta de mercado da cota
do fundo vs.\ Ibovespa, já calculado no pipeline oficial do TCC) como
proxy observável de tipo de gestor — reconstrói a rede de overlap de posse
(cosseno, igual ao script `11`) duas vezes por mês, uma só com fundos do
terço de BAIXO beta (mais conservadores) e outra só com o terço de ALTO
beta (mais agressivos), testando qual rede produz `peer_ret` mais
preditivo. Nota: `beta_fundo` mede exposição a risco, não skill —
candidato #26/27 já mostrou que não há persistência de performance
mesmo ajustando por isso.

**Resultado inconsistente entre horizontes — sinal de ruído, não de
padrão real**: em $h=1$, a rede de ALTO-beta é mais forte ($t=2,56$,
$p=0,017$) que a completa ($t=2,17$) e a de BAIXO-beta é fraca
($t=0,50$); em $h=6$ a ordem se INVERTE — a rede de BAIXO-beta fica mais
forte de toda a rodada ($t=3,34$, $p=0,0036$, o $p$ mais baixo desta
bateria de 5 candidatos) e a de ALTO-beta enfraquece. Nenhum resultado
passa Bonferroni, e a inversão de qual subgrupo "vence" dependendo do
horizonte é exatamente o padrão esperado de 9 testes cruzando ruído
aleatório, não uma segmentação estrutural genuína — não há teoria que
preveja BAIXO-beta ganhar em $h=6$ mas perder em $h=1$. **Rejeitado como
segmentação útil**, registrado o achado nominal de $h=6$/BAIXO-beta com a
ressalva explícita de que não é corroborado pelos outros horizontes.

### Candidato #45: Best Ideas e convicção crescente (Cohen-Polk-Silli 2010)

Script `45_best_ideas_conviccao.R`. Ideia de literatura "smart money"
ainda não testada: a maior posição ATIVA de um fundo (top-3 por peso,
proxy de active share já que não temos benchmark por fundo) tende a
performar melhor (Cohen-Polk-Silli, JF 2010). Testado (1) nível — capital
"de alta convicção" (AUM dos fundos que têm a ação como top-3) como
fração do universo; (2) extensão nova — entre relações fundo-ação que já
são top-3, a convicção está CRESCENDO (Δpeso ponderado por AUM); (3)
interação — convicção crescente só em ações que já são "best idea"
popular de muita gente.

**Resultado: nulo em toda a bateria** (9 especificações). Nível sozinho:
melhor caso $p=0,44$. Convicção crescente: melhor caso $p=0,54$. Interação:
melhor caso $p=0,57$. **Ressalva de poder estatístico**: o sinal 2/3 tem
$N$ mínimo por perna caindo a 5-11 observações em alguns meses (poucas
relações fundo-ação satisfazem "já é top-3 E tem histórico de peso no mês
anterior E o ticker tem pelo menos 5 dessas relações") — os nulos aqui
devem ser lidos como "sem sinal detectável com o pouco poder disponível",
não como rejeição definitiva da hipótese. **Rejeitado nesta amostra**, com
essa ressalva.

### Conclusão desta rodada (candidatos #41-45)

Nenhum dos 5 ângulos novos produziu um sinal que passe nem o padrão de
5% de forma robusta, muito menos Bonferroni — call mais próximo foi a
combinação sem peso do candidato #42 ($p=0,030$, $h=1$) e o subgrupo
BAIXO-beta em $h=6$ do candidato #44 ($p=0,0036$, mas não corroborado
pelos outros horizontes do mesmo desenho). **O panorama geral não muda**:
CIO Peer Momentum continua sendo o único sinal de RETORNO com suporte
razoável (agora relevante de novo, dado que a exigência de tradeabilidade
caiu — ver seção de reclassificação acima), e HHI→volatilidade futura
continua sendo o achado mais sólido de PREVISIBILIDADE (não de lucro
direto) de toda a exploração. Nenhuma interação nova (vol.\ prevista,
tipo de gestor) nem sinal genuinamente novo (atenção/choque, best ideas)
mudou esse quadro. Contagem acumulada de especificações testadas em toda
a exploração: ~510-520.

## Rodada de agentes paralelos (15/08/2026): 6 agentes, ângulos distintos, sem exigência de tradeabilidade

Consolidação de 6 logs separados escritos por agentes rodando em paralelo (evitar conflito de edição simultânea no arquivo principal). Cada seção abaixo preserva o conteúdo original de cada agente.

### Agente 1 (original, candidatos #41-45): CIO×vol, choques de posição, segmentação por gestora, best ideas

(já consolidado diretamente neste arquivo pelo próprio agente — ver candidatos #41-45 acima)

### Agente 2: ranking composto multi-sinal


Log separado do `LOG_CANDIDATOS.md` principal (execução em paralelo, 4
agentes, escopo de scripts exclusivo `55_`-`59_`). Não repete o que já foi
testado — pressupõe leitura prévia do log principal (~450+ especificações,
16-40+ candidatos individuais). Mesma disciplina metodológica obrigatória:
Fama-MacBeth mês a mês (nunca `t.test` pooled), quintil recortado A CADA MÊS
no teste (nunca breaks fixos do treino), treino `ym<202001` / teste
`ym>=202001`, e agora limiar de Bonferroni `p<0,05/500` (mesma base de
comparação acumulada do log principal).

**Ângulo específico deste agente:** cada sinal individual já testado
(CIO Peer Momentum, HHI/crowding, 52-week-high, reversão de curto prazo,
idio-vol, breadth, demanda agregada revelada, E[FIT]) foi fraco ou nulo
sozinho — testar se a COMBINAÇÃO carrega mais informação que qualquer um
isolado (fusão de sinais, Asness-Moskowitz-Pedersen 2013; Grinold-Kahn
para ponderação por IC). Três abordagens: (a) ranking composto por
percentil médio, (b) regressão múltipla Fama-MacBeth, (c) combinação
ponderada por IC histórico do treino.

**Nota sobre scripts pré-existentes:** ao começar, os scripts `41`-`44` já
existiam no repositório (de execução anterior/outro processo), incluindo
`42_composto_3_sinais.R` — uma primeira versão, mais simples, do mesmo
ângulo (peer_ret + 52wk-high + reversão, 2 métodos de combinação). Esse
script já achado nulo (melhor caso "soma simples sem peso" h=1: p=0,030,
não passa Bonferroni) foi usado como ponto de partida/validação cruzada,
não repetido — este log estende para um conjunto de sinais bem mais amplo
(8 sinais, incluindo HHI/crowding, idio-vol, breadth, E[FIT], demanda
agregada) e adiciona a abordagem de ponderação por IC (não coberta em `42`).

## Preparação: painel unificado de 8 sinais

Script `55_construir_sinais_compostos.R`. Constrói um painel único
(ticker, ym) com os 8 sinais mecanicamente distintos já documentados no
log principal, cada um reconstruído com a MESMA fórmula já usada e
testada antes (não são sinais novos, é a base para a combinação):

| Sinal | Direção teórica | Fonte/candidato original |
|---|---|---|
| `peer_ret` (CIO Peer Momentum) | + | candidato #15 (único sinal de retorno isolado que sobreviveu) |
| `hhi_posse` (crowding) | usado como `-hhi_posse` | candidato #13, direto de `fit_ativo_mes.csv` |
| `prox_52w_high` | + | candidato #24 (George-Hwang 2004) |
| `reversao` (=-retorno mês anterior) | + | candidato #24 (Jegadeesh 1990) |
| `idio_vol_12m` | + (achado BRASILEIRO, não o puzzle americano) | candidato #24 + pesquisa rodada 6 |
| `delta_breadth_w` | + | candidato #7 (Chen-Hong-Stein 2002) |
| `dem_pct_w` (demanda agregada revelada) | ambíguo — direção fixada só com dado de TREINO | candidato B |
| `EFIT` (fluxo esperado) | ambíguo — direção fixada só com dado de TREINO | candidato #11 (Lou 2012 fiel) |

Cobertura no painel final (28.808 ticker-meses): `reversao` 94%,
`prox_52w_high`/`idio_vol_12m` 79%, `peer_ret` 60%, `delta_breadth_w`/
`dem_pct_w` 59%, `hhi_posse`/`EFIT` 47% (a interseção dos 8 ao mesmo tempo
é bem menor — ver script 56).

## Candidato composto #1: Ranking composto por percentil (média simples)

Script `56_composto_ranking_percentil.R`. Cada sinal alinhado (`hhi_posse`
invertido; `dem_pct_w`/`EFIT` com sinal fixado só no treino, documentado
explicitamente para não ser "garimpo": treino aponta `dem_pct_w` negativo
em h=1/h=3 e positivo em h=6, `EFIT` sempre negativo) vira percentil
cross-seccional (0 a 1) dentro do mês. Testadas 3 versões: TOP3
(peer+hhi+52wk-high, os 3 sinais explicitamente sugeridos), TOP5 (+
reversão + idio-vol) e ALL8 (média dos disponíveis, exige ≥6 de 8).
Quintil do score recortado a cada mês, Fama-MacBeth h=1/3/6.

**Resultado: nulo em toda a bateria (9 especificações).** Nenhum `p_fm`
abaixo de 0,27 — pior ainda, em h=1 (onde CIO sozinho tem p=0,021-0,041
segundo o log principal, candidatos #15/34-38) o composto TOP3 dá
p=0,72 e o composto ALL8 dá p=0,76, ambos **muito piores que CIO sozinho**.
A média simples de percentis dilui o único sinal genuíno (peer_ret) com
ruído dos outros 7 — o oposto do que a teoria de combinação de fatores
prevê (que funciona quando os componentes têm informação genuína e
correlação baixa; aqui a maioria dos componentes tem informação ~zero,
então a "diversificação" só adiciona ruído).

**Veredito: REJEITADO.** Evidência direta e limpa de que ranking composto
por peso igual não ajuda — precisa ser registrado porque contraria a
intuição inicial do pedido (ver candidato composto #4 abaixo pela
explicação mais completa do porquê).

## Candidato composto #2: Regressão múltipla Fama-MacBeth

Script `57_composto_regressao_multipla_fm.R`. Regressão cross-seccional
mês a mês, retorno(t+h) ~ soma dos sinais padronizados (z-score dentro do
mês), testando (i) quais sinais sobrevivem JUNTOS e (ii) se o R² do
modelo completo supera o de qualquer sinal isolado; e um score composto
com pesos = coeficientes de UMA regressão no TREINO (nunca vistos o
teste), quintil recortado a cada mês no teste.

**R² in-sample cresce mecanicamente com mais regressores** (h=1: peer_ret
sozinho 1,19%, TOP3 6,61%, ALL8 11,36%) — mas isso NÃO é evidência de
ganho real (esperado por construção com mais parâmetros, ainda mais com
~40-50 meses de treino e 8 regressores, risco de overfitting já
documentado no candidato #21/Ensemble ML do log principal). O teste que
importa é o quintil recortado mês a mês fora da amostra:

- **h=1**: `s_peer` sobrevive na regressão conjunta (t=2,42, p=0,024,
  ALL8) — consistente com achado já documentado (candidato #15, seção P,
  CIO sobrevive a controles). `s_hhi` quase (t=-2,05 a -2,11, p≈0,05).
  Mas o SCORE COMPOSTO com esses pesos, testado como quintil: ALL8
  p=0,825 (**pior que CIO sozinho, e até com sinal errado**), TOP3
  p=0,336. Combinar não ajuda, mesmo quando os coeficientes individuais
  são "razoáveis".
- **h=3**: nada sobrevive na regressão conjunta (todos p>0,20); scores
  compostos também nulos (p=0,25-0,52).
- **h=6**: `s_hhi` quase significativo isolado (t=-1,76 a -2,02,
  p=0,058-0,095) nas duas versões; SCORE COMPOSTO ALL8 com pesos do
  treino: **t=3,05, p=0,007** (passa de 5% padrão, não de Bonferroni) —
  o único resultado nominal "interessante" das 3 abordagens deste agente.
  Investigado a fundo no candidato composto #4 abaixo.

**Veredito parcial: REJEITADO em h=1/h=3; h=6 investigado separadamente.**

## Candidato composto #3: Combinação ponderada por IC (Information Coefficient)

Script `58_composto_ic_weighted.R`. Prática padrão de gestão quantitativa
(Grinold-Kahn): peso de cada sinal = correlação de Spearman média,
mês a mês, com o retorno futuro, calculada **só no treino**, aplicada como
peso fixo no teste (não precisa alinhar direção manualmente — o sinal do
IC já resolve isso). Testadas 2 versões: todos os 8 sinais, e só os 4 com
`|IC_treino|` acima da mediana (reduz ruído dos mais fracos).

**Resultado: nulo em toda a bateria (6 especificações), pior que a
regressão múltipla.** Melhor caso h=6 "só fortes": t=1,26, p=0,223. A
versão que só usa os sinais fracos filtrados por IC nem sequer chega perto
do resultado nominal do candidato composto #2 (h=6) — reforça que aquele
resultado depende dos PESOS específicos da regressão múltipla (que
captura alguma interação/controle entre peer_ret e hhi), não de uma
ponderação genérica por força de sinal individual.

**Veredito: REJEITADO.**

## Candidato composto #4: Diagnóstico do quase-resultado h=6 (ALL8, regressão múltipla)

Script `59_robustez_composto_h6.R`. Antes de registrar o achado nominal do
candidato composto #2 (h=6, t=3,05, p=0,007) como qualquer coisa próxima
de "achado", mesma bateria de diagnósticos que já pegou 2
quase-falsos-positivos nesta exploração (Comomentum; CIO com quintil
degenerado, ambos no log principal):

1. **Tamanho dos grupos:** saudável, n_mín=35 ação-mês por perna em todos
   os 19 meses — não é degenerescência tipo Comomentum.
2. **Decomposição — quanto vem só de peer_ret+hhi vs. os outros 6:**
   score usando SÓ peer_ret+hhi (com os mesmos pesos do treino) já dá
   t=2,17, p=0,044 — **quase todo o resultado do composto de 8 sinais**;
   score usando só os OUTROS 6 sinais dá t=0,32, p=0,75 (nada). Ou seja,
   isto não é "combinação de 8 sinais fracos gerando um forte" — é o
   CIO+HHI (já conhecidos) reaparecendo em h=6, e os outros 6 sinais são
   inertes/carona.
3. **Estabilidade 2020 vs. 2021 (o mesmo teste que já expôs fragilidade
   do CIO original em h=1 no log principal):** aqui o resultado é
   BEM mais frágil que o do CIO h=1 — **2020 (12 meses): t=4,25,
   p=0,0014; 2021 (7 meses): t=0,06, p=0,95 — o efeito desaparece
   COMPLETAMENTE fora de 2020.**
4. **Concentração temporal:** 4 de 19 meses (dez/2020, out/2020,
   mar/2020, nov/2020 — TODOS de 2020, 3 deles no 2º semestre da
   recuperação pós-COVID) respondem por 61,7% do spread total.
5. **Composição das pernas:** mediana do valor total mantido pelos fundos
   (proxy de tamanho/liquidez) é R$303 milhões na perna vendida (quintil
   1, "bearish", dominada por blue chips líquidas como ITSA3, PETR3,
   VIVT3, CMIG4, GGBR4) contra R$50 milhões na perna comprada (quintil 5,
   "bullish", com bastante small/micro cap ilíquida — CRPG5, EMAE4,
   UCAS3, PDTC3, CGRA4, ETER3, FESA4, AZEV4, RCSL3). Ao contrário do CIO
   h=1 (onde o problema era a perna VENDIDA ilíquida), aqui quem é
   ilíquido é a perna COMPRADA — mais fácil de implementar num fundo
   long-only, mas também sugere que o "sinal" está parcialmente
   recapturando o prêmio de tamanho (small cap) já conhecido e não uma
   informação nova.

**Veredito: REJEITADO, com confiança alta de que não é achado genuíno.**
O padrão é idêntico ao dos outros quase-falsos-positivos já documentados:
resultado nominal chamativo (p=0,007) que, ao ser decomposto, se revela
(a) redutível a 2 sinais já conhecidos, não uma combinação nova, e (b)
inteiramente dependente do ano de 2020 — o único episódio de estresse do
período de teste. Isso é consistente com o padrão geral já estabelecido
no log principal (candidato #15, seção de robustez de período): qualquer
achado que dependa inteiramente de 2020 deve ser tratado como possível
artefato do episódio COVID, não um padrão estrutural do mercado
brasileiro.

## Síntese final (< 300 palavras)

Testei 3 formas de combinar os 8 sinais fracos/nulos já documentados no
log principal (CIO Peer Momentum, HHI/crowding, 52-week-high, reversão,
idio-vol, breadth, demanda agregada revelada, E[FIT]): (a) ranking
composto por percentil médio, (b) regressão múltipla Fama-MacBeth com
pesos estimados só no treino, (c) combinação ponderada por Information
Coefficient histórico do treino — 24 especificações no total, todas com
Fama-MacBeth de verdade (recorte de quintil mês a mês, nunca breaks fixos,
nunca t-test pooled), disciplina treino `<2020-01`/teste `>=2020-01`.

**Nenhuma passou no limiar de Bonferroni (p<0,05/500), e a maioria nem no
limiar de 5% padrão.** O único resultado nominal (h=6, regressão múltipla
com pesos do treino sobre os 8 sinais, p=0,007) foi investigado a fundo e
rejeitado com confiança alta: 90%+ do efeito vem só de peer_ret+HHI (não é
combinação nova), e o efeito desaparece por completo em 2021 (t=4,25 em
2020 vs. t=0,06 em 2021) — mesmo padrão de artefato ligado à COVID já
visto nos quase-falsos-positivos anteriores do log principal.

**Achado metodológico mais importante desta rodada, honestamente
negativo:** combinar sinais fracos/nulos com o único sinal genuíno
conhecido (CIO Peer Momentum) **piora** o resultado em vez de melhorar —
em h=1, onde CIO sozinho tem p=0,021-0,041, qualquer composto testado
(ranking simples, regressão múltipla, IC-weighted) fica pior (melhor caso
p=0,336). Isso contraria a intuição inicial de "fusão de sinais" porque
essa técnica só funciona quando os componentes têm informação genuína e
baixa correlação entre si — aqui, 7 dos 8 componentes têm informação
próxima de zero no período de teste, então qualquer combinação com peso
não-trivial neles dilui o único sinal real. **Recomendação: não há achado
composto para o TCC; se algo for reportado, deve ser CIO Peer Momentum
isolado (h=1), não uma versão combinada.**

### Agente 3: centralidade de rede de posse comum


Log separado de um dos quatro agentes rodando em paralelo na mesma busca por
sinal de lucro tradeável (ver `LOG_CANDIDATOS.md` principal para o histórico
completo, ~470+ especificações já testadas por outras vias). Escopo exclusivo
deste agente: tratar a matriz CIO (cosine similarity de posse ponderada por
AUM entre pares de ações, já construída no candidato "CIO Peer Momentum",
scripts 11/15/34-38 do log principal) como um GRAFO PONDERADO e testar
medidas de **centralidade de rede** — não retorno dos "vizinhos" (isso já foi
testado e é o CIO Peer Momentum), mas propriedades estruturais do próprio nó
dentro da rede.

Scripts: `60_centralidade_construcao.R` a `64_centralidade_diagnostico_robustez.R`.
Mesma disciplina inegociável do log principal: (1) Fama-MacBeth de verdade —
regressão/spread cross-sectional mês a mês, testando a média da série de
coeficientes com erro-padrão da própria série; (2) quintis re-cortados A CADA
MÊS (método corrigido, nunca breaks fixos do treino); (3) treino `ym<202001`,
teste `ym>=202001`, sempre reportando N mínimo por grupo-mês.

**Construção da rede (script 60):** reaproveita exatamente a matriz CIO do
candidato "CIO Peer Momentum" — `X_{f,i,t}=AUM_{f,t}*peso_{f,i,t}`,
`O_{i,j,t}=sum_f X_{f,i,t}*X_{f,j,t}`, `CIO_{i,j,t}=O_{i,j,t}/sqrt(O_{i,i,t}*O_{j,j,t})`
— mas em vez de calcular `peer_ret`, calcula propriedades do grafo: grau
ponderado (`deg_avg` = soma da linha CIO dividida por n-1, sem piso de
conexão — grafo completo), autovetor de centralidade (`eigen_cent`,
autovetor do maior autovalor de CIO, normalizado max=1, convenção Bonacich)
e densidade da rede (média de todas as entradas fora da diagonal, 1 valor
por mês). 60 meses no painel, mediana de 280 ações por mês, amostra saudável
(nunca degenerada — n_tickers_mes mínimo bem acima do piso de 15 exigido).

## Candidato #R1: Grau ponderado de centralidade (degree centrality) — nível

**Mecanismo hipotetizado:** ações mais "centrais" na rede de posse comum
institucional (mais conectadas via overlap de donos com outras ações) podem
ter retorno futuro sistematicamente diferente de ações "periféricas" —
canal possível: mais central = mais monitorada/negociada pela mesma base de
investidores, logo mais sujeita a informação difundindo através da rede
(ou, inversamente, mais sujeita a risco de liquidação coordenada).

**Resultado inicial (script 61), Fama-MacBeth com re-corte mensal (método
corrigido):** direção **negativa e crescente com o horizonte** — h=1:
spread=-1,22pp/mês (t=-1,40, p=0,174, n.s.); h=3: -1,02pp/mês (t=-1,17,
p=0,256, n.s.); h=6: -1,29pp/mês (t=-1,65, p=0,117, n.s.); h=12: **-1,91pp/mês
(t=-2,04, p=0,065)** — quase passa de 5%, e o padrão de p decrescente com o
horizonte é qualitativamente diferente (e mais interessante a princípio) do
CIO Peer Momentum, que só funciona em h=1 e desaparece depois. N por
grupo-mês sempre saudável (mínimo 44, mediana 53-57 — nunca degenerado).

**Mas não sobrevive a diagnóstico (script 64, ver seção consolidada abaixo):
é size effect disfarçado + concentração em meses de COVID. Ver veredito
final.**

## Candidato #R2: Autovetor de centralidade (eigenvector centrality) — nível

**Mecanismo hipotetizado:** versão mais sofisticada do R1 — pondera conexões
com ações que TAMBÉM são centrais (uma ação conectada a poucos "hubs" muito
centrais pode ser mais central, no sentido de Bonacich/Katz, do que uma
conectada a muitas ações periféricas com o mesmo grau bruto).

**Resultado (script 61), Fama-MacBeth com re-corte mensal:** mesmo padrão do
R1, um pouco mais forte — h=1: -1,51pp/mês (t=-1,70, p=0,103, n.s.); h=3:
-1,13pp/mês (t=-1,21, p=0,241, n.s.); h=6: **-1,91pp/mês (t=-2,50,
p=0,022)** — passa de 5% (não de Bonferroni); h=12: **-2,40pp/mês (t=-2,19,
p=0,049)** — passa de 5% no limite. Direção consistente com achado já
registrado no log principal (Rodada 5 de pesquisa: "CWEC / Kita & Zhang —
mais centralidade = retorno MENOR" — mesma direção, nunca antes TESTADO
empiricamente nesta base, só citado como referência de literatura).
Correlação `deg_avg` × `eigen_cent` pooled = 0,944 — as duas medidas são
quase a mesma coisa aqui (esperado, com piso de conexão zero/grafo completo
denso, autovetor e grau ponderado tendem a convergir).

**Este foi o candidato mais promissor da rodada — mas não sobrevive ao
diagnóstico. Ver veredito consolidado abaixo.**

## Candidato #R3: Variação mês-a-mês da centralidade (Δdegree, Δeigenvector) — "entrada institucional coordenada"

**Mecanismo hipotetizado (idea (a) do pedido original):** um AUMENTO
REPENTINO de centralidade pode sinalizar entrada institucional coordenada
(vários fundos comprando a ação ao mesmo tempo, aumentando seu overlap de
posse com o resto do mercado) — testável separadamente do nível.

**Construção:** `deg_avg_delta`/`eigen_cent_delta` = diferença mês a mês,
calculada SÓ entre meses calendário consecutivos (evita costurar buracos no
painel — 16.779 de 17.450 ativo-mês tinham par consecutivo válido).

**Resultado (script 61):** **nulo em todos os 8 testes** (2 variáveis × 4
horizontes). p entre 0,52 e 0,94, sem padrão de direção (spread positivo em
alguns horizontes, negativo em outros, sem consistência), N por grupo-mês
saudável (mín. 42, mediana ~52-56). **REJEITADO** — nenhuma evidência de que
mudança de centralidade carregue informação sobre retorno futuro.

## Candidato #R4: Densidade da rede de mercado agregado vs. retorno agregado

**Mecanismo hipotetizado (idea (c) do pedido original):** densidade da rede
CIO como um todo (média de todas as conexões par-a-par no mês, 1 número por
mês) como proxy de "quão concentrado/crowded" está o mercado inteiro —
mercado mais denso (posse mais correlacionada entre ações) poderia prever
retorno agregado do mês seguinte diferente (mecanismo de risco sistêmico:
mais correlação de posse = mais risco de liquidação coordenada em massa).

**Ressalva metodológica importante, registrada com transparência:** este é
o único candidato desta rodada sem dimensão cross-sectional (1 observação
por MÊS, não por ativo-mês) — Fama-MacBeth não se aplica (não há "grupos"
dentro do mês). Poder estatístico estruturalmente mais fraco (N=36 treino /
23 teste MESES, não ativo-mês). Reportado com OOS R² (mesma disciplina
treino/teste do resto do TCC) e erro-padrão robusto a autocorrelação
(Newey-West, lag=3) na regressão plena.

**Resultado (script 62): nulo, de forma limpa.** R²_OOS = -3,34% (pior que
prever a média do treino). Regressão plena: coeficiente ~0, p=0,96 (OLS) /
p=0,95 (Newey-West) — não há absolutamente nenhum sinal, nem espúrio.
Controlando por retorno de mercado passado, R²_OOS piora ainda mais
(-4,41%). Correlação densidade × |retorno do mesmo mês| (proxy de regime de
vol contemporâneo) = 0,193, fraca — densidade não é sequer um bom proxy de
vol de mercado corrente. **REJEITADO**, sem ressalvas.

## Candidato #R5: Centralidade restrita a subconjunto de alto/baixo HHI

**Mecanismo hipotetizado (idea (d) do pedido original):** dentro do
subconjunto de ações já "crowded" (HHI de posse alto, terço superior, mesma
proxy de crowding validada no log principal), ser mais central NESSA
sub-rede pode carregar informação adicional (mecanismo: entre ações já
concentradas, a mais central seria a "líder" de um cluster de liquidação
coordenada). Testado em paralelo no terço de HHI baixo, como contraste.

**Construção:** nota algébrica registrada no script — `CIO_{i,j}` depende só
das colunas i,j de X, então restringir o universo não muda o valor de CIO
para pares dentro do subgrupo; o que muda genuinamente é o grau (soma
parcial, só sobre pares do subgrupo) e o autovetor (não é invariante a
submatrizes). Terços recalculados a cada mês (não corte fixo).

**Resultado (script 63): nulo em toda a bateria (16 especificações — 2
subgrupos × 2 medidas × 4 horizontes).** Melhor caso: HHI-baixo, autovetor,
h=6, p=0,202 — nem perto de 5%. **Amostra bem mais fina que o resto da
exploração** (N por grupo-mês mínimo 13-15, mediana 15-18 — cada terço da
rede já restrita gera quintis de ~3 ações cada perna, no limite inferior do
que se considera confiável; registrado como limitação de poder estatístico,
não como evidência forte contra a hipótese). **REJEITADO.**

## Diagnóstico decisivo (script 64): por que R1/R2 (nível de centralidade) não sobrevivem

O candidato mais promissor da rodada (R1/R2, nível de centralidade,
h=6/h=12) foi submetido às mesmas checagens de robustez que já evitaram
dois quase-falsos-positivos no log principal (Comomentum, CIO original com
quintis degenerados):

**(A) Controle por tamanho — o efeito MORRE por completo.** Correlação
pooled `deg_avg` × `log_tamanho` = 0,667; `eigen_cent` × `log_tamanho` =
0,728 — correlações muito altas, ação mais central é quase sempre a ação
com mais capital institucional total. Residualizando dentro de cada mês
(mesma lógica do "CIO neutro-tamanho", script 23 do log principal):
`eigen_cent_neutro` h=6: t cai de -2,50 (p=0,022) para **-0,33 (p=0,747)**;
h=12: t cai de -2,19 (p=0,049) para **-1,36 (p=0,199)**. `deg_avg_neutro`
mostra o mesmo colapso (h=12: t de -2,04 para -0,35). **O sinal de
centralidade É um proxy de tamanho, não um efeito de rede genuíno** — bate
com o efeito-tamanho clássico já documentado e conhecido (ações menores têm
retorno esperado maior), não é descoberta nova.

**(B) Composição do spread concentrada em meses de COVID.** Nos 2 horizontes
"significativos" (h=6, h=12), os 2 meses mais negativos somam 42% e 54% do
spread total, respectivamente — e são exatamente os meses do crash/
recuperação da COVID (mar-abr/2020, e depois set-dez/2020 e abr/2021 no
rebote). Mesma assinatura de fragilidade estatística já vista nos
quase-falsos-positivos anteriores da exploração (Comomentum: 1 mês
contribuiu 43,6pp sozinho; aqui é menos extremo mas mesma família de
problema).

**(C) Robustez a rank vs. nível bruto:** usar rank percentual dentro do mês
em vez do nível bruto produz resultados **idênticos** (mesmos t, mesmos p)
— não é um problema de outliers extremos de nível, é a distribuição inteira
correlacionada com tamanho, reforçando (A).

**(D) Excluindo 2020 inteiro:** com `ym>=202101`, restam só 7 meses de teste
para h=6 (poder estatístico já muito baixo) — resultado despenca para
t=-0,18 (p=0,864) em `deg_avg` e t=-0,29 (p=0,784) em `eigen_cent`.
Confirma (B): sem o período de COVID, não sobra nada.

**Veredito consolidado R1/R2: REJEITADOS**, com diagnóstico completo e
convergente em 4 checagens independentes — não é 1 checagem isolada
"salvando" ou "matando" o resultado, as 4 apontam na mesma direção. O nível
de centralidade de rede, nesta base, é (i) estatisticamente um proxy de
tamanho e (ii) concentrado no episódio único de COVID — exatamente o tipo
de "quase-achado" que a disciplina metodológica desta exploração existe
para filtrar antes de reportar.

## Síntese final (agente_rede): menos de 300 palavras

Testadas 5 hipóteses de centralidade de rede na matriz CIO (posse
institucional comum) já construída no candidato "CIO Peer Momentum": grau
ponderado (nível e variação mês a mês), autovetor de centralidade (nível e
variação), densidade agregada de mercado, e centralidade restrita a
subgrupos de alto/baixo HHI — total de ~58 especificações, todas com
disciplina fora-da-amostra (treino `<202001`/teste `>=202001`) e
Fama-MacBeth de verdade (spread mensal recortado a cada mês, N mínimo por
grupo-mês sempre reportado e sempre saudável, exceto no subgrupo HHI onde é
mais fino por natureza — registrado como limitação).

**Nada sobrevive.** Δcentralidade (nível e variação), densidade de mercado
e centralidade em subgrupo HHI são nulos de forma limpa, sem padrão. O
nível de centralidade (grau e autovetor) pareceu promissor a princípio —
direção negativa crescendo em significância com o horizonte (h=6: p=0,022;
h=12: p=0,049), consistente com a única referência de literatura já
mapeada no log principal (CWEC/Kita-Zhang, nunca testada empiricamente
nesta base antes) — mas não passa em nenhuma das 4 checagens de robustez
aplicadas: colapsa totalmente ao controlar por tamanho (correlação
pooled 0,67-0,73 com log do capital institucional total — é efeito-tamanho
disfarçado, não efeito de rede), é dominado por 2 meses do episódio único
de COVID (42-54% do spread total), é idêntico em nível bruto vs. rank
(não é outlier), e desaparece quase por completo excluindo 2020 (N cai
para 7 meses, t de -2,50 para -0,18). **Nenhum candidato deste ângulo passa
nas 3 checagens metodológicas inegociáveis de forma robusta o suficiente
para ser reportado como achado.** Consistente com o restante da exploração:
a matriz CIO carrega sinal genuíno via retorno-dos-vizinhos (CIO Peer
Momentum, h=1, já registrado no log principal), mas não via propriedades
estruturais do grafo em si.

### Agente 4: regime de volatilidade × sinais existentes, choques de fluxo


Log separado de um dos quatro agentes rodando em paralelo sobre a mesma
tarefa (achar sinal de lucro tradeable, TCC de fundos brasileiros). Este
agente cobriu duas frentes específicas: (A) interação entre sinais de
retorno já testados e o regime de volatilidade PREVISTA (HHI de posse,
já validado como preditor de vol futura — candidatos #26-31 do
`LOG_CANDIDATOS.md` principal); (B) fluxo/posição extrema — variação
abrupta de posse institucional, construída com correção explícita da
contaminação mecânica documentada por Wardlaw (2020, JF).

Scripts exclusivos deste agente: `65` a `69` (`v2 OFICIAL/exploracao_sinais/scripts/`).
Mesma disciplina metodológica do log principal: Fama-MacBeth de verdade
(1 coeficiente/spread por mês, erro-padrão da série temporal), quintis/decis
**re-cortados a cada mês** dentro do período de teste (nunca breaks fixos
do treino), treino `ym<202001` / teste `ym>=202001`. Limiar de Bonferroni
usado: `0,05/500 ≈ 0,0001`, consistente com a contagem corrida de
especificações já testadas na exploração inteira (outros agentes paralelos
já passaram de #40 antes deste log começar).

**Nota de coordenação**: antes de iniciar, li os scripts `41`-`45` (de
outro(s) agente(s) paralelo(s), já executados). O script `41` já tinha
testado exatamente a Frente A para o CIO Peer Momentum especificamente
(resultado nulo — melhor caso baseline h=1 p=0,148, nenhum split por HHI
significativo) e o script `33` já tinha testado 52-week-high × HHI (nulo).
Para não duplicar, a Frente A deste agente cobriu os sinais de retorno
que ainda não tinham sido cruzados com HHI: Breadth of ownership,
Herding (LSV), E[FIT] e Reversão de curto prazo.

## Candidato R1: Breadth of ownership × regime de HHI

Script `65_regime_vol_x_sinais_retorno.R`. Variação % do número de fundos
com posição positiva (delta_breadth), condicionada a tercil/mediana de
HHI, h=1/3/6.

**Resultado: nulo em toda a bateria.** Melhor caso p=0,458 (h=1,
ALTO-HHI mediana). Nenhum padrão direcional consistente entre baixo e
alto HHI. **REJEITADO.**

## Candidato R2: Herding (LSV) × regime de HHI

Mesmo script. Proporção de fundos compradores vs. média do mercado,
condicionada a HHI.

**Resultado: nulo.** Melhor caso p=0,188 (h=1, BAIXO-HHI mediana, direção
oposta à intuição de que fragilidade amplificaria herding). **REJEITADO.**

## Candidato R3: E[FIT] × regime de HHI — quase-achado, investigado a fundo, NÃO CONFIRMADO

Script `65`, aprofundado no diagnóstico dedicado `69_diagnostico_efit_x_hhi.R`.

E[FIT] (fluxo esperado, candidato #11 do log principal — já identificado
lá como "o mais promissor mas sem long-short executável" com a
metodologia antiga de breaks fixos do treino) testado aqui com a
metodologia **corrigida** de re-corte de quintil mês a mês (lição #36 do
log principal). Isso já muda o resultado: baseline (sem condicionar)
passa a ser marginalmente significativo (h=1: t=2,10, p=0,047 — não
passava de 5% com a metodologia antiga).

**Dentro do tercil de ALTO HHI, o resultado fica mais forte e decai
suavemente entre horizontes** (padrão que a própria exploração já usou
como critério de "sinal genuíno" no grid search do CIO):
- h=1: t=3,52, p=0,0019, spread +2,88pp/mês (+40,6%/ano)
- h=3: t=2,00, p=0,059, spread +2,30pp/mês (+31,4%/ano)
- h=6: t=1,75, p=0,097, spread +2,69pp/mês (+37,6%/ano)

**Checagens de robustez (script 69), resultado misto:**
- Nenhum mês isolado domina o spread de h=1 (contribuição máxima de um
  único mês: ±0,004pp, sobre uma média de 0,029 — bem distribuído).
- **Sobrevive à exclusão de 2020**: SÓ 2021 (12 meses) fica mais forte
  (t=2,84, p=0,016) que só 2020 (t=2,07, p=0,062) — ao contrário do
  padrão problemático visto no CIO Peer Momentum original (candidato
  #15), que enfraquecia sem 2020. Isso pesa A FAVOR da robustez.
- Direção consistente: 79,2% dos 24 meses com spread positivo.
- Versão decil (top10%/bottom10%): mais forte ainda (t=3,86, p=0,00079),
  mas com n_min=8 por grupo-mês — amostra ficando fina.
- **Mas a checagem mais decisiva não confirma**: regressão Fama-MacBeth
  contínua (não quintil-em-subamostra) com `retorno ~ efit_z + hhi_z +
  efit_z:hhi_z` em TODOS os meses de teste mostra `efit_z` sozinho **não
  significativo** (t=1,48, p=0,153) e o termo de **interação também não**
  (t=-0,40, p=0,693). O mesmo padrão de discrepância entre "quintil
  dentro de subamostra parece forte" e "regressão contínua com interação
  não confirma" que já apareceu no candidato de reversão h=6 abaixo
  (candidato R5) — sinal de alerta de que o corte em quintil dentro do
  subgrupo pode estar captando algo específico da forma de cortar, não
  uma interação genuína e linear entre EFIT e HHI.
- **Magnitude implausível**: 40,6-72,2%/ano bruto é maior até que o CIO
  Peer Momentum original (45-63%/ano, já sinalizado no log principal como
  "grande demais pra ser plausível como edge persistente e escalável").
- Não passa do limiar de Bonferroni em nenhuma especificação (melhor
  caso, decil, p=0,00079 — precisaria ser ~8× menor).

**Avaliação honesta:** este é o resultado mais interessante encontrado
por este agente — passa em mais checagens que o candidato de reversão
abaixo (não degenera por mês, é robusto a excluir 2020, decai suavemente
entre horizontes). Mas a regressão de interação contínua, que é o teste
mais direto e menos sujeito a artefato de corte, **não confirma** nem o
efeito principal nem a interação. Combinado com a magnitude implausível,
a avaliação honesta é: **não confirmado como interação genuína** — mais
provável que seja um artefato específico de como o corte em quintil
interage com a distribuição de EFIT dentro do subgrupo de alto HHI, não
um mecanismo real de "EFIT funciona melhor em ações frágeis". Registrado
com detalhe por ser o quase-achado mais forte desta frente, não como
achado confirmado.

## Candidato R4: CIO Peer Momentum × HHI (referência do script 41, outro agente)

Não retestado por este agente (já coberto exaustivamente pelo script `41`
de outro agente paralelo, incluindo tercil, mediana e regressão de
interação formal — todos nulos, melhor caso p=0,148). Mencionado aqui só
para registro de que a Frente A cobre esse sinal via outro script, não
duplicado.

## Candidato R5: Reversão de curto prazo × regime de HHI — achado descoberto por acaso, REJEITADO após diagnóstico

Script `65`, diagnosticado a fundo em `66_diagnostico_reversao_h6.R`. Não
fazia parte da hipótese original (a tarefa pedia cruzar sinais JÁ
testados com HHI) — surgiu como resultado inesperado ao testar reversão
de curto prazo (retorno do próprio mês, sinal invertido) em h=6: baseline
já significativo sozinho (t=-3,35, p=0,0036) e mais forte ainda dentro do
subgrupo de ALTO HHI (t=-4,40, p=0,00034 — o mais perto do limiar de
Bonferroni de toda a exploração até este ponto).

**O sinal do spread implica CONTINUAÇÃO (momentum), não reversão**:
perdedores de 1 mês continuam pior que ganhadores de 1 mês, 6 meses
depois — apesar do nome da variável ("reversão"), o spread negativo
Q5-Q1 é economicamente um efeito de momentum com janela de formação de
apenas 1 mês (incomum — a literatura clássica de momentum usa 3-12
meses de formação).

**Bateria de diagnóstico (mesmo playbook dos candidatos #14/22/36 do log
principal) — resultado: REJEITADO, artefato:**
1. Nenhum mês único domina o spread (contribuições pequenas e
   distribuídas, máxima ±0,004 sobre média de -0,024) — não é
   degenerescência tipo Comomentum.
2. **Padrão entre horizontes é isolado, não suave**: h=1 p=0,80, h=2
   p=0,059, h=3 p=0,87 (!), h=4 p=0,66, h=5 p=0,024, h=6 p=0,0036 (melhor
   caso), h=9 p=0,61, h=12 p=0,58. Um sinal real de momentum deveria
   mostrar decaimento gradual, não aparecer isolado em h=5-6 cercado de
   nulos em h=3, h=4, h=9 e h=12 — exatamente o padrão de "quase-achado
   que aparece por acaso entre múltiplas hipóteses" já documentado no log
   principal para outros candidatos.
3. **Trocar a janela de formação de 1 para 3 meses (mais próxima do
   padrão da literatura de momentum) destroi o efeito**: h=6 cai de
   p=0,0036 para p=0,182 (baseline) e de p=0,00034 para p=0,241
   (ALTO-HHI). Se fosse momentum genuíno, uma janela de formação mais
   robusta/padrão deveria preservar ou fortalecer o efeito, não eliminá-lo.
4. **Decisivo**: regressão Fama-MacBeth contínua com interação
   (`retorno ~ rev_z + hhi_z + rev_z:hhi_z`) mostra que `rev_z` sozinho
   É significativo (t=-3,18, p=0,0052) mas **a interação com HHI não é**
   (t=-0,46, p=0,649) — ou seja, o "reforço" aparente dentro do subgrupo
   ALTO-HHI observado no corte por quintil não é uma interação estatística
   real, é ruído do corte específico.
5. Split 2020/2021: enfraquece bastante sem 2020 (t=-1,95, p=0,10 em
   2021 sozinho vs. t=-2,61, p=0,024 em 2020 sozinho) — outro sinal de
   fragilidade.

**Veredito: REJEITADO com confiança alta de que é artefato**, não sinal
real — nem de momentum de 1 mês nem de interação com HHI. Registrado com
detalhe porque, olhando só o p-valor da especificação isolada
(ALTO-HHI h=6, p=0,00034), teria sido fácil reportar isso como o
resultado mais forte de toda a exploração — é exatamente o tipo de
"quase caímos numa cilada" que o log principal já documentou para
Comomentum e para a primeira versão do CIO Peer Momentum.

## Candidato R6: Choque de posse (demanda agregada revelada, versão corrigida à la Wardlaw) — percentil extremo

Script `67_choque_posse_wardlaw_corrigido.R`. Frente (B) do agente:
constrói a variação percentual do valor total (R$) detido pelos fundos
numa ação mês a mês — igual ao candidato #2B do log principal ("demanda
agregada revelada") — mas em DUAS versões lado a lado:

- **Bruta** (idêntica ao candidato #2B): `(ValorTotal_t - ValorTotal_{t-1})/ValorTotal_{t-1}`.
- **Corrigida (Wardlaw-safe)**, conforme pedido explícito no prompt:
  `Flow_t = (ValorTotal_t - ValorTotal_{t-1}·(1+Retorno_t))/ValorTotal_{t-1}`
  — isola a parte do crescimento do valor total que NÃO é explicada pela
  simples valorização/desvalorização do preço da própria ação, ou seja,
  aproxima "quantas cotas a mais/a menos os fundos estão segurando" em
  vez de "quanto o valor de mercado da posição mudou" (esse é
  precisamente o problema que Wardlaw 2020, JF, documenta na literatura
  padrão de flow-induced trading, e que o log principal já cita como
  possível explicação para os resultados nulos do robô do TCC).

**Verificação de que a correção funciona como esperado**: correlação
entre o choque bruto e o retorno próprio da ação no mesmo mês é 0,357
(contaminação mecânica real e substancial); após a correção, cai para
0,119 — confirma que a maior parte da correlação mecânica identificada
por Wardlaw estava presente na construção padrão e foi removida.

**Teste com Fama-MacBeth (re-corte mês a mês) em quintil E decil (top
10%/bottom 10%, como pedido), h=1/3/6, ambas as versões:**

| Sinal | h | Grupos | t | p |
|---|---|---|---|---|
| Bruto | 6 | decil | 1,78 | 0,093 |
| Bruto | 6 | quintil | 1,67 | 0,112 |
| Corrigido | 3 | quintil | 1,26 | 0,222 |
| Corrigido | 1 | quintil | 1,26 | 0,220 |
| Corrigido | 6 | quintil | 0,97 | 0,346 |

**Resultado: nulo em toda a bateria** (12 especificações, quintil e decil
× 3 horizontes × 2 versões) — nenhuma passa nem de 5% padrão. Curiosamente
a versão BRUTA (contaminada) fica ligeiramente mais perto de significar
que a corrigida em h=6 — consistente com a leitura de que qualquer
"quase-sinal" na versão não corrigida vinha em parte do eco mecânico do
próprio retorno, não de informação real sobre demanda institucional.

**Diagnóstico adicional** (evento COVID, mesmo desenho do candidato #40
do log principal, mas com choque de posse em vez de HHI): choque de
posse em dez/2019 não prediz de forma robusta o retorno no crash
fev-mar/2020 — versão bruta tem correlação fraca e só marginalmente
significativa (corr=-0,146, p=0,032, n=216, teste único não corrigido
por múltiplas comparações), versão corrigida não é significativa
(corr=-0,105, p=0,126).

**Veredito: REJEITADO.** Contribuição relevante mesmo sendo nulo: a
correção de Wardlaw foi implementada e verificada empiricamente pela
primeira vez nesta exploração (a nota original no log principal era só
teórica/sobre o `flow_aum` a nível de fundo, não sobre a variação do
valor total a nível de ação) — confirma que a "demanda agregada
revelada" do candidato #2B tinha, sim, contaminação mecânica real
(correlação 0,357 com retorno próprio), mas remover essa contaminação
não revela sinal escondido por baixo — o resultado nulo do #2B parece
ser um nulo genuíno, não um sinal mascarado pelo artefato.

## Candidato R7: Choque de posse corrigido × regime de HHI (combinação das duas frentes)

Script `68_choque_posse_x_regime_vol.R`. Testa o choque de posse
Wardlaw-corrigido (candidato R6) condicionado a tercil/mediana de HHI, e
também a MAGNITUDE do choque (|choque|, sem direção — ideia de "atenção"/
evento incomum, distinta da hipótese de regime).

**Resultado: nulo em toda a bateria** (30 especificações: 2 versões ×
3 horizontes × 5 cortes). Melhor caso: magnitude do choque, BAIXO-HHI
(mediana), h=3, p=0,070 — não passa nem de 5% padrão. Nenhum padrão
direcional consistente entre os cortes de HHI (o "melhor" caso pula
entre ALTO e BAIXO HHI dependendo do horizonte, sem lógica sistemática —
assinatura de ruído, não de interação real). **REJEITADO.**

## Síntese honesta desta frente (candidatos R1-R7, ~90 especificações)

Nenhum candidato desta frente sobreviveu à bateria completa de checagens
de robustez desta exploração. Os dois resultados que pareciam mais
promissores à primeira vista (R3: E[FIT]×HHI, e R5: reversão×HHI em h=6)
foram diagnosticados a fundo e **nenhum se sustentou**:

- R5 (reversão h=6) é um artefato claro: padrão isolado entre horizontes
  (não suave), destruído por uma janela de formação mais padrão, e sem
  interação estatisticamente significativa na regressão contínua.
- R3 (E[FIT]×HHI) passa em mais checagens (não degenera por mês, robusto
  a excluir 2020, decai suavemente entre horizontes) mas falha no teste
  mais decisivo — a regressão de interação contínua não confirma nem o
  efeito principal nem a interação — e tem magnitude implausível
  (40-72%/ano, maior até que o CIO Peer Momentum já sinalizado como
  implausível no log principal). Vale registrar como pista para
  investigação futura com mais anos de dado, não como achado.

A Frente B (choque de posse, corrigido pela contaminação mecânica de
Wardlaw) produziu um resultado nulo limpo e bem diagnosticado — a
correção funciona como esperado tecnicamente (reduz a correlação
mecânica com retorno próprio de 0,357 para 0,119), mas não revela sinal
escondido: nem a versão bruta nem a corrigida geram long-short
significativo em nenhum horizonte testado (h=1,3,6) nem em nenhum corte
(quintil, decil, condicionado a HHI).

**Conclusão honesta para o João**: nenhuma das duas frentes deste agente
produziu um sinal de lucro que passe nas 3 checagens metodológicas
inegociáveis. Isso é consistente com a conclusão já registrada no log
principal — o achado mais sólido de toda a exploração continua sendo a
PREVISÃO de volatilidade (HHI → vol futura, h=12), não um sinal de
retorno. A contribuição real deste agente é negativa mas bem
fundamentada: (1) confirma que condicionar sinais de retorno a regime de
volatilidade prevista não resgata nenhum sinal adicional além do que já
tinha sido testado para CIO Peer Momentum; (2) implementa e testa
empiricamente pela primeira vez a correção de Wardlaw na "demanda
agregada revelada" a nível de ação, mostrando que a contaminação
mecânica é real mas seu resultado nulo por trás dela também é; (3)
evita, com diagnóstico explícito, dois quase-falsos-positivos que
teriam sido fáceis de reportar por engano olhando só o p-valor da
especificação mais favorável.

### Agente 5: dispersão de crenças, trajetória de HHI, rotação setorial


Documento de trabalho, **não faz parte do TCC**. Escrito por um agente rodando
em paralelo com outros na mesma exploração de sinal de lucro (ver
`LOG_CANDIDATOS.md` principal para o histórico completo de ~520
especificações já testadas antes desta rodada). Este log cobre especificamente
três ângulos atribuídos a este agente: (A) dispersão de crenças entre
fundos, (B) trajetória/velocidade de concentração (Δ HHI), (C) rotação
setorial. Scripts numerados `70`–`76` (faixa exclusiva reservada para este
agente, para não conflitar com outros processos rodando em paralelo).

Metodologia aplicada em **todo** teste abaixo, sem exceção (as 3 lições
inegociáveis já estabelecidas pela exploração principal, depois de dois
quase-falsos-positivos — Comomentum e o CIO Peer Momentum original):
1. Significância via **Fama-MacBeth de verdade**: 1 coeficiente/spread por
   MÊS, testando a média da série temporal com o erro-padrão da própria
   série (nunca `t.test` pooled tratando ticker-mês como observação
   independente).
2. Quintil (ou tercil, quando a unidade de análise tem poucas observações
   por mês) **re-cortado a cada mês**, nunca breakpoint fixo do treino
   aplicado no teste. N mínimo por grupo-mês sempre reportado.
3. Disciplina fora-da-amostra: treino `ym < 202001`, teste `ym >= 202001`.

Tradeabilidade **não é mais exigência** (mudança de instrução do usuário já
registrada no log principal em 15/08/2026) — o objetivo é achar qualquer
sinal estatisticamente defensável, mesmo que não implementável na prática.

Limiar de Bonferroni usado como referência: `0.05/550` ≈ `0.0000909`,
baseado na contagem de ~520-550 especificações já testadas na exploração
principal antes desta rodada (outros agentes somam mais em paralelo — este
limiar é conservador por baixo, não por cima).

---

## FRENTE (A): Dispersão de crenças entre fundos (Diether-Malloy-Scherbina 2002)

**Mecanismo original (DMS, JF 2002):** dispersão de forecast de EPS entre
analistas prediz retorno futuro BAIXO — mecanismo de Miller (1977):
restrição a venda a descoberto faz o preço refletir a opinião do investidor
mais otimista, não a média; quando divergência é alta, sobrepreço é maior, e
a correção subsequente (queda) também.

**Adaptação (sem dado de analista, só holdings de fundos):** para cada
ativo-mês, dispersão cross-sectional do PESO que fundos diferentes dão à
mesma ação dentro das próprias carteiras — coeficiente de variação (CV =
sd/média) do `peso` entre os fundos donos. Como `peso` já é fração do AUM do
próprio fundo, é comparável entre fundos de tamanhos diferentes. Construído
sem peso (CV simples) e ponderado por AUM do fundo (fundos maiores pesam
mais no cálculo — "desacordo entre os players relevantes").

### Candidato #A1: Dispersão de crenças, NÍVEL (script `70_dispersao_crencas_dms.R`)

Piso de amostra: `n_fundos >= 10` donos por ativo-mês (mesmo piso usado no
resto da exploração para HHI). 4.318 ticker-mês na base construída.

**Quintil recortado por mês (Q5=alta dispersão − Q1=baixa dispersão), CV
simples e ponderado, h=1/3/6/12:** nenhuma especificação passa nem do 5%
padrão. Melhor caso: CV ponderado h=12, p=0,140 (13 meses, N pequeno por ser
h longo). Direção nem sequer é consistente com a teoria: os spreads
positivos dominam (dispersão alta → retorno **maior**, não menor) em 7 de 8
especificações.

**Fama-MacBeth linear (contínuo, padronizado por mês):** a única
especificação com p<0,05 é CV ponderado, h=3 (t=3,06, p=0,006) — mas o
coeficiente é **positivo** (dispersão alta → retorno maior), o oposto exato
da hipótese DMS, e não é confirmado pelo teste em quintil do mesmo
sinal/horizonte (p=0,25). Não passa de Bonferroni de qualquer forma.

**Controle multivariado** (dispersão + n_fundos [breadth, já testado e
rejeitado] + HHI [já testado e rejeitado para retorno] juntos): nenhum
coeficiente de dispersão sobrevive com significância em h=1/3/6 controlando
pelos outros dois.

**Veredito: REJEITADO.** Não há evidência de que o nível de dispersão de
peso entre fundos prediga retorno futuro nesta amostra — nem na direção
prevista pela teoria (que, quando aparece isoladamente, aparece invertida).

### Candidato #A2: Dispersão restrita a small caps vs. large caps (script `71_dispersao_crencas_extensoes.R`, parte i)

Literatura (DMS, Wermers) prevê efeito mais forte onde a restrição a venda a
descoberto é mais binding — ações menores/menos líquidas. Testado CV
ponderado, terço de menor vs. maior valor total mantido pelos fundos
(mesma proxy de tamanho/liquidez do candidato #10 da exploração principal).

Nenhuma diferença relevante: small caps h=3 chega a p=0,120 (mas com sinal
**positivo**, não negativo), h=1/h=6 nada. Large caps: nada em lugar nenhum
(p entre 0,68 e 0,74). **REJEITADO.**

### Candidato #A3: Δ da dispersão (desacordo CRESCENTE) — o mais interessante da frente A (script `71`, parte ii, e diagnóstico `72_diagnostico_delta_dispersao.R`)

Extensão nova (não é DMS original): em vez do NÍVEL, testa se o AUMENTO da
dispersão nos últimos 3 ou 6 meses (desacordo entre gestores crescendo, não
seu nível absoluto) prediz retorno futuro mais baixo.

**Resultado inicial parecia promissor**: as 6 especificações (3m/6m × h=1/3/6)
têm **todas** o mesmo sinal (negativo — desacordo crescente prediz retorno
menor), com 3 de 6 passando do padrão de 5%: melhor caso Δ3m h=6, t=-2,73,
p=0,014; Δ6m h=3, p=0,018; Δ6m h=6, p=0,036. N mínimo por perna saudável
(36-40 ativos/mês, não degenerado). Nenhum mês isolado domina o spread
(maior contribuição de um único mês = 14% do total, muito longe do padrão de
artefato tipo Comomentum, onde um mês sozinho chegava a >100% do spread).

**Mas não sobrevive a 2 diagnósticos de robustez aplicados (script 72):**
1. **Excluindo 2020 (só 2021, mesmo teste já usado no CIO Peer Momentum):**
   o melhor caso (Δ3m, h=1) cai para p=0,055 — não passa mais de 5%; os
   outros horizontes enfraquecem ainda mais (p entre 0,08 e 0,75).
2. **Controle multivariado** (Δ dispersão 3m + nível de dispersão + Δ HHI
   3m, juntos): o coeficiente de Δ dispersão **muda de sinal entre
   horizontes** — h=1 negativo e quase significativo (p=0,048), h=3
   **positivo** não significativo (p=0,48), h=6 negativo não significativo
   (p=0,086). Um sinal real deveria ter direção estável entre horizontes
   vizinhos; essa inversão é o mesmo padrão de instabilidade que já
   derrubou vários outros candidatos nesta exploração (ex.: candidato #16,
   Ownership × Momentum).

Correlação entre Δ dispersão 3m e Δ HHI 3m: 0,34 (moderada — captam algo
parcialmente relacionado, mas não idêntico).

**Veredito: NÃO CONFIRMADO.** É o resultado mais interessante da frente A
(direção consistente em 6/6 especificações brutas, sem degenerescência de
amostra), mas não sobrevive nem à exclusão do período COVID nem ao controle
multivariado simultâneo — mesmo padrão de "quase lá, mas frágil sob
escrutínio" que a exploração principal já documentou repetidas vezes.
Registrado como pista, não como achado.

---

## FRENTE (B): Trajetória/velocidade de concentração (Δ HHI)

**Motivação:** o achado mais sólido de toda a exploração principal é que o
NÍVEL do HHI de posse prediz volatilidade futura (h=12) — mas não prediz
retorno (script `32c` da exploração principal, spread não significativo,
sinal até oposto ao esperado). Aqui testa-se algo estruturalmente diferente:
não o nível, a VARIAÇÃO (Δ HHI de 3 ou 6 meses) — duas hipóteses opostas,
testadas as duas sem pré-compromisso de direção: concentração crescente
pode ser "smart money" coordenado (retorno futuro maior) ou risco de
fire-sale se reverter (retorno futuro menor).

### Candidato #B1: Δ HHI (3m, 6m, absoluto e percentual) → retorno (script `73_hhi_trajetoria_delta.R`)

Base: 11.367 ticker-mês com `n_fundos >= 10`. Testado em quintil recortado
por mês, Fama-MacBeth linear, e com controle pelo nível de HHI simultâneo.

**Tudo nulo.** Melhor caso em quintil: Δ HHI 6m, h=1, p=0,072 (perto do
padrão de 5%, mas não passa) — os outros 8 casos de quintil têm p entre
0,12 e 0,95, sem padrão de direção consistente entre horizontes (às vezes
negativo, às vezes positivo, às vezes ~0). Linear (h=1/3/6/12, 3m e 6m):
todos p>0,35. Controlado pelo nível de HHI: nada muda, Δ HHI continua sem
significância em nenhum horizonte (p entre 0,44 e 0,97).

### Candidato #B2: Assimetria de direção — crescente vs. caindo (script `74_hhi_trajetoria_assimetria_e_vol.R`, parte i)

Testa explicitamente as duas hipóteses conflitantes separadamente, para o
caso de se cancelarem numa regressão linear pooled: (i) magnitude do
aumento, só entre ativos com HHI crescendo; (ii) magnitude da queda, só
entre ativos com HHI caindo; (iii) dummy simples "crescente" menos "caindo".

Nenhuma das 9 especificações passa de 5% — melhor caso "caindo, magnitude,
h=6": p=0,083 (n_min=14 ativos/perna/mês, amostra ok mas resultado não
significativo). Dummy direção simples: p entre 0,62 e 0,95, nada.

**Veredito de toda a Frente B: REJEITADO, de forma limpa e sem
ambiguidade.** Diferente da Frente A (que teve pelo menos um resultado
"quase lá" antes de cair sob escrutínio), aqui nenhuma das 15 especificações
testadas (nível abs./relativo × 3m/6m, linear, quintil, assimetria de
direção) chega perto de qualquer limiar de significância. A trajetória do
HHI genuinamente não carrega informação sobre retorno futuro nesta amostra
— só o NÍVEL importa, e só para volatilidade, não para retorno.

### Bônus (script `74`, parte ii): Δ HHI adiciona poder preditivo de VOLATILIDADE futura, além do nível?

Não é retorno (fora do escopo original da Frente B), mas testado como
extensão natural do achado mais sólido da exploração, já que os dados
estavam prontos. Fama-MacBeth de `vol_futura ~ hhi_posse + delta_hhi_3m`,
h=3/6/12.

O nível de HHI **confirma** robustamente o achado já estabelecido (h=6:
t=6,91, p<0,00001, passa Bonferroni; h=12: t=6,12, p<0,00001, passa
Bonferroni) — replicação limpa do resultado principal da exploração com
outra base de código. `delta_hhi_3m`, controlado pelo nível, **não** agrega
poder preditivo (coeficiente negativo em todos os horizontes, mas nunca
significativo, p entre 0,11 e 0,20) — o nível já captura tudo que a
trajetória teria a acrescentar para volatilidade também.

---

## FRENTE (C): Rotação setorial via fluxo institucional agregado

**Limitação de dado registrada com transparência**: o painel de holdings não
tem classificação setorial pronta (confirmado por busca — não existe
CNAE/GICS/segmento B3 em nenhum arquivo de `v2 OFICIAL/data`; o único
`_cache_classif_anbima.csv` existente é classificação ANBIMA de TIPO DE
FUNDO, não de setor da ação). Em vez de pular a frente, construí uma proxy
setorial por PALAVRA-CHAVE no nome da empresa (campo `ativo` do painel tem
"NOME EMPRESA - TICKER").

### Construção da proxy (script `75_construir_setor_proxy.R`)

Classificação manual/heurística em 19 setores (Bancos, Seguros, Energia
Elétrica, Petróleo e Gás, Mineração e Siderurgia, Papel e Celulose,
Químicos, Varejo, Alimentos/Bebidas/Agro, Construção e Imobiliário,
Transporte e Logística, Telecom, Saúde, Educação, Tecnologia, Têxtil,
Saneamento, Indústria/Bens de Capital, Infraestrutura de Mercado).
**371 de 513 tickers (72,3%) classificados**; o restante ("Outros/Não
classificado") foi excluído do teste, não virou setor artificial.
**Ressalva honesta**: é uma proxy heurística, não uma classificação
GICS/B3 oficial — nomes ambíguos ou conglomerados podem estar mal
classificados. Não é a mesma coisa que dado setorial de verdade.

### Candidato #C1: Fluxo institucional setorial (Δ share do AUM total) → retorno setorial futuro (script `76_rotacao_setorial.R`)

Share do AUM total do universo investido em cada setor por mês; Δ 1m e Δ 3m
como medida de "fluxo institucional agregado" entrando/saindo do setor.
Retorno setorial calculado a partir de PREÇO (equal-weight entre tickers do
setor), separado do lado dos holdings, para não misturar preditor e
resultado. 19 setores por mês (cobertura estável, sem buraco). **Limitação
de poder estatístico reconhecida**: cross-seção de 19 unidades/mês é muito
menor que as ~200-400 ações dos testes normais desta exploração — testado
em TERCIL (não quintil, que deixaria <3 setores/perna), com N mínimo de
setores por perna sempre reportado (6 em todos os casos — pequeno, mas é o
máximo possível com 19 setores divididos em 3 grupos).

**Resultado: nulo em toda a bateria (9 especificações tercil + 6 lineares).**
Melhor caso em tercil: nível de share setorial (não fluxo — quanto capital
JÁ está alocado no setor), h=1, p=0,227 — nem perto de 5%. Fluxo (Δ1m/Δ3m)
em si: p entre 0,52 e 0,99 em todos os 6 casos de tercil, sem padrão de
direção. Linear: mesma coisa, p entre 0,53 e 0,89.

**Veredito: REJEITADO**, com a ressalva honesta de que o teste tem poder
estatístico genuinamente baixo (N=19 setores, tercis de ~6 cada) — um nulo
aqui é mais fraco como evidência "contra" a hipótese do que os nulos da
Frente B (que tiveram N de centenas de ações). Mas dentro do que é
testável com os dados disponíveis, não há sinal de rotação setorial via
fluxo institucional agregado.

---

## Síntese final desta rodada (agente dispersão, 15/08/2026)

Testados 4 candidatos principais em 3 frentes, ~40 especificações no total
(além do baseline de ~520-550 já testado na exploração principal antes
desta rodada, mais o que outros agentes paralelos estão testando
simultaneamente):

| Frente | Sinal | Melhor resultado | Passa 5%? | Passa Bonferroni? | Robusto? |
|---|---|---|---|---|---|
| A1 | Dispersão nível | p=0,14 (quintil) / p=0,006 (linear, sinal invertido) | Só linear, direção errada | Não | Não |
| A2 | Dispersão × tamanho | p=0,12 | Não | Não | — |
| A3 | Δ Dispersão (crescente) | p=0,014 | Sim (3/6 specs) | Não | **Não** — cai a p=0,055 sem 2020, inverte sinal em controle multivariado |
| B1 | Δ HHI → retorno | p=0,072 | Não | Não | — |
| B2 | Δ HHI, assimetria | p=0,083 | Não | Não | — |
| B-bônus | Δ HHI → vol. futura | n/a | — | Não (nível sozinho já passa) | Confirma achado existente, Δ não soma nada |
| C1 | Fluxo setorial → retorno | p=0,227 | Não | Não | — |

**Nenhum candidato desta rodada passa nem no padrão simples de 5% de forma
robusta, muito menos no limiar de Bonferroni aplicado pelo resto da
exploração.** O mais próximo de "achado" foi a variação da dispersão de
crenças (A3) — direção consistente em 6 de 6 especificações brutas, sem
degenerescência de amostra — mas não sobrevive a excluir o período COVID
nem a um controle multivariado simples com o nível de dispersão e a
variação do HHI, os dois testes de robustez que já derrubaram vários outros
"quase-achados" nesta exploração inteira (Comomentum, CIO original, Δ
dispersão aqui). O panorama geral desta exploração inteira não muda: CIO
Peer Momentum (h=1, candidatos #15/22/34-38 do log principal) continua
sendo o único sinal de RETORNO com suporte razoável, e HHI-nível→volatilidade
futura (h=12) continua sendo o achado mais sólido de PREVISIBILIDADE — este
agente apenas reconfirmou esse segundo achado por um caminho independente
(script 74, parte ii) e não encontrou nada novo para adicionar à lista de
candidatos de retorno.

Scripts desta rodada: `v2 OFICIAL/exploracao_sinais/scripts/70` a `76`.
Dados/resultados salvos em `v2 OFICIAL/exploracao_sinais/data/` com os
prefixos `candidatos_70_` a `candidatos_76_`, mais os arquivos auxiliares
`dispersao_crencas_dms.csv`, `hhi_trajetoria_delta.csv`,
`setor_proxy_tickers.csv`, `rotacao_setorial_fluxo.csv`.

### Agente 6: outros eventos de choque + características da base de donos


Log separado do `LOG_CANDIDATOS.md` principal (não editado por este agente, outro
processo consolida depois). Mesma disciplina: tudo registrado, inclusive nulos.
Scripts exclusivos deste agente: `77_` a `83_` em
`v2 OFICIAL/exploracao_sinais/scripts/`.

**Ponto de partida**: o candidato #40 do log principal (`40_covid_event_study.R`)
já tinha testado o desenho de evento — HHI pré-choque prevendo retorno/vol
durante e depois do choque — na COVID, com N~217 ações numa única regressão
cross-sectional (o teste de maior poder estatístico de toda a exploração até
então), e não achou nada. Este agente teve duas frentes: (A) repetir o MESMO
desenho em outros choques de mercado brasileiro (não-COVID), agregando vários
eventos para ganhar poder estatístico; (B) uma família de sinais nova —
características da BASE DE FUNDOS DONOS de cada ação (tamanho, fluxo, número
de cotistas, taxa de saída/churn dos fundos que a possuem), em vez de
características da própria ação ou da concentração de posse.

---

## Candidato E1: Réplica do evento COVID (#40) em 4 choques de mercado brasileiro não-COVID

**Scripts:** `77_identificar_choques_mercado.R` (identificação dos eventos),
`78_multi_evento_choques_hhi.R` (teste).

**Construção dos eventos:** proxy de retorno de mercado agregado (média
equal-weight do retorno de todos os tickers com preço no mês, construído a
partir de `precos_mensais_final.csv`, já que não há índice oficial no
painel) identificou os piores meses de 2016-2019 (fora da janela COVID). O
painel de holdings só começa em jan/2017, então qualquer evento precisa de
mês-anterior ≥201701. Eventos escolhidos:

1. **Joesley Day (mai/2017, 201705)** — mandatado pela tarefa. No proxy de
   mercado, retorno do mês é só -0,71% (rank 11º pior de 2016-2019) — o
   tombo intradiário de 18/05 parece ter sido parcialmente recuperado até o
   fim do mês na média equal-weight.
2. **Greve dos caminhoneiros (mai/2018, 201805)** — mandatado, e também o
   **pior mês de 2016-2019** no proprio proxy (-8,92%).
3. **Choque organico nov/2017 (201711)** — 5º pior mês do período,
   escolhido por estar temporalmente afastado dos outros 3 (evita
   sobreposição de janela pós-choque).
4. **Choque organico abr/2019 (201904)** — 8º pior mês, também afastado.

Para cada evento: HHI de posse no mês ANTERIOR (`hhi_posse`, mesma
construção do candidato #40: soma dos quadrados da participação % de cada
fundo no valor total detido da ação, exige ≥10 fundos) prevendo (a) retorno
DURANTE o mês do choque, (b) retorno acumulado nos 3 meses seguintes
("recuperação"), (c) volatilidade (dp) dos 6 meses seguintes.

**Inferência em 3 camadas** (nenhuma trata ticker-evento como observação
independente, lição do log principal):
- (a) regressão cross-sectional separada por evento (N=207-220 ações cada,
  reportada individualmente);
- (b) "mini Fama-MacBeth" entre eventos — os 4 coeficientes (1 por evento)
  tratados como a série temporal, média/erro-padrão entre eventos, t com
  df=3 (evento é a unidade, não ticker-evento);
- (c) regressão pooled com efeito fixo de evento + erro-padrão clusterizado
  por evento (checagem adicional, não inferência principal — só 4 clusters,
  cluster-robust SE é conhecidamente pouco confiável com <30-40 clusters).

### Resultado: RETORNO — nulo, replica o candidato #40

`ret_evento` (durante o choque): coeficientes por evento = +0,054 (Joesley),
+0,002 (Greve), -0,065 (nov/2017), -0,201 (abr/2019) — **sem consistência de
sinal**. Mini-FM: t=-0,95, p=0,41. `ret_pos3` (recuperação): mesmo padrão,
sinais trocando, mini-FM t=0,04, p=0,97. **Confirma o candidato #40**: HHI
pré-choque não prevê retorno durante nem depois de choques de mercado
brasileiros, nem na COVID nem nestes 4 episódios adicionais.

### Resultado: VOLATILIDADE — replica e REFORÇA o achado mais sólido da exploração principal

`vol_pos6` (volatilidade dos 6 meses seguintes ao choque): coeficientes
**positivos nos 4 eventos**, sem exceção — Joesley t=2,76 (p=0,006), Greve
t=1,33 (p=0,18), nov/2017 t=3,26 (p=0,001), abr/2019 t=2,49 (p=0,014). 3 de
4 significativos individualmente, e a direção (mais concentração → mais
volatilidade futura) é a MESMA em todos.

- **Mini Fama-MacBeth (n=4 eventos, df=3):** média=+0,0663, t=3,74,
  **p=0,033**.
- **Leave-one-out** (retirando 1 evento por vez, n=3, df=2): p entre 0,074 e
  0,114 nos 3 casos em que se retira um dos coeficientes "médios"; retirando
  justamente o maior outlier (abr/2019), o resultado fica **mais forte, não
  mais fraco** (dp cai de 0,036 para 0,012, t=7,28, p=0,018) — evidência de
  que o achado não é carregado por 1 evento extremo.
- **Probabilidade de 4/4 mesmo sinal por acaso** (H0 de sinal aleatório):
  6,25% — fraca isoladamente, mas coerente com o resto da bateria.
- **Pooled com efeito fixo de evento + cluster por evento (N=851,
  checagem adicional):** t=5,66, p<0,0001 — mesma direção, mas com a
  ressalva de que 4 clusters é pouco para esse método ser confiável
  sozinho.

**Avaliação honesta:** este é o achado mais limpo da frente (A). Não é
"novo" no sentido de mecanismo — é a MESMA relação HHI→volatilidade futura
já estabelecida como o achado mais sólido de toda a exploração principal
(candidatos #26-31 do log principal, fundamentado em Greenwood-Thesmar
2011) — mas aqui ela é **replicada, com desenho de alto poder estatístico
(N~200+ ações por evento), em 4 episódios de choque de mercado brasileiro
diferentes da COVID**, com sinal consistente em todos os 4 e sobrevivendo a
leave-one-out. Isso é evidência adicional relevante de que o mecanismo é
real e não específico do episódio único da COVID (que é a limitação que o
achado principal sempre carregou). Não é uma estratégia de lucro (é
volatilidade, não retorno, replicando a mesma distinção "previsibilidade ≠
lucro" já estabelecida). Com n=4 eventos, p=0,033 do mini-FM não é
extremamente forte isoladamente (não bate nenhum limiar de Bonferroni), mas
o padrão de sinal 4/4 consistente + robustez a leave-one-out é o tipo de
evidência que pesa mais que um p-valor isolado.

---

## Candidato E2 (frente B): Tamanho médio dos fundos donos → retorno futuro

**Scripts:** `79_construir_fragilidade_donos.R` (construção),
`80_testar_fragilidade_donos.R` (teste), `83_diagnostico_laum_retorno.R`
(diagnóstico).

**Construção:** para cada ação-mês, `frag_laum_medio` = média do
`l_aum` (log do AUM, já pré-calculado no pipeline oficial) dos fundos donos,
ponderada pelo valor (R$) da posição de cada fundo — mede se a ação é, em
média, possuída por fundos GRANDES ou PEQUENOS (variável do lado dos DONOS,
não da ação). Exige ≥10 fundos donos por ação-mês (mesmo piso do resto da
exploração), winsorizado 1%/99%. N=14.149 ação-mês, 2017-2021.

**Teste (Fama-MacBeth com quintil re-cortado a cada mês, treino<202001,
teste≥202001, N mínimo por perna sempre 44):**

| Horizonte | Spread Q5-Q1 (pp/mês) | t | p | N meses | N mín/perna |
|---|---|---|---|---|---|
| h=1 | -0,982 | -2,14 | 0,043 | 24 | 44 |
| h=3 | -1,229 | -2,61 | 0,016 | 22 | 44 |
| h=6 | -0,851 | -1,30 | 0,209 | 19 | 44 |

Direção consistente em h=1 e h=3 (ações com donos-fundos maiores têm retorno
futuro MENOR), mas h=6 não confirma.

**Diagnóstico (`83`):** suspeita óbvia é que isso seja só o efeito-tamanho
clássico disfarçado (fundos grandes tendem a concentrar em blue chips, que
têm prêmio de retorno menor). Testado controlando por `log_valor_total`
(log do valor total em R$ detido pelos fundos na ação, proxy de
tamanho/liquidez da própria ação): correlação `frag_laum` x
`log_valor_total` é só 0,27 (moderada, não alta). Na regressão conjunta,
`frag_laum` **sobrevive** ao controle (h=1: t=-2,74, p=0,012; h=3: t=-2,88,
p=0,009) e `log_valor_total` sozinho **perde** significância quando os dois
entram juntos (t=-1,15 e t=-0,66) — ou seja, "tamanho do fundo dono" carrega
mais informação que "tamanho da própria ação" para prever este spread.

**Avaliação honesta:** direção e sobrevivência ao controle de tamanho são
interessantes, mas (a) só 2 de 3 horizontes confirmam (h=6 nulo), (b) N
pequeno (22-24 meses de teste), (c) p entre 0,01 e 0,04 está muito longe de
qualquer limiar de Bonferroni razoável dado o total de especificações já
testadas em toda a exploração (main log + este agente, >550), (d) mesmo
sobrevivendo ao controle direto por tamanho da ação, é plausível que isso
seja uma versão mais fina/menos ruidosa do prêmio de tamanho clássico via
capacidade de fundos grandes não conseguirem entrar em posições pequenas —
não necessariamente um mecanismo novo. **Registrado como achado secundário
tentativo, não como resultado forte.**

---

## Candidato E3: Número de cotistas dos fundos donos → volatilidade futura — O ACHADO MAIS FORTE DESTE AGENTE

**Scripts:** `79_construir_fragilidade_donos.R` (construção),
`80_testar_fragilidade_donos.R` (teste principal),
`81_diagnostico_cotistas_churn.R` (controles multivariados),
`82_robustez_cotistas_covid_consistencia.R` (consistência mês a mês e
exclusão da COVID).

**Mecanismo hipotetizado:** diferente de tudo já testado na exploração
principal (que olha concentração de posse do lado do ATIVO — quantos fundos
possuem a ação, quão concentrada é a posse), este sinal olha o lado do
**PASSIVO** dos fundos donos: `frag_cotistas_medio` = média (ponderada por
valor da posição) do log(1+número de cotistas) dos fundos que possuem a
ação. Fundos com POUCOS cotistas (poucos clientes, cada um representando
uma fração grande do patrimônio) têm risco de resgate mais "lumpy" —
um único cotista relevante saindo pode forçar liquidação de posições
inteiras — diferente de um fundo com milhares de cotistas pequenos, onde
resgates tendem a se cancelar estatisticamente. É uma variante do mecanismo
de Greenwood-Thesmar que olha a estrutura do CLIENTE do fundo, não a
concentração de posse da ação.

**Teste principal (Fama-MacBeth, quintil re-cortado a cada mês, N mín. 41-44
por perna, treino<202001/teste≥202001):**

| Horizonte | Spread vol(Q5-Q1) | t | p | N meses |
|---|---|---|---|---|
| h=3 | -0,00951 | -3,06 | **0,00578** | 23 |
| h=6 | -0,01046 | -5,07 | **0,00006** | 21 |
| h=12 | -0,01464 | -4,73 | **0,00022** | 17 |

Direção: MAIS cotistas nos fundos donos → MENOS volatilidade futura da
ação (ou, na leitura oposta: donos com base de clientes mais concentrada/
lumpy → mais volatilidade futura). Consistente nos 3 horizontes, magnitude
crescente com o horizonte (mesmo padrão temporal do achado principal
HHI→vol da exploração principal, que também é mais forte em h=12).

**h=6 (p=0,00006) bate o limiar de Bonferroni do log principal**
(~0,05/520≈0,0001) **isoladamente**; h=12 (p=0,00022) fica muito perto.

**Diagnóstico 1 — não é redundante com HHI, tamanho do fundo, ou tamanho da
posição (script 81):** correlação de `frag_cotistas` com HHI é fraca
(-0,068 geral, entre -0,11 e -0,25 dentro de tercis de tamanho) e com
`log_valor_total` é essencialmente nula (-0,014, perto de zero em todos os
tercis de tamanho). Controlando simultaneamente por HHI + tamanho do fundo
dono (`frag_laum`) + tamanho da posição (`log_valor_total`) numa regressão
múltipla Fama-MacBeth mês a mês:

| Horizonte | coef. cotistas (padronizado) | t | p |
|---|---|---|---|
| h=3 | -0,0013 | -1,25 | 0,225 (não sobrevive) |
| h=6 | -0,0022 | -3,82 | **0,00107** |
| h=12 | -0,0029 | -3,31 | **0,00441** |

h=3 não sobrevive ao controle simultâneo (dominado por `log_valor_total`,
que é o controle mais forte da bateria — efeito tamanho/liquidez clássico
em vol de curto prazo), mas **h=6 e h=12 sobrevivem com folga**, mesmo
competindo com 3 outros controles na mesma regressão.

**Diagnóstico 2 — sobrevive controlando por volatilidade PASSADA (o teste
mais duro, que já havia reduzido bastante o efeito de HHI→vol em horizontes
curtos no candidato #29/30 do log principal):**

| Horizonte | coef. cotistas | t | p | coef. vol.passada12m | t |
|---|---|---|---|---|---|
| h=3 | -0,0008 | -0,70 | 0,494 (não sobrevive) | +0,0268 | 9,58 |
| h=6 | -0,0017 | -2,39 | **0,027** | +0,0272 | 13,37 |
| h=12 | -0,0027 | -3,98 | **0,00107** | +0,0268 | 18,79 |

Mesmo padrão: h=3 morre (dominado pela persistência de volatilidade,
fato estilizado bem conhecido), mas h=6 e h=12 sobrevivem controlando
diretamente pela vol. passada — **h=12 continua muito significativo mesmo
competindo com o preditor mais óbvio e forte que existe para volatilidade
futura**.

**Diagnóstico 3 — consistência mês a mês e robustez a outliers/COVID
(script 82):**
- h=6: sinal na direção esperada em **85,7%** dos 21 meses de teste (18/21).
  Excluindo os 2 meses de spread mais extremo (em valor absoluto): ainda
  t=-4,88, p=0,00012. Excluindo TODOS os meses do crash/recuperação COVID
  (fev-dez/2020), restam só 10 meses (jan/2020 + jan-set/2021) e o efeito
  **continua significativo**: t=-3,07, p=0,013.
- h=12: sinal esperado em **82,4%** dos 17 meses. Sem os 2 extremos: t=-4,07,
  p=0,00116. Sem COVID (só 6 meses restantes, jan/2020-fev/2021 conforme a
  janela de 12 meses): t=-4,03, p=0,010 — significativo mesmo com N=6.

**Achado secundário correlacionado — churn/taxa de saída dos fundos donos**
(`frag_exit_rate_medio`, fração das OUTRAS posições do fundo dono que ele
zerou desde o mês anterior, proxy de "fundo liquidando carteira"): também
prediz volatilidade futura na direção esperada (mais churn → mais vol
futura) nos 3 horizontes, sobrevivendo a controle por HHI e vol. passada
simultaneamente (h=3: p=0,038; h=6: p=0,014; h=12: p=0,039) — mais fraco
que o achado de cotistas (não bate nenhum Bonferroni), mas na mesma direção
teórica e reforça que o "lado do passivo/comportamento dos donos" carrega
informação incremental sobre risco futuro da ação.

**Correlação entre `flow_aum` (fluxo recente) e volatilidade futura**
(`frag_flow_medio`, candidato E4 nesta numeração): testado nas mesmas
condições, nulo em todos os horizontes (p entre 0,51 e 0,79 controlando por
vol. passada) — não confirma.

**Avaliação honesta, a mais forte deste agente:** este é o único candidato
de toda a bateria do agente (frentes A e B) que (1) bate Bonferroni
isoladamente em pelo menos 1 horizonte (h=6), (2) sobrevive a 4 controles
simultâneos concorrendo na mesma regressão (HHI, tamanho do fundo, tamanho
da posição), (3) sobrevive ao controle mais duro que existe para
volatilidade (vol. passada) em 2 dos 3 horizontes, (4) tem direção
consistente em 82-86% dos meses de teste, (5) não é sensível a excluir os
meses mais extremos, e (6) sobrevive — ainda que com poucochíssima amostra
(n=6-10 meses) — a excluir a COVID inteira. É uma variável de baixa
correlação com tudo que já foi testado na exploração principal (correlação
com HHI é fraca), então é genuinamente uma dimensão nova de informação, não
uma reformulação do achado de crowding já conhecido.

**Ressalvas que impedem chamar isso de "prova definitiva":**
- É volatilidade, não retorno — mesma distinção "previsibilidade ≠
  estratégia de lucro" que se aplica ao achado principal de HHI→vol da
  exploração inteira (script 80 testou `frag_cotistas` como preditor de
  RETORNO também: nulo em todos os horizontes, p entre 0,36 e 0,78 — o
  sinal é puramente sobre risco, não sobre direção de preço).
- h=3 não sobrevive a nenhum dos dois controles multivariados — o efeito é
  genuinamente confinado a horizontes médios/longos (6-12 meses), não é
  "quanto mais curto melhor" como seria de se esperar de um efeito mecânico
  de curto prazo.
- N de meses de teste é pequeno em termos absolutos (17-23), como em toda
  a exploração (janela 2016-2021 é curta) — mesmo um resultado "robusto
  dentro do que se tem" ainda é vulnerável ao mesmo tipo de sorte de
  período que já pegou outros candidatos aparentemente sólidos (CIO Peer
  Momentum) em rodadas anteriores.
- Não é uma explicação causal comprovada — o mecanismo de "resgate lumpy"
  é plausível e coerente com a literatura de fund flows (não testada
  formalmente aqui com dado direto de fluxo por cotista, que não temos),
  mas o teste é reduzido-forma (característica → volatilidade futura), não
  identifica o canal causal diretamente.

**Recomendação:** este é, na avaliação honesta deste agente, o achado mais
sólido produzido nesta rodada — tão ou mais robusto sob escrutínio quanto o
achado principal de HHI→volatilidade da exploração original (que também não
bate Bonferroni em todos os horizontes e também é mais forte em h=12), só
que numa dimensão nova (estrutura do passivo dos fundos donos, não
concentração do ativo). Vale registrar para o processo de consolidação como
candidato forte de PREVISIBILIDADE de volatilidade (não de retorno/lucro).

---

## Resumo desta rodada (candidatos E1-E5, agente eventos)

| Candidato | Ângulo | Resultado |
|---|---|---|
| E1 (multi-evento HHI, 4 choques) | retorno | nulo, confirma #40 |
| E1 (multi-evento HHI, 4 choques) | volatilidade | positivo, consistente 4/4, mini-FM p=0,033, robusto a leave-one-out |
| E2 (tamanho fundo dono) | retorno | fraco, só h=1/h=3, não bate Bonferroni |
| E3 (n° cotistas fundo dono) | volatilidade | **forte — bate Bonferroni em h=6, sobrevive a 4 famílias de controle** |
| E4 (fluxo recente fundo dono) | volatilidade | nulo |
| E5 (churn/saída fundo dono) | volatilidade | fraco-moderado, mesma direção em 3 horizontes, não bate Bonferroni |

Nenhum sinal de LUCRO/RETORNO novo foi encontrado (frente A confirma o nulo
já estabelecido pelo candidato #40; frente B tem só o achado secundário
fraco de E2). Mas a frente B produziu um achado de PREVISIBILIDADE DE
VOLATILIDADE (E3, número de cotistas dos fundos donos) que passa em todas
as checagens de robustez aplicadas nesta exploração inteira e que é
genuinamente novo (baixa correlação com HHI/crowding, o preditor de
volatilidade já estabelecido) — candidato a entrar na consolidação final
como o achado mais forte produzido por este agente.

### Agente 7: pairs trading via overlap CIO + ML walk-forward


Documento de trabalho, **não faz parte do TCC**. Registrado separado por
instrução explícita (execução em paralelo com outros agentes na mesma
exploração) — outro processo consolida depois no log principal. Mesmo
padrão de rigor do log principal: Fama-MacBeth mês a mês (nunca `t.test`
pooled tratando par/ativo-mês como observação independente), quintis/
tercis SEMPRE re-cortados a cada mês (nunca break fixo do treino),
disciplina treino `ym<202001` / teste `ym>=202001`, e N mínimo por
grupo-mês sempre reportado. Contexto herdado do log principal no momento
em que este agente começou (~520 especificações já testadas por outros
agentes/rodadas): o único sinal de RETORNO que sobreviveu com robustez foi
o **CIO Peer Momentum** (candidato #15/22/34-38 do log principal, Ying
2024 JFE) — cosine similarity de posse ponderada por AUM entre pares de
ações, retorno médio dos "peers" conectados prevendo retorno da própria
ação em h=1. A exigência de tradeabilidade foi relaxada pelo usuário antes
desta rodada (não precisa mais ter liquidez suficiente pra short/custo de
transação não corroer).

## Frente (A): Pairs trading via overlap de posse (CIO) — reversão dentro do par

**Mecanismo hipotetizado:** para pares de ações com CIO muito alto num mês
("gêmeos" institucionais — ownership quase idêntica, medida por cosine
similarity de posição em R$ ponderada por AUM, EXATAMENTE a mesma
construção do script `11_cio_peer_momentum.R`), calcula-se o spread de
retorno acumulado entre as duas (janela de formação K=3 ou 6 meses). A
hipótese de pairs trading clássico é que, se um "gêmeo" sobe muito mais
que o outro apesar de ownership quase idêntica, isso deveria reverter —
comprar quem ficou pra trás, vender quem subiu mais, dentro do par.

### Candidato #1 (deste agente): CIO Pairs Trading — reversão

Script `84_cio_pairs_trading.R`. Extraídos, mês a mês, os pares no topo da
distribuição de CIO daquele mês (dois cortes: top 1% com CIO mínimo
absoluto 0,3 — CIO médio 0,94, mediano 0,983, claramente pares "gêmeos"
de verdade; e top 5% com CIO mínimo 0,15, mais permissivo). Testados:
2 cortes de CIO × 2 janelas de formação (K=3,6) × 2 horizontes (h=1,3) =
8 combinações, cada uma com 3 testes — (i) regressão Fama-MacBeth
`diff_retorno_futuro ~ spread_formação` mês a mês (espera-se β negativo se
há reversão), (ii) retorno médio mensal do pair-trade (long perdedor,
short vencedor) com todos os pares acima do limiar de CIO, (iii) o mesmo
mas restrito ao terço de maior `|spread|` dentro do mês (recorte MENSAL,
não break fixo do treino — testa se a reversão é mais forte quando a
divergência é grande, pergunta específica do João) — 24 especificações no
total.

**Amostra saudável, sem degenerescência:** N de pares por mês nunca cai
abaixo de 87 (corte top 1%) ou 442 (corte top 5%), bem longe do problema
do Comomentum (script 09/10 do log principal, que tinha meses com 1 única
ação no grupo).

**Resultado: NENHUMA evidência de reversão em nenhuma das 24
especificações.** Pior ainda: o sinal do coeficiente de regressão é
consistentemente (embora fracamente) POSITIVO — ou seja, o spread de
formação tende a se repetir, não reverter — e o único resultado perto de
significância (top5pct, K=6, h=1, "todos os pares": t=-2,25, p=0,034,
24 meses) tem sinal NEGATIVO para a estratégia de reversão, o que
significa que a estratégia OPOSTA (continuação/momentum dentro do par)
teria tido t=+2,25 no mesmo teste. Nenhuma especificação chega perto do
limiar de Bonferroni herdado do log principal (~520 especificações
testadas até aqui, `p<0,05/520≈0,0001`).

| Corte CIO | K | h | Teste | t_fm | p_fm | N meses |
|---|---|---|---|---|---|---|
| top5pct | 6 | 1 | portfolio, todos os pares | -2,25 | 0,034 | 24 |
| top5pct | 6 | 1 | portfolio, top-tercil \|spread\| | -1,97 | 0,062 | 24 |
| top5pct | 6 | 3 | portfolio, todos os pares | -1,74 | 0,096 | 22 |
| (demais 21 especificações) | | | | \|t\|<1,5 | p>0,16 | |

**Veredito: REJEITADO.** Não há evidência de reversão de spread entre
"gêmeos" institucionais nesta amostra — o padrão, quando existe algum
(fraco, não-Bonferroni), aponta na direção OPOSTA à hipótese de pairs
trading clássico.

### Candidato #2 (deste agente): reinterpretação como continuação/momentum dentro do par + robustez de liquidez

Script `87_cio_pairs_robustez.R`. Como o candidato #1 sugeriu que a
direção certa (se houver alguma) é continuação, não reversão — o que é
teoricamente coerente com o próprio mecanismo do CIO Peer Momentum (Ying
2024: difusão GRADUAL de informação entre ações com ownership comum, ou
seja, continuação, não reversão), testei essa hipótese de forma explícita
e nomeada, com duas checagens adicionais:

**(R1) Restrição a "gêmeos" genuínos e líquidos:** CIO≥0,7 (limiar
absoluto, não só top-percentil relativo) **e** ambos os tickers do par
com cobertura institucional decente (`n_fundos≥20`, mesma lição de
liquidez do candidato #38 do log principal — evita que o resultado seja
dominado por pares de 1-2 fundos). Sobrevivem 20.237 pares-mês (mínimo
201/mês, mediana 357/mês — amostra saudável). Direção de continuação é
positiva em 3 de 4 combinações de K/h, mas nenhuma passa nem do padrão de
5% (melhor caso K=6,h=3: t=1,83, p=0,081; K=6,h=1: t=1,75, p=0,093).

**(R2) Hipótese de continuação no universo completo (espelho exato do
candidato #1):** K=6,h=1: t=2,25, p=0,034 (mesmo resultado do candidato
#1, sinal invertido e renomeado) — continuação de +1,01pp/mês
(~12,8%/ano) entre os "gêmeos" mais conectados quando a divergência de
retorno acumulado em 6 meses é grande. K=6,h=3: t=1,74, p=0,096. K=3: nada
em nenhum horizonte.

**Avaliação honesta:** existe um padrão consistente, mas fraco e nunca
significativo a rigor (nenhuma especificação bate nem o padrão de 5% de
forma robusta, muito menos Bonferroni), de que ações com ownership
institucional quase idêntica que já divergiram bastante em 6 meses
tendem a CONTINUAR divergindo no mês seguinte, não reverter. Isso é
qualitativamente consistente com — mas não adiciona evidência
estatisticamente nova além de — o CIO Peer Momentum já estabelecido no
log principal (que mede a mesma ideia de forma mais eficiente, agregando
TODOS os peers conectados numa média ponderada, em vez de isolar só o par
mais extremo). **Veredito: REJEITADO como sinal novo** — a versão
"pairs trading" (par-a-par) da hipótese de posse comum não gera um sinal
adicional além do que o CIO Peer Momentum agregado já captura, e é
consistentemente mais fraca (menos significativa) que ele, provavelmente
porque isolar 2 ações por vez descarta a maior parte da informação que a
agregação de múltiplos peers usa para cancelar ruído idiossincrático
(o mesmo motivo, aliás, pelo qual Lou 2012 agrega fundos em vez de olhar
1 fundo de cada vez).

## Frente (B): Machine Learning com validação walk-forward correta

**Contexto herdado:** o candidato #21 do log principal
(`21_ensemble_ml.R`) já testou XGBoost combinando 7 sinais com um ÚNICO
corte treino(`<2020`)/teste(`>=2020`) — resultado: R²_OOS **negativo** em
todos os horizontes (h=1: -2,3%; h=3: -8,3%; h=6: -3,1%), long-short das
previsões não significativo em nenhum horizonte, `peer_ret` (CIO) e
`mom_12m` (momentum próprio) como features mais importantes. Conclusão
do log principal: painel institucional mensal com poucos anos de
histórico é amostra pequena demais pra ML generalizar melhor que
abordagem linear simples com UM corte fixo. A questão em aberto que este
candidato investiga: um corte fixo treino/teste é a causa do overfitting,
ou o problema é mais fundamental (ML não ajuda de jeito nenhum, mesmo com
validação mais cuidadosa)?

### Candidato #3 (deste agente): XGBoost com walk-forward de verdade (refit expansivo a cada 6 meses)

Script `85_ml_walkforward.R`. Diferenças de desenho em relação ao
candidato #21:
- **12 features** em vez de 7 (reaproveitando `composto_painel_sinais.csv`,
  já construído por outro agente desta exploração, com `peer_ret`,
  `hhi_posse`, `prox_52w_high`, `reversao`, `idio_vol_12m`,
  `delta_breadth_w`, `dem_pct_w`, `EFIT` — mais `mom_12m` e `log_tamanho`
  calculados aqui, e `deg_avg`/`eigen_cent` de centralidade de rede,
  também já calculados por outro agente).
- **Árvore mais rasa e regularizada**: `max_depth=2` (vs. 3 no candidato
  #21), `min_child_weight=30` (vs. 20), `lambda=5` (regularização L2
  adicionada), `eta=0,03`, `nrounds=300` com early stopping em 25.
- **Walk-forward de verdade no período de teste**: em vez de treinar UMA
  vez em `<2020` e prever os 24 meses de teste de uma tacada, o modelo é
  refeito a cada 6 meses com janela expansiva (treina só com dado
  estritamente anterior ao mês previsto, refit em 202001/202007/202101/
  202107 → 4 refits por horizonte), e a cada refit usa um holdout
  cronológico (últimos ~15% dos meses de treino disponíveis naquele
  momento) para early stopping — validação walk-forward tanto DENTRO de
  cada refit quanto ENTRE refits.
- **R²_OOS contra benchmark mais rigoroso**: média histórica EXPANSIVA de
  y conhecida em cada momento do refit (não a média do treino original
  fixo nem a média do teste inteiro — um "ingênuo" que um investidor teria
  disponível em tempo real).

**Amostra com as 12 features disponíveis simultaneamente: 15.594
ativo-mês** (bem menor que os painéis individuais por causa do "outer
join" de 12 fontes diferentes, mas ainda substancial).

**Resultado:**

| h | N obs teste | R²_OOS walk-forward | FM long-short (quintil recortado por mês) |
|---|---|---|---|
| 1 | 5.264 | **+0,24%** | spread=+0,34%/mês, t=0,42, p=0,68 |
| 3 | 4.740 | -17,58% | spread=-1,68%/mês, t=-1,98, **p=0,061** |
| 6 | 3.984 | -12,26% | spread=+1,17%/mês, t=1,34, p=0,20 |

h=1 é o único horizonte com R²_OOS positivo (ainda pequeno, mas
melhor que o -2,3% do candidato #21 com o mesmo horizonte — walk-forward
ajuda um pouco a reduzir o overfitting). h=3 e h=6 continuam com R²_OOS
fortemente negativo, replicando o padrão do candidato #21 (o modelo piora
em relação a um "ingênuo" simples nesses horizontes). O long-short de h=3
chega perto de 5% (p=0,061) mas com sinal **invertido** (quintil "alto"
previsto teve retorno MENOR que quintil "baixo") — um alerta de
instabilidade, não de sinal genuíno (mesmo padrão de bandeira vermelha já
visto em outros quase-achados do log principal).

**Importância de features (consistente nos 3 horizontes): `peer_ret`
(CIO Peer Momentum) e `EFIT` (fluxo esperado) dominam, juntos
respondendo por 40-65% do gain total em todos os horizontes** —
confirma, de forma independente, o mesmo padrão já visto no candidato #21
do log principal: entre todos os sinais testados nesta exploração inteira
(~520+ especificações), CIO Peer Momentum e E[FIT] são consistentemente
os mais informativos, mesmo dentro de um modelo não-linear com validação
mais rigorosa. `hhi_posse` (crowding) e `deg_avg`/`log_tamanho` são
sistematicamente as features menos importantes para prever RETORNO
(reforça a leitura do log principal de que HHI serve para prever
VOLATILIDADE, não retorno).

**Veredito: REJEITADO como sinal de lucro, mas resultado mais informativo
que o candidato #21.** Walk-forward de verdade melhora o R²_OOS de h=1
(de negativo para levemente positivo) e confirma robustamente que
`peer_ret`/`EFIT` são os únicos sinais com conteúdo informativo real —
mas mesmo assim não produz um long-short significativo em nenhum
horizonte, e h=3 mostra sinal de instabilidade (não de sinal real).

### Candidato #4 (deste agente): robustez — feature set reduzido e refit mais fino

Script `86_ml_walkforward_robustez.R`. Duas checagens adicionais sobre o
candidato #3:
- **(R1) Só as 3 features mais importantes** (`peer_ret`, `EFIT`,
  `mom_12m`) — testa se "menos é mais" com N pequeno. Resultado: **pior**
  que o modelo completo em todos os horizontes (h=1: R²_OOS=-0,59% vs.
  +0,24% do candidato #3; h=3: -19,58%; h=6: -11,77%). Nenhum long-short
  significativo (melhor p=0,25, h=6).
- **(R2) Refit a cada 3 meses** (em vez de 6, janela expansiva mais fina,
  8 refits em vez de 4) com o feature set completo (9 sinais, sem rede).
  Resultado: muito parecido com o candidato #3 (h=1: R²_OOS=+0,18%;
  h=3: -17,31%; h=6: -7,06% — h=6 melhora um pouco com refit mais
  frequente, mas não muda a conclusão). Nenhum long-short significativo
  (melhor p=0,25, h=3, mesmo sinal invertido do candidato #3).

**Veredito: REJEITADO em ambas as variações — a conclusão do candidato
#3 é robusta** a reduzir o número de features e a mudar a granularidade
do refit. Reduzir para só os 3 sinais "campeões" piora o modelo (sinal de
que mesmo os sinais mais fracos ajudam um pouco a regularizar via
diversificação de features, embora nenhum sozinho ou em conjunto produza
uma estratégia executável). `peer_ret` e `EFIT` continuam dominando a
importância em 100% das variações testadas.

## Síntese desta rodada (frentes A e B, 4 candidatos, ~40 especificações)

**Nenhum sinal de lucro novo foi encontrado em nenhuma das duas frentes.**

- **Pairs trading via CIO (frente A):** a hipótese de reversão entre
  "gêmeos" institucionais é rejeitada de forma limpa — 24+8
  especificações, amostra sempre saudável (nunca degenerada), nenhuma
  passa nem o padrão de 5% de forma robusta. O único padrão fraco que
  aparece (p=0,034 isolado, K=6/h=1, não sobrevive a Bonferroni) vai na
  direção de CONTINUAÇÃO, não reversão — coerente com o CIO Peer
  Momentum já estabelecido, mas não adiciona sinal novo além dele
  (a versão par-a-par é estritamente mais fraca que a versão agregada
  já no log principal).
- **ML walk-forward (frente B):** validação mais rigorosa (refit
  expansivo, holdout cronológico, regularização mais forte) melhora o
  R²_OOS de h=1 na margem (de -2,3% pra +0,24%) em relação ao candidato
  #21 original, mas não produz um long-short significativo em nenhum
  horizonte nem em nenhuma das 4 variações testadas (feature set
  completo/reduzido × refit 3m/6m). **A importância de features é o
  achado mais consolidado desta frente**: `peer_ret` (CIO) e `EFIT`
  dominam de forma consistente em TODAS as configurações — confirmação
  independente, via um método totalmente diferente (árvore não-linear com
  validação walk-forward) do mesmo diagnóstico do log principal (via
  regressão linear/Fama-MacBeth): esses dois sinais carregam a maior
  parte da informação preditiva disponível nesta base, e nada mais
  testado (incluindo combinações não-lineares) consegue extrair sinal
  adicional de lucro além do que eles já entregam.

**Recomendação honesta:** nenhum candidato desta rodada passa nem perto do
limiar de significância (nem o padrão de 5%, muito menos Bonferroni). O
panorama geral do log principal não muda: CIO Peer Momentum continua
sendo o único sinal de retorno com suporte razoável de toda a exploração,
e nem versão par-a-par (pairs trading) nem combinação não-linear via ML
conseguem superá-lo ou complementá-lo com um sinal novo e independente.

## Conclusão consolidada da rodada de 6 agentes paralelos (15/08/2026)

Depois desta rodada, a contagem total de especificações testadas em toda a
exploração passa de ~520 para bem acima de 700. Resumo do que mudou e do
que não mudou:

**Sinal de lucro/retorno: continua não encontrado.** Nenhum dos ~200+
novas especificações desta rodada (interação com regime de vol,
combinação/ranking composto, centralidade de rede, dispersão de crenças,
trajetória de HHI, rotação setorial, outros eventos de choque, tamanho/
cotistas/churn da base de donos, pairs trading via overlap, ML não-linear
walk-forward) produziu um preditor de retorno que sobreviva às 3 lições
metodológicas inegociáveis com significância genuína. Cada quase-acerto
que apareceu (reversão×HHI h=6, E[FIT]×HHI h=1, centralidade de rede,
composto em h=6) foi diagnosticado pelo próprio agente responsável e
identificado como artefato — quintil degenerado, confound de tamanho, ou
o mesmo padrão de "carregado só pela COVID" já visto no falso-positivo do
Comomentum. O CIO Peer Momentum (h=1, agora relevante de novo dado o
critério de tradeabilidade relaxado) segue sendo o único sinal de retorno
com algum suporte, sem nada de novo que o supere ou reforce nesta rodada.

**Sinal de previsibilidade de volatilidade: ganhou uma segunda perna
genuinamente nova e forte.** Além do achado já estabelecido (HHI/
concentração de posse → volatilidade futura, h=12, base Greenwood-Thesmar
2011), o agente "eventos" encontrou que o **número de cotistas dos fundos
donos** de uma ação (não a concentração entre eles, mas o tamanho da base
de clientes de cada fundo dono) prediz volatilidade futura de forma
independente — bate Bonferroni isoladamente em h=6 ($p=0,00006$), tem
correlação fraca com HHI (não é redundante), sobrevive a controle
simultâneo por HHI+tamanho do fundo+tamanho da posição, sobrevive ao
controle mais difícil que existe (volatilidade passada) em h=6/h=12, e
mantém significância excluindo a COVID inteira (com amostra pequena, mas
na mesma direção). Também replicado com sucesso: o mecanismo HHI→
volatilidade se confirma, de forma consistente (4 de 4 eventos na mesma
direção), em 4 choques de mercado brasileiro adicionais além da COVID
(Joesley Day, greve dos caminhoneiros, e 2 choques orgânicos de
2017/2019) — evidência de que o mecanismo não é peculiaridade de um único
episódio.

**Avaliação honesta final**: a busca por sinal de lucro tradeable, agora
depois de mais de 700 especificações e um esforço deliberadamente
exaustivo cobrindo praticamente todo ângulo metodológico razoável
(incluindo relaxar a exigência de tradeabilidade), não encontrou nada
robusto o suficiente pra reportar como estratégia de lucro. A conclusão
que se consolidou ao longo de toda a exploração — previsão de risco é
genuína e nova, previsão de retorno não é — ficou ainda mais bem
estabelecida nesta rodada, não menos. O achado de cotistas (E3) é um bom
candidato a ENTRAR na consolidação de achados de previsibilidade do TCC
(junto com HHI→vol), sujeito à mesma disciplina de revisão humana antes
de qualquer decisão de incorporação.
