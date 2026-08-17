Sugestão de documento: Cronograma de trabalho

Resumo: Minha monografia versa sobre a demanda por peso de VALE3 e demais ações na carteira dos
fundos multimercado e de ações geridos pelo Itaú, modelada por meio de um fator dinâmico de
ajuste parcial (arcabouço de demand-based asset pricing), e encontrou os seguintes resultados
parciais: (i) no corte transversal (Etapa 1, modelo logístico com seis características de
fundo/ativo), a concentração do restante da carteira (HHI do restante, medida em t-1) é a
característica com efeito mais consistente sobre o peso alocado — positivo em 86,5% dos meses e
estatisticamente significativo em 72,1% deles, mesmo controlando por tamanho, gestora e demais
características; (ii) o modelo de ajuste parcial (Etapas 2 e 3), com velocidade de ajuste λ
estimada apenas na amostra de treino, supera de forma consistente um benchmark ingênuo de
previsão de peso fora da amostra em todos os horizontes testados, com a razão de RMSE caindo de
0,983 (h = 1 mês) para 0,928 (h = 12 meses) — ou seja, o ganho de acurácia preditiva cresce com o
horizonte; (iii) na aplicação prática (identificação de "fundos replicantes"), a correlação de
resíduos entre pares de fundos da mesma gestora que compartilham um ativo identifica 1.272.131
pares estatisticamente significativos (correção de Benjamini-Hochberg), evidenciando
comportamento correlacionado entre fundos de uma mesma gestora — mas o fluxo do fundo "líder" de
cada par não tem poder preditivo sobre o retorno subsequente do ativo (R² ≈ 0,03%–0,07%,
coeficiente de sinal inconsistente entre especificações), de modo que não há evidência de que
essa correlação seja explorável como estratégia de replicação/front-running. No entanto, os
seguintes itens serão trabalhados neste curso de Monografia II com o objetivo de melhoria
substancial da qualidade desta Monografia:

1) **Introdução.** O que será melhorado: redigir a seção de Introdução, hoje um placeholder.
Porque será melhorado: é a seção que apresenta ao leitor/banca a motivação, o problema de
pesquisa e a contribuição do trabalho, e só podia ser escrita de forma definitiva depois de os
resultados estarem consolidados — o que já ocorreu. Como será melhorado: será redigida a partir
dos resultados já obtidos nas Etapas 1–3 e na Aplicação, situando o trabalho na pergunta de como
gestoras ajustam o peso de ações em resposta a fluxo e a características de carteira, e
explicitando a contribuição do trabalho (dado brasileiro, granularidade fundo-ativo-mês, teste
direto de uma estratégia de replicação entre fundos).

2) **Revisão de literatura.** O que será melhorado: redigir a seção de Revisão de literatura,
hoje um placeholder. Porque será melhorado: é necessário fundamentar teoricamente o modelo de
ajuste parcial dentro da literatura de demand-based asset pricing e de fluxo-retorno de fundos, e
situar criticamente o achado negativo da Aplicação frente à literatura sobre comportamento
correlacionado ("smart money"/manada) entre gestoras. Como será melhorado: por meio de
levantamento bibliográfico direcionado, a ser conduzido após indicação de referências pelo
orientador, com leitura e fichamento crítico dos textos-base do arcabouço demand-based e de
estudos de fluxo-retorno aplicados ao mercado brasileiro de fundos.

3) **Tratamento do look-through de fundos-cotistas (FIC).** O que será melhorado: expandir o
tratamento da posição indireta em ações via fundos investidos — hoje cada fundo-cotista é tratado
como uma posição em "cota", sem abrir a carteira do fundo investido. Porque será melhorado: é uma
limitação já identificada na Conclusão do trabalho — fundos multimercado que alocam via outros
fundos (fundos de fundos) têm sua exposição efetiva a VALE3 e demais ações subestimada, o que pode
atenuar o efeito estimado da concentração de carteira (HHI do restante) nesses casos. Como será
melhorado: utilizando a base de composição de carteira (CDA) dos fundos-cotistas mais relevantes
por patrimônio para "abrir" um nível de look-through e recalcular o peso efetivo em VALE3/ações,
testando se o coeficiente de HHI do restante se mantém robusto após essa correção.
