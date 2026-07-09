# Anotações do orientador (Maurício) e ideias do orientando

Registro fiel de **tudo** que guiou o projeto: as anotações cruas da reunião com o
Prof. Maurício Ferraresi Jr. e a evolução das ideias/decisões do orientando (João)
ao longo do trabalho. Serve para não perder a intenção original nem o raciocínio.

---

## 1. Anotações da reunião com o Maurício (cruas)

> Transcrição organizada das anotações que o orientando fez durante a conversa com
> o orientador. Mantidas próximas do original.

- Pegar **VALE**; ~1000 fundos têm VALE no mercado; ideia de **filtro de Kalman**.
- **Regressão em dois passos.** Uma **cross-section** porque já temos muita
  informação, e você consegue aprender um parâmetro que é uma **variável latente**
  que explica como as posições são explicadas pelas características dos fundos. O
  **beta da OLS vira um fator latente** que reduz um problema de alta dimensão para
  uma dimensão menor. E agora ele é **dinâmico no tempo** — você pode propor um
  formalismo ao longo do tempo. O seu **teta (θ) de janeiro é o de fevereiro mais um
  choque, igual a um random walk**; quem vai ajudar a prever é o θ. É um **modelo de
  fator dinâmico**.
- **Peso primeiro**, por ser mais simples e relativamente *bounded* (limitado em [0,1]).
- Pegar a **média global**.
- A regressão em cross-section **pode ser feita por ML**: tentar entender quais são as
  características que dá para aprender para **prever amanhã**.
- Pegar **só a VALE**, **todos os fundos do Itaú**, e escolher **~5 características de
  fundo** e **1 ou 2 da ação**. O conjunto de características tem que ser capaz (de
  explicar).
- **Características do fundo:** AUM dos fundos; **aplicação − resgate** (fluxo líquido);
  se o fundo é **FIC ou FI**; **quantidade de cotistas**. **Retirar fundos exclusivos.**
- **Características da ação:** **preço** da VALE; **beta** da VALE (pegar do começo do
  mês — testar começo do mês ou último fechamento do mês passado, e talvez outras datas).
- Com isso teremos o **beta estimado** e daí o **erro**: ter tudo isso de erros e ver se
  é **estacionário**, se tem **variância perto de zero**, etc.
- **A graça do modelo:** ter **posições consolidadas** + **modelo preditivo** para o
  peso; e **entender quais características os fundos olham**.

### Plano passo a passo (como o orientando resumiu)
Pegar VALE e a gestora Itaú e todos os seus fundos; pegar todos os pesos que VALE
representa em cada fundo. Do lado direito da equação: características do fundo (AUM;
aplicação − resgate; FIC ou FI; nº de cotistas) e da ação (preço e beta — beta no último
dia do mês anterior). Fazer um **beta OLS** (uma cross-section). Usar essas
características e o beta para prever o do próximo mês, dois meses, três meses… (ou usar
dois meses para prever os próximos). Comparar estimado vs real → **erro**. Resultado:
uma **matriz de erros** (nº de fundos do Itaú × nº de características); no cenário ótimo,
a média do erro seria 0. **Usar apenas as bases CONS e SH.**

---

## 2. Ideias, decisões e pensamentos do orientando (ao longo do projeto)

> Ordem cronológica aproximada. Atribuídas ao João (orientando), que conduziu as
> escolhas. As justificativas/efeitos estão no `log_decisoes.md`.

**Modo de trabalho (a regra de ouro).** "Ir devagar, passo por passo, questionando
tudo." Recomeçou o projeto do zero justamente por sentir que se ia rápido demais
("passei a carroça na frente dos bois"). Quer **entender cada coisa antes de fazer**;
nada de empilhar passos. Registrar tudo para sobreviver a troca de sessão/ferramenta.

**Escopo inicial.**
- Alvo = **peso** de VALE3; **posição direta** apenas (`VALE ON N1 - VALE3`).
- Gestoras: **Itaú Asset Management, Itaú DTVM, Itaú Unibanco**.
- Não remover fundos exclusivos no início (manter simples); revisitar depois.
- Começar **só por 2016** (rápido, menos custo) e depois generalizar.
- Entregar no GitHub também um **`R_full.R`** (todo o código num arquivo), além dos
  scripts segmentados.

**Tratamento de dados (o João sempre cobrou rigor).**
- SH é diária e CONS é mensal → casar pelo **dia exato** (a SH no mesmo dia da
  competência da CONS).
- Remover **pesos negativos** (resíduos) e **registrar todo o processo** de decisão.
- Parsing é **extremamente crítico** — conferir sempre, validar contra outra fonte.

**Fluxo líquido.** Ao ver que a SH só tem captação (não resgate), perguntou se havia
fonte melhor. Decisão: usar o **Informe Diário da CVM** (captação E resgate),
**validando** contra a SH. (Fonte externa, mas é a base-mãe e mais confiável.)

**Preço/beta.** Autorizou puxar de **fonte externa extremamente confiável** (ficou
Yahoo, validado contra a B3). Gostou de medir preço/beta no **último pregão do mês
anterior**.

**Timing de preço/beta — refinamento (26/06).** O João questionou: faz sentido fixar
preço/beta no último dia útil de `t-1`? O gestor decide **ao longo do mês**, olhando o
preço e o beta evoluírem — não num único dia. Discussão concluiu que o uso muda a
resposta: para **explicar** (cross-section/pooled) o melhor é o **contemporâneo (média
do mês `t`)**; para **prever** é obrigatório o **predeterminado (`t-1`)**, senão há
look-ahead. É exatamente o que o Maurício antecipou ("testar começo do mês ou último
fechamento do mês passado, e talvez outras datas"). Decisão: usar **média do mês `t`**
na regressão de explicação (`preco_mes`/`beta_mes`), mantendo `t-1` na previsão.
⚠️ Alerta registrado: o preço contemporâneo capta em parte o efeito **mecânico** (preço
sobe → fração na carteira sobe sozinha), não só decisão ativa — candidato a decompor
peso passivo vs ativo no futuro.

**Look-ahead.** Levantou a dúvida: definir o fim do download (`P2`) não seria
look-ahead? (Levou à análise que mostrou: beta e preço nominal são limpos; o preço
ajustado tem o nível retroajustado por proventos futuros → usar **preço nominal**.)

**Fundos exclusivos.** Pediu para **remover fundos com ≤ 3 cotistas** (decisão simples;
outras variantes depois).

**Validação.** Cobrou **0 erros** e re-testar todos os problemas ao estender para
2017–2021. (Levou à bateria de validação `R/15`: 0 erros; cross-check SH×Informe em
99% nos 6 anos.)

**Documentação.** Pediu **PDFs** (`results.pdf` e `explanation.pdf`) com tabelas,
**equações**, exemplo resolvido, interpretações e glossário — bem didáticos.

**Refino da previsão.** Concordou em testar **efeito-fixo de fundo + Kalman**
(resultado: o ingênuo/persistência ainda vence; o peso é quase um martingale).

**Opacidade de 90 dias (ideia do João).** Os fundos divulgam a carteira a cada ~90
dias → propôs testar prever **t+3** com informação de **t** (em vez de t+1). (Resultado:
o ingênuo piora mas ainda vence.)

**As duas margens (ideia do João).** Perguntou como tratamos fundos que **zeram** a
posição em VALE3 ou que **passam a ter**. Decisão: construir o **painel com zeros**
(margem **extensiva**: ter ou não ter), além da **intensiva** (quanto). Escolheu o
universo de **fundos de Ações**. (Resultado: as duas margens têm **sinais opostos**;
posse também é quase martingale.)

**Foco em Ações (ideia do João).** Apontou que a previsão deveria ser testada só com
fundos de Ações (universo mais limpo). Refeita a h=1 e h=3 — conclusão robusta.

**Explicitar dados/resultados.** Pediu estatísticas descritivas explícitas (Tabela 1)
e os números mais à mão nos dois PDFs.

---

## 3. Pendências / ideias guardadas para depois

- **`P2` dinâmico** no download do Yahoo (higiene; não muda resultado).
- **Margem extensiva mais a fundo:** o que prevê especificamente entradas/saídas; ou um
  modelo em duas partes (hurdle) formal.
- ~~**Winsorizar fluxo/AUM**~~ — feito em 09/07/2026 (R/36, ver seção 4).
- **Unificar** a análise no universo de Ações como principal (deixando "todos os
  holders" como robustez).
- ~~**Generalizar** para outras ações/gestoras~~ — feito em 09/07/2026 (R/31–32, ver
  seção 4), com escopo explícito documentado.
- **Redação do TCC** com base nos dois PDFs.
- **Elasticidade-preço à la Koijen-Yogo** (rodada 09/07): estimar o canal
  preço-demanda "de volta" — exige instrumento construído a partir das
  características dos demais investidores (a peça que este projeto explicitamente
  não estima, ver Introdução do tcc). Registrado como agenda futura, não
  implementado nesta rodada (decisão do João).
- **Cobertura de preço/beta para todo o universo multiativo** (hoje só VALE3 tem
  preço/beta próprios; o painel multiativo tem 1.777 ativos, inviável mapear todos
  para tickers do Yahoo nesta rodada — ver seção 4).

---

## 4. Rodada 09/07/2026 — segunda folha manuscrita do orientador

> Foto de um novo bloco de anotações do Maurício, fornecida pelo João em 09/07/2026.
> Transcrição/interpretação abaixo, confirmada com o João antes de implementar
> (`AskUserQuestion` sobre os 4 pontos mais ambíguos/caros).

**Itens implementados nesta rodada:**

- **Estimar λ além de MQO e VI(1 lag): Arellano-Bond.** `R/35` implementa painel
  dinâmico (GMM de Arellano-Bond, `plm::pgmm`) para a equação de ajuste parcial,
  como estimador adicional. Resultado (instrumentos completos, lag 2:99): ρ=0,584,
  λ(sobre w*)=+0,292 (t=6,8) — mas com 2.484 instrumentos para 307 fundos (T=72 é
  longo p/ o AB padrão), o Sargan fica não-informativo (p=1), sintoma de
  proliferação de instrumentos. **Achado importante (não estava previsto):**
  colapsando os instrumentos (`collapse=TRUE`, Roodman 2009 — a forma padrão de
  tratar proliferação, testada como robustez) o coeficiente **inverte de sinal**:
  λ=-0,331 (t=-6,2). Uma tentativa intermediária (truncar cru em lag 2:5, sem
  colapsar) deu matriz de covariância computacionalmente singular. Conclusão
  adotada no `tcc I.pdf`: o Arellano-Bond é instável (nem o sinal se sustenta) 
  neste desenho de painel (N=307 pequeno, T=72 grande — atípico para o
  estimador, pensado para N grande e T pequeno) e NÃO deve ser lido como uma
  confirmação adicional do fenômeno; a robustez do achado "λ>0" repousa nas
  outras seis especificações (MQO, VI em 3 esquemas de cluster, VI+efeito de
  tempo, FE+VI), que SÃO consistentes em sinal. Isso nuança (mas não invalida) o
  ponto de heterogeneidade persistente já levantado na revisão do Fable (I1).
- **Deixar claro por que Kalman para θ; documentar o pooled período a período;
  interpretar melhor as tabelas.** Tratado na redação do `tcc I.pdf` (não é
  código novo).
- **Padronizar tudo de forma consistente.** Já em andamento desde a rodada
  anterior (R/24/28); mantido e estendido ao painel multiativo (R/32: z-score
  calculado na distribuição de fundo-mês ÚNICOS, não repetida por ativo).
- **Beta do próprio fundo como controle (base SH).** `R/33`: retorno diário da
  COTA de cada fundo vs. Ibovespa, mesma metodologia de janela móvel 252 pregões
  do beta da VALE3. Média 0,50, min -0,27, max 1,27 (nos 307 fundos da amostra
  principal). 27% de fundo-mês sem beta ainda (falta 1 ano de histórico) —
  limitação documentada.
- **Manter gestora Itaú e ampliar para Ações.** `R/31`+`R/32`: painel
  fundo×ativo×mês generalizado (1.777 ativos distintos brutos, 305 fundos, 1,50
  milhão de linhas após filtro de cotistas). Escopo explícito decidido com o
  João: a estimação de demanda (θ por ativo-mês) foi generalizada de verdade; a
  parte dinâmica (Kalman, ajuste parcial, previsão OOS) continua com VALE3 como
  estudo de caso aprofundado, porque preço/beta de mercado para 1.777 tickers é
  inviável de mapear/buscar nesta rodada. Achado novo: os sinais de VALE3
  generalizam para 92-99% dos ~868 ativos com amostra suficiente, MAS o efeito
  de tamanho de VALE3 está no percentil 1 da distribuição entre ativos — VALE3
  é um caso extremo nessa dimensão, não típico.
- **NEFIN: pegar os prêmios (Carhart).** `R/34`: fatores diários do NEFIN-USP
  (Rm-Rf, SMB, HML, WML) agregados para mensal e adicionados como controle no
  pooled de VALE3. Achado: coeficientes de preço/beta praticamente não mudam ao
  controlar pelos fatores de mercado conhecidos (preço 0,00496→0,00526; beta
  -0,00649→-0,00609) — o efeito não é, na maior parte, exposição a risco de
  mercado conhecido.
- **Elasticidade-preço/covariância entre características:** ver seção 3 (agenda
  futura, não implementado — decisão explícita do João).

**Itens não decifrados com confiança na transcrição** (marcados na conversa, não
implementados por falta de leitura segura): a frase sobre "aplicação líquida...
mostrar base" e o comentário na introdução sobre "decisões de demanda tomadas sob
informações e incentivos parecidos". Se o Maurício retomar esses pontos, revisar.
