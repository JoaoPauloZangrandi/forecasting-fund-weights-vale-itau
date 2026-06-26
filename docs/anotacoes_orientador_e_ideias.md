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
- **Winsorizar fluxo/AUM** (tem caudas extremas de fundos com AUM minúsculo).
- **Unificar** a análise no universo de Ações como principal (deixando "todos os
  holders" como robustez).
- **Generalizar** para outras ações/gestoras.
- **Redação do TCC** com base nos dois PDFs.
