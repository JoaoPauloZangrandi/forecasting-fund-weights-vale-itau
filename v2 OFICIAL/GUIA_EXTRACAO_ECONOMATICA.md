# Guia de extração manual no Economatica (SH e cons, 2022 em diante)

**Aviso honesto, leia primeiro.** Eu não conheço a interface do Economatica e não vou
inventar nomes de menu, botão ou caixa de seleção. Este guia não diz onde clicar. Ele diz,
com precisão, **o que a extração precisa produzir**, para que você consiga casar o que
aparece na tela com o alvo, e para que você descubra em minutos, e não em horas, se acertou.

A ferramenta tem sessão limitada e o dado é grande. Por isso a ordem aqui é: extrair **2021
primeiro**, que você já tem, conferir contra o arquivo do professor, e só depois gastar tempo
com 2022 a 2026.

---

## Os três erros que arruínam a extração

### 1. Não filtre por "fundos de ações". Este é o erro mais provável.

Você me disse que "tem que ser por fundo de ações". **Não é**, e essa premissa quebraria a
comparabilidade com toda a série de 2016 a 2021.

Contei os fundos únicos por classificação ANBIMA dentro do `cons_2021.csv` do professor:

| Classificação ANBIMA | Fundos únicos |
|---|---|
| Multimercados Livre | 1.144 |
| Ações Livre | 873 |
| Ações Índice Ativo | 173 |
| Ações Valor/Crescimento | 110 |
| Ações Indexados | 71 |
| Ações Dividendos | 47 |
| Ações Sustentabilidade/Governança | 39 |
| Ações Setoriais | 39 |
| Ações Small Caps | 38 |
| Fundos de Índices - ETF | 6 |
| Não classificado | 2 |
| **Total** | **2.543** |

**45% da base não é fundo de ações.** São multimercados, ETFs e dois não classificados. Se
você marcar "somente fundos de ações", perde 1.152 fundos de 2.543 e produz uma base que não
conversa com a que já está no TCC.

No `SH_2021.csv` o desequilíbrio é ainda maior: dos 3.433 fundos, **1.991 não são "Ações",
ou seja 58%**, sendo 1.945 Multimercados Livre. Um filtro de fundo de ações no SH destruiria
mais da metade da base.

A confusão provavelmente vem de dois filtros diferentes que se parecem:

- **Filtro de ATIVO**: sim, é ações. No `cons_2021.csv` a coluna `Tipo_Ativo` tem um único
  valor em todas as linhas, `Ações`. Aqui você filtra.
- **Filtro de FUNDO**: não. É todo fundo que carregue ação, seja ele de ações ou
  multimercado. Aqui você **não** filtra.

O critério correto é: todo fundo que tenha posição em ação entra, independentemente da
classificação dele.

### 2. A consolidação (look-through) tem que estar ligada

É a caixinha que você mencionou, e é a mais importante de todas. Sem ela, um fundo de cotas
aparece investindo em outro fundo, e não na ação. Medimos isso em agosto: **sem look-through,
só cerca de 19% dos fundos batem**, porque a maioria da base é fundo-sobre-fundo.

Como conferir na hora, sem rodar nada: se a extração vier consolidada, a coluna de tipo de
ativo mostra ações com nome de papel, tipo `GERDAU PN N1 - GGBR4`. Se vier cota de fundo ou
nome de fundo no lugar do papel, a caixa está desligada.

### 3. As duas bases têm periodicidade diferente

- **SH é diária.** O `SH_2021.csv` tem 252 datas distintas, que são os pregões do ano.
- **cons é mensal.** O `cons_2021.csv` tem 12 datas, sempre o último pregão do mês:
  `2021-01-29`, `2021-02-26`, `2021-03-31`, e assim por diante até `2021-12-31`.

Se você extrair o cons em frequência diária, o arquivo fica cerca de vinte vezes maior sem
acrescentar informação. Se extrair o SH mensal, quebra o cálculo do beta, que precisa de 252
pregões de cota.

---

## Alvo 1: `SH_<ANO>.csv`

Dez colunas, nesta ordem exata, separadas por ponto e vírgula:

```
COD_FUNDO;CNPJ;NOME_FUNDO;GESTORA;CLASSIFICACAO_ANBIMA;DATA;APLICAÇÃO;COTA;NUMERO_DE_COTISTAS;PATRIMONIO_LIQUIDO_(MIL)
```

Linha real do arquivo de 2021, para você comparar:

```
191;77.054.658/0001-00;BRADESCO H ACUMULAÇÃO FIC AÇÕES;Bradesco Asset Management;Ações Índice Ativo;04/01/2021;R$ 0,00;3,192127;690;R$ 20.729,09
```

| Coluna | Formato | Observação |
|---|---|---|
| `COD_FUNDO` | inteiro | código interno do Economatica, não da CVM. É a chave de junção com `Código` do cons |
| `CNPJ` | `00.000.000/0001-00` | com pontuação |
| `NOME_FUNDO` | texto | caixa alta |
| `GESTORA` | texto | **marca controladora já normalizada**, ex. `Bradesco Asset Management`. É o que agrupa Itaú Asset, Itaú DTVM e Itaú Unibanco |
| `CLASSIFICACAO_ANBIMA` | texto | ex. `Ações Índice Ativo` |
| `DATA` | `dd/mm/aaaa` | diária, só pregões |
| `APLICAÇÃO` | `R$ 0,00` | com prefixo, ponto de milhar, vírgula decimal |
| `COTA` | `3,192127` | **vírgula decimal**, 6 casas |
| `NUMERO_DE_COTISTAS` | inteiro | |
| `PATRIMONIO_LIQUIDO_(MIL)` | `R$ 20.729,09` | **em milhares de reais**, com prefixo e vírgula decimal |

Referência de tamanho: 2021 tem 3.433 fundos únicos e 252 datas.

## Alvo 2: `cons_<ANO>.csv`

Nove colunas, nesta ordem exata:

```
CNPJ;Anbima;Código;Nome;Tipo_Ativo;Data_Competência;Nome_Ativo;Valor_Ativo_mil;Participação_Ativo
```

Linha real do arquivo de 2021:

```
00.829.163/0001-81;Multimercados Livre;19062;BRADESCO MULTIPERFORMANCE FIC MULTIMERCADO;Ações;2021-09-30;GERDAU PN N1 - GGBR4;5.361403671356667;6.369346644105552E-4
```

| Coluna | Formato | Observação |
|---|---|---|
| `CNPJ` | com pontuação | |
| `Anbima` | texto | mesma classificação do SH |
| `Código` | inteiro | casa com `COD_FUNDO` do SH |
| `Nome` | texto | |
| `Tipo_Ativo` | `Ações` | valor único em toda a base |
| `Data_Competência` | `aaaa-mm-dd` | **ISO, diferente do SH**, mensal, último pregão do mês |
| `Nome_Ativo` | `GERDAU PN N1 - GGBR4` | nome longo, hífen, e o ticker no fim. O pipeline extrai o ticker desse sufixo |
| `Valor_Ativo_mil` | `5.361403671356667` | **ponto decimal**, em milhares de reais, sem prefixo |
| `Participação_Ativo` | `6.369346644105552E-4` | **ponto decimal, notação científica**, fração do patrimônio, não porcentagem |

Referência de tamanho: 2021 tem 2.543 fundos únicos, 12 datas e 5.919.192 linhas.

**Composição esperada do `Nome_Ativo`**, medida por linha no arquivo de 2021. Serve como
gabarito rápido: se a sua extração fugir muito disso, alguma configuração está diferente.

| Categoria | % das linhas |
|---|---|
| Ação listada, termina em ticker B3 | 98,84% |
| Outros (inclui ação cedida em empréstimo) | 0,86% |
| `Direito de Subscrição - XXXX` | 0,21% |
| `Ações (omitidas)`, posição confidencial | 0,06% |
| `Ação de Companhia Fechada - ...` | 0,03% |

Repare que companhia fechada é irrisória em linhas, 0,03%, mas responde por 201 dos 1.434
nomes distintos. É cauda longa, e o pipeline do TCC já a descarta ao exigir o sufixo numérico
do ticker.

**Sobre a escala da participação:** mediana de 0,000048 e quantil 99,9% de 0,117, ou seja,
fração e não porcentagem. Existem 16 linhas em 5,9 milhões com participação acima de 1, que
são registro corrompido na própria fonte, como o fundo 616291 em outubro de 2021 com 4.781.
Isso já existe na base do professor e não é problema seu; o pipeline tem um corte de limpeza
para isso.

**Atenção à inconsistência entre os dois arquivos**, que não é erro seu: o SH usa vírgula
decimal e prefixo `R$`, o cons usa ponto decimal e notação científica. As datas também
divergem, `dd/mm/aaaa` contra ISO. Se a exportação deixar você escolher, replique cada um do
jeito que está. Se não deixar, tudo bem, é conversão trivial depois, mas **anote o que saiu
diferente** para eu ajustar o pipeline.

---

## Detalhes de arquivo

- Separador: **ponto e vírgula**.
- Codificação: **UTF-8 com BOM**. Acentos precisam sair corretos em `AÇÕES`, `Participação`,
  `Data_Competência`. Se vier `AÃ‡Ã•ES`, a codificação está errada.
- Uma linha de cabeçalho, com os nomes exatos acima.
- Sem separador de milhar nos campos numéricos do cons.
- Um arquivo por ano, nomeado `SH_2022.csv`, `cons_2022.csv`, e assim por diante.

Se a ferramenta impuser limite de linhas por exportação, quebre **por mês** e junte depois,
nunca por subconjunto de fundos, que é onde se perde fundo sem perceber. O cons de 2021 tem
cerca de 4,3 milhões de linhas; um ano de SH tem em torno de 700 mil.

---

## O teste que decide tudo: extraia 2021 primeiro

Não comece por 2022. Extraia **2021**, que você já tem do professor, com exatamente as
mesmas configurações que pretende usar nos outros anos. Depois rode:

```
Rscript "v2 OFICIAL/scripts/109_verifica_extracao_economatica.R" SH   caminho/do/seu/SH_2021.csv
Rscript "v2 OFICIAL/scripts/109_verifica_extracao_economatica.R" cons caminho/do/seu/cons_2021.csv
```

O script compara a sua extração contra o arquivo do professor e diz, em uma tela, se bate:
cabeçalho, número de fundos, datas, distribuição por classificação ANBIMA, e o grau de
acordo dos valores.

**A leitura do resultado:**

- Se bater quase perfeito, suas configurações estão certas. Extraia 2022 a 2026 com as mesmas
  e você terá uma base melhor do que qualquer reconstrução da CVM, sem o teto de 9% a 18% de
  erro que nos travou em agosto.
- Se não bater, o script diz onde divergiu, e aí a gente ajusta a configuração antes de você
  gastar cinco anos de extração no caminho errado.

Esse teste custa uma extração e vale as cinco seguintes.

---

## Se a extração der certo

Aí sim vale reabrir a questão de estender o TCC para 2016-2026, que foi arquivada em 30/08
justamente porque a base reconstruída não era confiável o bastante. Com dado real do
Economatica, o argumento muda por completo.

O que precisaria ser refeito está mapeado em `PLANO_EXPANSAO_2021_2026.md`, e o pipeline já
tem o `config_periodo.R`, em que basta trocar `ANO_FIM` para 2026 num lugar só.
