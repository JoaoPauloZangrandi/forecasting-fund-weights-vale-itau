# Roteiro de apresentação — 5 minutos, 6 slides

Cinco minutos são cerca de 750 palavras faladas. Não dá para improvisar. Este roteiro traz
falas quase literais, porque a única forma de caber é saber de antemão o que vai dizer.

**Orçamento de tempo:**

| Slide | Assunto | Tempo | Acumulado |
|---|---|---|---|
| 1 | Capa | 0:10 | 0:10 |
| 2 | A pergunta e a base | 0:55 | 1:05 |
| 3 | O modelo | 0:55 | 2:00 |
| 4 | As duas respostas | 1:15 | 3:15 |
| 5 | O achado | 1:10 | 4:25 |
| 6 | Conclusão | 0:35 | 5:00 |

Regra de ouro: **se você se pegar explicando um número que já está no slide, você está
perdendo tempo.** O slide mostra o número, você diz o que ele significa.

Treine uma vez com cronômetro. Se passar de 5:30 na primeira tentativa, o corte é a última
frase do slide 2 e o exemplo do slide 4.

---

## Slide 1 — Capa (10 segundos)

Sem preâmbulo, sem agradecimento, sem "hoje eu vou falar sobre".

> "Peso de ações nos fundos de investimento brasileiros. O trabalho investiga o que
> determina esse peso e se ele é previsível."

Vira o slide.

---

## Slide 2 — A pergunta e a base (55 segundos)

Duas ideias, nesta ordem, sem enfeite.

**Primeiro, a ideia que justifica tudo:**

> "A carteira divulgada de um fundo é demanda revelada. E quando eu olho todos os fundos
> que carregam a mesma ação no mesmo mês, o preço é o mesmo para todos. Então o que sobra
> para explicar a diferença entre as carteiras são características dos próprios fundos.
> Isso me dá duas perguntas: o que explica o peso, e ele é previsível."

**Segundo, o diferencial da base. Diga isso com firmeza, é o seu ativo mais forte:**

> "A base é o universo inteiro da CVM com posição em ações, entre 2016 e 2021. Três mil
> duzentos e cinquenta e quatro fundos no bruto, dois mil quinhentos e sete na amostra
> final, oito milhões de observações. Não é um recorte de uma gestora ou de um papel, é
> tudo, e é dado inteiramente público."

Não explique o funil de 3.254 para 2.507. Se perguntarem, você responde. Não gaste os seus
55 segundos com isso.

**Transição:** "O modelo tem três etapas."

---

## Slide 3 — O modelo (55 segundos)

Uma frase por etapa. Não deduza nada, não justifique a logística a menos que perguntem.

> "Etapa 1 responde à primeira pergunta. Para cada ação e cada mês, uma regressão logística
> do peso em seis características do fundo. Uma regressão por célula, sem agregar nada por
> cima. Logística porque o peso vive entre zero e um."

> "Etapa 2 dá a dinâmica. A Etapa 1 me dá um alvo, mas o fundo não pula para ele. O lambda
> mede que fração da distância ele fecha por mês."

> "Etapa 3 é o teste honesto. Estimo o lambda só com dado anterior a janeiro de 2020,
> congelo, e aplico no período seguinte. O adversário é a regra ingênua de repetir o último
> peso, que num processo tão persistente é difícil de bater."

Essa última frase é importante: ela prepara o terreno para o 1,5% do próximo slide não
parecer decepcionante. Não corte.

---

## Slide 4 — As duas respostas (1 min 15)

Metade do slide para cada coluna. Aponte, não leia.

**Coluna da esquerda, 35 segundos:**

> "Duas características dominam. Beta da cota, com sinal positivo em 88% das treze mil e
> setecentas células, e concentração do resto da carteira, com 87,6%. A leitura é direta:
> fundo mais exposto ao mercado carrega mais de qualquer ação, e quem já concentrava no mês
> anterior concentra de novo. Concentrar é um traço de estilo da casa, não uma decisão
> tomada ativo por ativo."

Essa última frase é a semente do slide 5. Diga com intenção.

> "Tamanho, fluxo e fundo de cotas ficam perto do acaso."

**Coluna da direita, 40 segundos:**

> "O lambda é 0,069 ao mês. Traduzindo: o fundo fecha só 7% da distância até o alvo por
> mês, meia-vida de dez meses. É um ajuste real, mas lento, e é essa lentidão que faz o
> peso parecer um passeio aleatório no curto prazo."

> "Fora da amostra, o modelo vence a ingênua nos quatro horizontes. A margem parece pequena
> em nível, 1,5% em um mês, mas ela cresce de forma monotônica até 7,1% em doze meses, que é
> exatamente o padrão que a teoria de ajuste parcial prevê. E vence em 23 dos 23 meses de
> teste, não na média."

**Transição, e é a mais importante da apresentação:** "Só que esse agregado esconde uma
coisa muito mais interessante."

---

## Slide 5 — O achado (1 min 10)

Este é o slide que a banca vai lembrar. Não corra.

O gráfico à esquerda é a Figura 2 do trabalho: à esquerda dele, as três gestoras mais
difíceis de prever; à direita, as três mais previsíveis. **Aponte para ele no primeiro
degrau**, porque a diferença de amplitude entre os dois painéis é visível de longe e faz o
argumento sozinha.

Construa em três degraus, um por vez:

> "Entre as quarenta gestoras, o erro varia quase trinta vezes. No painel da esquerda estão
> as três mais difíceis de prever, no da direita as três mais previsíveis, e os dois estão na
> mesma escala. A diferença de amplitude é o resultado."

> "E isso não é ruído. Dividindo o período de teste ao meio, cada gestora se parece consigo
> mesma, com correlação de 0,85 na dispersão do erro. Quem é difícil em 2020 continua
> difícil em 2021. É uma característica estrutural da casa."

> "E o que explica essa dificuldade é a concentração da carteira. Numa regressão com todas
> as seis características, a concentração média da gestora é a única significativa, e o
> modelo explica metade da variação entre casas."

Feche olhando para a banca, não para o slide. **Esta frase não está escrita em lugar nenhum
do slide, é sua e só sai se você falar. Decore.**

> "Ou seja: prever a carteira de um fundo é, em boa medida, uma função do estilo de
> concentração da casa. Não do tamanho dela, e não do quanto ela capta. Essa é a
> regularidade nova do trabalho."

---

## Slide 6 — Conclusão (35 segundos)

Não releia os três bullets. Diga o terceiro e passe às limitações, que é o que mostra
maturidade.

> "O achado central é o terceiro: a previsibilidade da carteira é função do estilo de
> concentração da gestora."

> "Três limitações que eu reconheço. Metade da variância entre gestoras segue sem
> explicação, provavelmente estilo e mandato, que eu não observo. O beta é medido no próprio
> mês, e não defasado como as outras características, o que abre endogeneidade mecânica. E o
> ajuste parcial tem a distância até o alvo e a variação do peso compartilhando um termo por
> construção, o que empurra o lambda para cima mesmo sob ruído."

> "Obrigado."

---

# Perguntas prováveis

Respostas curtas. Em arguição, resposta longa é resposta insegura.

**"Onde o modelo falha?"** — É a pergunta que você quer receber. Tenha esta história pronta:

> "O pior caso da amostra é um fundo da Squadra com uma posição em Equatorial entre 97% e
> 99,9% do patrimônio, estável o tempo todo. O modelo olha aquilo, acha que está longe demais
> do alvo, e prevê uma reversão todo mês. A reversão nunca vem e o erro se acumula, chegando
> a menos 45% de margem em doze meses. É um veículo de posição única, e características que
> descrevem fundos diversificados não capturam isso."

**"Por que logística e não mínimos quadrados?"**
Porque o peso é limitado entre zero e um. Uma linear preveria valores fora do intervalo
justamente nas posições concentradas. Na prática, nenhuma das 8,2 milhões de previsões da
logística caiu fora.

**"1,5% de margem é economicamente relevante?"**
Em nível, é pouco. Mas o adversário é a regra ingênua, que num processo muito persistente é
difícil de bater; a margem cresce de forma monotônica até 7,1%, que é o padrão previsto pela
teoria; e vence em 23 dos 23 meses, o que é mais informativo que a média.

**"O beta contemporâneo não vicia?"**
Pode viciar a leitura do coeficiente, e está registrado como limitação. Não é vazamento de
informação futura, porque peso e beta usam dado até o mesmo fechamento. E não afeta a Etapa
3, que compara duas previsões usando a mesma informação.

**"O lambda positivo não é artefato?"**
Em parte pode ser. A distância até o alvo e a variação do peso compartilham o peso corrente,
então ruído de medição empurra o lambda para cima, e eu não isolo essa parcela. O que
sustenta o resultado é ele sobreviver ao teste fora da amostra, com o parâmetro congelado.

**"Por que o funil de 3.254 para 2.507?"**
Quase tudo é exigência de beta. Dos 678 fundos que saem, 610 têm as outras características
mas nunca chegam a ter beta, e 91,5% desses simplesmente não acumulam 252 pregões de cota na
janela. São fundos jovens demais, não é buraco no dado.

**"Por que parar em 2021?"**
É até onde o dado público tem qualidade suficiente para a limpeza descrita. Cheguei a montar
uma base até 2026, mas não consegui validá-la no nível que o trabalho exige, e preferi não
usar dado que não posso defender.

**"Isso serve para investir?"**
Prever carteira não é prever preço. São coisas diferentes, e este trabalho faz a primeira.
**Não abra esse assunto se não perguntarem.**

---

# Véspera

- Cronometrar uma vez. Se passar de 5:30, cortar a última frase do slide 2.
- Quatro números na ponta da língua: **2.507 fundos, 8,2 milhões de observações, lambda de
  0,069, margem de 1,5% a 7,1%**.
- Saber dizer por que a concentração do resto exclui o ativo-alvo: para não explicar o peso
  com uma medida que já contém esse peso.
- Ter a história da Squadra pronta. É a sua melhor resposta e ela não está em nenhum slide.
- Levar o PDF em pendrive, além da nuvem.
