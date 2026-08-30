# Roteiro de apresentação — 9 minutos, 10 slides

**Orçamento de tempo:**

| Slide | Assunto | Tempo | Acumulado |
|---|---|---|---|
| 1 | Capa | 0:15 | 0:15 |
| 2 | A pergunta e a base | 0:55 | 1:10 |
| 3 | Etapa 1: o corte transversal | 1:05 | 2:15 |
| 4 | Etapas 2 e 3: as equações | 1:05 | 3:20 |
| 5 | Etapa 1: o que explica o peso | 0:55 | 4:15 |
| 6 | Etapas 2 e 3: os resultados | 1:00 | 5:15 |
| 7 | A heterogeneidade entre gestoras | 1:05 | 6:20 |
| 8 | Erro e margem por gestora | 0:55 | 7:15 |
| 9 | O que explica a variância | 1:00 | 8:15 |
| 10 | Conclusão | 0:45 | 9:00 |

Se precisar comprimir para 7 minutos, os cortes na ordem são: as duas últimas frases do
slide 3, o slide 8 inteiro (a tabela fala sozinha enquanto você resume em uma frase), e o
detalhe fundo-mês do slide 9.

Regra de ouro: **se você se pegar lendo em voz alta um número que já está no slide, está
perdendo tempo.** O slide mostra o número, você diz o que ele significa.

---

## Slide 1 — Capa (15 segundos)

Sem preâmbulo.

> "Peso de ações nos fundos de investimento brasileiros. O trabalho investiga o que
> determina esse peso e se ele é previsível."

---

## Slide 2 — A pergunta e a base (55 segundos)

**Primeiro, a ideia que justifica tudo:**

> "A carteira divulgada de um fundo é demanda revelada. E quando eu olho todos os fundos
> que carregam a mesma ação no mesmo mês, o preço é o mesmo para todos. Então o que sobra
> para explicar a diferença entre as carteiras são características dos próprios fundos.
> Daí as duas perguntas: o que explica o peso, e ele é previsível."

**Segundo, o diferencial da base. Diga com firmeza, é o seu ativo mais forte:**

> "A base é o universo inteiro da CVM com posição em ações, entre 2016 e 2021. Três mil
> duzentos e cinquenta e quatro fundos no bruto, dois mil quinhentos e sete na amostra
> final, oito milhões de observações. Não é um recorte de uma gestora ou de um papel, é
> tudo, e é dado inteiramente público."

Não explique o funil de 3.254 para 2.507 aqui. Se perguntarem, você responde.

---

## Slide 3 — Etapa 1, o corte transversal (1 min 05)

Este slide agora tem as equações completas. Não deduza nada, explique o papel de cada peça.

> "Para cada ação e cada mês eu rodo uma regressão logística do peso nas seis
> características do fundo. Uma regressão por célula, sem agregar nada por cima. A
> logística é escolhida porque o peso vive entre zero e um, e uma linear preveria peso
> negativo ou acima de cem justamente nas posições concentradas, que são as interessantes.
> Na prática nenhuma das oito milhões de previsões caiu fora do intervalo."

**A segunda equação é a contribuição original, e merece trinta segundos sozinha:**

> "A sexta característica é a concentração do restante da carteira. Eu pego o índice de
> Herfindahl da carteira do fundo e subtraio o quadrado do peso do próprio ativo que estou
> tentando explicar. Isso é essencial: sem subtrair, eu estaria explicando o peso com uma
> medida que já contém esse peso dentro dela. E defaso para o mês anterior, para manter a
> mesma disciplina das outras cinco características."

Se estiver adiantado, acrescente que essa é a característica que o trabalho introduz e que
não estava no desenho original. Se estiver atrasado, corte as duas últimas frases do slide.

---

## Slide 4 — Etapas 2 e 3, as equações (1 min 05)

> "A Etapa 1 me dá um alvo, mas o fundo não pula para ele. O lambda mede que fração da
> distância ele fecha por mês. É a mesma estrutura que a literatura de estrutura de capital
> usa para alavancagem-alvo."

**Depois, a Etapa 3, e faça questão de mostrar que o teste é honesto:**

> "Na Etapa 3 eu estimo o lambda só com dado anterior a janeiro de 2020, congelo o
> parâmetro, e aplico no período seguinte, que o modelo nunca viu. As duas linhas de cima
> são as duas previsões que eu comparo: a do ajuste parcial e a ingênua, que é simplesmente
> repetir o último peso. Embaixo está o erro e a margem, que é a redução percentual do
> RMSE."

Feche preparando o próximo resultado. **Não corte esta frase:**

> "Vale insistir que a ingênua é um adversário duro. Peso de carteira é muito persistente, e
> repetir o último valor já acerta quase tudo."

---

## Slide 5 — Etapa 1, o que explica o peso (55 segundos)

Aponte para as duas primeiras linhas e ignore o resto da tabela.

> "Duas características dominam. Beta da cota, com sinal positivo em 88% das treze mil e
> setecentas células, e concentração do resto, com 87,6%. E a concentração é significativa
> em 74% das células, com sinal positivo em 96% delas."

A leitura econômica, que é o que a banca quer ouvir:

> "Fundo mais exposto ao mercado carrega mais de qualquer ação, o que é quase mecânico mas
> confirma que o modelo captura algo real. E quem já concentrava no mês anterior concentra
> de novo. Concentrar é um traço de estilo da casa, não uma decisão tomada ativo por ativo."

**Guarde essa última frase, porque os slides 7 e 9 voltam nela.**

Se sobrar fôlego, o contraste que não está escrito no slide: tamanho e cotistas têm sinais
opostos, ou seja, fundo grande de poucos investidores e fundo pulverizado de varejo são
objetos diferentes, embora os dois pareçam grandes pelo patrimônio.

---

## Slide 6 — Etapas 2 e 3, os resultados (1 minuto)

Comece pelo lambda e traduza:

> "O lambda é 0,069 ao mês. O fundo fecha só 7% da distância até o alvo por mês, o que dá
> meia-vida de dez meses. É um ajuste real, mas lento, e é essa lentidão que faz o peso
> parecer um passeio aleatório no curto prazo."

**Não venda demais o resultado fora da amostra:**

> "A margem sobre a ingênua vai de 1,5% em um mês a 7,1% em doze. Parece pouco em nível, e é
> pouco, mas o que importa é o padrão: ela cresce de forma monotônica com o horizonte, que é
> exatamente o que a teoria de ajuste parcial prevê, e vence em 23 dos 23 meses de teste,
> não na média."

Aponte para a última linha da tabela:

> "O lambda estimado também cresce com o horizonte, de 0,07 para 0,34. Faz sentido: mais
> tempo, mais da distância percorrida."

**Transição, e é a mais importante da apresentação:** "Só que esse agregado esconde uma
coisa bem mais interessante."

---

## Slide 7 — A heterogeneidade (1 min 05)

O gráfico é a Figura 2 do trabalho. **Aponte para ele logo no começo**, porque a diferença
de amplitude entre os dois painéis é visível de longe e faz o argumento sozinha.

> "Entre as quarenta gestoras, o erro varia quase trinta vezes. No painel da esquerda estão
> as três mais difíceis de prever, no da direita as três mais previsíveis, e os dois estão
> na mesma escala. A diferença de amplitude é o resultado."

> "E não é ruído. Dividindo o período de teste ao meio, cada gestora se parece consigo
> mesma, com correlação de 0,85 na dispersão do erro. Quem é difícil em 2020 continua
> difícil em 2021. É uma característica estrutural da casa."

> "O viés, ao contrário, quase não importa. Responde por menos de meio por cento do erro
> quadrático, e quando eu tentei corrigir, estimando o desvio na primeira metade e aplicando
> na segunda, a melhora foi de 0,04%. Ou seja, nada. É a variância que precisa ser
> explicada."

---

## Slide 8 — Erro e margem por gestora (55 segundos)

Não leia a tabela. Diga o que ela mostra e aponte para duas linhas.

> "Aqui estão as cinco mais difíceis e as cinco mais previsíveis, dos quarenta grupos de
> gestora, com erro e margem nos quatro horizontes."

O ponto sutil, que mostra que você entendeu os próprios resultados:

> "E erro alto não é a mesma coisa que margem baixa. A AZ Quest é das mais difíceis de
> prever, e mesmo assim o modelo ajuda 15% nela em doze meses. Já a Squadra é o único caso
> em que o modelo piora de verdade, e piora cada vez mais com o horizonte, chegando a menos
> 45%."

Se tiver tempo, a história curta, que é o momento mais concreto da apresentação:

> "Fui olhar o que era. É um fundo só, com uma posição em Equatorial entre 97% e 99,9% do
> patrimônio, estável o tempo todo. O modelo acha que aquilo está longe demais do alvo e
> prevê uma reversão todo mês. A reversão nunca vem e o erro se acumula."

**Se estiver atrasado, este é o slide para resumir em uma frase e passar.**

---

## Slide 9 — O que explica a variância (1 minuto)

> "Para isolar o efeito de cada característica, eu regrido a dispersão do erro de cada
> gestora nas seis características, todas medidas só no treino, enquanto a dependente é
> medida só no teste. A concentração é a única significativa, a 1%, e o modelo explica
> metade da variação entre casas."

Depois o refinamento da direita, que é o que fecha o argumento:

> "Descendo ao nível de fundo e mês, a correlação contemporânea é 0,48 e a defasada é 0,49.
> Serem quase iguais afasta a hipótese de coincidência dentro do mesmo mês. Mas dentro do
> mesmo fundo, removendo a média dele, cai para 0,07. Ou seja, a concentração diferencia
> fundos entre si, e não meses de um mesmo fundo. É um traço permanente, não um sinal de
> curto prazo."

Antecipe a objeção óbvia, porque alguém vai levantar:

> "Alguém pode perguntar por que a concentração ainda explica se ela já está dentro do
> modelo. São perguntas diferentes. Na Etapa 1 ela explica o peso de uma posição
> específica. Aqui ela explica o erro de gestoras inteiras."

---

## Slide 10 — Conclusão (45 segundos)

Não releia os bullets. Vá direto ao terceiro e depois às limitações.

> "O achado central é o terceiro: prever a carteira de um fundo é, em boa medida, uma função
> do estilo de concentração da casa. Não do tamanho dela, e não do quanto ela capta. Essa é
> a regularidade nova do trabalho."

**Essa frase não está escrita em nenhum slide. É sua e só sai se você falar. Decore.**

Depois as limitações, sem defensividade:

> "Quatro limitações que eu reconheço. Metade da variância entre gestoras segue sem
> explicação, provavelmente estilo e mandato, que eu não observo. O beta é medido no próprio
> mês, e não defasado como as outras características, o que abre endogeneidade mecânica. O
> ajuste parcial tem a distância até o alvo e a variação do peso compartilhando um termo por
> construção, o que empurra o lambda para cima mesmo sob ruído. E a amostra termina em 2021."

> "Obrigado."

---

# Perguntas prováveis

Respostas curtas. Em arguição, resposta longa é resposta insegura.

**"Por que logística e não mínimos quadrados?"**
Porque o peso é limitado entre zero e um. Uma linear preveria valores fora do intervalo
justamente nas posições concentradas. Na prática, nenhuma das 8,2 milhões de previsões da
logística caiu fora.

**"Por que subtrair o próprio ativo do índice de concentração?"**
Para não explicar o peso com uma medida que já o contém. Se eu usasse o Herfindahl cheio, o
peso do ativo-alvo estaria dos dois lados da regressão, e o coeficiente seria em parte
circular. Subtrair o quadrado do peso defasado resolve isso de forma exata.

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
sustenta o resultado é ele sobreviver ao teste fora da amostra, com o parâmetro congelado no
treino.

**"O lambda é o mesmo para todos os fundos. Isso não é forte demais?"**
É, e é a extensão mais natural do trabalho. Os resultados de heterogeneidade dos slides 7 e
8 sugerem que deixar o lambda variar por gestora tem conteúdo empírico. Não fiz aqui para
manter o modelo com um parâmetro só e o teste fora da amostra limpo.

**"Por que o funil de 3.254 para 2.507?"**
Quase tudo é exigência de beta. Dos 678 fundos que saem, 610 têm as outras características
mas nunca chegam a ter beta, e 91,5% desses simplesmente não acumulam 252 pregões de cota na
janela. São fundos jovens demais, não é buraco no dado.

**"Por que o corte em 150% e não em 100%?"**
Porque fundos das classes Ações Livre e Multimercados Livre ultrapassam 100% de forma
persistente por alavancagem legítima. Um corte em 100% eliminaria carteiras válidas junto
com os erros de reporte. Em 150%, sobram 34 fundo-mês, que são erro de reporte da fonte.

**"Por que parar em 2021?"**
É até onde o dado público tem qualidade suficiente para a limpeza descrita. Cheguei a montar
uma base até 2026, mas não consegui validá-la no nível que o trabalho exige, e preferi não
usar dado que não posso defender.

**"O resultado não é dominado pela pandemia?"**
O período de teste é janeiro de 2020 a dezembro de 2021, então a pandemia está dentro dele.
Dois pontos: o modelo vence em 23 dos 23 meses, e não em alguns meses extremos; e a
persistência entre a primeira e a segunda metade do teste, 2020 contra 2021, é de 0,85, o
que seria improvável se o resultado viesse de um choque único.

**"Isso serve para investir?"**
Prever carteira não é prever preço. São coisas diferentes, e este trabalho faz a primeira.
**Não abra esse assunto se não perguntarem.**

---

# Véspera

- Cronometrar uma vez inteiro. Se passar de 9:30, cortar as duas últimas frases do slide 3 e
  resumir o slide 8 em uma frase.
- Números na ponta da língua: **2.507 fundos, 8,2 milhões de observações, lambda de 0,069,
  meia-vida de 10 meses, margem de 1,5% a 7,1%, persistência de 0,85, R² de 0,496**.
- Saber escrever de cabeça a equação da concentração do resto, porque é a contribuição
  original e é a mais provável de ser cobrada no quadro.
- Levar o PDF em pendrive, além da nuvem.
