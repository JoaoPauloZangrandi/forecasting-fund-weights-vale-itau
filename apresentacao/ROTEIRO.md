# Roteiro de apresentação

## Como usar

São duas camadas, e elas servem para coisas diferentes.

**O cartão de cena**, logo abaixo, é o que você leva. Uma linha por slide. Serve para o
momento em que você olha para baixo e precisa saber, em meio segundo, onde está e qual é o
próximo número.

**O texto corrido**, depois do cartão, é o que você ensaia. Está escrito para ser dito em voz
alta, não para ser lido com os olhos: frases curtas, poucas subordinadas, números por
extenso onde a leitura tropeça. Ensaie com ele duas vezes e depois esqueça que existe.

O que está entre colchetes é instrução, não fala. O que está marcado como opcional só entra
se você estiver adiantado.

### Quanto tempo isso dura de verdade

O texto corrido tem **1.203 palavras**. Contadas, não estimadas. O que isso vira em minutos
depende só do seu ritmo:

| Ritmo | Duração | Quando acontece |
|---|---|---|
| 130 palavras por minuto | **9,3 min** | pausado, com pausa nas transições |
| 145 palavras por minuto | **8,3 min** | ritmo de conversa, o mais provável |
| 160 palavras por minuto | **7,5 min** | acelerado, é o que a adrenalina faz |

Ou seja, o roteiro cabe confortavelmente em 9 minutos e sobra folga para apontar para os
gráficos e respirar nas transições. O risco real não é estourar, é o contrário: acelerar sem
perceber e terminar em sete minutos e meio, o que soa apressado.

**Por slide**, para você saber se está no ritmo:

| Slide | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 |
|---|---|---|---|---|---|---|---|---|---|---|
| Palavras | 36 | 149 | 144 | 140 | 97 | 137 | 132 | 143 | 125 | 100 |
| Segundos (145 ppm) | 15 | 62 | 60 | 58 | 40 | 57 | 55 | 59 | 52 | 41 |

A marca que importa é uma só: **ao terminar o slide 6, você deve estar por volta de 4 min 50**.
Se estiver muito antes disso, desacelere. Se estiver muito depois, aplique os cortes da seção
Véspera.

---

## Cartão de cena

| # | Slide | Âncora | Não esquecer |
|---|---|---|---|
| 1 | Capa | "o que determina o peso, e se ele é previsível" | 15s, sem preâmbulo |
| 2 | A pergunta e a base | "mesmo mês, mesmo preço, sobra o fundo" | universo inteiro, 2.507, 8 milhões |
| 3 | Etapa 1 | "logística porque o peso vive entre zero e um" | **subtrair o ativo-alvo do HHI** |
| 4 | Etapas 2 e 3 | "estimo antes de 2020, congelo, aplico depois" | a ingênua é adversário duro |
| 5 | O que explica o peso | "concentrar é traço de estilo da casa" | 88,2% e 87,6% |
| 6 | λ e fora da amostra | "fecha só 7% da distância por mês" | 23 de 23 meses |
| 7 | Heterogeneidade | "os dois painéis na mesma escala" | 30 vezes; persistência 0,85 |
| 8 | Tabela por gestora | "erro alto não é margem baixa" | AZ Quest +15%, Squadra −45% |
| 9 | A variância | "0,48 e 0,49 quase iguais, mas 0,07 dentro do fundo" | R² 0,496 |
| 10 | Conclusão | "função do estilo de concentração da casa" | dizer isso de cabeça |

**Transição que não pode falhar**, do slide 6 para o 7: *"Só que esse número agregado esconde
uma coisa bem mais interessante."*

---

## Texto corrido

### Slide 1 — Capa

> Bom dia. Peso de ações nos fundos de investimento brasileiros. O que eu investigo é o que
> determina o peso de cada ação na carteira de um fundo, e até que ponto esse peso é
> previsível.

[Vira o slide sem pausa.]

---

### Slide 2 — A pergunta e a base

> Começo pela ideia que organiza o trabalho todo. A carteira que um fundo divulga é demanda
> revelada. Quando ele escolhe carregar três por cento de uma ação, ele está dizendo alguma
> coisa sobre quanto quer daquele papel.

> E aí vem o pulo. Se eu olho todos os fundos que carregam a mesma ação, no mesmo mês, o
> preço é o mesmo para todos eles. O que sobra para explicar a diferença entre as carteiras
> são características dos próprios fundos.

> Daí as duas perguntas. O que explica o peso. E se esse peso é previsível.

[Aponte para a tabela.]

> A base é o universo inteiro da CVM com posição em ações, de 2016 a 2021. Três mil
> duzentos e cinquenta e quatro fundos no bruto. Dois mil quinhentos e sete na amostra
> final. Oito milhões de observações. Não é um recorte de uma gestora ou de um papel. É
> tudo, e é dado público.

---

### Slide 3 — Etapa 1, o corte transversal

> A Etapa 1 responde à primeira pergunta. Para cada ação e cada mês, eu rodo uma regressão
> logística do peso nas seis características do fundo. Uma regressão por célula, sem agregar
> nada por cima.

> A logística não é enfeite. O peso vive entre zero e um. Uma linear preveria peso negativo,
> ou acima de cem por cento, justamente nas posições concentradas, que são as interessantes.
> Na prática, nenhuma das oito milhões de previsões caiu fora do intervalo.

[Aponte para a segunda equação. Desacelere aqui, é a contribuição do trabalho.]

> A segunda equação é a concentração do restante da carteira. Eu pego o Herfindahl da
> carteira do fundo e subtraio o quadrado do peso do próprio ativo que estou tentando
> explicar.

> Isso é essencial. Se eu não subtraísse, eu estaria explicando o peso com uma medida que já
> contém esse peso dentro dela. E defaso para o mês anterior, para manter a mesma disciplina
> das outras cinco.

---

### Slide 4 — Etapas 2 e 3, as equações

> A Etapa 1 me dá um alvo. Mas o fundo não pula para o alvo. A Etapa 2 mede isso: o lambda é
> a fração da distância que o fundo fecha por mês. É a mesma estrutura que finanças
> corporativas usa para alavancagem-alvo.

> A Etapa 3 é onde eu testo se isso vale alguma coisa. Estimo o lambda só com dado anterior
> a janeiro de 2020. Congelo o parâmetro. Aplico no período seguinte, que o modelo nunca viu.

[Aponte para as duas equações de previsão.]

> Essas duas linhas são as previsões que eu comparo. A do ajuste parcial, e a ingênua, que é
> simplesmente repetir o último peso. Embaixo, o erro e a margem, que é a redução percentual
> do erro.

> E eu quero insistir num ponto. A ingênua é um adversário duro. Peso de carteira é muito
> persistente. Repetir o último valor já acerta quase tudo.

---

### Slide 5 — O que explica o peso

> Primeiro resultado. Duas características dominam. Beta da cota, com sinal positivo em
> oitenta e oito por cento das treze mil e setecentas células. E concentração do resto, com
> oitenta e sete e meio.

> A leitura é direta. Fundo mais exposto ao mercado carrega mais de qualquer ação. Isso é
> quase mecânico, mas confirma que o modelo captura algo real.

> O segundo é mais interessante. Quem já concentrava no mês anterior concentra de novo.
> Concentrar é um traço de estilo da casa, não uma decisão tomada ativo por ativo.

> Guardem essa frase, porque eu volto nela no fim.

[Opcional, se estiver adiantado: tamanho e cotistas têm sinais opostos, ou seja, fundo
grande de poucos investidores e fundo pulverizado de varejo são objetos diferentes.]

---

### Slide 6 — λ e o desempenho fora da amostra

> Segundo resultado. O lambda é zero vírgula zero seis nove ao mês. Traduzindo: o fundo fecha
> só sete por cento da distância até o alvo por mês. Meia-vida de dez meses. Ajuste real,
> mas lento. E é essa lentidão que faz o peso parecer um passeio aleatório no curto prazo.

> Fora da amostra, o modelo vence a ingênua nos quatro horizontes. A margem vai de um e meio
> por cento em um mês a sete por cento em doze.

> Parece pouco, e é pouco em nível. Mas o que importa é o padrão. Ela cresce de forma
> monotônica com o horizonte, que é o que a teoria prevê. E vence em vinte e três dos vinte
> e três meses de teste. Não é média, é todos.

[Transição. Pause meio segundo antes.]

> Só que esse número agregado esconde uma coisa bem mais interessante.

---

### Slide 7 — A heterogeneidade

[Aponte para o gráfico antes de falar.]

> Entre as quarenta gestoras, o erro varia quase trinta vezes. No painel da esquerda, as três
> gestoras mais difíceis de prever. No da direita, as três mais previsíveis. Os dois estão na
> mesma escala. A diferença de amplitude é o resultado.

> E não é ruído. Eu dividi o período de teste ao meio e comparei cada gestora consigo mesma.
> A correlação da dispersão do erro é zero vírgula oitenta e cinco. Quem é difícil em 2020
> continua difícil em 2021. É estrutural da casa.

> O viés, ao contrário, quase não importa. Responde por menos de meio por cento do erro
> quadrático. Tentei corrigir, estimando na primeira metade e aplicando na segunda, e a
> melhora foi de zero vírgula zero quatro por cento. Ou seja, nada. É a variância que precisa
> ser explicada.

---

### Slide 8 — Erro e margem por gestora

> Aqui está a evidência bruta. As cinco mais difíceis e as cinco mais previsíveis, dos
> quarenta grupos, com erro e margem nos quatro horizontes.

> E tem um detalhe importante. Erro alto não é a mesma coisa que margem baixa. A AZ Quest é
> das mais difíceis de prever, e mesmo assim o modelo ajuda quinze por cento nela em doze
> meses. Já a Squadra é o único caso em que o modelo piora de verdade, e piora cada vez mais
> com o horizonte, chegando a menos quarenta e cinco.

> Fui olhar o que era. É um fundo só. Uma posição em Equatorial entre noventa e sete e
> noventa e nove por cento do patrimônio, estável o tempo todo. O modelo acha que aquilo está
> longe demais do alvo e prevê uma reversão todo mês. A reversão nunca vem, e o erro se
> acumula.

---

### Slide 9 — O que explica a variância

> Então, o que explica essa variância. Eu regrido a dispersão do erro de cada gestora nas
> seis características, todas medidas só no treino, enquanto a dependente é medida só no
> teste. A concentração é a única significativa, e o modelo explica metade da variação entre
> casas.

[Aponte para o quadro da direita.]

> E esse quadro fecha o argumento. No nível de fundo e mês, a correlação contemporânea é zero
> vírgula quarenta e oito e a defasada é zero vírgula quarenta e nove. Serem quase iguais
> afasta coincidência dentro do mesmo mês. Mas dentro do mesmo fundo, removendo a média dele,
> cai para zero vírgula zero sete.

> Ou seja: a concentração diferencia fundos entre si, e não meses de um mesmo fundo. É um
> traço permanente, não um sinal de curto prazo.

---

### Slide 10 — Conclusão

> Fechando. O achado central é o terceiro.

[Olhe para a banca, não para o slide. Esta frase não está escrita em nenhum lugar.]

> Prever a carteira de um fundo é, em boa medida, uma função do estilo de concentração da
> casa. Não do tamanho dela, e não do quanto ela capta.

> E quatro limitações que eu reconheço. Metade da variância entre gestoras segue sem
> explicação. O beta é medido no próprio mês, e não defasado como as outras características,
> o que abre endogeneidade. O ajuste parcial tem a distância até o alvo e a variação do peso
> compartilhando um termo por construção, o que empurra o lambda para cima. E a amostra
> termina em 2021.

> Obrigado.

---

# Perguntas prováveis

Resposta curta. Em arguição, resposta longa soa insegura. Se não souber, diga que não sabe e
diga como descobriria.

**"Por que logística e não mínimos quadrados?"**
Porque o peso é limitado entre zero e um. Uma linear preveria valores fora do intervalo
justamente nas posições concentradas. Na prática, nenhuma das 8,2 milhões de previsões caiu
fora.

**"Por que subtrair o próprio ativo do índice de concentração?"**
Para não explicar o peso com uma medida que já o contém. Com o Herfindahl cheio, o peso do
ativo-alvo estaria dos dois lados da regressão e o coeficiente seria em parte circular.
Subtrair o quadrado do peso defasado resolve isso de forma exata.

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

**"O lambda é o mesmo para todos os fundos. Não é forte demais?"**
É, e é a extensão mais natural do trabalho. A heterogeneidade dos slides 7 e 8 sugere que
deixar o lambda variar por gestora tem conteúdo empírico. Não fiz aqui para manter um
parâmetro só e o teste fora da amostra limpo.

**"O resultado não é dominado pela pandemia?"**
O teste é janeiro de 2020 a dezembro de 2021, então ela está dentro. Dois pontos: o modelo
vence em 23 dos 23 meses, não em alguns meses extremos; e a persistência entre 2020 e 2021 é
de 0,85, o que seria improvável se viesse de um choque único.

**"Por que o funil de 3.254 para 2.507?"**
Quase tudo é exigência de beta. Dos 678 que saem, 610 têm as outras características mas nunca
chegam a ter beta, e 91,5% desses não acumulam 252 pregões de cota na janela. São fundos
jovens demais, não é buraco no dado.

**"Por que o corte em 150% e não em 100%?"**
Porque fundos Ações Livre e Multimercados Livre ultrapassam 100% de forma persistente por
alavancagem legítima. Cortar em 100% eliminaria carteiras válidas junto com os erros. Em
150%, sobram 34 fundo-mês, que são erro de reporte da fonte.

**"Por que parar em 2021?"**
É até onde o dado público tem qualidade suficiente para a limpeza descrita. Cheguei a montar
uma base até 2026, mas não consegui validá-la no nível que o trabalho exige, e preferi não
usar dado que não posso defender.

**"Isso serve para investir?"**
Prever carteira não é prever preço. São coisas diferentes, e este trabalho faz a primeira.
**Não abra esse assunto se não perguntarem.**

---

# Véspera

- Ensaiar duas vezes com o texto corrido, cronometrando. Depois uma terceira só com o cartão.
- Se estourar, corte nesta ordem: o parágrafo opcional do slide 5, o terceiro parágrafo do
  slide 8 (a história da Squadra vira resposta de arguição), e o segundo parágrafo do slide 3.
- Números na ponta da língua: **2.507 fundos, 8,2 milhões de observações, lambda de 0,069,
  meia-vida de 10 meses, margem de 1,5% a 7,1%, persistência de 0,85, R² de 0,496**.
- Saber escrever de cabeça a equação da concentração do resto. É a contribuição original e a
  mais provável de ser cobrada no quadro.
- Levar o PDF em pendrive, além da nuvem.
