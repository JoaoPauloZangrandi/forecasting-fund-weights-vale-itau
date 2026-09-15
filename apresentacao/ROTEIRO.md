# Roteiro de apresentação, versão de 7 minutos

## Como usar

São duas camadas, e elas servem para coisas diferentes.

**O cartão de cena**, logo abaixo, é o que você leva. Uma linha por slide. Serve para o
momento em que você olha para baixo e precisa saber, em meio segundo, onde está e qual é o
próximo número.

**O texto corrido**, depois do cartão, é o que você ensaia. Está escrito para ser dito em voz
alta, não para ser lido com os olhos: frases curtas, poucas subordinadas, números por
extenso onde a leitura tropeça. Ensaie com ele duas vezes e depois esqueça que existe.

O que está entre colchetes é instrução, não fala.

### Quanto tempo isso dura de verdade

O texto corrido tem **1.037 palavras**. Contadas, não estimadas. O que isso vira em
minutos depende só do seu ritmo:

| Ritmo | Duração | Quando acontece |
|---|---|---|
| 130 palavras por minuto | **7 min 59 s** | pausado, com pausa nas transições |
| 145 palavras por minuto | **7 min 09 s** | ritmo de conversa, o mais provável |
| 160 palavras por minuto | **6 min 29 s** | acelerado, é o que a adrenalina faz |

O alvo é 7 minutos no ritmo de conversa. Nos 130, que é o ritmo de quem respira nas
transições e aponta para os gráficos, você encosta em oito. Isso é folga, não problema: a
margem existe para as pausas e para o tempo que você gasta apontando para a figura do slide 7.

**Por slide**, para saber se está no ritmo:

| Slide | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 |
|---|---|---|---|---|---|---|---|---|---|---|
| Palavras | 33 | 128 | 112 | 120 | 86 | 128 | 109 | 117 | 107 | 97 |
| Segundos (145 ppm) | 14 | 53 | 46 | 50 | 36 | 53 | 45 | 48 | 44 | 40 |

A marca que importa é uma só: **ao terminar o slide 6, você deve estar por volta de
4 min 11 s**. Se estiver muito antes, desacelere. Se estiver muito depois, aplique os
cortes da seção Véspera.

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
| 8 | Tabela por gestora | "erro alto não é margem baixa" | AZ Quest +15%, Squadra −45,6% |
| 9 | A variância | "0,48 e 0,49 quase iguais, mas 0,07 dentro do fundo" | R² 0,496, ajustado 0,405 |
| 10 | Conclusão | "função do estilo de concentração da casa" | dizer isso de cabeça |

---

## Texto corrido

### Slide 1, capa

> Bom dia. Peso de ações nos fundos de investimento brasileiros. Eu investigo o que determina
> o peso de cada ação na carteira de um fundo, e até que ponto esse peso é previsível.

[Vira o slide sem pausa.]

---

### Slide 2, a pergunta e a base

> A carteira que um fundo divulga é demanda revelada. Quando ele carrega três por cento de
> uma ação, está dizendo quanto quer daquele papel.

> E aí vem o pulo. Se eu olho todos os fundos que carregam a mesma ação, no mesmo mês, o
> preço é o mesmo para todos. O que sobra para explicar a diferença entre as carteiras são
> características dos próprios fundos.

> Daí as duas perguntas. O que explica o peso, e se esse peso é previsível.

[Aponte para a tabela.]

> A base é o universo inteiro da CVM com posição em ações, de 2016 a 2021. Três mil duzentos
> e cinquenta e quatro fundos no bruto, dois mil quinhentos e sete na amostra final, oito
> milhões de observações. Não é recorte de uma gestora nem de um papel.

---

### Slide 3, Etapa 1, o corte transversal

> A Etapa 1 responde à primeira pergunta. Para cada ação e cada mês eu rodo uma regressão
> logística do peso nas seis características do fundo. Uma regressão por célula, sem agregar
> nada por cima.

> A logística não é enfeite. O peso vive entre zero e um, e uma linear preveria valores fora
> do intervalo justamente nas posições concentradas.

[Aponte para a segunda equação. Desacelere aqui, é a contribuição do trabalho.]

> A segunda equação é a concentração do restante da carteira. Eu pego o Herfindahl do fundo e
> subtraio o quadrado do peso do próprio ativo que estou tentando explicar. Se eu não
> subtraísse, estaria explicando o peso com uma medida que já o contém. E defaso para o mês
> anterior, como as outras cinco.

---

### Slide 4, Etapas 2 e 3, as equações

> A Etapa 1 me dá um alvo, mas o fundo não pula para ele. O lambda da Etapa 2 é a fração da
> distância que o fundo fecha por mês. É a mesma estrutura que finanças corporativas usa para
> alavancagem-alvo.

> A Etapa 3 testa se isso vale alguma coisa. Estimo o lambda só com dado anterior a janeiro
> de 2020, congelo o parâmetro e aplico no período seguinte, que o modelo nunca viu.

[Aponte para as duas equações de previsão.]

> São essas duas previsões que eu comparo: a do ajuste parcial e a ingênua, que é repetir o
> último peso. E eu quero insistir num ponto. A ingênua é um adversário duro. Peso de
> carteira é muito persistente, e repetir o último valor já acerta quase tudo.

---

### Slide 5, o que explica o peso

> Primeiro resultado. Duas características dominam. Beta da cota, com sinal positivo em
> oitenta e oito por cento das treze mil e setecentas células. E concentração do resto, com
> oitenta e sete e seis.

> Fundo mais exposto ao mercado carrega mais de qualquer ação, o que é quase mecânico. O
> segundo é mais interessante. Quem já concentrava no mês anterior concentra de novo.
> Concentrar é um traço de estilo da casa, não uma decisão tomada ativo por ativo.

> Guardem essa frase, porque eu volto nela no fim.

---

### Slide 6, λ e o desempenho fora da amostra

> Segundo resultado. O lambda é zero vírgula zero seis nove ao mês. O fundo fecha só sete por
> cento da distância até o alvo por mês, meia-vida de dez meses. Ajuste lento, e é essa
> lentidão que faz o peso parecer um passeio aleatório no curto prazo.

> Fora da amostra, o modelo vence a ingênua nos quatro horizontes, com margem de um e meio
> por cento em um mês a sete por cento em doze.

> Parece pouco, e é pouco em nível. Mas o que importa é o padrão. Ela cresce de forma
> monotônica com o horizonte, como a teoria prevê, e vence em vinte e três dos vinte e três
> meses. Não é média, é todos.

[Transição. Pause meio segundo antes.]

> Só que esse número agregado esconde uma coisa bem mais interessante.

---

### Slide 7, a heterogeneidade

[Aponte para o gráfico antes de falar.]

> Entre as quarenta gestoras, o erro varia quase trinta vezes. À esquerda, as três gestoras
> mais difíceis de prever. À direita, as três mais previsíveis, na mesma escala. A diferença
> de amplitude é o resultado.

> E não é ruído. Dividi o período de teste ao meio e comparei cada gestora consigo mesma. A
> correlação da dispersão do erro é zero vírgula oitenta e cinco. Quem é difícil em 2020
> continua difícil em 2021.

> O viés, ao contrário, quase não importa. Responde por menos de meio por cento do erro
> quadrático, e corrigi-lo melhora o RMSE em zero vírgula zero quatro por cento. É a
> variância que precisa ser explicada.

---

### Slide 8, erro e margem por gestora

> Aqui está a evidência bruta. As cinco mais difíceis e as cinco mais previsíveis, com erro e
> margem nos quatro horizontes.

> E tem um detalhe importante. Erro alto não é a mesma coisa que margem baixa. A AZ Quest é
> das mais difíceis de prever, e mesmo assim o modelo ajuda quinze por cento nela em doze
> meses. Já a Squadra é o único caso em que o modelo piora de verdade, e piora mais a cada
> horizonte, chegando a menos quarenta e cinco.

> Fui olhar. É um fundo só, com quase cem por cento do patrimônio em Equatorial, estável o
> tempo todo. O modelo prevê reversão todo mês, ela nunca vem, e o erro se acumula.

---

### Slide 9, o que explica a variância

> Então, o que explica essa variância. Eu regrido a dispersão do erro de cada gestora nas
> seis características, medidas só no treino, com a dependente medida só no teste. A
> concentração é a única significativa, e o modelo explica metade da variação entre
> casas.

[Aponte para o quadro da direita.]

> E esse quadro fecha o argumento. No nível de fundo e mês, a correlação contemporânea é zero
> vírgula quarenta e oito e a defasada é zero vírgula quarenta e nove. Serem quase iguais
> afasta coincidência dentro do mesmo mês. Mas dentro do mesmo fundo cai para zero vírgula
> zero sete. A concentração diferencia fundos entre si, e não meses de um mesmo fundo.

---

### Slide 10, conclusão

> Fechando. O achado central é o terceiro.

[Olhe para a banca, não para o slide. Esta frase não está escrita em nenhum lugar.]

> Prever a carteira de um fundo é, em boa medida, uma função do estilo de concentração da
> casa. Não do tamanho dela, e não do quanto ela capta.

> E quatro limitações. Metade da variância entre gestoras segue sem
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

**"O R² de 0,496 não é alto demais para 40 observações e 6 regressores?"**
Por isso o slide traz o ajustado junto, 0,405. A distância entre os dois é grande e é
esperada nesse tamanho de amostra. O que sustenta a leitura não é o R², é a concentração ser
a única significativa entre as seis, e o resultado aparecer de novo no nível de fundo e mês.

**"O resultado não é dominado pela pandemia?"**
O teste é janeiro de 2020 a dezembro de 2021, então ela está dentro. Dois pontos: o modelo
vence em 23 dos 23 meses, não em alguns meses extremos; e a persistência entre 2020 e 2021 é
de 0,85, o que seria improvável se viesse de um choque único.

**"Por que o funil de 3.254 para 2.507?"**
Quase tudo é exigência de beta. Dos 678 que saem, 610 têm as outras características mas nunca
chegam a ter beta, e 91,5% desses não acumulam 252 pregões de cota na janela. São fundos
jovens demais, não é buraco no dado. Antes disso há um degrau pequeno: 26 fundos do universo
só têm posição em empréstimo ou direito de subscrição, e por isso não geram nenhuma linha de
posição direta.

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
- Se estourar, corte nesta ordem: o terceiro parágrafo do slide 8 (a história da Squadra vira
  resposta de arguição), o segundo parágrafo do slide 3, e o segundo parágrafo do slide 7.
  Os três somam cerca de 40 segundos.
- Se sobrar tempo, o lugar de gastar é o slide 7, apontando para os dois painéis antes de
  falar, e o slide 5, acrescentando que tamanho e número de cotistas têm sinais opostos, ou
  seja, fundo grande de poucos investidores e fundo pulverizado de varejo são objetos
  diferentes.
- Números na ponta da língua: **2.507 fundos, 8,2 milhões de observações, lambda de 0,069,
  meia-vida de 10 meses, margem de 1,5% a 7,1%, persistência de 0,85, R² de 0,496 e ajustado
  de 0,405**.
- Saber escrever de cabeça a equação da concentração do resto. É a contribuição original e a
  mais provável de ser cobrada no quadro.
- Levar o PDF em pendrive, além da nuvem.
