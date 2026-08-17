# Auditoria adversarial: candidatos dos scripts 70-90 (exploração de sinais)

Auditoria de segunda camada, cética por construção — objetivo era achar
problemas, não confirmar que está tudo certo. Cobre `v2 OFICIAL/exploracao_sinais/scripts/70` a `87`
(todos os scripts existentes no intervalo 70-90) e, com atenção
especial, o candidato **E3 (número de cotistas dos fundos donos →
volatilidade futura)**, construído nos scripts 79-82. Nenhum código foi
alterado — só leitura, re-execução e re-derivação independente em R.

**Método:** reli o `LOG_CANDIDATOS.md` inteiro (foco na seção E3, linhas
2445-2607), li todo o código-fonte dos 18 scripts do intervalo, e
**re-executei em R** (não confiei no que o log reporta) a construção do
script 79 do zero, o teste do script 80 do zero, e uma correção
estatística (Newey-West) que nenhum script deste intervalo aplica.
Scripts usados para a re-derivação ficam documentados abaixo linha a
linha.

---

## Achado 1 (CRÍTICO): ausência de correção de autocorrelação em TODOS os testes Fama-MacBeth de volatilidade (h=3/6/12) — a alegação central "h=6 bate Bonferroni isoladamente" não sobrevive

**Onde:** `80_testar_fragilidade_donos.R` (função `fama_macbeth_recut_vol`,
linhas 144-166), `81_diagnostico_cotistas_churn.R` (função
`fm_multivariado`, linhas 82-109), e por extensão qualquer teste de
volatilidade com h>1 nesta família (inclui a réplica do achado principal
em `74_hhi_trajetoria_assimetria_e_vol.R`, linhas 146-171, parte "bônus").

**Descrição:** `vol_futura` em horizonte h é, por construção, o desvio-padrão
dos retornos dos PRÓXIMOS h meses (script 80, linhas 115-129: `calc_vol_futura`).
Isso significa que a "série temporal de spreads mensais" usada no teste
Fama-MacBeth (`t_fm <- media/(dp/sqrt(n_meses))`, ex. linha 163 do script 80)
é construída de **janelas sobrepostas**: o spread do mês `t` e o spread do
mês `t+1` compartilham `h-1` dos `h` meses de retorno que entram no cálculo
de `vol_futura`. Isso gera autocorrelação forte e mecânica entre
observações "mensais" consecutivas — violando a premissa de independência
que o cálculo de erro-padrão `dp/sqrt(n_meses)` assume. Esse é exatamente o
tipo de problema clássico de "overlapping data" em finanças (Hansen &
Hodrick 1980; Newey & West 1987), resolvido com erro-padrão HAC de lag
`h-1` — técnica que **a própria equipe já usou em outro lugar deste mesmo
log** (`LOG_CANDIDATOS.md` linha 1616-1617, candidato #R4: "erro-padrão
robusto a autocorrelação (Newey-West, lag=3)"), mas que nunca foi aplicada
a nenhum teste desta família de volatilidade (nem aqui, nem nos scripts
28-31 da exploração principal que estabeleceram o achado HHI→vol original).

**Re-derivação independente (script escrito do zero, não reaproveita
literalmente as funções dos scripts 80/81 — reimplementação própria com a
mesma lógica de `vol_futura`, mas medindo também a autocorrelação lag-1 do
spread e recomputando o t-stat com Newey-West, lag=h-1, kernel de Bartlett):

| Horizonte | AC(1) do spread mensal | t naive (como reportado) | p naive | t Newey-West | p Newey-West |
|---|---|---|---|---|---|
| h=3  | 0,484 | -3,057 | 0,00578  | -2,305 | **0,0310** |
| h=6  | 0,369 | -5,068 | 0,000059 | -3,385 | **0,00294** |
| h=12 | 0,810 | -4,734 | 0,000225 | -3,788 | **0,00161** |

A autocorrelação lag-1 é substancial em todos os horizontes (0,37 a 0,81 —
extrema em h=12, onde janelas consecutivas compartilham 11 dos 12 meses).
Depois da correção, **nenhum horizonte chega perto do limiar de Bonferroni
do log (0,05/520≈0,0001)** — o resultado central que justifica chamar E3 de
"achado mais forte" (h=6, p=0,00006 < 0,0001) some: com erro-padrão
correto, p=0,0029, quase 30× acima do limiar.

O mesmo problema também derruba o teste de robustez "mais duro" do script
81 (controle por volatilidade passada, linha 124): recomputando a mesma
série de coeficientes mensais com Newey-West, **h=6 deixa de ser
significativo a 5%** (p naive=0,027 → p NW=0,108); h=12 permanece
significativo (p NW=0,00091), mas com a ressalva de que, com n=17 meses e
lag=11 na correção HAC, o número de graus de liberdade efetivos é tão baixo
que a própria correção NW perde precisão nesse horizonte (ver Achado 2). O
controle de 4 variáveis simultâneas (script 81, linha 115) também
enfraquece bastante em h=6 (p naive=0,00107 → p NW=0,0124) mas continua
abaixo de 5%.

**Isso não é exclusivo do candidato E3** — o mesmo padrão (série mensal de
`vol_futura` sobreposta, sem correção) sustenta também o achado "mais
sólido de toda a exploração" (HHI→volatilidade, scripts 28-31 da
exploração principal, fora do escopo desta auditoria mas replicado dentro
dele em `74_hhi_trajetoria_assimetria_e_vol.R` linha 179). Recomendo
fortemente que a mesma correção seja aplicada lá também antes de qualquer
decisão final sobre o que entra no TCC.

**Solução concreta:** em `80_testar_fragilidade_donos.R` (linha ~163),
`81_diagnostico_cotistas_churn.R` (linha ~106) e `74_...` (linha ~164),
substituir `t_fm <- media/(dp/sqrt(n_meses))` por um erro-padrão HAC
(Newey-West, lag = h-1, kernel de Bartlett) sobre a série de spreads/
coeficientes mensais antes de calcular o p-valor. Reportar SEMPRE os dois
números (naive e HAC) daqui pra frente nesta família de testes, e
re-escrever a conclusão do candidato E3 no `LOG_CANDIDATOS.md` (linhas
2481-2482, 2545-2547) removendo a alegação "bate Bonferroni isoladamente" —
ela não é sustentável com erro-padrão correto.

---

## Achado 2 (IMPORTANTE): exclusão de COVID no script 82 purga o mês-preditor, mas não a janela de resultado (outcome) — a robustez de h=12 "sem COVID" é um artefato de amostra ínfima e sobreposta, não evidência adicional

**Onde:** `82_robustez_cotistas_covid_consistencia.R`, linha 74:
`sm_sem_covid <- sm[!(ym %in% 202002:202012)]`.

**Descrição:** `sm` é indexado pelo mês-BASE (mês em que `frag_cotistas` é
medido), não pelo mês do resultado. Mas `vol_futura` no mês-base `ym` é
calculada a partir dos retornos de `ym+1` até `ym+h` (script 82, linhas
29-41, idêntico ao script 80). Excluir só `ym ∈ [202002, 202012]` do
mês-base **não** garante que a janela de resultado fique livre de COVID.
Concretamente: o mês-base `202001` (jan/2020) **não** é excluído pelo
filtro da linha 74, mas sua janela de resultado para h=6 é fev-jul/2020 (o
crash inteiro) e para h=12 é fev/2020-jan/2021 (crash + toda a
recuperação) — ambos 100% dentro do período que o próprio script rotula
como "COVID". O texto do log (linha 2528: "jan/2020-fev/2021 conforme a
janela de 12 meses") já denuncia isso ao admitir que jan/2020 continua na
amostra "sem COVID".

**Re-derivação independente (script próprio, recomputando `sm` do zero e
comparando 3 versões):**

- h=6, como o script 82 realmente faz (só purga mês-base): n=10 meses,
  t=-3,070, p=0,0133 — bate com o log (t=-3,07, p=0,013).
- h=6, **corrigido** (purga também qualquer mês-base cuja janela de
  resultado toque fev-dez/2020 — nesse caso, só precisa remover jan/2020
  a mais): n=9 meses, t=-3,477, p=0,0084. **O resultado sobrevive bem à
  correção, inclusive fica um pouco mais forte** — bom sinal genuíno de
  robustez para h=6, uma vez que o erro é corrigido.
- h=12, como o script 82 faz: n=6 meses, t=-4,029, p=0,0100 — bate com o
  log (t=-4,03, p=0,010).
- h=12, **corrigido** (remove também jan/2020, cuja janela de 12 meses é
  100% COVID): restam **só 5 meses, todos de 2021 (jan-mai/2021)**, e o
  t-stat "corrigido" sai **t=-17,3, p=0,00007** — um número
  estatisticamente absurdo para n=5. Isso não é evidência mais forte; é
  o sintoma clássico de amostra minúscula com sobreposição extrema (para
  h=12, 5 meses consecutivos compartilham ≥ 11/12 dos dados nas suas
  janelas de resultado, ou seja, são quase **1 única observação
  independente** disfarçada de 5). O log não computou essa versão
  corrigida — reportou só a versão com o mês contaminado ainda dentro.

**Conclusão prática:** a alegação "h=6 sobrevive excluindo COVID de
verdade" está correta na direção, mas o número exato do log (t=-3,07,
n=10) usa uma amostra levemente contaminada — a versão corrigida (t=-3,48,
n=9) é, por acaso, ainda melhor, então isso não muda o veredito para h=6.
Já a alegação de robustez em h=12 "sem COVID" (log linha 2528, "sem COVID
... t=-4,03, p=0,010 — significativo mesmo com N=6") **deveria ser
descartada inteiramente** — com a contaminação corrigida, sobra amostra
insuficiente (n=5, quase 1 observação efetiva) para qualquer inferência
válida, em qualquer direção.

**Solução concreta:** em `82_robustez_cotistas_covid_consistencia.R`,
trocar o filtro da linha 74 (e o equivalente para h=12) para excluir
qualquer `ym` cuja janela `[ym+1, ym+h]` tenha interseção não-vazia com
`202002:202012`, não só o `ym` em si. Reportar o n resultante e, se cair
abaixo de ~8-10 meses efetivos (considerando a sobreposição), marcar o
teste como "amostra insuficiente para conclusão", em vez de reportar um
t-stat. Atualizar `LOG_CANDIDATOS.md` linhas 2526-2528 (a frase sobre h=12
"sem COVID" precisa ser removida ou fortemente requalificada).

---

## Achado 3 (IMPORTANTE): ML walk-forward (scripts 85-86) usa rótulos (labels) que ainda não existiam na data nominal de cada refit, para h=3 e h=6 — o comentário "sem vazamento" é impreciso

**Onde:** `85_ml_walkforward.R`, linha 95: `treino_disp <- m[ym < rp]  # SO' dado estritamente anterior -- sem vazamento`.
Mesmo padrão em `86_ml_walkforward_robustez.R`, linha 49.

**Descrição:** o filtro `ym < rp` restringe corretamente o **mês da
feature** de cada linha de treino a ser anterior ao ponto de refit `rp`.
Mas a coluna de treino `retorno` (o alvo/label) já foi mesclada
ANTES desse filtro (linha 74: `m <- merge(m, precos[...], by.x=c("ticker","ym_ret")...)`,
onde `ym_ret = addm(ym, h)`). Ou seja, a linha de treino com `ym = rp-1`
carrega como label o retorno do mês `rp-1+h`. Para h=3 e h=6, esse mês é
posterior ao próprio `rp` (a "data" nominal do refit) — na prática, o
modelo é treinado, no refit nominalmente feito "em julho/2020", usando
como alvo conhecido o retorno realizado até "dezembro/2020" (para h=6),
informação que um investidor real não teria em julho/2020. Isso não
contamina as LINHAS de teste em si (features e alvos do conjunto `alvo`
nunca se sobrepõem às linhas de treino), mas quebra a alegação de que o
walk-forward representa fielmente "o que seria conhecido em tempo real" no
momento de cada refit — o comentário de código ("sem vazamento") é
impreciso para h>1.

**Efeito prático:** não muda a conclusão substantiva (o candidato de ML já
foi rejeitado com R²_OOS negativo em h=3/h=6, então esse viés, se atuasse,
teria ajudado o modelo, não atrapalhado — e mesmo assim ele falhou). Mas é
uma inconsistência real entre o que o código alega fazer e o que
efetivamente faz, que deveria ser corrigida antes de reutilizar esse
padrão de walk-forward em qualquer candidato futuro que dependa de um
resultado positivo.

**Solução concreta:** em ambos os scripts, filtrar o treino por
`treino_disp <- m[ym + h < rp]` (ou equivalentemente `ym_ret < rp`), não só
`ym < rp`, garantindo que o rótulo usado em cada linha de treino já tenha
sido de fato realizado antes da data nominal do refit.

---

## Achado 4 (COSMÉTICO, verificado e descartado como bug real): chave de merge redundante no script 79

**Onde:** `79_construir_fragilidade_donos.R`, linha 62:
`merge(pp[, .(cod_fundo, ticker, ym, peso, aum_prev)], carac_fundo, by = c("cod_fundo","ym","aum_prev"))`.

**Descrição:** usar `aum_prev` como parte da chave de merge (além de
`cod_fundo` e `ym`) é redundante, já que `cod_fundo+ym` deveria bastar
para identificar unicamente uma linha de `carac_fundo`. Isso levantou a
hipótese de que poderia mascarar uma duplicação silenciosa (se
`carac_fundo` tivesse, por erro de dado, mais de uma linha por
fundo-mês). **Verificado empiricamente e descartado**: recomputando
`carac_fundo` a partir do painel bruto, `nrow(unique(carac_fundo[,.(cod_fundo,ym)])) == nrow(carac_fundo)`
(90.871 = 90.871, zero duplicatas). Também recomputei `frag_cotistas_medio`
inteiro do zero (agregação por `weighted.mean`, ponderada por
`peso*aum_prev`) e o resultado bate com o CSV salvo (`candidatos_79_fragilidade_donos.csv`)
com diferença máxima de 4,4e-14 (erro de ponto flutuante, não um erro
real) nas 14.149 linhas. **Não é um bug**, mas o código ficaria mais
claro e menos frágil a mudanças futuras sem a chave redundante.

**Solução concreta (opcional, só clareza):** trocar o `by` da linha 62
para `c("cod_fundo","ym")` e adicionar um `stopifnot(!anyDuplicated(carac_fundo[,.(cod_fundo,ym)]))`
logo após a linha 58, documentando explicitamente a premissa de
unicidade em vez de mascará-la via chave composta.

---

## Achado 5 (verificado, sem problema): origem e defasagem de `cotistas_prev`

**Onde:** `v2 OFICIAL/scripts/12_panel_predeterminado.R`, linhas 38-59.

**Descrição:** confirmado no código-fonte que `cotistas_prev` é `NR_COTST`
do Informe Diário da CVM (`inf_diario_fi_AAAAMM.csv`), extraído do
**último `DT_COMPTC` disponível dentro do mês (t-1)** (linha 58:
`ultimo <- ID[order(...), .SD[.N], by=.(CNPJ_FUNDO,ymk)]`), e mesclado ao
painel principal por `ym_prev = addm(ym,-1)` (linha 67). Ou seja,
`cotistas_prev` no mês `t` do painel é sempre o número de cotistas
observado ao final do mês `t-1` — corretamente predeterminado, sem
look-ahead. Esse ponto do escopo pedido está integralmente confirmado.

---

## Achados negativos (padrões catalogados 2× anteriormente na exploração — checados e NÃO encontrados nos scripts 70-87)

- **`t.test()` pooled tratando ticker-mês/par-mês como observação
  independente:** busca por `t\.test\(` em todos os 18 scripts do
  intervalo — **zero ocorrências**. Toda a inferência usa Fama-MacBeth
  (coeficiente/spread por mês, erro-padrão da série temporal).
- **Quintis com breakpoints fixos do treino aplicados ao teste:** busca
  por padrões de `quantile(...)` calculado sobre dado de treino e
  reaplicado — **zero ocorrências**. Todo corte de quintil/tercil usa
  `by = ym` (recorte mês a mês), inclusive nos scripts de robustez.
- **Grupos degenerados sem N mínimo reportado:** todos os scripts
  revisados (70, 71, 72, 73, 74, 76, 78, 80, 81, 82, 84, 85, 86, 87)
  reportam explicitamente o N mínimo por grupo-mês ou por par-mês antes
  de aceitar um resultado como válido.
- **Regressão pooled disfarçada de Fama-MacBeth (item 3 do escopo
  pedido):** confirmado por leitura de código que
  `81_diagnostico_cotistas_churn.R` (função `fm_multivariado`, linhas
  82-109) roda `lm()` **separadamente para cada mês** dentro de um loop
  (`for (mes in meses) { d <- teste[ym==mes]; fit <- lm(form, data=d) }`),
  não uma regressão única sobre o painel empilhado. É Fama-MacBeth de
  verdade — mas ver Achado 1: o erro-padrão daí em diante (média/dp da
  série de coeficientes mensais) sofre do mesmo problema de
  autocorrelação por janela sobreposta.

## Números centrais re-derivados independentemente (todos batem)

| Número no log | Valor no log | Valor recomputado do zero | Bate? |
|---|---|---|---|
| h=6, t (FM quintil, vol futura) | t=-5,07, p=0,00006 | t=-5,068, p=0,0000588 | Sim |
| h=12, t (FM quintil, vol futura) | t=-4,73, p=0,00022 | t=-4,734, p=0,000225 | Sim |
| Correlação frag_cotistas × hhi_posse | -0,068 | -0,0682 | Sim |
| Correlação frag_cotistas × log_valor_total | -0,014 | -0,0137 | Sim |
| % meses direção esperada, h=6 | 85,7% | 85,71% | Sim |
| Excluindo 2 meses extremos, h=6 | t=-4,88, p=0,00012 | t=-4,876, p=0,000122 | Sim |
| Excluindo COVID (como o script faz), h=6 | t=-3,07, p=0,013 | t=-3,070, p=0,0133 | Sim |
| Excluindo COVID (como o script faz), h=12 | t=-4,03, p=0,010 | t=-4,029, p=0,0100 | Sim |

Toda a aritmética do log está correta e é 100% reprodutível a partir dos
dados brutos — **não há nenhum erro de cálculo ou de transcrição em
nenhum número reportado**. O problema não é aritmético, é de desenho do
teste estatístico (erro-padrão sem correção de autocorrelação, Achado 1)
e de precisão na definição de "excluir COVID" (Achado 2).

---

## Resumo (≤200 palavras)

Revisei os 18 scripts (70-87) e re-derivei em R, do zero, os números
centrais do candidato E3 (cotistas dos fundos donos → volatilidade
futura). A aritmética do log está toda correta — cada número reproduzido
bateu com precisão de várias casas decimais. `cotistas_prev` está
corretamente defasado (t-1), a agregação por posição no script 79 não tem
look-ahead nem duplicação, e o controle multivariado do script 81 é
Fama-MacBeth genuíno, mês a mês. Mas achei um problema estatístico real e
não-cosmético: os testes de volatilidade (h=3/6/12) usam janelas de
resultado sobrepostas (autocorrelação lag-1 de 0,37 a 0,81) sem nenhuma
correção HAC — técnica que a própria exploração já usou em outro
candidato. Corrigindo com Newey-West (lag=h-1), a alegação central "h=6
bate Bonferroni isoladamente" (p=0,00006) sobe para p=0,0029 — comodamente
significativo a 5%, mas **~30× longe de Bonferroni**. O teste de robustez
"sem COVID" também tem um viés de janela (achado 2) que, uma vez
corrigido, ainda sustenta h=6 mas invalida completamente a alegação de
robustez em h=12.

**Conclusão: o achado de cotistas sobrevive, mas rebaixado.** Continua
sendo um sinal direcionalmente consistente e estatisticamente real de
previsibilidade de volatilidade (não de retorno) em h=6, mas deixa de ser
"o achado mais forte que bate Bonferroni isoladamente" — essa alegação
específica não resiste à correção estatística padrão que faltou.
