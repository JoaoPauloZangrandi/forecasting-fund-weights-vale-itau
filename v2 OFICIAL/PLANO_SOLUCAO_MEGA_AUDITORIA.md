# Plano de solução — mega-auditoria de 15/08/2026

Consolidação de 6 frentes de auditoria adversarial (5 agentes em paralelo +
verificação própria), cobrindo o pipeline oficial do TCC inteiro e os ~90
candidatos de `exploracao_sinais/`. Relatórios completos, com file:linha e
números re-derivados independentemente, estão em:

- `v2 OFICIAL/AUDITORIA_pipeline_lambda_pares.md` (Etapa 2/3, robô caça-replicantes)
- `v2 OFICIAL/exploracao_sinais/AUDITORIA_candidatos_01_27.md`
- `v2 OFICIAL/exploracao_sinais/AUDITORIA_candidatos_28_45.md`
- `v2 OFICIAL/exploracao_sinais/AUDITORIA_candidatos_55_69.md`
- `v2 OFICIAL/exploracao_sinais/AUDITORIA_candidatos_70_90.md`
- Este documento (síntese + plano de ação, triado por urgência)

## Resumo executivo

**A boa notícia**: nenhum achado muda a conclusão de nenhuma parte do
trabalho de forma dramática. O achado central que vai virar a estratégia do
Desafio Quant AI (HHI de posse → volatilidade futura) sobrevive à
re-auditoria mais profunda — números batem, sem look-ahead, sem grupos
degenerados. A conclusão "nulo" da busca por sinal de retorno também
sobrevive (e em alguns casos fica ainda mais robusta).

**A notícia que exige ação real**: as janelas de `h` meses sobrepostas
usadas em TODO teste de previsão de volatilidade desta exploração inflam o
$t$-estatístico se não corrigidas (autocorrelação induzida, não
autocorrelação espúria) — isso já foi confirmado reduzindo materialmente a
força dos nossos DOIS achados mais fortes (HHI e número de cotistas). E o
pipeline oficial do TCC (cadeia de pares replicantes, Seção 6) tem 3
achados críticos genuínos, embora nenhum inverta a conclusão final de
"nulo" do TCC.

---

## TIER 0 — Afeta diretamente a estratégia do Desafio Quant AI, resolver ANTES do relatório

### 0.1 [CRÍTICO] Correção de Newey-West para autocorrelação de janela sobreposta

**Achado**: `vol_futura` (h meses à frente) compartilha até `h-1` meses
entre observações mensais consecutivas — isso autocorrelaciona os
coeficientes de Fama-MacBeth (medido empiricamente: 0,35 a 0,90 de
autocorrelação lag-1, dependendo do horizonte). O teste "ingênuo" (que
usávamos até agora) trata os meses como i.i.d. e infla o $t$.

**Verificação feita agora** (script `91_verificacao_urgente_newey_west_hhi.R`,
HHI controlando por vol. passada — o teste mais rigoroso):

| Horizonte | $t$ ingênuo | $t$ Newey-West | Situação |
|---|---|---|---|
| h=6 | 1,81 (p=0,085) | 1,08 (p=0,29) | deixa de ser significativo |
| h=12 | 3,05 (p=0,008) | 2,21 (p=0,042) | continua significativo a 5%, mas muito mais fraco |

Mesmo padrão já confirmado independentemente pelo achado de cotistas (E3):
$p=0{,}00006 \to p=0{,}0029$ com a mesma correção.

**Ressalva importante**: com só 17 meses de teste em h=12 e a correção
exigindo defasagem 11, a estimativa de Newey-West fica instável nesse
tamanho de amostra (o teste univariado deu um resultado suspeito na
direção oposta). **Isso não é motivo pra ignorar a correção — é motivo pra
tratar o número pós-correção como uma faixa, não um valor exato, até termos
mais meses de teste.**

**Solução**:
1. Adotar Newey-West (lag=h-1) como prática obrigatória, a partir de agora,
   em TODO teste de Fama-MacBeth cuja variável dependente usa janela de `h`
   meses sobrepostos (volatilidade futura, retorno acumulado de vários
   meses, etc.) — vira a 4ª regra inegociável da metodologia, ao lado das
   3 já estabelecidas.
2. Quando os dados 2016-2026 chegarem: rerodar a verificação com ~60+ meses
   de teste em vez de 17 — a correção fica muito mais confiável nos dois
   sentidos, e só aí dá pra saber com confiança real se HHI→vol sobrevive
   ou não como o achado mais forte.
3. **Para o relatório do Desafio Quant AI**: reportar os números COM
   Newey-West desde já (não os ingênuos), e ser explícito na seção de
   backtest sobre por que essa correção é necessária (mostra rigor, é
   literalmente o tipo de coisa que o critério "mitigação de vieses"
   recompensa).

### 0.2 [JÁ DOCUMENTADO, reforçando aqui] Correções do script 32

Já registradas em `PLANO_EXPANSAO_2021_2026.md`, seção "ETAPA 3.5":
descompasso de horizonte (Desenho B reforma o tilt todo mês, mas o sinal só
funciona em h=12) e calibração com informação do período de teste inteiro
(Desenho A, mesmo problema que Cederburg et al. 2020 documentou). Resolver
junto com o rerun da base nova.

### 0.3 [IMPORTANTE] `hhi_posse` pré-calculado diverge da reconstrução fresca

Achado do agente de #55-69: o arquivo `fit_ativo_mes.csv` (usado pelo
script 55) tem uma versão de `hhi_posse` que diverge em centenas de fundos
da reconstrução feita do zero nos scripts 63/65/67/68/69 — possível bug de
agregação de ticker num arquivo herdado. **Ação**: antes de reusar
`fit_ativo_mes.csv` em qualquer script novo, confirmar qual das duas
versões está certa (provavelmente a reconstrução fresca, mais escrutinada)
e não usar a divergente sem investigar.

---

## TIER 1 — Robustez geral da exploração de sinais, prioridade moderada

### 1.1 [IMPORTANTE] Winsorização com informação do período de teste

Achado em dois agentes independentes (#1-27 e #55-69): pelo menos 9
scripts (`04`-`27`, `55`, `65`, `67`, `68`) calculam limites de
winsorização (1º/99º percentil) sobre treino+teste juntos, não só treino —
viés de look-ahead pequeno mas real. **Solução**: calcular limites só no
treino, aplicar fixo ao teste, mesmo princípio já usado pra `lambda` e
quintis. Baixo risco de mudar conclusões (a maioria desses candidatos já é
nula), mas deve virar regra padrão daqui pra frente.

### 1.2 [IMPORTANTE] Helper `fama_macbeth()` com quintil fixo copiado em ~20 scripts

Confirma o padrão já conhecido (mesmo bug do CIO original, já corrigido
nos scripts 34-38): a função reusada em `04`-`27` corta quintil uma vez no
treino. Como a maioria desses candidatos já foi rejeitada de qualquer
forma, o risco é baixo, mas ao rerodar tudo com a base nova (pedido já
registrado no `PLANO_EXPANSAO_2021_2026.md`), usar a versão corrigida
(recorte mensal) em vez de reaproveitar o helper antigo cegamente.

### 1.3 [IMPORTANTE] Script 82: "excluir COVID" em h=12 não é confiável

Ao purgar a COVID corretamente (mês do sinal E toda a janela de resultado,
não só o mês do sinal), sobram só 5 meses fortemente sobrepostos em
2021 — produz um $t$ implausível (-17,3), sinal de artefato de amostra
pequena, não confirmação. **Solução**: remover a alegação "sobrevive
excluindo COVID" especificamente para h=12 do achado de cotistas até haver
dado suficiente pós-2021 pra testar isso de verdade.

### 1.4 [COSMÉTICO] Candidato #33 (52wk-high × HHI) usa metodologia de quintil já sabida como defeituosa

O "quase interessante" (p=0,008) evapora pra p=0,57 com a correção. Já era
um candidato rejeitado de qualquer forma — só corrigir a entrada no
`LOG_CANDIDATOS.md` pra não citar o número errado se alguém reler depois.

---

## TIER 2 — Pipeline oficial do TCC (Seção 6, robô caça-replicantes) — importante pra integridade acadêmica, NÃO bloqueia o Desafio Quant AI de amanhã

Esta cadeia (scripts 61-72) não é usada na estratégia do Desafio Quant AI
(decidimos usar HHI/fragilidade, não o robô caça-replicantes) — então nada
aqui impede a entrega de amanhã. Mas são achados reais sobre o TCC em si,
vale decidir com calma, depois do desafio, se cabe correção antes da
entrega final do TCC ou se vira nota de limitação conhecida.

1. **[CRÍTICO] "Eco do líder" (scripts 66/67) usa informação de `t+1`
   rotulada como `t`** — o `u` do líder só é computável depois que o mês
   seguinte aconteceu, mas é combinado com o `d` do seguidor no mesmo `ym`,
   não defasado como o script 63 (que define quem é líder) já faz
   corretamente. Não inverte a conclusão de nulo (reforça), mas invalida a
   alegação de ter testado um sinal "operável em tempo real". Solução
   concreta (troca de chave de merge) já especificada no relatório completo.
2. **[CRÍTICO] Correção de Benjamini-Hochberg (script 64) usa uma premissa
   falsa** — assume que o ranking de p-valor dentro do pré-filtro
   (`|corr|≥0,6`) é o ranking verdadeiro entre os 31,9M testes, mas isso só
   valeria se `n` (meses de sobreposição) fosse parecido entre pares — não
   é (varia de 12 a 58). Confirmado numericamente: 58,1% dos 1.248.772
   pares "significativos" são vulneráveis a essa falha. Duas soluções
   (uma rigorosa, uma pragmática) já especificadas no relatório completo.
3. **[CRÍTICO] Denominador do BH (31.907.281, citado no TCC) não tem script
   que o produza**, e o arquivo é anterior ao corte de qualidade de
   12/08/2026 que invalidou o resto do pipeline antigo. Solução: gerar esse
   número dentro do próprio script 63, nunca mais como arquivo solto.
4. **[IMPORTANTE] Teste decisivo (scripts 70/71) é pooled, não
   Fama-MacBeth** — mesmo padrão de erro já visto em outros sinais deste
   projeto. Refazendo por mês, 2 de 4 células da Tabela `tab:teste-decisivo`
   mudam de conclusão (uma enfraquece bastante, outra fica limítrofe). Não
   muda a conclusão final do TCC, mas a frase "estatisticamente detectável"
   no texto atual precisa ser qualificada.
5. **[IMPORTANTE] Seleção de pares só usa correlação contemporânea**,
   nunca defasada, como critério de entrada — empobrece a busca (descarta
   replicantes genuínos com defasagem real) mas não infla resultado.
6. **[IMPORTANTE] Cadeia de pares usa λ de amostra inteira (0,0690), não o
   λ fora-da-amostra (0,0715) que a Seção 4/5 já usa corretamente** —
   inconsistência de rigor entre seções do mesmo TCC.
7-8. **[COSMÉTICO]** Dois comentários desatualizados mais (scripts 63 e 64),
   mesmo padrão já visto no script 100 — não afetam cálculo, só confundem
   quem lê.

**Recomendação**: levar isso ao orientador como parte da revisão do TCC,
não como bloqueio de prazo — são achados que fortalecem a tese (mostram
rigor na auto-crítica) mais do que a enfraquecem, já que a conclusão final
não muda.

---

## Ordem de execução recomendada

1. **Agora, antes do relatório Quant AI**: adotar Newey-West como regra
   padrão (0.1), decidir a versão final da estratégia (HHI com correção
   NW, números conservadores) — usar os $t$/$p$ corrigidos no relatório,
   não os ingênuos.
2. **Quando a base 2016-2026 chegar** (junto com o rerun completo já
   planejado): reaplicar Newey-West com amostra maior (resolve a
   instabilidade do item 0.1), corrigir o script 32 (0.2), corrigir
   winsorização (1.1) e helper de quintil (1.2) nos candidatos que forem
   rerodados.
3. **Depois do Desafio Quant AI, com calma**: decidir com o orientador o
   que fazer com os achados do Tier 2 (pipeline oficial do TCC) — não é
   urgente, mas não deve ser esquecido.
