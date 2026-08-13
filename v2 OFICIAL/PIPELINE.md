# Pipeline completo: da Etapa 1 até o TCC_finalV2.tex

Este arquivo existe porque em 12/08/2026 uma mudança no corte de qualidade de
dado (`soma_peso` > 105% → > 150%, script 100) foi propagada só para a Seção 6
(trilha `teste_estrategia`). A trilha principal (Seção 4/5,
`painel_multiativo_final.csv`) e o próprio `TCC_finalV2.tex` ficaram
descrevendo dados que o pipeline já não produzia mais, por uma sessão inteira,
sem que ninguém percebesse — porque não existia uma lista central do que
depende de quê. Este documento + `00_RUN_ALL_apos_etapa1.R` resolvem isso.

**Regra geral:** qualquer mudança no início da Etapa 1 (corte de qualidade de
dado, definição de característica, fórmula do HHI, filtro de universo) invalida
**tudo** abaixo dela no grafo. Não existe mudança "só na Seção 6" ou "só num
gráfico" — desde 12/08/2026 a Seção 4/5 e a Seção 6 correm sobre a mesma base
(`painel_multiativo_final.csv` == `painel_universo_completo_final.csv`).

## Como usar

1. Editar o que precisa mudar (ex.: `LIM` no script 100).
2. Rodar `00_RUN_ALL_apos_etapa1.R` inteiro (roda tudo na ordem certa,
   salva um log fresco `_log_NN_novo.txt` por script).
3. **Parar no aviso do meio do script** (lambda hardcoded nos scripts
   63/66/67 — ver seção "Armadilhas" abaixo) antes de deixar continuar.
4. Usar a tabela "Script → onde no TCC" abaixo para conferir, um por um,
   todos os números citados no `.tex` contra os logs frescos.
5. Recompilar (`latexmk -pdf TCC_finalV2.tex`) e conferir página/contagem
   antes de comitar.

## Grafo de dependência (ordem de execução)

```
100 (limpa soma_peso)
 └─ 58 (merge → painel_universo_completo_final.csv)
     └─ [resync manual → painel_multiativo_final.csv]
         ├─ 103 (diagnóstico de atrição, Seção "Dados") — lento (lê SH brutos)
         ├─ 99 (motor Etapa1+3, Seção 4/5) ─┬─ 98  (stats erro e/u, Seção 5)
         │                                   ├─ 82  (janela expansiva, Seção 5)
         │                                   ├─ 93  (tabelas 9/10 LaTeX, Seção 5)
         │                                   └─ 74,75,77,78,80,84,85,86,87,88,92,95,45
         │                                      (figuras/tabelas Seção 5 — todas
         │                                       leem etapa3_multiativo_* e/ou
         │                                       painel_multiativo_final.csv)
         │
         └─ 101 (motor Etapa1, Seção 6/funil) ─┬─ 61 (Etapa 2 λ, Seção 6) ─┐
                                                 │   ⚠ λ hardcoded em       │
                                                 │     63/66/67 — CONFERIR  │
                                                 ├─ 62 (Etapa 3 c/ correção │
                                                 │      mecânica, Seção 6)  │
                                                 └────────────────────────┬┘
                                                                          │
        63 → 64 → 65 → 66 → 67 → 68 → 69 (cadeia pares/sinal, SEQUENCIAL)
                                     │
        70 → 71 → 72 (teste decisivo, SEQUENCIAL, usa 62 e 69)
```

Scripts 51–57 (construção do painel bruto a partir de CONS/SH/Informe Diário)
**não** entram nesse rerun — só são necessários se os dados brutos da CVM
mudarem, não para mudanças de Etapa 1/corte de qualidade. Se a mudança for na
**definição de uma característica** (não só no corte de qualidade), os scripts
52 (features)/53 (beta) também precisam rodar de novo antes do 58.

## Como esta lista foi auditada (e como reauditar se desconfiar que falta algo)

Rodar, na raiz do projeto:
```
grep -lE "painel_multiativo_final\.csv|painel_universo_completo_final\.csv|painel_multiativo_direto_completo\.csv" "v2 OFICIAL/scripts"/*.R
```
Isso retorna ~60 arquivos (checado em 13/08/2026). A maioria é **superado**,
com aviso explícito no próprio cabeçalho do script (ex.: *"O script
99_pipeline_hhi_lag_etapa1.R é quem produz oficialmente X hoje... mantido só
por referência histórica"*) — são versões antigas (5 características sem
HHI_resto, ou ancoradas em VALE3/Itaú, da estrutura de documento anterior à
reescrita completa de 12/08) que sobrescrevem os MESMOS nomes de arquivo dos
scripts atuais se rodadas por engano. **Não rodar esses.**

Para qualquer script que aparecer nessa busca e **não** estiver na lista deste
documento nem tiver aviso de "superado" no cabeçalho: (1) ver que arquivo ele
produz (`grep "pdf(file.path\|fwrite(" no script`), (2) conferir se esse nome
aparece em algum `\includegraphics` ou `Fonte: script` do `TCC_finalV2.tex`.
Se sim, **falta nesta lista — adicionar** (foi assim que o script 103 foi
achado em 13/08/2026, faltando desde a rodada de 12/08). Se não aparecer no
`.tex`, é rascunho (prefixo `_`, ex. `_check_*.R`) ou um achado ainda não
incorporado ao documento — ver seção seguinte.

## Scripts com dado sensível ao início da Etapa 1 mas que NÃO estão no pipeline

**`105_fluxo_induzido_agregado.R`** — análise de Flow-Induced Trading (Lou
2012), pedida pelo João em 12/08/2026 como alternativa ao robô
caça-replicantes (que foi testado e rejeitado). Lê `painel_multiativo_final.csv`
diretamente, então é tão sensível ao corte de `soma_peso` quanto qualquer
script desta lista — mas **nunca foi citada em nenhum `Fonte:` do
`TCC_finalV2.tex`**, então não faz parte do pipeline obrigatório. Ficou com
dado desatualizado (base pré-150%) até pelo menos 13/08/2026. Se o achado for
incorporado ao texto no futuro, adicionar este script à Etapa C/D do
`00_RUN_ALL_apos_etapa1.R` nesse momento.

## Armadilhas conhecidas (cada uma já causou um erro real)

1. **Script 100 tem guarda de backup.** Só copia pro `.bak` se o `.bak` ainda
   não existir. Mudar `LIM` e rodar de novo sem restaurar o painel bruto do
   backup primeiro aplica o novo corte **em cima** do painel já cortado pelo
   corte antigo — errado silenciosamente se o novo corte for mais permissivo.
   `00_RUN_ALL_apos_etapa1.R` já restaura antes de rodar.
2. **`painel_multiativo_final.csv` não se atualiza sozinho.** É uma cópia
   manual de `painel_universo_completo_final.csv` (mesmo schema de 18
   colunas) — se você só rodar a cadeia 100→58, a Seção 4/5 continua com
   dado velho. Sempre re-sincronizar depois do 58.
3. **λ hardcoded em `63_pares_replicantes.R`, `66_beta_pares_lideranca.R`,
   `67_previsao_ajustada_seguidor.R`** (`lam <- 0.XXXX`, comentário datado
   ao lado). Esses 3 scripts não leem λ de nenhum CSV — se a Etapa 2 mudar
   (script 61), esses 3 precisam de edição manual ANTES de rodar, senão
   rodam com o λ errado sem erro nenhum.
4. **`90_pipeline_limpa_soma_peso.R` é de uma trilha morta** (scripts 17–29,
   marcados SUPERADOS desde 12/08). Não confundir com o script 100 — são
   dois scripts diferentes que limpam `soma_peso` em dois painéis diferentes
   por razões históricas. Só o 100 importa pro TCC_finalV2.tex atual.
5. **Muitos scripts só imprimem no console, não salvam CSV com o número
   final** (ex.: correlações do 87/88, coeficientes do 95, λ do 61/62).
   Sem `00_RUN_ALL_apos_etapa1.R` salvando log, esses números se perdem
   quando o R fecha — foi exatamente o que aconteceu em 12/08 (a sessão
   rodou tudo, mas ninguém guardou o output pra atualizar o `.tex` depois).
6. **`painel_multiativo_final.csv` reportado como "já limpo de
   soma_peso>105%"** no comentário do script 99 (linha ~27, `cat(...)`) —
   é só um texto de log, não afeta o cálculo, mas fica desatualizado depois
   de qualquer mudança de corte. Cosmético, mas confunde se você ler o log
   sem saber disso.

## Script → onde no TCC_finalV2.tex

| Script | Produz | Onde no `.tex` | O que conferir |
|---|---|---|---|
| 100 | painel limpo | linha ~62 (funil, texto) | corte %, fundo-mês/fundos excluídos |
| 58 | `painel_universo_completo_final.csv` | linha ~99 (`10.818.395 linhas`), Tabela funil linha 76–79 | linhas antes dos filtros, "+5 características" |
| 103 | diagnóstico de atrição | parágrafo logo após a Tabela do funil (~86–91) | fundos excluídos, % sem 4 características, % sem beta (jovem demais vs. outro motivo) |
| 99 | theta/erro_e/etapa3_multiativo_* | Tabela `tab:amostra` (~138), parágrafo Legacy Capital (~242), Tabela `tab:theta`/`tab:ape` (~259–298), Tabela `tab:erro-e` (~300), Tabela `tab:oos-agregado` (~354) | obs/fundos/ativos/meses, medianas θ/APE, % significância HHI, RMSE por horizonte, λ_h |
| 101 | theta/peso_pred (universo completo) | Tabela funil linha 79 (`+HHI_resto`) | fundos/ativos pós HHI-lag |
| 98 | erro_e_comD/erro_u | Tabela `tab:erro-u` (~330), linha `\widehat\lambda` (~327) | estatísticas erro u, λ pooled amostra completa |
| 82 | janela expansiva | Tabela `tab:janela_expansiva` (~374) | razão RMSE fixo/expansiva, Spearman |
| 61 | ajuste_parcial_universo_completo_h1 | Tabela de λ Etapa 2 (~746, Seção 6) | λ bruto/corrigido, R² |
| 62 | etapa3_universo_completo_* | Tabela (~763, Seção 6), footnote VALE3 (~772) | λ_h bruto/corrigido, margem mediana, % vence ingênua |
| 93 | linhas LaTeX prontas | Tabelas `tab:rmse-gestora`/`tab:margem-gestora` (~561–679) | copiar/colar direto do log, não recalcular à mão |
| 74 | 2 figuras dinâmica erro | Figuras `fig_dinamica_erro_*` (~391–403), legenda "3 gestoras mais difíceis" | nomes/ordem das gestoras extremas |
| 75 | fig margem dinâmica | Figura (~406), legenda | margem média h=1/h=3 |
| 77 | fig viés dinâmico | Figura (~426), legenda | viés extremos por gestora |
| 78 | fig beta vs erro | Figura (~540), legenda | N fundos, R², coef |
| 80 | fig ranking horizontes | Figura (~548), legenda | Spearman entre horizontes |
| 84 | fig persistência viés | Parágrafo (~434), Figura (~439) | correlação, R², N gestoras |
| 85 | (sem figura) | Parágrafo correção de viés (~445) | RMSE antes/depois, % melhora |
| 86 | fig decomposição + persistência dp | Parágrafos (~448, ~459), Figuras (~453, ~465) | % viés²/RMSE², correlação dp |
| 87 | fig explica variância | Parágrafo (~476), Figura (~493) | correlações beta/tamanho/HHI |
| 95 | (sem figura) | Tabela `tab:regressao-variancia` (~505) | coeficientes, t, R² |
| 88 | fig concentração mês-a-mês | Parágrafo (~526), Figura (~532) | correlações A/B/C |
| 92 | fig trajetória fundo | Figura (~412), parágrafo Squadra (~689) | fundo/ativo identificado, faixa de peso |
| 45 | fig multihorizonte scatter | Figura (~683) | (sem número específico na legenda) |
| 63→65 | pares_replicantes_significativos | Tabela `tab:pares` (~816–824) | contagens de pares, % mesma/diferente gestora, liderança |
| 66→67 | previsao_ajustada_seguidor | Tabela RMSE com/sem ajuste (~863) | N pares/obs, RMSE, % melhora |
| 68→69 | sinal_robo_net_mercado + figuras 1–3 | Parágrafo Itaú/boutiques (~835), Figura 3 legenda (~871) | fundos/taxa por par de gestora, maior compra/venda |
| 70→71→72 | teste_fluxo_prediz_retorno + figuras 4–5 | Tabela `tab:teste-decisivo` (~907–918), parágrafo (~935), Figuras 4/5 (~921–933) | coef/p-valor 4 células, R², diferença Q5-Q1 |

## O que este pipeline **não** automatiza

A edição do texto do `.tex` continua manual — os números estão hardcoded em
prosa/tabelas, não gerados via `\input{}`. `00_RUN_ALL_apos_etapa1.R` garante
que nenhum *script* seja esquecido; a tabela acima garante que nenhum *lugar
no texto* seja esquecido. As duas partes são necessárias.
