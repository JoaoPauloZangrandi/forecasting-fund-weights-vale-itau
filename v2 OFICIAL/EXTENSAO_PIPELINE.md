# Pipeline da extensão (scripts 125–135)

Cadeia nova, **inteiramente downstream do script 99**. Não toca a Etapa 1 e não exige rerun de
`00_RUN_ALL_apos_etapa1.R`. Amostra 2016-2021, horizontes h=1 (principal) e h=3 (replicação).

Documento de método: `PRE_REGISTRO_EXTENSAO.md`, commitado em `fbb244f` antes de qualquer
regressão, com duas emendas datadas (E1 vazamento em S2, E2 colinearidades da Parada 4).

## Ordem

| # | Script | Lê | Grava | Serve para |
|---|---|---|---|---|
| 125 | `125_diagnostico_amostra_extensao.R` | h1, painel, cache Anbima, checagem soma_peso | `diag_amostra_extensao.csv`, `diag_fundos_extensao.csv` | Parada 1: a amostra aguenta o desenho? |
| 126 | `126_var_decomp_nivel_fundo.R` | h1, painel | `var_fundo_entre_dentro.csv`, `erro_por_fundo_teste.csv`, `replica_dados_script95.csv` | Parada 2: entre/dentro, confiabilidade, LOO |
| 127 | `127_pre_registro_congelado.R` | pré-registro, `diag_fundos_extensao.csv` | `split_fundos_dev_conf.csv` | Congela o hold-out com o hash do commit |
| 128 | `128_construcao_turnover.R` | `erro_e_multiativo.csv`, preços, painel | `turnover_fundo_mes.csv`, `_treino.csv`, `_diagnostico.csv` | Parada 3: turnover é distinto do HHI? |
| 129 | `129_variaveis_novas_fundo.R` | painel, h1, cache Anbima, turnover | `caracteristicas_fundo_treino.csv` (+`_S2`), `colinearidade_*.csv` | Parada 4: colinearidade antes de estimar |
| 130 | `130_regressao_fundo_dev.R` | características, split | `reg_fundo_dev_*.csv` | Exploração, só no lado dev |
| 131 | `131_regressao_fundo_confirmacao.R` | idem | `reg_fundo_conf_*.csv`, `mandato_tabela_2x2.csv` | **Roda uma vez.** Resultado do artigo |
| 132 | `132_decomposicao_erro_shapley.R` | h1, painel | `decomp_shapley_erro.csv`, `decomp_var_componentes.csv` | Shapley em ativo/mês/fundo/gestora |
| 133 | `133_decomposicao_estrutural_lambda.R` | `erro_e_multiativo.csv`, h1 | `decomp_estrutural_*.csv` | Identidade erro = alvo + inovação |
| 134 | `134_replicacao_h3_e_anual.R` | h3, h1, características | `replicacao_h3_anual*.csv` | Replicação por horizonte e por ano |
| 135 | `135_tabelas_figuras_paper.R` | todos os CSV acima | `tabelas_paper_ext.tex`, `figuras/fig_ext_*.pdf` | Só monta saída |

`_funcoes_extensao.R` é biblioteca compartilhada entre 130 e 131 — as duas amostras passam pelo
**mesmo** código, senão o hold-out perderia o sentido.

## Armadilhas encontradas e resolvidas (não repetir)

1. **`fixest` remove singletons por padrão.** No script 132 isso fazia cada subconjunto de efeitos
   fixos rodar numa amostra diferente, e o R² chegava a *cair* ao acrescentar um fator — impossível
   em OLS. Resolvido com `fixef.rm = "none"`.
2. **Poisson não é monótono em R² de resposta.** Inviabiliza Shapley. A dependente primária virou
   `log(e²+c)`, a forma aditiva do mesmo modelo multiplicativo; Poisson ficou como robustez com
   pseudo-R² de deviance.
3. **Indexar vetor nomeado por `""` devolve NA em R.** O conjunto vazio do Shapley precisa de
   chave não vazia (usei o prefixo `S:`).
4. **`model.matrix` mantém colunas aliased; `coeftest` não.** Em A3 (within-gestora) isso quebrava
   o laço de coeficientes. Resolvido filtrando por `names(coef(fit))[!is.na(...)]`.
5. **O script 120 não pode ser reaproveitado para turnover.** Faz merge interno entre t-1 e t, o que
   descarta **480.819 saídas totais de posição**, e usa `G = 1 + retorno_fundo`, ignorando o fluxo.
   O script 128 usa frame balanceado por união e medida self-contained dentro da sleeve.
6. **`n_ativos` por fundo-mês e células distintas ao longo do período não são comparáveis.** A
   primeira versão do 125 comparava as duas e devolvia "filtrado > bruto", que é impossível.

## Verificações que precisam continuar passando

- **126**: reproduz R²=0,4963 e ajustado=0,4047 do script 95. Se não bater, parar.
- **128**: `TURN ∈ [0,1]` sem exceção; CV de `valor_mil/peso` dentro do fundo-mês = 0.
- **132**: soma dos valores de Shapley = R² total (propriedade de eficiência).
- **133**: identidade algébrica a < 1e-9; `cor(erro reconstruído, erro_oos do script 99)` ≈ 1.
- **135**: nenhum número novo é calculado; tudo vem de CSV.
