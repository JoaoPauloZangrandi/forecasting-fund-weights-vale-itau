# =============================================================================
# 64_significancia_pares_replicantes.R  (v2 OFICIAL -- teste_estrategia)
#
# FASE 4, passo 2: calcula p-valor (teste t padrao de significancia de
# correlacao) pros pares que sobreviveram ao pre-filtro grosso (R/63 +
# limpeza posterior: |corr_contemp|>0,6, reduzindo de 87M pros 8M mais
# promissores -- pre-filtro necessario porque com 87M testes, so' teria
# chance de sobreviver a Benjamini-Hochberg um |r| bem alto de qualquer
# jeito; nao descarta nada que teria sobrevivido, so poupa memoria/tempo).
#
# BH aplicado GLOBALMENTE (todos os pares pre-filtrados de uma vez, nao
# ativo por ativo) -- e' o numero de testes REAL (87.072.544, nao os 8M
# que sobraram do pre-filtro) que importa pro denominador do BH, senao a
# correcao fica fraca demais.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")

P <- fread(file.path(REPO, "v2 OFICIAL/data/pares_replicantes_bruto.csv"))
cat("Pares pre-filtrados (|corr_contemp|>=0,6, ja SEM posicao-poeira):", nrow(P), "\n")

# numero real de testes feitos no R/63 (pos filtro de posicao-poeira + cobertura
# minima de 12 meses) -- recalculado a parte porque o R/63 filtra e acumula so
# os pares com |corr|>=0,6, nao teria como contar o total de dentro dele sem
# refazer o trabalho
N_TESTES_TOTAL <- fread(file.path(REPO, "v2 OFICIAL/data/n_testes_total_fase4.csv"))$n_testes_total

pval_corr <- function(r, n) {
  t <- r * sqrt(n - 2) / sqrt(1 - r^2)
  2 * pt(-abs(t), df = n - 2)
}
P[, pval_contemp := pval_corr(corr_contemp, n_meses_contemp)]

# BH manual: como so temos os p-valores dos pares PRE-selecionados (os < 8M
# mais correlacionados), mas o denominador do BH usa o TOTAL de testes --
# construimos o vetor de p-valores "completo" na pratica assumindo que todo
# par fora do pre-filtro (com |r|<=0,6) tem p-valor > qualquer um dos aqui
# (verdade, pois correlacao mais fraca com n parecido sempre da p-valor
# maior) -- entao o rank desses pares DENTRO do conjunto completo e' o
# proprio rank deles aqui (sao os N_TESTES_TOTAL menores p-valores restantes
# vem TODOS de fora do pre-filtro). BH: p_adj = p * N_TESTES_TOTAL / rank.
setorder(P, pval_contemp)
P[, rank_pval := .I]
P[, p_bh := pmin(1, pval_contemp * N_TESTES_TOTAL / rank_pval)]
# BH e' monotonico (p_adj nao pode diminuir conforme o rank aumenta) --
# aplica o "min acumulado de tras pra frente", forma padrao do algoritmo
P[, p_bh := rev(cummin(rev(p_bh)))]

sig <- P[p_bh < 0.05]
cat("\nPares com p_bh < 0,05 (contemporaneo):", nrow(sig), "de", nrow(P), "pre-filtrados\n")
cat("Maior |corr_contemp| que NAO foi significativo:",
    round(max(abs(P[p_bh>=0.05]$corr_contemp)), 3), "\n")
cat("Menor |corr_contemp| que FOI significativo:",
    round(min(abs(sig$corr_contemp)), 3), "\n")

cat("\n===== Distribuicao mesma-gestora vs gestora-diferente (entre significativos) =====\n")
print(sig[, .N, by = mesma_gestora])
cat("Entre os", nrow(P), "pre-filtrados (antes de exigir significancia):\n")
print(P[, .N, by = mesma_gestora])

cat("\n===== Top 15 pares mais correlacionados (contemporaneo, significativos) =====\n")
top <- sig[order(-abs(corr_contemp))][1:15]
print(top[, .(ativo, fundo_a, fundo_b, n_meses_contemp, corr_contemp = round(corr_contemp,3),
               gestora_a, gestora_b, mesma_gestora, p_bh = signif(p_bh,3))])

fwrite(sig, file.path(REPO, "v2 OFICIAL/data/pares_replicantes_significativos.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/pares_replicantes_significativos.csv' (", nrow(sig), " pares)\n", sep="")
