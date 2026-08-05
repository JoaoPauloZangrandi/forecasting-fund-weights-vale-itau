# =============================================================================
# 89_checagem_soma_peso.R  (v2 OFICIAL)
#
# Achado no fundo 292826 (Vinci Partners, Figura 11 do TCC_final.tex):
# soma dos pesos (Participacao_Ativo) de TODAS as posicoes do fundo, em
# mar/2021 e abr/2021, chega a quase 500% do patrimonio -- confirmado na
# fonte bruta (painel_multiativo_direto.csv, direto da CDA da CVM), nao
# e bug de merge. O fundo estava em liquidacao (PL caiu de R$59mi em
# dez/2020 pra R$1,6mi em jun/2021, cotistas de 5 pra 2).
#
# Pergunta agora: isso e um caso isolado, ou ha outros fundos-mes com o
# mesmo problema (soma de peso >> 1) que podem estar contaminando outras
# analises do documento (erro, RMSE, vies, variancia, concentracao)?
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"),
            select = c("cod_fundo","ym","ativo","peso","gestora_grupo","aum_prev","cotistas_prev"))

soma <- pp[, .(soma_peso = sum(peso), n_ativos = .N), by = .(cod_fundo, ym)]
cat("Total de pares fundo-mes:", nrow(soma), "\n")
cat("\n===== Distribuicao da soma de peso por fundo-mes =====\n")
print(summary(soma$soma_peso))

for (lim in c(1.01, 1.05, 1.10, 1.25, 1.50, 2.00)) {
  n <- sum(soma$soma_peso > lim)
  cat(sprintf("Pares fundo-mes com soma_peso > %.2f : %d (%.3f%%)\n", lim, n, 100*n/nrow(soma)))
}

anomalos <- soma[soma_peso > 1.10]
cat("\n===== Fundos distintos afetados (soma_peso > 1,10) =====\n")
cat("Fundos distintos:", uniqueN(anomalos$cod_fundo), "de", uniqueN(soma$cod_fundo), "\n")

cat("\n===== Os 20 casos mais extremos =====\n")
setorder(anomalos, -soma_peso)
anomalos_info <- merge(anomalos[1:20], unique(pp[, .(cod_fundo, ym, gestora_grupo, aum_prev, cotistas_prev)]),
                        by = c("cod_fundo","ym"))
print(anomalos_info[, .(cod_fundo, ym, gestora_grupo, soma_peso = round(soma_peso,2), n_ativos, aum_prev, cotistas_prev)])

cat("\n===== Esses fundos-mes anomalos entraram na base final da Etapa 1/regressao? =====\n")
cat("(A amostra final da Etapa 1 exige is.finite(l_aum, l_cot, flow_aum, beta_fundo) e !is.na(is_fic),\n")
cat(" mas NAO filtra por soma_peso -- ou seja, esses casos provavelmente entraram sem filtro.)\n")

fwrite(soma, file.path(REPO, "v2 OFICIAL/data/checagem_soma_peso_fundo_mes.csv"))
cat("\nOK\n")
