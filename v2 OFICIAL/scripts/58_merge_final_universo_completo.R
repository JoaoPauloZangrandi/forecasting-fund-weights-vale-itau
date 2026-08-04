# =============================================================================
# 58_merge_final_universo_completo.R  (v2 OFICIAL -- teste_estrategia)
#
# FASE 1, passo 1: junta posicoes (R/51) + AUM/cotistas/fluxo/FIC (R/52,
# ja usa o spine COMPLETO do painel multiativo, mesma correcao que o R/28/29
# fizeram no pipeline original) + beta do fundo (R/53) -- painel final
# pronto pra Etapa 1 no universo expandido. Mesma logica exata do R/29.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_direto_completo.csv"))
d[, cod_fundo := as.character(cod_fundo)]
d[, ym := ano*100L + mes]
cat("Painel multiativo (universo completo):", nrow(d), "linhas\n")

feat <- fread(file.path(REPO, "v2 OFICIAL/data/features_predeterminado_completo.csv"))
feat[, cod_fundo := as.character(cod_fundo)]
d2 <- merge(d, feat[, .(cod_fundo, ym, aum_prev, cotistas_prev, fluxo_prev, is_fic)],
            by = c("cod_fundo","ym"), all.x = TRUE)
cat("Apos merge AUM/cotistas/fluxo/FIC:", nrow(d2), "linhas (deve ser igual a antes)\n")

bf <- fread(file.path(REPO, "v2 OFICIAL/data/beta_fundo_universo_completo.csv"))
bf[, cod_fundo := as.character(cod_fundo)]
setnames(bf, "ymk", "ym")
d2 <- merge(d2, bf, by = c("cod_fundo","ym"), all.x = TRUE)
cat("Apos merge beta_fundo:", nrow(d2), "linhas (deve ser igual a antes)\n")

d2[, l_aum := log(aum_prev)]
d2[, l_cot := log(cotistas_prev)]
d2[, flow_aum := fluxo_prev / aum_prev]
d3 <- d2[is.finite(l_aum) & is.finite(l_cot) & is.finite(flow_aum) & is.finite(beta_fundo) & !is.na(is_fic)]
cat("\nApos exigir as 5 caracteristicas completas:", nrow(d3), "linhas |",
    uniqueN(d3$cod_fundo), "fundos |", uniqueN(d3$ativo), "ativos\n")

nvale <- d3[ativo == "VALE ON N1 - VALE3", .N]
cat("VALE3 na amostra final (universo completo):", nvale,
    "(pipeline original tinha 8.335 no universo VALE3-ancorado -- comparar de perto)\n")

fwrite(d3, file.path(REPO, "v2 OFICIAL/data/painel_universo_completo_final.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/painel_universo_completo_final.csv'\n")
