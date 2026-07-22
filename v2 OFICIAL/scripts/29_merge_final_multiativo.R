# =============================================================================
# 29_merge_final_multiativo.R  (v2 OFICIAL)
#
# Substitui o merge do R/27 (que usava a tabela de features restrita a
# meses-com-VALE3) pela tabela completa do R/28 (spine correto: todos os
# fundo-mes do painel multiativo). Mescla tambem beta_fundo (ja cobre o
# universo inteiro, so precisa juntar).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_direto.csv"))
d[, ym := ano*100L + mes]
cat("Painel multiativo:", nrow(d), "linhas\n")

feat <- fread(file.path(REPO, "v2 OFICIAL/data/features_fundo_mes_multiativo.csv"))
cat("Features (spine completo):", nrow(feat), "linhas\n")

d2 <- merge(d, feat[, .(cod_fundo, ym, aum_prev, cotistas_prev, fluxo_prev)],
            by = c("cod_fundo","ym"), all.x = TRUE)
cat("Apos merge AUM/cotistas/fluxo:", nrow(d2), "linhas (deve ser igual a antes)\n")

# is_fic: pega da tabela ja existente (baseada no nome do fundo, nao muda por mes/spine)
fic <- unique(fread(file.path(REPO, "v2 OFICIAL/data/painel_todas_gestoras_predeterminado.csv"),
                     select = c("cod_fundo","is_fic")), by = "cod_fundo")
d2 <- merge(d2, fic, by = "cod_fundo", all.x = TRUE)
cat("Fundos sem is_fic mapeado:", d2[is.na(is_fic), uniqueN(cod_fundo)], "\n")

bf <- fread(file.path(REPO, "v2 OFICIAL/data/beta_fundo_todas_gestoras.csv"))
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
cat("VALE3 na amostra final multiativo:", nvale, "(Etapa 1 tinha 8.335 -- mesma logica, deve bater perto)\n")

fwrite(d3, file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/painel_multiativo_final.csv'\n")
