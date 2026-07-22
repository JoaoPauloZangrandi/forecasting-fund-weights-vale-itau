# =============================================================================
# 27_features_multiativo.R  (v2 OFICIAL)
#
# Junta as caracteristicas do FUNDO (AUM/cotistas/fluxo predeterminados +
# beta do fundo, ja calculadas na Etapa 2) ao painel multiativo (R/25b+26) --
# essas caracteristicas nao dependem do ativo, sao do fundo, entao so precisa
# fazer merge por (cod_fundo, mes).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_direto.csv"))
d[, ym := ano*100L + mes]
cat("Painel multiativo:", nrow(d), "linhas |", uniqueN(d$cod_fundo), "fundos |",
    uniqueN(d$ativo), "ativos\n")

addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
d[, ym_prev := addm(ym, -1L)]

feat <- fread(file.path(REPO, "v2 OFICIAL/data/painel_todas_gestoras_predeterminado.csv"),
              select = c("cod_fundo","ym","aum_prev","cotistas_prev","fluxo_prev","is_fic"))
setnames(feat, "ym", "ym_feat")
feat <- unique(feat, by = c("cod_fundo","ym_feat"))
cat("Tabela de features fundo-mes:", nrow(feat), "linhas\n")

bf <- fread(file.path(REPO, "v2 OFICIAL/data/beta_fundo_todas_gestoras.csv"))
setnames(bf, "ymk", "ym_feat")

d2 <- merge(d, feat, by.x = c("cod_fundo","ym"), by.y = c("cod_fundo","ym_feat"), all.x = TRUE)
cat("Apos merge com AUM/cotistas/fluxo/FIC:", nrow(d2), "linhas (deve ser igual a antes)\n")
d2 <- merge(d2, bf, by.x = c("cod_fundo","ym"), by.y = c("cod_fundo","ym_feat"), all.x = TRUE)
cat("Apos merge com beta_fundo:", nrow(d2), "linhas (deve ser igual a antes)\n")

d2[, l_aum := log(aum_prev)]
d2[, l_cot := log(cotistas_prev)]
d2[, flow_aum := fluxo_prev / aum_prev]
d3 <- d2[is.finite(l_aum) & is.finite(l_cot) & is.finite(flow_aum) & is.finite(beta_fundo)]
cat("\nApos exigir as 5 caracteristicas completas:", nrow(d3), "linhas |",
    uniqueN(d3$cod_fundo), "fundos |", uniqueN(d3$ativo), "ativos\n")

fwrite(d3, file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/painel_multiativo_final.csv'\n")
