# =============================================================================
# 72_figuras_teste_retorno.R  (v2 OFICIAL -- teste_estrategia)
#
# 2 figuras do teste fluxo-prediz-retorno (R/70, R/71):
#  (4) barras de retorno medio por quintil de fluxo previsto, amostra
#      completa vs restrita a ativos de margem positiva, lado a lado.
#  (5) dispersao fluxo x retorno futuro (amostra completa), com reta de
#      regressao -- visualiza o R2 praticamente zero.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG <- file.path(REPO, "v2 OFICIAL/figuras_estrategia")

M  <- fread(file.path(REPO, "v2 OFICIAL/data/teste_fluxo_prediz_retorno.csv"))
Mr <- fread(file.path(REPO, "v2 OFICIAL/data/teste_fluxo_retorno_restrito.csv"))

M[, quintil := cut(fluxo_pct, quantile(fluxo_pct, seq(0,1,0.2)), include.lowest=TRUE, labels=FALSE)]
q_full <- M[, .(retorno_medio = mean(retorno_fut)), by=quintil][order(quintil)]
q_rest <- fread(file.path(REPO, "v2 OFICIAL/data/quintis_fluxo_retorno_restrito.csv"))[order(quintil)]

pdf(file.path(FIG, "fig4_quintis_retorno.pdf"), width = 7.5, height = 5)
par(mfrow = c(1,2), mar = c(4,4,2.5,1))
bp1 <- barplot(100*q_full$retorno_medio, names.arg = paste0("Q",1:5), col = "#2E5C8A", border=NA,
               ylab = "Retorno médio subsequente (%)",
               main = sprintf("Amostra completa\n(%s obs)", format(nrow(M), big.mark=".")),
               ylim = c(0, max(100*q_full$retorno_medio, 100*q_rest$retorno_medio)*1.15))
bp2 <- barplot(100*q_rest$retorno_medio, names.arg = paste0("Q",1:5), col = "#B8452E", border=NA,
               ylab = "Retorno médio subsequente (%)",
               main = sprintf("Só ativos com margem>0\n(%s obs)", format(nrow(Mr), big.mark=".")),
               ylim = c(0, max(100*q_full$retorno_medio, 100*q_rest$retorno_medio)*1.15))
mtext("Quintil 1 = mais fluxo vendedor previsto | Quintil 5 = mais fluxo comprador previsto",
      side = 1, outer = TRUE, line = -1.2, cex = 0.75)
dev.off()
cat("Fig 4 OK\n")

pdf(file.path(FIG, "fig5_dispersao_fluxo_retorno.pdf"), width = 6.5, height = 5.5)
par(mar = c(4.2,4.2,1,1))
lim_x <- quantile(M$fluxo_pct, c(0.02, 0.98))
plot(M$fluxo_pct, 100*M$retorno_fut, pch = 16, col = adjustcolor("#2E5C8A", 0.35), cex = 0.6,
     xlim = lim_x, xlab = "Fluxo previsto (% do valor total da posição no ativo)",
     ylab = "Retorno do ativo, mês seguinte (%)")
fit <- lm(retorno_fut ~ fluxo_pct, data = M)
abline(fit, col = "#B8452E", lwd = 2)
abline(h = 0, col = "grey70", lty=2)
legend("topright", legend = sprintf("R² = %.4f%%", 100*summary(fit)$r.squared), bty = "n", cex=0.9)
dev.off()
cat("Fig 5 OK\n")
