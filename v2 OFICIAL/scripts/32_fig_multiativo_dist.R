suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG  <- file.path(REPO, "v2 OFICIAL/figuras")

avg <- fread(file.path(REPO, "v2 OFICIAL/data/theta_media_ativo.csv"))
vale <- avg[ativo == "VALE ON N1 - VALE3"]

# LOGIT: distribuicao do APE (pontos percentuais), nao mais do coeficiente linear bruto
pdf(file.path(FIG, "fig_multiativo_dist_v2.pdf"), width = 9, height = 6)
par(mfrow = c(2,3), mar = c(3,3,2.5,1))
vars <- c(ape_aum="tamanho", ape_cot="cotistas", ape_fic="fundo de cotas",
          ape_flow="captação/resgate", ape_betaf="beta do fundo")
for (v in names(vars)) {
  hist(avg[[v]], breaks = 30, col = "#B0B8C1", border = "white",
       main = vars[v], xlab = "", ylab = "")
  abline(v = vale[[v]], col = "#8A2E2E", lwd = 2)
  abline(v = median(avg[[v]]), col = "grey40", lty = 2)
}
plot.new()
legend("center", legend = c("VALE3", "mediana entre ativos"), col = c("#8A2E2E","grey40"),
       lwd = c(2,1), lty = c(1,2), bty = "n", cex = 1.1)
dev.off()
cat("OK - salvo em 'v2 OFICIAL/figuras/fig_multiativo_dist_v2.pdf'\n")
