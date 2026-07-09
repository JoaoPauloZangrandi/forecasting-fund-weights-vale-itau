# =============================================================================
# 37_fig_multiativo.R — gera docs/fig_multiativo_dist.pdf: histogramas da
# distribuicao ENTRE ATIVOS dos coeficientes medios theta_n (361 ativos com
# >=24 meses estimados, R/32), marcando onde VALE3 cai em cada um. Visualiza
# o achado central da Secao 5 do tcc I: os SINAIS generalizam, mas a
# MAGNITUDE de VALE3 no efeito de tamanho e extrema (percentil 1).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
avg <- fread(file.path(REPO, "data/processed/reg32_theta_media_ativo.csv"))
vale <- avg[ativo == "VALE ON N1 - VALE3"]

pdf(file.path(REPO, "docs/fig_multiativo_dist.pdf"), width = 9, height = 6.5)
op <- par(mfrow = c(2, 2), mar = c(4, 4, 2.8, 1))

painel <- function(var, titulo, xlab) {
  x <- avg[[var]]; v <- vale[[var]]
  pct <- round(100 * mean(x <= v), 0)
  hist(x, breaks = 40, col = "steelblue", border = "white",
       main = titulo, xlab = xlab, ylab = "nº de ativos")
  abline(v = v, col = "firebrick", lwd = 2.5, lty = 1)
  abline(v = median(x), col = "grey40", lwd = 1.5, lty = 2)
  legend("topright", bty = "n", cex = 0.8,
         legend = c(sprintf("VALE3 (percentil %d)", pct), "mediana entre ativos"),
         col = c("firebrick", "grey40"), lwd = c(2.5, 1.5), lty = c(1, 2))
}

painel("b_aum",  "ln(patrimônio)", expression(bar(theta)[ln(PL)]~"por ativo"))
painel("b_cot",  "ln(cotistas)",   expression(bar(theta)[ln(Cot)]~"por ativo"))
painel("b_fic",  "Fundo de cotas", expression(bar(theta)[FIC]~"por ativo"))
painel("b_flow", "Captação/PL",    expression(bar(theta)[Fluxo]~"por ativo"))

par(op); dev.off()
cat("OK - docs/fig_multiativo_dist.pdf gerado\n")
cat("VALE3 b_aum:", round(vale$b_aum,5), "| percentil:", round(100*mean(avg$b_aum<=vale$b_aum),0), "\n")
cat("VALE3 b_cot:", round(vale$b_cot,5), "| percentil:", round(100*mean(avg$b_cot<=vale$b_cot),0), "\n")
cat("VALE3 b_fic:", round(vale$b_fic,5), "| percentil:", round(100*mean(avg$b_fic<=vale$b_fic),0), "\n")
