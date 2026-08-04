# =============================================================================
# 75_figura_margem_dinamica.R  (v2 OFICIAL)
#
# Pedido do orientador: dinamica da MARGEM (nao so o erro) ao longo do
# tempo -- a Tabela 26 resume a margem do periodo de teste inteiro num
# unico numero por horizonte; esta figura mostra mes a mes se a vantagem
# do ajuste parcial sobre a ingenua e' estavel, cresce ou some ao longo da
# janela de teste. Usa a mesma base ja validada (etapa3_multiativo_h1/h3,
# Tabelas 25/26), nao o universo do teste_estrategia.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG <- file.path(REPO, "v2 OFICIAL/figuras")

h1 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h1.csv"))
h3 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h3.csv"))
rmse <- function(x) sqrt(mean(x^2))

margem_mensal <- function(dt) {
  m <- dt[, .(rmse_oos = rmse(erro_oos), rmse_naive = rmse(erro_naive), n = .N), by = ym]
  m[, margem := 100*(rmse_naive - rmse_oos)/rmse_naive]
  m[, data := as.Date(paste0(substr(ym,1,4),"-",substr(ym,5,6),"-01"))]
  setorder(m, data)
  m
}
m1 <- margem_mensal(h1); m3 <- margem_mensal(h3)
cat("h1: ", nrow(m1), "meses de teste | margem media:", round(mean(m1$margem),2), "%\n")
cat("h3: ", nrow(m3), "meses de teste | margem media:", round(mean(m3$margem),2), "%\n")

pdf(file.path(FIG, "fig_margem_dinamica_mensal.pdf"), width = 7.5, height = 5)
par(mar = c(4,4.5,1,1))
ylim_c <- range(c(m1$margem, m3$margem))
eixo_x <- range(c(m1$data, m3$data))
plot(m1$data, m1$margem, type="n", xlim=eixo_x, ylim=ylim_c, xaxt="n",
     xlab="Mês (teste, out-of-sample)", ylab="Margem do ajuste parcial sobre a ingênua (%)")
axis.Date(1, at=seq(eixo_x[1], eixo_x[2], by="4 months"), format="%b/%Y")
abline(h=0, col="grey50", lty=2)
lines(m1$data, m1$margem, col="#2E5C8A", lwd=2, type="o", pch=16, cex=0.6)
lines(m3$data, m3$margem, col="#B8452E", lwd=2, type="o", pch=16, cex=0.6)
legend("topright", legend=c("h = 1 mês","h = 3 meses"), col=c("#2E5C8A","#B8452E"), lwd=2, bty="n")
dev.off()
cat("\nOK - salvo em 'fig_margem_dinamica_mensal.pdf'\n")
