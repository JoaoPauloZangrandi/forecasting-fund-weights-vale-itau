# =============================================================================
# 11_erro_periodo_completo.R  (v2 OFICIAL)
#
# Aplica o lambda estimado SO no treino (2016-2019) ao periodo INTEIRO
# (2016-2021, treino + teste), e calcula o RMSE mes a mes do ajuste parcial
# vs. ingenua nos 72 meses -- para visualizar, num grafico so, como o erro se
# comporta antes e depois do corte treino/teste (linha tracejada em 2020-01).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG  <- file.path(REPO, "v2 OFICIAL/figuras")

M <- fread(file.path(REPO, "v2 OFICIAL/data/ajuste_parcial_erros.csv"))
CORTE <- 202001L
treino <- M[ym < CORTE]

fit_tr <- lm(dw ~ 0 + d, data = treino)
lam_tr <- coef(fit_tr)["d"]
cat(sprintf("lambda (treino) = %.4f\n", lam_tr))

M[, dw_prev_ajuste := lam_tr * d]
M[, erro_ajuste := dw - dw_prev_ajuste]
M[, erro_naive := dw]  # dw_previsto = 0

rmse <- function(x) sqrt(mean(x^2))
por_mes <- M[, .(rmse_ajuste = rmse(erro_ajuste), rmse_naive = rmse(erro_naive)), by = ym]
setorder(por_mes, ym)

datas <- as.Date(paste0(substr(por_mes$ym,1,4), "-", substr(por_mes$ym,5,6), "-01"))
data_corte <- as.Date("2020-01-01")

pdf(file.path(FIG, "fig_erro_periodo_completo.pdf"), width = 8, height = 4.8)
par(mar = c(3, 4, 2.5, 1))
plot(datas, por_mes$rmse_naive, type = "b", col = "#B0B8C1", pch = 16, lwd = 1.6, cex = 0.7,
     ylim = range(c(por_mes$rmse_naive, por_mes$rmse_ajuste)),
     xlab = "", ylab = "RMSE do mês",
     main = "RMSE mês a mês, período completo (2016-2021)")
lines(datas, por_mes$rmse_ajuste, type = "b", col = "#2E5C8A", pch = 16, lwd = 1.6, cex = 0.7)
abline(v = data_corte, col = "#8A2E2E", lwd = 2, lty = 2)
text(data_corte, max(c(por_mes$rmse_naive, por_mes$rmse_ajuste)) * 0.80,
     " início do teste (2020-01) ", col = "#8A2E2E", cex = 0.75, pos = 4)
legend("topleft", legend = c("Ajuste parcial (lambda de treino)", "Ingênua"),
       col = c("#2E5C8A", "#B0B8C1"), lwd = 1.6, pch = 16, bty = "n", cex = 0.85)
dev.off()

cat("OK - salvo em 'v2 OFICIAL/figuras/fig_erro_periodo_completo.pdf'\n")
