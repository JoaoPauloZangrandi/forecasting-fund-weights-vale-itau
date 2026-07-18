# =============================================================================
# 06_figuras_extra.R  (v2 OFICIAL)
#
# Figuras adicionais para o v2 OFICIAL.pdf:
#  (1) trajetoria de theta_t (Etapa 1, linear) ao longo dos 72 meses
#  (2) comparacao de RMSE dentro/fora da amostra, ajuste parcial vs ingenua
#  (3) RMSE mes a mes no periodo de teste (out-of-sample)
#  (4) dispersao: variacao do peso prevista vs observada, no teste
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG  <- file.path(REPO, "v2 OFICIAL/figuras")

# ---- (1) trajetoria de theta_t ----
theta <- fread(file.path(REPO, "v2 OFICIAL/data/theta_mensal.csv"))
setorder(theta, ym)
theta[, idx := .I]
datas <- as.Date(paste0(substr(theta$ym,1,4), "-", substr(theta$ym,5,6), "-01"))

pdf(file.path(FIG, "fig_theta_trajetoria.pdf"), width = 8, height = 7)
par(mfrow = c(3,2), mar = c(3,4,2.5,1))
plot(datas, theta$alpha, type = "l", col = "#2E5C8A", lwd = 1.6, main = "alpha (intercepto)", xlab = "", ylab = "")
abline(h = 0, col = "grey70", lty = 2)
plot(datas, theta$b_aum, type = "l", col = "#2E5C8A", lwd = 1.6, main = "b_aum (tamanho)", xlab = "", ylab = "")
abline(h = 0, col = "grey70", lty = 2)
plot(datas, theta$b_cot, type = "l", col = "#2E5C8A", lwd = 1.6, main = "b_cot (cotistas)", xlab = "", ylab = "")
abline(h = 0, col = "grey70", lty = 2)
plot(datas, theta$b_fic, type = "l", col = "#2E5C8A", lwd = 1.6, main = "b_fic (fundo de cotas)", xlab = "", ylab = "")
abline(h = 0, col = "grey70", lty = 2)
plot(datas, theta$b_flow, type = "l", col = "#2E5C8A", lwd = 1.6, main = "b_flow (captação/resgate)", xlab = "", ylab = "")
abline(h = 0, col = "grey70", lty = 2)
plot(datas, theta$r2, type = "l", col = "#8A2E2E", lwd = 1.6, main = "R2 da regressão mensal", xlab = "", ylab = "")
dev.off()

# ---- (2) barras: RMSE dentro/fora da amostra ----
M <- fread(file.path(REPO, "v2 OFICIAL/data/ajuste_parcial_erros.csv"))
CORTE <- 202001L
treino <- M[ym < CORTE]; teste <- M[ym >= CORTE & ym < 202112L]
fit_tr <- lm(dw ~ 0 + d, data = treino); lam_tr <- coef(fit_tr)["d"]
rmse <- function(x) sqrt(mean(x^2))

rmse_tab <- data.table(
  amostra = c("Treino\n(2016-2019)","Treino\n(2016-2019)","Teste\n(2020-2021)","Teste\n(2020-2021)"),
  modelo  = c("Ajuste parcial","Ingênua","Ajuste parcial","Ingênua"),
  rmse    = c(rmse(treino$dw - lam_tr*treino$d), rmse(treino$dw),
              rmse(teste$dw - lam_tr*teste$d), rmse(teste$dw))
)
mat <- matrix(rmse_tab$rmse, nrow = 2)
rownames(mat) <- c("Ajuste parcial","Ingênua")

pdf(file.path(FIG, "fig_oos_barras.pdf"), width = 6, height = 4.5)
bp <- barplot(mat, beside = TRUE, col = c("#2E5C8A","#B0B8C1"),
              names.arg = c("Treino\n(2016-2019)","Teste\n(2020-2021)"),
              ylab = "RMSE", main = "RMSE: ajuste parcial vs. ingênua",
              ylim = c(0, max(mat)*1.25))
legend("topright", legend = c("Ajuste parcial (lambda de treino)","Ingênua"),
       fill = c("#2E5C8A","#B0B8C1"), bty = "n", cex = 0.85)
text(bp, mat, sprintf("%.4f", mat), pos = 3, cex = 0.75)
dev.off()

# ---- (3) RMSE mes a mes no teste ----
por_mes <- fread(file.path(REPO, "v2 OFICIAL/data/out_of_sample_por_mes.csv"))
setorder(por_mes, ym)
datas_t <- as.Date(paste0(substr(por_mes$ym,1,4), "-", substr(por_mes$ym,5,6), "-01"))

pdf(file.path(FIG, "fig_oos_mensal.pdf"), width = 7, height = 4.5)
plot(datas_t, por_mes$rmse_naive, type = "b", col = "#B0B8C1", pch = 16, lwd = 1.6,
     ylim = range(c(por_mes$rmse_naive, por_mes$rmse_ajuste)),
     xlab = "", ylab = "RMSE do mês", main = "RMSE mês a mês, fora da amostra (2020-2021)")
lines(datas_t, por_mes$rmse_ajuste, type = "b", col = "#2E5C8A", pch = 16, lwd = 1.6)
legend("topleft", legend = c("Ajuste parcial","Ingênua"), col = c("#2E5C8A","#B0B8C1"),
       lwd = 1.6, pch = 16, bty = "n", cex = 0.85)
dev.off()

# ---- (4) dispersao: previsto vs observado, no teste ----
teste_full <- fread(file.path(REPO, "v2 OFICIAL/data/out_of_sample_resultado.csv"))
pdf(file.path(FIG, "fig_oos_dispersao.pdf"), width = 5.5, height = 5.5)
plot(teste_full$dw_prev_ajuste, teste_full$dw, pch = 16, col = rgb(0.18,0.36,0.54,0.35),
     xlab = "Variação do peso PREVISTA (ajuste parcial)", ylab = "Variação do peso OBSERVADA",
     main = "Fora da amostra: previsto vs. observado")
abline(0, 1, col = "#8A2E2E", lwd = 1.6, lty = 2)
dev.off()

cat("OK - figuras extras salvas em 'v2 OFICIAL/figuras/'\n")
