# =============================================================================
# 16_figuras_v2.R  (v2 OFICIAL)
#
# Regera as figuras da Etapa 1 (agora com 6 paineis: +beta do fundo),
# Etapa 2 (ajuste parcial) e Etapa 3 (in/out-of-sample), usando os dados
# recomputados no R/13 e R/15.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG  <- file.path(REPO, "v2 OFICIAL/figuras")

# ---- (1) trajetoria de theta LOGIT (especificacao principal), 6 paineis ----
theta <- fread(file.path(REPO, "v2 OFICIAL/data/theta_logit_mensal_v2.csv")); setorder(theta, ym)
datas <- as.Date(paste0(substr(theta$ym,1,4),"-",substr(theta$ym,5,6),"-01"))

cairo_pdf(file.path(FIG, "fig_theta_trajetoria_v2.pdf"), width = 8, height = 9.2)
par(mfrow = c(4,2), mar = c(3,4,2.5,1))
plot(datas, theta$alpha, type="l", col="#2E5C8A", lwd=1.6, main="α (intercepto)", xlab="", ylab=""); abline(h=0,col="grey70",lty=2)
plot(datas, theta$b_aum, type="l", col="#2E5C8A", lwd=1.6, main="θ-aum (tamanho)", xlab="", ylab=""); abline(h=0,col="grey70",lty=2)
plot(datas, theta$b_cot, type="l", col="#2E5C8A", lwd=1.6, main="θ-cot (cotistas)", xlab="", ylab=""); abline(h=0,col="grey70",lty=2)
plot(datas, theta$b_fic, type="l", col="#2E5C8A", lwd=1.6, main="θ-fic (fundo de cotas)", xlab="", ylab=""); abline(h=0,col="grey70",lty=2)
plot(datas, theta$b_flow, type="l", col="#2E5C8A", lwd=1.6, main="θ-flow (captação/resgate)", xlab="", ylab=""); abline(h=0,col="grey70",lty=2)
plot(datas, theta$b_betaf, type="l", col="#2E5C8A", lwd=1.6, main="θ-bf (beta do fundo)", xlab="", ylab=""); abline(h=0,col="grey70",lty=2)
plot(datas, theta$pr2, type="l", col="#8A2E2E", lwd=1.6, main="Pseudo-R² (McFadden) da regressão mensal", xlab="", ylab="")
frame()
dev.off()

# ---- (2) histograma + evolucao do erro e ------------------------------------
e_ind <- fread(file.path(REPO, "v2 OFICIAL/data/v2_erro_e.csv"))
e_mes <- fread(file.path(REPO, "v2 OFICIAL/data/v2_erro_e_por_mes.csv")); setorder(e_mes, ym)

pdf(file.path(FIG, "hist_erro_e_v2.pdf"), width = 6, height = 4.2)
hist(e_ind$e, breaks = 80, col = "#2E5C8A", border = "white",
     main = "Erro da Etapa 1 (cross-section + logit)",
     xlab = "e  =  peso observado  -  g(x'theta)", ylab = "Frequência")
abline(v = 0, col = "#8A2E2E", lwd = 2, lty = 2)
dev.off()

datas_ind <- as.Date(paste0(substr(e_ind$ym,1,4),"-",substr(e_ind$ym,5,6),"-01"))
datas_mes <- as.Date(paste0(substr(e_mes$ym,1,4),"-",substr(e_mes$ym,5,6),"-01"))
yl <- c(-0.03, 0.06)
n_fora <- sum(e_ind$e < yl[1] | e_ind$e > yl[2])
cat("erro_e_mensal_v2: pontos fora de", paste(yl,collapse=","), "=", n_fora, "de", nrow(e_ind), "\n")
pdf(file.path(FIG, "fig_erro_e_mensal_v2.pdf"), width = 7.5, height = 4.8)
par(mar = c(3,4,2.5,1))
plot(datas_ind, e_ind$e, pch=16, cex=0.35, col=rgb(0.4,0.4,0.4,0.18), ylim=yl,
     xlab="", ylab="erro (fração do peso)", main="Evolução mês a mês do erro e (Etapa 1)")
lines(datas_mes, e_mes$media_e, col="#2E5C8A", lwd=2.2)
lines(datas_mes, e_mes$dp_e, col="#8A2E2E", lwd=2.2, lty=2)
abline(h=0, col="grey50", lty=3)
legend("topleft", legend=c("erro individual (fundo-mês)","média mensal","desvio-padrão mensal"),
       col=c(rgb(0.4,0.4,0.4,0.6),"#2E5C8A","#8A2E2E"), pch=c(16,NA,NA), lty=c(NA,1,2),
       lwd=c(NA,2.2,2.2), pt.cex=c(0.8,NA,NA), bty="n", cex=0.8)
dev.off()

# ---- (3) histograma + evolucao do erro u ------------------------------------
u_ind <- fread(file.path(REPO, "v2 OFICIAL/data/v2_ajuste_parcial.csv"))
u_mes <- fread(file.path(REPO, "v2 OFICIAL/data/v2_ajuste_parcial_por_mes.csv")); setorder(u_mes, ym)

pdf(file.path(FIG, "hist_erro_u_v2.pdf"), width = 6, height = 4.2)
hist(u_ind$u, breaks = 80, col = "#2E5C8A", border = "white",
     main = "Erro da Etapa 2 (ajuste parcial)",
     xlab = "u  =  variação do peso observada  -  lambda * distância", ylab = "Frequência")
abline(v = 0, col = "#8A2E2E", lwd = 2, lty = 2)
dev.off()

datas_ind_u <- as.Date(paste0(substr(u_ind$ym,1,4),"-",substr(u_ind$ym,5,6),"-01"))
datas_mes_u <- as.Date(paste0(substr(u_mes$ym,1,4),"-",substr(u_mes$ym,5,6),"-01"))
yl_u <- c(-0.03, 0.04)
n_fora_u <- sum(u_ind$u < yl_u[1] | u_ind$u > yl_u[2])
cat("erro_u_mensal_v2: pontos fora de", paste(yl_u,collapse=","), "=", n_fora_u, "de", nrow(u_ind), "\n")
pdf(file.path(FIG, "fig_erro_u_mensal_v2.pdf"), width = 7.5, height = 4.8)
par(mar = c(3,4,2.5,1))
plot(datas_ind_u, u_ind$u, pch=16, cex=0.35, col=rgb(0.4,0.4,0.4,0.18), ylim=yl_u,
     xlab="", ylab="erro (fração do peso)", main="Evolução mês a mês do erro u (Etapa 2)")
lines(datas_mes_u, u_mes$media_u, col="#2E5C8A", lwd=2.2)
lines(datas_mes_u, u_mes$dp_u, col="#8A2E2E", lwd=2.2, lty=2)
abline(h=0, col="grey50", lty=3)
legend("topleft", legend=c("erro individual (fundo-mês)","média mensal","desvio-padrão mensal"),
       col=c(rgb(0.4,0.4,0.4,0.6),"#2E5C8A","#8A2E2E"), pch=c(16,NA,NA), lty=c(NA,1,2),
       lwd=c(NA,2.2,2.2), pt.cex=c(0.8,NA,NA), bty="n", cex=0.8)
dev.off()

# ---- (4) in/out-of-sample: barras + periodo completo com corte -------------
meta <- readRDS(file.path(REPO, "v2 OFICIAL/data/v2_oos_meta.rds"))
lam_tr <- meta$lam_treino; CORTE <- meta$corte
M <- fread(file.path(REPO, "v2 OFICIAL/data/v2_ajuste_parcial.csv"))
treino <- M[ym < CORTE]; teste <- M[ym >= CORTE & ym < 202112L]
rmse <- function(x) sqrt(mean(x^2))
mat <- matrix(c(rmse(treino$dw - lam_tr*treino$d), rmse(treino$dw),
                rmse(teste$dw - lam_tr*teste$d), rmse(teste$dw)), nrow = 2)
rownames(mat) <- c("Ajuste parcial","Ingênua")

pdf(file.path(FIG, "fig_oos_barras_v2.pdf"), width = 6, height = 4.5)
bp <- barplot(mat, beside = TRUE, col = c("#2E5C8A","#B0B8C1"),
              names.arg = c("Treino\n(2017-2019)","Teste\n(2020-2021)"),
              ylab = "RMSE", main = "RMSE: ajuste parcial vs. ingênua",
              ylim = c(0, max(mat)*1.25))
legend("topright", legend = c("Ajuste parcial (lambda de treino)","Ingênua"),
       fill = c("#2E5C8A","#B0B8C1"), bty = "n", cex = 0.85)
text(bp, mat, sprintf("%.4f", mat), pos = 3, cex = 0.75)
dev.off()

por_mes_c <- fread(file.path(REPO, "v2 OFICIAL/data/v2_oos_por_mes.csv")); setorder(por_mes_c, ym)
datas_c <- as.Date(paste0(substr(por_mes_c$ym,1,4),"-",substr(por_mes_c$ym,5,6),"-01"))
data_corte <- as.Date("2020-01-01")
pdf(file.path(FIG, "fig_erro_periodo_completo_v2.pdf"), width = 8, height = 4.8)
par(mar = c(3,4,2.5,1))
plot(datas_c, por_mes_c$rmse_naive, type="b", col="#B0B8C1", pch=16, lwd=1.6, cex=0.7,
     ylim = range(c(por_mes_c$rmse_naive, por_mes_c$rmse_ajuste)),
     xlab="", ylab="RMSE do mês", main="RMSE mês a mês, período completo (2017-2021)")
lines(datas_c, por_mes_c$rmse_ajuste, type="b", col="#2E5C8A", pch=16, lwd=1.6, cex=0.7)
abline(v = data_corte, col="#8A2E2E", lwd=2, lty=2)
text(data_corte, max(c(por_mes_c$rmse_naive,por_mes_c$rmse_ajuste))*0.80,
     " início do teste (2020-01) ", col="#8A2E2E", cex=0.75, pos=4)
legend("topleft", legend=c("Ajuste parcial (lambda de treino)","Ingênua"),
       col=c("#2E5C8A","#B0B8C1"), lwd=1.6, pch=16, bty="n", cex=0.85)
dev.off()

cat("\nOK - todas as figuras v2 salvas em 'v2 OFICIAL/figuras/'\n")
