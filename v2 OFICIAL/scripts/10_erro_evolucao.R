# =============================================================================
# 10_erro_evolucao.R  (v2 OFICIAL)
#
# Evolucao mes a mes dos erros, em grafico de linha -- mesmo espirito das
# figuras de evolucao de theta, mas para os erros das Etapas 2 (logit) e 3
# (ajuste parcial). Mostra TRES coisas juntas:
#   (1) a nuvem de pontos com o erro individual de cada fundo-mes (cinza,
#       transparente) -- para se ver a dispersao real por tras da media/dp;
#   (2) a media mensal (linha azul);
#   (3) o desvio-padrao mensal (linha vermelha tracejada).
#
# Nota: a media mensal do erro "e" (Etapa 2) e mecanicamente zero em todo mes
# (condicao de primeira ordem da MLE com intercepto -- ja documentado no
# texto), entao a linha azul fica achatada em zero -- e a nuvem de pontos
# individuais que mostra a dispersao real por tras disso.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG  <- file.path(REPO, "v2 OFICIAL/figuras")

plot_evolucao <- function(dt_ind, col_erro, dt_mes, col_media, col_dp, titulo, arquivo, yl) {
  setorder(dt_mes, ym)
  datas_mes <- as.Date(paste0(substr(dt_mes$ym,1,4), "-", substr(dt_mes$ym,5,6), "-01"))
  datas_ind <- as.Date(paste0(substr(dt_ind$ym,1,4), "-", substr(dt_ind$ym,5,6), "-01"))

  n_fora <- sum(dt_ind[[col_erro]] < yl[1] | dt_ind[[col_erro]] > yl[2])
  cat(sprintf("%s: %d de %d pontos individuais ficam fora de [%.3f, %.3f]\n",
              arquivo, n_fora, nrow(dt_ind), yl[1], yl[2]))

  pdf(file.path(FIG, arquivo), width = 7.5, height = 4.8)
  par(mar = c(3, 4, 2.5, 1))
  plot(datas_ind, dt_ind[[col_erro]], pch = 16, cex = 0.35,
       col = rgb(0.4, 0.4, 0.4, 0.18), ylim = yl,
       xlab = "", ylab = "erro (fração do peso)", main = titulo)
  lines(datas_mes, dt_mes[[col_media]], col = "#2E5C8A", lwd = 2.2)
  lines(datas_mes, dt_mes[[col_dp]], col = "#8A2E2E", lwd = 2.2, lty = 2)
  abline(h = 0, col = "grey50", lty = 3)
  legend("topleft", legend = c("erro individual (fundo-mês)", "média mensal", "desvio-padrão mensal"),
         col = c(rgb(0.4,0.4,0.4,0.6), "#2E5C8A", "#8A2E2E"), pch = c(16, NA, NA),
         lty = c(NA, 1, 2), lwd = c(NA, 2.2, 2.2), pt.cex = c(0.8, NA, NA),
         bty = "n", cex = 0.8)
  dev.off()
}

e_ind <- fread(file.path(REPO, "v2 OFICIAL/data/erro_cross_section.csv"))
e_mes <- fread(file.path(REPO, "v2 OFICIAL/data/erro_cross_section_por_mes.csv"))
plot_evolucao(e_ind, "e", e_mes, "media_e", "dp_e",
              "Evolução mês a mês do erro e (Etapa 2, logit)",
              "fig_erro_logit_mensal.pdf", yl = c(-0.025, 0.05))

u_ind <- fread(file.path(REPO, "v2 OFICIAL/data/ajuste_parcial_erros.csv"))
u_mes <- fread(file.path(REPO, "v2 OFICIAL/data/ajuste_parcial_erros_por_mes.csv"))
plot_evolucao(u_ind, "u", u_mes, "media_u", "dp_u",
              "Evolução mês a mês do erro u (Etapa 3, ajuste parcial)",
              "fig_erro_ajuste_mensal.pdf", yl = c(-0.03, 0.04))

cat("OK - salvos em 'v2 OFICIAL/figuras/fig_erro_logit_mensal.pdf' e 'fig_erro_ajuste_mensal.pdf'\n")
