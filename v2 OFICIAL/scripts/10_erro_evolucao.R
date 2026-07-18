# =============================================================================
# 10_erro_evolucao.R  (v2 OFICIAL)
#
# Evolucao mes a mes dos erros (media e desvio-padrao por mes), em grafico de
# linha -- mesmo espirito das figuras de evolucao de theta, mas para os erros
# das Etapas 2 (logit) e 3 (ajuste parcial).
#
# Nota: a media mensal do erro "e" (Etapa 2) e mecanicamente zero em todo mes
# (condicao de primeira ordem da MLE com intercepto -- ja documentado no
# texto), entao o grafico mostra as duas series (media e dp) juntas: a media
# fica visualmente achatada em zero, o dp e que mostra a evolucao real.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG  <- file.path(REPO, "v2 OFICIAL/figuras")

plot_evolucao <- function(dt, col_media, col_dp, titulo, arquivo) {
  setorder(dt, ym)
  datas <- as.Date(paste0(substr(dt$ym,1,4), "-", substr(dt$ym,5,6), "-01"))
  pdf(file.path(FIG, arquivo), width = 7.5, height = 4.5)
  par(mar = c(3, 4, 2.5, 1))
  yl <- range(c(dt[[col_media]], dt[[col_dp]]))
  plot(datas, dt[[col_media]], type = "l", col = "#2E5C8A", lwd = 1.8,
       ylim = yl, xlab = "", ylab = "erro (fração do peso)", main = titulo)
  lines(datas, dt[[col_dp]], col = "#8A2E2E", lwd = 1.8, lty = 2)
  abline(h = 0, col = "grey70", lty = 3)
  legend("topleft", legend = c("média mensal do erro", "desvio-padrão mensal do erro"),
         col = c("#2E5C8A", "#8A2E2E"), lwd = 1.8, lty = c(1,2), bty = "n", cex = 0.8)
  dev.off()
}

e_mes <- fread(file.path(REPO, "v2 OFICIAL/data/erro_cross_section_por_mes.csv"))
plot_evolucao(e_mes, "media_e", "dp_e",
              "Evolução mês a mês do erro e (Etapa 2, logit)",
              "fig_erro_logit_mensal.pdf")

u_mes <- fread(file.path(REPO, "v2 OFICIAL/data/ajuste_parcial_erros_por_mes.csv"))
plot_evolucao(u_mes, "media_u", "dp_u",
              "Evolução mês a mês do erro u (Etapa 3, ajuste parcial)",
              "fig_erro_ajuste_mensal.pdf")

cat("OK - salvos em 'v2 OFICIAL/figuras/fig_erro_logit_mensal.pdf' e 'fig_erro_ajuste_mensal.pdf'\n")
