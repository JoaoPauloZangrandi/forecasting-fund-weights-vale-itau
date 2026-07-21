# =============================================================================
# 22_fig_coef_gestora.R  (v2 OFICIAL)
#
# Evolucao mensal do coeficiente de gestora (vs. Itau) para as 5 gestoras
# mais positivas e as 5 mais negativas (por media, ver R/21) -- mostrar
# todas as 40 num grafico so seria ilegivel.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG  <- file.path(REPO, "v2 OFICIAL/figuras")

G <- fread(file.path(REPO, "v2 OFICIAL/data/coeficientes_gestora_mensal.csv"))
resumo <- fread(file.path(REPO, "v2 OFICIAL/data/coeficientes_gestora_resumo.csv"))
# so gestoras com cobertura razoavel (>=45 dos 59 meses), para a serie de
# tempo nao ficar cheia de buracos / dominada por 1-4 pontos isolados
resumo_cob <- resumo[n_meses >= 45]
setorder(resumo_cob, -media)

top5 <- head(resumo_cob$gestora, 5)
bot5 <- tail(resumo_cob$gestora, 5)
cat("Top 5 (mais positivas):", paste(top5, collapse=", "), "\n")
cat("Bottom 5 (mais negativas):", paste(bot5, collapse=", "), "\n")

meses_todos <- sort(unique(G$ym))
datas_todas <- as.Date(paste0(substr(meses_todos,1,4),"-",substr(meses_todos,5,6),"-01"))
cores <- c("#2E5C8A","#8A2E2E","#2E8A4E","#8A6D2E","#6B2E8A")

serie_mat <- function(gestoras) {
  m <- matrix(NA_real_, nrow = length(meses_todos), ncol = length(gestoras),
              dimnames = list(as.character(meses_todos), gestoras))
  for (g in gestoras) {
    gs <- G[gestora == g]
    m[as.character(gs$ym), g] <- gs$coef
  }
  m
}
m_top <- serie_mat(top5); m_bot <- serie_mat(bot5)

pdf(file.path(FIG, "fig_coef_gestora_evolucao.pdf"), width = 8, height = 7.5)
par(mfrow = c(2,1), mar = c(3,4,2.5,1))

plot(datas_todas, m_top[,1], type="n", ylim = range(m_top, na.rm=TRUE),
     xlab="", ylab="coeficiente vs. Itaú", main="As 5 gestoras com coeficiente mais POSITIVO (vs. Itaú)")
abline(h=0, col="grey70", lty=3)
for (i in seq_along(top5)) lines(datas_todas, m_top[,i], col=cores[i], lwd=1.8, type="o", pch=16, cex=0.5)
legend("topleft", legend=top5, col=cores, lwd=1.8, bty="n", cex=0.75, ncol=2)

plot(datas_todas, m_bot[,1], type="n", ylim = range(m_bot, na.rm=TRUE),
     xlab="", ylab="coeficiente vs. Itaú", main="As 5 gestoras com coeficiente mais NEGATIVO (vs. Itaú)")
abline(h=0, col="grey70", lty=3)
for (i in seq_along(bot5)) lines(datas_todas, m_bot[,i], col=cores[i], lwd=1.8, type="o", pch=16, cex=0.5)
legend("bottomleft", legend=bot5, col=cores, lwd=1.8, bty="n", cex=0.75, ncol=2)

dev.off()
cat("\nOK - salvo em 'v2 OFICIAL/figuras/fig_coef_gestora_evolucao.pdf'\n")
