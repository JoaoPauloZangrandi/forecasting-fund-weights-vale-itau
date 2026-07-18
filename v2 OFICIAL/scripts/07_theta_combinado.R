# =============================================================================
# 07_theta_combinado.R  (v2 OFICIAL)
#
# Figura extra: os 5 componentes de theta_t (Etapa 1) num unico grafico, para
# comparar a evolucao relativa entre eles. Como as escalas brutas sao muito
# diferentes (alpha ~0,01-0,07; b_flow ~-0,12-0,09), cada serie e padronizada
# (z-score) antes de entrar no mesmo eixo -- senao uma serie domina o grafico
# e as outras ficam achatadas perto de zero.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG  <- file.path(REPO, "v2 OFICIAL/figuras")

theta <- fread(file.path(REPO, "v2 OFICIAL/data/theta_mensal.csv"))
setorder(theta, ym)
datas <- as.Date(paste0(substr(theta$ym,1,4), "-", substr(theta$ym,5,6), "-01"))

z <- function(x) (x - mean(x)) / sd(x)
serie <- list(
  "alpha (intercepto)"      = z(theta$alpha),
  "b_aum (tamanho)"         = z(theta$b_aum),
  "b_cot (cotistas)"        = z(theta$b_cot),
  "b_fic (fundo de cotas)"  = z(theta$b_fic),
  "b_flow (captação/resgate)" = z(theta$b_flow)
)
cores <- c("#2E5C8A", "#8A2E2E", "#2E8A4E", "#8A6D2E", "#6B2E8A")

pdf(file.path(FIG, "fig_theta_combinado.pdf"), width = 8, height = 4.8)
par(mar = c(3, 4, 2.5, 1))
plot(datas, serie[[1]], type = "l", col = cores[1], lwd = 1.8,
     ylim = range(unlist(serie)), xlab = "", ylab = "theta padronizado (z-score)",
     main = "Os cinco componentes de theta_t, padronizados, no mesmo gráfico")
for (i in 2:length(serie)) lines(datas, serie[[i]], col = cores[i], lwd = 1.8)
abline(h = 0, col = "grey70", lty = 2)
legend("topleft", legend = names(serie), col = cores, lwd = 1.8, bty = "n", cex = 0.75, ncol = 2)
dev.off()

cat("OK - salvo em 'v2 OFICIAL/figuras/fig_theta_combinado.pdf'\n")
