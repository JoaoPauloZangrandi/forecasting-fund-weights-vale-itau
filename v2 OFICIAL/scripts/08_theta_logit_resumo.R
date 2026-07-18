# =============================================================================
# 08_theta_logit_resumo.R  (v2 OFICIAL)
#
# Estatisticas de theta_t (logit, Etapa 2) ao longo dos 72 meses -- mesmo
# formato da Tabela 1 (theta linear, Etapa 1) -- e figura com os 5
# componentes de theta_logit sobrepostos (padronizados), mesmo estilo da
# Figura 2.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG  <- file.path(REPO, "v2 OFICIAL/figuras")

theta <- fread(file.path(REPO, "v2 OFICIAL/data/theta_logit_mensal.csv"))
setorder(theta, ym)

cat("===== Theta_t (logit): estatisticas ao longo dos 72 meses =====\n")
resumo <- theta[, .(
  media = c(mean(alpha), mean(b_aum), mean(b_cot), mean(b_fic), mean(b_flow)),
  dp    = c(sd(alpha), sd(b_aum), sd(b_cot), sd(b_fic), sd(b_flow)),
  min   = c(min(alpha), min(b_aum), min(b_cot), min(b_fic), min(b_flow)),
  max   = c(max(alpha), max(b_aum), max(b_cot), max(b_fic), max(b_flow))
)]
resumo[, variavel := c("alpha (intercepto)", "b_aum", "b_cot", "b_fic", "b_flow")]
setcolorder(resumo, "variavel")
print(resumo[, .(variavel, media = round(media,4), dp = round(dp,4),
                  min = round(min,4), max = round(max,4))])

# ---- figura: os 5 componentes de theta_logit, padronizados, sobrepostos ----
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

pdf(file.path(FIG, "fig_theta_logit_combinado.pdf"), width = 8, height = 4.8)
par(mar = c(3, 4, 2.5, 1))
plot(datas, serie[[1]], type = "l", col = cores[1], lwd = 1.8,
     ylim = range(unlist(serie)), xlab = "", ylab = "theta logit padronizado (z-score)",
     main = "Os cinco componentes de theta_t (logit), padronizados, no mesmo gráfico")
for (i in 2:length(serie)) lines(datas, serie[[i]], col = cores[i], lwd = 1.8)
abline(h = 0, col = "grey70", lty = 2)
legend("topleft", legend = names(serie), col = cores, lwd = 1.8, bty = "n", cex = 0.75, ncol = 2)
dev.off()

fwrite(resumo, file.path(REPO, "v2 OFICIAL/data/theta_logit_resumo.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/figuras/fig_theta_logit_combinado.pdf' e 'data/theta_logit_resumo.csv'\n")
