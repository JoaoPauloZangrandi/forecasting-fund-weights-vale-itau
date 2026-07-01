# =============================================================================
# 29_fig_theta_padr.R — gera docs/fig_theta_padr.pdf (coeficientes PADRONIZADOS)
# para o tcc_novo, coerente com a Se��o 3.2 (ln PL e ln Cot padronizados; FIC e
# fluxo em escala natural; intercepto centrado = nivel medio). So p/ tcc_novo;
# NAO altera fig_theta.pdf (usado pelos outros PDFs).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
d <- fread(file.path(REPO, "data/processed/painel_vale_itau_2016_2021_filtrado.csv"))
d[, aum_num := as.numeric(aum)][, l_aum := log(aum_num)][, l_cot := log(n_cotistas)]
d[, flow_aum := fluxo_liq/aum_num][, ym := ano*100L+mes]
d <- d[!is.na(flow_aum)]
z <- function(x) (x-mean(x))/sd(x)
d[, z_aum := z(l_aum)][, z_cot := z(l_cot)]

meses <- sort(unique(d$ym))
co <- rbindlist(lapply(meses, function(m){
  cf <- coef(lm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum, data=d[ym==m]))
  data.table(ano=d[ym==m]$ano[1], mes=d[ym==m]$mes[1],
             intercepto=cf[[1]], b_aum=cf[[2]], b_cot=cf[[3]], b_fic=cf[[4]], b_flow=cf[[5]])
}))
dts <- as.Date(sprintf("%d-%02d-01", co$ano, co$mes))
series <- c(intercepto="intercepto", b_aum="log(AUM)", b_cot="log(cotistas)",
            b_fic="FIC", b_flow="fluxo/AUM")

pdf(file.path(REPO, "docs/fig_theta_padr.pdf"), width=9, height=6)
op <- par(mfrow=c(2,3), mar=c(3,4,2.5,1))
for (v in names(series)) {
  y <- co[[v]]
  plot(dts, y, type="l", lwd=2, col="steelblue", main=series[[v]], xlab="", ylab="coeficiente")
  abline(h=mean(y), lty=2, col="grey50")
}
plot.new(); legend("center", c("coeficiente mensal","media"), lwd=c(2,1), lty=c(1,2),
                   col=c("steelblue","grey50"), bty="n")
par(op); dev.off()
cat("OK - docs/fig_theta_padr.pdf gerado (coeficientes padronizados)\n")
