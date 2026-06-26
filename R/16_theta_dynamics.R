# =============================================================================
# 16_theta_dynamics.R
#
# Passo 2 (sub-passo descritivo): caracteriza a dinamica dos 72 coeficientes
# theta_t da cross-section (reg14). Para cada serie:
#   - persistencia (autocorrelacao lag-1) e AR(1) phi
#   - teste tipo Dickey-Fuller (random walk vs reversao a media)
#   - previsao pseudo-OOS: random walk vs media vs AR(1) (RMSE)
# Gera o grafico docs/fig_theta.pdf. RODAR COM CAMINHO ABSOLUTO.
# =============================================================================

suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

co <- fread(file.path(REPO, "data/processed/reg14_coefs_2016_2021.csv"))
setorder(co, ym)
series <- c(intercepto = "intercepto", b_l_aum = "log(AUM)", b_l_cot = "log(cotistas)",
            b_is_fic = "FIC", b_flow_aum = "fluxo/AUM")

# --- (1) estatisticas de dinamica ---
dyn <- rbindlist(lapply(names(series), function(v) {
  x <- co[[v]]; n <- length(x)
  ac1 <- cor(x[-1], x[-n])                         # autocorrelacao lag-1
  fit <- lm(x[-1] ~ x[-n]); phi <- coef(fit)[2]    # AR(1): x_t = c + phi x_{t-1}
  # Dickey-Fuller: d_x = a + rho x_{t-1}; t(rho) vs ~ -2.9 (5%)
  dx <- diff(x); df <- lm(dx ~ x[-n]); s <- summary(df)$coefficients
  data.table(serie = series[[v]], media = mean(x), dp = sd(x),
             autocorr1 = ac1, ar1_phi = phi,
             df_t = s[2, "t value"])
}))

# --- (2) previsao pseudo-OOS (janela expansiva a partir de t=24) ---
oos <- function(x, start = 24L) {
  n <- length(x); e_rw <- e_mu <- e_ar <- numeric(0)
  for (t in (start + 1L):n) {
    h <- x[1:(t - 1L)]
    f_rw <- h[length(h)]; f_mu <- mean(h)
    f_ar <- tryCatch({ a <- lm(h[-1] ~ h[-length(h)]); as.numeric(coef(a)[1] + coef(a)[2] * h[length(h)]) },
                     error = function(e) f_rw)
    e_rw <- c(e_rw, x[t] - f_rw); e_mu <- c(e_mu, x[t] - f_mu); e_ar <- c(e_ar, x[t] - f_ar)
  }
  c(rmse_rw = sqrt(mean(e_rw^2)), rmse_media = sqrt(mean(e_mu^2)), rmse_ar1 = sqrt(mean(e_ar^2)))
}
fc <- rbindlist(lapply(names(series), function(v) {
  r <- oos(co[[v]]); data.table(serie = series[[v]],
    rmse_rw = r[["rmse_rw"]], rmse_media = r[["rmse_media"]], rmse_ar1 = r[["rmse_ar1"]],
    melhor = c("RW","Media","AR1")[which.min(r)])
}))

cat("==== DINAMICA DOS theta_t (72 meses) ====\n")
print(dyn[, lapply(.SD, function(z) if (is.numeric(z)) round(z, 4) else z)])
cat("\n(DF t < -2.9 ~ rejeita raiz unitaria -> reversao a media; phi << 1 idem)\n")
cat("\n==== PREVISAO PSEUDO-OOS (RMSE, janela expansiva) ====\n")
print(fc[, lapply(.SD, function(z) if (is.numeric(z)) signif(z, 3) else z)])

# --- (3) grafico das series ---
pdf(file.path(REPO, "docs/fig_theta.pdf"), width = 9, height = 6)
op <- par(mfrow = c(2, 3), mar = c(3, 4, 2.5, 1))
dts <- as.Date(sprintf("%d-%02d-01", co$ano, co$mes))
for (v in names(series)) {
  plot(dts, co[[v]], type = "l", lwd = 2, col = "steelblue",
       main = series[[v]], xlab = "", ylab = "coeficiente")
  abline(h = mean(co[[v]]), lty = 2, col = "grey50")
}
plot.new(); legend("center", c("coeficiente mensal", "media"), lwd = c(2, 1),
                   lty = c(1, 2), col = c("steelblue", "grey50"), bty = "n")
par(op); dev.off()
cat("\nGrafico salvo em docs/fig_theta.pdf\n")

fwrite(dyn, file.path(REPO, "data/processed/reg16_theta_dynamics.csv"))
fwrite(fc,  file.path(REPO, "data/processed/reg16_theta_forecast.csv"))
