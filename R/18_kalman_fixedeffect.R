# =============================================================================
# 18_kalman_fixedeffect.R
#
# Passo 2 (refino): efeito-fixo de fundo + Kalman. Compara, OOS (janela
# expansiva, 1 mes a frente), modelos de previsao do peso:
#   M0 Ingenuo        : w_{i,t-1}
#   M1 Efeito-fixo    : mu_i (media expansiva do fundo)
#   M2 FE + Fator/Kalman: mu_i + theta_t^Kal . (x_{i,t} - xbar_i)
#   M3 Variacao (dw)  : w_{i,t-1} + delta'.features  (preve a MUDANCA)
# Kalman = local-level (estado RW + ruido de medida) ajustado por MV nas 72
# series de coeficientes (reg14). RODAR COM CAMINHO ABSOLUTO.
# =============================================================================

suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

co <- fread(file.path(REPO, "data/processed/reg14_coefs_2016_2021.csv")); setorder(co, ym)
d  <- fread(file.path(REPO, "data/processed/painel_vale_itau_2016_2021_filtrado.csv"))
d[, aum_num := as.numeric(aum)][, l_aum := log(aum_num)][, l_cot := log(n_cotistas)]
d[, flow_aum := fluxo_liq / aum_num][, ym := ano * 100L + mes]
d <- d[!is.na(flow_aum)]
# peso defasado (mes calendario anterior)
d[, ym_prev := ifelse(mes == 1L, (ano - 1L) * 100L + 12L, ym - 1L)]
lagw <- d[, .(cod_fundo, ymj = ym, peso_prev = peso_vale3)]
d <- merge(d, lagw, by.x = c("cod_fundo","ym_prev"), by.y = c("cod_fundo","ymj"), all.x = TRUE)
d[, dw := peso_vale3 - peso_prev]

# ---- Kalman local-level: estima Q,R por MV; devolve previsao 1-passo causal ----
nll <- function(par, y) {
  Q <- exp(par[1]); R <- exp(par[2]); a <- y[1]; P <- 1e7; ll <- 0
  for (t in seq_along(y)) {
    F <- P + R; v <- y[t] - a; ll <- ll - 0.5 * (log(2*pi*F) + v^2/F)
    K <- P/F; a <- a + K*v; P <- (1 - K)*P + Q
  }
  -ll
}
kal_pred <- function(y) {                      # pred[t] = E[theta_t | y_{<t}]
  fit <- optim(c(log(var(diff(y))+1e-12), log(var(y)+1e-12)), nll, y = y, method = "Nelder-Mead")
  Q <- exp(fit$par[1]); R <- exp(fit$par[2]); a <- y[1]; P <- 1e7; pred <- numeric(length(y))
  for (t in seq_along(y)) {
    pred[t] <- a
    F <- P + R; v <- y[t] - a; K <- P/F; a <- a + K*v; P <- (1 - K)*P + Q
  }
  pred
}
slopes <- c("b_l_aum","b_l_cot","b_is_fic","b_flow_aum")
kal <- as.data.table(lapply(slopes, function(v) kal_pred(co[[v]]))); setnames(kal, slopes)
kal[, ym := co$ym]

months <- sort(unique(d$ym)); xv <- c("l_aum","l_cot","is_fic","flow_aum")
E <- list()
for (i in 13:length(months)) {
  t <- months[i]; tr <- d[ym < t]
  mu  <- tr[, .(mu = mean(peso_vale3)), by = cod_fundo]
  xb  <- tr[, lapply(.SD, mean), by = cod_fundo, .SDcols = xv]
  setnames(xb, xv, paste0(xv, "_b"))
  X <- d[ym == t]
  X <- merge(X, mu, by = "cod_fundo"); X <- merge(X, xb, by = "cod_fundo")  # so fundos com historico
  kt <- as.numeric(kal[ym == t, ..slopes])
  X[, pred_fe   := mu]
  X[, pred_fek  := mu + kt[1]*(l_aum - l_aum_b) + kt[2]*(l_cot - l_cot_b) +
                        kt[3]*(is_fic - is_fic_b) + kt[4]*(flow_aum - flow_aum_b)]
  # M3 variacao: regride dw nas features no treino, preve a mudanca
  fitd <- lm(dw ~ l_aum + l_cot + is_fic + flow_aum, data = tr[!is.na(dw)])
  X[, pred_dw := peso_prev + as.numeric(predict(fitd, X))]
  E[[length(E)+1L]] <- X[, .(ym = t, cod_fundo, peso = peso_vale3,
        e_naive = peso_vale3 - peso_prev, e_fe = peso_vale3 - pred_fe,
        e_fek = peso_vale3 - pred_fek, e_dw = peso_vale3 - pred_dw)]
}
E <- rbindlist(E); E <- E[!is.na(e_naive)]   # avalia onde todos tem o lag
rmse <- function(x) sqrt(mean(x^2, na.rm=TRUE)); mae <- function(x) mean(abs(x), na.rm=TRUE)

cat("==== PREVISAO DO PESO 1 mes a frente (OOS) ====\n")
cat("Obs avaliadas:", nrow(E), "\n")
cat(sprintf("%-26s RMSE=%.5f  MAE=%.5f\n", "M0 Ingenuo (lag)",     rmse(E$e_naive), mae(E$e_naive)))
cat(sprintf("%-26s RMSE=%.5f  MAE=%.5f\n", "M1 Efeito-fixo (mu_i)", rmse(E$e_fe),   mae(E$e_fe)))
cat(sprintf("%-26s RMSE=%.5f  MAE=%.5f\n", "M2 FE + Fator/Kalman", rmse(E$e_fek),  mae(E$e_fek)))
cat(sprintf("%-26s RMSE=%.5f  MAE=%.5f\n", "M3 Variacao dw (fluxo)",rmse(E$e_dw),   mae(E$e_dw)))

best <- c("e_naive","e_fe","e_fek","e_dw")[which.min(c(rmse(E$e_naive),rmse(E$e_fe),rmse(E$e_fek),rmse(E$e_dw)))]
cat("\nMelhor:", best, "\n")
cat("\n==== MATRIZ DE ERROS (melhor modelo) ====\n")
cat("media:", round(mean(E[[best]]),6), "| dp:", round(sd(E[[best]]),5), "\n")
mm <- E[, .(m = mean(get(best))), by = ym]; x <- mm$m; n <- length(x)
cat("DF t do erro medio mensal:", round(summary(lm(diff(x)~x[-n]))$coefficients[2,3],2), "\n")
fwrite(E, file.path(REPO, "data/processed/reg18_forecast_compare.csv"))
cat("\nOK - salvo em data/processed/reg18_forecast_compare.csv\n")
