# =============================================================================
# 17_forecast_weights.R
#
# Passo 2 (previsao): preve o peso de VALE3 de cada fundo 1 mes a frente e monta
# a MATRIZ DE ERROS. Tres metodos:
#   (A) Fator + RW : peso_hat = theta_{t-1} . x_{i,t}
#   (B) Fator + AR(1): peso_hat = theta_hat_t(AR1) . x_{i,t}
#   (C) Ingenuo : peso_hat = peso_{i,t-1} (peso do proprio fundo no mes anterior)
# theta_t vem da cross-section mes a mes (reg14). x = caracteristicas do fundo.
# OOS a partir do 13o mes (>=12 meses p/ ajustar AR(1)). RODAR COM CAMINHO ABSOLUTO.
# =============================================================================

suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

co <- fread(file.path(REPO, "data/processed/reg14_coefs_2016_2021.csv")); setorder(co, ym)
d  <- fread(file.path(REPO, "data/processed/painel_vale_itau_2016_2021_filtrado.csv"))
d[, aum_num := as.numeric(aum)][, l_aum := log(aum_num)][, l_cot := log(n_cotistas)]
d[, flow_aum := fluxo_liq / aum_num][, ym := ano * 100L + mes]
d <- d[!is.na(flow_aum)]

months <- sort(unique(co$ym)); cf <- c("intercepto","b_l_aum","b_l_cot","b_is_fic","b_flow_aum")
ar1_fc <- function(h) { n <- length(h); if (n < 4) return(h[n])
  a <- lm(h[-1] ~ h[-n]); as.numeric(coef(a)[1] + coef(a)[2] * h[n]) }

err <- list()
for (i in 13:length(months)) {
  t <- months[i]; hist <- co[ym < t]
  th_rw  <- as.numeric(hist[.N, ..cf])
  th_ar1 <- sapply(cf, function(v) ar1_fc(hist[[v]]))
  X <- d[ym == t, .(cod_fundo, peso = peso_vale3, one = 1, l_aum, l_cot, is_fic, flow_aum)]
  M <- as.matrix(X[, .(one, l_aum, l_cot, is_fic, flow_aum)])
  X[, pred_rw  := as.numeric(M %*% th_rw)]
  X[, pred_ar1 := as.numeric(M %*% th_ar1)]
  prev <- d[ym == months[i - 1], .(cod_fundo, peso_prev = peso_vale3)]
  X <- merge(X, prev, by = "cod_fundo", all.x = TRUE)
  err[[length(err) + 1L]] <- X[, .(ym = t, cod_fundo, peso,
                                    e_rw = peso - pred_rw, e_ar1 = peso - pred_ar1,
                                    e_naive = peso - peso_prev)]
}
E <- rbindlist(err)

rmse <- function(x) sqrt(mean(x^2, na.rm = TRUE))
cat("==== PREVISAO DO PESO 1 mes a frente (OOS, 60 meses) ====\n")
cat("Obs avaliadas:", nrow(E), "\n")
cat(sprintf("RMSE  Fator+RW = %.5f | Fator+AR(1) = %.5f | Ingenuo(lag) = %.5f\n",
            rmse(E$e_rw), rmse(E$e_ar1), rmse(E$e_naive)))
cat(sprintf("MAE   Fator+RW = %.5f | Fator+AR(1) = %.5f | Ingenuo(lag) = %.5f\n",
            mean(abs(E$e_rw), na.rm=TRUE), mean(abs(E$e_ar1), na.rm=TRUE), mean(abs(E$e_naive), na.rm=TRUE)))

cat("\n==== MATRIZ DE ERROS (Fator+AR1): propriedades ====\n")
cat("media do erro:", round(mean(E$e_ar1), 6), "(ideal ~0)\n")
cat("desvio-padrao:", round(sd(E$e_ar1), 5), "\n")
mm <- E[, .(media = mean(e_ar1), dp = sd(e_ar1)), by = ym][order(ym)]
cat("media do erro por mes ~0? range das medias mensais:",
    round(range(mm$media), 5), "\n")
# estacionariedade do erro medio mensal (DF)
x <- mm$media; n <- length(x); dft <- summary(lm(diff(x) ~ x[-n]))$coefficients[2,"t value"]
cat("DF t do erro medio mensal:", round(dft, 2), "(< -2.9 ~ estacionario)\n")

fwrite(E, file.path(REPO, "data/processed/reg17_error_matrix.csv"))
cat("\nOK - matriz de erros salva em data/processed/reg17_error_matrix.csv\n")
