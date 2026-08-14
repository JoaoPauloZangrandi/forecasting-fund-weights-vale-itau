# =============================================================================
# 13_diagnostico_cio_peer_momentum.R  (exploracao_sinais)
#
# O candidato CIO peer momentum (script 11) passou de 5% em h=1 (t=2,51,
# p=0,021) com amostra saudavel (nao degenerada como o Comomentum). Antes
# de acreditar, testar a ressalva da literatura (Burt & Hrdlicka 2021):
# parte da previsibilidade "via rede de ownership" pode ser comovimento
# MECANICO (peers correlacionados por fator de risco comum -- setor,
# tamanho, beta -- nao por difusao de informacao real). Teste decisivo:
# peer_ret ainda prediz retorno(t+1) DEPOIS de controlar pelo retorno
# PROPRIO da acao no mesmo mes (que ja capta boa parte do fator de risco
# comum contemporaneo)? Se o coeficiente de peer_ret sumir ao controlar,
# e' mecanico. Se sobreviver, e' evidencia mais forte de sinal genuino.
#
# Tambem: checa o mes com N=1 no quintil (silenciado no script 11), e
# testa robustez excluindo esse mes.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

cio_dt <- fread(file.path(OUT, "cio_peer_return.csv"))
cio_dt[, ticker_ret := ticker]

# retorno PROPRIO da acao no mesmo mes t (pra controlar)
ret_proprio <- precos[, .(ticker, ym = ymk, ret_proprio = retorno)]
cio_dt <- merge(cio_dt, ret_proprio, by.x = c("ticker","ym"), by.y = c("ticker","ym"))

h <- 1
m <- copy(cio_dt); m[, ym_ret := addm(ym, h)]
m <- merge(m, precos, by.x = c("ticker_ret","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
m <- m[is.finite(retorno) & is.finite(peer_ret) & is.finite(ret_proprio)]

treino <- m[ym < CORTE]; teste <- m[ym >= CORTE]
cat("===== TESTE DECISIVO: peer_ret sobrevive controlando por retorno PROPRIO? =====\n")
cat("(h=1, mesma amostra do script 11)\n\n")

fit_so_peer <- lm(retorno ~ peer_ret, data = treino)
fit_so_proprio <- lm(retorno ~ ret_proprio, data = treino)
fit_ambos <- lm(retorno ~ peer_ret + ret_proprio, data = treino)

cat("--- So peer_ret ---\n"); print(round(summary(fit_so_peer)$coefficients,5))
cat("R2:", round(summary(fit_so_peer)$r.squared,5), "\n\n")

cat("--- So retorno proprio (autocorrelacao simples) ---\n"); print(round(summary(fit_so_proprio)$coefficients,5))
cat("R2:", round(summary(fit_so_proprio)$r.squared,5), "\n\n")

cat("--- Os DOIS juntos (teste decisivo) ---\n"); print(round(summary(fit_ambos)$coefficients,5))
cat("R2:", round(summary(fit_ambos)$r.squared,5), "\n\n")

# teste OOS com os dois juntos
pred_ambos <- predict(fit_ambos, newdata = teste)
sse_ambos <- sum((teste$retorno - pred_ambos)^2)
sse_naive <- sum((teste$retorno - mean(treino$retorno))^2)
r2_oos_ambos <- 1 - sse_ambos/sse_naive
cat("R2_OOS (peer_ret + ret_proprio juntos):", round(100*r2_oos_ambos,4), "%\n\n")

pred_so_proprio <- predict(fit_so_proprio, newdata = teste)
sse_so_proprio <- sum((teste$retorno - pred_so_proprio)^2)
r2_oos_so_proprio <- 1 - sse_so_proprio/sse_naive
cat("R2_OOS (SO ret_proprio, sem peer_ret):", round(100*r2_oos_so_proprio,4), "%\n")
cat("R2_OOS (SO peer_ret, do script 11): 1.5467% [referencia]\n\n")

cat("===== CHECAGEM DO MES COM N=1 NO QUINTIL =====\n")
breaks <- unique(quantile(treino$peer_ret, seq(0,1,0.2), na.rm = TRUE))
breaks[1] <- -Inf; breaks[length(breaks)] <- Inf
teste2 <- copy(teste)
teste2[, quintil := as.integer(cut(peer_ret, breaks, labels = FALSE, include.lowest = TRUE))]
n_por_mes <- teste2[quintil %in% c(1,5), .N, by = .(ym, quintil)]
setorder(n_por_mes, N)
print(head(n_por_mes, 5))
mes_degenerado <- n_por_mes[N == min(N)]$ym[1]
cat("Mes com N minimo:", mes_degenerado, "\n\n")

cat("===== ROBUSTEZ: Fama-MacBeth excluindo o mes com N minimo =====\n")
por_mes <- teste2[quintil %in% c(1,5), .(retorno_medio = mean(retorno)), by = .(ym, quintil)]
sm <- dcast(por_mes, ym ~ quintil, value.var = "retorno_medio")
setnames(sm, c("1","5"), c("q1","q5"))
sm <- sm[is.finite(q1) & is.finite(q5)]
sm[, spread := q5 - q1]
cat("COM todos os meses:\n")
media0 <- mean(sm$spread); dp0 <- sd(sm$spread); n0 <- nrow(sm)
t0 <- media0/(dp0/sqrt(n0)); p0 <- 2*pt(-abs(t0), df=n0-1)
cat(sprintf("  %d meses | spread=%.2fpp/mes | t=%.2f | p=%.4f\n", n0, 100*media0, t0, p0))

sm_sem <- sm[ym != mes_degenerado]
media1 <- mean(sm_sem$spread); dp1 <- sd(sm_sem$spread); n1 <- nrow(sm_sem)
t1 <- media1/(dp1/sqrt(n1)); p1 <- 2*pt(-abs(t1), df=n1-1)
cat(sprintf("SEM o mes degenerado (%d): %d meses | spread=%.2fpp/mes | t=%.2f | p=%.4f\n",
            mes_degenerado, n1, 100*media1, t1, p1))

cat("\nOK\n")
