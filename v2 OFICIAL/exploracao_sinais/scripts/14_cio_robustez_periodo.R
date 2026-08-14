# =============================================================================
# 14_cio_robustez_periodo.R  (exploracao_sinais)
#
# Ultima checagem critica do CIO peer momentum (h=1) antes de reportar como
# achado: sera que so existe por causa do periodo de volatilidade extrema
# (COVID, fev-dez/2020)? Testa excluindo esse periodo, e olha o spread
# mes a mes pra ver se esta concentrado ou distribuido.
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

h <- 1
m <- copy(cio_dt); m[, ym_ret := addm(ym, h)]
m <- merge(m, precos, by.x = c("ticker_ret","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
m <- m[is.finite(retorno) & is.finite(peer_ret)]
treino <- m[ym < CORTE]; teste <- copy(m[ym >= CORTE])

breaks <- unique(quantile(treino$peer_ret, seq(0,1,0.2), na.rm = TRUE))
breaks[1] <- -Inf; breaks[length(breaks)] <- Inf
teste[, quintil := as.integer(cut(peer_ret, breaks, labels = FALSE, include.lowest = TRUE))]

por_mes <- teste[quintil %in% c(1,5), .(retorno_medio = mean(retorno), n=.N), by = .(ym, quintil)]
sm <- dcast(por_mes, ym ~ quintil, value.var = "retorno_medio")
setnames(sm, c("1","5"), c("q1","q5"))
sm <- sm[is.finite(q1) & is.finite(q5)]
sm[, spread := q5 - q1]
setorder(sm, ym)
cat("===== Spread mes a mes (h=1) =====\n")
print(sm)

cat("\n===== Excluindo periodo COVID (retorno de fev-dez/2020, ou seja meses-base jan-nov/2020) =====\n")
sm_sem_covid <- sm[!ym %in% c(202001:202011)]
cat("Meses restantes:", nrow(sm_sem_covid), "\n")
if (nrow(sm_sem_covid) >= 6) {
  media <- mean(sm_sem_covid$spread); dp <- sd(sm_sem_covid$spread); n <- nrow(sm_sem_covid)
  t_stat <- media/(dp/sqrt(n)); p_val <- 2*pt(-abs(t_stat), df=n-1)
  cat(sprintf("Spread medio SEM 2020: %.4f (%.2fpp/mes) | t=%.2f | p=%.4f\n", media, 100*media, t_stat, p_val))
} else cat("Poucos meses sobrando\n")

cat("\n===== SO' 2021 (ano inteiro pos-crash, mais 'normal') =====\n")
sm_2021 <- sm[ym >= 202101]
cat("Meses:", nrow(sm_2021), "\n")
if (nrow(sm_2021) >= 6) {
  media <- mean(sm_2021$spread); dp <- sd(sm_2021$spread); n <- nrow(sm_2021)
  t_stat <- media/(dp/sqrt(n)); p_val <- 2*pt(-abs(t_stat), df=n-1)
  cat(sprintf("Spread medio 2021: %.4f (%.2fpp/mes) | t=%.2f | p=%.4f\n", media, 100*media, t_stat, p_val))
}

cat("\n===== Quantos meses o spread e' positivo? =====\n")
cat(sprintf("%d de %d meses (%.0f%%)\n", sum(sm$spread>0), nrow(sm), 100*mean(sm$spread>0)))

cat("\n===== Maior spread individual (mes que mais contribui) =====\n")
print(sm[order(-abs(spread))][1:3])

cat("\nOK\n")
