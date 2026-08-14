# =============================================================================
# 15_cio_validacao_fatores_custos.R  (exploracao_sinais)
#
# Validacao mais a fundo do CIO Peer Momentum (h=1, o unico candidato que
# sobreviveu ate aqui):
#
# (P) FAMA-MACBETH COM CONTROLES: regressao cross-sectional MES A MES de
#     retorno(t+1) ~ peer_ret(t) + tamanho(t) + beta(t) + retorno_proprio(t),
#     pegando 1 coeficiente de peer_ret por mes, testando a MEDIA desses
#     coeficientes (t-stat da propria serie temporal) -- e' o teste padrao-
#     ouro pra saber se um sinal tem poder preditivo incremental sobre
#     caracteristicas conhecidas (nao temos valor/book-to-market -- dado
#     nao disponivel no painel de holdings, registrar como limitacao).
#
# (Q) CUSTO DE TRANSACAO: quanto do spread bruto sobra depois de custos
#     realistas de negociacao no mercado brasileiro (corretagem+emolumentos
#     B3+spread bid-ask)? Testado em 4 niveis de custo round-trip.
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

# ---- tamanho: log(valor_total_mil) mantido pelo nosso universo de fundos, rank por mes ----
pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("ativo","ym","valor_mil"))
valor_total <- pp[, .(valor_total_mil = sum(valor_mil)), by = .(ativo, ym)]
valor_total[, ticker := trimws(sub(".*- ", "", ativo))]
valor_total[, log_tamanho := log(pmax(valor_total_mil, 1))]

# ---- beta: regressao trailing 12m de retorno do ativo vs retorno do "mercado" (media
#      equal-weighted do nosso universo, mesma convencao ja usada no projeto) ----
mercado <- precos[, .(ret_mercado = mean(retorno, na.rm=TRUE)), by = ymk]
pw <- merge(precos, mercado, by = "ymk")
setorder(pw, ticker, ymk)
calc_beta_trailing <- function(ret, ret_mkt) {
  n <- length(ret)
  beta_out <- rep(NA_real_, n)
  if (n < 12) return(beta_out)
  for (i in 12:n) {
    janela <- (i-11):i
    v <- var(ret_mkt[janela], na.rm=TRUE)
    if (is.finite(v) && v > 0) beta_out[i] <- cov(ret[janela], ret_mkt[janela], use="complete.obs")/v
  }
  beta_out
}
pw[, beta_trailing := calc_beta_trailing(retorno, ret_mercado), by = ticker]
beta_dt <- pw[!is.na(beta_trailing), .(ticker, ym = ymk, beta_trailing)]

# ---- retorno proprio (ja testado, incluido de novo aqui pra regressao conjunta) ----
ret_proprio <- precos[, .(ticker, ym = ymk, ret_proprio = retorno)]

# ---- junta tudo ----
h <- 1
m <- copy(cio_dt); m[, ym_ret := addm(ym, h)]
m <- merge(m, precos, by.x = c("ticker_ret","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
m <- merge(m, valor_total[, .(ticker, ym, log_tamanho)], by = c("ticker","ym"), all.x = TRUE)
m <- merge(m, beta_dt, by = c("ticker","ym"), all.x = TRUE)
m <- merge(m, ret_proprio, by = c("ticker","ym"), all.x = TRUE)
m <- m[is.finite(retorno) & is.finite(peer_ret) & is.finite(log_tamanho) & is.finite(beta_trailing) & is.finite(ret_proprio)]
cat("Obs com todos os controles disponiveis:", nrow(m), "\n")

# padroniza (z-score) os regressores DENTRO de cada mes, pra coeficientes comparaveis
m[, `:=`(peer_ret_z = scale(peer_ret), log_tamanho_z = scale(log_tamanho),
         beta_z = scale(beta_trailing), ret_proprio_z = scale(ret_proprio)), by = ym]

cat("\n===== (P) FAMA-MACBETH COM CONTROLES (regressao cross-sectional mes a mes) =====\n")
teste <- m[ym >= CORTE]
meses_fm <- sort(unique(teste$ym))
coefs_mensais <- list()
for (mm in meses_fm) {
  dm <- teste[ym == mm]
  if (nrow(dm) < 20) next
  fit <- tryCatch(lm(retorno ~ peer_ret_z + log_tamanho_z + beta_z + ret_proprio_z, data = dm), error=function(e) NULL)
  if (is.null(fit)) next
  cf <- coef(fit)
  coefs_mensais[[as.character(mm)]] <- data.table(ym = mm, n = nrow(dm), t(cf))
}
CM <- rbindlist(coefs_mensais, fill = TRUE)
cat("Meses com regressao valida:", nrow(CM), "\n\n")

fm_stat <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x); media <- mean(x); dp <- sd(x)
  t_stat <- media/(dp/sqrt(n)); p_val <- 2*pt(-abs(t_stat), df = n-1)
  data.table(media = media, dp = dp, n_meses = n, t = t_stat, p = p_val)
}
cat("Coeficiente medio (Fama-MacBeth) de cada variavel, controlando pelas outras 3:\n")
for (v in c("peer_ret_z","log_tamanho_z","beta_z","ret_proprio_z")) {
  r <- fm_stat(CM[[v]])
  cat(sprintf("  %-16s media=%+8.5f | t=%6.2f | p=%.4f | (%d meses)\n", v, r$media, r$t, r$p, r$n_meses))
}

# =============================================================================
# (Q) CUSTO DE TRANSACAO
# =============================================================================
cat("\n\n===== (Q) CUSTO DE TRANSACAO -- quanto sobra do spread bruto? =====\n")
treino_q <- m[ym < CORTE]
teste_q <- copy(m[ym >= CORTE])
breaks <- unique(quantile(treino_q$peer_ret, seq(0,1,0.2), na.rm = TRUE))
breaks[1] <- -Inf; breaks[length(breaks)] <- Inf
teste_q[, quintil := as.integer(cut(peer_ret, breaks, labels = FALSE, include.lowest = TRUE))]
por_mes <- teste_q[quintil %in% c(1,5), .(retorno_medio = mean(retorno), n=.N), by = .(ym, quintil)]
sm <- dcast(por_mes, ym ~ quintil, value.var = "retorno_medio")
setnames(sm, c("1","5"), c("q1","q5"))
sm <- sm[is.finite(q1) & is.finite(q5)]
sm[, spread_bruto := q5 - q1]
spread_medio_bruto <- mean(sm$spread_bruto)
cat(sprintf("Spread BRUTO medio: %.2f pp/mes (%.1f%%/ano)\n", 100*spread_medio_bruto, 100*((1+spread_medio_bruto)^12-1)))

# custo round-trip por PERNA (compra+venda de 1 posicao), reinvestido todo mes (turnover ~100%
# de cada lado a cada rebalanceamento -- premissa conservadora/pessimista mas realista pra
# long-short mensal sem otimizacao de turnover)
custos_bps <- c(30, 50, 100, 150)  # 0,30% a 1,50% round-trip por perna
cat("\nPremissa: rebalanceamento mensal completo (~100% turnover cada perna), custo round-trip aplicado 1x/mes por perna (long+short):\n")
for (c_bps in custos_bps) {
  custo_mensal <- c_bps/10000 * 2  # 2 pernas (long e short), cada uma paga o round-trip
  spread_liquido <- spread_medio_bruto - custo_mensal
  cat(sprintf("  Custo %.0fbps round-trip/perna -> custo total %.2fpp/mes -> spread LIQUIDO = %.2fpp/mes (%.1f%%/ano)\n",
              c_bps, 100*custo_mensal, 100*spread_liquido, 100*((1+spread_liquido)^12-1)))
}

fwrite(CM, file.path(OUT, "cio_fama_macbeth_controles.csv"))
cat("\nOK\n")
