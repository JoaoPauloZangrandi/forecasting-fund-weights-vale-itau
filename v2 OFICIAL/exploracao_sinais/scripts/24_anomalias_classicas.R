# =============================================================================
# 24_anomalias_classicas.R  (exploracao_sinais)
#
# NOVA FAMILIA de candidatos -- nao mais variacoes de holdings/fluxo, mas
# anomalias classicas de asset pricing, construidas so' com preco (nao
# precisam de holdings), testadas (a) sozinhas e (b) interagidas com
# ownership institucional (nosso dado de holdings ja calculado) -- pergunta:
# será que a presenca de fundos no nosso painel FORTALECE ou ENFRAQUECE
# essas anomalias classicas (limits to arbitrage)?
#
# (AA) 52-WEEK HIGH (George & Hwang 2004): proximidade ao preco maximo dos
#      ultimos 12 meses. Prediz retorno POSITIVO (momentum via ancoragem).
# (BB) REVERSAO DE CURTO PRAZO (Jegadeesh 1990): retorno do mes anterior.
#      Prediz retorno NEGATIVO no mes seguinte (overreaction de curto prazo).
# (CC) VOLATILIDADE IDIOSSINCRATICA (Ang-Hodrick-Xing-Zhang 2006): dp do
#      residuo (retorno - retorno de mercado) nos ultimos 12 meses. Prediz
#      retorno NEGATIVO (puzzle -- alta vol idiossincratica, retorno baixo).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","preco","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/260

setorder(precos, ticker, ymk)

# ---- (AA) 52-week high ----
precos[, preco_max_12m := frollapply(preco, 12, max, align="right"), by = ticker]
precos[, prox_52w_high := preco / preco_max_12m]

# ---- (BB) retorno do mes anterior (reversao) ----
precos[, ret_mes_anterior := shift(retorno, 1), by = ticker]

# ---- (CC) volatilidade idiossincratica ----
mercado <- precos[, .(ret_mercado = mean(retorno, na.rm=TRUE)), by = ymk]
pw <- merge(precos, mercado, by = "ymk")
setorder(pw, ticker, ymk)
calc_idio_vol <- function(ret, ret_mkt) {
  n <- length(ret); out <- rep(NA_real_, n)
  if (n < 12) return(out)
  for (i in 12:n) {
    jan <- (i-11):i
    fit <- tryCatch(lm(ret[jan] ~ ret_mkt[jan]), error=function(e) NULL)
    if (!is.null(fit)) out[i] <- sd(residuals(fit))
  }
  out
}
pw[, idio_vol_12m := calc_idio_vol(retorno, ret_mercado), by = ticker]

# ---- ownership institucional (valor total mantido pelos fundos, proxy ja usada) ----
pp0 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("ativo","ym","valor_mil"))
pp0[, ticker := trimws(sub(".*- ", "", ativo))]
valor_total <- pp0[, .(valor_total_mil = sum(valor_mil)), by = .(ticker, ym)]
valor_total[, own_rank := frank(valor_total_mil) / .N, by = ym]  # 0 a 1

base <- merge(precos[, .(ticker, ym=ymk, prox_52w_high, ret_mes_anterior)],
              pw[, .(ticker, ym=ymk, idio_vol_12m)], by = c("ticker","ym"))
base <- merge(base, valor_total[, .(ticker, ym, own_rank)], by = c("ticker","ym"))

fama_macbeth <- function(m, var_x, var_y, nome, h) {
  treino <- m[ym < CORTE]; teste <- copy(m[ym >= CORTE])
  if (nrow(treino) < 50 || nrow(teste) < 30) return(NULL)
  breaks <- unique(quantile(treino[[var_x]], seq(0,1,0.2), na.rm = TRUE))
  if (length(breaks) < 4) return(NULL)
  breaks[1] <- -Inf; breaks[length(breaks)] <- Inf
  teste[, quintil := as.integer(cut(get(var_x), breaks, labels = FALSE, include.lowest = TRUE))]
  ng <- length(breaks)-1
  if (ng<5) teste[, quintil := ifelse(quintil==1,1,ifelse(quintil==ng,5,quintil))]
  teste <- teste[!is.na(quintil)]
  por_mes <- teste[quintil %in% c(1,5), .(rm=mean(get(var_y))), by=.(ym,quintil)]
  sm <- dcast(por_mes, ym~quintil, value.var="rm")
  if (!all(c("1","5") %in% names(sm))) return(NULL)
  setnames(sm, c("1","5"), c("q1","q5")); sm <- sm[is.finite(q1)&is.finite(q5)]
  sm[, spread:=q5-q1]; nmes <- nrow(sm)
  if (nmes < 6) return(NULL)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(nmes)); p_fm <- 2*pt(-abs(t_fm), df=nmes-1)
  cat(sprintf("%-30s h=%d %2d meses spread=%+7.2fpp/mes t=%6.2f p=%.5f | Bonferroni:%s\n",
              nome, h, nmes, 100*media, t_fm, p_fm, ifelse(p_fm<LIMIAR_BONFERRONI,"SIM","nao")))
  data.table(sinal=nome, horizonte=h, n_meses=nmes, spread_pp=100*media, t_fm=t_fm, p_fm=p_fm,
             sig_bonferroni = p_fm<LIMIAR_BONFERRONI)
}

resultados <- list()

cat("===== (AA) 52-WEEK HIGH =====\n")
for (h in c(1,3,6)) {
  m <- copy(base); m[, ym_ret := addm(ym,h)]
  m <- merge(m, precos[,.(ticker,ymk,retorno)], by.x=c("ticker","ym_ret"), by.y=c("ticker","ymk"))
  m <- m[is.finite(retorno) & is.finite(prox_52w_high)]
  r <- fama_macbeth(m, "prox_52w_high", "retorno", "52-week high", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

cat("\n===== (BB) REVERSAO DE CURTO PRAZO =====\n")
for (h in c(1,3,6)) {
  m <- copy(base); m[, ym_ret := addm(ym,h)]
  m <- merge(m, precos[,.(ticker,ymk,retorno)], by.x=c("ticker","ym_ret"), by.y=c("ticker","ymk"))
  m <- m[is.finite(retorno) & is.finite(ret_mes_anterior)]
  r <- fama_macbeth(m, "ret_mes_anterior", "retorno", "Reversao curto prazo", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

cat("\n===== (CC) VOLATILIDADE IDIOSSINCRATICA =====\n")
for (h in c(1,3,6)) {
  m <- copy(base); m[, ym_ret := addm(ym,h)]
  m <- merge(m, precos[,.(ticker,ymk,retorno)], by.x=c("ticker","ym_ret"), by.y=c("ticker","ymk"))
  m <- m[is.finite(retorno) & is.finite(idio_vol_12m)]
  r <- fama_macbeth(m, "idio_vol_12m", "retorno", "Idio-vol", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

cat("\n===== INTERACAO: as 3 anomalias sao MAIS FORTES em BAIXA ownership institucional? =====\n")
for (var_x in c("prox_52w_high","ret_mes_anterior","idio_vol_12m")) {
  for (h in c(1,3)) {
    m <- copy(base); m[, ym_ret := addm(ym,h)]
    m <- merge(m, precos[,.(ticker,ymk,retorno)], by.x=c("ticker","ym_ret"), by.y=c("ticker","ymk"))
    m <- m[is.finite(retorno) & is.finite(get(var_x)) & is.finite(own_rank)]
    baixa <- m[own_rank <= 1/3]; alta <- m[own_rank >= 2/3]
    r1 <- fama_macbeth(baixa, var_x, "retorno", paste0(var_x,"|BAIXA-own"), h)
    r2 <- fama_macbeth(alta, var_x, "retorno", paste0(var_x,"|ALTA-own"), h)
    if (!is.null(r1)) resultados[[length(resultados)+1]] <- r1
    if (!is.null(r2)) resultados[[length(resultados)+1]] <- r2
  }
}

R <- rbindlist(resultados)
fwrite(R, file.path(OUT, "candidatos_24_anomalias_classicas.csv"))
cat("\n\n===== RESUMO GERAL =====\n")
print(R[order(p_fm)][, .(sinal, horizonte, n_meses, spread_pp=round(spread_pp,2), t_fm=round(t_fm,2), p_fm=round(p_fm,5), sig_bonferroni)])
cat("\nOK\n")
