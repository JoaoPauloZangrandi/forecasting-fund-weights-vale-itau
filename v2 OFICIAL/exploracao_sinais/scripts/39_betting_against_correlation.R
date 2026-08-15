# =============================================================================
# 39_betting_against_correlation.R  (exploracao_sinais)
#
# Asness-Frazzini-Gormsen-Pedersen (2020, JFE) "Betting Against Correlation":
# decompoe beta = correlacao(acao,mercado) * [vol(acao)/vol(mercado)] e
# mostra que o componente de CORRELACAO (nao vol) carrega o efeito
# "low-risk". Construido so' com retornos -- sem opcoes. JA restrito ao
# universo LIQUIDO desde o inicio (licao do candidato #38: sinais que so'
# funcionam em small caps ilíquidas nao sao tradeable de verdade).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
setorder(precos, ticker, ymk)

mercado <- precos[, .(ret_mercado = mean(retorno, na.rm=TRUE)), by = ymk]
pw <- merge(precos, mercado, by = "ymk")
setorder(pw, ticker, ymk)

calc_corr_vol <- function(ret, ret_mkt) {
  n <- length(ret); corr_out <- rep(NA_real_, n); vol_out <- rep(NA_real_, n)
  if (n < 12) return(list(corr=corr_out, vol=vol_out))
  for (i in 12:n) {
    jan <- (i-11):i
    if (sum(is.finite(ret[jan])) >= 10) {
      corr_out[i] <- suppressWarnings(cor(ret[jan], ret_mkt[jan], use="complete.obs"))
      vol_out[i] <- sd(ret[jan], na.rm=TRUE)
    }
  }
  list(corr=corr_out, vol=vol_out)
}
pw[, c("corr_12m","vol_12m") := calc_corr_vol(retorno, ret_mercado), by = ticker]

pp0 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("ativo","ym","valor_mil"))
pp0[, ticker := trimws(sub(".*- ", "", ativo))]
liquidez <- pp0[, .(valor_total_mil = sum(valor_mil)), by = .(ticker, ym)]
liquidez[, rank_liquidez := frank(-valor_total_mil)/.N, by = ym]

base <- merge(pw[,.(ticker,ym=ymk,corr_12m,vol_12m)], liquidez, by=c("ticker","ym"))
ret_prox <- precos[, .(ticker, ym_alvo = addm(ymk, -1), retorno_prox = retorno)]
base <- merge(base, ret_prox, by.x=c("ticker","ym"), by.y=c("ticker","ym_alvo"))
base <- base[is.finite(corr_12m) & is.finite(vol_12m) & is.finite(retorno_prox) & ym >= CORTE]

sharpe <- function(r) mean(r,na.rm=TRUE)/sd(r,na.rm=TRUE)*sqrt(12)
max_drawdown <- function(r) { cum<-cumprod(1+r); dd<-cum/cummax(cum)-1; min(dd,na.rm=TRUE) }
resumo <- function(r, nome) {
  tt <- t.test(r)
  cat(sprintf("%-45s Sharpe=%7.3f | ret_anual=%7.2f%% | maxDD=%7.2f%% | t=%5.2f | p=%.4f | N=%d\n",
              nome, sharpe(r), mean(r,na.rm=TRUE)*12*100, max_drawdown(r)*100, tt$statistic, tt$p.value, sum(is.finite(r))))
  data.table(estrategia=nome, sharpe=sharpe(r), ret_anual_pct=mean(r,na.rm=TRUE)*12*100,
             max_dd_pct=max_drawdown(r)*100, t=tt$statistic, p=tt$p.value, n_meses=sum(is.finite(r)))
}

resultados <- list()
for (corte_liq in c(1/2, 2/3, 1)) {
  b <- base[rank_liquidez <= corte_liq]
  b[, tercil_corr := as.integer(cut(corr_12m, quantile(corr_12m, 0:3/3, na.rm=TRUE), include.lowest=TRUE)), by=ym]
  sm <- b[tercil_corr %in% c(1,3), .(ret=mean(retorno_prox), n=.N), by=.(ym,tercil_corr)]
  smw <- dcast(sm, ym~tercil_corr, value.var="ret")
  if (!all(c("1","3") %in% names(smw))) next
  setnames(smw, c("1","3"), c("baixa_corr","alta_corr"))
  smw <- smw[is.finite(baixa_corr) & is.finite(alta_corr)]
  smw[, bac := baixa_corr - alta_corr]  # long baixa-correlacao, short alta-correlacao
  cat(sprintf("\n--- Universo liquido top %.0f%% (n_medio=%.0f) ---\n", 100*corte_liq, mean(sm$n)))
  r <- resumo(smw$bac, sprintf("BAC (long baixa-corr, short alta-corr), liq top%.0f%%", 100*corte_liq))
  resultados[[length(resultados)+1]] <- r
  # tambem versao long-only: baixa-correlacao vs equal-weight do universo
  bench <- b[, .(ret_bench=mean(retorno_prox)), by=ym]
  compL <- merge(b[tercil_corr==1, .(ret_long=mean(retorno_prox)), by=ym], bench, by="ym")
  compL[, excesso := ret_long - ret_bench]
  rl <- resumo(compL$excesso, sprintf("Long-only baixa-corr MENOS benchmark, liq top%.0f%%", 100*corte_liq))
  resultados[[length(resultados)+1]] <- rl
}

cat("\n===== Extra: HHI prediz CORRELACAO futura? (conecta com Lou-Polk Comomentum/Greenwood-Thesmar) =====\n")
pp3 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp3[, ticker := trimws(sub(".*- ", "", ativo))]
pp3 <- pp3[is.finite(aum_prev) & aum_prev > 0]
pp3[, valor_posicao := peso * aum_prev]
crowd <- pp3[, .(hhi_posse = sum((valor_posicao/sum(valor_posicao))^2), n_fundos=.N), by = .(ticker, ym)]
crowd <- crowd[n_fundos >= 10]
bc <- merge(crowd[,.(ticker,ym,hhi_posse)], pw[,.(ticker,ym=ymk,corr_12m)], by=c("ticker","ym"))
corr_fut <- pw[, .(ticker, ym_alvo = addm(ymk,-1), corr_futura = corr_12m)]
bc <- merge(bc, corr_fut, by.x=c("ticker","ym"), by.y=c("ticker","ym_alvo"))
bc <- bc[is.finite(hhi_posse) & is.finite(corr_futura) & ym >= CORTE]
meses <- sort(unique(bc$ym)); coefs <- list()
for (mm in meses) {
  dm <- bc[ym==mm]
  if (nrow(dm) < 30) next
  fit <- lm(corr_futura ~ hhi_posse, data=dm)
  coefs[[as.character(mm)]] <- coef(fit)[2]
}
cf <- unlist(coefs); n<-length(cf); media<-mean(cf); dp<-sd(cf); t_fm<-media/(dp/sqrt(n)); p_fm<-2*pt(-abs(t_fm),df=n-1)
cat(sprintf("HHI_t -> correlacao futura (t+1): coef_medio=%+.4f | t=%.2f | p=%.4f | (%d meses)\n", media, t_fm, p_fm, n))

R <- rbindlist(resultados)
fwrite(R, file.path(OUT, "candidatos_39_bac.csv"))
cat("\nOK\n")
