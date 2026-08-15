# =============================================================================
# 37_cio_corrigido_validacao_completa.R  (exploracao_sinais)
#
# Reteste COMPLETO do CIO Peer Momentum usando a metodologia CORRIGIDA
# (re-corte de quintil mes a mes, achado do candidato #36 -- a versao
# antiga usava breaks fixos do treino, que geravam quintis degenerados,
# ate' n=1 acao em varios meses, o MESMO artefato do falso-positivo do
# Comomentum). Aplica TODOS os testes de robustez ja usados na exploracao:
#   1) Fama-MacBeth com controles (tamanho, beta, retorno proprio)
#   2) Custo de transacao
#   3) h=3, h=6 com a metodologia corrigida (a rejeicao anterior de h=3
#      pode ter sofrido do mesmo problema de quintil degenerado)
#   4) Liquidez/composicao da perna vendida (e realista montar short nesses
#      papeis no Brasil?)
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno","preco"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
cio <- fread(file.path(OUT, "cio_peer_return.csv"))

# tamanho (proxy: valor total detido pelos fundos, ja usado no candidato #24)
pp0 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("ativo","ym","valor_mil","peso","aum_prev","cod_fundo"))
pp0[, ticker := trimws(sub(".*- ", "", ativo))]
tamanho <- pp0[, .(valor_total_mil = sum(valor_mil), n_fundos_donos = uniqueN(cod_fundo)), by = .(ticker, ym)]

# beta (mesmo estilo do candidato #24: janela de 12m contra media do mercado)
setorder(precos, ticker, ymk)
mercado <- precos[, .(ret_mercado = mean(retorno, na.rm=TRUE)), by = ymk]
pw <- merge(precos, mercado, by = "ymk")
setorder(pw, ticker, ymk)
calc_beta <- function(ret, ret_mkt) {
  n <- length(ret); out <- rep(NA_real_, n)
  if (n < 12) return(out)
  for (i in 12:n) {
    jan <- (i-11):i
    fit <- tryCatch(lm(ret[jan] ~ ret_mkt[jan]), error=function(e) NULL)
    if (!is.null(fit) && length(coef(fit))>=2) out[i] <- coef(fit)[2]
  }
  out
}
pw[, beta_12m := calc_beta(retorno, ret_mercado), by = ticker]

base <- copy(cio)
ret_prox <- precos[, .(ticker, ym_alvo = addm(ymk, -1), retorno_prox = retorno)]
base <- merge(base, ret_prox, by.x = c("ticker","ym"), by.y = c("ticker","ym_alvo"))
base <- merge(base, tamanho, by = c("ticker","ym"), all.x = TRUE)
base <- merge(base, pw[,.(ticker,ym=ymk,beta_12m,retorno_proprio=retorno)], by=c("ticker","ym"), all.x = TRUE)
base <- base[is.finite(peer_ret) & is.finite(retorno_prox)]

# =============================================================================
# 1) FAMA-MACBETH COM CONTROLES (padroniza, regressao cross-sectional mes a mes)
# =============================================================================
cat("===== 1) Fama-MacBeth: peer_ret sobrevive controlando tamanho/beta/retorno proprio? =====\n")
teste <- base[ym >= CORTE & is.finite(valor_total_mil) & is.finite(beta_12m) & is.finite(retorno_proprio)]
meses <- sort(unique(teste$ym))
rodar_fm <- function(vars_x, nome) {
  coefs <- list()
  for (mm in meses) {
    dm <- teste[ym == mm]
    dm[, `:=`(peer_ret_z = scale(peer_ret), tamanho_z = scale(log(valor_total_mil)),
              beta_z = scale(beta_12m), ret_proprio_z = scale(retorno_proprio))]
    dm <- dm[is.finite(peer_ret_z) & is.finite(tamanho_z) & is.finite(beta_z) & is.finite(ret_proprio_z)]
    if (nrow(dm) < 30) next
    form <- as.formula(paste("retorno_prox ~", paste(vars_x, collapse=" + ")))
    fit <- tryCatch(lm(form, data=dm), error=function(e) NULL)
    if (!is.null(fit)) coefs[[as.character(mm)]] <- data.table(ym=mm, t(coef(fit)))
  }
  CM <- rbindlist(coefs, fill=TRUE)
  cat(sprintf("\n%s (%d meses):\n", nome, nrow(CM)))
  for (v in vars_x) {
    x <- CM[[v]]; x <- x[is.finite(x)]; n <- length(x)
    media <- mean(x); dp <- sd(x); t_fm <- media/(dp/sqrt(n)); p_fm <- 2*pt(-abs(t_fm), df=n-1)
    cat(sprintf("  %-16s coef_medio=%+9.5f | t=%6.2f | p=%.4f\n", v, media, t_fm, p_fm))
  }
}
rodar_fm("peer_ret_z", "So' peer_ret")
rodar_fm(c("peer_ret_z","tamanho_z","beta_z","ret_proprio_z"), "peer_ret + tamanho + beta + retorno proprio")

# =============================================================================
# 2) CUSTO DE TRANSACAO (magnitude corrigida, metodo B: re-corte mensal)
# =============================================================================
cat("\n===== 2) Long-short com re-corte mensal, para varios horizontes =====\n")
sharpe <- function(r) mean(r,na.rm=TRUE)/sd(r,na.rm=TRUE)*sqrt(12)
resultados_h <- list()
for (h in c(1,3,6)) {
  m <- copy(cio); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos[,.(ticker,ymk,retorno)], by.x=c("ticker","ym_ret"), by.y=c("ticker","ymk"))
  m <- m[is.finite(retorno) & is.finite(peer_ret) & ym >= CORTE]
  m[, quintil := as.integer(cut(peer_ret, quantile(peer_ret, 0:5/5, na.rm=TRUE), include.lowest=TRUE)), by=ym]
  sm <- m[quintil %in% c(1,5), .(ret=mean(retorno), n=.N), by=.(ym,quintil)]
  smw <- dcast(sm, ym~quintil, value.var=c("ret","n"))
  setnames(smw, c("ret_1","ret_5"), c("short","long"))
  smw[, ret_ls := long - short]
  tt <- t.test(smw$ret_ls)
  cat(sprintf("h=%d: %d meses | Sharpe=%.3f | media=%.3f%%/mes (%.1f%%/ano) | t=%.2f | p=%.4f | n_min_perna=%d\n",
              h, nrow(smw), sharpe(smw$ret_ls), 100*mean(smw$ret_ls), 100*mean(smw$ret_ls)*12,
              tt$statistic, tt$p.value, min(c(smw$n_1,smw$n_5),na.rm=TRUE)))
  resultados_h[[length(resultados_h)+1]] <- data.table(h=h, n_meses=nrow(smw), sharpe=sharpe(smw$ret_ls),
    media_pct_mes=100*mean(smw$ret_ls), t=tt$statistic, p=tt$p.value)
}
RH <- rbindlist(resultados_h)

# custo de transacao pro h=1 (magnitude corrigida)
m1 <- copy(cio); m1[, ym_ret := addm(ym, 1)]
m1 <- merge(m1, precos[,.(ticker,ymk,retorno)], by.x=c("ticker","ym_ret"), by.y=c("ticker","ymk"))
m1 <- m1[is.finite(retorno) & is.finite(peer_ret) & ym >= CORTE]
m1[, quintil := as.integer(cut(peer_ret, quantile(peer_ret, 0:5/5, na.rm=TRUE), include.lowest=TRUE)), by=ym]
sm1 <- m1[quintil %in% c(1,5), .(ret=mean(retorno)), by=.(ym,quintil)]
smw1 <- dcast(sm1, ym~quintil, value.var="ret"); setnames(smw1,c("1","5"),c("short","long")); smw1[,ret_ls:=long-short]
spread_bruto_mes <- mean(smw1$ret_ls)
cat(sprintf("\n===== Custo de transacao (h=1, magnitude CORRIGIDA) =====\n"))
cat(sprintf("Spread bruto: %.3f%%/mes (%.1f%%/ano)\n", 100*spread_bruto_mes, 100*spread_bruto_mes*12))
for (custo_bps in c(30,50,100,150)) {
  liquido_mes <- spread_bruto_mes - 2*custo_bps/10000  # 2 pernas, rebalanceio mensal
  cat(sprintf("  Custo %dbps round-trip/perna: liquido=%.3f%%/mes (%.1f%%/ano)\n",
              custo_bps, 100*liquido_mes, 100*liquido_mes*12))
}

# =============================================================================
# 3) Composicao/liquidez da perna vendida (top 15 tickers mais frequentes no Q1)
# =============================================================================
cat("\n===== 3) Tickers mais frequentes na perna VENDIDA (Q1) em h=1, 24 meses =====\n")
freq_short <- m1[quintil==1, .N, by=ticker][order(-N)][1:15]
print(freq_short)

fwrite(RH, file.path(OUT, "candidatos_37_horizontes_corrigido.csv"))
cat("\nOK\n")
