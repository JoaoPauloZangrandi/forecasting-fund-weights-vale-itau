# =============================================================================
# 25_low_volatility_brasil.R  (exploracao_sinais)
#
# LOW VOLATILITY ANOMALY -- a UNICA anomalia classica com evidencia forte
# e consistente especificamente no mercado brasileiro (pesquisa achou 2
# estudos independentes: +6%/ano 2003-2021, +15,5%/ano 2003-2017).
# DIFERENTE de volatilidade idiossincratica (candidato #24, ja testado,
# residuo apos tirar mercado) -- aqui e' VOLATILIDADE TOTAL (dp do
# retorno bruto, sem ajustar por mercado), a mesma metodologia dos papers
# brasileiros que acharam o efeito.
#
# Testa tambem INTERACAO com ownership institucional (ja que a pergunta
# de fundo desta rodada e' isso) e com o CIO Peer Momentum (sera que
# baixa-vol + CIO alto e' melhor que qualquer um sozinho?).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/280

setorder(precos, ticker, ymk)
precos[, vol_total_12m := frollapply(retorno, 12, sd), by = ticker]

pp0 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("ativo","ym","valor_mil"))
pp0[, ticker := trimws(sub(".*- ", "", ativo))]
valor_total <- pp0[, .(valor_total_mil = sum(valor_mil)), by = .(ticker, ym)]
valor_total[, own_rank := frank(valor_total_mil) / .N, by = ym]

cio_dt <- fread(file.path(OUT, "cio_peer_return.csv"))[, .(ticker, ym, peer_ret)]

base <- merge(precos[, .(ticker, ym=ymk, vol_total_12m)], valor_total[, .(ticker, ym, own_rank)], by=c("ticker","ym"))

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
  cat(sprintf("%-30s h=%d %2d meses spread=%+7.2fpp/mes (~%+6.1f%%/ano) t=%6.2f p=%.5f | Bonferroni:%s\n",
              nome, h, nmes, 100*media, 100*((1+media)^12-1), t_fm, p_fm, ifelse(p_fm<LIMIAR_BONFERRONI,"SIM","nao")))
  data.table(sinal=nome, horizonte=h, n_meses=nmes, spread_pp=100*media, t_fm=t_fm, p_fm=p_fm,
             sig_bonferroni=p_fm<LIMIAR_BONFERRONI)
}

resultados <- list()

cat("===== LOW VOLATILITY, sozinha (LONG baixa vol, SHORT alta vol -- inverte sinal do quintil) =====\n")
for (h in c(1,3,6,12)) {
  m <- copy(base); m[, ym_ret := addm(ym,h)]
  m <- merge(m, precos[,.(ticker,ymk,retorno)], by.x=c("ticker","ym_ret"), by.y=c("ticker","ymk"))
  m <- m[is.finite(retorno) & is.finite(vol_total_12m)]
  m[, vol_invertida := -vol_total_12m]  # inverte pra Q5 = baixa vol (long), Q1 = alta vol (short)
  r <- fama_macbeth(m, "vol_invertida", "retorno", "Low-vol (Q5=baixa)", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

cat("\n===== LOW VOLATILITY x OWNERSHIP institucional =====\n")
for (h in c(1,3)) {
  m <- copy(base); m[, ym_ret := addm(ym,h)]
  m <- merge(m, precos[,.(ticker,ymk,retorno)], by.x=c("ticker","ym_ret"), by.y=c("ticker","ymk"))
  m <- m[is.finite(retorno) & is.finite(vol_total_12m) & is.finite(own_rank)]
  m[, vol_invertida := -vol_total_12m]
  baixa <- m[own_rank <= 1/3]; alta <- m[own_rank >= 2/3]
  r1 <- fama_macbeth(baixa, "vol_invertida", "retorno", "Low-vol|BAIXA-own", h)
  r2 <- fama_macbeth(alta, "vol_invertida", "retorno", "Low-vol|ALTA-own", h)
  if (!is.null(r1)) resultados[[length(resultados)+1]] <- r1
  if (!is.null(r2)) resultados[[length(resultados)+1]] <- r2
}

cat("\n===== COMBINACAO: Low-vol + CIO Peer Momentum (dupla filtragem) =====\n")
comb <- merge(base, cio_dt, by=c("ticker","ym"))
for (h in c(1,3)) {
  m <- copy(comb); m[, ym_ret := addm(ym,h)]
  m <- merge(m, precos[,.(ticker,ymk,retorno)], by.x=c("ticker","ym_ret"), by.y=c("ticker","ymk"))
  m <- m[is.finite(retorno) & is.finite(vol_total_12m) & is.finite(peer_ret)]
  # so' os 50% de MENOR volatilidade (mediana calculada DENTRO de cada mes), dentro
  # desse grupo testa CIO peer momentum
  m[, mediana_vol_mes := median(vol_total_12m, na.rm=TRUE), by=ym]
  m_lowvol <- m[vol_total_12m <= mediana_vol_mes]
  r <- fama_macbeth(m_lowvol, "peer_ret", "retorno", "CIO dentro de Low-vol", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

R <- rbindlist(resultados)
fwrite(R, file.path(OUT, "candidatos_25_lowvol.csv"))
cat("\n\n===== RESUMO =====\n")
print(R[order(p_fm)][, .(sinal,horizonte,n_meses,spread_pp=round(spread_pp,2),t_fm=round(t_fm,2),p_fm=round(p_fm,5),sig_bonferroni)])
cat("\nOK\n")
