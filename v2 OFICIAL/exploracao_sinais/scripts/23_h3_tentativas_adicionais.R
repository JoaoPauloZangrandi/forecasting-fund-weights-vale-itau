# =============================================================================
# 23_h3_tentativas_adicionais.R  (exploracao_sinais)
#
# Mais tentativas especificas em h=3, a pedido explicito do Joao mesmo
# apos eu explicar o risco de multiplas comparacoes. Com ~250 testes ja
# feitos, o limiar de significancia efetivo (Bonferroni, alfa=0,05) e'
# p < 0,05/250 = 0,0002 -- MUITO mais rigoroso que o p<0,05 padrao usado
# em cada teste individual. Qualquer resultado aqui sera avaliado contra
# esse limiar, nao o padrao, por honestidade.
#
# (X) CIO peer momentum SUAVIZADO (media movel 2-3 meses do peer_ret) --
#     ainda nao tentado (so' FIT tinha sido suavizado antes).
# (Y) Sorteio por DECIS em vez de quintis (extremos mais extremos, testa
#     se o efeito so aparece nas pontas mais distantes).
# (Z) Versao NEUTRA EM TAMANHO: usa o RESIDUO de peer_ret depois de tirar
#     o efeito de tamanho (regressao peer_ret ~ log_tamanho, usa residuo)
#     -- testa se o sinal e' realmente sobre ownership-comovimento, nao
#     so' um proxy de tamanho disfarcado.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/250

cio_dt <- fread(file.path(OUT, "cio_peer_return.csv"))
cio_dt[, ticker_ret := ticker]
setorder(cio_dt, ticker, ym)
cio_dt[, peer_ret_ma2 := frollmean(peer_ret, 2, align="right"), by = ticker]
cio_dt[, peer_ret_ma3 := frollmean(peer_ret, 3, align="right"), by = ticker]

fama_macbeth_generico <- function(m, var_x, var_y, nome, h, n_grupos_alvo = 5) {
  treino <- m[ym < CORTE]; teste <- copy(m[ym >= CORTE])
  if (nrow(treino) < 50 || nrow(teste) < 30) return(NULL)
  probs <- seq(0, 1, length.out = n_grupos_alvo+1)
  breaks <- unique(quantile(treino[[var_x]], probs, na.rm = TRUE))
  if (length(breaks) < 3) return(NULL)
  breaks[1] <- -Inf; breaks[length(breaks)] <- Inf
  teste[, grupo := as.integer(cut(get(var_x), breaks, labels = FALSE, include.lowest = TRUE))]
  ng <- length(breaks)-1
  teste <- teste[!is.na(grupo)]
  por_mes <- teste[grupo %in% c(1,ng), .(retorno_medio = mean(get(var_y)), n=.N), by = .(ym, grupo)]
  sm <- dcast(por_mes, ym ~ grupo, value.var = "retorno_medio")
  cols_precisa <- as.character(c(1,ng))
  if (!all(cols_precisa %in% names(sm))) return(NULL)
  setnames(sm, cols_precisa, c("q1","qN"))
  sm <- sm[is.finite(q1) & is.finite(qN)]
  sm[, spread := qN - q1]
  n_meses <- nrow(sm)
  if (n_meses < 6) return(NULL)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(n_meses)); p_fm <- 2*pt(-abs(t_fm), df = n_meses-1)
  sig_bonferroni <- p_fm < LIMIAR_BONFERRONI
  cat(sprintf("%-30s h=%d [ng=%d] %2d meses spread=%+7.2fpp/mes t=%6.2f p=%.5f | p<0,05:%s | p<Bonferroni(%.5f):%s\n",
              nome, h, n_grupos_alvo, n_meses, 100*media, t_fm, p_fm,
              ifelse(p_fm<0.05,"SIM","nao"), LIMIAR_BONFERRONI, ifelse(sig_bonferroni,"SIM","nao")))
  data.table(sinal=nome, horizonte=h, n_grupos=n_grupos_alvo, n_meses=n_meses, spread_pp=100*media,
             t_fm=t_fm, p_fm=p_fm, sig_5pct=p_fm<0.05, sig_bonferroni=sig_bonferroni)
}

resultados <- list()

cat("===== (X) CIO SUAVIZADO (media movel 2-3 meses), h=3 =====\n")
for (var in c("peer_ret_ma2","peer_ret_ma3")) {
  m <- copy(cio_dt); m[, ym_ret := addm(ym, 3)]
  m <- merge(m, precos, by.x = c("ticker_ret","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(get(var))]
  r <- fama_macbeth_generico(m, var, "retorno", var, 3)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

cat("\n===== (Y) DECIS em vez de quintis, h=3, sinal original =====\n")
m <- copy(cio_dt); m[, ym_ret := addm(ym, 3)]
m <- merge(m, precos, by.x = c("ticker_ret","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
m <- m[is.finite(retorno) & is.finite(peer_ret)]
r <- fama_macbeth_generico(m, "peer_ret", "retorno", "CIO decis", 3, n_grupos_alvo = 10)
if (!is.null(r)) resultados[[length(resultados)+1]] <- r

cat("\n===== (Z) CIO NEUTRO EM TAMANHO (residuo apos tirar efeito de log_tamanho), h=1 e h=3 =====\n")
pp0 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("ativo","ym","valor_mil"))
pp0[, ticker := trimws(sub(".*- ", "", ativo))]
valor_total <- pp0[, .(valor_total_mil = sum(valor_mil)), by = .(ticker, ym)]
valor_total[, log_tamanho := log(pmax(valor_total_mil,1))]
cio_tam <- merge(cio_dt, valor_total[, .(ticker, ym, log_tamanho)], by = c("ticker","ym"))
cio_tam <- cio_tam[is.finite(log_tamanho)]
cio_tam[, peer_ret_neutro := residuals(lm(peer_ret ~ log_tamanho)), by = ym]  # residuo DENTRO de cada mes
for (h in c(1,3)) {
  m <- copy(cio_tam); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker_ret","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(peer_ret_neutro)]
  r <- fama_macbeth_generico(m, "peer_ret_neutro", "retorno", "CIO neutro-tamanho", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

R <- rbindlist(resultados)
fwrite(R, file.path(OUT, "candidatos_23_h3_adicionais.csv"))
cat("\n\n===== RESUMO: algum resultado passa do limiar Bonferroni (p<", signif(LIMIAR_BONFERRONI,4), ")? =====\n")
print(R[, .(sinal, horizonte, p_fm=round(p_fm,5), sig_5pct, sig_bonferroni)])
cat("\nOK\n")
