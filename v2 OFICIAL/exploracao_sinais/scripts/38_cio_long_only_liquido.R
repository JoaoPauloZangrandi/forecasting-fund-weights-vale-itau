# =============================================================================
# 38_cio_long_only_liquido.R  (exploracao_sinais)
#
# Achado do candidato #37: a perna VENDIDA do long-short CIO Peer Momentum
# e' dominada por small/micro caps ilíquidas (CEEB3, DOHL4, MDNE3, HBOR3,
# GFSA3, etc.) -- aluguel pra short nesses papeis provavelmente custa bem
# mais que os 100bps que ja zeram o resultado, ou nem esta disponivel na
# pratica. Alem disso, os FUNDOS que o TCC estuda sao predominantemente
# LONG-ONLY (fundos de ações tradicionais, nao long-short/hedge). Desenho
# mais realista pro contexto: (a) restringir o universo a acoes liquidas
# ANTES de formar o sinal, (b) testar so' a perna COMPRADA (long-only tilt)
# contra o benchmark do mesmo universo liquido -- sem exigir short.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
cio <- fread(file.path(OUT, "cio_peer_return.csv"))

pp0 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("ativo","ym","valor_mil"))
pp0[, ticker := trimws(sub(".*- ", "", ativo))]
liquidez <- pp0[, .(valor_total_mil = sum(valor_mil)), by = .(ticker, ym)]
# proxy de liquidez: rank do valor total detido por fundos DENTRO do mes (quanto mais fundos/mais
# capital institucional, mais provavel ser um papel com volume/free-float razoavel)
liquidez[, rank_liquidez := frank(-valor_total_mil)/.N, by = ym]

base <- copy(cio)
ret_prox <- precos[, .(ticker, ym_alvo = addm(ymk, -1), retorno_prox = retorno)]
base <- merge(base, ret_prox, by.x = c("ticker","ym"), by.y = c("ticker","ym_alvo"))
base <- merge(base, liquidez, by = c("ticker","ym"), all.x = TRUE)
base <- base[is.finite(peer_ret) & is.finite(retorno_prox) & is.finite(rank_liquidez) & ym >= CORTE]

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
cat("===== Universo restrito a acoes mais LIQUIDAS (top 1/3 por posse institucional), long-only =====\n")
for (corte_liq in c(1/3, 1/2, 2/3)) {
  b <- base[rank_liquidez <= corte_liq]
  b[, quintil := as.integer(cut(peer_ret, quantile(peer_ret, 0:5/5, na.rm=TRUE), include.lowest=TRUE)), by=ym]
  top <- b[quintil==5, .(ret=mean(retorno_prox), n=.N), by=ym][order(ym)]
  bench <- b[, .(ret_bench=mean(retorno_prox)), by=ym][order(ym)]
  comp <- merge(top, bench, by="ym")
  comp[, excesso := ret - ret_bench]
  cat(sprintf("\n--- Universo liquido top %.0f%% (n_medio=%.0f acoes/mes) ---\n", 100*corte_liq, mean(top$n)))
  r1 <- resumo(comp$ret, sprintf("Long-only top-CIO, liquido top%.0f%%", 100*corte_liq))
  r2 <- resumo(comp$ret_bench, sprintf("Benchmark equal-weight, liquido top%.0f%%", 100*corte_liq))
  r3 <- resumo(comp$excesso, sprintf("EXCESSO (top-CIO menos benchmark), liquido top%.0f%%", 100*corte_liq))
  resultados[[length(resultados)+1]] <- rbindlist(list(r1,r2,r3))
}

cat("\n===== Long-short, mas restrito ao universo liquido (evita as small caps ilíquidas na perna vendida) =====\n")
for (corte_liq in c(1/3, 1/2)) {
  b <- base[rank_liquidez <= corte_liq]
  b[, quintil := as.integer(cut(peer_ret, quantile(peer_ret, 0:5/5, na.rm=TRUE), include.lowest=TRUE)), by=ym]
  sm <- b[quintil %in% c(1,5), .(ret=mean(retorno_prox), n=.N), by=.(ym,quintil)]
  smw <- dcast(sm, ym~quintil, value.var="ret"); setnames(smw,c("1","5"),c("short","long"))
  smw[, ret_ls := long-short]
  r <- resumo(smw$ret_ls, sprintf("Long-short CIO, universo liquido top%.0f%%", 100*corte_liq))
  resultados[[length(resultados)+1]] <- r
  freq <- b[quintil==1, .N, by=ticker][order(-N)][1:8]
  cat("  Tickers mais frequentes na perna vendida (top8):", paste(freq$ticker, collapse=", "), "\n")
}

R <- rbindlist(resultados)
fwrite(R, file.path(OUT, "candidatos_38_cio_liquido.csv"))
cat("\nOK\n")
