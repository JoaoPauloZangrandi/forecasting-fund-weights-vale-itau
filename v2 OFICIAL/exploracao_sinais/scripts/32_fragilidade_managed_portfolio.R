# =============================================================================
# 32_fragilidade_managed_portfolio.R  (exploracao_sinais)
#
# Converte o achado ja validado (HHI de posse prediz volatilidade futura,
# candidatos #28-31) em estrategias IMPLEMENTAVEIS so com acoes a vista
# (sem opcoes), seguindo a pesquisa de literatura (Moreira-Muir 2017 vol-
# managed portfolios; Asness-Frazzini-Gormsen-Pedersen 2020 "Betting Against
# Correlation"; Cederburg et al 2020 -- alerta de look-ahead bias).
#
# Tres desenhos, testados juntos (preferencia do usuario: multiplas versoes
# na duvida metodologica):
#
#  A) Fragility-managed market timing (a la Moreira-Muir): escala a exposicao
#     ao portfolio de mercado (equal-weight do universo) pelo inverso da
#     fragilidade agregada (HHI medio ponderado por valor) do mes anterior.
#     Comparado contra (i) buy-and-hold e (ii) vol-managed classico (escala
#     por vol realizada passada, sem HHI) -- isola o valor marginal do HHI.
#
#  B) Fragility-tilted cross-section (estilo risk-parity/BAB, nao e' uma
#     replica de Moreira-Muir): a cada mes, pesa cada acao inversamente ao
#     seu decil de HHI (menos peso pra mais fragil), rebalanceio mensal,
#     comparado contra equal-weight do mesmo universo.
#
#  C) Long-short de fragilidade (long baixo-HHI, short alto-HHI): testa se
#     o spread se comporta como uma posicao curta em volatilidade (perde
#     quando a vol de mercado sobe) e se tem assimetria negativa (skewness)
#     -- assinatura comportamental de "vender seguro contra choque", sem
#     alegar que e' um VRP de verdade (nao temos opcoes).
#
# Toda a analise usa so ym >= 202001 (mesmo periodo de teste do resto do
# TCC) para nao reintroduzir look-ahead nas comparacoes de Sharpe.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
setorder(precos, ticker, ymk)
precos[, vol_passada_12m := frollapply(retorno, 12, sd), by = ticker]

pp3 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp3[, ticker := trimws(sub(".*- ", "", ativo))]
pp3 <- pp3[is.finite(aum_prev) & aum_prev > 0]
pp3[, valor_posicao := peso * aum_prev]
crowd <- pp3[, .(hhi_posse = sum((valor_posicao/sum(valor_posicao))^2),
                  valor_total_mercado = sum(valor_posicao),
                  n_fundos = .N), by = .(ticker, ym)]
crowd <- crowd[n_fundos >= 10]

addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

# painel base: ticker, mes, HHI conhecido em t, retorno realizado em t+1 (sem look-ahead)
base <- merge(crowd[, .(ticker, ym, hhi_posse, valor_posicao_mercado = valor_total_mercado)],
              precos[, .(ticker, ym = ymk, retorno, vol_passada_12m)], by = c("ticker","ym"))
ret_prox <- precos[, .(ticker, ym_alvo = addm(ymk, -1), retorno_prox = retorno)]
base <- merge(base, ret_prox, by.x = c("ticker","ym"), by.y = c("ticker","ym_alvo"))
base <- base[is.finite(hhi_posse) & is.finite(retorno_prox)]

cat(sprintf("Painel base: %d obs (ticker-mes), %d meses, %d tickers\n",
            nrow(base), uniqueN(base$ym), uniqueN(base$ticker)))

# =============================================================================
# DESENHO B: fragility-tilted cross-section (risk-parity por decil de HHI)
# =============================================================================
base[, decil_hhi := as.integer(cut(hhi_posse, quantile(hhi_posse, probs = 0:10/10, na.rm=TRUE),
                                    include.lowest = TRUE)), by = ym]
# peso inverso ao decil: decil 1 (menos fragil) pesa 10, decil 10 (mais fragil) pesa 1
base[, peso_tilt := (11 - decil_hhi)]
base[, peso_tilt := peso_tilt / sum(peso_tilt), by = ym]
base[, peso_ew := 1 / .N, by = ym]

carteira <- base[ym >= CORTE, .(
  ret_tilt = sum(peso_tilt * retorno_prox),
  ret_ew   = sum(peso_ew   * retorno_prox)
), by = ym][order(ym)]

sharpe <- function(r) mean(r, na.rm=TRUE) / sd(r, na.rm=TRUE) * sqrt(12)
max_drawdown <- function(r) {
  cum <- cumprod(1 + r)
  dd <- cum / cummax(cum) - 1
  min(dd, na.rm=TRUE)
}

cat("\n===== DESENHO B: fragility-tilted (peso inverso ao decil de HHI) vs equal-weight =====\n")
cat(sprintf("Tilt por HHI  : Sharpe=%.3f | vol_anual=%.2f%% | ret_anual=%.2f%% | max_DD=%.2f%%\n",
            sharpe(carteira$ret_tilt), sd(carteira$ret_tilt)*sqrt(12)*100,
            mean(carteira$ret_tilt)*12*100, max_drawdown(carteira$ret_tilt)*100))
cat(sprintf("Equal-weight  : Sharpe=%.3f | vol_anual=%.2f%% | ret_anual=%.2f%% | max_DD=%.2f%%\n",
            sharpe(carteira$ret_ew), sd(carteira$ret_ew)*sqrt(12)*100,
            mean(carteira$ret_ew)*12*100, max_drawdown(carteira$ret_ew)*100))
cat(sprintf("Correlacao tilt x ew: %.3f | N meses teste: %d\n",
            cor(carteira$ret_tilt, carteira$ret_ew), nrow(carteira)))

# =============================================================================
# DESENHO A: fragility-managed market timing (escala portfolio de mercado)
# =============================================================================
mercado <- base[, .(ret_mercado_ew = mean(retorno_prox),
                     hhi_agregado = sum(hhi_posse * valor_posicao_mercado) / sum(valor_posicao_mercado)),
                 by = ym][order(ym)]
setorder(mercado, ym)
mercado[, hhi_agregado_lag := shift(hhi_agregado, 1)]
mercado[, vol_passada_mercado := frollapply(ret_mercado_ew, 12, sd)]
mercado[, vol_passada_mercado_lag := shift(vol_passada_mercado, 1)]

teste_mercado <- mercado[ym >= CORTE & is.finite(hhi_agregado_lag) & is.finite(vol_passada_mercado_lag)]

# normaliza escalonador para media 1 no periodo de teste (mesma convencao de
# Moreira-Muir -- alerta: isso usa informacao da propria janela de teste pra
# fixar a constante c, e' uma limitacao conhecida, documentada explicitamente)
teste_mercado[, w_hhi := (1/hhi_agregado_lag) / mean(1/hhi_agregado_lag)]
teste_mercado[, w_volpassada := (1/vol_passada_mercado_lag^2) / mean(1/vol_passada_mercado_lag^2)]
teste_mercado[, w_hhi := pmin(w_hhi, 3)]           # teto de alavancagem 3x
teste_mercado[, w_volpassada := pmin(w_volpassada, 3)]

teste_mercado[, ret_buyhold := ret_mercado_ew]
teste_mercado[, ret_hhi_managed := w_hhi * ret_mercado_ew]
teste_mercado[, ret_vol_managed := w_volpassada * ret_mercado_ew]

cat("\n===== DESENHO A: fragility-managed market timing vs buy-hold vs vol-managed classico =====\n")
for (nome in c("ret_buyhold","ret_hhi_managed","ret_vol_managed")) {
  r <- teste_mercado[[nome]]
  cat(sprintf("%-18s Sharpe=%.3f | vol_anual=%.2f%% | ret_anual=%.2f%% | max_DD=%.2f%%\n",
              nome, sharpe(r), sd(r)*sqrt(12)*100, mean(r)*12*100, max_drawdown(r)*100))
}
cat(sprintf("N meses teste: %d\n", nrow(teste_mercado)))

# =============================================================================
# DESENHO C: long-short de fragilidade (long baixo-HHI, short alto-HHI)
# =============================================================================
ls <- base[ym >= CORTE, .(
  ret_baixo_hhi = mean(retorno_prox[decil_hhi <= 2]),
  ret_alto_hhi  = mean(retorno_prox[decil_hhi >= 9])
), by = ym][order(ym)]
ls[, spread := ret_baixo_hhi - ret_alto_hhi]
ls <- merge(ls, mercado[, .(ym, ret_mercado_ew)], by = "ym")

t_spread <- t.test(ls$spread)
cat("\n===== DESENHO C: long-short fragilidade (baixo-HHI menos alto-HHI) =====\n")
cat(sprintf("Media mensal: %.4f | t=%.2f | p=%.4f | N=%d meses\n",
            mean(ls$spread), t_spread$statistic, t_spread$p.value, nrow(ls)))
cat(sprintf("Skewness do spread: %.3f  (negativo = assinatura de 'seguro contra choque', tipo Schneider-Wagner-Zechner 2020)\n",
            mean((ls$spread-mean(ls$spread))^3)/sd(ls$spread)^3))
cat(sprintf("Correlacao spread x retorno de mercado contemporaneo: %.3f\n", cor(ls$spread, ls$ret_mercado_ew)))
meses_alta_vol <- mercado[ym >= CORTE][order(-vol_passada_mercado)][1:round(.N/3)]$ym
cat(sprintf("Media do spread em 1/3 dos meses de MAIOR vol de mercado passada: %.4f (n=%d)\n",
            mean(ls[ym %in% meses_alta_vol]$spread), sum(ls$ym %in% meses_alta_vol)))
cat(sprintf("Media do spread no resto dos meses (menor vol passada)         : %.4f (n=%d)\n",
            mean(ls[!ym %in% meses_alta_vol]$spread), sum(!ls$ym %in% meses_alta_vol)))

fwrite(carteira, file.path(OUT, "candidatos_32b_fragility_tilt.csv"))
fwrite(teste_mercado, file.path(OUT, "candidatos_32a_market_timing.csv"))
fwrite(ls, file.path(OUT, "candidatos_32c_long_short_fragilidade.csv"))
cat("\nOK\n")
