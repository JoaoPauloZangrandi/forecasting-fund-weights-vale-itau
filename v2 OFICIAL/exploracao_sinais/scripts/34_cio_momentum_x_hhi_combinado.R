# =============================================================================
# 34_cio_momentum_x_hhi_combinado.R  (exploracao_sinais)
#
# Combina os DOIS unicos sobreviventes de toda a exploracao: CIO Peer
# Momentum (unico preditor de RETORNO que sobrou, fragil, so h=1 -- Ying
# 2024 JFE) e HHI de posse (preditor solido de VOLATILIDADE, h=12 --
# Greenwood-Thesmar 2011). Logica: usa CIO peer momentum pra escolher
# DIRECAO/quais acoes comprar, usa HHI pra dimensionar TAMANHO da posicao
# (menos peso em acoes mais fragilizadas, reduzindo risco de cauda dentro
# da carteira) -- ideia de Betting Against Beta/vol-managed aplicada
# DENTRO de uma estrategia direcional em vez de no mercado inteiro.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

cio <- fread(file.path(OUT, "cio_peer_return.csv"))  # ticker, ym, peer_ret, n_peers

pp3 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp3[, ticker := trimws(sub(".*- ", "", ativo))]
pp3 <- pp3[is.finite(aum_prev) & aum_prev > 0]
pp3[, valor_posicao := peso * aum_prev]
crowd <- pp3[, .(hhi_posse = sum((valor_posicao/sum(valor_posicao))^2), n_fundos=.N), by = .(ticker, ym)]
crowd <- crowd[n_fundos >= 10]

base <- merge(cio[, .(ticker, ym, peer_ret)], crowd[, .(ticker, ym, hhi_posse)], by = c("ticker","ym"))
ret_prox <- precos[, .(ticker, ym_alvo = addm(ymk, -1), retorno_prox = retorno)]
base <- merge(base, ret_prox, by.x = c("ticker","ym"), by.y = c("ticker","ym_alvo"))
base <- base[is.finite(peer_ret) & is.finite(hhi_posse) & is.finite(retorno_prox)]
base <- base[ym >= CORTE]

base[, quintil_peer := as.integer(cut(peer_ret, quantile(peer_ret, 0:5/5, na.rm=TRUE), include.lowest=TRUE)), by=ym]
base[, decil_hhi := as.integer(cut(hhi_posse, quantile(hhi_posse, 0:10/10, na.rm=TRUE), include.lowest=TRUE)), by=ym]
base[, peso_inv_hhi := (11 - decil_hhi)]

sharpe <- function(r) mean(r, na.rm=TRUE) / sd(r, na.rm=TRUE) * sqrt(12)
max_drawdown <- function(r) { cum <- cumprod(1+r); dd <- cum/cummax(cum)-1; min(dd, na.rm=TRUE) }
resumo <- function(r, nome) {
  cat(sprintf("%-40s Sharpe=%7.3f | vol_anual=%6.2f%% | ret_anual=%7.2f%% | maxDD=%7.2f%% | N=%d\n",
              nome, sharpe(r), sd(r,na.rm=TRUE)*sqrt(12)*100, mean(r,na.rm=TRUE)*12*100, max_drawdown(r)*100, sum(is.finite(r))))
  data.table(estrategia=nome, sharpe=sharpe(r), vol_anual_pct=sd(r,na.rm=TRUE)*sqrt(12)*100,
             ret_anual_pct=mean(r,na.rm=TRUE)*12*100, max_dd_pct=max_drawdown(r)*100, n_meses=sum(is.finite(r)))
}

# ---- (a) baseline: long-short CIO puro, equal-weight dentro de cada perna ----
ls_puro <- base[, .(
  ret_long  = mean(retorno_prox[quintil_peer==5]),
  ret_short = mean(retorno_prox[quintil_peer==1])
), by=ym][order(ym)]
ls_puro[, ret_ls := ret_long - ret_short]

# ---- (b) long-short CIO, pesado inversamente por decil de HHI dentro de cada perna ----
base[quintil_peer==5, w := peso_inv_hhi/sum(peso_inv_hhi), by=ym]
base[quintil_peer==1, w := peso_inv_hhi/sum(peso_inv_hhi), by=ym]
ls_hhi <- base[quintil_peer %in% c(1,5), .(ret = sum(w*retorno_prox)), by=.(ym,quintil_peer)]
ls_hhi_w <- dcast(ls_hhi, ym~quintil_peer, value.var="ret")
setnames(ls_hhi_w, c("1","5"), c("short","long"))
ls_hhi_w[, ret_ls_hhi := long - short]

# ---- (c) long-only top-quintile: equal-weight vs. inverso-HHI ----
top <- base[quintil_peer==5]
long_only <- top[, .(
  ret_ew  = mean(retorno_prox),
  ret_hhi = sum((peso_inv_hhi/sum(peso_inv_hhi)) * retorno_prox)
), by=ym][order(ym)]

cat("===== Long-short CIO Peer Momentum: puro vs. pesado por (inverso) HHI =====\n")
r1 <- resumo(ls_puro$ret_ls, "Long-short CIO puro (equal-weight)")
r2 <- resumo(ls_hhi_w$ret_ls_hhi, "Long-short CIO + tilt inverso-HHI")

cat("\n===== Long-only top-quintile CIO: equal-weight vs. inverso-HHI =====\n")
r3 <- resumo(long_only$ret_ew, "Long-only top-CIO (equal-weight)")
r4 <- resumo(long_only$ret_hhi, "Long-only top-CIO (tilt inverso-HHI)")

R <- rbindlist(list(r1,r2,r3,r4))
fwrite(R, file.path(OUT, "candidatos_34_cio_x_hhi.csv"))
cat("\nOK\n")
