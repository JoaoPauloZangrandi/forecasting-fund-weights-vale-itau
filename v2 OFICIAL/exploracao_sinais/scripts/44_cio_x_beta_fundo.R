# =============================================================================
# 44_cio_x_beta_fundo.R  (exploracao_sinais)
#
# Angulo (f) pedido pelo Joao: segmentacao por tipo de gestora/fundo,
# cruzada com um sinal ja existente. Usa `beta_fundo` (beta de mercado da
# COTA do fundo vs Ibovespa, janela movel de 252 pregoes -- ja calculado
# no pipeline oficial do TCC, script 19/53 de `v2 OFICIAL/scripts`) como
# proxy observavel de "tipo de gestor": fundos de BAIXO beta_fundo tendem
# a ser mais conservadores/possivelmente mais fundamentalistas (retornos
# de cota menos correlacionados ao mercado); fundos de ALTO beta_fundo sao
# mais agressivos/direcionais. Note: beta_fundo mede EXPOSICAO A RISCO, nao
# SKILL diretamente -- o candidato #26/27 (persistencia de performance,
# incl.\ alfa ajustado por beta_fundo) ja mostrou que nao ha evidencia de
# skill persistente no painel; aqui a pergunta e' diferente: será que a
# MOVIMENTACAO de holdings especificamente dos fundos de baixo-beta carrega
# mais informacao (rede de peers "fundamentalista") do que a dos de
# alto-beta, quando usada para construir o CIO Peer Momentum (candidato
# #11/15/34-38, unico sinal de retorno sobrevivente ate aqui)?
#
# Reconstroi a rede de overlap de posse (cosseno, igual ao script 11) DUAS
# VEZES por mes -- uma so' com fundos do tercil de BAIXO beta_fundo, outra
# so' com o tercil de ALTO beta_fundo (tercil calculado por mes, corte
# TRANSVERSAL entre fundos, nao ao longo do tempo) -- e testa qual rede
# produz peer_ret mais preditivo do retorno futuro da propria acao.
#
# METODOLOGIA (as 3 licoes inegociaveis): Fama-MacBeth de verdade; quintil
# de peer_ret RE-CORTADO A CADA MES (metodo B); teste so' em ym>=202001.
# Licao adicional do candidato #22 (Comomentum) e #36: sempre checar o N
# efetivo de peers por observacao antes de confiar no resultado -- aqui
# reduzir o universo de fundos a 1/3 reduz a densidade da rede, entao a
# checagem de n_peers minimo e' ainda mais importante que no script 11.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/500

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

pp <- fread(file.path(DATA, "painel_multiativo_final.csv"),
            select = c("cod_fundo","ativo","ym","peso","aum_prev","beta_fundo"))
pp <- pp[is.finite(aum_prev) & aum_prev > 0 & peso > 0 & is.finite(beta_fundo)]
pp[, valor := peso * aum_prev]
pp[, ticker := trimws(sub(".*- ", "", ativo))]
tickers_com_preco <- unique(precos$ticker)
pp <- pp[ticker %in% tickers_com_preco]

# tercil de beta_fundo POR MES, cross-section entre fundos (1 linha por fundo-mes)
fbeta <- unique(pp[, .(cod_fundo, ym, beta_fundo)])
fbeta[, tercil_beta := as.integer(cut(beta_fundo, quantile(beta_fundo, 0:3/3, na.rm = TRUE), include.lowest = TRUE)), by = ym]
cat("Distribuicao de tercil_beta (deve ser ~1/3 cada):\n"); print(table(fbeta$tercil_beta))

pp <- merge(pp, fbeta[, .(cod_fundo, ym, tercil_beta)], by = c("cod_fundo","ym"))

calc_peer_return_mes <- function(mes_atual, dsub) {
  d_mes <- dsub[ym == mes_atual]
  if (uniqueN(d_mes$ticker) < 15 || uniqueN(d_mes$cod_fundo) < 15) return(NULL)
  d_mes[, cod_fundo := as.character(cod_fundo)]
  wide <- dcast(d_mes, cod_fundo ~ ticker, value.var = "valor", fun.aggregate = sum, fill = 0)
  X <- as.matrix(wide[, -1, with = FALSE])
  tickers_mes <- colnames(X)
  if (ncol(X) < 15) return(NULL)
  O <- crossprod(X)
  diag_O <- diag(O); diag_O[diag_O <= 0] <- NA
  denom <- sqrt(outer(diag_O, diag_O))
  CIO <- O / denom; diag(CIO) <- 0; CIO[!is.finite(CIO)] <- 0

  ret_mes <- precos[ymk == mes_atual & ticker %in% tickers_mes]
  if (nrow(ret_mes) < 10) return(NULL)
  ret_vec <- setNames(rep(NA_real_, length(tickers_mes)), tickers_mes)
  ret_vec[ret_mes$ticker] <- ret_mes$retorno

  peer_ret <- rep(NA_real_, length(tickers_mes)); n_peers <- rep(0L, length(tickers_mes))
  for (i in seq_along(tickers_mes)) {
    pesos_i <- CIO[i, ]
    validos <- is.finite(ret_vec) & pesos_i > 0
    if (sum(validos) < 5) next
    piso <- quantile(pesos_i[validos], 0.75, na.rm = TRUE)
    usar <- validos & pesos_i >= piso
    if (sum(usar) < 5) usar <- validos
    if (sum(usar) < 5) next
    peer_ret[i] <- weighted.mean(ret_vec[usar], pesos_i[usar])
    n_peers[i] <- sum(usar)
  }
  data.table(ticker = tickers_mes, ym = mes_atual, peer_ret = peer_ret, n_peers = n_peers)[!is.na(peer_ret)]
}

meses <- sort(unique(pp$ym))
cat("\nCalculando rede CIO restrita a tercil BAIXO beta_fundo (fundos conservadores)...\n")
t0 <- Sys.time()
d_baixo <- pp[tercil_beta == 1]
res_baixo <- rbindlist(Filter(Negate(is.null), lapply(meses, calc_peer_return_mes, dsub = d_baixo)))
cat("  ", nrow(res_baixo), "ticker-mes | n_peers medio:", round(mean(res_baixo$n_peers),1),
    "| minimo:", min(res_baixo$n_peers), "| tempo:", round(as.numeric(Sys.time()-t0,units="secs"),1), "s\n")

cat("Calculando rede CIO restrita a tercil ALTO beta_fundo (fundos agressivos)...\n")
t0 <- Sys.time()
d_alto <- pp[tercil_beta == 3]
res_alto <- rbindlist(Filter(Negate(is.null), lapply(meses, calc_peer_return_mes, dsub = d_alto)))
cat("  ", nrow(res_alto), "ticker-mes | n_peers medio:", round(mean(res_alto$n_peers),1),
    "| minimo:", min(res_alto$n_peers), "| tempo:", round(as.numeric(Sys.time()-t0,units="secs"),1), "s\n")

fwrite(res_baixo, file.path(OUT, "cio_peer_return_baixo_beta.csv"))
fwrite(res_alto, file.path(OUT, "cio_peer_return_alto_beta.csv"))

fama_macbeth_recut <- function(m, var_x, var_y, nome, h) {
  teste <- copy(m[ym >= CORTE & is.finite(get(var_x)) & is.finite(get(var_y))])
  if (nrow(teste) < 30) return(NULL)
  teste[, quintil := {
    qs <- quantile(get(var_x), 0:5/5, na.rm = TRUE)
    if (length(unique(qs)) < 6) as.integer(NA) else as.integer(cut(get(var_x), qs, include.lowest = TRUE))
  }, by = ym]
  teste <- teste[!is.na(quintil)]
  chk <- teste[quintil %in% c(1,5), .N, by = .(ym, quintil)]
  n_min <- if (nrow(chk) > 0) min(chk$N) else 0
  por_mes <- teste[quintil %in% c(1,5), .(rm = mean(get(var_y)), n = .N), by = .(ym, quintil)]
  sm <- dcast(por_mes, ym ~ quintil, value.var = "rm")
  if (!all(c("1","5") %in% names(sm))) return(NULL)
  setnames(sm, c("1","5"), c("q1","q5")); sm <- sm[is.finite(q1) & is.finite(q5)]
  sm[, spread := q5 - q1]; nmes <- nrow(sm)
  if (nmes < 6) return(NULL)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(nmes)); p_fm <- 2*pt(-abs(t_fm), df = nmes-1)
  sh <- media/dp*sqrt(12)
  cat(sprintf("%-32s h=%d %2d meses n_min=%3d spread=%+7.2fpp/mes (%+6.1f%%/ano) Sharpe=%5.2f t=%6.2f p=%.5f | Bonf:%s\n",
              nome, h, nmes, n_min, 100*media, 100*((1+media)^12-1), sh, t_fm, p_fm,
              ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
  data.table(sinal = nome, horizonte = h, n_meses = nmes, n_min_grupo_mes = n_min,
             spread_pp = 100*media, sharpe = sh, t_fm = t_fm, p_fm = p_fm, sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
}

resultados <- list()
cat("\n===== CIO Peer Momentum: rede BAIXO-beta vs ALTO-beta vs baseline (todos os fundos) =====\n")
cio_full <- fread(file.path(OUT, "cio_peer_return.csv"))[, .(ticker, ym, peer_ret)]
for (h in c(1,3,6)) {
  for (rede in list(list(dt=res_baixo, nome="CIO|rede BAIXO-beta(conservador)"),
                     list(dt=res_alto,  nome="CIO|rede ALTO-beta(agressivo)"),
                     list(dt=cio_full,  nome="CIO|rede COMPLETA(baseline)"))) {
    m <- copy(rede$dt); m[, ym_ret := addm(ym, h)]
    m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
    r <- fama_macbeth_recut(m, "peer_ret", "retorno", rede$nome, h)
    if (!is.null(r)) resultados[[length(resultados)+1]] <- r
  }
}

R <- rbindlist(resultados, fill = TRUE)
fwrite(R, file.path(OUT, "candidatos_44_cio_x_beta_fundo.csv"))
cat("\n\n===== RESUMO (ordenado por p) =====\n")
print(R[order(p_fm)])
cat("\nOK\n")
