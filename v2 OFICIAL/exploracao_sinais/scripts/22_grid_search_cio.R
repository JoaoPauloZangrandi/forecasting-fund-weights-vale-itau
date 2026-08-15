# =============================================================================
# 22_grid_search_cio.R  (exploracao_sinais)
#
# Varredura sistematica de especificacoes do CIO Peer Momentum -- em vez
# de testar 1 especificacao de cada vez, roda uma GRADE completa de
# variacoes de construcao e registra TODOS os resultados (nao so os que
# "deram certo") pra ver se existe uma VIZINHANCA robusta de
# especificacoes que funcionam, ou se e' um ponto isolado (mais evidencia
# de fragilidade se so' a especificacao exata original funcionar).
#
# Dimensoes da grade:
#  - piso de conexao (percentil da CIO pra contar como "peer"): 50,60,70,75,80,90
#  - minimo de peers exigido: 3, 5, 10
#  - ponderacao do peer_ret: media ponderada por CIO (original) vs media simples
#  - horizonte: 1, 2, 3
# Total: 6 x 3 x 2 x 3 = 108 especificacoes, cada uma testada com OOS
# linear + Fama-MacBeth.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp <- pp[is.finite(aum_prev) & aum_prev > 0 & peso > 0]
pp[, ticker := trimws(sub(".*- ", "", ativo))]
pp <- pp[ticker %in% unique(precos$ticker)]
pp[, valor := peso * aum_prev]
meses <- sort(unique(pp$ym))

# ---- pre-computa a matriz CIO por mes UMA VEZ (cara), reutiliza pra todas as
#      combinacoes de piso/min_peers/ponderacao -- so' o piso de corte muda o
#      subconjunto usado, nao precisa recalcular a matriz toda vez ----
cat("Pre-computando matrizes CIO por mes (uma vez, reaproveitada em todas as especificacoes)...\n")
t0 <- Sys.time()
lista_cio_mes <- list()
lista_ret_mes <- list()
for (mm in meses) {
  d_mes <- pp[ym == mm]
  if (uniqueN(d_mes$ticker) < 15 || uniqueN(d_mes$cod_fundo) < 15) next
  d_mes[, cod_fundo := as.character(cod_fundo)]
  wide <- dcast(d_mes, cod_fundo ~ ticker, value.var = "valor", fun.aggregate = sum, fill = 0)
  X <- as.matrix(wide[, -1, with = FALSE])
  if (ncol(X) < 15) next
  O <- crossprod(X)
  diag_O <- diag(O); diag_O[diag_O <= 0] <- NA
  CIO <- O / sqrt(outer(diag_O, diag_O))
  diag(CIO) <- 0
  CIO[!is.finite(CIO)] <- 0
  lista_cio_mes[[as.character(mm)]] <- CIO
  ret_mes <- precos[ymk == mm & ticker %in% colnames(X)]
  rv <- setNames(rep(NA_real_, ncol(X)), colnames(X)); rv[ret_mes$ticker] <- ret_mes$retorno
  lista_ret_mes[[as.character(mm)]] <- rv
}
cat("Tempo pre-computo:", round(as.numeric(Sys.time()-t0,units="secs"),1), "s |", length(lista_cio_mes), "meses\n")

calc_peer_ret_espec <- function(piso_pct, min_peers, ponderado) {
  resultado <- list()
  for (mm_chr in names(lista_cio_mes)) {
    CIO <- lista_cio_mes[[mm_chr]]; ret_vec <- lista_ret_mes[[mm_chr]]
    tickers_mes <- rownames(CIO)
    peer_ret <- rep(NA_real_, length(tickers_mes))
    for (i in seq_along(tickers_mes)) {
      pesos_i <- CIO[i,]
      validos <- is.finite(ret_vec) & pesos_i > 0
      if (sum(validos) < min_peers) next
      piso <- quantile(pesos_i[validos], piso_pct, na.rm=TRUE)
      usar <- validos & pesos_i >= piso
      if (sum(usar) < min_peers) usar <- validos
      if (sum(usar) < min_peers) next
      peer_ret[i] <- if (ponderado) weighted.mean(ret_vec[usar], pesos_i[usar]) else mean(ret_vec[usar])
    }
    ok <- !is.na(peer_ret)
    if (any(ok)) resultado[[mm_chr]] <- data.table(ticker=tickers_mes[ok], ym=as.integer(mm_chr), peer_ret=peer_ret[ok])
  }
  rbindlist(resultado)
}

testa_espec <- function(cio_dt, h, piso_pct, min_peers, ponderado) {
  cio_dt[, ticker_ret := ticker]
  m <- copy(cio_dt); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker_ret","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(peer_ret)]
  treino <- m[ym < CORTE]; teste <- copy(m[ym >= CORTE])
  if (nrow(treino) < 100 || nrow(teste) < 50) return(NULL)

  fit_lm <- lm(retorno ~ peer_ret, data = treino)
  pred <- predict(fit_lm, newdata = teste)
  sse_m <- sum((teste$retorno-pred)^2); sse_n <- sum((teste$retorno-mean(treino$retorno))^2)
  r2_oos <- 1 - sse_m/sse_n

  breaks <- unique(quantile(treino$peer_ret, seq(0,1,0.2), na.rm=TRUE))
  if (length(breaks) < 4) return(data.table(piso_pct, min_peers, ponderado, h, n_treino=nrow(treino), n_teste=nrow(teste),
                                             r2_oos_pct=100*r2_oos, n_meses=NA, spread_pp=NA, t_fm=NA, p_fm=NA))
  breaks[1]<--Inf; breaks[length(breaks)]<-Inf
  teste[, quintil := as.integer(cut(peer_ret, breaks, labels=FALSE, include.lowest=TRUE))]
  ng <- length(breaks)-1
  if (ng<5) teste[, quintil := ifelse(quintil==1,1,ifelse(quintil==ng,5,quintil))]
  por_mes <- teste[quintil %in% c(1,5), .(rm=mean(retorno)), by=.(ym,quintil)]
  sm <- dcast(por_mes, ym~quintil, value.var="rm")
  if (!all(c("1","5") %in% names(sm))) return(data.table(piso_pct, min_peers, ponderado, h, n_treino=nrow(treino), n_teste=nrow(teste),
                                                            r2_oos_pct=100*r2_oos, n_meses=NA, spread_pp=NA, t_fm=NA, p_fm=NA))
  setnames(sm, c("1","5"), c("q1","q5")); sm <- sm[is.finite(q1)&is.finite(q5)]
  sm[, spread:=q5-q1]; nmes <- nrow(sm)
  if (nmes < 6) return(data.table(piso_pct, min_peers, ponderado, h, n_treino=nrow(treino), n_teste=nrow(teste),
                                   r2_oos_pct=100*r2_oos, n_meses=nmes, spread_pp=NA, t_fm=NA, p_fm=NA))
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(nmes)); p_fm <- 2*pt(-abs(t_fm), df=nmes-1)
  data.table(piso_pct, min_peers, ponderado, h, n_treino=nrow(treino), n_teste=nrow(teste),
             r2_oos_pct=100*r2_oos, n_meses=nmes, spread_pp=100*media, t_fm=t_fm, p_fm=p_fm)
}

grade <- CJ(piso_pct = c(0.50,0.60,0.70,0.75,0.80,0.90),
            min_peers = c(3,5,10),
            ponderado = c(TRUE,FALSE))
cat("\nTotal de combinacoes de construcao do sinal:", nrow(grade), "\n")

resultados <- list()
cont <- 0
for (i in seq_len(nrow(grade))) {
  cio_dt <- calc_peer_ret_espec(grade$piso_pct[i], grade$min_peers[i], grade$ponderado[i])
  if (nrow(cio_dt) < 100) next
  for (h in c(1,2,3)) {
    cont <- cont + 1
    r <- testa_espec(copy(cio_dt), h, grade$piso_pct[i], grade$min_peers[i], grade$ponderado[i])
    if (!is.null(r)) resultados[[length(resultados)+1]] <- r
    if (cont %% 20 == 0) cat("  ...", cont, "especificacoes testadas\n")
  }
}
R <- rbindlist(resultados, fill = TRUE)
fwrite(R, file.path(OUT, "grid_search_cio_completo.csv"))

cat("\n===== RESUMO: quantas especificacoes por horizonte passam de p<0,05 (Fama-MacBeth)? =====\n")
print(R[, .(total = .N, sig_5pct = sum(p_fm < 0.05, na.rm=TRUE),
            sig_10pct = sum(p_fm < 0.10, na.rm=TRUE)), by = h])

cat("\n===== Especificacoes com p<0,05, por horizonte =====\n")
print(R[p_fm < 0.05][order(h, p_fm)])

cat("\nOK -- total de especificacoes testadas nesta grade:", nrow(R), "\n")
