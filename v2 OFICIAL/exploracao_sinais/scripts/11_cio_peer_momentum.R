# =============================================================================
# 11_cio_peer_momentum.R  (exploracao_sinais)
#
# (N) CIO PEER MOMENTUM (Ying 2024, JFE; mesma familia de Anton-Polk 2014
#     "Connected Stocks" e Cohen-Frazzini "economic links"): informacao se
#     difunde GRADUALMENTE entre acoes com donos institucionais em comum --
#     investidores que reagem a um choque na acao A demoram a reajustar
#     posicoes em OUTRAS acoes (B) que os mesmos fundos tambem carregam.
#     Retorno de "pares" (peer) via ownership em comum HOJE deveria prever
#     retorno da propria acao no mes SEGUINTE. Frequencia MENSAL no
#     mecanismo -- bate direto com nosso painel, diferente do Comomentum
#     (que precisava semanal e deu artefato por causa disso).
#
# CONSTRUCAO (cosine similarity de posse ponderada por AUM, por mes):
#   X_{f,i,t} = AUM_{f,t} * peso_{f,i,t}   (posicao em R$ do fundo f na acao i)
#   O_{i,j,t} = sum_f X_{f,i,t} * X_{f,j,t}           (overlap bruto)
#   CIO_{i,j,t} = O_{i,j,t} / sqrt(O_{i,i,t} * O_{j,j,t})   (cosine, em [0,1])
#   peer_ret_{i,t} = media ponderada de retorno_{j,t} (j!=i) usando CIO_{i,j,t}
#     como peso -- SO' pares com CIO acima de um piso (evita ruido de pares
#     quase-nao-conectados dominando a media)
#
# LICAO DO COMOMENTUM (script 09/10): SEMPRE checar o N efetivo por
# observacao (quantos "peers" entram na media) antes de acreditar num
# resultado -- grupos degenerados geram significancia espuria.
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
pp[, valor := peso * aum_prev]
pp[, ticker := trimws(sub(".*- ", "", ativo))]

# so' ativos com preco disponivel (senao nao da pra testar retorno)
tickers_com_preco <- unique(precos$ticker)
pp <- pp[ticker %in% tickers_com_preco]

meses <- sort(unique(pp$ym))
cat("Meses no painel:", length(meses), "\n")

calc_peer_return_mes <- function(mes_atual) {
  d_mes <- pp[ym == mes_atual]
  if (uniqueN(d_mes$ticker) < 15 || uniqueN(d_mes$cod_fundo) < 15) return(NULL)

  # matriz fundo x ativo (valor em R$), formato esparso convertido pra denso (universo cabe em memoria)
  d_mes[, cod_fundo := as.character(cod_fundo)]
  wide <- dcast(d_mes, cod_fundo ~ ticker, value.var = "valor", fun.aggregate = sum, fill = 0)
  X <- as.matrix(wide[, -1, with = FALSE])
  tickers_mes <- colnames(X)
  if (ncol(X) < 15) return(NULL)

  O <- crossprod(X)  # ticker x ticker, overlap bruto (t(X) %*% X)
  diag_O <- diag(O)
  diag_O[diag_O <= 0] <- NA
  denom <- sqrt(outer(diag_O, diag_O))
  CIO <- O / denom
  diag(CIO) <- 0
  CIO[!is.finite(CIO)] <- 0

  # retorno do proprio mes (pra usar como "retorno dos peers")
  ret_mes <- precos[ymk == mes_atual & ticker %in% tickers_mes]
  if (nrow(ret_mes) < 10) return(NULL)
  ret_vec <- setNames(rep(NA_real_, length(tickers_mes)), tickers_mes)
  ret_vec[ret_mes$ticker] <- ret_mes$retorno

  # piso de conexao: so conta como "peer" se CIO > percentil 75 da linha (conexoes mais fortes),
  # e exige pelo menos 5 peers validos com retorno disponivel
  peer_ret <- rep(NA_real_, length(tickers_mes)); n_peers <- rep(0L, length(tickers_mes))
  for (i in seq_along(tickers_mes)) {
    pesos_i <- CIO[i, ]
    validos <- is.finite(ret_vec) & pesos_i > 0
    if (sum(validos) < 5) next
    piso <- quantile(pesos_i[validos], 0.75, na.rm = TRUE)
    usar <- validos & pesos_i >= piso
    if (sum(usar) < 5) usar <- validos  # relaxa se o piso deixar poucos
    if (sum(usar) < 5) next
    peer_ret[i] <- weighted.mean(ret_vec[usar], pesos_i[usar])
    n_peers[i] <- sum(usar)
  }
  data.table(ticker = tickers_mes, ym = mes_atual, peer_ret = peer_ret, n_peers = n_peers)[!is.na(peer_ret)]
}

cat("Calculando CIO peer return mes a mes (pode demorar, matriz por mes)...\n")
t0 <- Sys.time()
resultado_mensal <- lapply(meses, calc_peer_return_mes)
cat("Tempo:", round(as.numeric(Sys.time()-t0, units="secs"),1), "s\n")
cio_dt <- rbindlist(Filter(Negate(is.null), resultado_mensal))
cat("Ativo-mes com peer_ret calculavel:", nrow(cio_dt), "| n_peers medio:", round(mean(cio_dt$n_peers),1),
    "| n_peers minimo:", min(cio_dt$n_peers), "\n")
fwrite(cio_dt, file.path(OUT, "cio_peer_return.csv"))

# =============================================================================
# TESTE: peer_ret(t) prediz retorno(t+h) da PROPRIA acao?
# =============================================================================
teste_oos_linear <- function(m, var_x, var_y, nome, h) {
  treino <- m[ym < CORTE]; teste <- m[ym >= CORTE]
  if (nrow(treino) < 50 || nrow(teste) < 30) return(NULL)
  form <- as.formula(paste(var_y, "~", var_x))
  fit_lm <- lm(form, data = treino)
  alpha <- coef(fit_lm)[1]; beta <- coef(fit_lm)[2]
  pred <- alpha + beta * teste[[var_x]]
  sse_m <- sum((teste[[var_y]] - pred)^2); sse_n <- sum((teste[[var_y]] - mean(treino[[var_y]]))^2)
  r2_oos <- 1 - sse_m/sse_n
  s_full <- summary(lm(form, data = m))$coefficients
  cat(sprintf("%-30s h=%2d [linear] n_tr=%5d n_te=%4d beta=%9.5f R2_OOS=%8.4f%% p_full=%.4f\n",
              nome, h, nrow(treino), nrow(teste), beta, 100*r2_oos, s_full[2,4]))
  data.table(sinal = nome, horizonte = h, r2_oos_pct = 100*r2_oos, p_full = s_full[2,4])
}
fama_macbeth <- function(m, var_x, var_y, nome, h) {
  treino <- m[ym < CORTE]; teste <- copy(m[ym >= CORTE])
  if (nrow(treino) < 50 || nrow(teste) < 30) return(NULL)
  breaks <- unique(quantile(treino[[var_x]], seq(0,1,0.2), na.rm = TRUE))
  if (length(breaks) < 4) return(NULL)
  breaks[1] <- -Inf; breaks[length(breaks)] <- Inf
  teste[, quintil := as.integer(cut(get(var_x), breaks, labels = FALSE, include.lowest = TRUE))]
  n_grupos <- length(breaks) - 1
  if (n_grupos < 5) teste[, quintil := ifelse(quintil == 1, 1, ifelse(quintil == n_grupos, 5, quintil))]
  teste <- teste[!is.na(quintil)]
  n_por_mes <- teste[quintil %in% c(1,5), .N, by = .(ym, quintil)]
  cat(sprintf("   [checagem N por quintil-mes] min=%d | mediana=%.0f | max=%d\n",
              min(n_por_mes$N), median(n_por_mes$N), max(n_por_mes$N)))
  por_mes <- teste[quintil %in% c(1,5), .(retorno_medio = mean(get(var_y)), n=.N), by = .(ym, quintil)]
  sm <- dcast(por_mes, ym ~ quintil, value.var = "retorno_medio")
  if (!all(c("1","5") %in% names(sm))) return(NULL)
  setnames(sm, c("1","5"), c("q1","q5"))
  sm <- sm[is.finite(q1) & is.finite(q5)]
  sm[, spread := q5 - q1]
  n_meses <- nrow(sm)
  if (n_meses < 6) return(NULL)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(n_meses)); p_fm <- 2*pt(-abs(t_fm), df = n_meses-1)
  cat(sprintf("%-30s h=%2d [FM] %2d meses spread=%+7.2fpp/mes (anualizado~%+6.1f%%) t=%6.2f p=%.4f\n",
              nome, h, n_meses, 100*media, 100*((1+media)^12-1), t_fm, p_fm))
  data.table(sinal = nome, horizonte = h, n_meses = n_meses, spread_medio_pp = 100*media, t_fm = t_fm, p_fm = p_fm)
}

resultados_lin <- list(); resultados_fm <- list()
cio_dt[, ticker_ret := ticker]
for (h in c(1,3,6,12)) {
  m <- copy(cio_dt); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker_ret","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(peer_ret)]
  r1 <- teste_oos_linear(m, "peer_ret", "retorno", "CIO peer momentum", h)
  r2 <- fama_macbeth(m, "peer_ret", "retorno", "CIO peer momentum", h)
  if (!is.null(r1)) resultados_lin[[length(resultados_lin)+1]] <- r1
  if (!is.null(r2)) resultados_fm[[length(resultados_fm)+1]] <- r2
}

RL <- rbindlist(resultados_lin); RF <- rbindlist(resultados_fm)
fwrite(RL, file.path(OUT, "candidatos_11_linear.csv"))
fwrite(RF, file.path(OUT, "candidatos_11_fama_macbeth.csv"))
cat("\nOK\n")
