# =============================================================================
# 18_window_dressing_calendario.R  (exploracao_sinais)
#
# (U) WINDOW DRESSING / EFEITO DE CALENDARIO: gestores ajustam carteira
#     antes de datas de divulgacao (fim de trimestre/ano) pra "parecer
#     bem" no relatorio -- compram vencedoras recentes, vendem perdedoras,
#     mesmo sem convicao real. Testa se o sinal CIO Peer Momentum (ou a
#     demanda agregada revelada) tem comportamento DIFERENTE nos meses de
#     fim de trimestre (mar/jun/set/dez) vs os outros -- hipotese: se
#     window dressing contamina o sinal nesses meses, ele deveria ser mais
#     FRACO ou de sinal INVERTIDO logo depois (fundos revertem em jan/abr/
#     jul/out o que fizeram artificialmente antes).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

cio_dt <- fread(file.path(OUT, "cio_peer_return.csv"))
cio_dt[, ticker_ret := ticker]
cio_dt[, mes := ym %% 100L]
cio_dt[, fim_trimestre := mes %in% c(3,6,9,12)]

fama_macbeth <- function(m, var_x, var_y, nome, h) {
  treino <- m[ym < CORTE]; teste <- copy(m[ym >= CORTE])
  if (nrow(treino) < 30 || nrow(teste) < 20) { cat(sprintf("%s: amostra pequena demais (tr=%d te=%d)\n", nome, nrow(treino), nrow(teste))); return(NULL) }
  breaks <- unique(quantile(treino[[var_x]], seq(0,1,0.2), na.rm = TRUE))
  if (length(breaks) < 4) return(NULL)
  breaks[1] <- -Inf; breaks[length(breaks)] <- Inf
  teste[, quintil := as.integer(cut(get(var_x), breaks, labels = FALSE, include.lowest = TRUE))]
  n_grupos <- length(breaks) - 1
  if (n_grupos < 5) teste[, quintil := ifelse(quintil == 1, 1, ifelse(quintil == n_grupos, 5, quintil))]
  teste <- teste[!is.na(quintil)]
  por_mes <- teste[quintil %in% c(1,5), .(retorno_medio = mean(get(var_y))), by = .(ym, quintil)]
  sm <- dcast(por_mes, ym ~ quintil, value.var = "retorno_medio")
  if (!all(c("1","5") %in% names(sm))) return(NULL)
  setnames(sm, c("1","5"), c("q1","q5")); sm <- sm[is.finite(q1) & is.finite(q5)]
  sm[, spread := q5-q1]; n_meses <- nrow(sm)
  if (n_meses < 5) { cat(sprintf("%s: so %d meses, resultado fraco\n", nome, n_meses)) }
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(n_meses)); p_fm <- 2*pt(-abs(t_fm), df = max(n_meses-1,1))
  cat(sprintf("%-40s h=%d [FM] %2d meses spread=%+7.2fpp/mes t=%6.2f p=%.4f\n", nome, h, n_meses, 100*media, t_fm, p_fm))
  data.table(sinal = nome, horizonte = h, n_meses = n_meses, spread_pp = 100*media, t_fm = t_fm, p_fm = p_fm)
}

resultados <- list()

cat("===== CIO peer momentum, h=1, SO MESES DE FIM DE TRIMESTRE (mar/jun/set/dez) =====\n")
h <- 1
m <- copy(cio_dt); m[, ym_ret := addm(ym, h)]
m <- merge(m, precos, by.x = c("ticker_ret","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
m <- m[is.finite(retorno) & is.finite(peer_ret)]

m_fim <- m[fim_trimestre == TRUE]
m_naofim <- m[fim_trimestre == FALSE]
cat("Obs em meses de fim de trimestre:", nrow(m_fim), "| outros meses:", nrow(m_naofim), "\n")
r1 <- fama_macbeth(m_fim, "peer_ret", "retorno", "CIO (SO fim-trimestre)", h)
r2 <- fama_macbeth(m_naofim, "peer_ret", "retorno", "CIO (SO nao-fim-trimestre)", h)
if (!is.null(r1)) resultados[[length(resultados)+1]] <- r1
if (!is.null(r2)) resultados[[length(resultados)+1]] <- r2

cat("\n===== CIO peer momentum, h=1, SO MES DE DEZEMBRO (fechamento anual, window dressing mais forte) =====\n")
m_dez <- m[mes == 12]
cat("Obs em dezembro:", nrow(m_dez), "\n")
r3 <- fama_macbeth(m_dez, "peer_ret", "retorno", "CIO (SO dezembro)", h)
if (!is.null(r3)) resultados[[length(resultados)+1]] <- r3

cat("\n===== Efeito de JANEIRO (reversao pos-window-dressing)? peer_ret medido em DEZ prediz retorno de JAN =====\n")
# aqui: olhar retorno em jan diretamente (ja fazemos isso via h=1 quando ym=dez -> ym_ret=jan,
# mas queremos comparar especificamente contra os OUTROS meses)
dez_para_jan <- m[mes == 12]
cat("Spread medio (Q5-Q1) especificamente dez->jan, mes a mes:\n")
if (nrow(dez_para_jan) > 20) {
  breaks <- unique(quantile(dez_para_jan[ym<CORTE]$peer_ret, seq(0,1,0.2), na.rm=TRUE))
  if (length(breaks) >= 4) {
    breaks[1] <- -Inf; breaks[length(breaks)] <- Inf
    dez_para_jan[, quintil := as.integer(cut(peer_ret, breaks, labels=FALSE, include.lowest=TRUE))]
    print(dez_para_jan[, .(retorno_medio = mean(retorno), n=.N), by=.(ym,quintil)][order(ym,quintil)])
  }
}

R <- rbindlist(resultados)
fwrite(R, file.path(OUT, "candidatos_18_calendario.csv"))
cat("\nOK\n")
