# =============================================================================
# 16_cio_h2_e_janela_agregada.R  (exploracao_sinais)
#
# Refinamentos do CIO Peer Momentum (unico candidato que sobreviveu),
# tentando ver se o sinal aparece perto de h=3 de forma PRINCIPIADA (nao
# so testar h=2,3,4,5... ate um bater p<0,05 -- isso seria data dredging
# dentro do proprio candidato):
#
# (R) h=2: ponto intermediario natural entre h=1 (significativo) e h=3
#     (nao significativo) -- se o efeito decai suavemente, h=2 deveria
#     estar "no meio do caminho" em magnitude e significancia.
# (S) retorno ACUMULADO de t+1 a t+3 (janela de 3 meses, nao ponto no
#     mes 3 especificamente) -- testa se o efeito de h=1 "persiste"
#     quando acumulado, mesmo que o retorno de CADA mes individual
#     (t+2, t+3) nao seja mais previsivel isoladamente.
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
  cat(sprintf("   [N por quintil-mes] min=%d mediana=%.0f max=%d\n", min(n_por_mes$N), median(n_por_mes$N), max(n_por_mes$N)))
  por_mes <- teste[quintil %in% c(1,5), .(retorno_medio = mean(get(var_y))), by = .(ym, quintil)]
  sm <- dcast(por_mes, ym ~ quintil, value.var = "retorno_medio")
  if (!all(c("1","5") %in% names(sm))) return(NULL)
  setnames(sm, c("1","5"), c("q1","q5"))
  sm <- sm[is.finite(q1) & is.finite(q5)]
  sm[, spread := q5 - q1]
  n_meses <- nrow(sm)
  if (n_meses < 6) return(NULL)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(n_meses)); p_fm <- 2*pt(-abs(t_fm), df = n_meses-1)
  cat(sprintf("%-30s h=%s [FM] %2d meses spread=%+7.2fpp/mes t=%6.2f p=%.4f\n", nome, h, n_meses, 100*media, t_fm, p_fm))
  data.table(sinal = nome, horizonte = h, n_meses = n_meses, spread_medio_pp = 100*media, t_fm = t_fm, p_fm = p_fm)
}

resultados <- list()

# (R) h=2
cat("===== (R) CIO peer momentum, h=2 =====\n")
m2 <- copy(cio_dt); m2[, ym_ret := addm(ym, 2)]
m2 <- merge(m2, precos, by.x = c("ticker_ret","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
m2 <- m2[is.finite(retorno) & is.finite(peer_ret)]
r <- fama_macbeth(m2, "peer_ret", "retorno", "CIO peer momentum", "2")
if (!is.null(r)) resultados[[length(resultados)+1]] <- r

# (S) retorno acumulado t+1 a t+3
cat("\n===== (S) CIO peer momentum -> retorno ACUMULADO (t+1 a t+3) =====\n")
precos_w <- dcast(precos, ymk ~ ticker, value.var = "retorno")
acumula_3m <- function(tk, ym0) {
  meses3 <- c(addm(ym0,1), addm(ym0,2), addm(ym0,3))
  sub <- precos[ticker == tk & ymk %in% meses3]
  if (nrow(sub) < 3) return(NA_real_)
  prod(1+sub$retorno) - 1
}
# mais rapido: merge dos 3 meses individualmente e computa produto, evita loop linha a linha
r1 <- precos[, .(ticker, ym1 = addm(ymk,-1), ret1 = retorno)]
r2 <- precos[, .(ticker, ym1 = addm(ymk,-2), ret2 = retorno)]
r3 <- precos[, .(ticker, ym1 = addm(ymk,-3), ret3 = retorno)]
racc <- merge(r1, r2, by = c("ticker","ym1"))
racc <- merge(racc, r3, by = c("ticker","ym1"))
racc[, ret_acum_3m := (1+ret1)*(1+ret2)*(1+ret3) - 1]
racc <- racc[, .(ticker, ym = ym1, ret_acum_3m)]

m3 <- merge(cio_dt, racc, by.x = c("ticker_ret","ym"), by.y = c("ticker","ym"))
m3 <- m3[is.finite(ret_acum_3m) & is.finite(peer_ret)]
r <- fama_macbeth(m3, "peer_ret", "ret_acum_3m", "CIO peer momentum", "acum-1a3")
if (!is.null(r)) resultados[[length(resultados)+1]] <- r

R <- rbindlist(resultados)
fwrite(R, file.path(OUT, "candidatos_16_refinamentos.csv"))
cat("\nOK\n")
