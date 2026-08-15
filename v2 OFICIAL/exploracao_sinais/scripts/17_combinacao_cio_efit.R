# =============================================================================
# 17_combinacao_cio_efit.R  (exploracao_sinais)
#
# (T) COMBINACAO: os 2 candidatos com R2_OOS mais consistente/positivo
#     entre TODOS os testados foram CIO Peer Momentum (h=1, unico que
#     sobreviveu de fato) e E[FIT] (candidato 11, R2_OOS positivo em
#     quase todos os horizontes mas sem long-short executavel). Testa se
#     a COMBINACAO (soma dos 2 sinais, cada um padronizado por mes) tem
#     mais sinal que qualquer um isolado -- motivado por literatura de
#     ensemble/combinacao de sinais fracos (Green-Hand-Zhang "combination
#     forecasts" e afins).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

# ---- CIO peer return (h=1) ----
cio_dt <- fread(file.path(OUT, "cio_peer_return.csv"))
cio_dt[, ticker_ret := ticker]

# ---- E[FIT] (reconstroi rapido, mesma logica do script 08) ----
pp <- fread(file.path(DATA, "painel_multiativo_final.csv"),
            select = c("cod_fundo","ativo","ym","peso","aum_prev","flow_aum"))
pp[, cod_fundo := as.character(cod_fundo)]
rf <- fread(file.path(DATA, "retorno_fundo_mensal.csv"))
rf[, cod_fundo := as.character(cod_fundo)]
setorder(rf, cod_fundo, ymk)
rf[, log_ret := log(1 + retorno_fundo)]
rf[, skill_12m := exp(frollsum(log_ret, 12, align = "right")) - 1]
rf_skill <- rf[!is.na(skill_12m), .(cod_fundo, ym = ymk, skill_12m)]
fluxo_fundo <- unique(pp[, .(cod_fundo, ym, flow_aum)]); fluxo_fundo <- fluxo_fundo[is.finite(flow_aum)]
skill_lag <- copy(rf_skill); skill_lag[, ym := addm(ym, 1)]
fluxo_fundo <- merge(fluxo_fundo, skill_lag, by = c("cod_fundo","ym"), all.x = TRUE)
fluxo_fundo <- fluxo_fundo[is.finite(skill_12m)]
treino_1a <- fluxo_fundo[ym < CORTE]
fit_1a <- lm(flow_aum ~ skill_12m, data = treino_1a)
fluxo_fundo[, flow_aum_esperado := predict(fit_1a, newdata = fluxo_fundo)]
peso_prev <- pp[, .(cod_fundo, ativo, ym = addm(ym, 1), peso_prev = peso)]
d <- merge(pp[, .(cod_fundo, ativo, ym, aum_prev)], peso_prev, by = c("cod_fundo","ativo","ym"))
d <- merge(d, fluxo_fundo[, .(cod_fundo, ym, flow_aum_esperado)], by = c("cod_fundo","ym"))
d <- d[is.finite(peso_prev) & peso_prev > 0 & is.finite(aum_prev) & is.finite(flow_aum_esperado)]
d[, peso_valor := peso_prev * aum_prev]
efit <- d[, .(EFIT = sum(peso_valor * flow_aum_esperado) / sum(peso_valor), n_fundos = .N), by = .(ativo, ym)]
efit <- efit[n_fundos >= 10]
efit[, ticker := trimws(sub(".*- ", "", ativo))]

# ---- junta os dois sinais no mesmo ativo-mes ----
comb <- merge(cio_dt[, .(ticker, ym, peer_ret)], efit[, .(ticker, ym, EFIT)], by = c("ticker","ym"))
cat("Ativo-mes com os 2 sinais disponiveis:", nrow(comb), "\n")
comb[, `:=`(peer_ret_z = scale(peer_ret), EFIT_z = scale(EFIT)), by = ym]
comb[, sinal_combinado := peer_ret_z + EFIT_z]

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
  por_mes <- teste[quintil %in% c(1,5), .(retorno_medio = mean(get(var_y))), by = .(ym, quintil)]
  sm <- dcast(por_mes, ym ~ quintil, value.var = "retorno_medio")
  if (!all(c("1","5") %in% names(sm))) return(NULL)
  setnames(sm, c("1","5"), c("q1","q5")); sm <- sm[is.finite(q1) & is.finite(q5)]
  sm[, spread := q5-q1]; n_meses <- nrow(sm)
  if (n_meses < 6) return(NULL)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(n_meses)); p_fm <- 2*pt(-abs(t_fm), df=n_meses-1)
  cat(sprintf("%-25s h=%2d [FM] %2d meses spread=%+6.2fpp/mes t=%5.2f p=%.4f\n", nome, h, n_meses, 100*media, t_fm, p_fm))
  data.table(sinal = nome, horizonte = h, n_meses = n_meses, spread_pp = 100*media, t_fm = t_fm, p_fm = p_fm)
}

resultados <- list()
for (h in c(1,2,3,6)) {
  m <- copy(comb); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(sinal_combinado)]
  r <- fama_macbeth(m, "sinal_combinado", "retorno", "CIO+E[FIT] combinado", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

R <- rbindlist(resultados)
fwrite(R, file.path(OUT, "candidatos_17_combinacao.csv"))
cat("\nOK\n")
