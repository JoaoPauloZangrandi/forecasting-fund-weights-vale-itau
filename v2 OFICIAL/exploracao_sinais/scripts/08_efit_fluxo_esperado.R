# =============================================================================
# 08_efit_fluxo_esperado.R  (exploracao_sinais)
#
# (J) E[FIT] -- Lou (2012) usa fluxo ESPERADO (previsto pelo alfa passado do
#     fundo), nao fluxo realizado, argumentando que so' a parte previsivel
#     do fluxo-induzido pode gerar retorno futuro sem ser tautologica (fluxo
#     realizado pode ja refletir informacao contemporanea). O candidato #1
#     (FIT, script 105/01) usou fluxo REALIZADO -- essa e' a diferenca
#     estrutural mais provavel apontada pela pesquisa pra explicar por que
#     Lou achou algo e o TCC nao. Este script constroi a versao "esperada".
#
# Metodologia: pra cada fundo, estima flow_aum esperado no mes t como
# funcao do desempenho (skill_12m, retorno acumulado 12m) do fundo ATE
# t-1 -- reestimado so' com dado de treino, aplicado fora da amostra.
# Depois reconstroi FIT com esse fluxo esperado (nao o realizado).
#
# (K) FIT SUAVIZADO (media movel 3 meses) -- Lou explica parte do sucesso
#     pela agregacao cancelar ruido idiossincratico; testa se suavizar o
#     FIT no tempo (nao so' entre fundos) ajuda.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

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
  cat(sprintf("%-30s h=%2d [FM] %2d meses spread=%+7.2fpp/mes t=%6.2f p=%.4f\n", nome, h, n_meses, 100*media, t_fm, p_fm))
  data.table(sinal = nome, horizonte = h, n_meses = n_meses, spread_medio_pp = 100*media, t_fm = t_fm, p_fm = p_fm)
}

resultados_lin <- list(); resultados_fm <- list()

# =============================================================================
# (J) E[FIT] -- fluxo esperado em vez de realizado
# =============================================================================
cat("===== (J) E[FIT]: fluxo esperado (previsto por skill passado do fundo) =====\n")
pp <- fread(file.path(DATA, "painel_multiativo_final.csv"),
            select = c("cod_fundo","ativo","ym","peso","aum_prev","flow_aum"))
pp[, cod_fundo := as.character(cod_fundo)]

rf <- fread(file.path(DATA, "retorno_fundo_mensal.csv"))
rf[, cod_fundo := as.character(cod_fundo)]
setorder(rf, cod_fundo, ymk)
rf[, log_ret := log(1 + retorno_fundo)]
rf[, skill_12m := exp(frollsum(log_ret, 12, align = "right")) - 1]
rf_skill <- rf[!is.na(skill_12m), .(cod_fundo, ym = ymk, skill_12m)]

# flow_aum de cada fundo-mes, com skill do fundo ATE o mes anterior (t-1, sem look-ahead)
fluxo_fundo <- unique(pp[, .(cod_fundo, ym, flow_aum)])
fluxo_fundo <- fluxo_fundo[is.finite(flow_aum)]
skill_lag <- copy(rf_skill); skill_lag[, ym := addm(ym, 1)]  # skill calculado ate t-1, usado pra prever fluxo em t
fluxo_fundo <- merge(fluxo_fundo, skill_lag, by = c("cod_fundo","ym"), all.x = TRUE)
fluxo_fundo <- fluxo_fundo[is.finite(skill_12m)]
cat("Fundo-meses com flow_aum + skill defasado disponiveis:", nrow(fluxo_fundo), "\n")

# estima flow_aum esperado SO no treino, aplica a fundo-mes inteiro (1a estagio, fora do loop de retorno)
treino_1a <- fluxo_fundo[ym < CORTE]
fit_1a <- lm(flow_aum ~ skill_12m, data = treino_1a)
cat("1o estagio (flow_aum ~ skill_12m, so treino): coef=", round(coef(fit_1a)[2],5),
    " R2=", round(summary(fit_1a)$r.squared,5), "\n")
fluxo_fundo[, flow_aum_esperado := predict(fit_1a, newdata = fluxo_fundo)]

# reconstroi FIT com flow_aum_esperado no lugar do realizado
peso_prev <- pp[, .(cod_fundo, ativo, ym = addm(ym, 1), peso_prev = peso)]
d <- merge(pp[, .(cod_fundo, ativo, ym, aum_prev)], peso_prev, by = c("cod_fundo","ativo","ym"))
d <- merge(d, fluxo_fundo[, .(cod_fundo, ym, flow_aum_esperado)], by = c("cod_fundo","ym"))
d <- d[is.finite(peso_prev) & peso_prev > 0 & is.finite(aum_prev) & is.finite(flow_aum_esperado)]
d[, peso_valor := peso_prev * aum_prev]
efit <- d[, .(EFIT = sum(peso_valor * flow_aum_esperado) / sum(peso_valor), n_fundos = .N), by = .(ativo, ym)]
efit <- efit[n_fundos >= 10]
efit[, ticker := trimws(sub(".*- ", "", ativo))]
cat("Celulas ativo-mes com E[FIT] calculavel:", nrow(efit), "\n")

for (h in c(0,1,3,6,12)) {
  m <- copy(efit); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(EFIT)]
  r1 <- teste_oos_linear(m, "EFIT", "retorno", "E[FIT] (fluxo esperado)", h)
  r2 <- fama_macbeth(m, "EFIT", "retorno", "E[FIT] (fluxo esperado)", h)
  if (!is.null(r1)) resultados_lin[[length(resultados_lin)+1]] <- r1
  if (!is.null(r2)) resultados_fm[[length(resultados_fm)+1]] <- r2
}

# =============================================================================
# (K) FIT suavizado (media movel 3 meses)
# =============================================================================
cat("\n===== (K) FIT suavizado (media movel 3 meses) =====\n")
fit_completo <- fread(file.path(DATA, "fit_ativo_mes.csv"))
setorder(fit_completo, ativo, ym)
fit_completo[, FIT_ma3 := frollmean(FIT, 3, align = "right"), by = ativo]
fit_completo <- fit_completo[!is.na(FIT_ma3)]
fit_completo[, ticker := trimws(sub(".*- ", "", ativo))]

for (h in c(0,1,3,6,12)) {
  m <- copy(fit_completo); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(FIT_ma3)]
  r1 <- teste_oos_linear(m, "FIT_ma3", "retorno", "FIT suavizado (MA3)", h)
  r2 <- fama_macbeth(m, "FIT_ma3", "retorno", "FIT suavizado (MA3)", h)
  if (!is.null(r1)) resultados_lin[[length(resultados_lin)+1]] <- r1
  if (!is.null(r2)) resultados_fm[[length(resultados_fm)+1]] <- r2
}

RL <- rbindlist(resultados_lin); RF <- rbindlist(resultados_fm)
fwrite(RL, file.path(OUT, "candidatos_08_linear.csv"))
fwrite(RF, file.path(OUT, "candidatos_08_fama_macbeth.csv"))
cat("\nOK\n")
