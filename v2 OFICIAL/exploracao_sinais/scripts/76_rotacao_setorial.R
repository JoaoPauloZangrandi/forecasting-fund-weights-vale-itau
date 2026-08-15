# =============================================================================
# 76_rotacao_setorial.R  (exploracao_sinais)
#
# FRENTE (C): rotacao setorial via fluxo institucional agregado. Usa a
# proxy de setor construida no script 75 (classificacao por nome da
# empresa, 72.3% dos 513 tickers, 21 setores -- ver limitacoes no cabecalho
# do script 75).
#
# Mecanismo testado: setores para os quais o AUM agregado dos fundos
# (share do universo) esta' CRESCENDO recentemente tem retorno SETORIAL
# futuro maior? ("momentum de rotacao setorial via fluxo institucional",
# analogo setorial ao FIT/demanda agregada revelada ja testados a nivel de
# ACAO na exploracao principal -- aqui agregado por setor, testando se o
# mecanismo aparece numa unidade de analise mais grosseira).
#
# LIMITACAO DE PODER ESTATISTICO REGISTRADA COM TRANSPARENCIA: cross-secao
# de SETORES (nao de acoes) tem só ~15-19 unidades por mes (vs. ~200-400
# acoes nos testes normais desta exploracao) -- poder estatistico bem mais
# baixo. Por isso, alem do corte em quintil/tercil, reporta-se tambem a
# regressao linear (usa toda a variacao, nao so os extremos) e o N MINIMO
# de setores por perna em TODO mes (licao metodologica obrigatoria).
#
# METODOLOGIA: Fama-MacBeth de verdade (1 coef/spread por mes); tercil
# RE-CORTADO A CADA MES (quintil ficaria com <3 setores por perna, tercil e'
# o corte maximo defensavel com N tao pequeno); treino ym<202001, teste
# ym>=202001.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/550

addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
setores <- fread(file.path(OUT, "setor_proxy_tickers.csv"))
setores <- setores[setor != "Outros/Nao classificado"]
cat("Tickers classificados em setor:", nrow(setores), "em", uniqueN(setores$setor), "setores\n")

# ---- retorno setorial (baseado em PRECO, equal-weight entre tickers do setor,
#      independente de holdings -- separa claramente preditor de resultado) ----
precos_s <- merge(precos, setores[, .(ticker, setor)], by = "ticker")
ret_setor <- precos_s[, .(ret_setor = mean(retorno, na.rm = TRUE), n_tickers_ret = .N), by = .(setor, ym = ymk)]
cat("N setor-mes com retorno calculado:", nrow(ret_setor), "\n")

# ---- fluxo institucional agregado por setor (share do AUM total do
#      universo investido no setor, holdings de fundos) ----
pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp[, ticker := trimws(sub(".*- ", "", ativo))]
pp <- pp[is.finite(aum_prev) & aum_prev > 0 & peso > 0]
pp[, valor_posicao := peso * aum_prev]
pp <- merge(pp, setores[, .(ticker, setor)], by = "ticker")

aum_total_universo <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
aum_total_universo[, ticker := trimws(sub(".*- ", "", ativo))]
aum_total_universo <- aum_total_universo[is.finite(aum_prev) & aum_prev > 0 & peso > 0]
aum_total_universo[, valor_posicao := peso * aum_prev]
aum_total_universo <- aum_total_universo[, .(aum_total_mes = sum(valor_posicao)), by = ym]

flow_setor <- pp[, .(valor_setor = sum(valor_posicao), n_fundos_setor = uniqueN(cod_fundo),
                      n_tickers_setor = uniqueN(ticker)), by = .(setor, ym)]
flow_setor <- merge(flow_setor, aum_total_universo, by = "ym")
flow_setor[, share_setor := valor_setor / aum_total_mes]
setorder(flow_setor, setor, ym)

flow_prev1 <- flow_setor[, .(setor, ym = addm(ym, 1), share_prev1 = share_setor)]
flow_prev3 <- flow_setor[, .(setor, ym = addm(ym, 3), share_prev3 = share_setor)]
flow_setor <- merge(flow_setor, flow_prev1, by = c("setor","ym"))
flow_setor <- merge(flow_setor, flow_prev3, by = c("setor","ym"))
flow_setor[, delta_share_1m := share_setor - share_prev1]
flow_setor[, delta_share_3m := share_setor - share_prev3]

cat("\nCobertura: n_tickers_setor e n_fundos_setor por setor (media ao longo do periodo):\n")
print(flow_setor[, .(n_tickers_medio = round(mean(n_tickers_setor),1), n_fundos_medio = round(mean(n_fundos_setor),0)), by = setor][order(-n_fundos_medio)])

fwrite(flow_setor, file.path(OUT, "rotacao_setorial_fluxo.csv"))

base <- merge(flow_setor, ret_setor, by = c("setor","ym"))
cat("\nSetor-mes com fluxo E retorno:", nrow(base), "| setores distintos:", uniqueN(base$setor), "\n")
cat("N setores por mes (deve ser ~18-19 se cobertura for estavel):\n")
print(summary(base[, .N, by = ym]$N))

# =============================================================================
# TESTE: Fama-MacBeth, tercil RE-CORTADO A CADA MES (poucas unidades por
# mes -- tercil, nao quintil)
# =============================================================================
fama_macbeth_tercil <- function(m, var_x, var_y, nome, h) {
  teste <- copy(m[ym >= CORTE & is.finite(get(var_x)) & is.finite(get(var_y))])
  if (nrow(teste) < 15) return(NULL)
  teste[, tercil := {
    qs <- quantile(get(var_x), 0:3/3, na.rm = TRUE)
    if (length(unique(qs)) < 4) as.integer(NA) else as.integer(cut(get(var_x), qs, include.lowest = TRUE))
  }, by = ym]
  teste <- teste[!is.na(tercil)]
  chk <- teste[tercil %in% c(1,3), .N, by = .(ym, tercil)]
  n_min <- if (nrow(chk) > 0) min(chk$N) else 0
  por_mes <- teste[tercil %in% c(1,3), .(rm = mean(get(var_y)), n = .N), by = .(ym, tercil)]
  sm <- dcast(por_mes, ym ~ tercil, value.var = "rm")
  if (!all(c("1","3") %in% names(sm))) return(NULL)
  setnames(sm, c("1","3"), c("t1","t3")); sm <- sm[is.finite(t1) & is.finite(t3)]
  sm[, spread := t3 - t1]; nmes <- nrow(sm)
  if (nmes < 6) return(NULL)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(nmes)); p_fm <- 2*pt(-abs(t_fm), df = nmes-1)
  sh <- media/dp*sqrt(12)
  cat(sprintf("%-42s h=%d %2d meses n_min=%2d setor/perna spread(T3-T1)=%+7.3fpp/mes (%+6.1f%%/ano) Sharpe=%5.2f t=%6.2f p=%.5f | Bonf:%s\n",
              nome, h, nmes, n_min, 100*media, 100*((1+media)^12-1), sh, t_fm, p_fm,
              ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
  data.table(sinal = nome, horizonte = h, n_meses = nmes, n_min_setores_perna = n_min,
             spread_pp = 100*media, sharpe = sh, t_fm = t_fm, p_fm = p_fm, sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
}

fama_macbeth_linear_setor <- function(m, var_x, nome, h) {
  teste <- copy(m[ym >= CORTE])
  teste[, xz := scale(get(var_x)), by = ym]
  meses <- sort(unique(teste$ym))
  coefs <- list()
  for (mm in meses) {
    dm <- teste[ym == mm & is.finite(ret_setor_fwd) & is.finite(xz)]
    if (nrow(dm) < 8) next
    fit <- tryCatch(lm(ret_setor_fwd ~ xz, data = dm), error = function(e) NULL)
    if (is.null(fit)) next
    coefs[[as.character(mm)]] <- data.table(ym = mm, coef = coef(fit)["xz"], n_setores = nrow(dm))
  }
  CM <- rbindlist(coefs, fill = TRUE)
  if (nrow(CM) < 6) return(NULL)
  x <- CM$coef; n <- length(x); media <- mean(x); dp <- sd(x)
  t_fm <- media/(dp/sqrt(n)); p_fm <- 2*pt(-abs(t_fm), df = n-1)
  cat(sprintf("  [FM linear setor] %-32s h=%d coef_medio=%+9.5f t=%6.2f p=%.5f (%d meses, N setor/mes med=%.0f) | Bonf:%s\n",
              nome, h, media, t_fm, p_fm, n, median(CM$n_setores), ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
  data.table(sinal = nome, horizonte = h, n_meses = n, coef_medio = media, t_fm = t_fm, p_fm = p_fm,
             sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
}

resultados_terc <- list()
resultados_lin <- list()

cat("\n===== TESTE 1: tercil, delta_share_1m (fluxo institucional 1m) -> retorno setorial futuro =====\n")
for (h in c(1,3,6)) {
  m <- copy(base); m[, ym_ret := addm(ym, h)]
  m <- merge(m, ret_setor[, .(setor, ym, ret_setor_fwd = ret_setor)], by.x = c("setor","ym_ret"), by.y = c("setor","ym"), all.x = TRUE)
  r <- fama_macbeth_tercil(m, "delta_share_1m", "ret_setor_fwd", "Fluxo institucional setorial 1m", h)
  if (!is.null(r)) resultados_terc[[length(resultados_terc)+1]] <- r
  r2 <- fama_macbeth_linear_setor(m, "delta_share_1m", "Fluxo institucional setorial 1m (linear)", h)
  if (!is.null(r2)) resultados_lin[[length(resultados_lin)+1]] <- r2
}

cat("\n===== TESTE 1b: tercil, delta_share_3m (fluxo institucional 3m) -> retorno setorial futuro =====\n")
for (h in c(1,3,6)) {
  m <- copy(base); m[, ym_ret := addm(ym, h)]
  m <- merge(m, ret_setor[, .(setor, ym, ret_setor_fwd = ret_setor)], by.x = c("setor","ym_ret"), by.y = c("setor","ym"), all.x = TRUE)
  r <- fama_macbeth_tercil(m, "delta_share_3m", "ret_setor_fwd", "Fluxo institucional setorial 3m", h)
  if (!is.null(r)) resultados_terc[[length(resultados_terc)+1]] <- r
  r2 <- fama_macbeth_linear_setor(m, "delta_share_3m", "Fluxo institucional setorial 3m (linear)", h)
  if (!is.null(r2)) resultados_lin[[length(resultados_lin)+1]] <- r2
}

cat("\n===== TESTE 2: tercil, NIVEL de share_setor (setor ja' com muito capital -> retorno futuro) =====\n")
for (h in c(1,3,6)) {
  m <- copy(base); m[, ym_ret := addm(ym, h)]
  m <- merge(m, ret_setor[, .(setor, ym, ret_setor_fwd = ret_setor)], by.x = c("setor","ym_ret"), by.y = c("setor","ym"), all.x = TRUE)
  r <- fama_macbeth_tercil(m, "share_setor", "ret_setor_fwd", "Nivel de share setorial (capital ja' alocado)", h)
  if (!is.null(r)) resultados_terc[[length(resultados_terc)+1]] <- r
}

RT <- rbindlist(resultados_terc, fill = TRUE)
RL <- rbindlist(resultados_lin, fill = TRUE)
fwrite(RT, file.path(OUT, "candidatos_76_setorial_tercil.csv"))
fwrite(RL, file.path(OUT, "candidatos_76_setorial_linear.csv"))

cat("\n\n===== RESUMO TERCIL (ordenado por p) =====\n")
print(RT[order(p_fm)])
cat("\n\n===== RESUMO LINEAR (ordenado por p) =====\n")
print(RL[order(p_fm)])
cat("\nOK\n")
