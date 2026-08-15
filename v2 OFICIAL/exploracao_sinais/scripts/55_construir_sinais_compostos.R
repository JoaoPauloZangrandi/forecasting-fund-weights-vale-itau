# =============================================================================
# 55_construir_sinais_compostos.R  (exploracao_sinais, agente "composto")
#
# Constroi um PAINEL UNICO com todos os sinais individuais ja testados (e
# documentados no LOG_CANDIDATOS.md) que sao mecanicamente DIFERENTES entre
# si -- ingredientes para testar combinacao/ranking composto (angulo deste
# agente). Nao recalcula nada que ja exista pronto em CSV; reconstroi so o
# que so' existia "inline" dentro de scripts anteriores (breadth, demanda
# agregada, idio-vol, EFIT), usando exatamente a mesma formula ja usada e
# documentada (ver 05_breadth_e_latent_demand.R, 02_herding_...,
# 24_anomalias_classicas.R, 08_efit_fluxo_esperado.R).
#
# Sinais no painel final (ticker, ym):
#   peer_ret       -- CIO Peer Momentum (Ying 2024) -- unico sinal de RETORNO
#                     que sobreviveu isoladamente na exploracao (candidato #15)
#   hhi_posse      -- crowding/concentracao de posse (candidato #13; direto
#                     do fit_ativo_mes.csv ja calculado na Etapa 1/exploracao)
#   prox_52w_high  -- George-Hwang 2004 (candidato #24, quase-sig sozinho)
#   reversao       -- -retorno do mes anterior, Jegadeesh 1990 (candidato #24)
#   idio_vol_12m   -- Ang et al. 2006 (candidato #24)
#   delta_breadth_w-- Chen-Hong-Stein 2002, var. do n. de fundos (cand. #7)
#   EFIT           -- fluxo esperado (Lou 2012 fiel), unico com R2_OOS
#                     positivo consistente alem do CIO (candidato #11)
#   dem_pct_w      -- demanda agregada revelada, var. do valor_total (cand. B)
#
# Sem look-ahead: todo sinal em t so usa informacao ate t (breadth/demanda
# agregada usam t vs t-1, EFIT usa skill defasado, HHI e peer_ret sao
# contemporaneos ao mes t mas OBSERVAVEIS no fechamento do mes t, igual ao
# resto da exploracao -- mesma convencao usada em todos os candidatos
# anteriores, preservada aqui para comparabilidade).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

# ---- precos (52wk-high, reversao, idio-vol) --------------------------------
precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","preco","retorno"))
setorder(precos, ticker, ymk)
precos[, preco_max_12m := frollapply(preco, 12, max, align = "right"), by = ticker]
precos[, prox_52w_high := preco / preco_max_12m]
precos[, ret_mes_anterior := shift(retorno, 1), by = ticker]
precos[, reversao := -ret_mes_anterior]

mercado <- precos[, .(ret_mercado = mean(retorno, na.rm = TRUE)), by = ymk]
pw <- merge(precos, mercado, by = "ymk")
setorder(pw, ticker, ymk)
calc_idio_vol <- function(ret, ret_mkt) {
  n <- length(ret); out <- rep(NA_real_, n)
  if (n < 12) return(out)
  for (i in 12:n) {
    jan <- (i-11):i
    fit <- tryCatch(lm(ret[jan] ~ ret_mkt[jan]), error = function(e) NULL)
    if (!is.null(fit)) out[i] <- sd(residuals(fit))
  }
  out
}
pw[, idio_vol_12m := calc_idio_vol(retorno, ret_mercado), by = ticker]

sinais_preco <- merge(
  precos[, .(ticker, ym = ymk, prox_52w_high, reversao)],
  pw[, .(ticker, ym = ymk, idio_vol_12m)],
  by = c("ticker","ym")
)

# ---- HHI (crowding) -- direto do fit_ativo_mes.csv ja calculado -----------
fit_ativo <- fread(file.path(DATA, "fit_ativo_mes.csv"), select = c("ativo","ym","hhi_posse","n_fundos"))
fit_ativo[, ticker := trimws(sub(".*- ", "", ativo))]
fit_ativo <- fit_ativo[n_fundos >= 10]

# ---- CIO Peer Momentum -- ja pronto -----------------------------------
cio <- fread(file.path(OUT, "cio_peer_return.csv"))[, .(ticker, ym, peer_ret)]

# ---- breadth of ownership (delta % n. de fundos) ---------------------------
pp <- fread(file.path(DATA, "painel_multiativo_final.csv"),
            select = c("cod_fundo","ativo","ym","peso","valor_mil","aum_prev","flow_aum"))
breadth <- pp[peso > 0, .(breadth = uniqueN(cod_fundo)), by = .(ativo, ym)]
br_prev <- breadth[, .(ativo, ym = addm(ym, 1), breadth_prev = breadth)]
br <- merge(breadth, br_prev, by = c("ativo","ym"))
br[, delta_breadth_pct := (breadth - breadth_prev) / breadth_prev]
q <- quantile(br$delta_breadth_pct, c(0.01, 0.99), na.rm = TRUE)
br[, delta_breadth_w := pmin(pmax(delta_breadth_pct, q[1]), q[2])]
br[, ticker := trimws(sub(".*- ", "", ativo))]

# ---- demanda agregada revelada (delta % valor_total) -----------------------
valor_total <- pp[, .(valor_total_mil = sum(valor_mil)), by = .(ativo, ym)]
vt_prev <- valor_total[, .(ativo, ym = addm(ym, 1), valor_total_prev = valor_total_mil)]
dem <- merge(valor_total, vt_prev, by = c("ativo","ym"))
dem <- dem[valor_total_prev > 0]
dem[, dem_pct := (valor_total_mil - valor_total_prev) / valor_total_prev]
qd <- quantile(dem$dem_pct, c(0.01, 0.99), na.rm = TRUE)
dem[, dem_pct_w := pmin(pmax(dem_pct, qd[1]), qd[2])]
dem[, ticker := trimws(sub(".*- ", "", ativo))]

# ---- EFIT (fluxo esperado, mesma construcao do candidato #11) --------------
rf <- fread(file.path(DATA, "retorno_fundo_mensal.csv"))
rf[, cod_fundo := as.character(cod_fundo)]
setorder(rf, cod_fundo, ymk)
rf[, log_ret := log(1 + retorno_fundo)]
rf[, skill_12m := exp(frollsum(log_ret, 12, align = "right")) - 1]
rf_skill <- rf[!is.na(skill_12m), .(cod_fundo, ym = ymk, skill_12m)]

pp[, cod_fundo := as.character(cod_fundo)]
fluxo_fundo <- unique(pp[, .(cod_fundo, ym, flow_aum)])
fluxo_fundo <- fluxo_fundo[is.finite(flow_aum)]
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
efit <- d[, .(EFIT = sum(peso_valor * flow_aum_esperado) / sum(peso_valor), n_fundos_efit = .N), by = .(ativo, ym)]
efit <- efit[n_fundos_efit >= 10]
efit[, ticker := trimws(sub(".*- ", "", ativo))]

cat("Linhas por sinal (antes do merge final):\n")
cat("  precos (52wh/rev/idio):", nrow(sinais_preco), "\n")
cat("  HHI (fit_ativo_mes):   ", nrow(fit_ativo), "\n")
cat("  CIO peer_ret:          ", nrow(cio), "\n")
cat("  breadth delta:         ", nrow(br), "\n")
cat("  demanda agregada:      ", nrow(dem), "\n")
cat("  EFIT:                  ", nrow(efit), "\n")

# ---- merge final: outer join em (ticker, ym), 1 linha por celula com o
#      que estiver disponivel (cada sinal tem sua propria cobertura) --------
painel <- Reduce(function(a, b) merge(a, b, by = c("ticker","ym"), all = TRUE), list(
  sinais_preco,
  fit_ativo[, .(ticker, ym, hhi_posse)],
  cio,
  br[, .(ticker, ym, delta_breadth_w)],
  dem[, .(ticker, ym, dem_pct_w)],
  efit[, .(ticker, ym, EFIT)]
))

cat("\nPainel composto final:", nrow(painel), "linhas (ticker-mes)\n")
cat("Cobertura de cada sinal (n. celulas nao-NA):\n")
for (v in c("prox_52w_high","reversao","idio_vol_12m","hhi_posse","peer_ret","delta_breadth_w","dem_pct_w","EFIT")) {
  cat(sprintf("  %-16s %6d (%.1f%%)\n", v, sum(is.finite(painel[[v]])), 100*mean(is.finite(painel[[v]]))))
}

fwrite(painel, file.path(OUT, "composto_painel_sinais.csv"))
cat("\nOK -- salvo em composto_painel_sinais.csv\n")
