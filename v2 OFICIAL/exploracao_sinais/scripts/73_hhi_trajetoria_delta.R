# =============================================================================
# 73_hhi_trajetoria_delta.R  (exploracao_sinais)
#
# FRENTE (B): ja sabemos que o NIVEL do HHI de posse prediz volatilidade
# futura (h=12, achado mais solido de toda a exploracao principal) mas NAO
# prediz retorno (candidato do script 32c, spread nao significativo, sinal
# ate' oposto ao esperado). Aqui testa-se algo DIFERENTE: nao o nivel, mas a
# TRAJETORIA/velocidade -- a VARIACAO do HHI nos ultimos 3 ou 6 meses.
#
# Duas hipoteses opostas, testadas as duas (nao pre-comprometidas com uma
# direcao so'):
#   (H-otimista) concentracao CRESCENTE pode sinalizar "smart money"
#     entrando de forma coordenada -- varios gestores aumentando posicao ao
#     mesmo tempo por informacao real -> retorno futuro MAIOR.
#   (H-pessimista) concentracao CRESCENTE aumenta risco de fire-sale se
#     reverter (poucos donos, saida coordenada derruba o preco) -> retorno
#     futuro MENOR, especialmente em horizontes mais longos (quando a
#     reversao teria tempo de acontecer).
#
# METODOLOGIA: as 3 licoes inegociaveis (Fama-MacBeth de verdade; quintil
# RE-CORTADO A CADA MES; treino ym<202001, teste ym>=202001).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/550

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp[, ticker := trimws(sub(".*- ", "", ativo))]
pp <- pp[is.finite(aum_prev) & aum_prev > 0]
pp[, valor_posicao := peso * aum_prev]
hhi <- pp[, .(hhi_posse = sum((valor_posicao/sum(valor_posicao))^2), n_fundos = .N), by = .(ticker, ym)]
hhi <- hhi[n_fundos >= 10]
setorder(hhi, ticker, ym)

hhi_prev1 <- hhi[, .(ticker, ym = addm(ym, 1), hhi_prev1 = hhi_posse)]
hhi_prev3 <- hhi[, .(ticker, ym = addm(ym, 3), hhi_prev3 = hhi_posse)]
hhi_prev6 <- hhi[, .(ticker, ym = addm(ym, 6), hhi_prev6 = hhi_posse)]
hhi2 <- merge(hhi, hhi_prev1, by = c("ticker","ym"))
hhi2 <- merge(hhi2, hhi_prev3, by = c("ticker","ym"))
hhi2 <- merge(hhi2, hhi_prev6, by = c("ticker","ym"))
hhi2[, delta_hhi_1m := hhi_posse - hhi_prev1]
hhi2[, delta_hhi_3m := hhi_posse - hhi_prev3]
hhi2[, delta_hhi_6m := hhi_posse - hhi_prev6]
# variacao RELATIVA (em % do nivel de partida) -- controla o fato de que HHI
# baixo pode variar proporcionalmente mais em pontos absolutos
hhi2[, delta_hhi_3m_pct := (hhi_posse - hhi_prev3) / hhi_prev3]
hhi2[, delta_hhi_6m_pct := (hhi_posse - hhi_prev6) / hhi_prev6]

cat("N ticker-mes com delta_hhi_3m/6m calculado:", nrow(hhi2), "\n")
cat("Resumo delta_hhi_3m:\n"); print(summary(hhi2$delta_hhi_3m))
cat("Resumo delta_hhi_6m:\n"); print(summary(hhi2$delta_hhi_6m))
fwrite(hhi2, file.path(OUT, "hhi_trajetoria_delta.csv"))

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
  cat(sprintf("%-38s h=%d %2d meses n_min=%3d spread(Q5-Q1)=%+7.3fpp/mes (%+6.1f%%/ano) Sharpe=%5.2f t=%6.2f p=%.5f | Bonf:%s\n",
              nome, h, nmes, n_min, 100*media, 100*((1+media)^12-1), sh, t_fm, p_fm,
              ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
  data.table(sinal = nome, horizonte = h, n_meses = nmes, n_min_grupo_mes = n_min,
             spread_pp = 100*media, sharpe = sh, t_fm = t_fm, p_fm = p_fm, sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
}

fama_macbeth_linear <- function(m, vars_x, nome, h) {
  teste <- copy(m[ym >= CORTE])
  for (v in vars_x) teste[, (paste0(v,"_z")) := scale(get(v)), by = ym]
  meses <- sort(unique(teste$ym))
  coefs <- list()
  for (mm in meses) {
    dm <- teste[ym == mm & is.finite(retorno)]
    dm <- dm[complete.cases(dm[, paste0(vars_x,"_z"), with = FALSE])]
    if (nrow(dm) < 30) next
    form <- as.formula(paste("retorno ~", paste(paste0(vars_x,"_z"), collapse = " + ")))
    fit <- tryCatch(lm(form, data = dm), error = function(e) NULL)
    if (is.null(fit)) next
    coefs[[as.character(mm)]] <- data.table(ym = mm, t(coef(fit)))
  }
  CM <- rbindlist(coefs, fill = TRUE)
  resultado <- list()
  for (v in paste0(vars_x, "_z")) {
    if (!v %in% names(CM)) next
    x <- CM[[v]]; x <- x[is.finite(x)]
    n <- length(x); if (n < 6) next
    media <- mean(x); dp <- sd(x)
    t_fm <- media/(dp/sqrt(n)); p_fm <- 2*pt(-abs(t_fm), df = n-1)
    cat(sprintf("  [FM linear] %-28s h=%d var=%-18s coef_medio=%+9.5f t=%6.2f p=%.5f (%d meses) | Bonf:%s\n",
                nome, h, v, media, t_fm, p_fm, n, ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
    resultado[[v]] <- data.table(sinal = nome, horizonte = h, variavel = v, n_meses = n,
                                  coef_medio = media, t_fm = t_fm, p_fm = p_fm, sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
  }
  rbindlist(resultado, fill = TRUE)
}

resultados_qs <- list()
resultados_lin <- list()

cat("\n===== TESTE 1: quintil, delta_hhi_3m -> retorno =====\n")
for (h in c(1,3,6)) {
  m <- copy(hhi2); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  r <- fama_macbeth_recut(m, "delta_hhi_3m", "retorno", "Delta HHI 3m (nivel abs.)", h)
  if (!is.null(r)) resultados_qs[[length(resultados_qs)+1]] <- r
}

cat("\n===== TESTE 1b: quintil, delta_hhi_6m -> retorno =====\n")
for (h in c(1,3,6)) {
  m <- copy(hhi2); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  r <- fama_macbeth_recut(m, "delta_hhi_6m", "retorno", "Delta HHI 6m (nivel abs.)", h)
  if (!is.null(r)) resultados_qs[[length(resultados_qs)+1]] <- r
}

cat("\n===== TESTE 1c: quintil, delta_hhi_3m PERCENTUAL (relativo ao nivel de partida) =====\n")
for (h in c(1,3,6)) {
  m <- copy(hhi2); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  r <- fama_macbeth_recut(m, "delta_hhi_3m_pct", "retorno", "Delta HHI 3m (%)", h)
  if (!is.null(r)) resultados_qs[[length(resultados_qs)+1]] <- r
}

cat("\n===== TESTE 2: Fama-MacBeth linear, delta HHI sozinho =====\n")
for (h in c(1,3,6,12)) {
  m <- copy(hhi2); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  r <- fama_macbeth_linear(m, "delta_hhi_3m", "Delta HHI 3m sozinho", h)
  if (!is.null(r)) resultados_lin[[length(resultados_lin)+1]] <- r
  r2 <- fama_macbeth_linear(m, "delta_hhi_6m", "Delta HHI 6m sozinho", h)
  if (!is.null(r2)) resultados_lin[[length(resultados_lin)+1]] <- r2
}

cat("\n===== TESTE 3: controlado por NIVEL de HHI (angulo B e' sobre trajetoria, nao nivel -- sera' que sobrevive junto?) =====\n")
for (h in c(1,3,6)) {
  m <- copy(hhi2); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  r <- fama_macbeth_linear(m, c("delta_hhi_3m","hhi_posse"), "Delta HHI 3m + nivel HHI (controlado)", h)
  if (!is.null(r)) resultados_lin[[length(resultados_lin)+1]] <- r
}

RQ <- rbindlist(resultados_qs, fill = TRUE)
RL <- rbindlist(resultados_lin, fill = TRUE)
fwrite(RQ, file.path(OUT, "candidatos_73_quintil.csv"))
fwrite(RL, file.path(OUT, "candidatos_73_linear.csv"))

cat("\n\n===== RESUMO QUINTIL (ordenado por p) =====\n")
print(RQ[order(p_fm)])
cat("\n\n===== RESUMO LINEAR (ordenado por p) =====\n")
print(RL[order(p_fm)])
cat("\nOK\n")
