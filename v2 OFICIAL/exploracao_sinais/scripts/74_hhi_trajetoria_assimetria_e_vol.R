# =============================================================================
# 74_hhi_trajetoria_assimetria_e_vol.R  (exploracao_sinais)
#
# Continuacao do angulo B (trajetoria do HHI). O script 73 testou delta_hhi
# como preditor LINEAR continuo e em quintil -- nulo em quase tudo (melhor
# p=0.07). Mas a hipotese do Joao tem DUAS direcoes conflitantes (smart
# money coordenado vs. risco de fire-sale) -- se as duas coexistirem em
# proporcoes parecidas na amostra, uma regressao linear simples pode
# cancelar os dois efeitos e mostrar "nada" mesmo que cada regime
# individualmente tenha sinal. Testado aqui:
#
#   (i) Split em subamostras: SO' ativo-mes com concentracao CRESCENTE
#       (delta_hhi_3m > 0) vs. SO' com concentracao CAINDO (delta_hhi_3m<0)
#       -- dentro de cada subamostra, quintil por MAGNITUDE do delta,
#       spread Q5-Q1 (mais extremo vs. menos extremo dentro do MESMO regime
#       de direcao) -> retorno futuro.
#   (ii) Bonus, conecta com o achado mais solido de toda a exploracao
#       principal (HHI nivel -> volatilidade futura, h=12): delta_hhi
#       adiciona poder preditivo de VOLATILIDADE futura ALEM do nivel de
#       HHI (script 31 da exploracao principal ja fez HHI nivel + vol
#       passada -- aqui e' HHI nivel + trajetoria do HHI)?
#
# METODOLOGIA: as 3 licoes inegociaveis (FM de verdade, quintil recortado
# por mes, treino<202001/teste>=202001).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/550

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

hhi2 <- fread(file.path(OUT, "hhi_trajetoria_delta.csv"))

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
  cat(sprintf("%-46s h=%d %2d meses n_min=%3d spread(Q5-Q1)=%+7.3fpp/mes (%+6.1f%%/ano) Sharpe=%5.2f t=%6.2f p=%.5f | Bonf:%s\n",
              nome, h, nmes, n_min, 100*media, 100*((1+media)^12-1), sh, t_fm, p_fm,
              ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
  data.table(sinal = nome, horizonte = h, n_meses = nmes, n_min_grupo_mes = n_min,
             spread_pp = 100*media, sharpe = sh, t_fm = t_fm, p_fm = p_fm, sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
}

resultados <- list()

# ------------------------------------------------------------------
# (i) Split por direcao: dentro de "concentracao crescente" e dentro de
#     "concentracao caindo", o quao EXTREMA a mudanca foi prediz retorno?
# ------------------------------------------------------------------
cat("\n===== (i) SO ativos com concentracao CRESCENTE (delta_hhi_3m>0): magnitude -> retorno =====\n")
crescente <- hhi2[delta_hhi_3m > 0]
cat("N ticker-mes (crescente):", nrow(crescente), "\n")
for (h in c(1,3,6)) {
  m <- copy(crescente); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  r <- fama_macbeth_recut(m, "delta_hhi_3m", "retorno", "|Delta HHI 3m>0 (crescente), magnitude", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

cat("\n===== (i-b) SO ativos com concentracao CAINDO (delta_hhi_3m<0): magnitude -> retorno =====\n")
caindo <- hhi2[delta_hhi_3m < 0]
cat("N ticker-mes (caindo):", nrow(caindo), "\n")
for (h in c(1,3,6)) {
  m <- copy(caindo); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  r <- fama_macbeth_recut(m, "delta_hhi_3m", "retorno", "|Delta HHI 3m<0 (caindo), magnitude", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

# ------------------------------------------------------------------
# (i-c) Teste direto e mais simples da hipotese assimetrica: dummy
#       "crescente" vs "caindo" (nao magnitude, so' o SINAL da trajetoria)
#       -- media de retorno de quem esta' subindo HHI menos quem esta'
#       caindo, por mes
# ------------------------------------------------------------------
cat("\n===== (i-c) Dummy direcao (crescente vs caindo) -> retorno, media por mes =====\n")
for (h in c(1,3,6)) {
  m <- copy(hhi2); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  teste <- m[ym >= CORTE & is.finite(delta_hhi_3m) & is.finite(retorno) & delta_hhi_3m != 0]
  teste[, direcao := ifelse(delta_hhi_3m > 0, "crescente", "caindo")]
  por_mes <- teste[, .(rm = mean(retorno), n = .N), by = .(ym, direcao)]
  sm <- dcast(por_mes, ym ~ direcao, value.var = "rm")
  sm <- sm[is.finite(crescente) & is.finite(caindo)]
  sm[, spread := crescente - caindo]; nmes <- nrow(sm)
  if (nmes >= 6) {
    media <- mean(sm$spread); dp <- sd(sm$spread)
    t_fm <- media/(dp/sqrt(nmes)); p_fm <- 2*pt(-abs(t_fm), df = nmes-1)
    sh <- media/dp*sqrt(12)
    cat(sprintf("Dummy direcao (crescente-caindo)           h=%d %2d meses spread=%+7.3fpp/mes Sharpe=%5.2f t=%6.2f p=%.5f | Bonf:%s\n",
                h, nmes, 100*media, sh, t_fm, p_fm, ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
    resultados[[length(resultados)+1]] <- data.table(sinal = "Dummy direcao HHI (crescente-caindo)", horizonte = h,
                n_meses = nmes, n_min_grupo_mes = NA_integer_, spread_pp = 100*media, sharpe = sh, t_fm = t_fm,
                p_fm = p_fm, sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
  }
}

R <- rbindlist(resultados, fill = TRUE)
fwrite(R, file.path(OUT, "candidatos_74_assimetria_hhi.csv"))
cat("\n\n===== RESUMO (i) ASSIMETRIA (ordenado por p) =====\n")
print(R[order(p_fm)])

# ------------------------------------------------------------------
# (ii) BONUS: delta_hhi adiciona poder preditivo de VOLATILIDADE futura
#      alem do NIVEL de HHI? (extensao do achado mais solido da exploracao
#      principal, scripts 28-31)
# ------------------------------------------------------------------
cat("\n\n===== (ii) BONUS: Delta HHI + nivel HHI -> volatilidade futura (Fama-MacBeth) =====\n")
setorder(precos, ticker, ymk)
calc_vol_futura <- function(dt_precos, h) {
  pw <- copy(dt_precos)
  cols <- list()
  for (k in 1:h) {
    aux <- pw[, .(ticker, ym_base = addm(ymk, -k), ret_k = retorno)]
    setnames(aux, "ret_k", paste0("r",k))
    cols[[k]] <- aux
  }
  base <- Reduce(function(a,b) merge(a,b,by=c("ticker","ym_base"), all=TRUE), cols)
  ret_cols <- paste0("r",1:h)
  base[, vol_futura := apply(.SD, 1, sd, na.rm=TRUE), .SDcols = ret_cols]
  base[, n_obs_vol := apply(.SD, 1, function(x) sum(is.finite(x))), .SDcols = ret_cols]
  base <- base[n_obs_vol >= max(2,round(h*0.7))]
  base[, .(ticker, ym = ym_base, vol_futura)]
}

fama_macbeth_regressao <- function(m, vars_x, nome, h) {
  teste <- m[ym >= CORTE]
  meses <- sort(unique(teste$ym))
  coefs <- list()
  for (mm in meses) {
    dm <- teste[ym == mm]
    if (nrow(dm) < 30) next
    form <- as.formula(paste("vol_futura ~", paste(vars_x, collapse=" + ")))
    fit <- tryCatch(lm(form, data = dm), error=function(e) NULL)
    if (is.null(fit)) next
    coefs[[as.character(mm)]] <- data.table(ym=mm, t(coef(fit)))
  }
  CM <- rbindlist(coefs, fill=TRUE)
  resultado <- list()
  for (v in vars_x) {
    if (!v %in% names(CM)) next
    x <- CM[[v]]; x <- x[is.finite(x)]
    n <- length(x); media <- mean(x); dp <- sd(x)
    t_fm <- media/(dp/sqrt(n)); p_fm <- 2*pt(-abs(t_fm), df=n-1)
    cat(sprintf("  %-16s h=%d coef_medio=%+10.6f | t(FM)=%7.2f | p(FM)=%.5f | (%d meses) | Bonf:%s\n",
                v, h, media, t_fm, p_fm, n, ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
    resultado[[v]] <- data.table(sinal=nome, variavel=v, horizonte=h, n_meses=n, coef_medio=media, t_fm=t_fm,
                                  p_fm=p_fm, sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
  }
  rbindlist(resultado, fill = TRUE)
}

resultados_vol <- list()
for (h in c(3,6,12)) {
  vol_fut <- calc_vol_futura(precos, h)
  m <- merge(hhi2, vol_fut, by = c("ticker","ym"))
  m <- m[is.finite(hhi_posse) & is.finite(delta_hhi_3m) & is.finite(vol_futura)]
  cat(sprintf("\n-- h=%d (n=%d) --\n", h, nrow(m)))
  r <- fama_macbeth_regressao(m, c("hhi_posse","delta_hhi_3m"), "HHI nivel + delta HHI 3m -> vol futura", h)
  resultados_vol[[length(resultados_vol)+1]] <- r
}
RV <- rbindlist(resultados_vol, fill = TRUE)
fwrite(RV, file.path(OUT, "candidatos_74_delta_hhi_volatilidade.csv"))
cat("\n\n===== RESUMO (ii) DELTA HHI -> VOLATILIDADE (ordenado por p) =====\n")
print(RV[order(p_fm)])
cat("\nOK\n")
