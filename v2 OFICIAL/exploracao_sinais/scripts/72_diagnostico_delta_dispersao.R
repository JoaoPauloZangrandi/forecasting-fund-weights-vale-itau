# =============================================================================
# 72_diagnostico_delta_dispersao.R  (exploracao_sinais)
#
# O script 71 achou um padrao interessante: VARIACAO da dispersao de crencas
# (delta_cv_ponderado, desacordo CRESCENTE entre gestores) prediz retorno
# futuro mais BAIXO -- direcao consistente nas 6 especificacoes (3m/6m x
# h=1/3/6), melhor caso p=0.014 (h=6, delta 3m), mas nao bate Bonferroni
# (0.05/550). Antes de descartar OU de empolgar, aplica os mesmos
# diagnosticos de robustez que ja evitaram 2 quase-falsos-positivos nesta
# exploracao (Comomentum, CIO original): (1) tamanho de amostra saudavel por
# mes -- ja conferido no script 71 (n_min 36-40, nao degenerado); (2) o
# resultado depende so' do periodo do crash da COVID?; (3) sobrevive
# controlando pelo NIVEL de dispersao e pela mudanca de HHI (sera' que
# "desacordo crescente" e' so' um proxy de "concentracao mudando", que e'
# exatamente o angulo B testado em paralelo)?
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/550

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

disp <- fread(file.path(OUT, "dispersao_crencas_dms.csv"))
setorder(disp, ticker, ym)
disp_prev3 <- disp[, .(ticker, ym = addm(ym, 3), cv_prev3 = cv_peso_ponderado)]
disp_prev6 <- disp[, .(ticker, ym = addm(ym, 6), cv_prev6 = cv_peso_ponderado)]
disp2 <- merge(disp, disp_prev3, by = c("ticker","ym"))
disp2 <- merge(disp2, disp_prev6, by = c("ticker","ym"))
disp2[, delta_cv_3m := cv_peso_ponderado - cv_prev3]
disp2[, delta_cv_6m := cv_peso_ponderado - cv_prev6]

fama_macbeth_recut <- function(m, var_x, var_y, nome, h, meses_restringir = NULL) {
  teste <- copy(m[ym >= CORTE & is.finite(get(var_x)) & is.finite(get(var_y))])
  if (!is.null(meses_restringir)) teste <- teste[ym %in% meses_restringir]
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
    cat(sprintf("  [FM linear] %-32s h=%d var=%-16s coef_medio=%+9.5f t=%6.2f p=%.5f (%d meses) | Bonf:%s\n",
                nome, h, v, media, t_fm, p_fm, n, ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
    resultado[[v]] <- data.table(sinal = nome, horizonte = h, variavel = v, n_meses = n,
                                  coef_medio = media, t_fm = t_fm, p_fm = p_fm, sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
  }
  rbindlist(resultado, fill = TRUE)
}

resultados <- list()

# ------------------------------------------------------------------
# (1) Excluir meses do crash/recuperacao COVID (fev-dez/2020) -- sobra so'
#     2021, mesmo teste ja feito no CIO Peer Momentum (candidato #15)
# ------------------------------------------------------------------
meses_2021 <- unique(disp2[ym >= 202101]$ym)
cat("Meses de teste em 2021 (excluindo todo 2020):", length(meses_2021), "\n")
cat("\n===== (1) Delta dispersao 3m/6m -> retorno, RESTRITO A 2021 (sem COVID) =====\n")
for (h in c(1,3,6)) {
  m <- copy(disp2); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  r <- fama_macbeth_recut(m, "delta_cv_3m", "retorno", "Delta dispersao 3m | so 2021 (sem COVID)", h, meses_restringir = meses_2021)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
  r2 <- fama_macbeth_recut(m, "delta_cv_6m", "retorno", "Delta dispersao 6m | so 2021 (sem COVID)", h, meses_restringir = meses_2021)
  if (!is.null(r2)) resultados[[length(resultados)+1]] <- r2
}

# ------------------------------------------------------------------
# (2) Sobrevive controlando por (a) NIVEL de dispersao [ja testado sem
#     efeito no script 70] e (b) delta_HHI [angulo B, testado em paralelo]?
# ------------------------------------------------------------------
pp2 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp2[, ticker := trimws(sub(".*- ", "", ativo))]
pp2 <- pp2[is.finite(aum_prev) & aum_prev > 0]
pp2[, valor_posicao := peso * aum_prev]
hhi <- pp2[, .(hhi_posse = sum((valor_posicao/sum(valor_posicao))^2)), by = .(ticker, ym)]
setorder(hhi, ticker, ym)
hhi_prev3 <- hhi[, .(ticker, ym = addm(ym, 3), hhi_prev3 = hhi_posse)]
hhi2 <- merge(hhi, hhi_prev3, by = c("ticker","ym"))
hhi2[, delta_hhi_3m := hhi_posse - hhi_prev3]

disp3 <- merge(disp2, hhi2[, .(ticker, ym, delta_hhi_3m, hhi_posse)], by = c("ticker","ym"))
cat("\nCorrelacao delta_cv_3m x delta_hhi_3m:", round(cor(disp3$delta_cv_3m, disp3$delta_hhi_3m, use="complete.obs"), 3), "\n")
cat("(Se alta, os dois angulos A e B estao pegando o mesmo fenomeno subjacente.)\n")

cat("\n===== (2) Delta dispersao 3m sobrevive controlando por nivel de dispersao E delta_HHI 3m? =====\n")
for (h in c(1,3,6)) {
  m <- copy(disp3); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  r <- fama_macbeth_linear(m, c("delta_cv_3m","cv_peso_ponderado","delta_hhi_3m"),
                            "Delta disp 3m + nivel disp + delta HHI (controlado)", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

# ------------------------------------------------------------------
# (3) Magnitude e plausibilidade: qual mes contribui mais pro spread? --
#     mesma checagem que revelou o artefato do Comomentum (candidato #14)
# ------------------------------------------------------------------
cat("\n===== (3) Contribuicao mes-a-mes do spread, delta_cv_3m -> retorno h=6 (melhor caso) =====\n")
m <- copy(disp2); m[, ym_ret := addm(ym, 6)]
m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
teste <- m[ym >= CORTE & is.finite(delta_cv_3m) & is.finite(retorno)]
teste[, quintil := {
  qs <- quantile(delta_cv_3m, 0:5/5, na.rm = TRUE)
  if (length(unique(qs)) < 6) as.integer(NA) else as.integer(cut(delta_cv_3m, qs, include.lowest = TRUE))
}, by = ym]
teste <- teste[!is.na(quintil) & quintil %in% c(1,5)]
por_mes <- teste[, .(rm = mean(retorno), n = .N), by = .(ym, quintil)]
sm <- dcast(por_mes, ym ~ quintil, value.var = "rm")
setnames(sm, c("1","5"), c("q1","q5")); sm[, spread := q5 - q1]
sm <- sm[order(-abs(spread))]
print(sm)
cat(sprintf("\nMedia geral: %.3fpp | maior contribuicao isolada: %.3fpp (%.0f%% da media somada)\n",
            100*mean(sm$spread), 100*max(abs(sm$spread)), 100*max(abs(sm$spread))/sum(abs(sm$spread))))

R <- rbindlist(resultados, fill = TRUE)
fwrite(R, file.path(OUT, "candidatos_72_diagnostico_delta_dispersao.csv"))
cat("\n\n===== RESUMO (ordenado por p) =====\n")
print(R[order(p_fm)])
cat("\nOK\n")
