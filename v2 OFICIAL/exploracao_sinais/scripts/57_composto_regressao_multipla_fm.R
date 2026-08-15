# =============================================================================
# 57_composto_regressao_multipla_fm.R  (exploracao_sinais, agente "composto")
#
# ANGULO (b): Fama-MacBeth MULTIPLA -- retorno(t+h) ~ sinal1_z + sinal2_z +
# ... , coeficientes padronizados (z-score DENTRO do mes, cross-secional),
# 1 regressao por mes, testa a media da serie de coeficientes com o erro-
# padrao da propria serie (Fama-MacBeth de verdade). Pergunta: quais sinais
# sobrevivem JUNTOS (t com p<0.05, considerando ~500 especificacoes ja
# testadas -> Bonferroni), e o R2 medio do modelo composto e' maior que
# de qualquer sinal isolado?
#
# Tambem constroi um score composto com PESOS = coeficientes de uma unica
# regressao no TREINO (ym<2020, nunca olha o teste antes de fixar peso),
# aplicado com quintil re-cortado a cada mes no teste (mesmo padrao do
# script 42, mas agora com o conjunto AMPLO de 8 sinais em vez de so' 3).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/500
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
painel <- fread(file.path(OUT, "composto_painel_sinais.csv"))

# sinais ja alinhados na direcao "teoria diz que e' positivo" (mesma
# convencao do script 56 para os 6 sinais nao-ambiguos; dem_pct_w e EFIT
# entram SEM alinhar -- a propria regressao multipla estima o sinal do
# coeficiente, e' exatamente o que a regressao multipla serve para revelar,
# sem exigir decisao a priori)
painel[, s_peer   := peer_ret]
painel[, s_hhi     := -hhi_posse]
painel[, s_52wh    := prox_52w_high]
painel[, s_rev     := reversao]
painel[, s_idiovol := idio_vol_12m]
painel[, s_breadth := delta_breadth_w]
painel[, s_dem      := dem_pct_w]
painel[, s_efit     := EFIT]

vars8 <- c("s_peer","s_hhi","s_52wh","s_rev","s_idiovol","s_breadth","s_dem","s_efit")

fm_multipla <- function(mdt, vars, h, tag) {
  meses <- sort(unique(mdt$ym))
  coefs <- list(); r2s <- list()
  for (mm in meses) {
    dm <- mdt[ym == mm]
    dm <- dm[complete.cases(dm[, ..vars]) & is.finite(retorno)]
    if (nrow(dm) < 30) next
    for (v in vars) dm[, (paste0(v,"_z")) := as.numeric(scale(get(v)))]
    zcols <- paste0(vars, "_z")
    form <- as.formula(paste("retorno ~", paste(zcols, collapse = " + ")))
    fit <- tryCatch(lm(form, data = dm), error = function(e) NULL)
    if (is.null(fit)) next
    cf <- coef(fit); names(cf) <- gsub("_z$", "", names(cf))
    coefs[[as.character(mm)]] <- as.data.table(as.list(cf))
    coefs[[as.character(mm)]][, ym := mm]
    r2s[[as.character(mm)]] <- data.table(ym = mm, r2 = summary(fit)$r.squared, n = nrow(dm))
  }
  CM <- rbindlist(coefs, fill = TRUE); R2 <- rbindlist(r2s)
  n_meses <- nrow(CM)
  cat(sprintf("\n--- %s h=%d: %d meses com regressao valida (n_min=%d, n_med=%.0f) ---\n",
              tag, h, n_meses, min(R2$n), median(R2$n)))
  cat(sprintf("R2 medio (in-sample cross-secional, mes a mes): %.3f%% (mediana %.3f%%)\n",
              100*mean(R2$r2), 100*median(R2$r2)))
  out <- list()
  for (v in vars) {
    x <- CM[[v]]; x <- x[is.finite(x)]; n <- length(x)
    if (n < 6) next
    media <- mean(x); dp <- sd(x); t_fm <- media/(dp/sqrt(n)); p_fm <- 2*pt(-abs(t_fm), df = n-1)
    cat(sprintf("  %-10s coef_medio=%+9.5f  t=%6.2f  p=%.5f %s\n", v, media, t_fm, p_fm,
                ifelse(p_fm < LIMIAR_BONFERRONI, "** Bonf **", "")))
    out[[v]] <- data.table(modelo = tag, horizonte = h, variavel = v, n_meses = n,
                            coef_medio = media, t_fm = t_fm, p_fm = p_fm,
                            r2_medio_pct = 100*mean(R2$r2), sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
  }
  list(coef_tab = rbindlist(out, fill = TRUE), r2_medio = 100*mean(R2$r2), n_meses = n_meses)
}

resultados_coef <- list(); resultados_score <- list(); resumo_r2 <- list()

fama_macbeth_recut <- function(m, var_x, var_y, nome, h) {
  teste <- copy(m[is.finite(get(var_x)) & is.finite(get(var_y))])
  if (nrow(teste) < 30) return(NULL)
  teste[, quintil := {
    qs <- quantile(get(var_x), 0:5/5, na.rm = TRUE)
    if (length(unique(qs)) < 6) as.integer(NA) else as.integer(cut(get(var_x), qs, include.lowest = TRUE))
  }, by = ym]
  teste <- teste[!is.na(quintil)]
  chk <- teste[quintil %in% c(1,5), .N, by = .(ym, quintil)]
  n_min <- if (nrow(chk) > 0) min(chk$N) else 0
  por_mes <- teste[quintil %in% c(1,5), .(rm = mean(get(var_y))), by = .(ym, quintil)]
  sm <- dcast(por_mes, ym ~ quintil, value.var = "rm")
  if (!all(c("1","5") %in% names(sm))) return(NULL)
  setnames(sm, c("1","5"), c("q1","q5")); sm <- sm[is.finite(q1) & is.finite(q5)]
  sm[, spread := q5 - q1]; nmes <- nrow(sm)
  if (nmes < 6) return(NULL)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(nmes)); p_fm <- 2*pt(-abs(t_fm), df = nmes-1)
  sh <- media/dp*sqrt(12)
  cat(sprintf("%-32s h=%d %2d meses n_min=%3d spread=%+7.2fpp/mes Sharpe=%5.2f t=%6.2f p=%.5f | Bonf:%s\n",
              nome, h, nmes, n_min, 100*media, sh, t_fm, p_fm, ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
  data.table(sinal = nome, horizonte = h, n_meses = nmes, n_min_grupo_mes = n_min,
             spread_pp = 100*media, sharpe = sh, t_fm = t_fm, p_fm = p_fm, sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
}

for (h in c(1,3,6)) {
  cat(sprintf("\n\n========================= HORIZONTE h=%d =========================\n", h))
  m <- copy(painel); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno)]
  mtreino <- m[ym < CORTE]; mteste <- m[ym >= CORTE]

  # --- (b1) FM multipla so' com os 3 sinais "top" (mais completos em cobertura, comparavel ao script 42) ---
  res3 <- fm_multipla(mteste, c("s_peer","s_hhi","s_52wh"), h, "FM_multipla|top3(peer+hhi+52wh)")
  resultados_coef[[length(resultados_coef)+1]] <- res3$coef_tab

  # --- (b2) FM multipla com os 8 sinais (amostra menor, exige todos os 8 no mes) ---
  res8 <- fm_multipla(mteste, vars8, h, "FM_multipla|all8")
  resultados_coef[[length(resultados_coef)+1]] <- res8$coef_tab

  # --- (b3) R2 de cada sinal ISOLADO (regressao univariada mes a mes), pra comparar com o composto ---
  cat("\nR2 medio de cada sinal SOZINHO (mesma amostra do all8, univariada):\n")
  meses <- sort(unique(mteste$ym))
  for (v in vars8) {
    r2s <- c()
    for (mm in meses) {
      dm <- mteste[ym == mm]; dm <- dm[complete.cases(dm[, ..vars8]) & is.finite(retorno)]
      if (nrow(dm) < 30) next
      fit <- tryCatch(lm(as.formula(paste("retorno ~", v)), data = dm), error = function(e) NULL)
      if (!is.null(fit)) r2s <- c(r2s, summary(fit)$r.squared)
    }
    cat(sprintf("  %-10s R2 medio = %.3f%%\n", v, 100*mean(r2s)))
    resumo_r2[[length(resumo_r2)+1]] <- data.table(horizonte = h, variavel = v, tipo = "univariada", r2_medio_pct = 100*mean(r2s))
  }
  resumo_r2[[length(resumo_r2)+1]] <- data.table(horizonte = h, variavel = "TODOS_8_JUNTOS", tipo = "multipla", r2_medio_pct = res8$r2_medio)
  resumo_r2[[length(resumo_r2)+1]] <- data.table(horizonte = h, variavel = "TOP3_JUNTOS", tipo = "multipla", r2_medio_pct = res3$r2_medio)

  # --- score composto com pesos = coeficientes de regressao NO TREINO (all8) ---
  mtr <- copy(mtreino[complete.cases(mtreino[, ..vars8]) & is.finite(retorno)])
  for (v in vars8) mtr[, (paste0(v,"_z")) := as.numeric(scale(get(v)))]
  zcols <- paste0(vars8, "_z")
  fit_tr <- lm(as.formula(paste("retorno ~", paste(zcols, collapse=" + "))), data = mtr)
  w <- coef(fit_tr)[zcols]; names(w) <- vars8
  cat(sprintf("\nPesos do TREINO (all8): %s\n", paste(sprintf("%s=%.4f", names(w), w), collapse=", ")))

  mte <- copy(mteste[complete.cases(mteste[, ..vars8]) & is.finite(retorno)])
  for (v in vars8) mte[, (paste0(v,"_z")) := as.numeric(scale(get(v))), by = ym]
  mte[, score_pesos_treino := as.matrix(mte[, paste0(vars8,"_z"), with = FALSE]) %*% w]
  r <- fama_macbeth_recut(mte, "score_pesos_treino", "retorno", "COMPOSTO(pesos regressao treino,all8)", h)
  if (!is.null(r)) resultados_score[[length(resultados_score)+1]] <- r

  # versao top3 com pesos do treino (comparavel ao script 42, mas hhi no lugar de reversao)
  mtr3 <- copy(mtreino[complete.cases(mtreino[, c("s_peer","s_hhi","s_52wh")]) & is.finite(retorno)])
  for (v in c("s_peer","s_hhi","s_52wh")) mtr3[, (paste0(v,"_z")) := as.numeric(scale(get(v)))]
  fit_tr3 <- lm(retorno ~ s_peer_z + s_hhi_z + s_52wh_z, data = mtr3)
  w3 <- coef(fit_tr3)[c("s_peer_z","s_hhi_z","s_52wh_z")]
  mte3 <- copy(mteste[complete.cases(mteste[, c("s_peer","s_hhi","s_52wh")]) & is.finite(retorno)])
  for (v in c("s_peer","s_hhi","s_52wh")) mte3[, (paste0(v,"_z")) := as.numeric(scale(get(v))), by = ym]
  mte3[, score_top3_pesos_treino := w3[1]*s_peer_z + w3[2]*s_hhi_z + w3[3]*s_52wh_z]
  r3 <- fama_macbeth_recut(mte3, "score_top3_pesos_treino", "retorno", "COMPOSTO(pesos regressao treino,top3)", h)
  if (!is.null(r3)) resultados_score[[length(resultados_score)+1]] <- r3
}

RC <- rbindlist(resultados_coef, fill = TRUE)
RS <- rbindlist(resultados_score, fill = TRUE)
RR2 <- rbindlist(resumo_r2, fill = TRUE)
fwrite(RC, file.path(OUT, "candidatos_57_fm_multipla_coeficientes.csv"))
fwrite(RS, file.path(OUT, "candidatos_57_fm_multipla_scores.csv"))
fwrite(RR2, file.path(OUT, "candidatos_57_r2_comparacao.csv"))

cat("\n\n===== COEFICIENTES DA REGRESSAO MULTIPLA (ordenado por p) =====\n")
print(RC[order(p_fm)])
cat("\n\n===== SCORES COMPOSTOS COM PESOS DO TREINO (ordenado por p) =====\n")
print(RS[order(p_fm)])
cat("\nOK\n")
