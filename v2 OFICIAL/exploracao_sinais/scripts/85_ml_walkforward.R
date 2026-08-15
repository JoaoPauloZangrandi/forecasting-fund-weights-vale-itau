# =============================================================================
# 85_ml_walkforward.R  (exploracao_sinais, agente ML/pairs)
#
# FRENTE (B): ML com validacao walk-forward correta.
#
# O candidato #21 (script 21_ensemble_ml.R) ja testou XGBoost com 7 sinais
# combinados, mas com um UNICO corte treino/teste (ym<2020 treino, tudo
# ym>=2020 previsto de uma vez so' com o modelo treinado uma unica vez) --
# resultado: R2_OOS NEGATIVO em todos os horizontes (overfitting, amostra
# de treino pequena pra ML). Aqui a diferenca de desenho e':
#   (1) mais sinais como features (reaproveitando composto_painel_sinais.csv,
#       ja construido por outro agente desta exploracao, + mom_12m,
#       log_tamanho, e as metricas de rede -- deg_avg/eigen_cent, tambem
#       ja calculadas por outro agente -- 12 features no total);
#   (2) arvore RASA (max_depth<=2), poucos rounds, early stopping;
#   (3) walk-forward DE VERDADE no periodo de teste: em vez de treinar uma
#       vez e prever os 24 meses de teste de uma tacada, refaz o treino a
#       cada 6 meses (expanding window), usando SO' dado estritamente
#       anterior ao mes previsto -- e a cada refit usa um holdout cronologico
#       (ultimos meses do treino disponivel naquele momento) pra early
#       stopping, entao a validacao walk-forward acontece tanto DENTRO de
#       cada refit quanto ENTRE os refits.
#
# R2_OOS aqui e' calculado contra a MEDIA HISTORICA EXPANSIVA de y (media
# de todo y conhecido ate o momento do refit, nao a media do teste inteiro
# nem so' do treino original) -- benchmark mais rigoroso, um "ingenuo"
# real que um investidor teria disponivel em tempo real.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(xgboost) })
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

set.seed(42)

# ---- painel composto ja pronto (outro agente desta exploracao) -------------
painel <- fread(file.path(OUT, "composto_painel_sinais.csv"))

# ---- precos: retorno + momentum proprio 12m ---------------------------------
precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
setorder(precos, ticker, ymk)
precos[, log_ret := log(1 + retorno)]
precos[, mom_12m := exp(frollsum(log_ret, 12, align = "right")) - 1, by = ticker]
mom <- precos[!is.na(mom_12m), .(ticker, ym = ymk, mom_12m)]

# ---- tamanho: log do valor total detido pelos fundos ------------------------
pp0 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("ativo","ym","valor_mil"))
pp0[, ticker := trimws(sub(".*- ", "", ativo))]
tam <- pp0[, .(valor_total_mil = sum(valor_mil)), by = .(ticker, ym)]
tam[, log_tamanho := log(pmax(valor_total_mil, 1))]

# ---- centralidade de rede (ja calculada por outro agente desta exploracao) -
central <- fread(file.path(OUT, "centralidade_panel.csv"), select = c("ticker","ym","deg_avg","eigen_cent"))

# ---- junta tudo --------------------------------------------------------------
feat <- Reduce(function(a,b) merge(a, b, by = c("ticker","ym"), all = FALSE), list(
  painel, mom, tam[, .(ticker, ym, log_tamanho)], central
))
cat("Ativo-mes com TODAS as 12 features disponiveis:", nrow(feat), "\n")

vars_x <- c("peer_ret","hhi_posse","prox_52w_high","reversao","idio_vol_12m",
            "delta_breadth_w","dem_pct_w","EFIT","mom_12m","log_tamanho",
            "deg_avg","eigen_cent")
cat("Features usadas:", paste(vars_x, collapse=", "), "\n")

# =============================================================================
# WALK-FORWARD: refit a cada 6 meses, expanding window, sempre so' com dado
# estritamente anterior ao mes previsto
# =============================================================================
rodar_walkforward <- function(h, janela_refit = 6) {
  m <- copy(feat); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos[, .(ticker, ymk, retorno)], by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno)]
  for (v in vars_x) m <- m[is.finite(get(v))]
  setorder(m, ym)

  meses_teste <- sort(unique(m[ym >= CORTE]$ym))
  if (length(meses_teste) < 6) { cat(sprintf("h=%d: meses de teste insuficientes\n", h)); return(NULL) }

  # pontos de refit: CORTE, CORTE+6m, CORTE+12m, ... (expanding window)
  pontos_refit <- meses_teste[seq(1, length(meses_teste), by = janela_refit)]
  cat(sprintf("\nh=%d: %d meses de teste, %d refits (a cada %d meses)\n", h, length(meses_teste), length(pontos_refit), janela_refit))

  preds_todos <- list()
  imp_todos <- list()

  for (rp_idx in seq_along(pontos_refit)) {
    rp <- pontos_refit[rp_idx]
    # janela deste refit: do ponto de refit ate o proximo (exclusive) ou fim
    prox_rp <- if (rp_idx < length(pontos_refit)) pontos_refit[rp_idx+1] else max(meses_teste) + 100
    meses_desta_janela <- meses_teste[meses_teste >= rp & meses_teste < prox_rp]

    treino_disp <- m[ym < rp]  # SO' dado estritamente anterior -- sem vazamento
    alvo <- m[ym %in% meses_desta_janela]
    if (nrow(treino_disp) < 150 || nrow(alvo) < 10) next

    meses_tr <- sort(unique(treino_disp$ym))
    # holdout cronologico (ultimos ~15% dos meses de treino disponiveis) para
    # early stopping -- validacao walk-forward DENTRO deste refit
    corte_val <- meses_tr[max(1, floor(0.85 * length(meses_tr)))]
    idx_tr <- treino_disp$ym <= corte_val
    idx_val <- treino_disp$ym > corte_val
    if (sum(idx_val) < 20) { idx_val <- treino_disp$ym > meses_tr[max(1, length(meses_tr)-3)]; idx_tr <- !idx_val }

    X_tr <- as.matrix(treino_disp[idx_tr, ..vars_x]); y_tr <- treino_disp[idx_tr]$retorno
    X_val <- as.matrix(treino_disp[idx_val, ..vars_x]); y_val <- treino_disp[idx_val]$retorno
    X_alvo <- as.matrix(alvo[, ..vars_x])

    dtr <- xgb.DMatrix(data = X_tr, label = y_tr)
    dval <- xgb.DMatrix(data = X_val, label = y_val)

    modelo <- xgb.train(
      params = list(objective = "reg:squarederror", max_depth = 2, eta = 0.03,
                     subsample = 0.7, colsample_bytree = 0.7, min_child_weight = 30,
                     lambda = 5),
      data = dtr, nrounds = 300,
      watchlist = list(val = dval), early_stopping_rounds = 25, verbose = 0
    )

    pred_alvo <- predict(modelo, X_alvo)
    media_hist_expansiva <- mean(treino_disp$retorno)  # "ingenuo" disponivel no momento do refit

    preds_todos[[length(preds_todos)+1]] <- data.table(
      ticker = alvo$ticker, ym = alvo$ym, retorno = alvo$retorno, pred = pred_alvo,
      media_ingenua = media_hist_expansiva, refit_em = rp, melhor_iter = modelo$best_iteration
    )
    imp <- tryCatch(xgb.importance(model = modelo), error = function(e) NULL)
    if (!is.null(imp)) imp_todos[[length(imp_todos)+1]] <- imp[, refit_em := rp]

    cat(sprintf("  refit em ym=%d: n_treino=%d (val=%d) melhor_iter=%d | previu %d meses (%s a %s)\n",
                rp, sum(idx_tr), sum(idx_val), modelo$best_iteration, length(meses_desta_janela),
                min(meses_desta_janela), max(meses_desta_janela)))
  }

  if (length(preds_todos) == 0) return(NULL)
  PRED <- rbindlist(preds_todos)

  # R2_OOS contra a media historica expansiva conhecida em cada refit (mais
  # rigoroso que contra a media do treino original fixo)
  sse_m <- sum((PRED$retorno - PRED$pred)^2)
  sse_n <- sum((PRED$retorno - PRED$media_ingenua)^2)
  r2_oos <- 1 - sse_m / sse_n
  cat(sprintf("h=%d: R2_OOS walk-forward (%d obs, %d refits) = %.4f%%\n",
              h, nrow(PRED), length(preds_todos), 100*r2_oos))

  # importancia agregada (media do gain entre os refits, feature ausente num
  # refit conta como gain=0 naquele refit)
  IMP <- rbindlist(imp_todos, fill = TRUE)
  imp_agg <- IMP[, .(gain_medio = mean(Gain, na.rm=TRUE), n_refits_apareceu = .N), by = Feature]
  imp_agg <- merge(data.table(Feature = vars_x), imp_agg, by = "Feature", all.x = TRUE)
  imp_agg[is.na(gain_medio), gain_medio := 0]
  imp_agg[is.na(n_refits_apareceu), n_refits_apareceu := 0]
  setorder(imp_agg, -gain_medio)
  cat("  importancia media das features (entre refits):\n")
  print(imp_agg)

  # Fama-MacBeth do long-short baseado nas previsoes -- quintil RECORTADO A
  # CADA MES (nao break fixo), evita a armadilha ja documentada no log
  PRED[, quintil := as.integer(cut(pred, quantile(pred, 0:5/5, na.rm=TRUE), include.lowest = TRUE)), by = ym]
  n_por_mes <- PRED[quintil %in% c(1,5), .N, by = .(ym, quintil)]
  por_mes <- PRED[quintil %in% c(1,5), .(ret = mean(retorno)), by = .(ym, quintil)]
  sm <- dcast(por_mes, ym ~ quintil, value.var = "ret")
  fm_res <- NULL
  if (all(c("1","5") %in% names(sm))) {
    setnames(sm, c("1","5"), c("q1","q5"))
    sm <- sm[is.finite(q1) & is.finite(q5)]
    sm[, spread := q5 - q1]
    n_meses_fm <- nrow(sm)
    if (n_meses_fm >= 6) {
      media <- mean(sm$spread); dp <- sd(sm$spread)
      t_fm <- media / (dp/sqrt(n_meses_fm)); p_fm <- 2*pt(-abs(t_fm), df = n_meses_fm - 1)
      cat(sprintf("  Long-short (FM, quintil recortado por mes): %d meses, spread=%.3f%%/mes, t=%.2f, p=%.4f | N_quintil-mes(min/mediana)=%d/%.0f\n",
                  n_meses_fm, 100*media, t_fm, p_fm, min(n_por_mes$N), median(n_por_mes$N)))
      fm_res <- data.table(horizonte = h, n_meses_fm = n_meses_fm, spread_pct = 100*media, t_fm = t_fm, p_fm = p_fm,
                            n_quintil_mes_min = min(n_por_mes$N), n_quintil_mes_mediana = median(n_por_mes$N))
    }
  }

  list(
    resumo = data.table(horizonte = h, n_obs = nrow(PRED), n_refits = length(preds_todos), r2_oos_pct = 100*r2_oos),
    fm = fm_res,
    importancia = imp_agg[, horizonte := h],
    preds = PRED
  )
}

resultados_resumo <- list(); resultados_fm <- list(); resultados_imp <- list()
for (h in c(1, 3, 6)) {
  r <- rodar_walkforward(h, janela_refit = 6)
  if (!is.null(r)) {
    resultados_resumo[[length(resultados_resumo)+1]] <- r$resumo
    if (!is.null(r$fm)) resultados_fm[[length(resultados_fm)+1]] <- r$fm
    resultados_imp[[length(resultados_imp)+1]] <- r$importancia
    fwrite(r$preds, file.path(OUT, sprintf("candidatos_85_predicoes_h%d.csv", h)))
  }
}

RESUMO <- rbindlist(resultados_resumo)
FM <- if (length(resultados_fm) > 0) rbindlist(resultados_fm) else data.table()
IMP <- rbindlist(resultados_imp)

fwrite(RESUMO, file.path(OUT, "candidatos_85_ml_walkforward_resumo.csv"))
fwrite(FM, file.path(OUT, "candidatos_85_ml_walkforward_fm.csv"))
fwrite(IMP, file.path(OUT, "candidatos_85_ml_walkforward_importancia.csv"))

cat("\n\n===== RESUMO FINAL (R2_OOS) =====\n"); print(RESUMO)
cat("\n===== RESUMO FINAL (Fama-MacBeth long-short) =====\n"); print(FM)
cat("\nOK\n")
