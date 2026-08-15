# =============================================================================
# 86_ml_walkforward_robustez.R  (exploracao_sinais, agente ML/pairs)
#
# Robustez do candidato de ML walk-forward (script 85). Duas checagens:
#   (R1) Reduzir para so' as 3 features mais importantes no script 85
#        (peer_ret, EFIT, mom_12m -- que dominaram a importancia em TODOS
#        os horizontes) -- menos parametros pra estimar, menos risco de
#        overfitting com N pequeno; testa se "menos e' mais" aqui.
#   (R2) Refit mais frequente (a cada 3 meses em vez de 6) -- walk-forward
#        mais fino, mais responsivo a mudanca de regime, mas com folds de
#        treino um pouco menores em cada refit.
# Mesma funcao de walk-forward do script 85 (reaproveitada), so' variando
# vars_x e janela_refit.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(xgboost) })
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
set.seed(42)

painel <- fread(file.path(OUT, "composto_painel_sinais.csv"))
precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
setorder(precos, ticker, ymk)
precos[, log_ret := log(1 + retorno)]
precos[, mom_12m := exp(frollsum(log_ret, 12, align = "right")) - 1, by = ticker]
mom <- precos[!is.na(mom_12m), .(ticker, ym = ymk, mom_12m)]

feat <- merge(painel, mom, by = c("ticker","ym"))
cat("Ativo-mes com features (reduzido):", nrow(feat), "\n")

rodar_walkforward <- function(h, vars_x, janela_refit, tag) {
  m <- copy(feat); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos[, .(ticker, ymk, retorno)], by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno)]
  for (v in vars_x) m <- m[is.finite(get(v))]
  setorder(m, ym)

  meses_teste <- sort(unique(m[ym >= CORTE]$ym))
  if (length(meses_teste) < 6) return(NULL)
  pontos_refit <- meses_teste[seq(1, length(meses_teste), by = janela_refit)]

  preds_todos <- list(); imp_todos <- list()
  for (rp_idx in seq_along(pontos_refit)) {
    rp <- pontos_refit[rp_idx]
    prox_rp <- if (rp_idx < length(pontos_refit)) pontos_refit[rp_idx+1] else max(meses_teste) + 100
    meses_desta_janela <- meses_teste[meses_teste >= rp & meses_teste < prox_rp]
    treino_disp <- m[ym < rp]; alvo <- m[ym %in% meses_desta_janela]
    if (nrow(treino_disp) < 150 || nrow(alvo) < 10) next

    meses_tr <- sort(unique(treino_disp$ym))
    corte_val <- meses_tr[max(1, floor(0.85 * length(meses_tr)))]
    idx_tr <- treino_disp$ym <= corte_val; idx_val <- treino_disp$ym > corte_val
    if (sum(idx_val) < 20) { idx_val <- treino_disp$ym > meses_tr[max(1, length(meses_tr)-3)]; idx_tr <- !idx_val }

    X_tr <- as.matrix(treino_disp[idx_tr, ..vars_x]); y_tr <- treino_disp[idx_tr]$retorno
    X_val <- as.matrix(treino_disp[idx_val, ..vars_x]); y_val <- treino_disp[idx_val]$retorno
    X_alvo <- as.matrix(alvo[, ..vars_x])
    dtr <- xgb.DMatrix(data = X_tr, label = y_tr); dval <- xgb.DMatrix(data = X_val, label = y_val)

    modelo <- xgb.train(
      params = list(objective = "reg:squarederror", max_depth = 2, eta = 0.03,
                     subsample = 0.7, colsample_bytree = 0.8, min_child_weight = 30, lambda = 5),
      data = dtr, nrounds = 300, watchlist = list(val = dval), early_stopping_rounds = 25, verbose = 0
    )
    pred_alvo <- predict(modelo, X_alvo)
    preds_todos[[length(preds_todos)+1]] <- data.table(
      ticker = alvo$ticker, ym = alvo$ym, retorno = alvo$retorno, pred = pred_alvo,
      media_ingenua = mean(treino_disp$retorno), refit_em = rp
    )
    imp <- tryCatch(xgb.importance(model = modelo), error = function(e) NULL)
    if (!is.null(imp)) imp_todos[[length(imp_todos)+1]] <- imp[, refit_em := rp]
  }
  if (length(preds_todos) == 0) return(NULL)
  PRED <- rbindlist(preds_todos)
  sse_m <- sum((PRED$retorno - PRED$pred)^2); sse_n <- sum((PRED$retorno - PRED$media_ingenua)^2)
  r2_oos <- 1 - sse_m/sse_n

  IMP <- rbindlist(imp_todos, fill = TRUE)
  imp_agg <- IMP[, .(gain_medio = mean(Gain, na.rm=TRUE)), by = Feature]
  imp_agg <- merge(data.table(Feature = vars_x), imp_agg, by = "Feature", all.x = TRUE)
  imp_agg[is.na(gain_medio), gain_medio := 0]; setorder(imp_agg, -gain_medio)

  cortar_quintil <- function(x) {
    br <- unique(quantile(x, 0:5/5, na.rm = TRUE))
    if (length(br) < 3) return(rep(NA_integer_, length(x)))
    as.integer(cut(x, br, include.lowest = TRUE, labels = FALSE))
  }
  PRED[, quintil := cortar_quintil(pred), by = ym]
  # remapeia pro extremo quando ha' menos de 5 grupos naquele mes (mesma
  # convencao usada no resto da exploracao, ex. script 11/21)
  PRED[, n_grupos_mes := uniqueN(quintil, na.rm = TRUE), by = ym]
  PRED[!is.na(quintil) & n_grupos_mes < 5 & n_grupos_mes >= 2,
       quintil := ifelse(quintil == 1, 1, ifelse(quintil == n_grupos_mes, 5, quintil))]
  por_mes <- PRED[quintil %in% c(1,5), .(ret = mean(retorno)), by = .(ym, quintil)]
  sm <- dcast(por_mes, ym ~ quintil, value.var = "ret")
  fm_res <- NULL
  if (all(c("1","5") %in% names(sm))) {
    setnames(sm, c("1","5"), c("q1","q5")); sm <- sm[is.finite(q1)&is.finite(q5)]; sm[, spread := q5-q1]
    n_meses_fm <- nrow(sm)
    if (n_meses_fm >= 6) {
      media <- mean(sm$spread); dp <- sd(sm$spread)
      t_fm <- media/(dp/sqrt(n_meses_fm)); p_fm <- 2*pt(-abs(t_fm), df = n_meses_fm-1)
      fm_res <- data.table(tag = tag, horizonte = h, n_meses_fm = n_meses_fm, spread_pct = 100*media, t_fm = t_fm, p_fm = p_fm)
    }
  }
  cat(sprintf("[%s] h=%d: n_obs=%d n_refits=%d R2_OOS=%.4f%% | FM: %s\n",
              tag, h, nrow(PRED), length(preds_todos), 100*r2_oos,
              if (is.null(fm_res)) "NA" else sprintf("spread=%.3f%%/mes t=%.2f p=%.4f", fm_res$spread_pct, fm_res$t_fm, fm_res$p_fm)))
  cat("  importancia:\n"); print(imp_agg)
  list(resumo = data.table(tag = tag, horizonte = h, n_obs = nrow(PRED), n_refits = length(preds_todos), r2_oos_pct = 100*r2_oos),
       fm = fm_res)
}

vars_reduzido <- c("peer_ret", "EFIT", "mom_12m")
resumos <- list(); fms <- list()

cat("\n===== (R1) FEATURE SET REDUZIDO (top-3: peer_ret, EFIT, mom_12m), refit 6m =====\n")
for (h in c(1,3,6)) {
  r <- rodar_walkforward(h, vars_reduzido, 6, "R1_top3_refit6m")
  if (!is.null(r)) { resumos[[length(resumos)+1]] <- r$resumo; if (!is.null(r$fm)) fms[[length(fms)+1]] <- r$fm }
}

cat("\n===== (R2) FEATURE SET COMPLETO, refit mais fino (3 meses) =====\n")
vars_completo <- c("peer_ret","hhi_posse","prox_52w_high","reversao","idio_vol_12m",
                    "delta_breadth_w","dem_pct_w","EFIT","mom_12m")
for (h in c(1,3,6)) {
  r <- rodar_walkforward(h, vars_completo, 3, "R2_completo_refit3m")
  if (!is.null(r)) { resumos[[length(resumos)+1]] <- r$resumo; if (!is.null(r$fm)) fms[[length(fms)+1]] <- r$fm }
}

RESUMO <- rbindlist(resumos); FM <- if (length(fms)>0) rbindlist(fms) else data.table()
fwrite(RESUMO, file.path(OUT, "candidatos_86_ml_robustez_resumo.csv"))
fwrite(FM, file.path(OUT, "candidatos_86_ml_robustez_fm.csv"))
cat("\n\n===== RESUMO FINAL =====\n"); print(RESUMO)
cat("\n===== FM FINAL =====\n"); print(FM)
cat("\nOK\n")
