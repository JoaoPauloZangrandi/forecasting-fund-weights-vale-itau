# =============================================================================
# 21_ensemble_ml.R  (exploracao_sinais)
#
# (W) ENSEMBLE DE MACHINE LEARNING: a pesquisa de literatura (rodada 3)
#     achou que NINGUEM testou re-testar sinais de holdings institucionais
#     com metodos nao-lineares (random forest/gradient boosting) -- gap
#     genuino na literatura. Em vez de testar cada sinal isolado (o que
#     ja foi feito, ~19 candidatos), junta TODOS os sinais ja calculados
#     como features de um XGBoost, treinado SO no periodo de treino
#     (<2020-01), testado fora da amostra -- pode capturar interacoes
#     nao-lineares entre sinais fracos que nenhum teste linear pegaria.
#
# Features: CIO peer_ret, E[FIT], Stock IQ (2D), demanda agregada
# revelada, herding (LSV), crowding (HHI posse), retorno proprio (12m
# momentum), tamanho (log valor total).
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(xgboost) })
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

cat("===== Montando features =====\n")
# 1. CIO peer momentum
cio_dt <- fread(file.path(OUT, "cio_peer_return.csv"))[, .(ticker, ym, peer_ret)]

# 2. Stock IQ (2D, ja calculado no script 19)
stock_iq <- fread(file.path(OUT, "stock_iq.csv"))[, .(ticker, ym, stock_iq)]

# 3. Demanda agregada revelada
pp0 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("ativo","ym","valor_mil"))
pp0[, ticker := trimws(sub(".*- ", "", ativo))]
valor_total <- pp0[, .(valor_total_mil = sum(valor_mil)), by = .(ticker, ym)]
vt_prev <- valor_total[, .(ticker, ym = addm(ym, 1), valor_total_prev = valor_total_mil)]
dem <- merge(valor_total, vt_prev, by = c("ticker","ym"))
dem <- dem[valor_total_prev > 0]
dem[, dem_pct := (valor_total_mil - valor_total_prev) / valor_total_prev]
q <- quantile(dem$dem_pct, c(0.01,0.99), na.rm=TRUE)
dem[, dem_pct_w := pmin(pmax(dem_pct, q[1]), q[2])]
dem[, log_tamanho := log(pmax(valor_total_mil,1))]

# 4. Herding (LSV)
pp2 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso"))
pp2[, ticker := trimws(sub(".*- ", "", ativo))]
peso_prev2b <- pp2[, .(cod_fundo, ticker, ym = addm(ym, 1), peso_prev = peso)]
d2 <- merge(pp2, peso_prev2b, by = c("cod_fundo","ticker","ym"))
d2 <- d2[is.finite(peso_prev)]
d2[, compra := peso > peso_prev]; d2[, venda := peso < peso_prev]
por_am <- d2[, .(n_fundos=.N, n_compra=sum(compra), n_venda=sum(venda)), by=.(ticker,ym)]
por_am <- por_am[n_fundos >= 10]
por_am[, p_compra := n_compra/(n_compra+n_venda)]
media_mes2 <- por_am[, .(p_compra_medio_mes = mean(p_compra, na.rm=TRUE)), by=ym]
por_am <- merge(por_am, media_mes2, by="ym")
por_am[, HM_sinal := p_compra - p_compra_medio_mes]

# 5. Crowding (HHI de posse)
pp3 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp3[, ticker := trimws(sub(".*- ", "", ativo))]
pp3 <- pp3[is.finite(aum_prev) & aum_prev > 0]
pp3[, valor_posicao := peso * aum_prev]
crowd <- pp3[, .(hhi_posse = sum((valor_posicao/sum(valor_posicao))^2), n_fundos=.N), by = .(ticker, ym)]
crowd <- crowd[n_fundos >= 10]

# 6. momentum proprio (12m)
mom <- copy(precos); setorder(mom, ticker, ymk)
mom[, log_ret := log(1+retorno)]
mom[, mom_12m := exp(frollsum(log_ret, 12, align="right")) - 1, by = ticker]
mom <- mom[!is.na(mom_12m), .(ticker, ym = ymk, mom_12m)]

# ---- junta tudo ----
feat <- Reduce(function(a,b) merge(a,b,by=c("ticker","ym"), all=FALSE), list(
  cio_dt, stock_iq, dem[, .(ticker, ym, dem_pct_w, log_tamanho)],
  por_am[, .(ticker, ym, HM_sinal)], crowd[, .(ticker, ym, hhi_posse)], mom
))
cat("Ativo-mes com TODAS as features disponiveis:", nrow(feat), "\n")

vars_x <- c("peer_ret","stock_iq","dem_pct_w","log_tamanho","HM_sinal","hhi_posse","mom_12m")

treina_testa_ml <- function(h) {
  m <- copy(feat); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno)]
  for (v in vars_x) m <- m[is.finite(get(v))]

  treino <- m[ym < CORTE]; teste <- m[ym >= CORTE]
  cat(sprintf("\nh=%d: n_treino=%d n_teste=%d\n", h, nrow(treino), nrow(teste)))
  if (nrow(treino) < 200 || nrow(teste) < 100) { cat("amostra pequena demais\n"); return(NULL) }

  X_treino <- as.matrix(treino[, ..vars_x]); y_treino <- treino$retorno
  X_teste  <- as.matrix(teste[, ..vars_x]);  y_teste  <- teste$retorno

  # split interno do treino em treino/validacao (80/20 por MES, nao por linha, pra nao vazar)
  meses_treino <- sort(unique(treino$ym))
  corte_val <- meses_treino[floor(0.8*length(meses_treino))]
  idx_tr <- treino$ym <= corte_val; idx_val <- treino$ym > corte_val

  dtrain <- xgb.DMatrix(data = X_treino[idx_tr,,drop=FALSE], label = y_treino[idx_tr])
  dval   <- xgb.DMatrix(data = X_treino[idx_val,,drop=FALSE], label = y_treino[idx_val])

  set.seed(42)
  modelo <- xgb.train(
    params = list(objective = "reg:squarederror", max_depth = 3, eta = 0.03,
                   subsample = 0.7, colsample_bytree = 0.7, min_child_weight = 20),
    data = dtrain, nrounds = 500,
    watchlist = list(val = dval), early_stopping_rounds = 30, verbose = 0
  )
  cat("  melhor iteracao:", modelo$best_iteration, "\n")

  pred_teste <- predict(modelo, X_teste)
  sse_m <- sum((y_teste - pred_teste)^2); sse_n <- sum((y_teste - mean(y_treino))^2)
  r2_oos <- 1 - sse_m/sse_n
  cat(sprintf("  R2_OOS (XGBoost, %d features) = %.4f%%\n", length(vars_x), 100*r2_oos))

  imp <- xgb.importance(model = modelo)
  cat("  importancia das features:\n"); print(imp)

  # Fama-MacBeth no long-short baseado na PREVISAO do modelo
  teste[, pred := pred_teste]
  breaks <- unique(quantile(teste$pred, seq(0,1,0.2), na.rm = TRUE))
  if (length(breaks) < 4) { cat("  previsoes degeneradas, sem variacao suficiente\n"); return(NULL) }
  breaks[1] <- -Inf; breaks[length(breaks)] <- Inf
  teste[, quintil := as.integer(cut(pred, breaks, labels = FALSE, include.lowest = TRUE))]
  n_grupos <- length(breaks)-1
  if (n_grupos < 5) teste[, quintil := ifelse(quintil==1,1,ifelse(quintil==n_grupos,5,quintil))]
  por_mes <- teste[quintil %in% c(1,5), .(retorno_medio=mean(retorno)), by=.(ym,quintil)]
  sm <- dcast(por_mes, ym~quintil, value.var="retorno_medio")
  if (!all(c("1","5") %in% names(sm))) { cat("  sem quintis 1 e 5 completos\n"); return(NULL) }
  setnames(sm, c("1","5"), c("q1","q5")); sm <- sm[is.finite(q1)&is.finite(q5)]
  sm[, spread := q5-q1]; n_meses <- nrow(sm)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(n_meses)); p_fm <- 2*pt(-abs(t_fm), df=n_meses-1)
  cat(sprintf("  Long-short (Fama-MacBeth): %d meses, spread=%.2fpp/mes, t=%.2f, p=%.4f\n",
              n_meses, 100*media, t_fm, p_fm))
  data.table(horizonte=h, r2_oos_pct=100*r2_oos, n_meses=n_meses, spread_pp=100*media, t_fm=t_fm, p_fm=p_fm)
}

resultados <- list()
for (h in c(1,3,6)) {
  r <- treina_testa_ml(h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}
R <- rbindlist(resultados)
fwrite(R, file.path(OUT, "candidatos_21_ensemble_ml.csv"))
cat("\n\nOK\n")
