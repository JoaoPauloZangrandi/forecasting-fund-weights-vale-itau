# =============================================================================
# 80_testar_fragilidade_donos.R  (exploracao_sinais, agente eventos)
#
# Testa as 3 caracteristicas de "fragilidade da base de donos" construidas
# no script 79 (tamanho medio dos fundos donos, fluxo recente medio, taxa de
# saida/churn media -- todas ponderadas por valor da posicao) como
# preditoras de RETORNO futuro e de VOLATILIDADE futura da acao.
#
# Metodologia (as 3 licoes inegociaveis da exploracao):
#  - Fama-MacBeth de verdade (1 coeficiente/spread por MES, nao por
#    ticker-mes) para testar significancia.
#  - Quintis RE-CORTADOS A CADA MES (nao breaks fixos do treino aplicados
#    no teste -- licao do candidato #15/36, que quase gerou um 2o
#    falso-positivo tipo Comomentum).
#  - Disciplina fora-da-amostra treino<202001 / teste>=202001 para o
#    resultado OFICIAL reportado (o recorte mensal de quintil nao usa
#    informacao do treino, entao nao ha vazamento mesmo reportando os 2
#    periodos, mas o teste que conta oficialmente e o periodo >=202001).
#  - N minimo por perna sempre reportado (para excluir degenerescencia).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

frag <- fread(file.path(OUT, "candidatos_79_fragilidade_donos.csv"))
precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
precos <- precos[is.finite(retorno)]
setorder(precos, ticker, ymk)

# ---- winsorizar 1%/99% cada caracteristica (mesma pratica do resto da exploracao) ----
winsor <- function(x) { q <- quantile(x, c(0.01,0.99), na.rm=TRUE); pmin(pmax(x,q[1]),q[2]) }
frag[, frag_laum_w := winsor(frag_laum_medio)]
frag[, frag_flow_w := winsor(frag_flow_medio)]
frag[, frag_cotistas_w := winsor(frag_cotistas_medio)]
frag[, frag_exit_rate_w := winsor(frag_exit_rate_medio)]

caracteristicas <- list(
  list(var = "frag_laum_w", nome = "Tamanho medio dos fundos donos (log AUM)"),
  list(var = "frag_flow_w", nome = "Fluxo medio recente dos fundos donos"),
  list(var = "frag_cotistas_w", nome = "N. medio de cotistas dos fundos donos (log)"),
  list(var = "frag_exit_rate_w", nome = "Taxa media de saida/churn dos fundos donos")
)

# =============================================================================
# ALVO 1: RETORNO FUTURO (h = 1, 3, 6 meses)
# =============================================================================
teste_oos_linear <- function(m, var_x, var_y, nome, h) {
  treino <- m[ym < CORTE]; teste <- m[ym >= CORTE]
  if (nrow(treino) < 100 || nrow(teste) < 50) return(NULL)
  form <- as.formula(paste(var_y, "~", var_x))
  fit_lm <- lm(form, data = treino)
  alpha <- coef(fit_lm)[1]; beta <- coef(fit_lm)[2]
  pred <- alpha + beta * teste[[var_x]]
  sse_m <- sum((teste[[var_y]] - pred)^2); sse_n <- sum((teste[[var_y]] - mean(treino[[var_y]]))^2)
  r2_oos <- 1 - sse_m/sse_n
  s_full <- summary(lm(form, data = m))$coefficients
  data.table(sinal = nome, horizonte = h, n_treino = nrow(treino), n_teste = nrow(teste),
             beta_treino = beta, r2_oos_pct = 100*r2_oos, coef_full = s_full[2,1], t_full = s_full[2,3], p_full = s_full[2,4])
}

fama_macbeth_recut <- function(m, var_x, var_y, nome, h) {
  mm <- copy(m)
  setnames(mm, var_x, "x"); setnames(mm, var_y, "y")
  mm <- mm[is.finite(x) & is.finite(y)]
  # quintil RE-CORTADO a cada mes (by=ym) -- nunca usa break do treino
  mm[, quintil := as.integer(cut(x, quantile(x, 0:5/5, na.rm=TRUE), include.lowest=TRUE)), by = ym]
  mm <- mm[!is.na(quintil)]
  teste <- mm[ym >= CORTE]
  if (nrow(teste) < 50) return(NULL)
  n_por_grupo <- teste[quintil %in% c(1,5), .N, by = .(ym, quintil)]
  n_min <- if (nrow(n_por_grupo) > 0) min(n_por_grupo$N) else 0L
  por_mes <- teste[quintil %in% c(1,5), .(m = mean(y)), by = .(ym, quintil)]
  sm <- dcast(por_mes, ym ~ quintil, value.var = "m")
  if (!all(c("1","5") %in% names(sm))) return(NULL)
  setnames(sm, c("1","5"), c("q1","q5"))
  sm <- sm[is.finite(q1) & is.finite(q5)]
  sm[, spread := q5 - q1]
  n_meses <- nrow(sm)
  if (n_meses < 6) return(data.table(sinal = nome, horizonte = h, n_meses = n_meses, n_min_grupo = n_min,
                                       spread_medio_pp = NA, t_fm = NA, p_fm = NA, pct_meses_positivo = NA))
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(n_meses)); p_fm <- 2*pt(-abs(t_fm), df = n_meses-1)
  data.table(sinal = nome, horizonte = h, n_meses = n_meses, n_min_grupo = n_min,
             spread_medio_pp = 100*media, t_fm = t_fm, p_fm = p_fm, pct_meses_positivo = 100*mean(sm$spread > 0))
}

cat("===== ALVO 1: RETORNO FUTURO =====\n\n")
res_ret_lin <- list(); res_ret_fm <- list()
for (h in c(1,3,6)) {
  m <- copy(frag); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno)]
  for (car in caracteristicas) {
    sub <- m[is.finite(get(car$var)), .(ym, ym_ret, x = get(car$var), retorno)]
    setnames(sub, c("x","retorno"), c(car$var, "retorno"))
    r1 <- teste_oos_linear(sub, car$var, "retorno", car$nome, h)
    r2 <- fama_macbeth_recut(sub, car$var, "retorno", car$nome, h)
    if (!is.null(r1)) { res_ret_lin[[length(res_ret_lin)+1]] <- r1
      cat(sprintf("%-48s h=%d [linear]  beta=%+.5f R2_OOS=%7.3f%% p_full=%.4f\n", car$nome, h, r1$beta_treino, r1$r2_oos_pct, r1$p_full)) }
    if (!is.null(r2)) { res_ret_fm[[length(res_ret_fm)+1]] <- r2
      cat(sprintf("%-48s h=%d [FM-recut] %2d meses n_min=%3d spread=%+.3fpp t=%6.2f p=%.4f\n",
                  car$nome, h, r2$n_meses, r2$n_min_grupo, r2$spread_medio_pp, r2$t_fm, r2$p_fm)) }
  }
  cat("\n")
}

# =============================================================================
# ALVO 2: VOLATILIDADE FUTURA (h = 3, 6, 12 meses, dp do retorno t+1..t+h)
# =============================================================================
cat("\n===== ALVO 2: VOLATILIDADE FUTURA =====\n\n")
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

teste_oos_linear_vol <- function(m, var_x, nome, h) {
  mm <- copy(m); setnames(mm, var_x, "x")
  treino <- mm[ym < CORTE]; teste <- mm[ym >= CORTE]
  if (nrow(treino) < 100 || nrow(teste) < 50) return(NULL)
  fit_lm <- lm(vol_futura ~ x, data = treino)
  pred <- predict(fit_lm, newdata = teste)
  sse_m <- sum((teste$vol_futura-pred)^2); sse_n <- sum((teste$vol_futura-mean(treino$vol_futura))^2)
  r2_oos <- 1 - sse_m/sse_n
  s_full <- summary(lm(vol_futura~x, data=mm))$coefficients
  data.table(sinal=nome, horizonte=h, n_treino=nrow(treino), n_teste=nrow(teste),
             r2_oos_pct=100*r2_oos, coef_full=s_full[2,1], t_full=s_full[2,3], p_full=s_full[2,4])
}

fama_macbeth_recut_vol <- function(m, var_x, nome, h) {
  mm <- copy(m); setnames(mm, var_x, "x")
  mm <- mm[is.finite(x) & is.finite(vol_futura)]
  mm[, quintil := as.integer(cut(x, quantile(x, 0:5/5, na.rm=TRUE), include.lowest=TRUE)), by = ym]
  mm <- mm[!is.na(quintil)]
  teste <- mm[ym >= CORTE]
  if (nrow(teste) < 50) return(NULL)
  n_por_grupo <- teste[quintil %in% c(1,5), .N, by = .(ym, quintil)]
  n_min <- if (nrow(n_por_grupo) > 0) min(n_por_grupo$N) else 0L
  por_mes <- teste[quintil %in% c(1,5), .(m = mean(vol_futura)), by = .(ym, quintil)]
  sm <- dcast(por_mes, ym ~ quintil, value.var = "m")
  if (!all(c("1","5") %in% names(sm))) return(NULL)
  setnames(sm, c("1","5"), c("q1","q5"))
  sm <- sm[is.finite(q1) & is.finite(q5)]
  sm[, spread := q5 - q1]
  n_meses <- nrow(sm)
  if (n_meses < 6) return(data.table(sinal=nome, horizonte=h, n_meses=n_meses, n_min_grupo=n_min,
                                       spread_medio=NA, t_fm=NA, p_fm=NA))
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(n_meses)); p_fm <- 2*pt(-abs(t_fm), df = n_meses-1)
  data.table(sinal=nome, horizonte=h, n_meses=n_meses, n_min_grupo=n_min,
             spread_medio=media, t_fm=t_fm, p_fm=p_fm)
}

res_vol_lin <- list(); res_vol_fm <- list()
for (h in c(3,6,12)) {
  vol_h <- calc_vol_futura(precos, h)
  m <- merge(frag, vol_h, by = c("ticker","ym"))
  for (car in caracteristicas) {
    sub <- m[is.finite(get(car$var)) & is.finite(vol_futura), .(ym, x = get(car$var), vol_futura)]
    setnames(sub, "x", car$var)
    r1 <- teste_oos_linear_vol(sub, car$var, car$nome, h)
    r2 <- fama_macbeth_recut_vol(sub, car$var, car$nome, h)
    if (!is.null(r1)) { res_vol_lin[[length(res_vol_lin)+1]] <- r1
      cat(sprintf("%-48s h=%2d [linear]  R2_OOS=%7.3f%% p_full=%.5f\n", car$nome, h, r1$r2_oos_pct, r1$p_full)) }
    if (!is.null(r2)) { res_vol_fm[[length(res_vol_fm)+1]] <- r2
      cat(sprintf("%-48s h=%2d [FM-recut] %2d meses n_min=%3d spread(vol Q5-Q1)=%+.5f t=%6.2f p=%.5f\n",
                  car$nome, h, r2$n_meses, r2$n_min_grupo, r2$spread_medio, r2$t_fm, r2$p_fm)) }
  }
  cat("\n")
}

RL1 <- rbindlist(res_ret_lin); RF1 <- rbindlist(res_ret_fm, fill=TRUE)
RL2 <- rbindlist(res_vol_lin); RF2 <- rbindlist(res_vol_fm, fill=TRUE)
fwrite(RL1, file.path(OUT, "candidatos_80_retorno_linear.csv"))
fwrite(RF1, file.path(OUT, "candidatos_80_retorno_famamacbeth.csv"))
fwrite(RL2, file.path(OUT, "candidatos_80_volatilidade_linear.csv"))
fwrite(RF2, file.path(OUT, "candidatos_80_volatilidade_famamacbeth.csv"))

cat("\n\n===== RESUMO RETORNO (Fama-MacBeth, ordenado por p) =====\n")
print(RF1[order(p_fm)][, .(sinal, horizonte, n_meses, n_min_grupo, spread_medio_pp=round(spread_medio_pp,3), t_fm=round(t_fm,2), p_fm=round(p_fm,4))])

cat("\n\n===== RESUMO VOLATILIDADE (Fama-MacBeth, ordenado por p) =====\n")
print(RF2[order(p_fm)][, .(sinal, horizonte, n_meses, n_min_grupo, spread_medio=round(spread_medio,5), t_fm=round(t_fm,2), p_fm=round(p_fm,5))])

cat("\nOK\n")
