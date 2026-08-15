# =============================================================================
# 59_robustez_composto_h6.R  (exploracao_sinais, agente "composto")
#
# O UNICO resultado nominalmente "quase-interessante" das 3 abordagens de
# combinacao testadas (scripts 56/57/58): script 57, h=6, composto com
# pesos de regressao FM multipla estimados no TREINO (all8 sinais),
# t=3,05, p=0,007 -- passa de 5% padrao mas NAO do limiar de Bonferroni
# (0,05/500 = 0,0001). Antes de registrar como "achado", aplica a MESMA
# bateria de diagnosticos que ja pegou 2 quase-falsos-positivos nesta
# exploracao (Comomentum, CIO com quintil degenerado):
#   1) tamanho dos grupos mes-a-mes (degenerescencia)
#   2) estabilidade 2020 vs 2021 (o mesmo teste que derrubou CIO original)
#   3) decomposicao: quanto do resultado vem so' de peer_ret+hhi (os 2
#      sinais com peso maior) vs. os outros 6 sinais quase-zero
#   4) quanto e' 1-2 meses dominando o spread (outlier temporal, igual
#      Comomentum)
#   5) composicao da carteira (tamanho/liquidez das pernas)
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
painel <- fread(file.path(OUT, "composto_painel_sinais.csv"))
painel[, s_peer   := peer_ret]
painel[, s_hhi     := -hhi_posse]
painel[, s_52wh    := prox_52w_high]
painel[, s_rev     := reversao]
painel[, s_idiovol := idio_vol_12m]
painel[, s_breadth := delta_breadth_w]
painel[, s_dem      := dem_pct_w]
painel[, s_efit     := EFIT]
vars8 <- c("s_peer","s_hhi","s_52wh","s_rev","s_idiovol","s_breadth","s_dem","s_efit")

h <- 6
m <- copy(painel); m[, ym_ret := addm(ym, h)]
m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
m <- m[is.finite(retorno)]
mtreino <- m[ym < CORTE]; mteste <- m[ym >= CORTE]

# pesos do treino (all8), igual ao script 57
mtr <- copy(mtreino[complete.cases(mtreino[, ..vars8]) & is.finite(retorno)])
for (v in vars8) mtr[, (paste0(v,"_z")) := as.numeric(scale(get(v)))]
zcols <- paste0(vars8, "_z")
fit_tr <- lm(as.formula(paste("retorno ~", paste(zcols, collapse=" + "))), data = mtr)
w <- coef(fit_tr)[zcols]; names(w) <- vars8
cat("Pesos do treino (h=6, all8):\n"); print(round(w,4))

mte <- copy(mteste[complete.cases(mteste[, ..vars8]) & is.finite(retorno)])
for (v in vars8) mte[, (paste0(v,"_z")) := as.numeric(scale(get(v))), by = ym]
mte[, score_all8 := as.matrix(mte[, paste0(vars8,"_z"), with = FALSE]) %*% w]
# decomposicao: score usando SO peer+hhi (os 2 pesos maiores em magnitude)
mte[, score_peer_hhi := w["s_peer"]*s_peer_z + w["s_hhi"]*s_hhi_z]
# score usando so' os OUTROS 6 (sem peer nem hhi)
outros <- setdiff(vars8, c("s_peer","s_hhi"))
mte[, score_outros6 := as.matrix(mte[, paste0(outros,"_z"), with = FALSE]) %*% w[outros]]

quintil_spread_mensal <- function(dt, var_x) {
  d <- copy(dt[is.finite(get(var_x))])
  d[, quintil := {
    qs <- quantile(get(var_x), 0:5/5, na.rm = TRUE)
    if (length(unique(qs)) < 6) as.integer(NA) else as.integer(cut(get(var_x), qs, include.lowest = TRUE))
  }, by = ym]
  d <- d[!is.na(quintil)]
  chk <- d[quintil %in% c(1,5), .N, by = .(ym, quintil)]
  por_mes <- d[quintil %in% c(1,5), .(rm = mean(retorno), n=.N), by = .(ym, quintil)]
  sm <- dcast(por_mes, ym ~ quintil, value.var = "rm")
  if (!all(c("1","5") %in% names(sm))) return(NULL)
  setnames(sm, c("1","5"), c("q1","q5")); sm <- sm[is.finite(q1) & is.finite(q5)]
  sm[, spread := q5-q1]
  list(sm = sm, n_min = if(nrow(chk)>0) min(chk$N) else NA)
}

testar <- function(var_x, nome) {
  res <- quintil_spread_mensal(mte, var_x)
  if (is.null(res)) { cat(sprintf("%-20s -- sem dados suficientes\n", nome)); return(invisible(NULL)) }
  sm <- res$sm; nmes <- nrow(sm)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(nmes)); p_fm <- 2*pt(-abs(t_fm), df=nmes-1)
  cat(sprintf("%-20s %2d meses n_min=%3d spread=%+7.3fpp/mes t=%6.2f p=%.4f\n",
              nome, nmes, res$n_min, 100*media, t_fm, p_fm))
  sm
}

cat("\n===== 1) TAMANHO DOS GRUPOS (degenerescencia?) =====\n")
sm_full <- testar("score_all8", "score_all8 (completo)")
cat("N min por quintil-mes:", "ver acima (n_min)\n")

cat("\n===== 2) DECOMPOSICAO: so' peer+hhi vs. so' os outros 6 =====\n")
testar("score_peer_hhi", "so peer+hhi")
testar("score_outros6", "so outros 6")

cat("\n===== 3) ESTABILIDADE 2020 vs 2021 =====\n")
mte2020 <- mte[ym < 202100]; mte2021 <- mte[ym >= 202100]
cat("--- so 2020 ---\n"); testar_sub <- function(dt, var_x, nome) {
  res <- quintil_spread_mensal(dt, var_x)
  if (is.null(res)) { cat(sprintf("%-20s -- sem dados\n", nome)); return(invisible(NULL)) }
  sm <- res$sm; nmes <- nrow(sm)
  if (nmes < 3) { cat(sprintf("%-20s so %d meses, pouco pra testar\n", nome, nmes)); return(invisible(sm)) }
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(nmes)); p_fm <- 2*pt(-abs(t_fm), df=nmes-1)
  cat(sprintf("%-20s %2d meses n_min=%3d spread=%+7.3fpp/mes t=%6.2f p=%.4f\n",
              nome, nmes, res$n_min, 100*media, t_fm, p_fm))
  sm
}
testar_sub(mte2020, "score_all8", "2020 (score_all8)")
testar_sub(mte2021, "score_all8", "2021 (score_all8)")

cat("\n===== 4) CONTRIBUICAO MES-A-MES (outlier temporal?) =====\n")
if (!is.null(sm_full)) {
  sm_full[, contrib_pct := 100*spread/sum(spread)]
  print(sm_full[order(-abs(spread))][, .(ym, q1=round(q1,4), q5=round(q5,4), spread=round(spread,4), contrib_pct=round(contrib_pct,1))])
}

cat("\n===== 5) COMPOSICAO DAS PERNAS (top/bottom quintil, ultimo mes de teste) =====\n")
d5 <- copy(mte[is.finite(score_all8)])
d5[, quintil := {
  qs <- quantile(score_all8, 0:5/5, na.rm = TRUE)
  if (length(unique(qs)) < 6) as.integer(NA) else as.integer(cut(score_all8, qs, include.lowest = TRUE))
}, by = ym]
ultimo_mes <- max(d5$ym)
cat(sprintf("Ultimo mes de teste com dado: %d\n", ultimo_mes))
d5u <- d5[ym == ultimo_mes & quintil %in% c(1,5)]
print(d5u[order(quintil, -abs(score_all8))][, .(ticker, quintil, score_all8=round(score_all8,3), retorno=round(retorno,4))])

# valor total mantido pelos fundos (proxy de tamanho/liquidez) pra ver se a perna
# vendida (quintil 1, score baixo = "bearish") e' dominada por small caps
valor_total <- fread(file.path(DATA,"painel_multiativo_final.csv"), select=c("ativo","ym","valor_mil"))
valor_total[, ticker := trimws(sub(".*- ", "", ativo))]
vt <- valor_total[, .(valor_total_mil = sum(valor_mil)), by=.(ticker, ym)]
d5all <- merge(d5[quintil %in% c(1,5)], vt, by=c("ticker","ym"))
cat("\nMediana de valor_total_mil (proxy de tamanho/liquidez) por perna, todo o periodo de teste:\n")
print(d5all[, .(mediana_valor_mil = median(valor_total_mil), n=.N), by=quintil])

cat("\nOK\n")
