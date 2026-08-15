# =============================================================================
# 56_composto_ranking_percentil.R  (exploracao_sinais, agente "composto")
#
# ANGULO (a) do agente: ranking composto simples. Para cada mes, cada sinal
# vira percentil cross-sectional (0 a 1, maior = "mais bullish" depois de
# alinhar a direcao). Soma/media dos percentis = score composto. Quintil do
# score composto RE-CORTADO A CADA MES (metodo B, nunca breaks fixos do
# treino). Fama-MacBeth de verdade na serie de spreads mensais.
#
# DIRECAO de cada sinal (alinhamento "maior percentil = mais bullish"):
#   peer_ret        (+) CIO Peer Momentum -- teoria + achado empirico (cand#15)
#   hhi_posse       (-) inverso de HHI -- teoria de limits-to-arbitrage/
#                       crowding: mais concentrado = mais risco = pede premio
#                       maior OU pressão de liquidacao pior (ambas as leituras
#                       da literatura apontam para HHI alto = "ruim" para
#                       quem esta comprado -- usamos -hhi_posse)
#   prox_52w_high   (+) George-Hwang 2004, teoria padrao
#   reversao        (+) ja e' -retorno_mes_anterior, teoria padrao (Jegadeesh)
#   idio_vol_12m    (+) ACHADO BRASILEIRO (candidato #24 + pesquisa rodada 6):
#                       ao contrario do puzzle americano, no Brasil a relacao
#                       encontrada foi POSITIVA -- documentado no log, nao e'
#                       escolha arbitraria
#   delta_breadth_w (+) Chen-Hong-Stein 2002, teoria padrao
#   dem_pct_w       (?) sinal ambiguo na propria exploracao (candidato B) --
#                       direcao determinada SO com dado de TREINO (regressao
#                       univariada ym<2020), nunca com o teste, documentado
#                       abaixo explicitamente pra nao ser acusado de garimpo
#   EFIT            (?) idem dem_pct_w -- log mostra spreads negativos na
#                       maioria dos horizontes (cand#11); direcao fixada no
#                       treino, nao escolhida a dedo
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/500  # mesma base de comparacao ja usada pelo resto da exploracao (~450-500 especificacoes)
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
painel <- fread(file.path(OUT, "composto_painel_sinais.csv"))

# ---- alinhamento dos sinais ambiguos (dem_pct_w, EFIT) usando SO' treino ----
determinar_sinal_treino <- function(var_x, h) {
  m <- copy(painel); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"))
  m <- m[ym < CORTE & is.finite(get(var_x)) & is.finite(retorno)]
  if (nrow(m) < 50) return(1)
  fit <- lm(retorno ~ get(var_x), data = m)
  s <- sign(coef(fit)[2])
  if (s == 0 || is.na(s)) s <- 1
  s
}

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
  por_mes <- teste[quintil %in% c(1,5), .(rm = mean(get(var_y)), n = .N), by = .(ym, quintil)]
  sm <- dcast(por_mes, ym ~ quintil, value.var = "rm")
  if (!all(c("1","5") %in% names(sm))) return(NULL)
  setnames(sm, c("1","5"), c("q1","q5")); sm <- sm[is.finite(q1) & is.finite(q5)]
  sm[, spread := q5 - q1]; nmes <- nrow(sm)
  if (nmes < 6) return(NULL)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(nmes)); p_fm <- 2*pt(-abs(t_fm), df = nmes-1)
  sh <- media/dp*sqrt(12)
  pct_pos <- 100*mean(sm$spread > 0)
  cat(sprintf("%-32s h=%d %2d meses n_min=%3d spread=%+7.2fpp/mes Sharpe=%5.2f t=%6.2f p=%.5f %%pos=%.0f%% | Bonf:%s\n",
              nome, h, nmes, n_min, 100*media, sh, t_fm, p_fm, pct_pos,
              ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
  data.table(sinal = nome, horizonte = h, n_meses = nmes, n_min_grupo_mes = n_min,
             spread_pp = 100*media, sharpe = sh, t_fm = t_fm, p_fm = p_fm,
             pct_meses_positivo = pct_pos, sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
}

resultados <- list()

for (h in c(1,3,6)) {
  cat(sprintf("\n\n========================= HORIZONTE h=%d =========================\n", h))

  sinal_dem  <- determinar_sinal_treino("dem_pct_w", h)
  sinal_efit <- determinar_sinal_treino("EFIT", h)
  cat(sprintf("Direcao fixada no TREINO -- dem_pct_w: %s | EFIT: %s\n",
              ifelse(sinal_dem > 0, "+", "-"), ifelse(sinal_efit > 0, "+", "-")))

  m <- copy(painel); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  mteste <- m[ym >= CORTE & is.finite(retorno)]

  # sinais alinhados (maior = mais bullish)
  mteste[, s_peer   := peer_ret]
  mteste[, s_hhi     := -hhi_posse]
  mteste[, s_52wh    := prox_52w_high]
  mteste[, s_rev     := reversao]
  mteste[, s_idiovol := idio_vol_12m]
  mteste[, s_breadth := delta_breadth_w]
  mteste[, s_dem      := sinal_dem  * dem_pct_w]
  mteste[, s_efit     := sinal_efit * EFIT]

  # percentil cross-sectional por mes (0 a 1) de cada sinal alinhado
  vars_all <- c("s_peer","s_hhi","s_52wh","s_rev","s_idiovol","s_breadth","s_dem","s_efit")
  for (v in vars_all) {
    pv <- paste0("pct_", v)
    mteste[is.finite(get(v)), (pv) := frank(get(v))/.N, by = ym]
  }

  # ---- composto TOP3 (os sinais explicitamente pedidos: CIO + inv.HHI + 52wk-high) ----
  pcols3 <- c("pct_s_peer","pct_s_hhi","pct_s_52wh")
  mteste[, n_disp3 := rowSums(!is.na(.SD)), .SDcols = pcols3]
  mteste[n_disp3 == 3, score_top3 := rowMeans(.SD), .SDcols = pcols3]

  # ---- composto TOP5 (+ reversao + idio-vol) ----
  pcols5 <- c(pcols3, "pct_s_rev", "pct_s_idiovol")
  mteste[, n_disp5 := rowSums(!is.na(.SD)), .SDcols = pcols5]
  mteste[n_disp5 == 5, score_top5 := rowMeans(.SD), .SDcols = pcols5]

  # ---- composto ALL8 (todos os sinais, exige pelo menos 6 de 8 disponiveis) ----
  pcols8 <- paste0("pct_", vars_all)
  mteste[, n_disp8 := rowSums(!is.na(.SD)), .SDcols = pcols8]
  mteste[n_disp8 >= 6, score_all8 := rowMeans(.SD, na.rm = TRUE), .SDcols = pcols8]

  cat(sprintf("Cobertura: top3=%d | top5=%d | all8(>=6 sinais)=%d ticker-meses no teste\n",
              sum(!is.na(mteste$score_top3)), sum(!is.na(mteste$score_top5)), sum(!is.na(mteste$score_all8))))

  for (nm in c("score_top3","score_top5","score_all8")) {
    r <- fama_macbeth_recut(mteste, nm, "retorno", nm, h)
    if (!is.null(r)) resultados[[length(resultados)+1]] <- r
  }
}

R <- rbindlist(resultados, fill = TRUE)
fwrite(R, file.path(OUT, "candidatos_56_composto_ranking_percentil.csv"))
cat("\n\n===== RESUMO GERAL (ordenado por p) =====\n")
print(R[order(p_fm)])
cat("\nOK\n")
