# =============================================================================
# 58_composto_ic_weighted.R  (exploracao_sinais, agente "composto")
#
# ANGULO (c): ranking ponderado por IC (information coefficient) historico
# de cada sinal, calculado SO no treino (ym<2020) -- em vez de pesos de
# regressao multipla (script 57, sensivel a colinearidade entre os 8
# sinais) ou media simples (script 56), usa a correlacao de Spearman
# mes-a-mes de cada sinal ISOLADO com o retorno futuro, medida so' no
# treino, como peso da combinacao no teste. Pratica padrao em gestao
# quantitativa (Grinold-Kahn, "Active Portfolio Management") para combinar
# sinais fracos e pouco correlacionados.
#
# IC_treino[s] = media, ao longo dos meses de TREINO, da correlacao de
# Spearman cross-secional entre sinal_s (alinhado) e retorno(t+h).
# Composto = soma_s( IC_treino[s] * percentil_s(t) ) no periodo de TESTE.
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

painel[, s_peer   := peer_ret]
painel[, s_hhi     := -hhi_posse]
painel[, s_52wh    := prox_52w_high]
painel[, s_rev     := reversao]
painel[, s_idiovol := idio_vol_12m]
painel[, s_breadth := delta_breadth_w]
painel[, s_dem      := dem_pct_w]
painel[, s_efit     := EFIT]
vars8 <- c("s_peer","s_hhi","s_52wh","s_rev","s_idiovol","s_breadth","s_dem","s_efit")

calc_ic_mensal <- function(mdt, var_x) {
  meses <- sort(unique(mdt$ym))
  ics <- c()
  for (mm in meses) {
    dm <- mdt[ym == mm]
    dm <- dm[is.finite(get(var_x)) & is.finite(retorno)]
    if (nrow(dm) < 30) next
    ics <- c(ics, suppressWarnings(cor(dm[[var_x]], dm$retorno, method = "spearman", use = "complete.obs")))
  }
  ics
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

resultados <- list(); ics_todos <- list()

for (h in c(1,3,6)) {
  cat(sprintf("\n\n========================= HORIZONTE h=%d =========================\n", h))
  m <- copy(painel); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno)]
  mtreino <- m[ym < CORTE]; mteste <- m[ym >= CORTE]

  ic_treino <- sapply(vars8, function(v) mean(calc_ic_mensal(mtreino, v), na.rm = TRUE))
  cat("IC medio no TREINO (Spearman, mensal, media dos meses):\n")
  for (v in vars8) cat(sprintf("  %-10s IC_treino=%+7.4f\n", v, ic_treino[v]))
  ics_todos[[length(ics_todos)+1]] <- data.table(horizonte = h, variavel = vars8, ic_treino = as.numeric(ic_treino))

  # percentil de cada sinal, DENTRO do mes de teste
  for (v in vars8) {
    pv <- paste0("pct_", v)
    mteste[is.finite(get(v)), (pv) := frank(get(v))/.N, by = ym]
  }
  pcols <- paste0("pct_", vars8)
  mteste[, n_disp := rowSums(!is.na(.SD)), .SDcols = pcols]

  # score IC-weighted: soma dos percentis ponderados pelo IC de treino (sinal do IC ja
  # determina a direcao -- nao precisa alinhar manualmente, o IC negativo automaticamente
  # inverte a contribuicao do sinal)
  mteste[n_disp >= 6, score_ic_weighted := {
    ws <- ic_treino[vars8]
    vals <- as.matrix(.SD[, pcols, with = FALSE])
    vals[is.na(vals)] <- 0.5  # neutro quando faltando
    as.numeric(vals %*% ws) / sum(abs(ws))
  }]

  # versao so' com os sinais cujo |IC_treino| > mediana (reduz ruido de sinais fracos)
  ic_mediana <- median(abs(ic_treino))
  vars_fortes <- names(ic_treino)[abs(ic_treino) > ic_mediana]
  cat(sprintf("Sinais 'fortes' no treino (|IC|>mediana=%.4f): %s\n", ic_mediana, paste(vars_fortes, collapse=", ")))
  pcols_f <- paste0("pct_", vars_fortes)
  mteste[, n_disp_f := rowSums(!is.na(.SD)), .SDcols = pcols_f]
  mteste[n_disp_f == length(vars_fortes), score_ic_weighted_fortes := {
    ws <- ic_treino[vars_fortes]
    vals <- as.matrix(.SD[, pcols_f, with = FALSE])
    as.numeric(vals %*% ws) / sum(abs(ws))
  }]

  r1 <- fama_macbeth_recut(mteste, "score_ic_weighted", "retorno", "COMPOSTO(IC-weighted,all8)", h)
  r2 <- fama_macbeth_recut(mteste, "score_ic_weighted_fortes", "retorno", "COMPOSTO(IC-weighted,so fortes)", h)
  if (!is.null(r1)) resultados[[length(resultados)+1]] <- r1
  if (!is.null(r2)) resultados[[length(resultados)+1]] <- r2
}

R <- rbindlist(resultados, fill = TRUE)
ICS <- rbindlist(ics_todos)
fwrite(R, file.path(OUT, "candidatos_58_ic_weighted.csv"))
fwrite(ICS, file.path(OUT, "candidatos_58_ic_por_sinal.csv"))
cat("\n\n===== RESUMO GERAL (ordenado por p) =====\n")
print(R[order(p_fm)])
cat("\nOK\n")
