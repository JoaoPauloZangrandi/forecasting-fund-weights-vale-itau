# =============================================================================
# 42_composto_3_sinais.R  (exploracao_sinais)
#
# Angulo (a) pedido pelo Joao: combinacao de 3+ sinais via regressao
# multipla / ranking composto. Ingredientes -- 3 sinais de mecanismo
# DIFERENTE, cada um fraco isoladamente mas nao necessariamente
# correlacionado entre si, testados em rodadas anteriores:
#   1) CIO Peer Momentum (peer_ret) -- difusao gradual de informacao via
#      rede de posse institucional em comum (candidatos #11/15/22/34-38,
#      o unico sinal de RETORNO que sobreviveu ate aqui).
#   2) 52-week-high (George-Hwang 2004) -- proximidade da maxima de 12m
#      (candidato #24, sozinho p=0,091, quase-significativo).
#   3) Reversao de curto prazo (Jegadeesh 1990) -- retorno do mes anterior
#      com sinal invertido (candidato #24, sozinho p=0,095, quase-
#      significativo).
#
# Ideia: cada um fica perto de significar sozinho mas nao passa; juntos,
# via regressao multipla (pesos estimados no TREINO, aplicados fixos no
# teste) ou ranking composto simples (soma de z-scores), pode ser que a
# combinacao tenha poder preditivo que nenhum sozinho tem -- pratica
# padrao de combinacao de fatores fracos e pouco correlacionados
# (Asness-Moskowitz-Pedersen 2013 "Value and Momentum Everywhere" e toda
# a literatura de combinacao/ensemble de fatores).
#
# METODOLOGIA (as 3 licoes inegociaveis):
#   - Pesos da combinacao linear estimados SO no treino (ym<202001) --
#     nunca vistos o teste antes de fixar os pesos (sem look-ahead).
#   - Quintil do score composto RE-CORTADO A CADA MES no teste (metodo B).
#   - Fama-MacBeth de verdade na serie de spreads mensais.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/500

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","preco","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
setorder(precos, ticker, ymk)
precos[, preco_max_12m := frollapply(preco, 12, max, align = "right"), by = ticker]
precos[, prox_52w_high := preco / preco_max_12m]
precos[, reversao := -retorno]  # retorno do PROPRIO mes, sinal invertido (mom-1 reversal)

cio <- fread(file.path(OUT, "cio_peer_return.csv"))[, .(ticker, ym, peer_ret)]

base <- merge(cio, precos[, .(ticker, ym = ymk, prox_52w_high, reversao)], by = c("ticker","ym"))
base <- base[is.finite(peer_ret) & is.finite(prox_52w_high) & is.finite(reversao)]
cat("Ticker-mes com os 3 sinais disponiveis:", nrow(base), "\n")

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
  cat(sprintf("%-30s h=%d %2d meses n_min=%3d spread=%+7.2fpp/mes (%+6.1f%%/ano) Sharpe=%5.2f t=%6.2f p=%.5f | Bonf:%s\n",
              nome, h, nmes, n_min, 100*media, 100*((1+media)^12-1), sh, t_fm, p_fm,
              ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
  data.table(sinal = nome, horizonte = h, n_meses = nmes, n_min_grupo_mes = n_min,
             spread_pp = 100*media, sharpe = sh, t_fm = t_fm, p_fm = p_fm, sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
}

resultados <- list()
sinais_individuais <- c("peer_ret", "prox_52w_high", "reversao")

for (h in c(1,3)) {
  m <- copy(base); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos[, .(ticker, ymk, retorno)], by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno)]

  cat(sprintf("\n===== h=%d: sinais individuais (referencia, teste ym>=2020) =====\n", h))
  mteste <- m[ym >= CORTE]
  for (s in sinais_individuais) {
    r <- fama_macbeth_recut(mteste, s, "retorno", paste0("individual|", s), h)
    if (!is.null(r)) resultados[[length(resultados)+1]] <- r
  }

  # ---- regressao multipla FM (padronizada, mes a mes, so' teste) ----
  cat(sprintf("\n===== h=%d: regressao FM multipla (3 sinais juntos, mes a mes) =====\n", h))
  meses <- sort(unique(mteste$ym))
  coefs <- list()
  for (mm in meses) {
    dm <- mteste[ym == mm]
    if (nrow(dm) < 30) next
    dm[, `:=`(peer_z = as.numeric(scale(peer_ret)), high_z = as.numeric(scale(prox_52w_high)),
              rev_z = as.numeric(scale(reversao)))]
    fit <- tryCatch(lm(retorno ~ peer_z + high_z + rev_z, data = dm), error = function(e) NULL)
    if (!is.null(fit)) coefs[[as.character(mm)]] <- data.table(ym = mm, t(coef(fit)))
  }
  CM <- rbindlist(coefs, fill = TRUE)
  cat(sprintf("Meses com regressao valida: %d\n", nrow(CM)))
  for (v in c("peer_z","high_z","rev_z")) {
    x <- CM[[v]]; x <- x[is.finite(x)]; n <- length(x)
    media <- mean(x); dp <- sd(x); t_fm <- media/(dp/sqrt(n)); p_fm <- 2*pt(-abs(t_fm), df = n-1)
    cat(sprintf("  %-10s coef_medio=%+9.5f | t=%6.2f | p=%.4f\n", v, media, t_fm, p_fm))
  }

  # ---- score composto: pesos = coeficientes de UMA regressao no TREINO (ym<2020) ----
  # (media de retorno t+h ~ sinais_z do TREINO -- treino calculado igual ao teste
  #  mas com dados anteriores a 2020, standardizado dentro do proprio periodo de treino)
  mtreino <- m[ym < CORTE]
  mtreino[, `:=`(peer_z = as.numeric(scale(peer_ret)), high_z = as.numeric(scale(prox_52w_high)),
                 rev_z = as.numeric(scale(reversao)))]
  fit_treino <- lm(retorno ~ peer_z + high_z + rev_z, data = mtreino)
  w <- coef(fit_treino)[c("peer_z","high_z","rev_z")]
  cat(sprintf("\nPesos estimados no TREINO (ym<2020): peer=%.4f high=%.4f rev=%.4f\n", w[1], w[2], w[3]))

  # aplica pesos FIXOS no teste; z-score de cada sinal calculado DENTRO do
  # proprio mes de teste (nao vaza informacao futura, so usa a secao
  # transversal do mes corrente, igual seria feito em tempo real)
  mteste[, `:=`(peer_z = as.numeric(scale(peer_ret)), high_z = as.numeric(scale(prox_52w_high)),
                rev_z = as.numeric(scale(reversao))), by = ym]
  mteste[, score_composto := w[1]*peer_z + w[2]*high_z + w[3]*rev_z]
  r <- fama_macbeth_recut(mteste, "score_composto", "retorno", "COMPOSTO(pesos do treino)", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r

  # versao mais simples: soma NAO ponderada dos 3 z-scores (ranking composto puro,
  # sem estimar peso nenhum -- evita qualquer risco de overfit nos pesos)
  mteste[, score_simples := peer_z + high_z + rev_z]
  r2 <- fama_macbeth_recut(mteste, "score_simples", "retorno", "COMPOSTO(soma simples, sem peso)", h)
  if (!is.null(r2)) resultados[[length(resultados)+1]] <- r2
}

R <- rbindlist(resultados, fill = TRUE)
fwrite(R, file.path(OUT, "candidatos_42_composto_3_sinais.csv"))
cat("\n\n===== RESUMO (ordenado por p) =====\n")
print(R[order(p_fm)])
cat("\nOK\n")
