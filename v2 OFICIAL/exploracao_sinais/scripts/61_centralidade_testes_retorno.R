# =============================================================================
# 61_centralidade_testes_retorno.R  (exploracao_sinais / agente_rede)
#
# Testa se as medidas de centralidade construidas no script 60
# (deg_avg = grau ponderado medio; eigen_cent = autovetor de centralidade;
# deg_avg_delta / eigen_cent_delta = variacao mes-a-mes) preveem retorno
# FUTURO da propria acao, em h = 1, 3, 6, 12 meses.
#
# DISCIPLINA METODOLOGICA (licoes ja documentadas no LOG_CANDIDATOS.md
# principal, candidatos #1 e #14/Comomentum):
#   1) Fora-da-amostra sempre: treino ym<202001, teste ym>=202001.
#   2) Fama-MacBeth de VERDADE: 1 spread Q5-Q1 POR MES (nao por ativo-mes),
#      testando a media da serie temporal de spreads com o erro-padrao da
#      propria serie. Quintis RE-CORTADOS A CADA MES (metodo B / corrigido,
#      script 36-37), nunca breaks fixos do treino aplicados no teste --
#      isso e' o que gerou (e foi corrigido) o falso-positivo quase-repetido
#      do CIO Peer Momentum original.
#   3) Reporta SEMPRE o N minimo por grupo-mes.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
painel <- fread(file.path(OUT, "centralidade_panel.csv"))

# -----------------------------------------------------------------------------
# Funcoes de teste (mesmo padrao usado em toda a exploracao, ex. scripts 09/11,
# mas com Fama-MacBeth usando RE-CORTE MENSAL, metodo corrigido do script 37)
# -----------------------------------------------------------------------------
teste_oos_linear <- function(m, var_x, var_y, nome, h) {
  treino <- m[ym < CORTE]; teste <- m[ym >= CORTE]
  if (nrow(treino) < 50 || nrow(teste) < 30) return(NULL)
  form <- as.formula(paste(var_y, "~", var_x))
  fit_lm <- lm(form, data = treino)
  alpha <- coef(fit_lm)[1]; beta <- coef(fit_lm)[2]
  pred <- alpha + beta * teste[[var_x]]
  sse_m <- sum((teste[[var_y]] - pred)^2); sse_n <- sum((teste[[var_y]] - mean(treino[[var_y]]))^2)
  r2_oos <- 1 - sse_m/sse_n
  s_full <- summary(lm(form, data = m))$coefficients
  cat(sprintf("%-30s h=%2d [linear] n_tr=%5d n_te=%4d beta=%9.5f R2_OOS=%8.4f%% p_full=%.4f\n",
              nome, h, nrow(treino), nrow(teste), beta, 100*r2_oos, s_full[2,4]))
  data.table(sinal = nome, horizonte = h, r2_oos_pct = 100*r2_oos, p_full = s_full[2,4])
}

fama_macbeth_recorte_mensal <- function(m, var_x, var_y, nome, h) {
  teste <- copy(m[ym >= CORTE])
  teste <- teste[is.finite(get(var_x)) & is.finite(get(var_y))]
  if (nrow(teste) < 30) return(NULL)
  # RE-CORTE de quintil A CADA MES (metodo corrigido -- script 36/37), nao breaks fixos do treino
  teste[, quintil := {
    qs <- quantile(get(var_x), 0:5/5, na.rm = TRUE, type = 7)
    qs <- unique(qs)
    if (length(qs) < 3) rep(NA_integer_, .N)
    else as.integer(cut(get(var_x), qs, include.lowest = TRUE, labels = FALSE))
  }, by = ym]
  n_grupos <- teste[, uniqueN(quintil), by = ym][, max(V1, na.rm = TRUE)]
  teste <- teste[!is.na(quintil)]
  teste[, extremo := ifelse(quintil == min(quintil), "Q1", ifelse(quintil == max(quintil), "Q5", NA_character_)), by = ym]
  n_por_mes <- teste[!is.na(extremo), .N, by = .(ym, extremo)]
  if (nrow(n_por_mes) == 0) return(NULL)
  por_mes <- teste[!is.na(extremo), .(retorno_medio = mean(get(var_y)), n = .N), by = .(ym, extremo)]
  sm <- dcast(por_mes, ym ~ extremo, value.var = "retorno_medio")
  if (!all(c("Q1","Q5") %in% names(sm))) return(NULL)
  sm <- sm[is.finite(Q1) & is.finite(Q5)]
  sm[, spread := Q5 - Q1]
  n_meses <- nrow(sm)
  if (n_meses < 6) return(NULL)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(n_meses)); p_fm <- 2*pt(-abs(t_fm), df = n_meses-1)
  n_min <- min(n_por_mes$N); n_mediana <- median(n_por_mes$N)
  pct_positivo <- mean(sm$spread > 0)
  cat(sprintf("%-30s h=%2d [FM re-corte mensal] %2d meses spread=%+7.3fpp/mes (anualizado~%+6.1f%%) t=%6.2f p=%.4f | N_grupo-mes: min=%d mediana=%.0f | %%meses positivo=%.0f%%\n",
              nome, h, n_meses, 100*media, 100*((1+media)^12-1), t_fm, p_fm, n_min, n_mediana, 100*pct_positivo))
  data.table(sinal = nome, horizonte = h, n_meses = n_meses, spread_medio_pp = 100*media,
             t_fm = t_fm, p_fm = p_fm, n_min_grupo_mes = n_min, n_mediana_grupo_mes = n_mediana,
             pct_meses_positivo = pct_positivo)
}

# -----------------------------------------------------------------------------
# Monta base: sinal em t (centralidade) x retorno em t+h
# -----------------------------------------------------------------------------
sinais <- c("deg_avg", "eigen_cent", "deg_avg_delta", "eigen_cent_delta")
nomes  <- c("Centralidade: grau ponderado (nivel)",
            "Centralidade: autovetor (nivel)",
            "Centralidade: grau ponderado (delta m/m)",
            "Centralidade: autovetor (delta m/m)")

resultados_lin <- list(); resultados_fm <- list()

for (h in c(1,3,6,12)) {
  m <- copy(painel); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  for (k in seq_along(sinais)) {
    var_x <- sinais[k]; nome <- nomes[k]
    mm <- m[is.finite(retorno) & is.finite(get(var_x))]
    r1 <- teste_oos_linear(mm, var_x, "retorno", nome, h)
    r2 <- fama_macbeth_recorte_mensal(mm, var_x, "retorno", nome, h)
    if (!is.null(r1)) resultados_lin[[length(resultados_lin)+1]] <- r1
    if (!is.null(r2)) resultados_fm[[length(resultados_fm)+1]] <- r2
  }
}

RL <- rbindlist(resultados_lin); RF <- rbindlist(resultados_fm)
fwrite(RL, file.path(OUT, "candidatos_61_linear.csv"))
fwrite(RF, file.path(OUT, "candidatos_61_fama_macbeth.csv"))

cat("\n===== Resumo ordenado por p_fm (mais promissores primeiro) =====\n")
print(RF[order(p_fm)])
cat("\nOK\n")
