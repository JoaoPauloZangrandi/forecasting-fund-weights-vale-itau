# =============================================================================
# 06_lideres_ponderados_por_skill.R  (exploracao_sinais)
#
# (H) Cohen, Coval & Pastor (2005): a correlacao de trades com OUTROS
#     fundos so' vira sinal de retorno quando ponderada pela SKILL (retorno
#     historico) do fundo "parceiro" -- diferente do candidato #5 (script
#     03), que testou o sinal de TODOS os lideres indiscriminadamente e deu
#     nulo. Aqui, pondera pelo retorno acumulado dos ULTIMOS 12 MESES de
#     cada fundo-lider (so' informacao disponivel ATE o mes t, sem
#     look-ahead), e testa SO os lideres do quintil de MELHOR desempenho
#     recente.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

# ---- retorno acumulado dos ultimos 12 meses de cada fundo, ATE o mes t (sem look-ahead) ----
rf <- fread(file.path(DATA, "retorno_fundo_mensal.csv"))
rf[, cod_fundo := as.character(cod_fundo)]
setorder(rf, cod_fundo, ymk)
rf[, log_ret := log(1 + retorno_fundo)]
rf[, ret_acum_12m := frollsum(log_ret, 12, align = "right"), by = cod_fundo]
rf[, skill_12m := exp(ret_acum_12m) - 1]
rf_skill <- rf[!is.na(skill_12m), .(cod_fundo, ym = ymk, skill_12m)]
cat("Fundo-meses com skill_12m calculavel:", nrow(rf_skill), "\n")

beta_tab <- fread(file.path(DATA, "pares_lideranca_beta.csv"))
lideres <- unique(beta_tab$lider)
cat("Fundos-lider distintos:", length(lideres), "\n")

M <- fread(file.path(DATA, "ajuste_parcial_universo_completo_h1.csv"))
M[, cod_fundo := as.character(cod_fundo)]
M_lideres <- M[cod_fundo %in% lideres]
lam <- 0.0690
M_lideres[, u := dw_corrigido - lam * d]

# junta skill do lider (defasado -- skill medida ATE o mes ym, u tambem medido no mes ym, ambos predeterminados em relacao ao retorno futuro)
M_lideres <- merge(M_lideres, rf_skill, by = c("cod_fundo","ym"), all.x = TRUE)
cat("Obs de lideres com skill_12m disponivel:", M_lideres[!is.na(skill_12m), .N], "de", nrow(M_lideres), "\n")

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
  data.table(sinal = nome, horizonte = h, n_treino = nrow(treino), n_teste = nrow(teste),
             beta_treino = beta, r2_oos_pct = 100*r2_oos, p_full = s_full[2,4])
}
fama_macbeth <- function(m, var_x, var_y, nome, h) {
  treino <- m[ym < CORTE]; teste <- copy(m[ym >= CORTE])
  if (nrow(treino) < 50 || nrow(teste) < 30) return(NULL)
  breaks <- quantile(treino[[var_x]], seq(0,1,0.2), na.rm = TRUE)
  breaks[1] <- -Inf; breaks[6] <- Inf
  teste[, quintil := cut(get(var_x), breaks, labels = FALSE, include.lowest = TRUE)]
  teste <- teste[!is.na(quintil)]
  por_mes <- teste[quintil %in% c(1,5), .(retorno_medio = mean(get(var_y))), by = .(ym, quintil)]
  sm <- dcast(por_mes, ym ~ quintil, value.var = "retorno_medio")
  if (!all(c("1","5") %in% names(sm))) return(NULL)
  setnames(sm, c("1","5"), c("q1","q5"))
  sm <- sm[is.finite(q1) & is.finite(q5)]
  sm[, spread := q5 - q1]
  n_meses <- nrow(sm)
  if (n_meses < 6) return(NULL)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(n_meses)); p_fm <- 2*pt(-abs(t_fm), df = n_meses-1)
  data.table(sinal = nome, horizonte = h, n_meses = n_meses, spread_medio_pp = 100*media, t_fm = t_fm, p_fm = p_fm)
}

resultados_lin <- list(); resultados_fm <- list()

# --- (H1) TODOS os lideres, sinal ponderado pela skill (nao so' filtrado) ---
cat("\n===== (H1) Sinal dos lideres, PONDERADO pela skill (retorno 12m) =====\n")
Mw <- M_lideres[!is.na(skill_12m)]
Mw[, u_ponderado := u * skill_12m]  # lider bom + comprando forte = sinal mais forte
agg_w <- Mw[, .(sinal_skill = mean(u_ponderado, na.rm=TRUE), n_lideres = .N), by = .(ativo, ym)]
agg_w <- agg_w[n_lideres >= 5]
agg_w[, ticker := trimws(sub(".*- ", "", ativo))]
for (h in c(1,3,6)) {
  m <- copy(agg_w); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(sinal_skill)]
  r1 <- teste_oos_linear(m, "sinal_skill", "retorno", "Lideres ponderados por skill", h)
  r2 <- fama_macbeth(m, "sinal_skill", "retorno", "Lideres ponderados por skill", h)
  if (!is.null(r1)) { resultados_lin[[length(resultados_lin)+1]] <- r1; cat(sprintf("h=%d [linear] R2_OOS=%.4f%% p=%.4f\n", h, r1$r2_oos_pct, r1$p_full)) }
  if (!is.null(r2)) { resultados_fm[[length(resultados_fm)+1]] <- r2; cat(sprintf("h=%d [FM] %d meses spread=%.2fpp/mes t=%.2f p=%.4f\n", h, r2$n_meses, r2$spread_medio_pp, r2$t_fm, r2$p_fm)) }
}

# --- (H2) SO' os lideres do TOP quintil de skill (nao pondera, filtra) ---
cat("\n===== (H2) Sinal, SO' lideres do top quintil de skill_12m (por mes) =====\n")
Mf <- M_lideres[!is.na(skill_12m)]
Mf[, skill_rank := frank(-skill_12m) / .N, by = ym]  # rank dentro do mes, 0=melhor
top_skill <- Mf[skill_rank <= 0.2]
cat("Obs de lideres top-quintil skill:", nrow(top_skill), "\n")
agg_top <- top_skill[, .(u_medio_top = mean(u, na.rm=TRUE), n = .N), by = .(ativo, ym)]
agg_top <- agg_top[n >= 3]
agg_top[, ticker := trimws(sub(".*- ", "", ativo))]
for (h in c(1,3,6)) {
  m <- copy(agg_top); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(u_medio_top)]
  r1 <- teste_oos_linear(m, "u_medio_top", "retorno", "Lideres top-quintil skill", h)
  r2 <- fama_macbeth(m, "u_medio_top", "retorno", "Lideres top-quintil skill", h)
  if (!is.null(r1)) { resultados_lin[[length(resultados_lin)+1]] <- r1; cat(sprintf("h=%d [linear] R2_OOS=%.4f%% p=%.4f\n", h, r1$r2_oos_pct, r1$p_full)) }
  if (!is.null(r2)) { resultados_fm[[length(resultados_fm)+1]] <- r2; cat(sprintf("h=%d [FM] %d meses spread=%.2fpp/mes t=%.2f p=%.4f\n", h, r2$n_meses, r2$spread_medio_pp, r2$t_fm, r2$p_fm)) }
}

RL <- rbindlist(resultados_lin); RF <- rbindlist(resultados_fm)
fwrite(RL, file.path(OUT, "candidatos_06_linear.csv"))
fwrite(RF, file.path(OUT, "candidatos_06_fama_macbeth.csv"))
cat("\n\nOK\n")
