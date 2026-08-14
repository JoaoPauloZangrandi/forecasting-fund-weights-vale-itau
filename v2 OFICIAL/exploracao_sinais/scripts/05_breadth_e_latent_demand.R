# =============================================================================
# 05_breadth_e_latent_demand.R  (exploracao_sinais)
#
# 2 candidatos do Grupo A/B da pesquisa de literatura, ambos com teste OOS
# (regressao linear) E Fama-MacBeth (long-short por quintil, 1 obs/mes)
# desde o inicio -- ja aprendemos a licao do script 03/04.
#
# (F) BREADTH OF OWNERSHIP (Chen, Hong & Stein 2002, JFE): numero de fundos
#     distintos com posicao > 0 no ativo (margem extensiva, nao ponderada
#     por tamanho -- diferente de tudo que ja foi testado, que pesava por
#     R$). Teoria: queda de breadth sinaliza restricao de venda a
#     descoberto mais apertada -> preco caro -> retorno futuro baixo.
#     Sinal = variacao de breadth (contagem de fundos) mes a mes.
#
# (G) LATENT DEMAND (Koijen & Yogo 2019): em vez de converter o residuo do
#     modelo de peso esperado (Etapa 1) em "fluxo do robo" e so' DEPOIS
#     testar contra retorno (o que ja foi feito e rejeitado), usa o
#     residuo agregado por ativo-mes DIRETO como preditor -- ponderado por
#     AUM de cada fundo, media do erro (peso_real - peso_esperado) entre
#     todos os fundos que carregam o ativo naquele mes.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

# ---- funcoes de teste reutilizaveis (regressao OOS + Fama-MacBeth) --------
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
  data.table(sinal = nome, horizonte = h, tipo = "linear_oos", n_treino = nrow(treino), n_teste = nrow(teste),
             beta_treino = beta, r2_oos_pct = 100*r2_oos, coef_full = s_full[2,1], t_full = s_full[2,3], p_full = s_full[2,4])
}

fama_macbeth <- function(m, var_x, var_y, nome, h) {
  treino <- m[ym < CORTE]; teste <- copy(m[ym >= CORTE])
  if (nrow(treino) < 100 || nrow(teste) < 50) return(NULL)
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
  data.table(sinal = nome, horizonte = h, tipo = "fama_macbeth", n_meses = n_meses,
             spread_medio_pp = 100*media, t_fm = t_fm, p_fm = p_fm, pct_meses_positivo = 100*mean(sm$spread>0))
}

resultados_lin <- list(); resultados_fm <- list()

# =============================================================================
# (F) BREADTH OF OWNERSHIP
# =============================================================================
cat("===== (F) BREADTH OF OWNERSHIP (Chen-Hong-Stein 2002) =====\n")
pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso"))
breadth <- pp[peso > 0, .(breadth = uniqueN(cod_fundo)), by = .(ativo, ym)]
br_prev <- breadth[, .(ativo, ym = addm(ym, 1), breadth_prev = breadth)]
br <- merge(breadth, br_prev, by = c("ativo","ym"))
br[, delta_breadth_pct := (breadth - breadth_prev) / breadth_prev]
q <- quantile(br$delta_breadth_pct, c(0.01,0.99), na.rm=TRUE)
br[, delta_breadth_w := pmin(pmax(delta_breadth_pct, q[1]), q[2])]
br[, ticker := trimws(sub(".*- ", "", ativo))]
cat("Ativo-mes com breadth calculavel:", nrow(br), "| breadth media:", round(mean(br$breadth),1), "\n")

for (h in c(1,3,6,12)) {
  m <- copy(br); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(delta_breadth_w)]
  r1 <- teste_oos_linear(m, "delta_breadth_w", "retorno", "Breadth of ownership", h)
  r2 <- fama_macbeth(m, "delta_breadth_w", "retorno", "Breadth of ownership", h)
  if (!is.null(r1)) { resultados_lin[[length(resultados_lin)+1]] <- r1
    cat(sprintf("Breadth h=%2d [linear] beta=%.5f R2_OOS=%.4f%% p_full=%.4f\n", h, r1$beta_treino, r1$r2_oos_pct, r1$p_full)) }
  if (!is.null(r2)) { resultados_fm[[length(resultados_fm)+1]] <- r2
    cat(sprintf("Breadth h=%2d [Fama-MacBeth] %d meses, spread=%.2fpp/mes, t=%.2f, p=%.4f\n", h, r2$n_meses, r2$spread_medio_pp, r2$t_fm, r2$p_fm)) }
}

# =============================================================================
# (G) LATENT DEMAND (residuo do modelo de peso esperado, agregado por ativo-mes)
# =============================================================================
cat("\n===== (G) LATENT DEMAND (residuo agregado, ponderado por AUM) =====\n")
E <- fread(file.path(DATA, "erro_e_multiativo.csv"))
feat <- fread(file.path(DATA, "features_predeterminado_completo.csv"), select = c("cod_fundo","ym","aum_prev"))
feat[, cod_fundo := as.character(cod_fundo)]
E[, cod_fundo := as.character(cod_fundo)]
E2 <- merge(E, feat, by = c("cod_fundo","ym"))
E2 <- E2[is.finite(aum_prev) & aum_prev > 0]

# demanda latente agregada = media do erro ponderada por AUM (fundos maiores pesam mais)
lat <- E2[, .(latent_demand = weighted.mean(erro, aum_prev), n_fundos = .N), by = .(ativo, ym)]
lat <- lat[n_fundos >= 10]
q2 <- quantile(lat$latent_demand, c(0.01,0.99), na.rm=TRUE)
lat[, latent_demand_w := pmin(pmax(latent_demand, q2[1]), q2[2])]
lat[, ticker := trimws(sub(".*- ", "", ativo))]
cat("Ativo-mes com latent demand calculavel:", nrow(lat), "\n")

for (h in c(1,3,6,12)) {
  m <- copy(lat); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(latent_demand_w)]
  r1 <- teste_oos_linear(m, "latent_demand_w", "retorno", "Latent demand (nivel)", h)
  r2 <- fama_macbeth(m, "latent_demand_w", "retorno", "Latent demand (nivel)", h)
  if (!is.null(r1)) { resultados_lin[[length(resultados_lin)+1]] <- r1
    cat(sprintf("Latent demand (nivel) h=%2d [linear] beta=%.5f R2_OOS=%.4f%% p_full=%.4f\n", h, r1$beta_treino, r1$r2_oos_pct, r1$p_full)) }
  if (!is.null(r2)) { resultados_fm[[length(resultados_fm)+1]] <- r2
    cat(sprintf("Latent demand (nivel) h=%2d [Fama-MacBeth] %d meses, spread=%.2fpp/mes, t=%.2f, p=%.4f\n", h, r2$n_meses, r2$spread_medio_pp, r2$t_fm, r2$p_fm)) }
}

# tambem testar a VARIACAO da latent demand (choque), nao so o nivel -- Koijen-Yogo enfatiza persistencia + reversao de CHOQUES
lat_prev <- lat[, .(ativo, ym = addm(ym,1), latent_demand_prev = latent_demand)]
lat_chg <- merge(lat, lat_prev, by = c("ativo","ym"))
lat_chg[, delta_latent := latent_demand - latent_demand_prev]
q3 <- quantile(lat_chg$delta_latent, c(0.01,0.99), na.rm=TRUE)
lat_chg[, delta_latent_w := pmin(pmax(delta_latent, q3[1]), q3[2])]

for (h in c(1,3,6,12)) {
  m <- copy(lat_chg); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(delta_latent_w)]
  r1 <- teste_oos_linear(m, "delta_latent_w", "retorno", "Latent demand (choque)", h)
  r2 <- fama_macbeth(m, "delta_latent_w", "retorno", "Latent demand (choque)", h)
  if (!is.null(r1)) { resultados_lin[[length(resultados_lin)+1]] <- r1
    cat(sprintf("Latent demand (choque) h=%2d [linear] beta=%.5f R2_OOS=%.4f%% p_full=%.4f\n", h, r1$beta_treino, r1$r2_oos_pct, r1$p_full)) }
  if (!is.null(r2)) { resultados_fm[[length(resultados_fm)+1]] <- r2
    cat(sprintf("Latent demand (choque) h=%2d [Fama-MacBeth] %d meses, spread=%.2fpp/mes, t=%.2f, p=%.4f\n", h, r2$n_meses, r2$spread_medio_pp, r2$t_fm, r2$p_fm)) }
}

RL <- rbindlist(resultados_lin); RF <- rbindlist(resultados_fm)
fwrite(RL, file.path(OUT, "candidatos_05_linear.csv"))
fwrite(RF, file.path(OUT, "candidatos_05_fama_macbeth.csv"))
cat("\n\n===== RESUMO LINEAR =====\n"); print(RL[, .(sinal, horizonte, r2_oos_pct = round(r2_oos_pct,3), p_full = round(p_full,4))])
cat("\n===== RESUMO FAMA-MACBETH =====\n"); print(RF[, .(sinal, horizonte, spread_medio_pp = round(spread_medio_pp,3), t_fm = round(t_fm,2), p_fm = round(p_fm,4))])
cat("\nOK\n")
