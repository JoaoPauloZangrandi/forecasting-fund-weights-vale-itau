# =============================================================================
# 07_subamostra_small_caps.R  (exploracao_sinais)
#
# (I) A literatura (Wermers 1999, Chen-Hong-Stein 2002, e outros) documenta
#     efeitos de herding/breadth/pressao MAIS FORTES em small caps -- menos
#     liquidez, menos analistas cobrindo, restricao de short-sell mais
#     apertada. Testa os 3 candidatos com maior "quase-sinal" ate agora
#     (FIT h=1/h=3, demanda agregada h=6, lideres skill h=1) restritos ao
#     TERCO de ativos com MENOR valor_total_mil medio (proxy de tamanho/
#     liquidez, ja que nao temos market cap direto).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("ativo","ym","valor_mil"))
tamanho_medio <- pp[, .(valor_total_medio = mean(sum(valor_mil), na.rm=TRUE)), by = .(ativo, ym)]
tamanho_ativo <- pp[, .(valor_total_medio = sum(valor_mil)), by = .(ativo)]  # proxy de tamanho: total ao longo de toda a amostra
corte_small <- quantile(tamanho_ativo$valor_total_medio, 1/3)
small_caps <- tamanho_ativo[valor_total_medio <= corte_small]$ativo
cat("Ativos classificados como 'small cap' (terco menor por valor total mantido):", length(small_caps), "de", nrow(tamanho_ativo), "\n")

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
  cat(sprintf("%-30s h=%2d [linear] n_treino=%5d n_teste=%4d beta=%9.5f R2_OOS=%8.4f%% p_full=%.4f\n",
              nome, h, nrow(treino), nrow(teste), beta, 100*r2_oos, s_full[2,4]))
  data.table(sinal = nome, horizonte = h, n_treino = nrow(treino), n_teste = nrow(teste), r2_oos_pct = 100*r2_oos, p_full = s_full[2,4])
}
fama_macbeth <- function(m, var_x, var_y, nome, h) {
  treino <- m[ym < CORTE]; teste <- copy(m[ym >= CORTE])
  if (nrow(treino) < 50 || nrow(teste) < 30) return(NULL)
  breaks <- unique(quantile(treino[[var_x]], seq(0,1,0.2), na.rm = TRUE))
  if (length(breaks) < 4) return(NULL)  # sinal degenerado (poucos valores distintos), nao da pra formar quintis
  breaks[1] <- -Inf; breaks[length(breaks)] <- Inf
  teste[, quintil := as.integer(cut(get(var_x), breaks, labels = FALSE, include.lowest = TRUE))]
  n_grupos <- length(breaks) - 1
  if (n_grupos < 5) teste[, quintil := ifelse(quintil == 1, 1, ifelse(quintil == n_grupos, 5, quintil))]  # aproxima extremos se sinal tiver poucos valores unicos
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
  cat(sprintf("%-30s h=%2d [FM] %2d meses spread=%+7.2fpp/mes t=%6.2f p=%.4f\n", nome, h, n_meses, 100*media, t_fm, p_fm))
  data.table(sinal = nome, horizonte = h, n_meses = n_meses, spread_medio_pp = 100*media, t_fm = t_fm, p_fm = p_fm)
}

resultados_lin <- list(); resultados_fm <- list()

# FIT, restrito a small caps
cat("\n===== FIT (Lou), SO SMALL CAPS =====\n")
fit_completo <- fread(file.path(DATA, "fit_ativo_mes.csv"))
fit_sc <- fit_completo[ativo %in% small_caps]
fit_sc[, ticker := trimws(sub(".*- ", "", ativo))]
cat("Obs FIT em small caps:", nrow(fit_sc), "de", nrow(fit_completo), "\n")
for (h in c(0,1,3)) {
  m <- copy(fit_sc); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(FIT)]
  r1 <- teste_oos_linear(m, "FIT", "retorno", "FIT small-cap", h)
  r2 <- fama_macbeth(m, "FIT", "retorno", "FIT small-cap", h)
  if (!is.null(r1)) resultados_lin[[length(resultados_lin)+1]] <- r1
  if (!is.null(r2)) resultados_fm[[length(resultados_fm)+1]] <- r2
}

# Demanda agregada revelada, restrito a small caps
cat("\n===== DEMANDA AGREGADA REVELADA, SO SMALL CAPS =====\n")
valor_total <- pp[, .(valor_total_mil = sum(valor_mil)), by = .(ativo, ym)]
vt_prev <- valor_total[, .(ativo, ym = addm(ym, 1), valor_total_prev = valor_total_mil)]
dem <- merge(valor_total, vt_prev, by = c("ativo","ym"))
dem <- dem[valor_total_prev > 0 & ativo %in% small_caps]
dem[, dem_pct := (valor_total_mil - valor_total_prev) / valor_total_prev]
qd <- quantile(dem$dem_pct, c(0.01,0.99), na.rm=TRUE)
dem[, dem_pct_w := pmin(pmax(dem_pct, qd[1]), qd[2])]
dem[, ticker := trimws(sub(".*- ", "", ativo))]
for (h in c(1,3,6)) {
  m <- copy(dem); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(dem_pct_w)]
  r1 <- teste_oos_linear(m, "dem_pct_w", "retorno", "Demanda revelada small-cap", h)
  r2 <- fama_macbeth(m, "dem_pct_w", "retorno", "Demanda revelada small-cap", h)
  if (!is.null(r1)) resultados_lin[[length(resultados_lin)+1]] <- r1
  if (!is.null(r2)) resultados_fm[[length(resultados_fm)+1]] <- r2
}

# Herding, restrito a small caps
cat("\n===== HERDING (LSV), SO SMALL CAPS =====\n")
pp2 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso"))
peso_prev2b <- pp2[, .(cod_fundo, ativo, ym = addm(ym, 1), peso_prev = peso)]
d2 <- merge(pp2, peso_prev2b, by = c("cod_fundo","ativo","ym"))
d2 <- d2[is.finite(peso_prev) & ativo %in% small_caps]
d2[, compra := peso > peso_prev]; d2[, venda := peso < peso_prev]
por_am <- d2[, .(n_fundos=.N, n_compra=sum(compra), n_venda=sum(venda)), by=.(ativo,ym)]
por_am <- por_am[n_fundos >= 5]  # menor exigencia, small caps tem menos fundos
media_mes2 <- por_am[, .(p_compra_medio_mes = mean(n_compra/(n_compra+n_venda), na.rm=TRUE)), by=ym]
por_am[, p_compra := n_compra/(n_compra+n_venda)]
por_am <- merge(por_am, media_mes2, by="ym")
por_am[, HM_sinal := p_compra - p_compra_medio_mes]
por_am[, ticker := trimws(sub(".*- ", "", ativo))]
for (h in c(1,3,6)) {
  m <- copy(por_am); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(HM_sinal)]
  r1 <- teste_oos_linear(m, "HM_sinal", "retorno", "Herding small-cap", h)
  r2 <- fama_macbeth(m, "HM_sinal", "retorno", "Herding small-cap", h)
  if (!is.null(r1)) resultados_lin[[length(resultados_lin)+1]] <- r1
  if (!is.null(r2)) resultados_fm[[length(resultados_fm)+1]] <- r2
}

RL <- rbindlist(resultados_lin); RF <- rbindlist(resultados_fm)
fwrite(RL, file.path(OUT, "candidatos_07_linear.csv"))
fwrite(RF, file.path(OUT, "candidatos_07_fama_macbeth.csv"))
cat("\nOK\n")
