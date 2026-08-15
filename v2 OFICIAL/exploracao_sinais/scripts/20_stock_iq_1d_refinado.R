# =============================================================================
# 20_stock_iq_1d_refinado.R  (exploracao_sinais)
#
# Stock IQ (candidato #19) com segmentos de 1 dimensao (so tamanho, ou so
# momentum) em vez de 2D (tamanho x momentum, 9 grupos, mediana 32 acoes/
# grupo) -- mais acoes por segmento, mais poder estatistico, ao custo de
# isolar menos precisamente o "tilt de estilo".
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp <- pp[is.finite(aum_prev) & aum_prev > 0 & peso > 0]
pp[, ticker := trimws(sub(".*- ", "", ativo))]
pp <- pp[ticker %in% unique(precos$ticker)]

tamanho <- pp[, .(valor_total_mil = sum(peso*aum_prev)), by = .(ticker, ym)]
tamanho[, tam_quintil := as.integer(cut(valor_total_mil, quantile(valor_total_mil, seq(0,1,0.2)),
                                         labels = FALSE, include.lowest = TRUE)), by = ym]

mom <- copy(precos); setorder(mom, ticker, ymk)
mom[, log_ret := log(1+retorno)]
mom[, mom_12m := exp(frollsum(log_ret, 12, align="right")) - 1, by = ticker]
mom <- mom[!is.na(mom_12m), .(ticker, ym = ymk, mom_12m)]
mom[, mom_quintil := as.integer(cut(mom_12m, quantile(mom_12m, seq(0,1,0.2), na.rm=TRUE),
                                     labels = FALSE, include.lowest = TRUE)), by = ym]

rf <- fread(file.path(DATA, "retorno_fundo_mensal.csv"))
rf[, cod_fundo := as.character(cod_fundo)]
setorder(rf, cod_fundo, ymk)
rf[, log_ret := log(1 + retorno_fundo)]
rf[, skill_12m := exp(frollsum(log_ret, 12, align = "right")) - 1]
rf_skill <- rf[!is.na(skill_12m), .(cod_fundo, ym = ymk, skill_12m)]
pp[, cod_fundo := as.character(cod_fundo)]

teste_oos_linear <- function(m, var_x, var_y, nome, h) {
  treino <- m[ym < CORTE]; teste <- m[ym >= CORTE]
  if (nrow(treino) < 50 || nrow(teste) < 30) return(NULL)
  form <- as.formula(paste(var_y, "~", var_x))
  fit_lm <- lm(form, data = treino)
  pred <- predict(fit_lm, newdata = teste)
  sse_m <- sum((teste[[var_y]] - pred)^2); sse_n <- sum((teste[[var_y]] - mean(treino[[var_y]]))^2)
  r2_oos <- 1 - sse_m/sse_n
  s_full <- summary(lm(form, data = m))$coefficients
  cat(sprintf("%-20s h=%2d [linear] R2_OOS=%8.4f%% p_full=%.4f\n", nome, h, 100*r2_oos, s_full[2,4]))
  data.table(sinal=nome, horizonte=h, r2_oos_pct=100*r2_oos, p_full=s_full[2,4])
}
fama_macbeth <- function(m, var_x, var_y, nome, h) {
  treino <- m[ym < CORTE]; teste <- copy(m[ym >= CORTE])
  if (nrow(treino) < 50 || nrow(teste) < 30) return(NULL)
  breaks <- unique(quantile(treino[[var_x]], seq(0,1,0.2), na.rm = TRUE))
  if (length(breaks) < 4) return(NULL)
  breaks[1] <- -Inf; breaks[length(breaks)] <- Inf
  teste[, quintil := as.integer(cut(get(var_x), breaks, labels = FALSE, include.lowest = TRUE))]
  n_grupos <- length(breaks) - 1
  if (n_grupos < 5) teste[, quintil := ifelse(quintil == 1, 1, ifelse(quintil == n_grupos, 5, quintil))]
  teste <- teste[!is.na(quintil)]
  n_por_mes <- teste[quintil %in% c(1,5), .N, by = .(ym, quintil)]
  cat(sprintf("   [N por quintil-mes] mediana=%.0f\n", median(n_por_mes$N)))
  por_mes <- teste[quintil %in% c(1,5), .(retorno_medio = mean(get(var_y))), by = .(ym, quintil)]
  sm <- dcast(por_mes, ym ~ quintil, value.var = "retorno_medio")
  if (!all(c("1","5") %in% names(sm))) return(NULL)
  setnames(sm, c("1","5"), c("q1","q5")); sm <- sm[is.finite(q1) & is.finite(q5)]
  sm[, spread := q5-q1]; n_meses <- nrow(sm)
  if (n_meses < 6) return(NULL)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(n_meses)); p_fm <- 2*pt(-abs(t_fm), df=n_meses-1)
  cat(sprintf("%-20s h=%2d [FM] %2d meses spread=%+7.2fpp/mes t=%6.2f p=%.4f\n", nome, h, n_meses, 100*media, t_fm, p_fm))
  data.table(sinal=nome, horizonte=h, n_meses=n_meses, spread_pp=100*media, t_fm=t_fm, p_fm=p_fm)
}

calcula_stock_iq <- function(seg_dt, nome_seg) {
  d <- merge(pp[, .(cod_fundo, ticker, ym, peso, aum_prev)], seg_dt, by = c("ticker","ym"))
  d <- merge(d, rf_skill, by = c("cod_fundo","ym"))
  d[, peso_soma_segmento := sum(peso), by = .(cod_fundo, ym, segmento)]
  d[, n_segmento := .N, by = .(cod_fundo, ym, segmento)]
  d <- d[n_segmento >= 2]
  d[, peso_medio_resto_segmento := (peso_soma_segmento - peso) / (n_segmento - 1)]
  d[, desvio := peso - peso_medio_resto_segmento]
  d[, contrib := aum_prev * desvio * skill_12m]
  siq <- d[, .(stock_iq = sum(contrib)/sum(aum_prev), n_fundos=.N), by = .(ticker, ym)]
  siq <- siq[n_fundos >= 10]
  cat(sprintf("\n[%s] ativo-mes calculavel: %d\n", nome_seg, nrow(siq)))
  siq
}

resultados_lin <- list(); resultados_fm <- list()

for (config in list(list(nome="tamanho_1D", seg=tamanho[, .(ticker, ym, segmento=tam_quintil)]),
                     list(nome="momentum_1D", seg=mom[, .(ticker, ym, segmento=mom_quintil)]))) {
  siq <- calcula_stock_iq(config$seg, config$nome)
  for (h in c(1,3)) {
    m <- copy(siq); m[, ym_ret := addm(ym, h)]
    m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
    m <- m[is.finite(retorno) & is.finite(stock_iq)]
    r1 <- teste_oos_linear(m, "stock_iq", "retorno", paste0("StockIQ-",config$nome), h)
    r2 <- fama_macbeth(m, "stock_iq", "retorno", paste0("StockIQ-",config$nome), h)
    if (!is.null(r1)) resultados_lin[[length(resultados_lin)+1]] <- r1
    if (!is.null(r2)) resultados_fm[[length(resultados_fm)+1]] <- r2
  }
}

RL <- rbindlist(resultados_lin); RF <- rbindlist(resultados_fm)
fwrite(RL, file.path(OUT, "candidatos_20_linear.csv"))
fwrite(RF, file.path(OUT, "candidatos_20_fama_macbeth.csv"))
cat("\nOK\n")
