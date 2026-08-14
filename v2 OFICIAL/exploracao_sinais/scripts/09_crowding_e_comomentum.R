# =============================================================================
# 09_crowding_e_comomentum.R  (exploracao_sinais)
#
# (L) CROWDING (Chincarini, Lazo-Paz & Moneta 2026): sem dado de ADV/volume
#     diario (nao temos liquidez de mercado, so' o que os FUNDOS fazem),
#     uso uma proxy grosseira: HHI de posse entre os fundos que carregam o
#     ativo (quanto mais concentrado em poucos fundos grandes, mais dificil
#     de "sair" sem mover preco -- mesma logica de Days-to-ADV, sem o dado
#     de volume). Testa como preditor padrao E especificamente durante o
#     unico episodio de estresse do periodo de teste (fev-abr/2020, COVID)
#     -- e' onde a literatura preve que o efeito de crowding aparece.
#
# (M) COMOMENTUM (Lou & Polk): o candidato com a evidencia mais forte da
#     pesquisa (~9%/ano, identificacao quase-causal), ainda nao testado.
#     Adaptado pra dado mensal (a literatura usa semanal, entao isso e' uma
#     versao com MENOS poder estatistico que o ideal -- registrar essa
#     limitacao). Dentro do decil de MAIOR momentum (12m), mede a
#     correlacao media par-a-par do retorno residual (ajustado por
#     mercado) nos ultimos 12 meses -- ativos "comovendo" mais dentro do
#     grupo de momentum sao candidatos a reversao mais forte.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

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

# =============================================================================
# (L) CROWDING (proxy: HHI de posse entre fundos)
# =============================================================================
cat("===== (L) CROWDING (proxy HHI de posse, sem dado de ADV) =====\n")
pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp <- pp[is.finite(aum_prev) & aum_prev > 0]
pp[, valor_posicao := peso * aum_prev]
crowd <- pp[, .(hhi_posse = sum((valor_posicao/sum(valor_posicao))^2), n_fundos = .N, valor_total = sum(valor_posicao)),
            by = .(ativo, ym)]
crowd <- crowd[n_fundos >= 10]
crowd[, ticker := trimws(sub(".*- ", "", ativo))]
cat("Ativo-mes com crowding calculavel:", nrow(crowd), "\n")

for (h in c(1,3,6)) {
  m <- copy(crowd); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(hhi_posse)]
  r1 <- teste_oos_linear(m, "hhi_posse", "retorno", "Crowding (HHI posse)", h)
  r2 <- fama_macbeth(m, "hhi_posse", "retorno", "Crowding (HHI posse)", h)
  if (!is.null(r1)) resultados_lin[[length(resultados_lin)+1]] <- r1
  if (!is.null(r2)) resultados_fm[[length(resultados_fm)+1]] <- r2
}

# checagem especifica: crowding alto em jan/fev-2020 prediz queda MAIOR no crash (mar-abr/2020)?
cat("\n----- Checagem especifica: crowding pre-COVID prediz queda no crash? -----\n")
crowd_precrash <- crowd[ym == 202001]
ret_crash <- precos[ymk %in% c(202002,202003,202004)]
ret_crash_acum <- ret_crash[, .(ret_crash = prod(1+retorno)-1), by = ticker]
mc <- merge(crowd_precrash, ret_crash_acum, by = "ticker")
mc <- mc[is.finite(ret_crash) & is.finite(hhi_posse)]
if (nrow(mc) >= 10) {
  cor_crash <- cor(mc$hhi_posse, mc$ret_crash)
  fit_crash <- lm(ret_crash ~ hhi_posse, data = mc)
  s <- summary(fit_crash)$coefficients
  cat(sprintf("Crowding (jan/2020) vs retorno acumulado fev-abr/2020 (n=%d): corr=%.4f | coef=%.4f t=%.3f p=%.4f\n",
              nrow(mc), cor_crash, s[2,1], s[2,3], s[2,4]))
} else cat("Poucas observacoes pra checagem de crash\n")

# =============================================================================
# (M) COMOMENTUM (Lou & Polk 2013/2021) -- adaptado pra dado mensal
# =============================================================================
cat("\n===== (M) COMOMENTUM (comovimento de residuo dentro do decil de momentum) =====\n")
precos_wide <- dcast(precos, ymk ~ ticker, value.var = "retorno")
setorder(precos_wide, ymk)

# momentum: retorno acumulado t-12 a t-1
mom <- precos[, .(ticker, ymk, retorno)]
setorder(mom, ticker, ymk)
mom[, log_ret := log(1+retorno)]
mom[, mom_12m := exp(frollsum(log_ret, 12, align="right")) - 1, by = ticker]
mom <- mom[!is.na(mom_12m)]

# retorno de mercado (media simples entre ativos, cada mes) pra residuo simples
mercado <- precos[, .(ret_mercado = mean(retorno, na.rm=TRUE)), by = ymk]
precos_resid <- merge(precos, mercado, by = "ymk")
precos_resid[, retorno_resid := retorno - ret_mercado]
resid_wide <- dcast(precos_resid, ymk ~ ticker, value.var = "retorno_resid")
setorder(resid_wide, ymk)

meses_disponiveis <- sort(unique(mom$ym <- NULL))  # placeholder nao usado
meses_teste <- sort(unique(mom[ymk >= 201801]$ymk))  # janela de treino do comomentum comeca cedo (precisa de 12m de residuo)

calc_comomentum_mes <- function(mes_atual) {
  # decil de maior momentum NESTE mes
  m_mes <- mom[ymk == mes_atual]
  if (nrow(m_mes) < 20) return(NULL)
  corte_top <- quantile(m_mes$mom_12m, 0.9, na.rm=TRUE)
  top_mom <- m_mes[mom_12m >= corte_top]$ticker
  if (length(top_mom) < 8) return(NULL)

  # residuos dos ultimos 12 meses (ate mes_atual) pros ativos do decil
  janela <- resid_wide[ymk <= mes_atual][ymk > addm(mes_atual, -12)]
  cols_disp <- intersect(top_mom, names(janela))
  if (length(cols_disp) < 8) return(NULL)
  mat <- as.matrix(janela[, ..cols_disp])
  mat <- mat[, colSums(!is.na(mat)) >= 8, drop = FALSE]  # exige pelo menos 8 dos 12 meses
  if (ncol(mat) < 8) return(NULL)

  cor_mat <- suppressWarnings(cor(mat, use = "pairwise.complete.obs"))
  cor_mat[!is.finite(cor_mat)] <- NA
  diag(cor_mat) <- NA
  comomentum_por_ativo <- data.table(ticker = colnames(cor_mat), como = rowMeans(cor_mat, na.rm=TRUE), ym = mes_atual)
  comomentum_por_ativo[is.finite(como)]
}

lista_como <- lapply(meses_teste, calc_comomentum_mes)
como_dt <- rbindlist(Filter(Negate(is.null), lista_como))
cat("Ativo-mes com comomentum calculavel (dentro do top decil momentum):", nrow(como_dt), "\n")

if (nrow(como_dt) > 100) {
  addm_local <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
  for (h in c(1,3,6,12)) {
    m <- copy(como_dt); m[, ym_ret := addm_local(ym, h)]
    m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
    m <- m[is.finite(retorno) & is.finite(como)]
    r1 <- teste_oos_linear(m, "como", "retorno", "Comomentum (top-momentum)", h)
    r2 <- fama_macbeth(m, "como", "retorno", "Comomentum (top-momentum)", h)
    if (!is.null(r1)) resultados_lin[[length(resultados_lin)+1]] <- r1
    if (!is.null(r2)) resultados_fm[[length(resultados_fm)+1]] <- r2
  }
} else {
  cat("Poucas observacoes de comomentum pra testar com confianca\n")
}

RL <- rbindlist(resultados_lin); RF <- rbindlist(resultados_fm)
fwrite(RL, file.path(OUT, "candidatos_09_linear.csv"))
fwrite(RF, file.path(OUT, "candidatos_09_fama_macbeth.csv"))
cat("\nOK\n")
