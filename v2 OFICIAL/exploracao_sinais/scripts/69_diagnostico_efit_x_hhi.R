# =============================================================================
# 69_diagnostico_efit_x_hhi.R  (exploracao_sinais, agente "regime")
#
# Diagnostico do resultado mais interessante do script 65: E[FIT] (fluxo
# esperado, candidato #11 do LOG_CANDIDATOS.md principal -- Lou 2012,
# ja identificado la' como "o mais promissor mas sem long-short
# executavel") dentro do tercil de ALTO HHI mostra padrao que DECAI
# SUAVEMENTE (diferente do artefato de reversao do script 66):
#   h=1: t=3.52  p=0.0019   (+40.6%/ano)
#   h=3: t=2.00  p=0.0587   (+31.4%/ano)
#   h=6: t=1.75  p=0.0967   (+37.6%/ano)
# Decaimento suave em vez de isolado num horizonte so' e' o padrao que a
# propria exploracao (grid search do CIO, script 22) usou como criterio de
# "sinal genuino" -- mas magnitude de 30-40%/ano bruto e' grande demais
# pra ser plausivel sem investigar (mesmo heuristico que sinalizou
# Comomentum como artefato: literatura raramente documenta >15-20%/ano
# bruto).
#
# Checagens (mesmo playbook dos candidatos #14/22/36):
#   1) Contribuicao mes-a-mes ao spread (1 mes domina?).
#   2) Robustez excluindo 2020 (COVID).
#   3) Decil em vez de quintil (efeito mais extremo nos extremos?).
#   4) Regressao FM continua com interacao EFIT x HHI (nao so' quintil
#      dentro de subgrupo) -- teste decisivo de que a interacao existe de
#      verdade, nao e' artefato do corte.
#   5) Comparacao com o resultado ja registrado no LOG_CANDIDATOS.md
#      principal para o mesmo sinal SEM condicionar (candidato #11: usava
#      metodologia de quintil com breaks FIXOS do treino, nao re-cortados
#      por mes -- lecao #36 mostrou que isso pode gerar diferenca real).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/500

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

pp3 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp3[, ticker := trimws(sub(".*- ", "", ativo))]
pp3 <- pp3[is.finite(aum_prev) & aum_prev > 0 & peso > 0]
pp3[, valor_posicao := peso * aum_prev]
crowd <- pp3[, .(hhi_posse = sum((valor_posicao/sum(valor_posicao))^2), n_fundos = .N), by = .(ticker, ym)]
crowd <- crowd[n_fundos >= 10]

pp <- fread(file.path(DATA, "painel_multiativo_final.csv"),
            select = c("cod_fundo","ativo","ym","peso","aum_prev","flow_aum"))
pp[, cod_fundo := as.character(cod_fundo)]
pp[, ticker := trimws(sub(".*- ", "", ativo))]

rf <- fread(file.path(DATA, "retorno_fundo_mensal.csv"))
rf[, cod_fundo := as.character(cod_fundo)]
setorder(rf, cod_fundo, ymk)
rf[, log_ret := log(1 + retorno_fundo)]
rf[, skill_12m := exp(frollsum(log_ret, 12, align = "right")) - 1]
rf_skill <- rf[!is.na(skill_12m), .(cod_fundo, ym = ymk, skill_12m)]

fluxo_fundo <- unique(pp[, .(cod_fundo, ym, flow_aum)])
fluxo_fundo <- fluxo_fundo[is.finite(flow_aum)]
skill_lag <- copy(rf_skill); skill_lag[, ym := addm(ym, 1)]
fluxo_fundo <- merge(fluxo_fundo, skill_lag, by = c("cod_fundo","ym"), all.x = TRUE)
fluxo_fundo <- fluxo_fundo[is.finite(skill_12m)]
treino_1a <- fluxo_fundo[ym < CORTE]
fit_1a <- lm(flow_aum ~ skill_12m, data = treino_1a)
fluxo_fundo[, flow_aum_esperado := predict(fit_1a, newdata = fluxo_fundo)]

peso_prev2 <- pp[, .(cod_fundo, ativo, ym = addm(ym, 1), peso_prev = peso)]
d2 <- merge(pp[, .(cod_fundo, ativo, ym, aum_prev)], peso_prev2, by = c("cod_fundo","ativo","ym"))
d2 <- merge(d2, fluxo_fundo[, .(cod_fundo, ym, flow_aum_esperado)], by = c("cod_fundo","ym"))
d2 <- d2[is.finite(peso_prev) & peso_prev > 0 & is.finite(aum_prev) & is.finite(flow_aum_esperado)]
d2[, peso_valor := peso_prev * aum_prev]
efit <- d2[, .(EFIT = sum(peso_valor * flow_aum_esperado) / sum(peso_valor), n_fundos_e = .N), by = .(ativo, ym)]
efit <- efit[n_fundos_e >= 10]
efit[, ticker := trimws(sub(".*- ", "", ativo))]
sinal_efit <- efit[, .(ticker, ym, valor_sinal = EFIT)]

base <- merge(sinal_efit, crowd[, .(ticker, ym, hhi_posse)], by = c("ticker","ym"))

fama_macbeth_grupo <- function(m, var_x, var_y, nome, h, n_grupos = 5) {
  teste <- copy(m[is.finite(get(var_x)) & is.finite(get(var_y))])
  if (nrow(teste) < 30) return(NULL)
  probs <- seq(0, 1, length.out = n_grupos + 1)
  teste[, grupo := {
    qs <- quantile(get(var_x), probs, na.rm = TRUE)
    if (length(unique(qs)) < (n_grupos + 1)) as.integer(NA) else as.integer(cut(get(var_x), qs, include.lowest = TRUE))
  }, by = ym]
  teste <- teste[!is.na(grupo)]
  g_baixo <- 1L; g_alto <- n_grupos
  chk <- teste[grupo %in% c(g_baixo, g_alto), .N, by = .(ym, grupo)]
  n_min <- if (nrow(chk) > 0) min(chk$N) else 0
  por_mes <- teste[grupo %in% c(g_baixo, g_alto), .(rm = mean(get(var_y)), n = .N), by = .(ym, grupo)]
  sm <- dcast(por_mes, ym ~ grupo, value.var = "rm")
  nomes_col <- as.character(c(g_baixo, g_alto))
  if (!all(nomes_col %in% names(sm))) return(NULL)
  setnames(sm, nomes_col, c("q_baixo","q_alto")); sm <- sm[is.finite(q_baixo) & is.finite(q_alto)]
  sm[, spread := q_alto - q_baixo]; nmes <- nrow(sm)
  if (nmes < 6) return(invisible(list(res=NULL, serie=sm)))
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(nmes)); p_fm <- 2*pt(-abs(t_fm), df = nmes-1)
  sh <- media/dp*sqrt(12)
  cat(sprintf("%-30s h=%d [%dgrp] %2d meses n_min=%3d spread=%+7.3fpp/mes (%+7.1f%%/ano) Sharpe=%5.2f t=%6.2f p=%.5f | Bonf:%s\n",
              nome, h, n_grupos, nmes, n_min, 100*media, 100*((1+media)^12-1), sh, t_fm, p_fm,
              ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
  invisible(list(res = data.table(sinal = nome, horizonte = h, n_grupos = n_grupos, n_meses = nmes,
             n_min_grupo_mes = n_min, spread_pp = 100*media, sharpe = sh, t_fm = t_fm, p_fm = p_fm,
             sig_bonferroni = p_fm < LIMIAR_BONFERRONI), serie = sm))
}

cat("===== (1) Reconstruindo ALTO-HHI(tercil) h=1, serie de spreads mes a mes =====\n")
h <- 1
m <- copy(base); m[, ym_ret := addm(ym, h)]
m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
m <- m[ym >= CORTE & is.finite(retorno)]
m[, tercil_hhi := as.integer(cut(hhi_posse, quantile(hhi_posse, 0:3/3, na.rm = TRUE), include.lowest = TRUE)), by = ym]
alto_t <- m[tercil_hhi == 3]

r_alto <- fama_macbeth_grupo(alto_t, "valor_sinal", "retorno", "EFIT|ALTO-HHI(tercil)", h, 5)
print(r_alto$serie[order(ym)])

cat("\n===== (2) Contribuicao mes-a-mes ao spread =====\n")
sm <- r_alto$serie[order(ym)]
media_total <- mean(sm$spread)
cat(sprintf("Media total (%d meses): %.4f\n", nrow(sm), media_total))
for (i in seq_len(nrow(sm))) {
  sem_esse_mes <- mean(sm$spread[-i])
  cat(sprintf("  sem %d: media = %+.4f (contribuicao: %+.4f) | n_min naquele mes: verificar acima\n",
              sm$ym[i], sem_esse_mes, media_total - sem_esse_mes))
}

cat("\n===== (3) Robustez excluindo 2020 (SO 2021+) =====\n")
alto_2021 <- alto_t[ym >= 202101]
fama_macbeth_grupo(alto_2021, "valor_sinal", "retorno", "EFIT|ALTO-HHI SO 2021+", h, 5)
alto_2020 <- alto_t[ym < 202101]
fama_macbeth_grupo(alto_2020, "valor_sinal", "retorno", "EFIT|ALTO-HHI SO 2020", h, 5)

cat("\n===== (4) Decil em vez de quintil (efeito mais extremo nos extremos?) =====\n")
fama_macbeth_grupo(alto_t, "valor_sinal", "retorno", "EFIT|ALTO-HHI(tercil), decil", h, 10)

cat("\n===== (5) Regressao FM continua com interacao EFIT x HHI, TODOS os meses de teste, h=1 =====\n")
m[, `:=`(efit_z = as.numeric(scale(valor_sinal)), hhi_z = as.numeric(scale(hhi_posse))), by = ym]
m[, inter_z := efit_z * hhi_z]
meses <- sort(unique(m$ym))
coefs <- list()
for (mm_ym in meses) {
  dm <- m[ym == mm_ym]
  if (nrow(dm) < 30) next
  fit <- tryCatch(lm(retorno ~ efit_z + hhi_z + inter_z, data = dm), error = function(e) NULL)
  if (!is.null(fit)) coefs[[as.character(mm_ym)]] <- data.table(ym = mm_ym, t(coef(fit)))
}
CM <- rbindlist(coefs, fill = TRUE)
cat(sprintf("Meses com regressao valida: %d\n", nrow(CM)))
for (v in c("efit_z","hhi_z","inter_z")) {
  x <- CM[[v]]; x <- x[is.finite(x)]; n <- length(x)
  media <- mean(x); dp <- sd(x); t_fm <- media/(dp/sqrt(n)); p_fm <- 2*pt(-abs(t_fm), df = n-1)
  cat(sprintf("  %-10s coef_medio=%+9.5f | t=%6.2f | p=%.4f\n", v, media, t_fm, p_fm))
}

cat("\n===== (6) Checando se ALTO-HHI(tercil) e' so' um proxy de TAMANHO (acoes grandes = mais fundos = HHI mais baixo tipicamente, mas confirmar) =====\n")
# tamanho proxy: valor total detido pelos fundos (nao temos market cap direto)
valor_tot <- pp[, .(valor_total = sum(aum_prev*peso)), by = .(ticker, ym)]
mt <- merge(alto_t, valor_tot, by = c("ticker","ym"))
cat("Correlacao HHI x log(valor_total) dentro do tercil ALTO-HHI:",
    round(cor(mt$hhi_posse, log(mt$valor_total), use="complete.obs"),3), "\n")
cat("N medio de fundos no tercil ALTO-HHI:", round(mean(m[tercil_hhi==3]$n_fundos, na.rm=TRUE),1),
    "| tercil BAIXO-HHI:", round(mean(m[tercil_hhi==1]$n_fundos, na.rm=TRUE),1), "\n")

cat("\n===== (7) Consistencia direcional: % de meses com spread positivo =====\n")
cat(sprintf("ALTO-HHI(tercil) h=1: %.1f%% dos %d meses com spread positivo\n",
            100*mean(sm$spread > 0), nrow(sm)))

cat("\nOK\n")
