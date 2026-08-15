# =============================================================================
# 43_atencao_choque_posicao.R  (exploracao_sinais)
#
# Angulo (d) pedido pelo Joao: sentimento/atencao via VARIACAO ABRUPTA de
# posicao de fundos -- nao o NIVEL de posse (ja testado varias vezes: HHI,
# breadth, ownership) mas o CHOQUE, a MUDANCA incomum. Dois sinais novos:
#
#   1) INICIACAO LIQUIDA: (numero de fundos que abriram posicao NOVA no mes
#      - numero que ZERARAM posicao) / numero de donos no mes anterior.
#      Literatura de referencia: quebra na margem EXTENSIVA de posse
#      (Chen-Hong-Stein 2001 "breadth of ownership", Gompers-Metrick 2001)
#      -- mas aqui e' o FLUXO de entrada/saida de donos, nao o nivel.
#
#   2) CHOQUE DE VALOR: variacao percentual (log) do valor total detido
#      pelos fundos no mes, padronizada (z-score) pela media/DP dos ULTIMOS
#      12 meses DAQUELE MESMO ticker (nao um corte transversal -- cada acao
#      e' comparada com o seu proprio historico de "normalidade" de
#      variacao de posse, pra nao confundir acao que sempre tem holdings
#      volateis com uma acao que teve um choque real). Proxy de "atencao
#      institucional anormal" sem dado de noticia/busca (nao temos SVI/Google
#      Trends) -- ideia adjacente a Da-Engelberg-Gao (2011) "In Search of
#      Attention", mas construida com holdings em vez de buscas.
#
# METODOLOGIA (as 3 licoes inegociaveis): Fama-MacBeth de verdade na serie
# de spreads mensais; quintil RE-CORTADO A CADA MES no periodo de teste;
# treino ym<202001 / teste ym>=202001 (aqui o "treino" so' serve pra
# construir o historico de normalizacao do sinal 2, nunca pra fixar breaks
# de quintil).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/500

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","valor_mil"))
pp[, ticker := trimws(sub(".*- ", "", ativo))]
pp <- pp[peso > 0 & is.finite(valor_mil)]

# =============================================================================
# SINAL 1: iniciacao liquida (margem extensiva -- entradas menos saidas)
# =============================================================================
pres <- unique(pp[, .(cod_fundo, ticker, ym)])
pres[, presente := 1L]
pres_prev <- copy(pres); pres_prev[, ym := addm(ym, 1)]; setnames(pres_prev, "presente", "presente_prev")
merged <- merge(pres, pres_prev, by = c("cod_fundo","ticker","ym"), all = TRUE)
merged[is.na(presente), presente := 0L]
merged[is.na(presente_prev), presente_prev := 0L]
merged[, entrada := presente == 1L & presente_prev == 0L]
merged[, saida   := presente == 0L & presente_prev == 1L]

sinal1 <- merged[, .(n_entrada = sum(entrada), n_saida = sum(saida),
                      n_donos_t = sum(presente), n_donos_prev = sum(presente_prev)), by = .(ticker, ym)]
sinal1 <- sinal1[n_donos_prev >= 10]
sinal1[, iniciacao_liquida := (n_entrada - n_saida) / n_donos_prev]
cat("Ticker-mes com iniciacao_liquida (n_donos_prev>=10):", nrow(sinal1), "\n")
cat("Resumo iniciacao_liquida:\n"); print(summary(sinal1$iniciacao_liquida))

# =============================================================================
# SINAL 2: choque de valor total detido, padronizado pelo historico PROPRIO do ticker
# =============================================================================
valor_tk <- pp[, .(valor_total = sum(valor_mil), n_fundos = uniqueN(cod_fundo)), by = .(ticker, ym)]
valor_tk <- valor_tk[n_fundos >= 10 & valor_total > 0]
setorder(valor_tk, ticker, ym)
valor_tk[, dlog_valor := log(valor_total) - log(shift(valor_total)), by = ticker]
# media/DP dos 12 meses ANTERIORES (exclui o mes corrente -- shift antes do frollapply)
valor_tk[, dlog_lag1 := shift(dlog_valor), by = ticker]
valor_tk[, media_12m := frollmean(dlog_lag1, 12, align = "right"), by = ticker]
valor_tk[, dp_12m := frollapply(dlog_lag1, 12, sd), by = ticker]
valor_tk[, choque_valor_z := (dlog_valor - media_12m) / dp_12m]
valor_tk <- valor_tk[is.finite(choque_valor_z)]
cat("\nTicker-mes com choque_valor_z valido:", nrow(valor_tk), "\n")
cat("Resumo choque_valor_z:\n"); print(summary(valor_tk$choque_valor_z))

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
  cat(sprintf("%-30s h=%2d %2d meses n_min=%3d spread=%+7.2fpp/mes (%+6.1f%%/ano) Sharpe=%5.2f t=%6.2f p=%.5f | Bonf:%s\n",
              nome, h, nmes, n_min, 100*media, 100*((1+media)^12-1), sh, t_fm, p_fm,
              ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
  data.table(sinal = nome, horizonte = h, n_meses = nmes, n_min_grupo_mes = n_min,
             spread_pp = 100*media, sharpe = sh, t_fm = t_fm, p_fm = p_fm, sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
}

resultados <- list()

cat("\n===== SINAL 1: iniciacao liquida -> retorno futuro =====\n")
for (h in c(1,3,6)) {
  m <- copy(sinal1); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[ym >= CORTE & is.finite(retorno)]
  r <- fama_macbeth_recut(m, "iniciacao_liquida", "retorno", "Iniciacao liquida", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

cat("\n===== SINAL 2: choque de valor (z-score proprio do ticker) -> retorno futuro =====\n")
for (h in c(1,3,6)) {
  m <- copy(valor_tk); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[ym >= CORTE & is.finite(retorno)]
  r <- fama_macbeth_recut(m, "choque_valor_z", "retorno", "Choque de valor (z proprio)", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

# ---- versao "magnitude absoluta" (choque grande em qualquer direcao = mais atencao?) ----
cat("\n===== SINAL 2b: MAGNITUDE do choque de valor (|z|) -> retorno futuro =====\n")
for (h in c(1,3,6)) {
  m <- copy(valor_tk); m[, choque_abs := abs(choque_valor_z)]; m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[ym >= CORTE & is.finite(retorno)]
  r <- fama_macbeth_recut(m, "choque_abs", "retorno", "Choque de valor (magnitude |z|)", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

R <- rbindlist(resultados, fill = TRUE)
fwrite(R, file.path(OUT, "candidatos_43_atencao_choque.csv"))
cat("\n\n===== RESUMO (ordenado por p) =====\n")
print(R[order(p_fm)])
cat("\nOK\n")
