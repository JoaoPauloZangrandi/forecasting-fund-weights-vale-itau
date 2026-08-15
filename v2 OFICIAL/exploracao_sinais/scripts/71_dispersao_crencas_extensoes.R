# =============================================================================
# 71_dispersao_crencas_extensoes.R  (exploracao_sinais)
#
# Continuacao do candidato #A (dispersao de crencas, DMS 2002). O script 70
# testou o NIVEL da dispersao (CV do peso entre fundos donos) e nao achou
# nada que passe nem do 5% padrao no corte em quintil -- e a unica
# especificacao com p<0.05 (linear, h=3, ponderado) tem sinal OPOSTO ao
# esperado pela teoria (dispersao alta -> retorno FUTURO MAIOR, nao menor).
#
# Duas extensoes principiadas antes de descartar a frente A:
#
# (i) Restringir a small caps -- DMS e a literatura de limits-to-arbitrage
#     preveem o efeito mais forte onde a restricao a venda a descoberto e'
#     mais binding (acoes menores/menos liquidas) -- mesma logica ja usada
#     no candidato #10 da exploracao principal (que testou isso p/ outros
#     sinais e nao achou nada, mas nao tinha testado para dispersao).
#
# (ii) VARIACAO da dispersao (delta_cv 3m) -- ao inves do nivel, testa se
#     desacordo CRESCENTE entre gestores (opinioes divergindo cada vez mais)
#     prediz retorno futuro -- ideia adjacente ao angulo (B) do HHI mas
#     aplicada a' dispersao em vez de concentracao.
#
# METODOLOGIA: mesmas 3 licoes inegociaveis (FM de verdade, quintil recortado
# por mes, treino<202001/teste>=202001).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/550

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

disp <- fread(file.path(OUT, "dispersao_crencas_dms.csv"))

# tamanho do ativo (proxy: valor total mantido pelos fundos naquele mes) --
# mesma proxy de tamanho/liquidez usada no candidato #10 da exploracao principal
pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp[, ticker := trimws(sub(".*- ", "", ativo))]
pp <- pp[is.finite(aum_prev) & aum_prev > 0 & peso > 0]
pp[, valor_posicao := peso * aum_prev]
tam <- pp[, .(valor_total_mantido = sum(valor_posicao)), by = .(ticker, ym)]
disp <- merge(disp, tam, by = c("ticker","ym"))
disp[, tercil_tamanho := as.integer(cut(valor_total_mantido, quantile(valor_total_mantido, 0:3/3), include.lowest = TRUE)), by = ym]

fama_macbeth_recut <- function(m, var_x, var_y, nome, h) {
  teste <- copy(m[ym >= CORTE & is.finite(get(var_x)) & is.finite(get(var_y))])
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
  cat(sprintf("%-42s h=%d %2d meses n_min=%3d spread(Q5-Q1)=%+7.3fpp/mes (%+6.1f%%/ano) Sharpe=%5.2f t=%6.2f p=%.5f | Bonf:%s\n",
              nome, h, nmes, n_min, 100*media, 100*((1+media)^12-1), sh, t_fm, p_fm,
              ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
  data.table(sinal = nome, horizonte = h, n_meses = nmes, n_min_grupo_mes = n_min,
             spread_pp = 100*media, sharpe = sh, t_fm = t_fm, p_fm = p_fm, sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
}

resultados <- list()

# ------------------------------------------------------------------
# (i) small caps: terco de menor valor total mantido
# ------------------------------------------------------------------
cat("\n===== (i) Dispersao restrita a small caps (terco de menor valor total mantido) =====\n")
small <- disp[tercil_tamanho == 1]
cat("N ticker-mes no terco small:", nrow(small), "| mediana ativos distintos/mes:",
    small[, .N, by=ym][, median(N)], "\n")
for (h in c(1,3,6)) {
  m <- copy(small); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  r <- fama_macbeth_recut(m, "cv_peso_ponderado", "retorno", "Dispersao (ponderada) | small caps", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

cat("\n===== (i-b) Dispersao restrita a large caps (terco de maior valor total mantido, comparacao) =====\n")
large <- disp[tercil_tamanho == 3]
for (h in c(1,3,6)) {
  m <- copy(large); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  r <- fama_macbeth_recut(m, "cv_peso_ponderado", "retorno", "Dispersao (ponderada) | large caps", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

# ------------------------------------------------------------------
# (ii) variacao da dispersao (delta_cv 3m e 6m) -- desacordo CRESCENTE
# ------------------------------------------------------------------
cat("\n===== (ii) Variacao da dispersao (delta_cv_ponderado, 3m e 6m) -> retorno futuro =====\n")
setorder(disp, ticker, ym)
disp_prev3 <- disp[, .(ticker, ym = addm(ym, 3), cv_prev3 = cv_peso_ponderado)]
disp_prev6 <- disp[, .(ticker, ym = addm(ym, 6), cv_prev6 = cv_peso_ponderado)]
disp2 <- merge(disp, disp_prev3, by = c("ticker","ym"))
disp2 <- merge(disp2, disp_prev6, by = c("ticker","ym"))
disp2[, delta_cv_3m := cv_peso_ponderado - cv_prev3]
disp2[, delta_cv_6m := cv_peso_ponderado - cv_prev6]
cat("N ticker-mes com delta_cv calculado:", nrow(disp2), "\n")

for (h in c(1,3,6)) {
  m <- copy(disp2); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  r <- fama_macbeth_recut(m, "delta_cv_3m", "retorno", "Delta dispersao 3m (desacordo crescente)", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}
for (h in c(1,3,6)) {
  m <- copy(disp2); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  r <- fama_macbeth_recut(m, "delta_cv_6m", "retorno", "Delta dispersao 6m (desacordo crescente)", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

R <- rbindlist(resultados, fill = TRUE)
fwrite(R, file.path(OUT, "candidatos_71_extensoes_dispersao.csv"))
cat("\n\n===== RESUMO (ordenado por p) =====\n")
print(R[order(p_fm)])
cat("\nOK\n")
