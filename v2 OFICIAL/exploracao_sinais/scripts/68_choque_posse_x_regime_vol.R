# =============================================================================
# 68_choque_posse_x_regime_vol.R  (exploracao_sinais, agente "regime")
#
# Combina as duas frentes deste agente: o choque de posse Wardlaw-corrigido
# (script 67, nulo sozinho) condicionado ao regime de volatilidade PREVISTA
# (HHI -- candidatos #26-31), a mesma logica de interacao aplicada aos
# outros sinais de retorno no script 65. Tambem testa a MAGNITUDE do choque
# (|choque|, sem direcao) -- ideia de "atencao"/evento incomum, nao
# direcao de acumulacao/distribuicao -- que e conceitualmente distinta de
# "escolher o quintil baixo-HHI" (regime) e pode ter mecanismo proprio: um
# choque de posse extremo pode ser mais informativo justamente numa acao
# que normalmente e' "calma" (HHI baixo, poucos fundos concentrados) do
# que numa que ja e' cronicamente concentrada.
#
# METODOLOGIA (as 3 licoes inegociaveis, identica aos scripts 65/67).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/500

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

# ---- HHI (mesma construcao do script 65/41) ----
pp3 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp3[, ticker := trimws(sub(".*- ", "", ativo))]
pp3 <- pp3[is.finite(aum_prev) & aum_prev > 0 & peso > 0]
pp3[, valor_posicao := peso * aum_prev]
crowd <- pp3[, .(hhi_posse = sum((valor_posicao/sum(valor_posicao))^2), n_fundos_hhi = .N), by = .(ticker, ym)]
crowd <- crowd[n_fundos_hhi >= 10]

# ---- choque de posse corrigido (mesma construcao do script 67) ----
pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","valor_mil"))
pp[, ticker := trimws(sub(".*- ", "", ativo))]
pp <- pp[peso > 0 & is.finite(valor_mil) & valor_mil > 0]
valor_tk <- pp[, .(valor_total = sum(valor_mil), n_fundos = uniqueN(cod_fundo)), by = .(ticker, ym)]
valor_tk <- valor_tk[n_fundos >= 10]
setorder(valor_tk, ticker, ym)
valor_tk[, valor_total_prev := shift(valor_total), by = ticker]
valor_tk <- merge(valor_tk, precos, by.x = c("ticker","ym"), by.y = c("ticker","ymk"), all.x = TRUE)
valor_tk <- valor_tk[is.finite(valor_total_prev) & valor_total_prev > 0 & is.finite(retorno)]
valor_tk[, choque_corrigido := (valor_total - valor_total_prev*(1+retorno)) / valor_total_prev]
q <- quantile(valor_tk$choque_corrigido, c(0.01,0.99), na.rm = TRUE)
valor_tk[, choque_corrigido_w := pmin(pmax(choque_corrigido, q[1]), q[2])]
valor_tk[, choque_abs_w := abs(choque_corrigido_w)]

base <- merge(valor_tk[, .(ticker, ym, choque_corrigido_w, choque_abs_w)],
              crowd[, .(ticker, ym, hhi_posse)], by = c("ticker","ym"))
cat("Ticker-mes com choque corrigido + HHI:", nrow(base), "\n")

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
  cat(sprintf("%-46s h=%d %2d meses n_min=%3d spread=%+7.3fpp/mes (%+7.1f%%/ano) Sharpe=%5.2f t=%6.2f p=%.5f | Bonf:%s\n",
              nome, h, nmes, n_min, 100*media, 100*((1+media)^12-1), sh, t_fm, p_fm,
              ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
  data.table(sinal = nome, horizonte = h, n_meses = nmes, n_min_grupo_mes = n_min,
             spread_pp = 100*media, sharpe = sh, t_fm = t_fm, p_fm = p_fm, sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
}

resultados <- list()
sinais <- c("choque_corrigido_w" = "Choque posse(corrigido)", "choque_abs_w" = "Choque posse|MAGNITUDE(|choque|)")

for (var_x in names(sinais)) {
  nome <- sinais[[var_x]]
  cat(sprintf("\n\n########## %s ##########\n", nome))
  for (h in c(1,3,6)) {
    m <- copy(base); m[, ym_ret := addm(ym, h)]
    m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
    m <- m[ym >= CORTE & is.finite(retorno)]
    if (nrow(m) < 30) next

    r0 <- fama_macbeth_recut(m, var_x, "retorno", paste0(nome, "|baseline"), h)
    if (!is.null(r0)) resultados[[length(resultados)+1]] <- r0

    m[, tercil_hhi := as.integer(cut(hhi_posse, quantile(hhi_posse, 0:3/3, na.rm = TRUE), include.lowest = TRUE)), by = ym]
    baixo_t <- m[tercil_hhi == 1]; alto_t <- m[tercil_hhi == 3]
    r1 <- fama_macbeth_recut(baixo_t, var_x, "retorno", paste0(nome, "|BAIXO-HHI(tercil)"), h)
    r2 <- fama_macbeth_recut(alto_t,  var_x, "retorno", paste0(nome, "|ALTO-HHI(tercil)"), h)
    if (!is.null(r1)) resultados[[length(resultados)+1]] <- r1
    if (!is.null(r2)) resultados[[length(resultados)+1]] <- r2

    m[, mediana_hhi := median(hhi_posse), by = ym]
    baixo_m <- m[hhi_posse < mediana_hhi]; alto_m <- m[hhi_posse >= mediana_hhi]
    r3 <- fama_macbeth_recut(baixo_m, var_x, "retorno", paste0(nome, "|BAIXO-HHI(mediana)"), h)
    r4 <- fama_macbeth_recut(alto_m,  var_x, "retorno", paste0(nome, "|ALTO-HHI(mediana)"), h)
    if (!is.null(r3)) resultados[[length(resultados)+1]] <- r3
    if (!is.null(r4)) resultados[[length(resultados)+1]] <- r4
  }
}

R <- rbindlist(resultados, fill = TRUE)
fwrite(R, file.path(OUT, "candidatos_68_choque_x_regime_vol.csv"))
cat("\n\n===== RESUMO GERAL (ordenado por p, top 20) =====\n")
print(head(R[order(p_fm)], 20))
cat("\nOK\n")
