# =============================================================================
# 66_diagnostico_reversao_h6.R  (exploracao_sinais, agente "regime")
#
# Diagnostico do achado inesperado do script 65: "Reversao curto prazo"
# (-retorno do proprio mes) prevendo retorno em h=6 -- baseline ja
# significativo sozinho (p=0.0036) e mais forte ainda dentro do tercio/
# metade de ALTO HHI (p=0.00045, o mais perto do limiar de Bonferroni de
# toda a exploracao ate agora). Como isso NAO era a hipotese original
# (a hipotese testava CIO e outros sinais de retorno JA existentes, nao
# "descobrir" um sinal novo de momentum), e como ha precedente direto na
# historia desta exploracao de "quase-achados" que eram artefato de
# degenerescencia amostral (Comomentum, candidato #14) ou breaks de
# quintil mal cortados (CIO original, candidato #15/36), este script
# aplica a mesma bateria de checagens antes de levar isso a serio.
#
# O que esse sinal realmente mede: variavel = -retorno(t) [retorno do
# PROPRIO mes]; spread Q5-Q1 em retorno(t+6). Q5 = alta "reversao" =
# retorno(t) MUITO NEGATIVO (perdedor recente); Q1 = retorno(t) MUITO
# POSITIVO (ganhador recente). Spread NEGATIVO (achado do script 65)
# significa: perdedores recentes de 1 mes continuam pior que ganhadores
# recentes de 1 mes, 6 meses depois -- ou seja, e' CONTINUACAO
# (momentum), nao reversao, apesar do nome do sinal. Formation window de
# so 1 MES e incomum na literatura de momentum classico (Jegadeesh-Titman
# 1993 usa 3-12 meses de formacao) -- mais uma razao para desconfiar antes
# de acreditar.
#
# Checagens (mesmo playbook usado nos candidatos #14 e #36):
#   1) N minimo por quintil-mes ja registrado no script 65 (42, saudavel) --
#      re-confirmado aqui.
#   2) Estabilidade excluindo 2020 (ano do crash/recuperacao COVID).
#   3) Um unico mes domina o spread?
#   4) O efeito e' robusto a usar retorno acumulado de 3 meses como
#      formation window (mais proximo da literatura de momentum classico)
#      em vez de 1 mes?
#   5) Fama-MacBeth com regressao continua (nao so' quintil), controlando
#      por HHI simultaneamente.
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

sinal_reversao <- precos[, .(ticker, ym = ymk, valor_sinal = -retorno)]
base <- merge(sinal_reversao, crowd[, .(ticker, ym, hhi_posse)], by = c("ticker","ym"))

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
  cat(sprintf("%-40s h=%d %2d meses n_min=%3d spread=%+7.3fpp/mes (%+7.1f%%/ano) Sharpe=%5.2f t=%6.2f p=%.5f | Bonf:%s\n",
              nome, h, nmes, n_min, 100*media, 100*((1+media)^12-1), sh, t_fm, p_fm,
              ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
  return(invisible(list(res = data.table(sinal = nome, horizonte = h, n_meses = nmes, n_min_grupo_mes = n_min,
             spread_pp = 100*media, sharpe = sh, t_fm = t_fm, p_fm = p_fm, sig_bonferroni = p_fm < LIMIAR_BONFERRONI),
             serie = sm)))
}

cat("===== (1) Reconfirmando baseline h=6 e ALTO-HHI(mediana) h=6, com a serie de spreads mensais =====\n")
h <- 6
m <- copy(base); m[, ym_ret := addm(ym, h)]
m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
m <- m[ym >= CORTE & is.finite(retorno)]

r_base <- fama_macbeth_recut(m, "valor_sinal", "retorno", "Reversao|baseline", h)
m[, mediana_hhi := median(hhi_posse), by = ym]
alto_m <- m[hhi_posse >= mediana_hhi]
r_alto <- fama_macbeth_recut(alto_m, "valor_sinal", "retorno", "Reversao|ALTO-HHI(mediana)", h)

cat("\nSerie de spreads mensais (baseline h=6):\n")
print(r_base$serie[order(ym)])
cat("\nSerie de spreads mensais (ALTO-HHI h=6):\n")
print(r_alto$serie[order(ym)])

cat("\n===== (2) Um mes domina o spread? (baseline) =====\n")
sm <- r_base$serie[order(ym)]
media_total <- mean(sm$spread)
cat(sprintf("Media total (todos os %d meses): %.4f\n", nrow(sm), media_total))
for (i in seq_len(nrow(sm))) {
  sem_esse_mes <- mean(sm$spread[-i])
  cat(sprintf("  sem %d: media = %+.4f (contribuicao do mes: %+.4f)\n", sm$ym[i], sem_esse_mes, media_total - sem_esse_mes))
}

cat("\n===== (3) Robustez excluindo 2020 inteiro (so' 2021+) =====\n")
m2021 <- m[ym >= 202101]
r_2021 <- fama_macbeth_recut(m2021, "valor_sinal", "retorno", "Reversao|baseline SO 2021+", h)
alto_2021 <- alto_m[ym >= 202101]
r_alto_2021 <- fama_macbeth_recut(alto_2021, "valor_sinal", "retorno", "Reversao|ALTO-HHI SO 2021+", h)

cat("\n===== (4) So' meses de 2020 (pra comparar) =====\n")
m2020 <- m[ym < 202101]
r_2020 <- fama_macbeth_recut(m2020, "valor_sinal", "retorno", "Reversao|baseline SO 2020", h)

cat("\n===== (5) Formation window de 3 meses (retorno acumulado t-2 a t, sinal invertido) -- mais proximo da literatura classica de momentum =====\n")
setorder(precos, ticker, ymk)
precos[, log_ret := log(1 + retorno)]
precos[, ret_3m := exp(frollsum(log_ret, 3, align = "right")) - 1]
sinal_rev3 <- precos[is.finite(ret_3m), .(ticker, ym = ymk, valor_sinal = -ret_3m)]
base3 <- merge(sinal_rev3, crowd[, .(ticker, ym, hhi_posse)], by = c("ticker","ym"))
m3 <- copy(base3); m3[, ym_ret := addm(ym, h)]
m3 <- merge(m3, precos[, .(ticker, ymk, retorno)], by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
m3 <- m3[ym >= CORTE & is.finite(retorno)]
r_3m <- fama_macbeth_recut(m3, "valor_sinal", "retorno", "Reversao(formacao 3m)|baseline", h)
m3[, mediana_hhi := median(hhi_posse), by = ym]
alto_3m <- m3[hhi_posse >= mediana_hhi]
r_3m_alto <- fama_macbeth_recut(alto_3m, "valor_sinal", "retorno", "Reversao(formacao 3m)|ALTO-HHI", h)

cat("\n===== (6) Verificando outros horizontes com a MESMA janela de formacao 1m (h=1,2,3,4,5,6,9,12) pra ver se o padrao decai suavemente ou aparece isolado em h=6 =====\n")
resultados_h <- list()
for (hh in c(1,2,3,4,5,6,9,12)) {
  mm <- copy(base); mm[, ym_ret := addm(ym, hh)]
  mm <- merge(mm, precos[, .(ticker, ymk, retorno)], by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  mm <- mm[ym >= CORTE & is.finite(retorno)]
  rr <- fama_macbeth_recut(mm, "valor_sinal", "retorno", "Reversao|baseline", hh)
  if (!is.null(rr)) resultados_h[[length(resultados_h)+1]] <- rr$res
}
RH <- rbindlist(resultados_h)
cat("\nPadrao entre horizontes (deveria ser suave se for sinal real, nao isolado):\n")
print(RH[, .(horizonte, spread_pp, t_fm, p_fm)])

cat("\n===== (7) Fama-MacBeth regressao continua (nao quintil), controlando HHI simultaneamente, h=6 =====\n")
m[, `:=`(rev_z = as.numeric(scale(valor_sinal)), hhi_z = as.numeric(scale(hhi_posse))), by = ym]
m[, inter_z := rev_z * hhi_z]
meses <- sort(unique(m$ym))
coefs <- list()
for (mm_ym in meses) {
  dm <- m[ym == mm_ym]
  if (nrow(dm) < 30) next
  fit <- tryCatch(lm(retorno ~ rev_z + hhi_z + inter_z, data = dm), error = function(e) NULL)
  if (!is.null(fit)) coefs[[as.character(mm_ym)]] <- data.table(ym = mm_ym, t(coef(fit)))
}
CM <- rbindlist(coefs, fill = TRUE)
cat(sprintf("Meses com regressao valida: %d\n", nrow(CM)))
for (v in c("rev_z","hhi_z","inter_z")) {
  x <- CM[[v]]; x <- x[is.finite(x)]; n <- length(x)
  media <- mean(x); dp <- sd(x); t_fm <- media/(dp/sqrt(n)); p_fm <- 2*pt(-abs(t_fm), df = n-1)
  cat(sprintf("  %-10s coef_medio=%+9.5f | t=%6.2f | p=%.4f\n", v, media, t_fm, p_fm))
}

cat("\nOK\n")
