# =============================================================================
# 41_cio_x_vol_prevista.R  (exploracao_sinais)
#
# Angulo (b) pedido pelo Joao: interacao entre volatilidade PREVISTA e um
# sinal de retorno ja testado. Usa a variavel que a propria exploracao ja
# estabeleceu como preditor real de volatilidade FUTURA (HHI de posse --
# candidatos #26-31, achado mais solido de toda a busca) como proxy de
# "regime de volatilidade prevista" (nao volatilidade passada bruta, que
# ja foi testada de outro jeito no candidato #25 com resultado null/oposto
# a literatura). Hipotese: CIO Peer Momentum (unico sinal de RETORNO
# sobrevivente, candidatos #11/15/34-38) e mais limpo/mais forte em acoes
# de BAIXO HHI (regime de vol futura prevista baixa) porque a difusao
# gradual de informacao entre "peers" e mais provavel de ser o mecanismo
# dominante quando nao ha ruido de risco de liquidacao coordenada
# (Greenwood-Thesmar) contaminando o sinal.
#
# METODOLOGIA (as 3 licoes inegociaveis):
#   1) Fama-MacBeth de verdade: spread Q5-Q1 calculado MES A MES, testando
#      a media da serie temporal de spreads mensais (nao pooled).
#   2) Quintil RE-CORTADO A CADA MES (metodo B, mesmo do candidato #36/37 --
#      NUNCA breaks fixos do treino aplicados no teste).
#   3) treino ym<202001 usado so' para computar terceis/mediana de HHI
#      (variavel de CONDICIONAMENTO, nao a variavel ranqueada) -- o corte do
#      HHI tambem e' re-feito a cada mes (by=ym) para evitar o mesmo
#      artefato. O teste roda so' em ym>=202001.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/500

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

cio <- fread(file.path(OUT, "cio_peer_return.csv"))[, .(ticker, ym, peer_ret, n_peers)]

pp3 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp3[, ticker := trimws(sub(".*- ", "", ativo))]
pp3 <- pp3[is.finite(aum_prev) & aum_prev > 0 & peso > 0]
pp3[, valor_posicao := peso * aum_prev]
crowd <- pp3[, .(hhi_posse = sum((valor_posicao/sum(valor_posicao))^2), n_fundos = .N), by = .(ticker, ym)]
crowd <- crowd[n_fundos >= 10]

base <- merge(cio, crowd, by = c("ticker","ym"))
cat("Ticker-mes com peer_ret + HHI disponiveis:", nrow(base), "\n")

# --- funcao FM com re-corte de quintil MES A MES (metodo B, lecao #36) ---
fama_macbeth_recut <- function(m, var_x, var_y, nome, h) {
  teste <- copy(m[ym >= CORTE])
  teste <- teste[is.finite(get(var_x)) & is.finite(get(var_y))]
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
  cat(sprintf("%-38s h=%d %2d meses n_min_grupo-mes=%3d spread=%+7.2fpp/mes (%+6.1f%%/ano) t=%6.2f p=%.5f | Bonf:%s\n",
              nome, h, nmes, n_min, 100*media, 100*((1+media)^12-1), t_fm, p_fm,
              ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
  data.table(sinal = nome, horizonte = h, n_meses = nmes, n_min_grupo_mes = n_min,
             spread_pp = 100*media, t_fm = t_fm, p_fm = p_fm, sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
}

resultados <- list()

cat("===== Baseline: CIO peer momentum SEM condicionar (referencia, teste ym>=2020) =====\n")
for (h in c(1,3,6)) {
  m <- copy(base); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  r <- fama_macbeth_recut(m, "peer_ret", "retorno", "CIO|baseline(sem cond.)", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

cat("\n===== CIO peer momentum condicionado a TERCIL de HHI (re-cortado por mes) =====\n")
for (h in c(1,3,6)) {
  m <- copy(base); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno)]
  m[, tercil_hhi := as.integer(cut(hhi_posse, quantile(hhi_posse, 0:3/3, na.rm = TRUE), include.lowest = TRUE)), by = ym]
  baixo <- m[tercil_hhi == 1]  # baixo HHI = vol futura prevista BAIXA
  alto  <- m[tercil_hhi == 3]  # alto HHI = vol futura prevista ALTA
  r1 <- fama_macbeth_recut(baixo, "peer_ret", "retorno", "CIO|BAIXO-HHI(vol.prev.baixa)", h)
  r2 <- fama_macbeth_recut(alto,  "peer_ret", "retorno", "CIO|ALTO-HHI(vol.prev.alta)", h)
  if (!is.null(r1)) resultados[[length(resultados)+1]] <- r1
  if (!is.null(r2)) resultados[[length(resultados)+1]] <- r2
}

cat("\n===== CIO peer momentum condicionado a MEDIANA de HHI (re-cortado por mes) =====\n")
for (h in c(1,3,6)) {
  m <- copy(base); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno)]
  m[, mediana_hhi := median(hhi_posse), by = ym]
  baixo <- m[hhi_posse <  mediana_hhi]
  alto  <- m[hhi_posse >= mediana_hhi]
  r1 <- fama_macbeth_recut(baixo, "peer_ret", "retorno", "CIO|BAIXO-HHI(mediana)", h)
  r2 <- fama_macbeth_recut(alto,  "peer_ret", "retorno", "CIO|ALTO-HHI(mediana)", h)
  if (!is.null(r1)) resultados[[length(resultados)+1]] <- r1
  if (!is.null(r2)) resultados[[length(resultados)+1]] <- r2
}

# --- regressao FM formal com termo de interacao (padronizado, mes a mes) ---
cat("\n===== Regressao FM formal: retorno ~ peer_ret_z + hhi_z + peer_ret_z:hhi_z (h=1) =====\n")
m1 <- copy(base); m1[, ym_ret := addm(ym, 1)]
m1 <- merge(m1, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
m1 <- m1[ym >= CORTE & is.finite(retorno) & is.finite(peer_ret) & is.finite(hhi_posse)]
meses <- sort(unique(m1$ym))
coefs <- list()
for (mm in meses) {
  dm <- m1[ym == mm]
  if (nrow(dm) < 30) next
  dm[, `:=`(peer_z = as.numeric(scale(peer_ret)), hhi_z = as.numeric(scale(hhi_posse)))]
  dm[, inter_z := peer_z * hhi_z]
  fit <- tryCatch(lm(retorno ~ peer_z + hhi_z + inter_z, data = dm), error = function(e) NULL)
  if (!is.null(fit)) coefs[[as.character(mm)]] <- data.table(ym = mm, t(coef(fit)))
}
CM <- rbindlist(coefs, fill = TRUE)
cat(sprintf("Meses com regressao valida: %d\n", nrow(CM)))
for (v in c("peer_z","hhi_z","inter_z")) {
  x <- CM[[v]]; x <- x[is.finite(x)]; n <- length(x)
  media <- mean(x); dp <- sd(x); t_fm <- media/(dp/sqrt(n)); p_fm <- 2*pt(-abs(t_fm), df = n-1)
  cat(sprintf("  %-10s coef_medio=%+9.5f | t=%6.2f | p=%.4f\n", v, media, t_fm, p_fm))
}

R <- rbindlist(resultados, fill = TRUE)
fwrite(R, file.path(OUT, "candidatos_41_cio_x_vol_prevista.csv"))
cat("\n\n===== RESUMO (ordenado por p) =====\n")
print(R[order(p_fm)])
cat("\nOK\n")
