# =============================================================================
# 70_dispersao_crencas_dms.R  (exploracao_sinais)
#
# FRENTE (A) do angulo "dispersao": Diether, Malloy & Scherbina (2002, JF)
# -- dispersao de opiniao entre analistas prediz retorno BAIXO (mecanismo:
# restricao a venda a descoberto + Miller 1977, otimistas dominam o preco
# quando pessimistas nao podem vender a descoberto -> sobrepreco -> queda
# subsequente quando a divergencia se resolve).
#
# Adaptacao pro nosso dado (sem dado de analista, so' holdings de fundos):
# constroi, p/ cada ativo-mes, a DISPERSAO CROSS-SECTIONAL do PESO que
# fundos DIFERENTES dao aa MESMA acao dentro das proprias carteiras --
# coeficiente de variacao (CV = sd/mean) do peso entre os fundos que a
# carregam. Como "peso" ja e' normalizado (fracao do AUM do proprio fundo),
# CV e' comparavel entre fundos de tamanhos diferentes -- um fundo pequeno
# dando 5% de peso e' comparavel a um fundo grande dando 5% de peso.
#
# Interpretacao: CV alto = gestores MUITO em desacordo sobre quanto essa
# acao "merece" de peso na carteira (uns apostam pesado, outros so' tem
# uma posicao residual) -- proxy de "desacordo" analogo a dispersao de
# forecast de analista, so' que revelado via ALOCACAO DE CAPITAL em vez de
# previsao de EPS/preco-alvo.
#
# Duas construcoes (pedido explicito): sem peso (CV simples entre fundos)
# e ponderada por AUM do fundo (fundos maiores pesam mais no calculo da
# dispersao -- captura "desacordo entre os players relevantes", nao entre
# qualquer fundo pequeno).
#
# METODOLOGIA (as 3 licoes inegociaveis desta exploracao): Fama-MacBeth de
# verdade (1 coeficiente/spread por MES, testado com erro-padrao da propria
# serie temporal); quintil RE-CORTADO A CADA MES (nunca breakpoint fixo do
# treino aplicado no teste); treino ym<202001, teste ym>=202001.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/550   # baseline ~520-550 especificacoes ja testadas na exploracao principal (outros agentes rodando em paralelo somam mais)

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp[, ticker := trimws(sub(".*- ", "", ativo))]
pp <- pp[is.finite(aum_prev) & aum_prev > 0 & is.finite(peso) & peso > 0]
pp <- pp[ticker %in% unique(precos$ticker)]
cat("Linhas fundo-ativo-mes (peso>0):", nrow(pp), "\n")

# =============================================================================
# CONSTRUCAO DO SINAL: dispersao cross-sectional do peso entre fundos donos
# =============================================================================
disp <- pp[, {
  n <- .N
  m_simples <- mean(peso)
  sd_simples <- if (n >= 2) sd(peso) else NA_real_
  cv_simples <- sd_simples / m_simples

  wm <- weighted.mean(peso, aum_prev)
  wvar <- sum(aum_prev * (peso - wm)^2) / sum(aum_prev)
  wsd <- sqrt(wvar)
  cv_ponderado <- wsd / wm

  .(n_fundos = n, mean_peso = m_simples, cv_peso = cv_simples,
    wmean_peso = wm, cv_peso_ponderado = cv_ponderado)
}, by = .(ticker, ym)]

disp <- disp[n_fundos >= 10]   # mesmo piso usado no resto da exploracao p/ HHI (script 30)
cat("Ticker-mes com dispersao calculada (n_fundos>=10):", nrow(disp), "\n")
cat("Resumo cv_peso (simples):\n"); print(summary(disp$cv_peso))
cat("Resumo cv_peso_ponderado (por AUM):\n"); print(summary(disp$cv_peso_ponderado))
cat("Correlacao cv_peso x cv_peso_ponderado:", round(cor(disp$cv_peso, disp$cv_peso_ponderado, use="complete.obs"),3), "\n")
cat("Correlacao cv_peso x n_fundos (checar se dispersao nao e' so' um proxy de breadth):",
    round(cor(disp$cv_peso, disp$n_fundos, use="complete.obs"),3), "\n")

fwrite(disp, file.path(OUT, "dispersao_crencas_dms.csv"))

# =============================================================================
# Traz HHI de posse (ja calculado em outros scripts, recalculado aqui p/
# nao depender de arquivo de outro script) -- para controle multivariado:
# dispersao alta e' economicamente distinta de concentracao alta?
# =============================================================================
pp2 <- copy(pp)
pp2[, valor_posicao := peso * aum_prev]
hhi <- pp2[, .(hhi_posse = sum((valor_posicao/sum(valor_posicao))^2)), by = .(ticker, ym)]
disp <- merge(disp, hhi, by = c("ticker","ym"))
cat("\nCorrelacao cv_peso x hhi_posse:", round(cor(disp$cv_peso, disp$hhi_posse, use="complete.obs"),3), "\n")
cat("Correlacao cv_peso_ponderado x hhi_posse:", round(cor(disp$cv_peso_ponderado, disp$hhi_posse, use="complete.obs"),3), "\n")

# =============================================================================
# TESTE 1: Fama-MacBeth long-short, quintil RE-CORTADO A CADA MES
# =============================================================================
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
  cat(sprintf("%-38s h=%d %2d meses n_min=%3d spread(Q5-Q1)=%+7.3fpp/mes (%+6.1f%%/ano) Sharpe=%5.2f t=%6.2f p=%.5f | Bonf:%s\n",
              nome, h, nmes, n_min, 100*media, 100*((1+media)^12-1), sh, t_fm, p_fm,
              ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
  data.table(sinal = nome, horizonte = h, n_meses = nmes, n_min_grupo_mes = n_min,
             spread_pp = 100*media, sharpe = sh, t_fm = t_fm, p_fm = p_fm, sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
}

# =============================================================================
# TESTE 2: Fama-MacBeth regressao linear (continua, padronizada por mes) --
# robustez alem do corte em quintil, e' o que permite depois rodar o
# controle multivariado (dispersao + n_fundos + hhi juntos)
# =============================================================================
fama_macbeth_linear <- function(m, vars_x, nome, h) {
  teste <- copy(m[ym >= CORTE])
  for (v in vars_x) teste[, (paste0(v,"_z")) := scale(get(v)), by = ym]
  meses <- sort(unique(teste$ym))
  coefs <- list()
  for (mm in meses) {
    dm <- teste[ym == mm & is.finite(retorno)]
    dm <- dm[complete.cases(dm[, paste0(vars_x,"_z"), with = FALSE])]
    if (nrow(dm) < 30) next
    form <- as.formula(paste("retorno ~", paste(paste0(vars_x,"_z"), collapse = " + ")))
    fit <- tryCatch(lm(form, data = dm), error = function(e) NULL)
    if (is.null(fit)) next
    coefs[[as.character(mm)]] <- data.table(ym = mm, t(coef(fit)))
  }
  CM <- rbindlist(coefs, fill = TRUE)
  resultado <- list()
  for (v in paste0(vars_x, "_z")) {
    if (!v %in% names(CM)) next
    x <- CM[[v]]; x <- x[is.finite(x)]
    n <- length(x); if (n < 6) next
    media <- mean(x); dp <- sd(x)
    t_fm <- media/(dp/sqrt(n)); p_fm <- 2*pt(-abs(t_fm), df = n-1)
    cat(sprintf("  [FM linear] %-30s h=%d var=%-22s coef_medio=%+9.5f t=%6.2f p=%.5f (%d meses) | Bonf:%s\n",
                nome, h, v, media, t_fm, p_fm, n, ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
    resultado[[v]] <- data.table(sinal = nome, horizonte = h, variavel = v, n_meses = n,
                                  coef_medio = media, t_fm = t_fm, p_fm = p_fm, sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
  }
  rbindlist(resultado, fill = TRUE)
}

resultados_qs <- list()
resultados_lin <- list()

cat("\n===== TESTE 1: quintil (Q5=alta dispersao, Q1=baixa) -> retorno, cv_peso simples =====\n")
for (h in c(1,3,6,12)) {
  m <- copy(disp); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  r <- fama_macbeth_recut(m, "cv_peso", "retorno", "Dispersao (CV peso, simples)", h)
  if (!is.null(r)) resultados_qs[[length(resultados_qs)+1]] <- r
}

cat("\n===== TESTE 1b: quintil, cv_peso ponderado por AUM do fundo =====\n")
for (h in c(1,3,6,12)) {
  m <- copy(disp); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  r <- fama_macbeth_recut(m, "cv_peso_ponderado", "retorno", "Dispersao (CV peso, ponderado AUM)", h)
  if (!is.null(r)) resultados_qs[[length(resultados_qs)+1]] <- r
}

cat("\n===== TESTE 2: Fama-MacBeth linear, dispersao sozinha =====\n")
for (h in c(1,3,6,12)) {
  m <- copy(disp); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  r <- fama_macbeth_linear(m, "cv_peso", "Dispersao (CV peso, simples) sozinha", h)
  if (!is.null(r)) resultados_lin[[length(resultados_lin)+1]] <- r
  r2 <- fama_macbeth_linear(m, "cv_peso_ponderado", "Dispersao (CV peso, ponderado) sozinha", h)
  if (!is.null(r2)) resultados_lin[[length(resultados_lin)+1]] <- r2
}

cat("\n===== TESTE 3: controle multivariado -- dispersao sobrevive controlando por breadth (n_fundos) e concentracao (hhi_posse)? =====\n")
for (h in c(1,3,6)) {
  m <- copy(disp); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  r <- fama_macbeth_linear(m, c("cv_peso_ponderado","n_fundos","hhi_posse"), "Dispersao + n_fundos + HHI (controlado)", h)
  if (!is.null(r)) resultados_lin[[length(resultados_lin)+1]] <- r
}

RQ <- rbindlist(resultados_qs, fill = TRUE)
RL <- rbindlist(resultados_lin, fill = TRUE)
fwrite(RQ, file.path(OUT, "candidatos_70_quintil.csv"))
fwrite(RL, file.path(OUT, "candidatos_70_linear.csv"))

cat("\n\n===== RESUMO QUINTIL (ordenado por p) =====\n")
print(RQ[order(p_fm)])
cat("\n\n===== RESUMO LINEAR (ordenado por p) =====\n")
print(RL[order(p_fm)])
cat("\nOK\n")
