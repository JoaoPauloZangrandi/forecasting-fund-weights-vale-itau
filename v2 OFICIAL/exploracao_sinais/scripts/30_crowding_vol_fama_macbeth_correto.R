# =============================================================================
# 30_crowding_vol_fama_macbeth_correto.R  (exploracao_sinais)
#
# Refaz o controle (crowding + vol passada -> vol futura) com Fama-MacBeth
# DE VERDADE: regressao cross-sectional MES A MES (nao pooled), pega 1
# coeficiente de cada variavel por mes, testa a MEDIA desses coeficientes
# com o erro-padrao da propria serie temporal -- corrige a dependencia
# cross-sectional que o teste pooled (script 29) nao corrigia.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

setorder(precos, ticker, ymk)
precos[, vol_passada_12m := frollapply(retorno, 12, sd), by = ticker]

calc_vol_futura <- function(dt_precos, h) {
  pw <- copy(dt_precos)
  cols <- list()
  for (k in 1:h) {
    aux <- pw[, .(ticker, ym_base = addm(ymk, -k), ret_k = retorno)]
    setnames(aux, "ret_k", paste0("r",k))
    cols[[k]] <- aux
  }
  base <- Reduce(function(a,b) merge(a,b,by=c("ticker","ym_base"), all=TRUE), cols)
  ret_cols <- paste0("r",1:h)
  base[, vol_futura := apply(.SD, 1, sd, na.rm=TRUE), .SDcols = ret_cols]
  base[, n_obs_vol := apply(.SD, 1, function(x) sum(is.finite(x))), .SDcols = ret_cols]
  base <- base[n_obs_vol >= max(2,round(h*0.7))]
  base[, .(ticker, ym = ym_base, vol_futura)]
}

pp3 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp3[, ticker := trimws(sub(".*- ", "", ativo))]
pp3 <- pp3[is.finite(aum_prev) & aum_prev > 0]
pp3[, valor_posicao := peso * aum_prev]
crowd <- pp3[, .(hhi_posse = sum((valor_posicao/sum(valor_posicao))^2), n_fundos=.N), by = .(ticker, ym)]
crowd <- crowd[n_fundos >= 10]

fama_macbeth_regressao <- function(m, vars_x, nome, h) {
  teste <- m[ym >= CORTE]
  meses <- sort(unique(teste$ym))
  coefs <- list()
  for (mm in meses) {
    dm <- teste[ym == mm]
    if (nrow(dm) < 30) next
    form <- as.formula(paste("vol_futura ~", paste(vars_x, collapse=" + ")))
    fit <- tryCatch(lm(form, data = dm), error=function(e) NULL)
    if (is.null(fit)) next
    coefs[[as.character(mm)]] <- data.table(ym=mm, t(coef(fit)))
  }
  CM <- rbindlist(coefs, fill=TRUE)
  cat(sprintf("\n%s, h=%d -- %d meses de regressao cross-sectional valida\n", nome, h, nrow(CM)))
  resultado <- list()
  for (v in vars_x) {
    x <- CM[[v]]; x <- x[is.finite(x)]
    n <- length(x); media <- mean(x); dp <- sd(x)
    t_fm <- media/(dp/sqrt(n)); p_fm <- 2*pt(-abs(t_fm), df=n-1)
    cat(sprintf("  %-20s coef_medio=%+10.6f | t(FM)=%7.2f | p(FM)=%.5f | (%d meses)\n", v, media, t_fm, p_fm, n))
    resultado[[v]] <- data.table(sinal=nome, variavel=v, horizonte=h, n_meses=n, coef_medio=media, t_fm=t_fm, p_fm=p_fm)
  }
  rbindlist(resultado)
}

resultados <- list()
for (h in c(3,6,12)) {
  vol_fut <- calc_vol_futura(precos, h)
  m <- merge(crowd, precos[,.(ticker,ym=ymk,vol_passada_12m)], by=c("ticker","ym"))
  m <- merge(m, vol_fut, by=c("ticker","ym"))
  m <- m[is.finite(hhi_posse) & is.finite(vol_passada_12m) & is.finite(vol_futura)]
  cat(sprintf("\n\n========== h=%d (n=%d) ==========\n", h, nrow(m)))
  r1 <- fama_macbeth_regressao(m, "hhi_posse", "SO crowding", h)
  r2 <- fama_macbeth_regressao(m, "vol_passada_12m", "SO vol passada", h)
  r3 <- fama_macbeth_regressao(m, c("hhi_posse","vol_passada_12m"), "OS DOIS (Fama-MacBeth correto)", h)
  resultados[[length(resultados)+1]] <- r1
  resultados[[length(resultados)+1]] <- r2
  resultados[[length(resultados)+1]] <- r3
}

R <- rbindlist(resultados, fill=TRUE)
fwrite(R, file.path(OUT, "candidatos_30_fama_macbeth_correto.csv"))
cat("\n\n===== RESUMO FINAL: crowding sobrevive ao controle, com Fama-MacBeth de verdade? =====\n")
print(R[variavel=="hhi_posse"])
cat("\nOK\n")
