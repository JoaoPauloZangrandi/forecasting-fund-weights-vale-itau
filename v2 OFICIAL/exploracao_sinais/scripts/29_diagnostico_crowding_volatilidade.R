# =============================================================================
# 29_diagnostico_crowding_volatilidade.R  (exploracao_sinais)
#
# O candidato #28 achou: CROWDING (HHI de posse) prediz volatilidade
# futura com t=8,67 (h=6) e t=14,03 (h=12) -- passa Bonferroni, o
# PRIMEIRO resultado de toda a exploracao a fazer isso.
#
# TESTE DECISIVO antes de acreditar: volatilidade e' autocorrelacionada
# (fato estilizado bem conhecido, "volatility clustering"). Sera que
# crowding so' esta capturando volatilidade PASSADA/ATUAL da propria
# acao (que por sua vez prediz volatilidade futura por inercia pura, nao
# por causa do crowding)? Testa:
#  (1) Crowding sozinho (ja feito, replicado aqui pra comparar)
#  (2) Vol passada sozinha (benchmark de autocorrelacao pura)
#  (3) Os DOIS juntos -- crowding sobrevive controlando por vol passada?
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/380

setorder(precos, ticker, ymk)
# vol PASSADA (ultimos 12 meses, ate o mes t) -- mesma janela de referencia que crowding
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

cat("===== Correlacao crowding vs vol PASSADA (contemporaneo) =====\n")
base_corr <- merge(crowd, precos[,.(ticker,ym=ymk,vol_passada_12m)], by=c("ticker","ym"))
base_corr <- base_corr[is.finite(hhi_posse) & is.finite(vol_passada_12m)]
cat("Correlacao (Pearson):", round(cor(base_corr$hhi_posse, base_corr$vol_passada_12m),4), "\n")
cat("Correlacao (Spearman):", round(cor(base_corr$hhi_posse, base_corr$vol_passada_12m, method="spearman"),4), "\n\n")

testa_multi <- function(m, vars_x, nome, h) {
  treino <- m[ym < CORTE]; teste <- copy(m[ym >= CORTE])
  if (nrow(treino) < 100 || nrow(teste) < 50) return(NULL)
  form <- as.formula(paste("vol_futura ~", paste(vars_x, collapse=" + ")))
  fit_lm <- lm(form, data = treino)
  pred <- predict(fit_lm, newdata = teste)
  sse_m <- sum((teste$vol_futura-pred)^2); sse_n <- sum((teste$vol_futura-mean(treino$vol_futura))^2)
  r2_oos <- 1 - sse_m/sse_n
  s_full <- summary(lm(form, data=m))$coefficients
  cat(sprintf("\n--- %s, h=%d --- (n_tr=%d n_te=%d, R2_OOS=%.3f%%)\n", nome, h, nrow(treino), nrow(teste), 100*r2_oos))
  print(round(s_full,6))
  data.table(nome=nome, horizonte=h, r2_oos_pct=100*r2_oos)
}

resultados <- list()
for (h in c(3,6,12)) {
  vol_fut <- calc_vol_futura(precos, h)
  m <- merge(crowd, precos[,.(ticker,ym=ymk,vol_passada_12m)], by=c("ticker","ym"))
  m <- merge(m, vol_fut, by=c("ticker","ym"))
  m <- m[is.finite(hhi_posse) & is.finite(vol_passada_12m) & is.finite(vol_futura)]
  cat(sprintf("\n\n========== HORIZONTE h=%d (n=%d obs) ==========\n", h, nrow(m)))

  r1 <- testa_multi(m, "hhi_posse", "(1) SO crowding", h)
  r2 <- testa_multi(m, "vol_passada_12m", "(2) SO vol passada", h)
  r3 <- testa_multi(m, c("hhi_posse","vol_passada_12m"), "(3) OS DOIS JUNTOS", h)
  for (r in list(r1,r2,r3)) if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

R <- rbindlist(resultados, fill=TRUE)
fwrite(R, file.path(OUT, "candidatos_29_diagnostico_crowding_vol.csv"))
cat("\n\nOK\n")
