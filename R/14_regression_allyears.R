# =============================================================================
# 14_regression_allyears.R
#
# Passo 6 (filtro n_cotistas > 3) + Passo 7 (regressao) para 2016-2021 (72 meses).
#   (A) Cross-section MES A MES -> 72 vetores theta_t (fator latente) + residuos.
#       Resumo Fama-MacBeth (media dos 72 coefs, EP entre meses, t, p).
#   (B) Pooled (com preco/beta).
# Tratamentos: log(aum), log(n_cotistas), is_fic, fluxo_liq/aum.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================

suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "data/processed/painel_vale_itau_2016_2021_full.csv"))
d <- d[n_cotistas > 3]                                  # Passo 6
fwrite(d, file.path(REPO, "data/processed/painel_vale_itau_2016_2021_filtrado.csv"))

d[, aum_num  := as.numeric(aum)]
d[, l_aum    := log(aum_num)]
d[, l_cot    := log(n_cotistas)]
d[, flow_aum := fluxo_liq / aum_num]
d[, ym := ano * 100L + mes]
cat("Obs filtradas:", nrow(d), "| com fluxo (p/ regressao):", d[!is.na(flow_aum), .N], "\n")

fml <- peso_vale3 ~ l_aum + l_cot + is_fic + flow_aum
meses <- sort(unique(d$ym))
coefs <- vector("list", length(meses)); resids <- vector("list", length(meses))
for (k in seq_along(meses)) {
  dm  <- d[ym == meses[k] & !is.na(flow_aum)]
  fit <- lm(fml, data = dm); cf <- coef(fit)
  coefs[[k]] <- data.table(ym = meses[k], ano = dm$ano[1], mes = dm$mes[1],
                           n = nrow(dm), r2 = summary(fit)$r.squared,
                           intercepto = cf[["(Intercept)"]], b_l_aum = cf[["l_aum"]],
                           b_l_cot = cf[["l_cot"]], b_is_fic = cf[["is_fic"]],
                           b_flow_aum = cf[["flow_aum"]])
  resids[[k]] <- data.table(cod_fundo = dm$cod_fundo, ym = meses[k], resid = residuals(fit))
}
coefs  <- rbindlist(coefs); resids <- rbindlist(resids)

cat("\n==== (A) FAMA-MACBETH (72 meses) ====\n")
vars <- c("intercepto","b_l_aum","b_l_cot","b_is_fic","b_flow_aum")
fm <- rbindlist(lapply(vars, function(v) {
  x <- coefs[[v]]; nM <- length(x); m <- mean(x); se <- sd(x)/sqrt(nM); tt <- m/se
  data.table(variavel = v, media = m, fm_se = se, t = tt, p = 2*pt(-abs(tt), df = nM-1),
             meses_pos = sum(x > 0))
}))
print(fm)
cat("R2 medio:", round(mean(coefs$r2), 4), "| meses:", nrow(coefs), "\n")

cat("\n==== (B) POOLED (com preco/beta) ====\n")
fitp <- lm(peso_vale3 ~ l_aum + l_cot + is_fic + flow_aum + preco_nominal + beta_vale, data = d)
print(round(summary(fitp)$coefficients, 6))
cat("R2:", round(summary(fitp)$r.squared, 4), "| n:", nobs(fitp), "\n")

fwrite(coefs,  file.path(REPO, "data/processed/reg14_coefs_2016_2021.csv"))
fwrite(resids, file.path(REPO, "data/processed/reg14_resid_2016_2021.csv"))
cat("\nOK - coefs (72 meses) e residuos salvos em data/processed/reg14_*.csv\n")
