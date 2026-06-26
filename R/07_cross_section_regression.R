# =============================================================================
# 07_cross_section_regression.R
#
# Passo 7 (PASSO 1 da metodologia do Maurico): regressao do peso de VALE3 nas
# caracteristicas. Duas versoes:
#   (A) CROSS-SECTION mes a mes  -> coeficientes theta_t (o "fator latente")
#       + matriz de residuos (fundo x mes). Preco/beta NAO entram (constantes
#       no mes). Eles vao para o Passo 2 (dinamica de theta_t), depois.
#   (B) POOLED (painel empilhado) -> caracteristicas de fundo + preco + beta
#       juntos (preco/beta identificados pela variacao no tempo).
#
# Amostra: painel filtrado n_cotistas > 3 (passo 6).
# Tratamentos: log(aum), log(n_cotistas), is_fic dummy, fluxo_liq/aum.
# =============================================================================

suppressPackageStartupMessages({ library(data.table) })

PROJ_DIR <- Sys.getenv("PROJ_DIR",
                       unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
if (dir.exists(PROJ_DIR)) setwd(PROJ_DIR)

d <- fread("data/processed/painel_vale_itau_2016_filtrado_cotistas_gt3.csv")

# ---- transformacoes ---------------------------------------------------------
d[, aum_num   := as.numeric(aum)]
d[, l_aum     := log(aum_num)]
d[, l_cot     := log(n_cotistas)]
d[, flow_aum  := fluxo_liq / aum_num]
stopifnot(!anyNA(d$l_aum), !anyNA(d$l_cot), !anyNA(d$flow_aum))

fml_cs <- peso_vale3 ~ l_aum + l_cot + is_fic + flow_aum

# ---- (A) CROSS-SECTION mes a mes -------------------------------------------
meses <- sort(unique(d$mes))
coefs <- vector("list", length(meses)); resids <- vector("list", length(meses))
for (k in seq_along(meses)) {
  dm  <- d[mes == meses[k]]
  fit <- lm(fml_cs, data = dm)
  cf  <- coef(fit)
  coefs[[k]] <- data.table(mes = meses[k], n = nrow(dm),
                           r2 = summary(fit)$r.squared,
                           intercepto = cf[["(Intercept)"]],
                           b_l_aum = cf[["l_aum"]], b_l_cot = cf[["l_cot"]],
                           b_is_fic = cf[["is_fic"]], b_flow_aum = cf[["flow_aum"]])
  resids[[k]] <- data.table(cod_fundo = dm$cod_fundo, mes = meses[k],
                            resid = residuals(fit))
}
coefs  <- rbindlist(coefs)
resids <- rbindlist(resids)

cat("=========== (A) CROSS-SECTION mes a mes: coeficientes theta_t ===========\n")
print(coefs[, lapply(.SD, function(x) if (is.double(x)) round(x, 5) else x)])
cat("\n--- media e estabilidade de sinal dos coeficientes (12 meses) ---\n")
sumtab <- coefs[, .(media = sapply(.SD, mean), `>0` = sapply(.SD, function(x) sum(x > 0))),
                .SDcols = c("intercepto","b_l_aum","b_l_cot","b_is_fic","b_flow_aum")]
sumtab[, variavel := c("intercepto","b_l_aum","b_l_cot","b_is_fic","b_flow_aum")]
print(sumtab[, .(variavel, media = round(media, 6), meses_positivos = `>0`)])

cat("\n--- matriz de erros (residuos fundo x mes): propriedades ---\n")
cat("media do residuo por mes (deve ser ~0 por OLS):\n")
print(resids[, .(media_resid = round(mean(resid), 8)), by = mes][order(mes)])
cat("residuo geral: media =", round(mean(resids$resid), 8),
    "| desvio-padrao =", round(sd(resids$resid), 6), "\n")

# ---- (B) POOLED (com preco e beta) -----------------------------------------
cat("\n=========== (B) POOLED (fundo + preco + beta) ===========\n")
fitp <- lm(peso_vale3 ~ l_aum + l_cot + is_fic + flow_aum + preco_nominal + beta_vale,
           data = d)
print(summary(fitp))

# ---- salvar -----------------------------------------------------------------
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
fwrite(coefs,  "data/processed/reg07_cross_section_coefs_2016.csv")
fwrite(resids, "data/processed/reg07_cross_section_resid_2016.csv")
cat("\nOK - coeficientes e residuos salvos em data/processed/reg07_*.csv\n")
