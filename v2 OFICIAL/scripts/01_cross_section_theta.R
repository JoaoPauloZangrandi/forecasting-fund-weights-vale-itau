# =============================================================================
# 01_cross_section_theta.R  (v2 OFICIAL)
#
# ETAPA 1: cross-section por MQO, mes a mes.
#
# Para cada mes t, regride o peso de VALE3 nas 4 caracteristicas do fundo
# (AUM, cotistas, FIC, fluxo liquido/AUM), separadamente por mes -- uma
# regressao MQO por mes, sem nenhuma etapa posterior de resumo temporal
# (nao e Fama-MacBeth: aqui so queremos o vetor theta_t de cada mes).
#
#   peso_{i,t} = a_t + b1_t*z(ln AUM) + b2_t*z(ln cotistas) + b3_t*FIC
#              + b4_t*(fluxo/AUM) + erro
#
# theta_t = (a_t, b1_t, b2_t, b3_t, b4_t) -- o vetor de coeficientes daquele
# mes. Theta (grande) = a colecao dos 72 theta_t, um por mes.
#
# Reaproveita o painel ja construido e validado (R/01-R/10 da v1); nao refaz
# a limpeza de dados, so a etapa de estimacao.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "data/processed/painel_vale_itau_2016_2021_filtrado.csv"))
d[, aum_num := as.numeric(aum)]
d[, l_aum   := log(aum_num)]
d[, l_cot   := log(n_cotistas)]
d[, flow_aum := fluxo_liq / aum_num]
d[, ym := ano * 100L + mes]
d <- d[!is.na(flow_aum) & !is.na(l_aum) & !is.na(l_cot)]

z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

meses <- sort(unique(d$ym))
cat("Meses:", length(meses), "(", min(meses), "-", max(meses), ") | fundo-meses totais:", nrow(d), "\n")

out <- vector("list", length(meses))
for (i in seq_along(meses)) {
  t  <- meses[i]
  dt <- d[ym == t]
  dt[, z_aum := z(l_aum)][, z_cot := z(l_cot)]
  f <- lm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum, data = dt)
  cf <- coef(f)
  out[[i]] <- data.table(ym = t, n_fundos = nrow(dt),
                          alpha = cf["(Intercept)"], b_aum = cf["z_aum"],
                          b_cot = cf["z_cot"], b_fic = cf["is_fic"],
                          b_flow = cf["flow_aum"], r2 = summary(f)$r.squared)
}
theta <- rbindlist(out)

cat("\n===== Theta_t: primeiros 6 meses =====\n")
print(theta[1:6, .(ym, n_fundos, alpha = round(alpha,5), b_aum = round(b_aum,5),
                    b_cot = round(b_cot,5), b_fic = round(b_fic,5),
                    b_flow = round(b_flow,5), r2 = round(r2,3))])

cat("\n===== Theta_t: estatisticas ao longo dos 72 meses (media, dp, min, max) =====\n")
resumo <- theta[, .(
  media = c(mean(alpha), mean(b_aum), mean(b_cot), mean(b_fic), mean(b_flow)),
  dp    = c(sd(alpha), sd(b_aum), sd(b_cot), sd(b_fic), sd(b_flow)),
  min   = c(min(alpha), min(b_aum), min(b_cot), min(b_fic), min(b_flow)),
  max   = c(max(alpha), max(b_aum), max(b_cot), max(b_fic), max(b_flow))
)]
resumo[, variavel := c("alpha (intercepto)", "b_aum", "b_cot", "b_fic", "b_flow")]
setcolorder(resumo, "variavel")
print(resumo[, .(variavel, media = round(media,5), dp = round(dp,5),
                  min = round(min,5), max = round(max,5))])

cat("\n===== R2 medio das 72 regressoes mensais:", round(mean(theta$r2),4), "=====\n")

fwrite(theta, file.path(REPO, "v2 OFICIAL/data/theta_mensal.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/theta_mensal.csv'\n")
