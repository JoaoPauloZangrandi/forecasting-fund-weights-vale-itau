# =============================================================================
# 02_logit_erro.R  (v2 OFICIAL)
#
# ETAPA 2: reestima theta_t por LOGIT (resposta fracionaria, quasibinomial),
# mes a mes -- nao reaproveita o theta LINEAR da Etapa 1, porque um theta
# calibrado para reta nao e compativel com a escala da logistica (ver
# diagnostico: g(z) perto de z=0 da 0,5, mas o peso real e ~1-5%, entao
# aplicar g() num theta linear so produzia erro sistematico de ~-0,48).
#
#   peso_{i,t} = g(x_{i,t}' theta_t) + erro,   g(z) = 1/(1+exp(-z))
#
# Estimado por glm(family=quasibinomial), um por mes -- ainda um unico
# metodo, sem nenhuma correcao de erro-padrao em cima (isso fica pra depois,
# se for o caso). theta_t aqui esta em unidades de log-odds (nota z), nao
# pontos percentuais diretos -- e o mesmo principio da "forma logistica" da
# v1, so que theta e re-simulado mes a mes, sem framing de Fama-MacBeth.
#
#   peso_previsto_{i,t} = g(x_{i,t}' theta_t)
#   e_{i,t} = peso_{i,t} - peso_previsto_{i,t}
#
# Contemporaneo/dentro da amostra (usa o theta do proprio mes -- e o erro de
# ajuste, nao previsao fora da amostra).
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
# resposta fracionaria exige y em [0,1]; peso ja e uma fracao, ok.

z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

meses <- sort(unique(d$ym))
th_out <- vector("list", length(meses))
er_out <- vector("list", length(meses))

for (i in seq_along(meses)) {
  t  <- meses[i]
  dt <- d[ym == t]
  dt[, z_aum := z(l_aum)][, z_cot := z(l_cot)]
  f <- glm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum, family = quasibinomial(link = "logit"), data = dt)
  cf <- coef(f)
  th_out[[i]] <- data.table(ym = t, n_fundos = nrow(dt),
                             alpha = cf["(Intercept)"], b_aum = cf["z_aum"],
                             b_cot = cf["z_cot"], b_fic = cf["is_fic"], b_flow = cf["flow_aum"])
  dt[, peso_prev := fitted(f)]
  dt[, e := peso_vale3 - peso_prev]
  er_out[[i]] <- dt[, .(cod_fundo, ym, peso_vale3, peso_prev, e)]
}
theta_logit <- rbindlist(th_out)
E <- rbindlist(er_out)

cat("===== Theta_t (logit): primeiros 6 meses =====\n")
print(theta_logit[1:6, .(ym, n_fundos, alpha = round(alpha,4), b_aum = round(b_aum,4),
                          b_cot = round(b_cot,4), b_fic = round(b_fic,4), b_flow = round(b_flow,4))])

cat("\n===== Theta_t (logit): media ao longo dos 72 meses =====\n")
cat(sprintf("alpha=%.4f  b_aum=%.4f  b_cot=%.4f  b_fic=%.4f  b_flow=%.4f\n",
            mean(theta_logit$alpha), mean(theta_logit$b_aum), mean(theta_logit$b_cot),
            mean(theta_logit$b_fic), mean(theta_logit$b_flow)))

cat("\n===== Erro e: estatisticas gerais (", nrow(E), "fundo-meses ) =====\n")
cat(sprintf("media(e)     = %.6f\n", mean(E$e)))
cat(sprintf("dp(e)        = %.6f\n", sd(E$e)))
cat(sprintf("min(e)       = %.6f\n", min(E$e)))
cat(sprintf("max(e)       = %.6f\n", max(E$e)))
cat(sprintf("mediana(e)   = %.6f\n", median(E$e)))
cat(sprintf("%% de e > 0   = %.1f%%\n", 100*mean(E$e > 0)))

cat("\n===== Previsao (logistica) sempre em [0,1]? =====\n")
cat("peso_prev fora de [0,1]:", sum(E$peso_prev < 0 | E$peso_prev > 1), "de", nrow(E), "\n")

cat("\n===== Erro e: media e dp POR MES (primeiros 12 meses) =====\n")
por_mes <- E[, .(media_e = mean(e), dp_e = sd(e), n = .N), by = ym]
print(por_mes[1:12, .(ym, media_e = round(media_e,6), dp_e = round(dp_e,6), n)])

cat("\n===== Erro e: estatisticas da serie mensal (media entre fundos por mes) =====\n")
cat(sprintf("media da media mensal   = %.6f\n", mean(por_mes$media_e)))
cat(sprintf("dp da media mensal      = %.6f\n", sd(por_mes$media_e)))
cat(sprintf("min/max da media mensal = %.6f / %.6f\n", min(por_mes$media_e), max(por_mes$media_e)))
cat(sprintf("autocorrelacao lag-1 da media mensal = %.3f\n",
            cor(por_mes$media_e[-nrow(por_mes)], por_mes$media_e[-1])))

fwrite(theta_logit, file.path(REPO, "v2 OFICIAL/data/theta_logit_mensal.csv"))
fwrite(E, file.path(REPO, "v2 OFICIAL/data/erro_cross_section.csv"))
fwrite(por_mes, file.path(REPO, "v2 OFICIAL/data/erro_cross_section_por_mes.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/theta_logit_mensal.csv' e 'erro_cross_section*.csv'\n")
