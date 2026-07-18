# =============================================================================
# 05_in_out_sample.R  (v2 OFICIAL)
#
# ETAPA 4: esquema in/out-of-sample simples, um unico corte no tempo (nao e
# janela expansiva com muitas origens -- isso seria a complexidade que
# estamos evitando nesta versao).
#
#   IN-SAMPLE  (treino): 2016-01 a 2019-12 (48 meses)
#   OUT-OF-SAMPLE (teste): 2020-01 a 2021-12 (24 meses)
#
# lambda e estimado (MQO direto, igual Etapa 3) SO com os dados de treino.
# Esse lambda fixo e aplicado aos meses de teste para prever a variacao do
# peso; compara-se contra a previsao ingenua (nao muda nada, dw_previsto=0).
#
# theta_t (cross-section/logit) continua sendo estimado mes a mes de forma
# natural (nao ha vazamento: cada theta_t so usa dados do proprio mes t,
# dentro ou fora da amostra de treino do lambda).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

M <- fread(file.path(REPO, "v2 OFICIAL/data/ajuste_parcial_erros.csv"))

CORTE <- 202001L
treino <- M[ym < CORTE]
teste  <- M[ym >= CORTE]
cat(sprintf("Treino: %d obs (%d a %d) | Teste: %d obs (%d a %d)\n",
            nrow(treino), min(treino$ym), max(treino$ym),
            nrow(teste), min(teste$ym), max(teste$ym)))

# --- lambda estimado SO no treino ---
fit_tr <- lm(dw ~ 0 + d, data = treino)
lam_tr <- coef(fit_tr)["d"]
se_tr  <- summary(fit_tr)$coefficients["d","Std. Error"]
cat(sprintf("\nlambda (treino) = %.4f | EP = %.4f | t = %.2f\n",
            lam_tr, se_tr, lam_tr/se_tr))

# --- aplica lambda do treino ao teste ---
teste[, dw_prev_ajuste := lam_tr * d]
teste[, erro_ajuste := dw - dw_prev_ajuste]
teste[, dw_prev_naive := 0]
teste[, erro_naive := dw - dw_prev_naive]

rmse <- function(x) sqrt(mean(x^2))
mae  <- function(x) mean(abs(x))

cat("\n===== Desempenho FORA da amostra (2020-2021) =====\n")
cat(sprintf("RMSE ajuste parcial (lambda de treino) = %.6f\n", rmse(teste$erro_ajuste)))
cat(sprintf("RMSE ingenua (dw previsto = 0)          = %.6f\n", rmse(teste$erro_naive)))
cat(sprintf("MAE  ajuste parcial                     = %.6f\n", mae(teste$erro_ajuste)))
cat(sprintf("MAE  ingenua                            = %.6f\n", mae(teste$erro_naive)))
cat(sprintf("\nRazao RMSE (ajuste/ingenua) = %.4f  (< 1 = ajuste parcial vence)\n",
            rmse(teste$erro_ajuste)/rmse(teste$erro_naive)))

# --- checagem: desempenho DENTRO da amostra de treino, pra comparar ---
treino[, dw_prev_ajuste := lam_tr * d]
treino[, erro_ajuste := dw - dw_prev_ajuste]
cat("\n===== Para comparacao: desempenho DENTRO da amostra (treino) =====\n")
cat(sprintf("RMSE ajuste parcial (treino) = %.6f\n", rmse(treino$erro_ajuste)))
cat(sprintf("RMSE ingenua (treino)        = %.6f\n", rmse(treino$dw)))

# --- por mes, fora da amostra: o ajuste parcial vence em quantos dos 24 meses? ---
por_mes_teste <- teste[, .(rmse_ajuste = rmse(erro_ajuste), rmse_naive = rmse(erro_naive)), by = ym]
setorder(por_mes_teste, ym)
por_mes_teste[, vence_ajuste := rmse_ajuste < rmse_naive]
cat(sprintf("\nAjuste parcial vence a ingenua em %d dos %d meses de teste\n",
            sum(por_mes_teste$vence_ajuste), nrow(por_mes_teste)))
print(por_mes_teste[, .(ym, rmse_ajuste = round(rmse_ajuste,5), rmse_naive = round(rmse_naive,5), vence_ajuste)])

fwrite(teste, file.path(REPO, "v2 OFICIAL/data/out_of_sample_resultado.csv"))
fwrite(por_mes_teste, file.path(REPO, "v2 OFICIAL/data/out_of_sample_por_mes.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/out_of_sample_resultado.csv' e 'out_of_sample_por_mes.csv'\n")
