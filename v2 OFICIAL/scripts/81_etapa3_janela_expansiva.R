# =============================================================================
# 81_etapa3_janela_expansiva.R  (v2 OFICIAL)
#
# Pedido do Joao: comparar o lambda fixo (15_recompute_downstream.R -- um
# unico corte, lambda estimado so no treino < 2020-01, aplicado fixo a
# todo o teste) contra um esquema de JANELA EXPANSIVA: para cada mes s de
# teste, reestima lambda_s usando todos os pares (d,dw) com mes-origem < s
# (ou seja, todo par cujo dw ja era conhecido antes de s -- sem
# vazamento), e usa esse lambda_s so para prever o mes s. Repete mes a mes.
#
# ATENCAO: usa v2_ajuste_parcial.csv (script 15 -- painel_predeterminado,
# COM beta do fundo, 59 meses, 8.335/7.987 obs), que e a base que de fato
# alimenta a Tabela "RMSE e MAE dentro e fora da amostra" do TCC_final.tex
# -- NAO ajuste_parcial_erros.csv (scripts 02/03/05, pipeline antigo sem
# beta, 72 meses), que e uma base legada que nao corresponde a nenhuma
# tabela do documento (confundida com a correta numa primeira tentativa;
# corrigido apos checar os numeros contra o que esta publicado).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

M <- fread(file.path(REPO, "v2 OFICIAL/data/v2_ajuste_parcial.csv"))
CORTE <- 202001L
meses_teste <- sort(unique(M[ym >= CORTE & ym < 202112L]$ym))
cat("Meses de teste:", length(meses_teste), "(", min(meses_teste), "a", max(meses_teste), ")\n")

rmse <- function(x) sqrt(mean(x^2))
mae  <- function(x) mean(abs(x))

resultado <- list(); lambdas <- list()
for (s in meses_teste) {
  treino_s <- M[ym < s]
  fit_s <- lm(dw ~ 0 + d, data = treino_s)
  lam_s <- unname(coef(fit_s)["d"])
  lambdas[[as.character(s)]] <- data.table(ym = s, n_treino = nrow(treino_s), lambda = lam_s)

  alvo_s <- M[ym == s]
  alvo_s[, dw_prev_expansiva := lam_s * d]
  alvo_s[, erro_expansiva := dw - dw_prev_expansiva]
  resultado[[as.character(s)]] <- alvo_s
}
R <- rbindlist(resultado)
LAM <- rbindlist(lambdas)

cat("\n===== Lambda mes a mes (janela expansiva) =====\n")
print(LAM)

treino_fixo <- M[ym < CORTE]
fit_fixo <- lm(dw ~ 0 + d, data = treino_fixo)
lam_fixo <- unname(coef(fit_fixo)["d"])
cat(sprintf("\nLambda fixo (treino < 2020-01, script 15) = %.4f\n", lam_fixo))
cat(sprintf("Lambda expansiva: min = %.4f | max = %.4f | ultimo mes = %.4f\n",
            min(LAM$lambda), max(LAM$lambda), LAM[ym == max(ym)]$lambda))

R[, dw_prev_fixo := lam_fixo * d]
R[, erro_fixo := dw - dw_prev_fixo]
R[, dw_prev_naive := 0]
R[, erro_naive := dw - dw_prev_naive]

cat("\n===== Desempenho FORA da amostra (2020-2021): fixo vs. expansiva vs. ingenua =====\n")
cat(sprintf("RMSE lambda fixo       = %.6f\n", rmse(R$erro_fixo)))
cat(sprintf("RMSE janela expansiva  = %.6f\n", rmse(R$erro_expansiva)))
cat(sprintf("RMSE ingenua           = %.6f\n", rmse(R$erro_naive)))
cat(sprintf("MAE  lambda fixo       = %.6f\n", mae(R$erro_fixo)))
cat(sprintf("MAE  janela expansiva  = %.6f\n", mae(R$erro_expansiva)))
cat(sprintf("MAE  ingenua           = %.6f\n", mae(R$erro_naive)))
cat(sprintf("\nRazao RMSE fixo/ingenua      = %.4f\n", rmse(R$erro_fixo)/rmse(R$erro_naive)))
cat(sprintf("Razao RMSE expansiva/ingenua = %.4f\n", rmse(R$erro_expansiva)/rmse(R$erro_naive)))

por_mes <- R[, .(rmse_fixo = rmse(erro_fixo), rmse_expansiva = rmse(erro_expansiva),
                  rmse_naive = rmse(erro_naive)), by = ym]
setorder(por_mes, ym)
por_mes[, vence_fixo := rmse_fixo < rmse_naive]
por_mes[, vence_expansiva := rmse_expansiva < rmse_naive]
cat(sprintf("\nFixo vence a ingenua em %d dos %d meses | Expansiva vence em %d dos %d meses\n",
            sum(por_mes$vence_fixo), nrow(por_mes), sum(por_mes$vence_expansiva), nrow(por_mes)))
print(por_mes[, .(ym, rmse_fixo = round(rmse_fixo,5), rmse_expansiva = round(rmse_expansiva,5),
                   rmse_naive = round(rmse_naive,5), vence_fixo, vence_expansiva)])

fwrite(R, file.path(REPO, "v2 OFICIAL/data/etapa3_janela_expansiva_resultado.csv"))
fwrite(LAM, file.path(REPO, "v2 OFICIAL/data/etapa3_janela_expansiva_lambdas.csv"))
fwrite(por_mes, file.path(REPO, "v2 OFICIAL/data/etapa3_janela_expansiva_por_mes.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/etapa3_janela_expansiva_*.csv'\n")
