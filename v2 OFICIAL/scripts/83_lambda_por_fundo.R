# =============================================================================
# 83_lambda_por_fundo.R  (v2 OFICIAL)
#
# Pergunta do Joao: separar lambda por fundo mudaria muito? Nao seria mais
# "correto"? Testa isso na base VALE3-Itau (v2_ajuste_parcial.csv, com
# beta do fundo, 59 meses) -- estima um lambda_i por fundo (onde ha
# observacoes suficientes), compara com o lambda pooled, e mede o
# desempenho fora da amostra de cada abordagem.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

M <- fread(file.path(REPO, "v2 OFICIAL/data/v2_ajuste_parcial.csv"))
n_por_fundo <- M[, .N, by = cod_fundo]
cat("Fundos distintos:", nrow(n_por_fundo), "\n")
cat("N de pares (t,t+1) por fundo -- resumo:\n")
print(summary(n_por_fundo$N))
cat("\nDistribuicao (faixas de N):\n")
print(table(cut(n_por_fundo$N, breaks = c(0,5,10,20,40,100), right = TRUE)))

CORTE <- 202001L
treino <- M[ym < CORTE]
teste  <- M[ym >= CORTE & ym < 202112L]

rmse <- function(x) sqrt(mean(x^2))

cat("\n===== lambda pooled (referencia, ja no documento) =====\n")
fit_pool <- lm(dw ~ 0 + d, data = treino)
lam_pool <- unname(coef(fit_pool)["d"])
cat("lambda pooled =", round(lam_pool,4), "\n")
teste[, erro_pool := dw - lam_pool*d]
cat("RMSE teste (pooled) =", round(rmse(teste$erro_pool),6), "\n")

cat("\n===== lambda por fundo (so treino, MINIMO de 10 pares por fundo) =====\n")
n_treino_fundo <- treino[, .N, by = cod_fundo]
fundos_ok <- n_treino_fundo[N >= 10]$cod_fundo
cat("Fundos com >=10 pares no treino:", length(fundos_ok), "de", nrow(n_treino_fundo), "\n")

lam_fundo <- treino[cod_fundo %in% fundos_ok, {
  f <- lm(dw ~ 0 + d)
  se <- summary(f)$coefficients["d","Std. Error"]
  .(lambda_i = unname(coef(f)["d"]), se_i = se, n = .N)
}, by = cod_fundo]

cat("\nDistribuicao de lambda_i (", nrow(lam_fundo), "fundos):\n")
print(summary(lam_fundo$lambda_i))
cat("dp(lambda_i) =", round(sd(lam_fundo$lambda_i),4), "\n")
cat("Fundos com lambda_i negativo:", sum(lam_fundo$lambda_i < 0), "de", nrow(lam_fundo),
    sprintf("(%.1f%%)\n", 100*mean(lam_fundo$lambda_i < 0)))
cat("Erro-padrao medio de lambda_i:", round(mean(lam_fundo$se_i),4),
    "| lambda pooled (referencia):", round(lam_pool,4), "\n")
cat("Fundos cujo t-valor de lambda_i e significativo (|t|>1.96):",
    sum(abs(lam_fundo$lambda_i/lam_fundo$se_i) > 1.96), "de", nrow(lam_fundo), "\n")

cat("\n===== desempenho FORA da amostra: lambda por fundo vs pooled =====\n")
teste_ok <- merge(teste, lam_fundo[, .(cod_fundo, lambda_i)], by = "cod_fundo")
cat("Obs de teste cobertas por fundos com lambda_i estimavel:", nrow(teste_ok), "de", nrow(teste), "\n")
teste_ok[, erro_fundo := dw - lambda_i*d]
teste_ok[, erro_pool_mesmo := dw - lam_pool*d]
cat("RMSE teste, so nesses fundos: lambda por fundo =", round(rmse(teste_ok$erro_fundo),6),
    "| pooled =", round(rmse(teste_ok$erro_pool_mesmo),6), "\n")

cat("\n===== lambda_i vs caracteristicas do fundo (correlacao simples) =====\n")
painel <- fread(file.path(REPO, "v2 OFICIAL/data/painel_predeterminado.csv"))
painel[, l_aum := log(aum_prev)]
carac <- painel[ym < CORTE, .(l_aum = mean(l_aum, na.rm=TRUE), beta_fundo = mean(beta_fundo, na.rm=TRUE)), by = cod_fundo]
lam_carac <- merge(lam_fundo, carac, by = "cod_fundo")
cat("correlacao lambda_i x tamanho (l_aum):", round(cor(lam_carac$lambda_i, lam_carac$l_aum, use="complete.obs"),4), "\n")
cat("correlacao lambda_i x beta do fundo:", round(cor(lam_carac$lambda_i, lam_carac$beta_fundo, use="complete.obs"),4), "\n")

fwrite(lam_fundo, file.path(REPO, "v2 OFICIAL/data/lambda_por_fundo.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/lambda_por_fundo.csv'\n")
