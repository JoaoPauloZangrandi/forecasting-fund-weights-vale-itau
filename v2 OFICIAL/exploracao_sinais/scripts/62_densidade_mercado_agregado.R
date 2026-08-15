# =============================================================================
# 62_densidade_mercado_agregado.R  (exploracao_sinais / agente_rede)
#
# ANGULO (c): densidade da rede CIO como um todo, por mes (media de TODAS as
# entradas fora da diagonal da matriz CIO -- proxy de "quao concentrado/
# crowded" esta o mercado inteiro), como preditor do retorno AGREGADO do
# mercado (equal-weighted) no mes seguinte.
#
# DIFERENCA METODOLOGICA IMPORTANTE em relacao ao resto da exploracao: aqui
# so' existe 1 observacao por MES (nao ha dimensao cross-sectional -- e' uma
# serie temporal pura de ~60 pontos). Fama-MacBeth (que exige um spread por
# mes entre GRUPOS de ativos) nao se aplica; o "N" relevante aqui e' o
# NUMERO DE MESES, nao o numero de ativo-mes -- poder estatistico
# estruturalmente muito mais fraco (n~35 treino / ~24 teste) do que os
# testes de painel. Reporto R2_OOS (mesma disciplina treino/teste do resto
# do TCC) e, para a regressao plena, erro-padrao robusto a autocorrelacao
# (Newey-West) -- correlacao serial em series temporais financeiras e'
# esperada (volatility clustering, etc.) e infla o t-stat de OLS ingenuo.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(sandwich); library(lmtest) })
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L

dens <- fread(file.path(OUT, "centralidade_densidade_mercado.csv"))
setorder(dens, ym)
cat("Meses com densidade calculada:", nrow(dens), "\n")
print(summary(dens$densidade))

# -----------------------------------------------------------------------------
# (1) OOS: densidade(t) -> retorno de mercado(t+1), mesma disciplina treino/teste
# -----------------------------------------------------------------------------
base <- dens[is.finite(densidade) & is.finite(ret_mercado_prox)]
treino <- base[ym < CORTE]; teste <- base[ym >= CORTE]
cat(sprintf("\nTreino: %d meses | Teste: %d meses\n", nrow(treino), nrow(teste)))

fit <- lm(ret_mercado_prox ~ densidade, data = treino)
alpha <- coef(fit)[1]; beta <- coef(fit)[2]
pred <- alpha + beta * teste$densidade
sse_m <- sum((teste$ret_mercado_prox - pred)^2)
sse_n <- sum((teste$ret_mercado_prox - mean(treino$ret_mercado_prox))^2)
r2_oos <- 1 - sse_m/sse_n
cat(sprintf("Densidade -> retorno mercado(t+1): beta_treino=%.4f | R2_OOS=%.3f%%\n", beta, 100*r2_oos))

# regressao plena (todo o periodo) com erro-padrao Newey-West (autocorrelacao)
fit_full <- lm(ret_mercado_prox ~ densidade, data = base)
nw <- tryCatch(coeftest(fit_full, vcov = NeweyWest(fit_full, lag = 3, prewhite = FALSE)), error = function(e) NULL)
cat("\nRegressao plena (amostra inteira, so' contexto -- NAO e' o teste OOS):\n")
print(summary(fit_full)$coefficients)
if (!is.null(nw)) { cat("\nMesma regressao, erro-padrao Newey-West (lag=3):\n"); print(nw) }

# -----------------------------------------------------------------------------
# (2) Densidade sobrevive controlando por retorno de mercado PASSADO (evita
#     confundir "densidade prevendo retorno" com "reversao/momentum do
#     proprio mercado", mesmo espirito do controle de vol.passada no
#     candidato de crowding->volatilidade, script 30)
# -----------------------------------------------------------------------------
base2 <- copy(base)
setorder(base2, ym)
base2[, ret_mercado_passado := shift(ret_mercado)]
base2 <- base2[is.finite(ret_mercado_passado)]
fit2 <- lm(ret_mercado_prox ~ densidade + ret_mercado_passado, data = base2[ym < CORTE])
cat("\n----- Controlando por retorno de mercado passado (so' treino, contexto) -----\n")
print(summary(fit2)$coefficients)

pred2 <- predict(fit2, newdata = base2[ym >= CORTE])
teste2 <- base2[ym >= CORTE]
sse_m2 <- sum((teste2$ret_mercado_prox - pred2)^2)
sse_n2 <- sum((teste2$ret_mercado_prox - mean(base2[ym < CORTE]$ret_mercado_prox))^2)
r2_oos2 <- 1 - sse_m2/sse_n2
cat(sprintf("\nDensidade + ret.passado -> R2_OOS=%.3f%% (vs %.3f%% so' densidade)\n", 100*r2_oos2, 100*r2_oos))

# -----------------------------------------------------------------------------
# (3) Correlacao contemporanea: densidade e' so' um proxy de correlacao/vol de
#     mercado (nao um sinal novo)? Se densidade(t) correlaciona muito com
#     retorno ABSOLUTO de mercado(t) (proxy de vol realizada do proprio mes),
#     isso sugere que a "densidade" capta principalmente regime de vol, nao
#     informacao nova sobre POSSE institucional.
# -----------------------------------------------------------------------------
cat("\n----- Densidade(t) x |retorno de mercado(t)| (vol contemporanea) -----\n")
cc <- cor(base$densidade, abs(base$ret_mercado), use = "complete.obs")
cat(sprintf("Correlacao (nivel): %.3f\n", cc))

resumo <- data.table(
  teste = c("Densidade sozinha", "Densidade + ret.passado"),
  n_treino = c(nrow(treino), nrow(base2[ym<CORTE])),
  n_teste = c(nrow(teste), nrow(teste2)),
  r2_oos_pct = c(100*r2_oos, 100*r2_oos2)
)
fwrite(resumo, file.path(OUT, "candidatos_62_densidade_mercado.csv"))
cat("\nOK\n")
