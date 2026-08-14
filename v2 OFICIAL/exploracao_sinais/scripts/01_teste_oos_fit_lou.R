# =============================================================================
# 01_teste_oos_fit_lou.R  (exploracao_sinais -- NAO faz parte do TCC ainda)
#
# O script 105 (v2 OFICIAL/scripts) estimou FIT (Flow-Induced Trading, Lou
# 2012) e testou retorno ~ FIT numa unica regressao com a amostra INTEIRA --
# olhando os t-stats (-5 a -6, p<0.00001), parece muito mais forte que o
# "sinal do robo" ja testado e rejeitado no TCC (que tinha p entre 0,03 e
# 0,74). MAS: (1) nao e fora da amostra, exatamente o vies que o resto do
# documento tomou cuidado de evitar; (2) o padrao entre horizontes e
# estranho -- forte em h=0/h=1, some em h=3 (t=-0.09!), volta em h=6/h=12.
# Isso nao bate com nenhuma historia economica limpa (nem pressao-e-reversao
# classica, que preveria decaimento suave, nem persistencia).
#
# Este script faz o teste de verdade: replica a disciplina fora da amostra
# usada em TODO o resto do TCC (treino <2020-01, teste >=2020-01, sem
# reestimar nada no teste) e investiga se o padrao e robusto a outliers.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

fit <- fread(file.path(DATA, "fit_ativo_mes.csv"))
precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
fit[, ticker := trimws(sub(".*- ", "", ativo))]

addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

roda_oos <- function(h) {
  f2 <- copy(fit)
  f2[, ym_ret := addm(ym, h)]
  m <- merge(f2, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(FIT)]

  treino <- m[ym < CORTE]
  teste  <- m[ym >= CORTE]
  if (nrow(treino) < 100 || nrow(teste) < 50) {
    cat(sprintf("h=%2d: treino/teste pequeno demais (treino=%d, teste=%d), pulando\n", h, nrow(treino), nrow(teste)))
    return(NULL)
  }

  fit_lm <- lm(retorno ~ FIT, data = treino)
  alpha <- coef(fit_lm)[1]; beta <- coef(fit_lm)[2]
  teste[, pred := alpha + beta * FIT]
  teste[, erro_modelo := retorno - pred]
  teste[, erro_naive := retorno - mean(treino$retorno)]  # ingenua: media do treino, nao a media do proprio teste (evita vazamento)

  rmse <- function(x) sqrt(mean(x^2))
  rmse_modelo <- rmse(teste$erro_modelo)
  rmse_naive  <- rmse(teste$erro_naive)
  margem <- 100*(rmse_naive - rmse_modelo)/rmse_naive

  # R2 fora da amostra "de verdade" (nao o R2 do treino) -- pode ser NEGATIVO
  # se o modelo piorar em relacao a so' prever a media do treino
  sse_modelo <- sum(teste$erro_modelo^2)
  sse_naive  <- sum(teste$erro_naive^2)
  r2_oos <- 1 - sse_modelo/sse_naive

  cat(sprintf("h=%2d: n_treino=%6d n_teste=%5d | beta(treino)=%9.5f | RMSE modelo=%.5f RMSE ingenua=%.5f | margem=%6.2f%% | R2_OOS=%7.4f%%\n",
              h, nrow(treino), nrow(teste), beta, rmse_modelo, rmse_naive, margem, 100*r2_oos))
  data.table(horizonte = h, n_treino = nrow(treino), n_teste = nrow(teste), beta_treino = beta,
             rmse_modelo = rmse_modelo, rmse_naive = rmse_naive, margem_pct = margem, r2_oos_pct = 100*r2_oos)
}

cat("===== TESTE FORA DA AMOSTRA: FIT prediz retorno? (treino<2020-01, teste>=2020-01) =====\n")
resumo <- rbindlist(lapply(c(0,1,3,6,12), roda_oos))
fwrite(resumo, file.path(OUT, "fit_oos_resumo.csv"))

cat("\n===== SENSIBILIDADE A OUTLIERS: mesma coisa, winsorizando FIT e retorno em 1%/99% =====\n")
roda_oos_winsor <- function(h) {
  f2 <- copy(fit)
  f2[, ym_ret := addm(ym, h)]
  m <- merge(f2, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(FIT)]
  qf <- quantile(m$FIT, c(0.01,0.99)); qr <- quantile(m$retorno, c(0.01,0.99))
  m[, FIT_w := pmin(pmax(FIT, qf[1]), qf[2])]
  m[, retorno_w := pmin(pmax(retorno, qr[1]), qr[2])]

  treino <- m[ym < CORTE]; teste <- m[ym >= CORTE]
  if (nrow(treino) < 100 || nrow(teste) < 50) return(NULL)
  fit_lm <- lm(retorno_w ~ FIT_w, data = treino)
  alpha <- coef(fit_lm)[1]; beta <- coef(fit_lm)[2]
  teste[, pred := alpha + beta * FIT_w]
  teste[, erro_modelo := retorno_w - pred]
  teste[, erro_naive := retorno_w - mean(treino$retorno_w)]
  sse_modelo <- sum(teste$erro_modelo^2); sse_naive <- sum(teste$erro_naive^2)
  r2_oos <- 1 - sse_modelo/sse_naive
  s <- summary(lm(retorno_w ~ FIT_w, data = m))$coefficients
  cat(sprintf("h=%2d (winsorizado): beta(treino)=%9.5f | R2_OOS=%7.4f%% | [amostra inteira, so' referencia] coef=%.6f t=%.3f p=%.4f\n",
              h, beta, 100*r2_oos, s[2,1], s[2,3], s[2,4]))
}
invisible(lapply(c(0,1,6,12), roda_oos_winsor))

cat("\nOK\n")
