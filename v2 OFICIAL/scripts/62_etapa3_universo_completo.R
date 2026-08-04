# =============================================================================
# 62_etapa3_universo_completo.R  (v2 OFICIAL -- teste_estrategia)
#
# FASE 3: Etapa 3 (ajuste parcial fora da amostra), h=1 e h=3, no universo
# completo -- mesmo esquema de sempre (lambda estimado SO no treino
# <2020-01, aplicado fixo ao teste >=2020-01), agora COM a correcao do
# efeito mecanico de preco (Fase 2, ja validada e limpa de outliers).
#
# Calcula erro_oos tanto BRUTO (dw normal, igual ao pipeline ja publicado)
# quanto CORRIGIDO (dw_corrigido, tira o efeito de preco) -- os dois lado a
# lado, pra Fase 4/5 usar o corrigido mas mantendo o bruto como comparacao/
# checagem de robustez.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")

pred <- fread(file.path(REPO, "v2 OFICIAL/data/peso_pred_universo_completo.csv"))
pred[, cod_fundo := as.character(cod_fundo)]
pred[, d := peso_pred - peso]
pred[, ticker := trimws(sub(".*- ", "", ativo))]

mapa <- fread(file.path(REPO, "v2 OFICIAL/data/universo_completo_gestora.csv"))
mapa[, cod_fundo := as.character(cod_fundo)]
pred <- merge(pred, mapa[, .(cod_fundo, gestora_grupo)], by = "cod_fundo", all.x = TRUE)

precos <- fread(file.path(REPO, "v2 OFICIAL/data/precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
setnames(precos, c("ymk","retorno"), c("ym_fut","r_ativo"))
rfundo <- fread(file.path(REPO, "v2 OFICIAL/data/retorno_fundo_mensal.csv"), select = c("cod_fundo","ymk","retorno_fundo"))
rfundo[, cod_fundo := as.character(cod_fundo)]
setnames(rfundo, "ymk", "ym_fut")

addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L
rmse <- function(x) sqrt(mean(x^2, na.rm = TRUE))

roda_horizonte <- function(h) {
  fut <- pred[, .(cod_fundo, ativo, ym_fut_key = ym, peso_fut = peso)]
  atual <- pred[, .(cod_fundo, gestora_grupo, ativo, ticker, ym, d, peso, ym_fut = addm(ym, h))]
  M <- merge(atual, fut, by.x = c("cod_fundo","ativo","ym_fut"), by.y = c("cod_fundo","ativo","ym_fut_key"))
  M[, dw := peso_fut - peso]

  M <- merge(M, precos, by = c("ticker","ym_fut"), all.x = TRUE)
  M <- merge(M, rfundo, by = c("cod_fundo","ym_fut"), all.x = TRUE)
  M[, peso_mecanico := peso * (1 + r_ativo) / (1 + retorno_fundo)]
  M[, dw_corrigido := peso_fut - peso_mecanico]

  treino <- M[ym < CORTE]
  teste  <- M[ym >= CORTE & ym_fut <= 202112L]
  cat(sprintf("\nh=%d: treino %d obs | teste %d obs | teste com dw_corrigido: %d (%.1f%%)\n",
              h, nrow(treino), nrow(teste), teste[!is.na(dw_corrigido),.N],
              100*teste[!is.na(dw_corrigido),.N]/nrow(teste)))

  fit_bruto <- lm(dw ~ 0 + d, data = treino)
  lam_bruto <- unname(coef(fit_bruto)["d"])
  fit_corr  <- lm(dw_corrigido ~ 0 + d, data = treino[!is.na(dw_corrigido)])
  lam_corr  <- unname(coef(fit_corr)["d"])
  cat(sprintf("lambda treino, h=%d -- bruto: %.4f | corrigido: %.4f\n", h, lam_bruto, lam_corr))

  teste[, erro_oos_bruto := dw - lam_bruto * d]
  teste[, erro_naive_bruto := dw]
  teste[, erro_oos_corrigido := dw_corrigido - lam_corr * d]
  teste[, erro_naive_corrigido := dw_corrigido]
  teste[, horizonte := h]
  teste[, `:=`(lambda_bruto = lam_bruto, lambda_corrigido = lam_corr)]
  teste
}

R1 <- roda_horizonte(1)
R3 <- roda_horizonte(3)

cat("\n===== RMSE fora da amostra, por ativo, h=1 (bruto vs corrigido) =====\n")
por_ativo1 <- R1[, .(n_obs = .N,
                      rmse_naive_bruto = rmse(erro_naive_bruto), rmse_oos_bruto = rmse(erro_oos_bruto),
                      rmse_naive_corr = rmse(erro_naive_corrigido), rmse_oos_corr = rmse(erro_oos_corrigido)),
                  by = ativo]
por_ativo1[, margem_bruto := 100*(rmse_naive_bruto-rmse_oos_bruto)/rmse_naive_bruto]
por_ativo1[, margem_corr  := 100*(rmse_naive_corr-rmse_oos_corr)/rmse_naive_corr]
elegivel <- por_ativo1[n_obs >= 100]
cat("Ativos com >=100 obs de teste:", nrow(elegivel), "\n")
cat("Mediana margem BRUTO:", round(median(elegivel$margem_bruto),2),
    "% | mediana margem CORRIGIDO:", round(median(elegivel$margem_corr, na.rm=TRUE),2), "%\n")
cat("Ajuste parcial vence ingenua (bruto):", round(100*mean(elegivel$margem_bruto>0),1),
    "% | (corrigido):", round(100*mean(elegivel$margem_corr>0, na.rm=TRUE),1), "%\n")

cat("\n===== VALE3 especifico, h=1 =====\n")
print(por_ativo1[ativo=="VALE ON N1 - VALE3"])

fwrite(R1, file.path(REPO, "v2 OFICIAL/data/etapa3_universo_completo_h1.csv"))
fwrite(R3, file.path(REPO, "v2 OFICIAL/data/etapa3_universo_completo_h3.csv"))
fwrite(por_ativo1, file.path(REPO, "v2 OFICIAL/data/etapa3_universo_completo_por_ativo_h1.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/etapa3_universo_completo_h1.csv' / '_h3.csv' / '_por_ativo_h1.csv'\n")
