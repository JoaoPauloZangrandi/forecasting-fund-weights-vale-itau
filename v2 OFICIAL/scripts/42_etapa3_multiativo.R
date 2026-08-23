# =============================================================================
# !!! SUPERADO (12/08/2026) -- NAO RODAR SEM QUERER !!!
# Versao de 5 caracteristicas (sem HHI_resto) -- le erro_e_multiativo.csv da
# epoca de 5 caracteristicas. O script 99_pipeline_hhi_lag_etapa1.R e quem
# produz oficialmente etapa3_multiativo_h1.csv/h3.csv hoje (6 caracteristicas,
# HHI_resto defasado). Este script escreve NOS MESMOS nomes de arquivo --
# rodar por engano sobrescreve silenciosamente o resultado atual com a
# versao antiga, sem erro nem aviso. Mantido so por referencia
# historica/comparacao. Se precisar rerodar de verdade, rode 99, nao este.
# =============================================================================
# 42_etapa3_multiativo.R  (v2 OFICIAL)
#
# Generaliza a Etapa 3 (ajuste parcial fora da amostra, scripts 03/05/36,
# ate aqui so VALE3-Itau) para o painel multiativo inteiro (430 ativos, 41
# gestoras) -- pedido do Joao: "erro de todas as gestoras pra cada acao pra
# fazer a etapa 3" + "pegar os erros fora da amostra pra vale3 pra 3 meses
# de lag, quais fundos/gestoras erram mais e erram menos" + "etapa 3 pra
# todas as gestoras e todos os ativos, testar quais ativos com mais e
# menos erros".
#
# Reaproveita a base ja existente (R/37, erro_e_multiativo.csv): peso e
# peso_pred (o alvo w*) por (fundo, ativo, mes), da regressao por celula
# ativo-mes da Secao 6.1 -- nao precisa reestimar nada, so falta construir
# d = peso_pred - peso e dw_h = peso(t+h) - peso(t), no mesmo esquema de
# ajuste parcial de sempre (MQO direto, lambda unico estimado so no treino,
# aplicado ao teste, corte < 2020-01 vs >= 2020-01).
#
# Roda para h=1 (Etapa 3 principal, todos os ativos) e h=3 (o pedido
# especifico do Joao para VALE3, mas calculado pra todos os ativos de
# qualquer jeito -- nao custa nada a mais).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

E <- fread(file.path(REPO, "v2 OFICIAL/data/erro_e_multiativo.csv"))
E[, d := peso_pred - peso]
cat("Base (erro_e_multiativo):", nrow(E), "obs |", uniqueN(E$ativo), "ativos |",
    uniqueN(E$gestora_grupo), "gestoras |", uniqueN(E$cod_fundo), "fundos\n")

addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L
rmse <- function(x) sqrt(mean(x^2))

roda_horizonte <- function(h) {
  fut <- E[, .(cod_fundo, ativo, ym_fut_key = ym, peso_fut = peso)]
  atual <- E[, .(cod_fundo, gestora_grupo, ativo, ym, d, peso, ym_fut = addm(ym, h))]
  M <- merge(atual, fut, by.x = c("cod_fundo","ativo","ym_fut"),
             by.y = c("cod_fundo","ativo","ym_fut_key"))
  M[, dw := peso_fut - peso]

  treino <- M[ym < CORTE]
  teste  <- M[ym >= CORTE & ym_fut <= 202112L]
  cat(sprintf("\nh=%d: treino %d obs | teste %d obs\n", h, nrow(treino), nrow(teste)))

  fit <- lm(dw ~ 0 + d, data = treino)
  lam <- unname(coef(fit)["d"])
  cat(sprintf("lambda (treino, h=%d) = %.4f\n", h, lam))

  teste[, erro_oos := dw - lam * d]
  teste[, erro_naive := dw]
  teste[, horizonte := h]
  teste
}

R1 <- roda_horizonte(1)
R3 <- roda_horizonte(3)

cat("\n===== C3: Etapa 3 generalizada, h=1, ranqueado por ATIVO =====\n")
por_ativo <- R1[, .(n_obs = .N, rmse_oos = rmse(erro_oos), rmse_naive = rmse(erro_naive)), by = ativo]
por_ativo[, margem_pct := 100*(rmse_naive - rmse_oos)/rmse_naive]
setorder(por_ativo, -margem_pct)
cat("Ativos com amostra de teste (n_obs) suficiente (>=100 obs):", sum(por_ativo$n_obs>=100), "de", nrow(por_ativo), "\n")
print(por_ativo[n_obs >= 100][1:10], nrows=10)
cat("...\n")
print(tail(por_ativo[n_obs >= 100], 10))

cat("\n===== C1: VALE3, h=3, ranqueado por FUNDO =====\n")
v3_fundo <- R3[ativo == "VALE ON N1 - VALE3", .(n_obs = .N, rmse_oos = rmse(erro_oos)), by = cod_fundo]
setorder(v3_fundo, -rmse_oos)
cat("Fundos com VALE3 no teste (h=3):", nrow(v3_fundo), "\n")
print(head(v3_fundo, 5)); cat("...\n"); print(tail(v3_fundo, 5))

cat("\n===== C1: VALE3, h=3, ranqueado por GESTORA =====\n")
v3_gestora <- R3[ativo == "VALE ON N1 - VALE3", .(n_obs = .N, n_fundos = uniqueN(cod_fundo),
                  rmse_oos = rmse(erro_oos)), by = gestora_grupo]
setorder(v3_gestora, -rmse_oos)
print(v3_gestora, nrows = 50)

cat("\n===== C2: matriz de erro (dentro da amostra) gestora x ativo -- insumo =====\n")
matriz <- E[, .(n_obs = .N, media_erro = mean(erro), dp_erro = sd(erro)), by = .(gestora_grupo, ativo)]
cat("Celulas gestora-ativo:", nrow(matriz), "\n")

fwrite(R1, file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h1.csv"))
fwrite(R3, file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h3.csv"))
fwrite(por_ativo, file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_por_ativo_h1.csv"))
fwrite(v3_fundo, file.path(REPO, "v2 OFICIAL/data/etapa3_vale3_por_fundo_h3.csv"))
fwrite(v3_gestora, file.path(REPO, "v2 OFICIAL/data/etapa3_vale3_por_gestora_h3.csv"))
fwrite(matriz, file.path(REPO, "v2 OFICIAL/data/matriz_erro_gestora_ativo.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/etapa3_multiativo_*.csv' e 'matriz_erro_gestora_ativo.csv'\n")
