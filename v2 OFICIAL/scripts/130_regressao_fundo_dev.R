# =============================================================================
# 130_regressao_fundo_dev.R  (v2 OFICIAL)
#
# Regressoes no nivel de FUNDO, SO na amostra de DESENVOLVIMENTO.
# O lado de confirmacao nao e' tocado aqui (script 131).
#
# Especificacoes (pre-registro, Secao 5):
#   A1 pooled | A2 Mundlak (principal) | A3 within-gestora
# Dependentes: D1 (peso) e D2 (sleeve), sempre as duas.
# Parametrizacoes: direta e ortogonalizada (emenda E2), sempre as duas.
#
# Inferencia (pre-registro, Secao 6):
#   CV1 clusterizado | wild cluster bootstrap-t com nulo imposto
#   (Rademacher; Webb para regressor quase constante dentro do cluster)
#   | randomizacao para o bloco de mandato
# Correcao multipla: 4 testes F de bloco com Holm; coeficientes com
#   Holm intra-bloco e BH na familia toda.
#
# B = 1999 no desenvolvimento (exploracao). O script 131 usa 9999.
# =============================================================================
suppressPackageStartupMessages({
  library(data.table); library(sandwich); library(lmtest); library(fixest)
})

REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DD   <- file.path(REPO, "v2 OFICIAL/data")
set.seed(20260915L)
B_WCB <- 1999L

source(file.path(REPO, "v2 OFICIAL/scripts/_funcoes_extensao.R"))

cat("=============================================================\n")
cat("130 - Regressoes no nivel de fundo (DESENVOLVIMENTO)\n")
cat("=============================================================\n\n")

B1 <- fread(file.path(DD, "caracteristicas_fundo_treino.csv"))
SP <- fread(file.path(DD, "split_fundos_dev_conf.csv"))
B1[, cod_fundo := as.character(cod_fundo)]; SP[, cod_fundo := as.character(cod_fundo)]
D <- merge(B1, SP[, .(cod_fundo, grupo)], by = "cod_fundo")
DEV <- D[grupo == "dev"]
cat(sprintf("Amostra de desenvolvimento: %d fundos | %d gestoras\n",
            nrow(DEV), uniqueN(DEV$gestora_grupo)))
cat(sprintf("(confirmacao guardada: %d fundos, NAO olhada aqui)\n\n", D[grupo == "conf", .N]))

DEV <- prepara_dummies(DEV)
res <- roda_bateria(DEV, rotulo_amostra = "dev", B = B_WCB, verbose = TRUE)

fwrite(res$coef,   file.path(DD, "reg_fundo_dev_coef.csv"))
fwrite(res$blocos, file.path(DD, "reg_fundo_dev_blocos.csv"))
fwrite(res$ajuste, file.path(DD, "reg_fundo_dev_ajuste.csv"))
grava_ledger(res$ledger, DD)
cat("\nOK - reg_fundo_dev_coef.csv, reg_fundo_dev_blocos.csv, reg_fundo_dev_ajuste.csv\n")
