# =============================================================================
# 98_etapa1_etapa2_stats_multiativo.R  (v2 OFICIAL)
#
# TCC_finalV2 unifica toda a Secao 5 (Resultados) na base multiativo (nao
# mais VALE3-Itau pra Etapa 1/2). Faltam as estatisticas descritivas do
# erro e (Etapa 1) e do erro u (Etapa 2, lambda pooled em TODA a amostra,
# sem corte treino/teste -- diferente da Etapa 3, que so usa treino) nessa
# base -- nunca calculadas antes pra multiativo (so existiam pra VALE3-Itau).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

E <- fread(file.path(REPO, "v2 OFICIAL/data/erro_e_multiativo.csv"))
cat("erro_e_multiativo.csv (com HHI):", nrow(E), "obs\n\n")

moment3 <- function(x) mean(((x-mean(x))/sd(x))^3)
moment4 <- function(x) mean(((x-mean(x))/sd(x))^4)

cat("===== ETAPA 1: estatisticas do erro e (6 caracteristicas, com HHI) =====\n")
cat(sprintf("N = %d\n", nrow(E)))
cat(sprintf("Media = %.8f\n", mean(E$erro)))
cat(sprintf("Desvio-padrao = %.4f\n", sd(E$erro)))
cat(sprintf("Mediana = %.4f\n", median(E$erro)))
cat(sprintf("Minimo / Maximo = %.3f / %.3f\n", min(E$erro), max(E$erro)))
cat(sprintf("%% de obs com e>0 = %.1f%%\n", 100*mean(E$erro>0)))
cat(sprintf("Assimetria = %.2f\n", moment3(E$erro)))
cat(sprintf("Curtose = %.2f (normal=3)\n", moment4(E$erro)))
cat(sprintf("Previsoes (peso_pred) fora de [0,1]: %d de %d\n",
            sum(E$peso_pred < 0 | E$peso_pred > 1), nrow(E)))

# =============================================================================
# ETAPA 2: ajuste parcial pooled EM TODA A AMOSTRA (sem corte treino/teste --
# diferente da Etapa 3, que reestima lambda so no treino p/ testar fora da amostra)
# =============================================================================
cat("\n===== ETAPA 2: ajuste parcial pooled, amostra completa =====\n")
E[, d := peso_pred - peso]
addm1 <- function(ym) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + 1L; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
fut <- E[, .(cod_fundo, ativo, ym_fut_key = ym, peso_fut = peso)]
atual <- E[, .(cod_fundo, ativo, ym, d, peso, ym_fut = addm1(ym))]
M <- merge(atual, fut, by.x = c("cod_fundo","ativo","ym_fut"), by.y = c("cod_fundo","ativo","ym_fut_key"))
M[, dw := peso_fut - peso]
cat(sprintf("Pares de meses consecutivos: %d\n", nrow(M)))

fit <- lm(dw ~ 0 + d, data = M)
lam <- unname(coef(fit)["d"])
M[, u := dw - lam * d]
cat(sprintf("Lambda (pooled, amostra completa) = %.4f\n", lam))
cat(sprintf("N (erro u) = %d\n", nrow(M)))
cat(sprintf("Media(u) = %.8f\n", mean(M$u)))
cat(sprintf("Desvio-padrao(u) = %.4f\n", sd(M$u)))
cat(sprintf("Mediana(u) = %.4f\n", median(M$u)))
cat(sprintf("Minimo / Maximo = %.3f / %.3f\n", min(M$u), max(M$u)))
cat(sprintf("%% de obs com u>0 = %.1f%%\n", 100*mean(M$u>0)))
cat(sprintf("Assimetria = %.2f\n", moment3(M$u)))
cat(sprintf("Curtose = %.2f (normal=3)\n", moment4(M$u)))

fwrite(E[, .(cod_fundo, gestora_grupo, ativo, ym, peso, peso_pred, erro, d)],
       file.path(REPO, "v2 OFICIAL/data/erro_e_multiativo_comD.csv"))
fwrite(M, file.path(REPO, "v2 OFICIAL/data/erro_u_multiativo.csv"))
cat("\nOK\n")
