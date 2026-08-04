# =============================================================================
# 61_ajuste_parcial_universo_completo.R  (v2 OFICIAL -- teste_estrategia)
#
# FASE 2: Etapa 2 (ajuste parcial, h=1, amostra INTEIRA sem corte treino/
# teste -- isso fica pra Fase 3) no universo completo, COM a correcao do
# efeito mecanico de preco.
#
#   d_{i,n,t}          = peso_pred_{i,n,t} - peso_{i,n,t}      (Fase 1)
#   dw_real             = peso_{i,n,t+1} - peso_{i,n,t}
#   w_mecanico_{t+1}    = peso_t * (1+r_ativo_{t+1}) / (1+r_fundo_{t+1})
#   dw_corrigido         = peso_{i,n,t+1} - w_mecanico_{t+1}
#
# lambda estimado via lm(dw ~ 0 + d, ...) -- RODADO NAS DUAS VERSOES (dw_real
# e dw_corrigido) pra comparar o efeito de tirar a variacao mecanica de
# preco. Onde nao ha retorno de ativo disponivel (18 dos 536 tickers sem
# preco em nenhuma fonte, ver Fase 0), dw_corrigido fica NA pra essa
# observacao especifica -- nao afeta as demais.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")

pred <- fread(file.path(REPO, "v2 OFICIAL/data/peso_pred_universo_completo.csv"))
pred[, cod_fundo := as.character(cod_fundo)]
pred[, d := peso_pred - peso]
cat("Base peso_pred:", nrow(pred), "obs\n")

addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

# ---- par (t, t+1) ----------------------------------------------------------
fut <- pred[, .(cod_fundo, ativo, ym_fut_key = ym, peso_fut = peso)]
atual <- pred[, .(cod_fundo, ativo, ym, d, peso, ym_fut = addm(ym, 1L))]
M <- merge(atual, fut, by.x = c("cod_fundo","ativo","ym_fut"), by.y = c("cod_fundo","ativo","ym_fut_key"))
M[, dw := peso_fut - peso]
cat("Pares (t,t+1) disponiveis:", nrow(M), "\n")

# ---- retornos pra correcao mecanica ----------------------------------------
M[, ticker := trimws(sub(".*- ", "", ativo))]
precos <- fread(file.path(REPO, "v2 OFICIAL/data/precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
setnames(precos, c("ymk","retorno"), c("ym_fut","r_ativo"))
M <- merge(M, precos, by = c("ticker","ym_fut"), all.x = TRUE)

rfundo <- fread(file.path(REPO, "v2 OFICIAL/data/retorno_fundo_mensal.csv"), select = c("cod_fundo","ymk","retorno_fundo"))
rfundo[, cod_fundo := as.character(cod_fundo)]
setnames(rfundo, "ymk", "ym_fut")
M <- merge(M, rfundo, by = c("cod_fundo","ym_fut"), all.x = TRUE)

cat("\nCobertura de retorno de ativo:", M[!is.na(r_ativo), .N], "de", nrow(M),
    sprintf(" (%.1f%%)\n", 100*M[!is.na(r_ativo), .N]/nrow(M)))
cat("Cobertura de retorno de fundo:", M[!is.na(retorno_fundo), .N], "de", nrow(M),
    sprintf(" (%.1f%%)\n", 100*M[!is.na(retorno_fundo), .N]/nrow(M)))

M[, peso_mecanico := peso * (1 + r_ativo) / (1 + retorno_fundo)]
M[, dw_corrigido := peso_fut - peso_mecanico]

cobertura_dupla <- M[!is.na(dw_corrigido), .N]
cat("\nPares com dw_corrigido calculavel (precisa de r_ativo E retorno_fundo):", cobertura_dupla,
    sprintf(" de %d (%.1f%%)\n", nrow(M), 100*cobertura_dupla/nrow(M)))

# ---- lambda: com e sem correcao (mesma sub-amostra, pra comparar limpo) ---
amostra_comum <- M[!is.na(dw_corrigido)]
cat("\n===== Amostra comum (so onde dw_corrigido existe), n =", nrow(amostra_comum), "=====\n")

fit_bruto <- lm(dw ~ 0 + d, data = amostra_comum)
lam_bruto <- unname(coef(fit_bruto)["d"])
cat(sprintf("lambda SEM correcao mecanica  = %.4f (R2=%.4f)\n", lam_bruto, summary(fit_bruto)$r.squared))

fit_corrigido <- lm(dw_corrigido ~ 0 + d, data = amostra_comum)
lam_corrigido <- unname(coef(fit_corrigido)["d"])
cat(sprintf("lambda COM correcao mecanica  = %.4f (R2=%.4f)\n", lam_corrigido, summary(fit_corrigido)$r.squared))

cat(sprintf("\nDiferenca (corrigido - bruto): %.4f (%.1f%% do valor bruto)\n",
            lam_corrigido - lam_bruto, 100*(lam_corrigido-lam_bruto)/lam_bruto))

# lambda tambem na amostra INTEIRA sem correcao (n maior, comparar com o pipeline original)
fit_bruto_full <- lm(dw ~ 0 + d, data = M)
cat(sprintf("\n[referencia] lambda SEM correcao, amostra INTEIRA (n=%d) = %.4f\n",
            nrow(M), unname(coef(fit_bruto_full)["d"])))

fwrite(M, file.path(REPO, "v2 OFICIAL/data/ajuste_parcial_universo_completo_h1.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/ajuste_parcial_universo_completo_h1.csv'\n")
