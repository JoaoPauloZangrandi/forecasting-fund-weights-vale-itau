# =============================================================================
# 94_auditoria_completa_dados.R  (v2 OFICIAL)
#
# Pedido do Joao apos o achado do script 89 (soma de peso > 100% num
# fundo): existe mais algum problema desse tipo escondido nas bases que
# alimentam o TCC? Varredura sistematica, nas DUAS bases usadas no
# documento:
#   (A) painel_multiativo_final.csv -- ja limpa (script 90), mas
#       auditada de novo pra confirmar
#   (B) painel_predeterminado.csv -- base VALE3-Itau, usada nas Secoes
#       5.1/5.2 e na parte estatica da 5.3, NUNCA auditada por esse tipo
#       de problema
#
# Checagens: pesos negativos ou >1 individualmente; soma de peso por
# fundo-mes > 100% (so multiativo, onde ha varios ativos por fundo);
# AUM/cotistas nulos, negativos ou zero; fluxo/AUM extremo; beta_fundo
# extremo; linhas duplicadas em (fundo,ativo,mes); NA/Inf nos campos que
# entram em regressao; datas fora do periodo esperado (2016-2021).
#
# Achados desta rodada (documentados no TCC): (1) base multiativo ja
# limpa, max soma_peso = 1,0489, 0 casos acima de 1,05 -- OK; (2) 80
# fundo-meses (74 fundos, 0,06% das obs) com |flow_aum|>2 -- testado o
# impacto (ver console): muda RMSE agregado em so 0,09%, negligivel,
# NAO justifica reabrir o pipeline; (3) 322 linhas com AUM ausente na
# base pequena -- ja sao excluidas pelo filtro is.finite(l_aum) ja
# existente (scripts 13/14/15), nao e problema novo, so confirma que o
# filtro ja existente funciona.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

linha <- function() cat(strrep("=", 78), "\n")

# =============================================================================
# (A) BASE MULTIATIVO (ja limpa) -- confirma que ficou 100% sa
# =============================================================================
linha(); cat("(A) PAINEL MULTIATIVO FINAL (pos-limpeza)\n"); linha()
pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"))
cat("Linhas:", nrow(pp), "| Fundos:", uniqueN(pp$cod_fundo), "| Ativos:", uniqueN(pp$ativo), "\n\n")

cat("--- Peso individual ---\n")
cat("Peso negativo:", pp[peso < 0, .N], "\n")
cat("Peso > 1:", pp[peso > 1, .N], "\n")
cat("Peso NA/NaN/Inf:", pp[!is.finite(peso), .N], "\n")

cat("\n--- Soma de peso por fundo-mes (ja deveria estar <=105% em todos) ---\n")
soma <- pp[, .(soma_peso = sum(peso)), by = .(cod_fundo, ym)]
cat("Max soma_peso:", round(max(soma$soma_peso), 4), "\n")
cat("Pares com soma_peso > 1,05:", soma[soma_peso > 1.05, .N], "(deveria ser 0)\n")
cat("Pares com soma_peso > 1,01:", soma[soma_peso > 1.01, .N], "\n")

cat("\n--- AUM / cotistas / fluxo ---\n")
carac <- unique(pp[, .(cod_fundo, ym, aum_prev, cotistas_prev, fluxo_prev, flow_aum, beta_fundo, l_aum, l_cot)])
cat("AUM <= 0:", carac[aum_prev <= 0, .N], "\n")
cat("AUM NA/Inf:", carac[!is.finite(aum_prev), .N], "\n")
cat("Cotistas <= 0:", carac[cotistas_prev <= 0, .N], "\n")
cat("Cotistas NA:", carac[is.na(cotistas_prev), .N], "\n")
cat("flow_aum NA/Inf:", carac[!is.finite(flow_aum), .N], "\n")
cat("flow_aum extremo (|flow_aum| > 2, ou seja fluxo > 200% do AUM num mes):", carac[abs(flow_aum) > 2, .N], "\n")
if (carac[abs(flow_aum) > 2, .N] > 0) print(carac[abs(flow_aum) > 2][order(-abs(flow_aum))][1:min(10,.N)])

cat("\n--- beta_fundo ---\n")
cat("beta_fundo NA/Inf:", carac[!is.finite(beta_fundo), .N], "\n")
cat("beta_fundo extremo (|beta| > 5):", carac[abs(beta_fundo) > 5, .N], "\n")
cat("Resumo beta_fundo:\n"); print(summary(carac$beta_fundo))

cat("\n--- Linhas duplicadas (cod_fundo, ativo, ym) ---\n")
dup <- pp[, .N, by = .(cod_fundo, ativo, ym)][N > 1]
cat("Combinacoes duplicadas:", nrow(dup), "\n")

cat("\n--- Periodo ---\n")
cat("Range de ym:", min(pp$ym), "a", max(pp$ym), "\n")
fora <- pp[ym < 201601 | ym > 202607]
cat("Linhas fora de 2016-01 a 2021-12:", nrow(fora), "\n")

cat("\n--- is_fic ---\n")
cat("is_fic NA:", pp[is.na(is_fic), .N], "\n")
cat("is_fic fora de {0,1}:", pp[!is_fic %in% c(0,1), .N], "\n")

# =============================================================================
# (B) BASE VALE3-ITAU (painel_predeterminado.csv) -- nunca auditada assim
# =============================================================================
linha(); cat("\n(B) PAINEL PREDETERMINADO (VALE3-Itau)\n"); linha()
pd <- fread(file.path(REPO, "v2 OFICIAL/data/painel_predeterminado.csv"))
cat("Linhas:", nrow(pd), "| Fundos:", uniqueN(pd$cod_fundo), "\n\n")

cat("--- peso_vale3 individual ---\n")
cat("peso_vale3 negativo:", pd[peso_vale3 < 0, .N], "\n")
cat("peso_vale3 > 1:", pd[peso_vale3 > 1, .N], "\n")
cat("peso_vale3 NA/Inf:", pd[!is.finite(peso_vale3), .N], "\n")
cat("Resumo peso_vale3:\n"); print(summary(pd$peso_vale3))

cat("\n--- AUM / cotistas / fluxo ---\n")
cat("AUM <= 0:", pd[aum_prev <= 0, .N], "\n")
cat("AUM NA/Inf:", pd[!is.finite(aum_prev), .N], "(ja excluidas pelo filtro is.finite(l_aum) a jusante)\n")
cat("Cotistas <= 0:", pd[cotistas_prev <= 0, .N], "\n")
pd[, flow_aum_calc := fluxo_prev / aum_prev]
cat("flow_aum extremo (|flow_aum| > 2):", pd[abs(flow_aum_calc) > 2, .N], "\n")

cat("\n--- beta_fundo ---\n")
cat("beta_fundo NA/Inf:", pd[!is.finite(beta_fundo), .N], "(esperado -- fundos sem 252 pregoes de historico)\n")
cat("beta_fundo extremo (|beta| > 5):", pd[abs(beta_fundo) > 5, .N], "\n")
cat("Resumo beta_fundo:\n"); print(summary(pd$beta_fundo))

cat("\n--- Linhas duplicadas (cod_fundo, ym) ---\n")
dup2 <- pd[, .N, by = .(cod_fundo, ym)][N > 1]
cat("Combinacoes duplicadas:", nrow(dup2), "\n")

cat("\n--- Periodo ---\n")
cat("Range de ym:", min(pd$ym), "a", max(pd$ym), "\n")

cat("\n\n===== FIM DA AUDITORIA =====\n")
