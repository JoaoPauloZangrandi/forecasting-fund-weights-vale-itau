# =============================================================================
# 100_checagem_soma_peso_universo_completo.R  (v2 OFICIAL -- teste_estrategia)
#
# A limpeza de soma_peso>105% feita na trilha multiativo (scripts 89/90,
# 05/08/2026) NUNCA foi aplicada a trilha separada "universo completo"
# (scripts 50-73, teste_estrategia.tex) -- confirmado: o fundo 292826
# (Vinci Partners, o mesmo caso que motivou a limpeza original) ainda tem
# soma_peso de 498%/477% em mar/abr 2021 em painel_multiativo_direto_
# completo.csv, e aparece em 183 linhas de pares_replicantes_significativos.csv.
#
# Este script (1) faz a auditoria sistemica NESSA base especifica (3.254
# fundos -- NAO reaproveita a lista de 597 fundo-mes ja achada na trilha
# multiativo, essa base tem ~950 fundos a mais nunca auditados) e (2) limpa
# na origem: painel_multiativo_direto_completo.csv, o painel mais cedo da
# cadeia (antes de qualquer merge de features/beta) -- scripts 58-73
# rerodam sem alteracao de logica sobre esse painel ja limpo.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
LIM <- 1.50  # atualizado 12/08/2026: investigacao (posicao maxima individual, persistencia
             # mes-a-mes, classificacao Anbima) mostrou que 105% excluia alavancagem legitima
             # (fundos "Acoes Livre"/"Multimercados Livre" persistentemente >100%) junto com
             # erro de dado de verdade (posicao unica dominante, evento isolado). 150% separa
             # melhor os dois padroes -- ver TCC_finalV2.tex Secao 3 para a evidencia completa.

pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_direto_completo.csv"))
cat("Painel universo completo (bruto):", nrow(pp), "linhas |", uniqueN(pp$cod_fundo), "fundos\n")

soma <- pp[, .(soma_peso = sum(peso), n_ativos = .N), by = .(cod_fundo, ano, mes)]
cat("Total de pares fundo-mes:", nrow(soma), "\n")
cat("\n===== Distribuicao da soma de peso por fundo-mes =====\n")
print(summary(soma$soma_peso))

for (lim in c(1.01, 1.05, 1.10, 1.25, 1.50, 2.00)) {
  n <- sum(soma$soma_peso > lim)
  cat(sprintf("Pares fundo-mes com soma_peso > %.2f : %d (%.3f%%)\n", lim, n, 100*n/nrow(soma)))
}

anomalos <- soma[soma_peso > 1.10]
cat("\n===== Fundos distintos afetados (soma_peso > 1,10) =====\n")
cat("Fundos distintos:", uniqueN(anomalos$cod_fundo), "de", uniqueN(soma$cod_fundo), "\n")

cat("\n===== Comparando com a trilha multiativo (597 fundo-mes / 73 fundos ja conhecidos) =====\n")
fundos_multiativo_conhecidos <- tryCatch({
  chk <- fread(file.path(REPO, "v2 OFICIAL/data/checagem_soma_peso_fundo_mes.csv"))
  unique(chk[soma_peso > LIM]$cod_fundo)
}, error = function(e) integer(0))
bad_agora <- unique(soma[soma_peso > LIM]$cod_fundo)
cat("Fundos contaminados (>105%) nesta base:", length(bad_agora), "\n")
cat("Desses, quantos JA apareciam na trilha multiativo:", sum(bad_agora %in% fundos_multiativo_conhecidos), "\n")
cat("Fundos contaminados NOVOS (so existem no universo completo, nunca auditados antes):",
    sum(!bad_agora %in% fundos_multiativo_conhecidos), "\n")

cat("\n===== Confirmando o caso 292826 (Vinci Partners) especificamente =====\n")
print(soma[cod_fundo == 292826 & soma_peso > 1.05])

cat("\n===== Os 20 casos mais extremos =====\n")
setorder(anomalos, -soma_peso)
print(anomalos[1:min(20,.N), .(cod_fundo, ano, mes, soma_peso = round(soma_peso,2), n_ativos)])

fwrite(soma, file.path(REPO, "v2 OFICIAL/data/checagem_soma_peso_universo_completo.csv"))

# =============================================================================
# LIMPEZA: exclui fundo-mes com soma_peso > 105%, grava por cima do nome
# padrao (backup do original preservado)
# =============================================================================
cat("\n\n===== LIMPEZA =====\n")
bad <- soma[soma_peso > LIM, .(cod_fundo, ano, mes)]
cat(sprintf("Fundo-mes excluidos (soma_peso > %.2f): %d de %d (%.3f%%)\n",
            LIM, nrow(bad), nrow(soma), 100*nrow(bad)/nrow(soma)))

pp[, chave := paste(cod_fundo, ano, mes)]
bad[, chave := paste(cod_fundo, ano, mes)]
pp_limpo <- pp[!chave %in% bad$chave]
pp_limpo[, chave := NULL]
cat("Painel limpo:", nrow(pp_limpo), "linhas (", nrow(pp)-nrow(pp_limpo), "removidas ) |",
    uniqueN(pp_limpo$cod_fundo), "fundos\n")

ARQ <- file.path(REPO, "v2 OFICIAL/data/painel_multiativo_direto_completo.csv")
BAK <- file.path(REPO, "v2 OFICIAL/data/painel_multiativo_direto_completo_ANTES_LIMPEZA.csv.bak")
if (!file.exists(BAK)) file.copy(ARQ, BAK)
fwrite(pp_limpo, ARQ)
cat("\nOK - painel_multiativo_direto_completo.csv sobrescrito (limpo); backup em",
    basename(BAK), "\n")
