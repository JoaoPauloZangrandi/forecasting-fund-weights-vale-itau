# =============================================================================
# 40_panorama_erro_ativo.R  (v2 OFICIAL)
#
# Panorama completo da Secao 6.3 por ATIVO (espelha R/38, que resume por
# gestora). Mesmo erro (R/37), agora agregado por ativo em vez de gestora.
#
# Achado: a media do erro por ATIVO e ~0 por construcao (nao so
# aproximadamente pequena) -- diferente do caso por-gestora (R/38), onde a
# ausencia de FE de gestora faz a media ser informativa. Aqui, TODA
# observacao de um ativo vem de celulas (ativo,mes) indexadas por esse MESMO
# ativo, entao a propriedade de soma-zero dos residuos de cada celula (razao
# de primeira ordem do MLE) forca diretamente a media por ativo a ser zero --
# e o espelho do caso da Secao 5 (FE de gestora). Logo, so a DISPERSAO e
# informativa aqui, assim como na Secao 5.2 original.
#
# Mesmo filtro de posicao-poeira do R/39: peso mediano > 0,1%, alem do
# criterio de >=24 meses -- sem ele, os ativos "menos dispersos" sao dominados
# por papeis nunca efetivamente alocados (peso~0 em quase toda observacao),
# que tem erro~0 trivialmente (nada pra explicar), nao porque o modelo os
# explica bem.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

E <- fread(file.path(REPO, "v2 OFICIAL/data/erro_e_multiativo.csv"))
cat("Erro multiativo:", nrow(E), "obs |", uniqueN(E$ativo), "ativos\n")

por_mes <- E[, .(media_mes = mean(erro), n = .N), by = .(ativo, ym)]
resumo <- por_mes[, .(n_meses = .N, media = mean(media_mes), dp_media_mensal = sd(media_mes),
                       n_obs = sum(n)), by = ativo]
resumo[, t := media/(dp_media_mensal/sqrt(n_meses))]
resumo[, sig := ifelse(abs(t)>3.29,"***",ifelse(abs(t)>2.58,"**",ifelse(abs(t)>1.96,"*","n.s.")))]

dp_pool <- E[, .(dp = sd(erro)), by = ativo]
resumo <- merge(resumo, dp_pool, by = "ativo")

cat("\nChecagem -- media do erro por ativo e ~0 por construcao?\n")
cat("media(|media|) entre todos os ativos:", format(mean(abs(resumo$media)), scientific = TRUE), "\n")
vale_chk <- resumo[ativo == "VALE ON N1 - VALE3"]
cat("VALE3 media (deveria ser ~0):", format(vale_chk$media, scientific = TRUE), "\n\n")

# ---- filtro de posicao-poeira (mesmo do R/39) -------------------------------
d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"),
           select = c("ativo","peso"))
peso_med <- d[, .(peso_mediano = median(peso)), by = ativo]
resumo <- merge(resumo, peso_med, by = "ativo")
elig <- resumo[n_meses >= 24 & peso_mediano > 0.001]
cat("Elegiveis (>=24 meses E peso mediano > 0,1%):", nrow(elig), "de", nrow(resumo), "\n\n")

cat("Quartis da dispersao (dp) entre elegiveis:\n")
print(round(quantile(elig$dp, c(.25,.5,.75)), 5))
vale <- elig[ativo == "VALE ON N1 - VALE3"]
cat("\nVALE3 dp:", round(vale$dp,5), "| rank:",
    which(elig[order(-dp)]$ativo == "VALE ON N1 - VALE3"), "de", nrow(elig), "\n")

setorder(elig, -dp)
cat("\nTop5 (mais dispersos -- erro menos previsivel):\n")
print(head(elig[,.(ativo, n_obs, n_meses, dp=round(dp,5))], 5))
cat("\nBottom5 (menos dispersos -- erro mais previsivel/estavel):\n")
print(tail(elig[,.(ativo, n_obs, n_meses, dp=round(dp,5))], 5))

fwrite(elig, file.path(REPO, "v2 OFICIAL/data/erro_multiativo_ativo_resumo_filtrado.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/erro_multiativo_ativo_resumo_filtrado.csv'\n")
