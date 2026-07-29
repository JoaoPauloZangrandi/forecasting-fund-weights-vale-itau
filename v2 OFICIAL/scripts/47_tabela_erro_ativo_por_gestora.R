# =============================================================================
# 47_tabela_erro_ativo_por_gestora.R  (v2 OFICIAL)
#
# Tabela sistematica pedida pelo Joao: pra cada uma das 41 gestoras, qual
# ativo tem MAIS e qual tem MENOS erro fora da amostra (h=1), dentro do
# universo completo de acoes que ela carrega.
#
# Reaproveita a base gestora x ativo do R/46 (erro_ativo_por_gestora_h1.csv)
# -- so faltava filtrar o mesmo artefato de posicao-poeira ja visto varias
# vezes nesta sessao: sem filtro, "menos erro" fica dominado por celulas
# onde a gestora praticamente nunca carrega aquele ativo de verdade (peso
# mediano ~0), erro trivialmente ~0 por falta de posicao, nao por o modelo
# explicar bem. Filtro: peso mediano da CELULA gestora-ativo > 0,1% (mesmo
# limiar de sempre) + n_obs >= 24 (cobertura minima).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

GA1 <- fread(file.path(REPO, "v2 OFICIAL/data/erro_ativo_por_gestora_h1.csv"))
d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"),
           select = c("gestora_grupo","ativo","peso"))
peso_med <- d[, .(peso_mediano = median(peso)), by = .(gestora_grupo, ativo)]
GA1 <- merge(GA1, peso_med, by = c("gestora_grupo","ativo"))

elig <- GA1[n_obs >= 24 & peso_mediano > 0.001]
cat("Celulas gestora-ativo elegiveis (>=24 obs, peso mediano>0,1%):", nrow(elig), "de", nrow(GA1), "\n")
cat("Gestoras com pelo menos 1 ativo elegivel:", uniqueN(elig$gestora_grupo), "de 41\n")

# ---- por gestora: ativo com mais e menos erro (RMSE), entre os elegiveis --
tab <- elig[, {
  o <- .SD[order(-rmse_oos)]
  data.table(
    ativo_mais = o$ativo[1], rmse_mais = o$rmse_oos[1], margem_mais = o$margem_pct[1],
    ativo_menos = o$ativo[.N], rmse_menos = o$rmse_oos[.N], margem_menos = o$margem_pct[.N],
    n_ativos_elegiveis = .N
  )
}, by = gestora_grupo]
setorder(tab, -rmse_mais)
cat("\n===== Ativo com mais/menos erro (RMSE, h=1), por gestora (filtrado) =====\n")
print(tab, nrows = 50)

fwrite(tab, file.path(REPO, "v2 OFICIAL/data/tabela_erro_ativo_por_gestora_h1.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/tabela_erro_ativo_por_gestora_h1.csv'\n")
