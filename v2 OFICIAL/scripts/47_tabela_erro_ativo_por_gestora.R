# =============================================================================
# 47_tabela_erro_ativo_por_gestora.R  (v2 OFICIAL)
#
# Tabela sistematica pedida pelo Joao: pra cada uma das 41 gestoras, os TOP-3
# ativos com MAIS erro e os TOP-3 com MENOS erro fora da amostra (h=1),
# dentro do universo completo de acoes que ela carrega (pedido de
# acompanhamento -- versao anterior so trazia 1 de cada).
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
cat("Gestoras com pelo menos 3 ativos elegiveis (top-3 completo):",
    sum(elig[, .N, by = gestora_grupo]$N >= 3), "\n")

# ---- por gestora: top-3 ativos com mais e top-3 com menos erro (RMSE) ----
top_n <- function(dt, n, decrescente) {
  o <- dt[order(if (decrescente) -rmse_oos else rmse_oos)]
  data.table(rank = seq_len(min(n, nrow(o))), ativo = o$ativo[seq_len(min(n, nrow(o)))],
             rmse_oos = o$rmse_oos[seq_len(min(n, nrow(o)))])
}
tab <- elig[, {
  mais  <- top_n(.SD, 3, TRUE)
  menos <- top_n(.SD, 3, FALSE)
  data.table(rank = mais$rank, ativo_mais = mais$ativo, rmse_mais = mais$rmse_oos,
             ativo_menos = menos$ativo[match(mais$rank, menos$rank)],
             rmse_menos = menos$rmse_oos[match(mais$rank, menos$rank)],
             n_ativos_elegiveis = .N)
}, by = gestora_grupo]

# ordena gestoras pelo maior RMSE (rank 1) e mantem rank 1-2-3 agrupado
ordem <- tab[rank == 1][order(-rmse_mais), gestora_grupo]
tab[, gestora_grupo := factor(gestora_grupo, levels = ordem)]
setorder(tab, gestora_grupo, rank)
tab[, gestora_grupo := as.character(gestora_grupo)]

cat("\n===== Top-3 ativos com mais/menos erro (RMSE, h=1), por gestora (filtrado) =====\n")
print(tab, nrows = 150)

fwrite(tab, file.path(REPO, "v2 OFICIAL/data/tabela_erro_ativo_por_gestora_h1.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/tabela_erro_ativo_por_gestora_h1.csv'\n")
