# =============================================================================
# 47_tabela_erro_ativo_por_gestora.R  (v2 OFICIAL)
#
# Tabela sistematica pedida pelo Joao: pra cada uma das 41 gestoras, os TOP-3
# ativos com MAIS erro e os TOP-3 com MENOS erro fora da amostra, dentro do
# universo completo de acoes que ela carrega. Generalizado pra rodar em
# qualquer horizonte -- pedido de acompanhamento do Joao pra tambem ter h=3
# (a versao original so cobria h=1).
#
# Reaproveita a base gestora x ativo do R/46 (erro_ativo_por_gestora_h{h}.csv)
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

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"),
           select = c("gestora_grupo","ativo","peso"))
peso_med <- d[, .(peso_mediano = median(peso)), by = .(gestora_grupo, ativo)]

top_n <- function(dt, n, decrescente) {
  o <- dt[order(if (decrescente) -rmse_oos else rmse_oos)]
  data.table(rank = seq_len(min(n, nrow(o))), ativo = o$ativo[seq_len(min(n, nrow(o)))],
             rmse_oos = o$rmse_oos[seq_len(min(n, nrow(o)))])
}

monta_tabela <- function(h) {
  GA <- fread(file.path(REPO, sprintf("v2 OFICIAL/data/erro_ativo_por_gestora_h%d.csv", h)))
  GA <- merge(GA, peso_med, by = c("gestora_grupo","ativo"))

  elig <- GA[n_obs >= 24 & peso_mediano > 0.001]
  cat(sprintf("\n--- h=%d ---\n", h))
  cat("Celulas gestora-ativo elegiveis (>=24 obs, peso mediano>0,1%):", nrow(elig), "de", nrow(GA), "\n")
  cat("Gestoras com pelo menos 1 ativo elegivel:", uniqueN(elig$gestora_grupo), "de 41\n")
  cat("Gestoras com pelo menos 3 ativos elegiveis (top-3 completo):",
      sum(elig[, .N, by = gestora_grupo]$N >= 3), "\n")

  tab <- elig[, {
    mais  <- top_n(.SD, 3, TRUE)
    menos <- top_n(.SD, 3, FALSE)
    data.table(rank = mais$rank, ativo_mais = mais$ativo, rmse_mais = mais$rmse_oos,
               ativo_menos = menos$ativo[match(mais$rank, menos$rank)],
               rmse_menos = menos$rmse_oos[match(mais$rank, menos$rank)],
               n_ativos_elegiveis = .N)
  }, by = gestora_grupo]

  ordem <- tab[rank == 1][order(-rmse_mais), gestora_grupo]
  tab[, gestora_grupo := factor(gestora_grupo, levels = ordem)]
  setorder(tab, gestora_grupo, rank)
  tab[, gestora_grupo := as.character(gestora_grupo)]

  # dominancia de VALE3/PETR4/ITUB4 no top-3, mesma checagem de sempre
  vale <- tab[, any(grepl("VALE3", ativo_mais)), by = gestora_grupo]$V1
  petr <- tab[, any(grepl("PETR3|PETR4", ativo_mais)), by = gestora_grupo]$V1
  itub <- tab[, any(grepl("ITUB4", ativo_mais)), by = gestora_grupo]$V1
  n_g <- uniqueN(tab$gestora_grupo)
  cat("VALE3 no top-3 mais-erro:", sum(vale), "de", n_g, "\n")
  cat("VALE3 ou PETR3/4 no top-3:", sum(vale|petr), "de", n_g, "\n")
  cat("VALE3, PETR3/4 ou ITUB4 no top-3:", sum(vale|petr|itub), "de", n_g, "\n")

  fwrite(tab, file.path(REPO, sprintf("v2 OFICIAL/data/tabela_erro_ativo_por_gestora_h%d.csv", h)))
  cat("OK - salvo em 'v2 OFICIAL/data/tabela_erro_ativo_por_gestora_h", h, ".csv'\n", sep="")
  tab
}

tab1 <- monta_tabela(1)
tab3 <- monta_tabela(3)
