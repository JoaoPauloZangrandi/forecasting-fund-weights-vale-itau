suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

anos <- 2016:2021
res <- vector("list", length(anos))
for (i in seq_along(anos)) {
  y <- anos[i]
  res[[i]] <- fread(file.path(REPO, sprintf("v2 OFICIAL/data/painel_multiativo_ano_%d.csv", y)))
  cat("ano", y, ":", nrow(res[[i]]), "linhas\n")
}
painel <- rbindlist(res)
cat("\nTOTAL:", nrow(painel), "linhas |", uniqueN(painel$cod_fundo), "fundos |",
    uniqueN(painel$ativo), "ativos\n")

dup <- painel[, .N, by = .(cod_fundo, ativo, ano, mes)][N > 1]
cat("Pares (fundo,ativo,mes) duplicados:", nrow(dup), "\n")

nvale <- painel[ativo == "VALE ON N1 - VALE3", .N]
cat("VALE3:", nvale, "(esperado: 96.349)\n")
stopifnot(nvale == 96349L)

fwrite(painel, file.path(REPO, "v2 OFICIAL/data/painel_multiativo_direto.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/painel_multiativo_direto.csv'\n")

# limpeza dos arquivos intermediarios por ano
for (y in anos) file.remove(file.path(REPO, sprintf("v2 OFICIAL/data/painel_multiativo_ano_%d.csv", y)))
cat("Arquivos intermediarios por ano removidos.\n")
