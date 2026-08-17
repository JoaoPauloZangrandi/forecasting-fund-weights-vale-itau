# =============================================================================
# 106_benchmark_ibovespa.R  (v2 OFICIAL/scripts)
#
# Constroi retorno MENSAL do Ibovespa como benchmark, pra uso no Desafio
# Quant AI (recomendado explicitamente pelas diretrizes: "escolher um
# benchmark"). Criado em 15/08/2026, durante a mega-auditoria pre-dados-novos.
#
# Fonte: data/raw/yahoo_ibov.json, resposta bruta da API de chart do Yahoo
# Finance (formato "chart.result[[1]].timestamp" + "...indicators.quote[[1]].close"),
# ja baixado, cobre 2014-01-02 a 2021-12-30 (diario). Mesma logica de
# conversao diario->mensal do script 56 (ultimo pregao do mes).
#
# NOTA IMPORTANTE (mesma racional que ja motivou trocar Yahoo->COTAHIST pros
# precos de ACOES individuais, script 56): o problema do Yahoo era ticker
# desatualizado apos fusao/reestruturacao societaria de EMPRESA. O Ibovespa
# e' um INDICE, nao uma empresa -- nao sofre desse problema, entao reusar
# Yahoo so' pro benchmark e' seguro (COTAHIST tambem nao tem o indice como
# "ativo" negociado da forma que teria uma acao, so' o Yahoo tem isso pronto).
#
# QUANDO OS DADOS DE 2022-2026 CHEGAREM: este script SO cobre ate dez/2021.
# Vai precisar buscar um novo JSON do Yahoo (mesmo endpoint, ^BVSP) cobrindo
# 2022-2026 e concatenar, ou re-baixar o periodo inteiro de uma vez.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")

j <- fromJSON(file.path(REPO, "data/raw/yahoo_ibov.json"))
ts <- j$chart$result$timestamp[[1]]
close <- j$chart$result$indicators$quote[[1]]$close[[1]]

dt <- data.table(
  data  = as.Date(as.POSIXct(ts, origin = "1970-01-01", tz = "UTC")),
  preco = close
)
dt <- dt[is.finite(preco) & preco > 0]
setorder(dt, data)
cat("Ibovespa bruto (diario): ", nrow(dt), "pregoes | ", format(min(dt$data)), "a", format(max(dt$data)), "\n")

# ---- conversao diario -> mensal (ultimo pregao do mes, mesma convencao do script 56) ----
dt[, ymk := year(data)*100L + month(data)]
mlast <- dt[dt[, .I[which.max(data)], by = ymk]$V1, .(ymk, data_ref = data, preco)]
setorder(mlast, ymk)
mlast[, retorno_ibov := preco/shift(preco) - 1]

cat("\nIbovespa mensal:", nrow(mlast), "meses\n")
cat("Retorno medio mensal:", sprintf("%.3f%%", 100*mean(mlast$retorno_ibov, na.rm=TRUE)),
    "| retorno anualizado (media simples x12):", sprintf("%.1f%%", 100*mean(mlast$retorno_ibov, na.rm=TRUE)*12), "\n")
cat("Vol mensal:", sprintf("%.2f%%", 100*sd(mlast$retorno_ibov, na.rm=TRUE)),
    "| Vol anualizada:", sprintf("%.1f%%", 100*sd(mlast$retorno_ibov, na.rm=TRUE)*sqrt(12)), "\n")

OUT <- file.path(REPO, "v2 OFICIAL/data/benchmark_ibovespa_mensal.csv")
fwrite(mlast, OUT)
cat("\nOK - salvo em", OUT, "\n")
