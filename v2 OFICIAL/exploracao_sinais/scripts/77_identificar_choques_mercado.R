# =============================================================================
# 77_identificar_choques_mercado.R  (exploracao_sinais, agente eventos)
#
# Constroi um proxy de retorno agregado de mercado por mes a partir dos
# proprios dados de preco do painel (precos_mensais_final.csv) e identifica
# os piores meses do periodo 2016-2019 (fora da janela COVID, ja testada no
# candidato #40 do LOG_CANDIDATOS.md principal). Usado para escolher os
# "choques" a testar no desenho de evento (script 78).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
precos <- precos[is.finite(retorno)]

# proxy de mercado: media simples (equal-weight) do retorno de todos os
# tickers com preco no mes -- nao temos indice oficial no painel, isso e
# a melhor aproximacao disponivel com os dados que temos.
mkt_ew <- precos[, .(ret_mkt_ew = mean(retorno), n_tickers = .N,
                      ret_mkt_mediana = median(retorno)), by = ymk][order(ymk)]

cat("===== Retorno agregado (equal-weight) por mes, ordenado do pior pro melhor =====\n")
print(mkt_ew[order(ret_mkt_ew)][1:20])

cat("\n===== Foco 2016-2019 (fora da janela COVID 202001-202101), 15 piores meses =====\n")
sub <- mkt_ew[ymk >= 201601 & ymk <= 201912]
print(sub[order(ret_mkt_ew)][1:15])

cat("\n===== Checagem: onde ficam os eventos conhecidos (Joesley Day mai/2017, greve mai/2018)? =====\n")
print(mkt_ew[ymk %in% c(201705, 201805)])
cat("\nRanking desses 2 meses dentro de 2016-2019 (1 = pior):\n")
sub_ord <- sub[order(ret_mkt_ew)]
sub_ord[, rank_pior := .I]
print(sub_ord[ymk %in% c(201705, 201805)])

fwrite(mkt_ew, file.path(OUT, "candidatos_77_mercado_agregado_mensal.csv"))
cat("\nOK\n")
