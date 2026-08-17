# =============================================================================
# _check_cobertura_mercado_vale3.R (temporario, NAO documentado no TCC ainda)
#
# Pergunta do Joao: quanto da VALE3 nossa base cobre, em ago/2020?
# (1) Cobertura de POSICAO: valor total de VALE3 detido pelos fundos da
#     nossa base (cons_2020.csv, posicao direta, competencia ago/2020)
#     dividido pelo valor de mercado total da VALE3 na mesma data.
# (2) Proxy de FLUXO: variacao da posicao agregada de VALE3 (jul->ago 2020)
#     dividida pelo volume financeiro TOTAL negociado de VALE3 na B3 em
#     ago/2020 (fonte: COTAHIST, campo VOLTOT).
#
# Layout COTAHIST confirmado no script 56 (preult pos 109-121). Campos de
# quantidade/volume negociado (TOTNEG, QUATOT, VOLTOT) NAO foram usados
# ainda no projeto -- preciso confirmar a posicao exata antes de usar.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
SH_DIR <- "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF"
COTA <- file.path(REPO, "data/raw/cotahist/COTAHIST_A2020.TXT")

cat("===== 0) Confirmando layout do COTAHIST (tamanho de linha, campos) =====\n")
con <- file(COTA, "r"); lines <- readLines(con, n = -1L, encoding = "latin1"); close(con)
lines01 <- lines[substr(lines, 1, 2) == "01"]
cat("Tamanho de linha (deveria ser 245):", nchar(lines01[1]), "\n")
# layout oficial B3: TOTNEG 148-152, QUATOT 153-170, VOLTOT 171-188 (2 casas decimais)
exemplo <- lines01[substr(lines01,13,24) == "VALE3       " & substr(lines01,3,10) == "20200831"]
if (length(exemplo) == 0) exemplo <- lines01[trimws(substr(lines01,13,24)) == "VALE3"][1]
cat("Exemplo de linha VALE3:\n"); cat(exemplo, "\n")
cat("TOTNEG (148-152):", substr(exemplo,148,152), "\n")
cat("QUATOT (153-170):", substr(exemplo,153,170), "\n")
cat("VOLTOT (171-188):", substr(exemplo,171,188), "\n")
cat("PREULT (109-121)/100:", as.numeric(substr(exemplo,109,121))/100, "\n\n")

# ---- parse completo VALE3, mercado a vista (tpmerc 010), 2020 -------------
codneg <- trimws(substr(lines01, 13, 24))
tpmerc <- substr(lines01, 25, 27)
keep <- codneg == "VALE3" & tpmerc == "010"
sub <- lines01[keep]
dt <- data.table(
  data = as.Date(substr(sub,3,10), format="%Y%m%d"),
  preult = as.numeric(substr(sub,109,121))/100,
  totneg = as.numeric(substr(sub,148,152)),
  quatot = as.numeric(substr(sub,153,170)),
  voltot = as.numeric(substr(sub,171,188))/100
)
cat("===== 1) VALE3 -- pregoes 2020, mercado a vista:", nrow(dt), "=====\n")
print(dt[format(data,"%Y-%m")=="2020-08"])

vol_ago <- dt[format(data,"%Y-%m")=="2020-08", sum(voltot)]
qtd_ago <- dt[format(data,"%Y-%m")=="2020-08", sum(quatot)]
cat(sprintf("\nVolume financeiro TOTAL negociado VALE3 em ago/2020: R$ %.2f milhoes\n", vol_ago/1e6))
cat(sprintf("Quantidade total negociada: %.0f mil acoes\n", qtd_ago/1e3))

preco_3108 <- dt[data == as.Date("2020-08-31"), preult]
if (length(preco_3108)==0) preco_3108 <- dt[format(data,"%Y-%m")=="2020-08"][which.max(data), preult]
cat(sprintf("\nPreco de fechamento VALE3 (ultimo pregao ago/2020): R$ %.2f\n", preco_3108))

# =============================================================================
# (2) Cobertura de POSICAO: nossa base vs mercado
#
# NOTA: Valor_Ativo_mil no cons_YYYY.csv bruto tem formatacao MISTA entre
# linhas (algumas com "." como separador decimal, outras com "." como
# separador de milhar sem decimais -- ex. real achado nesta verificacao:
# "5.928135497153701" (decimal) vs "905.407.015" colado pelo Joao antes
# (milhar). fread importa a coluna como character por causa disso. Em vez
# de tentar desambiguar esse parsing arriscado, uso valor_posicao = peso *
# aum_prev do painel_multiativo_final.csv JA LIMPO (peso e aum_prev ja
# auditados e validados em rodadas anteriores) -- mais seguro.
# =============================================================================
cat("\n===== 2) Cobertura de POSICAO (via peso x aum_prev, ja validados) =====\n")
pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"))
vale_jul <- pp[ativo == "VALE ON N1 - VALE3" & ym == 202007]
vale_ago <- pp[ativo == "VALE ON N1 - VALE3" & ym == 202008]

vale_jul[, valor_posicao := peso * aum_prev]
vale_ago[, valor_posicao := peso * aum_prev]

valor_total_jul <- sum(vale_jul$valor_posicao, na.rm=TRUE)
valor_total_ago <- sum(vale_ago$valor_posicao, na.rm=TRUE)
cat(sprintf("Fundos com posicao direta em VALE3 (universo VALE3-anchored), jul/2020: %d | valor total: R$ %.1f milhoes\n",
            nrow(vale_jul), valor_total_jul/1e6))
cat(sprintf("Fundos com posicao direta em VALE3 (universo VALE3-anchored), ago/2020: %d | valor total: R$ %.1f milhoes\n",
            nrow(vale_ago), valor_total_ago/1e6))

valor_mercado_total <- qtd_ago_estoque <- NA  # placeholder, calculado depois com dado externo (shares outstanding)
cat(sprintf("\nValor de mercado da VALE3 (preco fechamento x acoes em circulacao) ainda NAO calculado -- precisa de dado externo de acoes em circulacao.\n"))

cat(sprintf("\n===== 3) Proxy de FLUXO LIQUIDO =====\n"))
delta_posicao <- valor_total_ago - valor_total_jul
cat(sprintf("Variacao da posicao agregada VALE3 (nossos fundos), jul->ago/2020: R$ %.1f milhoes (%s)\n",
            delta_posicao/1e6, ifelse(delta_posicao>=0,"compra liquida implicita","venda liquida implicita")))
cat(sprintf("Volume financeiro TOTAL negociado VALE3 na B3, ago/2020: R$ %.1f milhoes\n", vol_ago/1e6))
cat(sprintf("Proxy de fluxo / volume total negociado: %.3f%%\n", 100*abs(delta_posicao)/vol_ago))
