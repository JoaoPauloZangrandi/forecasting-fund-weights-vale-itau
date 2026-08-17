# =============================================================================
# _check_cobertura_mercado_vale3_multi.R (temporario, NAO documentado no TCC)
#
# Mesma verificacao do script anterior (_check_cobertura_mercado_vale3.R),
# generalizada para varios meses: jan/2020 e dez/2020, alem de ago/2020
# ja feito. Reusa a mesma logica (posicao via peso*aum_prev do painel
# ja limpo; volume/preco via COTAHIST oficial B3).
#
# LIMITACAO que fica mais forte aqui: acoes em circulacao da VALE3 so
# tenho o numero oficial de 31/12/2020 (5.129.910.942, do 20-F). Uso o
# MESMO numero pros 3 meses por falta de dado mes a mes -- para jan/2020
# isso e uma aproximacao mais fraca (quase 1 ano de distancia) do que
# para dez/2020 (mesma data) ou ago/2020 (~4 meses de distancia).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
COTA <- file.path(REPO, "data/raw/cotahist/COTAHIST_A2020.TXT")
ACOES_CIRCULACAO <- 5129910942  # 31/12/2020, Formulario 20-F Vale

# ---- COTAHIST: preco de fechamento + volume mensal, VALE3, mercado a vista --
con <- file(COTA, "r"); lines <- readLines(con, n = -1L, encoding = "latin1"); close(con)
lines01 <- lines[substr(lines, 1, 2) == "01"]
codneg <- trimws(substr(lines01, 13, 24))
tpmerc <- substr(lines01, 25, 27)
sub <- lines01[codneg == "VALE3" & tpmerc == "010"]
dt <- data.table(
  data = as.Date(substr(sub,3,10), format="%Y%m%d"),
  preult = as.numeric(substr(sub,109,121))/100,
  voltot = as.numeric(substr(sub,171,188))/100
)
dt[, ym := year(data)*100L + month(data)]

# ---- painel multiativo (posicao VALE3, ja limpo) ---------------------------
pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"))
vale <- pp[ativo == "VALE ON N1 - VALE3"]
vale[, valor_posicao := peso * aum_prev]
pos_mensal <- vale[, .(n_fundos = .N, valor_total = sum(valor_posicao, na.rm=TRUE)), by = ym]
setorder(pos_mensal, ym)

analisa_mes <- function(ym_atual, ym_anterior, rotulo) {
  cat("\n============================================================\n")
  cat(rotulo, "(ym =", ym_atual, ")\n")
  cat("============================================================\n")

  preco_fim_mes <- dt[ym == ym_atual][which.max(data), preult]
  vol_mes <- dt[ym == ym_atual, sum(voltot)]
  n_pregoes <- dt[ym == ym_atual, .N]
  cat(sprintf("Preco fechamento (ultimo pregao do mes): R$ %.2f | pregoes no mes: %d\n", preco_fim_mes, n_pregoes))
  cat(sprintf("Volume financeiro TOTAL negociado no mes: R$ %.1f milhoes\n", vol_mes/1e6))

  valor_mercado <- preco_fim_mes * ACOES_CIRCULACAO
  cat(sprintf("Valor de mercado estimado VALE3 (preco x %.0f mi acoes): R$ %.1f bilhoes\n",
              ACOES_CIRCULACAO/1e6, valor_mercado/1e9))

  linha_atual <- pos_mensal[ym == ym_atual]
  if (nrow(linha_atual) == 0) { cat("SEM DADO de posicao para este mes no painel.\n"); return(invisible(NULL)) }
  cat(sprintf("Fundos com posicao VALE3 na nossa base: %d | valor total: R$ %.1f milhoes\n",
              linha_atual$n_fundos, linha_atual$valor_total/1e6))
  cobertura <- linha_atual$valor_total / valor_mercado
  cat(sprintf("COBERTURA DE POSICAO: %.2f%%\n", 100*cobertura))

  linha_ant <- pos_mensal[ym == ym_anterior]
  if (nrow(linha_ant) == 0) {
    cat("SEM DADO de posicao no mes anterior (", ym_anterior, ") -- nao da pra calcular fluxo.\n")
    return(invisible(NULL))
  }
  delta <- linha_atual$valor_total - linha_ant$valor_total
  cat(sprintf("Posicao mes anterior (ym=%d): R$ %.1f milhoes\n", ym_anterior, linha_ant$valor_total/1e6))
  cat(sprintf("Variacao (proxy de fluxo liquido): R$ %.1f milhoes (%s)\n",
              delta/1e6, ifelse(delta>=0,"compra liquida implicita","venda liquida implicita")))
  cat(sprintf("PROXY FLUXO / VOLUME TOTAL NEGOCIADO: %.2f%%\n", 100*abs(delta)/vol_mes))
}

analisa_mes(202001, 201912, "JANEIRO/2020")
analisa_mes(202008, 202007, "AGOSTO/2020 (repetindo p/ conferir consistencia)")
analisa_mes(202012, 202011, "DEZEMBRO/2020")

cat("\n============================================================\n")
cat("RESUMO -- cobertura de posicao por mes\n")
cat("============================================================\n")
print(pos_mensal[ym %in% c(202001,202008,202012)])
