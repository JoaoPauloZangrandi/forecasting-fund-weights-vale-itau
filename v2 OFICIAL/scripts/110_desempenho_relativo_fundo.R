# =============================================================================
# 110_desempenho_relativo_fundo.R  (v2 OFICIAL / trilha TORNEIO)
#
# Peca comum aos tres testes de torneio (scripts 111, 112, 113). Constroi,
# para cada fundo e cada mes, o desempenho ACUMULADO NO ANO CIVIL e a posicao
# relativa dele, que e' a variavel de tratamento de toda a literatura de
# torneio desde Brown, Harlow e Starks (1996).
#
# Duas medidas, propositalmente (a literatura usa as duas e elas nao sao a
# mesma coisa):
#   (1) excesso_acum  = retorno acumulado da cota no ano - retorno acumulado
#                       do Ibovespa no mesmo intervalo. E' o "bater o
#                       benchmark", que e' como a industria brasileira
#                       comunica desempenho de fundo de acoes.
#   (2) rank_pct      = percentil do retorno acumulado do fundo entre todos
#                       os fundos do universo naquele mes. E' o torneio no
#                       sentido literal de BHS: o que importa e' a posicao na
#                       lista, nao a distancia ate o indice.
#
# Fontes (ambas ja no repo, nada novo e' extraido):
#   data/retorno_fundo_mensal.csv      (script 60, retorno da COTA)
#   data/benchmark_ibovespa_mensal.csv (script 106, Yahoo ^BVSP)
#
# Saida: data/torneio_desempenho_fundo_mes.csv
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
DD <- file.path(REPO, "v2 OFICIAL/data")

R <- fread(file.path(DD, "retorno_fundo_mensal.csv"))
B <- fread(file.path(DD, "benchmark_ibovespa_mensal.csv"))
U <- fread(file.path(DD, "universo_completo_gestora.csv"))

R[, cod_fundo := as.character(cod_fundo)]
U[, cod_fundo := as.character(cod_fundo)]

cat("Retorno bruto:", nrow(R), "linhas |", uniqueN(R$cod_fundo), "fundos\n")
cat("Ibovespa:", nrow(B), "meses |", min(B$ymk), "a", max(B$ymk), "\n")

# --- recorte: universo do TCC e janela do TCC -------------------------------
R <- R[cod_fundo %in% U$cod_fundo]
R <- R[ymk >= 201601L & ymk <= 202112L]
B <- B[ymk >= 201601L & ymk <= 202112L, .(ymk, retorno_ibov)]
R <- merge(R[, .(cod_fundo, ymk, retorno_fundo)], B, by = "ymk")
R <- R[is.finite(retorno_fundo) & is.finite(retorno_ibov)]
cat("Apos recorte universo+janela:", nrow(R), "linhas |", uniqueN(R$cod_fundo), "fundos\n")

R[, ano := ymk %/% 100L]
R[, mes := ymk %% 100L]
setorder(R, cod_fundo, ymk)

# --- exige serie COMPLETA de janeiro ate o mes corrente, dentro do ano ------
# Sem isso, um fundo que so aparece em setembro entraria com "acumulado do
# ano" que na verdade e' acumulado de um trimestre, e a comparacao entre
# fundos deixaria de ser entre iguais.
R[, n_meses_ano := seq_len(.N), by = .(cod_fundo, ano)]
R[, completo := (n_meses_ano == mes)]

R[, ret_acum_fundo := cumprod(1 + retorno_fundo) - 1, by = .(cod_fundo, ano)]
R[, ret_acum_ibov  := cumprod(1 + retorno_ibov)  - 1, by = .(cod_fundo, ano)]
R[, excesso_acum := ret_acum_fundo - ret_acum_ibov]

R <- R[completo == TRUE]
cat("Apos exigir serie completa desde janeiro:", nrow(R), "linhas |",
    uniqueN(R$cod_fundo), "fundos\n")

# --- percentil de ranking dentro do mes ------------------------------------
R[, n_fundos_rank := .N, by = ymk]
R[, rank_pct := frank(ret_acum_fundo, ties.method = "average") / .N, by = ymk]

# --- classificacao de torneio ----------------------------------------------
# Duas datas de corte: junho (desenho classico BHS, avaliacao semestral) e
# setembro (desenho de Li, Tiwari e Tong 2022, que olha o 4o trimestre).
interim <- function(mes_corte, sufixo) {
  X <- R[mes == mes_corte, .(cod_fundo, ano,
                             excesso_interim = excesso_acum,
                             rank_interim = rank_pct)]
  X[, perdedor_bench := as.integer(excesso_interim < 0)]
  X[, quartil_rank := cut(rank_interim, breaks = c(0, .25, .50, .75, 1.0),
                          labels = 1:4, include.lowest = TRUE)]
  setnames(X, c("excesso_interim","rank_interim","perdedor_bench","quartil_rank"),
           paste0(c("excesso_interim","rank_interim","perdedor_bench","quartil_rank"), sufixo))
  X
}
I6 <- interim(6L, "_jun")
I9 <- interim(9L, "_set")

OUT <- merge(R[, .(cod_fundo, ym = ymk, ano, mes, retorno_fundo, retorno_ibov,
                   ret_acum_fundo, excesso_acum, rank_pct, n_fundos_rank)],
             I6, by = c("cod_fundo","ano"), all.x = TRUE)
OUT <- merge(OUT, I9, by = c("cod_fundo","ano"), all.x = TRUE)
setorder(OUT, cod_fundo, ym)

fwrite(OUT, file.path(DD, "torneio_desempenho_fundo_mes.csv"))

cat("\n==== Resumo ====\n")
cat("Linhas gravadas:", nrow(OUT), "| fundos:", uniqueN(OUT$cod_fundo), "\n")
cat("Fundos com classificacao de junho:", uniqueN(OUT[!is.na(perdedor_bench_jun)]$cod_fundo), "\n")
cat("\nFundo-mes por ano:\n"); print(OUT[, .N, by = ano][order(ano)])
cat("\nProporcao de fundos abaixo do Ibovespa no acumulado ate junho, por ano:\n")
print(unique(OUT[mes == 6L, .(cod_fundo, ano, perdedor_bench_jun)])[
        , .(n = .N, pct_perdedor = round(100*mean(perdedor_bench_jun), 1)), by = ano][order(ano)])
cat("\nExcesso acumulado ate junho (distribuicao entre fundos-ano):\n")
print(summary(unique(OUT[mes == 6L, .(cod_fundo, ano, excesso_interim_jun)])$excesso_interim_jun))
cat("\nOK - 110 concluido\n")
