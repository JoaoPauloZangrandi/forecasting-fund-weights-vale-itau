# =============================================================================
# 57_precos_tickers_sucessores.R  (v2 OFICIAL -- teste_estrategia)
#
# FASE 0, passo 7: resolve parte dos 23 tickers sem preco (R/56) que sao
# codigos DESATUALIZADOS no cadastro de posicao da CVM -- a empresa trocou
# de ticker (fusao, reestruturacao, criacao de holding) e a CVM continuou
# usando o nome/codigo antigo no relatorio de carteira do fundo, mas o
# mercado (e a COTAHIST) so tem o codigo NOVO.
#
# Mapeamento pesquisado manualmente (30/07/2026), um por um:
#  - KROT11 (Kroton Units) -> KROT3 ate out/2019 -> COGN3 dai em diante
#    (criacao da holding Cogna Educacao; KROT11 ja nao existia em 2016,
#    a conversao de Units p/ ON aconteceu antes do inicio da amostra)
#  - UNID3 (Unidas S.A.)   -> LCAM3 (Cia de Locacao das Americas, ticker
#    que a Unidas manteve apos a fusao Unidas+LocaMerica em 2018)
#  - IMCH3 (IMC Holdings)  -> MEAL3 (mesma empresa, ticker atual)
#  - LLISE3 (Le Lis Blanc) -> LLIS3 (Restoque; "LLISE3" e' provavelmente
#    um erro de grafia/parsing no cadastro da CVM, ticker real e' LLIS3)
#  - HGTX4 (Cia Hering PN) -> HGTX3 (Hering so tinha classe ON negociada;
#    a PN "HGTX4" parece nao ter tido negociacao de mercado relevante --
#    peso mediano ja era 0 no nosso painel, ajuste de baixa prioridade)
#
# Os outros 18 tickers (GOLL3, BPAC7, IDVL11, GETI3/4, VPSC4/VPTA4, CRUZ3,
# LOAR4, GMEC4, DHBI4, PASS5/6, QVQP3, HEDA3, MSTR3, SJHO4, CBOH3) foram
# checados individualmente (busca ampla, 6 anos, qualquer tipo de mercado)
# e permanecem genuinamente ausentes da COTAHIST -- ou fecharam capital
# antes de 2016 (Souza Cruz/CRUZ3, confirmado: takeover privado concluido
# antes do inicio da amostra), ou sao classes de acao raramente/nunca
# negociadas (a maioria com peso mediano ~0 no nosso painel).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
RAWDIR <- file.path(REPO, "data/raw/cotahist")

sucessores <- c("KROT3", "COGN3", "LCAM3", "MEAL3", "LLIS3", "HGTX3")

res <- vector("list", 6)
for (i in seq_along(2016:2021)) {
  y <- (2016:2021)[i]
  con <- file(file.path(RAWDIR, sprintf("COTAHIST_A%d.TXT", y)), "r")
  lines <- readLines(con, n = -1L, encoding = "latin1"); close(con)
  lines <- lines[substr(lines, 1, 2) == "01"]
  codneg <- trimws(substr(lines, 13, 24))
  keep <- codneg %in% sucessores & substr(lines, 25, 27) == "010"
  lines <- lines[keep]; codneg <- codneg[keep]
  res[[i]] <- data.table(ticker = codneg, data = as.Date(substr(lines, 3, 10), format = "%Y%m%d"),
                          preult = as.numeric(substr(lines, 109, 121)) / 100)
  rm(lines, codneg); gc(FALSE)
}
D <- rbindlist(res)
cat("Linhas encontradas pros sucessores:\n"); print(D[, .N, by = ticker])

setorder(D, ticker, data)
D[, ymk := year(data)*100L + month(data)]
mlast <- D[D[, .I[which.max(data)], by = .(ticker, ymk)]$V1, .(ticker, ymk, data_ref = data, preco = preult)]
setorder(mlast, ticker, ymk)
mlast[, retorno := preco/shift(preco) - 1, by = ticker]

# ---- monta a serie do ticker ORIGINAL (o que a CVM usa) a partir do sucessor
mapa_simples <- list(UNID3 = "LCAM3", IMCH3 = "MEAL3", LLISE3 = "LLIS3", HGTX4 = "HGTX3")
subst_simples <- rbindlist(lapply(names(mapa_simples), function(orig) {
  s <- mlast[ticker == mapa_simples[[orig]]]
  if (nrow(s) == 0) return(NULL)
  s[, ticker := orig]
}))

# KROT11: KROT3 ate 2019-10 (inclusive), COGN3 de 2019-11 em diante
corte <- 201910L
krot_parte1 <- mlast[ticker == "KROT3" & ymk <= corte][, ticker := "KROT11"]
krot_parte2 <- mlast[ticker == "COGN3" & ymk > corte][, ticker := "KROT11"]
krot11 <- rbindlist(list(krot_parte1, krot_parte2))
setorder(krot11, ymk)
# recalcula retorno DENTRO da serie substituta unificada (evita salto artificial na troca de ticker)
krot11[, retorno := preco/shift(preco) - 1]

final <- rbindlist(list(subst_simples, krot11), use.names = TRUE)
cat("\nAtivo-mes reconstruidos via ticker sucessor:\n"); print(final[, .N, by = ticker])

fwrite(final, file.path(REPO, "v2 OFICIAL/data/precos_mensais_via_sucessor.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/precos_mensais_via_sucessor.csv'\n")
