# =============================================================================
# _check_lookthrough_fic.R (temporario, NAO documentado no TCC ainda)
#
# Testa se a CVM faz "look-through" de FIC para o fundo-mestre: se um FIC
# so compra cotas de UM fundo-alvo, ele deveria aparecer com pesos
# IDENTICOS (ou quase) aos de outro fundo no mesmo mes -- porque estaria
# so herdando a composicao de outro fundo, nao decidindo a propria
# carteira. Usa o fundo 93386 (ARX FIC ACOES) que o Joao trouxe como
# exemplo, dez/2016 (mesma competencia da amostra que ele colou).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
SH_DIR <- "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF"

classifica_direta <- function(ativo) {
  ticker <- trimws(sub(".*- ", "", ativo))
  sufixo_chr <- sub(".*?([0-9]+)$", "\\1", ticker)
  tem_sufixo <- sufixo_chr != ticker
  sufixo_num <- rep(NA_integer_, length(ativo))
  sufixo_num[tem_sufixo] <- suppressWarnings(as.integer(sufixo_chr[tem_sufixo]))
  kw_excluir <- "cedid|recebid|[Ss]ubscri|[Cc]ertificado ou recibo de dep|omitid|Outras Ações|IBOVESPA|IBOV11"
  !is.na(sufixo_num) & sufixo_num %in% c(3,4,5,6,7,8,11) & !grepl(kw_excluir, ativo)
}

cat("===== Carregando cons_2016.csv =====\n")
cv <- fread(file.path(SH_DIR, "cons_2016.csv"), encoding = "UTF-8", showProgress = FALSE)
cv[, PESO := as.numeric(Participação_Ativo)]
cv_dez <- cv[`Data_Competência` == "2016-12-30"]
cat("Linhas em 2016-12-30:", nrow(cv_dez), "| fundos distintos:", uniqueN(cv_dez$Código), "\n\n")

alvo <- cv_dez[Código == 93386 & classifica_direta(Nome_Ativo)]
alvo[, ticker := trimws(sub(".*- ", "", Nome_Ativo))]
cat("===== Carteira do fundo 93386 (ARX FIC AÇÕES), 30/12/2016 =====\n")
print(alvo[order(-PESO), .(ticker, PESO = round(PESO,4))])
cat("Soma de peso:", round(sum(alvo$PESO),4), "\n\n")

# ---- comparar com TODOS os outros fundos na mesma competencia -------------
outros <- cv_dez[Código != 93386 & classifica_direta(Nome_Ativo)]
outros[, ticker := trimws(sub(".*- ", "", Nome_Ativo))]

vec_alvo <- setNames(alvo$PESO, alvo$ticker)
tickers_alvo <- names(vec_alvo)

# restringe a candidatos que tem pelo menos 8 dos mesmos tickers (deveria
# ser raro por acaso -- fundo tem so ~500 acoes possiveis, 15 tickers
# especificos batendo por coincidencia e muito improvavel)
candidatos <- outros[ticker %in% tickers_alvo, .(n_match = uniqueN(ticker)), by = Código][n_match >= 8]
cat("Fundos com >=8 dos mesmos tickers do 93386:", nrow(candidatos), "\n\n")

if (nrow(candidatos) > 0) {
  resultados <- list()
  for (cod in candidatos$Código) {
    cand <- outros[Código == cod]
    vec_cand <- setNames(cand$PESO, cand$ticker)
    common <- intersect(tickers_alvo, names(vec_cand))
    dif_abs_media <- mean(abs(vec_alvo[common] - vec_cand[common]))
    n_total_cand <- length(vec_cand)
    resultados[[length(resultados)+1]] <- data.table(
      cod_fundo = cod, n_tickers_comuns = length(common), n_tickers_fundo = n_total_cand,
      n_tickers_alvo = length(tickers_alvo), dif_abs_media = dif_abs_media)
  }
  R <- rbindlist(resultados)
  setorder(R, dif_abs_media)
  cat("===== Fundos mais parecidos com 93386 (menor diferenca media de peso) =====\n")
  print(R[1:min(10,.N)])

  cat("\n===== Detalhe do candidato MAIS parecido =====\n")
  cod_mais_parecido <- R$cod_fundo[1]
  cand <- outros[Código == cod_mais_parecido]
  comp <- merge(alvo[,.(ticker,peso_93386=PESO)], cand[,.(ticker,peso_candidato=PESO)], by="ticker", all=TRUE)
  print(comp[order(-peso_93386)])
} else {
  cat("NENHUM fundo com >=8 tickers em comum -- 93386 parece ter carteira UNICA no mercado.\n")
}

# ---- busca direta por fundos "ARX" na SH -----------------------------------
cat("\n===== Fundos com 'ARX' no nome/gestora, SH 2016 =====\n")
sh <- fread(file.path(SH_DIR, "SH_2016.csv"), select = c("COD_FUNDO","NOME_FUNDO","GESTORA"),
            encoding = "UTF-8", showProgress = FALSE)
arx <- unique(sh[grepl("ARX", NOME_FUNDO, ignore.case=TRUE) | grepl("ARX", GESTORA, ignore.case=TRUE),
                  .(COD_FUNDO, NOME_FUNDO, GESTORA)])
print(arx)
