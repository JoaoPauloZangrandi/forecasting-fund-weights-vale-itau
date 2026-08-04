# =============================================================================
# 56_precos_cotahist_b3.R  (v2 OFICIAL -- teste_estrategia)
#
# FASE 0, passo 6 (substitui o R/54/55): preco/retorno mensal por ativo pra
# COBERTURA COMPLETA -- fonte oficial B3 (COTAHIST), nao Yahoo Finance.
#
# Motivo da troca (30/07/2026): Yahoo so guarda o ticker ATUAL de cada
# empresa -- 201 de 536 ativos falharam, incluindo empresas bem ativas
# (JBS, BRF, Natura) que so trocaram de codigo em reorganizacoes societarias
# (confirmado: JBSS3 -> JBSS32). A COTAHIST registra a cotacao sob o codigo
# EXATO negociado em cada dia historico, entao nao sofre desse problema.
#
# Layout do arquivo (validado contra preco real da VALE3 em 2020, bateu
# exatamente: R$54 em jan/2020, R$87 em dez/2020):
#   tipo registro: pos 1-2 (so usamos "01", cotacao diaria)
#   data:          pos 3-10 (AAAAMMDD)
#   codneg (ticker): pos 13-24
#   tpmerc:        pos 25-27 (mercado; "010" = mercado a vista/lote padrao)
#   preult (fechamento): pos 109-121, 2 casas decimais implicitas (/100)
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
RAWDIR <- file.path(REPO, "data/raw/cotahist")
dir.create(RAWDIR, recursive = TRUE, showWarnings = FALSE)

# ---- passo 1: baixar + descompactar os anos que faltam ---------------------
for (y in 2016:2021) {
  zipf <- file.path(RAWDIR, sprintf("COTAHIST_A%d.ZIP", y))
  txtf <- file.path(RAWDIR, sprintf("COTAHIST_A%d.TXT", y))
  if (!file.exists(txtf)) {
    if (!file.exists(zipf)) {
      cat("Baixando", y, "...\n"); flush.console()
      url <- sprintf("https://bvmf.bmfbovespa.com.br/InstDados/SerHist/COTAHIST_A%d.ZIP", y)
      system2("curl", c("-s", "-m", "120", "-A", shQuote("Mozilla/5.0"), shQuote(url), "-o", shQuote(zipf)))
    }
    unzip(zipf, exdir = RAWDIR)
  }
  cat(y, "- OK,", file.size(txtf), "bytes\n")
}

# ---- passo 2: parsear tipo-01 (cotacao diaria), mercado a vista -----------
todos_ativos <- unique(trimws(sub(".*- ", "",
  unique(fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_direto_completo.csv"),
               select = "ativo")$ativo))))
cat("\nTickers-alvo (painel multiativo):", length(todos_ativos), "\n")

res <- vector("list", 6)
for (i in seq_along(2016:2021)) {
  y <- (2016:2021)[i]
  cat("== parseando", y, "==\n"); flush.console()
  con <- file(file.path(RAWDIR, sprintf("COTAHIST_A%d.TXT", y)), "r")
  lines <- readLines(con, n = -1L, encoding = "latin1"); close(con)
  lines <- lines[substr(lines, 1, 2) == "01"]
  codneg <- trimws(substr(lines, 13, 24))
  keep <- codneg %in% todos_ativos & substr(lines, 25, 27) == "010"
  lines <- lines[keep]; codneg <- codneg[keep]
  dt <- data.table(ticker = codneg,
                    data = as.Date(substr(lines, 3, 10), format = "%Y%m%d"),
                    preult = as.numeric(substr(lines, 109, 121)) / 100)
  cat("  linhas mercado a vista, tickers-alvo:", nrow(dt), "|", uniqueN(dt$ticker), "tickers distintos\n")
  res[[i]] <- dt
  rm(lines, codneg, dt); gc(FALSE)
}
D <- rbindlist(res)
cat("\nTotal linhas diarias (todos os anos):", nrow(D), "| tickers distintos:", uniqueN(D$ticker),
    "de", length(todos_ativos), "no painel\n")

# ---- passo 3: preco/retorno MENSAL (ultimo pregao do mes) -----------------
setorder(D, ticker, data)
D[, ymk := year(data)*100L + month(data)]
mlast <- D[D[, .I[which.max(data)], by = .(ticker, ymk)]$V1, .(ticker, ymk, data_ref = data, preco = preult)]
setorder(mlast, ticker, ymk)
mlast[, retorno := preco/shift(preco) - 1, by = ticker]

cat("\nAtivo-mes com preco (COTAHIST):", nrow(mlast), "\n")
cat("Ativo-mes com retorno calculavel:", mlast[!is.na(retorno), .N], "\n")

faltando <- setdiff(todos_ativos, unique(mlast$ticker))
cat("\nTickers do painel SEM nenhuma cotacao na COTAHIST (mercado a vista, 2016-2021):", length(faltando), "\n")
if (length(faltando) > 0) print(faltando)

fwrite(mlast, file.path(REPO, "v2 OFICIAL/data/precos_mensais_ativos_b3.csv"))
if (length(faltando) > 0) {
  fwrite(data.table(ticker = faltando), file.path(REPO, "v2 OFICIAL/data/precos_mensais_faltando_b3.csv"))
}
cat("\nOK - salvo em 'v2 OFICIAL/data/precos_mensais_ativos_b3.csv'\n")
