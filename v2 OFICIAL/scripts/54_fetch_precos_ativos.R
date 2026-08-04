# =============================================================================
# 54_fetch_precos_ativos.R  (v2 OFICIAL -- teste_estrategia)
#
# FASE 0, passo 5: baixa preco/retorno (Yahoo Finance, mesma fonte/formato
# ja usada pra VALE3 e Ibovespa no R/05 e R/19) pra TODOS os 536 ativos
# distintos do painel multiativo completo (R/51) -- necessario pra Fase 2
# (separar efeito mecanico de preco de negociacao ativa).
#
# Mesmo padrao do R/05: curl pro endpoint de chart do Yahoo, cache em
# data/raw/yahoo_ativos/<TICKER>.json, pula se ja existir. Delay pequeno
# entre requisicoes pra nao ser bloqueado.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
OUTDIR <- file.path(REPO, "data/raw/yahoo_ativos")
dir.create(OUTDIR, recursive = TRUE, showWarnings = FALSE)

p <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_direto_completo.csv"), select = "ativo")
ativos <- unique(p$ativo)
tickers <- sort(unique(trimws(sub(".*- ", "", ativos))))
cat("Tickers a buscar:", length(tickers), "\n")

P1 <- as.integer(as.POSIXct("2015-12-01", tz = "UTC"))
P2 <- as.integer(as.POSIXct("2022-01-15", tz = "UTC"))

fetch_yahoo <- function(symbol, outfile) {
  if (file.exists(outfile) && file.size(outfile) > 50) return("cache")
  url <- sprintf(paste0("https://query1.finance.yahoo.com/v8/finance/chart/%s",
                        "?period1=%d&period2=%d&interval=1d"),
                 utils::URLencode(symbol, reserved = TRUE), P1, P2)
  system2("curl", c("-s", "-m", "30", "-A", shQuote("Mozilla/5.0"),
                    shQuote(url), "-o", shQuote(outfile)))
  Sys.sleep(0.25)
  "baixado"
}

status <- character(length(tickers))
for (i in seq_along(tickers)) {
  tk <- tickers[i]
  outfile <- file.path(OUTDIR, sprintf("%s.json", tk))
  status[i] <- tryCatch(fetch_yahoo(sprintf("%s.SA", tk), outfile), error = function(e) "erro")
  if (i %% 50 == 0) cat("...", i, "de", length(tickers), "\n")
}

cat("\nResumo do download:\n"); print(table(status))
cat("Arquivos salvos em", OUTDIR, "\n")
fwrite(data.table(ticker = tickers, status = status), file.path(REPO, "v2 OFICIAL/data/log_fetch_precos.csv"))
