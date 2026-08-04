# =============================================================================
# 55_parse_precos_ativos.R  (v2 OFICIAL -- teste_estrategia)
#
# FASE 0, passo 6: parseia os 536 JSONs baixados no R/54 (mesmo parser do
# R/05/R/19) e monta um painel MENSAL de preco/retorno por ativo -- ultimo
# pregao de cada mes (mesma convencao "predeterminado" do resto do projeto).
#
# Alguns tickers podem "baixar" com sucesso (curl exit 0) mas o JSON conter
# um erro do Yahoo (ticker deslistado, trocou de codigo, etc.) -- checagem
# explicita antes de aceitar cada um.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
INDIR <- file.path(REPO, "data/raw/yahoo_ativos")

tickers <- sort(sub("\\.json$", "", list.files(INDIR, pattern = "\\.json$")))
cat("Arquivos JSON encontrados:", length(tickers), "\n")

getv <- function(lst) vapply(lst, function(x) if (is.null(x)) NA_real_ else as.numeric(x), numeric(1))

parse_um <- function(tk) {
  path <- file.path(INDIR, sprintf("%s.json", tk))
  j <- tryCatch(fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(j)) return(list(ok = FALSE, motivo = "json_invalido", dt = NULL))
  err <- j$chart$error
  if (!is.null(err)) return(list(ok = FALSE, motivo = paste("yahoo_erro:", err$description), dt = NULL))
  res <- j$chart$result
  if (is.null(res) || length(res) == 0) return(list(ok = FALSE, motivo = "sem_resultado", dt = NULL))
  res <- res[[1]]
  ts <- res$timestamp
  if (is.null(ts) || length(ts) == 0) return(list(ok = FALSE, motivo = "sem_timestamp", dt = NULL))
  ts <- vapply(ts, as.numeric, numeric(1))
  adj <- if (!is.null(res$indicators$adjclose)) getv(res$indicators$adjclose[[1]]$adjclose) else rep(NA_real_, length(ts))
  close <- getv(res$indicators$quote[[1]]$close)
  dt <- data.table(date = as.Date(as.POSIXct(ts, origin = "1970-01-01", tz = "America/Sao_Paulo")),
                    close = close, adjclose = adj)
  dt <- dt[!is.na(close)]
  if (nrow(dt) < 30) return(list(ok = FALSE, motivo = "poucos_pregoes", dt = NULL))
  list(ok = TRUE, motivo = "ok", dt = dt)
}

status <- vector("character", length(tickers))
serie_mensal <- vector("list", length(tickers))
for (i in seq_along(tickers)) {
  r <- parse_um(tickers[i])
  status[i] <- r$motivo
  if (r$ok) {
    dtb <- r$dt
    # usa adjclose se disponivel (proventos/desdobramentos), senao close
    dtb[, preco := fifelse(!is.na(adjclose), adjclose, close)]
    setorder(dtb, date)
    dtb[, ymk := year(date)*100L + month(date)]
    mlast <- dtb[dtb[, .I[which.max(date)], by = ymk]$V1, .(ymk, data_ref = date, preco)]
    setorder(mlast, ymk)
    mlast[, retorno := preco/shift(preco) - 1]
    mlast[, ticker := tickers[i]]
    serie_mensal[[i]] <- mlast
  }
  if (i %% 100 == 0) cat("...", i, "de", length(tickers), "\n")
}

cat("\nResumo do parsing:\n"); print(table(status))

precos <- rbindlist(serie_mensal, use.names = TRUE, fill = TRUE)
cat("\nAtivos com serie de preco valida:", uniqueN(precos$ticker), "de", length(tickers), "\n")
cat("Fundo-mes... digo, ativo-mes com preco:", nrow(precos), "\n")
cat("Ativo-mes com retorno calculavel (nao-NA):", precos[!is.na(retorno), .N], "\n")

falhas <- data.table(ticker = tickers, motivo = status)[status != "ok"]
cat("\nTickers SEM preco valido (", nrow(falhas), "):\n", sep="")
print(falhas)

fwrite(precos, file.path(REPO, "v2 OFICIAL/data/precos_mensais_ativos.csv"))
fwrite(falhas, file.path(REPO, "v2 OFICIAL/data/precos_mensais_falhas.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/precos_mensais_ativos.csv' (+ log de falhas)\n")
