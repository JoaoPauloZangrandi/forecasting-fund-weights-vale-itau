# =============================================================================
# 13_add_stock_allyears.R
#
# Generaliza o Passo 5 (preco e beta da VALE3) para 2016-2021.
# preco_nominal (close), preco_ajust (adjclose), beta_vale (cov/var movel 252
# pregoes vs Ibovespa, retorno simples). Medidos no ULTIMO PREGAO DO MES ANTERIOR
# a cada competencia. Fonte Yahoo (data/raw/yahoo_*.json, range 2014-2022).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================

suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

getv <- function(lst) vapply(lst, function(x) if (is.null(x)) NA_real_ else as.numeric(x), numeric(1))
parse_yahoo <- function(path) {
  res <- fromJSON(path, simplifyVector = FALSE)$chart$result[[1]]
  ts  <- vapply(res$timestamp, as.numeric, numeric(1))
  adj <- if (!is.null(res$indicators$adjclose)) getv(res$indicators$adjclose[[1]]$adjclose) else NA_real_
  data.table(date = as.Date(as.POSIXct(ts, origin = "1970-01-01", tz = "America/Sao_Paulo")),
             close = getv(res$indicators$quote[[1]]$close), adjclose = adj)
}
v  <- parse_yahoo(file.path(REPO, "data/raw/yahoo_vale3.json"))
ib <- parse_yahoo(file.path(REPO, "data/raw/yahoo_ibov.json"))
cat("Dias com NA removidos -> VALE3:", sum(is.na(v$close) | is.na(v$adjclose)),
    "| Ibov:", sum(is.na(ib$close)), "\n")
v  <- v[!is.na(close) & !is.na(adjclose)]
ib <- ib[!is.na(close)]

stk <- merge(v[, .(date, close, adjclose)], ib[, .(date, ibov = close)], by = "date")
setorder(stk, date)
stk[, r_vale := adjclose / shift(adjclose) - 1]
stk[, r_ibov := ibov / shift(ibov) - 1]
W <- 252L
stk[, beta_vale := (frollmean(r_vale * r_ibov, W) - frollmean(r_vale, W) * frollmean(r_ibov, W)) /
                   (frollmean(r_ibov * r_ibov, W) - frollmean(r_ibov, W)^2)]
stk[, ymk := year(date) * 100L + month(date)]
mlast <- stk[stk[, .I[which.max(date)], by = ymk]$V1,
             .(ymk, data_ref = date, preco_nominal = close, preco_ajust = adjclose, beta_vale)]

painel <- fread(file.path(REPO, "data/processed/painel_vale_itau_2016_2021_features.csv"))
painel[, ymk_prev := ifelse(mes == 1L, (ano - 1L) * 100L + 12L, ano * 100L + (mes - 1L))]
pan2 <- merge(painel, mlast, by.x = "ymk_prev", by.y = "ymk", all.x = TRUE)
pan2[, ymk_prev := NULL]

cat("==== STOCK FEATURES: medicao por mes ====\n")
print(mlast[ymk >= 201512 & ymk <= 202111][order(ymk)][, .(ymk, data_ref,
            preco_nominal, beta_vale = round(beta_vale, 3))])
cat("\nObs:", nrow(pan2), "| NAs beta:", pan2[is.na(beta_vale), .N],
    "| NAs preco:", pan2[is.na(preco_nominal), .N], "\n")
cat("Beta por ano:\n")
print(pan2[, .(beta_med = round(mean(beta_vale), 3),
               preco_med = round(mean(preco_nominal), 2)), by = ano][order(ano)])

fwrite(pan2, file.path(REPO, "data/processed/painel_vale_itau_2016_2021_full.csv"))
cat("\nOK - painel FINAL salvo em data/processed/painel_vale_itau_2016_2021_full.csv\n")
