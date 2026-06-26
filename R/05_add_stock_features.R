# =============================================================================
# 05_add_stock_features.R
#
# Passo 5: caracteristicas da ACAO (VALE3) ao painel (2016):
#   - preco_nominal : fechamento (close) da VALE3
#   - preco_ajust   : fechamento ajustado (adjclose, proventos/desdobramentos)
#   - beta_vale     : beta movel de 252 pregoes vs Ibovespa (cov/var)
#
# Todos medidos no ULTIMO PREGAO DO MES ANTERIOR a cada competencia (sem
# look-ahead). Fonte: Yahoo Finance (VALE3.SA e ^BVSP). Retornos SIMPLES;
# retorno da VALE pelo adjclose, do Ibov pelo close. Ver docs/log_decisoes.md.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(jsonlite)
})

PROJ_DIR <- Sys.getenv("PROJ_DIR",
                       unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
if (dir.exists(PROJ_DIR)) setwd(PROJ_DIR)

P1 <- 1388534400L  # 2014-01-01
P2 <- 1485907200L  # 2017-02-01

# ---- baixa do Yahoo se nao estiver em cache (data/raw) ----------------------
fetch_yahoo <- function(symbol, outfile) {
  if (file.exists(outfile)) return(invisible())
  url <- sprintf(paste0("https://query1.finance.yahoo.com/v8/finance/chart/%s",
                        "?period1=%d&period2=%d&interval=1d"),
                 utils::URLencode(symbol, reserved = TRUE), P1, P2)
  dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
  system2("curl", c("-s", "-m", "60", "-A", shQuote("Mozilla/5.0"),
                    shQuote(url), "-o", shQuote(outfile)))
}
fetch_yahoo("VALE3.SA", "data/raw/yahoo_vale3.json")
fetch_yahoo("^BVSP",    "data/raw/yahoo_ibov.json")

# ---- parse do JSON do Yahoo -------------------------------------------------
getv <- function(lst) vapply(lst, function(x) if (is.null(x)) NA_real_ else as.numeric(x), numeric(1))
parse_yahoo <- function(path) {
  res <- fromJSON(path, simplifyVector = FALSE)$chart$result[[1]]
  ts  <- vapply(res$timestamp, as.numeric, numeric(1))
  adj <- if (!is.null(res$indicators$adjclose))
           getv(res$indicators$adjclose[[1]]$adjclose) else NA_real_
  data.table(
    date     = as.Date(as.POSIXct(ts, origin = "1970-01-01", tz = "America/Sao_Paulo")),
    close    = getv(res$indicators$quote[[1]]$close),
    adjclose = adj
  )
}
v  <- parse_yahoo("data/raw/yahoo_vale3.json")
ib <- parse_yahoo("data/raw/yahoo_ibov.json")
# feriados em que o Yahoo retorna close = null (B3 fechada): poucos e legitimos.
# remove-os antes de calcular retornos; se aparecerem MUITOS NAs e' sinal de
# erro de parsing -> aborta (mantem o espirito do parsing critico).
na_v <- sum(is.na(v$close) | is.na(v$adjclose)); na_ib <- sum(is.na(ib$close))
cat("Dias com NA removidos -> VALE3:", na_v, "| Ibov:", na_ib, "\n")
stopifnot(na_v <= 15L, na_ib <= 15L)
v  <- v[!is.na(close) & !is.na(adjclose)]
ib <- ib[!is.na(close)]

# ---- retornos simples e beta movel 252 pregoes ------------------------------
stock <- merge(v[, .(date, close, adjclose)], ib[, .(date, ibov = close)], by = "date")
setorder(stock, date)
stock[, r_vale := adjclose / shift(adjclose) - 1]
stock[, r_ibov := ibov / shift(ibov) - 1]

W <- 252L
stock[, mx  := frollmean(r_vale, W)]
stock[, my  := frollmean(r_ibov, W)]
stock[, mxy := frollmean(r_vale * r_ibov, W)]
stock[, myy := frollmean(r_ibov * r_ibov, W)]
stock[, beta_vale := (mxy - mx * my) / (myy - my * my)]  # cov/var (pop. cancela)

# ---- timing das variaveis de mercado ---------------------------------------
stock[, ymk := year(date) * 100L + month(date)]
# (a) predeterminado: ultimo pregao do mes ANTERIOR (p/ previsao)
mlast <- stock[stock[, .I[which.max(date)], by = ymk]$V1,
               .(ymk, data_ref = date,
                 preco_nominal = close, preco_ajust = adjclose, beta_vale)]
# (b) contemporaneo: MEDIA do mes t (p/ explicacao)
avgm <- stock[, .(preco_mes = mean(close), beta_mes = mean(beta_vale, na.rm = TRUE)), by = ymk]

# ---- painel: junta pelo MES ANTERIOR (predeterminado) e pela MEDIA do mes t -
painel <- fread("data/processed/painel_vale_itau_2016_features.csv")
painel[, ymk_prev := ifelse(mes == 1L, (ano - 1L) * 100L + 12L, ano * 100L + (mes - 1L))]
pan2 <- merge(painel, mlast, by.x = "ymk_prev", by.y = "ymk", all.x = TRUE)
pan2[, ymk_prev := NULL]
pan2[, ymk_cur := ano * 100L + mes]
pan2 <- merge(pan2, avgm, by.x = "ymk_cur", by.y = "ymk", all.x = TRUE)
pan2[, ymk_cur := NULL]

# ---- auditoria + salvar -----------------------------------------------------
cat("Fund-months:", nrow(pan2),
    "| sem preco/beta (NA):", pan2[is.na(beta_vale), .N], "\n")
cat("Pares (fundo,mes) duplicados:", pan2[, .N, by = .(cod_fundo, data)][N > 1, .N], "\n")
cat("\nMedicao por mes (ultimo pregao do mes anterior):\n")
print(unique(pan2[order(data), .(competencia = data, data_ref,
                                 preco_nominal, preco_ajust,
                                 beta_vale = round(beta_vale, 3))]))

fwrite(pan2, "data/processed/painel_vale_itau_2016_full.csv")
cat("\nResumo beta_vale:\n"); print(summary(pan2$beta_vale))
cat("\nOK - painel final salvo em data/processed/painel_vale_itau_2016_full.csv\n")
