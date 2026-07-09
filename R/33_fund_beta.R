# =============================================================================
# 33_fund_beta.R
#
# Adiciona o BETA DO PROPRIO FUNDO (nao da VALE3) como caracteristica de
# controle, pedido do orientador. Metodologia identica ao beta da VALE3
# (R/13): retorno diario da COTA do fundo (base SH) vs Ibovespa, cov/var em
# janela movel de 252 pregoes, medido no ULTIMO PREGAO DO MES ANTERIOR
# (predeterminado, sem look-ahead). Interpretacao: quanto mais "parecido com
# o mercado" e o retorno do fundo, maior o beta -- proxy para o quao
# equity-oriented / agressivo e o mandato do fundo (distinto do AUM/cotistas/
# FIC, que descrevem porte e estrutura, nao risco).
#
# Escopo: os 307 fundos da amostra de regressao principal (VALE3, R/14).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA_DIR <- "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF"

alvo <- unique(fread(file.path(REPO,"data/processed/painel_vale_itau_2016_2021_filtrado.csv"))$cod_fundo)
cat("Fundos-alvo (amostra de regressao VALE3):", length(alvo), "\n")

pdate <- function(x){ x<-trimws(as.character(x)); o<-as.Date(rep(NA_character_,length(x)))
  for(f in c("%Y-%m-%d","%d/%m/%Y")){m<-is.na(o);if(!any(m))break;o[m]<-as.Date(x[m],format=f)};o }

sh_all <- list()
for (y in 2016:2021) {
  sh <- fread(file.path(DATA_DIR, sprintf("SH_%d.csv", y)), encoding = "UTF-8", showProgress = FALSE,
              select = c("COD_FUNDO","DATA","COTA"))
  sh <- sh[COD_FUNDO %in% alvo]
  sh[, DT := pdate(DATA)][, cota_num := as.numeric(gsub(",", ".", COTA, fixed = TRUE))]
  sh_all[[length(sh_all)+1L]] <- sh[, .(cod_fundo = COD_FUNDO, data = DT, cota = cota_num)]
  cat("ano", y, "cotas lidas\n"); flush.console()
}
cotas <- rbindlist(sh_all)
cotas <- cotas[!is.na(cota) & cota > 0]
setorder(cotas, cod_fundo, data)
cotas[, r_fundo := cota/shift(cota) - 1, by = cod_fundo]
cat("\nRetornos diarios de cota calculados:", cotas[is.finite(r_fundo), .N], "obs |",
    uniqueN(cotas$cod_fundo), "fundos\n")

# --- Ibovespa (mesma fonte/parse do R/13) ---
getv <- function(lst) vapply(lst, function(x) if (is.null(x)) NA_real_ else as.numeric(x), numeric(1))
parse_yahoo <- function(path) {
  res <- fromJSON(path, simplifyVector = FALSE)$chart$result[[1]]
  ts  <- vapply(res$timestamp, as.numeric, numeric(1))
  data.table(date = as.Date(as.POSIXct(ts, origin = "1970-01-01", tz = "America/Sao_Paulo")),
             close = getv(res$indicators$quote[[1]]$close))
}
ib <- parse_yahoo(file.path(REPO, "data/raw/yahoo_ibov.json"))
ib <- ib[!is.na(close)]; setorder(ib, date)
ib[, r_ibov := close/shift(close) - 1]

M <- merge(cotas, ib[, .(data = date, r_ibov)], by = "data")
M <- M[is.finite(r_fundo) & is.finite(r_ibov)]
cat("Obs casadas com Ibovespa:", nrow(M), "\n")

# --- beta rolante 252 pregoes, por fundo ---
W <- 252L
setorder(M, cod_fundo, data)
M[, beta_fundo := (frollmean(r_fundo*r_ibov, W) - frollmean(r_fundo, W)*frollmean(r_ibov, W)) /
                  (frollmean(r_ibov*r_ibov, W) - frollmean(r_ibov, W)^2), by = cod_fundo]
M[, ymk := year(data)*100L + month(data)]

# --- ultimo pregao do mes anterior (mesma convencao predeterminada do R/13) ---
mlast <- M[!is.na(beta_fundo), .SD[which.max(data)], by = .(cod_fundo, ymk)][, .(cod_fundo, ymk, beta_fundo)]
cat("\nFundo-meses com beta_fundo estimado:", nrow(mlast), "\n")
cat("beta_fundo: min", round(min(mlast$beta_fundo),3), "| media", round(mean(mlast$beta_fundo),3),
    "| max", round(max(mlast$beta_fundo),3), "\n")

# junta ao painel principal com timing t-1 -> mes t (predeterminado)
painel <- fread(file.path(REPO,"data/processed/painel_vale_itau_2016_2021_filtrado.csv"))
painel[, ymk_prev := ifelse(mes==1L, (ano-1L)*100L+12L, ano*100L+(mes-1L))]
painel2 <- merge(painel, mlast, by.x = c("cod_fundo","ymk_prev"), by.y = c("cod_fundo","ymk"), all.x = TRUE)
painel2[, ymk_prev := NULL]
cat("\nPainel principal + beta_fundo: NAs beta_fundo:", painel2[is.na(beta_fundo), .N],
    "de", nrow(painel2), "(fundos novos sem 252 pregoes de historico ainda)\n")

fwrite(mlast, file.path(REPO, "data/processed/reg33_beta_fundo_mensal.csv"))
fwrite(painel2, file.path(REPO, "data/processed/painel_vale_itau_2016_2021_filtrado_betafundo.csv"))
cat("\nOK - salvo em data/processed/reg33_beta_fundo_mensal.csv e painel_..._betafundo.csv\n")
