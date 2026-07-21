# =============================================================================
# 19_beta_fundo_todas_gestoras.R  (v2 OFICIAL)
#
# Beta do proprio fundo (retorno diario da COTA vs Ibovespa, janela movel de
# 252 pregoes, medido no ULTIMO PREGAO DO MES ANTERIOR) para TODOS os fundos
# do painel de todas as gestoras -- generaliza o R/33 (so 307 fundos Itau)
# para o universo completo (2.821 fundos).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA_DIR <- "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF"

alvo <- unique(fread(file.path(REPO,"v2 OFICIAL/data/painel_todas_gestoras_2016_2021.csv"))$cod_fundo)
cat("Fundos-alvo (todas as gestoras):", length(alvo), "\n")

pdate <- function(x){ x<-trimws(as.character(x)); o<-as.Date(rep(NA_character_,length(x)))
  for(f in c("%Y-%m-%d","%d/%m/%Y")){m<-is.na(o);if(!any(m))break;o[m]<-as.Date(x[m],format=f)};o }

sh_all <- list()
for (y in 2016:2021) {
  sh <- fread(file.path(DATA_DIR, sprintf("SH_%d.csv", y)), encoding = "UTF-8", showProgress = FALSE,
              select = c("COD_FUNDO","DATA","COTA"))
  sh <- sh[COD_FUNDO %in% alvo]
  sh[, DT := pdate(DATA)][, cota_num := as.numeric(gsub(",", ".", COTA, fixed = TRUE))]
  sh_all[[length(sh_all)+1L]] <- sh[, .(cod_fundo = COD_FUNDO, data = DT, cota = cota_num)]
  cat("ano", y, "cotas lidas:", nrow(sh_all[[length(sh_all)]]), "\n"); flush.console()
}
cotas <- rbindlist(sh_all)
cotas <- cotas[!is.na(cota) & cota > 0]
setorder(cotas, cod_fundo, data)
cotas[, r_fundo := cota/shift(cota) - 1, by = cod_fundo]
cat("\nRetornos diarios de cota calculados:", cotas[is.finite(r_fundo), .N], "obs |",
    uniqueN(cotas$cod_fundo), "fundos\n")

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

W <- 252L
setorder(M, cod_fundo, data)
M[, beta_fundo := (frollmean(r_fundo*r_ibov, W) - frollmean(r_fundo, W)*frollmean(r_ibov, W)) /
                  (frollmean(r_ibov*r_ibov, W) - frollmean(r_ibov, W)^2), by = cod_fundo]
M[, ymk := year(data)*100L + month(data)]

mlast <- M[!is.na(beta_fundo), .SD[which.max(data)], by = .(cod_fundo, ymk)][, .(cod_fundo, ymk, beta_fundo)]
cat("\nFundo-meses com beta_fundo estimado (bruto):", nrow(mlast), "\n")

# ---- filtro de sanidade: betas extremos sao artefato de cota esparsa/------
# irregular em janelas com poucos pregoes efetivos (denominador degenerado),
# nao um beta real. Limite generoso (|beta|<=5, bem acima do p99.9=1.53 da
# distribuicao) para excluir so os casos claramente quebrados.
n_antes <- nrow(mlast)
extremos <- mlast[abs(beta_fundo) > 5]
if (nrow(extremos) > 0) {
  cat("ATENCAO: excluindo", nrow(extremos), "fundo-meses com |beta_fundo|>5 (artefato de cota",
      "esparsa/irregular, nao beta real) --- fundos:", paste(unique(extremos$cod_fundo), collapse=", "), "\n")
  mlast <- mlast[abs(beta_fundo) <= 5]
}
cat("Fundo-meses com beta_fundo (apos filtro de sanidade):", nrow(mlast), "de", n_antes, "\n")
cat("beta_fundo: min", round(min(mlast$beta_fundo),3), "| media", round(mean(mlast$beta_fundo),3),
    "| max", round(max(mlast$beta_fundo),3), "\n")
cat("Fundos distintos com beta_fundo em algum mes:", uniqueN(mlast$cod_fundo), "de", length(alvo), "\n")

fwrite(mlast, file.path(REPO, "v2 OFICIAL/data/beta_fundo_todas_gestoras.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/beta_fundo_todas_gestoras.csv'\n")
