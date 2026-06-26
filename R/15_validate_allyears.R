# =============================================================================
# 15_validate_allyears.R
#
# Bateria de validacao do pipeline multi-ano (2016-2021). Re-testa todos os
# problemas que enfrentamos. RODAR COM CAMINHO ABSOLUTO.
# =============================================================================

suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA_DIR <- "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF"
ok <- function(cond, msg) cat(if (isTRUE(cond)) "[OK]   " else "[FALHA]", msg, "\n")

parse_brl <- function(x) { x <- trimws(as.character(x)); x <- gsub("R\\$","",x)
  x <- gsub("[[:space:]]","",x); x <- gsub("\\.","",x); x <- gsub(",",".",x)
  x[x == ""] <- NA; suppressWarnings(as.numeric(x)) }
norm <- function(x) { x <- iconv(as.character(x),"","ASCII//TRANSLIT"); trimws(gsub("[[:space:]]+"," ",toupper(x))) }
pdate <- function(x) { x <- trimws(as.character(x)); o <- as.Date(rep(NA_character_, length(x)))
  for (f in c("%Y-%m-%d","%d/%m/%Y")) { m <- is.na(o); if (!any(m)) break; o[m] <- as.Date(x[m], format = f) }; o }

full <- fread(file.path(REPO, "data/processed/painel_vale_itau_2016_2021_full.csv"))
full[, data := as.Date(data)]

cat("===== 1) PAINEL: integridade geral =====\n")
ok(nrow(full) == 26123, paste("obs =", nrow(full), "(esperado 26123)"))
ok(nrow(full[, .N, by = .(cod_fundo, ano, mes)][N > 1]) == 0, "0 pares (fundo,ano,mes) duplicados")
ok(full[peso_vale3 < 0, .N] == 0, paste("0 pesos negativos (achados:", full[peso_vale3 < 0, .N], ")"))
ok(sum(is.na(full$peso_vale3)) == 0, "0 NAs no peso")
ok(sum(is.na(full$aum)) == 0 && sum(is.na(full$n_cotistas)) == 0, "0 NAs em aum/cotistas")
ok(sum(is.na(full$beta_vale)) == 0 && sum(is.na(full$preco_nominal)) == 0, "0 NAs em beta/preco")
cat("NAs em fluxo_liq:", sum(is.na(full$fluxo_liq)), "(esperado ~119, fund-months fora do inf_diario)\n")

cat("\n===== 2) 2016 do multi-ano == painel 2016 validado =====\n")
p16 <- fread(file.path(REPO, "data/processed/painel_vale_itau_2016_full.csv"))
p16[, data := as.Date(data)]
a <- full[ano == 2016, .(cod_fundo, data, peso_vale3, aum, n_cotistas, captacao, resgate, fluxo_liq,
                         preco_nominal, beta_vale)][order(cod_fundo, data)]
b <- p16[, .(cod_fundo, data, peso_vale3, aum, n_cotistas, captacao, resgate, fluxo_liq,
             preco_nominal, beta_vale)][order(cod_fundo, data)]
ok(nrow(a) == nrow(b), paste("mesmo nrow:", nrow(a), "vs", nrow(b)))
dif_fundo <- max(abs(a$peso_vale3 - b$peso_vale3), abs(a$aum - b$aum),
                 abs(a$n_cotistas - b$n_cotistas), abs(a$captacao - b$captacao),
                 abs(a$resgate - b$resgate), abs(a$fluxo_liq - b$fluxo_liq),
                 abs(a$preco_nominal - b$preco_nominal), na.rm = TRUE)
ok(dif_fundo < 1e-9, paste("dados de fundo + preco nominal IDENTICOS (max |dif| =", signif(dif_fundo, 3), ")"))
dif_beta <- max(abs(a$beta_vale - b$beta_vale), na.rm = TRUE)
ok(dif_beta < 1e-4, paste("beta ~igual (dif", signif(dif_beta, 3),
                          "= reajuste do adjclose por proventos pos-2017; esperado, nao e bug)"))

cat("\n===== 3) Identidade do fluxo (regra 6) =====\n")
idd <- full[!is.na(fluxo_liq)]
ok(max(abs(idd$fluxo_liq - (idd$captacao - idd$resgate))) < 1e-3,
   "fluxo_liq == captacao - resgate (ao epsilon)")

cat("\n===== 4) Cross-check SH.APLICACAO vs Informe Diario (regra 5), por ano =====\n")
for (y in 2016:2021) {
  sh <- fread(file.path(DATA_DIR, sprintf("SH_%d.csv", y)), encoding = "UTF-8", showProgress = FALSE,
              select = c("CNPJ","GESTORA","DATA","APLICAÇÃO"),
              colClasses = list(character = "APLICAÇÃO"))
  setnames(sh, "APLICAÇÃO", "AP")
  sh[, GN := norm(GESTORA)]; sh <- sh[grepl("ITAU ASSET|ITAU DTVM|ITAU UNIBANCO", GN)]
  sh[, APn := parse_brl(AP)]; sh[, DT := pdate(DATA)]
  sh[, ym := year(DT) * 100L + month(DT)]
  shm <- sh[, .(ap_sh = sum(APn, na.rm = TRUE)), by = .(cnpj = CNPJ, ym)]
  fm <- full[ano == y, .(cnpj, ym = ano * 100L + mes, captacao)]
  cmp <- merge(fm, shm, by = c("cnpj","ym"))
  cmp <- cmp[!is.na(captacao)]
  batem <- cmp[abs(captacao - ap_sh) < 0.01, .N]
  cat(sprintf("  %d: comparados %d | batem ao centavo %d (%.1f%%)\n",
              y, nrow(cmp), batem, 100 * batem / max(nrow(cmp), 1)))
}

cat("\n===== 5) Beta por 3 metodos (amostra de datas) =====\n")
getv <- function(lst) vapply(lst, function(x) if (is.null(x)) NA_real_ else as.numeric(x), numeric(1))
py <- function(path) { res <- fromJSON(path, simplifyVector = FALSE)$chart$result[[1]]
  ts <- vapply(res$timestamp, as.numeric, numeric(1))
  adj <- getv(res$indicators$adjclose[[1]]$adjclose)
  data.table(date = as.Date(as.POSIXct(ts, origin = "1970-01-01", tz = "America/Sao_Paulo")),
             close = getv(res$indicators$quote[[1]]$close), adjclose = adj) }
v <- py(file.path(REPO, "data/raw/yahoo_vale3.json")); ib <- py(file.path(REPO, "data/raw/yahoo_ibov.json"))
v <- v[!is.na(adjclose)]; ib <- ib[!is.na(close)]
s <- merge(v[, .(date, adjclose)], ib[, .(date, ibov = close)], by = "date"); setorder(s, date)
s[, rx := adjclose/shift(adjclose) - 1][, ry := ibov/shift(ibov) - 1]
W <- 252L
s[, bf := (frollmean(rx*ry,W) - frollmean(rx,W)*frollmean(ry,W)) / (frollmean(ry*ry,W) - frollmean(ry,W)^2)]
for (dd in c("2016-12-30","2018-06-29","2020-03-31","2021-11-30")) {
  i <- which(s$date == as.Date(dd)); if (length(i) == 0) next
  w <- s[(i-W+1):i]; bcov <- cov(w$rx, w$ry)/var(w$ry); blm <- coef(lm(rx ~ ry, w))[2]
  cat(sprintf("  %s: froll=%.5f cov/var=%.5f lm=%.5f | difmax=%.2e\n",
              dd, s$bf[i], bcov, blm, max(abs(c(s$bf[i]-bcov, s$bf[i]-blm)))))
}
cat("\nFIM da validacao.\n")
