# =============================================================================
# 12_add_features_allyears.R
#
# Generaliza o Passo 4 (caracteristicas de fundo da SH) para 2016-2021:
#   aum (PL na competencia, R$ cheios), n_cotistas, is_fic, classif_anbima.
# Snapshot dia-exato (merge por COD_FUNDO + data) por ano. is_fic pelo NOME
# por token. RODAR COM CAMINHO ABSOLUTO.
# =============================================================================

suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA_DIR <- "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF"

parse_brl <- function(x) { x <- trimws(as.character(x)); x <- gsub("R\\$","",x)
  x <- gsub("[[:space:]]","",x); x <- gsub("\\.","",x); x <- gsub(",",".",x)
  x[x == ""] <- NA; suppressWarnings(as.numeric(x)) }
norm <- function(x) { x <- iconv(as.character(x),"","ASCII//TRANSLIT"); trimws(gsub("[[:space:]]+"," ",toupper(x))) }
pdate <- function(x) { x <- trimws(as.character(x)); o <- as.Date(rep(NA_character_, length(x)))
  for (f in c("%Y-%m-%d","%d/%m/%Y")) { m <- is.na(o); if (!any(m)) break; o[m] <- as.Date(x[m], format = f) }; o }

painel <- fread(file.path(REPO, "data/processed/painel_vale_itau_2016_2021_fluxos.csv"))
painel[, data := as.Date(data)]
alvo <- unique(painel$cod_fundo)

res <- list()
for (y in 2016:2021) {
  sh <- fread(file.path(DATA_DIR, sprintf("SH_%d.csv", y)), encoding = "UTF-8", showProgress = FALSE,
              select = c("COD_FUNDO","DATA","PATRIMONIO_LIQUIDO_(MIL)","NUMERO_DE_COTISTAS","CLASSIFICACAO_ANBIMA"),
              colClasses = list(character = "PATRIMONIO_LIQUIDO_(MIL)"))
  sh <- sh[COD_FUNDO %in% alvo]
  sh[, DT := pdate(DATA)]; sh[, pl_mil := parse_brl(`PATRIMONIO_LIQUIDO_(MIL)`)]
  snap <- sh[, .(cod_fundo = COD_FUNDO, data = DT, aum = pl_mil * 1000,
                 n_cotistas = NUMERO_DE_COTISTAS, classif_anbima = CLASSIFICACAO_ANBIMA)]
  py <- merge(painel[ano == y], snap, by = c("cod_fundo","data"), all.x = TRUE)
  res[[length(res) + 1L]] <- py
  cat("ano", y, "ok\n"); flush.console()
}
pan2 <- rbindlist(res)
pan2[, is_fic := as.integer(grepl("\\bFIC\\b|\\bFICFI\\b", norm(nome_fundo)))]

cat("\n==== AUDITORIA FEATURES POR ANO ====\n")
print(pan2[, .(obs = .N, na_aum = sum(is.na(aum)), na_cot = sum(is.na(n_cotistas)),
               fic = sum(is_fic), fi = sum(is_fic == 0)), by = ano][order(ano)])
cat("\nPares (fundo,ano,mes) duplicados:", nrow(pan2[, .N, by = .(cod_fundo, ano, mes)][N > 1]), "\n")
cat("AUM (R$) resumo:\n"); print(summary(pan2$aum))

fwrite(pan2, file.path(REPO, "data/processed/painel_vale_itau_2016_2021_features.csv"))
cat("\nOK - salvo em data/processed/painel_vale_itau_2016_2021_features.csv\n")
