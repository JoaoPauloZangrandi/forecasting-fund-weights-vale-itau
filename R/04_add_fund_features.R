# =============================================================================
# 04_add_fund_features.R
#
# Passo 4: adicionar caracteristicas de FUNDO ao painel (2016), todas da SH:
#   - aum         : patrimonio liquido na COMPETENCIA (snapshot), em R$ cheios
#   - n_cotistas  : numero de cotistas na competencia
#   - is_fic      : 1 se o fundo e FIC (Fundo de Inv. em Cotas), 0 se FI
#   - classif_anbima : categoria de estrategia ANBIMA (extra)
#
# Snapshot no dia da competencia (merge dia-exato por COD_FUNDO + data), o mesmo
# criterio do passo 2 (0% de perda). A ANBIMA NAO distingue FIC/FI -> FIC vem do
# NOME do fundo, por TOKEN (\bFIC\b), robusto a substrings. Ver docs/log_decisoes.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

PROJ_DIR <- Sys.getenv("PROJ_DIR",
                       unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
if (dir.exists(PROJ_DIR)) setwd(PROJ_DIR)

YEAR <- 2016L
DATA_DIR <- Sys.getenv("CVM_DATA_DIR",
  unset = "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF")

parse_brl <- function(x) {              # formato "R$ x.xxx,xx" da SH
  x <- trimws(as.character(x)); x <- gsub("R\\$", "", x)
  x <- gsub("[[:space:]]", "", x); x <- gsub("\\.", "", x); x <- gsub(",", ".", x)
  x[x == ""] <- NA; suppressWarnings(as.numeric(x))
}
norm_txt <- function(x) {
  x <- iconv(as.character(x), "", "ASCII//TRANSLIT")
  trimws(gsub("[[:space:]]+", " ", toupper(x)))
}

# ---- 1) painel do passo 3 (com fluxos) -------------------------------------

painel <- fread("data/processed/painel_vale_itau_2016_fluxos.csv")
painel[, data := as.Date(data)]

# ---- 2) SH: snapshot de PL, cotistas, classificacao na competencia ---------

sh <- fread(file.path(DATA_DIR, sprintf("SH_%d.csv", YEAR)), encoding = "UTF-8",
            showProgress = FALSE,
            colClasses = list(character = "PATRIMONIO_LIQUIDO_(MIL)"))
sh <- sh[COD_FUNDO %in% unique(painel$cod_fundo)]
sh[, DT     := as.Date(DATA, format = "%d/%m/%Y")]
sh[, pl_mil := parse_brl(`PATRIMONIO_LIQUIDO_(MIL)`)]
stopifnot(!anyNA(sh$pl_mil))   # parsing critico

sh_snap <- sh[, .(COD_FUNDO, DT,
                  aum        = pl_mil * 1000,          # R$ cheios
                  n_cotistas = NUMERO_DE_COTISTAS,
                  classif_anbima = CLASSIFICACAO_ANBIMA)]

# ---- 3) merge dia-exato (COD_FUNDO + competencia) --------------------------

pan2 <- merge(painel, sh_snap,
              by.x = c("cod_fundo", "data"), by.y = c("COD_FUNDO", "DT"),
              all.x = TRUE)

cat("Fund-months:", nrow(pan2),
    "| sem match SH (aum NA):", pan2[is.na(aum), .N], "\n")
dups <- pan2[, .N, by = .(cod_fundo, data)][N > 1]
cat("Pares (fundo, mes) duplicados:", nrow(dups), "\n")

# ---- 4) FIC vs FI pelo NOME (token, robusto a substring) -------------------

pan2[, nome_norm := norm_txt(nome_fundo)]
pan2[, is_fic := as.integer(grepl("\\bFIC\\b|\\bFICFI\\b", nome_norm))]
pan2[, nome_norm := NULL]
cat("FIC:", pan2[is_fic == 1, .N], "| FI:", pan2[is_fic == 0, .N], "fund-months\n")

# ---- 5) auditoria + salvar -------------------------------------------------

cat("\n== AUDITORIA ==\n")
cat("NAs: aum", sum(is.na(pan2$aum)), "| n_cotistas", sum(is.na(pan2$n_cotistas)),
    "| classif_anbima", sum(is.na(pan2$classif_anbima)), "\n")
cat("AUM (R$) resumo:\n");        print(summary(pan2$aum))
cat("N_cotistas resumo:\n");      print(summary(pan2$n_cotistas))

setcolorder(pan2, c("cod_fundo", "cnpj", "nome_fundo", "gestora", "is_fic",
                    "classif_anbima", "data", "ano", "mes",
                    "peso_vale3", "valor_mil", "aum", "n_cotistas",
                    "captacao", "resgate", "fluxo_liq", "n_dias",
                    "captacao_sh", "div_captacao"))
fwrite(pan2, "data/processed/painel_vale_itau_2016_features.csv")
cat("\nExemplos:\n")
print(head(pan2[, .(cod_fundo, data, is_fic, peso_vale3, aum, n_cotistas,
                    fluxo_liq)], 8))
cat("\nOK - salvo em data/processed/painel_vale_itau_2016_features.csv\n")
