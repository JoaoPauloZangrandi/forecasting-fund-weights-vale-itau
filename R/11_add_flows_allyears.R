# =============================================================================
# 11_add_flows_allyears.R
#
# Generaliza o Passo 3 (fluxo via Informe Diario CVM) para 2016-2021 e junta ao
# painel multi-ano. Le os CSVs mensais por NOME de coluna (robusto ao formato de
# 2021, que tem coluna extra TP_FUNDO). Fluxo liquido mensal = soma diaria de
# (CAPTC_DIA - RESG_DIA). Join por CNPJ + ano + mes.
#
# RODAR COM CAMINHO ABSOLUTO (ver gotcha do R/10).
# Pre-requisito: zips do Informe Diario ja baixados/extraidos em data/raw:
#   2016-2020 anuais (HIST) -> inf_diario_fi_YYYYMM.csv (12/ano)
#   2021 mensais (DADOS)    -> inf_diario_fi_2021MM.csv
# =============================================================================

suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
RAW  <- file.path(REPO, "data/raw")
COLS <- c("CNPJ_FUNDO","DT_COMPTC","CAPTC_DIA","RESG_DIA")

painel <- fread(file.path(REPO, "data/processed/painel_vale_itau_2016_2021.csv"))
cnpjs  <- unique(painel$cnpj)

ler <- list()
for (y in 2016:2021) {
  for (mm in sprintf("%02d", 1:12)) {
    f <- file.path(RAW, sprintf("inf_diario_fi_%d%s.csv", y, mm))
    if (!file.exists(f)) { cat("FALTA:", f, "\n"); next }
    d <- fread(f, sep = ";", select = COLS, showProgress = FALSE)
    ler[[length(ler) + 1L]] <- d[CNPJ_FUNDO %in% cnpjs]
  }
  cat("ano", y, "lido\n"); flush.console()
}
id <- rbindlist(ler)
stopifnot(!anyNA(id$CAPTC_DIA), !anyNA(id$RESG_DIA))   # parsing critico
id[, DT := as.Date(DT_COMPTC)]
id[, ano := year(DT)][, mes := month(DT)]

# dup check: cada (CNPJ, dia) deve ser unico
dup_dia <- nrow(id[, .N, by = .(CNPJ_FUNDO, DT)][N > 1])
cat("Linhas Informe Diario (alvo):", nrow(id), "| (CNPJ,dia) duplicados:", dup_dia, "\n")

flow <- id[, .(captacao  = sum(CAPTC_DIA),
               resgate   = sum(RESG_DIA),
               fluxo_liq = sum(CAPTC_DIA - RESG_DIA),
               n_dias    = .N),
           by = .(cnpj = CNPJ_FUNDO, ano, mes)]

pan2 <- merge(painel, flow, by = c("cnpj","ano","mes"), all.x = TRUE)

# ---- auditoria por ano + integridade (regra 6) ----
cat("\n==== MATCH DO FLUXO POR ANO ====\n")
print(pan2[, .(obs = .N, com_fluxo = sum(!is.na(fluxo_liq)),
               pct = round(100 * mean(!is.na(fluxo_liq)), 1)), by = ano][order(ano)])
cat("\nIdentidade fluxo_liq == captacao - resgate (max |dif|):",
    pan2[!is.na(fluxo_liq), max(abs(fluxo_liq - (captacao - resgate)))], "\n")
cat("Pares (fundo,ano,mes) duplicados:",
    nrow(pan2[, .N, by = .(cod_fundo, ano, mes)][N > 1]),
    "| NAs fluxo:", pan2[is.na(fluxo_liq), .N], "\n")

fwrite(pan2, file.path(REPO, "data/processed/painel_vale_itau_2016_2021_fluxos.csv"))
cat("\nOK - salvo em data/processed/painel_vale_itau_2016_2021_fluxos.csv\n")
