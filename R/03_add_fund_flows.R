# =============================================================================
# 03_add_fund_flows.R
#
# Passo 3: adicionar a caracteristica de FLUXO ao painel de pesos (2016).
#
# Fonte: Informe Diario de Fundos da CVM (inf_diario_fi), que tem captacao
# (CAPTC_DIA) E resgate (RESG_DIA) -> fluxo liquido VERDADEIRO, sem suposicao.
# (A SH so traz a captacao; ver docs/log_decisoes.md.)
#
# Mantem 3 colunas: captacao bruta, resgate bruto e fluxo liquido (mensal).
# Junta ao painel por CNPJ + ano + mes. NAO faz a validacao via SH (passo 3b).
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

PROJ_DIR <- Sys.getenv("PROJ_DIR",
                       unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
if (dir.exists(PROJ_DIR)) setwd(PROJ_DIR)

YEAR <- 2016L
ZIP  <- sprintf("data/raw/inf_diario_fi_%d.zip", YEAR)

DATA_DIR <- Sys.getenv("CVM_DATA_DIR",
  unset = "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF")

# parser para "R$ x.xxx,xx" (formato brasileiro da coluna APLICACAO da SH)
parse_brl <- function(x) {
  x <- trimws(as.character(x)); x <- gsub("R\\$", "", x)
  x <- gsub("[[:space:]]", "", x); x <- gsub("\\.", "", x); x <- gsub(",", ".", x)
  x[x == ""] <- NA; suppressWarnings(as.numeric(x))
}

# ---- 1) painel do passo 2 -> CNPJs dos fundos Itau-VALE3 -------------------

painel <- fread("data/processed/painel_vale_itau_2016.csv")
cnpjs_alvo <- unique(painel$cnpj)
cat("Fundos-alvo (CNPJ distintos no painel):", length(cnpjs_alvo), "\n")

# ---- 2) extrair os 12 CSVs mensais do Informe Diario (se preciso) ----------

if (!file.exists(ZIP)) {
  stop("Zip do Informe Diario nao encontrado: ", ZIP,
       "\nBaixe de https://dados.cvm.gov.br/dados/FI/DOC/INF_DIARIO/DADOS/HIST/")
}
csvs <- sprintf("data/raw/inf_diario_fi_%d%02d.csv", YEAR, 1:12)
if (!all(file.exists(csvs))) {
  cat("Extraindo CSVs do zip...\n")
  unzip(ZIP, exdir = "data/raw")
}

# ---- 3) ler cada mes, filtrar CNPJs-alvo, certificar parsing ---------------
# Informe Diario e limpo: sep ';', decimal '.', sem R$. fread parseia direto.

cols <- c("CNPJ_FUNDO", "DT_COMPTC", "CAPTC_DIA", "RESG_DIA")
ler_mes <- function(f) {
  d <- fread(f, sep = ";", encoding = "UTF-8", select = cols, showProgress = FALSE)
  d <- d[CNPJ_FUNDO %in% cnpjs_alvo]
  # PARSING CRITICO: numericos nao podem ter NA
  stopifnot(!anyNA(d$CAPTC_DIA), !anyNA(d$RESG_DIA))
  d
}
id <- rbindlist(lapply(csvs, ler_mes))
id[, DT  := as.Date(DT_COMPTC)]
id[, ano := year(DT)]
id[, mes := month(DT)]
cat("Linhas Informe Diario (fundos-alvo, ", YEAR, "):", nrow(id), "\n", sep = "")
cat("CAPTC/RESG negativos:", sum(id$CAPTC_DIA < 0), "/", sum(id$RESG_DIA < 0), "\n")

# ---- 4) agregacao MENSAL por fundo (fluxo = soma diaria) -------------------

flow <- id[, .(
  captacao  = sum(CAPTC_DIA),
  resgate   = sum(RESG_DIA),
  fluxo_liq = sum(CAPTC_DIA - RESG_DIA),
  n_dias    = .N
), by = .(cnpj = CNPJ_FUNDO, ano, mes)]

# ---- 4b) flag de divergencia: SH.APLICACAO vs Informe Diario CAPTC_DIA ------
# Cross-check de robustez (ver docs/log_decisoes.md). A SH e serie revisada e o
# Informe Diario e o "as-reported" bruto: ~99,7% batem ao centavo; marcamos os
# ~0,3% que divergem (picos isolados de 1 dia). Mantemos o Informe Diario.
sh <- fread(file.path(DATA_DIR, sprintf("SH_%d.csv", YEAR)), encoding = "UTF-8",
            showProgress = FALSE, colClasses = list(character = "APLICAÇÃO"))
sh <- sh[CNPJ %in% cnpjs_alvo]
sh[, AP  := parse_brl(`APLICAÇÃO`)]
sh[, DT  := as.Date(DATA, format = "%d/%m/%Y")]
sh[, ano := year(DT)]
sh[, mes := month(DT)]
sh_capt <- sh[, .(captacao_sh = sum(AP, na.rm = TRUE)), by = .(cnpj = CNPJ, ano, mes)]

flow <- merge(flow, sh_capt, by = c("cnpj", "ano", "mes"), all.x = TRUE)
flow[, div_captacao := as.integer(!is.na(captacao_sh) &
                                  abs(captacao - captacao_sh) >= 0.01)]
cat("Fund-months com divergencia SH vs Informe Diario:",
    flow[div_captacao == 1, .N], "de", nrow(flow), "\n")

# ---- 5) merge no painel por CNPJ + ano + mes -------------------------------

pan2 <- merge(painel, flow, by = c("cnpj", "ano", "mes"), all.x = TRUE)

# integridade: nenhum (fundo, mes) duplicado apos o merge
dups <- pan2[, .N, by = .(cod_fundo, data)][N > 1]
cat("Pares (fundo, mes) duplicados apos merge:", nrow(dups), "\n")

n_match <- pan2[!is.na(fluxo_liq), .N]
cat("Fund-months do painel:", nrow(pan2),
    "| com fluxo casado:", n_match,
    sprintf("(%.1f%%)\n", 100 * n_match / nrow(pan2)))

# ---- 6) salvar + auditoria -------------------------------------------------

setcolorder(pan2, c("cod_fundo", "cnpj", "nome_fundo", "gestora",
                    "data", "ano", "mes", "peso_vale3", "valor_mil",
                    "captacao", "resgate", "fluxo_liq", "n_dias",
                    "captacao_sh", "div_captacao"))
fwrite(pan2, "data/processed/painel_vale_itau_2016_fluxos.csv")

cat("\n==== PAINEL 2016 + FLUXOS ====\n")
cat("Resumo fluxo_liq (R$):\n"); print(summary(pan2$fluxo_liq))
cat("\nExemplos:\n")
print(head(pan2[, .(cod_fundo, data, peso_vale3, captacao, resgate,
                    fluxo_liq, n_dias)], 10))
cat("\nOK - salvo em data/processed/painel_vale_itau_2016_fluxos.csv\n")
