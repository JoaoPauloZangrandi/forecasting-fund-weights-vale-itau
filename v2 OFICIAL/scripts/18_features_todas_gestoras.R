# =============================================================================
# 18_features_todas_gestoras.R  (v2 OFICIAL)
#
# Reconstroi AUM/cotistas/fluxo predeterminados (t-1, via Informe Diario) para
# TODOS os fundos do painel de todas as gestoras (2.821 fundos) -- mesma
# logica do R/12, so generalizada do universo Itau (308 fundos) para o
# universo completo.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
RAW  <- file.path(REPO, "data/raw")

painel <- fread(file.path(REPO, "v2 OFICIAL/data/painel_todas_gestoras_2016_2021.csv"))
painel[, ym := ano*100L + mes]
cnpjs <- unique(painel$cnpj)
cat("Fundo-meses no painel (todas gestoras):", nrow(painel), "| CNPJs distintos:", length(cnpjs), "\n")

addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
painel[, ym_prev := addm(ym, -1L)]
meses_prev <- sort(unique(painel$ym_prev))
cat("Meses (t-1) necessarios:", length(meses_prev), "(", min(meses_prev), "-", max(meses_prev), ")\n")

extrai_mes <- function(ymk) {
  path <- file.path(RAW, sprintf("inf_diario_fi_%d.csv", ymk))
  if (!file.exists(path)) { cat("AVISO: nao achei", path, "\n"); return(NULL) }
  d <- fread(path, encoding = "UTF-8", showProgress = FALSE,
             select = c("CNPJ_FUNDO","DT_COMPTC","VL_PATRIM_LIQ","CAPTC_DIA","RESG_DIA","NR_COTST"))
  d <- d[CNPJ_FUNDO %in% cnpjs]
  d[, DT_COMPTC := as.Date(DT_COMPTC)]
  d
}

blocos <- vector("list", length(meses_prev))
for (i in seq_along(meses_prev)) {
  blocos[[i]] <- extrai_mes(meses_prev[i])
  if (i %% 12 == 0) cat("... lido", i, "de", length(meses_prev), "meses\n")
}
ID <- rbindlist(blocos, fill = TRUE)
cat("\nLinhas do Informe Diario filtradas:", nrow(ID), "\n")

ID[, ymk := year(DT_COMPTC)*100L + month(DT_COMPTC)]
ultimo <- ID[order(CNPJ_FUNDO, ymk, DT_COMPTC), .SD[.N], by = .(CNPJ_FUNDO, ymk)]
ultimo <- ultimo[, .(CNPJ_FUNDO, ymk, aum_prev = VL_PATRIM_LIQ, cotistas_prev = NR_COTST)]
fluxo <- ID[, .(fluxo_prev = sum(CAPTC_DIA - RESG_DIA, na.rm = TRUE)), by = .(CNPJ_FUNDO, ymk)]
prev <- merge(ultimo, fluxo, by = c("CNPJ_FUNDO","ymk"))
cat("Fundo x mes(t-1) com AUM/cotistas/fluxo extraidos:", nrow(prev), "\n")

painel2 <- merge(painel, prev, by.x = c("cnpj","ym_prev"), by.y = c("CNPJ_FUNDO","ymk"), all.x = TRUE)
cat("\nFundo-meses SEM dado predeterminado (t-1) disponivel:", painel2[is.na(aum_prev), .N],
    "de", nrow(painel2), sprintf("(%.1f%%)\n", 100*painel2[is.na(aum_prev), .N]/nrow(painel2)))

# ---- FIC vs FI pelo nome (mesmo criterio do R/04) ---------------------------
norm_txt <- function(x) trimws(gsub("[[:space:]]+"," ", toupper(iconv(as.character(x), "", "ASCII//TRANSLIT"))))
painel2[, nome_norm := norm_txt(nome_fundo)]
painel2[, is_fic := as.integer(grepl("\\bFIC\\b|\\bFICFI\\b", nome_norm))]
painel2[, nome_norm := NULL]
cat("FIC:", painel2[is_fic==1,.N], "| FI:", painel2[is_fic==0,.N], "fundo-meses\n")

fwrite(painel2, file.path(REPO, "v2 OFICIAL/data/painel_todas_gestoras_predeterminado.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/painel_todas_gestoras_predeterminado.csv'\n")
