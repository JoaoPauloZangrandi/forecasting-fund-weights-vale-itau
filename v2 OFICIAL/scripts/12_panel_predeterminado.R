# =============================================================================
# 12_panel_predeterminado.R  (v2 OFICIAL)
#
# ETAPA 1 (nova versao): reconstroi as caracteristicas do fundo (AUM,
# cotistas, fluxo) para usar SEMPRE o ultimo dia do mes ANTERIOR (t-1),
# igual ja se fazia para preco/beta da VALE3 na v1 e para beta_fundo (R/33).
# Objetivo: theta_t passa a ser 100% predeterminado (conhecido antes do mes
# t comecar), sem look-ahead, em vez de contemporaneo.
#
# Fonte: Informe Diario da CVM (data/raw/inf_diario_fi_AAAAMM.csv), que ja
# tem AUM (VL_PATRIM_LIQ), cotistas (NR_COTST) e fluxo (CAPTC_DIA-RESG_DIA)
# na MESMA fonte diaria -- mais limpo que misturar SH (AUM/cotistas) com
# Informe Diario (fluxo), como a v1 fazia.
#
# Para cada fundo-mes (i,t) do painel principal:
#   aum_prev, cotistas_prev = VL_PATRIM_LIQ / NR_COTST no ULTIMO DT_COMPTC
#                              disponivel DENTRO do mes (t-1)
#   fluxo_prev = soma de (CAPTC_DIA - RESG_DIA) em TODOS os dias do mes (t-1)
#
# beta_fundo ja e predeterminado (R/33, mesmo t-1) -- so mesclado aqui.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
RAW  <- file.path(REPO, "data/raw")

painel <- fread(file.path(REPO, "data/processed/painel_vale_itau_2016_2021_filtrado.csv"))
painel[, ym := ano*100L + mes]
cnpjs <- unique(painel$cnpj)
cat("Fundo-meses no painel principal:", nrow(painel), "| CNPJs distintos:", length(cnpjs), "\n")

addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
painel[, ym_prev := addm(ym, -1L)]
meses_prev <- sort(unique(painel$ym_prev))
cat("Meses (t-1) necessarios:", length(meses_prev), "(", min(meses_prev), "-", max(meses_prev), ")\n")

# ---- le o Informe Diario mes a mes, filtra so os CNPJs do painel ------------
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
cat("\nLinhas do Informe Diario filtradas (nossos fundos):", nrow(ID), "\n")

# ---- por CNPJ x mes(t-1): ultimo dia (AUM, cotistas) + soma do mes (fluxo) --
ID[, ymk := year(DT_COMPTC)*100L + month(DT_COMPTC)]
ultimo <- ID[order(CNPJ_FUNDO, ymk, DT_COMPTC), .SD[.N], by = .(CNPJ_FUNDO, ymk)]
ultimo <- ultimo[, .(CNPJ_FUNDO, ymk, aum_prev = VL_PATRIM_LIQ, cotistas_prev = NR_COTST, ultimo_dia = DT_COMPTC)]

fluxo <- ID[, .(fluxo_prev = sum(CAPTC_DIA - RESG_DIA, na.rm = TRUE), n_dias_prev = .N), by = .(CNPJ_FUNDO, ymk)]

prev <- merge(ultimo, fluxo, by = c("CNPJ_FUNDO","ymk"))
cat("Fundo x mes(t-1) com AUM/cotistas/fluxo extraidos:", nrow(prev), "\n")

# ---- merge no painel principal (por cnpj + ym_prev) -------------------------
painel2 <- merge(painel, prev, by.x = c("cnpj","ym_prev"), by.y = c("CNPJ_FUNDO","ymk"), all.x = TRUE)
cat("\nFundo-meses SEM dado predeterminado (t-1) disponivel:", painel2[is.na(aum_prev), .N],
    "de", nrow(painel2), sprintf("(%.1f%%)\n", 100*painel2[is.na(aum_prev), .N]/nrow(painel2)))

# ---- mescla beta_fundo (ja predeterminado, R/33) -----------------------------
bf <- fread(file.path(REPO, "data/processed/reg33_beta_fundo_mensal.csv"))
painel2 <- merge(painel2, bf, by.x = c("cod_fundo","ym_prev"), by.y = c("cod_fundo","ymk"), all.x = TRUE)
cat("Fundo-meses SEM beta_fundo (precisam de >=252 pregoes de historico):", painel2[is.na(beta_fundo), .N],
    "de", nrow(painel2), sprintf("(%.1f%%)\n", 100*painel2[is.na(beta_fundo), .N]/nrow(painel2)))

cat("\n===== Comparacao rapida: aum contemporaneo (painel atual) vs aum_prev (novo) =====\n")
cat("Correlacao:", round(cor(painel2$aum, painel2$aum_prev, use = "complete.obs"), 4), "\n")
cat("Correlacao n_cotistas vs cotistas_prev:", round(cor(painel2$n_cotistas, painel2$cotistas_prev, use = "complete.obs"), 4), "\n")
cat("Correlacao fluxo_liq vs fluxo_prev:", round(cor(painel2$fluxo_liq, painel2$fluxo_prev, use = "complete.obs"), 4), "\n")

fwrite(painel2, file.path(REPO, "v2 OFICIAL/data/painel_predeterminado.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/painel_predeterminado.csv'\n")
