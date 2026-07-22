# =============================================================================
# 28_features_multiativo_completo.R  (v2 OFICIAL)
#
# Corrige um gap encontrado no R/27: a tabela de AUM/cotistas/fluxo
# predeterminados (R/18) so cobria os fundo-meses onde o fundo tinha VALE3
# (o painel de origem, ver R/17) -- 16,8% dos 115.841 pares (fundo,mes) do
# painel MULTIATIVO nao tinham cobertura (fundos que naquele mes especifico
# so tinham OUTRAS acoes, nao VALE3). Refeito usando como base o proprio
# spine (fundo,mes) do painel multiativo, nao o do painel so-VALE3.
#
# beta_fundo (R/19) NAO tem esse problema -- foi calculado por fundo (todos
# os 2.820), usando o historico INTEIRO de cota, nao restrito a meses com
# VALE3 -- so precisa ser mesclado de novo, sem refazer.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
RAW  <- file.path(REPO, "data/raw")

dmulti <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_direto.csv"),
                select = c("cod_fundo","cnpj","ano","mes"))
dmulti[, ym := ano*100L + mes]
spine <- unique(dmulti[, .(cod_fundo, cnpj, ym)])
cat("Spine (fundo,mes) do painel multiativo:", nrow(spine), "\n")

cnpjs <- unique(spine$cnpj)
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
spine[, ym_prev := addm(ym, -1L)]
meses_prev <- sort(unique(spine$ym_prev))
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
  if (i %% 12 == 0) { cat("... lido", i, "de", length(meses_prev), "meses\n"); flush.console() }
}
ID <- rbindlist(blocos, fill = TRUE)
rm(blocos); gc(FALSE)
cat("\nLinhas do Informe Diario filtradas:", nrow(ID), "\n")

ID[, ymk := year(DT_COMPTC)*100L + month(DT_COMPTC)]
ultimo <- ID[order(CNPJ_FUNDO, ymk, DT_COMPTC), .SD[.N], by = .(CNPJ_FUNDO, ymk)]
ultimo <- ultimo[, .(CNPJ_FUNDO, ymk, aum_prev = VL_PATRIM_LIQ, cotistas_prev = NR_COTST)]
fluxo <- ID[, .(fluxo_prev = sum(CAPTC_DIA - RESG_DIA, na.rm = TRUE)), by = .(CNPJ_FUNDO, ymk)]
prev <- merge(ultimo, fluxo, by = c("CNPJ_FUNDO","ymk"))
rm(ID, ultimo, fluxo); gc(FALSE)
cat("Fundo x mes(t-1) com AUM/cotistas/fluxo extraidos:", nrow(prev), "\n")

spine2 <- merge(spine, prev, by.x = c("cnpj","ym_prev"), by.y = c("CNPJ_FUNDO","ymk"), all.x = TRUE)
cat("\nSpine SEM dado predeterminado (t-1) disponivel:", spine2[is.na(aum_prev), .N],
    "de", nrow(spine2), sprintf("(%.1f%%)\n", 100*spine2[is.na(aum_prev), .N]/nrow(spine2)))

fwrite(spine2, file.path(REPO, "v2 OFICIAL/data/features_fundo_mes_multiativo.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/features_fundo_mes_multiativo.csv'\n")
