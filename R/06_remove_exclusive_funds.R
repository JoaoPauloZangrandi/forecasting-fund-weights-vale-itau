# =============================================================================
# 06_remove_exclusive_funds.R
#
# Passo 6: remove fundos com poucos cotistas.
# Criterio simples aprovado: manter apenas observacoes com n_cotistas > 3.
# O painel completo continua salvo e intacto; este script gera uma versao filtrada.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

PROJ_DIR <- Sys.getenv("PROJ_DIR",
                       unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
if (dir.exists(PROJ_DIR)) setwd(PROJ_DIR)

painel <- fread("data/processed/painel_vale_itau_2016_full.csv")

stopifnot("n_cotistas" %in% names(painel))
stopifnot(!anyNA(painel$n_cotistas))

obs_total <- nrow(painel)
fundos_total <- uniqueN(painel$cod_fundo)

painel_filtrado <- painel[n_cotistas > 3]

obs_filtrado <- nrow(painel_filtrado)
fundos_filtrado <- uniqueN(painel_filtrado$cod_fundo)

cat("PASSO 6 - filtro de fundos com poucos cotistas\n")
cat("Criterio: manter n_cotistas > 3\n")
cat("Obs antes:", obs_total, "| depois:", obs_filtrado,
    "| removidas:", obs_total - obs_filtrado, "\n")
cat("Fundos antes:", fundos_total, "| depois:", fundos_filtrado,
    "| fundos com alguma remocao:", uniqueN(painel$cod_fundo[painel$n_cotistas <= 3]), "\n")
cat("NAs em n_cotistas:", painel[is.na(n_cotistas), .N], "\n")
cat("Pares (fundo,mes) duplicados no filtrado:",
    painel_filtrado[, .N, by = .(cod_fundo, data)][N > 1, .N], "\n")

fwrite(painel_filtrado,
       "data/processed/painel_vale_itau_2016_filtrado_cotistas_gt3.csv")

cat("\nOK - painel filtrado salvo em ",
    "data/processed/painel_vale_itau_2016_filtrado_cotistas_gt3.csv\n",
    sep = "")
