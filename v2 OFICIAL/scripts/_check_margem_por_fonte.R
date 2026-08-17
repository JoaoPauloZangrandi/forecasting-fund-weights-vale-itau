suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
precos <- fread(file.path(REPO, "v2 OFICIAL/data/precos_mensais_final.csv"))
fonte_ticker <- unique(precos[, .(ticker, fonte)])
pa <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_universo_completo_por_ativo_h1.csv"))
pa[, ticker := trimws(sub(".*- ", "", ativo))]
elegivel <- pa[n_obs >= 100]
elegivel <- merge(elegivel, fonte_ticker, by = "ticker", all.x = TRUE)
elegivel <- elegivel[!is.na(fonte)]
cat("Total elegiveis:", nrow(elegivel), "\n")
cat("NAs em margem_corr:", sum(is.na(elegivel$margem_corr)), "de", nrow(elegivel), "\n")
print(elegivel[, .(n = .N, margem_bruto_mediana = round(median(margem_bruto, na.rm=TRUE),2),
                    margem_corr_mediana = round(median(margem_corr, na.rm=TRUE),2)), by = fonte])
