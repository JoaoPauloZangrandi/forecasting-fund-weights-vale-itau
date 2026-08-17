suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
pa <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_universo_completo_por_ativo_h1.csv"))
print(pa[grepl("ALPAR", ativo, ignore.case=TRUE)])
cat("\n--- 10 piores margem_corr (elegiveis >=100 obs) ---\n")
print(pa[n_obs>=100][order(margem_corr)][1:10])
