suppressPackageStartupMessages(library(data.table))
th <- fread("C:/Users/joaoz/forecasting-fund-weights-vale-itau/v2 OFICIAL/data/theta_multiativo.csv")
cat("N celulas:", nrow(th), "\n")
for (v in c("b_aum","b_cot","b_fic","b_flow","b_betaf","b_hhi")) {
  cat(sprintf("%-8s mediana=%9.5f | %% positivo=%.1f%%\n", v, median(th[[v]]), 100*mean(th[[v]]>0)))
}
for (v in c("ape_aum","ape_cot","ape_fic","ape_flow","ape_betaf","ape_hhi")) {
  cat(sprintf("%-8s mediana=%9.6f | %% positivo=%.1f%%\n", v, median(th[[v]]), 100*mean(th[[v]]>0)))
}
