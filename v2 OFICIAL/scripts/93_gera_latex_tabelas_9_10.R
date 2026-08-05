suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
W <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_gestora_multihorizonte.csv"))
setorder(W, -rmse_h1)

fmt_num <- function(x, nd=5) {
  if (is.na(x)) return("---")
  s <- formatC(x, format="f", digits=nd)
  s <- sub("\\.", "{,}", s)
  paste0("$", s, "$")
}
fmt_pct <- function(x, nd=1) {
  if (is.na(x)) return("---")
  s <- formatC(x, format="f", digits=nd)
  s <- sub("\\.", "{,}", s)
  paste0("$", s, "\\%$")
}

cat("===== TABELA 9 (RMSE) =====\n")
for (i in 1:nrow(W)) {
  r <- W[i]
  cat(sprintf("%s & %d & %s & %s & %s & %s \\\\\n",
              r$gestora_grupo, r$n_fundos_h1,
              fmt_num(r$rmse_h1), fmt_num(r$rmse_h3), fmt_num(r$rmse_h6), fmt_num(r$rmse_h12)))
}

cat("\n\n===== TABELA 10 (MARGEM) =====\n")
for (i in 1:nrow(W)) {
  r <- W[i]
  cat(sprintf("%s & %s & %s & %s & %s \\\\\n",
              r$gestora_grupo,
              fmt_pct(r$margem_h1), fmt_pct(r$margem_h3), fmt_pct(r$margem_h6), fmt_pct(r$margem_h12)))
}

cat("\n\nGestoras sem VALE3/Legacy check -- quantas tem NA em h3/h6/h12?\n")
print(W[is.na(rmse_h3) | is.na(rmse_h6) | is.na(rmse_h12), .(gestora_grupo, rmse_h1, rmse_h3, rmse_h6, rmse_h12)])
