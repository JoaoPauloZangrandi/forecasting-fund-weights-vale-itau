# =============================================================================
# 91_comparacao_limpo_vs_original.R  (v2 OFICIAL)
#
# Completa a comparacao que travou no script 90 (bug de sintaxe no
# setorder). So le os arquivos ja salvos pelo script 90 -- rapido, nao
# refaz nenhuma regressao.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
rmse <- function(x) sqrt(mean(x^2))

R1 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h1_limpo.csv"))
R1_orig <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h1.csv"))
R3 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h3_limpo.csv"))
R3_orig <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h3.csv"))

cat("===== RMSE agregado: original (contaminado) vs. limpo =====\n")
for (nm in c("h1","h3")) {
  o <- get(paste0("R", substr(nm,2,2), "_orig")); l <- get(paste0("R", substr(nm,2,2)))
  cat(sprintf("%s: original=%.6f | limpo=%.6f | dif=%.2f%%\n", nm, rmse(o$erro_oos), rmse(l$erro_oos),
              100*(rmse(l$erro_oos)-rmse(o$erro_oos))/rmse(o$erro_oos)))
}

W <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_gestora_multihorizonte_limpo.csv"))
W_orig <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_gestora_multihorizonte.csv"))
comp <- merge(W_orig[, .(gestora_grupo, rmse_h1_orig=rmse_h1, rmse_h3_orig=rmse_h3,
                          rmse_h6_orig=rmse_h6, rmse_h12_orig=rmse_h12)],
              W[, .(gestora_grupo, rmse_h1_limpo=rmse_h1, rmse_h3_limpo=rmse_h3,
                    rmse_h6_limpo=rmse_h6, rmse_h12_limpo=rmse_h12)], by="gestora_grupo")
comp[, dif_pct_h1 := 100*(rmse_h1_limpo-rmse_h1_orig)/rmse_h1_orig]
comp[, dif_pct_h3 := 100*(rmse_h3_limpo-rmse_h3_orig)/rmse_h3_orig]
comp[, abs_dif_h1 := abs(dif_pct_h1)]
setorder(comp, -abs_dif_h1)
cat("\n===== Gestoras com maior mudanca no RMSE h=1 (limpo vs original) =====\n")
print(comp[1:15, .(gestora_grupo, rmse_h1_orig=round(rmse_h1_orig,5), rmse_h1_limpo=round(rmse_h1_limpo,5),
                    dif_pct_h1=round(dif_pct_h1,1), dif_pct_h3=round(dif_pct_h3,1))])

cat("\n===== Ranking de dificuldade (RMSE h=1): muda muito? =====\n")
comp[, rank_orig := rank(-rmse_h1_orig)]
comp[, rank_limpo := rank(-rmse_h1_limpo)]
cat(sprintf("Correlacao de Spearman entre ranking original e limpo (RMSE h=1): %.4f\n",
            cor(comp$rank_orig, comp$rank_limpo, method="spearman")))
cat("Top 5 mais dificeis, ORIGINAL:\n"); print(comp[order(-rmse_h1_orig)][1:5, .(gestora_grupo, rmse_h1_orig)])
cat("Top 5 mais dificeis, LIMPO:\n"); print(comp[order(-rmse_h1_limpo)][1:5, .(gestora_grupo, rmse_h1_limpo)])

cat("\n\n===== Fundo com maior RMSE h=3, base LIMPA (candidato p/ substituir a Figura 11) =====\n")
por_fundo_h3 <- R3[, .(n=.N, rmse_h3 = rmse(erro_oos)), by = .(cod_fundo, gestora_grupo)][n>=6]
setorder(por_fundo_h3, -rmse_h3)
print(head(por_fundo_h3, 15))

cat("\n===== Esse fundo (o antigo campeao, 292826) ainda esta na base limpa? =====\n")
cat("292826 em R3 (limpo):", 292826 %in% R3$cod_fundo, "\n")
if (292826 %in% por_fundo_h3$cod_fundo) {
  cat("Posicao no ranking limpo:", which(por_fundo_h3$cod_fundo==292826), "de", nrow(por_fundo_h3), "\n")
  print(por_fundo_h3[cod_fundo==292826])
}

fwrite(comp, file.path(REPO, "v2 OFICIAL/data/comparacao_limpo_vs_original.csv"))
fwrite(por_fundo_h3, file.path(REPO, "v2 OFICIAL/data/ranking_fundo_h3_limpo.csv"))
cat("\nOK\n")
