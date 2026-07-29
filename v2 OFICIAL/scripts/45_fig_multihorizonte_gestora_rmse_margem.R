# =============================================================================
# 45_fig_multihorizonte_gestora_rmse_margem.R  (v2 OFICIAL)
#
# Grafico pedido pelo Joao, no estilo da Figura 12 (vies x dispersao, script
# 38): agora cruza RMSE fora da amostra (h=1, quao dificil e prever a
# gestora em termos absolutos) com a MARGEM vs ingenua (h=1, se o ajuste
# parcial realmente ajuda ou atrapalha) -- mesma logica de "duas medidas de
# qualidade diferentes pro mesmo problema", so que agora pra Tabela 25/26 em
# vez do erro em-amostra da Secao 6.3.
#
# So rotula os extremos (mesma regra da Fig 12): top-6 por |RMSE| ou top-6
# por margem mais negativa, pra nao poluir o grafico com as 41 gestoras.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG  <- file.path(REPO, "v2 OFICIAL/figuras")

W <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_gestora_multihorizonte.csv"))
cat("Gestoras:", nrow(W), "\n")

pdf(file.path(FIG, "fig_multihorizonte_gestora_rmse_margem.pdf"), width = 7, height = 6)
par(mar = c(4,4,1,1))
plot(W$rmse_h1, W$margem_h1, pch = 16, col = "#2E5C8A", cex = 1.1,
     xlab = "RMSE fora da amostra, h=1 (dificuldade absoluta)",
     ylab = "margem vs. ingênua, h=1 (%) -- negativo = ajuste parcial piora")
abline(h = 0, col = "grey70", lty = 2)
rank_rmse <- rank(-W$rmse_h1); rank_margem <- rank(W$margem_h1)
rotula <- rank_rmse <= 6 | rank_margem <= 6
text(W$rmse_h1[rotula], W$margem_h1[rotula], labels = W$gestora_grupo[rotula],
     pos = 3, cex = 0.65, col = "grey20")
dev.off()
cat("OK - salvo em 'v2 OFICIAL/figuras/fig_multihorizonte_gestora_rmse_margem.pdf'\n")
