# =============================================================================
# 80_figura_ranking_horizontes.R  (v2 OFICIAL)
#
# Ideia 4: visualiza a estabilidade do ranking de "gestora mais dificil de
# prever" entre horizontes (ja afirmada em texto via Spearman 0,90-0,99,
# nunca mostrada visualmente) -- slopegraph, uma linha por gestora
# conectando sua posicao no ranking em h=1,3,6,12. So rotula os extremos
# (top-3 mais dificeis e top-3 mais previsiveis) pra nao poluir com 41
# linhas.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG <- file.path(REPO, "v2 OFICIAL/figuras")

tab <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_gestora_multihorizonte.csv"))
cat("Gestoras (bruto):", nrow(tab), "\n")

# exclui gestoras com NA em algum horizonte (aqui, so' Legacy Capital, que so
# tem dado em h=1) -- rank() trataria NA como "maior valor" por padrao,
# distorcendo o rank dos outros horizontes (bug pego ao comparar com o
# Spearman ja publicado no texto: dava 0,82 com o bug, 0,90 sem, batendo
# com "a mais baixa, 0,90" ja afirmado no v2 OFICIAL.tex)
tab <- tab[!is.na(rmse_h1) & !is.na(rmse_h3) & !is.na(rmse_h6) & !is.na(rmse_h12)]
cat("Gestoras completas nos 4 horizontes (usadas no slopegraph):", nrow(tab), "\n")

for (h in c("h1","h3","h6","h12")) {
  col <- paste0("rmse_", h)
  tab[[paste0("rank_",h)]] <- rank(-tab[[col]])  # rank 1 = maior RMSE = mais dificil
}
cat("\nCorrelacao de Spearman entre ranks:\n")
print(cor(tab[, .(rank_h1,rank_h3,rank_h6,rank_h12)], method="spearman"))

extremos <- unique(c(tab[order(rank_h1)][1:3]$gestora_grupo, tab[order(-rank_h1)][1:3]$gestora_grupo))
cat("\nGestoras rotuladas:", paste(extremos, collapse=", "), "\n")

pdf(file.path(FIG, "fig_ranking_horizontes_slopegraph.pdf"), width = 7.5, height = 6)
par(mar=c(4,2,3,9))
xs <- 1:4
plot(NULL, xlim=c(0.7,4.5), ylim=c(nrow(tab),1), xaxt="n", yaxt="n", xlab="", ylab="",
     main="Ranking de dificuldade de previsão fora da amostra,\npor horizonte (1 = mais difícil de prever)")
axis(1, at=xs, labels=c("h=1","h=3","h=6","h=12"))
for (g in tab$gestora_grupo) {
  y <- as.numeric(tab[gestora_grupo==g, .(rank_h1,rank_h3,rank_h6,rank_h12)])
  destaque <- g %in% extremos
  lines(xs, y, col = if(destaque) "#B8452E" else adjustcolor("grey70",0.5), lwd = if(destaque) 2 else 1)
}
for (g in extremos) {
  y4 <- tab[gestora_grupo==g]$rank_h12
  text(4.15, y4, g, adj=0, cex=0.65, col="#B8452E", xpd=NA)
}
dev.off()
cat("\nOK\n")
