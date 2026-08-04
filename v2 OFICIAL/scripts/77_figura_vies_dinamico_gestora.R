# =============================================================================
# 77_figura_vies_dinamico_gestora.R  (v2 OFICIAL)
#
# Ideia 1: vies (erro medio, COM sinal, nao RMSE) por gestora ao longo do
# tempo -- pergunta diferente da "quem erra mais": quem compra/vende
# sistematicamente mais do que o modelo prevê, de forma persistente?
# Selecao das gestoras em destaque por VIES medio (nao por RMSE), pode
# dar nomes diferentes dos que ja apareceram nas figuras de magnitude.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG <- file.path(REPO, "v2 OFICIAL/figuras")

h1 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h1.csv"))

# mesmo filtro de posicao-poeira de sempre, aplicado a celula (fundo,ativo)
pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"), select=c("cod_fundo","ativo","peso"))
cel <- pp[, .(peso_mediano = median(peso)), by = .(cod_fundo, ativo)]
h1f <- merge(h1, cel[peso_mediano >= 0.001], by = c("cod_fundo","ativo"))
cat("h1 antes do filtro:", nrow(h1), "| depois:", nrow(h1f), "\n")

por_gestora <- h1f[, .(vies_medio = mean(erro_oos), n = .N), by = gestora_grupo]
por_gestora <- por_gestora[n >= 100]
setorder(por_gestora, -vies_medio)
cat("\nTop 5 vies mais POSITIVO (compram mais do que o modelo preve):\n")
print(por_gestora[1:5])
cat("\nTop 5 vies mais NEGATIVO (compram menos / vendem mais do que o modelo preve):\n")
print(tail(por_gestora,5))

pos <- por_gestora[1:3]$gestora_grupo
neg <- tail(por_gestora,3)$gestora_grupo

por_mes <- h1f[gestora_grupo %in% c(pos,neg), .(vies = mean(erro_oos)), by=.(gestora_grupo, ym)]
por_mes[, data := as.Date(paste0(substr(ym,1,4),"-",substr(ym,5,6),"-01"))]
setorder(por_mes, gestora_grupo, data)

pdf(file.path(FIG, "fig_vies_dinamico_gestora.pdf"), width = 8, height = 5.5)
par(mfrow=c(1,2), mar=c(4,4,2.5,1))
eixo_x <- range(por_mes$data)
ylim_c <- range(por_mes$vies)
cores_p <- c("#1E7A4D","#5DA9C7","#8A3E9E")
cores_n <- c("#B8452E","#D98A3D","#C74B7A")

plot(eixo_x, c(0,0), type="n", xaxt="n", ylim=ylim_c, xlab="Mês (teste)", ylab="Viés médio mensal (h=1)",
     main="Viés mais positivo")
axis.Date(1, at=seq(eixo_x[1],eixo_x[2],by="6 months"), format="%b/%Y")
abline(h=0,col="grey80")
for(i in seq_along(pos)){ dm<-por_mes[gestora_grupo==pos[i]]; lines(dm$data,dm$vies,col=cores_p[i],lwd=2) }
legend("topright", legend=pos, col=cores_p, lwd=2, bty="n", cex=0.6)

plot(eixo_x, c(0,0), type="n", xaxt="n", ylim=ylim_c, xlab="Mês (teste)", ylab="Viés médio mensal (h=1)",
     main="Viés mais negativo")
axis.Date(1, at=seq(eixo_x[1],eixo_x[2],by="6 months"), format="%b/%Y")
abline(h=0,col="grey80")
for(i in seq_along(neg)){ dm<-por_mes[gestora_grupo==neg[i]]; lines(dm$data,dm$vies,col=cores_n[i],lwd=2) }
legend("bottomright", legend=neg, col=cores_n, lwd=2, bty="n", cex=0.6)
dev.off()
cat("\nOK\n")
