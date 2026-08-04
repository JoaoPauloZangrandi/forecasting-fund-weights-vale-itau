# =============================================================================
# 84_persistencia_vies_gestora.R  (v2 OFICIAL)
#
# Pedido do Joao: o vies por gestora (script 77) e estavel, ou e so um
# acidente da janela de teste inteira? Divide os 23 meses de teste (h=1,
# base multiativo) em duas metades e checa se o vies medio de cada
# gestora na 1a metade prediz o vies na 2a metade (correlacao +
# regressao). Mesmo filtro de posicao-poeira do script 77.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG <- file.path(REPO, "v2 OFICIAL/figuras")

h1 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h1.csv"))
pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"), select=c("cod_fundo","ativo","peso"))
cel <- pp[, .(peso_mediano = median(peso)), by = .(cod_fundo, ativo)]
h1f <- merge(h1, cel[peso_mediano >= 0.001], by = c("cod_fundo","ativo"))

meses <- sort(unique(h1f$ym))
cat("Meses de teste:", length(meses), "(", min(meses), "a", max(meses), ")\n")
corte_meio <- meses[ceiling(length(meses)/2)]
cat("1a metade: ate", corte_meio, "| 2a metade: depois de", corte_meio, "\n")

h1f[, metade := ifelse(ym <= corte_meio, "primeira", "segunda")]

vies_metade <- h1f[, .(vies = mean(erro_oos), n = .N), by = .(gestora_grupo, metade)]
wide <- dcast(vies_metade, gestora_grupo ~ metade, value.var = c("vies","n"))
wide <- wide[!is.na(vies_primeira) & !is.na(vies_segunda) & n_primeira >= 50 & n_segunda >= 50]
cat("\nGestoras com >=50 obs em cada metade:", nrow(wide), "\n")

cat("\n===== Persistencia do vies entre 1a e 2a metade do teste =====\n")
corr <- cor(wide$vies_primeira, wide$vies_segunda)
fit <- lm(vies_segunda ~ vies_primeira, data = wide)
cat(sprintf("Correlacao (Pearson) = %.4f\n", corr))
cat(sprintf("Correlacao (Spearman, ranking) = %.4f\n", cor(wide$vies_primeira, wide$vies_segunda, method="spearman")))
cat(sprintf("Regressao: vies_2a = %.5f + %.4f * vies_1a | R2 = %.4f | p(coef) = %.4g\n",
            coef(fit)[1], coef(fit)[2], summary(fit)$r.squared, summary(fit)$coefficients[2,4]))

setorder(wide, -vies_primeira)
cat("\nTop 5 vies mais positivo na 1a metade -> o que fizeram na 2a:\n")
print(wide[1:5, .(gestora_grupo, vies_primeira=round(vies_primeira,5), vies_segunda=round(vies_segunda,5))])
cat("\nTop 5 vies mais negativo na 1a metade -> o que fizeram na 2a:\n")
print(tail(wide,5)[, .(gestora_grupo, vies_primeira=round(vies_primeira,5), vies_segunda=round(vies_segunda,5))])

pdf(file.path(FIG, "fig_persistencia_vies_gestora.pdf"), width=6.5, height=6)
plot(wide$vies_primeira, wide$vies_segunda, pch=19, col=adjustcolor("steelblue",0.6),
     xlab="Viés médio, 1ª metade do teste", ylab="Viés médio, 2ª metade do teste",
     main=sprintf("Persistência do viés por gestora\ncorrelação = %.3f", corr))
abline(h=0,v=0,col="grey80"); abline(fit, col="firebrick", lwd=2)
dev.off()

fwrite(wide, file.path(REPO, "v2 OFICIAL/data/persistencia_vies_gestora.csv"))
cat("\nOK - figura salva em fig_persistencia_vies_gestora.pdf\n")
